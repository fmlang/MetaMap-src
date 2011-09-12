
/****************************************************************************
*
*                          PUBLIC DOMAIN NOTICE                         
*         Lister Hill National Center for Biomedical Communications
*                      National Library of Medicine
*                      National Institues of Health
*           United States Department of Health and Human Services
*                                                                         
*  This software is a United States Government Work under the terms of the
*  United States Copyright Act. It was written as part of the authors'
*  official duties as United States Government employees and contractors
*  and thus cannot be copyrighted. This software is freely available
*  to the public for use. The National Library of Medicine and the
*  United States Government have not placed any restriction on its
*  use or reproduction.
*                                                                        
*  Although all reasonable efforts have been taken to ensure the accuracy 
*  and reliability of the software and data, the National Library of Medicine
*  and the United States Government do not and cannot warrant the performance
*  or results that may be obtained by using this software or data.
*  The National Library of Medicine and the U.S. Government disclaim all
*  warranties, expressed or implied, including warranties of performance,
*  merchantability or fitness for any particular purpose.
*                                                                         
*  For full details, please see the MetaMap Terms & Conditions, available at
*  http://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/

% File:	    db_access.pl
% Module:   DB Access
% Author:   Lan
% Purpose:  Provide access to the NLS DBs (Berkeley DB version)


:- module(db_access, [
	db_get_all_acros_abbrs/2,
	db_get_concept_cui/2,
	db_get_concept_cuis/2,
	% db_get_concept_sts/2,
	db_get_cui_sourceinfo/2,
	db_get_cui_sources/2,
	db_get_cui_sts/2,
	db_get_mesh_mh/2,
	db_get_mesh_tc_relaxed/2,
	db_get_meta_mesh/2,
	db_get_mwi_word_count/3,
	db_get_mwi_word_data/4,
	db_get_synonyms/2,
	db_get_synonyms/3,
	db_get_unique_acros_abbrs/2,
	db_get_root_source_name/2,
	db_get_variants/3,
	db_get_versioned_source_name/2,
	default_release/1,
	get_data_version/1,
	get_data_year/1,
	initialize_db_access/0,
	initialize_db_access/3,
	stop_db_access/0
    ]).


:- use_module(skr_lib(nls_system), [
	control_option/1,
	control_value/2
    ]).

:- use_module(skr_lib(ctypes), [
	is_digit/1
    ]).

:- use_module(skr_lib(nls_strings),[
	atom_codes_list/2,
	concatenate_items_to_atom/2,
	concatenate_items_to_string/2,
	eliminate_multiple_meaning_designator_string/2,
	is_print_string/1,
	trim_whitespace/2
    ]).

:- use_module(skr_lib(sicstus_utils),[
	concat_atom/2,
	lower/2,
	ttyflush/0
    ]).

:- use_module(skr(skr_utilities),[
	debug_call/2,
	debug_message/3,
	ensure_atom/2,
	send_message/2
    ]).

:- use_module(library(file_systems), [
	directory_exists/1,
	directory_exists/2,
	file_exists/2
   ]).

:- use_module(library(sets),[
	del_element/3
    ]).


:- use_module(library(system),[
	environ/2
    ]).

:- use_module(library(lists),[
	append/2,
	remove_dups/2
    ]).

:- dynamic db_access_status/3.
:- dynamic db_access_var_table/1.

% foreign_resource(c_nls_db, [
foreign_resource(db_access, [
	c_nls_db_exec_2_list_jgm,
	exec_init_dbs,
	exec_destroy_dbs
   ]).

foreign(c_nls_db_exec_2_list_jgm, c, c_nls_db_exec_2_list_jgm(+string,-term,[-integer])).

foreign(exec_init_dbs, c, exec_init_dbs(+string)).

foreign(exec_destroy_dbs, c, exec_destroy_dbs).

:- load_foreign_resource('../db_access').

% :- abolish(foreign_resource/2, [force(true)]).

% :- abolish(foreign/3, [force(true)]).

verify_valid_dbs(Location) :-
	( \+ directory_exists(Location) ->
	  fatal_error('~n~nERROR: Database directory ~q does not exist.~n',
		      [Location]),
	  halt
	;  \+ directory_exists(Location, [read]) ->
	  fatal_error('~n~nERROR: Database directory ~q exists but is not readable.~n',
		      [Location]),
	  halt
	;  \+ directory_exists(Location, [execute]) ->
	  fatal_error('~n~nERROR: Database directory ~q exists but is not executable.~n',
		      [Location]),
	  halt
	; exec_init_dbs(Location)
	).

/* initialize_db_access
   initialize_db_access(+Version, +Year, +Model)
   stop_db_access

initialize_db_access/0 calls verify_valid_dbs to validate the BDB directory
for the specified version and data model.

The default version is "normal".

Current models for each version are: relaxed or strict.
stop_db_access/0 calls exec_destroy_dbs to close them.  */

default_version('USAbase').

default_release('2011AA').

initialize_db_access :-
	get_data_year(Year),
	get_data_version(Version),
	get_data_model(Model),
	initialize_db_access(Version, Year, Model).

initialize_db_access(Version, Year, Model) :-
	db_access_status(Version, Year, Model),
	!.
initialize_db_access(Version, Year, Model) :-
	set_var_table(VarTable),
	% Version is one of normal (default), nal, level0, etc.
	% Model   is one of strict (default), relaxed
	model_location(Version, Year, Model, Location),
	verify_valid_dbs(Location),
	assert(db_access_status(Version, Year, Model)),
	conditionally_announce_database_info(Version, Year, Model, Location, VarTable),
	!.
initialize_db_access(Version, Year, Model) :-
	fatal_error('~NERROR: Cannot open Berkeley DB databases (~a ~a ~a model).~n',
		    [Version,Year,Model]),
	halt.

conditionally_announce_database_info(Version, Year, Model, Location, VarTable) :-
	( \+ control_option(silent) ->
	   format('Berkeley DB databases (~a ~a ~a model) are open.~n',
		  [Version, Year, Model]),
	  format('Static variants will come from table ~s in ~w.~n',[VarTable,Location]),
	  announce_variant_info
	; true
	).

announce_variant_info :-
	variant_generation_mode(Mode),
	format('Derivational Variants: ~w.~n',[Mode]).

variant_generation_mode(Mode) :-
	( control_option(no_derivational_variants) ->
	  Mode = 'NONE'
	; control_option(all_derivational_variants) ->
	  Mode = 'ALL'
	; Mode = 'Adj/noun ONLY'
	).


set_var_table(VarTable) :-
	retractall(db_access_var_table(_)),
	( control_option(unique_acros_abbrs_only) ->
	    ( control_option(all_derivational_variants) ->
	      VarTable="varsu"
	    ; VarTable="varsanu"
	    )
        ;   ( control_option(all_derivational_variants) ->
	      VarTable="vars"
	    ; VarTable="varsan"
	    )
	),
	assert(db_access_var_table(VarTable)).

stop_db_access :-
	retractall(db_access_status(_,_,_)),
	exec_destroy_dbs.

model_location(Version, Year, ModelName, Location) :-
	model_location_base_dir(Path),
	concat_atom([Path, '/DB.', Version, '.', Year, '.', ModelName],  Location).

model_location_base_dir(Path) :- environ('MODEL_LOCATION_BASE_DIR', Path).

run_query(Query, QueryType, Results, Return) :-
	debug_message(db, '~N### Running ~w query ~w~n', [QueryType, Query]),
	c_nls_db_exec_2_list_jgm(Query, Results, Return),
	debug_call(db, length(Results, Length)),
	debug_message(db, '~N### Query returned ~d result(s)~n', [Length]).

/* db_get_concept_cui(+Concept, -CUI)

db_get_concept_cui/2 gets the CUI for Input which is assumed to be a concept
name.  */

db_get_concept_cui(Concept, CUIAtom) :-
	( Concept == [] ->
	  CUIAtom = []
	; ensure_string(Concept, ConceptString),
	  db_get_concept_cui_aux(ConceptString, Results),
	  get_cui_from_results(Results, CUIAtom)
	),
	!.
db_get_concept_cui(Concept, []) :-
	fatal_error('~NERROR: db_access: db_get_concept_cui failed for ~w~n', [Concept]),
	halt.

db_get_concept_cui_aux(ConceptAtom, CUI) :-
	form_simple_query("cui", "conceptcui", "concept", ConceptAtom, Query),
	run_query(Query, simple, CUIs0, 1),
	append(CUIs0, CUI).

get_cui_from_results([],      'C0000000').
get_cui_from_results([CUI|_], CUI).

db_get_versioned_source_name(RootSourceName, VersionedSourceName) :-
	ensure_string(RootSourceName, RootSourceNameString),
	db_get_versioned_source_name_aux(RootSourceNameString, VersionedSourceName),
	!.
db_get_versioned_source_name(RootSourceName, []) :-
	fatal_error('~NERROR: db_access: db_get_versioned_source_name failed for ~w~n', [RootSourceName]),
	halt.

db_get_versioned_source_name_aux(RootSourceName, VersionedSourceNames) :-
	form_simple_query("versioned, exists", "sab_rv", "root", RootSourceName, Query),
	run_query(Query, simple, VersionedSourceNames, 1).

db_get_root_source_name(VersionedSourceName, RootSourceName) :-
	ensure_string(VersionedSourceName, VersionedSourceNameString),
	db_get_root_source_name_aux(VersionedSourceNameString, RootSourceName),
	!.
db_get_root_source_name(VersionedSourceName, []) :-
	fatal_error('~NERROR: db_access: db_get_root_source_name failed for ~w~n', [VersionedSourceName]),
	halt.

db_get_root_source_name_aux(VersionedSourceName, RootSourceNames) :-
	form_simple_query("root, exists", "sab_vr", "versioned", VersionedSourceName, Query),
	run_query(Query, simple, RootSourceNames0, 1),
	append(RootSourceNames0, RootSourceNames).

/* db_get_concept_cuis(+Concept, -CUIs)

db_get_concept_cuis/2 gets the CUIs for Input which is assumed to be a concept
name. Note that this predicate is only needed for non-standard data sets
in which different CUIs can have the same preferred name. */

db_get_concept_cuis(Concept, CUIs) :-
	( Concept == [] ->
	  CUIs = []
	; ensure_string(Concept, ConceptString),
	  db_get_concept_cui_aux(ConceptString, CUIs0),
	  get_cuis_from_results(CUIs0, CUIs)
	).

get_cuis_from_results([],             ['C0000000']).
get_cuis_from_results([CUI|RestCUIs], [CUI|RestCUIs]).

/* db_get_cui_sources(+CUI, -Sources)

db_get_cui_sources/2 gets the sources, Sources, for the preferred name of Input, a CUI.  */

db_get_cui_sources(CUI, Sources) :-
	( CUI == [] ->
	  Sources = []
	; ensure_atom(CUI, CUIAtom),
	  db_get_cui_sources_aux(CUIAtom, Sources)
	),
	!.
db_get_cui_sources(CUI, []) :-
	fatal_error('~NERROR: db_access~w: db_get_cui_sources failed for ~p~n', [CUI]),
	halt.

db_get_cui_sources_aux(CUIAtom, Sources) :-
	form_simple_query("src", "cuisrc", "cui", CUIAtom, Query),
	run_query(Query, simple, Sources0, 1),
	append(Sources0, Sources).


/* db_get_cui_sourceinfo(+CUI, -Sourceinfo)

db_get_cui_sourceinfo/2 gets Sourceinfo for CUI, where Sourceinfo is a list of
lists [I,STR,SAB,TTY] where I is the ith entry (the order being
determined by mrconso.eng, STR is the string for source SAB with term type
TTY). Warning, CUI can be a string, but returned elements are either atoms
or numbers, not strings. */

db_get_cui_sourceinfo(CUI, SourceInfo) :-
	( CUI == [] ->
	  SourceInfo = []
	; ensure_string(CUI, CUIString),
	  db_get_cui_sourceinfo_aux(CUIString, SourceInfo)
	),
	!.
db_get_cui_sourceinfo(CUI, []) :-
	fatal_error('~NERROR: db_access~w: db_get_cui_sourceinfo failed for ~p~n', [CUI]),
	halt.

db_get_cui_sourceinfo_aux(CUIAtom, SourceInfo) :-
	form_simple_query("i, str, src, tty", "cuisourceinfo", "cui", CUIAtom, Query),
	run_query(Query, simple, SourceInfo0, 1),
	sort(SourceInfo0, SourceInfo).

%%% /* db_get_concept_sts(+Concept, -SemTypes)
%%% 
%%% db_get_concept_sts/2 gets the (abbreviated) semantic types, SemTypes, for Input.  */
%%% 
%%% db_get_concept_sts(Concept, SemTypes) :-
%%% 	( Concept == [] ->
%%% 	  SemTypes = []
%%% 	; ensure_atom(Concept, ConceptAtom),
%%% 	  db_get_concept_sts_aux(ConceptAtom, SemTypes)
%%% 	),
%%% 	!.
%%% db_get_concept_sts(Concept, []) :-
%%% 	fatal_error('~NERROR: db_access~w: db_get_concept_sts failed for ~w~n', [Concept]),
%%% 	halt.
%%% 
%%% db_get_concept_sts_aux(ConceptAtom, SemTypes) :-
%%% 	form_simple_query("st", "conceptst", "concept", ConceptAtom, Query),
%%% 	run_query(Query, simple, SemTypes0, 1),
%%% 	append(SemTypes0, SemTypes).

/* db_get_cui_sts(+CUI, -SemTypes)

db_get_cui_sts/2 gets the (abbreviated) semantic types, SemTypes, for Input.  */

db_get_cui_sts(CUI, SemTypes) :-
	( CUI == [] ->
	  SemTypes = []
	; ensure_atom(CUI, CUIAtom),
	  db_get_cui_sts_aux(CUIAtom, SemTypes)
	),
	!.
db_get_cui_sts(CUI, []) :-
	fatal_error('~NERROR: db_access~w: db_get_cui_sts failed for ~w~n', [CUI]),
	halt.

db_get_cui_sts_aux(CUIAtom, SemTypes) :-
	form_simple_query("st", "cuist", "concept", CUIAtom, Query),
	run_query(Query, simple, SemTypes0, 1),
	append(SemTypes0, SemTypes).

/* db_get_all_acros_abbrs(+Word, -AAPairs)

db_get_all_acros_abbrs/2 gets the list of acronym/abbreviation pairs
of Input using db_get_all_acros_abbrs_aux/2.  The elements of AAPairs are 
of the form <AA>:<type> where <AA> is an acronym, abbreviation or expansion
and <type> is either 'a' (acronym/abbreviation) or 'e' (expansion).  */

db_get_all_acros_abbrs(Word, AAPairs) :-
	( Word == [] ->
	  AAPairs = []
	; lower(Word, LCWord),
	  ensure_atom(LCWord, LCWordAtom),
	  db_get_all_acros_abbrs_aux(LCWordAtom, AAPairs)
	),
	!.
db_get_all_acros_abbrs(Word, []) :-
	fatal_error('~NERROR: db_access: db_get_all_acros_abbrs failed for ~p~n', [Word]).

db_get_all_acros_abbrs_aux(Word, AAPairs) :-
	form_simple_query("expansion, type", "nlsaa", "word", Word, Query),
	run_query(Query, simple, AAListPairs, 1),
	list_pairs_to_pairs(AAListPairs, AAPairs).

list_pairs_to_pairs([], []).
list_pairs_to_pairs([[L,R]|Rest], [L:R|RestPairs]) :-
	list_pairs_to_pairs(Rest, RestPairs).

/* db_get_unique_acros_abbrs(+Word, -AAPairs)

db_get_unique_acros_abbrs/2 gets the list of unique acronym/abbreviation pairs
of Input using db_get_unique_acros_abbrs_aux/2.  The elements of AAPairs are 
of the form <AA>:<type> where <AA> is an acronym, abbreviation or expansion
and <type> is either 'a' (acronym/abbreviation) or 'e' (expansion).  */

db_get_unique_acros_abbrs(Word, AAPairs) :-
	( Word == [] ->
	  AAPairs = []
	; lower(Word, LCWord),
	  ensure_atom(LCWord, LCWordAtom),
	  db_get_unique_acros_abbrs_aux(LCWordAtom, AAPairs)
	),
	!.
db_get_unique_acros_abbrs(Word, []) :-
	fatal_error('~NERROR: db_access: get_unique_acros_abbrs failed for ~p~n', [Word]).

% Result must be in a list for subsequent processing
% db_get_unique_acros_abbrs_aux(Word, [Expansion:Type]) :-
db_get_unique_acros_abbrs_aux(Word, AAPairs) :-
	form_simple_query("expansion, type", "nlsaau", "word", Word, Query),
	run_query(Query, simple, AAListPairs, 1),
        list_pairs_to_pairs(AAListPairs, AAPairs).

/* db_get_synonyms(+Word, -Synonyms)
   db_get_synonyms(+Word, +Category, -Synonyms)

db_get_synonyms/2 gets the list of Synonyms which are either
Dorland or NLS synonyms of Input.  The elements of Synonyms are either atoms
or strings depending on Input.
db_get_synonyms/3 restricts results by Category. */

db_get_synonyms(Word, Synonyms) :-
	( Word == [] ->
	  Synonyms = []
	; ensure_atom(Word, WordAtom),
	  db_get_synonyms_aux(WordAtom,Synonyms0),
	  remove_input(Synonyms0, WordAtom, Synonyms)
	),
	!.
db_get_synonyms(Word, []) :-
	fatal_error('~NERROR: db_access: db_get_synonyms/2 failed for ~p.~n', [Word]).

db_get_synonyms_aux(WordAtom, Synonyms) :-
	form_simple_query("syn, scat", "syns", "word", WordAtom, Query),
	run_query(Query, simple, Synonyms0, 1),
	normalize_synonyms(Synonyms0, Synonyms1),
	remove_dups(Synonyms1, Synonyms).

remove_input([], _, []).
remove_input([Input-_|Rest], Input, ModifiedRest) :-
	!,
	remove_input(Rest, Input, ModifiedRest).
remove_input([First|Rest],Input, [First|ModifiedRest]) :-
	remove_input(Rest, Input, ModifiedRest).

db_get_synonyms(Word, WordCategory, Synonyms) :-
	( Word == [] ->
	  Synonyms = []
        ; ensure_atom(Word, WordAtom),
	  ensure_atom(WordCategory, WordCategoryAtom),
	  db_get_synonyms_aux(WordAtom, WordCategoryAtom, Synonyms0),
	  del_element(WordAtom-WordCategoryAtom, Synonyms0, Synonyms)
	),
	!.
db_get_synonyms(Word, WordCategory, []) :-
	fatal_error('~NERROR: db_access: db_get_synonyms/3 failed for ~w/~w.~n', [Word, WordCategory]).

db_get_synonyms_aux(WordAtom, WordCategoryAtom, Synonyms) :-
	form_complex_query("syn, scat", "syns", "word", WordAtom, "wcat", WordCategoryAtom, Query),
	run_query(Query, complex, Synonyms0, 1),
	normalize_synonyms(Synonyms0, Synonyms1),
	remove_dups(Synonyms1, Synonyms).

/* normalize_synonyms(+Synonyms, -NormalizedSynonyms)
   normalize_synonym(+Synonym, -NormalizedSynonym)

normalize_synonyms/2 normalizes Synonyms using normalize_synonym/2
by lowercasing, stripping off expressions of the form <n> and stripping spaces.  */

normalize_synonyms([], []).
normalize_synonyms([[Syn,Cat]|Rest], [NormalizedSyn-Cat|NormalizedRest]) :-
	normalize_synonym(Syn, NormalizedSyn),
	normalize_synonyms(Rest, NormalizedRest).

normalize_synonym(Synonym, NormalizedSynonym) :-
	lower(Synonym, LCSynonym),
	atom_codes(LCSynonym, LCSynonymString),
	eliminate_multiple_meaning_designator_string(LCSynonymString, CSString0),
	trim_whitespace(CSString0, NormalizedSynonymString),
	atom_codes(NormalizedSynonym, NormalizedSynonymString).

/* db_get_mesh_tc_relaxed(+MeSH, -TreeCodes)

db_get_mesh_tc_relaxed/2 gets the list of MeSH tree codes for Input.
The elements of TreeCodes are either atoms or strings depending on Input.
Only MeSH terminology WITH treecodes will be found; thus subheadings and
check tags will produce nothing.  */

db_get_mesh_tc_relaxed(MeSH, TreeCodes) :-
	( MeSH == [] ->
	  TreeCodes = []
	; ensure_atom(MeSH, MeSHAtom),
	  db_get_mesh_tc_relaxed_aux(MeSHAtom, TreeCodes0),
	  atom_codes_list(TreeCodes0, TreeCodes)
	),
	!.
db_get_mesh_tc_relaxed(MeSH, []) :-
	fatal_error('~NERROR: db_access: db_get_mesh_tc_relaxed failed for ~p.~n', [MeSH]).

db_get_mesh_tc_relaxed_aux(MeSHAtom, TreeCodes) :-
	form_simple_query("tc", "meshtcrelaxed", "mesh", MeSHAtom, Query),
	run_query(Query, simple, TreeCodes0, 1),
	append(TreeCodes0, TreeCodes).

/* db_get_mesh_mh(+MeSH, -MH)

db_get_mesh_mh/2 gets the MeSH heading for Input, if any. All MeSH terms with a
heading should succeed. Everything else fails. */

db_get_mesh_mh(MeSH, MH) :-
	( MeSH == [] ->
	  MH = []
	; ensure_atom(MeSH, MeSHAtom),
	  db_get_mesh_mh_aux(MeSHAtom, MH)
	),
	!.
% db_get_mesh_mh(MeSH, []) :-
% 	fatal_error('~NERROR: db_access: db_get_mesh_mh failed for ~p.~n', [MeSH]).

db_get_mesh_mh_aux(MeSHAtom, MH) :-
	form_simple_query("mh", "meshmh", "mesh", MeSHAtom, Query),
	run_query(Query, simple, [[Result]], 1),
	( Result == 'X' ->
	  MH = MeSHAtom
	; MH = Result
	).

/* db_get_meta_mesh(+MeSH, -MHString)

db_get_meta_mesh/2 gets the MeSH heading for MeSH. It fails if there is no MeSH
for MeSH. Only MeSH terminology with (pseudo)treecodes will succeed. Thus
subheadings will fail. */

db_get_meta_mesh(MeSH, MHString) :-
	( MeSH == [] ->
	  MHString = []
	; ensure_atom(MeSH, MeSHAtom),
	  db_get_meta_mesh_aux(MeSHAtom, MHAtom),
	  atom_codes(MHAtom, MHString)
	),
	!.
% db_get_meta_mesh(MeSH, []) :-
% 	fatal_error('~NERROR: db_access: db_get_meta_mesh failed for ~p.~n', [MeSH]).

db_get_meta_mesh_aux(MeSHAtom, MH) :-
	form_simple_query("mesh", "metamesh", "meta", MeSHAtom, Query),
	run_query(Query, simple, [[Result]], 1),
	( Result == 'X' ->
	  MH = MeSHAtom
	; MH = Result
	).

/* db_get_mwi_word_data(+Table, +Word, +DebugFlags, -Results)

db_get_mwi_word_data/4 gets word data (Results) from Table for Word.
Results is a list of terms of the form
     usc(NMStr,String,Concept)
where NMStr is the normalized form of String.
*/

db_get_mwi_word_data(Table, Word, DebugFlags, Results) :-
	( Word == [] ->
	  Results = []
	; ensure_atom(Word, WordAtom),
	  ensure_atom(Table, TableAtomTemp),
	  possibly_widen_table(TableAtomTemp, TableAtom, Widen),
	  debug_db_get_mwi_data_1(DebugFlags),
	  db_get_mwi_word_data_aux(Widen, TableAtom, WordAtom, DebugFlags, RawResults),
	  debug_db_get_mwi_data_2(DebugFlags),
	  form_uscs(RawResults, 0, Results),
	  debug_db_get_mwi_data_3(DebugFlags, Results)
	  ),
	!.
db_get_mwi_word_data(Table, Word, _DebugFlags, []) :-
	fatal_error('~NERROR: db_access: db_get_mwi_word_data failed for word ~p on table ~p.~n',
		    [Word,Table]),
	halt.

possibly_widen_table(NarrowTable, WideTable, Widen) :-
	db_access_status(Version, Year, Model),
	model_location(Version, Year, Model, Location),
	concat_atom([NarrowTable, '_WIDE'], MaybeWideTable),
	concat_atom([Location, '/', MaybeWideTable], WideTablePath),
	( file_exists(WideTablePath, read) ->
	  WideTable = MaybeWideTable,
	  Widen is 1
	; WideTable = NarrowTable,
	  Widen is 0
	).

% Suppose QueryString (the word being looked up) is "heart" and Table is first_wordsb.
% The monstrous code below creates the query
% 'select suistrings.nmstr, suistrings.str, cuiconcept.concept
%         from first_wordsb, suistrings, cuiconcept
%        where first_wordsb.word=''heart''
%          and first_wordsb.sui = suistrings.sui
%          and first_wordsb.cui = cuiconcept.cui'

db_get_mwi_word_data_aux(0, Table, Word, DebugFlags, RawResults) :-
	db_get_mwi_word_data_NARROW(Table, Word, DebugFlags, RawResults).
db_get_mwi_word_data_aux(1, Table, Word, DebugFlags, RawResults) :-
	db_get_mwi_word_data_WIDE(Table, Word, DebugFlags, RawResults).

% This is the wide version
db_get_mwi_word_data_WIDE(Table, Word, DebugFlags, RawResults) :-
	form_simple_query("nmstr, str, concept", Table, "word", Word, Query),	
        run_query(Query, simple, RawResults, 1),
	debug_db_get_mwi_data_aux_2(DebugFlags, RawResults),
	!.
db_get_mwi_word_data_WIDE(Table, Word, _DebugFlags, _RawResults) :-
	fatal_error('~NERROR: db_access: db_get_mwi_word_data_aux failed for ~p on table ~p.~n',
		    [Word,Table]),
	halt.

% This is the narrow version
db_get_mwi_word_data_NARROW(Table, Word, DebugFlags, RawResults) :-
	% Table is one of all_words, first_words, first_wordsb,
	% first_words_of_one, first_words_of_two
	concatenate_items_to_string(["suistrings.nmstr, suistrings.str, ",
                                    "cuiconcept.concept"],
                                   Fields),
        concatenate_items_to_string([Table,
                                     ", suistrings, cuiconcept"],
                                    Tables),
        concatenate_items_to_string([Table,".word"], Field),
        form_simple_query(Fields, Tables, Field, Word, Query0),
        concatenate_items_to_string([" and ",
                                     Table, ".sui = suistrings.sui and ",
                                     Table, ".cui = cuiconcept.cui"],
                                    QueryTail),
        concatenate_items_to_atom([Query0,QueryTail], Query),
        run_query(Query, join, RawResults, 1),
	debug_db_get_mwi_data_aux_2(DebugFlags, RawResults),
	!.

db_get_mwi_word_data_NARROW(Table, Word, _DebugFlags, _RawResults) :-
	fatal_error('~NERROR: db_access: db_get_mwi_word_data_aux failed for ~p on table ~p.~n',
		    [Word,Table]),
	halt.

form_uscs([], _N, []) :- !.
form_uscs([First|Rest], N, [usc(Nmstr,Str,Concept)|ModifiedRest]) :-
	First = [Nmstr,Str,Concept0],
	!,
	( Concept0 == 'X' ->
	  Concept = Str
	; Concept = Concept0
	),
	NewN is N + 1,
	form_uscs(Rest, NewN, ModifiedRest).
form_uscs([First|_], N, _) :-
	!,
	NewN is N + 1,
	fatal_error('~NERROR: db_access: form_uscs failed on item ~d = ~p.~n', [NewN,First]),
	halt.
form_uscs(X, N, _) :-
	fatal_error('~NERROR: db_access: form_uscs failed after item ~d; the non-list is ~p.~n',
		    [N,X]),
	halt.

/* db_get_mwi_word_count(+Table, +Input, -Count)

db_get_mwi_word_count/3 gets the word Count from Table for Input.
It fails if Input is not in Table. Note that since counts of 1 are not stored
in the table, failure indicates a count of either 0 or 1. */

db_get_mwi_word_count(Table, Word, Count) :-
	Word \== [],
	ensure_atom(Table, TableAtom),
	ensure_atom(Word, WordAtom),
	db_get_mwi_word_count_aux(TableAtom, WordAtom, [[Count]]),
	!.
% This predicate must be allowed to fail gracefully
% db_get_mwi_word_count(Table, Word, 0) :-
%        fatal_error('~NERROR: db_access: db_get_mwi_word_count failed for ~p on table ~p.~n',
%              	      [Word,Table]).

db_get_mwi_word_count_aux(TableAtom, WordAtom, WordCount) :-
	form_simple_query("wcount", TableAtom, "word", WordAtom, Query),
	run_query(Query, simple, WordCount, 1).

/* 
   db_get_variants(+Concept, +Category, -Variants)

db_get_variants/3 gets the list of Variants of Concept, which can be either
an atom or a string.  The result can depend on the Category, but if the
Category is [], no filtering is done on Category.
The table used ot obtain the variants is determined by db_access_var_table/1. */

db_get_variants(Concept, Category, Variants) :-
	( ensure_string(Concept, ConceptString),
	  db_get_variants_aux(ConceptString, Category, Variants)
	),
	!.
db_get_variants(Concept, Category, []) :-
	fatal_error('~NERROR: db_access: db_get_variants/3 failed for ~p (~p).~n',
		    [Concept,Category]),
	halt.

% always treat adjectives as nouns, if possible
db_get_variants_aux(Concept, Category, Variants) :-
	( Category == adj,
	  get_variants_from_db(Concept, noun, Variants),
	  Variants \== [] ->
	  true
	; get_variants_from_db(Concept, Category, Variants)
	),
	!.

get_variants_from_db(Concept, Category, Variants) :-
	db_access_var_table(VarTable),
	ensure_string(VarTable, VarTableString),
	ensure_atom(Category, CategoryAtom),
	form_complex_query("var, vcat, dist, hist, roots",
			   VarTableString, "word", Concept, "wcat", CategoryAtom, Query),
	run_query(Query, complex, Variants0, 1),
	% format('~nget_variants_aux:~nquery = ~p~nresult = ~p~n',[Query,Variants0]),
	sort(Variants0, Variants1),
	% format('~nafter sort~n', []),
	convert_to_variant_terms(Variants1, Variants).

convert_to_variant_terms([], []).
% temp  to handle null values until they're removed
% convert_to_variant_terms([[]|Rest],ConvertedRest) :-
%    !,
%    convert_to_variant_terms(Rest,ConvertedRest).
convert_to_variant_terms([[Var,VCat0,Distance,Hist0,Roots]|Rest],
                         [v(Var,VCat,DistInteger,Hist,Roots,_)|ConvertedRest]) :-
	ensure_atom(Distance, DistAtom),
	atom_codes(DistAtom, DistCodes),
	number_codes(DistInteger, DistCodes),
	convert_variant_category(VCat0, VCat),
	% format('~nconverted ~q to ~q~n', [VCat0, VCat]),
	atom_codes(Hist0, Hist),
	convert_to_variant_terms(Rest, ConvertedRest).

convert_variant_category(VCat0, VCat) :-
	( VCat0 == none ->
	  VCat = []
	; VCat0 == [] ->
	  VCat = []
	; VCat = [VCat0]
	).

/* form_simple_query(+Fields, +Table, +Field, +Value, -Query)
   form_complex_query(+Fields, +Table, +Field1, +Value1, +Field2, +Value2, -Query)

form_simple_query/5 constructs the atom Query of the form
     select <Fields> from <Table> where <Field>="Value"
where Fields, Table, Field, and Value can be atoms or strings.
form_complex_query/7 constructs the atom Query of the form
     select <Fields> from <Table> where <Field1>="Value1" and <Field2>="Value2"
where Fields, Table, Field1, Value1, Field2, and Value2 can be atoms or strings.
*/

form_simple_query(Fields, Table, Field, Value0, Query) :-
	form_complex_query(Fields, Table, Field, Value0, [], [], Query).

form_complex_query(Fields, Table, Field1, Value1_0, Field2, Value2_0, Query) :-
        ensure_string(Value1_0, Value1_1),
        double_quotes(Value1_1, Value1),
        form_rest_query(Value2_0, Field2, RestQuery),
        concatenate_items_to_atom(["select ",Fields," from ",Table,
                                   " where ",Field1,"='",Value1 | RestQuery],
                                  Query).

form_rest_query(Value2_0, Field2, RestQuery) :-
	% in db_get_variants/3, Value2_0 is the Category;
	% if the Category is [], ignore it, and
	% don't instantiate the rest of the query,
	% other than closing the final quote!
	( Value2_0 == [] ->
	  RestQuery = ["'"]
	; ensure_string(Value2_0, Value2_1),
	  double_quotes(Value2_1, Value2),
	  RestQuery = [ "' and ",Field2,"='",Value2,"'"]
	).

/* double_quotes(+String, -ModifiedString)

double_quotes/2 computes ModifiedString by doubling each occurrence of a
single quotation mark.  */

double_quotes("", "").
double_quotes([39|Rest], [39,39|ModifiedRest]) :-
	!,
	double_quotes(Rest, ModifiedRest).
double_quotes([First|Rest], [First|ModifiedRest]) :-
	double_quotes(Rest, ModifiedRest).

get_data_version(Version) :-
	( control_value(mm_data_version, Version) ->
	  true
        ; default_version(Version)
        ).

% 2008AA, 2009AB, etc.
get_data_year(NormalizedYear) :-
	default_release(DefaultYear),
	normalize_db_access_year(DefaultYear, NormalizedDefaultYear),
	% Is the model year explicitly specified on the command line?
	( control_value(mm_data_year, SpecifiedYear),
	  normalize_db_access_year(SpecifiedYear, NormalizedSpecifiedYear),
	  NormalizedDefaultYear \== NormalizedSpecifiedYear ->
	  send_message('##### WARNING: Overriding default model ~w with ~w.~n',
		       [NormalizedDefaultYear, SpecifiedYear]),
	  NormalizedYear = NormalizedSpecifiedYear
	; NormalizedYear = NormalizedDefaultYear
	).

normalize_db_access_year(Year, NormalizedYear) :-
	ensure_atom(Year, YearAtom),
	( YearAtom = '99' ->
	  NormalizedYear = '1999AA'
	; YearAtom = '1999' ->
	  NormalizedYear = '1999AA'
	; atom_length(YearAtom, YearLength),
	  atom_codes(YearAtom, Codes),
	  ( % e.g., 08, 09, 10, etc.
	    YearLength =:= 2,
	    all_digits(Codes) ->
	    concat_atom(['20', YearAtom, 'AA'], NormalizedYear)
	  ; YearLength =:= 4,
	    all_digits(Codes) ->
	    Codes = [FirstCode,SecondCode|_],
	      % e.g., 2008, 2009, 2010, etc.
	    ( FirstCode == 0'2 ->
	      concat_atom([YearAtom, 'AA'], NormalizedYear)
	      % e.g., 0809, 0910, etc.
	    ; FirstCode == 0'0 ->
	      atom_codes(FirstDigit, [FirstCode]),
	      atom_codes(SecondDigit, [SecondCode]),
	      concat_atom(['20', FirstDigit, SecondDigit, 'AB'], NormalizedYear)
	      % e.g., 1011, 1112, etc.
	    ; FirstCode == 0'1 ->
	      atom_codes(FirstDigit, [FirstCode]),
	      atom_codes(SecondDigit, [SecondCode]),
	      concat_atom(['20', FirstDigit, SecondDigit, 'AB'], NormalizedYear)
	    )
	  ; YearLength =:= 4,
	    % Nonstandard, e.g., 08AA, 09AB, 10AA, etc.
	    Codes = [D1, D2, AB1, AB2],
	    is_digit(D1),
	    is_digit(D2),
	    is_A_or_B(AB1),
	    is_A_or_B(AB2) ->
	    concat_atom(['20', Year], NormalizedYear)
	  ; YearLength =:= 6,
	    % e.g., 2008AA, 2009AB, 2010AA, etc.
	    Codes = [D1, D2, D3, D4, AB1, AB2],
	    all_digits([D1,D2,D3,D4]),
	    is_A_or_B(AB1),
	    is_A_or_B(AB2) ->
	    NormalizedYear = YearAtom
	  )
	),
	!.

normalize_db_access_year(Year, _NormalizedYear) :-
	format(user_output, 'ERROR in specifying Model Year (~w)~n', [Year]),
	fail.


all_digits([]).
all_digits([H|T]) :- is_digit(H), all_digits(T).

is_A_or_B(0'A).
is_A_or_B(0'B).


get_data_model(Model) :-
	% This is more complex than it need be.
	( control_option(strict_model) ->
	  Model = strict
	; control_option(relaxed_model) ->
	  Model = relaxed
	; Model = strict
	).

debug_db_get_mwi_data_1(DebugFlags) :-
	( memberchk(7, DebugFlags) ->
	  format('~ndb_get_mwi_word_data: Input is an atom.~n',[]),
	  ttyflush
	; true
	).

debug_db_get_mwi_data_2(DebugFlags) :-
	( memberchk(7, DebugFlags) ->
	  format('db_get_mwi_word_data: back from db_get_mwi_word_data_aux calling form_uscs.~n',[]),
	  ttyflush
	; true
	).

debug_db_get_mwi_data_3(DebugFlags, Results) :-
	( memberchk(7, DebugFlags) ->
	  format('db_get_mwi_word_data: ',[]),
	  ttyflush,
	  length(Results,NR),
	  format('~d form_uscs results.~n',[NR]),
	  ttyflush
	; true
	).

debug_db_get_mwi_data_aux_2(DebugFlags, RawResults) :-
	( memberchk(7, DebugFlags) ->
	  format('db_get_mwi_word_data_aux: ',[]),
	  ttyflush,
	  length(RawResults,NRR),
	  format('~d RawResults.~n',[NRR]),
	  ttyflush,
	  format('db_get_mwi_word_data_aux: RawResults = ~p~n',[RawResults]),
	  ttyflush
	; true
	).

ensure_string(AtomOrString, String) :-
        ( is_print_string(AtomOrString) ->
          String = AtomOrString
        ; atom_codes(AtomOrString, String)
        ).

fatal_error(Message, Args) :-
	format(user_output, Message, Args),
	ttyflush,
	current_output(OutputStream),
	% don't duplicate message if the default output stream is user_output!
	( stream_property(OutputStream, alias(user_output)) ->
	  true
	; format(Message, Args)
	).
