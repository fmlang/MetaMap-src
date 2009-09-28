% File:	    db_access.pl
% Module:   DB Access
% Author:   Lan
% Purpose:  Provide access to the NLS DBs (Berkeley DB version)


:- module(db_access,[
	default_full_year/1,
	default_year/1,
	db_get_concept_cui/2,
	db_get_concept_cuis/2,
	db_get_concept_sts/2,
	db_get_cui_sourceinfo/2,
	db_get_cui_sources/2,
	db_get_cui_sts/2,
	db_get_mesh_mh/2,
	db_get_mesh_tc_relaxed/2,
	db_get_mesh_tc_strict/2,
	db_get_meta_mesh/2,
	db_get_meta_mesh_tc/3,
	db_get_mwi_word_count/3,
	db_get_mwi_word_data/4,
	db_get_all_acros_abbrs/2,
	db_get_synonyms/2,
	db_get_synonyms/3,
	db_get_unique_acro_abbr/2,
	db_get_variants/3,
	get_db_access_year/1,
	initialize_db_access/0,
	initialize_db_access/3,
	stop_db_access/0
    ]).


:- use_module(skr_lib(nls_system), [
	control_option/1,
	control_value/2
    ]).

:- use_module(skr_lib(nls_strings),[
	atom_codes_list/2,
	atom_codes_pair_list/2,
	concatenate_items_to_atom/2,
	concatenate_items_to_string/2,
	double_quotes/2,
	eliminate_multiple_meaning_designator_string/2,
	is_print_string/1,
	trim_whitespace/2
    ]).

:- use_module(skr_lib(sicstus_utils),[
	concat_atoms/2,
	lower/2,
	ttyflush/0
    ]).

:- use_module(skr(skr_utilities),[
	debug_call/2,
	debug_message/3,
	ensure_atom/2
    ]).

:- use_module(library(bdb), [
	db_close/1,
     	db_fetch/3,
     	db_open/4,
	db_findall/5
   ]).

:- use_module(library(file_systems), [
	directory_exists/1,
	directory_exists/2
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
% foreign_resource(db_access, [
% 	c_nls_db_exec_2_list_jgm,
% 	exec_init_dbs,
% 	exec_destroy_dbs
%    ]).

% foreign(c_nls_db_exec_2_list_jgm, c, c_nls_db_exec_2_list_jgm(+string,-term,[-integer])).

% foreign(exec_init_dbs, c, exec_init_dbs(+string)).

% foreign(exec_destroy_dbs, c, exec_destroy_dbs).

% :- load_foreign_resource(db_access).

% :- abolish(foreign_resource/2, [force(true)]).

% :- abolish(foreign/3, [force(true)]).

verify_valid_dbs(Location) :-
	( \+ directory_exists(Location) ->
	  format('~n~nERROR: Database directory ~q does not exist!!!~n~n',
		 [Location]),
	  abort
	;  \+ directory_exists(Location, [read]) ->
	  format('~n~nERROR: Database directory ~q exists but is not readable!!!~n~n',
		 [Location]),
	  abort
	;  \+ directory_exists(Location, [execute]) ->
	  format('~n~nERROR: Database directory ~q exists but is not executable!!!~n~n',
		 [Location]),
	  abort
	; true
	).

/* initialize_db_access
   initialize_db_access(+Version, +Year,+Model)
   stop_db_access

initialize_db_access/0 calls verify_valid_dbs to validate the BDB directory
default version and data model.

The default version is "normal".

Current models for each version are: relaxed or strict.
stop_db_access/0 calls exec_destroy_dbs to close them.  */

default_version(normal).

default_year('09').

default_full_year(2009).

initialize_db_access :-
	get_db_access_version(Version),
	get_db_access_model(Model),
	get_db_access_year(Year),
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
	format('~NERROR: Cannot open Berkeley DB databases (~a ~a ~a model).~n',
	       [Version,Year,Model]),
	abort.

conditionally_announce_database_info(Version, Year, Model, Location, VarTable) :-
	( \+ control_option(no_header_info) ->
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


warn_about_model(Model) :-
    format('WARNING: The additional request for the ~a has been ignored.~n',
           [Model]).

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
	retractall(db_access_status(_,_,_)).

model_location(Version, Year, ModelName, Location) :-
	environ('MODEL_LOCATION_BASE_DIR', PathBase),
	concat_atoms([PathBase, '/DB.', Version, '.', Year, '.', ModelName],  Location).

create_BDB_full_path(DBDir, Version, Year, Model, TableName, TablePathName) :-
	concat_atoms([DBDir, '/', 'DB', '.', Version, '.', Year, '.', Model, '/', TableName],
		     TablePathName).

% The QueryTerm is a Prolog term whose functor is the name of the table being queried.
skr_db_fetch_data(QueryTerm) :-
	functor(QueryTerm, TableName, _Arity),
	skr_db_open(TableName, DBRef),
	skr_db_fetch(DBRef, TableName, QueryTerm, _TermRef),
	skr_db_close(TableName, DBRef).

skr_db_findall_data(QueryTerm, Template, OtherGoal, Results) :-
	functor(QueryTerm, TableName, _Arity),
	skr_db_open(TableName, DBRef),
	skr_db_findall(DBRef, TableName, Template, QueryTerm, OtherGoal, Results),
	skr_db_close(TableName, DBRef).

skr_db_open(TableName, DBRef) :-
	environ('MODEL_LOCATION_BASE_DIR', DBDir),
	db_access_status(Version, Year, Model),
 	debug_message(trace, '~N### Opening table ~w in model ~w.~w.~w~n',
		      [TableName,Version,Year,Model]),
	create_BDB_full_path(DBDir, Version, Year, Model, TableName, TablePathName),
	db_open(TablePathName, read, _SpecList, DBRef).

skr_db_close(TableName, DBRef) :-
 	debug_message(trace, '~N### Closing table ~w~n', [TableName]),
	db_close(DBRef).

skr_db_fetch(DBRef, TableName, QueryTerm, _TermRef) :-
	arg(1, QueryTerm, Arg),
 	debug_message(trace, '~N###     Getting single value from ~w for ~q~n', [TableName, Arg]),
	db_fetch(DBRef, QueryTerm, _TermRef),
	% Must have a cut after db_fetch
	!.

skr_db_findall(DBRef, TableName, Template, QueryTerm, OtherGoal, Results) :-
	arg(1, QueryTerm, Arg),
	debug_message(trace, '~N###     Getting multiple values from ~w for ~q: ', [TableName, Arg]),
	db_findall(DBRef, Template, QueryTerm, OtherGoal, Results),
	debug_call(trace, length(Results, ResultsLength)),
	debug_message(trace, '~w~n', [ResultsLength]).

% run_query(Query, Results, Return) :-
% 	debug_message(trace, '~N### Running query ~w~n', [Query]),
% 	c_nls_db_exec_2_list_jgm(Query, Results, Return).


/* db_get_concept_cui(+Input, -CUI)

db_get_concept_cui/2 gets the CUI for Input which is assumed to be a concept
name.  */

db_get_concept_cui(Concept, CUI) :-
	( Concept == [] ->
	  CUI = []
	; ensure_atom(Concept, ConceptAtom),
	  db_get_concept_cui_aux(ConceptAtom, CUI)
	  % get_first_cui(CUIResults, CUI)
	),
	!.
db_get_concept_cui(Concept, []) :-
	format('~NERROR: db_access: db_get_concept_cui failed for ~w~n', [Concept]).

db_get_concept_cui_aux(ConceptAtom, CUI) :-
	% This call always returns a single element, so db_fetch is sufficient.
	skr_db_fetch_data(concept_cui(ConceptAtom, CUI)).
	% form_simple_query("cui", "conceptcui", "concept", QueryString, Query),
	% run_query(Query, CUIs0, 1).

get_first_cui(CUIResults, CUI) :-
	( CUIResults = [CUI|_] ->
	  true
	; CUI = 'C0000000'
	).

/* db_get_concept_cuis(+Input, -CUIs)

db_get_concept_cuis/2 gets the CUIs for Input which is assumed to be a concept
name. Note that this predicate is only needed for non-standard data sets
in which different CUIs can have the same preferred name. */

db_get_concept_cuis([],[]) :-
    !.
db_get_concept_cuis(Input,CUIs) :-
    (   atom(Input) ->
        db_get_concept_cui_aux(Input,CUIs0),
        (CUIs0==[] ->
            CUIs=['C0000000']
        ;   CUIs=CUIs0
        )
    ;   is_print_string(Input) ->
	atom_codes(Atom, Input),
        db_get_concept_cui_aux(Atom,CUIs0),
        (CUIs0==[] ->
            CUIs1=['C0000000']
        ;   CUIs1=CUIs0
        ),
        atom_codes_list(CUIs1,CUIs)
    ;   fail
    ),
    !.

/* db_get_cui_sts(+Input, -SemTypes)

db_get_cui_sts/2 gets the (abbreviated) semantic types, SemTypes, for the CUI,
Input.  */

db_get_cui_sts(CUI, SemTypes) :-
	( CUI == [] ->
	  SemTypes = []
	; ensure_atom(CUI, CUIAtom),
	  db_get_cui_sts_aux(CUIAtom, SemTypes)
	),
	!.
db_get_cui_sts(CUI, []) :-
	format('~NERROR: db_access: db_get_cui_sts failed for ~w~n', [CUI]).

% I don't think this ever gets called
db_get_cui_sts_aux(CUIAtom, SemTypes) :-
	% If this code ever does get called,
	% it must be able to return a list, so use db_findall.
	skr_db_findall_data(cui_st(CUIAtom, SemType), SemType, true, SemTypes).
	% form_simple_query("st", "cuist", "cui", QueryString, Query),
	% run_query(Query, SemTypes0, 1),


/* db_get_cui_sources(+Input, -Sources)

db_get_cui_sources/2 gets the sources, Sources, for the preferred name of Input, a
CUI.  */

db_get_cui_sources(CUI, Sources) :-
	( CUI == [] ->
	  Sources = []
	; ensure_atom(CUI, CUIAtom),
	  db_get_cui_sources_aux(CUIAtom, Sources)
	),
	!.
db_get_cui_sources(CUI, []) :-
	format('~NERROR: db_access~w: db_get_cui_sources failed for ~p~n', [CUI]).
	       
db_get_cui_sources_aux(CUIAtom, Sources) :-
	% This call can return multiple values, so must be a db_findall.
	skr_db_findall_data(cui_src(CUIAtom, Source), Source, true, Sources).
	% form_simple_query("src", "cuisrc", "cui", QueryString, Query),
	% run_query(Query, Sources0, 1),
	% append(Sources0, Sources).


/* db_get_cui_sourceinfo(+CUI, -Sourceinfo)

db_get_cui_sourceinfo/2 gets Sourceinfo for CUI, where Sourceinfo is a list of
lists [I,STR,SAB,TTY] where I is the ith entry (the order being
determined by mrconso.eng, STR is the string for source SAB with term type
TTY). Warning, CUI can be a string, but returned elements are either atoms
or numbers, not strings. */

db_get_cui_sourceinfo(CUI, SourceInfo) :-
	( CUI == [] ->
	  SourceInfo = []
	; ensure_atom(CUI, CUIAtom),
	  db_get_cui_sourceinfo_aux(CUIAtom, SourceInfo)
	),
	!.
db_get_cui_sourceinfo(CUI, []) :-
	format('~NERROR: db_access~w: db_get_cui_sourceinfo failed for ~p~n', [CUI]).

db_get_cui_sourceinfo_aux(CUIAtom, SourceInfo) :-
	% This call can return multiple values, so must be a db_findall.
	skr_db_findall_data(cui_sourceinfo(CUIAtom,_SUI,I,Str,Src,Tty),
			    [I,Str,Src,Tty], true, SourceInfo0),
	% form_simple_query("i, str, src, tty", "cuisourceinfo", "cui", QueryString, Query),
	% run_query(Query, Sourceinfo0, 1),
	sort(SourceInfo0, SourceInfo).


/* db_get_concept_sts(+Input, -SemTypes)

db_get_concept_sts/2 gets the (abbreviated) semantic types, SemTypes, for Input.  */

db_get_concept_sts(Concept, SemTypes) :-
	( Concept == [] ->
	  SemTypes = []
	; ensure_atom(Concept, ConceptAtom),
	  db_get_concept_sts_aux(ConceptAtom, SemTypes)
	),
	!.
db_get_concept_sts(Concept, []) :-
	format('~NERROR: db_access~w: db_get_concept_sts failed for ~w~n', [Concept]).

db_get_concept_sts_aux(ConceptAtom, SemTypes) :-
	% This call can, albeit in rare cases, return multiple values,
	% so must be a db_findall.
	skr_db_findall_data(concept_st(ConceptAtom,SemType), SemType, true, SemTypes).
	% form_simple_query("st", "conceptst", "concept", QueryString, Query),
	% run_query(Query, SemTypes0, 1),
	% append(SemTypes0,SemTypes).

/* db_get_all_acros_abbrs(+Input, -AAPairs)

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
	format('~NERROR: db_access: db_get_all_acros_abbrs failed for ~p~n', [Word]).

db_get_all_acros_abbrs_aux(Word, AAPairs) :-
	% form_simple_query("expansion, type", "nlsaa", "word", QueryString, Query),
	% run_query(Query, AAListPairs, 1),
	% list_pairs_to_pairs(AAListPairs, AAPairs).
	% This call can return multiple values, so must be a db_findall.
	skr_db_findall_data(nls_aa(Word,Expansion,Type,_), Expansion:Type, true, AAPairs).

list_pairs_to_pairs([], []).
list_pairs_to_pairs([[L,R]|Rest], [L:R|RestPairs]) :-
	list_pairs_to_pairs(Rest, RestPairs).


/* get_unique_acros_abbrs(+Input, -AAPairs)

get_unique_acros_abbrs/2 gets the list of unique acronym/abbreviation pairs
of Input using get_unique_acros_abbrs_aux/2.  The elements of AAPairs are 
of the form <AA>:<type> where <AA> is an acronym, abbreviation or expansion
and <type> is either 'a' (acronym/abbreviation) or 'e' (expansion).  */

db_get_unique_acro_abbr(Word, AAPairs) :-
	( Word == [] ->
	  AAPairs = []
	; lower(Word, LCWord),
	  ensure_atom(LCWord, LCWordAtom),
	  db_get_unique_acro_abbr_aux(LCWordAtom, AAPairs)
	),
	!.
db_get_unique_acro_abbr(Word, []) :-
	format('~NERROR: db_access: get_unique_acros_abbrs failed for ~p~n', [Word]).

% Result must be in a list for subsequent processing
db_get_unique_acro_abbr_aux(Word, [Expansion:Type]) :-
	% form_simple_query("expansion, type", "nlsaau", "word", QueryString, Query),
	% run_query(Query, AAListPairs, 1),
	% list_pairs_to_pairs(AAListPairs, AAPairs).	
	% This call always returns a single element, so db_fetch is sufficient.
	skr_db_fetch_data(nls_aau(Word,Expansion,Type,_)).

/* db_get_synonyms(+Input, -Synonyms)
   db_get_synonyms(+Input, +Category, -Synonyms)

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
	format('~NERROR: db_access: db_get_synonyms/2 failed for ~p.~n', [Word]).

db_get_synonyms_aux(WordAtom, Synonyms) :-
	% This call can return multiple values, so must be a db_findall.
	skr_db_findall_data(syns(WordAtom,_WordCategory,Synonym,SynonymCategory),
			    Synonym-SynonymCategory, true, Synonyms0),
	% form_simple_query("syn, scat", "syns", "word", QueryString, Query),
	% run_query(Query, Synonyms0, 1),
	normalize_synonyms(Synonyms0, Synonyms1),
	remove_dups(Synonyms1, Synonyms).

remove_input([], _, []).
remove_input([Input-_|Rest], Input, ModifiedRest) :-
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
	format('~NERROR: db_access: db_get_synonyms/3 failed for ~w/~w.~n', [Word, WordCategory]).

db_get_synonyms_aux(WordAtom, WordCategoryAtom, Synonyms) :-
	% This call can return multiple values, so must be a db_findall.
	skr_db_findall_data(syns(WordAtom,WordCategoryAtom,Synonym,SynonymCategory),
			    Synonym-SynonymCategory, true, Synonyms0),
	% form_complex_query("syn, scat", "syns", "word", QueryString, "wcat", CategoryString, Query),
	% run_query(Query, Synonyms0, 1),
	normalize_synonyms(Synonyms0, Synonyms1),
	remove_dups(Synonyms1, Synonyms).

/* normalize_synonyms(+Synonyms, -NormalizedSynonyms)
   normalize_synonym(+Synonym, -NormalizedSynonym)

normalize_synonyms/2 normalizes Synonyms using normalize_synonym/2
by lowercasing, stripping off expressions of the form <n> and stripping
spaces.  */

normalize_synonyms([], []).
normalize_synonyms([Syn-Cat|Rest], [NormalizedSyn-Cat|NormalizedRest]) :-
	normalize_synonym(Syn, NormalizedSyn),
	normalize_synonyms(Rest, NormalizedRest).

normalize_synonym(Synonym, NormalizedSynonym) :-
	lower(Synonym, LCSynonym),
	atom_codes(LCSynonym, LCSynonymString),
	eliminate_multiple_meaning_designator_string(LCSynonymString, CSString0),
	trim_whitespace(CSString0, NormalizedSynonymString),
	atom_codes(NormalizedSynonym, NormalizedSynonymString).


/* db_get_meta_mesh_tc(+Input, -MH, -TreeCodes)

db_get_meta_mesh_tc/3 gets the MeSH heading and list of MeSH tree codes for Input.
The elements of TreeCodes are either atoms or strings depending on Input.
Only MeSH terminology WITH treecodes will be found; thus subheadings and
check tags will produce nothing.  */

db_get_meta_mesh_tc(Meta, MH, TreeCodes) :-
	db_get_meta_mesh_tc_2(Meta, MH:TreeCodes).

db_get_meta_mesh_tc_2(Meta, MH:TreeCodes) :-
	( Meta == [] ->
	  MH = '',
	  TreeCodes = []
	; ensure_atom(Meta, MetaAtom),
	  db_get_meta_mesh_tc_aux(MetaAtom, MH, TreeCodes)
	),
	!.
db_get_meta_mesh_tc_2(Meta, '':[]) :-
	format('~NERROR: db_access: db_get_meta_mesh_tc failed for ~p.~n', [Meta]).

db_get_meta_mesh_tc_aux(MetaAtom, MH, TreeCodes) :-
	% This call can return multiple values, so must be a db_findall.
	skr_db_findall_data(meta_mesh_tc_opt(MetaAtom, Mesh, TC), Mesh-TC, true, ListPairs),
	% form_simple_query("mesh, tc", "metameshtc", "meta", QueryString, Query),
	% run_query(Query, ListPairs, 1),
	( ListPairs = [MH0-_|_] ->
	    ( MH0 == 'X' ->
	      MH = MetaAtom
	    ; MH = MH0
	    )
	; MH = ''
	),
	extract_right_of_list_pair(ListPairs, TreeCodes0),
	sort(TreeCodes0, TreeCodes).

extract_right_of_list_pair([], []).
extract_right_of_list_pair([_L-R|Rest], [R|ExtractedRest]) :-
	extract_right_of_list_pair(Rest, ExtractedRest).


/* db_get_mesh_tc_strict(+Input, -TreeCodes)

db_get_mesh_tc_strict/2 gets the list of MeSH tree codes for Input.
The elements of TreeCodes are either atoms or strings depending on Input.
Only MeSH terminology WITH treecodes will be found; thus subheadings and
check tags will produce nothing.  */

db_get_mesh_tc_strict(Mesh, TreeCodes) :-
	( Mesh == [] ->
	  TreeCodes = []
	; ensure_atom(Mesh, MeshAtom),
	  db_get_mesh_tc_strict_aux(MeshAtom, TreeCodes)
	),
	!.
db_get_mesh_tc_strict(Mesh, []) :-
	format('~NERROR: db_access: db_get_mesh_tc_strict failed for ~p.~n', [Mesh]).

db_get_mesh_tc_strict_aux(MeshAtom, TreeCodes) :-
	% This call can return multiple values, so must be a db_findall.
	skr_db_findall_data(mesh_tc_strict(MeshAtom, TC), TC, true, TreeCodes).
	% form_simple_query("tc", "meshtcstrict", "mesh", QueryString, Query),
	% run_query(Query, TreeCodes0, 1),
	% append(TreeCodes0, TreeCodes).


/* db_get_mesh_tc_relaxed(+Input, -TreeCodes)

db_get_mesh_tc_relaxed/2 gets the list of MeSH tree codes for Input.
The elements of TreeCodes are either atoms or strings depending on Input.
Only MeSH terminology WITH treecodes will be found; thus subheadings and
check tags will produce nothing.  */

db_get_mesh_tc_relaxed(Mesh, TreeCodes) :-
	( Mesh == [] ->
	  TreeCodes = []
	; ensure_atom(Mesh, MeshAtom),
	  db_get_mesh_tc_relaxed_aux(MeshAtom, TreeCodes)
	),
	!.
db_get_mesh_tc_relaxed(Mesh, []) :-
	format('~NERROR: db_access: db_get_mesh_tc_relaxed failed for ~p.~n', [Mesh]).

db_get_mesh_tc_relaxed_aux(MeshAtom, TreeCodes) :-
	% This call can return multiple values, so must be a db_findall.
	skr_db_findall_data(mesh_tc_relaxed(MeshAtom, TC), TC, true, TreeCodes).
	% form_simple_query("tc", "meshtcrelaxed", "mesh", QueryString, Query),
	% run_query(Query, TreeCodes0, 1),
	% append(TreeCodes0, TreeCodes).

/* db_get_mesh_mh(+Input, -MH)

db_get_mesh_mh/2 gets the MeSH heading for Input, if any. All MeSH terms with a
heading should succeed. Everything else fails. */

db_get_mesh_mh(Mesh, MH) :-
	( Mesh == [] ->
	  MH = []
	; ensure_atom(Mesh, MeshAtom),
        db_get_mesh_mh_aux(MeshAtom, MH)
	),
	!.
db_get_mesh_mh(Mesh, []) :-
	format('~NERROR: db_access: db_get_mesh_mh failed for ~p.~n', [Mesh]).

db_get_mesh_mh_aux(MeshAtom, MH) :-
	% This call does not return multiple values, but I don't know that it can't,
	% so it's a db_findall.
	skr_db_findall_data(mesh_mh_opt(MeshAtom, MH), MH, true, [Result]),
	% form_simple_query("mh", "meshmh", "mesh", QueryString, Query),
	% run_query(Query, [[Result]], 1),
	( Result == 'X' ->
	  MH = MeshAtom
	; MH = Result
	).

/* db_get_meta_mesh(+Mesh, -MH)

db_get_meta_mesh/2 gets the MeSH heading for Mesh. It fails if there is no MeSH
for Mesh. Only MeSH terminology with (pseudo)treecodes will succeed. Thus
subheadings will fail. */

db_get_meta_mesh(Mesh, MH) :-
	( Mesh == [] ->
	  MH = []
	; ensure_atom(Mesh, MeshAtom),
	  db_get_meta_mesh_aux(MeshAtom, MH)
	),
	!.
db_get_meta_mesh(Mesh, []) :-
	format('~NERROR: db_access: db_get_meta_mesh failed for ~p.~n', [Mesh]).

db_get_meta_mesh_aux(MeshAtom, MH) :-
	% This call does not return multiple values, but I don't know that it can't,
	% so it's a db_findall.
	skr_db_findall_data(meta_mesh_opt(MeshAtom,MH), MH, true, [Result]),
	% form_simple_query("mesh", "metamesh", "meta", QueryString, Query),
	% run_query(Query, [[Result]], 1),
	( Result == 'X' ->
	  MH = MeshAtom
	; MH = Result
	).

/* db_get_mwi_word_data(+Table, +Concept, +DebugFlags, -Results)

db_get_mwi_word_data/4 gets word data (Results) from Table for Concept.
Results is a list of terms of the form
     usc(NMStr,String,Concept)
where NMStr is the normalized form of String.
*/

db_get_mwi_word_data(Table, Concept, DebugFlags, Results) :-
	( Concept == [] ->
	  Results = []
	; ensure_atom(Concept, ConceptAtom),
	  ensure_atom(Table, TableAtom),
	  debug_db_get_mwi_data_1(DebugFlags),
	  db_get_mwi_word_data_aux(TableAtom, ConceptAtom, DebugFlags, RawResults),
	  debug_db_get_mwi_data_2(DebugFlags),
	  form_uscs(RawResults, 0, Results),
	  debug_db_get_mwi_data_3(DebugFlags, Results)
	  ),
	!.
db_get_mwi_word_data(Table, Concept, _DebugFlags, []) :-
       format('~NERROR: db_access: db_get_mwi_word_data failed for ~p on table ~p.~n',
	      [Concept,Table]).

db_get_mwi_word_data_aux(Table, Concept, DebugFlags, RawResults) :-
	% Table is one of all_words, first_words, first_wordsb,
	% first_words_of_one, first_words_of_two
	TableQueryTerm =.. [Table,Concept,SUI,CUI],
	% concatenate_items_to_string(["suistrings.nmstr, suistrings.str, ",
        %                             "cuiconcept.concept"],
        %                            Fields),
        % concatenate_items_to_string([Table,
        %                              ", suistrings, cuiconcept"],
        %                             Tables),
        % concatenate_items_to_string([Table,".word"], Field),
        % form_simple_query(Fields, Tables, Field, QueryString, Query0),
        % concatenate_items_to_string([" and ",
        %                              Table, ".sui = suistrings.sui and ",
        %                              Table, ".cui = cuiconcept.cui"],
        %                             QueryTail),
        % concatenate_items_to_atom([Query0,QueryTail], Query),
        % run_query(Query, RawResults, 1),
	% This call can return multiple values, so must be a db_findall.
	skr_db_findall_data(TableQueryTerm, SUI-CUI, true, SuiCuiPairs),
	debug_db_get_mwi_data_aux_1(DebugFlags, SuiCuiPairs),
	generate_raw_results(SuiCuiPairs, RawResults),
	debug_db_get_mwi_data_aux_2(DebugFlags, RawResults),
	!.
db_get_mwi_word_data_aux(Table, QueryString, _DebugFlags, _RawResults) :-
	format('~NERROR: db_access: db_get_mwi_word_data_aux failed for ~p on table ~p.~n',
	       [QueryString,Table]).

generate_raw_results([], []).
generate_raw_results([H|T], RawResults) :-
	SuiCuiPairs = [H|T],
	create_SUI_and_CUI_lists(SuiCuiPairs, SUIList, CUIList),
	skr_db_open(sui_nmstr_str, SUIStringsDBRef),
	get_all_SUI_strings(SUIList, SUIStringsDBRef, SUIData),
	skr_db_close(sui_nmstr_str, SUIStringsDBRef),
	skr_db_open(cui_concept, CUIConceptDBRef),
	get_all_CUI_concepts(CUIList, CUIConceptDBRef, CUIData),
	skr_db_close(cui_concept, CUIConceptDBRef),
	replace_SUIs_and_CUIs(SuiCuiPairs, SUIData, CUIData, RawResults).
	
% Suppose QueryString (the word being looked up) is "heart" and Table is first_wordsb.
% The monstrous code below creates the query
% 'select suistrings.nmstr, suistrings.str, cuiconcept.concept
%         from first_wordsb, suistrings, cuiconcept
%        where first_wordsb.word=''heart''
%          and first_wordsb.sui = suistrings.sui
%          and first_wordsb.cui = cuiconcept.cui'

get_all_SUI_strings([], _SUIStringsDBRef, []).
get_all_SUI_strings([SUI|RestSUIs], SUIStringsDBRef, [SUI-NMStr-Str|RestSUIData]) :-
	SUIStringsQueryTerm = sui_nmstr_str(SUI, NMStr, Str),
	% This call always returns a single element, so db_fetch is sufficient.
	skr_db_fetch(SUIStringsDBRef, sui_nmstr_str, SUIStringsQueryTerm, _TermRef),
	get_all_SUI_strings(RestSUIs, SUIStringsDBRef, RestSUIData).


get_all_CUI_concepts([], _CUIConceptDBRef, []).
get_all_CUI_concepts([CUI|RestCUIs], CUIConceptDBRef, [CUI-Concept|RestCUIData]) :-
	% This is an efficiency hack:
	% The first row in the cui_concept table is simply
	% C.......|X
	% so don't even go into the DB for it
	get_one_CUI_concept(CUI, CUIConceptDBRef, Concept),
	get_all_CUI_concepts(RestCUIs, CUIConceptDBRef, RestCUIData).

get_one_CUI_concept(CUI, CUIConceptDBRef, Concept) :-
	( CUI == 'C.......' ->
	  Concept = 'X'
	; CUIConceptQueryTerm = cui_concept(CUI, Concept),
	  % This call always returns a single element, so db_fetch is sufficient.
	  skr_db_fetch(CUIConceptDBRef, cui_concept, CUIConceptQueryTerm, _TermRef)
	).


replace_SUIs_and_CUIs([], _SUIData, _CUIData, []).
replace_SUIs_and_CUIs([SUI-CUI|RestSUIsCUIs],
		      [SUI-NMStr-Str|RestSUIData],
		      [CUI-Concept|RestCUIData],
		      [NMStr-Str-Concept|RestRawData]) :-
	replace_SUIs_and_CUIs(RestSUIsCUIs, RestSUIData, RestCUIData, RestRawData).


create_SUI_and_CUI_lists([], [], []).
create_SUI_and_CUI_lists([SUI-CUI|RestPairs], [SUI|RestSUIs], [CUI|RestCUIs]) :-
	create_SUI_and_CUI_lists(RestPairs, RestSUIs, RestCUIs).

form_uscs([], _N, []) :- !.
form_uscs([First|Rest], N, [usc(Nmstr,Str,Concept)|ModifiedRest]) :-
	First = Nmstr-Str-Concept0,
	!,
	( Concept0 == 'X' ->
	  Concept = Str
	; Concept = Concept0
	),
	NewN is N + 1,
	form_uscs(Rest, NewN, ModifiedRest).
form_uscs([First|_], N, _) :-
	NewN is N + 1,
	format('~NERROR: db_access: form_uscs failed on item ~d = ~p.~n', [NewN,First]),
	ttyflush,
	!,
	fail.
form_uscs(X, N, _) :-
	format('~NERROR: db_access: form_uscs failed after item ~d; the non-list is ~p.~n',
	       [N,X]),
	ttyflush,
	!,
	fail.

/* db_get_mwi_word_count(+Table, +Input, -Count)

db_get_mwi_word_count/3 gets the word Count from Table for Input.
It fails if Input is not in Table. Note that since counts of 1 are not stored
in the table, failure indicates a count of either 0 or 1. */

db_get_mwi_word_count(Table, Word, Count) :-
	Word \== [],
	ensure_atom(Table, TableAtom),
	ensure_atom(Word, WordAtom),
	db_get_mwi_word_count_aux(TableAtom, WordAtom, Count),
	!.
% This predicate must be allowed to fail gracefully
% db_get_mwi_word_count(Table, Word, 0) :-
%        format('~NERROR: db_access: db_get_mwi_word_count failed for ~p on table ~p.~n',
% 	      [Word,Table]).

db_get_mwi_word_count_aux(TableAtom, WordAtom, WordCount) :-
	TableQueryTerm =.. [TableAtom,WordAtom,WordCount],
	% This call always returns a single element, so db_fetch is sufficient.
	skr_db_fetch_data(TableQueryTerm).
	% form_simple_query("wcount", Table, "word", QueryString, Query),
	% run_query(Query, RawResults, 1).


/* 
   db_get_variants(+Input, +Category, -Variants)

db_get_variants/3 gets the list of Variants of Input, which can be either
an atom or a string.  The result can depend on the Category, but if the
Category is [], no filtering is done on Category.
The table used ot obtain the variants is determined by db_access_var_table/1. */

db_get_variants(Concept, Category, Variants) :-
	( ensure_atom(Concept, ConceptAtom),
	  db_get_variants_aux(ConceptAtom, Category, Variants)
	),
	!.
db_get_variants(Concept, Category, []) :-
	format('~NERROR: db_access: db_get_variants/3 failed for ~p (~p).~n', [Concept,Category]).

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
	ensure_atom(Concept, ConceptAtom),
	db_access_var_table(VarTable),
	ensure_atom(VarTable, VarTableAtom),
	ensure_atom(Category, CategoryAtom),
	% QueryTerm will become a term such as
	% varsan(ConceptAtom,CategoryAtom,Var,Vcat,Dist,Hist,Roots)
	QueryTerm =.. [VarTableAtom,ConceptAtom,CategoryAtom,Var,Vcat,Dist,Hist,Roots],
	% This call can return multiple values, so must be a db_findall.
	skr_db_findall_data(QueryTerm, v(Var,Vcat,Dist,Hist,Roots), true, Variants0),
	% form_complex_query("var, vcat, dist, hist, roots",
	% 		   VarTable, "word", QueryString, "wcat", Category, Query),
	% format(user_output, '~w~n', [Query]),
	% run_query(Query, Variants0, 1),
	% format('~nget_variants_aux:~nquery = ~p~nresult = ~p~n',[Query,Variants0]),
	sort(Variants0, Variants1),
	convert_to_variant_terms(Variants1, Variants).

convert_to_variant_terms([], []).
% temp  to handle null values until they're removed
%convert_to_variant_terms([[]|Rest],ConvertedRest) :-
%    !,
%    convert_to_variant_terms(Rest,ConvertedRest).
convert_to_variant_terms([v(Var,VCat0,DistAtom,Hist0,Roots)|Rest],
                         [v(Var,VCat, DistInteger,Hist, Roots,_)|ConvertedRest]) :-
	atom_codes(DistAtom, DistCodes),
	number_codes(DistInteger, DistCodes),
	convert_variant_category(VCat0, VCat),
	atom_codes(Hist0,Hist),
	convert_to_variant_terms(Rest, ConvertedRest).

convert_variant_category(VCat0, VCat) :-
	( VCat0 == none ->
	  VCat = []
	; VCat0 == [] ->
	  VCat = []
	; VCat = [VCat0]
	).

/* form_simple_query(+Fields, +Tables, +Field, +Value, -Query)
   form_complex_query(+Fields, +Tables, +Field1, +Value1, +Field2, +Value2, -Query)

form_simple_query/5 constructs the atom Query of the form
     select <Fields> from <Tables> where <Field>="Value"
where Fields, Tables, Field, and Value can be atoms or strings.
form_complex_query/7 constructs the atom Query of the form
     select <Fields> from <Tables> where <Field1>="Value1" and <Field2>="Value2"
where Fields, Tables, Field1, Value1, Field2, and Value2 can be atoms or strings.
*/

% form_simple_query(Fields, Tables, Field, Value0, Query) :-
% 	form_complex_query(Fields, Tables, Field, Value0, [], [], Query).

% form_complex_query(Fields, Tables, Field1, Value1_0, Field2, Value2_0, Query) :-
% 	convert_to_string(Value1_0, Value1_1),
% 	double_quotes(Value1_1, Value1),
% 	form_rest_query(Value2_0, Field2, RestQuery),
% 	concatenate_items_to_atom(["select ",Fields," from ",Tables,
% 				   " where ",Field1,"='",Value1 | RestQuery],
% 				  Query).

% form_rest_query(Value2_0, Field2, RestQuery) :-
% 	% in get_variants/3, Value2_0 is the Category;
% 	% if the Category is [], ignore it, and
% 	% don't instantiate the rest of the query,
% 	% other than closing the final quote!
% 	( Value2_0 == [] ->
% 	  RestQuery = ["'"]
% 	; convert_to_string(Value2_0, Value2_1),
% 	  double_quotes(Value2_1, Value2),
% 	  RestQuery = [ "' and ",Field2,"='",Value2,"'"]
% 	).

convert_to_string(AtomOrString, String) :-
	( is_print_string(AtomOrString) ->
	  String = AtomOrString
	; atom_codes(AtomOrString, String)
	).


get_db_access_version(Version) :-
	( control_value(mm_data_version,Version) ->
	  true
        ; default_version(Version)
        ).

get_db_access_year(Year) :-
	( control_value(mm_data_year,Year) ->
	  true
	; default_year(Year)
	).

get_db_access_model(Model) :-
	% This is more complex than it need be.
	( control_option(strict_model) ->
	  Model = strict
	; control_option(relaxed_model) ->
	  warn_about_model('relaxed model'),
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

debug_db_get_mwi_data_aux_1(DebugFlags, SuiCuiPairs) :-
	( memberchk(7, DebugFlags) ->
	  format('db_get_mwi_word_data_aux: SuiCuiPairs = ~p~n', [SuiCuiPairs]),
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

