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

% File:	    metamap_utilities.pl
% Module:   MetaMap
% Author:   Lan
% Purpose:  MetaMap utility predicates


:- module(metamap_utilities, [
	% must be exported for mm_print
	build_concept_name_1/4,
	candidate_term/16,
	dump_aphrase_mappings/2,
	dump_evaluations_indented/6,
	% must be exported for mm_print
	dump_evaluations_indented/7,
	dump_variants_labelled/2,
	extract_nonexcluded_sources/3,
	extract_relevant_sources/3,
	extract_name_in_source/2,
	extract_unique_sources/2,
	num_dump_evaluations_indented/6,
	positions_overlap/2,
	wgvcs/1,
	wl/1,
	write_avl_list/1,
	write_list_indented/1
    ]).

:- use_module(skr(skr_umls_info), [
	convert_to_root_sources/2
    ]).

:- use_module(skr(skr_utilities), [
	ensure_atom/2,
	fatal_error/2,
	get_candidate_feature/3,
	get_all_candidate_features/3
    ]).

:- use_module(skr_lib(semtype_translation_2012AB), [
	expand_semtypes/2
    ]).

:- use_module(skr_lib(nls_system), [
	control_option/1,
	control_value/2
    ]).

:- use_module(skr_lib(nls_strings), [
	concatenate_items_to_atom/2,
	trim_whitespace/2
    ]).

:- use_module(skr_lib(sicstus_utils), [
	concat_atom/2
    ]).

:- use_module(library(lists), [
	append/2,
	is_list/1,
	rev/2
    ]).


/* ************************************************************************
   ************************************************************************
   ************************************************************************
                            MetaMap Utility Predicates
   ************************************************************************
   ************************************************************************
   ************************************************************************ */


/* positions_overlap(+Position1, +Position2)

positions_overlap/2
xxx
*/

positions_overlap([Begin1,End1], [Begin2,End2]) :-
	( Begin1 =:= 0,
	  End1 =:= -1 ->
	  fail
	; Begin2 =:= 0,
	  End2 =:= -1 ->
	  fail
	; Begin1 =< Begin2,
	  End1 >= Begin2 ->
	  true
	; Begin2 < Begin1,
	  End2 >= Begin1
	).

/* dump_aphrase_mappings(+APhrases, +Label)
   dump_aphrase_mappings_aux(+APhrases, +Label)
   dump_mapping(+Mapping, +Label, +Value)

dump_aphrase_mappings/2
dump_aphrase_mappings_aux/2
dump_mapping/2
xxx
*/

dump_aphrase_mappings([], Label) :-
	format('Meta ~a: <none>~n', [Label]).
dump_aphrase_mappings([H|T], Label) :-
	APhrases = [H|T],
	( control_option(number_the_mappings) ->
	  Counter is 1
	; Counter is 0
	),
	dump_aphrase_mappings_aux(APhrases, Label, Counter).

dump_aphrase_mappings_aux([], _Label, _Counter).
dump_aphrase_mappings_aux([ap(NegValue,_,_,Mapping)|Rest], Label, Counter) :-
	Score is -NegValue,
	get_display_and_increment_counter(Counter, CounterDisplay, NextCounter),
	dump_mapping(Mapping, Label, Score, CounterDisplay),
	dump_aphrase_mappings_aux(Rest, Label, NextCounter).

get_display_and_increment_counter(Counter, CounterDisplay, NextCounter) :-
	( Counter =:= 0 ->
	  CounterDisplay = '',
	  NextCounter is 0
	; ensure_atom(Counter, CounterAtom),
	  % atom_concat/3 is a SICStus Prolog built-in predicate
	  atom_concat(CounterAtom, '. ', CounterDisplay),
	  NextCounter is Counter + 1
	).	  

dump_mapping([], Label, _Score, _DisplayCounter) :-
	format('Meta ~a: <none>~n', [Label]).
dump_mapping([H|T], Label, Score, DisplayCounter) :-
	Mapping = [H|T],
	format('~wMeta ~a (~d):~n', [DisplayCounter,Label,Score]),
	dump_evaluations(Mapping).

dump_evaluations([]).
dump_evaluations([FirstEV|RestEVs]) :-
	RelatedEvaluations = [],
	dump_one_evaluation(FirstEV:RelatedEvaluations),
	dump_evaluations(RestEVs).

% MetaMap now has status symbols :-)

choose_status_symbol(_Status, _Negated, Symbol) :-
	control_option(silent),
	!,
	Symbol = ''.
%  0 means the candidate was kept, but possibly negated
choose_status_symbol(0, Negated, Symbol) :-
	( Negated == 1 ->
	  Symbol = 'N'
	; Symbol = ' '
	).
%  1 means the candidate was Excluded
choose_status_symbol(1, _Negated, 'E').
%  2 means the candidate was Pruned
choose_status_symbol(2, _Negated, 'P').

dump_one_evaluation(Candidate:RelatedCandidates) :-
	get_all_candidate_features([negvalue,cui,metaterm,metaconcept,
				    semtypes,sources,status,negated],
				   Candidate,
				   [NegValue,CUI,MetaTerm,MetaConcept,
				    SemTypes0,SourceInfo,Status,Negated]),
	Value is -NegValue,
	% build_concept_name(CUI,MetaConcept,ConceptName),
	build_concept_name_1(MetaConcept, CUI, SourceInfo, ConceptName),
	choose_status_symbol(Status, Negated, StatusSymbol),
	display_concept_info(MetaTerm, Value, CUI, ConceptName, StatusSymbol),
	conditionally_expand_semtypes(SemTypes0),
	format('~n', []),
	dump_related_evaluations(RelatedCandidates).

display_concept_info(MetaTerm, Value, CUI, ConceptName, StatusSymbol) :-
	( control_option(show_cuis) ->
	  DisplayCUI = CUI,
	  Colon = ':'
	; DisplayCUI = '',
	  Colon = ''
	),
	format('~t~d ~w~8| ~p~w~p (~p)', [Value,StatusSymbol,DisplayCUI,Colon,MetaTerm,ConceptName]).

conditionally_expand_semtypes(SemTypes0) :-
    	( \+ control_option(hide_semantic_types) ->
	  expand_semtypes(SemTypes0, SemTypes),
	  format(' ~p', [SemTypes])
	; true
	).

/* build_concept_name_1(+MetaConcept, +CUI, +Sources, -ConceptName) :-

build_concept_name/4 constructs ConceptName from MetaConcept by optionally
appending slist, the list of sources for ConceptName. slist is appended when
-G (--sources) is active, and it is affected by options
-R (--restrict_to_sources) and -e (--exlude_sources). */

build_concept_name_1(MetaConcept, _CUI, _Sources, MetaConcept) :-
	\+control_option(sources),
	!.
build_concept_name_1(MetaConcept, _CUI, Sources, ConceptName) :-
	( control_option(restrict_to_sources) ->
	  control_value(restrict_to_sources, Sources),
	  convert_to_root_sources(Sources, RootSources),
	  extract_relevant_sources_1(Sources0, RootSources, Sources)
	; control_option(exclude_sources) ->
	  control_value(exclude_sources, Sources),
	  convert_to_root_sources(Sources, RootSources),
	  extract_nonexcluded_sources_1(Sources0, RootSources, Sources)
	; Sources = Sources0
	),
	build_concept_name_1_aux(Sources, MetaConcept, ConceptName),
	!.
% should never occur
build_concept_name_1(MetaConcept, CUI, _Sources, _ConceptName) :-
	fatal_error('Unable to build concept name for ~q/~q~n', [CUI,MetaConcept]).

build_concept_name_1_aux(Sources, MetaConcept, ConceptName) :-
	Sources = [FirstSource|RestSources],
	build_list(RestSources, [], RestSourceList),
	append([[MetaConcept,' {',FirstSource],RestSourceList,['}']], ItemList),
	concatenate_items_to_atom(ItemList, ConceptName).

extract_unique_sources(SourceInfo, UniqueSources) :-
	(  foreach([_I, _Str, SRC, _TTY], SourceInfo),
	   foreach(SRC, Sources)
	do true
	),
	sort(Sources, UniqueSources).

build_list([], ListIn, ListOut) :-
	rev(ListIn, ListOut).
build_list([First|Rest], ListIn, ListOut) :-
	build_list(Rest,[First,', '|ListIn], ListOut).


dump_evaluations_indented(Candidates, TotalCandidateCount,
			  ExcludedCandidateCount, PrunedCandidateCount,
			  RemainingCandidateCount, Label, OutputStream) :-
	current_output(CurrentOutput),
	set_output(OutputStream),
	dump_evaluations_indented(Candidates, TotalCandidateCount,
			  ExcludedCandidateCount, PrunedCandidateCount,
			  RemainingCandidateCount, Label),
	!,
	set_output(CurrentOutput).




% This should never be called,
% because generate_candidates_output/6 tests for Evaluations3 \== []
dump_evaluations_indented([], _TotalCandidateCount,
			  _ExcludedCandidateCount, _PrunedCandidateCount,
			  _RemainingCandidateCount, Label) :-
	format('Meta ~a (0): <none>~n', [Label]).
dump_evaluations_indented([H|T], TotalCandidateCount,
			  ExcludedCandidateCount, PrunedCandidateCount,
			  RemainingCandidateCount, Label) :-
	Candidates = [H|T],
	( control_option(silent) ->
	  format('Meta ~a (~d):~n', [Label,TotalCandidateCount])
	; format('Meta ~a (Total=~d; Excluded=~d; Pruned=~d; Remaining=~d)~n',
		 [Label,TotalCandidateCount,
		  ExcludedCandidateCount,PrunedCandidateCount,
		  RemainingCandidateCount])
	),
	construct_related_evaluations(Candidates, CandidatePairs),
	dump_evaluations_indented_aux(CandidatePairs).

construct_related_evaluations([], []).
construct_related_evaluations([FirstCandidate|RestCandidates],
			      [FirstCandidate:FirstRelated|RestRelated]) :-
	% Look for other ev() terms with the same preferred name
	get_candidate_feature(metaconcept, FirstCandidate, Concept),
	construct_related_evaluations_6(RestCandidates, Concept,
					[], RevFirstRelated, [], RevNewRest),
	rev(RevFirstRelated, FirstRelated),
	rev(RevNewRest, NewRest),
	construct_related_evaluations(NewRest, RestRelated).

construct_related_evaluations_6([], _Concept, RelatedIn, RelatedIn, UnrelatedIn, UnrelatedIn).
construct_related_evaluations_6([FirstCandidate|RestCandidates], Concept,
				RelatedIn, RelatedOut,
				UnrelatedIn, UnrelatedOut) :-
	get_candidate_feature(metaconcept, FirstCandidate, Concept),
	!,
	construct_related_evaluations_6(RestCandidates, Concept,
					[FirstCandidate|RelatedIn], RelatedOut,
					UnrelatedIn, UnrelatedOut).
construct_related_evaluations_6([FirstCandidate|RestCandidates], Concept,
				RelatedIn, RelatedOut, UnrelatedIn, UnrelatedOut) :-
	construct_related_evaluations_6(RestCandidates, Concept,
					RelatedIn, RelatedOut,
					[FirstCandidate|UnrelatedIn], UnrelatedOut).

dump_evaluations_indented_aux([]).
dump_evaluations_indented_aux([FirstEV|RestEVs]) :-
	dump_one_evaluation(FirstEV),
	dump_evaluations_indented_aux(RestEVs).

dump_related_evaluations([]).
dump_related_evaluations([FirstCandidate|RestCandidates]) :-
	get_all_candidate_features([metaterm,status,negated],
				   FirstCandidate,
				   [MetaTerm,Status,Negated]),
	choose_status_symbol(Status, Negated, StatusSymbol),
	format('       ~w   ~p~n', [StatusSymbol,MetaTerm]),
	dump_related_evaluations(RestCandidates).

% This should never be called,
% because generate_candidates_output/6 tests for Evaluations3 \== []
num_dump_evaluations_indented([], _TotalCandidateCount,
			      _ExcludedCandidateCount, _PrunedCandidateCount,
			      _RemainingCandidateCount, Label) :-
	  format('Meta ~a (0): <none>~n', [Label]).
num_dump_evaluations_indented([H|T], TotalCandidateCount,
			  ExcludedCandidateCount, PrunedCandidateCount,
			  RemainingCandidateCount, Label) :-
	Candidates = [H|T],
	( control_option(silent) ->
	  format('Meta ~a (~d):~n', [Label,TotalCandidateCount])
	; format('Meta ~a (Total=~d; Excluded=~d; Pruned=~d; Remaining=~d)~n',
		 [Label,TotalCandidateCount,
		  ExcludedCandidateCount,PrunedCandidateCount,
		  RemainingCandidateCount])
	),
	construct_related_evaluations(Candidates, CandidatePairs),
	num_dump_evaluations_indented_aux(CandidatePairs, 1).

num_dump_evaluations_indented_aux([], _).
num_dump_evaluations_indented_aux([Candidate:RelatedCandidates|Rest], N) :-
	num_dump_one_evaluation_indented(Candidate:RelatedCandidates, N, NextN),
	num_dump_evaluations_indented_aux(Rest, NextN).

num_dump_one_evaluation_indented(Candidate:RelatedCandidates, N, NextN) :-
	get_all_candidate_features([negvalue,cui,metaterm,metaconcept,
				    semtypes,sources,status,negated],
				   Candidate,
				   [NegValue,CUI,MetaTerm,MetaConcept,
				    SemTypes0,SourceInfo,Status,Negated]),
	Value is -NegValue,
	build_concept_name_1(MetaConcept, CUI, SourceInfo, ConceptName),
	choose_status_symbol(Status, Negated, StatusSymbol),
	num_display_concept_info(MetaTerm, N, Value, CUI, ConceptName, StatusSymbol),
	conditionally_expand_semtypes(SemTypes0),
	format('~n',[]),
	N1 is N + 1,
	num_dump_related_evaluations(RelatedCandidates, N1, NextN).

num_display_concept_info(MetaTerm, N, Value, CUI, ConceptName, StatusSymbol) :-
	( control_option(show_cuis) ->
	  DisplayCUI = CUI,
	  Colon = ':'
	; DisplayCUI = '',
	  Colon = ''
	),
	format('~t~d~4|. ~t~d~10| ~t~w~12| ~p~w~p (~p)',
	       [N,Value,StatusSymbol,DisplayCUI,Colon,MetaTerm,ConceptName]).

num_dump_related_evaluations([], NIn, NIn).
num_dump_related_evaluations([FirstCandidate|RestCandidates], NIn, NOut) :-
	get_candidate_feature(metaterm, FirstCandidate, MetaTerm),
	format('~t~d~4|.           ~p~n', [NIn,MetaTerm]),
	NInOut is NIn + 1,
	num_dump_related_evaluations(RestCandidates, NInOut, NOut).

/* dump_variants_labelled(+Label, +Variants)

dump_variants_labelled/2
xxx
*/

dump_variants_labelled(Label,Variants) :-
    length(Variants,NVariants),
    (NVariants=:=0 ->
        format('~a variants (n=0):~n<none>~n',[Label])
    ;   format('~a variants (n=~d):~n',[Label,NVariants]),
        dump_variants(Variants),
        format('~n',[])
    ).

dump_variants([]).
dump_variants([v(Word,Categories,VarLevel,History,_Roots,_NFR)|Rest]) :-
    rev(History,RevHistory),
    (Categories==[] ->
        format('~p{~d=~p}  ',[Word,VarLevel,RevHistory])
    ;   format('~p{~p, ~d=~p}  ',[Word,Categories,VarLevel,RevHistory])
    ),
    dump_variants(Rest).

/* extract_relevant_sources(+SourceInfo, +Sources, -ExtractedSourceInfo)
   extract_nonexcluded_sources(+SourceInfo, +Sources, -ExtractedSourceInfo)

extract_relevant_sources/3 produces the subset ExtractedSourceInfo of
SourceInfo containing a source in Sources (which are root, or non-versioned,
sources).
extract_nonexcluded_sources/3 does the same thing except that it excludes
entries in SourceInfo containing one of Sources. */

extract_relevant_sources([], _, []).
extract_relevant_sources([[I,ConceptNameInSource,Source,TTY]|Rest],
			 Sources,
			 [[I,ConceptNameInSource,Source,TTY]|ExtractedRest]) :-
	memberchk(Source, Sources),
	!,
	extract_relevant_sources(Rest, Sources, ExtractedRest).
extract_relevant_sources([_|Rest], Sources, ExtractedRest) :-
	extract_relevant_sources(Rest, Sources, ExtractedRest).


extract_relevant_sources_1([], _, []).
extract_relevant_sources_1([Source|RestSources],
			   Sources,
			   [Source|ExtractedRest]) :-
	memberchk(Source, Sources),
	!,
	extract_relevant_sources_1(RestSources, Sources, ExtractedRest).
extract_relevant_sources_1([_|Rest], Sources, ExtractedRest) :-
	extract_relevant_sources_1(Rest, Sources, ExtractedRest).

extract_nonexcluded_sources([], _, []).
extract_nonexcluded_sources([[I,ConceptNameInSource,Source,TTY]|Rest],
			    Sources,
			    [[I,ConceptNameInSource,Source,TTY]|ExtractedRest]) :-
	\+ memberchk(Source, Sources),
	!,
	extract_nonexcluded_sources(Rest, Sources, ExtractedRest).
extract_nonexcluded_sources([_|Rest], Sources, ExtractedRest) :-
	extract_nonexcluded_sources(Rest, Sources, ExtractedRest).

extract_nonexcluded_sources_1([] ,_, []).
extract_nonexcluded_sources_1([Source|RestSources],
			      Sources,
			      [Source|ExtractedRest]) :-
	\+ memberchk(Source, Sources),
	!,
	extract_nonexcluded_sources_1(RestSources, Sources, ExtractedRest).
extract_nonexcluded_sources_1([_|Rest], Sources, ExtractedRest) :-
	extract_nonexcluded_sources_1(Rest, Sources, ExtractedRest).


/* extract_name_in_source(+SourceInfo, -ConceptNameInSource)

extract_name_in_source/2 extracts ConceptNameInSource from the first entry in SourceInfo.
SourceInfo should never be empty; but if it is, ConceptNameInSource is set to
'<none>'. */

% 

% should never happen
extract_name_in_source([], '<none>').
extract_name_in_source([[_I,ConceptNameInSource,_Source,_TTY]|_Rest], ConceptNameInSource).

wgvcs([]).
wgvcs([First|Rest]) :-
    wgvc(First),
    wgvcs(Rest).

wgvc(gvc(G,Vs,Cs)) :-
    format('gvc:  ~p~n',[G]),
    wl(Vs),
    wl(Cs).

wl(X) :-
    var(X),
    !,
    format('  <none>~n',[]).
wl([]).
wl([First|Rest]) :-
    format('  ~p~n',[First]),
    wl(Rest).


write_avl_list([]).
write_avl_list([Key-Value|Rest]) :-
    format('~n~p~n',[Key]),
    (   is_vinfo_list(Value) ->
        write_vinfo_list(Value)
    ;   is_list(Value) ->
        wl(Value)
    ;   format('  ~p~n',[Value])
    ),
    write_avl_list(Rest).

is_vinfo_list([First|_]) :-
    functor(First,vinfo,5),
    !.

write_vinfo_list([]).
write_vinfo_list([vinfo(Generator,Position,InvolvesHead,Variant,Words)|Rest]) :-
    format('  vinfo:~p~n',[Generator]),
    format('        ~p,~p~n',[Position,InvolvesHead]),
    format('        ~p~n',[Variant]),
    format('        ~p~n',[Words]),
    write_vinfo_list(Rest).

write_list_indented([]).
write_list_indented([First|Rest]) :-
    format('  ~p~n',[First]),
    write_list_indented(Rest).

% Create candidate term or extract features
candidate_term(NegValue, CUI, MetaTerm, MetaConcept, MetaWords, SemTypes,
	       MatchMap, LSComponents, TargetLSComponent, InvolvesHead,
	       IsOvermatch, UniqueSources, PosInfo, Status, Negated, Evaluation) :-
	( Evaluation = ev(NegValue,CUI,MetaTerm,MetaConcept,MetaWords,SemTypes,
			  MatchMap,LSComponents,TargetLSComponent,
			  InvolvesHead,IsOvermatch,UniqueSources,PosInfo,Status,Negated) ->
	  true
	  % This is simply for mm_print, which sees the printed representation of candidates,
	  % which does NOT include the LSComponents and TargetLSComponents fields.
	; Evaluation = ev(NegValue,CUI,MetaTerm,MetaConcept,MetaWords,SemTypes,
			  MatchMap,InvolvesHead,IsOvermatch,UniqueSources,PosInfo,Status,Negated)
	).
