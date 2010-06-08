
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


:- module(metamap_utilities,[
	% must be exported for mm_print
	build_concept_name_1/4,
	dump_aphrase_mappings/2,
	dump_evaluations_indented/2,
	dump_variants_labelled/2,
	extract_nonexcluded_sources/3,
	extract_relevant_sources/3,
	extract_source_name/2,
	extract_unique_sources/3,
	num_dump_evaluations_indented/2,
	positions_overlap/2,
	wgvcs/1,
	wl/1,
	write_avl_list/1,
	write_list_indented/1
    ]).

:- use_module(skr_db(db_access),[
	db_get_cui_sources/2
    ]).

:- use_module(skr(skr_umls_info10),[
	convert_to_root_sources/2
    ]).

:- use_module(skr_lib(semtype_translation10),[
	expand_semtypes/2
    ]).

:- use_module(skr_lib(nls_system),[
	control_option/1,
	control_value/2
    ]).

:- use_module(skr_lib(nls_strings),[
	concatenate_items_to_atom/2,
	trim_whitespace/2
    ]).

:- use_module(library(lists),[
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
	format('Meta ~a: <none>~n', [Label]),
	!.
dump_aphrase_mappings([H|T], Label) :-
	APhrases = [H|T],
	dump_aphrase_mappings_aux(APhrases, Label).

dump_aphrase_mappings_aux([], _Label).
dump_aphrase_mappings_aux([ap(NegValue,_,_,Mapping)|Rest], Label) :-
	Value is -NegValue,
	dump_mapping(Mapping, Label, Value),
	dump_aphrase_mappings_aux(Rest, Label).

dump_mapping([], Label, _) :-
	format('Meta ~a: <none>~n', [Label]).
dump_mapping([H|T], Label, Value) :-
	Mapping = [H|T],
	format('Meta ~a (~d):~n', [Label,Value]),
	dump_evaluations(Mapping).

dump_evaluations([]).
dump_evaluations([FirstEV|RestEVs]) :-
	dump_one_evaluation(FirstEV),
	dump_evaluations(RestEVs).

dump_one_evaluation(ev(NegValue,CUI,MetaTerm,MetaConcept,_MetaWords,SemTypes0,
                     _MatchMap,_InvolvesHead,_IsOvermatch,SourceInfo,_PosInfo)) :-
	Value is -NegValue,
	% build_concept_name(CUI, MetaConcept, ConceptName),
	build_concept_name_1(MetaConcept, CUI, SourceInfo, ConceptName),
	display_concept_info(MetaTerm, MetaConcept, Value, CUI, ConceptName),
	conditionally_expand_semtypes(SemTypes0),
	conditionally_get_cui_sources(CUI),
	format('~n', []).

display_concept_info(MetaTerm, MetaConcept, Value, CUI, ConceptName) :-
	control_option(show_cuis),
	!,
	( preferred_name_only(MetaTerm, MetaConcept) ->
	  format('~t~d~6| ~p:~p', [Value,CUI,ConceptName])
	; format('~t~d~6| ~p:~p (~p)', [Value,CUI,MetaTerm,ConceptName])
	).

display_concept_info(MetaTerm, MetaConcept, Value, _CUI, ConceptName) :-
	( preferred_name_only(MetaTerm, MetaConcept) ->
	  format('~t~d~6| ~p', [Value,ConceptName])
	; format('~t~d~6| ~p (~p)', [Value,MetaTerm,ConceptName])
	).

preferred_name_only(MetaTerm, MetaConcept) :-
	( MetaTerm == MetaConcept ->
	  true
	; control_option(show_preferred_names_only)
	).

conditionally_expand_semtypes(SemTypes0) :-
    	( \+ control_option(hide_semantic_types) ->
	  expand_semtypes(SemTypes0, SemTypes),
	  format(' ~p', [SemTypes])
	; true
	).
	
conditionally_get_cui_sources(CUI) :-
	( control_option(preferred_name_sources) ->
	  db_get_cui_sources(CUI, Sources),
	  format(' ~p',[Sources])
	; true
	).

/* build_concept_name_1(+MetaConcept, +CUI, +SourceInfo, -ConceptName) :-

build_concept_name/4 constructs ConceptName from MetaConcept by optionally
appending slist, the list of sources for ConceptName. slist is appended when
-G (--sources) is active, and it is affected by options
-R (--restrict_to_sources) and -e (--exlude_sources). */

build_concept_name_1(MetaConcept, _CUI, _SourceInfo, MetaConcept) :-
	\+control_option(sources),
	!.
build_concept_name_1(MetaConcept, _CUI, SourceInfo0, ConceptName) :-
	( control_option(restrict_to_sources) ->
	  control_value(restrict_to_sources, Sources),
	  convert_to_root_sources(Sources, RootSources),
	  extract_relevant_sources_1(SourceInfo0, RootSources, SourceInfo)
	; control_option(exclude_sources) ->
	  control_value(exclude_sources, Sources),
	  convert_to_root_sources(Sources, RootSources),
	  extract_nonexcluded_sources_1(SourceInfo0, RootSources, SourceInfo)
	; SourceInfo = SourceInfo0
	),
	build_concept_name_1_aux(SourceInfo, MetaConcept, ConceptName),
	!.
% should never occur
build_concept_name_1(MetaConcept, CUI, _SourceInfo, ConceptName) :-
	format('~nERROR: Unable to build concept name for ~q/~q~n', [CUI,MetaConcept]),
	concatenate_items_to_atom(['<',CUI,'>'], ConceptName).

build_concept_name_1_aux(SourceInfo, MetaConcept, ConceptName) :-
	SourceInfo = [FirstSource|RestSources],
	build_list(RestSources, [], RestSourceList),
	append([[MetaConcept,' {',FirstSource],RestSourceList,['}']], ItemList),
	concatenate_items_to_atom(ItemList, ConceptName).

extract_unique_sources([], SourcesIn, SourcesOut) :-
	!,
	rev(SourcesIn, SourcesOut).
extract_unique_sources([[_I,_SourceName,Source,_TTY]|Rest],
		       SourcesIn, SourcesOut) :-
	( memberchk(Source, SourcesIn) ->
	  extract_unique_sources(Rest, SourcesIn, SourcesOut)
	; extract_unique_sources(Rest, [Source|SourcesIn], SourcesOut)
	).

build_list([], ListIn, ListOut) :-
	rev(ListIn, ListOut).
build_list([First|Rest], ListIn, ListOut) :-
	build_list(Rest,[First,', '|ListIn], ListOut).

dump_evaluations_indented(candidates(Evaluations), Label) :-
	( Evaluations == [] ->
	  format('Meta ~a (0): <none>~n', [Label])
	; length(Evaluations, NEvaluations),
          format('Meta ~a (~d):~n', [Label,NEvaluations]),
	  construct_related_evaluations(Evaluations, EvaluationPairs),
	  dump_evaluations_indented(EvaluationPairs)
	).

construct_related_evaluations([], []).
construct_related_evaluations([First|Rest], [First:FirstRelated|RestRelated]) :-
	\+ control_option(allow_duplicate_concept_names),
	!,
	First = ev(_,_,_,Concept,_,_,_,_,_,_SourceInfo,_PosInfo),
	construct_related_evaluations_6(Rest, Concept, [], RevFirstRelated, [], RevNewRest),
	rev(RevFirstRelated, FirstRelated),
	rev(RevNewRest, NewRest),
	construct_related_evaluations(NewRest, RestRelated).
construct_related_evaluations([First|Rest], [First:FirstRelated|RestRelated]) :-
	% control_option(allow_duplicate_concept_names),
	!,
	First = ev(_,CUI,_,Concept,_,_,_,_,_,_SourceInfo,_PosInfo),
	construct_related_evaluations_7(Rest, CUI, Concept, [], RevFirstRelated, [], RevNewRest),
	rev(RevFirstRelated, FirstRelated),
	rev(RevNewRest, NewRest),
	construct_related_evaluations(NewRest, RestRelated).

construct_related_evaluations_6([], _Concept, RelatedIn,RelatedIn, UnrelatedIn, UnrelatedIn).
construct_related_evaluations_6([First|Rest], Concept, RelatedIn, RelatedOut,
				UnrelatedIn, UnrelatedOut) :-
	First = ev(_,_,_,Concept,_,_,_,_,_,_SourceInfo,_PosInfo),
	!,
	construct_related_evaluations_6(Rest, Concept,
					[First|RelatedIn], RelatedOut, UnrelatedIn, UnrelatedOut).
construct_related_evaluations_6([First|Rest], Concept,
				RelatedIn, RelatedOut, UnrelatedIn, UnrelatedOut) :-
	construct_related_evaluations_6(Rest, Concept,
					RelatedIn,RelatedOut, [First|UnrelatedIn], UnrelatedOut).

construct_related_evaluations_7([], _CUI, _Concept,
				RelatedIn, RelatedIn, UnrelatedIn, UnrelatedIn).
construct_related_evaluations_7([First|Rest], CUI, Concept,
			       RelatedIn, RelatedOut, UnrelatedIn, UnrelatedOut) :-
	First = ev(_,CUI,_,Concept,_,_,_,_,_SourceInfo,_PosIinfo),
	!,
	construct_related_evaluations_7(Rest, CUI, Concept,
					[First|RelatedIn], RelatedOut, UnrelatedIn, UnrelatedOut).
construct_related_evaluations_7([First|Rest], CUI, Concept,
				RelatedIn, RelatedOut, UnrelatedIn, UnrelatedOut) :-
	construct_related_evaluations_7(Rest, CUI, Concept,
					RelatedIn, RelatedOut, [First|UnrelatedIn], UnrelatedOut).

dump_evaluations_indented([]).
dump_evaluations_indented([FirstEV|RestEVs]) :-
	dump_one_evaluation_indented(FirstEV),
	dump_evaluations_indented(RestEVs).

dump_one_evaluation_indented(ev(NegValue,CUI,MetaTerm,MetaConcept,_MetaWords,
                              SemTypes0,_MatchMap,_InvolvesHead,_IsOvermatch,SourceInfo,_PosInfo)
			     :RelatedEvaluations) :-
	Value is -NegValue,
	% build_concept_name(CUI,MetaConcept,ConceptName),
	build_concept_name_1(MetaConcept, CUI, SourceInfo, ConceptName),
	display_concept_info(MetaTerm, MetaConcept, Value, CUI, ConceptName),
	conditionally_expand_semtypes(SemTypes0),
	conditionally_get_cui_sources(CUI),
	format('~n', []),
	dump_related_evaluations(RelatedEvaluations).

dump_related_evaluations([]).
dump_related_evaluations([ev(_NegValue,_CUI,MetaTerm,_MetaConcept,_MetaWords,
                             _SemTypes,_MatchMap,_InvolvesHead,_IsOvermatch,_SourceInfo,_PosInfo)
			 |Rest]) :-
	format('          ~p~n', [MetaTerm]),
	dump_related_evaluations(Rest).


num_dump_evaluations_indented(Evaluations, Label) :-
	( Evaluations == [] ->
	  format('Meta ~a (0): <none>~n', [Label])
	; length(Evaluations, NEvaluations),
          format('Meta ~a (~d):~n', [Label,NEvaluations]),
	  construct_related_evaluations(Evaluations, EvaluationPairs),
	  num_dump_evaluations_indented_aux(EvaluationPairs, 1)
	).

num_dump_evaluations_indented_aux([], _).
num_dump_evaluations_indented_aux([Evaluation:RelatedEvaluations|Rest], N) :-
	num_dump_one_evaluation_indented(Evaluation:RelatedEvaluations, N, NextN),
	num_dump_evaluations_indented_aux(Rest, NextN).


num_dump_one_evaluation_indented(Evaluation:RelatedEvaluations, N, NextN) :-
	Evaluation = ev(NegValue,CUI,MetaTerm,MetaConcept,
			_MetaWords,SemTypes0,_MatchMap,
			_InvolvesHead,_IsOvermatch,SourceInfo,_PosInfo),
	Value is -NegValue,
	build_concept_name_1(MetaConcept, CUI, SourceInfo, ConceptName),
	num_display_concept_info(MetaTerm, MetaConcept, N, Value, CUI, ConceptName),
	conditionally_expand_semtypes(SemTypes0),
	conditionally_get_cui_sources(CUI),
	format('~n',[]),
	N1 is N + 1,
	num_dump_related_evaluations(RelatedEvaluations, N1, NextN).

num_display_concept_info(MetaTerm, MetaConcept, N, Value, CUI, ConceptName) :-
	control_option(show_cuis),
	!,
	( preferred_name_only(MetaTerm, MetaConcept) ->
	  format('~t~d~4|. ~t~d~12| ~p:~p', [N,Value,CUI,ConceptName])
	; format('~t~d~4|. ~t~d~12| ~p:~p (~p)', [N,Value,CUI,MetaTerm,ConceptName])
	).
num_display_concept_info(MetaTerm, MetaConcept, N, Value, _CUI, ConceptName) :-
	( preferred_name_only(MetaTerm, MetaConcept) ->
	  format('~t~d~4|. ~t~d~12| ~p', [N,Value,ConceptName])
	; format('~t~d~4|. ~t~d~12| ~p (~p)', [N,Value,MetaTerm,ConceptName])
	).
	
num_dump_related_evaluations([],NIn,NIn).
num_dump_related_evaluations([ev(_NegValue,_CUI,MetaTerm,_MetaConcept,
				 _MetaWords,_SemTypes,_MatchMap,_InvolvesHead,
				 _IsOvermatch,_SourceInfo,_PosInfo)|
                          Rest],NIn,NOut) :-
    format('~t~d~4|.           ~p~n',[NIn,MetaTerm]),
    NInOut is NIn + 1,
    num_dump_related_evaluations(Rest,NInOut,NOut).

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
extract_relevant_sources([[I,SourceName,Source,TTY]|Rest],
			 Sources,
			 [[I,SourceName,Source,TTY]|ExtractedRest]) :-
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

extract_nonexcluded_sources([],_,[]).
extract_nonexcluded_sources([[I,SourceName,Source,TTY]|Rest],
			    Sources,
			    [[I,SourceName,Source,TTY]|ExtractedRest]) :-
	\+memberchk(Source, Sources),
	!,
	extract_nonexcluded_sources(Rest, Sources, ExtractedRest).
extract_nonexcluded_sources([_|Rest], Sources, ExtractedRest) :-
	extract_nonexcluded_sources(Rest, Sources, ExtractedRest).

extract_nonexcluded_sources_1([],_,[]).
extract_nonexcluded_sources_1([Source|RestSources],
			    Sources,
			    [Source|ExtractedRest]) :-
	\+memberchk(Source, Sources),
	!,
	extract_nonexcluded_sources_1(RestSources, Sources, ExtractedRest).
extract_nonexcluded_sources_1([_|Rest], Sources, ExtractedRest) :-
	extract_nonexcluded_sources_1(Rest, Sources, ExtractedRest).


/* extract_source_name(+SourceInfo, -SourceName)

extract_source_name/2 extracts SourceName from the first entry in SourceInfo.
SourceInfo should never be empty; but if it is, SourceName is set to
'<none>'. */

 % should never happen
extract_source_name([], '<none>').
extract_source_name([[_I,SourceName,_Source,_TTY]|_Rest], SourceName).

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
