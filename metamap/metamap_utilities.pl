% File:	    metamap_utilities.pl
% Module:   MetaMap
% Author:   Lan
% Purpose:  MetaMap utility predicates


:- module(metamap_utilities,[
	build_concept_name/3,
	dump_aphrase_mappings/2,
	dump_evaluations_indented/2,
	dump_variants_labelled/2,
	extract_nonexcluded_sources/3,
	extract_relevant_sources/3,
	extract_source_name/2,
	extract_unique_sources/3,
	get_phrase_text/4,
	num_dump_evaluations_indented/2,
	positions_overlap/2,
	wgvcs/1,
	wl/1,
	write_avl_list/1,
	write_list_indented/1
    ]).

:- use_module(metamap(metamap_tokenization),[
	extract_input_matches/4
    ]).

:- use_module(skr_db(db_access),[
	db_get_cui_sources/2,
	db_get_cui_sourceinfo/2
    ]).

:- use_module(skr(skr_umls_info09),[
	convert_to_root_sources/2
    ]).

:- use_module(skr_lib(semtype_translation09),[
	expand_semtypes/2
    ]).

:- use_module(skr_lib(nls_system),[
	control_option/1,
	control_value/2
    ]).

:- use_module(skr_lib(nls_strings),[
	atom_codes_list/2,
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

% positions_overlap([0,-1], _Position2) :-
% 	!,
% 	fail.
% positions_overlap(_Position1, [0,-1]) :-
% 	!,
% 	fail.
% positions_overlap([Begin1,End1], [Begin2,_End2]) :-
% 	Begin1 =< Begin2,
% 	End1 >= Begin2,
% 	!.
% positions_overlap([Begin1,_End1], [Begin2,End2]) :-
% 	Begin2 < Begin1,
% 	End2 >= Begin1.

/* get_phrase_text(+Phrase, +TextIn, -TextOut, -PhraseText)

get_phrase_text/4 finds PhraseText, the part of TextIn corresponding to the
parsed Phrase.  TextOut is what remains.  All text is strings.  */

get_phrase_text(Phrase, TextIn, TextOut, PhraseText) :-
	extract_input_matches(Phrase, unfiltered, IMWordAtoms, _),
	atom_codes_list(IMWordAtoms, IMWords),
	match_texts(IMWords, TextIn, TextOut, PhraseText),
	!.
get_phrase_text(Phrase, TextIn, TextIn, '') :-
	format(user_output, 'WARNING: get_phrase_text/4 failed for phrase ~p and input text ~p~n',
	       [Phrase,TextIn]),
	abort.

/* match_texts(+TextTokens, +TextIn, -TextOut, -MatchedText)
   match_texts_1(+TextTokens, +TextIn, -TextOut, +MatchedTextIn, -MatchedTextOut)

match_texts/4 matches TextIn with all text in TextTokens producing MatchedText,
a prefix of TextIn, and TextOut, the remaining text. All text are strings.
match_texts_1/5 is an auxiliary.  */

match_texts(TextTokens, TextIn, TextOut, MatchedText) :-
	match_texts_1(TextTokens, TextIn, TextOut, "", MatchedText0),
	trim_whitespace(MatchedText0, MatchedText).

match_texts_1([], TextIn, TextIn, MatchedTextIn, MatchedTextIn) :-
	!.
match_texts_1([First|Rest], TextIn, TextOut, MatchedTextIn, MatchedTextOut) :-
	append(First, TextInOut, TextIn),
	!,
	append(MatchedTextIn, First, MatchedTextInOut),
	match_texts_1(Rest, TextInOut, TextOut, MatchedTextInOut, MatchedTextOut).
match_texts_1(TextTokens, TextIn, TextOut, MatchedTextIn, MatchedTextOut) :-
	append(" ", TextInOut, TextIn),
	!,
	append(MatchedTextIn, " ", MatchedTextInOut),
	match_texts_1(TextTokens, TextInOut, TextOut, MatchedTextInOut, MatchedTextOut).
match_texts_1(TextTokens, TextIn, TextOut, MatchedTextIn, MatchedTextOut) :-
	append("""", TextInOut, TextIn),
	!,
	append(MatchedTextIn, """", MatchedTextInOut),
	match_texts_1(TextTokens, TextInOut, TextOut, MatchedTextInOut, MatchedTextOut).
    
/* dump_aphrase_mappings(+APhrases, +Label)
   dump_aphrase_mappings_aux(+APhrases, +Label)
   dump_mapping(+Mapping, +Label, +Value)

dump_aphrase_mappings/2
dump_aphrase_mappings_aux/2
dump_mapping/2
xxx
*/

dump_aphrase_mappings([],Label) :-
    format('Meta ~a: <none>~n',[Label]),
    !.
dump_aphrase_mappings(APhrases,Label) :-
    dump_aphrase_mappings_aux(APhrases,Label).

dump_aphrase_mappings_aux([],_Label).
dump_aphrase_mappings_aux([ap(NegValue,_,_,Mapping)|Rest],Label) :-
    Value is -NegValue,
    dump_mapping(Mapping,Label,Value),
    dump_aphrase_mappings_aux(Rest,Label).

dump_mapping([],Label,_) :-
    !,
    format('Meta ~a: <none>~n',[Label]).
dump_mapping(Mapping,Label,Value) :-
    format('Meta ~a (~d):~n',[Label,Value]),
    dump_evaluations(Mapping).


dump_evaluations([]).
dump_evaluations([ev(NegValue,CUI,MetaTerm,MetaConcept,_MetaWords,SemTypes0,
                     _MatchMap,_InvolvesHead,_IsOvermatch,_SourceInfo,_PosInfo)|Rest]) :-
    Value is -NegValue,
    build_concept_name(CUI,MetaConcept,ConceptName),
    (control_option(show_cuis) ->
	((MetaTerm==MetaConcept;
	 control_option(show_preferred_names_only)) ->
	    format('~t~d~6| ~p:~p',[Value,CUI,ConceptName])
	;   format('~t~d~6| ~p:~p (~p)',[Value,CUI,MetaTerm,ConceptName])
	)
    ;   ((MetaTerm==MetaConcept;
	 control_option(show_preferred_names_only)) ->
	    format('~t~d~6| ~p',[Value,ConceptName])
	;   format('~t~d~6| ~p (~p)',[Value,MetaTerm,ConceptName])
	)
    ),
    ( \+ control_option(hide_semantic_types) ->
      expand_semtypes(SemTypes0,SemTypes),
      format(' ~p',[SemTypes])
    ; true
    ),
    (control_option(preferred_name_sources) ->
	db_get_cui_sources(CUI,Sources),
        format(' ~p',[Sources])
    ;   true
    ),
    format('~n',[]),
    dump_evaluations(Rest).


/* build_concept_name(+CUI, +MetaConcept, -ConceptName)

build_concept_name/2 constructs ConceptName from MetaConcept by optionally
appending slist, the list of sources for ConceptName. slist is appended when
-G (--sources) is active, and it is affected by options
-R (--restrict_to_sources) and -e (--exlude_sources). */

build_concept_name(_CUI,MetaConcept,MetaConcept) :-
    \+control_option(sources),
    !.
build_concept_name(CUI,MetaConcept,ConceptName) :-
    db_get_cui_sourceinfo(CUI,SourceInfo0),
    (   control_option(restrict_to_sources) ->
	control_value(restrict_to_sources,Sources),
	convert_to_root_sources(Sources,RootSources),
	extract_relevant_sources(SourceInfo0,RootSources,SourceInfo)
    ;   control_option(exclude_sources) ->
        control_value(exclude_sources,Sources),
	convert_to_root_sources(Sources,RootSources),
	extract_nonexcluded_sources(SourceInfo0,RootSources,SourceInfo)
    ;   SourceInfo=SourceInfo0
    ),
    build_concept_name_aux(SourceInfo,MetaConcept,ConceptName),
    !.
% should never occur
build_concept_name(CUI,_MetaConcept,ConceptName) :-
    concatenate_items_to_atom(['<',CUI,'>'],ConceptName).


build_concept_name_aux(SourceInfo,MetaConcept,ConceptName) :-
    SourceInfo=[[_I,FirstName,_Source,_TTY]|_],
    (FirstName==MetaConcept ->
	true
    ;   (control_option(warnings) ->
	     format('WARNING: build_concept_name clash expecting ~p finding ~p.~n',
		   [MetaConcept,FirstName])
	;   true
	)
    ),
    extract_unique_sources(SourceInfo,[],UniqueSources),
    UniqueSources=[FirstSource|RestSources],
    build_list(RestSources,[],RestSourceList),
    append([[MetaConcept,' {',FirstSource],RestSourceList,['}']],ItemList),
    concatenate_items_to_atom(ItemList,ConceptName),
    !.

extract_unique_sources([],SourcesIn,SourcesOut) :-
    !,
    rev(SourcesIn,SourcesOut).
extract_unique_sources([[_I,_SourceName,Source,_TTY]|Rest],
		       SourcesIn,SourcesOut) :-
    (memberchk(Source,SourcesIn) ->
	extract_unique_sources(Rest,SourcesIn,SourcesOut)
    ;   extract_unique_sources(Rest,[Source|SourcesIn],SourcesOut)
    ).

build_list([],ListIn,ListOut) :-
    rev(ListIn,ListOut).
build_list([First|Rest],ListIn,ListOut) :-
    build_list(Rest,[First,', '|ListIn],ListOut).

dump_evaluations_indented(candidates(Evaluations),Label) :-
    (Evaluations==[] ->
        format('Meta ~a (0): <none>~n',[Label])
    ;   length(Evaluations,NEvaluations),
        format('Meta ~a (~d):~n',[Label,NEvaluations]),
        construct_related_evaluations(Evaluations,EvaluationPairs),
        dump_evaluations_indented(EvaluationPairs)
    ).

construct_related_evaluations([],[]).
construct_related_evaluations([First|Rest],[First:FirstRelated|RestRelated]) :-
    \+control_option(allow_duplicate_concept_names),
    !,
    First = ev(_,_,_,Concept,_,_,_,_,_,_SourceInfo,_PosInfo),
    construct_related_evaluations(Rest,Concept,[],RevFirstRelated,[],
                                  RevNewRest),
    rev(RevFirstRelated,FirstRelated),
    rev(RevNewRest,NewRest),
    construct_related_evaluations(NewRest,RestRelated).
construct_related_evaluations([First|Rest],[First:FirstRelated|RestRelated]) :-
    control_option(allow_duplicate_concept_names),
    !,
    First = ev(_,CUI,_,Concept,_,_,_,_,_,_SourceInfo,_PosInfo),
    construct_related_evaluations(Rest,CUI,Concept,[],RevFirstRelated,[],
                                  RevNewRest),
    rev(RevFirstRelated,FirstRelated),
    rev(RevNewRest,NewRest),
    construct_related_evaluations(NewRest,RestRelated).

construct_related_evaluations([],_Concept,RelatedIn,RelatedIn,
                              UnrelatedIn,UnrelatedIn) :-
    !.
construct_related_evaluations([First|Rest],Concept,RelatedIn,RelatedOut,
                              UnrelatedIn,UnrelatedOut) :-
    First = ev(_,_,_,Concept,_,_,_,_,_,_SourceInfo,_PosInfo),
    !,
    construct_related_evaluations(Rest,Concept,[First|RelatedIn],RelatedOut,
                                  UnrelatedIn,UnrelatedOut).
construct_related_evaluations([First|Rest],Concept,RelatedIn,RelatedOut,
                              UnrelatedIn,UnrelatedOut) :-
    !,
    construct_related_evaluations(Rest,Concept,RelatedIn,RelatedOut,
                                  [First|UnrelatedIn],UnrelatedOut).

construct_related_evaluations([],_CUI,_Concept,RelatedIn,RelatedIn,
                              UnrelatedIn,UnrelatedIn) :-
    !.
construct_related_evaluations([First|Rest],CUI,Concept,RelatedIn,RelatedOut,
                              UnrelatedIn,UnrelatedOut) :-
    First = ev(_,CUI,_,Concept,_,_,_,_,_,_),
    !,
    construct_related_evaluations(Rest,CUI,Concept,[First|RelatedIn],RelatedOut,
                                  UnrelatedIn,UnrelatedOut).
construct_related_evaluations([First|Rest],CUI,Concept,RelatedIn,RelatedOut,
                              UnrelatedIn,UnrelatedOut) :-
    !,
    construct_related_evaluations(Rest,CUI,Concept,RelatedIn,RelatedOut,
                                  [First|UnrelatedIn],UnrelatedOut).

dump_evaluations_indented([]).
dump_evaluations_indented([ev(NegValue,CUI,MetaTerm,MetaConcept,_MetaWords,
                              SemTypes0,_MatchMap,_InvolvesHead,_IsOvermatch,_SourceInfo,_PosInfo):
                           RelatedEvaluations|Rest]) :-
    Value is -NegValue,
    build_concept_name(CUI,MetaConcept,ConceptName),
    (control_option(show_cuis) ->
	((MetaTerm==MetaConcept;
	 control_option(show_preferred_names_only)) ->
	    format('~t~d~6| ~p:~p',[Value,CUI,ConceptName])
	;   format('~t~d~6| ~p:~p (~p)',[Value,CUI,MetaTerm,ConceptName])
	)
    ;   ((MetaTerm==MetaConcept;
	 control_option(show_preferred_names_only)) ->
	    format('~t~d~6| ~p',[Value,ConceptName])
	;   format('~t~d~6| ~p (~p)',[Value,MetaTerm,ConceptName])
	)
    ),
    ( \+ control_option(hide_semantic_types) ->
      expand_semtypes(SemTypes0,SemTypes),
      format(' ~p',[SemTypes])
    ; true
    ),
    (control_option(preferred_name_sources) ->
	db_get_cui_sources(CUI,Sources),
        format(' ~p',[Sources])
    ;   true
    ),
    format('~n',[]),
    dump_related_evaluations(RelatedEvaluations),
    dump_evaluations_indented(Rest).

dump_related_evaluations([]).
dump_related_evaluations([ev(_NegValue,_CUI,MetaTerm,_MetaConcept,_MetaWords,
                             _SemTypes,_MatchMap,_InvolvesHead,_IsOvermatch,_SourceInfo,_PosInfo)|
                          Rest]) :-
    format('          ~p~n',[MetaTerm]),
    dump_related_evaluations(Rest).


num_dump_evaluations_indented(Evaluations,Label) :-
    (Evaluations==[] ->
        format('Meta ~a (0): <none>~n',[Label])
    ;   length(Evaluations,NEvaluations),
        format('Meta ~a (~d):~n',[Label,NEvaluations]),
        construct_related_evaluations(Evaluations,EvaluationPairs),
        num_dump_evaluations_indented_aux(EvaluationPairs,1)
    ).

num_dump_evaluations_indented_aux([],_).
num_dump_evaluations_indented_aux([ev(NegValue,CUI,MetaTerm,MetaConcept,
				      _MetaWords,SemTypes0,_MatchMap,
				      _InvolvesHead,_IsOvermatch,_SourceInfo,_PosInfo):
                           RelatedEvaluations|Rest],N) :-
    Value is -NegValue,
    build_concept_name(CUI,MetaConcept,ConceptName),
    (control_option(show_cuis) ->
	((MetaTerm==MetaConcept;
	 control_option(show_preferred_names_only)) ->
	    format('~t~d~4|. ~t~d~12| ~p:~p',[N,Value,CUI,ConceptName])
	;   format('~t~d~4|. ~t~d~12| ~p:~p (~p)',
		   [N,Value,CUI,MetaTerm,ConceptName])
	)
    ;   ((MetaTerm==MetaConcept;
	 control_option(show_preferred_names_only)) ->
	    format('~t~d~4|. ~t~d~12| ~p',[N,Value,ConceptName])
	;   format('~t~d~4|. ~t~d~12| ~p (~p)',[N,Value,MetaTerm,ConceptName])
	)
    ),
    ( \+ control_option(hide_semantic_types) ->
      expand_semtypes(SemTypes0,SemTypes),
      format(' ~p',[SemTypes])
    ; true
    ),
    (control_option(preferred_name_sources) ->
	db_get_cui_sources(CUI,Sources),
        format(' ~p',[Sources])
    ;   true
    ),
    format('~n',[]),
    M is N + 1,
    num_dump_related_evaluations(RelatedEvaluations,M,L),
    num_dump_evaluations_indented_aux(Rest,L).

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

extract_relevant_sources([],_,[]) :-
    !.
extract_relevant_sources([[I,SourceName,Source,TTY]|Rest],Sources,
			 [[I,SourceName,Source,TTY]|ExtractedRest]) :-
    memberchk(Source,Sources),
    !,
    extract_relevant_sources(Rest,Sources,ExtractedRest).
extract_relevant_sources([_|Rest],Sources,ExtractedRest) :-
    extract_relevant_sources(Rest,Sources,ExtractedRest).

extract_nonexcluded_sources([],_,[]) :-
    !.
extract_nonexcluded_sources([[I,SourceName,Source,TTY]|Rest],Sources,
			    [[I,SourceName,Source,TTY]|ExtractedRest]) :-
    \+memberchk(Source,Sources),
    !,
    extract_nonexcluded_sources(Rest,Sources,ExtractedRest).
extract_nonexcluded_sources([_|Rest],Sources,ExtractedRest) :-
    extract_nonexcluded_sources(Rest,Sources,ExtractedRest).


/* extract_source_name(+SourceInfo, -SourceName)

extract_source_name/2 extracts SourceName from the first entry in SourceInfo.
SourceInfo should never be empty; but if it is, SourceName is set to
'<none>'. */

extract_source_name([],'<none>') :- % should never happen
    !.
extract_source_name([[_I,SourceName,_Source,_TTY]|_Rest],SourceName) :-
    !.


% temp, for debugging

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
