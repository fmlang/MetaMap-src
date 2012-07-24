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

% File:	    metamap_evaluation.pl
% Module:   MetaMap
% Author:   Lan
% Purpose:  Evaluate MetaMap candidates/mappings


:- module(metamap_evaluation, [
	% consolidate_matchmap/3,
	evaluate_all_GVCs/16,
	extract_components/3,
	component_intersects_components/2,
	compute_match_value/8,
	connect_components/2,
	matching_token/3,
	merge_contiguous_components/2,
	compute_extra_meta/3,
	word_is_last_word_of_some_variant/2
    ]).


:- use_module(metamap(metamap_utilities), [
	candidate_term/15,
	extract_unique_sources/2,
	positions_overlap/2
    ]).

:- use_module(metamap(metamap_tokenization), [
	linearize_components/2
    ]).

:- use_module(skr(skr_utilities), [
    	compute_sum/3,
    	debug_message/3,
	split_word/3
    ]).

:- use_module(skr_db(db_access), [
	db_get_concept_cui/2,
	db_get_concept_cuis/2,
	db_get_cui_sourceinfo/2
    ]).

:- use_module(skr_lib(nls_avl), [
	add_to_avl/4
   ]).

:- use_module(skr_lib(nls_system), [
	control_option/1,
	control_value/2
    ]).

:- use_module(skr_lib(nls_text), [
    	eliminate_multiple_meaning_designator/2
    ]).

:- use_module(skr_lib(pos_info), [
    	collapse_pos_info/3
    ]).

:- use_module(skr_lib(sicstus_utils), [
	concat_atom/2
    ]).

:- use_module(library(avl), [
	avl_fetch/3,
	avl_incr/4,
	avl_member/3
    ]).

:- use_module(library(between),[
	between/3
    ]).

:- use_module(library(lists), [
	append/2,
	last/2,
	nth1/3,
	rev/2,
	select/3
    ]).

/* evaluate_all_GVCs(+GVCs, +DebugFlags, +Label, +UtteranceText,
   		     +Variants, +TokenPhraseWords, +PhraseTokenLength,
		     +TokenHeadWords,
		     +PhraseTokens, +RawTokensOut, +AAs,
		     +InputmatchPhraseWords,
		     +CCsIn, -CCsOut,
		     +EvaluationsIn, -EvaluationsOut)
   evaluate_one_gvc(+GVC, +DebugFlags, +Variants, +TokenPhraseWords, +PhraseTokenLength,
   		    +TokenHeadWords, +PhraseTokens, +RawTokensOut,
		    +AAs, +InputmatchPhraseWords,
		    +CCsIn, -CCsOut,
		    +EvaluationsIn, -EvaluationsOut)
*/

evaluate_all_GVCs([], _DebugFlags, _Label, _UtteranceText, _Variants, _TokenPhraseWords,
		  _PhraseTokenLength, _TokenHeadWords,
		  _PhraseTokens, _RawTokensOut, _AAs,
		  _InputmatchPhraseWords,
		  CCsIn, CCsIn,
		  EvaluationsIn, EvaluationsIn).
evaluate_all_GVCs([First|Rest], DebugFlags, Label, UtteranceText, Variants, TokenPhraseWords,
		  PhraseTokenLength, TokenHeadWords,
		  PhraseTokens, RawTokensOut, AAs,
		  InputmatchPhraseWords,
		  CCsIn, CCsOut,
		  EvaluationsIn, EvaluationsOut) :-
	evaluate_one_GVC(First, DebugFlags, Label, UtteranceText, Variants, TokenPhraseWords,
			 PhraseTokenLength, TokenHeadWords,
			 PhraseTokens,  RawTokensOut, AAs,
			 InputmatchPhraseWords,
			 CCsIn, CCsInOut,
			 EvaluationsIn, EvaluationsInOut),
	evaluate_all_GVCs(Rest, DebugFlags, Label, UtteranceText, Variants, TokenPhraseWords,
			  PhraseTokenLength, TokenHeadWords,
			  PhraseTokens, RawTokensOut, AAs,
			  InputmatchPhraseWords,
			  CCsInOut, CCsOut,
			  EvaluationsInOut, EvaluationsOut).

evaluate_one_GVC(gvc(Generator,_,Candidates), DebugFlags, Label, UtteranceText, Variants,
		 TokenPhraseWords, PhraseTokenLength,
		 TokenHeadWords, PhraseTokens,  RawTokensOut, AAs,
		 InputmatchPhraseWords,
		 CCsIn, CCsOut,
		 EvaluationsIn, EvaluationsOut) :-
	debug_message(trace, '~N### Generator: ~q~n', [Generator]),
	% length(EvaluationsIn, EvaluationsInLength),
	% format(user_output, '~n### Appending Length ~w~n', [EvaluationsInLength]),
	append(EvaluationsIn, Evaluations, EvaluationsOut),
	% length(Candidates, CandidatesLength),
	% format(user_output, '~n### Evaluating Generator ~q; Candidate List Length ~w~n',
	%      [Generator, CandidatesLength]),
	evaluate_candidate_list(Candidates, DebugFlags, Label, UtteranceText,
				Variants, TokenPhraseWords, PhraseTokenLength,
				TokenHeadWords, PhraseTokens, RawTokensOut,
				AAs, InputmatchPhraseWords,
				CCsIn, CCsOut, Evaluations).
	% format(user_output, '~n### Generator evaluation DONE!~n', []).

evaluate_candidate_list([], _DebugFlags, _Label, _UtteranceText, _Variants, _TokenPhraseWords,
			_PhraseTokenLength, _Headwords,
			_PhraseTokens, _RawTokensOut, _AAs,
			_InputmatchPhraseWords,
			CCsIn, CCsIn, []).
% MetaCanonical = words forming Normalized Metathesaurus String
% MetaString    = the actual Metathesaurus string
% MetaConcept   = the preferred name of the concept
evaluate_candidate_list([usc(MetaCanonical,MetaString,MetaConcept)|Rest],
			DebugFlags, Label, UtteranceText,
			Variants, TokenPhraseWords, PhraseTokenLength, TokenHeadWords,
			PhraseTokens, RawTokensOut, AAs,
			InputmatchPhraseWords,
			CCsIn, CCsOut, Evaluations) :-
	debug_evaluate_candidate_list_1(DebugFlags, MetaCanonical,MetaString, MetaConcept),
	% avl_size(CCsIn, CCsInSize),
	% format(user_output,'~N### CCsIn Length: ~w~n', [CCsInSize]),
	avl_fetch(MetaCanonical, CCsIn, SavedValues),
	memberchk(MetaConcept, SavedValues),
	% member(cc(MetaCanonical,MetaConcept), CCsIn),
	eliminate_multiple_meaning_designator(MetaConcept, MetaConcept),
	!,
	% format(user_output, '~N### FOUND ~q|~q in cache~n', [MetaCanonical,MetaConcept]),
	debug_evaluate_candidate_list_2(DebugFlags),
	% format('~N### Eval 1: ~q|~q|~q~n', [MetaCanonical,MetaString,MetaConcept]),
	evaluate_candidate_list(Rest, DebugFlags, Label, UtteranceText,
				Variants, TokenPhraseWords, PhraseTokenLength, TokenHeadWords,
				PhraseTokens, RawTokensOut, AAs,
				InputmatchPhraseWords,
				CCsIn, CCsOut, Evaluations).

% MetaCanonical is the normalized string, tokenized into words
% MetaString    is the non-normalized string
% MetaConcept   is the preferred name of the concept
evaluate_candidate_list([usc(MetaCanonical,MetaString,MetaConcept)|Rest],
			DebugFlags, Label, UtteranceText,
			Variants, TokenPhraseWords, PhraseTokenLength, TokenHeadWords,
			PhraseTokens, RawTokensOut, AAs,
			InputmatchPhraseWords,
			CCsIn, CCsOut, Evaluations) :-
	% \+ control_option(allow_duplicate_concept_names),
	% format(user_output, '~n### USC ~q|~q|~q~n', [MetaCanonical,MetaString,MetaConcept]),
	!,
	% format(user_output, '~w ~w ~w~n', [MetaCanonical,MetaString,MetaConcept]),
	( compute_one_evaluation(MetaCanonical, DebugFlags, Label, UtteranceText,
				 MetaString, MetaConcept,
				 Variants, TokenPhraseWords, PhraseTokenLength,
				 RawTokensOut, AAs,
				 InputmatchPhraseWords,
				 TokenHeadWords, PhraseTokens, Evaluation) ->
	  % format(user_output, '~n### Eval ~q|~q|~q~n', [MetaCanonical,MetaConcept,Evaluation]) ->
	  debug_evaluate_candidate_list_3(DebugFlags, Evaluation),
	  % format(user_output, 'YES: ~q~n', [usc(MetaCanonical,MetaString,MetaConcept)]),
	  % format(user_output, '     ~q~n', [Evaluation]),	    
	  Evaluations = [Evaluation|RestEvaluations],
	  add_to_avl(MetaCanonical, MetaConcept, CCsIn, CCsNext)
	; Evaluations = RestEvaluations,
	  % format(user_output, ' NO: ~q~n', [usc(MetaCanonical,MetaString,MetaConcept)]),
	  debug_evaluate_candidate_list_4(DebugFlags),
	  CCsNext = CCsIn
	),
	% format('~N### Eval 2: ~q|~q|~q~n', [MetaCanonical,MetaString,MetaConcept]),
	% 
	evaluate_candidate_list(Rest, DebugFlags, Label, UtteranceText,
				Variants, TokenPhraseWords, PhraseTokenLength, TokenHeadWords,
				PhraseTokens, RawTokensOut, AAs,
				InputmatchPhraseWords,
				CCsNext, CCsOut, RestEvaluations).

% evaluate_candidate_list([usc(MetaCanonical,MetaString,MetaConcept)|Rest],
% 			DebugFlags, Label, UtteranceText,
% 			Variants, TokenPhraseWords, PhraseTokenLength, TokenHeadWords,
% 			PhraseTokens, RawTokensOut, AAs,
% 			InputmatchPhraseWords,
% 			CCsIn, CCsOut, Evaluations) :-
% 	% control_option(allow_duplicate_concept_names),
% 	( compute_all_evaluations(MetaCanonical, DebugFlags, Label, UtteranceText,
% 				  MetaString, MetaConcept,
% 				  Variants, TokenPhraseWords, PhraseTokenLength,
% 				  RawTokensOut, AAs,
% 				  InputmatchPhraseWords,
% 				  TokenHeadWords, PhraseTokens, NewEvaluations) ->
% 	  debug_evaluate_candidate_list_5(DebugFlags, NewEvaluations),
% 	  append(NewEvaluations, RestEvaluations, Evaluations)
% 	; Evaluations = RestEvaluations,
% 	  debug_evaluate_candidate_list_6(DebugFlags)
% 	),
% 	% format('~N### Eval 3: ~q|~q|~q~n', [MetaCanonical,MetaString,MetaConcept]),
% 	add_to_avl(MetaCanonical, MetaConcept, CCsIn, CCsNext),
% 	evaluate_candidate_list(Rest, DebugFlags, Label, UtteranceText,
% 				Variants, TokenPhraseWords, PhraseTokenLength, TokenHeadWords,
% 				PhraseTokens, RawTokensOut, AAs,
% 				InputmatchPhraseWords,
% 				CCsNext, CCsOut, RestEvaluations).

/* 
   compute_one_evaluation(+MetaWords, +DebugFlags, +Label, +UtteranceText, +MetaTerm, +MetaConcept,
   			  +Variants, +TokenPhraseWords, +PhraseTokenLength,
			  +RawTokensOut, +AAs, +InputmatchPhraseWords,
		          +TokenHeadWords, +PhraseTokens, -Evaluation)

   compute_all_evaluations(+MetaWords, +DebugFlags, +Label, +UtteranceText, +MetaTerm, +MetaConcept,
                           +Variants, +TokenPhraseWords, +PhraseTokenLength,
			   +RawTokensOut, +AAs, +InputmatchPhraseWords,
		           +TokenHeadWords, +PhraseTokens, -Evaluations)

compute_one_evaluation/14 computes the ev/9 term corresponding to the inputs.
compute_all_evaluations/14 allows for multiple results (in the non-standard case
in which multiple CUIs can have the same preferred name).
*/

compute_one_evaluation(MetaWords, DebugFlags, Label, UtteranceText, MetaTerm, MetaConcept,
		       Variants, TokenPhraseWords, PhraseTokenLength,
		       RawTokensOut, _AAs, _InputmatchPhraseWords,
		       TokenHeadWords, PhraseTokens, Evaluation) :-
	% filter_out_multiple_meaning_designators(MetaWords0,MetaWords),
	compute_phrase_match(TokenHeadWords, Label, UtteranceText,
			     TokenPhraseWords, MetaWords,
			     Variants, PhraseTokenLength,
			     MatchMap, InvolvesHead, IsOvermatch),
	% TempMatchMap = [MatchMapHead|MatchMapTail],
	% consolidate_matchmap(MatchMapTail, MatchMapHead, MatchMap),
	% get_matching_phrasewords(TokenPhraseWords, MatchMap, MatchingWords),
	% format(user_output,'MetaWords     = ~q~n', [MetaWords]),	
	% format(user_output,'MetaTerm      = ~q~n', [MetaTerm]),	
	% format(user_output,'MetaConcept   = ~q~n', [MetaConcept]),
	test_minimum_length(TokenPhraseWords, MatchMap),
	debug_message(trace, '~N### computing one evaluation: ~q~n', [MetaWords]),
	% format(user_output, '~w:~w:~w~n', [MetaWords,MatchMap,PhraseTokenLength]),
	MatchMap \== [],
	compute_connected_components(MatchMap, MatchCCs),
	length(TokenPhraseWords, NTokenPhraseWords),
	length(MetaWords, NMetaWords),
	compute_extra_meta(MatchMap, MetaWords, ExtraMetaWords),
	% format(user_output, '~w: ~w~n', [MetaWords,MatchMap]),
	debug_compute_one_evaluation_1(DebugFlags, TokenPhraseWords, MetaWords,
				       MatchMap, MatchCCs, ExtraMetaWords),
	compute_match_value(MatchMap, MatchCCs, NTokenPhraseWords, NMetaWords,
			    ExtraMetaWords, Variants, InvolvesHead, Value),
	debug_compute_one_evaluation_2(DebugFlags, MetaTerm),
	NegValue is -Value,
	db_get_concept_cui(MetaConcept, CUI),
	db_get_cui_sourceinfo(CUI, Sources),
	extract_unique_sources(Sources, UniqueSources),
        compute_target_LS_component(MatchMap, LSComponents, TargetLSComponent),
	candidate_term(NegValue, CUI, MetaTerm, MetaConcept, MetaWords, _SemTypes,
		       MatchMap, LSComponents, TargetLSComponent, InvolvesHead,
		       IsOvermatch, UniqueSources, PosInfo, _Status, Evaluation),
	% format(user_output, 'EV:~q:~n', [Evaluation]),
	% format(user_output, '~N~q:~q:~q~n', [MetaTerm,MetaWords,TokenPhraseWords]),
	% This is just so the debugger will let me examine Evaluation
	Evaluation \== [],
	% get the positional info corresponding to this MatchMap
	get_all_pos_info(MatchMap, TokenPhraseWords, PhraseTokens, RawTokensOut, PosInfo).

compute_target_LS_component(MatchMap, LSComponents, TargetLSComponent) :-
	extract_components(MatchMap, PhraseComponents, _MetaComponents),
	linearize_components(PhraseComponents, LSComponents0),
	% LSComponents is a list of integers representing
	% all phrase positions covered by the string in the ev/11 term.
	append(LSComponents0, LSComponents),
	% TargetLSComponent is the last phrase position covered.
	last(LSComponents, TargetLSComponent).

test_minimum_length(PhraseWords, MatchMap) :-
	( control_value(min_length, MinLength) ->
	  get_matching_phrasewords(PhraseWords, MatchMap, MatchingWords),
	  length(MatchingWords, TempMatchingWordsLength),
	  ExtraSpaces is TempMatchingWordsLength - 1,
	  concat_atom(MatchingWords, MatchingWordsAtom),
	  atom_length(MatchingWordsAtom, MatchingWordsLength),
	  TotalLength is MatchingWordsLength + ExtraSpaces,
	  TotalLength >= MinLength
	; true
	).

get_matching_phrasewords(PhraseWords, MatchMap, MatchingWords) :-
	extract_components(MatchMap, PhraseComponents, _MetaComponents),
	linearize_components(PhraseComponents, LinearizedPhraseComponents),
	append(LinearizedPhraseComponents, Indexes0),
	sort(Indexes0, Indexes),
	get_all_indexes(Indexes, PhraseWords, MatchingWords).

get_all_indexes([], _PhraseWords, []).
get_all_indexes([Index|RestIndexes], PhraseWords, [MatchingWord|RestMatchingWords]) :-
	nth1(Index, PhraseWords, MatchingWord),
	get_all_indexes(RestIndexes, PhraseWords, RestMatchingWords).

% We no longer consolidate MatchMaps because of
% the thorny issue of combining lexical variation scores
% consolidate_matchmap(MatchMapTail, MatchMapHead, ConsolidatedMatchMap) :-
%	ConsolidatedMatchMap = [MatchMapHead|MatchMapTail].

% consolidate_matchmap(MatchMapTail, MatchMapHead, ConsolidatedMatchMap) :-
% 	consolidate_matchmap_aux(MatchMapTail, MatchMapHead, MatchMap0),
% 	ConsolidatedMatchMap = MatchMap0.
% 	% force_variation_to_integer(MatchMap0, ConsolidatedMatchMap).	

% consolidate_matchmap_aux([], MatchMapHead, [MatchMapHead]).
% consolidate_matchmap_aux([MatchMap2|RestMatchMap], MatchMap1, MatchMap) :-
% 	( merge_matchmap_components(MatchMap1, MatchMap2, MergedMatchMap) ->
% 	  consolidate_matchmap_aux(RestMatchMap, MergedMatchMap, MatchMap)
% 	; MatchMap = [MatchMap1|MatchMapOut],
% 	  consolidate_matchmap_aux(RestMatchMap, MatchMap2, MatchMapOut)
% 	).

% force_variation_to_integer([], []).
% force_variation_to_integer([HIn|TIn], [HOut|TOut]) :-
% 	HIn  = [TextMatch,ConceptMatch,Variation],
% 	HOut = [TextMatch,ConceptMatch,VariationInt],
% 	VariationInt is round(Variation),
% 	force_variation_to_integer(TIn, TOut).

merge_matchmap_components([[Text1Start,Text1End], [Concept1Start,Concept1End], Variation1],
			  [[Text2Start,Text2End], [Concept2Start,Concept2End], Variation2],
			  [[Text1Start,Text2End], [Concept1Start,Concept2End], Variation3]) :-
	Text2Start is Text1End + 1,
	Concept2Start is Concept1End + 1,
	Variation3 is Variation1 + Variation2.

	% Sum
	% Variation3 is Variation1 + Variation2.
	% Max
	% Variation3 is max(Variation1, Variation2).

%%% modify_pos_info_for_aa_count(AAExtraCharCount, TempPosInfo, PosInfo) :-
%%% 	( AAExtraCharCount =:= 0 ->
%%% 	  PosInfo = TempPosInfo;
%%% 	  TempPosInfo = [FirstTempPosInfo|RestTempPosInfo],
%%% 	  increase_final_length(RestTempPosInfo, FirstTempPosInfo, AAExtraCharCount, PosInfo)
%%% 	).
%%% 	
%%% increase_final_length([], LastTempPosInfo, AAExtraCharCount, [PosInfo]) :-
%%% 	LastTempPosInfo = LastStartPos/LastLength,
%%% 	NewLastLength is LastLength + AAExtraCharCount,
%%% 	PosInfo = LastStartPos/NewLastLength.
%%% 
%%% increase_final_length([NextTempPosInfo|RestTempPosInfo], FirstTempPosInfo,
%%% 		      AAExtraCharCount,
%%% 		      [FirstTempPosInfo|RestPosInfo]) :-
%%% 	increase_final_length(RestTempPosInfo, NextTempPosInfo, AAExtraCharCount, RestPosInfo).


% A MatchMap list is a list of lists of the form [[X1,Y1], [X2,Y2], Z].
% In each MatchMap list, all we care about is the [X1,Y1],
% which represents the first (X1) and last (Y1) words in the PhraseWord list.
% We want to calculate the starting character position (StartPos) and the Length
% of the string representing that word span from X1/Y1 and the enhanced token list,
% which consists of terms of the following form:
 
% tok(lc,absence,absence,pos(192,199),pos(410,7))
% tok(ws,' ',' ',pos(199,200),pos(417,1))
% tok(lc,of,of,pos(200,202),pos(418,2))
% tok(ws,' ',' ',pos(202,203),pos(420,1))
% tok(lc,extracellular,extracellular,pos(203,216),pos(421,13))
% tok(ws,' ',' ',pos(216,217),pos(434,1))
% tok(an,'Ca2',ca2,pos(217,220),pos(435,3))
% tok(pn,+,+,pos(220,221),pos(438,1))
% tok(ws,' ',' ',pos(221,222),pos(439,1))
% tok(lc,and,and,pos(222,225),pos(440,3))
% tok(ws,' ',' ',pos(225,226),pos(443,1))
% tok(ic,'Type',type,pos(226,230),pos(444,4))
% tok(ws,' ',' ',pos(230,231),pos(448,1))
% tok(uc,'I',i,pos(231,232),pos(449,1))
% tok(ws,' ',' ',pos(232,233),pos(450,1))
% tok(lc,collagen,collagen,pos(233,241),pos(457,8))

get_all_pos_info(MatchMap, TokenPhraseWords, PhraseTokens, RawTokensOut, PosInfo) :-
	get_all_pos_info_1(MatchMap, TokenPhraseWords, PhraseTokens, RawTokensOut, TempPosInfo),
	append(TempPosInfo, AppendedPosInfo),
	sort(AppendedPosInfo, SortedPosInfo),
	SortedPosInfo = [H|T],
	collapse_pos_info(T, H, MergedPosInfo),
	PosInfo = MergedPosInfo.

get_all_pos_info_1([], _TokenPhraseWords, _PhraseTokens, _RawTokensOut, []).
get_all_pos_info_1([FirstMatchMap|RestMatchMaps], TokenPhraseWords,
		   PhraseTokens, RawTokensOut, [FirstPosInfo|RestPosInfo]) :-
	get_one_pos_info(FirstMatchMap, TokenPhraseWords, PhraseTokens, RestPhraseTokens,
			 RawTokensOut, _OneMatchingTokens, FirstPosInfo),
	get_all_pos_info_1(RestMatchMaps, TokenPhraseWords, RestPhraseTokens,
			   RawTokensOut, RestPosInfo).

get_one_pos_info(MatchMap, TokenPhraseWords, PhraseTokensIn, PhraseTokensOut,
		 RawTokensOut, [MatchingToken|RestMatchingTokens],
		 [FirstStartPos/FirstLength|RestPosInfo]) :-
	% get X1 and Y1 out of the MatchMap
	get_matchmap_indices(MatchMap, FirstMatch, LastMatch),
	% get words X1-->Y1 from the PhraseWord List; these are the MatchingTokenPhraseWords
	get_matching_phrase_words(FirstMatch, LastMatch, 1, TokenPhraseWords, MatchingTokenPhraseWords),
	MatchingTokenPhraseWords = [FirstMatchingPhraseWord|RestMatchingTokenPhraseWords],
	% get the second pos(_,_) from the tok(_,_,_,_,_) term
	% corresponding to FirstMatchingPhraseWord;
	% PhraseTokensNext is all subsequent tokens
 	get_one_word_pos_info(FirstMatchingPhraseWord, PhraseTokensIn,
			      RawTokensOut, MatchingToken, PhraseTokensNext,
			      FirstStartPos, FirstLength),
 	get_rest_word_pos_info(RestMatchingTokenPhraseWords, PhraseTokensNext,
			       RawTokensOut, RestMatchingTokens,
			       PhraseTokensOut, RestPosInfo).

get_matchmap_indices([[X,Y]|_], X, Y).

% get words X1-->Y1 from the PhraseWord List; these are the MatchingTokenPhraseWords
get_matching_phrase_words(First, Last, Current, TokenPhraseWords, MatchingTokenPhraseWords) :-
	( First > Current ->
	  Next is Current + 1,
	  TokenPhraseWords = [_FirstPhraseWord|RestTokenPhraseWords],
	  get_matching_phrase_words(First, Last, Next, RestTokenPhraseWords, MatchingTokenPhraseWords)
	; Current > Last ->
	  MatchingTokenPhraseWords = []
	; TokenPhraseWords = [FirstPhraseWord|RestTokenPhraseWords],
	  MatchingTokenPhraseWords = [FirstPhraseWord|RestMatchingTokenPhraseWords],
	  Next is Current + 1,
	  get_matching_phrase_words(First, Last, Next, RestTokenPhraseWords, RestMatchingTokenPhraseWords)
	).	  

get_one_word_pos_info(MatchingPhraseWord, [FirstToken|RestTokens],
		      RawTokensOut, MatchingToken, RemainingTokens,
		      StartPos, Length) :-
	% get_one_word_pos_info must be backtrackable if we want to find PosInfo for
	% [transcription,factor] inside [nuclear,factor,kappab,transcription,factor,relb]
	( control_option(ignore_word_order) ->
	  select(MatchingToken, [FirstToken|RestTokens], RemainingTokens),
	  matching_token(lc, MatchingPhraseWord, MatchingToken),
	  get_pos_info_from_token(MatchingToken, StartPos, Length)
	; matching_token(lc, MatchingPhraseWord, FirstToken) ->
	  get_pos_info_from_token(FirstToken, StartPos, Length),
	  MatchingToken = FirstToken,
	  RemainingTokens = RestTokens
	; get_one_word_pos_info(MatchingPhraseWord, RestTokens,
				RawTokensOut,
				MatchingToken, RemainingTokens,
				StartPos, Length) ->
	  true
	; format(user_output,
		 '~n### ERROR: get_one_word_pos_info failed on ~q/~p~n',
		 [MatchingPhraseWord, [FirstToken|RestTokens]]),
	  abort
	).

get_rest_word_pos_info([], PhraseTokens, _RawTokensOut, [], PhraseTokens, []).
get_rest_word_pos_info([FirstMatchingPhraseWord|RestMatchingTokenPhraseWords],
		       PhraseTokensIn, RawTokensOut, [MatchingToken|RestMatchingTokens],
		       PhraseTokensOut, [ThisStartPos/ThisLength|RestPosInfo]) :-
	get_one_word_pos_info(FirstMatchingPhraseWord, PhraseTokensIn,
			      RawTokensOut, MatchingToken, PhraseTokensNext,
			      ThisStartPos, ThisLength),
	get_rest_word_pos_info(RestMatchingTokenPhraseWords, PhraseTokensNext,
			       RawTokensOut, RestMatchingTokens, PhraseTokensOut, RestPosInfo).

get_pos_info_from_token(tok(_Type, _TokenString, _LCTokenString, _Pos1, pos(StartPos,Length)),
			StartPos, Length).

% Matching-Case token match
matching_token(mc, MatchingPhraseWordAtom,
               tok(_Type, TokenString, _LCTokenString, _Pos1, _Pos2)) :-
        atom_codes(MatchingPhraseWordAtom, TokenString).

% Lower-Case token match
matching_token(lc, MatchingPhraseWordAtom,
               tok(Type, _TokenString, LCTokenString, _Pos1, _Pos2)) :-
        atom_codes(MatchingPhraseWordAtom, MatchingPhraseWordCodes),
	% apostrophe-s token should match the token w/o the apostrophe-s
	( Type = xx ->
	  ( append(MatchingPhraseWordCodes, [39,0's], LCTokenString) % 39 is apostrophe
	  ; MatchingPhraseWordCodes = LCTokenString
	  )
	; MatchingPhraseWordCodes = LCTokenString
	).	  


% compute_all_evaluations(MetaWords, DebugFlags, Label, UtteranceText, MetaTerm, MetaConcept,
% 			Variants, TokenPhraseWords, PhraseTokenLength,
% 			RawTokensOut, AAs, InputmatchPhraseWords,
% 			TokenHeadWords, PhraseTokens, Evaluations) :-
% 	% filter_out_multiple_meaning_designators(MetaWords0,MetaWords),
% 	debug_message(trace, '~N### computing ALL evaluations: ~q~n', [MetaWords]),
% 	compute_phrase_match(TokenHeadWords, Label, UtteranceText,
% 			     TokenPhraseWords, MetaWords, Variants, PhraseTokenLength,
% 			     MatchMap, InvolvesHead, IsOvermatch),
% 	% TempMatchMap = [MatchMapHead|MatchMapTail],
% 	% consolidate_matchmap(MatchMapTail, MatchMapHead, MatchMap),
% 	test_minimum_length(TokenPhraseWords, MatchMap),
%         % format(user_output, '~w:~w:~w~n', [MetaWords,MatchMap,PhraseTokenLength]),
% 	MatchMap \== [],
% 	compute_connected_components(MatchMap, MatchCCs),
% 	length( TokenPhraseWords, NTokenPhraseWords),
% 	length(MetaWords, NMetaWords),
% 	compute_extra_meta(MatchMap, MetaWords, ExtraMetaWords),
% 	( memberchk(5, DebugFlags) ->
% 	  format('~n~p~n~p~n~p~n~p~n~p~n',
% 		 [TokenPhraseWords,MetaWords,MatchMap,MatchCCs,ExtraMetaWords])
% 	; true
% 	),
% 	compute_match_value(MatchMap, MatchCCs, NTokenPhraseWords, NMetaWords,
% 			    ExtraMetaWords, Variants, InvolvesHead, Value),
% 	( memberchk(5, DebugFlags) ->
% 	  format(' <-- ~p~n',[MetaTerm])
% 	; true
% 	),
% 	NegValue is -Value,
% 	db_get_concept_cuis(MetaConcept, CUIs),
% 	form_evaluations(CUIs, NegValue, MetaTerm, MetaConcept, MetaWords, MatchMap,
% 			 TokenPhraseWords, PhraseTokens, RawTokensOut, AAs,
% 			 InputmatchPhraseWords, InvolvesHead, IsOvermatch, Evaluations).
% 
% form_evaluations([], _, _, _, _, _, _, _, _, _, _, _, _, []) :- !.
% form_evaluations([CUI|Rest], NegValue, MetaTerm, MetaConcept, MetaWords, MatchMap,
% 		 TokenPhraseWords, PhraseTokens, RawTokensOut, AAs,
% 		 InputmatchPhraseWords,
%                  InvolvesHead, IsOvermatch, [FirstEvaluation|RestEvaluations]) :-
% 	db_get_cui_sourceinfo(CUI, SourceInfo),
% 	extract_unique_sources(SourceInfo, [], UniqueSources),
%         compute_target_LS_component(MatchMap, LSComponents, TargetLSComponent),
% 	candidate_term(NegValue, CUI, MetaTerm, MetaConcept, MetaWords, _SemTypes,
% 		       MatchMap, LSComponents, TargetLSComponent, InvolvesHead,
% 		       IsOvermatch, UniqueSources, PosInfo, _Status, FirstEvaluation),
% 	get_all_pos_info(MatchMap, TokenPhraseWords, PhraseTokens, RawTokensOut, PosInfo),
% 	form_evaluations(Rest, NegValue, MetaTerm, MetaConcept, MetaWords, MatchMap,
% 			 TokenPhraseWords, PhraseTokens, RawTokensOut, AAs,
% 			 InputmatchPhraseWords,
% 			 InvolvesHead, IsOvermatch, RestEvaluations).

compute_extra_meta(MatchMap, MetaWords, ExtraMetaWords) :-
	extract_components(MatchMap, _PhraseComponents, MetaComponents),
	linearize_components(MetaComponents, LMetaComponents),
	append(LMetaComponents, MetaIndexes0),
	sort(MetaIndexes0, MetaIndexes),
	compute_extra_meta_aux(MetaWords, 1, MetaIndexes, ExtraMetaWords).

compute_extra_meta_aux([], _, _, []).
compute_extra_meta_aux([_First|Rest], N, [N|RestMetaIndexes], ComputedRest) :-
	!,
	M is N + 1,
	compute_extra_meta_aux(Rest, M, RestMetaIndexes, ComputedRest).
compute_extra_meta_aux([First|Rest], N, MetaIndexes, [First|ComputedRest]) :-
	M is N + 1,
	compute_extra_meta_aux(Rest, M, MetaIndexes, ComputedRest).

/* compute_phrase_match(+TokenHeadWords, +Label, +UtteranceText,
			+TokenPhraseWords, +MetaWords,
   		        +Variants, +PhraseTokenLength,
                        -MatchMap,-InvolvesHead, -IsOvermatch)
   compute_phrase_match_aux(+MetaWords, +Label, +UtteranceText,
   			    +MetaWords, +TokenPhraseWords,
    			    +NMeta, +Variants, +PhraseTokenLength,
                            +MatchMapIn, -MatchMapOut,
                            +InvolvesHeadIn, -InvolvesHeadOut)
*/

compute_phrase_match(TokenHeadWords, Label, UtteranceText,
		     TokenPhraseWords, MetaWords,
		     Variants, PhraseTokenLength,
                     MatchMap, InvolvesHead, IsOvermatch) :-
	( control_option(allow_overmatches) ->
	  true
	  % reversed order of args from QP library version!
	; last(MetaWords, Last),
	  word_is_last_word_of_some_variant(Last, Variants) ->
	  true
	% This is for the "breastfeeding" cases
	% ; MetaWords = [Word1,Word2],
	%   concat_atom(MetaWords, SingleAtom),
	%   split_word(SingleAtom, Word1, Word2),
	%   word_is_last_word_of_some_variant(SingleAtom, Variants) ->
	%   true
	),
	!,
	% Note: Whether InvolvesHead is yes if TokenHeadWords==[] has varied over time
	% I now think that it should in order to avoid the anomalous situation
	% where a headless phrase matching a string perfectly does not get a perfect score.
	get_gap_size_parameters(MinPhraseLength, MaxGapSize),
	% format(user_output, 'MW:~w~n', [MetaWords]),
	set_involves_head_in(TokenHeadWords, InvolvesHeadIn),
	NMeta0 is 1,
	MatchMap0 = [],
	compute_phrase_match_aux(MetaWords, Label, UtteranceText,
				 MetaWords, TokenPhraseWords,
				 NMeta0, Variants, PhraseTokenLength,
				 MinPhraseLength, MaxGapSize,
				 MatchMap0, MatchMap1,
				 InvolvesHeadIn, InvolvesHead),
	rev(MatchMap1, MatchMap),
	extract_components(MatchMap, _PhraseComponents, MetaComponents),
	length(MetaWords, NMetaWords),
	( component_intersects_components(MetaComponents, [1,1]),
	  component_intersects_components(MetaComponents, [NMetaWords,NMetaWords]) ->
	  IsOvermatch = no
	; ( control_option(allow_overmatches) ->
	    IsOvermatch = yes
	  ; fail
	  )
	).
	% format(user_output, 'MM: ~w~n~w~n~w~n~n', [MatchMap,MetaWords,TokenPhraseWords]).

set_involves_head_in([],    yes).
set_involves_head_in([_|_], no).

get_gap_size_parameters(MinPhraseLength, MaxGapSize) :-
	( control_value(gap_size,[MinPhraseLength,MaxGapSize]) ->
	  true
	; MinPhraseLength is 0,
	  MaxGapSize is 0
	).

compute_phrase_match_aux([], _Label, _UtteranceText, _AllMetaWords, _TokenPhraseWords,
			 _NMeta, _Variants, _PhraseTokenLength,
			 _MinPhraseLength, _MaxGapSize,
			 MatchMapIn, MatchMapIn,
			 InvolvesHeadIn, InvolvesHeadIn).
compute_phrase_match_aux([First|Rest], Label, UtteranceText, AllMetaWords, TokenPhraseWords,
			 NMeta, Variants, PhraseTokenLength,
			 MinPhraseLength, MaxGapSize,
			 MatchMapIn, MatchMapOut,
			 InvolvesHeadIn, InvolvesHeadOut) :-
	extract_components(MatchMapIn, PhraseComponents, _MetaComponents),
	get_one_from_avl(First, PhraseComponents, Variants, VariantInfo),
	VariantInfo = vinfo(_Generator,GeneratorPosition,GeneratorInvolvesHead,
			    Variant,[First|RestVariantWords]),
	get_previous_begin_and_end(MatchMapIn, PreviousBegin, _PreviousEnd),
	GeneratorPosition = [CurrentBegin|_],
	% allow_short_gaps_only(MinPhraseLength, MaxGapSize, Label, UtteranceText,
	% 		      AllMetaWords, TokenPhraseWords, PhraseTokenLength,
	% 		      CurrentBegin, PreviousEnd),
	( component_intersects_components(PhraseComponents, GeneratorPosition) ->
	  control_option(allow_concept_gaps),
	  MatchMapInOut = MatchMapIn,
	  InvolvesHeadInOut = InvolvesHeadIn,
	  NewNMeta is NMeta+1,
	  append(RestVariantWords,NewRest,Rest),
	  compute_phrase_match_aux(NewRest, Label, UtteranceText, AllMetaWords, TokenPhraseWords,
				   NewNMeta, Variants, PhraseTokenLength,
				   MinPhraseLength, MaxGapSize,
				   MatchMapInOut, MatchMapOut,
				   InvolvesHeadInOut, InvolvesHeadOut)
	; update_involves_head(GeneratorInvolvesHead, InvolvesHeadIn, InvolvesHeadInOut),
	  ( CurrentBegin < PreviousBegin,  % don't require for -i
	    \+ control_option(ignore_word_order) ->
	    MatchMapOut = MatchMapIn,
            InvolvesHeadOut = InvolvesHeadIn
	  ; length(RestVariantWords, NVariantWords),
	    End is NMeta + NVariantWords,
            Variant = v(_Word,_Categories,VarLevel,_,_,_),
            MatchMapInOut = [[GeneratorPosition,[NMeta,End],VarLevel] | MatchMapIn],
	    NewNMeta is NMeta + NVariantWords + 1,
            append(RestVariantWords, NewRest, Rest),
            compute_phrase_match_aux(NewRest, Label, UtteranceText,
				     AllMetaWords, TokenPhraseWords,
				     NewNMeta, Variants, PhraseTokenLength,
				     MinPhraseLength, MaxGapSize,
				     MatchMapInOut, MatchMapOut,
				     InvolvesHeadInOut, InvolvesHeadOut)
	  ),
	!
	).
compute_phrase_match_aux([_First|Rest], Label, UtteranceText, AllMetaWords, TokenPhraseWords,
			 NMeta, Variants, PhraseTokenLength,
			 MinPhraseLength, MaxGapSize,
			 MatchMapIn,MatchMapOut,
			 InvolvesHeadIn,InvolvesHeadOut) :-
	( control_option(allow_overmatches) ->
	  true
	; control_option(allow_concept_gaps)
	),
	NewNMeta is NMeta + 1,
	compute_phrase_match_aux(Rest, Label, UtteranceText, AllMetaWords, TokenPhraseWords,
				 NewNMeta, Variants, PhraseTokenLength,
				 MinPhraseLength, MaxGapSize,
				 MatchMapIn, MatchMapOut,
				 InvolvesHeadIn, InvolvesHeadOut).

update_involves_head(GeneratorInvolvesHead, InvolvesHeadIn, InvolvesHeadInOut) :-
	( GeneratorInvolvesHead == yes ->
	  InvolvesHeadInOut = yes
	; InvolvesHeadInOut = InvolvesHeadIn
	).

get_previous_begin_and_end(MatchMap, PreviousBegin, PreviousEnd) :-
	  ( MatchMap = [[PreviousGeneratorPosition|_]|_] ->
	    PreviousGeneratorPosition = [PreviousBegin,PreviousEnd]
	  ; PreviousBegin = 0,
	    PreviousEnd   = 0
	  ).

%%% % If the phrase contains fewer than 8 tokens, don't bother checking the size of gaps;
%%% % if PreviousBegin == 0, this is the first MatchMap, so there's nothing to check;
%%% % otherwise, ensure that the gap between this word and the previous does not exceed 5.
%%% allow_short_gaps_only(MinPhraseLength, MaxGapSize, _Label, _UtteranceText,
%%% 		      _MetaWords, _TokenPhraseWords,
%%% 		      PhraseTokenLength, CurrentBegin, PreviousEnd) :-
%%% 	( control_option(ignore_word_order) ->
%%% 	  true
%%% 	  % MinPhraseLength =:= 0 means that
%%% 	  % the gap_size control option is not set
%%% 	; MinPhraseLength =:= 0 ->
%%% 	  true
%%% 	  % If the phrase isn't at least as long as MinPhraseLength,
%%% 	  % don't check anything.
%%% 	; PhraseTokenLength < MinPhraseLength ->
%%% 	  true
%%% 	  % If PreviousEnd =:= 0, this is the first MatchMap,
%%% 	  % so there's nothing to check.
%%% 	; PreviousEnd =:= 0 ->
%%% 	  true
%%% 	; CurrGapSize is abs(CurrentBegin - PreviousEnd) - 1,
%%% 	  % Gap =< PhraseTokenLength div 2
%%% 	  % Now, finally, if we get to here,
%%% 	  % verify that the current gap size is no longer than MaxGapSize.
%%% 	  CurrGapSize =< MaxGapSize
%%% 	),
%%% 	!.
%%% allow_short_gaps_only(MinPhraseLength, MaxGapSize, Label, UtteranceText,
%%% 		      MetaWords, TokenPhraseWords,
%%% 		      PhraseTokenLength, CurrentBegin, PreviousEnd) :-
%%% 	CurrGapSize is abs(CurrentBegin - PreviousEnd) - 1,
%%% 	current_output(OutputStream),
%%% 	atom_codes_list(MetaWords, MetaWordsStrings),
%%% 	form_one_string(MetaWordsStrings, " ", MetaWordsSingleString),
%%% 	atom_codes(MetaWordsAtom, MetaWordsSingleString),
%%% 
%%% 	atom_codes_list(TokenPhraseWords, TokenPhraseWordsStrings),
%%% 	form_one_string(TokenPhraseWordsStrings, " ", TokenPhraseWordsSingleString),
%%% 	atom_codes(TokenPhraseWordsAtom, TokenPhraseWordsSingleString),
%%% 	debug_failed_short_gap_concept(OutputStream,
%%% 				       MetaWordsAtom, TokenPhraseWordsAtom,
%%% 				       Label, UtteranceText,  MinPhraseLength, PhraseTokenLength,
%%% 				       MaxGapSize, CurrGapSize, CurrentBegin, PreviousEnd),
%%% 	fail.
%%% 
%%% debug_failed_short_gap_concept(OutputStream,
%%% 			       MetaWordsAtom, TokenPhraseWordsAtom,
%%% 			       Label, UtteranceText, MinPhraseLength, PhraseTokenLength,
%%% 			       MaxGapSize, CurrGapSize, CurrentBegin, PreviousEnd) :-
%%% 	format(OutputStream,
%%% 	       '### ~w|~w|~w|~w|Phrase: ~w/~w|Gap: ~w/~w (~w-~w)~n',
%%% 	       [MetaWordsAtom,TokenPhraseWordsAtom,
%%% 		Label,UtteranceText, MinPhraseLength,PhraseTokenLength,
%%% 		MaxGapSize,CurrGapSize,CurrentBegin,PreviousEnd]).

/* word_is_last_word_of_some_variant(+Word, +Variants)

word_is_lasts_word_of_some_variant/2 succeeds if Word is the last word of a
variant in Variants.  */

word_is_last_word_of_some_variant(Word, Variants) :-
	% first try the word itself as a key
	get_one_from_avl(Word, [], Variants,
			 vinfo(_Generator,_GeneratorPosition,_GeneratorInvolvesHead,
			       _Variant,VariantWords)),
	% reversed order of args from QP library version!
	last(VariantWords, Word),
	!.
word_is_last_word_of_some_variant(Word, Variants) :-
	avl_member(_Key, Variants, Values),
	member(vinfo(_Generator,_GeneratorPosition,
		     _GeneratorInvolvesHead,_Variant,VariantWords),
	       Values),
	% reversed order of args from QP library version!
	last(VariantWords, Word),
	!.


/* get_one_from_avl(+Key, +PhraseComponents, +AVL, -Value)

get_one_from_avl/3 looks up Key in AVL.  The value for Key is assumed to be
a list of values in reverse order.  get_one_from_avl/3 returns one Value,
returning the next on backtracking.  PhraseComponents is used to reorder
the values preferring those which do not intersect PhraseComponents.  */

get_one_from_avl(Key,PhraseComponents,AVL,Value) :-
    avl_fetch(Key,AVL,RevValues),
    rev(RevValues,Values0),
    % Values0 = RevValues,
    reorder_by_phrase_components(Values0,PhraseComponents,Values),
    % ( member(X, Values), write('VARIANT':X), nl, fail ; nl),
    member(Value,Values).

reorder_by_phrase_components([],_,[]) :-
    !.
reorder_by_phrase_components(Values,PhraseComponents,ReorderedValues) :-
    split_by_phrase_components(Values,PhraseComponents,
                               [],RevNonIntersectValues,[],RevIntersectValues),
    rev(RevNonIntersectValues,NonIntersectValues),
    rev(RevIntersectValues,IntersectValues),
    append(NonIntersectValues,IntersectValues,ReorderedValues).

split_by_phrase_components([],_,RNIVIn,RNIVIn,RIVIn,RIVIn).
split_by_phrase_components([Value|Rest],PhraseComponents,
                           RNIVIn,RNIVOut,RIVIn,RIVOut) :-
    Value=vinfo(_,GeneratorPosition,_,_,_),
    component_intersects_components(PhraseComponents,GeneratorPosition),
    !,
    split_by_phrase_components(Rest,PhraseComponents,
                               RNIVIn,RNIVOut,[Value|RIVIn],RIVOut).
split_by_phrase_components([Value|Rest],PhraseComponents,
                           RNIVIn,RNIVOut,RIVIn,RIVOut) :-
    !,
    split_by_phrase_components(Rest,PhraseComponents,
                               [Value|RNIVIn],RNIVOut,RIVIn,RIVOut).


/* compute_connected_components(+MatchMap, -MatchCCs)

compute_connected_components/2
xxx
*/

compute_connected_components(MatchMap, [PhraseCCs,MetaCCs]) :-
	extract_components(MatchMap, PhraseComponents, MetaComponents),
	connect_components(PhraseComponents, PhraseCCs),
	connect_components(MetaComponents, MetaCCs).


/* extract_components(+MatchMap, -PhraseComponents, -MetaComponents)

extract_components/3
xxx
*/

extract_components([], [], []).
extract_components([[PhraseComponent,MetaComponent|_]|Rest],
		   [PhraseComponent|RestPhraseComponents],
		   [MetaComponent|RestMetaComponents]) :-
	extract_components(Rest, RestPhraseComponents, RestMetaComponents).


/* connect_components(+Components, -CCs)

connect_components/2
xxx
*/

connect_components(Components, CCs) :-
	merge_contiguous_components(Components, MergedComponents),
	extract_ccs(MergedComponents, CCs).


/* merge_contiguous_components(+Components, -MergedComponents)

merge_contiguous_components/2
xxx
*/

merge_contiguous_components([], []).
merge_contiguous_components([[Begin1,End1],[Begin2,End2]|Rest], MergedComponents) :-
	% B1/E1 and B2/E2 are contiguous, so merge them into B1/E2
	Begin2 is End1 + 1,
	!,
	merge_contiguous_components([[Begin1,End2]|Rest], MergedComponents).
merge_contiguous_components([First|Rest], [First|MergedRest]) :-
	merge_contiguous_components(Rest, MergedRest).


/* extract_ccs(+Components, -CCs)

extract_ccs/2
xxx
*/

extract_ccs([], []).
extract_ccs([[Begin,End]|Rest], [Size|ExtractedRest]) :-
	Size is End - Begin + 1,
	extract_ccs(Rest, ExtractedRest).


/* component_intersects_components(+Component, +Components)

component_intersects_components/2
xxx
*/

component_intersects_components([First|Rest], Component) :-
	( positions_overlap(Component, First) ->
	  true
	; component_intersects_components(Rest, Component)
	).

/* compute_match_value(+MatchMap, +MatchCCs, +NTokenPhraseWords, +NMetaWords,
                       +ExtraMetaWords, +Variants, +InvolvesHead, -Value)

compute_match_value/8
xxx
*/

compute_match_value(MatchMap, MatchCCs, NTokenPhraseWords, NMetaWords,
                    ExtraMetaWords, Variants, InvolvesHead, Value) :-
	compute_centrality_value(InvolvesHead, CenValue),
	compute_variation_value(MatchMap, VarValue),
	compute_cohesiveness_value(MatchCCs, NTokenPhraseWords, NMetaWords, CohValue),
	( control_option(ignore_word_order) ->
	  compute_involvement_value(MatchMap, NTokenPhraseWords, NMetaWords,
				    ExtraMetaWords, Variants, InvValue),
	  combine_values(CenValue, VarValue, InvValue, CohValue, Value)
	; compute_coverage_value(MatchMap, NTokenPhraseWords, NMetaWords, CovValue),
          combine_values(CenValue, VarValue, CovValue, CohValue, Value)
	),
	( control_value(debug, DebugFlags),
	  memberchk(5, DebugFlags) ->
	  ( var(CovValue) ->
	    CovValue = 0.0
	  ; true
	  ),
	  ( var(InvValue) ->
	    InvValue = 0.0
	  ; true
	  ),
	  format('=~t~d~5| ~~ ~2f, ~2f, ~2f, ~2f, ~2f',
		 [Value,CenValue,VarValue,CovValue,CohValue,InvValue])
	; true
	).

/* compute_centrality_value(+InvolvesHead, -CenValue)

compute_centrality_value/2
xxx
*/

compute_centrality_value(yes, 1.0).
compute_centrality_value(no,  0.0).


/* compute_variation_value(+MatchMap, -VarValue)

compute_variation_value/4
xxx
*/

compute_variation_value(MatchMap, VarValue) :-
	extract_variation_values(MatchMap, Values),
	length(Values, NValues),
	% ( NValues =:= 0 ->
	%   VarValue is 0.0
	% ; compute_sum(Values, 0.0, Sum),
	compute_sum(Values, 0.0, Sum),
        VarValue is Sum/NValues.

/* extract_variation_values(+MatchMap, -Values)

extract_variation_values/2
xxx
*/

extract_variation_values([], []).
extract_variation_values([[_,_,VarLevel]|Rest], [VariationValue|ExtractedRest]) :-
	convert_variation_level_to_value(VarLevel, VariationValue),
	extract_variation_values(Rest, ExtractedRest).


/* convert_variation_level_to_value(+VarLevel, -VariationValue)

convert_variation_level_to_value/2
xxx
*/

% Normal computation
convert_variation_level_to_value(VarLevel, VariationValue) :-
	VariationValue is 4/(VarLevel+4).

% Canonicalization computation
%convert_variation_level_to_value(0,1.0) :-
%    !.
%convert_variation_level_to_value(_VarLevel,0.0).


/* compute_coverage_value(+MatchMap, +NTokenPhraseWords, +NMetaWords, -CovValue)

compute_coverage_value/4
xxx
*/

compute_coverage_value(MatchMap, NTokenPhraseWords, NMetaWords, CovValue) :-
	extract_components(MatchMap, PhraseComponents, MetaComponents),
	compute_bounds(PhraseComponents, PhraseLB, PhraseUB),
	PhraseSpan is PhraseUB - PhraseLB + 1,
	compute_bounds(MetaComponents, MetaLB, MetaUB),
	MetaSpan is MetaUB - MetaLB + 1,
	CovValue is (PhraseSpan/NTokenPhraseWords + (2*MetaSpan)/NMetaWords)/3.0.


/* compute_bounds(+Components, -LowerBound, -UpperBound)
   compute_bounds(+Components, +LowerBoundIn, -LowerBoundOut, +UpperBoundIn,
                  -LowerBoundOut)

compute_bounds/3
compute_bounds/5
xxx
*/

compute_bounds([], 0, -1).
compute_bounds([[Begin,End]|Rest], LB, UB) :-
	compute_bounds(Rest, Begin, LB, End, UB).

compute_bounds([], LBIn, LBIn, UBIn, UBIn).
compute_bounds([[Begin,End]|Rest], LBIn, LBOut, UBIn, UBOut) :-
	( Begin < LBIn ->
	  LBNext = Begin
	; LBNext = LBIn
	),
	( End > UBIn ->
	  UBNext = End
	; UBNext = UBIn
	),
	compute_bounds(Rest, LBNext, LBOut, UBNext, UBOut).


/* compute_strict_coverage_value(+MatchMap, +NTokenPhraseWords, +NMetaWords,
                                 -CovValue)

compute_strict_coverage_value/4
xxx
*/

% temp eval
%compute_strict_coverage_value(MatchMap,NTokenPhraseWords,NMetaWords,SCovValue) :-
%    extract_components(MatchMap,PhraseComponents,MetaComponents),
%    compute_number_involved(PhraseComponents,PhraseN),
%    compute_number_involved(MetaComponents,MetaN),
%    SCovValue is (PhraseN/NTokenPhraseWords + (2*MetaN)/NMetaWords)/3.0.
%
%compute_number_involved(Components,N) :-
%    compute_number_involved_aux(Components,NList),
%    sumlist(NList,N).
%
%compute_number_involved_aux([],[]).
%compute_number_involved_aux([[Begin,End]|Rest],[N|ComputedRest]) :-
%    N is End + 1 - Begin,
%    compute_number_involved_aux(Rest,ComputedRest).


/* compute_cohesiveness_value(+MatchCCs, +NTokenPhraseWords, +NMetaWords, -CohValue)

compute_cohesiveness_value/4
xxx
*/

compute_cohesiveness_value([PhraseCCs,PhraseCCs], NTokenPhraseWords, NMetaWords, CohValue) :-
	!,
	compute_squares(PhraseCCs, PhraseCCsSquared),
	compute_sum(PhraseCCsSquared, 0.0, Sum),
	( control_option(prefer_multiple_concepts) ->
	  CohValue is 1.0 - ((Sum/(NTokenPhraseWords*NTokenPhraseWords) +
			     (2*Sum)/(NMetaWords*NMetaWords))/3.0)
	; CohValue is (Sum/(NTokenPhraseWords*NTokenPhraseWords) +
		      (2*Sum)/(NMetaWords*NMetaWords))/3.0
	).
compute_cohesiveness_value([PhraseCCs,MetaCCs], NTokenPhraseWords, NMetaWords, CohValue) :-
	compute_squares(PhraseCCs, PhraseCCsSquared),
	compute_sum(PhraseCCsSquared, 0.0, PhraseSum),
	compute_squares(MetaCCs, MetaCCsSquared),
	compute_sum(MetaCCsSquared, 0.0, MetaSum),
	( control_option(prefer_multiple_concepts) ->
	  CohValue is 1.0 - ((PhraseSum/(NTokenPhraseWords*NTokenPhraseWords) +
			     (2*MetaSum)/(NMetaWords*NMetaWords))/3.0)
	; CohValue is (PhraseSum/(NTokenPhraseWords*NTokenPhraseWords) +
		      (2*MetaSum)/(NMetaWords*NMetaWords))/3.0
	).

/* compute_involvement_value(+PhraseCForms, +MetaCForms, -InvValue)

compute_involvement_value/3
xxx
*/

% temp eval
%compute_involvement_value(PhraseCForms,MetaCForms,InvValue) :-
%    ord_intersection(PhraseCForms,MetaCForms,InvolvedCForms),
%    length(PhraseCForms,NPhraseCForms),
%    length(MetaCForms,NMetaCForms),
%    length(InvolvedCForms,NInvolvedCForms),
%    InvValue is (NInvolvedCForms/NPhraseCForms
%               + (2*NInvolvedCForms)/NMetaCForms)/3.0.

compute_involvement_value(MatchMap, NTokenPhraseWords, NMetaWords,
                          ExtraMetaWords, Variants, InvValue) :-
	extract_components(MatchMap, PhraseComponents, MetaComponents),
	linearize_components(PhraseComponents, LPhraseComponents),
	append(LPhraseComponents, PhraseIndexes),
	length(PhraseIndexes, NPhrase),
	linearize_components(MetaComponents, LMetaComponents),
	append(LMetaComponents, MetaIndexes),
	length(MetaIndexes, NMeta),
	filter_by_variants(ExtraMetaWords, Variants, RelevantExtraMetaWords),
	( control_value(debug, DebugFlags),
	  memberchk(5, DebugFlags),
	  RelevantExtraMetaWords\==[] ->
	  format('Relevant extras: ~p~n',[RelevantExtraMetaWords])
	; true
	),
	length(RelevantExtraMetaWords, NExtra),
	( control_value(debug, DebugFlags),
	  memberchk(6, DebugFlags) ->
	  format('NPInvolved,NEx,NP,NMInvolved,NEx,NM: ~d ~d ~d ~d ~d ~d~n',
		 [NPhrase,NExtra,NTokenPhraseWords,NMeta,NExtra,NMetaWords])
	; true
	),
	TotalP is NPhrase + NExtra,
	TotalM is NMeta + NExtra,
	( TotalP > NTokenPhraseWords ->  % phrase involvement is being overcounted
          InvValue is (NPhrase/NTokenPhraseWords + NMeta/NMetaWords)/2.0
	  % (1.0 + TotalM/NMetaWords)/2.0
	  ;   InvValue is (TotalP/NTokenPhraseWords + TotalM/NMetaWords)/2.0
	).

filter_by_variants([], _Variants, []).
filter_by_variants([First|Rest],Variants, [First|FilteredRest]) :-
	avl_fetch(First, Variants, _),
	!,
	filter_by_variants(Rest, Variants, FilteredRest).
filter_by_variants([_First|Rest], Variants, FilteredRest) :-
	filter_by_variants(Rest, Variants, FilteredRest).


/* compute_squares(+Values, -ValuesSquared)

compute_squares/2
xxx
*/

compute_squares([], []).
compute_squares([First|Rest], [FirstSquared|RestSquared]) :-
	FirstSquared is First * First,
	compute_squares(Rest, RestSquared).


/* combine_values(+CenValue, +VarValue, +CovValue, +CohValue, -Value)

combine_values/5
xxx
*/


combine_values(CenValue, VarValue, CovValue, CohValue, Value) :-
	Value0 is (CenValue + VarValue + 2.0*(CovValue + CohValue))/6.0,
	Value is integer(1000*Value0).

% made obsolete since involvement only replaces coverage, not also
% cohesiveness; this allows -iY (i.e., both --ignore_word_order and
% --prefer_multiple_concepts) to have meaning where -i says to
% replace coverage with involvement, but -Y says to invert cohesiveness
%combine_values(CenValue,VarValue,InvValue,Value) :-
%    Value0 is (CenValue + VarValue + 4.0*InvValue)/6.0,
%    Value is integer(1000*Value0).


debug_evaluate_candidate_list_1(DebugFlags, MetaCanonical,MetaString,MetaConcept) :-
	( memberchk(3, DebugFlags) ->
	  format('::~p  ~p  ~p::', [MetaCanonical,MetaString,MetaConcept])
	; true
	).

debug_evaluate_candidate_list_2(DebugFlags) :-
	( memberchk(3, DebugFlags) ->
	  format('  DUP~n',[])
	; true
	).

debug_evaluate_candidate_list_3(DebugFlags, Evaluation) :-
	  ( memberchk(3, DebugFlags) ->
	    format('  YES~n    ~p~n',[Evaluation])
	  ; true
	  ).


debug_evaluate_candidate_list_4(DebugFlags) :-
	  ( memberchk(3, DebugFlags) ->
	    format('  NO~n',[])
	  ; true
	  ).


debug_evaluate_candidate_list_5(DebugFlags, NewEvaluations) :-
	  ( control_value(debug, DebugFlags),
	    memberchk(3, DebugFlags) ->
	    format('  YES~n    ~p~n',[NewEvaluations])
	  ; true
	  ).


debug_evaluate_candidate_list_6(DebugFlags) :-
	  ( control_value(debug, DebugFlags),
	    memberchk(3, DebugFlags) ->
	    format('  NO~n',[])
	  ; true
	  ).


debug_compute_one_evaluation_1(DebugFlags, TokenPhraseWords, MetaWords,
			       MatchMap,MatchCCs,ExtraMetaWords) :-
	( memberchk(5, DebugFlags) ->
	  format('~n~p~n~p~n~p~n~p~n~p~n',
		 [TokenPhraseWords,MetaWords,MatchMap,MatchCCs,ExtraMetaWords])
	; true
	).

debug_compute_one_evaluation_2(DebugFlags, MetaTerm) :-
	( memberchk(5, DebugFlags) ->    % see compute_match_value
	  format(' <-- ~p~n',[MetaTerm])
	; true
	).
