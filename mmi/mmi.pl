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

% File:     mmi.pl
% Module:   MetaMap Indexing (MMI)
% Author:   Lan
% Purpose:  Ranks concepts found by MetaMap in biomedical text (MEDLINE
%           citations).

:- module(mmi,[
	do_MMI_processing/4
    ]).

:- use_module(lexicon(qp_lexicon),[
	concat_atoms_intelligently/2
    ]).

:- use_module(metamap(metamap_evaluation),[
	extract_components/3,
	merge_contiguous_components/2
    ]).

:- use_module(metamap(metamap_tokenization),[
	local_punct/1,
	local_ws/1,
	tokenize_text/2,
	tokenize_text_more/2,
	tokenize_text_utterly/2
    ]).

:- use_module(skr(skr_utilities), [
	conditionally_skr_begin_write/2,
   	conditionally_skr_end_write/2,
	fatal_error/2,
	get_all_candidate_features/3,
	get_candidate_feature/3
    ]).

:- use_module(skr_db(db_access),[
	initialize_db_access/3,
	db_get_mesh_mh/2,
	db_get_meta_mesh/2,
	db_get_mesh_tc_relaxed/2
    ]).

:- use_module(skr_lib(nls_strings),[
	atom_codes_list/2,
	split_string/4,
	split_string_completely/3,
	trim_and_compress_whitespace/2
    ]).

:- use_module(skr_lib(nls_system), [
	control_option/1,
	control_value/2
    ]).

:- use_module(skr_lib(pos_info), [
	collapse_pos_info/3
    ]).

:- use_module(skr_lib(sicstus_utils),[
	concat_atom/3,
	concat_strings_with_separator/3,
	lower/2,
	upper/2
   ]).

:- use_module(skr(skr_utilities), [
	debug_message/3,
	ensure_number/2,
	skr_begin_write/1,
	skr_end_write/1,
	token_template/5
    ]).

:- use_module(skr(skr_xml), [
	xml_output_format/1
    ]).

:- use_module(text(text_object_tokens),[
	all_alnum/1
   ]).		     

:- use_module(text(text_objects),[
	dump_all_avl/4,
	extract_token_strings/2,
	reformat_aa_output_list/3
   ]).		     

:- use_module(library(avl),[
	avl_member/3,
	avl_to_list/2
    ]).

:- use_module(library(lists),[
	append/2,
	last/2,
	max_member/2,
	rev/2,
	% selectchk/3,
	subseq0/2,
	sumlist/2
    ]).

:- use_module(library(system), [
        environ/2
   ]).

do_MMI_processing(OrigUtterances, BracketedOutput, AAs, DisambMMOutput) :-
	% Do MMI processing, if requested
	( control_option(fielded_mmi_output) ->
	  conditionally_skr_begin_write(BracketedOutput, 'MMI'),
          current_output(OutputStream),
	  get_UIAtom(OrigUtterances, UIAtom),
	  % The "pmid" argument specifies that the PMID should be the first field in he AA output
	  dump_all_avl(AAs, pmid, UIAtom, OutputStream),
	  process_citation(UIAtom, DisambMMOutput, OutputStream),
	  conditionally_skr_end_write(BracketedOutput, 'MMI')
        ; true
	).

get_UIAtom(OrigUtterances, UIAtom) :-
	OrigUtterances = [FirstUtterance|_],
	FirstUtterance = utterance(UtteranceIDAtom,_,_,_),
	atom_codes(UtteranceIDAtom, UtteranceIDString),
	append([PMIDString, ".", _Rest], UtteranceIDString),
	!,
	atom_codes(UIAtom, PMIDString).

% conditionally_skr_begin_write(BracketedOutput, Message) :-
% 	  ( BracketedOutput =:= 1 ->
% 	    skr_begin_write(Message)
% 	  ; true
% 	  ).

% conditionally_skr_end_write(BracketedOutput) :-
% 	  ( BracketedOutput =:= 1 ->
% 	    skr_end_write(Message)
% 	  ; true
% 	  ).

% 2000 parameters
% NOTE: These are weighting parameters only, and do not need to be changed when MeSH changes.
% MeSH-specific parameters are handled by the three environment variables
% * MMI_TREE_DEPTH_SPECIFICITY_DIVISOR,
% * MMI_WORD_SPECIFICITY_DIVISOR, and
% * MMI_CHARACTER_SPECIFICITY_DIVISOR,
% which are set in SKRenv.XX and SKRrun.XX
% and used in compute_specificities/12 below.

processing_parameter(nc,    0).   % character normalization index
processing_parameter(nf,   -5).   % frequency normalization index
processing_parameter(nm,    0).   % MeSH normalization index
processing_parameter(nmm, -10).   % MetaMap normalization index
processing_parameter(nw,    0).   % word normalization index
processing_parameter(nz,    0).   % final normalization index
processing_parameter(wc,    0).   % character count weight
processing_parameter(wd,    1).   % default tree depth
processing_parameter(wm,   14).   % MeSH tree depth weight
processing_parameter(wmm,   1).   % MetaMap weight
processing_parameter(ww,    0).   % word count weight

max_freq(13).

/* process_citation(+OrigUtterances, UIAtom, +Sentences, MMOutput, +Stream) */

process_citation(UIAtom, MMOutput, FieldedStream) :-
	max_freq(MaxFreqIn),
	compute_mesh_in_text(MMOutput, MaxFreqIn, TFInfo, MaxFreqOut),
	% format(user_output, '~p~n', [TFInfo]),
	process_tf(TFInfo, MaxFreqOut, AATFInfo),
	% format(user_output, '~p~n', [AATFInfo]),
        dump_aatf_info_fielded(AATFInfo, UIAtom, FieldedStream),
	!.
process_citation(UIAtom, _MMOutput, _FieldedStream) :-
	fatal_error('process_citation/6 failed for UI ~a.~n',[UIAtom]).

% WARNING: This is actually a misnomer; the real goal here is the treecodes
%          but the concept can be non-MeSH unless --restrict_to_mesh is on
%          This operation will eventually be generalized to use other
%          hierarchies.
% for use with debug version (arity 3)
%compute_mesh_in_text(MMOutput,TFInfo) :-
%    compute_mesh_in_text(user_output,MMOutput,TFInfo).

% compute_mesh_in_text(ReportStream,MMOutput,TFInfo) :-
compute_mesh_in_text(MMOutput, MaxFreqIn, TFInfo, MaxFreqOut) :-
        get_pre_tf_in_all_utterances(MMOutput, PreTFInfo0),
	sort(PreTFInfo0, PreTFInfo),
	compute_mesh_in_textfields(PreTFInfo, MaxFreqIn, TFInfo0, MaxFreqOut),
	sort(TFInfo0, TFInfo1),
	collapse_tf(TFInfo1, TFInfo).

get_pre_tf_in_all_utterances([], []).
get_pre_tf_in_all_utterances([Utterance|Rest], PreTFInfo) :-
	get_pre_tf_in_utterance(Utterance, UtterancePreTFInfo),
	append(UtterancePreTFInfo, RestPreTFInfo, PreTFInfo),
	get_pre_tf_in_all_utterances(Rest, RestPreTFInfo).

get_pre_tf_in_utterance(Utterance, PreTFInfo) :-
	Utterance = mm_output(utterance(Label,_Text,_PosInfo,_ReplPos),
			      CitationTextAtom,_ModifiedText,_Tagging,AAs,
			      _Syntax,Phrases,_ExtractedPhrases),
	determine_field_nsent(Label, Field, NSent),
	avl_to_list(AAs, AAList),
	UI = 'empty',
	reformat_aa_output_list(AAList, UI, SortedAAList),
	get_pre_tf_in_phrases(Phrases, CitationTextAtom, SortedAAList, Field, NSent, PreTFInfo).

determine_field_nsent(Label, Field, NSent) :-
	atom_codes(Label, LabelString),
	append([_X,":",_Y,":",FieldString,":",NString],LabelString),
	!,
	atom_codes(Field, FieldString),
	( number_codes(NSent,NString) ->
	  true
	; NSent = 0
	).
determine_field_nsent(Label, Field, NSent) :-
	atom_codes(Label, LabelString),
	% We want the last two period-delimited components of LabelString
	split_string_completely(LabelString, ".", SplitLabelString),
	append(_, [FieldString,NString], SplitLabelString),
	% append([_X,".",FieldString,".",NString],LabelString),
	!,
	atom_codes(Field, FieldString),
	( number_codes(NSent, NString) ->
	  true
	; NSent = 0
	).

%  add_breakpoint([pred(mmi:get_lexcat/6), goal(_:G), true(arg(6,G,'UNKNOWN'))]-[exit,print,ask], BID).

get_pre_tf_in_phrases([], _CitationTextAtom, _SortedAAList, _Field, _NSent, []).
get_pre_tf_in_phrases([Phrase|RestPhrases], CitationTextAtom, SortedAAList, Field, NSent, PreTFInfo) :-
	get_pre_tf_in_phrase(Phrase, CitationTextAtom, SortedAAList, Field, NSent, PhrasePreTFInfo),
	append(PhrasePreTFInfo, RestPreTFInfo, PreTFInfo),
	get_pre_tf_in_phrases(RestPhrases, CitationTextAtom, SortedAAList, Field, NSent, RestPreTFInfo).

get_pre_tf_in_phrase(Phrase, CitationTextAtom, SortedAAList, Field, NSent, PreTFInfo) :-
	% extract the mappings component, which is a list of terms of the form
	% map(-888,[
	%  ev(-694,'C0025545','Metallothionein','Metallothionein',
	%      [metallothionein],[aapp,bacs],[[[1,1],[1,1],0]],no,no,Srcs,PI)])
	phrase_info(mappings, Phrase, mappings(AllMappings)),
	phrase_info(phrase,   Phrase, phrase(_TextAtom,Syntax,_Pos,_ReplPos)),
	extract_inputmatch_and_tag_pairs(Syntax, InputMatchTagPairs0),
	sort(InputMatchTagPairs0, InputMatchTagPairs1),
	% shouldn't be necessary any more
	% remove_punct_pairs(InputMatchTagPairs1, InputMatchTagPairs),
	InputMatchTagPairs = InputMatchTagPairs1,
        form_super_mapping(AllMappings, SuperMapping),
	get_pre_tf_in_mapping(SuperMapping, InputMatchTagPairs,
			      CitationTextAtom, SortedAAList, Field, NSent, PreTFInfo).

remove_punct_pairs([], []).
remove_punct_pairs([Token-LexCat|RestPairs], NoPunctPairs) :-
	( atom_codes(Token, [Code]),
	  local_punct(Code) ->
	  NoPunctPairs = RestNoPunctPairs
	; NoPunctPairs = [Token-LexCat|RestNoPunctPairs]
	),
	remove_punct_pairs(RestPairs, RestNoPunctPairs).


extract_inputmatch_and_tag_pairs([], []).
extract_inputmatch_and_tag_pairs([Element|RestElements], Pairs) :-
	arg(1, Element, Attributes),
	( Element \= punc(_),
	  memberchk(inputmatch(InputMatchList), Attributes),
	  get_tag(Attributes, Tag) ->
	  concat_atoms_intelligently(InputMatchList, InputMatchToken),
	  tokenize_text_utterly(InputMatchToken, [First|_]),
	  add_pairs([First,InputMatchToken|InputMatchList], Tag, Pairs, RestPairs)
	; Pairs = RestPairs
	),
	extract_inputmatch_and_tag_pairs(RestElements, RestPairs).

add_pairs([], _Tag, Pairs, Pairs).
add_pairs([H|T], Tag, [LowerHNoApostropheS-Tag|RemainingPairs], RestPairs) :-
	lower(H, LowerH),
	remove_apostrophe_s(LowerH, LowerHNoApostropheS),
	add_pairs(T, Tag, RemainingPairs, RestPairs).

remove_apostrophe_s(LowerH, LowerHNoApostropheS) :-
	( atom_concat(LowerHNoApostropheS, '''s', LowerH) ->
	  true
	; LowerHNoApostropheS = LowerH
	).

get_tag(FeatureList, Tag) :-
	( memberchk(tag(Tag), FeatureList) ->
	  true
	; memberchk(features([Tag|_]), FeatureList) ->
	  true
	; Tag = other
	).

% Collect the ev(_) terms from all mappings.
% Previously, this code had exluded mappings other than those with the highest
% (absolute value) score. We decided on 10/27/2009, however, to collect ev(_) terms
% from *all* mappings. Why? Because if compute_all_mappings is NOT on, the only
% remaining mappings will be those with the highest score, so there's no point in
% checking for highest-scoring mappings. If, however, compute_all_mappings IS on,
% we still want ev(_) terms from *all* mappings, so there's *still* no point in checking
% for highest-scoring mappings!
form_super_mapping(Mappings, SuperMapping) :-
	extract_evs(Mappings, EVs),
	append(EVs, TempMappings),
	sort(TempMappings, SuperMapping).

extract_evs([], []).
extract_evs([map(_Score,CandidateList)|RestMaps], [CandidateList|RestMappings]) :-
	  extract_evs(RestMaps, RestMappings).

% assemble the tf0(_) term
% A mapping is a list of candidates; each candidate is a term of the form
% ev(NegValue, CUI, MetaTerm, MetaConcept, MetaWords, STs, MatchMap,
%    InvolvesHead, IsOvermatch, SourceInfo, PosInfo, Status, Negated),

% the tf0 term is of the form
% tf0(ConceptString,STs,TermString,Value,Text,Field,CUI,NSent,PosInfo,LexCat,Negated)
% where
% ConceptString = stringified MetaConcept (preferred name of concept)
% STs           = STs from ev term
% TermString    = stringified MetaTerm (possibly non-preferred name of concept)
% Value         = -NegValue
% Text          = text in citation
% Field         = ti or ab
% CUI           = CUI from ev term
% NSent         = index of utterance in TI or AB
% PosInfo       = PosInfo of concept
% LexCat        = Lexical Category of concept
% Negated       = 1/0 depending on NegEx status of concept

get_pre_tf_in_mapping([], _Syntax, _CitationTextAtom, _SortedAAList, _Field, _NSent, []).
get_pre_tf_in_mapping([Candidate|Rest], Syntax, CitationTextAtom,
		      SortedAAList, Field, NSent, [TF0|RestPreTFInfo]) :-
	get_pre_tf_in_candidate(Candidate, Syntax, CitationTextAtom, SortedAAList, Field, NSent, TF0),
	get_pre_tf_in_mapping(Rest, Syntax, CitationTextAtom, SortedAAList, Field, NSent, RestPreTFInfo).


get_pre_tf_in_candidate(Candidate, Syntax, CitationTextAtom, SortedAAList, Field, NSent, TF0) :-
	get_all_candidate_features([negvalue,cui,metaterm,metaconcept,
				    metawords,semtypes,posinfo,negated],
				   Candidate,
				   [NegValue,CUI,MetaTerm,MetaConcept,
				    MetaWords,STs,PosInfo,Negated]),
	% Candidate = ev(NegValue,CUI,MetaTerm,MetaConcept,_MetaWords,STs,_MatchMap,
	% 	       _InvolvesHead,_IsOvermatch,_SourceInfo,PosInfo,_Pruned),
	Value is -NegValue,
	atom_codes(MetaConcept, ConceptString),
	atom_codes(MetaTerm, TermString),
	PosInfo = [H|T],
	collapse_pos_info(T, H, CollapsedPosInfo),
	extract_text_string(CollapsedPosInfo, CitationTextAtom, TextString),
	get_lexcat(TextString, SortedAAList, MetaWords, Syntax, Candidate, LexCat),
	TF0 = tf0(ConceptString,STs,TermString,Value,TextString,Field,
		  CUI,NSent,CollapsedPosInfo,LexCat,Negated),
	!.
get_pre_tf_in_candidate(Candidate, _Syntax, _CitationTextAtom, _SortedAAList, _Field, _NSent, _TF0) :-
	get_candidate_feature(metaconcept, Candidate, MetaConcept),
	% Candidate = ev(_NegValue,_CUI,_MetaTerm,MetaConcept,_MetaWords,_SemTypes,
	% 	       _MatchMap,_InvolvesHead,_IsOvermatch,_SourceInfo,_PosInfo),
	fatal_error('get_pre_tf_in_mapping/5 failed for ~p~n', [MetaConcept]).


remove_non_alnum_atoms([], []).
remove_non_alnum_atoms([H|T], Tokens) :-
	atom_codes(H, CodesH),
	( all_alnum(CodesH) ->
	  Tokens = [H|RestTokens]
	; Tokens = RestTokens
	),
	remove_non_alnum_atoms(T, RestTokens).

lc_all([], []).
lc_all([H|T], [LowerH|LowerT]) :-
	lower(H, LowerH),
	lc_all(T, LowerT).


get_AA_expansion(TextString0, SortedAAList, Expansion, TextString) :-
	( SortedAAList \== [],
	  UI = empty,
	  atom_codes(TextAtom0, TextString0),
	  tokenize_text_utterly(TextAtom0, TokenizedTextAtom0),
	  member(Token, [TextAtom0|TokenizedTextAtom0]),
	  memberchk(UI-Token-ExpansionTextAtom-_AATokenLength-_AATextLength-
		   _ExpansionTokenLength-_ExpansionTextLength,
		    SortedAAList) ->
	  tokenize_text(ExpansionTextAtom, TokenizedExpansionTextAtom),
	  % English is a head-final language, so try to find the lexcat of the last word
	  rev(TokenizedExpansionTextAtom, RevTokenizedExpansionTextAtom),
	  Expansion = [ExpansionTextAtom|RevTokenizedExpansionTextAtom],
	  atom_codes(ExpansionTextAtom, ExpansionTextString),
	  TextString = ExpansionTextString
	; Expansion = [],
	  TextString = TextString0
	).

get_lexcat(TextString0, SortedAAList, MetaWords, Syntax, Candidate, LexCat) :-
	trim_and_compress_whitespace(TextString0, TextString1), 
	% If TextString0 is AA, replace it with its expansion,
	% for which there will presumably be a lexcat.
 	get_AA_expansion(TextString1, SortedAAList, Expansion, TextString),
	atom_codes(TextAtom1, TextString1),
	tokenize_text_utterly(TextAtom1, TokenizedAtom0),
	lower(TextString, TextStringLC),
	atom_codes(TextAtomLC, TextStringLC),
	tokenize_text_utterly(TextAtomLC, TokenizedTextAtomLC),
	% English is a head-final language, so try to find the lexcat of the last word
	rev(TokenizedTextAtomLC, RevTokenizedTextAtomLC),
	rev(MetaWords, RevMetaWords),
	append([Expansion, TokenizedAtom0,
		[TextAtomLC|RevTokenizedTextAtomLC], RevMetaWords], AtomWordList0),
	lc_all(AtomWordList0, AtomWordList2),
	remove_punct_and_ws(AtomWordList2, AtomWordList),
	( all_lexcats(AtomWordList, Syntax, AllLexCats),
	  sort(AllLexCats, SortedLexCats),
	  SortedLexCats = [_Weight-LexCat|_Rest] ->
	  true
	; LexCat = 'UNKNOWN',
	  fml_format(user_output, '### UNKNOWN lexcat for ~q|~q|~q~n', [AtomWordList,Candidate,Syntax])
	).

% The lower the weight, the better
lexcat_weight(noun, 0) :- !.
lexcat_weight(adj,  1) :- !.
lexcat_weight(verb, 2) :- !.
lexcat_weight(_,    3) :- !.


all_lexcats([], _Syntax, []).
all_lexcats([H|T], Syntax, AllLexCats) :-
	( memberchk(H-LexCat, Syntax) ->
	  lexcat_weight(LexCat, Weight),
	  AllLexCats = [Weight-LexCat|RestLexCats]
	; AllLexCats = RestLexCats
	),
	all_lexcats(T, Syntax, RestLexCats).


remove_punct_and_ws([], []).
remove_punct_and_ws([H|T], RemainingTokens) :-
	( atom_codes(H, [Code]),
	  ( local_punct(Code) ->
	    true
	  ; local_ws(Code)
	  ) ->
	  RemainingTokens = Rest
	; RemainingTokens = [H|Rest]
	),
	remove_punct_and_ws(T, Rest).

fml_format(A, B, C) :- format(A, B, C).

extract_text_string(PosInfoList, CitationTextAtom, TextString) :-
	extract_text_atoms(PosInfoList, CitationTextAtom, AtomList),
	concat_atoms_intelligently(AtomList, TextAtom),
	atom_codes(TextAtom, TextString).

extract_text_atoms([], _CitationTextAtom, []).
extract_text_atoms([StartPos/Length|RestPosInfo], CitationTextAtom, [Atom|RestAtoms]) :-
	% substring(CitationTextAtom, Atom, StartPos, Length),
	sub_atom(CitationTextAtom, StartPos, Length, _After, Atom),
	extract_text_atoms(RestPosInfo, CitationTextAtom, RestAtoms).

compute_mesh_and_treecodes(MetaConcept, TreeCodes) :-
	( db_get_meta_mesh(MetaConcept, Concept) ->
	  true
        ; db_get_mesh_mh(MetaConcept, Concept) ->
	  true
	; Concept = MetaConcept
	),
	db_get_mesh_tc_relaxed(Concept, TreeCodes).

tuple4(A, B, C, D,          tuple4(A,B,C,D)).
tuple5(A, B, C, D, E,       tuple5(A,B,C,D,E)).
tuple6(A, B, C, D, E, F,    tuple6(A,B,C,D,E,F)).
tuple7(A, B, C, D, E, F, G, tuple7(A,B,C,D,E,F,G)).


compute_mesh_in_textfields([], MaxFreq, [], MaxFreq).
% Consolidate tf0/6 terms with same MetaConcept, CUI, and STs into a single tf/9 term
compute_mesh_in_textfields([tf0(MetaConcept,STs,Term1,Value1,Text1,
				Field1,CUI,NSent1,PosInfo1,LexCat1,Neg1),
			    tf0(MetaConcept,STs,Term2,Value2,Text2,
				Field2,CUI,NSent2,PosInfo2,LexCat2,Neg2)|Rest],
			   MaxFreqIn,
			   [tf(MetaConcept,STs,Tuples,TitleFlag,CUI,
			       FrequencyCount,AverageValue,TreeCodes)|ComputedRest],
			   MaxFreqOut) :-
	!,
	accumulate_rest_fields(Rest, MetaConcept, NewRest, RestTuples,
			       RestValues, RestFields, RestPosInfoData),
	tuple5(Term1, Text1, Field1, NSent1, PosInfo1, PosInfoData1),
	tuple5(Term2, Text2, Field2, NSent2, PosInfo2, PosInfoData2),
	% PosInfoData1 = Term1-Text1-Field1-NSent1-PosInfo1,
	% PosInfoData2 = Term2-Text2-Field2-NSent2-PosInfo2,
	sort([PosInfoData1,PosInfoData2|RestPosInfoData], SortedPosInfoData),
	consolidate_pos_info_data(SortedPosInfoData, ConsolidatedPosInfoData),
	sort_pos_info_fields(ConsolidatedPosInfoData, SortedConsolidatedPosInfoData),
	tuple6(Term1, Field1, NSent1, Text1, LexCat1, Neg1, Tuple61),
	tuple6(Term2, Field2, NSent2, Text2, LexCat2, Neg2, Tuple62),
	Tuples0 = [Tuple61, Tuple62|RestTuples],
	sort(Tuples0, Tuples1),
	add_pos_info_to_tuples(Tuples1, SortedConsolidatedPosInfoData, Tuples),
	Values = [Value1,Value2|RestValues],
	compute_average(Values, AverageValue),
	% Texts0 = [Text1,Text2|RestTexts],
	% sort(Texts0, Texts),
	Fields = [Field1,Field2|RestFields],
	length(Fields, FrequencyCount),
	update_max_freq(MaxFreqIn, FrequencyCount, MaxFreqNext),
	set_title_flag_n(Fields, ti, TitleFlag),
	compute_mesh_and_treecodes(MetaConcept, TreeCodes),
	compute_mesh_in_textfields(NewRest, MaxFreqNext, ComputedRest, MaxFreqOut).
compute_mesh_in_textfields([tf0(MetaConcept,STs,Term,Value,Text,
				Field,CUI,NSent,PosInfo,LexCat,Neg)|Rest],
			   MaxFreqIn,
			   [tf(Concept,STs,[Tuple7],
			       TitleFlag,CUI,1,Value,TreeCodes)|ComputedRest],
			   MaxFreqOut) :-
        tuple7(Term, Field, NSent, Text, LexCat, Neg, PosInfo, Tuple7),
	set_title_flag_1(Field, TitleFlag),
	compute_mesh_and_treecodes(MetaConcept, TreeCodes),
	Concept = MetaConcept,
	compute_mesh_in_textfields(Rest, MaxFreqIn, ComputedRest, MaxFreqOut).

consolidate_pos_info_data([], []).
consolidate_pos_info_data([Tuple51,Tuple52|RestPIData],
			  ConsolidatedPIData) :-
		tuple5(Term1, Text1, Field1, NSent1, PosInfo1, Tuple51),
		tuple5(Term1, Text1, Field1, NSent1, PosInfo2, Tuple52),
		!,
		append(PosInfo1, PosInfo2, ConsolidatedPosInfo),
		tuple5(Term1, Text1, Field1, NSent1, ConsolidatedPosInfo, ConsTuple5),
		consolidate_pos_info_data([ConsTuple5|RestPIData],
					 ConsolidatedPIData).
consolidate_pos_info_data([Tuple5|RestPIData],
			  [Tuple5|RestConsolidatedPIData]) :-
	consolidate_pos_info_data(RestPIData, RestConsolidatedPIData).


sort_pos_info_fields([], []).
sort_pos_info_fields([Tuple5|Rest], [Tuple5Sorted|RestSorted]) :-
	tuple5(Term, Text, Field, NSent, PosInfo, Tuple5),
	sort(PosInfo, SortedPosInfo),
	tuple5(Term, Text, Field, NSent, SortedPosInfo, Tuple5Sorted),
	sort_pos_info_fields(Rest, RestSorted).

add_pos_info_to_tuples([], _PosInfo, []).
add_pos_info_to_tuples([Tuple6|RestTuples], AllPosInfo, [Tuple7|RestTuplesWithPosInfo]) :-
	tuple6(Term, Field, NSent, Text, LexCat, Neg, Tuple6),
	tuple7(Term, Field, NSent, Text, LexCat, Neg, PosInfo, Tuple7),
	tuple5(Term, Text, Field, NSent, PosInfo, Tuple5),
	memberchk(Tuple5, AllPosInfo),
	add_pos_info_to_tuples(RestTuples, AllPosInfo, RestTuplesWithPosInfo).

set_title_flag_1(Field, TitleFlag) :-
	( Field == ti ->
	  TitleFlag = yes
	; TitleFlag = no
	).

set_title_flag_n(Fields, Value, TitleFlag) :-
	( member(Value,Fields) ->
	  TitleFlag = yes
	; TitleFlag = no
	).

update_max_freq(MaxFreqIn, FrequencyCount, MaxFreqOut) :-
	( FrequencyCount > MaxFreqIn ->
	  % Don't display
	  %        format(user_output,'Resetting maximum frequency to ~d for ~p~n',
	  %               [FrequencyCount,MetaConcept]),
	  MaxFreqOut is FrequencyCount
	; MaxFreqOut is MaxFreqIn
	).

compute_average(Values, AverageValue) :-
	sum_and_length(Values, 0, Sum, 0, Length),
	AverageValue is Sum / Length.


sum_and_length([], Sum, Sum, Length, Length).
sum_and_length([H|T], SumIn, SumOut, LengthIn, LengthOut) :-
	SumNext is SumIn + H,
	LengthNext is LengthIn + 1,
	sum_and_length(T, SumNext, SumOut, LengthNext, LengthOut).

accumulate_rest_fields([], _MetaConcept, [], [], [], [], []).
accumulate_rest_fields([tf0(MetaConcept,_STs,Term,Value,Text,Field,_CUI,NSent,PosInfo,_Neg)|Rest],
		       MetaConcept, NewRest,
		       [Tuple4|RestTuples], [Value|RestValues],
		       [Field|RestFields],   [Tuple5|RestPosInfo]) :-
	tuple4(Term, Field, NSent, Text, Tuple4),
	tuple5(Term, Text, Field, NSent, PosInfo, Tuple5),
	!,
	accumulate_rest_fields(Rest, MetaConcept, NewRest, RestTuples,
			       RestValues, RestFields, RestPosInfo).
accumulate_rest_fields([H|T], _MetaConcept, [H|T], [], [], [], []).

collapse_tf([], []).
collapse_tf([tf(Concept,STs,Tuples1,TF1,CUI,Freq1,Value1,TC1),
	     tf(Concept,STs,Tuples2,TF2,CUI,Freq2,Value2,_TC2)|Rest],
	    [tf(Concept,STs,Tuples,TitleFlag,CUI,FrequencyCount,Value,TC1)|ComputedRest]) :-
	!,
	accumulate_rest_tfvs(Rest, Concept, NewRest, RestTuples,
			     RestTFs, RestFreqs, RestValues),
	Tuples0 = [Tuples1,Tuples2|RestTuples],
	append(Tuples0, Tuples),
	TFs = [TF1,TF2|RestTFs],
	set_title_flag_n(TFs, yes, TitleFlag),
	Freqs = [Freq1,Freq2|RestFreqs],
	% Texts0 = [Texts1,Texts2|RestTexts],
	% append(Texts0, Texts),
	Values = [Value1,Value2|RestValues],
	compute_weighted_value(Freqs, Values, Value),
	sumlist(Freqs, FrequencyCount),
	collapse_tf(NewRest,ComputedRest).
collapse_tf([tf(Concept,STs,Tuples,TF,CUI,Freq,Value,TC)|Rest],
	    [tf(Concept,STs,Tuples,TF,CUI,Freq,Value,TC)|ComputedRest]) :-
	collapse_tf(Rest, ComputedRest).

accumulate_rest_tfvs([], _Concept, [], [], [], [], []) :- !.
accumulate_rest_tfvs([tf(Concept,_STs,Tuples,TF,_CUI,Freq,Value,_TC)|Rest],
		     Concept, NewRest,
		     [Tuples|RestTuples], [TF|RestTFs],
		     [Freq|RestFreqs], [Value|RestValues]) :-
	!,
	accumulate_rest_tfvs(Rest, Concept, NewRest, RestTuples,
			     RestTFs, RestFreqs, RestValues).
accumulate_rest_tfvs(Rest, _Concept, Rest, [], [], [], []).

compute_weighted_value(Freqs, Values, Value) :-
	compute_prods(Freqs, Values, FVs),
	sumlist(FVs, Sum),
	sumlist(Freqs, N),
	Value is Sum/N.

compute_prods([], [], []).
compute_prods([FirstL|RestL], [FirstR|RestR], [Prod|RestProds]) :-
	Prod is FirstL * FirstR,
	compute_prods(RestL, RestR, RestProds).

process_tf(TFInfo, MaxFreq, AATFInfo) :-
	processing_parameter(nc,  NC),
	processing_parameter(nf,  NF),
	processing_parameter(nm,  NM),
	processing_parameter(nmm, NMM),
	processing_parameter(nw,  NW),
	processing_parameter(nz,  NZ),
	processing_parameter(wc,  WC),
	processing_parameter(wd,  WD),
	processing_parameter(wm,  WM),
	processing_parameter(wmm, WMM),
	processing_parameter(ww,  WW),
	process_tf_1(TFInfo,
		     MaxFreq, NC, NF, NM, NMM, NW, NZ, WC, WD, WM, WMM, WW,
		     AATFInfo0),
	sort(AATFInfo0, AATFInfo).

process_tf_1([], _, _, _, _, _, _, _, _, _, _, _, _, []).
process_tf_1([FirstTF|RestTFs],
	     MaxFreq, NC, NF, NM, NMM, NW, NZ, WC, WD, WM, WMM, WW,
	     [FirstAATF|RestAATFs]) :-
	FirstTF = tf(Concept,STs,Tuples,TitleFlag,CUI,FrequencyCount,MMValue,TreeCodes),
	Freq is FrequencyCount / MaxFreq,
	normalize_value(NF, Freq, NFreq),
	compute_specificities(Concept, MMValue, TreeCodes, WD, NMM, NM, NW, NC,
			      NMMSpec, NMSpec, NWSpec, NCSpec),
	compute_weighted_value([WMM,WM,WW,WC], [NMMSpec,NMSpec,NWSpec,NCSpec], Spec),
	set_aatf_rank(TitleFlag, Spec, NFreq, Rank),
	normalize_value(NZ, Rank, NormalizedRank),
	NegNRank is -NormalizedRank,
	FirstAATF = aatf(NegNRank,Concept,STs,CUI,Tuples,TreeCodes),
	% Just to allow the debugger to examine FirstAATF
	FirstAATF \== [],
	process_tf_1(RestTFs,
		     MaxFreq, NC, NF, NM, NMM, NW, NZ, WC, WD, WM, WMM, WW,
		     RestAATFs).

normalize_value(0, Value, NValue) :-
	!,
	set_value_1(Value, Value1),
	NValue = Value1.
normalize_value(N, Value, NValue) :-
	N > 0,
	!,
	set_value_1(Value, Value1),
	EN is exp(N),
	A is EN + 1,
	B is EN - 1,
	C is -N * Value1,
	EC is exp(C),
	NValue is (A/B) * (1 - EC)/(1 + EC).
normalize_value(N, Value, NValue) :-
	N < 0,
	!,
	set_value_1(Value, Value1),
	M is -N,
	EM is exp(M),
	A is EM + 1,
	B is EM - 1,
	C is (A + B*Value1) / (A - B*Value1),
	LC is log(C),
	NValue is LC / M.

set_value_1(Value, Value1) :-
	( Value > 1 ->
	  Value1 = 1
	; Value < 0 ->
	  Value1 = 0
	; Value1 = Value
	).	

compute_specificities(Concept, MMValue, TreeCodes, WD, NMM, NM, NW, NC, 
                      NMMSpec, NMSpec, NWSpec, NCSpec) :-
	% MetaMap specificity
	MMSpec is MMValue / 1000,
	normalize_value(NMM, MMSpec, NMMSpec),
	% MeSH tree depth specificity
	compute_tree_depth_specificity(Concept, TreeCodes, WD, MValue),
	% TreeDepthSpecificityDivisor used to be hard-coded as 9
	environ('MMI_TREE_DEPTH_SPECIFICITY_DIVISOR', TreeDepthSpecificityDivisorVar),
	ensure_number(TreeDepthSpecificityDivisorVar, TreeDepthSpecificityDivisor),
	MSpec is MValue / TreeDepthSpecificityDivisor,
	normalize_value(NM, MSpec, NMSpec),
	% Word specificity
	tokenize_text_more(Concept, ConceptWords),
	length(ConceptWords, WValue),
	% WordSpecificityDivisor used to be hard-coded as 26
	environ('MMI_WORD_SPECIFICITY_DIVISOR', WordSpecificityDivisorVar),
	ensure_number(WordSpecificityDivisorVar, WordSpecificityDivisor),
	WSpec is WValue / WordSpecificityDivisor,
	normalize_value(NW, WSpec, NWSpec),
	% Character specificity
	length(Concept, CValue),
	% CharacterSpecificityDivisor used to be hard-coded as 102
	environ('MMI_CHARACTER_SPECIFICITY_DIVISOR', CharacterSpecificityDivisorVar),
	ensure_number(CharacterSpecificityDivisorVar, CharacterSpecificityDivisor),
	CSpec is CValue / CharacterSpecificityDivisor,
	normalize_value(NC, CSpec, NCSpec).

compute_tree_depth_specificity(_Concept, TreeCodes, WD, MValue) :-
	( TreeCodes == [] ->
	  MValue = WD
	; compute_tree_depths(TreeCodes, TreeDepths),
	  %% try both average and max--max is better (see runs a, b, and c)
	  %% compute_average(TreeDepths,MValue)
          max_member(MValue, TreeDepths)
	).

compute_tree_depths([], []).
compute_tree_depths([First|Rest], [FirstDepth|RestDepths]) :-
	split_string_completely(First, ".", SplitFirst),
	length(SplitFirst, FirstDepth),
	compute_tree_depths(Rest, RestDepths).

set_aatf_rank(TitleFlag, Spec, NFreq, Rank) :-
	( TitleFlag == yes ->
	  Rank is Spec
	; Rank is NFreq * Spec
	).

dump_aatf_info_fielded([], _UIAtom, _Stream).
dump_aatf_info_fielded([aatf(NegRank,Concept,STs,CUI,Tuples7,TreeCodes)|Rest],
			UIAtom, Stream) :-
	reverse_sort_tuples(Tuples7, SortedTuples7),
	ScaledRank is -1000.0 * NegRank,
	construct_fields_atom(SortedTuples7, FieldsAtom),
	% extract_pos_info(SortedTuples7, PosInfo),
	% format(Stream,'SortedTuples: ~p~nTitleflag: ~p~n',[SortedTuples,TitleFlag]),
	SortedTuples7 = [FirstTuple7|RestTuples7],
	dump_output(Stream, UIAtom, ScaledRank, Concept, CUI, STs,
		    RestTuples7, FirstTuple7, FieldsAtom, TreeCodes),
	% convert_all_MMI_pos_info(PosInfo, PosInfoTerm),
	% print_all_MMI_pos_info(PosInfoTerm, Stream),
	dump_aatf_info_fielded(Rest, UIAtom, Stream).

dump_output(Stream, UIAtom, ScaledRank, Concept, CUI, STs,
	    RestTuples7, FirstTuple7, FieldsAtom, TreeCodes) :-
	% convert_tuples_to_hyphen_terms(Tuples7, HyphenTerms),
	% format(Stream, '~a|MMI|~2f|~s|~a|~p|~p|~a|',
	%        [UIAtom,ScaledRank,Concept,CUI,STs,HyphenTerms,FieldsAtom]).
	format(Stream, '~a|MMI|~2f|~s|~a|~p|[',
	       [UIAtom,ScaledRank,Concept,CUI,STs]),
	dump_tuples7(RestTuples7, FirstTuple7, Stream),
	format(Stream, ']|~a|', [FieldsAtom]),
	print_all_mmi_pi(RestTuples7, FirstTuple7, Stream),
	write(Stream, '|'),
	print_treecode_info(TreeCodes, Stream),
	nl(Stream).

dump_tuples7([], LastTuple7, Stream) :-
	dump_one_tuple7(LastTuple7, Stream).
dump_tuples7([NextTuple7|RestTuples7], FirstTuple7, Stream) :-
	dump_one_tuple7(FirstTuple7, Stream),
	write(Stream, ','),
	dump_tuples7(RestTuples7, NextTuple7, Stream).

dump_one_tuple7(Tuple7, Stream) :-
	tuple7(Term, Field, NSent, Text, LexCat, Neg, _PosInfo, Tuple7),
	format(Stream, '~p-~p-~p-~p-~p-~p', [Term,Field,NSent,Text,LexCat,Neg]).
	

% convert_tuples_to_hyphen_terms(TuplesNoPosInfo, HyphenTerms) :-
% 	(  foreach(Tuple7, TuplesNoPosInfo),
% 	   foreach(Hyphen, HyphenTerms)
% 	do
% 	   tuple7(Term, Field, NSent, Text, LexCat, Neg, _PosInfo, Tuple7),
% 	   Hyphen = Term-Field-NSent-Text-LexCat-Neg
% 	).
 
print_all_mmi_pi([], LastTuple7, Stream) :-
	print_one_mmi_pi(LastTuple7, Stream).
print_all_mmi_pi([NextTuple7|RestTuples7], FirstTuple7, Stream) :-
	print_one_mmi_pi(FirstTuple7, Stream),
	write(Stream, (;)),
	print_all_mmi_pi(RestTuples7, NextTuple7, Stream).


print_one_mmi_pi(FirstTuple7, Stream) :-
	tuple7(_Term, _Field, _NSent, _Text, _LexCat, _Neg, PosInfo, FirstTuple7),
	PosInfo = [FirstPosInfoPair|RestPosInfoPairs],
	print_one_mmi_pi_aux(RestPosInfoPairs, FirstPosInfoPair, Stream).


print_one_mmi_pi_aux([], StartPos/Length, Stream) :-
	format(Stream, '~d:~d', [StartPos,Length]).
print_one_mmi_pi_aux([Next|Rest], StartPos/Length, Stream) :-
	format(Stream, '~d:~d^', [StartPos,Length]),
	print_one_mmi_pi_aux(Rest, Next, Stream).
	
% print_all_MMI_pos_info((A;B), Stream) :-
% 	print_all_MMI_pos_info(A, Stream),
% 	write(Stream, (;)),
% 	print_all_MMI_pos_info(B, Stream).
% print_all_MMI_pos_info(A^B, Stream) :-
% 	write(Stream, A),
% 	write(Stream, ^),
% 	print_all_MMI_pos_info(B, Stream).
% print_all_MMI_pos_info(A:B, Stream) :-
%  	write(Stream, A:B).
% 
% convert_all_MMI_pos_info(PosInfo, PosInfoTerm) :-
% 	list_to_term(PosInfo, [(;), ^], PosInfoTerm).

% Given a list of functors that are infix operators (and preferably xfy),
% say, [(;), ^], transform a list [a,b,c,d,e] into the term a;b;c;d;e.
% Moreover, if any of the a, b, c, d, are themselves lists,
% convert them using the next operator in the list of functors.
% The intended application is to transforma a list of positional information terms,
% e.g., [ [1/2, 3/4, 5/6], [7/8, 9/10, 11/12], [13/14, 15/16, 17/18]]
% into a term which, when printed using print_all_MMI_pos_info/2, will look like
% 1:2 ^ 3:4 ^ 5:6 ; 7:8 ^ 9:10 ^ 11:12 ; 13:14 ^ 15:16 ^ 17:18
% (with extra whitespace thown in in the example for clarity).

% list_to_term([H|T], FunctorList, Term) :-
% 	list_to_term_aux(T, H, FunctorList, Term).
% list_to_term(A/B, _FunctorList, A:B).
% 
% list_to_term_aux([], H, [_Functor|RestFunctors], Term) :-
% 	( RestFunctors == [] ->
% 	  H = A/B,
% 	  Term = A:B
% 	; list_to_term(H, RestFunctors, Term)
% 	).
% list_to_term_aux([Next|Rest], H, [Functor|RestFunctors], Term) :-
% 	functor(Term, Functor, 2),
% 	list_to_term(H, RestFunctors, TermH),
% 	arg(1, Term, TermH),
% 	arg(2, Term, RestTerm),
% 	list_to_term_aux(Rest, Next, [Functor|RestFunctors], RestTerm).

% extract_pos_info([], []).
% extract_pos_info([Tuple7|RestTuples7], [PosInfo|RestPosInfo]) :-
% 	tuple7(_Term, _Field, _NSent, _Text, _LexCat, _Neg, PosInfo, Tuple7),
% 	extract_pos_info(RestTuples7, RestPosInfo).
% 

print_treecode_info([], _Stream).
print_treecode_info([FirstTreeCode|RestTreeCodes], Stream) :-
	format(Stream, '~s', [FirstTreeCode]),
	( foreach(TreeCode, RestTreeCodes),
	  param(Stream)
	  do
	  format(Stream, ';~s', [TreeCode])
	).


% We want the 4-tuple terms sorted in reverse order of appearance in the original citation,
% i.e., all X-ab-N-Y terms before all X-ti-N-Y terms, and within all the ab and ti terms,
% in reverse order of utterance number.
% We do this via keysort, by
% (1) creating keys of the form ab-NegN, where NegN is the negative of the original N,
% (2) key sorting the list, and then
% (3) removing the keys.

reverse_sort_tuples(Tuples, SortedTuples) :-
	add_keys(Tuples, TuplesWithKeys),
	keysort(TuplesWithKeys, SortedTuplesWithKeys),
	% to delete the keys maps,
	% simply call add_keys/2 with the args reversed!
	add_keys(SortedTuples, SortedTuplesWithKeys),
	!. % Cut is necessary, because second call to add_keys/2 will be called with var first arg.
	
add_keys([], []).
add_keys([Tuple7|Rest],
	 [TiOrAb-NegUttNum-Tuple7|RestWithKeys]) :-
	tuple7(_Concept, TiOrAb, UttNum, _String, _LexCat, _Neg, _PosInfo, Tuple7),
	NegUttNum is -UttNum,
	add_keys(Rest, RestWithKeys).

construct_fields_atom(SortedTuples, FieldsAtom) :-
	( extract_fields(SortedTuples, Fields0),
	  sort(Fields0, Fields),
	  form_fields_atom(Fields, FieldsAtom) ->
	  true
	; FieldsAtom = 'unknown'
	).

extract_fields([], []).
extract_fields([Tuple7|Rest], [Field|ExtractedRest]) :-
	tuple7(_Concept, Field, _UttNum, _String, _LexCat, _Neg, _PosInfo, Tuple7),
	extract_fields(Rest, ExtractedRest).

form_fields_atom(Fields0, FieldsAtom) :-
	augment_fields(Fields0, AugFields0),
	sort(AugFields0, AugFields1),
	deaugment_fields(AugFields1, Fields),
	form_fields_atom_aux(Fields, FieldsAtom).

form_fields_atom_aux(Fields, FieldsAtom) :-
	atom_codes_list(Fields, FieldsStrings0),
	concat_strings_with_separator(FieldsStrings0, ";", FieldsString),
	atom_codes(FieldsAtom, FieldsString).

augment_fields([], []).
augment_fields([Field|Rest], [N-UCField|AugmentedRest]) :-
	fieldn(Field, UCField, N),
	augment_fields(Rest, AugmentedRest).

fieldn(Field, UCField, N) :-
	( Field == ti ->
	  UCField = 'TI',
	  N is 1
	; Field == ab ->
	  UCField = 'AB',
	  N is 2
	; upper(Field, UCField),
	  N is 3
	).

deaugment_fields([], []).
deaugment_fields([_-Field|Rest], [Field|DeaugmentedRest]) :-
	deaugment_fields(Rest, DeaugmentedRest).

/* phrase_info(+FieldName, ?Phrase, ?Field)

phrase_info/3 instantiates or retrieves the FieldName Field of
Phrase.  */

phrase_info(phrase,          phrase(Value,_,_,_,_,_,_), Value).
phrase_info(candidates,      phrase(_,Value,_,_,_,_,_), Value).
phrase_info(mappings,        phrase(_,_,Value,_,_,_,_), Value).
phrase_info(pwi,             phrase(_,_,_,Value,_,_,_), Value).
phrase_info(gvcs,            phrase(_,_,_,_,Value,_,_), Value).
phrase_info(ev0,             phrase(_,_,_,_,_,Value,_), Value).
phrase_info(aphrases,        phrase(_,_,_,_,_,_,Value), Value).

% :- use_module(skr_lib(addportray)).
% portray_mm_output(mm_output(_ExpandedUtterance,_CitationTextAtom,_ModifiedText,_Tagging,
% 			      _AAs,_Syntax,_DisambiguatedMMOPhrases,_ExtractedPhrases)) :-
%  	write('MM_OUTPUT').
% :- add_portray(portray_mm_output).
