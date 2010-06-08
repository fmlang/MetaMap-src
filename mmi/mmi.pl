
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

:- use_module(lexicon(lexical),[
	concatenate_strings/3
    ]).

:- use_module(metamap(metamap_evaluation),[
	extract_components/3,
	merge_contiguous_components/2
    ]).

:- use_module(metamap(metamap_tokenization),[
	tokenize_text/2,
	tokenize_text_more/2,
	tokenize_text_utterly/2
    ]).

:- use_module(skr_db(db_access),[
	get_year/1,
	initialize_db_access/3,
	db_get_mesh_mh/2,
	db_get_meta_mesh/2,
	db_get_mesh_tc_relaxed/2
    ]).

:- use_module(skr_lib(ctypes),[
	is_alnum/1,
	is_white/1
    ]).

:- use_module(skr_lib(nls_strings),[
	atom_codes_list/2,
	split_string/4,
	split_string_completely/3,
	trim_and_compress_internal_whitespace/2
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
	index/3,
	lower/2,
	substring/4,
	ttyflush/0,
	upper/2
   ]).

:- use_module(skr(skr_utilities), [
	skr_begin_write/1,
	skr_end_write/1,
	token_template/5
    ]).

:- use_module(text(text_objects),[
	extract_token_strings/2
   ]).		     

:- use_module(library(avl),[
	avl_member/3
    ]).

:- use_module(library(lists),[
	append/2,
	last/2,
	max_member/2,
	rev/2,
	selectchk/3,
	subseq0/2,
	sumlist/2
    ]).

do_MMI_processing(OrigUtterances, BracketedOutput, _Sentences, DisambMMOutput) :-
	% Do MMI processing, if requested
	( control_value('XML', _) ->
	  true
	; control_option(machine_output) ->
	  true
        ; control_option(fielded_mmi_output) ->
	  conditionally_skr_begin_write(BracketedOutput),
          current_output(Stream),
	  get_UIAtom(OrigUtterances, UIAtom),
	  process_citation(UIAtom, DisambMMOutput, Stream),
	  conditionally_skr_end_write(BracketedOutput)
        ; true
	).

get_UIAtom(OrigUtterances, UIAtom) :-
	OrigUtterances = [FirstUtterance|_],
	FirstUtterance = utterance(UtteranceIDAtom,_,_,_),
	atom_codes(UtteranceIDAtom, UtteranceIDString),
	append([PMIDString, ".", _Rest], UtteranceIDString),
	!,
	atom_codes(UIAtom, PMIDString).

conditionally_skr_begin_write(BracketedOutput) :-
	  ( BracketedOutput =:= 1 ->
	    skr_begin_write('MMI')
	  ; true
	  ).

conditionally_skr_end_write(BracketedOutput) :-
	  ( BracketedOutput =:= 1 ->
	    skr_end_write('MMI')
	  ; true
	  ).

% 2000 parameters
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
	process_tf(TFInfo, MaxFreqOut, AATFInfo),
        dump_aatf_info_fielded(AATFInfo, UIAtom, FieldedStream),
	!.
process_citation(UIAtom, _MMOutput, _FieldedStream) :-
	format('ERROR: process_citation/6 failed for UI ~a.~n',[UIAtom]),
	abort.

% WARNING: This is actually a misnomer; the real goal here is the treecodes
%          but the concept can be non-MeSH unless --restrict_to_mesh is on
%          This operation will eventually be generalized to use other
%          hierarchies.
% for use with debug version (arity 3)
%compute_mesh_in_text(MMOutput,TFInfo) :-
%    compute_mesh_in_text(user_output,MMOutput,TFInfo).

% compute_mesh_in_text(ReportStream,MMOutput,TFInfo) :-
compute_mesh_in_text(MMOutput, MaxFreqIn, TFInfo, MaxFreqOut) :-
        get_pre_tf_in_utterances(MMOutput, PreTFInfo0),
	sort(PreTFInfo0, PreTFInfo),
	compute_mesh_in_textfields(PreTFInfo, MaxFreqIn, TFInfo0, MaxFreqOut),
	sort(TFInfo0, TFInfo1),
	collapse_tf(TFInfo1, TFInfo).

get_pre_tf_in_utterances([], []).
get_pre_tf_in_utterances([Utterance|Rest], PreTFInfo) :-
	get_pre_tf_in_utterance(Utterance, UtterancePreTFInfo),
	append(UtterancePreTFInfo, RestPreTFInfo, PreTFInfo),
	get_pre_tf_in_utterances(Rest, RestPreTFInfo).

get_pre_tf_in_utterance(Utterance, PreTFInfo) :-
	Utterance = mm_output(utterance(Label,_Text,_PosInfo,_ReplPos),
			      CitationTextAtom,_ModifiedText,_Tagging,AAs,
			      _Syntax,Phrases,_ExtractedPhrases),
	determine_field_nsent(Label, Field, NSent),
	get_pre_tf_in_phrases(Phrases, CitationTextAtom, AAs, Field, NSent, PreTFInfo).

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

get_pre_tf_in_phrases([], _CitationTextAtom, _AAs, _Field, _NSent, []).
get_pre_tf_in_phrases([Phrase|Rest], CitationTextAtom, AAs, Field, NSent, PreTFInfo) :-
	get_pre_tf_in_phrase(Phrase, CitationTextAtom, AAs, Field, NSent, PhrasePreTFInfo),
	append(PhrasePreTFInfo, RestPreTFInfo, PreTFInfo),
	get_pre_tf_in_phrases(Rest, CitationTextAtom, AAs, Field, NSent, RestPreTFInfo).

get_pre_tf_in_phrase(Phrase, CitationTextAtom, _AAs, Field, NSent, PreTFInfo) :-
	% extract the mappings component, which is a list of terms of the form
	% map(-888,[
	%  ev(-694,'C0025545','Metallothionein','Metallothionein',
	%      [metallothionein],[aapp,bacs],[[[1,1],[1,1],0]],no,no,Srcs,PI)])
	phrase_info(mappings, Phrase, mappings(AllMappings)),
        form_super_mapping(AllMappings, SuperMapping),
	get_pre_tf_in_mapping(SuperMapping, CitationTextAtom, Field, NSent, PreTFInfo).

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
% ev(NegValue,_CUI,MetaTerm,MetaConcept,_MetaWords,STs,
%    MatchMap,_InvolvesHead,_IsOvermatch,_SourceInfo,_PosInfo),

% the tf0 term is of the form
% tf0(ConceptString,STs,TermString,Value,Text,Field,CUI,NSent,PosInfo)
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

get_pre_tf_in_mapping([], _CitationTextAtom, _Field, _NSent, []).
get_pre_tf_in_mapping([Candidate|Rest], CitationTextAtom, Field, NSent, [TF0|RestPreTFInfo]) :-
	get_pre_tf_in_candidate(Candidate, CitationTextAtom, Field, NSent, TF0),
	get_pre_tf_in_mapping(Rest, CitationTextAtom, Field, NSent, RestPreTFInfo).


get_pre_tf_in_candidate(Candidate, CitationTextAtom, Field, NSent, TF0) :-
	Candidate = ev(NegValue,CUI,MetaTerm,MetaConcept,_MetaWords,STs,
		       _MatchMap,_InvolvesHead,_IsOvermatch,_SourceInfo,PosInfo),
	Value is -NegValue,
	atom_codes(MetaConcept, ConceptString),
	atom_codes(MetaTerm, TermString),
	PosInfo = [H|T],
	collapse_pos_info(T, H, CollapsedPosInfo),
	construct_text_atom(CollapsedPosInfo, CitationTextAtom, Text),
	TF0 = tf0(ConceptString,STs,TermString,Value,Text,Field,CUI,NSent,CollapsedPosInfo),
	!.
get_pre_tf_in_candidate(Candidate, _CitationTextAtom, _Field, _NSent, _TF0) :-
	Candidate = ev(_NegValue,_CUI,_MetaTerm,MetaConcept,_MetaWords,_SemTypes,
		       _MatchMap,_InvolvesHead,_IsOvermatch,_SourceInfo,_PosInfo),
	format(user_output, 'ERROR: get_pre_tf_in_mapping/5 failed for ~p~n', [MetaConcept]),
	format('ERROR: get_pre_tf_in_mapping/5 failed for ~p~n', [MetaConcept]),
	abort.

construct_text_atom(PosInfoList, CitationTextAtom, TextString) :-
	extract_text_atoms(PosInfoList, CitationTextAtom, AtomList),
	concat_atom(AtomList, ' ', TextAtom),
	atom_codes(TextAtom, TextString).

extract_text_atoms([], _CitationTextAtom, []).
extract_text_atoms([StartPos/Length|RestPosInfo], CitationTextAtom, [Atom|RestAtoms]) :-
	substring(CitationTextAtom, Atom, StartPos, Length),
	extract_text_atoms(RestPosInfo, CitationTextAtom, RestAtoms).

compute_mesh_and_treecodes(MetaConcept, TreeCodes) :-
	( db_get_meta_mesh(MetaConcept, Concept) ->
	  true
        ; db_get_mesh_mh(MetaConcept, Concept) ->
	  true
	; Concept = MetaConcept
	),
	db_get_mesh_tc_relaxed(Concept, TreeCodes).

compute_mesh_in_textfields([], MaxFreq, [], MaxFreq).
% Consolidate tf0/6 terms with same MetaConcept, CUI, and STs into a single tf/9 term
compute_mesh_in_textfields([tf0(MetaConcept,STs,Term1,Value1,Text1,Field1,CUI,NSent1,PosInfo1),
			    tf0(MetaConcept,STs,Term2,Value2,Text2,Field2,CUI,NSent2,PosInfo2)|Rest],
			   MaxFreqIn,
			   [tf(MetaConcept,STs,Tuples,TitleFlag,CUI,
			       FrequencyCount,AverageValue,TreeCodes)|ComputedRest],
			   MaxFreqOut) :-
	!,
	accumulate_rest_fields(Rest, MetaConcept, NewRest, RestTuples,
			       RestValues, RestFields, RestPosInfoData),
	PosInfoData1 = Term1-Text1-Field1-NSent1-PosInfo1,
	PosInfoData2 = Term2-Text2-Field2-NSent2-PosInfo2,
	sort([PosInfoData1,PosInfoData2|RestPosInfoData], SortedPosInfoData),
	consolidate_pos_info_data(SortedPosInfoData, ConsolidatedPosInfoData),
	sort_pos_info_fields(ConsolidatedPosInfoData, SortedConsolidatedPosInfoData),
	Tuples0 = [Term1-Field1-NSent1-Text1,Term2-Field2-NSent2-Text2|RestTuples],
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
compute_mesh_in_textfields([tf0(MetaConcept,STs,Term,Value,Text,Field,CUI,NSent,PosInfo)|Rest],
			   MaxFreqIn,
			   [tf(Concept,STs,[Term-Field-NSent-Text-PosInfo],
			       TitleFlag,CUI,1,Value,TreeCodes)|ComputedRest],
			   MaxFreqOut) :-
	set_title_flag_1(Field, TitleFlag),
	compute_mesh_and_treecodes(MetaConcept, TreeCodes),
	Concept = MetaConcept,
	compute_mesh_in_textfields(Rest, MaxFreqIn, ComputedRest, MaxFreqOut).

consolidate_pos_info_data([], []).
consolidate_pos_info_data([Term1-Text1-Field1-NSent1-PosInfo1,
			   Term1-Text1-Field1-NSent1-PosInfo2|RestPIData],
			  ConsolidatedPIData) :-
		!,
		append(PosInfo1, PosInfo2, ConsolidatedPosInfo),
		consolidate_pos_info_data([Term1-Text1-Field1-NSent1-ConsolidatedPosInfo|RestPIData],
					 ConsolidatedPIData).
consolidate_pos_info_data([Term-Text-Field-NSent-PosInfo|RestPIData],
			  [Term-Text-Field-NSent-PosInfo|RestConsolidatedPIData]) :-
	consolidate_pos_info_data(RestPIData, RestConsolidatedPIData).


sort_pos_info_fields([], []).
sort_pos_info_fields([Term-Text-Field-NSent-PosInfo|Rest],
		     [Term-Text-Field-NSent-SortedPosInfo|RestSorted]) :-
	sort(PosInfo, SortedPosInfo),
	sort_pos_info_fields(Rest, RestSorted).

add_pos_info_to_tuples([], _PosInfo, []).
add_pos_info_to_tuples([Term-Field-NSent-Text|RestTuples],
		      AllPosInfo,
		      [Term-Field-NSent-Text-PosInfo|RestTuplesWithPosInfo]) :-
	selectchk(Term-Text-Field-NSent-PosInfo, AllPosInfo, RemainingPosInfo),
	add_pos_info_to_tuples(RestTuples, RemainingPosInfo, RestTuplesWithPosInfo).

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
accumulate_rest_fields([tf0(MetaConcept,_STs,Term,Value,Text,Field,_CUI,NSent,PosInfo)|Rest],
		       MetaConcept,
		       NewRest,
		       [Term-Field-NSent-Text|RestTuples],
		       [Value|RestValues],
		       [Field|RestFields],
		       [Term-Text-Field-NSent-PosInfo|RestPosInfo]) :-
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

accumulate_rest_tfvs([], _Concept, [], [], [], [], []).
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
	FirstAATF = aatf(NegNRank,Concept,STs,CUI,Tuples),
	% Just to allow the debugger to examine FirstAATF
	FirstAATF \== [],
	process_tf_1(RestTFs,
		     MaxFreq, NC, NF, NM, NMM, NW, NZ, WC, WD, WM, WMM, WW,
		     RestAATFs).

normalize_value(0, Value, NValue) :-
	!,
	( Value > 1 ->
	  NValue = 1
	; Value < 0 ->
	  NValue = 0
	; NValue = Value
	).
normalize_value(N, Value, NValue) :-
	N > 0,
	!,
	( Value > 1 ->
          Value1 = 1
	; Value < 0 ->
	  Value1 = 0
	; Value1 = Value
	),
	EN is exp(N),
	A is EN + 1,
	B is EN - 1,
	C is -N * Value1,
	EC is exp(C),
	NValue is (A/B) * (1 - EC)/(1 + EC).
normalize_value(N, Value, NValue) :-
	N < 0,
	!,
	( Value > 1 ->
          Value1 = 1
	; Value < 0 ->
	  Value1 = 0
	; Value1 = Value
	),
	M is -N,
	EM is exp(M),
	A is EM + 1,
	B is EM - 1,
	C is (A + B*Value1) / (A - B*Value1),
	LC is log(C),
	NValue is LC / M.

compute_specificities(Concept, MMValue, TreeCodes, WD, NMM, NM, NW, NC, 
                      NMMSpec, NMSpec, NWSpec, NCSpec) :-
	% MetaMap specificity
	MMSpec is MMValue / 1000,
	normalize_value(NMM, MMSpec, NMMSpec),
	% MeSH tree depth specificity
	compute_tree_depth_specificity(Concept, TreeCodes, WD, MValue),
	MSpec is MValue / 9,
	normalize_value(NM, MSpec, NMSpec),
	% Word specificity
	tokenize_text_more(Concept, ConceptWords),
	length(ConceptWords, WValue),
	WSpec is WValue / 26,
	normalize_value(NW, WSpec, NWSpec),
	% Character specificity
	length(Concept, CValue),
	CSpec is CValue / 102,
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
dump_aatf_info_fielded([aatf(NegRank,Concept,STs,CUI,Tuples)|Rest],
			UIAtom, Stream) :-
	reverse_sort_tuples(Tuples, SortedTuples),
	ScaledRank is -1000.0 * NegRank,
	construct_fields_atom(SortedTuples, FieldsAtom),
	extract_pos_info(SortedTuples, TuplesNoPosInfo, PosInfo),
	% format(Stream,'SortedTuples: ~p~nTitleflag: ~p~n',[SortedTuples,TitleFlag]),
	dump_output(Stream, UIAtom, ScaledRank, Concept, CUI, STs, TuplesNoPosInfo, FieldsAtom),
	print_MMI_pos_info(PosInfo, Stream),
	dump_aatf_info_fielded(Rest, UIAtom, Stream).

dump_output(Stream, UIAtom, ScaledRank, Concept, CUI, STs, TuplesNoPosInfo, FieldsAtom) :-
	( \+ control_option(hide_semantic_types) ->
	  format(Stream, '~a|MM|~2f|~s|~a|~p|~p|~a',
	                 [UIAtom,ScaledRank,Concept,CUI,STs,TuplesNoPosInfo,FieldsAtom])
	; format(Stream, '~a|MM|~2f|~s|~a|~p|~a',
		 	 [UIAtom,ScaledRank,Concept,CUI,TuplesNoPosInfo,FieldsAtom])
	).

extract_pos_info([], [], []).
extract_pos_info([Term-Field-NSent-Text-PosInfo|RestTuples],
		 [Term-Field-NSent-Text|RestTuplesNoPosInfo],
		 [PosInfo|RestPosInfo]) :-
	extract_pos_info(RestTuples, RestTuplesNoPosInfo, RestPosInfo).

print_MMI_pos_info([], Stream) :- nl(Stream).
print_MMI_pos_info([FirstPosInfo|RestPosInfo], Stream) :-
	FirstPosInfo = [StartPos/Length|RestPairs],
	format(Stream, '|~d:~d', [StartPos, Length]),
	print_rest_MMI_info(RestPairs, Stream),
        print_MMI_pos_info(RestPosInfo, Stream).

print_rest_MMI_info([], _Stream).
print_rest_MMI_info([StartPos/Length|T], Stream) :-
        print_rest_MMI_info_aux(T, StartPos, Length, Stream).


print_rest_MMI_info_aux([], StartPos, Length, Stream) :-
        format(Stream, ',~d:~d', [StartPos, Length]).
print_rest_MMI_info_aux([NextStartPos/NextLength|RestPosInfo], ThisStartPos, ThisLength, Stream) :-
        format(Stream, ',~d:~d', [ThisStartPos,ThisLength]),
        print_rest_MMI_info_aux(RestPosInfo, NextStartPos, NextLength, Stream).

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
add_keys([Concept-TiOrAb-UttNum-String-PosInfo|Rest],
	 [TiOrAb-NegUttNum-(Concept-TiOrAb-UttNum-String-PosInfo)|RestWithKeys]) :-
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
extract_fields([_Concept-Field-_UttNum-_String-_PosInfo|Rest], [Field|ExtractedRest]) :-
	extract_fields(Rest, ExtractedRest).

form_fields_atom(Fields0, FieldsAtom) :-
	augment_fields(Fields0, AugFields0),
	sort(AugFields0, AugFields1),
	deaugment_fields(AugFields1, Fields),
	form_fields_atom_aux(Fields, FieldsAtom).

form_fields_atom_aux(Fields, FieldsAtom) :-
    atom_codes_list(Fields, FieldsStrings0),
    concatenate_strings(FieldsStrings0, ";", FieldsString),
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
