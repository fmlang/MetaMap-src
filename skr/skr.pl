
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

% File:     skr.pl
% Module:   SKR
% Author:   Lan
% Purpose:  Provide access to all SKR processing: MetaMap, MMI and SemRep


:- module(skr,[
	% calculate_aa_extra_chars/4,
	extract_phrases_from_aps/2,
	get_inputmatch_atoms_from_phrase/2,
	get_phrase_tokens/4,
	% called by MetaMap API -- do not change signature!
	initialize_skr/1,
	skr_phrases/17,
	% called by MetaMap API -- do not change signature!
	stop_skr/0
    ]).


:- use_module(lexicon(lex_access),[
	initialize_lexicon/2
    ]).

:- use_module(lexicon(lexical),[
	concatenate/3
    ]).

:- use_module(metamap(metamap_candidates),[
	add_candidates/7
    ]).

:- use_module(metamap(metamap_evaluation),[
	consolidate_matchmap/3,
	evaluate_all_GVCs/16,
	extract_components/3,
	component_intersects_components/2,
	compute_extra_meta/3,
	compute_match_value/8,
	connect_components/2,
	matching_token/3,
	merge_contiguous_components/2
    ]).

:- use_module(metamap(metamap_parsing),[
	collapse_syntactic_analysis/2,
	demote_heads/2
    ]).

:- use_module(metamap(metamap_stop_phrase),[
	stop_phrase/2
    ]).

:- use_module(metamap(metamap_tokenization),[
	add_tokens_to_phrases/2,
	extract_tokens_with_tags/2,
	get_phrase_item_feature/3,
	get_phrase_item_name/2,
	get_phrase_item_subitems/2,
	get_subitems_feature/3,
	linearize_components/2,
	linearize_phrase/4,
	new_phrase_item/3,
	parse_phrase_word_info/3,
	set_subitems_feature/4
    ]).

:- use_module(metamap(metamap_utilities),[
	extract_relevant_sources/3,
	extract_nonexcluded_sources/3,
	extract_source_name/2,
	wgvcs/1,
	wl/1,
	write_avl_list/1
    ]).

:- use_module(metamap(metamap_variants),[
	initialize_metamap_variants/1,
	compute_variant_generators/2,
	augment_GVCs_with_variants/1,
	gather_variants/4
    ]).

:- use_module(skr(skr_umls_info09),[
	convert_to_root_sources/2
    ]).

:- use_module(skr(skr_utilities),[
	debug_call/2,
	debug_message/3,
	replace_crs_with_blanks/4,
	token_template/5
    ]).

:- use_module(skr_db(db_access),[
	initialize_db_access/0,
	stop_db_access/0,
	db_get_concept_sts/2,
	db_get_cui_sourceinfo/2
    ]).

:- use_module(skr_lib(efficiency),[
	maybe_atom_gc/2
    ]).

:- use_module(skr_lib(nls_lists),[
	truncate_list/4
    ]).

:- use_module(skr_lib(nls_strings), [
	split_string_completely/3
   ]).

:- use_module(skr_lib(nls_system),[
	add_to_control_options/1,
	control_option/1,
	control_value/2,
	set_control_options/1,
	subtract_from_control_options/1
    ]).

:- use_module(skr_lib(sicstus_utils),[
	interleave_string/3,
	lower/2,
	subchars/4,
	ttyflush/0
    ]).

:- use_module(text(text_objects),[
	extract_token_strings/2
    ]).

:- use_module(text(text_object_util),[
	an_tok/1,
	higher_order_or_annotation_tok/1,
	ws_or_pn_tok/1,
	ws_tok/1
    ]).

:- use_module(wsd(wsdmod),[
	do_WSD/11
    ]).

:- use_module(library(avl),[
	avl_member/3,
	avl_to_list/2,
	empty_avl/1
    ]).

:- use_module(library(file_systems),[
	close_all_streams/0
    ]).

:- use_module(library(lists),[
	append/2,
	last/2,
	prefix/2,
	rev/2,
	suffix/2
    ]).

:- use_module(library(sets),[
	intersection/3
    ]).

/* 
   initialize_skr(+Options)
   stop_skr

initialize_skr/0 calls initialize_skr/1 to initialize modules that
it uses and to set Options.  (For skr_fe, Options is [] since it has
already set the options it uses.)
stop_skr/0 stops access to other modules and closes all streams.  */

initialize_skr(Options) :-
	set_control_options(Options),
	initialize_db_access,
	initialize_lexicon(_, _),
	( control_option(dynamic_variant_generation) ->
	  initialize_metamap_variants(dynamic)
	; initialize_metamap_variants(static)
	),
	!.
initialize_skr(_) :-
	format('~NERROR: initialize_skr/1 failed.~n', []),
	stop_skr,
	fail.

stop_skr :-
	( stop_db_access ->
	  true
	; true
	),
	close_all_streams.

skr_phrases(InputLabel, UtteranceText, CitationTextAtom,
	    AAs, SyntacticAnalysis, WordDataCacheIn, USCCacheIn, RawTokensIn,
	    WSDServerHosts, WSDForced, WSDServerPort,
	    RawTokensOut, WordDataCacheOut, USCCacheOut,
	    MMOPhrases, ExtractedPhrases, SemRepPhrasesOut) :-
	( skr_phrases_aux(InputLabel, UtteranceText, CitationTextAtom,
			  AAs, SyntacticAnalysis, WordDataCacheIn, USCCacheIn, RawTokensIn,
			  WSDServerHosts, WSDForced, WSDServerPort,
			  RawTokensOut, WordDataCacheOut, USCCacheOut,
			  MMOPhrases, ExtractedPhrases, SemRepPhrasesOut) ->
	  true
        ; format('~NERROR: skr_phrases/14 failed for text ~p: ~p~n',
		  [InputLabel,UtteranceText]),
	  format(user_output,
		 '~NERROR: skr_phrases/14 failed for text ~p: ~p~n',
		 [InputLabel,UtteranceText]),
	  current_output(OutputStream),
	  flush_output(OutputStream),
	  abort
        ).

skr_phrases_aux(InputLabel, UtteranceText, CitationTextAtom,
		AAs, SyntacticAnalysis, WordDataCacheIn, USCCacheIn, RawTokensIn,
		WSDServerHosts, WSDForced, WSDServerPort,
		RawTokensOut, WordDataCacheOut, USCCacheOut,
		DisambiguatedMMOPhrases, ExtractedPhrases,
		minimal_syntax(SemRepPhrasesWithWSD)) :-
	maybe_atom_gc(_DidGC, _SpaceCollected),
	SyntacticAnalysis = minimal_syntax(Phrases0),
	add_tokens_to_phrases(Phrases0, Phrases),
	% UtteranceText contains no AAs. E.g.,
	% "heart attack (HA)" will become simply "heart attack".
	skr_phrases_1(Phrases, InputLabel, UtteranceText,
		      AAs, CitationTextAtom,
		      WordDataCacheIn, USCCacheIn,
		      WordDataCacheOut, USCCacheOut,
		      RawTokensIn, RawTokensOut,
		      MMOPhrases, ExtractedPhrases),
	do_WSD(UtteranceText, InputLabel, CitationTextAtom, AAs, RawTokensIn,
	       WSDServerHosts, WSDForced, WSDServerPort,
	       MMOPhrases, DisambiguatedMMOPhrases, SemRepPhrasesWithWSD).

skr_phrases_1([], _InputLabel, _AllUtteranceText, _AAs, _CitationTextAtom,
	      WordDataCache, USCCache, WordDataCache, USCCache,
	      RawTokens, RawTokens, [], []).
skr_phrases_1([PhraseIn|OrigRestPhrasesIn], InputLabel, AllUtteranceText,
	      AAs, CitationTextAtom,
	      WordDataCacheIn, USCCacheIn,
	      WordDataCacheOut, USCCacheOut,
	      RawTokensIn, RawTokensOut,
	      [FirstMMOPhrase|RestMMOPhrases],
	      [Phrases|RestPhrases]) :-
	get_composite_phrases([PhraseIn|OrigRestPhrasesIn],
			      CompositePhrase, RestCompositePhrases, CompositeOptions),
	% Merge consecutive phrases spanned by an AA
	merge_aa_phrases(RestCompositePhrases, CompositePhrase, AAs,
			 MergedPhrase0, RemainingCompositePhrases),
	% AllUtteranceText is never used in skr_phrase; it's there only for gap analysis,
	% which is no longer used!!
	skr_phrase(InputLabel, AllUtteranceText,
		   MergedPhrase0, AAs, CitationTextAtom, RawTokensIn, GVCs,
		   WordDataCacheIn, USCCacheIn,
		   WordDataCacheNext, USCCacheNext,
		   RawTokensNext, APhrases, FirstMMOPhrase),
	set_var_GVCs_to_null(GVCs),
	extract_phrases_from_aps(APhrases, Phrases),
	subtract_from_control_options(CompositeOptions),
	% add_semtypes_to_phrases_if_necessary(Phrases0,FirstEPPhrases),
	skr_phrases_1(RemainingCompositePhrases, InputLabel, AllUtteranceText,
		      AAs, CitationTextAtom,
		      WordDataCacheNext, USCCacheNext, WordDataCacheOut, USCCacheOut,
		      RawTokensNext, RawTokensOut, RestMMOPhrases, RestPhrases).

% Determine if an AA Expansion spans two (or more) consecutive phrases.
% If so, collapse the spanned phrases into one.

merge_aa_phrases([], CompositePhrase, _AAs, CompositePhrase, []).
merge_aa_phrases([NextPhrase|RestPhrases], FirstPhrase, AAs,
		 MergedPhrase, RestMergedPhrases) :-
	( acronym_expansion_spans_phrases(AAs, 
					  [FirstPhrase,NextPhrase|RestPhrases],
					  PhrasesToMerge, RemainingPhrases) ->
	  merge_aa_phrases_1(PhrasesToMerge, MergedPhrase),
	  RestMergedPhrases = RemainingPhrases
	; MergedPhrase = FirstPhrase,
	  RestMergedPhrases = [NextPhrase|RestPhrases]
	).

merge_aa_phrases_1(PhrasesToMerge, MergedPhrase) :-
	append(PhrasesToMerge, AppendedPhrasesToMerge),
	demote_heads(AppendedPhrasesToMerge, MergedPhrase).

acronym_expansion_spans_phrases(AAs, [FirstPhrase|RestPhrases],
				[FirstPhrasePrefix|PhrasesToMerge], RemainingPhrases) :-
	avl_member(_AATokens, AAs, [ExpansionTokens]),
	% extract_token_strings(ExpansionTokens, ExpansionStrings),
	append(FirstPhrasePrefix, FirstPhraseSuffix, FirstPhrase),
	FirstPhraseSuffix \== [],
	% reversed order of args from QP library version!
	last(FirstPhraseSuffix, LastPhraseSuffix),
	not_non_hyphen_punc(LastPhraseSuffix),
	tokens_match_inputmatch(ExpansionTokens, FirstPhraseSuffix, RestPhrases,
				0, _NumPhrasesMerged, PhrasesToMerge, RemainingPhrases).

not_non_hyphen_punc(LastPhraseSuffix) :-
	% Either the LastPhraseSuffix syntax element is not a punc(_) structure....
	( \+ LastPhraseSuffix = punc(_) ->
	  true
	% ...or if it is, the punc char is not a hyphen
	; LastPhraseSuffix = punc(Features),
	  memberchk(inputmatch(InputMatch), Features),
	  % reversed order of args from QP library version!
	  last(InputMatch, '-')
	).

% tokens_match_inputmatch(+ExpansionTokens, +FirstPhrase, +RestPhrases,
% 			  +NumPhrasesMergedIn, -NumPhrasesMergedOut,
% 			  -PhrasesToMerge, -RemainingPhrases).

% Base case: If we've exhausted all the AA expansion tokens, we've succeded.
% There are no more phrases to merge, so return all leftover phrases as RemainingPhrases.

tokens_match_inputmatch([], FirstPhrase, RestPhrases,
			NumPhrasesMerged, NumPhrasesMerged,
			[], [FirstPhrase|RestPhrases]) :-
	% Merging phrases requires that > 1 phrase be merged!
	NumPhrasesMerged > 1.
% There are more AA expansion tokens to be matched with inputmatch words.
tokens_match_inputmatch([FirstExpansionTokens|RestExpansionTokens],
			FirstPhrase, RestPhrases,
			NumPhrasesMergedIn, NumPhrasesMergedOut,
			PhrasesToMerge, RemainingPhrases) :-
	% Match expansion tokens to inputmatch words from first phrase.
	% Either the Expansion Tokens or the first phrase's inputmatch tokens
	% must be completely consumed (possibly both).
	match_expansion_tokens_to_phrase([FirstExpansionTokens|RestExpansionTokens],
					 FirstPhrase, RemainingExpansionTokens),
	NumPhrasesMergedNext is NumPhrasesMergedIn + 1,
	% so add first phrase to list of phrases to merge
	% format(user_output, '~n~q~n', [PhrasesToMerge = [FirstPhrase|RestPhrasesToMerge]]),
	PhrasesToMerge = [FirstPhrase|RestPhrasesToMerge],
	% format(user_output, '~n~q~n', [RestPhrases = [NextPhrase|RestPhrases1]]),
	( RestPhrases == [] ->
	  RestPhrasesToMerge = [],
	  % There had better be no expansion tokens leftover, either!
	  RemainingExpansionTokens == [],
	  RemainingPhrases = [],
	  % Merging phrases requires that > 1 phrase be merged!
	  NumPhrasesMergedNext > 1
	; RestPhrases = [NextPhrase|RestPhrases1],
	  tokens_match_inputmatch(RemainingExpansionTokens, NextPhrase, RestPhrases1,
				  NumPhrasesMergedNext, NumPhrasesMergedOut,
				  RestPhrasesToMerge, RemainingPhrases)
	).

match_expansion_tokens_to_phrase([], _RestPhraseElements, []).
match_expansion_tokens_to_phrase([FirstExpansionToken|RestExpansionTokens],
				 PhraseElements, RemainingExpansionTokens) :-
	  % We've consumed the inputmatches of all the elements of the current phrase
	( PhraseElements == [] ->
	  RemainingExpansionTokens = [FirstExpansionToken|RestExpansionTokens]
	; PhraseElements = [FirstPhraseElement|RestPhraseElements],
	  % In matching expansion tokens to a phrase element's inputmatch tokens,
	  % either the expansion tokens or the inputmatch must be completely consumed (or both)
	  arg(1, FirstPhraseElement, FeatureList),
	  memberchk(inputmatch(InputMatchAtoms), FeatureList),
	  append(_InputMatchAtomsPrefix, InputMatchAtomsSuffix, InputMatchAtoms),
	  InputMatchAtomsSuffix \== [],
	  match_tokens_to_inputmatch([FirstExpansionToken|RestExpansionTokens],
				     InputMatchAtomsSuffix, NextExpansionTokens),
	  match_expansion_tokens_to_phrase(NextExpansionTokens, RestPhraseElements,
					   RemainingExpansionTokens)
	).

match_tokens_to_inputmatch([], _RestInputMatchAtoms, []).
match_tokens_to_inputmatch([FirstExpansionToken|RestExpansionTokens],
			       InputMatchAtoms, RemainingExpansionTokens) :-
	  % We've consumed all the inputmatch atoms from this phrase element
	( InputMatchAtoms == [] ->
	  RemainingExpansionTokens = [FirstExpansionToken|RestExpansionTokens]
	  % Skip over ws tokens, because blank spaces aren't represented in inputmatch
	; ws_tok(FirstExpansionToken) ->
	  match_tokens_to_inputmatch(RestExpansionTokens,
				     InputMatchAtoms, RemainingExpansionTokens)
	; InputMatchAtoms = [FirstInputMatchAtom|RestInputMatchAtoms],
	  token_template(FirstExpansionToken, _TokenType, TokenString, _LCTokenString, _PosInfo),
	  atom_codes(TokenAtom, TokenString),
	  TokenAtom == FirstInputMatchAtom,
	  match_tokens_to_inputmatch(RestExpansionTokens,
				     RestInputMatchAtoms, RemainingExpansionTokens)
	).

%%% syntax_matches_aa_tokens([], AATokens, AATokens).
%%% syntax_matches_aa_tokens([FirstSyntax|RestSyntax], AATokensIn, AATokensOut) :-
%%% 	arg(1, FirstSyntax, FeatureList),
%%% 	memberchk(inputmatch(InputMatchAtoms), FeatureList),
%%% 	match_inputmatch_atoms(InputMatchAtoms, AATokensIn, AATokensNext),
%%% 	syntax_matches_aa_tokens(RestSyntax, AATokensNext, AATokensOut).
%%% 
%%% match_inputmatch_atoms([], AATokens, AATokens).
%%% match_inputmatch_atoms([FirstAtom|RestAtoms], AATokensIn, AATokensOut) :-
%%% 	match_one_inputmatch_atom(AATokensIn, FirstAtom, AATokensNext),
%%% 	match_inputmatch_atoms(RestAtoms, AATokensNext, AATokensOut).
%%% 
%%% 
%%% match_one_inputmatch_atom([FirstAAToken|RestAATokens], FirstAtom, AATokensNext) :-
%%% 	( ws_tok(FirstAAToken) ->
%%% 	  match_one_inputmatch_atom(RestAATokens, FirstAtom, AATokensNext)
%%% 	; FirstAAToken = tok(_TokenType, TokenString, _LCTokenString, _PosInfo),
%%% 	  atom_codes(FirstAtom, TokenString),
%%% 	  AATokensNext = RestAATokens
%%% 	).
	

set_var_GVCs_to_null(GVCs) :-
	( var(GVCs) ->
	  GVCs=[]
	; true
	).

extract_phrases_from_aps([], []).
extract_phrases_from_aps([ap(_NegValue,Phrase,_PhraseMap,_Mapping)|Rest],
			 [Phrase|ExtractedRest]) :-
	extract_phrases_from_aps(Rest, ExtractedRest).

/* skr_phrase(+Label, +AllUtteranceText, +PhraseText, +Phrase, +AAs,
   	      +CitationTextAtom, +RawTokensIn
              -GVCs, -RawTokensOut, -APhrases, -MMOPhraseTerm)

skr_phrase/18 is the main predicate for finding Meta terms for
a Phrase, which is of the form [<item1>,...,<itemn>] where each item is
a (possibly nested) unary term containing a (possibly multi-word) atom.
Examples:
   [head(application)]
   [prep(of),head(computers)]
   [prep(in),mod(the),head(intensive care unit)]
PhraseWordInfoPair is a pair PhraseWordInfo:FilteredPhraseWordInfo where each
element of the pair is of the form
     pwi(PhraseWordL,PhraseHeadWordL,PhraseMap).
*/

skr_phrase(Label, UtteranceText,
	   PhraseSyntax, AAs, CitationTextAtom, RawTokensIn, GVCs,
	   WordDataCacheIn, USCCacheIn,
	   WordDataCacheOut, USCCacheOut,
	   RawTokensOut, APhrases, MMOPhraseTerm) :-
	( skr_phrase_1(Label, UtteranceText,
		       PhraseSyntax, AAs, RawTokensIn,
		       CitationTextAtom, GVCs,
		       WordDataCacheIn, USCCacheIn,
		       WordDataCacheOut, USCCacheOut,
		       RawTokensOut, APhrases, MMOPhraseTerm) ->
	  true
        ; format('~n#### ERROR: skr_phrase failed on ~w ~w~n~n', [Label, PhraseSyntax]),
          format(user_output,
		 '~n#### ERROR: skr_phrase failed on ~w ~w~n~n', [Label, PhraseSyntax]),
	  abort
        ).

skr_phrase_1(Label, UtteranceTextString,
	     PhraseSyntax, AAs,
	     RawTokensIn, CitationTextAtom, GVCs,
	     WordDataCacheIn, USCCacheIn,
	     WordDataCacheOut, USCCacheOut,
	     RawTokensOut, APhrases, MMOPhraseTerm) :-
	get_pwi_info(PhraseSyntax, PhraseWordInfoPair, TokenPhraseWords, TokenPhraseHeadWords),
	get_phrase_info(PhraseSyntax, AAs, InputMatchPhraseWords, RawTokensIn, CitationTextAtom,
			PhraseTokens, RawTokensOut, PhraseStartPos, PhraseLength,
			OrigPhraseTextAtom, ReplacementPositions),
	atom_codes(OrigPhraseTextAtom, OrigPhraseTextString),
	debug_phrase(Label, TokenPhraseWords, InputMatchPhraseWords),
	atom_codes(UtteranceTextAtom, UtteranceTextString),
	% format(user_output, '~n### Generating Evaluations~n', []),
	generate_initial_evaluations(Label, UtteranceTextAtom,
				     OrigPhraseTextString, PhraseSyntax, Variants,
				     GVCs, WordDataCacheIn, USCCacheIn, RawTokensOut, AAs,
				     InputMatchPhraseWords, PhraseTokens, TokenPhraseWords,
				     TokenPhraseHeadWords, WordDataCacheOut,
				     USCCacheOut, Evaluations0),
	refine_evaluations(Evaluations0, EvaluationsAfterSTs, Evaluations),
	% format(user_output, 'Candidates = ~q~n', [Evaluations]), ttyflush,
	debug_evaluations(Evaluations),

	% Construct mappings only if the mappings option is set; this is new
	% and eases the problem of mapping computation when not desired (e.g.,
	% in browse mode)
	% length(Evaluations, EvaluationsLength),
	% format(user_output, '~n### Generating Mappings from ~q Evaluations~n', [EvaluationsLength]),
	generate_best_mappings(Evaluations, OrigPhraseTextString, PhraseSyntax,
			       PhraseWordInfoPair, Variants, APhrases0, Mappings0),
	truncate_mappings(Mappings0, APhrases0, Mappings, APhrases),
	% I have here the data structures to call disambiguate_mmo/2
	% format(user_output, 'Candidates: ~q~n', [candidates(Evaluations)]),
	% format(user_output, 'Mappings:   ~q~n', [mappings(Mappings)]),
	% format(user_output, 'PWI:        ~q~n', [pwi(
	% format(user_output, 'GVCs:       ~q~n', [gvcs(GVCs)]),
	% format(user_output, 'EV0:        ~q~n', [ev0(Evaluations0)]),
	% format(user_output, 'APhrases:   ~q~n', [aphrases(APhrases)]),
	MMOPhraseTerm = phrase(phrase(OrigPhraseTextAtom,PhraseSyntax,
				      PhraseStartPos/PhraseLength,ReplacementPositions),
			       candidates(Evaluations),
			       mappings(Mappings),
			       pwi(PhraseWordInfoPair),
			       gvcs(GVCs),
			       ev0(EvaluationsAfterSTs),
			       aphrases(APhrases)).

debug_phrase(Label, TokenPhraseWords, InputMatchPhraseWords) :-
	( phrase_debugging ->
	  get_label_components(Label, [PMID,TiOrAB,UtteranceNum]),
	  length(TokenPhraseWords, TokenPhraseLength),
	  current_output(OutputStream),
	  format(OutputStream, 'Phrase|~s|~s|~s|~d|~q~n',
		 [PMID,TiOrAB,UtteranceNum,TokenPhraseLength,InputMatchPhraseWords]),
	  flush_output(OutputStream),
	  % don't duplicate output to user_output
	  ( OutputStream == user_output ->
	    true
	  ; format(user_output, 'Phrase|~s|~s|~s|~d|~q~n',
		   [PMID,TiOrAB,UtteranceNum,TokenPhraseLength,InputMatchPhraseWords]),
	    ttyflush
	  )
	; true
	).

phrase_debugging :-
	( control_value(debug, DebugFlags),
	  memberchk(phrases, DebugFlags) ->
	  true
	; control_option(phrases_only)
	).

get_label_components(Label, [PMID, TiOrAB, UtteranceNum]) :-
	  atom_codes(Label, LabelString),
	  ( split_string_completely(LabelString, ".", [PMID, TiOrAB, UtteranceNum]) ->
	    true
	  ; PMID = "<>",
	    TiOrAB = "<>",
	    UtteranceNum = "<>"
	  ).

get_pwi_info(Phrase, PhraseWordInfoPair, TokenPhraseWords, TokenPhraseHeadWords) :-
	( control_option(term_processing) ->
	  parse_phrase_word_info(nofilter, Phrase, PhraseWordInfoPair)
	; parse_phrase_word_info(filter, Phrase, PhraseWordInfoPair)
	),
	PhraseWordInfoPair = _AllPhraseWordInfo:FilteredPhraseWordInfo,
	FilteredPhraseWordInfo = pwi(FPhraseWordL,FPhraseHeadWordL,_FPhraseMap),
	FPhraseWordL = wdl(_,TokenPhraseWords),
	FPhraseHeadWordL = wdl(_,TokenPhraseHeadWords).

get_phrase_info(Phrase, AAs, InputMatchPhraseWords, RawTokensIn, CitationTextAtom,
		PhraseTokens, RawTokensOut, PhraseStartPos, PhraseLength,
		OrigPhraseTextAtom, ReplacementPositions) :-
	get_inputmatch_atoms_from_phrase(Phrase, InputMatchPhraseWords),
	% format(user_output, '*** InputMatchPhraseWords: ~q~n', [InputMatchPhraseWords]),
	% For each word in InputMatchPhraseWords, extract the matching tokens from RawTokensIn.
	% We need to match the words in the raw tokens to get the correct pos info
	% and to get the phrase with all the blanks.
	get_phrase_tokens(InputMatchPhraseWords, RawTokensIn, PhraseTokens, RawTokensOut),
	get_phrase_startpos_and_length(PhraseTokens, PhraseStartPos, PhraseLength0),
	subchars(CitationTextAtom, PhraseTextStringWithCRs0, PhraseStartPos, PhraseLength0),
	add_AA_suffix(PhraseTextStringWithCRs0, AAs, PhraseTokens, PhraseLength0,
		      PhraseTextStringWithCRs, PhraseLength),
	replace_crs_with_blanks(PhraseTextStringWithCRs, PhraseStartPos,
				OrigPhraseTextString, ReplacementPositions),
	atom_codes(OrigPhraseTextAtom, OrigPhraseTextString).
	
	% atom_codes(RealPhraseText, RealPhraseTextString).

% If the last phrase token matches the last expansion token of a given AA,
% then add " (" + AA + ")" to the phrase string, and modify the length accordingly.

add_AA_suffix(PhraseTextStringWithCRs0, AAs, PhraseTokens, PhraseLength0,
	      PhraseTextStringWithCRs, PhraseLength) :-
	% reversed order of args from QP library version!
	( last(PhraseTokens, LastPhraseToken),
	  avl_member(AATokens, AAs, [ExpansionTokens]),
	  % reversed order of args from QP library version!
	  last(ExpansionTokens, LastExpansionToken),
	  % ExpansionTokens in the AA AVL tree are of the form
	  % tok(Type, String, LCString, PosInfo1), but
	  % Tokens in the Phrase Token List are of the form
	  % tok(Type, String, LCString, PosInfo1, PosInfo2).
	  % We require that the first 4 fields match.
	  matching_tokens_4(LastExpansionToken, LastPhraseToken) ->
	  get_AA_text(AATokens, AATextString0),
	  append(AATextString0, AATextString),
	  append([PhraseTextStringWithCRs0, " (", AATextString, ")"], PhraseTextStringWithCRs),
	  length(AATextString, AATextStringLength),
	  PhraseLength is PhraseLength0 + 2 + AATextStringLength + 1
	; PhraseTextStringWithCRs = PhraseTextStringWithCRs0,
	  PhraseLength is PhraseLength0
	).
	
	
% First 4 fields must be identical	
matching_tokens_4(tok(TokenType, TokenString, TokenLCString, Pos1),
		  tok(TokenType, TokenString, TokenLCString, Pos1, _Pos2)).
		  
	
get_AA_text(AATokens, AATextString) :-
	extract_token_strings(AATokens, AATokenStrings),
	interleave_string(AATokenStrings, " ", AATextString).

generate_initial_evaluations(Label, UtteranceText,
			     PhraseTextString, Phrase, Variants,
			     GVCs, WordDataCacheIn, USCCacheIn, RawTokensOut, AAs,
			     InputMatchPhraseWords, PhraseTokens, TokenPhraseWords,
			     TokenPhraseHeadWords, WordDataCacheOut, USCCacheOut, Evaluations0) :-
	% If phrases_only is on, don't bother generating any evaluations,
	% because we've already computed and displayed the phrase lengths,
	% and that's all we care about if this option is on.
	% Moreover, setting Evaluations0 to [] will short-circuit
	% all subsequent evaluation processing.
	( control_option(phrases_only) ->
	  Evaluations0 = []
	; generate_initial_evaluations_1(Label, UtteranceText,
					 PhraseTextString, Phrase, Variants,
					 GVCs, WordDataCacheIn, USCCacheIn, RawTokensOut, AAs,
					 InputMatchPhraseWords, PhraseTokens, TokenPhraseWords,
					 TokenPhraseHeadWords, WordDataCacheOut,
					 USCCacheOut, Evaluations0)
	).

generate_initial_evaluations_1(Label, UtteranceText,
			       PhraseTextString, Phrase, Variants,
			       GVCs, WordDataCacheIn, USCCacheIn, RawTokensOut, AAs,
			       InputMatchPhraseWords, PhraseTokens, TokenPhraseWords,
			       TokenPhraseHeadWords, WordDataCacheOut, USCCacheOut, Evaluations0) :-
	( check_generate_initial_evaluations_1_control_options_1,
	  lower(PhraseTextString, LCPhraseTextString),
	  extract_syntactic_tags(Phrase, Tags),
	  atom_codes(LCPhraseAtom, LCPhraseTextString),
	  stop_phrase(LCPhraseAtom, Tags) ->
	  Evaluations0 = [],
	  WordDataCacheOut = WordDataCacheIn,
	  USCCacheOut = USCCacheIn
	; check_generate_initial_evaluations_1_control_options_2 ->
	  % format(user_output, 'About to call compute_evaluations~n', []), ttyflush,
	  compute_evaluations(Label, UtteranceText,
			      Phrase, Variants, GVCs,
			      WordDataCacheIn, USCCacheIn, RawTokensOut, AAs,
			      InputMatchPhraseWords,
			      PhraseTokens, TokenPhraseWords, TokenPhraseHeadWords,
			      WordDataCacheOut, USCCacheOut, Evaluations0)
	  % format(user_output, 'Done with compute_evaluations~n', []), ttyflush
	;  Evaluations0 = [],
	   WordDataCacheOut = WordDataCacheIn,
	   USCCacheOut = USCCacheIn
	).


refine_evaluations(Evaluations0, EvaluationsAfterSTs, Evaluations) :-
	( control_option(truncate_candidates_mappings) ->
	  truncate_list(Evaluations0, TruncatedEvaluations, 100, _NETruncated)
	; TruncatedEvaluations = Evaluations0
	),
	( control_option(restrict_to_sources) ->
	  control_value(restrict_to_sources, Sources),
	  filter_evaluations_to_sources(TruncatedEvaluations, Sources, EvaluationsAfterSources)
	; control_option(exclude_sources) ->
	  control_value(exclude_sources, Sources),
	  filter_evaluations_excluding_sources(TruncatedEvaluations, Sources, EvaluationsAfterSources)
	; EvaluationsAfterSources = TruncatedEvaluations
	),
	( control_option(restrict_to_sts) ->
	  control_value(restrict_to_sts, STs),
	  filter_evaluations_to_sts(EvaluationsAfterSources, STs, EvaluationsAfterSTs)
	; control_option(exclude_sts) ->
	  control_value(exclude_sts, STs),
	  filter_evaluations_excluding_sts(EvaluationsAfterSources, STs, EvaluationsAfterSTs)
	; EvaluationsAfterSTs = EvaluationsAfterSources
	),
 	( \+ control_option(compute_all_mappings) ->
 	  filter_out_subsumed_evaluations(EvaluationsAfterSTs, Evaluations)
 	; Evaluations = EvaluationsAfterSTs
 	).


debug_evaluations(Evaluations) :-
	( control_value(debug, DebugFlags),
	  memberchk(4, DebugFlags) ->
	  length(Evaluations, NEvals),
	  format('~nNon-subsumed evaluations (~d):~n', [NEvals]),
	  wl(Evaluations)
	; true
	).

generate_best_mappings(Evaluations, PhraseTextString, Phrase,
		       PhraseWordInfoPair, Variants, APhrases0, Mappings0) :-
	( check_generate_best_mappings_control_options ->
	  % format(user_output, 'About to call construct_best_mappings~n', []), ttyflush,
	  construct_best_mappings(Evaluations, PhraseTextString, Phrase,
				  PhraseWordInfoPair, Variants, APhrases0, Mappings0)
	  % format(user_output, 'Done with construct_best_mappings~n', []), ttyflush
	; APhrases0 = [],
          Mappings0 = []
	).

truncate_mappings(Mappings0, APhrases0, Mappings, APhrases) :-
	( control_option(truncate_candidates_mappings) ->
	  truncate_list(Mappings0,Mappings,8,_NMTruncated),
	  truncate_list(APhrases0,APhrases,8,_NMTruncated)
	; Mappings = Mappings0,
          APhrases = APhrases0
	).

extract_syntactic_tags([], []).
extract_syntactic_tags([First|Rest], [FirstSTag|RestSTags]) :-
	functor(First, FirstSTag, _Arity),
	extract_syntactic_tags(Rest, RestSTags).


get_all_generators_and_candidate_lengths([], []).
get_all_generators_and_candidate_lengths([GVC|RestGVCs], [G-CandidatesLength|RestGenerators]) :-
	GVC = gvc(Generator,_V,Candidates),
	Generator = v(G,_,_,_,_,_),
	length(Candidates, CandidatesLength),
	get_all_generators_and_candidate_lengths(RestGVCs, RestGenerators).

compute_evaluations(Label, UtteranceText,
		    Phrase, Variants, GVCs, WordDataCacheIn, USCCacheIn,
		    RawTokensOut, AAs, InputMatchPhraseWords,
		    PhraseTokens, TokenPhraseWords, TokenPhraseHeadWords,
		    WordDataCacheOut, USCCacheOut, Evaluations) :-
	% *******************
	% Expansion Algorithm
	% *******************
	get_debug_control_value(DebugFlags),
	debug_message(trace, '~N### Calling generate_variants: ~q~n', [TokenPhraseWords]),
	generate_variants(TokenPhraseWords, TokenPhraseHeadWords,
			  Phrase, DebugFlags, GVCs, Variants),
	% format(user_output, '~N### generate_variants DONE!~n', []),
	debug_compute_evaluations_2(DebugFlags, GVCs, Variants),

	debug_message(trace, '~N### Calling add_candidates: ~q~n', [TokenPhraseWords]),
	add_candidates(GVCs, Variants, DebugFlags,
		       WordDataCacheIn, USCCacheIn,
		       WordDataCacheOut, USCCacheOut),
	% format(user_output, '~N### add_candidates DONE!~n', []),
	debug_compute_evaluations_3(DebugFlags, GVCs),

	length(TokenPhraseWords, PhraseTokenLength),
	get_all_generators_and_candidate_lengths(GVCs, GeneratorsAndCandidateLengths),
	debug_message(trace,
		      '~N### Calling evaluate_all_GVCs: ~q~n',
		      [GeneratorsAndCandidateLengths]),
	empty_avl(CCsIn),
	evaluate_all_GVCs(GVCs, DebugFlags, Label, UtteranceText,
			  Variants, TokenPhraseWords,
			  PhraseTokenLength, TokenPhraseHeadWords,
			  PhraseTokens, RawTokensOut, AAs,
			  InputMatchPhraseWords,
			  CCsIn, _CCsOut,[], Evaluations0),
	% format(user_output, '~N### All GVCs DONE!~n', []),
	sort(Evaluations0, Evaluations1),
	maybe_filter_evaluations_by_threshold(Evaluations1, Evaluations2),

	debug_compute_evaluations_4(DebugFlags, Evaluations2),
	filter_out_redundant_evaluations(Evaluations2, Evaluations),
	add_semtypes_to_evaluations(Evaluations),
	debug_compute_evaluations_5(DebugFlags, Evaluations).

generate_variants(TokenPhraseWords, TokenPhraseHeadWords, Phrase, DebugFlags, GVCs3, Variants) :-
	compute_variant_generators(TokenPhraseWords, GVCs0),
	% format(user_output, '~n### ~q~n',
	%        [compute_variant_generators(TokenPhraseWords, GVCs0)]),
	debug_compute_evaluations_1(DebugFlags, GVCs0),
	filter_variants_by_tags(GVCs0, Phrase, GVCs1),
	% format(user_output, '~n### ~q~n',
	%        [filter_variants_by_tags(GVCs0, Phrase, GVCs1)]),
	augment_GVCs_with_variants(GVCs1),
	% format(user_output, '~n### ~q~n',
	%        [augment_GVCs_with_variants(GVCs1)]),
	maybe_filter_out_dvars(GVCs1, GVCs2),
	maybe_filter_out_aas(GVCs2, GVCs3),
	gather_variants(GVCs3, TokenPhraseWords, TokenPhraseHeadWords, Variants).
	% format(user_output, '~n### ~q~n',
	%       [gather_variants(GVCs3, TokenPhraseWords, TokenPhraseHeadWords, Variants)]).

%%% The StartPos of a list of tokens = the minimum StartPos of all the tokens.
%%% The EndPos   of a list of tokens = the maximum StartPos of all the tokens
%%%                                  + the length of the token with the maximum StartPos
%%% The Length of a list of tokens   = EndPos = StartPos

get_phrase_startpos_and_length([], 0, 0).
get_phrase_startpos_and_length([FirstToken|RestTokens], PhraseStartPos, PhraseLength) :-
	% Initialize MinStartPos, MaxStartPos, and MaxLength
	% with the values from the first token
	get_token_startpos_and_length(FirstToken, FirstStartPos, FirstLength),
	MinStartPos is FirstStartPos,
	MaxStartPos is FirstStartPos,
	MaxLength is FirstLength,
	get_phrase_startpos_and_length_aux(RestTokens,
					   MinStartPos, MaxStartPos, MaxLength,
					   PhraseStartPos, PhraseLength).

% When there are no more tokens,
% use the current MinStartPos, MaxStartPos, and MaxLength
% to determine the entire phrase's StartPos and Length.
get_phrase_startpos_and_length_aux([],
				   MinStartPos, MaxStartPos, MaxLength,
				   PhraseStartPos, PhraseLength) :-
	PhraseStartPos is MinStartPos,
	PhraseEndPos is MaxStartPos + MaxLength,
	PhraseLength is PhraseEndPos - PhraseStartPos.
get_phrase_startpos_and_length_aux([Token|RestTokens],
				   MinStartPosIn, MaxStartPosIn, MaxLengthIn,
				   PhraseStartPos, PhraseLength) :-
	update_startpos_and_length(MinStartPosIn, MaxStartPosIn, MaxLengthIn,
				   Token,
				   MinStartPosNext, MaxStartPosNext, MaxLengthNext),
	get_phrase_startpos_and_length_aux(RestTokens,
					   MinStartPosNext, MaxStartPosNext, MaxLengthNext,
					   PhraseStartPos, PhraseLength).

update_startpos_and_length(MinStartPosIn, MaxStartPosIn, MaxLengthIn,
			   Token,
			   MinStartPosOut, MaxStartPosOut, MaxLengthOut) :-
	get_token_startpos_and_length(Token, TokenStartPos, TokenLength),
	update_min_startpos(MinStartPosIn, TokenStartPos, MinStartPosOut),
	update_max_startpos_and_length(MaxStartPosIn, MaxLengthIn,
				       TokenStartPos, TokenLength,
				       MaxStartPosOut, MaxLengthOut).

% If the current token's StartPos < the MinStartPos so far,
% update MinStartPos to the current token's StartPos;
% otherwise, keep the same MinStartPos.
update_min_startpos(MinStartPosIn, TokenStartPos, MinStartPosOut) :-
	( TokenStartPos < MinStartPosIn ->
	  MinStartPosOut is TokenStartPos
	; MinStartPosOut is MinStartPosIn
	).

% If the current token's StartPos > the MaxStartPos so far,
% update MaxStartPos and Length to the current token's StartPos and Length;
% otherwise, keep the same MaxStartPos and Length.
update_max_startpos_and_length(MaxStartPosIn, MaxLengthIn,
			       TokenStartPos, TokenLength,
			       MaxStartPosOut, MaxLengthOut) :-
	( TokenStartPos > MaxStartPosIn ->
	  MaxStartPosOut is TokenStartPos,
	  MaxLengthOut is TokenLength
	; MaxStartPosOut is MaxStartPosIn,
	  MaxLengthOut is MaxLengthIn
	).

get_token_startpos_and_length(tok(_TokenType, _String, _LCString, _Pos1, pos(StartPos, Length)),
			       StartPos, Length).
get_token_startpos_and_length(tok(_TokenType, _String, _LCString, pos(StartPos, EndPos)),
			       StartPos, Length) :-
	Length is EndPos - StartPos.

% get_phrase_tokens(+PhraseWords, +RawTokensIn, -PhraseTokens, -RawTokensOut)
% 
% PhraseWords = [in,patients]
% RawTokensIn = [
% 	tok(lc,"in","in",pos(30,32),pos(183,2)),
% 	tok(ws," "," ",pos(32,33),pos(185,1)),
% 	tok(lc,"patients","patients",pos(33,41),pos(186,8)),
% 	tok(ws," "," ",pos(41,42),pos(194,1)),
% 	tok(lc,"with","with",pos(42,46),pos(195,4)),
% 	tok(ws," "," ",pos(46,47),pos(199,1)),
%         ...  ]
% 
% Extract from RawTokensIn the tokens matching the woreds in PhraseWords.

get_phrase_tokens([], RestRawTokens, [], RestRawTokens).
get_phrase_tokens([H|T], RawTokensIn, [TokenH|TokensT], RawTokensOut) :-
        get_word_token(H, RawTokensIn, TokenH, RawTokensNext),
        get_phrase_tokens(T, RawTokensNext, TokensT, RawTokensOut).

% Skip non-text tokens and ws tokens.
get_word_token(Word, [TokenToSkip|Rest], Token, RestRawTokens) :-
        ( higher_order_or_annotation_tok(TokenToSkip) ->
          true
        ; ws_tok(TokenToSkip) ->
          true
        ),
        !,
        get_word_token(Word, Rest, Token, RestRawTokens).
% The next real token should match the word.
get_word_token(Word, [Token|RestRawTokens], Token, RestRawTokens) :-
        matching_token(mc, Word, Token),
        !.

% If we get to this next clause, there's a problem, presumably because
% the positional information re-tokenization failed to handle
% either an aa or an aadef token and simply skipped over it.

% This next clause is also used in term processing, because the syntactic uninversion
% done by that option changes, e.g., "cancer, lung" to "lung cancer".

% If next real (i.e., text) token doesn't match,
% which is the case if we've made it to this clause,
% then look for a matching token in the next 4 tokens.
% If one is found, then use that matching token,
% and skip over all tokens up to the match;
% otherwise (i.e., no match in the next 4 tokens)
% create a token on the fly, using the two PI terms
% of the next token, which is presumably an AA,
% and the non-matching words in the input stream
% are its expansion.

% Suppose the text includes "patients with acquired immunodeficiency syndrome (AIDS)."
% and the PI tokenization fails to process the AA.
% We'd have
% PhraseWords = [acquired,immunodeficiency,syndrome]
% and
% RawTokensIn = [
%     tok(uc,"AIDS","aids",pos(246,250),pos(439,4)),
%     tok(pn,")",")",pos(250,251),pos(443,1)),
%     tok(ws," "," ",pos(251,252),pos(444,1)),
%     tok(lc,"and","and",pos(252,255),pos(445,3))
%     ...       ]

% We don't find a match for "acquired" in the next 4 tokens,
% so assume (hope, pray...) that "AIDS" is the intended match,
% and use the "AIDS" token's PI.

get_word_token(Word, RestTokens, MatchingToken, NewRestTokens) :-
        % format(user_output, '#### WARNING: Token mismatch for "~w"; ', [Word]),
        ( find_next_matching_token(5, Word, RestTokens, MatchingToken, NewRestTokens) ->
	  true
          % format(user_output, ' FOUND matching token.~n', [])
        ; create_new_token(Word, RestTokens, MatchingToken),
          % format(user_output, ' CREATING new token.~n', []),
          NewRestTokens = RestTokens
        ).

find_next_matching_token(Count, Word, Tokens, MatchingToken, RemainingTokens) :-
        Count > 0,
        Tokens = [NextToken|RestTokens],
        ( matching_token(mc, Word, NextToken) ->
          MatchingToken = NextToken,
          RemainingTokens = RestTokens
        ; decrement_count_if_an_tok(NextToken, Count, NextCount),
          RemainingTokens = [NextToken|RestRemainingTokens],
          find_next_matching_token(NextCount, Word, RestTokens, MatchingToken, RestRemainingTokens)
        ).

decrement_count_if_an_tok(NextToken, Count, NextCount) :-
	( an_tok(NextToken) ->
	  NextCount is Count - 1
	; NextCount is Count
	).

create_new_token(Word, RestTokens, CreatedToken) :-
	RestTokens = [NextToken|_],
	NextToken = tok(_TokenType, _Text, _LCText,
			pos(StartPos1, Length1),
			pos(StartPos2, Length2)),
	lower(Word, LowerWord),
	atom_codes(Word, WordString),
	atom_codes(LowerWord, LowerWordString),
	CreatedToken = tok(xx, WordString, LowerWordString,
			   pos(StartPos1, Length1),
			   pos(StartPos2, Length2)).
	
% If the phrase represents an aadef, e.g.,
%      heart attack (HA)
% the AA text itself "(HA)" has been lost at this point of processing,
% but it's recoverable from the AAs AVL tree.
% Here, we determine if the PhraseTokens represent an AA;
% this is done by matching the PhraseTokens against all the
% ExpansionTokens in the AA AVL tree.
% If there is a match, we must calculate how many characters
% should be added to the Positional Info to account for the AA itself--e.g.,"(HA)".
% We cannot do this by using the PosInfo in the AVL tree, because that PosInfo
% does not take into account the actual spacing such as the 6-character indentation
% at the beginning of all lines in Medline citations.
% To get that, we must look in the RawTokens coming after the PhraseTokens
% and extract the pe token that comes after the PhraseTokens. Ugh...
%
% AAs = 
% node([tok(uc,"CPPV","cppv",pos(63,67))],
%       [[tok(ic,"Continuous","continuous",pos(21,31)),
%         tok(ws," "," ",pos(31,32)),
% 	tok(lc,"positive","positive",pos(32,40)),
% 	tok(ws," "," ",pos(40,41)),
% 	tok(lc,"pressure","pressure",pos(41,49)),
% 	tok(ws," "," ",pos(49,50)),
% 	tok(lc,"ventilation","ventilation",pos(50,61))]],
%       0,empty,empty))
% 
% PhraseTokens = [
% tok(ic,"Continuous","continuous",pos(21,31),pos(198,10)),
% tok(lc,"positive","positive",pos(32,40),pos(209,8)),
% tok(lc,"pressure","pressure",pos(41,49),pos(218,8)),
% tok(lc,"ventilation","ventilation",pos(50,61),pos(227,11))]
% 
% 
% RemainingTokens is the list of tokens that comes after the AAdef token,
% so its position identifies the position after the AAdef.
 
filter_out_dvars([], []).
filter_out_dvars([gvc(G,Vs,Cs)|Rest], [gvc(G,FilteredVs,Cs)|FilteredRest]) :-
	filter_out_dvars_aux(Vs, FilteredVs),
	filter_out_dvars(Rest, FilteredRest).

filter_out_dvars_aux([], []).
filter_out_dvars_aux([First|Rest], Filtered) :-
	First = v(_,_,_,History,_,_),
	( memberchk(0'd, History) ->
	  FilteredRest = Filtered
	; Filtered = [First|FilteredRest]
	),
	filter_out_dvars_aux(Rest, FilteredRest).

% Remove all variants whose history contains either "a" (AA) or "e" AA expansion
filter_out_aas([], []).
filter_out_aas([gvc(G,Vs,Cs)|Rest], [gvc(G,FilteredVs,Cs)|FilteredRest]) :-
	filter_out_aas_aux(Vs, FilteredVs),
	filter_out_aas(Rest, FilteredRest).

filter_out_aas_aux([], []).
filter_out_aas_aux([FirstVariant|RestVariants], FilteredRest) :-
	FirstVariant = v(_Word,_LexCat,_VarLevel,History,_Roots,_NFR),
	( memberchk(0'a, History) ->
	  filter_out_aas_aux(RestVariants, FilteredRest)
	; memberchk(0'e, History) ->
	  filter_out_aas_aux(RestVariants, FilteredRest)
	; FilteredRest = [FirstVariant|Tail],
	  filter_out_aas_aux(RestVariants, Tail)
	).

% Given
% (1) a list of GVC terms of the form
%     gvc(Generator, _Variants, _Candidates)
%     where Generator is of the form
%     v(Word, LexicalCategories, VarLevel, History, Roots, _NFR), and
% (2) the phrase parse,
% Remove from the list all GVC terms whose combination of Word and LecicalCategories
% does not appear in the phrase parse. E.g.,

% GVCList = [gvc(v(hydrophobic,[noun],0,[],[hydrophobic],_103295),_103309,_103310),
% 	   gvc(v(hydrophobic,[adj],0,[],[hydrophobic],_103295),_103332,_103333),
% 	   gvc(v(core,[verb],0,[],[core],_103456),_103470,_103471),
% 	   gvc(v(core,[noun],0,[],[core],_103456),_103493,_103494)]
% 
% contains hydrophobic-noun, hydrophobic-adj, core-verb, and core-noun. 
% 
% Phrase = [mod([lexmatch([hydrophobic]),inputmatch([hydrophobic]),tag(adj),tokens([hydrophobic])]),
% 	  head([lexmatch([core]),inputmatch([core]),tag(noun),tokens([core])]),
% 	  punc([inputmatch(['.']),tokens([])])]
% 
% However, in the phrase hydrophobic is only an adj, and core is only a noun,
% so we keep only only the first and last GVC terms.

filter_variants_by_tags(GVCsIn, Phrase, GVCsOut) :-
	% get the tokens and tags in the phrase
	extract_tokens_with_tags(Phrase, ToksTags),
	% convert the format
	convert_tokens_tags(ToksTags, WordTags0),
	% discard those wordtags that do not occur in GVCsIn
	filter_word_tags(WordTags0, GVCsIn, WordTags),
	% discard those GVCs that do not occur in WordTags
	filter_variants_by_tags_aux(GVCsIn, WordTags, GVCsOut).

filter_variants_by_tags_aux([], _, []).
filter_variants_by_tags_aux([First|Rest], WordTags, Result) :-
	First = gvc(v(Word,[Tag],_,_,_,_),_,_),
	% find_tags(WordTags, Word, Tags),
	memberchk(wordtags(Word,Tags), WordTags),
	!,
	( memberchk(Tag, Tags) ->
	  Result = [First|FilteredRest]
	; Result = FilteredRest
	),
	filter_variants_by_tags_aux(Rest, WordTags, FilteredRest).
filter_variants_by_tags_aux([First|Rest], WordTags, [First|FilteredRest]) :-
	filter_variants_by_tags_aux(Rest, WordTags, FilteredRest).

% find_tags([],_,_) :-
%     !,
%     fail.
% find_tags([wordtags(Word,Tags)|_], Word, Tags) :- !.
% find_tags([_|Rest],Word, Tags) :-
% 	find_tags(Rest, Word, Tags).

convert_tokens_tags(ToksTags, WordTags) :-
	convert_tokens_tags_aux(ToksTags, WordTags0),
	merge_word_tags(WordTags0, WordTags).

convert_tokens_tags_aux([], []).
convert_tokens_tags_aux([tokenstag(Tokens,Tag)|RestTokensTags],
                        [wordtags(Word,[Tag])|RestWordTags]) :-
	concatenate(Tokens, " ", Word),
	convert_tokens_tags_aux(RestTokensTags, RestWordTags).

% If a word appears in several wordtags/2 structures, merge the lexical categories
merge_word_tags([], []).
merge_word_tags([First|Rest], [wordtags(Word,Tags)|MergedRest]) :-
	First = wordtags(Word,[Tag]),
	find_wordtags(Rest, Word, RestTags, NewRest),
	Tags = [Tag|RestTags],
	merge_word_tags(NewRest, MergedRest).

find_wordtags([], _, [], []).
find_wordtags([wordtags(Word,[Tag])|Rest], Word, [Tag|Tags], NewRest) :-
	!,
	find_wordtags(Rest, Word, Tags, NewRest).
find_wordtags([First|Rest], Word, Tags, [First|NewRest]) :-
	find_wordtags(Rest, Word, Tags, NewRest).


filter_word_tags([], _, []).
filter_word_tags([First|Rest], GVCs, [First|FilteredRest]) :-
	First = wordtags(Word,Tags),
	occurs_in_gvcs(GVCs, Word, Tags),
	!,
	filter_word_tags(Rest, GVCs, FilteredRest).
filter_word_tags([_|Rest], GVCs, FilteredRest) :-
	filter_word_tags(Rest, GVCs, FilteredRest).

% occurs_in_gvcs([],_,_) :-
%     !,
%     fail.
occurs_in_gvcs([gvc(v(Word,[Tag],_,_,_,_),_,_)|_Rest], Word, Tags) :-
	memberchk(Tag, Tags),
	!.
occurs_in_gvcs([_|Rest], Word, Tags) :-
	occurs_in_gvcs(Rest, Word, Tags).


/* filter_evaluations_to_sources(+EvaluationsIn, +Sources, -EvaluationsOut)
   filter_evaluations_to_sources_aux(+EvaluationsIn, +RootSources,
                                     -EvaluationsOut)

filter_evaluations_to_sources/3 removes those evaluations from EvaluationsIn
which do not represent terms from Sources. It produces EvaluationsOut
*** REPLACING THE PREFERRED CONCEPT NAME WITH A NAME FROM Sources ***
where necessary. At least temporarily, when this is done the source of
the name is included in curly braces.
filter_evaluations_to_sources_aux/3 performs the actual work on the RootSources
for Sources. */

filter_evaluations_to_sources(EvaluationsIn,Sources,EvaluationsOut) :-
    convert_to_root_sources(Sources,RootSources),
    filter_evaluations_to_sources_aux(EvaluationsIn,RootSources,EvaluationsOut).

filter_evaluations_to_sources_aux([],_,[]) :-
    !.
filter_evaluations_to_sources_aux([First|Rest],RootSources,Results) :-
% temp
%    format('~p~n',[First]),
    First = ev(NegValue,CUI,MetaTerm,_MetaConcept,MetaWords,SemTypes,
	       MatchMap,InvolvesHead,IsOvermatch,Sources,PosInfo),
% temp
%    format('~p~n',[CUI]),
    db_get_cui_sourceinfo(CUI,SourceInfo0),
% temp
%    wl(SourceInfo0),
    extract_relevant_sources(SourceInfo0,RootSources,SourceInfo),
    (SourceInfo==[] ->
	Results=ModRest
    ;   extract_source_name(SourceInfo,SourceName),
	Results=[ev(NegValue,CUI,MetaTerm,SourceName,MetaWords,SemTypes,
		    MatchMap,InvolvesHead,IsOvermatch,Sources,PosInfo)|ModRest]
    ),
    filter_evaluations_to_sources_aux(Rest,RootSources,ModRest).

/* filter_evaluations_excluding_sources(+EvaluationsIn, +Sources,
                                        -EvaluationsOut)
   filter_evaluations_excluding_sources_aux(+EvaluationsIn, +RootSources,
                                        -EvaluationsOut)

filter_evaluations_excluding_sources/3 removes those evaluations from
EvaluationsIn which only represent terms from Sources. It produces
EvaluationsOut
*** REPLACING THE PREFERRED CONCEPT NAME IF IT IS FROM ONE OF Sources ***
where necessary.
filter_evaluations_excluding_sources_aux/3 performs the actual work on the
RootSources for Sources. */

filter_evaluations_excluding_sources(EvaluationsIn, Sources, EvaluationsOut) :-
	convert_to_root_sources(Sources, RootSources),
	filter_evaluations_excluding_sources_aux(EvaluationsIn, RootSources, EvaluationsOut).

filter_evaluations_excluding_sources_aux([], _, []).
filter_evaluations_excluding_sources_aux([First|Rest], RootSources, Results) :-
	First = ev(NegValue,CUI,MetaTerm,_MetaConcept,MetaWords,SemTypes,
		   MatchMap,InvolvesHead,IsOvermatch,SourceInfo,PosInfo),
	db_get_cui_sourceinfo(CUI, SourceInfo0),
	extract_nonexcluded_sources(SourceInfo0, RootSources, ExtractedSourceInfo),
	( ExtractedSourceInfo == [] ->
	  Results = ModRest
	; extract_source_name(ExtractedSourceInfo, SourceName),
	  Results=[ev(NegValue,CUI,MetaTerm,SourceName,MetaWords,SemTypes,
		      MatchMap,InvolvesHead,IsOvermatch,SourceInfo,PosInfo)|ModRest]
	),
	filter_evaluations_excluding_sources_aux(Rest, RootSources, ModRest).


/* filter_evaluations_to_sts(+EvaluationsIn, +STs, -EvaluationsOut)

filter_evaluations_to_sts/3 removes those evaluations from EvaluationsIn
which do not represent concepts with some ST in STs producing EvaluationsOut. */

filter_evaluations_to_sts([],_,[]) :-
    !.
filter_evaluations_to_sts([First|Rest],STs,[First|FilteredRest]) :-
% temp
%    format('fets: ~p~n',[First]),
    First = ev(_NegValue,_CUI,_MetaTerm,_MetaConcept,_MetaWords,SemTypes,
	       _MatchMap,_InvolvesHead,_IsOvermatch,_SourceInfo,_PosInfo),
    intersection(SemTypes,STs,Intersection),
% temp
%    format(' Intersection=~p~n',[Intersection]),
    Intersection\==[],
    !,
% temp
%    format(' ok.~n',[]),
    filter_evaluations_to_sts(Rest,STs,FilteredRest).
filter_evaluations_to_sts([_First|Rest],STs,FilteredRest) :-
% temp
%    format(' filtered out.~n',[]),
    filter_evaluations_to_sts(Rest,STs,FilteredRest).


/* filter_evaluations_excluding_sts(+EvaluationsIn, +STs, -EvaluationsOut)

filter_evaluations_excluding_sts/3 removes those evaluations from
EvaluationsIn which represent concepts with any ST in STs producing
EvaluationsOut. */

filter_evaluations_excluding_sts([],_,[]) :-
    !.
filter_evaluations_excluding_sts([First|Rest],STs,[First|FilteredRest]) :-
% temp
%    format('fets: ~p~n',[First]),
    First = ev(_NegValue,_CUI,_MetaTerm,_MetaConcept,_MetaWords,SemTypes,
	       _MatchMap,_InvolvesHead,_IsOvermatch,_SourceInfo,_PosInfo),
    intersection(SemTypes,STs,Intersection),
% temp
%    format(' Intersection=~p~n',[Intersection]),
    Intersection==[],
    !,
% temp
%    format(' ok.~n',[]),
    filter_evaluations_excluding_sts(Rest,STs,FilteredRest).
filter_evaluations_excluding_sts([_First|Rest],STs,FilteredRest) :-
% temp
%    format(' filtered out.~n',[]),
    filter_evaluations_excluding_sts(Rest,STs,FilteredRest).

construct_best_mappings(_Evaluations, PhraseTextString, Phrase,
			PhraseWordInfoPair, Variants, APhrases, BestMaps) :-
	% special clause for detecting structures such as
	% amino acid sequences (e.g., His-Ala-Asp-Gly-...); and
	% other hyphenated structures (e.g., (D)-C-Q-W- A-V-G-H-L-C-NH2)
	% to prevent computing mappings
	length(Phrase, PhraseLength),
	PhraseLength > 5,
	PhraseWordInfoPair=_:pwi(wdl(_,LCFilteredWords),_,_),
	( contains_n_amino_acids(LCFilteredWords,3)
	; text_contains_n_hyphens_within_word(PhraseTextString, 4)
	),
	!,
	AllMappings = [],
	construct_best_mappings_1(Phrase, PhraseWordInfoPair,
				  AllMappings, Variants, APhrases, BestMaps).

construct_best_mappings(Evaluations, _PhraseTextString, Phrase,
			PhraseWordInfoPair, Variants, APhrases, BestMaps) :-
	augment_evaluations(Evaluations, AEvaluations),
	% consider doing construction, augmentation and filtering more linearly
	% to increase control and to avoid keeping non-optimal results when
	% control_option(compute_all_mappings) is not in effect
	( check_construct_best_mappings_control_options ->
	  debug_call(trace, length(AEvaluations, AEvaluationsLength)), 
	  debug_message(trace, '~n### Calling construct_all_mappings on ~w AEvs~n', [AEvaluationsLength]),
	  % write_aevs(AEvaluations),
	  construct_all_mappings(AEvaluations, AllMappings),
	  % construct_all_mappings_OLD(AEvaluations, AllMappings)
	  % user:'=?'(AllMappings, OldAllMappings),
	  debug_call(trace, length(AllMappings, AllMappingsLength)),
	  debug_message(trace, '~n### DONE with construct_all_mappings: ~w~n', [AllMappingsLength])
	; AllMappings = []
	),
	construct_best_mappings_1(Phrase, PhraseWordInfoPair,
				  AllMappings, Variants, APhrases, BestMaps).

%%% write_aevs([]).
%%% write_aevs([H|T]) :-
%%% 	format(user_output, '~N~q~n', [H]),
%%% 	write_aevs(T).


construct_best_mappings_1(Phrase, PhraseWordInfoPair,
			  AllMappings, Variants, APhrases, BestMaps) :-
	debug_message(trace, '~N### Calling augment_phrase_with_mappings~n', []),
	augment_phrase_with_mappings(AllMappings, Phrase,
				     PhraseWordInfoPair, Variants, APhrases0),
	% The sort is now done inside augment_phrase_with_mappings
	% sort(APhrases0,APhrases1),
	debug_message(trace, '~N### Calling conditionally_filter_best_aphrases~n', []),
	conditionally_filter_best_aphrases(APhrases0, APhrases),
	debug_message(trace, '~N### Calling aphrases_maps~n', []),
	aphrases_maps(APhrases, BestMaps),
	debug_message(trace, '~N### Done with aphrases_maps~n', []).

conditionally_filter_best_aphrases([], []).
conditionally_filter_best_aphrases([H|T], APhrases) :-
	APhrases0 = [H|T],
	( \+ control_option(compute_all_mappings) ->
	  H = ap(BestValue,_,_,_),
	  filter_best_aphrases(APhrases0, BestValue, APhrases)
	; APhrases = APhrases0
	).	

contains_n_amino_acids(LCWords, N) :-
	contains_n_amino_acids(LCWords, 0, N).


contains_n_amino_acids([First|Rest], I, N) :-
	( I =:= N ->
	  true
	; is_an_amino_acid(First) ->
	  J is I + 1,
	  contains_n_amino_acids(Rest, J, N)
	; contains_n_amino_acids(Rest, I, N)
	).

is_an_amino_acid(ala).
is_an_amino_acid(arg).
is_an_amino_acid(gly).
is_an_amino_acid(leu).
is_an_amino_acid(glu).
is_an_amino_acid(lys).
is_an_amino_acid(asp).
is_an_amino_acid(ser).
is_an_amino_acid(asn).
is_an_amino_acid(gln).
is_an_amino_acid(tyr).
is_an_amino_acid(phe).
is_an_amino_acid(ile).
is_an_amino_acid(thr).
is_an_amino_acid(pro).
is_an_amino_acid(val).
is_an_amino_acid(cys).
is_an_amino_acid(his).
is_an_amino_acid(met).
is_an_amino_acid(trp).

text_contains_n_hyphens_within_word(PhraseText, N) :-
	( atom(PhraseText) ->
	  atom_codes(PhraseText, PhraseString)
	; PhraseString = PhraseText
	),
	contains_n_hyphens_within_word(PhraseString, 0, N).

contains_n_hyphens_within_word(_,N,N) :-
    !.
contains_n_hyphens_within_word("",_,_) :-
    !,
    fail.
contains_n_hyphens_within_word([0'-|Rest],I,N) :-
    !,
    J is I+1,
    contains_n_hyphens_within_word(Rest,J,N).
contains_n_hyphens_within_word([0' |Rest],_I,N) :-
    !,
    contains_n_hyphens_within_word(Rest,0,N).
contains_n_hyphens_within_word([_|Rest],I,N) :-
    contains_n_hyphens_within_word(Rest,I,N).

augment_evaluations([], []).
augment_evaluations([First|Rest], [AugmentedFirst|AugmentedRest]) :-
	augment_one_evaluation(First, AugmentedFirst),
	augment_evaluations(Rest, AugmentedRest).

% augmenting an evaluation
% * changes the functor from ev/11 to aev/14,
% * adds the merged phrase component of the MatchMap, and
% * adds the span of the entire MatchMap.
augment_one_evaluation(First, AugmentedFirst) :-
	First = ev(NegValue,CUI,MetaTerm,MetaConcept,MetaWords,SemTypes,
		   MatchMap,InvolvesHead,IsOvermatch,SourceInfo,PosInfo),
	extract_components(MatchMap, PhraseComponents0, _MetaComponents),
	sort(PhraseComponents0, PhraseComponents1),
	merge_contiguous_components(PhraseComponents1, PhraseComponents),
	compute_component_span(PhraseComponents, Low, High),
	AugmentedFirst = aev(PhraseComponents,Low,High,
			     NegValue,CUI,MetaTerm,MetaConcept,MetaWords,SemTypes,
			     MatchMap,InvolvesHead,IsOvermatch,SourceInfo,PosInfo).

filter_best_aphrases([], _BestValue, []).
filter_best_aphrases([First|Rest], BestValue, Filtered) :-
	First = ap(Value,_,_,_),
	( Value =:= BestValue ->
	  Filtered = [First|RestFiltered],
	  filter_best_aphrases(Rest, BestValue, RestFiltered)
	; Filtered = []
	).

aphrases_maps([], []).
aphrases_maps([ap(-1000,_,_,[])|Rest], RestMaps) :-
	!,
	aphrases_maps(Rest, RestMaps).
aphrases_maps([ap(NegValue,_,_,Mapping)|Rest], [map(NegValue,Mapping)|RestMaps]) :-
	aphrases_maps(Rest, RestMaps).

construct_all_mappings(AEvaluations, Mappings) :-
	expand_aevs(AEvaluations, 1, ExpandedMappings),
	debug_message(trace, '~N### Assembling~n', []),
	assemble_all_mappings(ExpandedMappings, [], Mappings0),
	debug_call(trace, length(Mappings0, Mappings0Length)),
	debug_message(trace, '~N### Flattening ~w list(s)~n', [Mappings0Length]),
	ChunkSize = 1000,
	postprocess_all_mappings_lists(Mappings0, 1, ChunkSize, Mappings0Length, Mappings1),
	flatten_all_mappings(Mappings1, [], Mappings2),
	debug_call(trace, length(Mappings2, Mappings2Length)),
	debug_message(trace, '~N### Deaugmenting and Reordering ~w mapping(s)~n', [Mappings2Length]),
	% deaugment_and_reorder_all_mappings(Mappings2, Mappings3),
	Mappings3 = Mappings2,
	% We want to avoid calling filter_out_subsumed_mappings if possible,
	% because it's extremely computationally intensive.
	% Counterintuitively, we must call filter_out_subsumed_mappings
	% if and only if compute_all_mappings is on.
	% The reason is that conditionally_filter_best_aphrases/2
	% will keep only the best-scoring mappings anyway.
	% ( control_option(compute_all_mappings) ->
 	filter_out_subsumed_mappings_test(Mappings3, ChunkSize, Mappings).
	% ; Mappings = Mappings3
	% ).

postprocess_all_mappings_lists([], _N, _ChunkSize, _Mappings0Length, []).
postprocess_all_mappings_lists([H|T], N, ChunkSize, Mappings0Length, [NewH|NewT]) :-
	debug_message(trace, '~N### Postprocessing list ~w of ~w~n', [N, Mappings0Length]),
        postprocess_one_mappings_list(H, ChunkSize, NewH),
	N1 is N + 1,
        postprocess_all_mappings_lists(T, N1, ChunkSize, Mappings0Length, NewT).
        
postprocess_one_mappings_list(Mappings0, ChunkSize, Mappings4) :-
        Mappings0 = [H|_],
        ( is_aev_term(H) ->
          Mappings1 = [Mappings0]
        ; Mappings1 = Mappings0
        ),
        flatten_all_mappings(Mappings1, [], Mappings2),
        deaugment_and_reorder_all_mappings(Mappings2, Mappings3),
        filter_out_subsumed_mappings_test(Mappings3, ChunkSize, Mappings4).


% At this stage, a mapping is a list of aev/14 terms.
% The Mappings0 variable coming out of assemble_all_mappings/3
% contains mappings nested to varying depths, so we must flatten the list
% to create a list of lists of aev/14 terms -- i.e., a list of Mappings.
% This predicate also has the side effect of reversing the order
% of the lists, which, mirabile dictu, is actually desireable!
flatten_all_mappings([], Mappings, Mappings).
flatten_all_mappings([H|T], MappingsIn, MappingsOut) :-
	flatten_mappings_1(H, MappingsIn, MappingsNext),
	flatten_all_mappings(T, MappingsNext, MappingsOut).

flatten_mappings_1(MappingsList, MappingsIn, MappingsOut) :-
	MappingsList = [H|_T],
	% We will assume that if the first element of MappingsList is an AEV term,
	% then all of the elements of MappingsList are AEV terms.
	( is_aev_or_ev_term(H) ->
	  MappingsOut = [MappingsList|MappingsIn]
	; flatten_all_mappings(MappingsList, MappingsIn, MappingsOut)
	).

% In the description below, "aev/14" represents an aev/14 term.

% A Final Mapping is a term of the form
% []-[aev/14]

% A Temporary Mapping is either a final mapping or a term of the form
% X-[aev/14]
% where X is a list of Temporary Mappings.

% Use the original algorithm to create InitialMappings,
% a list of terms of the form AEvaluationsInOut-[ChosenAEvaluation].
% AEvaluations is a possibly empty list of aev/14 terms.
% If AEvalutionsInOut == [], the mapping is final;
% if AEvalutionsInOut = [_|_], the mapping needs to be expanded.
expand_aevs([], _Debug, []).
expand_aevs([H|T], Debug, ExpandedMappings) :-
	% length(AEvaluations, AEvaluationsLength),
	% debug_message(trace, '~N### Expanding ~w AEvs~n', [AEvaluationsLength]),
	AEvaluations = [H|T],
        create_initial_mappings(T, H, AEvaluations, InitialMappings),
	length(InitialMappings, InitialMappingsLength), 
        expand_all_mappings(InitialMappings, Debug, 1, InitialMappingsLength, ExpandedMappings).

conditionally_debug_message(0, _, _).
conditionally_debug_message(1, Message, Args) :-
	debug_message(trace, Message, Args).


create_initial_mappings([], H, [H], [[]-[H]]).
create_initial_mappings([Next|Rest], First, [First,Next|Rest], InitialMappings) :-
	create_initial_mappings_1([First,Next|Rest], [First,Next|Rest], InitialMappings).


create_initial_mappings_1([], _AllAEvaluations, []).
create_initial_mappings_1([FirstAEv|RestAEvs], AllAEvaluations, InitialMappings) :-
	FirstAEv = aev(_PhraseComponents1,_Low1,_High1,
		       _NegValue,CUI,MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		       _MatchMap,_InvolvesHead,_IsOvermatch,_SourceInfo,_PosInfo),
	% ChosenAEvaluation is simply FirstAEv with its CUI and MetaTerm prepended
	% This prepending is just an efficiency hack to reduce the amount of
	% unification done in choose_an_aevaluation_aux/3.
	ChosenAEvaluation = CUI-MetaTerm-FirstAEv,
	( choose_an_aevaluation(AllAEvaluations, ChosenAEvaluation, RestAEvaluations) ->
	  filter_aevaluations_by_matchmap(RestAEvaluations, FirstAEv, AEvaluationsInOut),
	  InitialMappings = [FirstInitMapping|RestInitMappings],
	  FirstInitMapping = AEvaluationsInOut-[FirstAEv]
	; RestInitMappings = InitialMappings
	),
	create_initial_mappings_1(RestAEvs, AllAEvaluations, RestInitMappings).

expand_all_mappings([], _Debug, _N, _NumMappings, []).
expand_all_mappings([H|T], Debug, N, NumMappings, [ExpandedH|ExpandedT]) :-
        H = X-Y,
	Y = [M],
	conditionally_debug_message(Debug, '~N### Expanding ~w of ~w: ~q~n', [N, NumMappings, M]),
        expand_one_mapping(X, Y, ExpandedH),
	N1 is N + 1,
        expand_all_mappings(T, Debug, N1, NumMappings, ExpandedT).

% If X (i.e., AEvaluationsInOut) == [], the mapping is final.
% Otherwise, the mapping needs to be expanded.

% ExpandedMapping is
% []-[AEv] or
%  X-[AEv], where X is a list of ExpandedMappings

expand_one_mapping([], [ChosenAEv], []-[ChosenAEv]).
expand_one_mapping([H|T], [ChosenAEv], ExpandedMappings-[ChosenAEv]) :-
        expand_aevs([H|T], 0, ExpandedMappings).


% :- use_module(library(addportray)).
% portray_aev([]-[aev(_,_,_,_,CUI,_,_,_,_,_,_,_,_,_)]) :- write(final(CUI)).
% portray_aev(aev(_,_,_,_,CUI,_,_,_,_,_,_,_,_,_)) :- write(aev(CUI)).
% :- add_portray(portray_aev).
% portray_ev(ev(_,CUI,_,_,_,_,_,_,_,_,_)) :- write(ev(CUI)).
% :- add_portray(portray_ev).

% assemble_all_mappings/2 is called on a list of ExpandedMappings.
assemble_all_mappings([], _AEvs, []).
assemble_all_mappings([H|T], AEvs, [AssembledH|AssembledT]) :-
        H = X-Y,
        assemble_mappings_1(X, Y, AEvs, AssembledH),
        assemble_all_mappings(T, AEvs, AssembledT).

% A Mapping is a list of aev/14 terms
assemble_mappings_1([], Y, AEvs, Result) :-
	append(Y, AEvs, Result).
assemble_mappings_1([H|T], [ChosenMapping], AEvs, Mappings) :-
        assemble_all_mappings([H|T], [ChosenMapping|AEvs], Mappings).

is_aev_or_ev_term(T) :-
	( is_aev_term(T) ->
	  true
	; is_ev_term(T)
	).

is_aev_term(aev(_,_,_,_,_,_,_,_,_,_,_,_,_,_)).

is_ev_term(ev(_,_,_,_,_,_,_,_,_,_,_)).

/* choose_an_aevaluation(+AEvaluations, -AEvaluation, -RestAEvaluations)
   choose_an_aevaluation_aux(+AEvaluations, -AEvaluation, -RestAEvaluations)

choose_an_aevaluation/3
xxx
*/

% AEvaluations come in sorted by (1) NegScore and (2) CUI.
% First, mark the AEvaluations as follows:
% Begin by determining, for each AEvaluation, if it interacts with the first AEv in the list.
% That gives us a list of Results, which are terms of the form X-Y.
% Y is the position of the AEv in the list.
% X == 1 means that the AEv does interact with the first AEv in the list;
% X == 0 means that the AEv does NOT interact with the first AEv in the list.

% Next, given this result list, e.g., [1-1, 1-2, 1-3, 1-4, 0-5, 0-6, 1-7],
% identify the last X-Y in the list whose X == 1. In the above example, it is 1-7.

% Finally, mark as 1 the last AEvaluation in the list whose X == 1,
% and mark as 0 all the others.

% After the AEvaluations are marked, nondeterministically return in
% * AEvaluation: each of them in order, and in
% * RestAEvaluations all AEvaluations after the chosen one
% up to and including the one marked 1.
% The evaluations occurring in the list after the one marked 1,
% which are not chosen, do not overlap with the first evaluation.
% It's possible that a chosen AEvaluation does not overlap with the first one
% (e.g. in the case of "very high gravity medium").

choose_an_aevaluation(AEvaluations, ChosenAEvaluation, RestAEvaluations) :-
	% format(user_output, '~NAEvs = ~q,~n~n', [AEvaluations]),
	mark_aevaluations(AEvaluations, MarkedAEvaluations),

	% MarkedAEvaluations = [_OneOrZero-FirstAEv|_],
	% FirstAEv = aev(PhraseComponents1,Low1,High1,_,_,_,_,_,_,_,_,_,_,_),

	% mark_aevaluations(AEvaluations, MarkedAEvaluations),
	% format(user_output, '~nMarkedAEvs = ~q,~n~n', [MarkedAEvaluations]),
	choose_an_aevaluation_aux(MarkedAEvaluations, ChosenAEvaluation, RestAEvaluationTerms),
	% format(user_output, '~nChosen = ~q,~n~n', [ChosenAEvaluation]),
	% ChosenAEvaluation = aev(PhraseComponents2,Low2,High2,_,_,_,_,_,_,_,_,_,_,_),
	% ( aevaluations_interact(PhraseComponents1,Low1,High1,
	% 			PhraseComponents2,Low2,High2) ->
	%   true
	% ; format(user_output, '~N### NO INTERACTION:~n### FIRST: ~q~n### CHOSEN: ~q~n', [FirstAEv,ChosenAEvaluation])
	% ),
	unmark_aevaluations(RestAEvaluationTerms, RestAEvaluations).


choose_an_aevaluation_aux([OneOrZero:CUI1-MetaTerm1-_AEv1|Rest],
			  ChosenAEvaluation, RestAEvs) :-
	ChosenAEvaluation = CUI2-MetaTerm2-_AEv2,
	( CUI1 == CUI2,
	  MetaTerm1 == MetaTerm2 ->
	  RestAEvs = Rest,
	!
	; OneOrZero =:= 1 ->
	!,
	fail
	; choose_an_aevaluation_aux(Rest, ChosenAEvaluation, RestAEvs)
	).
	
unmark_aevaluations([], []).
unmark_aevaluations([_OneOrZero:_CUI-_MetaTerm-AEv|RestTerms], [AEv|RestAEvs]) :-
	unmark_aevaluations(RestTerms, RestAEvs).

mark_aevaluations(AEvaluations, MarkedAEvaluations) :-
	AEvaluations = [H|_T],
	H = aev(PhraseComponents0,Low0,High0,_,_,_,_,_,_,_,_,_,_SrcInfo,_PosInfo),
	% Create Results, 
	determine_interacting_aevs(AEvaluations, 1, PhraseComponents0, Low0, High0, Results),
	find_last_1(Results, Last1),
	mark_aevaluations_aux(AEvaluations, 1, Last1, MarkedAEvaluations).

determine_interacting_aevs([], _AEvPosition, _PhraseComponents0, _Low0, _High0, []).
determine_interacting_aevs([FirstAEv|RestAEvs], AEvPosition,
			PhraseComponents0, Low0, High0,
			[FirstResult|RestResults]) :-
	FirstAEv = aev(PhraseComponents1,Low1,High1,
		       _NegValue,_CUI,_MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		       _MatchMap,_InvolvesHead,_IsOvermatch,_SourceInfo,_PosInfo),
	( aevaluations_interact(PhraseComponents0, Low0, High0,
				PhraseComponents1, Low1, High1) ->
	  % FirstResult is a term of the form X-Y.
	  % This is NOT arithmetic evaluation, but unification!!
	  FirstResult = 1-AEvPosition
	; FirstResult = 0-AEvPosition
	),
	NextAEvPosition is AEvPosition + 1,
	determine_interacting_aevs(RestAEvs, NextAEvPosition,
				   PhraseComponents0, Low0, High0,
				   RestResults).

mark_aevaluations_aux([], _AEvPosition, _Last1, []).
mark_aevaluations_aux([FirstAEv|RestAEvs], AEvPosition, Last1,
		      [FirstMarkedAEv|RestMarkedAEvs]) :-
	FirstAEv = aev(_PhraseComponents1,_Low1,_High1,
		       _NegValue,CUI,MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		       _MatchMap,_InvolvesHead,_IsOvermatch,_SourceInfo,_PosInfo),
	( AEvPosition =:= Last1 ->
	  FirstMarkedAEv = 1:CUI-MetaTerm-FirstAEv
	; FirstMarkedAEv = 0:CUI-MetaTerm-FirstAEv
	),
	NextAEvPosition is AEvPosition + 1,
	mark_aevaluations_aux(RestAEvs, NextAEvPosition, Last1, RestMarkedAEvs).

% given a list of terms of the form X-Y, where
% * X is either 1 or 0, and
% * Y is integers (actually irrelevant to this predicate),
% return the value of Y for the *last* X-Y in the list for which X == 1.

% E.g., given
% [1-1, 1-2, 1-3, 0-4, 1-5, 1-6, 0-7, 0-8, 1-9, 1-10],
% return 10.
find_last_1([_X-Y|T], Last1) :-
	find_last_1_aux(T, Y, Last1).

find_last_1_aux([], LastY, LastY).
find_last_1_aux([Next|Rest], Current, LastY) :-
	( Next = 1-Mark ->
	  find_last_1_aux(Rest, Mark, LastY)
	; find_last_1_aux(Rest, Current, LastY)
	).

aevaluations_interact(PhraseComponents0, Low0, High0,
		      PhraseComponents1, Low1, High1) :-
	( components_intersect_components(PhraseComponents0, PhraseComponents1) ->
	  true
	; spans_overlap(Low0, High0, Low1, High1) ->
	  true
	; is_proper_subspan(Low1, High1, Low0, High0),
	  component_intersects_components(PhraseComponents0, [Low1,High1]) ->
	  true
	; is_proper_subspan(Low0, High0, Low1, High1),
	  component_intersects_components(PhraseComponents1, [Low0,High0])
	).

%%% mark_aevaluations(AEvaluations, MarkedAEvaluations) :-
%%% 	% length(AEvaluations, AEvaluationsLength),
%%% 	% format(user_output, '~n### Marking ~w AEvaluations~n', [AEvaluationsLength]),
%%% 	% Get the PhraseComponents, Low, and High of the first (i.e., best) AEvaluation
%%% 	AEvaluations = [H|_T],
%%% 	H = aev(_,PhraseComponents0,Low0,High0,_,_,_,_,_,_,_,_,_,_SrcInfo,_PosInfo),
%%% 	rev(AEvaluations, RevAEvaluations),
%%% 	mark_aevaluations_1(RevAEvaluations, PhraseComponents0, Low0, High0, RevMarkedAEvaluations),
%%% 	rev(RevMarkedAEvaluations, MarkedAEvaluations).
%%% 
%%% % PhraseComponent0, Low0, and High0 come from the first (i.e., the best) evaluation.
%%% % All but one of the AEvaluations are marked "go".
%%% % The one marked "stop" is the first one (working from the end of the list)
%%% % that interacts with the first (best) evaluation.
%%% mark_aevaluations_1([], _, _, _, []).
%%% mark_aevaluations_1([First|Rest], PhraseComponents0, Low0, High0, [MarkedFirst|MarkedRest]) :-
%%% 	First = aev(_,PhraseComponents1,Low1,High1,
%%% 		    NegValue,CUI,MetaTerm,MetaConcept,MetaWords,SemTypes,
%%% 		    MatchMap,InvolvesHead,IsOvermatch,SourceInfo,PosInfo),
%%% 	( aevaluations_interact(PhraseComponents0, Low0, High0,
%%% 				PhraseComponents1, Low1, High1) ->
%%%           MarkedFirst = aev(stop,PhraseComponents1,Low1,High1,
%%% 			    NegValue,CUI,MetaTerm,MetaConcept,MetaWords,SemTypes,
%%% 			    MatchMap,InvolvesHead,IsOvermatch,SourceInfo,PosInfo),
%%% 	  mark_aevaluations_go(Rest, MarkedRest)
%%% 	; MarkedFirst = aev(go,PhraseComponents1,Low1,High1,
%%% 			    NegValue,CUI,MetaTerm,MetaConcept,MetaWords,SemTypes,
%%% 			    MatchMap,InvolvesHead,IsOvermatch,SourceInfo,PosInfo),
%%% 	  mark_aevaluations_1(Rest, PhraseComponents0, Low0, High0, MarkedRest)
%%% 	).
%%% 
%%% mark_aevaluations_go([], []).
%%% mark_aevaluations_go([aev(_,PhraseComponents,Low,High,
%%%                           NegValue,CUI,MetaTerm,MetaConcept,MetaWords,SemTypes,
%%%                           MatchMap,InvolvesHead,IsOvermatch,SourceInfo,PosInfo)|Rest],
%%%                      [aev(go,PhraseComponents,Low,High,
%%%                           NegValue,CUI,MetaTerm,MetaConcept,MetaWords,SemTypes,
%%%                           MatchMap,InvolvesHead,IsOvermatch,SourceInfo,PosInfo)|MarkedRest]) :-
%%% 	mark_aevaluations_go(Rest, MarkedRest).

/* filter_aevaluations_by_matchmap(+AEvaluationsIn, +AEvaluation,
                                   -AEvaluationsOut)
   filter_aevaluations_by_matchmap(+AEvaluationsIn, +FilterPhraseComponents,
                                   +FilterLow, +FilterHigh, -AEvaluationsOut)

filter_aevaluations_by_matchmap/3
filter_aevaluations_by_matchmap_1/5
xxx
*/

filter_aevaluations_by_matchmap([], _AEvaluation, []).
filter_aevaluations_by_matchmap([H|T], AEvaluation, AEvaluationsOut) :-
	AEvaluation = aev(PhraseComponents,Low,High,_,_,_,_,_,_,_,_,_,_SrcInfo,_PosInfo),
	filter_aevaluations_by_matchmap_1([H|T], PhraseComponents, Low, High, AEvaluationsOut).

filter_aevaluations_by_matchmap_1([], _FilterPhraseComponents, _FilterLow, _FilterHigh, []).
filter_aevaluations_by_matchmap_1([First|Rest], FilterPhraseComponents, FilterLow, FilterHigh, Result) :-
	First = aev(PhraseComponents,Low,High,_,_,_,_,_,_,_,_,_,_SrcInfo,_PosInfo),
	( aevaluations_interact(PhraseComponents, Low, High,
				FilterPhraseComponents, FilterLow, FilterHigh) ->
	  Result = FilteredRest
	; Result = [First|FilteredRest]
	),
	filter_aevaluations_by_matchmap_1(Rest, FilterPhraseComponents,
					  FilterLow, FilterHigh, FilteredRest).

/* components_intersect_components(+Components1, +Components2)

components_intersect_components/2
xxx
*/

components_intersect_components([First|Rest], Components) :-
	( component_intersects_components(Components, First) ->
	  true
	; components_intersect_components(Rest,Components)
	).

/* compute_component_span(+Components, -Low, -High)

compute_component_span/3
xxx
*/

compute_component_span(Components, Low, High) :-
	Components = [[Low,_]|_],
	% reversed order of args from QP library version!
	last(Components, [_,High]).

/* spans_overlap(+Low1, +High1, +Low2, +High2)

spans_overlap/4
xxx
*/

spans_overlap(Low1, High1, Low2, High2) :-
	( Low1 < Low2,
	  Low2 < High1,
	  High1 < High2 ->
	  true
	; Low2 < Low1,
	  Low1 < High2,
	  High2 < High1
	).

/* is_proper_subspan(+Low1, +High1, +Low2, +High2)

is_proper_subspan/4
xxx
*/

is_proper_subspan(Low1, High1, Low2, High2) :-
	Low2 < Low1,
	High1 < High2.


/* filter_out_subsumed_mappings(+Mappings, -FilteredMappings)

filter_out_subsumed_mappings/2
xxx

A mapping M1 is subsumed by another M2 if all of M1's components occur in M2.

% filter_out_subsumed_mappings([], []).
% filter_out_subsumed_mappings([First|Rest], Result) :-
%       ( mapping_is_subsumed(Rest, First) ->
%         Result = FilteredRest
%       ; Result = [First|FilteredRest]
%       ),
%       filter_out_subsumed_mappings(Rest, FilteredRest).
*/

% The following is a more complicated, but more efficient way
% of filtering out subsumed mappings. It's more efficient because
% the old way called intersection/3 on the entire mappings, which required
% a lot of unification on the mappings terms.
% This method uses the same strategy, but unifies only CUIs and the MetaString.

filter_out_subsumed_mappings_test(MappingsIn, ChunkSize, FilteredMappings) :-
	length(MappingsIn, MappingsLength),
	( MappingsLength =< ChunkSize ->
	  filter_out_subsumed_mappings(MappingsIn, ChunkSize, 1, 1, FilteredMappings)
	; debug_message(trace, '~N### Splitting list of ~w~n', [MappingsLength]),
	  split_list(MappingsIn, 0, ChunkSize, MappingsChunked, 1, NumLists),
	  filter_out_each(MappingsChunked, ChunkSize, 1, NumLists, FilteredMappingsChunked),
	  append(FilteredMappingsChunked, TempFilteredMappings),
	  length(TempFilteredMappings, TempFilteredMappingsLength),
	  % filter_out_subsumed_mappings(TempFilteredMappings, 1, 1, FilteredMappings),
	  filter_out_subsumed_mappings_again(MappingsLength, ChunkSize,
					     TempFilteredMappingsLength,
					     TempFilteredMappings, FilteredMappings)
	).

% Continue splitting the list of mappings iff the previous split-and-filter pass
% reduced the number of mappings by at least 20%.
filter_out_subsumed_mappings_again(MappingsLength, ChunkSize, TempFilteredMappingsLength,
				   TempFilteredMappings, FilteredMappings) :-
	Ratio is MappingsLength / TempFilteredMappingsLength,
	debug_message(trace,
		      '~N### Ratio of ~w to ~w is ~w~n',
		      [MappingsLength,TempFilteredMappingsLength,Ratio]),
	( Ratio > 1.10 ->
	  filter_out_subsumed_mappings_test(TempFilteredMappings, ChunkSize, FilteredMappings)
	; filter_out_subsumed_mappings(TempFilteredMappings, ChunkSize, 1, 1, FilteredMappings)
	).	

split_list([], _N, _Limit, [[]], NumLists, NumLists).
split_list([H|T], N, Limit, ListOfLists, ListCount, NumLists) :-
	( N =:= Limit ->
	  ListOfLists = [[]|RestLists],
	  NextListCount is ListCount + 1,
	  split_list([H|T], 0, Limit, RestLists, NextListCount, NumLists)
	; ListOfLists = [[H|Rest]|RestLists],
	  N1 is N + 1,
	  split_list(T, N1, Limit, [Rest|RestLists], ListCount, NumLists)
	).

filter_out_each([], _ChunkSize, _ListCount, _NumLists, []).
filter_out_each([H|T], ChunkSize, ListCount, NumLists, [FilteredH|FilteredT]) :-
	filter_out_subsumed_mappings(H, ChunkSize, ListCount, NumLists, FilteredH),
	NextListCount is ListCount + 1,
	filter_out_each(T, ChunkSize, NextListCount, NumLists, FilteredT).

% This is the basic filtering predicate that operates on lists of mappings
filter_out_subsumed_mappings(Mappings, ChunkSize, ListCount, NumLists, FilteredMappings) :-
	debug_call(trace, length(Mappings, MappingsLength)),
	debug_message(trace, '~N### Filtering ~w of ~w: ~w',
		      		[ListCount, NumLists, MappingsLength]),
	ttyflush,
	prepend_data(Mappings, MappingsWithPrependedData),
        filter_out_subsumed_mappings_aux(MappingsWithPrependedData, ChunkSize, 0, FilteredMappings),
	debug_call(trace, length(FilteredMappings, FilteredMappingsLength)),
	debug_message(trace, ' --> ~w~n', [FilteredMappingsLength]),
	ttyflush.

filter_out_subsumed_mappings_aux([], _ChunkSize, _N, []).
filter_out_subsumed_mappings_aux([FirstData-FirstMapping|RestMappings],
				 ChunkSize, MappingsFiltered, Result) :-
        ( mapping_is_subsumed(RestMappings, FirstData-FirstMapping) ->
          Result = FilteredRest
        ; Result = [FirstMapping|FilteredRest]
        ),
	( MappingsFiltered > 0,
	  0 is MappingsFiltered mod ChunkSize ->
	  debug_message(trace, '~N### Filtered ~w~n', [MappingsFiltered])
	; true
	),
	MappingsFiltered1 is MappingsFiltered + 1,
        filter_out_subsumed_mappings_aux(RestMappings, ChunkSize, MappingsFiltered1, FilteredRest).

/* mapping_is_subsumed(+Mappings, +Mapping)

mapping_is_subsumed/2
WATCH ORDER OF ARGS
xxx

% mapping_is_subsumed([First|Rest], Mapping) :-
%       ( intersection(Mapping, First, Mapping) ->
%         true
%       ; mapping_is_subsumed(Rest, Mapping)
%       ).

*/

       
mapping_is_subsumed([FirstMappingData-_FirstMapping|RestMappings], ThisMappingData-ThisMapping) :-
        ( intersection(ThisMappingData, FirstMappingData, ThisMappingData) ->
          true
        ; mapping_is_subsumed(RestMappings, ThisMappingData-ThisMapping)
        ).

prepend_data([], []).
prepend_data([FirstMapping|RestMappings], [Data-FirstMapping|RestMappingsWithMSs]) :-
        get_data_for_mapping(FirstMapping, Data),
        prepend_data(RestMappings, RestMappingsWithMSs).

get_data_for_mapping([], []).
get_data_for_mapping([FirstEval|RestEvals], [FirstCUI/FirstMS|RestData]) :-
        FirstEval = ev(_NegValue,FirstCUI,FirstMS,_MetaConcept,_MetaWords,_SemTypes,
                       _MatchMap,_InvolvesHead,_IsOvermatch,_Sources,_PosInfo),
        get_data_for_mapping(RestEvals, RestData).



% This predicate combines the de-augmentation and re-ordering
% so that the entire list need not be traversed twice.
deaugment_and_reorder_all_mappings([], []).
deaugment_and_reorder_all_mappings([H|T], [DeAugmentedAndReorderedH|DeAugmentedAndReorderedT]) :-
	% sort(H, SortedH),
	% deaugment_mapping_evaluations(SortedH, DeAugmentedAndReorderedH),
	deaugment_mapping_evaluations(H, DeAugmentedH),
	reorder_mapping(DeAugmentedH, DeAugmentedAndReorderedH),
	deaugment_and_reorder_all_mappings(T, DeAugmentedAndReorderedT).
	

/* deaugment_mapping_evaluations(+Mapping, -DeaugmentedMapping)

deaugment_mapping_evaluations/2
xxx
*/

deaugment_mapping_evaluations([],[]).
deaugment_mapping_evaluations([FirstAEv|RestAEvs], [FirstEv|RestEvs]) :-
	deaugment_one_mapping_evaluation(FirstAEv, FirstEv),
	deaugment_mapping_evaluations(RestAEvs, RestEvs).

deaugment_one_mapping_evaluation(aev(_PhraseComponents,_Low,_High,
				     NegValue,CUI,MetaTerm,MetaConcept,
				     MetaWords,SemTypes,MatchMap,InvolvesHead,
				     IsOvermatch,SourceInfo,PosInfo),
				 ev(NegValue,CUI,MetaTerm,MetaConcept,
				    MetaWords,SemTypes,MatchMap,InvolvesHead,
				    IsOvermatch,SourceInfo,PosInfo)).

/* reorder_mapping(+Mapping, -OrderedMapping)

reorder_mapping/2
xxx
*/

reorder_mapping([], []).
reorder_mapping([H|T], OrderedMapping) :-
	prepend_phrase_maps([H|T], PrependedMapping),
	keysort(PrependedMapping, OrderedPrependedMapping),
	% to delete the phrase maps,
	% simply call prepend_phrase_maps/2 with the args reversed!
	prepend_phrase_maps(OrderedMapping, OrderedPrependedMapping).

/* prepend_phrase_maps(+Mapping, -PPMapping)

prepend_phrase_maps/2
xxx
*/

prepend_phrase_maps(MapsIn, MapsOut) :-
	( var(MapsIn) ->
	  prepend_phrase_maps_1(MapsOut, MapsIn)
	; prepend_phrase_maps_1(MapsIn, MapsOut)
	).

prepend_phrase_maps_1([], []).
prepend_phrase_maps_1([H|T], [PrePendedH|PrePendedT]) :-
	prepend_one_phrase_map(H, PrePendedH),
	prepend_phrase_maps_1(T, PrePendedT).

prepend_one_phrase_map(ev(NegValue,CUI,MetaTerm,MetaConcept,MetaWords,SemTypes,
			  MatchMap,InvolvesHead,IsOvermatch,SourceInfo,PosInfo),
		       MatchMap-ev(NegValue,CUI,MetaTerm,MetaConcept,MetaWords,SemTypes,
                                   MatchMap,InvolvesHead,IsOvermatch,SourceInfo,PosInfo)).

prepend_one_phrase_map(MatchMap-ev(NegValue,CUI,MetaTerm,MetaConcept,MetaWords,SemTypes,
                                   MatchMap,InvolvesHead,IsOvermatch,SourceInfo,PosInfo),
		       ev(NegValue,CUI,MetaTerm,MetaConcept,MetaWords,SemTypes,
			  MatchMap,InvolvesHead,IsOvermatch,SourceInfo,PosInfo)).


/* augment_phrase_with_mappings(+Mappings, +Phrase, +PhraseWordInfoPair, -APhrases)
   augment_lphrase_with_mappings(+Mappings, +LPhrase, +LPhraseMap, +NPhraseWords, -APhrases)
   augment_lphrase_with_mapping(+Mapping, +LPhrase, +LPhraseMap, +NPhraseWords, -APhrase)

augment_phrase_with_mappings/4
augment_lphrase_with_mappings/5
augment_lphrase_with_mapping/5
xxx
*/

augment_phrase_with_mappings([], _Phrase, _PhraseWordInfoPair, _Variants, []).
augment_phrase_with_mappings([H|T], Phrase, PhraseWordInfoPair, Variants, APhrases) :-
	MappingsList = [H|T],
	PhraseWordInfoPair = _AllPhraseWordInfo:FilteredPhraseWordInfo,
	PhraseWordInfo = FilteredPhraseWordInfo,
	PhraseWordInfo = pwi(PhraseWordL, _PhraseHeadWordL, PhraseMap),
	PhraseWordL = wdl(_, LCPhraseWords),
	linearize_phrase(Phrase, PhraseMap, LPhrase, LPhraseMap),
	% temp
	%dump_syntax_nicely('apwm LPhrase',LPhrase),
	length(LCPhraseWords, NPhraseWords),
	augment_lphrase_with_mappings(MappingsList, LPhrase, LPhraseMap,
				      NPhraseWords, Variants, APhrases0),
	sort(APhrases0, APhrases).

augment_lphrase_with_mappings([],_,_,_,_,[]).
augment_lphrase_with_mappings([First|Rest],LPhrase,LPhraseMap,
			      NPhraseWords, Variants,
			      [AugmentedFirst|AugmentedRest]) :-
	augment_lphrase_with_mapping(First,LPhrase,LPhraseMap,NPhraseWords,
				     Variants, AugmentedFirst),
	augment_lphrase_with_mappings(Rest,LPhrase,LPhraseMap,NPhraseWords,
				      Variants, AugmentedRest).

augment_lphrase_with_mapping(Mapping,LPhraseIn,LPhraseMapIn,NPhraseWords,
                             Variants, APhrase) :-
	% temp
	%dump_syntax_nicely('alwm LPhraseIn',LPhraseIn),
	augment_lphrase_with_meta_concepts(LPhraseIn,LPhraseMapIn,Mapping,
					   LPhraseInOut,LPhraseMapInOut),
	% temp
	%dump_syntax_nicely('alwm LPhraseInOut',LPhraseInOut),
	augment_lphrase_with_confidence_value(LPhraseInOut,LPhraseMapInOut,Mapping,
					      NPhraseWords,LPhraseOut,LPhraseMapOut,
					      Variants, NegValue),
	% temp
	%dump_syntax_nicely('alwm LPhraseOut',LPhraseOut),
	APhrase=ap(NegValue,LPhraseOut,LPhraseMapOut,Mapping).


/* augment_lphrase_with_meta_concepts(+LPhraseIn, +LPhraseMapIn, +Evaluations,
                                      -LPhraseOut, -LPhraseMapOut)
   augment_lphrase_with_meta_concept(+LPhraseIn, +LPhraseMapIn, +Evaluation,
                                     -LPhraseOut, -LPhraseMapOut)

augment_lphrase_with_meta_concepts/5
augment_lphrase_with_meta_concept/5
xxx
*/

augment_lphrase_with_meta_concepts(LPhraseIn,LPhraseMapIn,[],
                                   LPhraseIn,LPhraseMapIn) :-
    !.
augment_lphrase_with_meta_concepts(LPhraseIn,LPhraseMapIn,[First|Rest],
                                   LPhraseOut,LPhraseMapOut) :-
    augment_lphrase_with_meta_concept(LPhraseIn,LPhraseMapIn,First,
                                      LPhraseInOut,LPhraseMapInOut),
    augment_lphrase_with_meta_concepts(LPhraseInOut,LPhraseMapInOut,Rest,
                                       LPhraseOut,LPhraseMapOut).

augment_lphrase_with_meta_concept(LPhraseIn,LPhraseMapIn,Evaluation,
                                  LPhraseOut,LPhraseMapOut) :-
    %% AHA!!
    Evaluation = ev(_NegValue,CUI,_MetaTerm,MetaConcept,_MetaWords,SemTypes,
		    MatchMap,_InvolvesHead,_IsOvermatch,_SourceInfo,_PosInfo),
    extract_components(MatchMap,PhraseComponents,_MetaComponents),
    linearize_components(PhraseComponents,LComponents0),
    append(LComponents0,LComponents),
    rev(LComponents,[TargetLComponent|_]),
    ( var(SemTypes) ->
      MetaInfo=MetaConcept:CUI
    ; MetaInfo=MetaConcept:CUI:SemTypes
    ),
    join_phrase_items(LPhraseIn,LPhraseMapIn,MetaInfo,
                      LComponents,TargetLComponent,
                      LPhraseOut,LPhraseMapOut).


/* join_phrase_items(+LPhraseIn, +LPhraseMapIn, +MetaInfo, +LComponents,
                     +TargetLComponent, -LPhraseOut, -LPhraseMapOut)
   join_phrase_items(+LPhraseIn, +LPhraseMapIn,
                     +MetaInfo, +LComponents, +TargetLComponent,
                     +RevLexMatch, +RevInputMatch, +RevTokens,
                     -LPhraseOut, -LPhraseMapOut)

join_phrase_items/7
join_phrase_items/11
xxx
*/

join_phrase_items(LPhraseIn,LPhraseMapIn,MetaInfo,
                  LComponents,TargetLComponent,
                  LPhraseOut,LPhraseMapOut) :-
    join_phrase_items(LPhraseIn,LPhraseMapIn,MetaInfo,
                      LComponents,TargetLComponent,[],[],[],[],
                      LPhraseOut,LPhraseMapOut).

join_phrase_items([FirstItem|RestItems],[[TargetLComponent]|RestLMap],
                  MetaInfo,LComponents,TargetLComponent,
                  RevLexMatch,RevInputMatch,RevTokens,RevBases,
                  [NewFirstItem|RestItems],[LComponents|RestLMap]) :-
    !,
    % retrieve accumulated results and finish up
    get_phrase_item_name(FirstItem,ItemName),
    get_phrase_item_subitems(FirstItem,Subitems0),
    get_subitems_feature(Subitems0,lexmatch,NewLexMatch),
    get_subitems_feature(Subitems0,inputmatch,NewInputMatch),
    get_subitems_feature(Subitems0,tokens,NewToken),
    get_subitems_feature(Subitems0,bases,NewBase),
    (member(NewLexMatch,RevLexMatch) ->
        NewRevLexMatch=RevLexMatch
    ;   NewRevLexMatch=[NewLexMatch|RevLexMatch]
    ),
    rev(NewRevLexMatch,LexMatchList),
    append(LexMatchList,LexMatch),
    (member(NewInputMatch,RevInputMatch) ->
        NewRevInputMatch=RevInputMatch
    ;   NewRevInputMatch=[NewInputMatch|RevInputMatch]
    ),
    rev(NewRevInputMatch,InputMatchList),
    append(InputMatchList,InputMatch),
    NewRevTokens=[NewToken|RevTokens],
    rev(NewRevTokens,TokenList),
    append(TokenList,Tokens),
    NewRevBases=[NewBase|RevBases],
    rev(NewRevBases,BaseList),
    append(BaseList,Bases),
    (LexMatch==[] ->
        Subitems1 = []
    ;   set_subitems_feature(Subitems0,lexmatch,LexMatch,Subitems1)
    ),
    set_subitems_feature(Subitems1,inputmatch,InputMatch,Subitems2),
    set_subitems_feature(Subitems2,tokens,Tokens,Subitems3),
    set_subitems_feature(Subitems3,bases,Bases,Subitems4),
    set_subitems_feature(Subitems4,metaconc,[MetaInfo],Subitems),
    new_phrase_item(ItemName,Subitems,NewFirstItem).
join_phrase_items([FirstItem|RestItems],[[FirstLMapComponent]|RestLMap],
                  MetaInfo,LComponents,TargetLComponent,
                  RevLexMatch,RevInputMatch,RevTokens,RevBases,
                  JoinedRestItems,JoinedRestLMap) :-
    member(FirstLMapComponent,LComponents),
    !,
    % accumulate
    get_phrase_item_subitems(FirstItem,Subitems),
    get_subitems_feature(Subitems,lexmatch,NewLexMatch),
    get_subitems_feature(Subitems,inputmatch,NewInputMatch),
    get_subitems_feature(Subitems,tokens,NewToken),
    get_subitems_feature(Subitems,bases,NewBase),
    (member(NewLexMatch,RevLexMatch) ->
        NewRevLexMatch=RevLexMatch
    ;   NewRevLexMatch=[NewLexMatch|RevLexMatch]
    ),
    (member(NewInputMatch,RevInputMatch) ->
        NewRevInputMatch=RevInputMatch
    ;   NewRevInputMatch=[NewInputMatch|RevInputMatch]
    ),
    NewRevTokens=[NewToken|RevTokens],
    NewRevBases=[NewBase|RevBases],
    join_phrase_items(RestItems,RestLMap,
                      MetaInfo,LComponents,TargetLComponent,
                      NewRevLexMatch,NewRevInputMatch,NewRevTokens,NewRevBases,
                      JoinedRestItems,JoinedRestLMap).
join_phrase_items([FirstItem|RestItems],[FirstLMap|RestLMap],
                  MetaInfo,LComponents,TargetLComponent,
                  RevLexMatch,RevInputMatch,RevTokens,RevBases,
                  [FirstItem|JoinedRestItems],[FirstLMap|JoinedRestLMap]) :-
    % just go on
    join_phrase_items(RestItems,RestLMap,
                      MetaInfo,LComponents,TargetLComponent,
                      RevLexMatch,RevInputMatch,RevTokens,RevBases,
                      JoinedRestItems,JoinedRestLMap).


/* augment_lphrase_with_confidence_value(+LPhraseIn, +LPhraseMapIn, +Mapping,
                                        +NPhraseWords, -LPhraseOut,
                                        -LPhraseMapOut, -NegValue)

augment_lphrase_with_confidence_value/7
xxx
*/

augment_lphrase_with_confidence_value(LPhraseIn,LPhraseMapIn,[],
				      _NPhraseWords,LPhraseIn,LPhraseMapIn,
				      _Variants,
				      -1000) :-
    !.
augment_lphrase_with_confidence_value(LPhraseIn, LPhraseMapIn, Mapping,
				      NPhraseWords, LPhraseOut, LPhraseMapOut,
				      Variants, NegValue) :-
    glean_info_from_mapping(Mapping,[],MatchMap0,[],TermLengths,
                            0,NMetaWords,no,InvolvesHead,
                            ExtraMetaWords),
    sort(MatchMap0, MatchMap1),
    MatchMap1 = [MatchMapHead|MatchMapTail],
    consolidate_matchmap(MatchMapTail, MatchMapHead, MatchMap),
    % the connected components are computed in the normal fashion for
    % the phrase; but for Meta, the components are simply the lengths
    % of the terms participating in the mapping
    extract_components(MatchMap,PhraseComponents,_MetaComponents),
    connect_components(PhraseComponents,PhraseCCs),
    MetaCCs=TermLengths,
    MatchCCs=[PhraseCCs,MetaCCs],
    ( control_value(debug, DebugFlags),
      memberchk(5, DebugFlags) ->
      glean_concepts_from_mapping(Mapping,Concepts),
      format('~n',[]),
      wl(Concepts),
      format('~p~n~p~n',[MatchMap,MatchCCs])
    ; true
    ),
    compute_match_value(MatchMap,MatchCCs,NPhraseWords,NMetaWords,
                        ExtraMetaWords,Variants,
                        InvolvesHead,Value),
    ( control_value(debug, DebugFlags),
      memberchk(5, DebugFlags) ->
      format('~n',[])
    ; true
    ),
    NegValue is -Value,
    add_confidence_value(LPhraseIn,Value,LPhraseOut),
    append(LPhraseMapIn,[[0]],LPhraseMapOut).


/* glean_info_from_mapping(+Mapping, +MatchMapIn, -MatchMapOut,
                           +TermLengthsIn, -TermLengthsOut,
                           +NMetaWordsIn, -NMetaWordsOut,
                           +InvolvesHeadIn, -InvolvesHeadOut)
   glean_concepts_from_mapping(+Mapping, -Concepts)

glean_info_from_mapping/7
xxx
*/

glean_info_from_mapping([],MatchMapIn,MatchMapIn,
                        TermLengthsIn,TermLengthsIn,
                        NMetaWordsIn,NMetaWordsIn,
                        InvolvesHeadIn,InvolvesHeadIn,[]).
glean_info_from_mapping([First|Rest],MatchMapIn,MatchMapOut,
                        TermLengthsIn,TermLengthsOut,
                        NMetaWordsIn,NMetaWordsOut,
                        InvolvesHeadIn,InvolvesHeadOut,ExtraMetaWords) :-
    First = ev(_NegValue,_CUI,_MetaTerm,_MetaConcept,MetaWords,_SemTypes,
	       MatchMap0,InvolvesHead,_IsOvermatch,_SourceInfo,_PosInfo),
    modify_matchmap_for_concatenation(MatchMap0,NMetaWordsIn,MatchMap),
    append(MatchMapIn,MatchMap,MatchMapInOut),
    length(MetaWords,NMetaWords),
    append(TermLengthsIn,[NMetaWords],TermLengthsInOut),
    NMetaWordsInOut is NMetaWordsIn+NMetaWords,
    (InvolvesHead==yes ->
        InvolvesHeadInOut=yes
    ;   InvolvesHeadInOut=InvolvesHeadIn
    ),
    compute_extra_meta(MatchMap0,MetaWords,FirstExtra),
    append(FirstExtra,RestExtra,ExtraMetaWords),
    glean_info_from_mapping(Rest,MatchMapInOut,MatchMapOut,
                            TermLengthsInOut,TermLengthsOut,
                            NMetaWordsInOut,NMetaWordsOut,
                            InvolvesHeadInOut,InvolvesHeadOut,RestExtra).

glean_concepts_from_mapping([],[]) :-
    !.
glean_concepts_from_mapping([First|Rest],[MetaConcept|RestConcepts]) :-
    First = ev(_NegValue,_CUI,_MetaTerm,MetaConcept,_MetaWords,_SemTypes,
	       _MatchMap0,_InvolvesHead,_IsOvermatch,_SourceInfo,_PosInfo),
    glean_concepts_from_mapping(Rest,RestConcepts).


/* modify_matchmap_for_concatenation(+MatchMapIn, +NMetaWords, -MatchMapOut)

modify_matchMap_for_concatenation/3
xxx
*/

modify_matchmap_for_concatenation(MatchMapIn,0,MatchMapIn) :-
    !.
modify_matchmap_for_concatenation([],_NMetaWords,[]).
modify_matchmap_for_concatenation([[PhraseComponent,MetaComponent,VarLevel]|
                                   Rest],
                                  NMetaWords,
                                  [[PhraseComponent,ModifiedMetaComponent,
                                    VarLevel]|ModifiedRest]) :-
    MetaComponent=[Begin,End],
    NewBegin is Begin+NMetaWords,
    NewEnd is End+NMetaWords,
    ModifiedMetaComponent=[NewBegin,NewEnd],
    modify_matchmap_for_concatenation(Rest,NMetaWords,ModifiedRest).


/* add_confidence_value(+LPhraseIn, +Value, -LPhraseOut)

add_confidence_value/3
xxx
*/

add_confidence_value(LPhraseIn,Value,LPhraseOut) :-
    append(LPhraseIn,[confid(Value)],LPhraseOut).


/* filter_evaluations_by_threshold(+Evaluations, -FilteredEvaluations)
   filter_evaluations_by_threshold(+Evaluations, +Threshold,
                                   -FilteredEvaluations)

filter_evaluations_by_threshold/2 retains only those Evaluations with
value Threshold or better.  */

filter_evaluations_by_threshold(Evaluations,FilteredEvaluations) :-
    control_value(threshold,Threshold),
    NegThreshold is -Threshold,
    filter_evaluations_by_threshold(Evaluations,NegThreshold,
                                    FilteredEvaluations).

filter_evaluations_by_threshold([],_,[]).
filter_evaluations_by_threshold([First|_Rest],NegThreshold,[]) :-
    First = ev(NegValue,_,_,_,_,_,_,_,_,_SourceInfo,_PosInfo),
    NegValue > NegThreshold,
    !.
filter_evaluations_by_threshold([First|Rest],NegThreshold,
                                [First|FilteredRest]) :-
    filter_evaluations_by_threshold(Rest,NegThreshold,FilteredRest).


/* filter_out_redundant_evaluations(+Evaluations, -FilteredEvaluations)
   filter_out_redundant_evaluations_aux(+Evaluations, -FilteredEvaluations)

filter_out_redundant_evaluations/2
filter_out_redundant_evaluations_aux/2

Evaluations are redundant if they involve the same concept and have the same
phrase involvement.
*/

filter_out_redundant_evaluations([], []).
filter_out_redundant_evaluations([First|Rest], FilteredEvaluations) :-
	rev([First|Rest], RevEvaluations),
	filter_out_redundant_evaluations_aux(RevEvaluations, RevFilteredEvaluations),
	rev(RevFilteredEvaluations, FilteredEvaluations).

filter_out_redundant_evaluations_aux([], []).
filter_out_redundant_evaluations_aux([First|Rest],Result) :-
	( evaluation_is_redundant(Rest, First) ->
	  Result = FilteredRest
	; Result = [First|FilteredRest]
	),
	filter_out_redundant_evaluations_aux(Rest, FilteredRest).


/* evaluation_is_redundant(+Evaluations, +Evaluation)

WATCH ORDER OF ARGS
evaluation_is_redundant/2 determines if Evaluation involves the same
concept and the same phrase involvement as one of Evaluations. 
In addition, if --allow_duplicate_concept_names is in effect, then the
CUIs must also match in order for an evaluation to be redundant. */

evaluation_is_redundant([ev(_,_,_,MetaConcept,_,_,MatchMap2,_,_,_SourceInfo2,_PosInfo2)|_Rest],
                         ev(_,_,_,MetaConcept,_,_,MatchMap1,_,_,_SourceInfo1,_PosInfo1)) :-
	\+ control_option(allow_duplicate_concept_names),
	matchmaps_are_equivalent(MatchMap1, MatchMap2),
	!.
evaluation_is_redundant([ev(_,CUI,_,MetaConcept,_,_,MatchMap2,_,_,_SourceInfo2,_PosInfo2)|_Rest],
                         ev(_,CUI,_,MetaConcept,_,_,MatchMap1,_,_,_SourceInfo2,_PosInfo2)) :-
	control_option(allow_duplicate_concept_names),
	matchmaps_are_equivalent(MatchMap1,MatchMap2),
	!.
evaluation_is_redundant([_First|Rest], Evaluation) :-
	evaluation_is_redundant(Rest, Evaluation).


/* filter_out_subsumed_evaluations(+Evaluations, -FilteredEvaluations)
   filter_out_subsumed_evaluations_aux(+Evaluations, -FilteredEvaluations)

filter_out_subsumed_evaluations/2
filter_out_subsumed_evaluations_aux/2

An evaluation E1 is subsumed by another E2 if E1's score is strictly worse than E2's
and E1 and E2 have the same phrase involvement.

*/

filter_out_subsumed_evaluations([], []).
filter_out_subsumed_evaluations([H|T], FilteredEvaluations) :-
        rev([H|T], RevEvaluations),
        filter_out_subsumed_evaluations_aux(RevEvaluations, RevFilteredEvaluations),
        rev(RevFilteredEvaluations, FilteredEvaluations).

filter_out_subsumed_evaluations_aux([], []).
filter_out_subsumed_evaluations_aux([First|Rest], Result) :-
        ( evaluation_is_subsumed(Rest, First) ->
          Result = FilteredRest
        ; Result = [First|FilteredRest]
        ),
        filter_out_subsumed_evaluations_aux(Rest, FilteredRest).


/* evaluation_is_subsumed(+Evaluations, +Evaluation)
   evaluation_is_subsumed_aux(+Evaluations, +NegValue, +MatchMap)
   evaluation_is_subsumed_aux(+Evaluations, +NegValue, +MatchMap, +SemTypes)

WATCH ORDER OF ARGS
evaluation_is_subsumed/2
evaluation_is_subsumed_aux/3,4

see filter_out_subsumed_evaluations/2 above.  */

evaluation_is_subsumed([H|T],
                       ev(NegValue,_,_,Concept,_,_SemTypes,MatchMap,_,_,_SourceInfo,_PosInfo)) :-
	evaluation_is_subsumed_4([H|T], NegValue, Concept, MatchMap).

evaluation_is_subsumed_4([First|Rest], NegValue1, Concept1, MatchMap1) :-
        First = ev(NegValue2,_,_,_,_,_,MatchMap2,_,_,_SourceInfo,_PosInfo),
        ( NegValue2 < NegValue1,
          matchmaps_are_equivalent(MatchMap1, MatchMap2) ->
          true
        ; evaluation_is_subsumed_4(Rest, NegValue1, Concept1, MatchMap1)
        ).

/* matchmaps_are_equivalent(+MatchMap1, +MatchMap2)

matchmaps_are_equivalent/2 determines if the phrase components of MatchMap1
and MatchMap2 are the same.  */

matchmaps_are_equivalent(MatchMap1, MatchMap2) :-
	extract_components(MatchMap1, PhraseCs1, _),
	linearize_components(PhraseCs1, LPhraseCs1),
	append(LPhraseCs1, CompactCs1),
	extract_components(MatchMap2, PhraseCs2, _),
	linearize_components(PhraseCs2, LPhraseCs2),
	append(LPhraseCs2, CompactCs1).

/* add_semtypes_to_evaluations(?Evaluations)

add_semtypes_to_evaluations/1 instantiates the SemTypes argument of ev/8 terms
in Evaluations.  */

add_semtypes_to_evaluations([]).
add_semtypes_to_evaluations([ev(_,_,_,MetaConcept,_,SemTypes,_,_,_,_SourceInfo,_PosInfo)|Rest]) :-
	db_get_concept_sts(MetaConcept, SemTypes),
	!,
	add_semtypes_to_evaluations(Rest).

get_inputmatch_atoms_from_phrase(PhraseElements, InputMatchAtoms) :-
	get_inputmatch_lists_from_phrase(PhraseElements, InputMatchLists),
	append(InputMatchLists, InputMatchAtoms).

get_inputmatch_lists_from_phrase([], []).
get_inputmatch_lists_from_phrase([FirstPhraseComponent|RestPhraseComponents],
				 [FirstInputMatchList|RestInputMatchLists]) :-
	arg(1, FirstPhraseComponent, FeatureList),
	memberchk(inputmatch(FirstInputMatchList), FeatureList),
	get_inputmatch_lists_from_phrase(RestPhraseComponents, RestInputMatchLists).
		
get_composite_phrases([PhraseIn|RestPhrasesIn],
		      CompositePhrase, RestCompositePhrasesIn, CompositeOptions) :-
	( control_option(composite_phrases) ->
	  true
	; control_option(quick_composite_phrases)
	),
	begins_with_composite_phrase([PhraseIn|RestPhrasesIn],
				     CompositePhrase0, RestCompositePhrasesIn),
	!,
	collapse_syntactic_analysis(CompositePhrase0, CompositePhrase),
	% append(CompositePhrase1, CompositePhrase),
	( control_option(composite_phrases) ->
	  CompositeOptions=[term_processing,     % -z
			    allow_overmatches,   % -o
			    allow_concept_gaps,  % -g
			    ignore_word_order,   % -i
			    truncate_candidates_mappings]  % -X
	; CompositeOptions=[term_processing,               % -z
			    ignore_word_order]             % -i
	),
	% add composite options
	add_to_control_options(CompositeOptions).
get_composite_phrases([PhraseIn|RestPhrases], PhraseIn, RestPhrases, []).

/* begins_with_composite_phrase(+Phrases, -CompositePhrase, -Rest)

begins_with_composite_phrase/3 determines if Phrases begins with a
CompositePhrase returning it and the Rest of the phrases, where a composite
phrases is
  a phrase (non-prepositional and not ending with punctuation)
  followed by a prepositional phrase
  followed by zero or more 'of' prepositional phrases. */

begins_with_composite_phrase([First,Second|Rest],[First,Second|RestComposite],
                             NewRest) :-
    \+is_prep_phrase(First),
    \+ends_with_punc(First),
    is_prep_phrase(Second),
    !,
    initial_of_phrases(Rest,RestComposite,NewRest).

%%%%% begins_with_composite_phrase([First,Second|Rest],
%%%%% 			     [First,Second|RestComposite],
%%%%%                              NewRest) :-
%%%%% 	is_of_phrase(Second),
%%%%% 	!,
%%%%% 	initial_of_phrases(Rest, RestComposite, NewRest).

is_prep_phrase([PhraseItem|_]) :-
	get_phrase_item_name(PhraseItem, prep),
	!.

ends_with_punc(PhraseItems) :-
	% reversed order of args from QP library version!
	last(PhraseItems, LastPhraseItem),
	get_phrase_item_name(LastPhraseItem, punc),
	!.

initial_of_phrases([], [], []).
initial_of_phrases([First|Rest], [First|RestOf], NewRest) :-
	is_of_phrase(First),
	!,
	initial_of_phrases(Rest, RestOf, NewRest).
initial_of_phrases(Phrases, [], Phrases).

is_of_phrase([PhraseItem|_]) :-
	get_phrase_item_name(PhraseItem, prep),
	get_phrase_item_feature(PhraseItem, lexmatch, [of]).

debug_compute_evaluations_1(DebugFlags, GVCs0) :-
	( memberchk(1, DebugFlags) ->
	  format('~n~nGs:~n', []),
	  wgvcs(GVCs0)
	; true
	).

debug_compute_evaluations_2(DebugFlags, GVCs3, Variants) :-
	( memberchk(1, DebugFlags) ->
	  format('~n~nGVs:~n', []),
	  wgvcs(GVCs3),
          format('~n~nVariants:~n', []),
	  avl_to_list(Variants, VariantsList),
	  write_avl_list(VariantsList)
	; true
	).

debug_compute_evaluations_3(DebugFlags, GVCs) :-
	( memberchk(2, DebugFlags) ->
	  format('~n~nGVCs:~n', []),
	  wgvcs(GVCs)
	; true
	).

debug_compute_evaluations_4(DebugFlags, Evaluations2) :-
	( memberchk(4, DebugFlags) ->
	  length(Evaluations2, NEvals2),
	  format('~nPre-filtered evaluations (~d):~n', [NEvals2]),
	  wl(Evaluations2)
	; true
	).

debug_compute_evaluations_5(DebugFlags, Evaluations) :-
	( memberchk(4, DebugFlags) ->
	  length(Evaluations, NEvals),
	  format('~nNon-redundant evaluations (~d):~n', [NEvals]),
	  wl(Evaluations)
	; true
	).


maybe_filter_out_dvars(GVCs1, GVCs2) :-
	( control_option(no_derivational_variants) ->
	  filter_out_dvars(GVCs1, GVCs2)
	; GVCs2 = GVCs1
	).

maybe_filter_out_aas(GVCs2, GVCs3) :-
	( \+ control_option(all_acros_abbrs),
	  \+ control_option(unique_acros_abbrs_only) ->
	  filter_out_aas(GVCs2, GVCs3)
	; GVCs3 = GVCs2
	).

maybe_filter_evaluations_by_threshold(Evaluations1, Evaluations2) :-
	( control_option(threshold) ->
	  filter_evaluations_by_threshold(Evaluations1,Evaluations2)
	; Evaluations2=Evaluations1
	).

get_debug_control_value(DebugFlags) :-
	( control_value(debug, DebugFlags) ->
	  true
	; DebugFlags = []
	).

check_generate_best_mappings_control_options :-
	( \+ control_option(hide_mappings)   -> true
	; control_option(mmi_output)         -> true
	; control_option(fielded_mmi_output) -> true
	; control_option(semrep_output)      -> true
	; control_option(machine_output)     -> true
	; control_option('XML')
	).

check_construct_best_mappings_control_options :-
	( \+ control_option(hide_mappings)    -> true
	; control_option(mmi_output)          -> true
	; control_option(fielded_mmi_output)  -> true
	; control_option(semrep_output)       -> true
	; control_option(as_module)           -> true
	; control_option(machine_output)     -> true
	; control_option('XML')
	).

check_generate_initial_evaluations_1_control_options_1 :-
	\+ control_option(allow_overmatches),
	\+ control_option(allow_concept_gaps),
	\+ control_option(ignore_stop_phrases),
	% -D and -a must be in force because
	% that's how stop phrases were computed
	\+ control_option(all_derivational_variants),
	\+ control_option(all_acros_abbrs).

check_generate_initial_evaluations_1_control_options_2 :-
	( \+ control_option(hide_candidates) -> true
	; \+ control_option(hide_mappings)   -> true
	; control_option(mmi_output)         -> true
	; control_option(fielded_mmi_output) -> true
	; control_option(semrep_output)      -> true
	; control_option(machine_output)     -> true
	; control_option('XML')              -> true
	; control_option(as_module)
	).
