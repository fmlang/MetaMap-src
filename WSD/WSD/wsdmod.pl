
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

:- module(wsdmod, [
	do_WSD/9,
	% called by MetaMap API -- do not change signature!
	extract_SemRep_phrases_1/3
    ]).

:- use_module(metamap(metamap_tokenization), [
        get_utterance_token_list/4
   ]).

:- use_module(skr(skr_utilities), [
	fatal_error/2,
        get_candidate_feature/3
    ]).

:- use_module(skr_lib(nls_lists), [
         get_from_list/3
    ]).

:- use_module(skr_lib(nls_strings), [
	trim_whitespace_right/2
   ]).

:- use_module(skr_lib(nls_system), [
	control_option/1,
	control_value/2
   ]).

:- use_module(skr(skr), [
	extract_phrases_from_aps/2,
	get_inputmatch_atoms_from_phrase/2,
	get_phrase_tokens/4
   ]).

:- use_module(skr(skr_utilities), [
	debug_message/3,
	ensure_number/2,
	generate_aa_term/2
   ]).

:- use_module(skr_lib(nls_strings), [
	atom_codes_list/2,
	split_string_completely/3,
	trim_and_compress_whitespace/2
   ]).

:- use_module(skr_lib(sicstus_utils), [
	ttyflush/0
   ]).

:- use_module(skr_lib(xml), [
	xml_parse/3
   ]).

:- use_module(wsd(mmoxml), [
	mmo_terms_to_xml_chars/3
   ]).

:- use_module(library(between),[
	between/3
   ]).

:- use_module(library(codesio),[
	read_from_codes/2
   ]).

:- use_module(library(lists),[
	append/2,
	max_member/2,
	rev/2
   ]).

:- use_module(library(lists),[
	last/3
    ]).

:- use_module(library(random),[
	random_member/2
    ]).

:- use_module(library(sets),[
	subtract/3
    ]).

:- use_module(library(system), [
	environ/2
   ]).

% :- prolog_flag(character_escapes,_,on).

do_WSD(UtteranceText, InputLabel, CitationTextAtom, AAs, Tokens,
       WSDServerStream, MMOPhrases, DisambMMOPhrases, SemRepPhrasesWithWSD) :-
	( control_option(word_sense_disambiguation) ->
	  generate_aa_term([mm_output(utterance, citation, modifiedtext, tagging,
				      AAs, syntax, MMOPhrases, extractedphrases)],
			   AATerm),
	  MMOutput = mm_output(utterance(InputLabel, UtteranceText, PosInfo, ReplPos),
			       citation(CitationTextAtom), modifiedtext, tagging,
			       AATerm, syntax,
			       MMOPhrases, extractedphrases),
	 % format(user_output, 'BEFORE disambiguate_mmoutput~n', []), ttyflush,
	 disambiguate_mmoutput([MMOutput], WSDServerStream, [DisambMMOutput]),
	 % format(user_output, 'AFTER disambiguate_mmoutput~n', []), ttyflush,
	 DisambMMOutput = mm_output(utterance(InputLabel, UtteranceText, PosInfo, ReplPos),
				    citation(CitationTextAtom), modifiedtext, tagging,
				    AATerm, syntax,
				    DisambMMOPhrases, _ExtractedPhrases),
	 get_utterance_token_list(Tokens, TokensThisUtterance, _CharOffset, _TokensOut),
	 extract_SemRep_phrases(DisambMMOutput, TokensThisUtterance, SemRepPhrasesWithWSD)
       ; DisambMMOPhrases = MMOPhrases
       ).

extract_SemRep_phrases(mm_output(_Utt,_CitText,_MT,_TG,_AA,_Sx,MMOPhrases,_Ex),
		       Tokens, SemRepPhrases) :-
	extract_SemRep_phrases_aux(MMOPhrases, Tokens, SemRepPhrases).

extract_SemRep_phrases_aux([], _Tokens, []).
extract_SemRep_phrases_aux([phrase(Phrase,_,_,_,_,_,aphrases(APhrases))|RestPhraseTerms],
			   Tokens, [SemRepPhrases|RestSemRepPhrases]) :-
	Phrase = phrase(_,PhraseList,_,_),
	get_inputmatch_atoms_from_phrase(PhraseList, InputMatchPhraseWords),
	get_phrase_tokens(InputMatchPhraseWords, Tokens, PhraseTokens, TokensRest),
	extract_SemRep_phrases_1(APhrases, PhraseTokens, SemRepPhrases),
	!,
	extract_SemRep_phrases_aux(RestPhraseTerms, TokensRest, RestSemRepPhrases).
extract_SemRep_phrases_aux([phrase(_,_,_,_,_,_,aphrases(APhrases))|RestPhraseTerms],
	                   Tokens, [SemRepPhrases|RestSemRepPhrases]) :-
	extract_phrases_from_aps(APhrases, SemRepPhrases),
	extract_SemRep_phrases_aux(RestPhraseTerms, Tokens, RestSemRepPhrases).

extract_SemRep_phrases_1([], _Tokens, []).
extract_SemRep_phrases_1([ap(_NegValue,Phrase,_PhraseMap,Mapping)|Rest],
	                 Tokens, [PhraseWithPos|ExtractedRest]) :-
	add_pos_info_to_phrases(Phrase, Mapping, Tokens, PhraseWithPos),
	!,
	extract_SemRep_phrases_1(Rest, Tokens, ExtractedRest).
extract_SemRep_phrases_1([ap(_NegValue,_Phrase,_PhraseMap,[])|Rest],
	                 Tokens, ExtractedRest) :-
	extract_SemRep_phrases_1(Rest, Tokens, ExtractedRest).

add_pos_info_to_phrases([], _Eval, _Tokens, []).
add_pos_info_to_phrases([Item|RestItems], Evaluations, Tokens, [ItemPos|RestItemsPos]) :-
	functor(Item, Type, 1),
	arg(1, Item, Elements),
	get_from_list(metaconc,Elements, [_Conc:CUI:_SemType]),
	get_metaconc_position(CUI, Evaluations, Tokens, StartPos, EndPos),
	!,
	functor(ItemPos, Type, 1),
	append(Elements, [position(StartPos,EndPos)], ElementsWithPos),
	arg(1,ItemPos, ElementsWithPos),
	add_pos_info_to_phrases(RestItems, Evaluations, Tokens, RestItemsPos).
add_pos_info_to_phrases([Item|RestItems], Evals, Tokens, [ItemPos|RestItemsPos]):-
	get_item_position(Item, Tokens, TokensRest, ItemPos),
	!,
	add_pos_info_to_phrases(RestItems, Evals, TokensRest, RestItemsPos).
add_pos_info_to_phrases([Item|RestItems], Evals, Tokens, [Item|RestItemsPos]) :-
	add_pos_info_to_phrases(RestItems, Evals, Tokens, RestItemsPos).


get_metaconc_position(CUI,[Eval|_RestEval], Tokens, StartPos, EndPos) :-
	functor(Eval, ev, 11),
	arg(2, Eval, CUI),
	arg(11, Eval, PosInfoList),
	match_pos_token(PosInfoList, Tokens, MatchingTokens),
	% \+ MatchingTokens == [],
	compute_positions(MatchingTokens, StartPos, EndPos),
	!.
get_metaconc_position(CUI,[_Eval|RestEval], Tokens, StartPos, EndPos) :-
	get_metaconc_position(CUI,RestEval, Tokens, StartPos, EndPos).

match_pos_token([], _Tokens, []) :- !.
match_pos_token(_PosInfoList, [], []) :- !.
match_pos_token([Start/Len|RestPosInfoList], [Token|TokenRest], [Token|TokenRestMatch]) :-
	Token = tok(_,_,_,_,pos(StartTok,LenTok)),
	StartTok >= Start,
	EndTok is StartTok + LenTok,
	EndPos is Start + Len,
	EndPos =:=  EndTok,
	!,
	match_pos_token(RestPosInfoList,TokenRest,TokenRestMatch).
match_pos_token([Start/Len|RestPosInfoList], [Token|TokenRest], [Token|TokenRestMatch]) :-
	Token = tok(_,_,_,_,pos(StartTok,LenTok)),
	StartTok >= Start,
	EndTok is StartTok + LenTok,
	EndPos is Start + Len,
	EndPos >=  EndTok,
	!,
	match_pos_token([Start/Len|RestPosInfoList], TokenRest, TokenRestMatch).
match_pos_token(PosInfoList,[_Token|RestTokens], MatchTokens) :-
	!,
	match_pos_token(PosInfoList, RestTokens, MatchTokens).
match_pos_token([_PosInfo|RestPosInfoList], [_Token|RestTokens], MatchTokens) :-
	match_pos_token(RestPosInfoList, RestTokens, MatchTokens).

get_item_position(Item, TokensIn, TokensOut, ItemPos) :-
	functor(Item, Type, 1),
	arg(1, Item, ArgList),
	get_from_list(inputmatch, ArgList, InputMatch),
	match_token(TokensIn, InputMatch, MatchingTokens, TokensOut),
	compute_positions(MatchingTokens, Start, End),
	functor(ItemPos, Type, 1),
	append(ArgList, [position(Start,End)], ItemArgList),
	arg(1, ItemPos, ItemArgList),
	!.

match_token(Tokens, [], [], Tokens) :- !.
match_token([Token|RestTokens], [WordAtom|RestInput], [Token|RestMatch], TokensOut) :-
	arg(2, Token, Word),
	atom_codes(WordAtom, Word),
	!,
	match_token(RestTokens, RestInput, RestMatch, TokensOut).
match_token(Tokens, [WordAtom|RestInput], [MatchingToken|RestMatch], TokensOut) :-
	find_word_with_gap(WordAtom,Tokens, TokensBefore, MatchingToken, TokensAfter),
	!,
	append(TokensBefore, TokensAfter, TokensOut0),
	match_token(TokensOut0, RestInput, RestMatch, TokensOut).
match_token([_Token|RestTokens],InputMatch,MatchTokens,TokensOut) :-
	match_token(RestTokens, InputMatch, MatchTokens, TokensOut).

compute_positions(Tokens, StartPos, EndPos) :-
	Tokens = [FirstToken|_],
	arg(4, FirstToken, FirstPos),
	FirstPos = pos(StartPos0,_),
	rev(Tokens, RevTokens),
	RevTokens = [LastToken|_],
	arg(4, LastToken, LastPos),
	LastPos = pos(_,EndPos),
	StartPos is StartPos0 + 1,
	!.

find_word_with_gap(_WordAtom, [], [], [], []) :- !.
find_word_with_gap(WordAtom, [Token|Rest], _TokensBefore, Token, Rest) :-
	arg(2, Token, Word),
	atom_codes(WordAtom, Word),
	!.
find_word_with_gap(WordAtom, [Token|Rest], [Token|BeforeRest], Match, After) :-
	find_word_with_gap(WordAtom, Rest, BeforeRest, Match, After).
	
	
/* disambiguate_mmoutput(MMOutput, +WSDServerStream, -DisambMMOutput)
   extract_mmo_from_utterance(+MMOutput, +UttNum, -MMO, -MarkedMappings, -MarkedMMOutput)
   extract_mmo_phrases(+MMOutput, +UttNum, +PhraseNum, -MMO, -MarkedMappings,
                       -MarkedMMOutput)
   disambiguate_mmo(+MMOIn, -MMOOut)
   reinsert_mmo(+DisambMMO, +MarkedMappings, +MarkedMMOutput, -ModifiedMMOutput)

disambiguate_mmoutput/3 performs word sense disambiguation (WSD) on the
MetaMap output embedded in MMOutput forming DisambMMOutput.
extract_mmo/5 extracts the actual MetaMap output (MMO) from MMOutput
creating MarkedMappings and MarkedMMOutput (with marked mappings (phrases,
respectively) with their zero-based utterance and phrase number) in the
process. It uses extract_mmo_phrases/6.
disambiguate_mmo/2 calls disambiguation on MMOIn and produces MMOOut.
And reinsert_mmo/4 strips MarkedMappings of unwanted senses according to
DisambMMO and inserts the stripped mappings into MarkedMMOutput producing
DisambMMOutput. */

/*

MMOutput and DisambMMOutput are lists of MMO_Terms [MMOTerm1, ..., MMOTermN]

where each MMOTerm corresponds to an utterance in the citation and is of the form

mm_output(ExpSentence, Citation, ModifiedText, Tagging, AA, Syntax, MMOPhrases, ExtractedPhrases)

*/

disambiguate_mmoutput(MMOutput, _WSDServerStream, DisambMMOutput) :-
	unambiguous_output(MMOutput, _UtteranceText),
	!,
	% format('~n~n### Unambiguous utterance: ~w~n~n', [UtteranceText]),
	DisambMMOutput = MMOutput.
disambiguate_mmoutput(MMOutput, WSDServerStream, DisambMMOutput) :-
	InitialUtteranceNumber is 1,
	extract_mmo_from_utterance(MMOutput, Citation,
				   InitialUtteranceNumber,
				   MMO, [],
				   MarkedMappings, [],
				   MarkedMMOutput),
	MMOutput = [mm_output(_Utt,_Cit,_ModTxt,_Tag,AAs,_Stx,_MMOPhrases,_ExtrPhrases)|_],
	% MarkedMappings is now built up using difference lists; no need to append!
	disambiguate_mmo([Citation,AAs|MMO], WSDServerStream, DisambMMO),
	reinsert_mmo(DisambMMO, MarkedMappings, MarkedMMOutput, DisambMMOutput),
	!.
disambiguate_mmoutput(MMOutput, _WSDServerStream, MMOutput) :-
	format('disambiguate_mmoutput/2 failed for:~n~p~nIgnoring any disambiguations.~n',
	       [MMOutput]).

unambiguous_output([MMOutput], UtteranceText) :-
	MMOutput = mm_output(utterance(_Label, UtteranceTextString, _PosInfo, _ReplPos),
			     _Cit,_ModTxt,_Tag,_AAs,_Stx,MMOPhrases,_ExtrPhrases),
	atom_codes(UtteranceText, UtteranceTextString),
	all_unambiguous_phrases(MMOPhrases).

all_unambiguous_phrases([]).
all_unambiguous_phrases([FirstPhrase|RestPhrases]) :-
	unambiguous_phrase(FirstPhrase),
	all_unambiguous_phrases(RestPhrases).

unambiguous_phrase(PhraseTerm) :-
	PhraseTerm = phrase(_Phrase,_Candidates,Mappings,_PWI,_GVCs,_EV0,_APhrases),
	Mappings = mappings(MapTermList),
	length(MapTermList, Length),
	Length =< 1.

/*
extract_mmo/8 takes MMOutput, a list of MMO_Terms [MMOTerm1, MMOTerm2, ..., MMOTermN]

where each MMOTerm corresponds to an utterance in the citation and is of the form

mm_output(ExpSentence, Citation, ModifiedText, Tagging, AAs, Syntax, MMOPhrases, ExtractedPhrases)

The only component of the mm_output structure we care about is MMOPhrases,

which is a list [PhraseTerm1, PhraseTerm2, ..., PhraseTermN]

where each PhraseTerm is of the form

phrase(phrase(_), candidates(_), mappings(_), pwi(_), gvcs(_), ev0(_), aphrases(_))

extract_mmo/8 produces three data structures:
(1) MMO, which gets sent to the WSD client.
MMO is a list of the following form:

[utterance(_), phrase(_), candidates(_), mappings(_), 
	       phrase(_), candidates(_), mappings(_), 
	       ...
 'EOU',
 utterance(_), phrase(_), candidates(_), mappings(_), 
	       phrase(_), candidates(_), mappings(_), 
	       ...
 'EOU',
 ...
 ]

(2) MarkedMappings, a list of terms of the form
    markedmappings(UttNum,PhraseNum,Mappings,APhrases), and

(3) MarkedMMOutput, a list like MMOutput of terms of the form
    mm_output(Utterance,Citation,ModifiedText,Tagging,AAs,Syntax,MarkedMMOPhrases,ExtractedPhrases)
    MarkedMMOPhrases is like MMOPhrases, except that instead of
    phrase(phrase(_), candidates(_), mappings(_), pwi(_), gvcs(_), ev0(_), aphrases(_))
    the terms are of the form
    markedphrase(UttNum,PhraseNum,Phrase,Candidates,Mappings,PWI,GVCs,EV0,APhrases)

*/

% extract_mmo_from_utterance is called once per utterance to create 
% (1) the MMO to be sent to the WSD client,
% (2) the MarkedMappings, and
% (3) the MarkedMMOutput

extract_mmo_from_utterance([], _Citation, _UtteranceNum,
			   MMO, MMO, MarkedMappings, MarkedMappings, []).
extract_mmo_from_utterance([MMOutput|RestMMOutput], Citation, UtteranceNum,
			   MMOIn, MMOOut,
			   MarkedMappingsIn, MarkedMappingsOut,
			   [MarkedMMOutput|RestMarkedMMOutput]) :-
	MMOutput =
           mm_output(_Utterance,Citation,_ModifiedText,_Tagging,
		     _AAs,_Syntax,_MMOPhrases,_ExtractedPhrases),
	extract_mmo_aux(MMOutput, UtteranceNum,
			MMOIn, MMONext,
			MarkedMappingsIn, MarkedMappingsNext,
			MarkedMMOutput),
	NextUtteranceNum is UtteranceNum + 1,
	extract_mmo_from_utterance(RestMMOutput, Citation, NextUtteranceNum,
		    MMONext, MMOOut,
		    MarkedMappingsNext, MarkedMappingsOut,
		    RestMarkedMMOutput).
	   
extract_mmo_aux(MMOutput, UttNum,
		[Utterance|MMOIn], MMOOut,
		MarkedMappingsIn, MarkedMappingsOut,
		MarkedMMOutput) :-
	% Identify the MMOPhrases component of the MMOutput term
	MMOutput =
           mm_output(Utterance,Citation,ModifiedText,Tagging,AAs,Syntax,MMOPhrases,ExtractedPhrases),
	InitialPhraseNumber is 1,
	extract_mmo_phrases(MMOPhrases, UttNum, InitialPhraseNumber, MMOIn, MMOOut,
			    MarkedMappingsIn, MarkedMappingsOut, MarkedMMOPhrases),
	% MarkedMMOPhrases is a list just like MMOPhrases, except that each term
	% phrase(Phrase,Candidates,Mappings,PWI,GVCs,EV0,APhrases)
	% is replaced by a term with the UtteranceNumber and the PhraseNumber
	% phrase(UttNum,PhraseNum,Phrase,Candidates,Mappings,PWI,GVCs,EV0,APhrases)
	% MarkedMMOutput is a copy of MMOutput, but with MarkedMMOPhrases instead of MMOPhrases
	MarkedMMOutput =
           mm_output(Utterance,Citation,ModifiedText,Tagging,
		     AAs,Syntax,MarkedMMOPhrases,ExtractedPhrases).
	% temp
	%format('~nextract_mmo--~nUtterance: ~p~nModifiedText: ...~nTagging: ...~nSyntax: ...~nMMOPhrases: ~p~nMarkedMMOPhrases: ~p~n',[Utterance,MMOPhrases,MarkedMMOPhrases]),
	% Add the 'EOU' token in the base case of extract_mmo_phrases
	% append(MMO0, ['EOU'], MMO1),
	% MMOIn = [Utterance|MMOTemp],
	% temp
	%format('~nMMO: ~p~n',[MMO]),

% MarkedMMOPhrases adds UttNum and PhraseNum to phrases
extract_mmo_phrases([], _UttNum, _PhraseNum, ['EOU'|MMO], MMO, MarkedMappings, MarkedMappings, []).
extract_mmo_phrases([Phrase|Rest],
		    UttNum, PhraseNum, MMOIn, MMOOut,
		    MarkedMappingsIn, MarkedMappingsOut,
		    [MarkedPhrase|MarkedPhraseRest]) :-
	Phrase = phrase(Phrase1,Candidates,Mappings,PWI,GVCs,EV0,APhrases),
	MMOIn = [Phrase1,Candidates,Mappings|MMONext],
	MarkedPhrase =
            markedphrase(UttNum,PhraseNum,Phrase1,Candidates,Mappings,PWI,GVCs,EV0,APhrases),
	MarkedMappingsIn = [markedmappings(UttNum,PhraseNum,Mappings,APhrases)|MarkedMappingsNext],
	NextPhraseNum is PhraseNum + 1,
	extract_mmo_phrases(Rest, UttNum, NextPhraseNum, MMONext, MMOOut,
			    MarkedMappingsNext, MarkedMappingsOut, MarkedPhraseRest).

disambiguate_mmo(MMOTermList, WSDServerStream, DisambMMO) :-
	( call_WSD(MMOTermList, WSDServerStream, RawDisambMMOString) ->
	  test_parse_disamb_mmo(RawDisambMMOString, DisambMMO)
	; format(user_output,
		 'Fatal error: call_WSD/2 could not process:~n~p~n',
		 [MMOTermList]),
	  ttyflush,
	  format('Fatal error: call_WSD/2 could not process:~n~p~n',
		 [MMOTermList]),
	  current_output(CurrentOutputStream),
	  flush_output(CurrentOutputStream),
	  abort
      ).

test_parse_disamb_mmo(RawDisambMMOString, DisambMMO) :-
	( parse_disamb_mmo(RawDisambMMOString, DisambMMO) ->
	  true
	; atom_codes(RawDisambMMOAtom, RawDisambMMOString),
	  format(user_output,
		 'Fatal error: disambiguate_mmo/2 could not parse:~n~p~n',
		 [RawDisambMMOAtom]),
	  ttyflush,
	  format('Fatal error: disambiguate_mmo/2 could not parse:~n~p~n',
	  	 [RawDisambMMOAtom]),
	  current_output(CurrentOutputStream),
	  flush_output(CurrentOutputStream),
	  abort
	  ).

/*
RawDisambMMO is an atom of the form
00000000.tx.1|0|0|[Acute - Triage Code$Admission Level of Care Code - Acute$acute]|[acute]|
00000000.tx.1|0|5|[Changed status$Changing]|[Changing]|
00000000.tx.2|1|1|[Clinical Research$Room of building - Study$Scientific Study]|[Scientific Study]|

parse_disamb_mmo/2 parses RawDisambMMO into a list of terms

[disamb('00000000.tx.1',0,0,
	['Acute - Triage Code','Admission Level of Care Code - Acute',acute],
	[acute]),
 disamb('00000000.tx.1',0,5,
	['Changed status','Changing'],
	['Changing']),
 disamb('00000000.tx.2',1,1,
	['Clinical Research','Room of building - Study','Scientific Study'],
	['Scientific Study'])
 ]

*/

parse_disamb_mmo(RawDisambMMO, DisambMMO) :-
	% first split into lines using "^J" as separator character
	split_string_completely(RawDisambMMO, [10], RawDisambMMOLines),
	% temp
	%    length(RawDisambMMOLines,NRDML),
	%    format('parse_disamb_mmo/2: ~d RawDisambMMOLines = ~p~n',[NRDML,RawDisambMMOLines]),
	parse_disamb_mmo_aux(RawDisambMMOLines, DisambMMO).

parse_disamb_mmo_aux([], []).
parse_disamb_mmo_aux([[]|Rest], DisambRest) :-
	!,
	parse_disamb_mmo_aux(Rest, DisambRest).
parse_disamb_mmo_aux([First|Rest], Result) :-
	% First, split each line into fields using "|" as separator character
	split_string_completely(First, "|", Fields),
	% temp
	%    length(Fields,NFields),
	%    format('parse_disamb_mmo_aux fields: (~d) ~p~n',[NFields,Fields]),
	( Fields==["None."] ->  % There is nothing to disambiguate
	  Result=DisambRest
        ; Fields=[Label0,I0,N0,AllSenses0,DisambSenses0,_],
          ( append("[Error JDI",_,DisambSenses0) ->
	    format('Fatal error: parse_disamb_mmo_aux/2 failed ~p on ~p~n',
		   [DisambSenses0,First]),
	    abort
	  ; true
	  ),
          ( ( DisambSenses0 == "[No match found.]"
            ; DisambSenses0 == "[None of the Above]"
            ; DisambSenses0 == "[JDI unable to disambiguate input]"
            ) ->
	    Result = DisambRest,
	    debug_message('WSD', '~n### WSD response: ~s~n', [DisambSenses0])
	  ; atom_codes(Label, Label0),
	    number_codes(I, I0),
	    number_codes(N, N0),
	    parse_senses(AllSenses0, AllSenses),
	    parse_senses(DisambSenses0, DisambSenses),
	    % temp
	    % format('  AllSenses: ~p~n',[AllSenses]),
	    % format('  DisambSenses: ~p~n',[DisambSenses]),
	    Result = [disamb(Label,I,N,AllSenses,DisambSenses)|DisambRest],
	    length(AllSenses, NumWSDInputs),
	    length(DisambSenses, NumWSDOutputs),
	    debug_message('WSD', '~n### WSD inputs (~d): ~w~n', [NumWSDInputs,AllSenses]),
	    debug_message('WSD', '### WSD output (~d): ~w~n~n',  [NumWSDOutputs,DisambSenses])

	  )
      ),
      !,
      parse_disamb_mmo_aux(Rest, DisambRest).
parse_disamb_mmo_aux([_First|Rest], DisambRest) :-
	% temp
	parse_disamb_mmo_aux(Rest, DisambRest).

% parse_senses("[]", []) :- !.
parse_senses(Senses0, Senses) :-
	Senses0 = [_|RestSensesChars],
	last(Senses1, _LastSensesChar, RestSensesChars),
	phrase(term_list(Senses), Senses1).

% ---------------------------------------------------------
% -------------- $-separated term list grammar --------------
% ---------------------------------------------------------

term_list(Ts) -->
          term(T), "$", !, term_list(Us), {Ts=[T|Us]}
	| term(T), {Ts=[T]}.

term(T) --> term_chars(Cs), !, {atom_codes(T,Cs)}.

term_chars(Cs) -->
	  [C], {is_term_char(C)}, !, term_chars(Ds), {Cs=[C|Ds]}
	| {Cs=[]}.

is_term_char(C) :- \+is_non_term_char(C).

is_non_term_char(0'$).
%is_non_term_char(0'[). % not really necessary
%is_non_term_char(0']).

% DisambMMO is a list of terms of the form
% disamb('00000000.tx.1',0,5,['Changed status','Changing'],['Changing'])

% MarkedMappings is a list of terms of the form
% markedmappings(UttNum,PhraseNum,Mappings,APhrases)
% We need the UttNum/PhraseNum to know from what Utt/Phrase mappings
% to remove senses ruled out by WSD

% MarkedMMOutput a list like MMOutput of terms of the form
% mm_output(Utterance,Citation,ModifiedText,Tagging,AAs,Syntax,MarkedMMOPhrases,ExtractedPhrases)

% in which MarkedMMOPhrases is a list of terms of the form
% phrase(UttNum,PhraseNum,Phrase,Candidates,Mappings,PWI,GVCs,EV0,APhrases)


reinsert_mmo(DisambMMO, MarkedMappings, MarkedMMOutput, DisambMMOutput) :-
	% temp
	% format('MarkedMappings: ~p~n~n',[MarkedMappings]),
	strip_mappings(DisambMMO, MarkedMappings, StrippedMarkedMappings),
	% temp
	% format('StrippedMarkedMappings: ~p~n~n',[StrippedMarkedMappings]),
	reinsert_mmoutput(MarkedMMOutput, StrippedMarkedMappings, DisambMMOutput),
	% temp
	%format('MarkedMMOutput:~p~n~nDisambMMOutput: ~p~n~n',
	%       [MarkedMMOutput,DisambMMOutput]),
	!.
% temp
reinsert_mmo(DisambMMO, _MarkedMappings, MarkedMMOutput, MarkedMMOutput) :-
	%    fatal_error('reinsert_mmo/4 failed for~nDisambMMO: ~p~nMarkedMappings: ~p~nMarkedMMOutput: ~p~nContinuing...~n',
	%	   [DisambMMO,MarkedMappings,MarkedMMOutput]).
	fatal_error('reinsert_mmo/4 failed for~nDisambMMO: ~p~n', [DisambMMO]).


% The cut in the base clause is necessary, because the third clause has a var first arg.
strip_mappings([], MarkedMappings, MarkedMappings) :- !.
% This clause fires when
% the disamb term's UttNum/PhraseNum match
% the markedmappings term's UttNum/PhraseNum
strip_mappings([disamb(_Label,UttNum,PhraseNum,AllSenses,SensesToKeep)|RestDisambs],
	       [markedmappings(UttNum,PhraseNum,Mappings,APhrases)|RestMarkedMappings],
	       StrippedMarkedMappings) :-
	!,
	subtract(AllSenses, SensesToKeep, SensesToStrip),
	% temp
	%format('  SensesToStrip: ~p~n',[SensesToStrip]),
	% temp; the number of disambiguated senses is no longer restricted to one
	%    length(Senses,NS),
	%    length(SensesToStrip,NSTS),
	%    NDiff is NS-NSTS,
	%    (NDiff=:=1 ->
	%	true
	%    ;   fatal_error('strip_mappings/3 found bad senses (~p) with sense ~p~n',
	%	       [Senses,Sense])
	%    ),
	strip_mappings_aux(SensesToStrip, SensesToKeep, Mappings, StrippedMappings),
	% Presumably, the aphrases that need to be stripped are those
	% in the same positions as the mappings that were stripped?
	strip_aphrases(SensesToStrip, SensesToKeep, APhrases, StrippedAPhrases),
	% temp
	%Mappings=mappings(M),
	%length(M,LM),
	%StrippedMappings=mappings(SM),
	%length(SM,LSM),
	%StrippedAPhrases=aphrases(SAP),
	%length(SAP,LSAP),
	%format('~nstrip_mappings/3: Sense = ~p~n',[Sense]),
	%format('Numbers: ~d ~d ~d~n',[LM,LSM,LSAP]),
	%format('StrippedMappings: ~p~n',[StrippedMappings]),
	%format('StrippedAPhrases: ~p~n',[StrippedAPhrases]),

	% If there is another disambiguation for this (utterance,phrase),
	% update Mappings and APhrases to StrippedMappings and StrippedAPhrases
	% in the current markedmappings term, and recurse with the next disamb term
	% and the updated current markedmappings plus the rest of the markedmappings;
	% otherwise, recurse with the next disamb term and the rest of the markedmappings.
	( RestDisambs = [disamb(_,UttNum,PhraseNum,_,_)|_] ->
	  strip_mappings(RestDisambs,
	  		 [markedmappings(UttNum,PhraseNum,StrippedMappings,StrippedAPhrases)
		           |RestMarkedMappings],
		         StrippedMarkedMappings)
    	; StrippedMarkedMappings = 
		[markedmappings(UttNum,PhraseNum,StrippedMappings,StrippedAPhrases)
	          | RestStrippedMarkedMappings],
	  strip_mappings(RestDisambs,RestMarkedMappings,RestStrippedMarkedMappings)
        ).
strip_mappings(DisambMMO, [FirstMarkedMapping|RestMarkedMappings],
	       [FirstMarkedMapping|RestStrippedMarkedMappings]) :-
	strip_mappings(DisambMMO, RestMarkedMappings, RestStrippedMarkedMappings).

/*
  Suppose WSD returns [Age$Elderly]|[Age],
  meaning that where an Age/Elderly ambiguity exists,
  strip out the Elderly sense, and keep the Age sense.

  Let's call
  ['Elderly'] SensesToStrip
  and
  ['Age'] SensesToKeep.

  Suppose further that Maps is a list of terms like this:

[ map(-888,[
    ev(-694, C0001792, Elderly, Elderly, [elderly], [aggp], [[[1,1],[1,1],0]], no,  no, Srcs, PI),
    ev(-861, C0001779, Age,     Age,     [age],     [orga], [[[2,2],[1,1],0]], yes, no, Srcs, PI)]),
  map(-888,[
    ev(-694, C0001792, Elderly, Elderly, [elderly], [aggp], [[[1,1],[1,1],0]], no,  no, Srcs, PI),
    ev(-861, C0001792, age,     Elderly, [age],     [aggp], [[[2,2],[1,1],0]], yes, no, Srcs, PI)])]) ]

What has historically been done here is that a map(NegValue, EVList) term was stripped
iff one of the ev terms in its EVList had one of the SensesToStrip as its fourth argument.
In the example above, BOTH map terms would be stripped, because both map terms' EVList
contains an ev term with 'Elderly' as a fourth argument.

This is too strict a test, because in this (real) case, ALL senses would be stripped.

We need instead to strip out a map term MT1 iff
(1) Its EVList contains an ev term whose fourth argument
    is one of the SensesToStrip, AND
(2) Another map term (MT2)'s EVList contains an ev term whose fourth argument
    is one of the SensesToKeep, AND
(3) Both the ev terms are in the same relative position in the MT1's and MT2's EVLists.

*/

strip_mappings_aux([], _SensesToKeep, Mappings, Mappings).
strip_mappings_aux([SenseToStrip|RestSensesToStrip], SensesToKeep,
		   mappings(Maps), mappings(StrippedMaps)) :-
	strip_maps(Maps, [], [SenseToStrip|RestSensesToStrip], SensesToKeep, StrippedMaps).

strip_maps([], _MapsToKeep, _SensesToStrip, _SensesToKeep, []).
% Strip this map
strip_maps([FirstMap|RestMaps], MapsToKeep, SensesToStrip, SensesToKeep, RestStrippedMaps) :-
	map_contains_sense_to_strip(FirstMap, RestMaps, MapsToKeep, SensesToStrip, SensesToKeep),
	!,
	strip_maps(RestMaps, MapsToKeep, SensesToStrip, SensesToKeep, RestStrippedMaps).
% Keep this map by putting it on the MapsToKeep List.
% We need to do that to allow it to match against a later map.
strip_maps([FirstMap|RestMaps], MapsToKeep,
	   SensesToStrip, SensesToKeep, [FirstMap|RestStrippedMaps]) :-
	strip_maps(RestMaps, [FirstMap|MapsToKeep],
		   SensesToStrip, SensesToKeep, RestStrippedMaps).

% ThisMap contains one of SensesToStrip; in addition,
% the Nth EVTerm in ThisMap's EV list had one of the SensesToStrip as its 4th arg
% AND
% The Nth EVTerm in the EV list of one of the other maps,
%  * either one we've already seen and decided to keep (MapsToKeep), or
%  * one we haven't seen yet (RestMaps)
% had one of the SensesToKeep as its 4th arg.

map_contains_sense_to_strip(ThisMap, RestMaps, MapsToKeep,
			    SensesToStrip, SensesToKeep) :-
	ThisMap = map(_ThisMapNegScore, ThisMapEVs),
	evs_contain_sense(ThisMapEVs, 1, MatchingPosition, SensesToStrip),
	% do NOT change these calls to member/2 to calls to memberchk/2!!
	( member(OtherMap, RestMaps)
        ; member(OtherMap, MapsToKeep)
        ),
	OtherMap = map(_OtherMapNegScore, OtherMapEVs),
	evs_contain_sense(OtherMapEVs, 1, MatchingPosition, SensesToKeep),
	!.

% :- nondet evs_contain_sense/4.

% At least one of Evs contains at least one of SensesToStrip
evs_contain_sense([Candidate|_RestEVs], Position, Position, SensesToStrip) :-
	get_candidate_feature(metaconcept, Candidate, Concept),
	% ev(_,_,_,Concept,_,_,_,_,_,_Srcs,_PosInfo)|_RestEVs
	memberchk(Concept, SensesToStrip).
	% Must NOT have a cut here because ev_contains_sense has to be backtrackable
evs_contain_sense([_First|Rest], CurrentPosition, MatchingPosition, SensesToStrip) :-
	NextPosition is CurrentPosition + 1,
	evs_contain_sense(Rest, NextPosition, MatchingPosition,SensesToStrip).

% strip_aphrases is identical to strip_mappings_aux,
% other than the form of the data structures used.
% The two predicates could be combined via paramaterization,
% but I have chosen not to in order to maintain declaratory clarity.

strip_aphrases([], _SensesToKeep, APhrases, APhrases).
strip_aphrases([Sense|RestSenses], SensesToKeep,
		aphrases(APs), aphrases(StrippedAPs)) :-
	strip_aps(APs, [], [Sense|RestSenses], SensesToKeep, StrippedAPs).

strip_aps([], _APsToKeep, _SensesToStrip, _SensesToKeep, []).
% Strip this APhrase
strip_aps([FirstAP|RestAPs], APsToKeep, SensesToStrip, SensesToKeep, RestStrippedAPs) :-
	ap_contains_sense_to_strip(FirstAP, RestAPs, APsToKeep, SensesToStrip, SensesToKeep),
	!,
	strip_aps(RestAPs, APsToKeep, SensesToStrip, SensesToKeep, RestStrippedAPs).
strip_aps([FirstAP|RestAPs], APsToKeep,
	  SensesToStrip, SensesToKeep, [FirstAP|RestStrippedAPs]) :-
	strip_aps(RestAPs, [FirstAP|APsToKeep],
		  SensesToStrip, SensesToKeep, RestStrippedAPs).

% The ap contains at least one of Senses, to be more accurate
ap_contains_sense_to_strip(ThisAP, RestAPs, APsToKeep,
			   SensesToStrip, SensesToKeep) :-
	ThisAP = ap(_ThisAPNegScore,_ThisAPLinearPhrase,_ThisAPLinearPhraseMap,ThisAPEVs),
	evs_contain_sense(ThisAPEVs, 1, MatchingPosition, SensesToStrip),
	% do NOT change these calls to member/2 to calls to memberchk/2!!
	( member(OtherAP, RestAPs)
	; member(OtherAP, APsToKeep)
	),
	OtherAP = ap(_OtherAPNegScore,_OtherAPLinearPhrase,_OtherAPLinearPhraseMap,OtherAPEVs),
	evs_contain_sense(OtherAPEVs, 1, MatchingPosition, SensesToKeep).

reinsert_mmoutput([], [], []) :- !.
reinsert_mmoutput([mm_output(Utterance,Citation,ModifiedText,Tagging,
			     AAs,Syntax,MMOPhrases,_ExtractedPhrases)
		   |RestMMOutput],
		  StrippedMarkedMappingsIn,
		  [mm_output(Utterance,Citation,ModifiedText,Tagging,
			     AAs,Syntax,ReinsertedMMOPhrases,ReinsertedExtractedPhrases)
		   |RestReinserted]) :-
	% temp
	% format('~nreinsert_mmoutput/3:~nExtractedPhrases: ~p~n',[ExtractedPhrases]),
	reinsert_mmoutput_aux(MMOPhrases,
			      StrippedMarkedMappingsIn, StrippedMarkedMappingsNext,
			      ReinsertedMMOPhrases, ReinsertedExtractedPhrases),
	!,
	% temp
	%format('reinsert_mmoutput/3:~nReinsertedExtractedPhrases: ~p~n',
	%[ReinsertedExtractedPhrases]),
	%(ExtractedPhrases==ReinsertedExtractedPhrases ->
	%    format('No change.~n',[])
	%;   format('They differ.~n',[])
	%),
	reinsert_mmoutput(RestMMOutput, StrippedMarkedMappingsNext, RestReinserted).
reinsert_mmoutput(MarkedMMOutput, StrippedMarkedMappings, MarkedMMOutput) :-
	fatal_error('reinsert_mmoutput/3 failed for~nMarkedMMOutput: ~p~nStrippedMarkedMappings: ~p~~n',
	       [MarkedMMOutput,StrippedMarkedMappings]).

reinsert_mmoutput_aux([], StrippedMarkedMappingsIn, StrippedMarkedMappingsIn, [], []) :-
	!.
reinsert_mmoutput_aux([markedphrase(UttNum,PhraseNum,Phrase1,Candidates,
				    _Mappings,PWI,GVCs,EV0,_APhrases)
		        |RestMarkedPhrases],
		      [markedmappings(UttNum,PhraseNum,ReinsertedMappings,ReinsertedAPhrases)
		        |RestStrippedMarkedMappings],
		      StrippedMarkedMappingsOut,
		      [phrase(Phrase1,Candidates,ReinsertedMappings,
		      	      PWI,GVCs,EV0,ReinsertedAPhrases)
			|RestReinsertedMMOPhrases],
		      [ReinsertedExtractedPhrases
		  	|RestReinsertedExtractedPhrases]) :-
	ReinsertedAPhrases = aphrases(ReinsertedAPs),
	extract_phrases_from_aps(ReinsertedAPs, ReinsertedExtractedPhrases),
	!,
	% temp
	%format('reinsert_mmoutput/5:~nReinsertedExtractedPhrases: ~p~n',
	%[ReinsertedExtractedPhrases]),
	reinsert_mmoutput_aux(RestMarkedPhrases,
			      RestStrippedMarkedMappings, StrippedMarkedMappingsOut,
			      RestReinsertedMMOPhrases, RestReinsertedExtractedPhrases).
% temp
reinsert_mmoutput_aux(MMOPhrases, StrippedMarkedMappings, _, MMOPhrases, _) :-
	fatal_error('reinsert_mmoutput/4 failed for~nMMOPhrases: ~p~nStrippedMarkedMappings: ~p~n~n',
	       [MMOPhrases,StrippedMarkedMappings]).

call_WSD(MMOTermList, WSDServerStream, WSDString) :-
	get_WSD_parameters(MethodList, BeginDelimiterChars, EndDelimiterChars),
	mmo_terms_to_xml_chars(MMOTermList, MethodList, XMLRequest),
	% format(user_output, 'BEFORE call_WSD_client~n', []),
	call_WSD_server(XMLRequest, WSDServerStream, Response),
	append([BeginDelimiterChars, WSDString, EndDelimiterChars, [10]], Response),
	!.
	% atom_codes(WSDAtomOut, WSDString).
	% format("wsdmod.pl:wsd/1: ~a~n",[WSDAtomOut]), ttyflush.

get_WSD_parameters(MethodList, BeginDelimiterChars, EndDelimiterChars) :-
	environ('WSD_METHODS', MethodsAtom),
	atom_codes(MethodsAtom, MethodChars),
	append(MethodChars, ".", MethodCharsWithPeriod),
	read_from_codes(MethodCharsWithPeriod, Methods),
	environ('WSD_WEIGHTS', WeightsAtom),
	atom_codes(WeightsAtom, WeightsChars),	
	append(WeightsChars, ".", WeightsCharsWithPeriod),
	read_from_codes(WeightsCharsWithPeriod, Weights),
	environ('WSD_SERVER_BEGIN_DELIMITER', BEGIN_DELIMITER),
	atom_codes(BEGIN_DELIMITER, BeginDelimiterChars),
	environ('WSD_SERVER_END_DELIMITER',   END_DELIMITER),
	atom_codes(END_DELIMITER, EndDelimiterChars),
	make_method_list(Methods, Weights, MethodList).

call_WSD_server(XMLRequest, WSDServerStream, Response) :-
	% format("initiating conversation: request: ~s~n~n^THE_END^~n",[XMLRequest]),
	% atom_codes(XMLRequestAtom, XMLRequest),
	% format(user_output, 'BEFORE post_WSD_query: ~w~n', [XMLRequestAtom]),
	test_post_WSD_query(WSDServerStream, XMLRequest),
	% format(user_output, 'BEFORE get_WSD_result~n', []),
	test_get_WSD_result(WSDServerStream, XMLRequest, Response),
	% atom_codes(ResponseAtom, Response),
	% format(user_output, 'WSD: ~w~n', [ResponseAtom]),
	% atom_codes(ResponseAtom, Response),
	% format(user_output, 'RESULT: ~q~n~n', [ResponseAtom]), ttyflush,
	% close(SocketStream),
	!.
	% format("conversation ended: response: ~s.~n",[Response]).


% post_WSD_query/2
% first argument is an XML request for Knowledge Source server.
test_post_WSD_query(SocketStream, Request) :-
	( format(SocketStream, "~s~n~n^THE_END^~n", [Request]),
	  flush_output(SocketStream) ->
	  true
        ; fatal_error('Unable to post WSD query~n~w~n', [Request])
        ).

% get_WSD_result/2
% first argument is an XML response for Knowledge Source server to be filled in.
test_get_WSD_result(SocketStream, Request, StreamTerm) :-
	( get_codes_WSD(SocketStream, StreamTerm) ->
	  true
        ; fatal_error('Unable to get WSD result for ~n~w~n', [Request])
        ).

get_codes_WSD(Stream, Input) :-
	get_code(Stream, Code),
	( Code is -1 ->
	  Input = []
	; Input = [Code|RestCodes],
	  get_codes_WSD(Stream, RestCodes)
	).

make_method_list([], [], []).
make_method_list([Method0|Methods], [Weight|Weights], [MethodElement|MethodList]) :-
	atom_codes(Method0, Method),
	MethodElement = method(Method, Weight),
	make_method_list(Methods, Weights, MethodList).

display_WSD_methods :-
	get_WSD_methods(WSDMethods),
	sort(WSDMethods, SortedWSDMethods),
	(  foreach(ThisShortName-_ThisLongName, SortedWSDMethods),
	   foreach(ThisShortNameLength, ShortNameLengths)
	do
	   length(ThisShortName, ThisShortNameLength)
	),
	max_member(LongestShortNameLength, ShortNameLengths),
	member(ShortName-LongName, SortedWSDMethods),
	trim_whitespace_right(LongName, TrimmedLongName),
	length(ShortName, ShortNameLength),
	Padding is LongestShortNameLength - ShortNameLength,
	format('~s~*c : ~s~n', [ShortName,Padding,32,TrimmedLongName]),
	fail
      ; true.


get_WSD_methods(Methods) :-
	call_WSD_server("<?xml version=""1.0""?><rdf:RDF xmlns:rdf=""http://www.w3.org/1999/02/22-rdf-syntax-ns#""           xmlns:wsd=""http://wsd.nlm.nih.gov/wsdserver#"">      <rdf:description about=""methodslistrequest"">      </rdf:description></rdf:RDF>",'ii-server2',0,5554, ResponseString),
	xml_parse(ResponseString, ResponseTerm, [format(false)]),
	arg(2, ResponseTerm, [_,NS,_]),
	arg(3, NS, RDFElement),
	arg(3, RDFElement, List1),
	memberchk(element(description,_,List2), List1),
	memberchk(element('Bag',_,List3), List2),
	(  foreach(Member, List3),
	   fromto([], S0, S, Methods)
	do ( Member = element(li, [], _) ->
	     arg(3, Member, [NameSpaceTerm]),
	     arg(3, NameSpaceTerm, MethodElement),
	     arg(2, MethodElement, [id=MethodShortName]),
	     arg(3, MethodElement, [pcdata(MethodLongName)]),
	     S = [MethodShortName-MethodLongName|S0]
	   ; S = S0
	   )
	).	
