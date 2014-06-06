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

% negex initial implementation
% ./src/skr/negex.pl, Thu Jul 31 11:00:57 2008, edit by Will Rogers
% Author: Willie Rogers

% See http://code.google.com/p/negex/wiki/NegExTerms

:- module(negex,[
	compute_negex/4,
	default_negex_semtypes/1,
	final_negation_template/6,
	generate_all_negex_output/1,
	generate_negex_output/1
   ]).

:- use_module(metamap(metamap_tokenization),[
        tokenize_text_utterly/2
   ]).

:- use_module(skr(skr_utilities), [
	fatal_error/2,
	get_candidate_feature/3,
	get_all_candidate_features/3
   ]).

:- use_module(skr_lib(flatten), [
	flatten/2
   ]).

:- use_module(skr_lib(nls_strings),[
	split_string_completely/3
   ]).

:- use_module(skr_lib(nls_system),[
        control_option/1,
	control_value/2
   ]).

:- use_module(skr_lib(sicstus_utils), [
	concat_atom/3,
	sublist/2,
	ttyflush/0
   ]).

:- use_module(skr(skr_xml),[
        xml_output_format/1
   ]).

:- use_module(library(lists), [
	append/2,
	delete/3,
	delete/4,
	last/2,
	select/3,
	subseq1/2
   ]).

:- use_module(library(sets), [
	disjoint/2,
	intersection/3,
	subtract/3
   ]).

:- use_module(library(system), [
	environ/2
   ]).

:- use_module(skr_lib(print_chars)).

% These two predicates should be the ONLY instances
% where the negation/6 term appears explicitly.

% orig_negation_template is the initial form of negation/6 terms
% as they're initially created -- before they're merged
orig_negation_template(negation(Type, TriggerText, TriggerPosInfo,
				ConceptName, CUI, ConceptPosInfo),
		       Type, TriggerText, TriggerPosInfo,
		       ConceptName, CUI, ConceptPosInfo).

% final_negation_template is the final form of negation/5 terms
% after multiple concepts and CUIs from the same negation are merged
final_negation_template(negation(Type, TriggerText, TriggerPosInfo,
				 ConceptCUIList, ConceptPosInfo),
			Type, TriggerText, TriggerPosInfo,
			ConceptCUIList, ConceptPosInfo).
		  

% find all negated concepts
compute_negex(RawTokenList, Lines, DisambMMOutput, NegationTerms) :-
	( compute_negex_1(RawTokenList, DisambMMOutput, NegationTerms) ->
	  true
	; fatal_error('Negex failed on "~p".~n', [Lines]),
	  abort
	).

do_negex :-
	( control_option(negex) ->
	  true
	; control_option(fielded_mmi_output) ->
	  true
	; control_value(negex_st_add, _) ->
	  true
	; control_value(negex_st_del, _) ->
	  true
	; control_value(negex_st_set, _)
	).

compute_negex_1(RawTokenList, DisambMMOutput, NegationTerms) :-
	( \+ control_option(machine_output),
	  \+ control_option(fielded_mmi_output),
	  \+ xml_output_format(_XMLFormat),
	  \+ do_negex ->
	  NegationTerms = []
	; environ('NEGEX_UTTERANCE_MAX_DIST', UtteranceMaxDistAtom),
	  atom_codes(UtteranceMaxDistAtom,    UtteranceMaxDistChars),
	  number_codes(UtteranceMaxDistNum ,  UtteranceMaxDistChars),
	  environ('NEGEX_CONCEPT_MAX_DIST',   ConceptMaxDistAtom),
	  atom_codes(ConceptMaxDistAtom,      ConceptMaxDistChars),
	  number_codes(ConceptMaxDistNum ,    ConceptMaxDistChars),
	  % Split the token list for the entire utterance into a list of lists of tokens;
	  % each sub-list contains all the tokens for a single utterance.
	  split_token_list(RawTokenList, ListOfTokenLists),
	  negex_aux(ListOfTokenLists, DisambMMOutput,
		    UtteranceMaxDistNum, ConceptMaxDistNum, NegationTerms0),
	  flatten(NegationTerms0, NegationTerms)
	).
	
negex_aux([], [], _UtteranceMaxDist, _ConceptMaxDist, []).
negex_aux([RawTokenList|ListOfTokenLists], [MMOutput|MMOutputList],
	  UtteranceMaxDist, ConceptMaxDist, [NegationTerm|NegationTerms]) :-
	MMOutput = mm_output(_,_,_,_,_,_,DisambiguatedMMOPhraseList,_),
	token_negex(RawTokenList, DisambiguatedMMOPhraseList,
		    UtteranceMaxDist, ConceptMaxDist, NegationTerm),
	negex_aux(ListOfTokenLists, MMOutputList, UtteranceMaxDist, ConceptMaxDist, NegationTerms).


% NegEx output should be generated IFF
% * --negex is on, and
% neither machine_output nor XML is on.
generate_negex_output(NegationTerms) :-
	( do_negex,
	  \+ control_option(machine_output),
	  \+ control_option(fielded_mmi_output),
	  \+ xml_output_format(_XMLFormat) ->
	   format('~nNEGATIONS:~n', []),
	   generate_all_negex_output(NegationTerms),
	   nl
	; true
	).

generate_all_negex_output([]).
generate_all_negex_output([H|T]) :-
	generate_one_negex_output(H),
	generate_all_negex_output(T).

generate_one_negex_output(NegExTerm) :-
	final_negation_template(NegExTerm,
				NegationType, NegationTrigger, NegationPosInfo,
				ConceptCUIList, NegatedConceptPosInfo),
	display_negex_info('Negation Type:     ', NegationType),
	display_negex_info('Negation Trigger:  ', NegationTrigger),
	display_negex_info('Negation PosInfo:  ', NegationPosInfo),
	display_negex_info('Negated  Concept:  ', ConceptCUIList),
	display_negex_info('Concept  PosInfo:  ', NegatedConceptPosInfo),
	nl.


display_negex_info(Header, Data) :-
	( atomic(Data) ->
	  display_negex_atom_info(Header, Data)
	; display_negex_list_info(Header, Data)
	).

display_negex_atom_info(Header, AtomData) :-
	format('~w~w~n', [Header,AtomData]).

display_negex_list_info(Header, ListData) :-
	format('~w', [Header]),
	ListData = [H|T],
	write_list_elements(T, H).

write_list_elements([], Datum) :-
	format('~w~n', [Datum]).
write_list_elements([H|T], Datum) :-
	format('~w, ', [Datum]),
	write_list_elements(T, H).

% token based marking of nega (and pnega) negations
%
% 1. find negation phrase in tokenlist
% 2. extract tokens following negation phrase.
% 3. mark as negated concepts in phrase mappings references by extracted tokens.
%
% first attempt -- mostly exploration with using token based 
token_negex(RawTokenList, DisambiguatedMMOPhrases,
	    UtteranceMaxDist, ConceptMaxDist, ConsolidatedNegationTerms) :-
	% Remove field, label, pn, sn, pe, and ws tokens.
 	remove_nonuseful_tokens(RawTokenList, UsefulTokenList),
 	extract_atoms_from_tokenlist(UsefulTokenList, AtomList),
	% [ [nega, [not]] ... ]
 	get_negation_phrase_list(AtomList, NegationPhraseList0),
	sort(NegationPhraseList0, NegationPhraseList),
	generate_negation_terms_1(NegationPhraseList, DisambiguatedMMOPhrases,
				  UtteranceMaxDist, ConceptMaxDist,
				  UsefulTokenList, ConsolidatedNegationTerms).

generate_negation_terms_1([], _DisambiguatedMMOPhrases,
			  _UtteranceMaxDist, _ConceptMaxDist, _UsefulTokenList, []).
generate_negation_terms_1([H|T], DisambiguatedMMOPhrases,
			  UtteranceMaxDist, ConceptMaxDist,
			  UsefulTokenList, NegationTerms) :-
	NegationPhraseList = [H|T],
	% TriggerList = [trigger(nega,[tok(lc,"not","not",pos(83,86),pos(83,3))])]
	get_triggerlist(UsefulTokenList, NegationPhraseList, TriggerList0),
	% PhraseMaps0 is a list of lists of the form [PhraseObject,Mappings];
	% PhraseObject is phrase(PhraseAtom, Syntax, StartPos/Length, ReplPos), and
	% Mappings is a list of the mappings generated from that phrase.
	list_phrases_with_mappings(DisambiguatedMMOPhrases, PhraseMaps0),
	% Keep only [PhraseObject,Mappings] lists in which
	% PhraseObject is head or verb, and Mappings is nonempty.
	keep_useful_phrasemaps(PhraseMaps0, PhraseMaps),
	generate_negation_terms_2(PhraseMaps, UsefulTokenList,
				  UtteranceMaxDist, ConceptMaxDist,
				  TriggerList0, NegationTerms).

generate_negation_terms_2([], _UsefulTokenList,
			  _UtteranceMaxDist, _ConceptMaxDist, _TriggerList0, []).
generate_negation_terms_2([H|T], UsefulTokenList,
			  UtteranceMaxDist, ConceptMaxDist,
			  TriggerList0, ConsolidatedNegationTerms) :-
	PhraseMaps = [H|T],
	extract_trigger_sequences(TriggerList0, TriggerSeqList0),
	remove_proper_subseqs(TriggerSeqList0, TriggerSeqList),
	keep_triggers_with_seqs(TriggerList0, TriggerSeqList, Triggers),
	list_concepts_and_tokens(PhraseMaps, UsefulTokenList, ConceptTokensPhraseMaps),
	list_concepts_for_triggers(Triggers, ConceptTokensPhraseMaps, NegationTerms0),
	remove_negation_terms_after_conjs(NegationTerms0, NegationTerms1),
	cull_negterms_eliminated_by_conj_and(UsefulTokenList, NegationTerms1, NegationTerms2),
	remove_spurious_negterms(NegationTerms2,
				 UtteranceMaxDist, ConceptMaxDist,
				 UsefulTokenList, NegationTerms),
	consolidate_negation_terms(NegationTerms, ConsolidatedNegationTerms).

remove_nonuseful_tokens([], []).
remove_nonuseful_tokens([H|T], TokenListOut) :-
	( non_useful(H) ->
	  RestTokenListOut = TokenListOut
	; TokenListOut = [H|RestTokenListOut]
	),
	remove_nonuseful_tokens(T, RestTokenListOut).

non_useful(tok(TokenType,_,_,_,_)) :- nonuseful_token_type(TokenType).

nonuseful_token_type(field).
nonuseful_token_type(label).
nonuseful_token_type(pn).
nonuseful_token_type(sn).
nonuseful_token_type(pe).
nonuseful_token_type(ws).

% list tokens which correspond to the supplied PhraseMap
list_mapped_tokens([], _, []).
list_mapped_tokens([Token|RawTokenListIn], PhraseMap, MappedTokens) :-
	Token = tok(_Type,_String,_Nstring, _Pos0, pos(TokenStart,TokenLength)),	
	PhraseMap = [phrase(_PhraseText,_Mincoman,_PhraseStartPos/_PhraseEndPos,_),
		     mappings(MapList)],
	( maplist_contains_range(MapList, TokenStart, TokenLength) ->
	  MappedTokens = [Token|RestMappedTokens]
	; MappedTokens = RestMappedTokens
	),
	list_mapped_tokens(RawTokenListIn, PhraseMap, RestMappedTokens).

maplist_contains_range([Map|MapList], TokenStart, TokenLength) :-
	Map = map(_score,EvList),
	( evlist_contains_range(EvList, TokenStart, TokenLength) ->
	  true
	; maplist_contains_range(MapList, TokenStart, TokenLength)
	).

evlist_contains_range([FirstCandidate|RestCandidates], TokenStart, TokenLength) :-
	get_candidate_feature(posinfo, FirstCandidate, PositionList),
	( positionlist_contains_range(PositionList, TokenStart, TokenLength) ->
	  true
	; evlist_contains_range(RestCandidates, TokenStart, TokenLength)
	).

%% Is the Token within the mapping?
%% These assumptions must hold:
%% (1)  TokenStart >= MappingStart and TokenStart < (MappingStart + MappingLength)
%% (2) (TokenStart + TokenLength) <= (MappingStart + MappingLength)

positionlist_contains_range([Position|Positionlist], TokenStart, TokenLength) :-
	Position = MappingStart/MappingLength,
	MappingEnd is (MappingStart + MappingLength + 1),
	TokenEnd is (TokenStart + TokenLength),
	( TokenStart >= MappingStart,
	  TokenStart < MappingEnd,
	  TokenEnd < MappingEnd ->
	  true
	; positionlist_contains_range(Positionlist, TokenStart, TokenLength)
	).

% extract_tokens_from_string(PhraseText,Tokens)
list_concepts_and_tokens([], _, []).
list_concepts_and_tokens([PhraseMap|PhraseMaps], RawTokenListIn, ConceptTokensPhraseMap) :-
	list_mapped_tokens(RawTokenListIn, PhraseMap, MappedTokens),
	( MappedTokens = [] ->
	  ConceptTokensPhraseMap = RestConceptTokensPhraseMap
	  % PhraseObject = _,
	  % Mappings = _
	; PhraseMap = [PhraseObject,Mappings],
	  % reversed order of args from QP library version!
	  last(MappedTokens, LastMappedToken),
	  ConceptTokensPhraseMap = [[LastMappedToken,PhraseObject,Mappings]
				    |RestConceptTokensPhraseMap]
	),
	list_concepts_and_tokens(PhraseMaps, RawTokenListIn, RestConceptTokensPhraseMap).

% build a list of normalized atoms from list of tokens.
extract_atoms_from_tokenlist([], []).
extract_atoms_from_tokenlist([Token|RestTokens], [Atom|RestAtoms]) :-
	Token = tok(_Type,_UnNormString,String,_StartPos,_EndPos),
	atom_codes(Atom, String),
	extract_atoms_from_tokenlist(RestTokens, RestAtoms).

% atomize_strings/2 - convert a list of strings to a list of atoms
atomize_strings([], []).
atomize_strings([String|Stringlist], [Atom|Atomlist]) :-
	atom_codes(Atom,String),
	atomize_strings(Stringlist, Atomlist).

get_triggerlist(UsefulTokenList,NegationPhraseList,TriggerList) :-
	findall(X,
		get_neg_phrase_triggerlist(UsefulTokenList,NegationPhraseList,X),
		TriggerList0),
	flatten(TriggerList0,TriggerList).

get_neg_phrase_triggerlist(RawTokenListIn, NegPhraseList, TriggerList) :-
	member(NegPair, NegPhraseList),
	NegPair = [NegType,NegTerm],
	findall(X, map_negterm_tokens(RawTokenListIn,NegTerm,X), TokenResult),
	make_triggers(TokenResult, NegType, TriggerList).

make_triggers([], _, []).
make_triggers([TokenList|ListOfTokenLists], NegType, [Trigger|TriggerList]) :-
	Trigger = trigger(NegType,TokenList),
	make_triggers(ListOfTokenLists, NegType, TriggerList).

map_negterm_tokens(TokenList, NegTerm, NegTermTokenList) :-
	atomize_strings(NegTermStringList, NegTerm),
	tokenize_strings(NegTermStringList, NegTermTokenList),
	sublist(TokenList, NegTermTokenList).

tokenize_strings([], []).
tokenize_strings([String|StringList], [Token|TokenList]) :-
	Token = tok(_,_,String,_,_),
	tokenize_strings(StringList, TokenList).

% finding negations in a sentence (or utterance?)
%
% | ?- Z = ["the"," ","patient"," ","denies"," ","chest"," ","pain"," ","and"," ","has"," ","no"," ","shortage"," ","of"," ","breath"], atomize_strings(Z,Y), sublist(Y,X), negation_phrase_tokens(nega,X).
%
% Z = ["the"," ","patient"," ","denies"," ","chest"," ","pain"," ","and"," ","has"," ","no"," ","shortage"," ","of"," ","breath"],
% Y = [the,' ',patient,' ',denies,' ',chest,' ',pain,' ',and,' ',has,' ',no,' ',shortage,' ',of,' ',breath],
% X = [no] ;
%
% Z = ["the"," ","patient"," ","denies"," ","chest"," ","pain"," ","and"," ","has"," ","no"," ","shortage"," ","of"," ","breath"],
% Y = [the,' ',patient,' ',denies,' ',chest,' ',pain,' ',and,' ',has,' ',no,' ',shortage,' ',of,' ',breath],
% X = [denies] ;
%
% no
%
check_for_negation_phrase(Type, Target, WordList) :-
	sublist(Target, WordList),
	WordList = [H|T],
	negation_phrase_tokens(Type, H, T).

negation_phrase_tokens(nega, H, T) :-
	nega_phrase_tokens(H, T).
negation_phrase_tokens(negb, H, T) :-
	negb_phrase_tokens(H, T).
negation_phrase_tokens(pnega, H, T) :-
	pnega_phrase_tokens(H, T).
negation_phrase_tokens(pnegb, H, T) :-
	pnegb_phrase_tokens(H, T).
negation_phrase_tokens(pseudoneg, H, T) :-
	pseudoneg_phrase_tokens(H, T).
negation_phrase_tokens(conj, H, T) :-
	conj_phrase_tokens(H, T).

% Getting a list of negations using findall/3
%
% Y = [the,' ',patient,' ',denies,' ',chest,' ',pain,' ',and,' ',has,' ',no,' ',shortage,' ',of,' ',breath], findall(X, check_for_negation_phrase(nega,Y,X),L).
%
% Y = [the,' ',patient,' ',denies,' ',chest,' ',pain,' ',and,' ',has,' ',no,' ',shortage,' ',of,' ',breath],
% X = _7902,
% L = [[no],[denies]] 

get_negation_phrase_list(Target, ResultList) :-
	findall([Type,X],
		check_for_negation_phrase(Type,Target,X),
		ResultList).


% Return list of phrases and their associated mappings.
list_phrases_with_mappings([],[]).   
list_phrases_with_mappings([DisambiguatedMMOPhrase|DisambiguatedMMOPhraseList],
			   [PhraseMap|PhraseMaps]) :-
	DisambiguatedMMOPhrase =
	          phrase(PhraseObject,_Candidates,Mappings,_PWI,_GVCs,_EV0,_APhrases),
	PhraseMap = [PhraseObject,Mappings],
	list_phrases_with_mappings(DisambiguatedMMOPhraseList,PhraseMaps).


% map concepts of head and verb phrases, ignore the others
keep_useful_phrasemaps(PhraseMaps, FinalPhraseMaps) :-
	delete_phrasemaps_with_empty_mappings(PhraseMaps, FinalPhraseMaps0),
	delete_nonuseful_phrasemaps(FinalPhraseMaps0, FinalPhraseMaps).

mincoman_get_pos_tag([], []).
mincoman_get_pos_tag([MincomanElement|MincomanElements], Tag) :-
	( ( MincomanElement = head(_)
	  ; MincomanElement = verb(_) ) ->
	    Tag = MincomanElement
	; mincoman_get_pos_tag(MincomanElements, Tag)
	).

phrase_get_pos_tag(Phrase,Tag) :-
	Phrase = phrase(_LowerPhraseText,Mincoman,_PosPair,_),
	mincoman_get_pos_tag(Mincoman,Tag).

delete_nonuseful_phrasemaps([], []).
delete_nonuseful_phrasemaps([PhraseMap|PhraseMaps], FinalPhraseMaps) :-
	PhraseMap = [Phrase,_Mappings],
	phrase_get_pos_tag(Phrase, Tag),
	( ( Tag=head(_);  Tag=verb(_) ) ->
	    FinalPhraseMaps = [PhraseMap|RestFinalPhraseMaps]
	  ; FinalPhraseMaps = RestFinalPhraseMaps
	),
	delete_nonuseful_phrasemaps(PhraseMaps, RestFinalPhraseMaps).

delete_phrasemaps_with_empty_mappings([], []).
delete_phrasemaps_with_empty_mappings([PhraseMap|PhraseMaps], FinalPhraseMaps) :-
	PhraseMap = [_Phrase,Mappings],
	( Mappings = mappings([]) ->
	  FinalPhraseMaps = RestFinalPhraseMaps
	; FinalPhraseMaps = [PhraseMap|RestFinalPhraseMaps]
	),
	delete_phrasemaps_with_empty_mappings(PhraseMaps, RestFinalPhraseMaps).

%
% List concepts that have positions before or after trigger, based on
% type of negation.
%
list_concepts_for_triggers(Triggers,ConceptTokensPhraseMaps,PhraseMaps) :-
	list_concepts_for_triggers_aux(Triggers,ConceptTokensPhraseMaps,PhraseMaps0),
	flatten(PhraseMaps0,PhraseMaps1),
	sort(PhraseMaps1, PhraseMaps).

list_concepts_for_triggers_aux([],_,[]).
list_concepts_for_triggers_aux([Trigger|Triggers],ConceptTokensPhraseMaps,[PhraseMap|PhraseMaps]) :-
	Trigger = trigger(NegationType, TokenList),
	list_concepts_for_one_trigger(NegationType, TokenList, ConceptTokensPhraseMaps, PhraseMap),
	list_concepts_for_triggers_aux(Triggers,ConceptTokensPhraseMaps,PhraseMaps).

before_or_after(nega,      after).
before_or_after(pnega,     after).
before_or_after(pseudoneg, after).
before_or_after(negb,      before).
before_or_after(pnegb,     before).
before_or_after(conj,      after).

list_concepts_for_one_trigger(NegationType, TokenList, ConceptTokensPhraseMaps, PhraseMaps) :-
	before_or_after(NegationType, BeforeOrAfter),	
	list_concepts_for_one_trigger_aux(ConceptTokensPhraseMaps,
					  trigger(NegationType,TokenList),
					  BeforeOrAfter,
					  PhraseMaps0), 
	flatten(PhraseMaps0, PhraseMaps). 

list_concepts_for_one_trigger_aux([], _, _BeforeOrAfter, []).
list_concepts_for_one_trigger_aux([ConceptTokensPhraseMap|ConceptTokensPhraseMaps],
				  Trigger, BeforeOrAfter, PhraseMaps) :-
	get_trigger_position(BeforeOrAfter, Trigger, TriggerPositionTerm),
	get_negation_data(TriggerPositionTerm, ConceptTokensPhraseMap,
			  TriggerPosition, ConceptPosition, Phrase, Mappings),
	( % ConceptPosition > TriggerPosition ->
	  test_relative_position(BeforeOrAfter, ConceptPosition, TriggerPosition) ->
	  make_negation_terms(Trigger, Phrase, Mappings, PhraseMap),
	  PhraseMaps = [PhraseMap|RestPhraseMaps]
	; PhraseMaps = RestPhraseMaps
	),
	list_concepts_for_one_trigger_aux(ConceptTokensPhraseMaps, Trigger,
					  BeforeOrAfter, RestPhraseMaps).

get_trigger_position(BeforeOrAfter, trigger(_,Tokens), TriggerPositionTerm) :-
	get_trigger_token(BeforeOrAfter, Tokens, TriggerToken),
	TriggerToken = tok(_Type,_String,_LSCtring,_Pos,TriggerPositionTerm).

get_trigger_token(before, Tokens, TriggerToken) :-
	Tokens = [TriggerToken|_].
get_trigger_token(after, Tokens, TriggerToken) :-
	% reversed order of args from QP library version!
	last(Tokens, TriggerToken).

test_relative_position(before, ConceptPosition, TriggerPosition) :-
	ConceptPosition =< TriggerPosition.
test_relative_position(after, ConceptPosition, TriggerPosition) :-
	ConceptPosition >= TriggerPosition.

get_negation_data(TriggerPositionTerm, ConceptTokensPhraseMap,
		  TriggerPosition, ConceptPosition, Phrase, Mappings) :-
	TriggerPositionTerm     = pos(TriggerPosition,_TriggerLength),
	ConceptTokensPhraseMap  = [ConceptToken,Phrase,Mappings],
	ConceptToken            = tok(_Type,_String,_LCString,_Pos,ConceptPositionTerm),
	ConceptPositionTerm     = pos(ConceptPosition,_ConceptLength).

make_negation_terms(Trigger, Phrase, Mappings, NegationTermList) :-
	Trigger = trigger(Type,Tokens),
	Tokens = [FirstToken|_Rest],
	FirstToken = tok(_FType,_FString,_FLCString,
			 pos(TriggerAbsStartPos,_),
			 pos(TriggerStartPos,_)),
	% reversed order of args from QP library version!
	last(Tokens, LastToken),
	LastToken = tok(_LType,_LString,_LLCstring,
			pos(_,TriggerAbsEndPos),
			pos(_,_TriggerEndPos)),
	TriggerLength is TriggerAbsEndPos - TriggerAbsStartPos,
	extract_atoms_from_tokenlist(Tokens, AtomList),
	concat_atom(AtomList, ' ', TriggerPhraseText),
	Phrase = phrase(_PhraseText,_Mincoman,_PhraseStartPos/_PhraseEndPos,_),
	Mappings = mappings(MapList),
	negationlist_from_maplist(MapList, Type, TriggerPhraseText,
				  TriggerStartPos, TriggerLength, NegationTermList0),
	flatten(NegationTermList0, NegationTermList).
	
negationlist_from_maplist([], _, _, _, _, []).
negationlist_from_maplist([Map|MapList], Type, TriggerPhraseText, TriggerStartPos, TriggerLength,
			  [NegationTerm|NegationTermList]) :-
	Map = map(_score,EvList),
	negationlist_from_evlist(EvList, Type, TriggerPhraseText,
				 TriggerStartPos, TriggerLength, NegationTerm),
	negationlist_from_maplist(MapList, Type, TriggerPhraseText,
				  TriggerStartPos, TriggerLength, NegationTermList).

negationlist_from_evlist([], _, _, _, _, []).
negationlist_from_evlist([FirstCandidate|RestCandidates], Type, TriggerPhraseText, TriggerStartPos,
			 TriggerLength, NegationTerms) :-
	get_all_candidate_features([cui,metaterm,semtypes,posinfo],
				   FirstCandidate,
				   [CUI,ConceptName,SemTypes,CandidatePosInfo]),
	% format('SemGroup=~q, CUI=~q, ConceptName=~q, SemTypes=~q~n',
	%        [[fndg,dsyn,sosy,cgab,acab,lbtr,inpo,biof,phsf,menp,mobd,comd,anab,emod,patf],
	%         CUI,ConceptName,SemTypes]),
	% true if set of semantic types for the ev term
	% does not contain any in the negex semantic group set
	% Note: The neop and patf semtypes are not part of the original specification.
	negex_semtypes(NegExSemTypes),
	( disjoint(SemTypes, NegExSemTypes) ->
	  NegationTerms = RestNegationTerms
	; % ConceptPosInfo = [ConceptPosInfo0],
	  orig_negation_template(NegationTerm,
				 Type, TriggerPhraseText, [TriggerStartPos/TriggerLength],
				 ConceptName, CUI, CandidatePosInfo),
	  NegationTerms = [NegationTerm|RestNegationTerms]
	),
	negationlist_from_evlist(RestCandidates, Type, TriggerPhraseText,
				 TriggerStartPos, TriggerLength, RestNegationTerms).

negex_semtypes(NegExSemTypes) :-
	default_negex_semtypes(DefaultNegExSemTypes),
	( control_value(negex_st_add, NegExSemTypesAdd) ->
	  append(DefaultNegExSemTypes, NegExSemTypesAdd, NegExSemTypes0),
	  sort(NegExSemTypes0, NegExSemTypes1)
	; NegExSemTypes1 = DefaultNegExSemTypes
	),
	( control_value(negex_st_del, NegExSemTypesDel) ->
	  subtract(NegExSemTypes1, NegExSemTypesDel, NegExSemTypes2),
	  sort(NegExSemTypes2, NegExSemTypes3)
	; NegExSemTypes3 = NegExSemTypes1
	),
	( control_value(negex_st_set, NegExSemTypesSet) ->
	  sort(NegExSemTypesSet, NegExSemTypes4)
	; NegExSemTypes4 = NegExSemTypes3
	),
	( intersection(NegExSemTypes4, [all,'ALL'], [_|_]) ->
	  NegExSemTypes = _ALL
	; NegExSemTypes = NegExSemTypes3
	).

default_negex_semtypes([acab,anab,biof,cgab,comd,dsyn,emod,fndg,
			inpo,lbtr,menp,mobd,neop,patf,phsf,sosy]).

% remove negation terms after conjunctions if there are any conjunctions.
remove_negation_terms_after_conjs(NegationTerms,FilteredNegationTerms) :-
	( find_conj_terms(NegationTerms,ConjunctionTerms) ->
	  remove_negation_terms_after_conjs_aux(NegationTerms,ConjunctionTerms,
						FilteredNegationTerms0),
	  flatten(FilteredNegationTerms0, FilteredNegationTerms)
	; FilteredNegationTerms = NegationTerms
	).

remove_negation_terms_after_conjs_aux([] ,_, []).
remove_negation_terms_after_conjs_aux([NegationTerm|NegationTerms], ConjunctionTerms,
 				      FilteredNegationTerms) :-
	( is_negation_term_after_conj(ConjunctionTerms, NegationTerm) ->
	  FilteredNegationTerms = RestFilteredNegationTerms
	; FilteredNegationTerms = [NegationTerm|RestFilteredNegationTerms]
	),
	remove_negation_terms_after_conjs_aux(NegationTerms, ConjunctionTerms,
					      RestFilteredNegationTerms).

% does negation term correspond to one the conjunction negation terms?
is_negation_term_after_conj([ConjunctionTerm|ConjunctionTerms],NegationTerm) :-
	orig_negation_template(ConjunctionTerm,
			       conj, _TriggerPhraseText, [ConjTriggerStartPos/_],
			       _Concept1, _CUI1, ConjConceptPosInfo),
	orig_negation_template(NegationTerm,
			       _Type, _NegationPhraseText, [NegationTriggerStartPos/_],
			       _Concept2, _CUI2, NegationConceptPosInfo),
	% NOTE: ConjPosInfo in conjunction and NegationPosInfo should be
	% exactly the same if they came from the same mapping.
	( ConjTriggerStartPos >= NegationTriggerStartPos,
	  ConjConceptPosInfo  == NegationConceptPosInfo ->
	  true
	; is_negation_term_after_conj(ConjunctionTerms, NegationTerm)
	).

% list the conjunction negation term if they exist
find_conj_terms([], []).
% find_conj_terms_aux([NegationTerm|NegationTerms], [ConjunctionTerm|ConjunctionTerms]) :-
find_conj_terms([NegationTerm|NegationTerms], ConjunctionTerms) :-
	( orig_negation_template(NegationTerm,
				 conj, _ConjTrigger, _ConjTriggerPosInfo,
				 _ConjConcept, _ConjCUI, _ConjConceptPosInfo) ->
	  ConjunctionTerm = NegationTerm,
	  ConjunctionTerms = [ConjunctionTerm|RestConjunctionTerms]
	; ConjunctionTerms = RestConjunctionTerms
	),
	find_conj_terms(NegationTerms, RestConjunctionTerms).

extract_trigger_sequences([], []).
extract_trigger_sequences([Trigger|TriggerList], [Seq|SeqList]) :-
	Trigger = trigger(_, Seq),
	extract_trigger_sequences(TriggerList, SeqList).

% Keep triggers that have an associated token sequence that matches an
% element of the list of unsubsumed token sequences.  
keep_triggers_with_seqs([],_,[]).
keep_triggers_with_seqs([Trigger0|Triggers0], TriggerSeqList, Triggers) :-
	Trigger0 = trigger(_, Seq),
	% is Seq of Trigger0 member of TriggerSeqList?
	( memberchk(Seq, TriggerSeqList) -> 
	  Trigger = Trigger0,
	  Triggers = [Trigger|RestTriggers]
	; Triggers = RestTriggers
	),
	keep_triggers_with_seqs(Triggers0, TriggerSeqList, RestTriggers).

%
% Given 
%   X=[[a],[a,b,c]]
% such that 
%   Y=[[a,b,c]]
% or given
%   X=[[a],[a,b],[a,b,c]]
% such that 
%   Y=[[a,b,c]]
% Given a list of sequences, remove all sequences that are proper
% subsequences of any sequence in the list.

remove_proper_subseqs([], []).
remove_proper_subseqs([Seq|Seqs], Results) :-
	( is_proper_subseq_of_anyseq(Seq,Seqs) ->
	  Results = RestResults
	; Results = [Seq|RestResults]
	),
 	remove_proper_subseqs(Seqs, RestResults).

list_proper_subseq_of_anyseq([],_,[]).
list_proper_subseq_of_anyseq([Seq|Seqs], TargetSeq, [Result|Results]) :-
	( subseq1(Seq, TargetSeq) -> 
	  Result = Seq
	; Result = []
	),
	list_proper_subseq_of_anyseq(Seqs, TargetSeq, Results).

is_proper_subseq_of_anyseq(TargetSeq, Seqs) :-
	list_proper_subseq_of_anyseq(Seqs,TargetSeq,Results0),
	flatten(Results0,Results),
	length(Results,L), 
	L > 0.

%
% if "and" in utterance and
%      a negation phrase precedes the "and" and a negation phrase follows the "and"
% then
%     umls concepts preceding the "and" belong to the negation phrase preceding the and
%     umls concepts following the "and" belong to the negation phrase following the and
% From: 
% NegEx version 2: (http://www.dbmi.pitt.edu/chapman/NegEx.html), III. NegEx Algorithm:, Section A.
%
cull_negterms_eliminated_by_conj_and(TokenList, NegationTerms, FilteredNegationTerms) :-
	( string_find_tokenpos(TokenList, "and", AndPos) ->
	    ( are_negationterms_before_target(NegationTerms,AndPos),
	      are_negationterms_after_target(NegationTerms,AndPos) ->
	      list_negation_pairs_before_target(NegationTerms, AndPos, NegationTerms0),
	      list_negation_pairs_after_target(NegationTerms, AndPos, NegationTerms1),
	      append(NegationTerms0, NegationTerms1, FilteredNegationTerms)
	    ; FilteredNegationTerms = NegationTerms
	    )
	; FilteredNegationTerms = NegationTerms
	).

string_find_tokenpos([Token|TokenList], TermString, TokenPos) :-
	Token = tok(_,_,NString,_,PosLen),
	PosLen = pos(Pos,_),
	( NString == TermString ->
	  TokenPos = Pos
	; string_find_tokenpos(TokenList, TermString, TokenPos)
	).

are_negationterms_before_target([NegationTerm|NegationTermList], TargetPos) :-
	orig_negation_template(NegationTerm,
			       _Type, _TriggerPhraseText, TriggerPosInfo,
			       _Concept, _CUI, _ConceptPosInfo),

	TriggerPosInfo = [TermPos/_|_],
	( TermPos < TargetPos ->
	  true
	; are_negationterms_before_target(NegationTermList, TargetPos)
	).
	
are_negationterms_after_target([NegationTerm|NegationTermList], TargetPos) :-
	orig_negation_template(NegationTerm,
			       _Type, _TriggerText, TriggerPosInfo,
			       _ConceptName, _CUI, _ConceptPosInfo),
	TriggerPosInfo = [TermPos/_|_],
	( TermPos > TargetPos ->
	  true
	; are_negationterms_after_target(NegationTermList, TargetPos)
	).

list_negation_pairs_before_target([], _, []).
list_negation_pairs_before_target([NegationTerm|NegationTermList], TargetPos,
				  FilteredNegationTerms) :-
	orig_negation_template(NegationTerm,
			       _Type, _TriggerText, TriggerPosInfo,
			       _ConceptName, _CUI, ConceptPosInfo),
	% PosInfo can consist of several StartPos/Length terms
	TriggerPosInfo = [TriggerStartPos/_|_],
	ConceptPosInfo = [ConceptStartPos/_|_],
	( TriggerStartPos < TargetPos,
	  ConceptStartPos < TargetPos ->
	  FilteredNegationTerms = [NegationTerm|RestFilteredNegationTerms]
	; FilteredNegationTerms = RestFilteredNegationTerms
	),
	list_negation_pairs_before_target(NegationTermList, TargetPos, RestFilteredNegationTerms).

list_negation_pairs_after_target([], _, []).
list_negation_pairs_after_target([NegationTerm|NegationTermList], TargetPos,
				 FilteredNegationTerms) :-
	orig_negation_template(NegationTerm,
			       _Type, _TriggerText, TriggerPosInfo,
			       _ConceptName, _CUI, ConceptPosInfo),
	% PosInfo can consist of several StartPos/Length terms
	TriggerPosInfo = [TriggerStartPos/_|_],
	ConceptPosInfo = [ConceptStartPos/_|_],
	( TriggerStartPos > TargetPos,
	  ConceptStartPos > TargetPos ->
	  FilteredNegationTerms = [NegationTerm|RestFilteredNegationTerms]
	; FilteredNegationTerms = RestFilteredNegationTerms
	),
	list_negation_pairs_after_target(NegationTermList, TargetPos, RestFilteredNegationTerms).

remove_spurious_negterms([], _, _, _, []).
remove_spurious_negterms([NegationTerm|NegationTermList],
			 UtteranceMaxDist, ConceptMaxDist,
			 TokenList, NegationTermsOut) :- 
	( spurious_negterm(TokenList, UtteranceMaxDist, ConceptMaxDist, NegationTerm) ->
	  NegationTermsOut = RestNegationTermsOut
	; NegationTermsOut = [NegationTerm|RestNegationTermsOut]
	),
	remove_spurious_negterms(NegationTermList,
					   UtteranceMaxDist, ConceptMaxDist,
					   TokenList, RestNegationTermsOut).

spurious_negterm(TokenList, UtteranceMaxDist, ConceptMaxDist, NegationTerm) :-
	orig_negation_template(NegationTerm,
			       _Type, TriggerText, TriggerPosInfo,
			       _ConceptName, _CUI, ConceptPosInfo),
	charpos_to_tokenindex(TokenList, NegationTerm, TriggerPosInfo, 1, TriggerTokenPos),
	charpos_to_tokenindex(TokenList, NegationTerm, ConceptPosInfo, 1, ConceptTokenPos),
	( negterm_outside_window(TokenList, TriggerText,
				 TriggerTokenPos, ConceptTokenPos,
				 UtteranceMaxDist, ConceptMaxDist) ->
	  true
	; intervening_negation_trigger(TokenList, NegationTerm)
	).

% We have decided to modify the window logic as follows:
% (1) If the number of tokens between the end of the negation trigger
%     and the end of the utterance is =< UtteranceMaxDist (currently 20),
%     do not use any window -- i.e., DO NOT rule out the negation;
% (2) Otherwise, use a window size of ConceptMaxDist (currently 10)
%     within which both the negation trigger and the negated concept must be found.

% The logic reduces to this: The negation term is outside the window
% IFF the following two conditions both hold:
% (a) the distance between the trigger and the end of the utterance
%     exceeds UtteranceMaxDist
% (b) the distance between the trigger and the concept
%     exceeds ConceptMaxDist

negterm_outside_window(TokenList, TriggerText,
		       TriggerTokenPos, ConceptTokenPos,
		       UtteranceMaxDist, ConceptMaxDist) :-
	length(TokenList, TokenListLength),
	term_wordlength(TriggerText, WordLength),
	TriggerLastTokenPos is TriggerTokenPos + WordLength - 1,
	TriggerDistanceFromUtteranceEnd is TokenListLength - TriggerLastTokenPos,
	TriggerDistanceFromConcept is abs(ConceptTokenPos - TriggerLastTokenPos),
	TriggerDistanceFromUtteranceEnd > UtteranceMaxDist,
	TriggerDistanceFromConcept > ConceptMaxDist.

intervening_negation_trigger(TokenList, NegationTerm) :-
	orig_negation_template(NegationTerm,
			       _Type, TriggerPhraseText, TriggerPosInfo,
			       _Concept, _CUI, ConceptPosInfo),
	TriggerPosInfo = [FirstTriggerStartPos/_FirstTriggerEndPos|_],
	ConceptPosInfo = [FirstConceptStartPos/_FirstConceptEndPos|_],
	sort([FirstTriggerStartPos, FirstConceptStartPos], [EarlierStartPos, LaterStartPos]),
	extract_negation_tokens(TokenList, EarlierStartPos, LaterStartPos, NegationTokens),
 	extract_atoms_from_tokenlist(NegationTokens, NegationAtomList),
	tokenize_text_utterly(TriggerPhraseText, TriggerPhraseAtomsWithBlanks),
	remove_blank_atoms(TriggerPhraseAtomsWithBlanks, TriggerPhraseAtoms),
	append([_Before, TriggerPhraseAtoms, PhraseAtomsAfterTrigger], NegationAtomList),
	!,
	check_for_negation_phrase(_Type, PhraseAtomsAfterTrigger, _WordList),
	!.

% Extract all the tokens from the TokenList that
% lie between the negation trigger and the negated concept.
extract_negation_tokens([], _EarlierStartPos, _LaterStartPos, []).
extract_negation_tokens([FirstToken|RestTokens],
			EarlierStartPos, LaterStartPos, NegationTokens) :-
	FirstToken = tok(_Type,_String,_NormalizedString,_AbsPosInfo,RelPosInfo),
	RelPosInfo = pos(TokenStartPos, _Length),
	( TokenStartPos < EarlierStartPos ->
	  extract_negation_tokens(RestTokens, EarlierStartPos, LaterStartPos, NegationTokens)
	; TokenStartPos >= EarlierStartPos,
	  TokenStartPos =< LaterStartPos ->
	  NegationTokens = [FirstToken|RestNegationTokens],
	  extract_negation_tokens(RestTokens, EarlierStartPos, LaterStartPos, RestNegationTokens)
	; NegationTokens = []
	).  


remove_blank_atoms([], []).
remove_blank_atoms([H|T], AtomsWithNoBlanks) :-
	( H == ' ' ->
	  remove_blank_atoms(T, AtomsWithNoBlanks)
	; AtomsWithNoBlanks = [H|RestAtomsWithNoBlanks],
	  remove_blank_atoms(T, RestAtomsWithNoBlanks)
	).

% determine number of words in term.
term_wordlength(Term, WordLength) :-
	atom_codes(Term, TermString),
	split_string_completely(TermString, " ", TermList),
	length(TermList, WordLength).

% This error prevents TokenIndex from being returned uninstantiated,
% which should never happen!
charpos_to_tokenindex([], NegationTerm, CharPosInfo, _, _) :-
	fatal_error('NegEx negation "~p" beyond char pos ~w.~n~n',
		    [NegationTerm,CharPosInfo]).
charpos_to_tokenindex([Token|TokenList], NegationTerm, CharPosInfo, Start, TokenIndex) :-
	Token = tok(_,_,NString,AbsPosInfo,RelPosInfo),
	AbsPosInfo = pos(AbsTokenStart,_),
	RelPosInfo = pos(RelTokenStart,_),
	CharPosInfo = [CharStart/_|_],
	( AbsTokenStart >= CharStart ->
	  TokenIndex = Start
	; RelTokenStart >= CharStart ->
	  TokenIndex = Start
	; set_next(NString, Next, Start) ->
	  charpos_to_tokenindex(TokenList, NegationTerm, CharPosInfo, Next, TokenIndex)
	).

set_next(NString, Next, Start) :-
	( NString == " " ->
	  Next is Start
	; Next is Start + 1
	).

/* Consolidating negation terms:
   Consider the utterance "No abnormality". Generic NegEx will generate
   two negations from this utterance:

   negation(nega,
   	    no, [0/2],
	    ['C0000768':'ABNORMALITY'], [3/12])

   and

   negation(nega,
   	    no, [0/2],
	    ['C1704258':'Abnormality'], [3/12])

   However, there is really only one negation, but the negated concept
   has two mappings. It would be more appropriate to distribute
   the negation trigger over the multiple mappings of the negated concept,
   and have a single negation instead:

   negation(nega,
   	    no,                                                   [37/2],
            ['C0000768':'ABNORMALITY', 'C1704258':'Abnormality'], [81/12])

   This is the purpose of consolidate_negation_terms/2.
   We cannot assume that negations to be consolidated are consecutive
   in the incoming list of negation terms, so the second clause
   cycles through all the other negations (using the backtrackable select/3),
   and if one is found that can be combined, they are combined.

*/

consolidate_negation_terms([], []).
consolidate_negation_terms([H|T], ConsolidatedNegationTerms) :-
	convert_negation_template(H, ConvertedH),
	group_negation_terms(T, ConvertedH, ConsolidatedNegationTerms).

group_negation_terms([], LastNegationTerm, [ConvertedLastNegationTerm]) :-
	convert_negation_template(LastNegationTerm, ConvertedLastNegationTerm).
% FirstNegationTerm has already been converted to the new ConceptCUI list format
group_negation_terms([H|T], FirstNegationTerm, FinalNegationTerms) :-
	select(NegationTerm, [H|T], RestNegationTerms),
	negation_terms_overlap(NegationTerm, FirstNegationTerm),
	!,
	merge_negation_terms(NegationTerm, FirstNegationTerm, ConsolidatedNegationTerm),
	NextNegationTerm = ConsolidatedNegationTerm,
	FinalNegationTerms = RestFinalNegationTerms,
	group_negation_terms(RestNegationTerms, NextNegationTerm, RestFinalNegationTerms).
group_negation_terms([H|T], FirstNegationTerm, FinalNegationTerms) :-
	FinalNegationTerms = [FirstNegationTerm|RestFinalNegationTerms],
	convert_negation_template(H, NextNegationTerm),
	group_negation_terms(T, NextNegationTerm, RestFinalNegationTerms).


convert_negation_template(H, ConvertedH) :-
	( final_negation_template(H,
				  _Type, _TriggerText, _TriggerPosInfo,
				  _ConceptCUIList, _ConceptPosInfo) ->
	  ConvertedH = H
	; orig_negation_template(H,
				 Type, TriggerText, TriggerPosInfo,
				 ConceptName, CUI, ConceptPosInfo),
	  final_negation_template(ConvertedH,
				  Type, TriggerText, TriggerPosInfo,
				  ConceptCUIList, ConceptPosInfo),
	  ConceptCUIList = [CUI:ConceptName]
	).
	
negation_terms_overlap(NegationTerm1, NegationTerm2) :-
	orig_negation_template(NegationTerm1,
			       Type1, TriggerText1, TriggerPosInfo1,
			       _ConceptName1, _CUI1, ConceptPosInfo1),
	final_negation_template(NegationTerm2,
				Type2, TriggerText2, TriggerPosInfo2,
				_ConceptCUIList2, ConceptPosInfo2),
	% Yes this could be done via unification in the head of the clause,
	% but this way the intent is made much clearer.
	Type1 == Type2,
	TriggerText1 == TriggerText2,
	TriggerPosInfo1 == TriggerPosInfo2,
	ConceptPosInfo1 == ConceptPosInfo2.	

merge_negation_terms(NegationTerm1, NegationTerm2, MergedNegationTerm) :-
	orig_negation_template(NegationTerm1,
			       Type, TriggerText, TriggerPosInfo,
			       ConceptName1, CUI1, ConceptPosInfo),
	final_negation_template(NegationTerm2,
				Type, TriggerText, TriggerPosInfo,
				ConceptCUIList2, ConceptPosInfo),
	final_negation_template(MergedNegationTerm,
				Type, TriggerText, TriggerPosInfo,
				[CUI1:ConceptName1|ConceptCUIList2], ConceptPosInfo).

% debug predicate
% print_negterms_distance([], _).
% print_negterms_distance([NegationTerm|NegationTermList], TokenList) :-
% 	negterm_concept_distance(TokenList, NegationTerm, Distance),
% 	format('~q, distance = ~q~n', [NegationTerm,Distance]),
% 	print_negterms_distance(NegationTermList, TokenList).

%
% François Lang's tokenlist splitting predicate
%
split_token_list([], []).
split_token_list([FirstToken|RestTokens], SplitTokenList) :-
	FirstToken = tok(FirstTokenType,_,_,_,_),
	ignore_token_type(FirstTokenType),
	!,
	split_token_list(RestTokens, SplitTokenList).
split_token_list([FirstToken|RestTokens], [FirstUtteranceTokens|RestUtteranceTokens]) :-
	FirstToken = tok(sn,_,_,_,_),
	!,
	FirstUtteranceTokens = [FirstToken|RestFirstUtteranceTokens],
	get_utterance_tokens(RestTokens, RestFirstUtteranceTokens, RemainingTokens),
	split_token_list(RemainingTokens, RestUtteranceTokens).

get_utterance_tokens([], [], []).
get_utterance_tokens([CurrentToken|RestTokens], RestUtteranceTokens, RemainingTokens) :-
	CurrentToken = tok(CurrentTokenType,_,_,_,_),
	ignore_token_type(CurrentTokenType),
	!,
	get_utterance_tokens(RestTokens, RestUtteranceTokens, RemainingTokens).
get_utterance_tokens([CurrentToken|RestTokens], [CurrentToken|RestUtteranceTokens], RemainingTokens) :-
	CurrentToken = tok(CurrentTokenType,_,_,_,_),
	CurrentTokenType \== sn,
	!,
	get_utterance_tokens(RestTokens, RestUtteranceTokens, RemainingTokens).
get_utterance_tokens([CurrentToken|RestTokens], [], [CurrentToken|RestTokens]).

ignore_token_type(field).
ignore_token_type(label).

%
% List of negation phrases.
%

% In http://code.google.com/p/negex/wiki/NegExTerms,
% these are "Pre-condition negation terms (used to mark an indexed term as negated)".

nega_phrase_tokens(absence, [of]).
nega_phrase_tokens(cannot, []).
nega_phrase_tokens(cannot, [see]).
nega_phrase_tokens(checked, [for]).
nega_phrase_tokens(declined, []).
nega_phrase_tokens(declines, []).
nega_phrase_tokens(deny, []).
nega_phrase_tokens(denied, []).
nega_phrase_tokens(denies, []).
nega_phrase_tokens(denying, []).
nega_phrase_tokens(evaluate, [for]).
nega_phrase_tokens(fails, [to,reveal]).
nega_phrase_tokens(free, [of]).
nega_phrase_tokens(negative, [for]).
nega_phrase_tokens(never, [developed]).
nega_phrase_tokens(never, [had]).
nega_phrase_tokens(no, []).
nega_phrase_tokens(no, [abnormal]).
nega_phrase_tokens(no, [cause,of]).
nega_phrase_tokens(no, [complaints,of]).
nega_phrase_tokens(no, [evidence]).
nega_phrase_tokens(no, [new,evidence]).
nega_phrase_tokens(no, [other,evidence]).
nega_phrase_tokens(no, [evidence,to,suggest]).
nega_phrase_tokens(no, [findings,of]).
nega_phrase_tokens(no, [findings,to,indicate]).
nega_phrase_tokens(no, [mammographic,evidence,of]).
nega_phrase_tokens(no, [new]).
nega_phrase_tokens(no, [radiographic,evidence,of]).
nega_phrase_tokens(no, [sign,of]).
nega_phrase_tokens(no, [significant]).
nega_phrase_tokens(no, [signs,of]).
nega_phrase_tokens(no, [suggestion,of]).
nega_phrase_tokens(no, [suspicious]).
nega_phrase_tokens(not, []).
nega_phrase_tokens(not, [appear]).
nega_phrase_tokens(not, [appreciate]).
nega_phrase_tokens(not, [associated,with]).
nega_phrase_tokens(not, [complain,of]).
nega_phrase_tokens(not, [demonstrate]).
nega_phrase_tokens(not, [exhibit]).
nega_phrase_tokens(not, [feel]).
nega_phrase_tokens(not, [had]).
nega_phrase_tokens(not, [have]).
nega_phrase_tokens(not, [know,of]).
nega_phrase_tokens(not, [known,to,have]).
nega_phrase_tokens(not, [reveal]).
nega_phrase_tokens(not, [see]).
nega_phrase_tokens(not, [to,be]).
nega_phrase_tokens(patient, [was,not]).
nega_phrase_tokens(rather, [than]).
nega_phrase_tokens(resolved, []).
nega_phrase_tokens(test, [for]).
nega_phrase_tokens(to, [exclude]).
nega_phrase_tokens(unremarkable, [for]).
nega_phrase_tokens(with, [no]).
nega_phrase_tokens(without, []).
nega_phrase_tokens(without, [any,evidence,of]).
nega_phrase_tokens(without, [evidence]).
nega_phrase_tokens(without, [indication,of]).
nega_phrase_tokens(without, [sign,of]).
nega_phrase_tokens(rules, [out]).
nega_phrase_tokens(rules, [him,out]).
nega_phrase_tokens(rules, [her,out]).
nega_phrase_tokens(rules, [the,patient,out]).
nega_phrase_tokens(rules, [out,for]).
nega_phrase_tokens(rules, [him,out,for]).
nega_phrase_tokens(rules, [her,out,for]).
nega_phrase_tokens(rules, [the,patient,out,for]).
nega_phrase_tokens(ruled, [out]).
nega_phrase_tokens(ruled, [him,out]).
nega_phrase_tokens(ruled, [her,out]).
nega_phrase_tokens(ruled, [the,patient,out]).
nega_phrase_tokens(ruled, [out,for]).
nega_phrase_tokens(ruled, [him,out,for]).
nega_phrase_tokens(ruled, [her,out,for]).
nega_phrase_tokens(ruled, [the,patient,out,for]).
nega_phrase_tokens(ruled, [out,against]).
nega_phrase_tokens(ruled, [him,out,against]).
nega_phrase_tokens(ruled, [her,out,against]).
nega_phrase_tokens(ruled, [the,patient,out,against]).
nega_phrase_tokens(did, [rule,out]).
nega_phrase_tokens(did, [rule,out,for]).
nega_phrase_tokens(did, [rule,out,against]).
nega_phrase_tokens(did, [rule,him,out]).
nega_phrase_tokens(did, [rule,her,out]).
nega_phrase_tokens(did, [rule,the,patient,out]).
nega_phrase_tokens(did, [rule,him,out,for]).
nega_phrase_tokens(did, [rule,her,out,for]).
nega_phrase_tokens(did, [rule,him,out,against]).
nega_phrase_tokens(did, [rule,her,out,against]).
nega_phrase_tokens(did, [rule,the,patient,out,for]).
nega_phrase_tokens(did, [rule,the,patient,out,against]).
nega_phrase_tokens(can, [rule,out]).
nega_phrase_tokens(can, [rule,out,for]).
nega_phrase_tokens(can, [rule,out,against]).
nega_phrase_tokens(can, [rule,him,out]).
nega_phrase_tokens(can, [rule,her,out]).
nega_phrase_tokens(can, [rule,the,patient,out]).
nega_phrase_tokens(can, [rule,him,out,for]).
nega_phrase_tokens(can, [rule,her,out,for]).
nega_phrase_tokens(can, [rule,the,patient,out,for]).
nega_phrase_tokens(can, [rule,him,out,against]).
nega_phrase_tokens(can, [rule,her,out,against]).
nega_phrase_tokens(can, [rule,the,patient,out,against]).
nega_phrase_tokens(adequate, [to,rule,out]).
nega_phrase_tokens(adequate, [to,rule,him,out]).
nega_phrase_tokens(adequate, [to,rule,her,out]).
nega_phrase_tokens(adequate, [to,rule,the,patient,out]).
nega_phrase_tokens(adequate, [to,rule,out,for]).
nega_phrase_tokens(adequate, [to,rule,him,out,for]).
nega_phrase_tokens(adequate, [to,rule,her,out,for]).
nega_phrase_tokens(adequate, [to,rule,the,patient,out,for]).
nega_phrase_tokens(adequate, [to,rule,the,patient,out,against]).
nega_phrase_tokens(sufficient, [to,rule,out]).
nega_phrase_tokens(sufficient, [to,rule,him,out]).
nega_phrase_tokens(sufficient, [to,rule,her,out]).
nega_phrase_tokens(sufficient, [to,rule,the,patient,out]).
nega_phrase_tokens(sufficient, [to,rule,out,for]).
nega_phrase_tokens(sufficient, [to,rule,him,out,for]).
nega_phrase_tokens(sufficient, [to,rule,her,out,for]).
nega_phrase_tokens(sufficient, [to,rule,the,patient,out,for]).
nega_phrase_tokens(sufficient, [to,rule,out,against]).
nega_phrase_tokens(sufficient, [to,rule,him,out,against]).
nega_phrase_tokens(sufficient, [to,rule,her,out,against]).
nega_phrase_tokens(sufficient, [to,rule,the,patient,out,against]).
% The following nega_phrase_token was added by NLM
nega_phrase_tokens(with, [no,evidence,of]).

% In http://code.google.com/p/negex/wiki/NegExTerms,
% these are "Post-condition negation terms".

negb_phrase_tokens(unlikely, []).
negb_phrase_tokens(free, []).
negb_phrase_tokens(was, [ruled,out]).
negb_phrase_tokens(is, [ruled,out]).
negb_phrase_tokens(are, [ruled,out]).
negb_phrase_tokens(have, [been,ruled,out]).
negb_phrase_tokens(has, [been,ruled,out]).

% In http://code.google.com/p/negex/wiki/NegExTerms,
% these are "Pre-condition possibility phrase (used to mark an indexed term as possible)".

pnega_phrase_tokens(rule, [out]).
pnega_phrase_tokens('r/o', []).
pnega_phrase_tokens(ro, []).
pnega_phrase_tokens(rule, [him,out]).
pnega_phrase_tokens(rule, [her,out]).
pnega_phrase_tokens(rule, [the,patient,out]).
pnega_phrase_tokens(rule, [out,for]).
pnega_phrase_tokens(rule, [him,out,for]).
pnega_phrase_tokens(rule, [her,out,for]).
pnega_phrase_tokens(rule, [the,patient,out,for]).
pnega_phrase_tokens(be, [ruled,out,for]).
pnega_phrase_tokens(should, [be,ruled,out,for]).
pnega_phrase_tokens(ought, [to,be,ruled,out,for]).
pnega_phrase_tokens(may, [be,ruled,out,for]).
pnega_phrase_tokens(might, [be,ruled,out,for]).
pnega_phrase_tokens(could, [be,ruled,out,for]).
pnega_phrase_tokens(will, [be,ruled,out,for]).
pnega_phrase_tokens(can, [be,ruled,out,for]).
pnega_phrase_tokens(must, [be,ruled,out,for]).
pnega_phrase_tokens(is, [to,be,ruled,out,for]).
pnega_phrase_tokens(what, [must,be,ruled,out,is]).

% In http://code.google.com/p/negex/wiki/NegExTerms,
% these are "Post-condition possibility terms (used to mark an indexed term as possible)".

pnegb_phrase_tokens(did, [not,rule,out]).
pnegb_phrase_tokens(not, [ruled,out]).
pnegb_phrase_tokens(not, [been,ruled,out]).
pnegb_phrase_tokens(being, [ruled,out]).
pnegb_phrase_tokens(be, [ruled,out]).
pnegb_phrase_tokens(should, [be,ruled,out]).
pnegb_phrase_tokens(ought, [to,be,ruled,out]).
pnegb_phrase_tokens(may, [be,ruled,out]).
pnegb_phrase_tokens(might, [be,ruled,out]).
pnegb_phrase_tokens(could, [be,ruled,out]).
pnegb_phrase_tokens(will, [be,ruled,out]).
pnegb_phrase_tokens(can, [be,ruled,out]).
pnegb_phrase_tokens(must, [be,ruled,out]).
pnegb_phrase_tokens(is, [to,be,ruled,out]).

% In http://code.google.com/p/negex/wiki/NegExTerms,
% these are "Pseudo negation terms".

pseudoneg_phrase_tokens(no,	 [increase]).
pseudoneg_phrase_tokens(no,      [suspicious,change]).
pseudoneg_phrase_tokens(no,      [significant,change]).
pseudoneg_phrase_tokens(no,      [change]).
pseudoneg_phrase_tokens(no,      [interval,change]).
pseudoneg_phrase_tokens(no,      [definite,change]).
pseudoneg_phrase_tokens(no,      [significant,interval,change]).
pseudoneg_phrase_tokens(not,     [extend]).
pseudoneg_phrase_tokens(not,     [cause]).
pseudoneg_phrase_tokens(not,     [drain]).
pseudoneg_phrase_tokens(not,     [certain,if]).
pseudoneg_phrase_tokens(not,     [certain,whether]).
pseudoneg_phrase_tokens(gram,    [negative]).
pseudoneg_phrase_tokens(without, [difficulty]).
pseudoneg_phrase_tokens(not,     [necessarily]).
pseudoneg_phrase_tokens(not,     [only]).

% In http://code.google.com/p/negex/wiki/NegExTerms,
% these are "Termination terms".

conj_phrase_tokens(although, []).
conj_phrase_tokens(apart, [from]).
conj_phrase_tokens(as, [a,cause,for]).
conj_phrase_tokens(as, [a,cause,of]).
conj_phrase_tokens(as, [a,etiology,for]).
conj_phrase_tokens(as, [a,etiology,of]).
conj_phrase_tokens(as, [a,reason,for]).
conj_phrase_tokens(as, [a,reason,of]).
conj_phrase_tokens(as, [a,secondary,cause,for]).
conj_phrase_tokens(as, [a,secondary,cause,of]).
conj_phrase_tokens(as, [a,secondary,etiology,for]).
conj_phrase_tokens(as, [a,secondary,etiology,of]).
conj_phrase_tokens(as, [a,secondary,origin,for]).
conj_phrase_tokens(as, [a,secondary,origin,of]).
conj_phrase_tokens(as, [a,secondary,reason,for]).
conj_phrase_tokens(as, [a,secondary,reason,of]).
conj_phrase_tokens(as, [a,secondary,source,for]).
conj_phrase_tokens(as, [a,secondary,source,of]).
conj_phrase_tokens(as, [a,source,for]).
conj_phrase_tokens(as, [a,source,of]).
conj_phrase_tokens(as, [an,cause,for]).
conj_phrase_tokens(as, [an,cause,of]).
conj_phrase_tokens(as, [an,etiology,for]).
conj_phrase_tokens(as, [an,etiology,of]).
conj_phrase_tokens(as, [an,origin,for]).
conj_phrase_tokens(as, [an,origin,of]).
conj_phrase_tokens(as, [an,reason,for]).
conj_phrase_tokens(as, [an,reason,of]).
conj_phrase_tokens(as, [an,secondary,cause,for]).
conj_phrase_tokens(as, [an,secondary,cause,of]).
conj_phrase_tokens(as, [an,secondary,etiology,for]).
conj_phrase_tokens(as, [an,secondary,etiology,of]).
conj_phrase_tokens(as, [an,secondary,origin,for]).
conj_phrase_tokens(as, [an,secondary,origin,of]).
conj_phrase_tokens(as, [an,secondary,reason,for]).
conj_phrase_tokens(as, [an,secondary,reason,of]).
conj_phrase_tokens(as, [an,secondary,source,for]).
conj_phrase_tokens(as, [an,secondary,source,of]).
conj_phrase_tokens(as, [an,source,for]).
conj_phrase_tokens(as, [an,source,of]).
conj_phrase_tokens(as, [the,cause,for]).
conj_phrase_tokens(as, [the,cause,of]).
conj_phrase_tokens(as, [the,etiology,for]).
conj_phrase_tokens(as, [the,etiology,of]).
conj_phrase_tokens(as, [the,origin,for]).
conj_phrase_tokens(as, [the,origin,of]).
conj_phrase_tokens(as, [the,reason,for]).
conj_phrase_tokens(as, [the,reason,of]).
conj_phrase_tokens(as, [the,secondary,cause,for]).
conj_phrase_tokens(as, [the,secondary,cause,of]).
conj_phrase_tokens(as, [the,secondary,etiology,for]).
conj_phrase_tokens(as, [the,secondary,etiology,of]).
conj_phrase_tokens(as, [the,secondary,origin,for]).
conj_phrase_tokens(as, [the,secondary,origin,of]).
conj_phrase_tokens(as, [the,secondary,reason,for]).
conj_phrase_tokens(as, [the,secondary,reason,of]).
conj_phrase_tokens(as, [the,secondary,source,for]).
conj_phrase_tokens(as, [the,secondary,source,of]).
conj_phrase_tokens(as, [the,source,for]).
conj_phrase_tokens(as, [the,source,of]).
conj_phrase_tokens(aside, [from]).
conj_phrase_tokens(but, []).
conj_phrase_tokens(cause, [for]).
conj_phrase_tokens(cause, [of]).
conj_phrase_tokens(causes, [for]).
conj_phrase_tokens(causes, [of]).
conj_phrase_tokens(etiology, [for]).
conj_phrase_tokens(etiology, [of]).
conj_phrase_tokens(except, []).
conj_phrase_tokens(however, []).
conj_phrase_tokens(nevertheless, []).
conj_phrase_tokens(origin, [for]).
conj_phrase_tokens(origin, [of]).
conj_phrase_tokens(origins, [for]).
conj_phrase_tokens(origins, [of]).
conj_phrase_tokens(other, [possibilities,of]).
conj_phrase_tokens(reason, [for]).
conj_phrase_tokens(reason, [of]).
conj_phrase_tokens(reasons, [for]).
conj_phrase_tokens(reasons, [of]).
conj_phrase_tokens(secondary, [to]).
conj_phrase_tokens(source, [for]).
conj_phrase_tokens(source, [of]).
conj_phrase_tokens(sources, [for]).
conj_phrase_tokens(sources, [of]).
conj_phrase_tokens(still, []).
conj_phrase_tokens(though, []).
conj_phrase_tokens(trigger, [event,for]).
conj_phrase_tokens(yet, []).

% The following conj_phrase_tokens were added  by NLM
conj_phrase_tokens(other,     [than]).
conj_phrase_tokens(otherwise, []).
conj_phrase_tokens(then,      []).
conj_phrase_tokens(to,        [account,for]).
conj_phrase_tokens(to,        [explain]).

% Negated UMLS concepts must below to one of the following semantic types
% this is essentially a specially defined semantic group that is a super set of "Disorders"
% negex_semtype_list([acab,anab,biof,cgab,comd,dsyn,emod,fndg,
%		      inpo,lbtr,menp,mobd,neop,patf,phsf,sosy]).

% fin

% :- use_module(library(addportray)).
% portray_mm_output(mm_output(_ExpandedUtterance,_CitationTextAtom,_ModifiedText,_Tagging,_AAs,
% 			    _Syntax,_DisambiguatedMMOPhrases,_ExtractedPhrases)) :- write('MMO').
% :- add_portray(portray_mm_output).
