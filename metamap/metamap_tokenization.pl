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
*  https://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/

% File:	    metamap_tokenization.pl
% Module:   MetaMap
% Author:   Lan
% Purpose:  MetaMap tokenization routines


:- module(metamap_tokenization,[
	add_tokens_to_phrases/2,
	% must be exported for mwi_utilities
	ends_with_s/1,
	extract_tokens_with_tags/2,
	% must be exported for mm_print
	filter_tokens/3,
	% possessive handling
	% phrase predicates
	get_phrase_item_feature/3,
	get_phrase_item_name/2,
	get_phrase_item_subitems/2,
	get_subitems_feature/3,
	get_subitem_value/2,
	% must be exported for mwi_utilities
	is_punct_or_ws/1,
	% must be exported for mwi_utilities
	is_ws/1,
	% must be exported for mwi_utilities
	is_ws_word/1,
	linearize_phrase/4,
	linearize_components/2,
	listify/2,
	local_alnum/1,
	local_alpha/1,
	local_ascii/1,
	local_digit/1,
	local_lower/1,
	local_print/1,
	local_punct/1,
	local_upper/1,
	local_to_lower/2,
	local_to_upper/2,
	local_ws/1,
	new_phrase_item/3,
	no_combine_pronoun/1,
	normalize_possessives/3,
	parse_phrase_word_info/3,
	% whitespace tokenization (break at whitespace and hyphens; ignore colons)
	tokenize_text/2,
	% wordind tokenization (maximal alphanumeric sequences)
	tokenize_text_more/2,
	tokenize_text_more_lc/2,
	% MetaMap tokenization (wordind - possessives)
	tokenize_text_mm/2,
	tokenize_text_mm_lc/2,
	% "complete" tokenization (wordind + punctuation)
	% "utter" tokenization (wordind + punctuation + whitespace)
	tokenize_fields_utterly/2,
	tokenize_text_utterly/2,
	set_subitems_feature/4,
	transparent_tag/1
    ]).

:- use_module(lexicon(qp_lookup),[
	punct_token/2
    ]).

% :- use_module(skr_lib(ctypes),[
% 	ctypes_bits/2
%     ]).

% :- use_module(skr_lib(mincoman), [%
%	punc_mark1/1
%    ]).

:- use_module(skr_lib(nls_lists),[
	first_n_or_less/3
    ]).

:- use_module(skr_lib(nls_strings),[
	atom_codes_list/2
    ]).

:- use_module(skr_lib(nls_text),[
	is_all_graphic_text/1
    ]).

:- use_module(skr_lib(sicstus_utils),[
	concat_strings_with_separator/3,
	lowercase_list/2,
	ttyflush/0
    ]).

:- use_module(skr(skr_utilities),[
	fatal_error/2,
	send_message/2
    ]).

:- use_module(text(text_object_util),[
	hyphen_punc_char/1
    ]).

:- use_module(library(lists),[
	append/2,
	last/2,
	rev/2
    ]).

/* ************************************************************************
   ************************************************************************
   ************************************************************************
                          MetaMap Tokenizing Predicates
   ************************************************************************
   ************************************************************************
   ************************************************************************ */

/*   add_tokens_to_phrases(+PhrasesIn, -PhrasesOut)
     add_tokens_to_phrase(+PhraseIn, -PhraseOut)

The tokens added by add_tokens_to_phrase/2 are normally obtained by
tokenizing the lexmatch/1 subitem (if any).  For shapes/2 and punc/1 items,
the newly created inputmatch/1 subitem is tokenized.  For example, text
"Fifty-six vena caval stent filters." produces the phrase

     [shapes([Fifty,-,six],[word_numeral]),
      mod([lexmatch(vena caval),inputmatch([vena,caval])]),
      mod([lexmatch(stent),inputmatch([stent])]),
      head([lexmatch(filters),inputmatch([filters])]),
      punc(.)]

which preprocesses to

     [shapes([inputmatch([Fifty,-,six]),features([word_numeral]),
              tokens([fifty,six])]),
      mod([lexmatch([vena caval]),inputmatch([vena,caval]),
           tokens([vena,caval])]),
      mod([lexmatch([stent]),inputmatch([stent]),tokens([stent])]),
      head([lexmatch([filters]),inputmatch([filters]),tokens([filters])]),
      punc([inputmatch([.]),tokens([])])]

*/

add_tokens_to_phrases([], []).
add_tokens_to_phrases([First|Rest], [ModifiedFirst|ModifiedRest]) :-
	add_tokens_to_one_phrase(First, ModifiedFirst),
	add_tokens_to_phrases(Rest, ModifiedRest).

add_tokens_to_one_phrase([], []).
add_tokens_to_one_phrase([First|Rest], [TokenizedFirst|TokenizedRest]) :-
	% add_lc_inputmatch_tokens_to_phrase_item(First, TokenizedFirst),
	add_tokens_to_phrase_item(First, TokenizedFirst),
	add_tokens_to_one_phrase(Rest, TokenizedRest).

add_lc_inputmatch_tokens_to_phrase_item(Item, ItemWithTokens) :-
	get_phrase_item_subitems(Item, SubItems),
	get_subitems_feature(SubItems, inputmatch, TextList),
	lowercase_list(TextList, TextListLC),
	set_phrase_item_feature(Item, tokens, TextListLC, ItemWithTokens).
	
% Existing version
% add_tokens_to_phrase_item(Item, TokenizedItem) :-
%         get_phrase_item_subitems(Item, SubItems),
%         ( get_non_null_lexmatch(SubItems, LexMatch) ->
%           make_list(LexMatch, TextList)
%         ; get_subitems_feature(SubItems,inputmatch,TextList)
%         ),
%         tokenize_all_text_mm_lc(TextList, TokenLists),
%         append(TokenLists, Tokens),
%         set_phrase_item_feature(Item, tokens, Tokens, TokenizedItem),
%         !.
% add_tokens_to_phrase_item(Item, Item).

% New version
add_tokens_to_phrase_item(Item, TokenizedItem) :-
	get_phrase_item_subitems(Item, SubItems),
	get_subitems_feature(SubItems, inputmatch, InputMatch),
	lowercase_list(InputMatch, InputMatchLC),
	remove_punct_tokens(InputMatchLC, InputMatchLCNoPunct),
 	undo_possessives(InputMatchLCNoPunct, InputMatchLCNoPunctNoPoss),
	% InputMatchLCNoPunctNoPoss = InputMatchLCNoPunct,
	set_phrase_item_feature(Item, tokens, InputMatchLCNoPunctNoPoss, TokenizedItem),
	!.
add_tokens_to_phrase_item(Item, Item).


remove_punct_tokens([], []).
remove_punct_tokens([H|T], InputMatchLCNoPunct) :-
	( punct_token(H, _) ->
	  InputMatchLCNoPunct = InputMatchLCNoPunctRest
	; InputMatchLCNoPunct = [H|InputMatchLCNoPunctRest]
	),
	remove_punct_tokens(T, InputMatchLCNoPunctRest).

undo_possessives([], []).
undo_possessives([H|T], [HNoPoss|TNoPoss]) :-
	undo_one_possessive(H, HNoPoss),
	undo_possessives(T, TNoPoss).

undo_one_possessive(Token, TokenNoPoss) :-
	atom_codes(Token, TokenCodes),
	( append(TokenNoPossCodes, [0''', 0's], TokenCodes) ->
	  atom_codes(TokenNoPoss, TokenNoPossCodes)
	; TokenNoPoss = Token
	).

% get_non_null_lexmatch(SubItems, LexMatch) :-
% 	get_subitems_feature(SubItems, lexmatch, LexMatch),
% 	LexMatch \== [].

% make_list(LexMatch, TextList) :-
% 	( atom(LexMatch) ->
% 	  TextList = [LexMatch]
% 	; TextList = LexMatch
% 	).

/* 
   parse_phrase_word_info(+Phrase, +Option, -PhraseWordInfoPair)
   parse_phrase_word_info_aux(+Phrase, +FilterFlag, +WordsBegin
                          +PhraseWordsIn, -PhraseWordsOut,
                          +PhraseHeadWordsIn, -PhraseWordsOut,
                          +PhraseMapIn, -PhraseMapOut)

parse_phrase_word_info/3 extracts PhraseWordInfoPair from Phrase where
PhraseWordInfoPair is PhraseWordInfo:FilteredPhraseWordInfo and each element
of the pair is of the form
     pwi(PhraseWordL,PhraseHeadWordL,PhraseMap).
If Option is nofilter, then FilteredPhraseWordInfo is the same as PhraseWordInfo.
parse_phrase_word_info/2 calls parse_phrase_word_info/3 with Option filter.
Filtering consists of removing syntactic items with "opaque" tags
such as prepositions and determiners. See *input_match* family description for full list.
parse_phrase_word_info/9 is the auxiliary which does the work.  */

parse_phrase_word_info(Phrase, FilterChoice,
                       pwi(PhraseWordL,PhraseHeadWordL,PhraseMap)
                      :pwi(FilteredPhraseWordL,FilteredPhraseHeadWordL,FilteredPhraseMap)) :-
	parse_phrase_word_info_aux(Phrase, unfiltered, 1, [], PhraseWords,
				   [], PhraseHeadWords, [], PhraseMap0),
	create_word_list(PhraseWords, PhraseWordL),
	create_word_list(PhraseHeadWords, PhraseHeadWordL),
	rev(PhraseMap0, PhraseMap),
	( FilterChoice == filtered ->
	  parse_phrase_word_info_aux(Phrase, filtered, 1, [], FilteredPhraseWords,
				     [], FilteredPhraseHeadWords, [], FilteredPhraseMap0),
	  create_word_list(FilteredPhraseWords, FilteredPhraseWordL),
	  create_word_list(FilteredPhraseHeadWords, FilteredPhraseHeadWordL),
	  rev(FilteredPhraseMap0, FilteredPhraseMap)
	; FilterChoice == unfiltered ->
	  FilteredPhraseWordL = PhraseWordL,
	  FilteredPhraseHeadWordL = PhraseHeadWordL,
	  FilteredPhraseMap = PhraseMap
	).

parse_phrase_word_info_aux([], _FilterFlag, _WordsBegin,
			   PhraseWordsIn, PhraseWordsIn,
			   PhraseHeadWordsIn, PhraseHeadWordsIn,
			   PhraseMapIn, PhraseMapIn).
parse_phrase_word_info_aux([PhraseItem|Rest], FilterFlag, WordsBegin,
			   PhraseWordsIn, PhraseWordsOut,
			   PhraseHeadWordsIn, PhraseHeadWordsOut,
			   PhraseMapIn, PhraseMapOut) :-
	( FilterFlag == unfiltered ->
          % gets Inputches now
	  extract_tokens(PhraseItem, IMWords, IMHeadWords)
	  % filter_tokens will exclude stop words,
	  % which is desirable, except not for the first word,
	  % so that we can identify concepts whose first word is a stop word,
	  % e.g., "in breathing" and "the uppermost part of the stomach", etc.
	  % ; WordsBegin =:= 1 ->
	  %   extract_tokens(PhraseItem, IMWords, IMHeadWords)
	; FilterFlag == filtered ->
	  filter_tokens(PhraseItem, IMWords, IMHeadWords)
	),
	!,
	( IMWords == [] ->
	  PhraseWordsInOut = PhraseWordsIn,
	  PhraseHeadWordsInOut = PhraseHeadWordsIn,
	  PhraseMapInOut = [[0,-1]|PhraseMapIn],
	  NewWordsBegin = WordsBegin
	; append(PhraseWordsIn, IMWords, PhraseWordsInOut),
	  append(PhraseHeadWordsIn, IMHeadWords, PhraseHeadWordsInOut),
	  length(IMWords, NIMWords),
	  NewWordsBegin is WordsBegin + NIMWords,
	  WordsEnd is NewWordsBegin - 1,
	  PhraseMapInOut = [[WordsBegin,WordsEnd]|PhraseMapIn]
	),
	parse_phrase_word_info_aux(Rest, FilterFlag, NewWordsBegin,
				   PhraseWordsInOut, PhraseWordsOut,
				   PhraseHeadWordsInOut, PhraseHeadWordsOut,
				   PhraseMapInOut, PhraseMapOut).
parse_phrase_word_info_aux([_|Rest], FilterFlag, WordsBegin,
			   PhraseWordsIn, PhraseWordsOut,
			   PhraseHeadWordsIn, PhraseHeadWordsOut,
			   PhraseMapIn, PhraseMapOut) :-
	parse_phrase_word_info_aux(Rest, FilterFlag, WordsBegin,
				   PhraseWordsIn, PhraseWordsOut,
				   PhraseHeadWordsIn, PhraseHeadWordsOut,
				   [[0,-1]|PhraseMapIn], PhraseMapOut).


/* create_word_list(+Words, -WordL)

create_word_list/2 forms wdl(Words,LCWords).  */

create_word_list(Words,wdl(Words,LCWords)) :-
    lowercase_list(Words,LCWords).

/* transparent_tag(?Tag)

transparent_tag/1 is a factual predicate of phrase tags which are essentially
ignored in determining the input match words for a phrase, i.e., processing
continues with the argument of the tag.  The only effect is that in
extract_input_match/3, IMHeadWords is always []. */

% Tagger tags
transparent_tag(adj).
transparent_tag(adv).
transparent_tag(noun).
transparent_tag(prefix).
transparent_tag(verb).
% Parser tags
transparent_tag(mod).
transparent_tag(pre).
transparent_tag(shapes).
transparent_tag(prefix).
transparent_tag(not_in_lex).  % Let unknowns through
transparent_tag(no_tag).      % Let unknowns through
transparent_tag(ing).         % Obsolete(?)
transparent_tag(pastpart).    % Obsolete(?)
% The "unknown" tag is added by get_this_variant/4 in generate_variant_info
% if no variants are found for a particular token. This is a hack.
transparent_tag(unknown).

/* opaque_tag(?Tag)

opaque_tag/1 is a factual predicate of phrase tags which prevent further
search for matching input.  */

opaque_tag(Tag) :- \+ transparent_tag(Tag).

/* extract_tokens(+PhraseItem, -TokenWords, -TokenHeadWords)
   extract_tokens_aux(+SubItemList, -TokenWords)
   extract_tokens_with_tags(+PhraseItemList, -IMsTags)
   filter_tokens(+PhraseItem, -TokenWords, -TokenHeadWords)
   filter_tokens_1(+PhraseItem, -TokenWords, -TokenHeadWords)
   filter_tokens_2(+PhraseItem, -TokenWords, -TokenHeadWords)

The *_tokens* family extracts tokens/1 information from
syntactic structures:

     PhraseItem ::= <tag>(<SubItemList>) | <primitivetag>(<atom>)

     SubItemList ::= <list> of <SubItem>

     SubItem ::= <unary term>  (e.g., tokens(_))

extract_tokens/3 finds the tokens/1 term, TokenWords, within PhraseItem.
If PhraseItem is a head, then TokenHeadWords=TokenWords.
extract_tokens_aux/2 finds the argument of the tokens/1 term in
SubItemList.
extract_tokens_with_tags/2 extracts the tokens and tag elements
from syntax.
filter_tokens/3 is similar to extract_tokens/3 except that only
"significant" tokens are reported where everything except prepositions,
determiners, conjunctions, punctuation and some modifiers (e.g., "-") is
significant.  filter_tokens/3 uses two filtering predicates,
filter_tokens_1/3 and filter_tokens_2/2.  */

remove_punc_marks([], []).
remove_punc_marks([H|T], Removed) :-
	( atom_codes(H, [C]),
	  local_punct(C) ->
	  Removed = RestRemoved
	; Removed = [H|RestRemoved]
	),
	remove_punc_marks(T, RestRemoved).

extract_tokens(SyntaxElement, TokenWords, TokenHeadWords) :-
	extract_tokens_1(SyntaxElement, TokenWords0, TokenHeadWords0),
	remove_punc_marks(TokenWords0, TokenWords),
	remove_punc_marks(TokenHeadWords0, TokenHeadWords).	
	

extract_tokens_1(head(SubItemList), Words, Words) :-
	!,
% 	extract_tokens_aux(SubItemList, TokenWords),
	extract_inputmatches_aux(SubItemList, Words).
%      	append(TokenWords, InputMatchWords, Words).
% try this!
extract_tokens_1(shapes(ShapesList,_FeatList), ShapesList, []) :- !.
extract_tokens_1(PhraseItem, TokenWords, []) :-
	functor(PhraseItem, _Tag, 1),
	arg(1,PhraseItem, SubItemList),
	!,
	extract_inputmatches_aux(SubItemList, TokenWords).
extract_tokens_1(PhraseItem, [], []) :-
	fatal_error('extract_tokens/3 failed for ~p.~n', [PhraseItem]).

extract_tokens_aux(PhraseItems, TokenWords) :-
	extract_inputmatches_aux(PhraseItems, TokenWords).

% for "primitive" tags
% extract_tokens_aux(PhraseItems, TokenWords) :-
%	( atom(PhraseItems) ->
%	  TokenWords = [PhraseItems],
%	  send_message('PRIMITIVE: ~q~n', [PhraseItems])
%	; % PhraseItems = [H|T],
%	  memberchk(tokens(TokenWords), PhraseItems)
%	).

extract_inputmatches_aux(PhraseItems, InputMatchWords) :-
	( atom(PhraseItems) ->
	  InputMatchWords = [PhraseItems],
	  send_message('PRIMITIVE: ~q~n', [PhraseItems])
	; % PhraseItems = [H|T],
	  memberchk(inputmatch(InputMatchWords), PhraseItems)
	).

% For each syntactic element in arg 1 with non-null tokens(_) and tag(_), e.g.,
% [head([lexmatch(['heart attack']),inputmatch([heart,attack]),tag(noun),tokens([heart,attack])])]
% create a structure of the form tokenstag(Tokens,Tag).
% In this case, tokenstag([heart,attack], noun)
extract_tokens_with_tags([], []).
extract_tokens_with_tags([PhraseItem|RestPhraseItems], Result) :-
	arg(1, PhraseItem, SubItemList),
	extract_tokens_aux(SubItemList, Tokens),
	extract_tag(SubItemList, Tag),
	!,
	( Tokens == [] ->
	  Result = RestTokensTags
	; Tag == none ->
	  Result = RestTokensTags
	; Result = [tokenstag(Tokens,Tag)|RestTokensTags]
	),
	extract_tokens_with_tags(RestPhraseItems, RestTokensTags).
extract_tokens_with_tags([_|RestPhraseItems], Result) :-
	extract_tokens_with_tags(RestPhraseItems, Result).


extract_tag(SubItemList, Tag) :-
	( memberchk(tag(Tag), SubItemList) ->
	  true
	; Tag = none
	).

% extract_tag([], none).
% extract_tag([tag(Tag)|_], Tag) :-
% 	!.
% extract_tag([_|Rest], Tag) :-
% 	extract_tag(Rest,Tag).
 
filter_tokens(PhraseItem, TokenWords, TokenHeadWords) :-
	filter_tokens_1(PhraseItem, TokenWords0, TokenHeadWords0),
	filter_tokens_2(TokenWords0, TokenWords),
	filter_tokens_2(TokenHeadWords0, TokenHeadWords).

filter_tokens_1(head(SubItemList), TokenWords,TokenWords) :-
	!,
	extract_tokens_aux(SubItemList, TokenWords).
filter_tokens_1(PhraseItem, TokenWords, []) :-
	functor(PhraseItem, Tag, 1),
	( opaque_tag(Tag) ->        % stop
	  TokenWords = []
	; transparent_tag(Tag) ->   % continue
	  arg(1, PhraseItem, SubItemList),
	  extract_tokens_aux(SubItemList, TokenWords)
	; arg(1, PhraseItem, SubItemList),
	  extract_tokens_aux(SubItemList, TokenWords),
	  send_message('### WARNING: ~q is an unexpected phrase item!~n', [PhraseItem]),
	  ttyflush
	),
	!.
filter_tokens_1(PhraseItem, [], []) :-
	fatal_error('filter_tokens_1/3 failed for ~p.~n', [PhraseItem]).

filter_tokens_2([], []).
filter_tokens_2([First|Rest], [First|FilteredRest]) :-
	\+ is_all_graphic_text(First),
	!,
	filter_tokens_2(Rest, FilteredRest).
filter_tokens_2([_First|Rest], FilteredRest) :-
	filter_tokens_2(Rest, FilteredRest).

/* linearize_phrase(+Phrase, +PhraseMap, -LPhrase, -LPhraseMap)
   linearize_phrase_item(+MapComponent, +PhraseItem, -LItems, -LMap)
   linearize_phrase_item_aux(+PhraseItem, +LMapComponent, +InputMatches,
                             -LItems, -LMap)
   linearize_components(+Components, -LComponents)
   linearize_component(+Component, -LComponent)
   linearize_component(+Begin, +End, -LComponent)

linearize_phrase/4 chops up Phrase (and PhraseMap) into LPhrase (and LPhraseMap)
so that each element of the map is a singleton.  Chopping is performed on both
the map itself as well as on the inputmatch/1 and tokens/1 terms of the phrase
(but not the lexmatch/1 terms).

For example, given text "Fifty-six vena caval stent filters.",

     Phrase: [shapes([inputmatch(['Fifty',-,six]),
		      features([word_numeral]),
		      tokens([fifty,six])]),
	      mod([lexmatch(['vena caval']),
		   inputmatch([vena,caval]),
		   tag(adj),
		   tokens([vena,caval])]),
	      mod([lexmatch([stent]),
		   inputmatch([stent]),
		   tag(noun),
		   tokens([stent])]),
	      head([lexmatch([filters]),
		    inputmatch([filters]),
		    tag(noun),
		    tokens([filters])]),
	      punc([inputmatch(['.']),
		    tokens([])])]

     PhraseMap: [[1,2],[3,4],[5,5],[6,6],[0,-1]]
     is linearized to

     LPhrase: [shapes([inputmatch(['Fifty',-]),
		      features([word_numeral]),
		      tokens([fifty])]),
	      shapes([inputmatch([six]),
		      features([word_numeral]),
		      tokens([six])]),
	      mod([lexmatch(['vena caval']),
		   inputmatch([vena]),
		   tag(adj),
		   tokens([vena])]),
	      mod([lexmatch(['vena caval']),
		   inputmatch([caval]),
		   tag(adj),
		   tokens([caval])]),
	      mod([lexmatch([stent]),
		   inputmatch([stent]),
		   tag(noun),
		   tokens([stent])]),
	      head([lexmatch([filters]),
		    inputmatch([filters]),
		    tag(noun),
		    tokens([filters])]),
	      punc([inputmatch(['.']),
		    tokens([])])]

     LPhraseMap: [[1],[2],[3],[4],[5],[6],[0]]

linearize_phrase_item/4
linearize_phrase_item/5
linearize_components/2
linearize_component/2
linearize_component/3
*/

linearize_phrase([], [], [], []).
linearize_phrase([FirstItem|RestItems],  [[Begin,End]|RestMap],
                 [FirstItem|RestLItems], [[Begin]|RestLMap]) :-
	( Begin =:= 0
	; Begin =:= End),
	!,
	linearize_phrase(RestItems, RestMap, RestLItems, RestLMap).
linearize_phrase([FirstItem|RestItems], [FirstMap|RestMap], LItems, LMap) :-
	linearize_phrase_item(FirstMap, FirstItem, FirstLItems, FirstLMap),
	!,
	%temp
	%format('~nlp FirstItem~n~p~n',[FirstItem]),
	%format('lp FirstMap~n~p~n',[FirstMap]),
	%format('lp FirstLItems~n~p~n',[FirstLItems]),
	%format('lp FirstLMap~n~p~n',[FirstLMap]),
	append(FirstLItems, RestLItems, LItems),
	append(FirstLMap, RestLMap, LMap),
	linearize_phrase(RestItems, RestMap, RestLItems, RestLMap).

linearize_phrase_item(MapComponent, PhraseItem, LItems, LMap) :-
	get_phrase_item_feature(PhraseItem, tokens, Tokens),
	get_phrase_item_feature(PhraseItem, inputmatch, IM0),
	coordinate_tokens(Tokens, IM0, IM),
	linearize_component(MapComponent, LMapComponent),
	get_phrase_item_feature(PhraseItem, bases, Bases0),
	( Bases0 == [] ->
	  linearize_phrase_item_6(LMapComponent, PhraseItem, Tokens, IM, LItems, LMap)
	; coordinate_tokens(Tokens, Bases0, Bases),
	  linearize_phrase_item_7(LMapComponent, PhraseItem, Tokens, IM, Bases, LItems, LMap)
	).

linearize_phrase_item_6([], _PhraseItem, [], [], [], []) :- !.
linearize_phrase_item_6([FirstMap|RestMap], PhraseItem, [FirstToken|RestTokens],
			[FirstIM|RestIM], [FirstLItem|RestLItems], [[FirstMap]|RestLMap]) :-
	set_phrase_item_feature(PhraseItem, tokens, [FirstToken], FirstLItem0),
	set_phrase_item_feature(FirstLItem0, inputmatch, FirstIM, FirstLItem1),
	( RestMap == [] ->
	  FirstLItem = FirstLItem1
	; demote_phrase_item(FirstLItem1, FirstLItem)
	),
	!,
	linearize_phrase_item_6(RestMap, PhraseItem, RestTokens, RestIM, RestLItems, RestLMap).
linearize_phrase_item_6(_, PhraseItem, _, _, _, _) :-
	fatal_error('Cannot linearize ~p (mapping/input match)~n', [PhraseItem]).

linearize_phrase_item_7([], _PhraseItem, [], [], [], [], []) :- !.
linearize_phrase_item_7([FirstMap|RestMap], PhraseItem, [FirstToken|RestTokens],
			[FirstIM|RestIM], [FirstBase|RestBases],
			[FirstLItem|RestLItems], [[FirstMap]|RestLMap]) :-
	set_phrase_item_feature(PhraseItem,  tokens,     [FirstToken], FirstLItem0),
	set_phrase_item_feature(FirstLItem0, inputmatch, FirstIM,      FirstLItem1),
	set_phrase_item_feature(FirstLItem1, bases,      FirstBase,    FirstLItem2),
	( RestMap == [] ->
	  FirstLItem = FirstLItem2
	; demote_phrase_item(FirstLItem2,FirstLItem)
	),
	!,
	linearize_phrase_item_7(RestMap, PhraseItem, RestTokens, RestIM, RestBases, RestLItems, RestLMap).
linearize_phrase_item_7(_, PhraseItem, _, _, _, _, _) :-
	fatal_error('Cannot linearize ~p (mapping/input match)~n', [PhraseItem]).

coordinate_tokens(Tokens, IM0, [FirstIM|RestIM]) :-
	length(Tokens, NTok),
	length(IM0, NIM),
	NFirst is NIM - NTok + 1,
	first_n_or_less(IM0, NFirst, FirstIM),
	append(FirstIM, IM1, IM0),
	listify(IM1, RestIM).

listify([], []).
listify([First|Rest], [[First]|ModifiedRest]) :-
	listify(Rest, ModifiedRest).

linearize_components([], []).
linearize_components([First|Rest], [LFirst|LRest]) :-
	linearize_component(First, LFirst),
	linearize_components(Rest, LRest).

linearize_component([Begin,Begin], [Begin]) :- !.
linearize_component([Begin,End], [0]) :-
	Begin > End,
	!.
linearize_component([Begin,End], LComponent) :-
	linearize_component_aux(Begin, End, LComponent).

linearize_component_aux(End, End, [End]) :- !.
linearize_component_aux(Begin, End, [Begin|LComponent]) :-
	NewBegin is Begin + 1,
	linearize_component_aux(NewBegin, End, LComponent).

/* demote_phrase_item(+PhraseItem, -DemotedPhraseItem)

demote_phrase_item/2 transforms head/1 phrase items into mod/1 phrase items
leaving all other phrase items alone.
*/

demote_phrase_item(head(Subitems), mod(Subitems)) :- !.
demote_phrase_item(Item, Item).

/* 
   tokenize_text(+Text, -TokText)

tokenize_text/2 transforms Text (atom/string) into a list of tokens TokText
breaking at break characters (see break_character/1) including spaces
and hyphens and ignoring some characters (see ignore_character/1) such
as colons.
*/

tokenize_text(Text,TokText) :-
    (   atom(Text) ->
        atom_codes(Text,String),
        phrase(tt_string(TokString),String),
        atom_codes_list(TokText,TokString)
    ;   phrase(tt_string(TokText),Text)
    ),
    !.

/*  TOKENIZE TEXT GRAMMAR  */

tt_string(TS) --> tt_token(T), {T\==[]}, !, tt_string(S), {TS=[T|S]}

              ;   [_Char], !, tt_string(S), {TS=S}

              ;   {TS=[]}.

tt_token(T) --> [Char], {ignore_character(Char)}, !, tt_token(S), {T=S}

            ;   [Char], {\+break_character(Char)}, !, tt_token(S), {T=[Char|S]}

            ;   {T=[]}.


/* break_character(?Char)

break_character/1 is a factual predicate of token break characters. */

break_character(0' ).
break_character(0'-).


/* ignore_character(?Char)

ignore_character/1 is a factual predicate of token characters which are
ignored. */

ignore_character(0':).

tokenize_text_more(Text,TokText) :-
    (   atom(Text) ->
        atom_codes(Text,String),
        phrase(ttm_string(TokString),String),
        atom_codes_list(TokText,TokString)
    ;   phrase(ttm_string(TokText),Text)
    ),
    !.

tokenize_text_more_lc(Text,TokText) :-
    tokenize_text_more(Text,TokText0),
    lowercase_list(TokText0,TokText).



/*  TOKENIZE TEXT MORE GRAMMAR  */

ttm_string(TS) --> ttm_token(T), {T\==[]}, !, ttm_string(S), {TS=[T|S]}

               ;   [_Char], !, ttm_string(S), {TS=S}

               ;   {TS=[]}.

ttm_token(T) --> [Char], {local_alnum(Char)}, !, ttm_token(S), {T=[Char|S]}

             ;   {T=[]}.


/* 
   tokenize_text_mm(+Text, -TokText)
   tokenize_all_text_mm_lc(+TextList, -TokTextList)
   tokenize_text_mm_lc(+Text, -TokText)

tokenize_text_mm/2 transforms Text (atom/string) into a list of tokens TokText
similar to the wordind regime used in tokenize_text_more/2. The difference
is that tokenize_text_mm/2 respects possessives ("'s" at the end of a word).
tokenize_text_mm_lc/2 lowercases the result of tokenize_text_mm/2.
tokenize_all_.../2 tokenizes a list of text into a SINGLE list of tokens. */

tokenize_text_mm(Text, TokText) :-
	( atom(Text) ->
	  atom_codes(Text, String),
	  tokenize_text_utterly(String, StringToks0),
	  % normalize_possessives_and_remove_nonwords(StringToks0, StringToks),
	  remove_possessives_and_nonwords(StringToks0, StringToks),
	  atom_codes_list(TokText, StringToks)
	; tokenize_text_utterly(Text, TokText0),
	  remove_possessives_and_nonwords(TokText0, TokText)
	).


/* remove_possessives_and_nonwords(+UTokensIn, -UTokensOut)

remove_possessives_and_nonwords/2 filters out possessives and nonwords
from the results of tokenize_text_utterly/2. */

remove_possessives_and_nonwords([], []).
remove_possessives_and_nonwords([NonWord|Rest], FilteredRest) :-
	% \+ is_ws_word(NonWord),
	is_punct_or_ws(NonWord),
	!,
	remove_possessives_and_nonwords(Rest, FilteredRest).
% singular possessives
remove_possessives_and_nonwords([Word,"'","s"], [Word]) :- !.
remove_possessives_and_nonwords([Word,"'","s",WhiteSpace|Rest], [Word|FilteredRest]) :-
	is_ws(WhiteSpace),
	!,
	remove_possessives_and_nonwords(Rest, FilteredRest).
% plural possessives
remove_possessives_and_nonwords([Word,"'"], [Word]) :-
	ends_with_s(Word),
	!.
remove_possessives_and_nonwords([Word,"'",WhiteSpace|Rest], [Word|FilteredRest]) :-
	ends_with_s(Word),
	is_ws(WhiteSpace),
	!,
	remove_possessives_and_nonwords(Rest, FilteredRest).
remove_possessives_and_nonwords([First|Rest], [First|FilteredRest]) :-
	remove_possessives_and_nonwords(Rest, FilteredRest).


%%% normalize_possessives_and_remove_nonwords(TokensIn, TokensOut) :-
%%% 	KeepWhiteSpace = 0,
%%% 	normalize_possessives(TokensIn, KeepWhiteSpace, TokensInOut),
%%% 	remove_nonwords(TokensInOut, TokensOut).
%%% 
%%% remove_nonwords([], []).
%%% remove_nonwords([NonWord|Rest], FilteredRest) :-
%%% 	% \+ is_ws_word(NonWord),
%%% 	is_punct_or_ws(NonWord),
%%% 	!,
%%% 	remove_nonwords(Rest, FilteredRest).
%%% remove_nonwords([Word|Rest], [Word|FilteredRest]) :-
%%% 	remove_nonwords(Rest, FilteredRest).
%%% 
% singular possessives
%%% % remove_possessives_and_nonwords([Word,"'","s"], [Word]) :- !.
%%% normalize_possessives([], _KeepWhiteSpace, []).
%%% normalize_possessives([PossessiveWord], _KeepWhiteSpace, [Word]) :-
%%% 	% append(Word, "'s", WordApostropheS),
%%% 	append(Word, [C1,C2], PossessiveWord),
%%% 	atom_codes(Atom, [C1,C2]),
%%% 	apostrophe_s_or_s_apostrophe(Atom),
%%% 	!.
%%% % normalize_possessives([Word,"'","s",WhiteSpace|Rest], [Word|FilteredRest]) :-
%%% % normalize_possessives([WordApostropheS,WhiteSpace|Rest], KeepWhiteSpace, [Word|FilteredRest]) :-
%%% normalize_possessives([WordApostropheS,WhiteSpace|Rest], KeepWhiteSpace, NormalizedTokens) :-
%%% 
%%% 	% append(Word, "'s", WordApostropheS),
%%% 	append(Word, [C1,C2], WordApostropheS),
%%% 	atom_codes(Atom, [C1,C2]),
%%% 	apostrophe_s_or_s_apostrophe(Atom),
%%% 	is_ws(WhiteSpace),
%%% 	!,
%%% 	( KeepWhiteSpace =:= 1 ->
%%% 	  NormalizedTokens = [Word,WhiteSpace|FilteredRest]
%%% 	; NormalizedTokens = [Word|FilteredRest]
%%% 	),
%%% 	normalize_possessives(Rest, KeepWhiteSpace, FilteredRest).
%%% % plural possessives
%%% % normalize_possessives([Word,"'"], [Word]) :-
%%% % normalize_possessives([WordSApostrophe], [Word]) :-
%%% % 	append(Word, "s'", WordSApostrophe),
%%% %	% ends_with_s(Word),
%%% % 	!.
%%% % normalize_possessives([Word,"'",WhiteSpace|Rest], [Word|FilteredRest]) :-
%%% % normalize_possessives([WordSApostrophe,WhiteSpace|Rest], [Word|FilteredRest]) :-
%%% % 	append(Word, "s'", WordSApostrophe),
%%% % 	% ends_with_s(Word),
%%% % 	is_ws(WhiteSpace),
%%% % 	!,
%%% % 	normalize_possessives(Rest, FilteredRest).
%%% normalize_possessives([First|Rest], KeepWhiteSpace, [First|FilteredRest]) :-
%%% 	normalize_possessives(Rest, KeepWhiteSpace, FilteredRest).
%%% 
%%% apostrophe_s_or_s_apostrophe('''s').
%%% apostrophe_s_or_s_apostrophe('s''').


% ORIG
% is_ws_word([]).
% is_ws_word([AlNum|Rest]) :-
% 	is_alnum(AlNum),
% 	is_ws_word(Rest).

is_punct_or_ws([Char]) :-
	( local_punct(Char) ->
	  true
	; local_ws(Char)
	).

% MATS
is_ws_word([]).
is_ws_word([AlNum|Rest]) :-
	local_alnum(AlNum),
	is_ws_word(Rest).

ends_with_s(Word) :-
	% reversed order of args from QP library version!
	last(Word, 0's).

% ORIG
% is_ws([Char]) :-
% 	is_space(Char).

% MATS
is_ws([Char]) :-
	local_ws(Char).

% tokenize_all_text_mm_lc([], []).
% tokenize_all_text_mm_lc([First|Rest], [TokenizedFirst|TokenizedRest]) :-
% 	tokenize_text_mm_lc(First, TokenizedFirst),
% 	tokenize_all_text_mm_lc(Rest, TokenizedRest).
 
tokenize_text_mm_lc(Text, TokText) :-
	tokenize_text_mm(Text, TokText0),
	lowercase_list(TokText0, TokText).

/* tokenize_fields_utterly(+Fields, -TokFields)
/* tokenize_text_utterly(+Text, -TokText)

tokenize_fields_utterly/2 uses tokenize_text_utterly/2 to tokenize the text
in Fields which is a list of fields of the form
  [<field>,<lines>] where <field> is a string and <lines> is a list of strings.
TokFields is a list of tokenized fields of the form
  [<field>,<tokens>] where <field> is a string and <tokens> is a list of
strings.
Note that the <lines> for each <field> are concatenated together with a
blank separator between each line, forming a single string to pass off to
tokenize_text_utterly/2 (which retains the newline characters in the
tokenization it produces.

tokenize_text_utterly/2 transforms Text (atom/string) into a list of
tokens TokText breaking at, but keeping, all non-alphanumerics.  This is
like the current tokenization regime used by wordind except that whitespace
and punctuation are not ignored here.  */

tokenize_fields_utterly([], []).
tokenize_fields_utterly([First|Rest], [TokFirst|TokRest]) :-
	tokenize_one_field_utterly(First, TokFirst),
	tokenize_fields_utterly(Rest, TokRest).

tokenize_one_field_utterly([Field,Lines], [Field,TokField]) :-
	concat_strings_with_separator(Lines, " ", FieldText),
	tokenize_text_utterly(FieldText, TokField0),
	% re_attach_apostrophe_s_tokens(TokField0, TokField),
	TokField = TokField0,
	!.

% form_decimal_numbers([], []).
% form_decimal_numbers([Token1,Token2,Token3|RestTokensIn], [DecimalToken|RestTokensOut]) :-
% 	Token2 = ".",
% 	nls_strings:is_integer_string(Token1),
% 	nls_strings:is_integer_string(Token3),
% 	!,
% 	append([Token1, Token2, Token3], DecimalToken),
% 	form_decimal_numbers(RestTokensIn, RestTokensOut).
% form_decimal_numbers([H|RestIn], [H|RestOut]) :-
% 	form_decimal_numbers(RestIn, RestOut).

% The call to tokenize_text_utterly(FieldText, TokField0)
% will parse words ending in "'s" (apostrophe + s) into multiple tokens, e.g.,
% tokenize_text_utterly("finkelstein's test positive", TokField0)
% will instantiate TokField0 to ["finkelstein", "'", "s", " ", "test", " ", "positive"].
% MetaMap's (new!) default behavior is to reattach the "'s"  to the previous token.

% There are numerous cases of ill-formed text involving apostrophe-s
% that need to be handled via special cases.

% Special case for input like "area, wernicke's"
tokenized_field_in_out(["'", "s"], [], []).

tokenized_field_in_out(["'", "s", TokenAfterS           | RestTokenizedFieldIn],
		       TokenAfterS, RestTokenizedFieldIn).
% Ill-formed case 1:  e.g., "Crohn' s"
tokenized_field_in_out(["'", " ", "s", TokenAfterS      | RestTokenizedFieldIn],
		       TokenAfterS, RestTokenizedFieldIn).
% Ill-formed case 2:  e.g., "Crohn 's"
tokenized_field_in_out([" ", "'", "s", TokenAfterS      | RestTokenizedFieldIn],
		       TokenAfterS, RestTokenizedFieldIn).
% Ill-formed case 3:  e.g., "Crohn ' s"
tokenized_field_in_out([" ", "'", " ", "s", TokenAfterS | RestTokenizedFieldIn],
		       TokenAfterS, RestTokenizedFieldIn).


re_attach_apostrophe_s_tokens([], []).
re_attach_apostrophe_s_tokens(TokenizedFieldIn, TokenizedFieldOut) :-
	TokenizedFieldIn = [OrigString | TailTokenizedFieldIn],
	\+ no_reattach_string(OrigString),
	tokenized_field_in_out(TailTokenizedFieldIn, TokenAfterS, RestTokenizedFieldIn),
	% if the apostrophe-s appears as "'s'", as in, e.g.,
	% "more typical 's'-shaped VCs" (PMID 20444214), do not re-attach!
	% TokenAfterS \== [0'''],
	!,
	TokenizedFieldOut = [StringWithApostropheS|RestTokenizedFieldOut],
	append([OrigString, "'", "s"], StringWithApostropheS),
	% TokenizedFieldOut = [StringWithApostropheS | RestTokenizedField],
	% Special case for input like "area, wernicke's"
	( TokenAfterS == [],
	  RestTokenizedFieldIn == [] ->
	  RestTokenizedFieldOut = []
	; re_attach_apostrophe_s_tokens([TokenAfterS|RestTokenizedFieldIn], RestTokenizedFieldOut)
	).
re_attach_apostrophe_s_tokens([H|Rest], [H|NewRest]) :-
	re_attach_apostrophe_s_tokens(Rest, NewRest).


% succeeds if OrigString (the string representation of the previous token)
% is a string to which the following "'s" (apostrophe-s) should not be re-attached.
% That's true of any token ending in a non-alnum char, "he", "she", and "it".
no_reattach_string(OrigString) :-
	( last(OrigString, LastChar),
	  \+ local_alnum(LastChar) ->
	  true
	; atom_codes(Atom, OrigString),
	  no_combine_pronoun(Atom)
	).

no_combine_pronoun(he).
no_combine_pronoun(she).
no_combine_pronoun(it).

tokenize_text_utterly(Text, TokenizedText) :-
	% PrevChar = '',
	( atom(Text) ->
	  atom_codes(Text, String),
	  % ttu_string(String, PrevChar, TokenizedString, []),
	  ttu_string(String, TokenizedString, []),
	  atom_codes_list(TokenizedText, TokenizedString)
	% ; ttu_string(Text, PrevChar, TokenizedText, [])
	; ttu_string(Text, TokenizedText, [])
	),
	!.

% ORIG
% tokenize_text_utterly(Text,TokText) :-
% 	( atom(Text) ->
% 	  atom_codes(Text,String),
% 	  phrase(ttu_string(TokString),String),
% 	  atom_codes_list(TokText,TokString)
% 	; phrase(ttu_string(TokText),Text)
% 	),
% 	!.

% MATS
% tokenize_text_utterly(Text, TokenizedText) :-
% 	tokenize_text_utterly_1(Text, TokenizedText0),
% 	TokenizedText = TokenizedText0.
% 	% form_decimal_numbers(TokenizedText0, TokenizedText).

% tokenize_text_utterly_1(Text,TokText) :-
% 	( atom(Text) ->
% 	  atom_codes(Text,String),
% 	  ttu_string(TokString,String,[]),
% 	  atom_codes_list(TokText,TokString)
% 	; ttu_string(TokText,Text,[])
% 	),
% 	!.

/*  TOKENIZE TEXT UTTERLY GRAMMAR  */

% ORIG
% ttu_string(TS) -->
% 	( ttu_token(T),
% 	  { T\==[] },
% 	  !,
% 	  ttu_string(S),
% 	  { TS=[T|S] }
% 	; [Char],
% 	  !,
% 	  ttu_string(S),
% 	  { TS = [[Char]|S] }
% 	; { TS= [] }
% 	).

ttu_string([], Remainder, Remainder).
ttu_string([Char1, 39, Char2|RestStringIn], TokenizedString, Remainder) :-
	local_alnum(Char1),
	local_alnum(Char2),
	!,
	TokenizedString = [[Char1, 39, Char2|RestToken]|RemainingTokens],
	ttu_token(RestStringIn, RestToken, RestStringOut),
	ttu_string(RestStringOut, RemainingTokens, Remainder).
ttu_string([Char|RestStringIn], TokenizedString, Remainder) :-
	( local_alnum(Char) ->
	  TokenizedString = [[Char|RestToken]|RemainingTokens],
	  ttu_token(RestStringIn, RestToken, RestStringOut)
	; TokenizedString = [[Char]|RemainingTokens],
	  RestStringOut = RestStringIn
	),
	ttu_string(RestStringOut, RemainingTokens, Remainder).

ttu_token([], [], []).
ttu_token([Char1, 39, Char2|RestString], RestTokenIn, RestTokenOut) :-
	local_alnum(Char1),
	local_alnum(Char2),
	!,
	RestTokenIn = [Char1, 39, Char2|RestTokenNext],
	ttu_token(RestString, RestTokenNext, RestTokenOut).
ttu_token([Char|RestString], RestTokenIn, RestTokenOut) :-
	( local_alnum(Char) ->
	  RestTokenIn = [Char|RestTokenNext],
	  ttu_token(RestString, RestTokenNext, RestTokenOut)
	; RestTokenIn = [],
	  RestTokenOut = [Char|RestString]
	).

% No-DCG version to allow folding ttu_token2/3 into ttu_token/3
%%% ttu_string([], _PrevChar, Remainder, Remainder).
%%% ttu_string([Char|RestStringIn], PrevChar, TokenizedString, Remainder) :-
%%% 	( Char is 39,
%%% 	  local_alnum(PrevChar),
%%% 	  PrevChar \== 39,
%%% 	  RestStringIn = [NextChar|_],
%%% 	  local_alnum(NextChar),
%%% 	  NextChar \== 39 ->
%%% 	  TokenizedString = [[Char|RestToken]|RemainingTokens],
%%% 	  ttu_token(RestStringIn, Char, RestToken, LastChar, RestStringOut)
%%% 	; local_alnum(Char),
%%% 	  Char \== 39 ->
%%% 	  TokenizedString = [[Char|RestToken]|RemainingTokens],
%%% 	  ttu_token(RestStringIn, Char, RestToken, LastChar, RestStringOut)
%%% 	; TokenizedString = [[Char]|RemainingTokens],
%%% 	  RestStringOut = RestStringIn,
%%% 	  LastChar = Char
%%% 	),
%%% 	ttu_string(RestStringOut, LastChar, RemainingTokens, Remainder).
%%% 
%%% ttu_token([], PrevChar, [], PrevChar, []).
%%% ttu_token([Char|RestStringIn], PrevChar, RestTokenIn, LastChar, RestTokenOut) :-
%%% 	( Char is 39,
%%% 	  local_alnum(PrevChar),
%%% 	  PrevChar \== 39,
%%% 	  RestStringIn = [NextChar|_],
%%% 	  local_alnum(NextChar),
%%% 	  NextChar \== 39 ->
%%% 	  RestTokenIn = [Char|RestTokenNext],
%%% 	  ttu_token(RestStringIn, Char, RestTokenNext, LastChar, RestTokenOut)
%%% 	; local_alnum(Char),
%%% 	  Char \== 39 ->
%%% 	  RestTokenIn = [Char|RestTokenNext],
%%% 	  ttu_token(RestStringIn, Char, RestTokenNext, LastChar, RestTokenOut)
%%% 	; RestTokenIn = [],
%%% 	  LastChar = PrevChar,
%%% 	  RestTokenOut = [Char|RestStringIn]
%%% 	).

% MATS
% ttu_string(TS) -->
% 	[Char],
% 	{ local_alnum(Char) }, !,
% 	{ TS=[[Char|S]|R] },
% 	ttu_token(S),
% 	ttu_string(R).
% ttu_string(TS) -->
% 	[Char], !,
% 	{ TS = [[Char]|S] },
% 	ttu_string(S).
% ttu_string([]) --> [].
 
% ORIG
% ttu_token(T) -->
% 	( [Char],
% 	  { is_alnum(Char) },
% 	  !,
% 	  ttu_token(S),
% 	  { T=[Char|S] }
% 	; { T =[] }
% 	).

% MATS
% ttu_token(T) -->
% 	[Char],
% 	{ local_alnum(Char) }, !,
% 	{ T=[Char|S] },
% 	ttu_token(S).
% ttu_token([]) --> [].

% MATS 2
% ttu_token(T, S0, S) :-
% 	ttu_token2(S0, T, S).
% 
% ttu_token2([], [], []).
% ttu_token2([Char|S0], T, S) :-
%	ctypes_bits(Char, Bits),
%	% succeeds if Char is alnum
%	Mask is Bits /\ 3840,
%	(   Mask =\= 0 ->
% 	(   local_alnum(Char) ->
% 	    T = [Char|R],
% 	    ttu_token2(S0, R, S)
% 	;   T = [],
% 	    S = [Char|S0]
% 	).

/* ************************************************************************
   ************************************************************************
   ************************************************************************
                       MetaMap Phrase Access Predicates
   ************************************************************************
   ************************************************************************
   ************************************************************************ */


/* get_phrase_item_feature(+PhraseItem, +Feature, -FeatureValue)
   get_phrase_item_name(+PhraseItem, -ItemName)
   get_phrase_item_subitems(+PhraseItem, -Subitems)
   new_phrase_item(+ItemName, +Subitems, -PhraseItem)
   get_subitems_feature(+Subitems, +Feature, -FeatureValue)
   get_subitem_name(+Subitem, -Name)
   get_subitem_value(+Subitem, -Value)
   set_phrase_item_feature(+PhraseItem, +Feature, +FeatureValue,
                           -ModifiedPhraseItem)
   set_subitems_feature(+Subitems, +Feature, +FeatureValue, -ModifiedSubitems)

get_phrase_item_feature/3
get_phrase_item_name/2
get_phrase_item_subitems/2
new_phrase_item/3
get_subitems_feature/3
get_subitem_name/2
get_subitem_value/2
set_phrase_item_feature/4
set_subitems_feature/4
*/

get_phrase_item_feature(PhraseItem, Feature, FeatureValue) :-
	get_phrase_item_subitems(PhraseItem, Subitems),
	get_subitems_feature(Subitems, Feature, FeatureValue).

get_phrase_item_name(PhraseItem, ItemName) :-
	functor(PhraseItem, ItemName, 1).

get_phrase_item_subitems(PhraseItem, Subitems) :-
	arg(1, PhraseItem, Subitems).

new_phrase_item(ItemName,Subitems,PhraseItem) :-
	functor(PhraseItem, ItemName, 1),
	arg(1, PhraseItem, Subitems).

get_subitems_feature([], _, []).  % return [] for non-existing feature
get_subitems_feature([First|_Rest], Feature, FeatureValue) :-
	get_subitem_name(First, Feature),
	!,
	get_subitem_value(First, FeatureValue).
get_subitems_feature([_First|Rest], Feature, FeatureValue) :-
	get_subitems_feature(Rest, Feature, FeatureValue).

get_subitem_name(Subitem, Name) :-
	functor(Subitem, Name, 1).

get_subitem_value(Subitem, Value) :-
	arg(1, Subitem, Value).

set_phrase_item_feature(PhraseItem, Feature, FeatureValue, ModifiedPhraseItem) :-
	get_phrase_item_name(PhraseItem, ItemName),
	get_phrase_item_subitems(PhraseItem, Subitems),
	set_subitems_feature(Subitems, Feature, FeatureValue, ModifiedSubitems),
	new_phrase_item(ItemName, ModifiedSubitems, ModifiedPhraseItem).

set_subitems_feature([], Feature, FeatureValue, [NewSubitem]) :-
	functor(NewSubitem, Feature, 1),
	arg(1, NewSubitem, FeatureValue).
set_subitems_feature([First|Rest], Feature, FeatureValue, [NewSubitem|Rest]) :-
	functor(First, Feature, 1),
	!,
	functor(NewSubitem, Feature, 1),
	arg(1, NewSubitem, FeatureValue).
set_subitems_feature([First|Rest], Feature, FeatureValue, [First|ModifiedRest]) :-
	set_subitems_feature(Rest, Feature, FeatureValue, ModifiedRest).

local_ws( 9).
local_ws(10).
local_ws(11).
local_ws(12).
local_ws(13).
local_ws(31).
local_ws(32).

local_digit(48). % 0
local_digit(49). % 1
local_digit(50). % 2
local_digit(51). % 3
local_digit(52). % 4
local_digit(53). % 5
local_digit(54). % 6
local_digit(55). % 7
local_digit(56). % 8
local_digit(57). % 9

local_punct(33).  % !
local_punct(34).  % "
local_punct(35).  % #
local_punct(36).  % $
local_punct(37).  %  %
local_punct(38).  % &
local_punct(39).  % '
local_punct(40).  % (
local_punct(41).  % )
local_punct(42).  % *
local_punct(43).  % +
local_punct(44).  % ,
local_punct(45).  % -
local_punct(46).  % .
local_punct(47).  % /
local_punct(58).  % % 
local_punct(59).  % ;
local_punct(60).  % <
local_punct(61).  % =
local_punct(62).  % >
local_punct(63).  % ?
local_punct(64).  % @
local_punct(91).  % [
local_punct(92).  % \
local_punct(93).  % ]
local_punct(94).  % ^
local_punct(95).  % _
local_punct(96).  % `
local_punct(123). % {
local_punct(124). % |
local_punct(125). % }
local_punct(126). %  ~

% local_alnum(48). % 0
% local_alnum(49). % 1
% local_alnum(50). % 2
% local_alnum(51). % 3
% local_alnum(52). % 4
% local_alnum(53). % 5
% local_alnum(54). % 6
% local_alnum(55). % 7
% local_alnum(56). % 8
% local_alnum(57). % 9
% local_alnum(65). % A
% local_alnum(66). % B
% local_alnum(67). % C
% local_alnum(68). % D
% local_alnum(69). % E
% local_alnum(70). % F
% local_alnum(71). % G
% local_alnum(72). % H
% local_alnum(73). % I
% local_alnum(74). % J
% local_alnum(75). % K
% local_alnum(76). % L
% local_alnum(77). % M
% local_alnum(78). % N
% local_alnum(79). % O
% local_alnum(80). % P
% local_alnum(81). % Q
% local_alnum(82). % R
% local_alnum(83). % S
% local_alnum(84). % T
% local_alnum(85). % U
% local_alnum(86). % V
% local_alnum(87). % W
% local_alnum(88). % X
% local_alnum(89). % Y
% local_alnum(90). % Z
% local_alnum(97). % a
% local_alnum(98). % b
% local_alnum(99). % c
% local_alnum(100). % d
% local_alnum(101). % e
% local_alnum(102). % f
% local_alnum(103). % g
% local_alnum(104). % h
% local_alnum(105). % i
% local_alnum(106). % j
% local_alnum(107). % k
% local_alnum(108). % l
% local_alnum(109). % m
% local_alnum(110). % n
% local_alnum(111). % o
% local_alnum(112). % p
% local_alnum(113). % q
% local_alnum(114). % r
% local_alnum(115). % s
% local_alnum(116). % t
% local_alnum(117). % u
% local_alnum(118). % v
% local_alnum(119). % w
% local_alnum(120). % x
% local_alnum(121). % y
% local_alnum(122). % z
% local_alnum(181). % µ
% local_alnum(192). % À
% local_alnum(193). % Á
% local_alnum(194). % Â
% local_alnum(195). % Ã
% local_alnum(196). % Ä
% local_alnum(197). % Å
% local_alnum(198). % Æ
% local_alnum(199). % Ç
% local_alnum(200). % È
% local_alnum(201). % É
% local_alnum(202). % Ê
% local_alnum(203). % Ë
% local_alnum(204). % Ì
% local_alnum(205). % Í
% local_alnum(206). % Î
% local_alnum(207). % Ï
% local_alnum(208). % Ð
% local_alnum(209). % Ñ
% local_alnum(210). % Ò
% local_alnum(211). % Ó
% local_alnum(212). % Ô
% local_alnum(213). % Õ
% local_alnum(214). % Ö
% local_alnum(216). % Ø
% local_alnum(217). % Ù
% local_alnum(218). % Ú
% local_alnum(219). % Û
% local_alnum(220). % Ü
% local_alnum(221). % Ý
% local_alnum(222). % Þ
% local_alnum(223). % ß
% local_alnum(224). % à
% local_alnum(225). % á
% local_alnum(226). % â
% local_alnum(227). % ã
% local_alnum(228). % ä
% local_alnum(229). % å
% local_alnum(230). % æ
% local_alnum(231). % ç
% local_alnum(232). % è
% local_alnum(233). % é
% local_alnum(234). % ê
% local_alnum(235). % ë
% local_alnum(236). % ì
% local_alnum(237). % í
% local_alnum(238). % î
% local_alnum(239). % ï
% local_alnum(240). % ð
% local_alnum(241). % ñ
% local_alnum(242). % ò
% local_alnum(243). % ó
% local_alnum(244). % ô
% local_alnum(245). % õ
% local_alnum(246). % ö
% local_alnum(247). % ÷
% local_alnum(248). % ø
% local_alnum(249). % ù
% local_alnum(250). % ú
% local_alnum(251). % û
% local_alnum(252). % ü
% local_alnum(253). % ý
% local_alnum(254). % þ
% local_alnum(255). % ÿ
% https://en.wikipedia.org/wiki/List_of_Unicode_characters
% Latin Extended-A
% local_alnum(256). % Ā
% local_alnum(257). % ā
% local_alnum(258). % Ă
% local_alnum(259). % ă
% local_alnum(260). % Ą
% local_alnum(261). % ą
% local_alnum(262). % Ć
% local_alnum(263). % ć
% local_alnum(264). % Ĉ
% local_alnum(265). % ĉ
% local_alnum(266). % Ċ
% local_alnum(267). % ċ
% local_alnum(268). % Č
% local_alnum(269). % č
% local_alnum(270). % Ď
% local_alnum(271). % ď
% local_alnum(272). % Đ
% local_alnum(273). % đ
% local_alnum(274). % Ē
% local_alnum(275). % ē
% local_alnum(276). % Ĕ
% local_alnum(277). % ĕ
% local_alnum(278). % Ė
% local_alnum(279). % ė
% local_alnum(280). % Ę
% local_alnum(281). % ę
% local_alnum(282). % Ě
% local_alnum(283). % ě
% local_alnum(284). % Ĝ
% local_alnum(285). % ĝ
% local_alnum(286). % Ğ
% local_alnum(287). % ğ
% local_alnum(288). % Ġ
% local_alnum(289). % ġ
% local_alnum(290). % Ģ
% local_alnum(291). % ģ
% local_alnum(292). % Ĥ
% local_alnum(293). % ĥ
% local_alnum(294). % Ħ
% local_alnum(295). % ħ
% local_alnum(296). % Ĩ
% local_alnum(297). % ĩ
% local_alnum(298). % Ī
% local_alnum(299). % ī
% local_alnum(300). % Ĭ
% local_alnum(301). % ĭ
% local_alnum(302). % Į
% local_alnum(303). % į
% local_alnum(304). % İ
% local_alnum(305). % ı
% local_alnum(306). % Ĳ
% local_alnum(307). % ĳ
% local_alnum(308). % Ĵ
% local_alnum(309). % ĵ
% local_alnum(310). % Ķ
% local_alnum(311). % ķ
% local_alnum(312). % ĸ
% local_alnum(313). % Ĺ
% local_alnum(314). % ĺ
% local_alnum(315). % Ļ
% local_alnum(316). % ļ
% local_alnum(317). % Ľ
% local_alnum(318). % ľ
% local_alnum(319). % Ŀ
% local_alnum(320). % ŀ
% local_alnum(321). % Ł
% local_alnum(322). % ł
% local_alnum(323). % Ń
% local_alnum(324). % ń
% local_alnum(325). % Ņ
% local_alnum(326). % ņ
% local_alnum(327). % Ň
% local_alnum(328). % ň
% local_alnum(329). % ŉ
% local_alnum(330). % Ŋ
% local_alnum(331). % ŋ
% local_alnum(332). % Ō
% local_alnum(333). % ō
% local_alnum(334). % Ŏ
% local_alnum(335). % ŏ
% local_alnum(336). % Ő
% local_alnum(337). % ő
% local_alnum(338). % Œ
% local_alnum(339). % œ
% local_alnum(340). % Ŕ
% local_alnum(341). % ŕ
% local_alnum(342). % Ŗ
% local_alnum(343). % ŗ
% local_alnum(344). % Ř
% local_alnum(345). % ř
% local_alnum(346). % Ś
% local_alnum(347). % ś
% local_alnum(348). % Ŝ
% local_alnum(349). % ŝ
% local_alnum(350). % Ş
% local_alnum(351). % ş
% local_alnum(352). % Š
% local_alnum(353). % š
% local_alnum(354). % Ţ
% local_alnum(355). % ţ
% local_alnum(356). % Ť
% local_alnum(357). % ť
% local_alnum(358). % Ŧ
% local_alnum(359). % ŧ
% local_alnum(360). % Ũ
% local_alnum(361). % ũ
% local_alnum(362). % Ū
% local_alnum(363). % ū
% local_alnum(364). % Ŭ
% local_alnum(365). % ŭ
% local_alnum(366). % Ů
% local_alnum(367). % ů
% local_alnum(368). % Ű
% local_alnum(369). % ű
% local_alnum(370). % Ų
% local_alnum(371). % ų
% local_alnum(372). % Ŵ
% local_alnum(373). % ŵ
% local_alnum(374). % Ŷ
% local_alnum(375). % ŷ
% local_alnum(376). % Ÿ
% local_alnum(377). % Ź
% local_alnum(378). % ź
% local_alnum(379). % Ż
% local_alnum(380). % ż
% local_alnum(381). % Ž
% local_alnum(382). % ž
% local_alnum(383). % ſ
% https://en.wikipedia.org/wiki/List_of_Unicode_characters
% Latin Extended-B
% local_alnum(384). % ƀ
% local_alnum(385). % Ɓ
% local_alnum(386). % Ƃ
% local_alnum(387). % ƃ
% local_alnum(388). % Ƅ
% local_alnum(389). % ƅ
% local_alnum(390). % Ɔ
% local_alnum(391). % Ƈ
% local_alnum(392). % ƈ
% local_alnum(393). % Ɖ
% local_alnum(394). % Ɗ
% local_alnum(395). % Ƌ
% local_alnum(396). % ƌ
% local_alnum(397). % ƍ
% local_alnum(398). % Ǝ
% local_alnum(399). % Ə
% local_alnum(400). % Ɛ
% local_alnum(401). % Ƒ
% local_alnum(402). % ƒ
% local_alnum(403). % Ɠ
% local_alnum(404). % Ɣ
% local_alnum(405). % ƕ
% local_alnum(406). % Ɩ
% local_alnum(407). % Ɨ
% local_alnum(408). % Ƙ
% local_alnum(409). % ƙ
% local_alnum(410). % ƚ
% local_alnum(411). % ƛ
% local_alnum(412). % Ɯ
% local_alnum(413). % Ɲ
% local_alnum(414). % ƞ
% local_alnum(415). % Ɵ
% local_alnum(416). % Ơ
% local_alnum(417). % ơ
% local_alnum(418). % Ƣ
% local_alnum(419). % ƣ
% local_alnum(420). % Ƥ
% local_alnum(421). % ƥ
% local_alnum(422). % Ʀ
% local_alnum(423). % Ƨ
% local_alnum(424). % ƨ
% local_alnum(425). % Ʃ
% local_alnum(426). % ƪ
% local_alnum(427). % ƫ
% local_alnum(428). % Ƭ
% local_alnum(429). % ƭ
% local_alnum(430). % Ʈ
% local_alnum(431). % Ư
% local_alnum(432). % ư
% local_alnum(433). % Ʊ
% local_alnum(434). % Ʋ
% local_alnum(435). % Ƴ
% local_alnum(436). % ƴ
% local_alnum(437). % Ƶ
% local_alnum(438). % ƶ
% local_alnum(439). % Ʒ
% local_alnum(440). % Ƹ
% local_alnum(441). % ƹ
% local_alnum(442). % ƺ
% local_alnum(443). % ƻ
% local_alnum(444). % Ƽ
% local_alnum(445). % ƽ
% local_alnum(446). % ƾ
% local_alnum(447). % ƿ
% local_alnum(448). % ǀ
% local_alnum(449). % ǁ
% local_alnum(450). % ǂ
% local_alnum(451). % ǃ
% local_alnum(452). % Ǆ
% local_alnum(453). % ǅ
% local_alnum(454). % ǆ
% local_alnum(455). % Ǉ
% local_alnum(456). % ǈ
% local_alnum(457). % ǉ
% local_alnum(458). % Ǌ
% local_alnum(459). % ǋ
% local_alnum(460). % ǌ
% local_alnum(461). % Ǎ
% local_alnum(462). % ǎ
% local_alnum(463). % Ǐ
% local_alnum(464). % ǐ
% local_alnum(465). % Ǒ
% local_alnum(466). % ǒ
% local_alnum(467). % Ǔ
% local_alnum(468). % ǔ
% local_alnum(469). % Ǖ
% local_alnum(470). % ǖ
% local_alnum(471). % Ǘ
% local_alnum(472). % ǘ
% local_alnum(473). % Ǚ
% local_alnum(474). % ǚ
% local_alnum(475). % Ǜ
% local_alnum(476). % ǜ
% local_alnum(477). % ǝ
% local_alnum(478). % Ǟ
% local_alnum(479). % ǟ
% local_alnum(480). % Ǡ
% local_alnum(481). % ǡ
% local_alnum(482). % Ǣ
% local_alnum(483). % ǣ
% local_alnum(484). % Ǥ
% local_alnum(485). % ǥ
% local_alnum(486). % Ǧ
% local_alnum(487). % ǧ
% local_alnum(488). % Ǩ
% local_alnum(489). % ǩ
% local_alnum(490). % Ǫ
% local_alnum(491). % ǫ
% local_alnum(492). % Ǭ
% local_alnum(493). % ǭ
% local_alnum(494). % Ǯ
% local_alnum(495). % ǯ
% local_alnum(496). % ǰ
% local_alnum(497). % Ǳ
% local_alnum(498). % ǲ
% local_alnum(499). % ǳ
% local_alnum(500). % Ǵ
% local_alnum(501). % ǵ
% local_alnum(502). % Ƕ
% local_alnum(503). % Ƿ
% local_alnum(504). % Ǹ
% local_alnum(505). % ǹ
% local_alnum(506). % Ǻ
% local_alnum(507). % ǻ
% local_alnum(508). % Ǽ
% local_alnum(509). % ǽ
% local_alnum(510). % Ǿ
% local_alnum(511). % ǿ
% local_alnum(512). % Ȁ
% local_alnum(513). % ȁ
% local_alnum(514). % Ȃ
% local_alnum(515). % ȃ
% local_alnum(516). % Ȅ
% local_alnum(517). % ȅ
% local_alnum(518). % Ȇ
% local_alnum(519). % ȇ
% local_alnum(520). % Ȉ
% local_alnum(521). % ȉ
% local_alnum(522). % Ȋ
% local_alnum(523). % ȋ
% local_alnum(524). % Ȍ
% local_alnum(525). % ȍ
% local_alnum(526). % Ȏ
% local_alnum(527). % ȏ
% local_alnum(528). % Ȑ
% local_alnum(529). % ȑ
% local_alnum(530). % Ȓ
% local_alnum(531). % ȓ
% local_alnum(532). % Ȕ
% local_alnum(533). % ȕ
% local_alnum(534). % Ȗ
% local_alnum(535). % ȗ
% local_alnum(536). % Ș
% local_alnum(537). % ș
% local_alnum(538). % Ț
% local_alnum(539). % ț
% local_alnum(540). % Ȝ
% local_alnum(541). % ȝ
% local_alnum(542). % Ȟ
% local_alnum(543). % ȟ
% local_alnum(544). % Ƞ
% local_alnum(545). % ȡ
% local_alnum(546). % Ȣ
% local_alnum(547). % ȣ
% local_alnum(548). % Ȥ
% local_alnum(549). % ȥ
% local_alnum(550). % Ȧ
% local_alnum(551). % ȧ
% local_alnum(552). % Ȩ
% local_alnum(553). % ȩ
% local_alnum(554). % Ȫ
% local_alnum(555). % ȫ
% local_alnum(556). % Ȭ
% local_alnum(557). % ȭ
% local_alnum(558). % Ȯ
% local_alnum(559). % ȯ
% local_alnum(560). % Ȱ
% local_alnum(561). % ȱ
% local_alnum(562). % Ȳ
% local_alnum(563). % ȳ
% local_alnum(564). % ȴ
% local_alnum(565). % ȵ
% local_alnum(566). % ȶ
% local_alnum(567). % ȷ
% local_alnum(568). % ȸ
% local_alnum(569). % ȹ
% local_alnum(570). % Ⱥ
% local_alnum(571). % Ȼ
% local_alnum(572). % ȼ
% local_alnum(573). % Ƚ
% local_alnum(574). % Ⱦ
% local_alnum(575). % ȿ
% local_alnum(576). % ɀ
% local_alnum(577). % Ɂ
% local_alnum(578). % ɂ
% local_alnum(579). % Ƀ
% local_alnum(580). % Ʉ
% local_alnum(581). % Ʌ
% local_alnum(582). % Ɇ
% local_alnum(583). % ɇ
% local_alnum(584). % Ɉ
% local_alnum(585). % ɉ
% local_alnum(586). % Ɋ
% local_alnum(587). % ɋ
% local_alnum(588). % Ɍ
% local_alnum(589). % ɍ
% local_alnum(590). % Ɏ
% local_alnum(591). % ɏ
% local_alnum(592). % ɐ
% local_alnum(593). % ɑ
% local_alnum(594). % ɒ
% local_alnum(595). % ɓ
% local_alnum(596). % ɔ
% local_alnum(597). % ɕ
% local_alnum(598). % ɖ
% local_alnum(599). % ɗ
% local_alnum(600). % ɘ
% local_alnum(601). % ə
% local_alnum(602). % ɚ
% local_alnum(603). % ɛ
% local_alnum(604). % ɜ
% local_alnum(605). % ɝ
% local_alnum(606). % ɞ
% local_alnum(607). % ɟ
% local_alnum(608). % ɠ
% local_alnum(609). % ɡ
% local_alnum(610). % ɢ
% local_alnum(611). % ɣ
% local_alnum(612). % ɤ
% local_alnum(613). % ɥ
% local_alnum(614). % ɦ
% local_alnum(615). % ɧ
% local_alnum(616). % ɨ
% local_alnum(617). % ɩ
% local_alnum(618). % ɪ
% local_alnum(619). % ɫ
% local_alnum(620). % ɬ
% local_alnum(621). % ɭ
% local_alnum(622). % ɮ
% local_alnum(623). % ɯ
% local_alnum(624). % ɰ
% local_alnum(625). % ɱ
% local_alnum(626). % ɲ
% local_alnum(627). % ɳ
% local_alnum(628). % ɴ
% local_alnum(629). % ɵ
% local_alnum(630). % ɶ
% local_alnum(631). % ɷ
% local_alnum(632). % ɸ
% local_alnum(633). % ɹ
% https://en.wikipedia.org/wiki/List_of_Unicode_characters
% Latin Additional
% local_alnum(634). % ɺ
% local_alnum(635). % ɻ
% local_alnum(636). % ɼ
% local_alnum(637). % ɽ
% local_alnum(638). % ɾ
% local_alnum(639). % ɿ
% local_alnum(640). % ʀ
% local_alnum(641). % ʁ
% local_alnum(642). % ʂ
% local_alnum(643). % ʃ
% local_alnum(644). % ʄ
% local_alnum(645). % ʅ
% local_alnum(646). % ʆ
% local_alnum(647). % ʇ
% local_alnum(648). % ʈ
% local_alnum(649). % ʉ
% local_alnum(650). % ʊ
% local_alnum(651). % ʋ
% local_alnum(652). % ʌ
% local_alnum(653). % ʍ
% local_alnum(654). % ʎ
% local_alnum(655). % ʏ
% local_alnum(656). % ʐ
% local_alnum(657). % ʑ
% local_alnum(658). % ʒ
% local_alnum(659). % ʓ
% local_alnum(660). % ʔ
% local_alnum(661). % ʕ
% local_alnum(662). % ʖ
% local_alnum(663). % ʗ
% local_alnum(664). % ʘ
% local_alnum(665). % ʙ
% local_alnum(666). % ʚ
% local_alnum(667). % ʛ
% local_alnum(668). % ʜ
% local_alnum(669). % ʝ
% local_alnum(670). % ʞ
% local_alnum(671). % ʟ
% local_alnum(672). % ʠ
% local_alnum(673). % ʡ
% local_alnum(674). % ʢ
% local_alnum(675). % ʣ
% local_alnum(676). % ʤ
% local_alnum(677). % ʥ
% local_alnum(678). % ʦ
% local_alnum(679). % ʧ
% local_alnum(680). % ʨ
% local_alnum(681). % ʩ
% local_alnum(682). % ʪ
% local_alnum(683). % ʫ
% local_alnum(684). % ʬ
% local_alnum(685). % ʭ
% local_alnum(686). % ʮ
% local_alnum(687). % ʯ
% http://www.fileformat.info/info/charset/UTF-8/list.htm
% local_alnum(880). % Ͱ
% local_alnum(881). % ͱ
% local_alnum(882). % Ͳ
% local_alnum(883). % ͳ
% local_alnum(884). % ʹ
% local_alnum(885). % ͵
% local_alnum(886). % Ͷ
% local_alnum(887). % ͷ
% local_alnum(888). % ͸
% local_alnum(889). % ͹
% local_alnum(890). % ͺ
% local_alnum(891). % ͻ
% local_alnum(892). % ͼ
% local_alnum(893). % ͽ
% local_alnum(894). % ;
% local_alnum(895). % Ϳ
% local_alnum(896). % ΀
% local_alnum(897). % ΁
% local_alnum(898). % ΂
% local_alnum(899). % ΃
% local_alnum(900). % ΄
% local_alnum(901). % ΅
% local_alnum(902). % Ά
% local_alnum(903). % ·
% local_alnum(904). % Έ
% local_alnum(905). % Ή
% local_alnum(906). % Ί
% local_alnum(907). % ΋
% local_alnum(908). % Ό
% local_alnum(909). % ΍
% local_alnum(910). % Ύ
% local_alnum(911). % Ώ
% local_alnum(912). % ΐ
% local_alnum(913). % Α
% local_alnum(914). % Β
% local_alnum(915). % Γ
% local_alnum(916). % Δ
% local_alnum(917). % Ε
% local_alnum(918). % Ζ
% local_alnum(919). % Η
% local_alnum(920). % Θ
% local_alnum(921). % Ι
% local_alnum(922). % Κ
% local_alnum(923). % Λ
% local_alnum(924). % Μ
% local_alnum(925). % Ν
% local_alnum(926). % Ξ
% local_alnum(927). % Ο
% local_alnum(928). % Π
% local_alnum(929). % Ρ
% local_alnum(930). % ΢
% local_alnum(931). % Σ
% local_alnum(932). % Τ
% local_alnum(933). % Υ
% local_alnum(934). % Φ
% local_alnum(935). % Χ
% local_alnum(936). % Ψ
% local_alnum(937). % Ω
% local_alnum(938). % Ϊ
% local_alnum(939). % Ϋ
% local_alnum(940). % ά
% local_alnum(941). % έ
% local_alnum(942). % ή
% local_alnum(943). % ί
% local_alnum(944). % ΰ
% local_alnum(945). % α
% local_alnum(946). % β
% local_alnum(947). % γ
% local_alnum(948). % δ
% local_alnum(949). % ε
% local_alnum(950). % ζ
% local_alnum(951). % η
% local_alnum(952). % θ
% local_alnum(953). % ι
% local_alnum(954). % κ
% local_alnum(955). % λ
% local_alnum(956). % μ
% local_alnum(957). % ν
% local_alnum(958). % ξ
% local_alnum(959). % ο
% local_alnum(960). % π
% local_alnum(961). % ρ
% local_alnum(962). % ς
% local_alnum(963). % σ
% local_alnum(964). % τ
% local_alnum(965). % υ
% local_alnum(966). % φ
% local_alnum(967). % χ
% local_alnum(968). % ψ
% local_alnum(969). % ω
% local_alnum(970). % ϊ
% local_alnum(971). % ϋ
% local_alnum(972). % ό
% local_alnum(973). % ύ
% local_alnum(974). % ώ
% local_alnum(975). % Ϗ
% local_alnum(976). % ϐ
% local_alnum(977). % ϑ
% local_alnum(978). % ϒ
% local_alnum(979). % ϓ
% local_alnum(980). % ϔ
% local_alnum(981). % ϕ
% local_alnum(982). % ϖ
% local_alnum(983). % ϗ
% local_alnum(984). % Ϙ
% local_alnum(985). % ϙ
% local_alnum(986). % Ϛ
% local_alnum(987). % ϛ
% local_alnum(988). % Ϝ
% local_alnum(989). % ϝ
% local_alnum(990). % Ϟ
% local_alnum(991). % ϟ
% local_alnum(992). % Ϡ
% local_alnum(993). % ϡ
% local_alnum(994). % Ϣ
% local_alnum(995). % ϣ
% local_alnum(996). % Ϥ
% local_alnum(997). % ϥ
% local_alnum(998). % Ϧ
% local_alnum(999). % ϧ
% local_alnum(1000). % Ϩ
% local_alnum(1001). % ϩ
% local_alnum(1002). % Ϫ
% local_alnum(1003). % ϫ
% local_alnum(1004). % Ϭ
% local_alnum(1005). % ϭ
% local_alnum(1006). % Ϯ
% local_alnum(1007). % ϯ
% local_alnum(1008). % ϰ
% local_alnum(1009). % ϱ
% local_alnum(1010). % ϲ
% local_alnum(1011). % ϳ
% local_alnum(1012). % ϴ
% local_alnum(1013). % ϵ
% local_alnum(1014). % ϶
% local_alnum(1015). % Ϸ
% local_alnum(1016). % ϸ
% local_alnum(1017). % Ϲ
% local_alnum(1018). % Ϻ
% local_alnum(1019). % ϻ
% local_alnum(1020). % ϼ
% local_alnum(1021). % Ͻ
% local_alnum(1022). % Ͼ
% local_alnum(1023). % Ͽ

local_alnum(C) :- local_print(C), \+ local_non_alnum(C).

local_non_alnum(32). %  
local_non_alnum(33). % !
local_non_alnum(34). % "
local_non_alnum(35). % #
local_non_alnum(36). % $
local_non_alnum(37). % %
local_non_alnum(38). % &
local_non_alnum(39). % '
local_non_alnum(40). % (
local_non_alnum(41). % )
local_non_alnum(42). % *
local_non_alnum(43). % +
local_non_alnum(44). % ,
local_non_alnum(45). % -
local_non_alnum(46). % .
local_non_alnum(47). % /
local_non_alnum(58). % :
local_non_alnum(59). % ;
local_non_alnum(60). % <
local_non_alnum(61). % =
local_non_alnum(62). % >
local_non_alnum(63). % ?
local_non_alnum(64). % @
local_non_alnum(91). % [
local_non_alnum(92). % \
local_non_alnum(93). % ]
local_non_alnum(94). % ^
local_non_alnum(95). % _
local_non_alnum(96). % `
local_non_alnum(123). % {
local_non_alnum(124). % |
local_non_alnum(125). % }
local_non_alnum(126). % ~
local_non_alnum(161). % ¡
local_non_alnum(162). % ¢
local_non_alnum(163). % £
local_non_alnum(164). % ¤
local_non_alnum(165). % ¥
local_non_alnum(166). % ¦
local_non_alnum(167). % §
local_non_alnum(168). % ¨
local_non_alnum(169). % ©
% local_non_alnum(170). % ª
local_non_alnum(171). % «
local_non_alnum(172). % ¬
local_non_alnum(173). % ­
local_non_alnum(174). % ®
local_non_alnum(175). % ¯
local_non_alnum(176). % °
local_non_alnum(177). % ±
% local_non_alnum(178). % ²
% local_non_alnum(179). % ³
local_non_alnum(180). % ´
local_non_alnum(182). % ¶
local_non_alnum(183). % ·
local_non_alnum(184). % ¸
local_non_alnum(185). % ¹
% degree sign
% local_non_alnum(186). % º
local_non_alnum(187). % »
local_non_alnum(188). % ¼
local_non_alnum(189). % ½
local_non_alnum(190). % ¾
local_non_alnum(191). % ¿
local_non_alnum(215). % ×

local_alpha(65).  % A
local_alpha(66).  % B
local_alpha(67).  % C
local_alpha(68).  % D
local_alpha(69).  % E
local_alpha(70).  % F
local_alpha(71).  % G
local_alpha(72).  % H
local_alpha(73).  % I
local_alpha(74).  % J
local_alpha(75).  % K
local_alpha(76).  % L
local_alpha(77).  % M
local_alpha(78).  % N
local_alpha(79).  % O
local_alpha(80).  % P
local_alpha(81).  % Q
local_alpha(82).  % R
local_alpha(83).  % S
local_alpha(84).  % T
local_alpha(85).  % U
local_alpha(86).  % V
local_alpha(87).  % W
local_alpha(88).  % X
local_alpha(89).  % Y
local_alpha(90).  % Z
local_alpha(97).  % a
local_alpha(98).  % b
local_alpha(99).  % c
local_alpha(100). % d
local_alpha(101). % e
local_alpha(102). % f
local_alpha(103). % g
local_alpha(104). % h
local_alpha(105). % i
local_alpha(106). % j
local_alpha(107). % k
local_alpha(108). % l
local_alpha(109). % m
local_alpha(110). % n
local_alpha(111). % o
local_alpha(112). % p
local_alpha(113). % q
local_alpha(114). % r
local_alpha(115). % s
local_alpha(116). % t
local_alpha(117). % u
local_alpha(118). % v
local_alpha(119). % w
local_alpha(120). % x
local_alpha(121). % y
local_alpha(122). % z

local_upper(65).  % A
local_upper(66).  % B
local_upper(67).  % C
local_upper(68).  % D
local_upper(69).  % E
local_upper(70).  % F
local_upper(71).  % G
local_upper(72).  % H
local_upper(73).  % I
local_upper(74).  % J
local_upper(75).  % K
local_upper(76).  % L
local_upper(77).  % M
local_upper(78).  % N
local_upper(79).  % O
local_upper(80).  % P
local_upper(81).  % Q
local_upper(82).  % R
local_upper(83).  % S
local_upper(84).  % T
local_upper(85).  % U
local_upper(86).  % V
local_upper(87).  % W
local_upper(88).  % X
local_upper(89).  % Y
local_upper(90).  % Z

local_lower(97).  % a
local_lower(98).  % b
local_lower(99).  % c
local_lower(100). % d
local_lower(101). % e
local_lower(102). % f
local_lower(103). % g
local_lower(104). % h
local_lower(105). % i
local_lower(106). % j
local_lower(107). % k
local_lower(108). % l
local_lower(109). % m
local_lower(110). % n
local_lower(111). % o
local_lower(112). % p
local_lower(113). % q
local_lower(114). % r
local_lower(115). % s
local_lower(116). % t
local_lower(117). % u
local_lower(118). % v
local_lower(119). % w
local_lower(120). % x
local_lower(121). % y
local_lower(122). % z

local_ascii(0).
local_ascii(1).
local_ascii(2).
local_ascii(3).
local_ascii(4).
local_ascii(5).
local_ascii(6).
local_ascii(7).
local_ascii(8).
local_ascii(9).
local_ascii(10).
local_ascii(11).
local_ascii(12).
local_ascii(13).
local_ascii(14).
local_ascii(15).
local_ascii(16).
local_ascii(17).
local_ascii(18).
local_ascii(19).
local_ascii(20).
local_ascii(21).
local_ascii(22).
local_ascii(23).
local_ascii(24).
local_ascii(25).
local_ascii(26).
local_ascii(27).
local_ascii(28).
local_ascii(29).
local_ascii(30).
local_ascii(31).
local_ascii(32).
local_ascii(33).
local_ascii(34).
local_ascii(35).
local_ascii(36).
local_ascii(37).
local_ascii(38).
local_ascii(39).
local_ascii(40).
local_ascii(41).
local_ascii(42).
local_ascii(43).
local_ascii(44).
local_ascii(45).
local_ascii(46).
local_ascii(47).
local_ascii(48).
local_ascii(49).
local_ascii(50).
local_ascii(51).
local_ascii(52).
local_ascii(53).
local_ascii(54).
local_ascii(55).
local_ascii(56).
local_ascii(57).
local_ascii(58).
local_ascii(59).
local_ascii(60).
local_ascii(61).
local_ascii(62).
local_ascii(63).
local_ascii(64).
local_ascii(65).
local_ascii(66).
local_ascii(67).
local_ascii(68).
local_ascii(69).
local_ascii(70).
local_ascii(71).
local_ascii(72).
local_ascii(73).
local_ascii(74).
local_ascii(75).
local_ascii(76).
local_ascii(77).
local_ascii(78).
local_ascii(79).
local_ascii(80).
local_ascii(81).
local_ascii(82).
local_ascii(83).
local_ascii(84).
local_ascii(85).
local_ascii(86).
local_ascii(87).
local_ascii(88).
local_ascii(89).
local_ascii(90).
local_ascii(91).
local_ascii(92).
local_ascii(93).
local_ascii(94).
local_ascii(95).
local_ascii(96).
local_ascii(97).
local_ascii(98).
local_ascii(99).
local_ascii(100).
local_ascii(101).
local_ascii(102).
local_ascii(103).
local_ascii(104).
local_ascii(105).
local_ascii(106).
local_ascii(107).
local_ascii(108).
local_ascii(109).
local_ascii(110).
local_ascii(111).
local_ascii(112).
local_ascii(113).
local_ascii(114).
local_ascii(115).
local_ascii(116).
local_ascii(117).
local_ascii(118).
local_ascii(119).
local_ascii(120).
local_ascii(121).
local_ascii(122).
local_ascii(123).
local_ascii(124).
local_ascii(125).
local_ascii(126).
local_ascii(127).

% local_print(C) :-
% 	( C == 32 ->
% 	    true
% 	; \+ local_ws(C)
% 	).

local_print(9).  % TAB
local_print(32). %  #U+0020 	  	32 	Space 	0001
local_print(33). % !#U+0021 	! 	33 	Exclamation mark 	0002
local_print(34). % "#U+0022 	" 	34 	Quotation mark 	0003
local_print(35). % ##U+0023 	# 	35 	Number sign, Hashtag, Octothorpe, Sharp 	0004
local_print(36). % $#U+0024 	$ 	36 	Dollar sign 	0005
local_print(37). % %#U+0025 	% 	37 	Percent sign 	0006
local_print(38). % &#U+0026 	& 	38 	Ampersand 	0007
local_print(39). % '#U+0027 	' 	39 	Apostrophe 	0008
local_print(40). % (#U+0028 	( 	40 	Left parenthesis 	0009
local_print(41). % )#U+0029 	) 	41 	Right parenthesis 	0010
local_print(42). % *#U+002A 	* 	42 	Asterisk 	0011
local_print(43). % +#U+002B 	+ 	43 	Plus sign 	0012
local_print(44). % ,#U+002C 	, 	44 	Comma 	0013
local_print(45). % -#U+002D 	- 	45 	Hyphen-minus 	0014
local_print(46). % .#U+002E 	. 	46 	Full stop 	0015
local_print(47). % /#U+002F 	/ 	47 	Slash (Solidus) 	0016
local_print(48). % 0#U+0030 	0 	48 	Digit Zero 	0017
local_print(49). % 1#U+0031 	1 	49 	Digit One 	0018
local_print(50). % 2#U+0032 	2 	50 	Digit Two 	0019
local_print(51). % 3#U+0033 	3 	51 	Digit Three 	0020
local_print(52). % 4#U+0034 	4 	52 	Digit Four 	0021
local_print(53). % 5#U+0035 	5 	53 	Digit Five 	0022
local_print(54). % 6#U+0036 	6 	54 	Digit Six 	0023
local_print(55). % 7#U+0037 	7 	55 	Digit Seven 	0024
local_print(56). % 8#U+0038 	8 	56 	Digit Eight 	0025
local_print(57). % 9#U+0039 	9 	57 	Digit Nine 	0026
local_print(58). % :#U+003A 	: 	58 	Colon 	0027
local_print(59). % ;#U+003B 	; 	59 	Semicolon 	0028
local_print(60). % <#U+003C 	< 	60 	Less-than sign 	0029
local_print(61). % =#U+003D 	= 	61 	Equal sign 	0030
local_print(62). % >#U+003E 	> 	62 	Greater-than sign 	0031
local_print(63). % ?#U+003F 	? 	63 	Question mark 	0032
local_print(64). % @#U+0040 	@ 	64 	At sign 	0033
local_print(65). % A#U+0041 	A 	65 	Latin Capital letter A 	0034
local_print(66). % B#U+0042 	B 	66 	Latin Capital letter B 	0035
local_print(67). % C#U+0043 	C 	67 	Latin Capital letter C 	0036
local_print(68). % D#U+0044 	D 	68 	Latin Capital letter D 	0037
local_print(69). % E#U+0045 	E 	69 	Latin Capital letter E 	0038
local_print(70). % F#U+0046 	F 	70 	Latin Capital letter F 	0039
local_print(71). % G#U+0047 	G 	71 	Latin Capital letter G 	0040
local_print(72). % H#U+0048 	H 	72 	Latin Capital letter H 	0041
local_print(73). % I#U+0049 	I 	73 	Latin Capital letter I 	0042
local_print(74). % J#U+004A 	J 	74 	Latin Capital letter J 	0043
local_print(75). % K#U+004B 	K 	75 	Latin Capital letter K 	0044
local_print(76). % L#U+004C 	L 	76 	Latin Capital letter L 	0045
local_print(77). % M#U+004D 	M 	77 	Latin Capital letter M 	0046
local_print(78). % N#U+004E 	N 	78 	Latin Capital letter N 	0047
local_print(79). % O#U+004F 	O 	79 	Latin Capital letter O 	0048
local_print(80). % P#U+0050 	P 	80 	Latin Capital letter P 	0049
local_print(81). % Q#U+0051 	Q 	81 	Latin Capital letter Q 	0050
local_print(82). % R#U+0052 	R 	82 	Latin Capital letter R 	0051
local_print(83). % S#U+0053 	S 	83 	Latin Capital letter S 	0052
local_print(84). % T#U+0054 	T 	84 	Latin Capital letter T 	0053
local_print(85). % U#U+0055 	U 	85 	Latin Capital letter U 	0054
local_print(86). % V#U+0056 	V 	86 	Latin Capital letter V 	0055
local_print(87). % W#U+0057 	W 	87 	Latin Capital letter W 	0056
local_print(88). % X#U+0058 	X 	88 	Latin Capital letter X 	0057
local_print(89). % Y#U+0059 	Y 	89 	Latin Capital letter Y 	0058
local_print(90). % Z#U+005A 	Z 	90 	Latin Capital letter Z 	0059
local_print(91). % [#U+005B 	[ 	91 	Left Square Bracket 	0060
local_print(92). % \#U+005C 	\ 	92 	Backslash 	0061
local_print(93). % ]#U+005D 	] 	93 	Right Square Bracket 	0062
local_print(94). % ^#U+005E 	^ 	94 	Circumflex accent 	0063
local_print(95). % _#U+005F 	_ 	95 	Low line 	0064
local_print(96). % `#U+0060 	` 	96 	Grave accent 	0065
local_print(97). % a#U+0061 	a 	97 	Latin Small Letter A 	0066
local_print(98). % b#U+0062 	b 	98 	Latin Small Letter B 	0067
local_print(99). % c#U+0063 	c 	99 	Latin Small Letter C 	0068
local_print(100). % d#U+0064 	d 	100 	Latin Small Letter D 	0069
local_print(101). % e#U+0065 	e 	101 	Latin Small Letter E 	0070
local_print(102). % f#U+0066 	f 	102 	Latin Small Letter F 	0071
local_print(103). % g#U+0067 	g 	103 	Latin Small Letter G 	0072
local_print(104). % h#U+0068 	h 	104 	Latin Small Letter H 	0073
local_print(105). % i#U+0069 	i 	105 	Latin Small Letter I 	0074
local_print(106). % j#U+006A 	j 	106 	Latin Small Letter J 	0075
local_print(107). % k#U+006B 	k 	107 	Latin Small Letter K 	0076
local_print(108). % l#U+006C 	l 	108 	Latin Small Letter L 	0077
local_print(109). % m#U+006D 	m 	109 	Latin Small Letter M 	0078
local_print(110). % n#U+006E 	n 	110 	Latin Small Letter N 	0079
local_print(111). % o#U+006F 	o 	111 	Latin Small Letter O 	0080
local_print(112). % p#U+0070 	p 	112 	Latin Small Letter P 	0081
local_print(113). % q#U+0071 	q 	113 	Latin Small Letter Q 	0082
local_print(114). % r#U+0072 	r 	114 	Latin Small Letter R 	0083
local_print(115). % s#U+0073 	s 	115 	Latin Small Letter S 	0084
local_print(116). % t#U+0074 	t 	116 	Latin Small Letter T 	0085
local_print(117). % u#U+0075 	u 	117 	Latin Small Letter U 	0086
local_print(118). % v#U+0076 	v 	118 	Latin Small Letter V 	0087
local_print(119). % w#U+0077 	w 	119 	Latin Small Letter W 	0088
local_print(120). % x#U+0078 	x 	120 	Latin Small Letter X 	0089
local_print(121). % y#U+0079 	y 	121 	Latin Small Letter Y 	0090
local_print(122). % z#U+007A 	z 	122 	Latin Small Letter Z 	0091
local_print(123). % {#U+007B 	{ 	123 	Left Curly Bracket 	0092
local_print(124). % |#U+007C 	| 	124 	Vertical bar 	0093
local_print(125). % }#U+007D 	} 	125 	Right Curly Bracket 	0094
local_print(126). % ~#U+007E 	~ 	126 	Tilde 	0095
% https://en.wikipedia.org/wiki/List_of_Unicode_characters
% Latin-1 Supplement#
local_print(161). % ¡#U+00A1 	¡ 	0161 	&iexcl; 	Inverted Exclamation Mark 	0097
local_print(162). % ¢#U+00A2 	¢ 	0162 	&cent; 	Cent sign 	0098
local_print(163). % £#U+00A3 	£ 	0163 	&pound; 	Pound sign 	0099
local_print(164). % ¤#U+00A4 	¤ 	0164 	&curren; 	Currency sign 	0100
local_print(165). % ¥#U+00A5 	¥ 	0165 	&yen; 	Yen sign 	0101
local_print(166). % ¦#U+00A6 	¦ 	0166 	&brvbar; 	Broken bar 	0102
local_print(167). % §#U+00A7 	§ 	0167 	&sect; 	Section sign 	0103
local_print(168). % ¨#U+00A8 	¨ 	0168 	&uml; 	Diaeresis (Umlaut) 	0104
local_print(169). % ©#U+00A9 	© 	0169 	&copy; 	Copyright sign 	0105
local_print(170). % ª#U+00AA 	ª 	0170 	&ordf; 	Feminine Ordinal Indicator 	0106
local_print(171). % «#U+00AB 	« 	0171 	&laquo; 	Left-pointing double angle quotation mark 	0107
local_print(172). % ¬#U+00AC 	¬ 	0172 	&not; 	Not sign 	0108
local_print(173). % ­#U+00AD 		0173 	&shy; 	Soft hyphen 	0109
local_print(174). % ®#U+00AE 	® 	0174 	&reg; 	Registered sign 	0110
local_print(175). % ¯#U+00AF 	¯ 	0175 	&macr; 	macron 	0111
local_print(176). % °#U+00B0 	° 	0176 	&deg; 	Degree symbol 	0112
local_print(177). % ±#U+00B1 	± 	0177 	&plusmn; 	Plus-minus sign 	0113
local_print(178). % ²#U+00B2 	² 	0178 	&sup2; 	Superscript two 	0114
local_print(179). % ³#U+00B3 	³ 	0179 	&sup3; 	Superscript three 	0115
local_print(180). % ´#U+00B4 	´ 	0180 	&acute; 	Acute accent 	0116
local_print(181). % µ#U+00B5 	µ 	0181 	&micro; 	Micro sign 	0117
local_print(182). % ¶#U+00B6 	¶ 	0182 	&para; 	Pilcrow sign 	0118
local_print(183). % ·#U+00B7 	· 	0183 	&middot; 	Middle dot 	0119
local_print(184). % ¸#U+00B8 	¸ 	0184 	&cedil; 	Cedilla 	0120
local_print(185). % ¹#U+00B9 	¹ 	0185 	&sup1; 	Superscript one 	0121
local_print(186). % º#U+00BA 	º 	0186 	&ordm; 	Masculine ordinal indicator 	0122
local_print(187). % »#U+00BB 	» 	0187 	&raquo; 	Right-pointing double angle quotation mark 	0123
local_print(188). % ¼#U+00BC 	¼ 	0188 	&frac14; 	Vulgar fraction one quarter 	0124
local_print(189). % ½#U+00BD 	½ 	0189 	&frac12; 	Vulgar fraction one half 	0125
local_print(190). % ¾#U+00BE 	¾ 	0190 	&frac34; 	Vulgar fraction three quarters 	0126
local_print(191). % ¿#U+00BF 	¿ 	0191 	&iquest; 	Inverted Question Mark 	0127
local_print(192). % À#U+00C0 	À 	0192 	&Agrave; 	Latin Capital Letter A with grave 	0128
local_print(193). % Á#U+00C1 	Á 	0193 	&Aacute; 	Latin Capital letter A with acute 	0129
local_print(194). % Â#U+00C2 	Â 	0194 	&Acirc; 	Latin Capital letter A with circumflex 	0130
local_print(195). % Ã#U+00C3 	Ã 	0195 	&Atilde; 	Latin Capital letter A with tilde 	0131
local_print(196). % Ä#U+00C4 	Ä 	0196 	&Auml; 	Latin Capital letter A with diaeresis 	0132
local_print(197). % Å#U+00C5 	Å 	0197 	&Aring; 	Latin Capital letter A with ring above 	0133
local_print(198). % Æ#U+00C6 	Æ 	0198 	&AElig; 	Latin Capital letter Æ 	0134
local_print(199). % Ç#U+00C7 	Ç 	0199 	&Ccedil; 	Latin Capital letter C with cedilla 	0135
local_print(200). % È#U+00C8 	È 	0200 	&Egrave; 	Latin Capital letter E with grave 	0136
local_print(201). % É#U+00C9 	É 	0201 	&Eacute; 	Latin Capital letter E with acute 	0137
local_print(202). % Ê#U+00CA 	Ê 	0202 	&Ecirc; 	Latin Capital letter E with circumflex 	0138
local_print(203). % Ë#U+00CB 	Ë 	0203 	&Euml; 	Latin Capital letter E with diaeresis 	0139
local_print(204). % Ì#U+00CC 	Ì 	0204 	&Igrave; 	Latin Capital letter I with grave 	0140
local_print(205). % Í#U+00CD 	Í 	0205 	&Iacute; 	Latin Capital letter I with acute 	0141
local_print(206). % Î#U+00CE 	Î 	0206 	&Icirc; 	Latin Capital letter I with circumflex 	0142
local_print(207). % Ï#U+00CF 	Ï 	0207 	&Iuml; 	Latin Capital letter I with diaeresis 	0143
local_print(208). % Ð#U+00D0 	Ð 	0208 	&ETH; 	Latin Capital letter Eth 	0144
local_print(209). % Ñ#U+00D1 	Ñ 	0209 	&Ntilde; 	Latin Capital letter N with tilde 	0145
local_print(210). % Ò#U+00D2 	Ò 	0210 	&Ograve; 	Latin Capital letter O with grave 	0146
local_print(211). % Ó#U+00D3 	Ó 	0211 	&Oacute; 	Latin Capital letter O with acute 	0147
local_print(212). % Ô#U+00D4 	Ô 	0212 	&Ocirc; 	Latin Capital letter O with circumflex 	0148
local_print(213). % Õ#U+00D5 	Õ 	0213 	&Otilde; 	Latin Capital letter O with tilde 	0149
local_print(214). % Ö#U+00D6 	Ö 	0214 	&Ouml; 	Latin Capital letter O with diaeresis 	0150
local_print(215). % ×#U+00D7 	× 	0215 	&times; 	Multiplication sign 	0151
local_print(216). % Ø#U+00D8 	Ø 	0216 	&Oslash; 	Latin Capital letter O with stroke 	0152
local_print(217). % Ù#U+00D9 	Ù 	0217 	&Ugrave; 	Latin Capital letter U with grave 	0153
local_print(218). % Ú#U+00DA 	Ú 	0218 	&Uacute; 	Latin Capital letter U with acute 	0154
local_print(219). % Û#U+00DB 	Û 	0219 	&Ucirc; 	Latin Capital Letter U with circumflex 	0155
local_print(220). % Ü#U+00DC 	Ü 	0220 	&Uuml; 	Latin Capital Letter U with diaeresis 	0156
local_print(221). % Ý#U+00DD 	Ý 	0221 	&Yacute; 	Latin Capital Letter Y with acute 	0157
local_print(222). % Þ#U+00DE 	Þ 	0222 	&THORN; 	Latin Capital Letter Thorn 	0158
local_print(223). % ß#U+00DF 	ß 	0223 	&szlig; 	Latin Small Letter sharp S 	0159
local_print(224). % à#U+00E0 	à 	0224 	&agrave; 	Latin Small Letter A with grave 	0160
local_print(225). % á#U+00E1 	á 	0225 	&aacute; 	Latin Small Letter A with acute 	0161
local_print(226). % â#U+00E2 	â 	0226 	&acirc; 	Latin Small Letter A with circumflex 	0162
local_print(227). % ã#U+00E3 	ã 	0227 	&atilde; 	Latin Small Letter A with tilde 	0163
local_print(228). % ä#U+00E4 	ä 	0228 	&auml; 	Latin Small Letter A with diaeresis 	0164
local_print(229). % å#U+00E5 	å 	0229 	&aring; 	Latin Small Letter A with ring above 	0165
local_print(230). % æ#U+00E6 	æ 	0230 	&aelig; 	Latin Small Letter Æ 	0166
local_print(231). % ç#U+00E7 	ç 	0231 	&ccedil; 	Latin Small Letter C with cedilla 	0167
local_print(232). % è#U+00E8 	è 	0232 	&egrave; 	Latin Small Letter E with grave 	0168
local_print(233). % é#U+00E9 	é 	0233 	&eacute; 	Latin Small Letter E with acute 	0169
local_print(234). % ê#U+00EA 	ê 	0234 	&ecirc; 	Latin Small Letter E with circumflex 	0170
local_print(235). % ë#U+00EB 	ë 	0235 	&euml; 	Latin Small Letter E with diaeresis 	0171
local_print(236). % ì#U+00EC 	ì 	0236 	&igrave; 	Latin Small Letter I with grave 	0172
local_print(237). % í#U+00ED 	í 	0237 	&iacute; 	Latin Small Letter I with acute 	0173
local_print(238). % î#U+00EE 	î 	0238 	&icirc; 	Latin Small Letter I with circumflex 	0174
local_print(239). % ï#U+00EF 	ï 	0239 	&iuml; 	Latin Small Letter I with diaeresis 	0175
local_print(240). % ð#U+00F0 	ð 	0240 	&eth; 	Latin Small Letter Eth 	0176
local_print(241). % ñ#U+00F1 	ñ 	0241 	&ntilde; 	Latin Small Letter N with tilde 	0177
local_print(242). % ò#U+00F2 	ò 	0242 	&ograve; 	Latin Small Letter O with grave 	0178
local_print(243). % ó#U+00F3 	ó 	0243 	&oacute; 	Latin Small Letter O with acute 	0179
local_print(244). % ô#U+00F4 	ô 	0244 	&ocirc; 	Latin Small Letter O with circumflex 	0180
local_print(245). % õ#U+00F5 	õ 	0245 	&otilde; 	Latin Small Letter O with tilde 	0181
local_print(246). % ö#U+00F6 	ö 	0246 	&ouml; 	Latin Small Letter O with diaeresis 	0182
local_print(247). % ÷#U+00F7 	÷ 	0247 	&divide; 	Division sign 	0183
local_print(248). % ø#U+00F8 	ø 	0248 	&oslash; 	Latin Small Letter O with stroke 	0184
local_print(249). % ù#U+00F9 	ù 	0249 	&ugrave; 	Latin Small Letter U with grave 	0185
local_print(250). % ú#U+00FA 	ú 	0250 	&uacute; 	Latin Small Letter U with acute 	0186
local_print(251). % û#U+00FB 	û 	0251 	&ucirc; 	Latin Small Letter U with circumflex 	0187
local_print(252). % ü#U+00FC 	ü 	0252 	&uuml; 	Latin Small Letter U with diaeresis 	0188
local_print(253). % ý#U+00FD 	ý 	0253 	&yacute; 	Latin Small Letter Y with acute 	0189
local_print(254). % þ#U+00FE 	þ 	0254 	&thorn; 	Latin Small Letter Thorn 	0190
local_print(255). % ÿ#U+00FF 	ÿ 	0255 	&yuml; 	Latin Small Letter Y with diaeresis 	0191 
% https://en.wikipedia.org/wiki/List_of_Unicode_characters#
% Latin Extended-A#
local_print(256). % Ā#U+0100 	Ā 	256 	&Amacr; 	Latin Capital Letter A with macron 	0192
local_print(257). % ā#U+0101 	ā 	257 	&amacr; 	Latin Small Letter A with macron 	0193
local_print(258). % Ă#U+0102 	Ă 	258 	&Abreve; 	Latin Capital Letter A with breve 	0194
local_print(259). % ă#U+0103 	ă 	259 	&abreve; 	Latin Small Letter A with breve 	0195
local_print(260). % Ą#U+0104 	Ą 	260 	&Aogon; 	Latin Capital Letter A with ogonek 	0196
local_print(261). % ą#U+0105 	ą 	261 	&aogon; 	Latin Small Letter A with ogonek 	0197
local_print(262). % Ć#U+0106 	Ć 	262 	&Cacute; 	Latin Capital Letter C with acute 	0198
local_print(263). % ć#U+0107 	ć 	263 	&cacute; 	Latin Small Letter C with acute 	0199
local_print(264). % Ĉ#U+0108 	Ĉ 	264 	&Ccirc; 	Latin Capital Letter C with circumflex 	0200
local_print(265). % ĉ#U+0109 	ĉ 	265 	&ccirc; 	Latin Small Letter C with circumflex 	0201
local_print(266). % Ċ#U+010A 	Ċ 	266 	&Cdot; 	Latin Capital Letter C with dot above 	0202
local_print(267). % ċ#U+010B 	ċ 	267 	&cdot; 	Latin Small Letter C with dot above 	0203
local_print(268). % Č#U+010C 	Č 	268 	&Ccaron; 	Latin Capital Letter C with caron 	0204
local_print(269). % č#U+010D 	č 	269 	&ccaron; 	Latin Small Letter C with caron 	0205
local_print(270). % Ď#U+010E 	Ď 	270 	&Dcaron; 	Latin Capital Letter D with caron 	0206
local_print(271). % ď#U+010F 	ď 	271 	&dcaron; 	Latin Small Letter D with caron 	0207
local_print(272). % Đ#U+0110 	Đ 	272 	&Dstrok; 	Latin Capital Letter D with stroke 	0208
local_print(273). % đ#U+0111 	đ 	273 	&dstrok; 	Latin Small Letter D with stroke 	0209
local_print(274). % Ē#U+0112 	Ē 	274 	&Emacr; 	Latin Capital Letter E with macron 	0210
local_print(275). % ē#U+0113 	ē 	275 	&emacr; 	Latin Small Letter E with macron 	0211
local_print(276). % Ĕ#U+0114 	Ĕ 	276 	&Ebreve; 	Latin Capital Letter E with breve 	0212
local_print(277). % ĕ#U+0115 	ĕ 	277 	&ebreve; 	Latin Small Letter E with breve 	0213
local_print(278). % Ė#U+0116 	Ė 	278 	&Edot; 	Latin Capital Letter E with dot above 	0214
local_print(279). % ė#U+0117 	ė 	279 	&edot; 	Latin Small Letter E with dot above 	0215
local_print(280). % Ę#U+0118 	Ę 	280 	&Eogon; 	Latin Capital Letter E with ogonek 	0216
local_print(281). % ę#U+0119 	ę 	281 	&eogon; 	Latin Small Letter E with ogonek 	0217
local_print(282). % Ě#U+011A 	Ě 	282 	&Ecaron; 	Latin Capital Letter E with caron 	0218
local_print(283). % ě#U+011B 	ě 	283 	&ecaron; 	Latin Small Letter E with caron 	0219
local_print(284). % Ĝ#U+011C 	Ĝ 	284 	&Gcirc; 	Latin Capital Letter G with circumflex 	0220
local_print(285). % ĝ#U+011D 	ĝ 	285 	&gcirc; 	Latin Small Letter G with circumflex 	0221
local_print(286). % Ğ#U+011E 	Ğ 	286 	&Gbreve; 	Latin Capital Letter G with breve 	0222
local_print(287). % ğ#U+011F 	ğ 	287 	&gbreve; 	Latin Small Letter G with breve 	0223
local_print(288). % Ġ#U+0120 	Ġ 	288 	&Gdot; 	Latin Capital Letter G with dot above 	0224
local_print(289). % ġ#U+0121 	ġ 	289 	&gdot; 	Latin Small Letter G with dot above 	0225
local_print(290). % Ģ#U+0122 	Ģ 	290 	&Gcedil; 	Latin Capital Letter G with cedilla 	0226
local_print(291). % ģ#U+0123 	ģ 	291 		Latin Small Letter G with cedilla 	0227
local_print(292). % Ĥ#U+0124 	Ĥ 	292 	&Hcirc; 	Latin Capital Letter H with circumflex 	0228
local_print(293). % ĥ#U+0125 	ĥ 	293 	&hcirc; 	Latin Small Letter H with circumflex 	0229
local_print(294). % Ħ#U+0126 	Ħ 	294 	&Hstrok; 	Latin Capital Letter H with stroke 	0230
local_print(295). % ħ#U+0127 	ħ 	295 	&hstrok; 	Latin Small Letter H with stroke 	0231
local_print(296). % Ĩ#U+0128 	Ĩ 	296 	&Itilde; 	Latin Capital Letter I with tilde 	0232
local_print(297). % ĩ#U+0129 	ĩ 	297 	&itilde; 	Latin Small Letter I with tilde 	0233
local_print(298). % Ī#U+012A 	Ī 	298 	&Imacr; 	Latin Capital Letter I with macron 	0234
local_print(299). % ī#U+012B 	ī 	299 	&imacr; 	Latin Small Letter I with macron 	0235
local_print(300). % Ĭ#U+012C 	Ĭ 	300 	&Ibreve; 	Latin Capital Letter I with breve 	0236
local_print(301). % ĭ#U+012D 	ĭ 	301 	&ibreve; 	Latin Small Letter I with breve 	0237
local_print(302). % Į#U+012E 	Į 	302 	&Iogon; 	Latin Capital Letter I with ogonek 	0238
local_print(303). % į#U+012F 	į 	303 	&iogon; 	Latin Small Letter I with ogonek 	0239
local_print(304). % İ#U+0130 	İ 	304 	&Idot; 	Latin Capital Letter I with dot above 	0240
local_print(305). % ı#U+0131 	ı 	305 	&inodot; 	Latin Small Letter dotless I 	0241
local_print(306). % Ĳ#U+0132 	Ĳ 	306 	&IJlig; 	Latin Capital Ligature IJ 	0242
local_print(307). % ĳ#U+0133 	ĳ 	307 	&ijlig; 	Latin Small Ligature IJ 	0243
local_print(308). % Ĵ#U+0134 	Ĵ 	308 	&Jcirc; 	Latin Capital Letter J with circumflex 	0244
local_print(309). % ĵ#U+0135 	ĵ 	309 	&jcirc; 	Latin Small Letter J with circumflex 	0245
local_print(310). % Ķ#U+0136 	Ķ 	310 	&Kcedil; 	Latin Capital Letter K with cedilla 	0246
local_print(311). % ķ#U+0137 	ķ 	311 	&kcedil; 	Latin Small Letter K with cedilla 	0247
local_print(312). % ĸ#U+0138 	ĸ 	312 		Latin Small Letter Kra 	0248
local_print(313). % Ĺ#U+0139 	Ĺ 	313 	&Lacute; 	Latin Capital Letter L with acute 	0249
local_print(314). % ĺ#U+013A 	ĺ 	314 	&lacute; 	Latin Small Letter L with acute 	0250
local_print(315). % Ļ#U+013B 	Ļ 	315 	&Lcedil; 	Latin Capital Letter L with cedilla 	0251
local_print(316). % ļ#U+013C 	ļ 	316 	&lcedil; 	Latin Small Letter L with cedilla 	0252
local_print(317). % Ľ#U+013D 	Ľ 	317 	&Lcaron; 	Latin Capital Letter L with caron 	0253
local_print(318). % ľ#U+013E 	ľ 	318 	&lcaron; 	Latin Small Letter L with caron 	0254
local_print(319). % Ŀ#U+013F 	Ŀ 	319 		Latin Capital Letter L with middle dot 	0255
local_print(320). % ŀ#U+0140 	ŀ 	320 		Latin Small Letter L with middle dot 	0256
local_print(321). % Ł#U+0141 	Ł 	321 	&Lstrok; 	Latin Capital Letter L with stroke 	0257
local_print(322). % ł#U+0142 	ł 	322 	&lstrok; 	Latin Small Letter L with stroke 	0258
local_print(323). % Ń#U+0143 	Ń 	323 	&Nacute; 	Latin Capital Letter N with acute 	0259
local_print(324). % ń#U+0144 	ń 	324 	&nacute; 	Latin Small Letter N with acute 	0260
local_print(325). % Ņ#U+0145 	Ņ 	325 	&Ncedil; 	Latin Capital Letter N with cedilla 	0261
local_print(326). % ņ#U+0146 	ņ 	326 	&ncedil; 	Latin Small Letter N with cedilla 	0262
local_print(327). % Ň#U+0147 	Ň 	327 	&Ncaron; 	Latin Capital Letter N with caron 	0263
local_print(328). % ň#U+0148 	ň 	328 	&ncaron; 	Latin Small Letter N with caron 	0264
local_print(329). % ŉ#U+0149 	ŉ 	329 		Latin Small Letter N preceded by apostrophe[1] 	0265
local_print(330). % Ŋ#U+014A 	Ŋ 	330 	&ENG; 	Latin Capital Letter Eng 	0266
local_print(331). % ŋ#U+014B 	ŋ 	331 	&eng; 	Latin Small Letter Eng 	0267
local_print(332). % Ō#U+014C 	Ō 	332 	&Omacr; 	Latin Capital Letter O with macron 	0268
local_print(333). % ō#U+014D 	ō 	333 	&omacr; 	Latin Small Letter O with macron 	0269
local_print(334). % Ŏ#U+014E 	Ŏ 	334 	&Obreve; 	Latin Capital Letter O with breve 	0270
local_print(335). % ŏ#U+014F 	ŏ 	335 	&obreve; 	Latin Small Letter O with breve 	0271
local_print(336). % Ő#U+0150 	Ő 	336 	&Odblac; 	Latin Capital Letter O with double acute 	0272
local_print(337). % ő#U+0151 	ő 	337 	&odblac; 	Latin Small Letter O with double acute 	0273
local_print(338). % Œ#U+0152 	Œ 	338 	&OElig; 	Latin Capital Ligature OE 	0274
local_print(339). % œ#U+0153 	œ 	339 	&oelig; 	Latin Small Ligature OE 	0275
local_print(340). % Ŕ#U+0154 	Ŕ 	340 	&Racute; 	Latin Capital Letter R with acute 	0276
local_print(341). % ŕ#U+0155 	ŕ 	341 	&racute; 	Latin Small Letter R with acute 	0277
local_print(342). % Ŗ#U+0156 	Ŗ 	342 	&Rcedil; 	Latin Capital Letter R with cedilla 	0278
local_print(343). % ŗ#U+0157 	ŗ 	343 	&rcedil; 	Latin Small Letter R with cedilla 	0279
local_print(344). % Ř#U+0158 	Ř 	344 	&Rcaron; 	Latin Capital Letter R with caron 	0280
local_print(345). % ř#U+0159 	ř 	345 	&rcaron; 	Latin Small Letter R with caron 	0281
local_print(346). % Ś#U+015A 	Ś 	346 	&Sacute; 	Latin Capital Letter S with acute 	0282
local_print(347). % ś#U+015B 	ś 	347 	&sacute; 	Latin Small Letter S with acute 	0283
local_print(348). % Ŝ#U+015C 	Ŝ 	348 	&Scirc; 	Latin Capital Letter S with circumflex 	0284
local_print(349). % ŝ#U+015D 	ŝ 	349 	&scirc; 	Latin Small Letter S with circumflex 	0285
local_print(350). % Ş#U+015E 	Ş 	350 	&Scedil; 	Latin Capital Letter S with cedilla 	0286
local_print(351). % ş#U+015F 	ş 	351 	&scedil; 	Latin Small Letter S with cedilla 	0287
local_print(352). % Š#U+0160 	Š 	352 	&Scaron; 	Latin Capital Letter S with caron 	0288
local_print(353). % š#U+0161 	š 	353 	&scaron; 	Latin Small Letter S with caron 	0289
local_print(354). % Ţ#U+0162 	Ţ 	354 	&Tcedil; 	Latin Capital Letter T with cedilla 	0290
local_print(355). % ţ#U+0163 	ţ 	355 	&tcedil; 	Latin Small Letter T with cedilla 	0291
local_print(356). % Ť#U+0164 	Ť 	356 	&Tcaron; 	Latin Capital Letter T with caron 	0292
local_print(357). % ť#U+0165 	ť 	357 	&tcaron; 	Latin Small Letter T with caron 	0293
local_print(358). % Ŧ#U+0166 	Ŧ 	358 	&Tstrok; 	Latin Capital Letter T with stroke 	0294
local_print(359). % ŧ#U+0167 	ŧ 	359 	&tstrok; 	Latin Small Letter T with stroke 	0295
local_print(360). % Ũ#U+0168 	Ũ 	360 	&Utilde; 	Latin Capital Letter U with tilde 	0296
local_print(361). % ũ#U+0169 	ũ 	361 	&utilde; 	Latin Small Letter U with tilde 	0297
local_print(362). % Ū#U+016A 	Ū 	362 	&Umacr; 	Latin Capital Letter U with macron 	0298
local_print(363). % ū#U+016B 	ū 	363 	&umacr; 	Latin Small Letter U with macron 	0299
local_print(364). % Ŭ#U+016C 	Ŭ 	364 	&Ubreve; 	Latin Capital Letter U with breve 	0300
local_print(365). % ŭ#U+016D 	ŭ 	365 	&ubreve; 	Latin Small Letter U with breve 	0301
local_print(366). % Ů#U+016E 	Ů 	366 	&Uring; 	Latin Capital Letter U with ring above 	0302
local_print(367). % ů#U+016F 	ů 	367 	&uring; 	Latin Small Letter U with ring above 	0303
local_print(368). % Ű#U+0170 	Ű 	368 	&Udblac; 	Latin Capital Letter U with double acute 	0304
local_print(369). % ű#U+0171 	ű 	369 	&udblac; 	Latin Small Letter U with double acute 	0305
local_print(370). % Ų#U+0172 	Ų 	370 	&Uogon; 	Latin Capital Letter U with ogonek 	0306
local_print(371). % ų#U+0173 	ų 	371 	&uogon; 	Latin Small Letter U with ogonek 	0307
local_print(372). % Ŵ#U+0174 	Ŵ 	372 	&Wcirc; 	Latin Capital Letter W with circumflex 	0308
local_print(373). % ŵ#U+0175 	ŵ 	373 	&wcirc; 	Latin Small Letter W with circumflex 	0309
local_print(374). % Ŷ#U+0176 	Ŷ 	374 	&Ycirc; 	Latin Capital Letter Y with circumflex 	0310
local_print(375). % ŷ#U+0177 	ŷ 	375 	&ycirc; 	Latin Small Letter Y with circumflex 	0311
local_print(376). % Ÿ#U+0178 	Ÿ 	376 	&Yuml; 	Latin Capital Letter Y with diaeresis 	0312
local_print(377). % Ź#U+0179 	Ź 	377 	&Zacute; 	Latin Capital Letter Z with acute 	0313
local_print(378). % ź#U+017A 	ź 	378 	&zacute; 	Latin Small Letter Z with acute 	0314
local_print(379). % Ż#U+017B 	Ż 	379 	&Zdot; 	Latin Capital Letter Z with dot above 	0315
local_print(380). % ż#U+017C 	ż 	380 	&zdot; 	Latin Small Letter Z with dot above 	0316
local_print(381). % Ž#U+017D 	Ž 	381 	&Zcaron; 	Latin Capital Letter Z with caron 	0317
local_print(382). % ž#U+017E 	ž 	382 	&zcaron; 	Latin Small Letter Z with caron 	0318
local_print(383). % ſ#U+017F 	ſ 	383 		Latin Small Letter long S 	0319
% https://en.wikipedia.org/wiki/List_of_Unicode_characters#
% Latin Extended-B#
local_print(384). % ƀ#U+0180 	ƀ	384 	Latin Small Letter B with stroke 	·
local_print(385). % Ɓ#U+0181 	Ɓ	385 	Latin Capital Letter B with hook
local_print(386). % Ƃ#U+0182 	Ƃ	386 	Latin Capital Letter B with top bar
local_print(387). % ƃ#U+0183 	ƃ	387 	Latin Small Letter B with top bar
local_print(388). % Ƅ#U+0184 	Ƅ	388 	Latin Capital Letter Tone Six
local_print(389). % ƅ#U+0185 	ƅ	389 	Latin Small Letter Tone Six
local_print(390). % Ɔ#U+0186 	Ɔ	390 	Latin Capital Letter Open O
local_print(391). % Ƈ#U+0187 	Ƈ	391 	Latin Capital Letter C with hook
local_print(392). % ƈ#U+0188 	ƈ	392 	Latin Small Letter C with hook
local_print(393). % Ɖ#U+0189 	Ɖ	393 	Latin Capital Letter African D
local_print(394). % Ɗ#U+018A 	Ɗ	394 	Latin Capital Letter D with hook
local_print(395). % Ƌ#U+018B 	Ƌ	395 	Latin Capital Letter D with top bar
local_print(396). % ƌ#U+018C 	ƌ	396 	Latin Small Letter D with top bar
local_print(397). % ƍ#U+018D 	ƍ	397 	Latin Small Letter Turned Delta
local_print(398). % Ǝ#U+018E 	Ǝ	398 	Latin Capital Letter Reversed E
local_print(399). % Ə#U+018F 	Ə	399 	Latin Capital Letter Schwa 	0320 	for Azerbaijani
local_print(400). % Ɛ#U+0190 	Ɛ	400 	Latin Capital Letter Open E 	·
local_print(401). % Ƒ#U+0191 	Ƒ	401 	Latin Capital Letter F with hook
local_print(402). % ƒ#U+0192 	ƒ	402 	Latin Small Letter F with hook 	0321 	in WGL4
local_print(403). % Ɠ#U+0193 	Ɠ	403 	Latin Capital Letter G with hook 	·
local_print(404). % Ɣ#U+0194 	Ɣ	404 	Latin Capital Letter Gamma
local_print(405). % ƕ#U+0195 	ƕ	405 	Latin Small Letter HV
local_print(406). % Ɩ#U+0196 	Ɩ	406 	Latin Capital Letter Iota
local_print(407). % Ɨ#U+0197 	Ɨ	407 	Latin Capital Letter I with stroke
local_print(408). % Ƙ#U+0198 	Ƙ	408 	Latin Capital Letter K with hook
local_print(409). % ƙ#U+0199 	ƙ	409 	Latin Small Letter K with hook
local_print(410). % ƚ#U+019A 	ƚ	410 	Latin Small Letter L with bar
local_print(411). % ƛ#U+019B 	ƛ	411 	Latin Small Letter Lambda with stroke
local_print(412). % Ɯ#U+019C 	Ɯ	412 	Latin Capital Letter Turned M
local_print(413). % Ɲ#U+019D 	Ɲ	413 	Latin Capital Letter N with left hook
local_print(414). % ƞ#U+019E 	ƞ	414 	Latin Small Letter N with long right leg
local_print(415). % Ɵ#U+019F 	Ɵ	415 	Latin Capital Letter O with middle tilde
local_print(416). % Ơ#U+01A0 	Ơ	416 	Latin Capital Letter O with horn
local_print(417). % ơ#U+01A1 	ơ	417 	Latin Small Letter O with horn
local_print(418). % Ƣ#U+01A2 	Ƣ	418 	Latin Capital Letter OI (= Latin Capital Letter Gha)
local_print(419). % ƣ#U+01A3 	ƣ	419 	Latin Small Letter OI (= Latin Small Letter Gha)
local_print(420). % Ƥ#U+01A4 	Ƥ	420 	Latin Capital Letter P with hook
local_print(421). % ƥ#U+01A5 	ƥ	421 	Latin Small Letter P with hook
local_print(422). % Ʀ#U+01A6 	Ʀ	422 	Latin Letter YR
local_print(423). % Ƨ#U+01A7 	Ƨ	423 	Latin Capital Letter Tone Two
local_print(424). % ƨ#U+01A8 	ƨ	424 	Latin Small Letter Tone Two
local_print(425). % Ʃ#U+01A9 	Ʃ	425 	Latin Capital Letter Esh
local_print(426). % ƪ#U+01AA 	ƪ	426 	Latin Letter Reversed Esh Loop
local_print(427). % ƫ#U+01AB 	ƫ	427 	Latin Small Letter T with palatal hook
local_print(428). % Ƭ#U+01AC 	Ƭ	428 	Latin Capital Letter T with hook
local_print(429). % ƭ#U+01AD 	ƭ	429 	Latin Small Letter T with hook
local_print(430). % Ʈ#U+01AE 	Ʈ	430 	Latin Capital Letter T with retroflex hook
local_print(431). % Ư#U+01AF 	Ư	431 	Latin Capital Letter U with horn
local_print(432). % ư#U+01B0 	ư	432 	Latin Small Letter U with horn
local_print(433). % Ʊ#U+01B1 	Ʊ	433 	Latin Capital Letter Upsilon
local_print(434). % Ʋ#U+01B2 	Ʋ	434 	Latin Capital Letter V with hook
local_print(435). % Ƴ#U+01B3 	Ƴ	435 	Latin Capital Letter Y with hook
local_print(436). % ƴ#U+01B4 	ƴ	436 	Latin Small Letter Y with hook
local_print(437). % Ƶ#U+01B5 	Ƶ	437 	Latin Capital Letter Z with stroke
local_print(438). % ƶ#U+01B6 	ƶ	438 	Latin Small Letter Z with stroke
local_print(439). % Ʒ#U+01B7 	Ʒ	439 	Latin Capital Letter Ezh 	0322 	for Sami
local_print(440). % Ƹ#U+01B8 	Ƹ	440 	Latin Capital Letter Ezh reversed 	·
local_print(441). % ƹ#U+01B9 	ƹ	441 	Latin Small Letter Ezh reversed
local_print(442). % ƺ#U+01BA 	ƺ	442 	Latin Small Letter Ezh with tail
local_print(443). % ƻ#U+01BB 	ƻ	443 	Latin Letter Two with stroke
local_print(444). % Ƽ#U+01BC 	Ƽ	444 	Latin Capital Letter Tone Five
local_print(445). % ƽ#U+01BD 	ƽ	445 	Latin Small Letter Tone Five
local_print(446). % ƾ#U+01BE 	ƾ	446 	Latin Letter Inverted Glottal Stop with stroke
local_print(447). % ƿ#U+01BF 	ƿ	447 	Latin Letter Wynn
local_print(448). % ǀ#U+01C0 	ǀ	448 	Latin Letter Dental Click
local_print(449). % ǁ#U+01C1 	ǁ	449 	Latin Letter Lateral Click
local_print(450). % ǂ#U+01C2 	ǂ	450 	Latin Letter Alveolar Click
local_print(451). % ǃ#U+01C3 	ǃ	451 	Latin Letter Retroflex ClickCroatian
local_print(452). % Ǆ#U+01C4 	Ǆ	452 	Latin Capital Letter DZ with caron
local_print(453). % ǅ#U+01C5 	ǅ	453 	Latin Capital Letter D with Small Letter Z with caron
local_print(454). % ǆ#U+01C6 	ǆ	454 	Latin Small Letter DZ with caron
local_print(455). % Ǉ#U+01C7 	Ǉ	455 	Latin Capital Letter LJ
local_print(456). % ǈ#U+01C8 	ǈ	456 	Latin Capital Letter L with Small Letter J
local_print(457). % ǉ#U+01C9 	ǉ	457 	Latin Small Letter LJ
local_print(458). % Ǌ#U+01CA 	Ǌ	458 	Latin Capital Letter NJ
local_print(459). % ǋ#U+01CB 	ǋ	459 	Latin Capital Letter N with Small Letter J
local_print(460). % ǌ#U+01CC 	ǌ	460 	Latin Small Letter NJ
local_print(461). % Ǎ#U+01CD 	Ǎ	461 	Latin Capital Letter A with caron
local_print(462). % ǎ#U+01CE 	ǎ	462 	Latin Small Letter A with caron
local_print(463). % Ǐ#U+01CF 	Ǐ	463 	Latin Capital Letter I with caron
local_print(464). % ǐ#U+01D0 	ǐ	464 	Latin Small Letter I with caron
local_print(465). % Ǒ#U+01D1 	Ǒ	465 	Latin Capital Letter O with caron
local_print(466). % ǒ#U+01D2 	ǒ	466 	Latin Small Letter O with caron
local_print(467). % Ǔ#U+01D3 	Ǔ	467 	Latin Capital Letter U with caron
local_print(468). % ǔ#U+01D4 	ǔ	468 	Latin Small Letter U with caron
local_print(469). % Ǖ#U+01D5 	Ǖ	469 	Latin Capital Letter U with diaeresis and macron
local_print(470). % ǖ#U+01D6 	ǖ	470 	Latin Small Letter U with diaeresis and macron
local_print(471). % Ǘ#U+01D7 	Ǘ	471 	Latin Capital Letter U with diaeresis and acute
local_print(472). % ǘ#U+01D8 	ǘ	472 	Latin Small Letter U with diaeresis and acute
local_print(473). % Ǚ#U+01D9 	Ǚ	473 	Latin Capital Letter U with diaeresis and caron
local_print(474). % ǚ#U+01DA 	ǚ	474 	Latin Small Letter U with diaeresis and caron
local_print(475). % Ǜ#U+01DB 	Ǜ	475 	Latin Capital Letter U with diaeresis and grave
local_print(476). % ǜ#U+01DC 	ǜ	476 	Latin Small Letter U with diaeresis and grave
local_print(477). % ǝ#U+01DD 	ǝ	477 	Latin Small Letter Turned E
local_print(478). % Ǟ#U+01DE 	Ǟ	478 	Latin Capital Letter A with diaeresis and macron 	0323 	for Sami
local_print(479). % ǟ#U+01DF 	ǟ	479 	Latin Small Letter A with diaeresis and macron 	0324
local_print(480). % Ǡ#U+01E0 	Ǡ	480 	Latin Capital Letter A with dot above and macron 	0325
local_print(481). % ǡ#U+01E1 	ǡ	481 	Latin Small Letter A with dot above and macron 	0326
local_print(482). % Ǣ#U+01E2 	Ǣ	482 	Latin Capital Letter Æ with macron 	0327
local_print(483). % ǣ#U+01E3 	ǣ	483 	Latin Small Letter Æ with macron 	0328
local_print(484). % Ǥ#U+01E4 	Ǥ	484 	Latin Capital Letter G with stroke 	0329
local_print(485). % ǥ#U+01E5 	ǥ	485 	Latin Small Letter G with stroke 	0330
local_print(486). % Ǧ#U+01E6 	Ǧ	486 	Latin Capital Letter G with caron 	0331
local_print(487). % ǧ#U+01E7 	ǧ	487 	Latin Small Letter G with caron 	0332
local_print(488). % Ǩ#U+01E8 	Ǩ	488 	Latin Capital Letter K with caron 	0333
local_print(489). % ǩ#U+01E9 	ǩ	489 	Latin Small Letter K with caron 	0334
local_print(490). % Ǫ#U+01EA 	Ǫ	490 	Latin Capital Letter O with ogonek 	0335
local_print(491). % ǫ#U+01EB 	ǫ	491 	Latin Small Letter O with ogonek 	0336
local_print(492). % Ǭ#U+01EC 	Ǭ	492 	Latin Capital Letter O with ogonek and macron 	0337
local_print(493). % ǭ#U+01ED 	ǭ	493 	Latin Small Letter O with ogonek and macron 	0338
local_print(494). % Ǯ#U+01EE 	Ǯ	494 	Latin Capital Letter Ezh with caron 	0339
local_print(495). % ǯ#U+01EF 	ǯ	495 	Latin Small Letter Ezh with caron 	0340
local_print(496). % ǰ#U+01F0 	ǰ	496 	Latin Small Letter J with caron 	·
local_print(497). % Ǳ#U+01F1 	Ǳ	497 	Latin Capital Letter DZ
local_print(498). % ǲ#U+01F2 	ǲ	498 	Latin Capital Letter D with Small Letter Z
local_print(499). % ǳ#U+01F3 	ǳ	499 	Latin Small Letter DZ
local_print(500). % Ǵ#U+01F4 	Ǵ	500 	Latin Capital Letter G with acute
local_print(501). % ǵ#U+01F5 	ǵ	501 	Latin Small Letter G with acute
local_print(502). % Ƕ#U+01F6 	Ƕ	502 	Latin Capital Letter Hwair
local_print(503). % Ƿ#U+01F7 	Ƿ	503 	Latin Capital Letter Wynn
local_print(504). % Ǹ#U+01F8 	Ǹ	504 	Latin Capital Letter N with grave
local_print(505). % ǹ#U+01F9 	ǹ	505 	Latin Small Letter N with grave
local_print(506). % Ǻ#U+01FA 	Ǻ	506 	Latin Capital Letter A with ring above and acute 	0341 	in WGL4
local_print(507). % ǻ#U+01FB 	ǻ	507 	Latin Small Letter A with ring above and acute 	0342
local_print(508). % Ǽ#U+01FC 	Ǽ	508 	Latin Capital Letter Æ with acute 	0343
local_print(509). % ǽ#U+01FD 	ǽ	509 	Latin Small Letter Æ with acute 	0344
local_print(510). % Ǿ#U+01FE 	Ǿ	510 	Latin Capital Letter O with stroke and acute 	0345
local_print(511). % ǿ#U+01FF 	ǿ	511 	Latin Small Letter O with stroke and acute 	0346
local_print(512). % Ȁ#U+0200 	Ȁ	512 	Latin Capital Letter A with double grave 	·
local_print(513). % ȁ#U+0201 	ȁ	513 	Latin Small Letter A with double grave
local_print(514). % Ȃ#U+0202 	Ȃ	514 	Latin Capital Letter A with inverted breve
local_print(515). % ȃ#U+0203 	ȃ	515 	Latin Small Letter A with inverted breve
local_print(516). % Ȅ#U+0204 	Ȅ	516 	Latin Capital Letter E with double grave
local_print(517). % ȅ#U+0205 	ȅ	517 	Latin Small Letter E with double grave
local_print(518). % Ȇ#U+0206 	Ȇ	518 	Latin Capital Letter E with inverted breve
local_print(519). % ȇ#U+0207 	ȇ	519 	Latin Small Letter E with inverted breve
local_print(520). % Ȉ#U+0208 	Ȉ	520 	Latin Capital Letter I with double grave
local_print(521). % ȉ#U+0209 	ȉ	521 	Latin Small Letter I with double grave
local_print(522). % Ȋ#U+020A 	Ȋ	522 	Latin Capital Letter I with inverted breve
local_print(523). % ȋ#U+020B 	ȋ	523 	Latin Small Letter I with inverted breve
local_print(524). % Ȍ#U+020C 	Ȍ	524 	Latin Capital Letter O with double grave
local_print(525). % ȍ#U+020D 	ȍ	525 	Latin Small Letter O with double grave
local_print(526). % Ȏ#U+020E 	Ȏ	526 	Latin Capital Letter O with inverted breve
local_print(527). % ȏ#U+020F 	ȏ	527 	Latin Small Letter O with inverted breve
local_print(528). % Ȑ#U+0210 	Ȑ	528 	Latin Capital Letter R with double grave
local_print(529). % ȑ#U+0211 	ȑ	529 	Latin Small Letter R with double grave
local_print(530). % Ȓ#U+0212 	Ȓ	530 	Latin Capital Letter R with inverted breve
local_print(531). % ȓ#U+0213 	ȓ	531 	Latin Small Letter R with inverted breve
local_print(532). % Ȕ#U+0214 	Ȕ	532 	Latin Capital Letter U with double grave
local_print(533). % ȕ#U+0215 	ȕ	533 	Latin Small Letter U with double grave
local_print(534). % Ȗ#U+0216 	Ȗ	534 	Latin Capital Letter U with inverted breve
local_print(535). % ȗ#U+0217 	ȗ	535 	Latin Small Letter U with inverted breve
local_print(536). % Ș#U+0218 	Ș	536 	Latin Capital Letter S with comma below 	0347 	for Romanian
local_print(537). % ș#U+0219 	ș	537 	Latin Small Letter S with comma below 	0348
local_print(538). % Ț#U+021A 	Ț	538 	Latin Capital Letter T with comma below 	0349
local_print(539). % ț#U+021B 	ț	539 	Latin Small Letter T with comma below 	0350
local_print(540). % Ȝ#U+021C 	Ȝ	540 	Latin Capital Letter Yogh 	·
local_print(541). % ȝ#U+021D 	ȝ	541 	Latin Small Letter Yogh
local_print(542). % Ȟ#U+021E 	Ȟ	542 	Latin Capital Letter H with caron 	0351 	for Finnish Romani, Scots
local_print(543). % ȟ#U+021F 	ȟ	543 	Latin Small Letter H with caron 	0352
local_print(544). % Ƞ#U+0220 	Ƞ	544 	Latin Capital Letter N with long right leg 	·
local_print(545). % ȡ#U+0221 	ȡ	545 	Latin Small Letter D with curl
local_print(546). % Ȣ#U+0222 	Ȣ	546 	Latin Capital Letter OU
local_print(547). % ȣ#U+0223 	ȣ	547 	Latin Small Letter OU
local_print(548). % Ȥ#U+0224 	Ȥ	548 	Latin Capital Letter Z with hook
local_print(549). % ȥ#U+0225 	ȥ	549 	Latin Small Letter Z with hook
local_print(550). % Ȧ#U+0226 	Ȧ	550 	Latin Capital Letter A with dot above
local_print(551). % ȧ#U+0227 	ȧ	551 	Latin Small Letter A with dot above
local_print(552). % Ȩ#U+0228 	Ȩ	552 	Latin Capital Letter E with cedilla
local_print(553). % ȩ#U+0229 	ȩ	553 	Latin Small Letter E with cedilla
local_print(554). % Ȫ#U+022A 	Ȫ	554 	Latin Capital Letter O with diaeresis and macron
local_print(555). % ȫ#U+022B 	ȫ	555 	Latin Small Letter O with diaeresis and macron
local_print(556). % Ȭ#U+022C 	Ȭ	556 	Latin Capital Letter O with tilde and macron
local_print(557). % ȭ#U+022D 	ȭ	557 	Latin Small Letter O with tilde and macron
local_print(558). % Ȯ#U+022E 	Ȯ	558 	Latin Capital Letter O with dot above
local_print(559). % ȯ#U+022F 	ȯ	559 	Latin Small Letter O with dot above
local_print(560). % Ȱ#U+0230 	Ȱ	560 	Latin Capital Letter O with dot above and macron
local_print(561). % ȱ#U+0231 	ȱ	561 	Latin Small Letter O with dot above and macron
local_print(562). % Ȳ#U+0232 	Ȳ	562 	Latin Capital Letter Y with macron
local_print(563). % ȳ#U+0233 	ȳ	563 	Latin Small Letter Y with macron
local_print(564). % ȴ#U+0234 	ȴ	564 	Latin Small Letter L with curl
local_print(565). % ȵ#U+0235 	ȵ	565 	Latin Small Letter N with curl
local_print(566). % ȶ#U+0236 	ȶ	566 	Latin Small Letter T with curl
local_print(567). % ȷ#U+0237 	ȷ	567 	Latin Small Letter Dotless J
local_print(568). % ȸ#U+0238 	ȸ	568 	Latin Small Letter DB Digraph
local_print(569). % ȹ#U+0239 	ȹ	569 	Latin Small Letter QP Digraph
local_print(570). % Ⱥ#U+023A 	Ⱥ	570 	Latin Capital Letter A with stroke
local_print(571). % Ȼ#U+023B 	Ȼ	571 	Latin Capital Letter C with stroke
local_print(572). % ȼ#U+023C 	ȼ	572 	Latin Small Letter C with stroke
local_print(573). % Ƚ#U+023D 	Ƚ	573 	Latin Capital Letter L with bar
local_print(574). % Ⱦ#U+023E 	Ⱦ	574 	Latin Capital Letter T with diagonal stroke
local_print(575). % ȿ#U+023F 	ȿ	575 	Latin Small Letter S with swash tail
local_print(576). % ɀ#U+0240 	ɀ	576 	Latin Small Letter Z with swash tail
local_print(577). % Ɂ#U+0241 	Ɂ	577 	Latin Capital Letter Glottal Stop
local_print(578). % ɂ#U+0242 	ɂ	578 	Latin Small Letter Glottal Stop
local_print(579). % Ƀ#U+0243 	Ƀ	579 	Latin Capital Letter B with stroke
local_print(580). % Ʉ#U+0244 	Ʉ	580 	Latin Capital Letter U bar
local_print(581). % Ʌ#U+0245 	Ʌ	581 	Latin Capital Letter Turned V
local_print(582). % Ɇ#U+0246 	Ɇ	582 	Latin Capital Letter E with stroke
local_print(583). % ɇ#U+0247 	ɇ	583 	Latin Small Letter E with stroke
local_print(584). % Ɉ#U+0248 	Ɉ	584 	Latin Capital Letter J with stroke
local_print(585). % ɉ#U+0249 	ɉ	585 	Latin Small Letter J with stroke
local_print(586). % Ɋ#U+024A 	Ɋ	586 	Latin Capital Letter Q with hook tail
local_print(587). % ɋ#U+024B 	ɋ	587 	Latin Small Letter Q with hook tail
local_print(588). % Ɍ#U+024C 	Ɍ	588 	Latin Capital Letter R with stroke
local_print(589). % ɍ#U+024D 	ɍ	589 	Latin Small Letter R with stroke
local_print(590). % Ɏ#U+024E 	Ɏ	590 	Latin Capital Letter Y with stroke
local_print(591). % ɏ#U+024F 	ɏ	591 	Latin Small Letter Y with stroke 
local_print(592). %   592	ɐ 	LATIN SMALL LETTER TURNED A (U+0250) 	c990
local_print(593). %   593	ɑ 	LATIN SMALL LETTER ALPHA (U+0251) 	c991
local_print(594). %   594	ɒ 	LATIN SMALL LETTER TURNED ALPHA (U+0252) 	c992
local_print(595). %   595	ɓ 	LATIN SMALL LETTER B WITH HOOK (U+0253) 	c993
local_print(596). %   596	ɔ 	LATIN SMALL LETTER OPEN O (U+0254) 	c994
local_print(597). %   597	ɕ 	LATIN SMALL LETTER C WITH CURL (U+0255) 	c995
local_print(598). %   598	ɖ 	LATIN SMALL LETTER D WITH TAIL (U+0256) 	c996
local_print(599). %   599	ɗ 	LATIN SMALL LETTER D WITH HOOK (U+0257) 	c997
local_print(600). %   600	ɘ 	LATIN SMALL LETTER REVERSED E (U+0258) 	c998
local_print(601). %   601	ə 	LATIN SMALL LETTER SCHWA (U+0259) 	c999
local_print(602). %   602	ɚ 	LATIN SMALL LETTER SCHWA WITH HOOK (U+025A) 	c99a
local_print(603). %   603	ɛ 	LATIN SMALL LETTER OPEN E (U+025B) 	c99b
local_print(604). %   604	ɜ 	LATIN SMALL LETTER REVERSED OPEN E (U+025C) 	c99c
local_print(605). %   605	ɝ 	LATIN SMALL LETTER REVERSED OPEN E WITH HOOK (U+025D) 	c99d
local_print(606). %   606	ɞ 	LATIN SMALL LETTER CLOSED REVERSED OPEN E (U+025E) 	c99e
local_print(607). %   607	ɟ 	LATIN SMALL LETTER DOTLESS J WITH STROKE (U+025F) 	c99f
local_print(608). %   608	ɠ 	LATIN SMALL LETTER G WITH HOOK (U+0260) 	c9a0
local_print(609). %   609	ɡ 	LATIN SMALL LETTER SCRIPT G (U+0261) 	c9a1
local_print(610). %   610	ɢ 	LATIN LETTER SMALL CAPITAL G (U+0262) 	c9a2
local_print(611). %   611	ɣ 	LATIN SMALL LETTER GAMMA (U+0263) 	c9a3
local_print(612). %   612	ɤ 	LATIN SMALL LETTER RAMS HORN (U+0264) 	c9a4
local_print(613). %   613	ɥ 	LATIN SMALL LETTER TURNED H (U+0265) 	c9a5
local_print(614). %   614	ɦ 	LATIN SMALL LETTER H WITH HOOK (U+0266) 	c9a6
local_print(615). %   615	ɧ 	LATIN SMALL LETTER HENG WITH HOOK (U+0267) 	c9a7
local_print(616). %   616	ɨ 	LATIN SMALL LETTER I WITH STROKE (U+0268) 	c9a8
local_print(617). %   617	ɩ 	LATIN SMALL LETTER IOTA (U+0269) 	c9a9
local_print(618). %   618	ɪ 	LATIN LETTER SMALL CAPITAL I (U+026A) 	c9aa
local_print(619). %   619	ɫ 	LATIN SMALL LETTER L WITH MIDDLE TILDE (U+026B) 	c9ab
local_print(620). %   620	ɬ 	LATIN SMALL LETTER L WITH BELT (U+026C) 	c9ac
local_print(621). %   621	ɭ 	LATIN SMALL LETTER L WITH RETROFLEX HOOK (U+026D) 	c9ad
local_print(622). %   622	ɮ 	LATIN SMALL LETTER LEZH (U+026E) 	c9ae
local_print(623). %   623	ɯ 	LATIN SMALL LETTER TURNED M (U+026F) 	c9af
local_print(624). %   624	ɰ 	LATIN SMALL LETTER TURNED M WITH LONG LEG (U+0270) 	c9b0
local_print(625). %   625	ɱ 	LATIN SMALL LETTER M WITH HOOK (U+0271) 	c9b1
local_print(626). %   626	ɲ 	LATIN SMALL LETTER N WITH LEFT HOOK (U+0272) 	c9b2
local_print(627). %   627	ɳ 	LATIN SMALL LETTER N WITH RETROFLEX HOOK (U+0273) 	c9b3
local_print(628). %   628	ɴ 	LATIN LETTER SMALL CAPITAL N (U+0274) 	c9b4
local_print(629). %   629	ɵ 	LATIN SMALL LETTER BARRED O (U+0275) 	c9b5
local_print(630). %   630	ɶ 	LATIN LETTER SMALL CAPITAL OE (U+0276) 	c9b6
local_print(631). %   631	ɷ 	LATIN SMALL LETTER CLOSED OMEGA (U+0277) 	c9b7
local_print(632). %   632	ɸ 	LATIN SMALL LETTER PHI (U+0278) 	c9b8
local_print(633). %   633	ɹ 	LATIN SMALL LETTER TURNED R (U+0279) 	c9b9
 %
 %
local_print(634). %   634	ɺ 	LATIN SMALL LETTER TURNED R WITH LONG LEG (U+027A) 	c9ba
local_print(635). %   635	ɻ 	LATIN SMALL LETTER TURNED R WITH HOOK (U+027B) 	c9bb
local_print(636). %   636	ɼ 	LATIN SMALL LETTER R WITH LONG LEG (U+027C) 	c9bc
local_print(637). %   637	ɽ 	LATIN SMALL LETTER R WITH TAIL (U+027D) 	c9bd
local_print(638). %   638	ɾ 	LATIN SMALL LETTER R WITH FISHHOOK (U+027E) 	c9be
local_print(639). %   639	ɿ 	LATIN SMALL LETTER REVERSED R WITH FISHHOOK (U+027F) 	c9bf
local_print(640). %   640	ʀ 	LATIN LETTER SMALL CAPITAL R (U+0280) 	ca80
local_print(641). %   641	ʁ 	LATIN LETTER SMALL CAPITAL INVERTED R (U+0281) 	ca81
local_print(642). %   642	ʂ 	LATIN SMALL LETTER S WITH HOOK (U+0282) 	ca82
local_print(643). %   643	ʃ 	LATIN SMALL LETTER ESH (U+0283) 	ca83
local_print(644). %   644	ʄ 	LATIN SMALL LETTER DOTLESS J WITH STROKE AND HOOK (U+0284) 	ca84
local_print(645). %   645	ʅ 	LATIN SMALL LETTER SQUAT REVERSED ESH (U+0285) 	ca85
local_print(646). %   646	ʆ 	LATIN SMALL LETTER ESH WITH CURL (U+0286) 	ca86
local_print(647). % ʇ U+1E02 	Ḃ 	Latin Capital Letter B with dot above 	0647 	ISO 8859-14
local_print(648). % ʈ U+1E03 	ḃ 	Latin Small Letter B with dot above 	0648
local_print(649). % ʉ U+1E0A 	Ḋ 	Latin Capital Letter D with dot above 	0649
local_print(650). % ʊ U+1E0B 	ḋ 	Latin Small Letter D with dot above 	0650
local_print(651). % ʋ U+1E1E 	Ḟ 	Latin Capital Letter F with dot above 	0651
local_print(652). % ʌ U+1E1F 	ḟ 	Latin Small Letter F with dot above 	0652
local_print(653). % ʍ U+1E40 	Ṁ 	Latin Capital Letter M with dot above 	0653
local_print(654). % ʎ U+1E41 	ṁ 	Latin Small Letter M with dot above 	0654
local_print(655). % ʏ U+1E56 	Ṗ 	Latin Capital Letter P with dot above 	0655
local_print(656). % ʐ U+1E57 	ṗ 	Latin Small Letter P with dot above 	0656
local_print(657). % ʑ U+1E60 	Ṡ 	Latin Capital Letter S with dot above 	0657
local_print(658). % ʒ U+1E61 	ṡ 	Latin Small Letter S with dot above 	0658
local_print(659). % ʓ U+1E6A 	Ṫ 	Latin Capital Letter T with dot above 	0659
local_print(660). % ʔ U+1E6B 	ṫ 	Latin Small Letter T with dot above 	0660
local_print(661). % ʕ U+1E80 	Ẁ 	Latin Capital Letter W with grave 	0661 	in WGL4
local_print(662). % ʖ U+1E81 	ẁ 	Latin Small Letter W with grave 	0662
local_print(663). % ʗ U+1E82 	Ẃ 	Latin Capital Letter W with acute 	0663
local_print(664). % ʘ U+1E83 	ẃ 	Latin Small Letter W with acute 	0664
local_print(665). % ʙ U+1E84 	Ẅ 	Latin Capital Letter W with diaeresis 	0665
local_print(666). % ʚ U+1E85 	ẅ 	Latin Small Letter W with diaeresis 	0666
local_print(667). % ʛ U+1E9B 	ẛ 	Latin Small Letter Long S with dot above 	0667 	for Fraktur, Irish Gaelic, Old English
local_print(668). % ʜ U+1EF2 	Ỳ 	Latin Capital Letter Y with grave 	0668 	in WGL4
local_print(669). % ʝ U+1EF3 	ỳ 	Latin Small Letter Y with grave 	0669
local_print(670). % ʞ 0670
local_print(671). % ʟ 0671
local_print(672). % ʠ 0672
local_print(673). % ʡ p673
local_print(674). % ʢ 0674
local_print(675). % ʣ 0675
local_print(676). % ʤ 0676
local_print(677). % ʥ 0677
local_print(678). % ʦ 0678
local_print(679). % ʧ 0679
local_print(680). % ʨ 0680
local_print(681). % ʩ 0681
local_print(682). % ʪ 0682
local_print(683). % ʫ 0683
local_print(684). % ʬ 0684
local_print(685). % ʭ 0685
local_print(686). % ʮ 0686
local_print(687). % ʯ 0687
% http://www.fileformat.info/info/charset/UTF-8/list.htm 
% local_print(880). % Ͱ 0880
% local_print(881). % ͱ 0881
% local_print(882). % Ͳ 0882
% local_print(883). % ͳ 0883
local_print(884). % ʹ U+0374 	ʹ 	884 	Greek Numeral Sign 	0371
local_print(885). % ͵ U+0375 	͵ 	885 	Greek Lower Numeral Sign 	0372
% local_print(886). % Ͷ U+0376 	Ͷ 	886 	Greek Capital Letter Pamphylian Digamma 	·
% local_print(887). % ͷ U+0377 	ͷ 	887 	Greek Small Letter Pamphylian Digamma
% local_print(888). % ͸ 
% local_print(889). % ͹ 
local_print(890). % ͺ U+037A 	ͺ 	890 	Greek Ypogegrammeni 	0373
local_print(891). % ͻ U+037B 	ͻ 	891 	Greek Small Reversed Lunate Sigma Symbol 	·
local_print(892). % ͼ U+037C 	ͼ 	892 	Greek Small Dotted Lunate Sigma Symbol
local_print(893). % ͽ U+037D 	ͽ 	893 	Greek Small Reversed Dotted Lunate Sigma Symbol
local_print(894). % ; U+037E 	; 	894 	Greek Question Mark 	0374
% local_print(895). % Ϳ U+037F 	Ϳ 	895 	Greek Capital Letter Yot 	·
% local_print(896). % ΀ 
% local_print(897). % ΁ 
% local_print(898). % ΂ 
% local_print(899). % ΃  
% local_print(900). % ΄ U+0384 	΄ 	900 	Greek acute accent (tonos) 	0375
local_print(901). % ΅ U+0385 	΅ 	901 	Greek diaeresis with acute accent 	0376
local_print(902). % Ά U+0386 	Ά 	902 	Greek Capital Letter A with acute accent 	0377
local_print(903). % · U+0387 	· 	903 	Greek Ano Teleia 	0378
local_print(904). % Έ U+0388 	Έ 	904 	Greek Capital Letter Epsilon with acute accent 	0379
local_print(905). % Ή U+0389 	Ή 	905 	Greek Capital Letter Eta with acute accent 	0380
local_print(906). % Ί U+038A 	Ί 	906 	Greek Capital Letter Iota with acute accent 	0381
% local_print(907). % ΋ 
local_print(908). % Ό U+038C 	Ό 	908 	Greek Capital Letter Omicron with acute accent 	0382
% local_print(909). % ΍ 
local_print(910). % Ύ U+038E 	Ύ 	910 	Greek Capital Letter Upsilon with acute accent 	0383
local_print(911). % Ώ U+038F 	Ώ 	911 	Greek Capital Letter Omega with acute accent 	0384
local_print(912). % ΐ U+0390 	ΐ 	912 	Greek Small Letter Iota with diaeresis and acute accent 	0385
local_print(913). % Α U+0391 	Α 	913 	Greek Capital Letter Alpha 	0386
local_print(914). % Β U+0392 	Β 	914 	Greek Capital Letter Beta 	0387
local_print(915). % Γ U+0393 	Γ 	915 	Greek Capital Letter Gamma 	0388
local_print(916). % Δ U+0394 	Δ 	916 	Greek Capital Letter Delta 	0389
local_print(917). % Ε U+0395 	Ε 	917 	Greek Capital Letter Epsilon 	0390
local_print(918). % Ζ U+0396 	Ζ 	918 	Greek Capital Letter Zeta 	0391
local_print(919). % Η U+0397 	Η 	919 	Greek Capital Letter Eta 	0392
local_print(920). % Θ U+0398 	Θ 	920 	Greek Capital Letter Theta 	0393
local_print(921). % Ι U+0399 	Ι 	921 	Greek Capital Letter Iota 	0394
local_print(922). % Κ U+039A 	Κ 	922 	Greek Capital Letter Kappa 	0395
local_print(923). % Λ U+039B 	Λ 	923 	Greek Capital Letter Lambda 	0396
local_print(924). % Μ U+039C 	Μ 	924 	Greek Capital Letter Mu 	0397
local_print(925). % Ν U+039D 	Ν 	925 	Greek Capital Letter Nu 	0398
local_print(926). % Ξ U+039E 	Ξ 	926 	Greek Capital Letter Xi 	0399
local_print(927). % Ο U+039F 	Ο 	927 	Greek Capital Letter Omicron 	0400
local_print(928). % Π U+03A0 	Π 	928 	Greek Capital Letter Pi 	0401
local_print(929). % Ρ U+03A1 	Ρ 	929 	Greek Capital Letter Rho 	0402
% local_print(930). % ΢ 
local_print(931). % Σ U+03A3 	Σ 	931 	Greek Capital Letter Sigma 	0403
local_print(932). % Τ U+03A4 	Τ 	932 	Greek Capital Letter Tau 	0404
local_print(933). % Υ U+03A5 	Υ 	933 	Greek Capital Letter Upsilon 	0405
local_print(934). % Φ U+03A6 	Φ 	934 	Greek Capital Letter Phi 	0406
local_print(935). % Χ U+03A7 	Χ 	935 	Greek Capital Letter Chi 	0407
local_print(936). % Ψ U+03A8 	Ψ 	936 	Greek Capital Letter Psi 	0408
local_print(937). % Ω U+03A9 	Ω 	937 	Greek Capital Letter Omega 	0409
local_print(938). % Ϊ U+03AA 	Ϊ 	938 	Greek Capital Letter Iota with diaeresis 	0410
local_print(939). % Ϋ U+03AB 	Ϋ 	939 	Greek Capital Letter Upsilon with diaeresis 	0411
local_print(940). % ά U+03AC 	ά 	940 	Greek Small Letter Alpha with acute accent 	0412
local_print(941). % έ U+03AD 	έ 	941 	Greek Small Letter Epsilon with acute accent 	0413
local_print(942). % ή U+03AE 	ή 	942 	Greek Small Letter Eta with acute accent 	0414
local_print(943). % ί U+03AF 	ί 	943 	Greek Small Letter Iota with acute accent 	0415
local_print(944). % ΰ U+03B0 	ΰ 	944 	Greek Small Letter Upsilon with diaeresis and acute accent 	0416
local_print(945). % α U+03B1 	α 	945 	Greek Small Letter Alpha 	0417
local_print(946). % β U+03B2 	β 	946 	Greek Small Letter Beta 	0418
local_print(947). % γ U+03B3 	γ 	947 	Greek Small Letter Gamma 	0419
local_print(948). % δ U+03B4 	δ 	948 	Greek Small Letter Delta 	0420
local_print(949). % ε U+03B5 	ε 	949 	Greek Small Letter Epsilon 	0421
local_print(950). % ζ U+03B6 	ζ 	950 	Greek Small Letter Zeta 	0422
local_print(951). % η U+03B7 	η 	951 	Greek Small Letter Eta 	0423
local_print(952). % θ U+03B8 	θ 	952 	Greek Small Letter Theta 	0424
local_print(953). % ι U+03B9 	ι 	953 	Greek Small Letter Iota 	0425
local_print(954). % κ U+03BA 	κ 	954 	Greek Small Letter Kappa 	0426
local_print(955). % λ U+03BB 	λ 	955 	Greek Small Letter Lambda 	0427
local_print(956). % μ U+03BC 	μ 	956 	Greek Small Letter Mu 	0428
local_print(957). % ν U+03BD 	ν 	957 	Greek Small Letter Nu 	0429
local_print(958). % ξ U+03BE 	ξ 	958 	Greek Small Letter Xi 	0430
local_print(959). % ο U+03BF 	ο 	959 	Greek Small Letter Omicron 	0431
local_print(960). % π U+03C0 	π 	960 	Greek Small Letter Pi 	0432
local_print(961). % ρ U+03C1 	ρ 	961 	Greek Small Letter Rho 	0433
local_print(962). % ς U+03C2 	ς 	962 	Greek Small Letter Final Sigma 	0434
local_print(963). % σ U+03C3 	σ 	963 	Greek Small Letter Sigma 	0435
local_print(964). % τ U+03C4 	τ 	964 	Greek Small Letter Tau 	0436
local_print(965). % υ U+03C5 	υ 	965 	Greek Small Letter Upsilon 	0437
local_print(966). % φ U+03C6 	φ 	966 	Greek Small Letter Phi 	0438
local_print(967). % χ U+03C7 	χ 	967 	Greek Small Letter Chi 	0439
local_print(968). % ψ U+03C8 	ψ 	968 	Greek Small Letter Psi 	0440
local_print(969). % ω U+03C9 	ω 	969 	Greek Small Letter Omega 	0441
local_print(970). % ϊ U+03CA 	ϊ 	970 	Greek Small Letter Iota with diaeresis 	0442
local_print(971). % ϋ U+03CB 	ϋ 	971 	Greek Small Letter Upsilon with diaeresis 	0443
local_print(972). % ό U+03CC 	ό 	972 	Greek Small Letter Omicron with acute accent 	0444
local_print(973). % ύ U+03CD 	ύ 	973 	Greek Small Letter Upsilon with acute accent 	0445
local_print(974). % ώ U+03CE 	ώ 	974 	Greek Small Letter Omega with acute accent 	0446
% local_print(975). % Ϗ U+03CF 	Ϗ 	975 	Greek Capital Kai Symbol 	·
local_print(976). % ϐ U+03D0 	ϐ 	976 	Greek Beta Symbol 	·
local_print(977). % ϑ U+03D1 	ϑ 	977 	Greek Theta Symbol
local_print(978). % ϒ U+03D2 	ϒ 	978 	Greek Upsilon with hook Symbol
local_print(979). % ϓ U+03D3 	ϓ 	979 	Greek Upsilon with acute and hook Symbol
local_print(980). % ϔ U+03D4 	ϔ 	980 	Greek Upsilon with diaeresis and hook Symbol
local_print(981). % ϕ U+03D5 	ϕ 	981 	Greek Phi Symbol
local_print(982). % ϖ U+03D6 	ϖ 	982 	Greek Pi Symbol
local_print(983). % ϗ U+03D7 	ϗ 	983 	Greek Kai Symbol 	0447
local_print(984). % Ϙ U+03D8 	Ϙ 	984 	Greek Letter Qoppa 	·
local_print(985). % ϙ U+03D9 	ϙ 	985 	Greek Small Letter Qoppa
local_print(986). % Ϛ U+03DA 	Ϛ 	986 	Greek Letter Stigma (letter) 	0448
local_print(987). % ϛ U+03DB 	ϛ 	987 	Greek Small Letter Stigma 	0449
local_print(988). % Ϝ U+03DC 	Ϝ 	988 	Greek Letter Digamma 	0450
local_print(989). % ϝ U+03DD 	ϝ 	989 	Greek Small Letter Digamma 	0451
local_print(990). % Ϟ U+03DE 	Ϟ 	990 	Greek Letter Koppa 	0452
local_print(991). % ϟ U+03DF 	ϟ 	991 	Greek Small Letter Koppa 	0453
local_print(992). % Ϡ U+03E0 	Ϡ 	992 	Greek Letter Sampi 	0454
local_print(993). % ϡ U+03E1 	ϡ 	993 	Greek Small Letter Sampi 	0455
local_print(994). % Ϣ U+03E2 	Ϣ 	994 	Coptic Capital Letter Shei 	·
local_print(995). % ϣ U+03E3 	ϣ 	995 	Coptic Small Letter Shei
local_print(996). % Ϥ U+03E4 	Ϥ 	996 	Coptic Capital Letter Fei
local_print(997). % ϥ U+03E5 	ϥ 	997 	Coptic Small Letter Fei
local_print(998). % Ϧ U+03E6 	Ϧ 	998 	Coptic Capital Letter Khei
local_print(999). % ϧ U+03E7 	ϧ 	999 	Coptic Small Letter Khei
local_print(1000). % Ϩ U+03E8 	Ϩ 	1000 	Coptic Capital Letter Hori
local_print(1001). % ϩ U+03E9 	ϩ 	1001 	Coptic Small Letter Hori
local_print(1002). % Ϫ U+03EA 	Ϫ 	1002 	Coptic Capital Letter Gangia
local_print(1003). % ϫ U+03EB 	ϫ 	1003 	Coptic Small Letter Gangia
local_print(1004). % Ϭ U+03EC 	Ϭ 	1004 	Coptic Capital Letter Shima
local_print(1005). % ϭ U+03ED 	ϭ 	1005 	Coptic Small Letter Shima
local_print(1006). % Ϯ U+03EE 	Ϯ 	1006 	Coptic Capital Letter Dei
local_print(1007). % ϯ U+03EF 	ϯ 	1007 	Coptic Small Letter Dei
local_print(1008). % ϰ U+03F0 	ϰ 	1008 	Greek Kappa Symbol
local_print(1009). % ϱ U+03F1 	ϱ 	1009 	Greek Rho Symbol
local_print(1010). % ϲ U+03F2 	ϲ 	1010 	Greek Lunate Sigma Symbol
local_print(1011). % ϳ U+03F3 	ϳ 	1011 	Greek Letter Yot
local_print(1012). % ϴ U+03F4 	ϴ 	1012 	Greek Capital Theta Symbol
local_print(1013). % ϵ U+03F5 	ϵ 	1013 	Greek Lunate Epsilon Symbol
local_print(1014). % ϶ U+03F6 	϶ 	1014 	Greek Reversed Lunate Epsilon Symbol
local_print(1015). % Ϸ U+03F7 	Ϸ 	1015 	Greek Capital Sho
local_print(1016). % ϸ U+03F8 	ϸ 	1016 	Greek Small Letter Sho
local_print(1017). % Ϲ U+03F9 	Ϲ 	1017 	Greek Capital Lunate Sigma Symbol
local_print(1018). % Ϻ U+03FA 	Ϻ 	1018 	Greek Capital San
local_print(1019). % ϻ U+03FB 	ϻ 	1019 	Greek Small Letter San
local_print(1020). % ϼ U+03FC 	ϼ 	1020 	Greek Rho with stroke Symbol
local_print(1021). % Ͻ U+03FD 	Ͻ 	1021 	Greek Capital Reversed Lunate Sigma Symbol
local_print(1022). % Ͼ U+03FE 	Ͼ 	1022 	Greek Capital Dotted Lunate Sigma Symbol
local_print(1023). % Ͽ U+03FF 	Ͽ 	1023 	Greek Capital Reversed Dotted Lunate Sigma Symbol

% https://www.codetable.net/Group/superscripts-and-subscripts

local_print(8304).   % Superscript Zero
local_print(8305).   % Superscript Latin Small Letter I
local_print(8308).   % Superscript Four
local_print(8309).   % Superscript Five
local_print(8310).   % Superscript Six
local_print(8311).   % Superscript Seven
local_print(8312).   % Superscript Eight
local_print(8313).   % Superscript Nine
% local_print(8314).   % Superscript Plus Sign
% local_print(8315).   % Superscript Minus
% local_print(8316).   % Superscript Equals Sign
% local_print(8317).   % Superscript Left Parenthesis
% local_print(8318).   % Superscript Right Parenthesis
local_print(8319).   % Superscript Latin Small Letter N
local_print(8320).   % Subscript Zero
local_print(8321).   % Subscript One
local_print(8322).   % Subscript Two
local_print(8323).   % Subscript Three
local_print(8324).   % Subscript Four
local_print(8325).   % Subscript Five
local_print(8326).   % Subscript Six
local_print(8327).   % Subscript Seven
local_print(8328).   % Subscript Eight
local_print(8329).   % Subscript Nine
% local_print(8330).   % Subscript Plus Sign
% local_print(8331).   % Subscript Minus
% local_print(8332).   % Subscript Equals Sign
% local_print(8333).   % Subscript Left Parenthesis
% local_print(8334).   % Subscript Right Parenthesis
local_print(8336).   % Latin Subscript Small Letter A
local_print(8337).   % Latin Subscript Small Letter E
local_print(8338).   % Latin Subscript Small Letter O
local_print(8339).   % Latin Subscript Small Letter X
local_print(8340).   % Latin Subscript Small Letter Schwa
local_print(8341).   % Latin Subscript Small Letter H
local_print(8342).   % Latin Subscript Small Letter K
local_print(8343).   % Latin Subscript Small Letter L
local_print(8344).   % Latin Subscript Small Letter M
local_print(8345).   % Latin Subscript Small Letter N
local_print(8346).   % Latin Subscript Small Letter P
local_print(8347).   % Latin Subscript Small Letter S
local_print(8348).   % Latin Subscript Small Letter T
% local_print(8482).   % Unicode Character 'TRADE MARK SIGN' (U+2122)


% Needed for these rows in 2019AC MRCONSO.RRF:
% C1328242|ENG|P|L3501981|PF|S4034347|Y|A29946619|||CDR0000039676|PDQ|BN|CDR0000039676|Light Infusion Therapy™|0|N|256|
% C1541153|ENG|P|L5503220|PF|S6296307|Y|A29931099|||CDR0000361792|PDQ|BN|CDR0000361792|TOCOSOL™ Paclitaxel|0|N|256|
% C1541154|ENG|P|L5503246|PF|S6296266|Y|A29940235|||CDR0000377520|PDQ|BN|CDR0000377520|Resmycin™|0|N|256|
% C1541162|ENG|P|L5503276|PF|S6296330|Y|A29943898|||CDR0000418694|PDQ|BN|CDR0000418694|Xenavex™|0|N|256|
% C1541163|ENG|P|L5502972|VO|S6296287|Y|A29943917|||CDR0000420851|PDQ|BN|CDR0000420851|SLIT™ cisplatin|0|N|256|
% C3180499|ENG|P|L10118291|VO|S18344532|Y|A29945211|||CDR0000742626|PDQ|FBD|CDR0000742626|Targinact™|0|N|256|
% C3827051|ENG|P|L11664778|VO|S18343186|Y|A29933065|||CDR0000756792|PDQ|BN|CDR0000756792|HyperAcute™ Renal|0|N|256|
% C3896720|ENG|P|L11880628|VO|S18343183|Y|A29945442|||CDR0000762341|PDQ|BN|CDR0000762341|HylaCare™ cream|0|N|256|
% C3896811|ENG|S|L15088665|PF|S18348480|Y|A29945487|||CDR0000764994|PDQ|SY|CDR0000764994|humanized gpA33 x CD3 DART™ protein MGD007|0|N|256|
% C4727635|ENG|P|L14990922|VO|S18342504|Y|A29940993|||CDR0000434427|PDQ|BN|CDR0000434427|Cynviloq™|0|N|256|
% C4732972|ENG|P|L15087567|PF|S18342186|Y|A29947210|||CDR0000486330|PDQ|FBD|CDR0000486330|CAVATAK™|0|N|256|
% C4733036|ENG|P|L15087625|PF|S18343662|Y|A29941349|||CDR0000588952|PDQ|BN|CDR0000588952|Medihoney™|0|N|256|

local_to_lower(0,0).
local_to_lower(1,1).
local_to_lower(2,2).
local_to_lower(3,3).
local_to_lower(4,4).
local_to_lower(5,5).
local_to_lower(6,6).
local_to_lower(7,7).
local_to_lower(8,8).
local_to_lower(9,9).
local_to_lower(10,10).
local_to_lower(11,11).
local_to_lower(12,12).
local_to_lower(13,13).
local_to_lower(14,14).
local_to_lower(15,15).
local_to_lower(16,16).
local_to_lower(17,17).
local_to_lower(18,18).
local_to_lower(19,19).
local_to_lower(20,20).
local_to_lower(21,21).
local_to_lower(22,22).
local_to_lower(23,23).
local_to_lower(24,24).
local_to_lower(25,25).
local_to_lower(26,26).
local_to_lower(27,27).
local_to_lower(28,28).
local_to_lower(29,29).
local_to_lower(30,30).
local_to_lower(31,31).
local_to_lower(32,32).
local_to_lower(33,33).
local_to_lower(34,34).
local_to_lower(35,35).
local_to_lower(36,36).
local_to_lower(37,37).
local_to_lower(38,38).
local_to_lower(39,39).
local_to_lower(40,40).
local_to_lower(41,41).
local_to_lower(42,42).
local_to_lower(43,43).
local_to_lower(44,44).
local_to_lower(45,45).
local_to_lower(46,46).
local_to_lower(47,47).
local_to_lower(48,48).
local_to_lower(49,49).
local_to_lower(50,50).
local_to_lower(51,51).
local_to_lower(52,52).
local_to_lower(53,53).
local_to_lower(54,54).
local_to_lower(55,55).
local_to_lower(56,56).
local_to_lower(57,57).
local_to_lower(58,58).
local_to_lower(59,59).
local_to_lower(60,60).
local_to_lower(61,61).
local_to_lower(62,62).
local_to_lower(63,63).
local_to_lower(64,64).
local_to_lower(65,97).
local_to_lower(66,98).
local_to_lower(67,99).
local_to_lower(68,100).
local_to_lower(69,101).
local_to_lower(70,102).
local_to_lower(71,103).
local_to_lower(72,104).
local_to_lower(73,105).
local_to_lower(74,106).
local_to_lower(75,107).
local_to_lower(76,108).
local_to_lower(77,109).
local_to_lower(78,110).
local_to_lower(79,111).
local_to_lower(80,112).
local_to_lower(81,113).
local_to_lower(82,114).
local_to_lower(83,115).
local_to_lower(84,116).
local_to_lower(85,117).
local_to_lower(86,118).
local_to_lower(87,119).
local_to_lower(88,120).
local_to_lower(89,121).
local_to_lower(90,122).
local_to_lower(91,91).
local_to_lower(92,92).
local_to_lower(93,93).
local_to_lower(94,94).
local_to_lower(95,95).
local_to_lower(96,96).
local_to_lower(97,97).
local_to_lower(98,98).
local_to_lower(99,99).
local_to_lower(100,100).
local_to_lower(101,101).
local_to_lower(102,102).
local_to_lower(103,103).
local_to_lower(104,104).
local_to_lower(105,105).
local_to_lower(106,106).
local_to_lower(107,107).
local_to_lower(108,108).
local_to_lower(109,109).
local_to_lower(110,110).
local_to_lower(111,111).
local_to_lower(112,112).
local_to_lower(113,113).
local_to_lower(114,114).
local_to_lower(115,115).
local_to_lower(116,116).
local_to_lower(117,117).
local_to_lower(118,118).
local_to_lower(119,119).
local_to_lower(120,120).
local_to_lower(121,121).
local_to_lower(122,122).
local_to_lower(123,123).
local_to_lower(124,124).
local_to_lower(125,125).
local_to_lower(126,126).
local_to_lower(127,127).
local_to_lower(128,128).
local_to_lower(129,129).
local_to_lower(130,130).
local_to_lower(131,131).
local_to_lower(132,132).
local_to_lower(133,133).
local_to_lower(134,134).
local_to_lower(135,135).
local_to_lower(136,136).
local_to_lower(137,137).
local_to_lower(138,138).
local_to_lower(139,139).
local_to_lower(140,140).
local_to_lower(141,141).
local_to_lower(142,142).
local_to_lower(143,143).
local_to_lower(144,144).
local_to_lower(145,145).
local_to_lower(146,146).
local_to_lower(147,147).
local_to_lower(148,148).
local_to_lower(149,149).
local_to_lower(150,150).
local_to_lower(151,151).
local_to_lower(152,152).
local_to_lower(153,153).
local_to_lower(154,154).
local_to_lower(155,155).
local_to_lower(156,156).
local_to_lower(157,157).
local_to_lower(158,158).
local_to_lower(159,159).
local_to_lower(160,160).
local_to_lower(161,161).
local_to_lower(162,162).
local_to_lower(163,163).
local_to_lower(164,164).
local_to_lower(165,165).
local_to_lower(166,166).
local_to_lower(167,167).
local_to_lower(168,168).
local_to_lower(169,169).
local_to_lower(170,170).
local_to_lower(171,171).
local_to_lower(172,172).
local_to_lower(173,173).
local_to_lower(174,174).
local_to_lower(175,175).
local_to_lower(176,176).
local_to_lower(177,177).
local_to_lower(178,178).
local_to_lower(179,179).
local_to_lower(180,180).
local_to_lower(181,181).
local_to_lower(182,182).
local_to_lower(183,183).
local_to_lower(184,184).
local_to_lower(185,185).
local_to_lower(186,186).
local_to_lower(187,187).
local_to_lower(188,188).
local_to_lower(189,189).
local_to_lower(190,190).
local_to_lower(191,191).
local_to_lower(192,224).
local_to_lower(193,225).
local_to_lower(194,226).
local_to_lower(195,227).
local_to_lower(196,228).
local_to_lower(197,229).
local_to_lower(198,230).
local_to_lower(199,231).
local_to_lower(200,232).
local_to_lower(201,233).
local_to_lower(202,234).
local_to_lower(203,235).
local_to_lower(204,236).
local_to_lower(205,237).
local_to_lower(206,238).
local_to_lower(207,239).
local_to_lower(208,240).
local_to_lower(209,241).
local_to_lower(210,242).
local_to_lower(211,243).
local_to_lower(212,244).
local_to_lower(213,245).
local_to_lower(214,246).
local_to_lower(215,247).
local_to_lower(216,248).
local_to_lower(217,249).
local_to_lower(218,250).
local_to_lower(219,251).
local_to_lower(220,252).
local_to_lower(221,253).
local_to_lower(222,254).
local_to_lower(223,223).
local_to_lower(224,224).
local_to_lower(225,225).
local_to_lower(226,226).
local_to_lower(227,227).
local_to_lower(228,228).
local_to_lower(229,229).
local_to_lower(230,230).
local_to_lower(231,231).
local_to_lower(232,232).
local_to_lower(233,233).
local_to_lower(234,234).
local_to_lower(235,235).
local_to_lower(236,236).
local_to_lower(237,237).
local_to_lower(238,238).
local_to_lower(239,239).
local_to_lower(240,240).
local_to_lower(241,241).
local_to_lower(242,242).
local_to_lower(243,243).
local_to_lower(244,244).
local_to_lower(245,245).
local_to_lower(246,246).
local_to_lower(247,247).
local_to_lower(248,248).
local_to_lower(249,249).
local_to_lower(250,250).
local_to_lower(251,251).
local_to_lower(252,252).
local_to_lower(253,253).
local_to_lower(254,254).
local_to_lower(255,255).

local_to_upper(0,0).
local_to_upper(1,1).
local_to_upper(2,2).
local_to_upper(3,3).
local_to_upper(4,4).
local_to_upper(5,5).
local_to_upper(6,6).
local_to_upper(7,7).
local_to_upper(8,8).
local_to_upper(9,9).
local_to_upper(10,10).
local_to_upper(11,11).
local_to_upper(12,12).
local_to_upper(13,13).
local_to_upper(14,14).
local_to_upper(15,15).
local_to_upper(16,16).
local_to_upper(17,17).
local_to_upper(18,18).
local_to_upper(19,19).
local_to_upper(20,20).
local_to_upper(21,21).
local_to_upper(22,22).
local_to_upper(23,23).
local_to_upper(24,24).
local_to_upper(25,25).
local_to_upper(26,26).
local_to_upper(27,27).
local_to_upper(28,28).
local_to_upper(29,29).
local_to_upper(30,30).
local_to_upper(31,31).
local_to_upper(32,32).
local_to_upper(33,33).
local_to_upper(34,34).
local_to_upper(35,35).
local_to_upper(36,36).
local_to_upper(37,37).
local_to_upper(38,38).
local_to_upper(39,39).
local_to_upper(40,40).
local_to_upper(41,41).
local_to_upper(42,42).
local_to_upper(43,43).
local_to_upper(44,44).
local_to_upper(45,45).
local_to_upper(46,46).
local_to_upper(47,47).
local_to_upper(48,48).
local_to_upper(49,49).
local_to_upper(50,50).
local_to_upper(51,51).
local_to_upper(52,52).
local_to_upper(53,53).
local_to_upper(54,54).
local_to_upper(55,55).
local_to_upper(56,56).
local_to_upper(57,57).
local_to_upper(58,58).
local_to_upper(59,59).
local_to_upper(60,60).
local_to_upper(61,61).
local_to_upper(62,62).
local_to_upper(63,63).
local_to_upper(64,64).
local_to_upper(65,65).
local_to_upper(66,66).
local_to_upper(67,67).
local_to_upper(68,68).
local_to_upper(69,69).
local_to_upper(70,70).
local_to_upper(71,71).
local_to_upper(72,72).
local_to_upper(73,73).
local_to_upper(74,74).
local_to_upper(75,75).
local_to_upper(76,76).
local_to_upper(77,77).
local_to_upper(78,78).
local_to_upper(79,79).
local_to_upper(80,80).
local_to_upper(81,81).
local_to_upper(82,82).
local_to_upper(83,83).
local_to_upper(84,84).
local_to_upper(85,85).
local_to_upper(86,86).
local_to_upper(87,87).
local_to_upper(88,88).
local_to_upper(89,89).
local_to_upper(90,90).
local_to_upper(91,91).
local_to_upper(92,92).
local_to_upper(93,93).
local_to_upper(94,94).
local_to_upper(95,95).
local_to_upper(96,96).
local_to_upper(97,65).
local_to_upper(98,66).
local_to_upper(99,67).
local_to_upper(100,68).
local_to_upper(101,69).
local_to_upper(102,70).
local_to_upper(103,71).
local_to_upper(104,72).
local_to_upper(105,73).
local_to_upper(106,74).
local_to_upper(107,75).
local_to_upper(108,76).
local_to_upper(109,77).
local_to_upper(110,78).
local_to_upper(111,79).
local_to_upper(112,80).
local_to_upper(113,81).
local_to_upper(114,82).
local_to_upper(115,83).
local_to_upper(116,84).
local_to_upper(117,85).
local_to_upper(118,86).
local_to_upper(119,87).
local_to_upper(120,88).
local_to_upper(121,89).
local_to_upper(122,90).
local_to_upper(123,123).
local_to_upper(124,124).
local_to_upper(125,125).
local_to_upper(126,126).
local_to_upper(127,127).
local_to_upper(128,128).
local_to_upper(129,129).
local_to_upper(130,130).
local_to_upper(131,131).
local_to_upper(132,132).
local_to_upper(133,133).
local_to_upper(134,134).
local_to_upper(135,135).
local_to_upper(136,136).
local_to_upper(137,137).
local_to_upper(138,138).
local_to_upper(139,139).
local_to_upper(140,140).
local_to_upper(141,141).
local_to_upper(142,142).
local_to_upper(143,143).
local_to_upper(144,144).
local_to_upper(145,145).
local_to_upper(146,146).
local_to_upper(147,147).
local_to_upper(148,148).
local_to_upper(149,149).
local_to_upper(150,150).
local_to_upper(151,151).
local_to_upper(152,152).
local_to_upper(153,153).
local_to_upper(154,154).
local_to_upper(155,155).
local_to_upper(156,156).
local_to_upper(157,157).
local_to_upper(158,158).
local_to_upper(159,159).
local_to_upper(160,160).
local_to_upper(161,161).
local_to_upper(162,162).
local_to_upper(163,163).
local_to_upper(164,164).
local_to_upper(165,165).
local_to_upper(166,166).
local_to_upper(167,167).
local_to_upper(168,168).
local_to_upper(169,169).
local_to_upper(170,170).
local_to_upper(171,171).
local_to_upper(172,172).
local_to_upper(173,173).
local_to_upper(174,174).
local_to_upper(175,175).
local_to_upper(176,176).
local_to_upper(177,177).
local_to_upper(178,178).
local_to_upper(179,179).
local_to_upper(180,180).
local_to_upper(181,181).
local_to_upper(182,182).
local_to_upper(183,183).
local_to_upper(184,184).
local_to_upper(185,185).
local_to_upper(186,186).
local_to_upper(187,187).
local_to_upper(188,188).
local_to_upper(189,189).
local_to_upper(190,190).
local_to_upper(191,191).
local_to_upper(192,192).
local_to_upper(193,193).
local_to_upper(194,194).
local_to_upper(195,195).
local_to_upper(196,196).
local_to_upper(197,197).
local_to_upper(198,198).
local_to_upper(199,199).
local_to_upper(200,200).
local_to_upper(201,201).
local_to_upper(202,202).
local_to_upper(203,203).
local_to_upper(204,204).
local_to_upper(205,205).
local_to_upper(206,206).
local_to_upper(207,207).
local_to_upper(208,208).
local_to_upper(209,209).
local_to_upper(210,210).
local_to_upper(211,211).
local_to_upper(212,212).
local_to_upper(213,213).
local_to_upper(214,214).
local_to_upper(215,215).
local_to_upper(216,216).
local_to_upper(217,217).
local_to_upper(218,218).
local_to_upper(219,219).
local_to_upper(220,220).
local_to_upper(221,221).
local_to_upper(222,222).
local_to_upper(223,223).
local_to_upper(224,192).
local_to_upper(225,193).
local_to_upper(226,194).
local_to_upper(227,195).
local_to_upper(228,196).
local_to_upper(229,197).
local_to_upper(230,198).
local_to_upper(231,199).
local_to_upper(232,200).
local_to_upper(233,201).
local_to_upper(234,202).
local_to_upper(235,203).
local_to_upper(236,204).
local_to_upper(237,205).
local_to_upper(238,206).
local_to_upper(239,207).
local_to_upper(240,208).
local_to_upper(241,209).
local_to_upper(242,210).
local_to_upper(243,211).
local_to_upper(244,212).
local_to_upper(245,213).
local_to_upper(246,214).
local_to_upper(247,215).
local_to_upper(248,216).
local_to_upper(249,217).
local_to_upper(250,218).
local_to_upper(251,219).
local_to_upper(252,220).
local_to_upper(253,221).
local_to_upper(254,222).
local_to_upper(255,255).
