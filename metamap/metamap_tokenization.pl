
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
	get_utterance_token_list/4,
	% must be exported for mwi_utilities
	is_ws/1,
	% must be exported for mwi_utilities
	is_ws_word/1,
	linearize_phrase/4,
	linearize_components/2,
	parse_phrase_word_info/3,
	parse_phrase_word_info/9,
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
	% possessive handling
	% phrase predicates
	get_phrase_item_feature/3,
	get_phrase_item_name/2,
	get_phrase_item_subitems/2,
	new_phrase_item/3,
	get_subitems_feature/3,
	get_subitem_value/2,
	set_subitems_feature/4
    ]).

:- use_module(lexicon(lexical),[
	lowercase_list/2,
	concatenate_strings/3
    ]).

:- use_module(skr_lib(nls_lists),[
	first_n_or_less/3
    ]).

:- use_module(skr_lib(nls_strings),[
	atom_codes_list/2
	% is_print_string/1
    ]).

:- use_module(skr_lib(nls_text),[
	is_all_graphic_text/1
    ]).

:- use_module(skr_lib(sicstus_utils),[
	ttyflush/0
    ]).

:- use_module(skr(skr_utilities),[
	token_template/6
    ]).

:- use_module(text(text_object_util),[
	field_or_label_type/1,
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
    add_tokens_to_phrase_item(First, TokenizedFirst),
    add_tokens_to_one_phrase(Rest, TokenizedRest).

add_tokens_to_phrase_item(Item, TokenizedItem) :-
	get_phrase_item_subitems(Item, SubItems),
	( get_non_null_lexmatch(SubItems, LexMatch) ->
	  make_list(LexMatch, TextList)
	; get_subitems_feature(SubItems,inputmatch,TextList)
	),
	tokenize_all_text_mm_lc(TextList, TokenLists),
	append(TokenLists, Tokens),
	set_phrase_item_feature(Item, tokens, Tokens, TokenizedItem),
	!.
add_tokens_to_phrase_item(Item, Item).


get_non_null_lexmatch(SubItems, LexMatch) :-
	get_subitems_feature(SubItems, lexmatch, LexMatch),
	LexMatch \== [].


make_list(LexMatch, TextList) :-
	( atom(LexMatch) ->
	  TextList = [LexMatch]
	; TextList=LexMatch
	).

/* 
   parse_phrase_word_info(+Option, +Phrase, -PhraseWordInfoPair)
   parse_phrase_word_info(+Phrase, +FilterFlag, +WordsBegin
                          +PhraseWordsIn, -PhraseWordsOut,
                          +PhraseHeadWordsIn, -PhraseWordsOut,
                          +PhraseMapIn, -PhraseMapOut)

parse_phrase_word_info/3 extracts PhraseWordInfoPair from Phrase where
PhraseWordInfoPair is PhraseWordInfo:FilteredPhraseWordInfo and each element
of the pair is of the form
     pwi(PhraseWordL,PhraseHeadWordL,PhraseMap).
If Option is nofilter, then FilteredPhraseWordInfo is the same as
PhraseWordInfo.
parse_phrase_word_info/2 calls parse_phrase_word_info/3 with Option filter.
Filtering consists of removing syntactic items with "opaque" tags
such as prepositions and determiners. See *input_match* family description
for full list.
parse_phrase_word_info/9 is the auxiliary which does the work.  */

parse_phrase_word_info(filter,Phrase,
                       pwi(PhraseWordL,PhraseHeadWordL,PhraseMap):
                       pwi(FPhraseWordL,FPhraseHeadWordL,FPhraseMap)) :-
    parse_phrase_word_info(Phrase,unfiltered,1,[],PhraseWords,
                           [],PhraseHeadWords,[],PhraseMap0),
    create_word_list(PhraseWords,PhraseWordL),
    create_word_list(PhraseHeadWords,PhraseHeadWordL),
    rev(PhraseMap0,PhraseMap),
    parse_phrase_word_info(Phrase,filtered,1,[],FPhraseWords,
                           [],FPhraseHeadWords,[],FPhraseMap0),
    create_word_list(FPhraseWords,FPhraseWordL),
    create_word_list(FPhraseHeadWords,FPhraseHeadWordL),
    rev(FPhraseMap0,FPhraseMap).
parse_phrase_word_info(nofilter,Phrase,
                       pwi(PhraseWordL,PhraseHeadWordL,PhraseMap):
                       pwi(PhraseWordL,PhraseHeadWordL,PhraseMap)) :-
    parse_phrase_word_info(Phrase,unfiltered,1,[],PhraseWords,
                           [],PhraseHeadWords,[],PhraseMap0),
    create_word_list(PhraseWords,PhraseWordL),
    create_word_list(PhraseHeadWords,PhraseHeadWordL),
    rev(PhraseMap0,PhraseMap).

parse_phrase_word_info([],_FilterFlag,_WordsBegin,
                       PhraseWordsIn,PhraseWordsIn,
                       PhraseHeadWordsIn,PhraseHeadWordsIn,
                       PhraseMapIn,PhraseMapIn).
parse_phrase_word_info([PhraseItem|Rest],FilterFlag,WordsBegin,
                       PhraseWordsIn,PhraseWordsOut,
                       PhraseHeadWordsIn,PhraseHeadWordsOut,
                       PhraseMapIn,PhraseMapOut) :-
    (FilterFlag==unfiltered ->
% try tokens instead of input_match
        extract_tokens(PhraseItem, IMWords, IMHeadWords)
    ;   filter_tokens(PhraseItem, IMWords, IMHeadWords)
    ),
    !,
    (IMWords==[] ->
        PhraseWordsInOut=PhraseWordsIn,
        PhraseHeadWordsInOut=PhraseHeadWordsIn,
        PhraseMapInOut=[[0,-1]|PhraseMapIn],
        NewWordsBegin=WordsBegin
    ;   append(PhraseWordsIn,IMWords,PhraseWordsInOut),
        append(PhraseHeadWordsIn,IMHeadWords,PhraseHeadWordsInOut),
        length(IMWords,NIMWords),
        NewWordsBegin is WordsBegin + NIMWords,
        WordsEnd is NewWordsBegin - 1,
        PhraseMapInOut=[[WordsBegin,WordsEnd]|PhraseMapIn]
    ),
    parse_phrase_word_info(Rest,FilterFlag,NewWordsBegin,
                           PhraseWordsInOut,PhraseWordsOut,
                           PhraseHeadWordsInOut,PhraseHeadWordsOut,
                           PhraseMapInOut,PhraseMapOut).
parse_phrase_word_info([_|Rest],FilterFlag,WordsBegin,
                       PhraseWordsIn,PhraseWordsOut,
                       PhraseHeadWordsIn,PhraseHeadWordsOut,
                       PhraseMapIn,PhraseMapOut) :-
    parse_phrase_word_info(Rest,FilterFlag,WordsBegin,
                           PhraseWordsIn,PhraseWordsOut,
                           PhraseHeadWordsIn,PhraseHeadWordsOut,
                           [[0,-1]|PhraseMapIn],PhraseMapOut).


/* create_word_list(+Words, -WordL)

create_word_list/2 forms wdl(Words,LCWords).  */

create_word_list(Words,wdl(Words,LCWords)) :-
    lowercase_list(Words,LCWords).

/* opaque_tags(?Tag)

opaque_tags/1 is a factual predicate of phrase tags which prevent further
search for matching input.  */

opaque_tags(error).    %  Unknown
% Tagger tags
opaque_tags(aux).
opaque_tags(compl).
opaque_tags(conj).
opaque_tags(det).
opaque_tags(modal).
opaque_tags(prep).
opaque_tags(pron).
opaque_tags(punc).
% Other tags
opaque_tags(num).      %  digits only (Tagger tokenizer)
opaque_tags(am).       %  & (Tagger tokenizer)
opaque_tags(ap).       %  ' (Tagger tokenizer)
opaque_tags(at).       %  @ (Tagger tokenizer)
opaque_tags(ax).       %  * (Tagger tokenizer)
opaque_tags(ba).       %  | (Tagger tokenizer)
opaque_tags(bk).       %  [ or ] (Tagger tokenizer)
opaque_tags(bl).       %  \ (Tagger tokenizer)
opaque_tags(bq).       %  ` (Tagger tokenizer)
opaque_tags(br).       %  { or } (Tagger tokenizer)
opaque_tags(cl).       %  : (Tagger tokenizer)
opaque_tags(cm).       %  , (Tagger tokenizer)
opaque_tags(dl).       %  $ (Tagger tokenizer)
opaque_tags(dq).       %  " (Tagger tokenizer)
opaque_tags(eq).       %  = (Tagger tokenizer)
opaque_tags(ex).       %  ! (Tagger tokenizer)
opaque_tags(gr).       %  > (Tagger tokenizer)
opaque_tags(hy).       %  - (Tagger tokenizer)
opaque_tags(ls).       %  < (Tagger tokenizer)
opaque_tags(nm).       %  # (Tagger tokenizer)
opaque_tags(pa).       %  ( or ) (Tagger tokenizer)
opaque_tags(pc).       %  % (Tagger tokenizer)
opaque_tags(pd).       %  . (Tagger tokenizer)
opaque_tags(pl).       %  + (Tagger tokenizer)
opaque_tags(qu).       %  ? (Tagger tokenizer)
opaque_tags(sc).       %  ; (Tagger tokenizer)
opaque_tags(sl).       %  / (Tagger tokenizer)
opaque_tags(tl).       %  ~ (Tagger tokenizer)
opaque_tags(un).       %  _ (Tagger tokenizer)
opaque_tags(up).       %  ^ (Tagger tokenizer)


/* transprent_tags(?Tag)

transparent_tags/1 is a factual predicate of phrase tags which are essentially
ignored in determining the input match words for a phrase, i.e., processing
continues with the argument of the tag.  The only effect is that in
extract_input_match/3, IMHeadWords is always []. */

% Tagger tags
transparent_tags(adj).
transparent_tags(adv).
transparent_tags(noun).
transparent_tags(prefix).
transparent_tags(verb).
% Parser tags
transparent_tags(mod).
transparent_tags(pre).
transparent_tags(shapes).
transparent_tags(prefix).
transparent_tags(not_in_lex).  %  Let unknowns through
transparent_tags(no_tag).      %  Let unknowns through
transparent_tags(ing).         %  Obsolete(?)
transparent_tags(pastpart).    %  Obsolete(?)


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

extract_tokens(head(SubItemList), TokenWords, TokenWords) :-
	!,
	extract_tokens_aux(SubItemList, TokenWords).
% try this!
extract_tokens(shapes(ShapesList,_FeatList), ShapesList, []) :- !.
extract_tokens(PhraseItem, TokenWords, []) :-
	functor(PhraseItem, _Tag, 1),
	arg(1,PhraseItem, SubItemList),
	!,
	extract_tokens_aux(SubItemList, TokenWords).
extract_tokens(PhraseItem, [], []) :-
	format('~NERROR: extract_tokens/3 failed for ~p.~n', [PhraseItem]),
	!,
	fail.

% for "primitive" tags
extract_tokens_aux(PhraseItems, TokenWords) :-
	( atom(PhraseItems) ->
	  TokenWords = [PhraseItems],
	  format(user_output, "PRIMITIVE: ~q~n", [PhraseItems])
	; % PhraseItems = [H|T],
	  memberchk(tokens(TokenWords), PhraseItems)
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
	( opaque_tags(Tag) ->        % stop
	  TokenWords = []
	; transparent_tags(Tag) ->   % continue
	  arg(1, PhraseItem, SubItemList),
	  extract_tokens_aux(SubItemList, TokenWords)
	; fail                       % fail
	),
	!.
filter_tokens_1(PhraseItem, [], []) :-
	format('~NERROR: filter_tokens_1/3 failed for ~p.~n', [PhraseItem]),
	!,
	fail.

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

linearize_phrase/4 chops up Phrase (and PhraseMap) into LPhrase (and
LPhraseMap) so that each element of the map is a singleton.  Chopping is
performed on both the map itself and the tokens/1 terms of the phrase.
For example, given text "Fifty-six vena caval stent filters.",
     Phrase: [shapes([inputmatch([Fifty,-,six]),
                      features([word_numeral]),
                      tokens([fifty,six])]),
              mod([lexmatch([vena caval]),
                   inputmatch([vena,caval]),
                   tokens([vena,caval])]),
              mod([lexmatch([stent]),
                   inputmatch([stent]),
                   tokens([stent])]),
              head([lexmatch([filters]),
                    inputmatch([filters]),
                    tokens([filters])]),
              punc([inputmatch([.]),
                    tokens([.])])]
  PhraseMap: [[1,2],[3,4],[5,5],[6,6],[0,-1]]
is linearized to
    LPhrase: [shapes([inputmatch([Fifty,-,six]),
                      features([word_numeral]),
                      tokens([fifty])]),
              shapes([inputmatch([Fifty,-,six]),
                      features([word_numeral]),
                      tokens([six])]),
              mod([lexmatch([vena caval]),
                   inputmatch([vena,caval]),
                   tokens([vena])]),
              mod([lexmatch([vena caval]),
                   inputmatch([vena,caval]),
                   tokens([caval])]),
              mod([lexmatch([stent]),
                   inputmatch([stent]),
                   tokens([stent])]),
              head([lexmatch([filters]),
                    inputmatch([filters]),
                    tokens([filters])]),
              punc([inputmatch([.]),
                    tokens([.])])]
 LPhraseMap: [[1],[2],[3],[4],[5],[6],[0]]

linearize_phrase_item/4
linearize_phrase_item/5
linearize_components/2
linearize_component/2
linearize_component/3
xxx
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
	format('~NERROR: Cannot linearize ~p (mapping/input match)~n', [PhraseItem]),
	!,
	fail.

linearize_phrase_item_7([], _PhraseItem, [], [], [], [], []) :- !.
linearize_phrase_item_7([FirstMap|RestMap], PhraseItem, [FirstToken|RestTokens],
			[FirstIM|RestIM], [FirstBase|RestBases],
			[FirstLItem|RestLItems], [[FirstMap]|RestLMap]) :-
	set_phrase_item_feature(PhraseItem, tokens, [FirstToken], FirstLItem0),
	set_phrase_item_feature(FirstLItem0, inputmatch, FirstIM,  FirstLItem1),
	set_phrase_item_feature(FirstLItem1, bases, FirstBase, FirstLItem2),
	( RestMap == [] ->
	  FirstLItem = FirstLItem2
	; demote_phrase_item(FirstLItem2,FirstLItem)
	),
	!,
	linearize_phrase_item_7(RestMap, PhraseItem, RestTokens, RestIM, RestBases, RestLItems, RestLMap).
linearize_phrase_item_7(_, PhraseItem, _, _, _, _, _) :-
	format('~NERROR: Cannot linearize ~p (mapping/input match)~n', [PhraseItem]),
	fail.

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

linearize_component([Begin,Begin], [Begin]) :-    !.
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
%    ;   is_print_string(Text) ->
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
is that tokenize_text_mm/2 respects possessives ("'s" at the end of a
word).
tokenize_text_mm_lc/2 lowercases the result of tokenize_text_mm/2.
tokenize_all_.../2 tokenizes a list of text into a SINGLE list of tokens. */

tokenize_text_mm(Text, TokText) :-
	( atom(Text) ->
	  atom_codes(Text, String),
	  tokenize_text_utterly(String, StringToks0),
	  remove_possessives_and_nonwords(StringToks0, StringToks),
	  atom_codes_list(TokText, StringToks)
	% ;is_print_string(Text) ->
	; tokenize_text_utterly(Text, TokText0),
	  remove_possessives_and_nonwords(TokText0, TokText)
	).


/* remove_possessives_and_nonwords(+UTokensIn, -UTokensOut)

remove_possessives_and_nonwords/2 filters out possessives and nonwords
from the results of tokenize_text_utterly/2. */

remove_possessives_and_nonwords([], []).
remove_possessives_and_nonwords([NonWord|Rest], FilteredRest) :-
	\+ is_ws_word(NonWord),
	!,
	remove_possessives_and_nonwords(Rest,FilteredRest).
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

% ORIG
% is_ws_word([]).
% is_ws_word([AlNum|Rest]) :-
% 	is_alnum(AlNum),
% 	is_ws_word(Rest).

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
	local_space(Char).

tokenize_all_text_mm_lc([], []).
tokenize_all_text_mm_lc([First|Rest], [TokenizedFirst|TokenizedRest]) :-
	tokenize_text_mm_lc(First, TokenizedFirst),
	tokenize_all_text_mm_lc(Rest, TokenizedRest).

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
	concatenate_strings(Lines, " ", FieldText),
	tokenize_text_utterly(FieldText, TokField),
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
tokenize_text_utterly(Text,TokText) :-
	( atom(Text) ->
	  atom_codes(Text,String),
	  ttu_string(TokString,String,[]),
	  atom_codes_list(TokText,TokString)
	; ttu_string(TokText,Text,[])
	),
	!.

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

% MATS
ttu_string(TS) -->
	[Char],
	{ local_alnum(Char) }, !,
	{ TS=[[Char|S]|R] },
	ttu_token(S),
	ttu_string(R).
ttu_string(TS) -->
	[Char], !,
	{ TS = [[Char]|S] },
	ttu_string(S).
ttu_string([]) --> [].

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
ttu_token(T, S0, S) :-
	ttu_token2(S0, T, S).

ttu_token2([], [], []).
ttu_token2([Char|S0], T, S) :-
	ctypes:bits(Char, Bits), 
	Mask is Bits /\ 3840,
	(   Mask =\= 0 ->
	    T = [Char|R],
	    ttu_token2(S0, R, S)
	;   T = [],
	    S = [Char|S0]
	).

local_space( 9).
local_space(10).
local_space(11).
local_space(12).
local_space(13).
local_space(31).
local_space(32).

local_alnum(48).
local_alnum(49).
local_alnum(50).
local_alnum(51).
local_alnum(52).
local_alnum(53).
local_alnum(54).
local_alnum(55).
local_alnum(56).
local_alnum(57).
local_alnum(65).
local_alnum(66).
local_alnum(67).
local_alnum(68).
local_alnum(69).
local_alnum(70).
local_alnum(71).
local_alnum(72).
local_alnum(73).
local_alnum(74).
local_alnum(75).
local_alnum(76).
local_alnum(77).
local_alnum(78).
local_alnum(79).
local_alnum(80).
local_alnum(81).
local_alnum(82).
local_alnum(83).
local_alnum(84).
local_alnum(85).
local_alnum(86).
local_alnum(87).
local_alnum(88).
local_alnum(89).
local_alnum(90).
local_alnum(97).
local_alnum(98).
local_alnum(99).
local_alnum(100).
local_alnum(101).
local_alnum(102).
local_alnum(103).
local_alnum(104).
local_alnum(105).
local_alnum(106).
local_alnum(107).
local_alnum(108).
local_alnum(109).
local_alnum(110).
local_alnum(111).
local_alnum(112).
local_alnum(113).
local_alnum(114).
local_alnum(115).
local_alnum(116).
local_alnum(117).
local_alnum(118).
local_alnum(119).
local_alnum(120).
local_alnum(121).
local_alnum(122).
local_alnum(192).
local_alnum(193).
local_alnum(194).
local_alnum(195).
local_alnum(196).
local_alnum(197).
local_alnum(198).
local_alnum(199).
local_alnum(200).
local_alnum(201).
local_alnum(202).
local_alnum(203).
local_alnum(204).
local_alnum(205).
local_alnum(206).
local_alnum(207).
local_alnum(208).
local_alnum(209).
local_alnum(210).
local_alnum(211).
local_alnum(212).
local_alnum(213).
local_alnum(214).
local_alnum(215).
local_alnum(216).
local_alnum(217).
local_alnum(218).
local_alnum(219).
local_alnum(220).
local_alnum(221).
local_alnum(222).
local_alnum(223).
local_alnum(224).
local_alnum(225).
local_alnum(226).
local_alnum(227).
local_alnum(228).
local_alnum(229).
local_alnum(230).
local_alnum(231).
local_alnum(232).
local_alnum(233).
local_alnum(234).
local_alnum(235).
local_alnum(236).
local_alnum(237).
local_alnum(238).
local_alnum(239).
local_alnum(240).
local_alnum(241).
local_alnum(242).
local_alnum(243).
local_alnum(244).
local_alnum(245).
local_alnum(246).
local_alnum(247).
local_alnum(248).
local_alnum(249).
local_alnum(250).
local_alnum(251).
local_alnum(252).
local_alnum(253).
local_alnum(254).
local_alnum(255).


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
xxx
*/

get_phrase_item_feature(PhraseItem,Feature,FeatureValue) :-
    get_phrase_item_subitems(PhraseItem,Subitems),
    get_subitems_feature(Subitems,Feature,FeatureValue).

get_phrase_item_name(PhraseItem,ItemName) :-
    functor(PhraseItem,ItemName,1).

get_phrase_item_subitems(PhraseItem,Subitems) :-
    arg(1,PhraseItem,Subitems).

new_phrase_item(ItemName,Subitems,PhraseItem) :-
    functor(PhraseItem,ItemName,1),
    arg(1,PhraseItem,Subitems).

get_subitems_feature([],_,[]).  % return [] for non-existing feature
get_subitems_feature([First|_Rest],Feature,FeatureValue) :-
    get_subitem_name(First,Feature),
    !,
    get_subitem_value(First,FeatureValue).
get_subitems_feature([_First|Rest],Feature,FeatureValue) :-
    get_subitems_feature(Rest,Feature,FeatureValue).

get_subitem_name(Subitem,Name) :-
    functor(Subitem,Name,1).

get_subitem_value(Subitem,Value) :-
    arg(1,Subitem,Value).

set_phrase_item_feature(PhraseItem,Feature,FeatureValue,ModifiedPhraseItem) :-
    get_phrase_item_name(PhraseItem,ItemName),
    get_phrase_item_subitems(PhraseItem,Subitems),
    set_subitems_feature(Subitems,Feature,FeatureValue,ModifiedSubitems),
    new_phrase_item(ItemName,ModifiedSubitems,ModifiedPhraseItem).

set_subitems_feature([],Feature,FeatureValue,[NewSubitem]) :-
    functor(NewSubitem,Feature,1),
    arg(1,NewSubitem,FeatureValue).
set_subitems_feature([First|Rest],Feature,FeatureValue,[NewSubitem|Rest]) :-
    functor(First,Feature,1),
    !,
    functor(NewSubitem,Feature,1),
    arg(1,NewSubitem,FeatureValue).
set_subitems_feature([First|Rest],Feature,FeatureValue,[First|ModifiedRest]) :-
    set_subitems_feature(Rest,Feature,FeatureValue,ModifiedRest).

/* SHOULD REALLY BE SOMEWHERE ELSE, not in NEGEX either
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Pass in    [FirstToken|RestTokensIn]
the list of all tokens for all current input text.
Return in  [FirstToken|RestTokensThisUtterance]
the list of tokens corresponding to the current utterance.
Return in TokenListOut
the list of tokens corresponding to the remaining utterances.

How does this work?
The first token for each utterance is of the form
tok(sn,[],0,pos(_,_)),
so the entire token list will look like this:

tok(sn,[],0,pos(Utt1StartPos,Utt1EndPos)) %%% token signalling beginning of first utterance
tok(...)                                  %%% first  "real" token  of first utterance
tok(...)                                  %%% second "real" token  of first utterance
tok(...)                                  %%% third  "real" token  of first utterance
...                                       %%% intervening   tokens of first utterance
tok(...)                                  %%% last   "real" token  of first utterance
tok(sn,[],0,pos(Utt2StartPos,Utt2EndPos)) %%% token signalling beginning of second utterance
...                                       %%% remaining tokens for rest of input text

FirstToken is the first token of the current utterance.
To get the remaining tokens corresponding to the first utterance,
we need to recognize that
 *  the list of tokens for this utterance appended to
    the list of tokens for the rest of the utterances is equal to
    the entire list of tokens.
If we then specify that the first token of the rest of the utterances must be of the form
        tok(sn,[],0,pos(_,_))
we can then get the list of tokens for the first utterance. I.e.,

        append([FirstToken|RestTokensThisUtterance], RestTokensRestUtterances, AllTokens),
        RestTokensRestUtterances = [tok(sn,[],0,pos(_,_))|_]

*/

get_utterance_token_list(TokensIn, TokensThisUtterance, CharOffset, TokenListOut) :-
        remove_field_and_label_tokens(TokensIn, TokensOut),
        get_utterance_token_list_aux(TokensOut, TokensThisUtterance, CharOffset, TokenListOut).

get_utterance_token_list_aux([FirstToken|RestTokensIn],
			     TokensThisUtterance, CharOffset, TokenListOut) :-
        first_token_char_offset(FirstToken, CharOffset),
        get_utterance_token_list_1([FirstToken|RestTokensIn],
				   TokensThisUtterance,TokenListOut).
	% [FirstToken|RestTokensThisUtterance], TokenListOut).


get_utterance_token_list_1([FirstToken|RestTokensIn],
			   TokensThisUtterance,  TokenListOut) :-
	token_template(FirstToken, sn, [], 0, _PosInfo1, _PosInfo2),
	% FirstToken = tok(sn,[],0,_,_),
	token_template(NewFirstToken, sn, [], 0, _NewPosInfo1, _NewPosInfo2),
        append(RestTokensThisUtterance,
	       % [tok(sn,[],0,pos(StartPos,EndPos),pos(StartPos1,EndPos1))|RestTokenListOut],
	       [NewFirstToken|RestTokenListOut],
               RestTokensIn),
        !,
        append([FirstToken], RestTokensThisUtterance, TokensThisUtterance),
	% TokenListOut = [tok(sn,[],0,pos(StartPos,EndPos),pos(StartPos1,EndPos1))|RestTokenListOut].
	TokenListOut = [NewFirstToken|RestTokenListOut].
get_utterance_token_list_1([FirstToken|RestTokensIn], TokensThisUtterance, []) :-
	% FirstToken = tok(sn,[],0,_,_),
	token_template(FirstToken, sn, [], 0, _PosInfo1, _PosInfo2),
	!,
	TokensThisUtterance = [FirstToken|RestTokensIn].
get_utterance_token_list_1([_FirstToken|RestTokensIn], TokensThisUtterance, TokenListOut) :-
        get_utterance_token_list_1(RestTokensIn, TokensThisUtterance, TokenListOut).

% first_token_char_offset(tok(_,_,_,pos(StartPos,_EndPos),_), StartPos).
first_token_char_offset(FirstToken, StartPos) :-
	token_template(FirstToken, _TokenType, _TokenString, _LCString,
		       pos(StartPos,_EndPos), _PosInfo2).

remove_field_and_label_tokens([], []).
remove_field_and_label_tokens([Token|RestTokens], TokensOut) :-
	token_template(Token, TokenType, _TokenString, _LCTokenString, _PosInfo1, _PosInfo2),
	( field_or_label_type(TokenType) ->
	  TokensOut = RestTokensOut
	; TokensOut = [Token|RestTokensOut]
	),
	remove_field_and_label_tokens(RestTokens, RestTokensOut).
