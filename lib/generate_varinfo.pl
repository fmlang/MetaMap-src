
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

% File:	    generate_varinfo.pl
% Module:   generate_varinfo
% Author:   tcr
% Purpose:  generate variant information from a lexical entry



% ----- Module declaration and exported predicates

:- module(generate_varinfo, [
	generate_variant_info/3
   ]).


% ----- Imported predicates

:- use_module(skr(skr_utilities), [
	fatal_error/2
  ]).

:- use_module(lexicon(lex_access), [
	get_variants_for_form/3,
	get_varlist/3
  ]).

:- use_module(skr_lib(nls_system), [
        control_option/1,
        control_value/2
   ]).

:- use_module(skr_lib(sicstus_utils), [
	lower/2,
	midstring/6,
	string_size/2
  ]).


% ******************************* GENERATE_VARIANT_INFO *******************************

generate_variant_info([], _LexiconServerStream, []).
generate_variant_info([LexiconOrOther:List|RestDefinitions], LexiconServerStream, Variants) :-
	generate_variant_info_1(LexiconOrOther, LexiconServerStream, List, RestDefinitions, Variants).

generate_variant_info_1(unknown, LexiconServerStream, UnkList, RestDefinitions, [ThisItem|NewGap]) :-
	!,
	get_lex_item(UnkList, inputmatch, InpMatch),
	InpMatch = [ThisItem],
	generate_variant_info(RestDefinitions, LexiconServerStream, NewGap).
generate_variant_info_1(lexicon, LexiconServerStream,
			[lexmatch:[LexMatch], InputMatch|_],
			RestDefinitions, [LexMatch:ThisVarInfo|NewGap]) :-
	!,
	lower(LexMatch, LexMatchLC),
	get_varlist(LexMatchLC, LexiconServerStream, VarInfo),
	get_this_variant(VarInfo, LexMatch, ThisVarInfo, VariantTail),
	% format(user_output, 'ThisVarInfo: ~q~n', [ThisVarInfo]),	
	% append(ThisVarInfo, [InputMatch], ThisVarInfoAndInputMatch),   % Lan needs InputMatch
	VariantTail = [InputMatch], 
	generate_variant_info(RestDefinitions, LexiconServerStream, NewGap).

% This is for shapes, punctuation, and perhaps other stuff
generate_variant_info_1(Other, LexiconServerStream, OtherList,
			RestDefinitions, [Other:OtherList|NewGap]) :-
	generate_variant_info(RestDefinitions, LexiconServerStream, NewGap).

% ----- GET_LEX_ITEM

get_lex_item([Item:ItemInfo|_More], Item, ItemInfo) :-
	!.
get_lex_item([_Other|More], Item, ItemInfo) :-
	get_lex_item(More, Item, ItemInfo).

% ----- GET_THIS_VARIANT
% LexKeys other than forms of *be* and *have* have the format: Entry:VarList
% LexKeys for forms of *be* have the format: 'VarForm;Agr':VarList

get_this_variant([], _ThisWord, ThisVarInfo, ThisVarInfo).
get_this_variant([LexKey:[ThisList]|MoreVariants], ThisWord, [ThisList|Rest], Tail) :-
	% I believe this predicate is useless
	% get_actual_lex_key(LexKey, ActualLexKey, _AgrInfo),
	ActualLexKey = LexKey,
	lower(ActualLexKey, LowerLexKey),
	% ThisWord can end in "'s"
	lower_apostrophe_s(ThisWord, LowerLexKey),
	!,
	% format(user_output, 'ThisList: ~q~n', [ThisList]),
	get_this_variant(MoreVariants, ThisWord, Rest, Tail).
get_this_variant([_Other|MoreVariants], ThisWord, ThisVarInfo, Rest) :-
	get_this_variant(MoreVariants, ThisWord, ThisVarInfo, Rest).


lower_apostrophe_s(ThisWord, LowerLexKey) :-
	( lower(ThisWord, LowerLexKey) ->
	  true
	; midstring(ThisWord, ThisWordWithoutApostropheS, '''s', 0, _Length, 2),
	  lower(ThisWordWithoutApostropheS, LowerLexKey)
	).
