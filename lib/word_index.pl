
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

% File:     word_index.pl
% Module:   Word Index
% Author:   Lan
% Purpose:  Access a (DB) Meta word index.


:- module(word_index, [
	get_filtered_uscs_for_word/7
    ]).

:- use_module(skr_db(db_access), [
	db_get_mwi_word_data/4
    ]).

:- use_module(metamap(metamap_tokenization), [
	tokenize_text_mm/2
    ]).

:- use_module(skr(skr_utilities), [
	debug_message/3
    ]).

:- use_module(skr_lib(sicstus_utils), [
	concat_atom/2,
	sublist/2
    ]).

:- use_module(library(avl), [
	avl_fetch/3,
	avl_store/4
    ]).

:- use_module(library(lists), [
	prefix/2
    ]).

/* get_filtered_uscs_for_word(+Table, +Word, +DebugFlags, +FilterWordStrings,
   			      +WordDataCacheIn, -WordDataCacheOut, -USCs)

get_filtered_uscs_for_word/7 returns a list of terms
        usc(NMStr,String,Concept)
for Word in Table.  FilterWordStrings must occur as a subsequence of
NMStr (an initial subsequence if Table is not all_words) where NMStr is the
normalized version of String.

Legal values for Table are
     all_words,
     first_words,
     first_wordsb,
     first_words_of_one, and
     first_words_of_two.  */

get_filtered_uscs_for_word(Table, Word, DebugFlags, FilterWordStrings,
			   WordDataCacheIn, WordDataCacheOut, USCs) :-
	debug_message(db, '~N### Calling db_get_mwi_word_data for ~q~n', [Word]),
	make_avl_key_1(Word, Table, AVLKey),
	( avl_fetch(AVLKey, WordDataCacheIn, USCs0) ->
	  WordDataCacheOut = WordDataCacheIn,
	  debug_message(db, '~N### WordData CACHE FOUND ~q from ~q~n', [Word,Table])
	; db_get_mwi_word_data(Table, Word, DebugFlags, USCs0) ->
	  avl_store(AVLKey, WordDataCacheIn, USCs0, WordDataCacheOut)
	; USCs0 = [],
	  avl_store(AVLKey, WordDataCacheIn, USCs0, WordDataCacheOut)
	),
	debug_message(db, '~N### db_get_mwi_word_data DONE~n', []),
	debug_message(db, '~N### Calling filter_uscs for ~q|~q~n', [Word,FilterWordStrings]),
	filter_uscs(Table, USCs0, FilterWordStrings, USCs),
	debug_message(db, '~N### filter_uscs DONE~n', []).

make_avl_key_1(Word, Table, AVLKey) :-
	concat_atom([Word,'-',Table], AVLKey).

filter_uscs(Table, USCs0, FilterWordStrings, USCs) :-
	( Table == all_words ->
	  filter_uscs_subseq(USCs0, FilterWordStrings, USCs)
	; filter_uscs_init_subseq(USCs0, FilterWordStrings, USCs)
	).

filter_uscs_subseq([], _, []).
filter_uscs_subseq([usc(UIString,S,C)|Rest], FilterWordStrings,
		   [usc(UIStringTokens,S,C)|FilteredRest]) :-   % why tokens?
	%%% temp fix for proliferated "other <n>" strings
	% UIString\=='other',
	% UIString\=='Other',
	tokenize_text_mm(UIString, UIStringTokens),
	sublist(UIStringTokens, FilterWordStrings),
	!,
	filter_uscs_subseq(Rest, FilterWordStrings, FilteredRest).
filter_uscs_subseq([_First|Rest], FilterWordStrings, FilteredRest) :-
	filter_uscs_subseq(Rest, FilterWordStrings, FilteredRest).

filter_uscs_init_subseq([], _, []).
filter_uscs_init_subseq([usc(UIString,S,C)|Rest], FilterWordStrings,
			[usc(UIStringTokens,S,C)|FilteredRest]) :- % why tokens?
	%%% temp fix for proliferated "other <n>" strings
	% UIString\=='other',
	% UIString\=='Other',
	tokenize_text_mm(UIString, UIStringTokens),
	prefix(UIStringTokens, FilterWordStrings),
	!,
	filter_uscs_init_subseq(Rest, FilterWordStrings, FilteredRest).
filter_uscs_init_subseq([_First|Rest], FilterWordStrings, FilteredRest) :-
	filter_uscs_init_subseq(Rest, FilterWordStrings, FilteredRest).
