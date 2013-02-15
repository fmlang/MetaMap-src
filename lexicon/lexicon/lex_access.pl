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

% File:	    lex_access.pl
% Module:   Lexicon Access
% Author:   Lan
% Purpose:  Provide access to the new lexicon access facility

:- module(lex_access, [
	% get_base_forms_for_form/4,
        get_categories_for_form/3,
	get_base_forms_for_form_with_cats/4,
	get_derivational_variants_for_form/4,
	get_spellings_and_inflections_for_form/5,
	get_variants_for_citation_form/3,
	get_variants_for_form/3,
	get_varlist/3,
	initialize_lexicon/2,
	% is_a_base_form/2,
	is_a_base_form_with_categories/2,
	% is_a_form/2,
	tokenize_string_for_lexical_lookup/2
    ]).

:- use_module(skr(testlvg), [
	lexAccess_get_base_forms_for_form/3,
	lexAccess_get_base_forms_for_form_with_cats/4,
	lexAccess_get_lex_form_cats/3,
	get_varlist_for_all_forms/3,
	% lexAccess_get_varlist_for_citation_form/3,
	lexAccess_get_varlist_for_form/4
	% lexAccess_is_form/2
	% lexAccess_is_root_form/2
	% lexAccess_is_root_form_with_cats/3
  ]).

:- use_module(lexicon(qp_lexicon), [
	lex_init/2,
	lex_cit_ci_vars/2,
	lex_form_ci_cats/2,
	lex_form_ci_recs/3,
	lex_form_ci_vars/2,
	lex_is_a_form_ci/1,
	lex_is_a_root_ci/1,
	lex_is_a_root_ci_cats/2,
	reformat_list/2
    ]).

:- use_module(lexicon(qp_token), [
	tokenize_string/2
    ]).

:- use_module(lexicon(qp_lex_util), [
	lex_form_ci_ord_5/5,
	lex_get_base_from_record_3/3
	% lex_get_spvar_from_record/2
    ]).

:- use_module(skr_lib(nls_system), [
        control_option/1,
        control_value/2
   ]).

:- use_module(skr_lib(sicstus_utils), [
        midstring/6
   ]).

:- use_module(skr(skr_utilities), [
        fatal_error/2
   ]).

:- use_module(library(lists), [
        append/2
   ]).

/* ************************************************************************
   ************************************************************************
   ************************************************************************
                               Lexicon Access
   ************************************************************************
   ************************************************************************
   ************************************************************************ */


/* initialize_lexicon(-Lexicon, -Index)

initialize_lexicon/2 and initialize_lexicon_quietly/2 call lex_init/2 and
lex_init_quietly/2, respectively. They also call c_initialize_lexAccess/0
for the new lexicon access. This initialization will eventually allow
the caller to specify lexiconVersion but for now it will be fixed. */

initialize_lexicon(L,I) :-
    % temp
    lex_init(L,I),
    !.
initialize_lexicon(_L, _I) :-
    fatal_error('Cannot connect to a lexicon.~n', []).

/* tokenize_string_for_lexical_lookup(+String, -TokenLists)

tokenize_string_for_lexical_lookup/2 calls tokenize_string/2. */

tokenize_string_for_lexical_lookup(S,T) :-
    tokenize_string(S,T).


%%% is_a_form(Form, LexiconServerStream) :-
%%% 	is_a_form_LEXACCESS_TOGGLE(Form, LexiconServerStream).
%%% 
%%% is_a_form_LEXACCESS_TOGGLE(Form, LexiconServerStream) :-
%%% 	( control_value(lexicon, c) ->
%%% 	  % format(user_output, 'is_a_form(~q)~n', [Form]),
%%% 	  lex_is_a_form_ci(Form)
%%%  	; lexAccess_is_form(Form, LexiconServerStream)
%%% 	).

%%% is_a_base_form(Form, LexiconServerStream) :-
%%% 	is_a_base_form_LEXACCESS_TOGGLE(Form, LexiconServerStream).
%%% 
%%% is_a_base_form_LEXACCESS_TOGGLE(Form, LexiconServerStream) :-
%%% 	( control_value(lexicon, c) ->
%%% 	  lex_is_a_root_ci(Form)
%%%  	; lexAccess_is_root_form(Form, LexiconServerStream)
%%% 	).	

is_a_base_form_with_categories(Form, Categories) :-
	  lex_is_a_root_ci_cats(Form, Categories).

%%% get_variants_for_citation_form(CitationForm, LexiconServerStream, VarList) :-
%%% 	get_variants_for_citation_form_LEXACCESS_TOGGLE(CitationForm, LexiconServerStream, VarList).
%%% 
%%% 
%%% get_variants_for_citation_form_LEXACCESS_TOGGLE(CitationForm, LexiconServerStream, VarList) :-
%%% 	( control_value(lexicon, c) ->
%%% 	  lex_cit_ci_vars(CitationForm, VarList)
%%%  	; lexAccess_get_varlist_for_citation_form(CitationForm, LexiconServerStream, VarList)
%%% 	).
	
% get_varlist first computes the citation form(s) for the token,
% then computes the variants of the citation form(s).
get_varlist(LexMatch, LexiconServerStream, VarInfo) :-
	get_varlist_LEXACCESS_TOGGLE(LexMatch, LexiconServerStream, VarInfo).

get_varlist_LEXACCESS_TOGGLE(LexMatch, LexiconServerStream, VarInfo) :-
	( control_value(lexicon, c) ->
	  lex_form_ci_vars(LexMatch, VarInfo)
 	; get_base_forms_for_form_apostrophe_s(LexMatch, LexiconServerStream, BaseForms),
	  sort(BaseForms, SortedBaseForms),
	  get_varlist_for_all_forms(SortedBaseForms, LexiconServerStream, VarInfo0),
	  sort(VarInfo0, VarInfo)
	).

% get_variants_for_form simply computes the variants of the given token,
% and not the variants of the token's citation form(s).
get_variants_for_form(Form, LexiconServerStream, VarList) :-
	get_variants_for_form_LEXACCESS_TOGGLE(Form, LexiconServerStream, VarList).

get_variants_for_form_LEXACCESS_TOGGLE(Form, LexiconServerStream, VarList) :-
	( control_value(lexicon, c) ->
	  lex_form_ci_vars(Form, VarList)
 	; lexAccess_get_varlist_for_form(Form, LexiconServerStream, VarList, [])
	).
	
get_categories_for_form(Form, LexiconServerStream, LexCats) :-
	get_categories_for_form_LEXACCESS_TOGGLE(Form, LexiconServerStream, LexCats).

get_categories_for_form_LEXACCESS_TOGGLE(Form, LexiconServerStream, LexCats) :-
	( control_value(lexicon, c) ->
	  lex_form_ci_cats(Form, LexCats)
 	; lexAccess_get_lex_form_cats(Form, LexiconServerStream, LexCats)
	).

get_spellings_and_inflections_for_form(Term, Categories, LexiconServerStream, Spelling, Inflections) :-
    lex_form_ci_ord_5(Term, Categories, LexiconServerStream, Spelling, Inflections).


get_base_forms_for_form_apostrophe_s(FormAtom, LexiconServerStream, BaseForms) :-
	( lexAccess_get_base_forms_for_form(FormAtom, LexiconServerStream, BaseForms),
	  BaseForms = [_|_] ->
	  true
	; midstring(FormAtom, FormAtomWithoutApostropheS, '''s', 0, _Length, 2),
	  lexAccess_get_base_forms_for_form(FormAtomWithoutApostropheS,
						LexiconServerStream, BaseForms),
	  BaseForms = [_|_] ->
	  true
	; format(user_output, '### WARNING: no base forms for "~q"!~n', [FormAtom]),
	  BaseForms = [FormAtom]
	).

get_base_forms_for_form_with_cats(Form, CategoryList, LexiconServerStream, BaseForms) :-
	get_base_forms_for_form_with_cats_LEXACCESS_TOGGLE(Form, CategoryList,
							   LexiconServerStream, BaseForms).

get_base_forms_for_form_with_cats_LEXACCESS_TOGGLE(Form, CategoryList,
						   LexiconServerStream, BaseForms) :-
 	( control_value(lexicon, c) ->
 	  lex_form_ci_recs(Form, LexiconServerStream, LexRecords),
 	   (findall(Cit,
 		    (member(LexRecord,LexRecords),
 		     lex_get_base_from_record_3(LexRecord,CategoryList,Cit)),
 		    BaseForms) ->
 	             true
 	   ;   BaseForms=[]
 	   )
 	; get_base_forms_for_form_apostrophe_s(Form, CategoryList,
					       LexiconServerStream, BaseForms)
 	).

get_base_forms_for_form_apostrophe_s(FormAtom, CategoryList,
				     LexiconServerStream, BaseForms) :-
	( lexAccess_get_base_forms_for_form_with_cats(FormAtom, CategoryList,
						      LexiconServerStream, BaseForms),
	  BaseForms = [_|_] ->
	  true
	; midstring(FormAtom, FormAtomWithoutApostropheS, '''s', 0, _Length, 2),
	  lexAccess_get_base_forms_for_form_with_cats(FormAtomWithoutApostropheS, CategoryList,
						      LexiconServerStream, BaseForms),
	  BaseForms = [_|_] ->
	  true
	; BaseForms = []
	).

/* 
%%%    get_base_forms_for_form(+Form, +Categories, +LexiconServerStream, -Bases)
%%% 
%%% get_base_forms_for_form/2 calls lex_form_ci_recs/2 followed by calls
%%% to lex_get_base_from_record/2 (where here, base really means citation)
%%% and lex_get_spvar_from_record/2.
%%% get_base_forms_for_form/3 respects Categories. */
%%% 
%%% get_base_forms_for_form(Form, Categories, LexiconServerStream, Bases) :-
%%% 	lex_form_ci_recs(Form, LexiconServerStream, LexRecords),
%%% 	add_base_forms2(LexRecords,Categories,[],Bases),
%%% 	!.
%%% 
%%% add_base_forms2([], _Categories, Bases, Bases).
%%% add_base_forms2([FirstLexRecord|RestLexRecords], Categories, BasesIn, BasesOut) :-
%%% 	( add_base_forms2(FirstLexRecord, Categories, FirstBases) ->
%%% 	  append(BasesIn, FirstBases, BasesInOut)
%%% 	; BasesInOut = BasesIn
%%% 	),
%%% 	add_base_forms2(RestLexRecords, Categories, BasesInOut, BasesOut).
%%% 
%%% add_base_forms2(LexRecord, Categories, Bases) :-
%%% 	lex_get_base_from_record_3(LexRecord, Categories, CitForm),
%%% 	( lex_get_spvar_from_record(LexRecord, Spvars) ->
%%% 	  Bases = [CitForm|Spvars]
%%% 	; Bases=[CitForm]
%%% 	).
