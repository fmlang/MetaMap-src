
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

:- module(lex_access,[
    % compare_lexAccess/3,
    initialize_lexicon/2,
    tokenize_string_for_lexical_lookup/2,
    assemble_definitions/2,
    is_a_form/1,
    is_a_base_form/1,
    is_a_base_form_with_categories/2,
    get_variants_for_citation_form/2,
    get_variants_for_form/2,
    get_derivational_variants_for_form/3,
    get_categories_for_form/2,
    get_spellings_and_inflections_for_form/4,
    get_citation_forms_for_form/2,
    get_citation_forms_for_form_with_cats/3,
    get_base_forms_for_form/3
    ]).

% :- use_module(skr(testlvg),[
% 	% lexAccess_get_citation_forms_for_form_init/3,
% 	lexAccess_get_lex_form_cats_init/2,
% 	lexAccess_get_varlist_for_citation_form_init/2,
% 	lexAccess_get_varlist_for_form_init/2,
% 	lexAccess_is_form_init/1,
% 	lexAccess_is_root_form_init/1,
% 	lexAccess_is_root_form_with_cats_init/2
%     ]).



% Old lexical access; will be obsolete

:- use_module(lexicon(qp_lexicon),[
    lex_init/2,
    lex_cit_ci_vars/2,
    lex_form_ci_cats/2,
    lex_form_ci_recs/2,
    lex_form_ci_vars/2,
    lex_is_a_form_ci/1,
    lex_is_a_root_ci/1,
    lex_is_a_root_ci_cats/2,
    reformat_list/2
    ]).

:- use_module(lexicon(qp_token),[
    tokenize_string/2
    ]).

:- use_module(lexicon(qp_lookup),[
    assembledefns/2
    ]).

:- use_module(lexicon(qp_lex_util),[
    lex_form_ci_ord_4/4,
    lex_get_base_from_record_3/3,
    lex_get_spvar_from_record/2
    ]).

:- use_module(morph(qp_morph), [
    dm_variants/3
    ]).

:- use_module(skr_lib(nls_system), [
        control_option/1,
        control_value/2
   ]).

:- use_module(library(lists), [
        append/2
   ]).

:- dynamic lexAccess_lexicon/1.

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

initialize_lexicon(_L,_I) :-
    lexAccess_lexicon(_),
    !.
initialize_lexicon(L,I) :-
    % temp
    lex_init(L,I),
%%    format('OLD c_initialize_lexAccess succeeded.~n',[]),
    % end temp
%%    stop_lex_access,
%%    c_initialize_lexAccess(1),
%%    LexiconVersion='Static2006Lexicon', % fixed for now
%%    form_open_session_request(LexiconVersion,Session),
%%    format('Session request = ~p~n',[Session]),
%%    c_query_2_atom(Session,Result),
%%    format('Session result = ~p~n',[Result]),
%%    assert(lexAccess_lexicon(LexiconVersion)),
    !.
initialize_lexicon(_L, _I) :-
    format('ERROR: Cannot connect to a lexicon.~n',[]),
    abort.

/* tokenize_string_for_lexical_lookup(+String, -TokenLists)

tokenize_string_for_lexical_lookup/2 calls tokenize_string/2. */

tokenize_string_for_lexical_lookup(S,T) :-
    tokenize_string(S,T).


/* assemble_definitions(+Input, -Recs)

assemble_definitions/2 calls assembledefns/2. */

assemble_definitions(I,R) :-
%      assembledefns_shortest(I,R).
     assembledefns(I,R).
%    format('assembledefns for~n~p~n~n~p~n~n',[I,R]).


/* is_a_form(+Form)
   is_a_form(+Form, -Result)

is_a_form/1 calls is_a_form/2 to determine if Form is in the lexicon using
the new lexicon access. */

% is_a_form(F) :-
%     !,
%     lex_is_a_form_ci(F).

is_a_form(Form) :-
	is_a_form_LEXACCESS_TOGGLE(Form).

is_a_form_LEXACCESS_TOGGLE(Form) :-
%	( control_value(lexicon, c) ->
	  lex_is_a_form_ci(Form).
% 	; control_value(lexicon, java) ->
% 	  lexAccess_is_form_init(Form)
%	; format(user_error, '### ERROR: lexicon setting must be either c or java!~n', []),
%	  abort
%	).

/* is_a_base_form(+Form)

is_a_base_form/1 calls lex_is_a_root_ci/1. */

% is_a_base_form(F) :-
%     lex_is_a_root_ci(F).

is_a_base_form(Form) :-
	is_a_base_form_LEXACCESS_TOGGLE(Form).

is_a_base_form_LEXACCESS_TOGGLE(Form) :-
%	( control_value(lexicon, c) ->
	  lex_is_a_root_ci(Form).
% 	; control_value(lexicon, java) ->
% 	  lexAccess_is_root_form_init(Form)
%	; format(user_error, '### ERROR: lexicon setting must be either c or java!~n', []),
%	  abort
%	).	


/* is_a_base_form_with_categories(+Form, +Cats)

is_a_base_form_with_categories/2 calls lex_is_a_root_ci_cats/2. */

% is_a_base_form_with_categories(F,C) :-
% %    length(C,N),
% %    format('@~d|is_a_base_form_with_categories|~p|~p~n',[N,F,C]),
%     lex_is_a_root_ci_cats(F,C).

is_a_base_form_with_categories(Form, Categories) :-
	is_a_base_form_with_categories_LEXACCESS_TOGGLE(Form, Categories).

is_a_base_form_with_categories_LEXACCESS_TOGGLE(Form, Categories) :-
%	( control_value(lexicon, c) ->
	  lex_is_a_root_ci_cats(Form, Categories).
% 	; control_value(lexicon, java) ->
% 	  lexAccess_is_root_form_with_cats_init(Form, Categories)
%	; format(user_error, '### ERROR: lexicon setting must be either c or java!~n', []),
%	  abort
%	).	

/* get_variants_for_citation_form(+Cit, -Vars)

get_variants_for_citation_form/2 calls lex_cit_ci_vars/2. */

% get_variants_for_citation_form(Citation, VarList) :-
%     lex_cit_ci_vars(Citation, VarListOld),
%     VarList = VarListOld,
%     lexAccess_get_varlist_for_citation_form_init(Citation, VarListNew),
%     compare_lexAccess(lexAccess_get_varlist_for_citation_form_init(Citation, 'VarList'),
% 		      VarListOld, VarListNew).
     

get_variants_for_citation_form(CitationForm, VarList) :-
	get_variants_for_citation_form_LEXACCESS_TOGGLE(CitationForm, VarList).


get_variants_for_citation_form_LEXACCESS_TOGGLE(CitationForm, VarList) :-
%	( control_value(lexicon, c) ->
	  lex_cit_ci_vars(CitationForm, VarList).
% 	; control_value(lexicon, java) ->
% 	  lexAccess_get_varlist_for_citation_form_init(CitationForm, VarList)
%	; format(user_error, '### ERROR: lexicon setting must be either c or java!~n', []),
%	  abort
%	).
	

/* get_variants_for_form(+Form, -Vars)

get_variants_for_form/2 calls lex_form_ci_vars/2. */

% get_variants_for_form(Form, VarList) :-
%     lex_form_ci_vars(Form,VarListOld),
%     VarList = VarListOld,
%     lexAccess_get_varlist_for_form_init(Form, VarListNew),
%     compare_lexAccess(lex_access_get_variants_for_form_init(Form, 'VarList'), VarListOld, VarListNew).
% 
% compare_lexAccess(Params, Old, New) :-
% 	sort(Old, SortedOld),
% 	sort(New, SortedNew),
% 	( SortedOld == SortedNew ->
% 	  true
% 	; format(user_output, '@@@ DIFF in ~w:~n', [Params]),
% 	  format(user_output, '@@@ OLD: ~q~n', [SortedOld]),
% 	  format(user_output, '@@@ NEW: ~q~n', [SortedNew])
% 	).

get_variants_for_form(Form, VarList) :-
	get_variants_for_form_LEXACCESS_TOGGLE(Form, VarList).

get_variants_for_form_LEXACCESS_TOGGLE(Form, VarList) :-
%	( control_value(lexicon, c) ->
	  lex_form_ci_vars(Form, VarList).
% 	; control_value(lexicon, java) ->
% 	  lexAccess_get_varlist_for_form_init(Form, VarList0),
% 	  append(VarList0, VarList)
%	; format(user_error, '### ERROR: lexicon setting must be either c or java!~n', []),
%	  abort
%	).
	
/* get_derivational_variants_for_form(+Term, +Cats, -Vars)

get_derivational_variants_for_form/3 calls dm_variants/3. */

get_derivational_variants_for_form(T,C,V) :-
%    length(C,N),
%    format('@~d|get_derivational_variants_for_form|~p|~p~n',[N,T,C]),
    dm_variants(T,C,V).


/* get_categories_for_form(+Form, -Cats)

get_categories_for_form/2 calls lex_form_ci_cats/2. */

get_categories_for_form(Form, LexCats) :-
	get_categories_for_form_LEXACCESS_TOGGLE(Form, LexCats).

get_categories_for_form_LEXACCESS_TOGGLE(Form, LexCats) :-
%	( control_value(lexicon, c) ->
	  lex_form_ci_cats(Form, LexCats).
% 	; control_value(lexicon, java) ->
% 	  lexAccess_get_lex_form_cats_init(Form, LexCats)
%	; format(user_error, '### ERROR: lexicon setting must be either c or java!~n', []),
%	  abort
%	).
	% compare_lexAccess(get_categories_for_form(Form, 'Cats'), LexCatsOld, LexCatsNew).


/* get_spellings_and_inflections_for_form(+Term, +Cats, -SPVars, -Infls)

get_spellings_and_inflections_for_form/4 calls lex_form_ci_ord_4/4. */

get_spellings_and_inflections_for_form(T,C,S,I) :-
%    length(C,N),
%    format('@~d|get_spellings_and_inflections_for_form|~p|~p~n',[N,T,C]),
    lex_form_ci_ord_4(T,C,S,I).
%    format('~nlex_form_ci_ord for ~p with cats ~p:~nSPVars=~p~nInfls=~p~n~n',
%	   [T,C,S,I]).


/* get_citation_forms_for_form(+Form, -Cits)
   get_citation_forms_for_form(+Form, +Categories, -Cits)

get_citation_forms_for_form/2 calls lex_form_ci_recs/2 followed by calls to
lex_get_base_from_record/2 (where here, base really means citation).
get_citation_forms_for_form/3 respects Categories. */

get_citation_forms_for_form(Form, CitationForms) :-
	get_citation_forms_for_form_LEXACCESS_TOGGLE(Form, CitationForms).

get_citation_forms_for_form_LEXACCESS_TOGGLE(Form, CitationForms) :-
%	( control_value(lexicon, c) ->
	  lex_form_ci_recs(Form,LexRecords),
	   (findall(Cit,
		    (member(LexRecord,LexRecords),
			lex_get_base_from_record_3(LexRecord,_CategoryList,Cit)),
		    CitationForms) ->
	             true
	   ;   CitationForms=[]
	   ).
%	; lexAccess_get_citation_forms_for_form_init(Form, CitationForms)
%	).


get_citation_forms_for_form_with_cats(Form, CategoryList, CitationForms) :-
	get_citation_forms_for_form_with_cats_LEXACCESS_TOGGLE(Form, CategoryList, CitationForms).

get_citation_forms_for_form_with_cats_LEXACCESS_TOGGLE(Form, CategoryList, CitationForms) :-
%	( control_value(lexicon, c) ->
	  lex_form_ci_recs(Form,LexRecords),
	   (findall(Cit,
		    (member(LexRecord,LexRecords),
			lex_get_base_from_record_3(LexRecord,CategoryList,Cit)),
		    CitationForms) ->
	             true
	   ;   CitationForms=[]
	   ).
%	; lexAccess_get_citation_forms_for_form_init(Form, CategoryList, CitationForms)
%	).

/* 
   get_base_forms_for_form(+Form, +Categories, -Bases)

get_base_forms_for_form/2 calls lex_form_ci_recs/2 followed by calls
to lex_get_base_from_record/2 (where here, base really means citation)
and lex_get_spvar_from_record/2.
get_base_forms_for_form/3 respects Categories. */

get_base_forms_for_form(Form,Categories,Bases) :-
    lex_form_ci_recs(Form,LexRecords),
    add_base_forms2(LexRecords,Categories,[],Bases),
    !.


add_base_forms2([],_Categories,Bases,Bases).
add_base_forms2([FirstLexRecord|RestLexRecords],Categories,BasesIn,BasesOut) :-
    (add_base_forms2(FirstLexRecord,Categories,FirstBases) ->
        append(BasesIn,FirstBases,BasesInOut)
    ;   BasesInOut=BasesIn
    ),
    add_base_forms2(RestLexRecords,Categories,BasesInOut,BasesOut).

add_base_forms2(LexRecord,Categories,Bases) :-
    lex_get_base_from_record_3(LexRecord,Categories,CitForm),
    (lex_get_spvar_from_record(LexRecord,Spvars) ->
        Bases=[CitForm|Spvars]
    ;   Bases=[CitForm]
    ).
