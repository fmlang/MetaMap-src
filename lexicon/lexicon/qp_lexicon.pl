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

/* qp_lexicon.pl - all lexicon-related predicates.
*/

/*
    Some general comments regarding predicate names:

    - All retrieval predicates start with the prefix "lex_"
    - After that comes one of "cit|root|form".  This specifies
      whether the search is done for a citation form, a root form
      (lexical base) or any form in the lexicon resp.
    - After that comes "cs|ci" meaning case sensitive or
      case insensitive search.
    - Finally comes "recs|vars|cats" for retrieval of records,
      variants, categories etc.

    All predicates have matching predicates with small arity
    for which the lexicon and index default to the standard.
*/

:- module(qp_lexicon, [
	lex_form_ci_recs/3,	% lexical form, case insensitive
	lex_form_ci_cats/2,	% lexical item, case insensitive, returns cats
	lex_cit_ci_vars/2,	% citation form, case insensitive, returns variants
	lex_form_ci_vars/2,	% lexical item, case insensitive, returns variants
	lex_form_ci_var_lists_5/5,
	lex_is_a_root_ci/1,	% root form, case insensitive
	lex_is_a_root_ci_cats/2,
	lex_is_a_form_ci/1,	% lexical form, case insensitive
	lex_form_ci_recs_input_7_LEXACCESS_TOGGLE/7,
        lex_init/2,
	default_lexicon_file/1,
	default_index_file/1,
	reformat_list/2
    ]).

:- use_module(skr(testlvg),[
	lexAccess_find_prefix/3,
	lexAccess_find_subterms/3,
	lexAccess_get_all_lexical_records/3,
 	lexAccess_get_lex_form_cats/3,
 	lexAccess_get_varlist_for_form/4
   ]).

:- use_module(lexicon(qp_fm_lexrec), [
	fm_lexical_record/3
   ]).

:- use_module(lexicon(qp_recio), [
	read_lex_record/3
   ]).

:- use_module(metamap(metamap_parsing), [
	re_attach_apostrophe_s_to_prev_word/3
   ]).

:- use_module(skr(skr_utilities), [
	 check_valid_file_type/2,
	 fatal_error/2,
	 send_message/2
    ]).

:- use_module(skr_lib(ctypes), [
	 is_alnum/1,
	 is_punct/1,
	 is_white/1
    ]).

:- use_module(skr_lib(flatten), [
	 flatten/2
    ]).

 :- use_module(skr_lib(mincoman), [
	 punc_mark1/1
    ]).

 :- use_module(skr_db(db_access), [
	 default_release/1
    ]).

 :- use_module(skr_lib(nls_system), [
	 control_option/1,
	 control_value/2
    ]).

 :- use_module(skr_lib(sicstus_utils), [
	 concat_atom/2,
	 lower/2
    ]).

 :- use_module(library(lists), [
	 append/2,
	 delete/3,
	 last/2,
	 rev/2,
	 sublist/5
    ]).

 :- use_module(library(ordsets), [
	 ord_symdiff/3
    ]).

:- use_module(library(sets), [
	 intersection/3
    ]).

 :- use_module(library(system), [
	 environ/2
    ]).

 :- dynamic default_lexicon_file/1.
 :- dynamic default_index_file/1.
 :- dynamic lexicon_files/2.

 %%% Define the foreign interface
 foreign_resource(qp_lexicon, [
	 % c_lex_cit, c_lex_root, c_lex_form,
	 c_lex_cit, c_lex_form,
	 c_lex_form,
	 % c_lex_cit_cats, c_lex_root_cats,
	 c_lex_form_cats,
	 c_lex_is_a_root, c_lex_is_a_form,
	 c_lex_is_a_root_cats,
	 c_lex_form_input,
	 c_get_varlist
     ]).

 % Replaced with new lexAccess code
 foreign(c_lex_cit,            c,
	 c_lex_cit(+string, +string, +integer, +integer, +integer, -term, [-integer])).

 % foreign(c_lex_root,           c,
 % 	c_lex_root(+string, +string, +integer, +integer, +integer, -term, [-integer])).

 foreign(c_lex_form,           c,
	 c_lex_form(+string, +string, +integer, +integer, +integer, -term, [-integer])).

 foreign(c_lex_form_cats,      c,
	 c_lex_form_cats(+string, +string, +integer, +integer, -term, [-integer])).

 foreign(c_lex_is_a_root,      c,
	 c_lex_is_a_root(+string, +string, +integer, +integer, [-integer])).

 foreign(c_lex_is_a_form,      c,
	 c_lex_is_a_form(+string, +string, +integer, +integer, [-integer])).

 foreign(c_lex_is_a_root_cats, c,
	 c_lex_is_a_root_cats(+string, +string, +integer, +integer, +term, [-integer])).

 foreign(c_lex_form_input,     c,
	 c_lex_form_input(+string, +integer, +term, -term, [-integer])).

 foreign(c_get_varlist,        c,
	 c_get_varlist(+string, +integer, -term, [-integer])).

 :- load_foreign_resource('../../qp_lexicon').

 %%% lex_init(-Lexicon, -Index)
 %%% This predicate will return the names of the default location of the lexicon files
lex_init(LexiconFile, LexiconIndex) :-
	lexicon_files(LexiconFile, LexiconIndex),
	!.  % already initialized
lex_init(LexiconFile, LexiconIndex) :-
	lex_init_quietly(LexiconFile, LexiconIndex),
	conditionally_announce_lexicon(LexiconFile).

lex_init_quietly(LexiconFile, LexiconIndex) :-
	lexicon_files(LexiconFile, LexiconIndex),
	!.  % already initialized
lex_init_quietly(LexiconFile, LexiconIndex) :-
	get_lexicon_year(LexiconYear),
	% environ('DEFAULT_LEXICON_FILE', Lexicon),
	environ('LEXICON_DATA', LexiconDataDir),
	concat_atom([LexiconDataDir, '/lexiconStatic', LexiconYear], LexiconFile),
	check_valid_file_type(LexiconFile, 'Lexicon'),
	% environ('DEFAULT_LEXICON_INDEX_FILE', Index),
	concat_atom([LexiconFile, 'Ind'], LexiconIndex),
	concat_atom([LexiconIndex, 'ByEui.dbx'], EuiIndexFile),
	check_valid_file_type(EuiIndexFile, 'Lexicon Index'),
	concat_atom([LexiconIndex, 'ByInfl.dbx'], InflIndexFile),
	check_valid_file_type(InflIndexFile, 'Lexicon Index'),
	retractall(default_lexicon_file(_)),
	assert(default_lexicon_file(LexiconFile)),
	retractall(default_index_file(_)),
	assert(default_index_file(LexiconIndex)),
	retractall(lexicon_files(_,_)),
	assert(lexicon_files(LexiconFile,LexiconIndex)).

get_lexicon_year(NormalizedLexiconYear) :-
	default_release(DefaultRelease),
	atom_codes(DefaultRelease, Codes),
	Codes = [D1,D2,D3,D4,_AorB1,_AorB2],
	atom_codes(LexiconYear, [D1,D2,D3,D4]),
	normalize_lexicon_year(LexiconYear, NormalizedDefaultLexiconYear),
	% Is the lexicon year explicitly specified on the command line?
	( control_value(lexicon_year, SpecifiedLexiconYear),
	  normalize_lexicon_year(SpecifiedLexiconYear, NormalizedSpecifiedLexiconYear),
	  NormalizedDefaultLexiconYear \== NormalizedSpecifiedLexiconYear ->
	  send_message('### WARNING: Overriding default lexicon ~w with ~w.~n',
		       [NormalizedDefaultLexiconYear, SpecifiedLexiconYear]),
	  NormalizedLexiconYear = NormalizedSpecifiedLexiconYear
	; NormalizedLexiconYear = NormalizedDefaultLexiconYear
	).

normalize_lexicon_year(LexiconYear, NormalizedLexiconYear) :-
	( LexiconYear == '99' ->
	  NormalizedLexiconYear = '1999'
	; LexiconYear == '1999' ->
	  NormalizedLexiconYear = LexiconYear
	; atom_length(LexiconYear, LexiconLength),
	  ( LexiconLength =:= 2 ->
	    concat_atom(['20', LexiconYear], NormalizedLexiconYear)
	  ; NormalizedLexiconYear = LexiconYear
	  )
	).

conditionally_announce_lexicon(Lexicon) :-
	( \+ control_option(silent) ->
	  format('Accessing lexicon ~a.~n', [Lexicon])
	; true
	).

 %%% Retrieve records given root form

lex_form_ci_recs(Form, LexiconServerStream, LexicalRecords) :-
	lex_form_ci_recs_LEXACCESS_TOGGLE(Form, LexiconServerStream, LexicalRecords).

lex_form_ci_recs_LEXACCESS_TOGGLE(Form, LexiconServerStream, LexicalRecords) :-
	( control_value(lexicon, c) ->
	  default_lexicon_file(Lexicon),
	  default_index_file(Index),
	  lex_recs(form, Form, LexicalRecords, 1, 0, Lexicon, Index)
	; control_value(lexicon, java) ->
	  lexAccess_get_all_lexical_records([Form], LexiconServerStream, LexicalRecords)
	; fatal_error('Lexicon setting must be either c or java!~n', [])
	).

%%% Retrieves category given root form

lex_form_ci_cats(Form, LexicalCategories) :-
	  default_index_file(Index),
	  lex_cats(form, Form, LexicalCategories, 1, Index).


%% Retrieves inflectional variant info for a citation form

%%% lex_cit_ci_vars(+Cit, -Vars)
lex_cit_ci_vars(Cit, Vars) :-
	default_lexicon_file(Lexicon),
	default_index_file(Index),
	lex_vars(cit, Cit, Vars, 1, Lexicon, Index).

%%% Retrieves inflectional variant info for a lexical item

%%% lex_form_ci_vars(+Form, -Vars)
lex_form_ci_vars(Form, Vars) :-
	default_lexicon_file(Lexicon),
	default_index_file(Index),
	lex_vars(form, Form, Vars, 1, Lexicon, Index).

lex_form_ci_var_lists_5(Form, Lexicon, Index, LexiconServerStream, VarLists) :-
	lex_form_ci_var_lists_5_LEXACCESS_TOGGLE(Form, Lexicon, Index, LexiconServerStream, VarLists).

lex_form_ci_var_lists_5_LEXACCESS_TOGGLE(Form, Lexicon, Index, LexiconServerStream, VarLists) :-
	( control_value(lexicon, c) ->
	  lex_var_lists(form, Form, VarLists, 1, Lexicon, Index)
	; control_value(lexicon, java) ->
	  lexAccess_get_varlist_for_form(Form, LexiconServerStream, VarLists0, []),
	  VarLists = [VarLists0]
	; fatal_error('Lexicon setting must be either c or java!~n', [])
	).

%%% generic record retrieval predicate
lex_recs(form, Form, Rec, LowerFlag, FlushFlag, Lexicon, Index) :-
	LexiconType = 0,
	c_lex_form(Index, Form, LexiconType, LowerFlag, FlushFlag, OfsList, 1),
	sort(OfsList, SortedOfsList),
	get_records_from_offsets(SortedOfsList, Rec, Lexicon).

%%% generic category retrieval predicate
lex_cats(form, Form, Cats, LowerFlag, Index) :-
	LexiconType = 0,
	% LexiconType = 0,
	c_lex_form_cats(Index, Form, LexiconType, LowerFlag, Cats, 1).
	% format(user_output, '~q~n', [c_lex_form_cats(Index, Form, LexiconType, LowerFlag, Cats, 1)]).

%%% generic variant retrieval predicate
% Replaced with new lexAccess code
lex_vars(cit, Cit, Vars, LowerFlag, Lexicon, Index) :-
	lex_vars_cit(Cit, Vars, LowerFlag, Lexicon, Index).
lex_vars(form, Form, Vars, LowerFlag, Lexicon, Index) :-
	lex_vars_form(Form, Vars, LowerFlag, Lexicon, Index).

% Replaced with new lexAccess code
lex_vars_cit(Cit, Vars, LowerFlag, Lexicon, Index) :-
	LexiconType = 0,
	c_lex_cit(Index, Cit, LexiconType, LowerFlag, 0, OfsList, 1),
	sort(OfsList, SortedOfsList),
	lex_vars_aux(SortedOfsList, Vars, Lexicon).

lex_vars_form(Form, Vars, LowerFlag, Lexicon, Index) :-
	LexiconType = 0,
	c_lex_form(Index, Form, LexiconType, LowerFlag, 0, OfsList, 1),
	sort(OfsList, SortedOfsList),
	lex_vars_aux(SortedOfsList, Vars, Lexicon).

%%% auxiliary
lex_vars_aux(OfsList, Vars, Lexicon) :-
	lex_vars_aux_1(OfsList, Lexicon, Vars0),
	flatten(Vars0, FlattenedVars),
	reformat_list(FlattenedVars, ReformattedFlattenedVars),
	sort(ReformattedFlattenedVars, Vars).

lex_vars_aux_1([], _Lexicon, []).
lex_vars_aux_1([Ofs|RestOfsList], Lexicon, [Vars|RestVars]) :-
	c_get_varlist(Lexicon, Ofs, Vars, 1),
	% format(user_output, '~q~n', [c_get_varlist(lexicon, Ofs, Vars, 1)]),
	lex_vars_aux_1(RestOfsList, Lexicon, RestVars).

%%% generic variant retrieval predicate
%%% LRA
lex_var_lists(form, Form, VarLists, LowerFlag, Lexicon, Index) :-
	LexiconType = 0,
	c_lex_form(Index, Form, LexiconType, LowerFlag, 0, OfsList, 1),
	sort(OfsList, SortedOfsList),
	lex_var_lists_aux(SortedOfsList, Lexicon, VarLists).

%%% auxiliary
lex_var_lists_aux([], _, []).
lex_var_lists_aux([Ofs|Rest], Lexicon, [V|RestV]) :-
	c_get_varlist(Lexicon, Ofs, V0, 1),
	% format(user_output, '~q~n', [c_get_varlist(lexicon, Ofs, V0, 1)]),
	reformat_list(V0, V1),
	sort(V1, V),
	lex_var_lists_aux(Rest, Lexicon, RestV).

/* Simple queries */

%%% lex_is_a_root_ci(+Root)
%%% Queries for a root form.
lex_is_a_root_ci(Root) :-
	default_index_file(Index),
	lex_is_a_root_ci_2(Root, Index).

lex_is_a_root_ci_2(Root, Index) :-
	LexiconType = 0,
	c_lex_is_a_root(Index, Root, LexiconType, 1, 1).
	% format(user_output, '~q~n', [c_lex_is_a_root(Root, Return)]).

%%% lex_is_a_form_ci(+Form)
%%% Queries for a lexical item.
lex_is_a_form_ci(Form) :-
	default_index_file(Index),
	lex_is_a_form_ci_2(Form, Index).

lex_is_a_form_ci_2(Form, Index) :-
	LexiconType = 0,
	c_lex_is_a_form(Index, Form, LexiconType, 1, 1).
	% format(user_output, '~q~n', [c_lex_is_a_form(Form, Return)]).

%%% lex_is_a_root_ci_cats(+Root, +Cats)
%%% succeeds if +Root is a root form in any category in +Cats
lex_is_a_root_ci_cats(Root, Cats) :-
	default_index_file(Index),
	lex_is_a_root_ci_cats_3(Root, Cats, Index).

lex_is_a_root_ci_cats_3(Root, Cats, Index) :-
	LexiconType = 0,
	c_lex_is_a_root_cats(Index, Root, LexiconType, 1, Cats, 1).

lex_form_ci_recs_input_7_LEXACCESS_TOGGLE(Input, Recs, Remaining, TagList,
					  LexiconServerStream, Lexicon, Index) :-
	( control_value(lexicon, c) ->
	  lex_form_ci_recs_input_6_C(Input, OldRecs, OldRemaining, TagList, Lexicon, Index),
	  Recs = OldRecs,
	  Remaining = OldRemaining
	; control_value(lexicon, java) ->
	  lex_form_ci_recs_input_7_JAVA(Input, NewRecs, NewRemaining, TagList,
					LexiconServerStream, Lexicon, Index),
	  Recs = NewRecs,
	  Remaining = NewRemaining
	; fatal_error('Must specify lexicon setting (c or java).~n', []),
	  abort
	).

lex_form_ci_recs_input_6_C(Input, OrigRecs, OrigRemaining, TagList, Lexicon, Index) :-
	LexiconType = 0,
	c_lex_form_input(Index, LexiconType, Input, Matches, 1),
	sort_matches(Matches, SortedMatches),
	get_best_match(SortedMatches, BestMatch),
	BestMatch = match(OrigLexMatch, OfsList, BestLength),
	get_records_from_offsets(OfsList, OrigLexRecs, Lexicon),
	skip_n_tokens(BestLength, Input, OrigInputMatch0, OrigRemaining),
	re_attach_apostrophe_s_to_prev_word(OrigInputMatch0, TagList, OrigInputMatch),
	% re-glue the apostrophe-s in OrigInputMatch?
	% just to avoid compiler warning about singleton var!
	OrigRemaining = OrigRemaining,
	OrigRecs = lexicon:[lexmatch:[OrigLexMatch], inputmatch:OrigInputMatch, records:OrigLexRecs].

lex_form_ci_recs_input_7_JAVA(InputTokenList, LexRecs, RemainingTokens, TagList,
			      LexiconServerStream, _Lexicon, _Index) :-
	lexAccess_find_prefix(InputTokenList, LexiconServerStream, PrefixResults),
	% lexAccess_find_subterms(InputTokenList, LexiconServerStream, PrefixResults),
	PrefixResults = [FirstPrefixResult|_],
	FirstPrefixResult = NegConsumedTokenCount-LexMatch-EUIList,
	ConsumedTokenCount is -NegConsumedTokenCount,
	skip_n_tokens(ConsumedTokenCount, InputTokenList, InputMatchList0, RemainingTokens0),
	% the "''" token is introduced in retokenize.pl to prevent "in patients"
	% and similar token sequences from being analyzed as a lexical item.
	% The Java code consumes the "''" token, but we can't allow it in the InputMatchList list.
	( append(AllButLast, [''], InputMatchList0) ->
	  InputMatchList1 = AllButLast
	; InputMatchList1 = InputMatchList0
	),
	RemainingTokens = RemainingTokens0,
	InputMatchList2 = InputMatchList1,
	re_attach_apostrophe_s_to_prev_word(InputMatchList2, TagList, InputMatchList),
        lexAccess_get_all_lexical_records(EUIList, LexiconServerStream, AllLexicalEntries),
	% There can be multiple base forms, so get them all
	% LexicalEntries = [lexrec:[base:[LexRecBaseForm]|_]|_],
	LexRecs  = lexicon:[lexmatch:[LexMatch],
			   inputmatch:InputMatchList,
			   records:AllLexicalEntries].


get_all_lexmatches([], LexMatches, LexMatches).
get_all_lexmatches([H|T], [LCLexMatch-LCLexMatch|LexMatcheNext], LexMatchesOut) :-
	H = _NegConsumedTokenCount-LexMatch-_EUIList,
	lower(LexMatch, LCLexMatch),
	get_all_lexmatches(T, LexMatcheNext, LexMatchesOut).

%%% gets the best match, and collapses all offsets
get_best_match([FirstMatch|RestMatches], N) :-
	FirstMatch = match(Type, Term, Ofs, Length),
	% The `C' lexicon code recognizes "HE's" as a lexical item. Go figure.
	Term \== 'HE''s',
	!,
	( findall(SomeOfs, member(match(Type, Term, SomeOfs, Length), RestMatches), MoreOfs) ->
	 N = match(Term, [Ofs|MoreOfs], Length)
	; N = match(Term, [Ofs], Length)
	).
get_best_match([_|RestMatches], N) :-
	get_best_match(RestMatches, N).


%%% skip_n_tokens(+N, +List, -NTok, -Remain)
%%% returns the result of skipping 'n' tokens in a list

skip_n_tokens(N, List, Tokens, Remaining) :-
	( N =:= 0 ->
	 Tokens = [],
	 Remaining = List
	; N1 is N - 1,
	 List = [FirstTokenInList|RestTokensInList],
	 Tokens = [FirstTokenInList|RestTokensChosen],
	 skip_n_tokens(N1, RestTokensInList, RestTokensChosen, Remaining)
	).

%%% get_records_from_offsets(+OfsList, -RecsList, Lexicon)
get_records_from_offsets([], [], _Lexicon).
get_records_from_offsets([Ofs|R], [X|Y], Lexicon) :-
	read_lex_record(Lexicon, Ofs, Rec),
	fm_lexical_record(X, Rec, []),
	get_records_from_offsets(R, Y, Lexicon).

%%% sort_matches(+In, -Out)
%%% sorts matches by decreasing match length.  Also, discards
%%% lower case matches in favor of and exact match of same length
%%% and record offset.
sort_matches(In, Out) :-
	make_keys(In, InKeys),
	keysort(InKeys, OutKeys),
	rev(OutKeys, RevOutKeys),
	make_unkeys(RevOutKeys, UnOut),
	prune(UnOut, Out).

%%% constructs keys for sorting
make_keys([], []).
make_keys([F1|R1], [F2|R2]) :-
	F1 = match(Type, _Term, Ofs, Length),
	map_type(Type, MappedType),
	F2 = key(Length, MappedType, Ofs) - F1,
	make_keys(R1, R2).

map_type(punct, 0).
map_type(lower, 1).
map_type(exact, 2).

%%% removes keys
make_unkeys([], []).
make_unkeys([F1|R1], [F2|R2]) :-
	F1 = _Key - Term,
	F2 = Term,
	make_unkeys(R1, R2).

%%% Between matches of the same length and offset, prefers the exact.
prune([], []).
prune([X], [X]) :- !.
prune([X1, X2|R1], Y) :-
	X1 = match(exact, _T1, Ofs, Length),
	( X2 = match(lower, _T2, Ofs, Length)
	; X2 = match(punct, _T3, Ofs, Length)
        ),
	!,
	prune([X1|R1], Y).
prune([F|R1], [F|R2]) :-
	prune(R1, R2).

reformat_list([], []).
reformat_list([F|R], [X|Y]) :-
	functor(F, Term, 2),
	arg(1, F, Cat),
	arg(2, F, Feature),
	X = Term:[Cat:[Feature]],
	reformat_list(R, Y).
