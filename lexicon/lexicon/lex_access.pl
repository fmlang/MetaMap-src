% File:	    lex_access.pl
% Module:   Lexicon Access
% Author:   Lan
% Purpose:  Provide access to the new lexicon access facility

:- module(lex_access,[
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
    get_citation_forms_for_form/3,
    get_base_forms_for_form/3
    ]).


% Old lexical access; will be obsolete

:- use_module(lexicon(qp_lexicon),[
    lex_init/2,
    lex_cit_ci_vars/2,
    lex_form_ci_cats/2,
    lex_form_ci_recs/2,
    lex_form_ci_vars/2,
    lex_is_a_form_ci/1,
    lex_is_a_root_ci/1,
    lex_is_a_root_ci_cats/2
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

:- use_module(morph(qp_morph),[
    dm_variants/3
    ]).

:- dynamic lex_access_lexicon/1.

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
    lex_access_lexicon(_),
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
%%    assert(lex_access_lexicon(LexiconVersion)),
    !.
initialize_lexicon(_L, _I) :-
    format('ERROR: Cannot connect to a lexicon server.~n',[]),
    fail.

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

is_a_form(F) :-
    !,
    lex_is_a_form_ci(F).

/* is_a_base_form(+Form)

is_a_base_form/1 calls lex_is_a_root_ci/1. */

is_a_base_form(F) :-
    lex_is_a_root_ci(F).


/* is_a_base_form_with_categories(+Form, +Cats)

is_a_base_form_with_categories/2 calls lex_is_a_root_ci_cats/2. */

is_a_base_form_with_categories(F,C) :-
%    length(C,N),
%    format('@~d|is_a_base_form_with_categories|~p|~p~n',[N,F,C]),
    lex_is_a_root_ci_cats(F,C).


/* get_variants_for_citation_form(+Cit, -Vars)

get_variants_for_citation_form/2 calls lex_cit_ci_vars/2. */

get_variants_for_citation_form(C,V) :-
    lex_cit_ci_vars(C,V).


/* get_variants_for_form(+Form, -Vars)

get_variants_for_form/2 calls lex_form_ci_vars/2. */

get_variants_for_form(F,V) :-
    lex_form_ci_vars(F,V).


/* get_derivational_variants_for_form(+Term, +Cats, -Vars)

get_derivational_variants_for_form/3 calls dm_variants/3. */

get_derivational_variants_for_form(T,C,V) :-
%    length(C,N),
%    format('@~d|get_derivational_variants_for_form|~p|~p~n',[N,T,C]),
    dm_variants(T,C,V).


/* get_categories_for_form(+Form, -Cats)

get_categories_for_form/2 calls lex_form_ci_cats/2. */

get_categories_for_form(F,C) :-
    lex_form_ci_cats(F,C).


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

get_citation_forms_for_form(Form,Categories,Cits) :-
    lex_form_ci_recs(Form,LexRecords),
    (findall(Cit,
             (member(LexRecord,LexRecords),
	      lex_get_base_from_record_3(LexRecord,Categories,Cit)),
	     Cits) ->
        true
    ;   Cits=[]
    ).

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



%%%%% 
%%%%% /* ************************************************************************
%%%%%    ************************************************************************
%%%%%    ************************************************************************
%%%%%                                  Utilities
%%%%%    ************************************************************************
%%%%%    ************************************************************************
%%%%%    ************************************************************************ */
%%%%% 
%%%%% 
%%%%% /* form_query(+Type, +Argument, -Query)
%%%%% 
%%%%% form_query/3 forms an XML Query from Type and Argument. Possible values
%%%%% for Type are
%%%%%   xxx
%%%%% */
%%%%% 
%%%%% form_query(Type,Argument,Query) :-
%%%%%     % the following is just wrong, concatenate_... accomplishes it
%%%%% %    preprocess_argument(Argument0,Argument1),
%%%%%     concatenate_items_to_atom(['<Query QueryType="',Type,'">',
%%%%% 			       '<QueryTerm>',Argument,'</QueryTerm>',
%%%%% 			       '</Query>'],
%%%%% 			      Query),
%%%%%     !.
%%%%% 
%%%%% 
%%%%% /* form_open_session_request(+LexiconVersion, -Request)
%%%%%    form_close_session_request(-Request)
%%%%% 
%%%%% form_open_session_request/2 forms an XML Request indicating which version of
%%%%% the lexicon to use and specifying Prolog output. Possible values for
%%%%% LexiconVersion are
%%%%%   Static2000Lexicon
%%%%%   Static2006Lexicon
%%%%%   Static2006Lexicon
%%%%%   Reduced2006Lexicon
%%%%% form_close_session_request/1 forms an XML Request closing the current
%%%%% session.
%%%%% */
%%%%% 
%%%%% form_open_session_request(LexiconVersion,Request) :-
%%%%%     concatenate_items_to_atom(['<Session lexiconVersion="',LexiconVersion,'" ',
%%%%% 			       'outputFormat="Prolog">'],
%%%%% 			      Request),
%%%%%     !.
%%%%% 
%%%%% form_close_session_request(Request) :-
%%%%%     Request='</Session>',
%%%%%     !.
%%%%% 
%%%%% 
%%%%% %/* preprocess_argument(+ArgumentIn, -ArgumentOut)
%%%%% %
%%%%% %preprocess_argument/2 doubles each single quote character in the atom
%%%%% %ArgumentIn producing string ArgumentOut. */
%%%%% %
%%%%% %preprocess_argument(ArgumentIn,ArgumentOut) :-
%%%%% %    atom_codes(ArgumentIn,Argument1),
%%%%% %    double_squotes(Argument1,ArgumentOut).
%%%%% %
%%%%% %
%%%%% %/* double_squotes(+String, -ModifiedString)
%%%%% %
%%%%% %double_squotes/2 doubles each single quote character in String producing
%%%%% %ModifiedString. */
%%%%% %
%%%%% %double_squotes("","") :-
%%%%% %    !.
%%%%% %double_squotes([0''|Rest],[0'',0''|ModifiedRest]) :-
%%%%% %    !,
%%%%% %    double_squotes(Rest,ModifiedRest).
%%%%% %double_squotes([First|Rest],[First|ModifiedRest]) :-
%%%%% %    double_squotes(Rest,ModifiedRest).
%%%%% 
%%%%% 
%%%%% /* execute_query(+Query, -Result)
%%%%% 
%%%%% execute_query/2 executes the XML Query producing (Prolog-formatted) Result. */
%%%%% 
%%%%% c_query_to_atom(Query, Results) :-
%%%%% 	format('The lexAccess config file is no longer used~n',[]).
%%%%% 
%%%%% execute_query(Query,Result) :-
%%%%%     c_query_2_atom(Query,Result).
%%%%% 
%%%%% 
%%%%% /* parse_query_results(+QueryResults, -ParsedResults)
%%%%% 
%%%%% parse_query_results/2 parses raw QueryResults (an atom) into a Prolog term,
%%%%% ParsedResults. QueryResults almost has the form of a ground Prolog term except
%%%%% that "atoms" can have initial caps and may contain spaces. Specifically,
%%%%% QueryResults is a list of terms each of which is one of the following:
%%%%%   a functor with arguments which are terms,
%%%%%   a list of terms, or
%%%%%   an "atom". */
%%%%% 
%%%%% parse_query_results(QueryResults,ParsedResults) :-
%%%%%     atom_codes(QueryResults,QRString),
%%%%%     phrase(term_list(ParsedResults),QRString),
%%%%%     !.
%%%%% parse_query_results(QueryResults,error) :-
%%%%%     format('ERROR: Cannot parse ~p~nReturning ''error''.~n',[QueryResults]).
%%%%% 
%%%%% 
%%%%% % ---------------------------------------------------------
%%%%% % -------------- Limited Prolog Term Grammar --------------
%%%%% % ---------------------------------------------------------
%%%%% 
%%%%% term_list(Ts) -->
%%%%% 	  "[]", !, {Ts=[]}
%%%%% 	| "[" , !, term(T), rest_term_list(Us), {Ts=[T|Us]}.
%%%%% 
%%%%% rest_term_list(Ts) -->
%%%%% 	  "]", !, {Ts=[]}
%%%%% 	| ",", !, term(T), rest_term_list(Us), {Ts=[T|Us]}.
%%%%% 
%%%%% term(T) -->
%%%%% 	  term_list(T), !
%%%%% 	| name(N), "()", !, {form_functor(N,[],T)}
%%%%% 	| name(N), "(", !, term(A), rest_arg_list(As),
%%%%% 	  % temporary fix until Guy fixes SpellingVariants
%%%%% 	  {length(As,LenAs), (LenAs=:=0 ->
%%%%% 				 form_functor(N,[A],T)
%%%%% 			     ;   form_functor(N,[[A|As]],T)
%%%%% 			     )}
%%%%% %	  {form_functor(N,[A|As],T)}
%%%%% 	| name(T).
%%%%% 
%%%%% name(N) --> name_chars(Cs), !, {atom_codes(N,Cs)}.
%%%%% 
%%%%% name_chars(Cs) -->
%%%%% 	  [C], {is_name_char(C)}, !, name_chars(Ds), {Cs=[C|Ds]}
%%%%% 	| {Cs=[]}.
%%%%% 
%%%%% is_name_char(C) :- \+is_non_name_char(C).
%%%%% 
%%%%% is_non_name_char(0',).
%%%%% is_non_name_char(0'[).
%%%%% is_non_name_char(0']).
%%%%% is_non_name_char(0'().
%%%%% is_non_name_char(0')).
%%%%% 
%%%%% form_functor(Name,Args,Functor) :-
%%%%%     length(Args,NArgs),
%%%%%     functor(Functor,Name,NArgs),
%%%%%     instantiate_args(Args,1,Functor),
%%%%%     !.
%%%%% 
%%%%% instantiate_args([],_,_) :-
%%%%%     !.
%%%%% instantiate_args([Arg|Rest],I,Functor) :-
%%%%%     arg(I,Functor,Arg),
%%%%%     NewI is I + 1,
%%%%%     instantiate_args(Rest,NewI,Functor).
%%%%% 
%%%%% rest_arg_list(As) -->
%%%%% 	  ")", !, {As=[]}
%%%%% 	| ",", !, term(A), rest_arg_list(Bs), {As=[A|Bs]}.
%%%%% 
%%%%% 
%%%%% /* extract_boolean(+QueryResults, -Boolean)
%%%%% 
%%%%% extract_boolean/2 extracts a Boolean from QueryResults which is expected
%%%%% to be of the form
%%%%%      [<func>(<value>)]
%%%%% where <value> is true or false. */
%%%%% 
%%%%% extract_boolean(QueryResults,Boolean) :-
%%%%%     QueryResults=[Term],
%%%%%     functor(Term,_Functor,1),
%%%%%     arg(1,Term,Boolean),
%%%%%     (Boolean==true; Boolean==false),
%%%%%     !.
%%%%% extract_boolean(QueryResults,false) :-
%%%%%     format('~NError: extract_boolean/2 failed for ~p~n',[QueryResults]),
%%%%%     format('Returning false.~n',[]).
%%%%% 
%%%%% 
%%%%% /* extract_variants(+QueryResults, -Variants, -SPVariants)
%%%%%    extract_variants(+VarList, -Variants, -SPVariants)
%%%%% 
%%%%% extract_variants/3 extracts Variants and SPVariants from QueryResults which
%%%%% is expected
%%%%% to be of the form
%%%%%      [Variants(<varlist>)] or
%%%%%      [Variants(<varlist>),SpellingVariants(<varlist>)]
%%%%% where <varlist> is a list of <variant> of the form
%%%%%      [<variant>,<category>,<inflection>]
%%%%% where <variant>, <category> and <inflection> are names. Variants is a list
%%%%% of terms of the form <variant>:[<category>:<inflection>].
%%%%% extract_variants_aux/2 processes each element of <varlist>. */
%%%%% 
%%%%% extract_variants(QueryResults,Variants,[]) :-
%%%%%     QueryResults=['Variants'(VarList)],
%%%%%     extract_variants_aux(VarList,Variants),
%%%%%     !.
%%%%% extract_variants(QueryResults,Variants1,Variants2) :-
%%%%%     QueryResults=['Variants'(VarList1),'SpellingVariants'(VarList2)],
%%%%%     extract_variants_aux(VarList1,Variants1),
%%%%%     extract_variants_aux(VarList2,Variants2),
%%%%%     !.
%%%%% extract_variants(QueryResults,[],[]) :-
%%%%%     format('~NError: extract_variants/2 failed for ~p~n',[QueryResults]),
%%%%%     format('Returning [].~n',[]).
%%%%% 
%%%%% extract_variants_aux([],[]) :-
%%%%%     !.
%%%%% extract_variants_aux([[Variant,Category,Inflection]|Rest],
%%%%% 		     [Variant:[Category:[Inflection]]|ExtractedRest]) :-
%%%%%     extract_variants_aux(Rest,ExtractedRest).
%%%%% 
%%%%% 
%%%%% 
%%%%% 
%%%%% /* ************************************************************************
%%%%%    ************************************************************************
%%%%%    ************************************************************************
%%%%%                             Temporary code for testing
%%%%%    ************************************************************************
%%%%%    ************************************************************************
%%%%%    ************************************************************************ */
%%%%% 
%%%%% 
%%%%% test :-
%%%%%     environ('LEXICON_CONFIG_FILE', ConfigFile),
%%%%%     initialize_lexicon(ConfigFile,_,_),
%%%%%     format('~n2006 lexicon initialized.~n~n',[]),
%%%%%     execution_tests,
%%%%%     parsing_tests,
%%%%%     value_tests,
%%%%%     stop_lex_access,
%%%%%     format('End of tests.~n~n',[]).
%%%%% 
%%%%% execution_tests :-
%%%%%     format('~nExecution tests:~n~n',[]),
%%%%%     execution_test,
%%%%%     fail.
%%%%% execution_tests.
%%%%% 
%%%%% execution_test :-
%%%%%     % sample query
%%%%%     Newline0=[10],
%%%%%     atom_codes(Newline,Newline0),
%%%%%     concatenate_items_to_atom(['<Query QueryType="getAllInflectionalVariants">',
%%%%% 			       Newline,
%%%%% 			       '<QueryTerm>sleep</QueryTerm>',
%%%%% 			       Newline,
%%%%% 			       '<Cats>',
%%%%% 			       Newline,
%%%%% 			       '<Cat type="verb"/>',
%%%%% 			       Newline,
%%%%% 			       '</Cats>',
%%%%% 			       Newline,
%%%%% 			       '</Query>'],
%%%%% 			      Query1),
%%%%%     execute_query(Query1,Result1),
%%%%%     format('~nQuery = ~p~nResult = ~p~n~n',[Query1,Result1]),
%%%%%     % sample query without newlines
%%%%%     concatenate_items_to_atom(['<Query QueryType="getAllInflectionalVariants">',
%%%%% 			       '<QueryTerm>sleep</QueryTerm>',
%%%%% 			       '<Cats>',
%%%%% 			       '<Cat type="verb"/>',
%%%%% 			       '</Cats>',
%%%%% 			       '</Query>'],
%%%%% 			      Query2),
%%%%%     execute_query(Query2,Result2),
%%%%%     format('~nQuery = ~p~nResult = ~p~n~n',[Query2,Result2]),
%%%%%     (Result1==Result2 ->
%%%%% 	format('The last two results are identical.~n~n',[])
%%%%%     ;   format('The last two results DIFFER!~n~n',[])
%%%%%     ).
%%%%% % currently hangs the system
%%%%% %execution_test :-
%%%%% %    % dummy query
%%%%% %    Query0='doodah',
%%%%% %    execute_query(Query0,Result0),
%%%%% %    format('~nQuery = ~p~nResult = ~p~n~n',[Query0,Result0]).
%%%%% 
%%%%% 
%%%%% parsing_tests :-
%%%%%     format('~nParsing tests:~n~n',[]),
%%%%%     parsing_test,
%%%%%     fail.
%%%%% parsing_tests.
%%%%% 
%%%%% %parsing_test :-
%%%%% %    XML="null",
%%%%% %    parse_xml(XML,ParsedXML),
%%%%% %    format('XML = ~p~n~nParsed XML = ~p~n~n',[XML,ParsedXML]).
%%%%% %parsing_test :-
%%%%% %    XML="null<? Declaration ?><Cat type=""noun""/>",
%%%%% %    parse_xml(XML,ParsedXML),
%%%%% %    format('XML = ~p~n~nParsed XML = ~p~n~n',[XML,ParsedXML]).
%%%%% %parsing_test :-
%%%%% %    XML="null<? Declaration ?><A><B>one</B><B>two</B></A>",
%%%%% %    parse_xml(XML,ParsedXML),
%%%%% %    format('XML = ~p~n~nParsed XML = ~p~n~n',[XML,ParsedXML]).
%%%%% %parsing_test :-
%%%%% %    XML="null<? Declaration ?><A><B val1=""one"" val2=""two""></B></A>",
%%%%% %    parse_xml(XML,ParsedXML),
%%%%% %    format('XML = ~p~n~nParsed XML = ~p~n~n',[XML,ParsedXML]).
%%%%% parsing_test :-
%%%%%     concatenate_items_to_atom(['<Query QueryType="getAllInflectionalVariants">',
%%%%% 			       '<QueryTerm>sleep</QueryTerm>',
%%%%% 			       '<Cats>',
%%%%% 			       '<Cat type="verb"/>',
%%%%% 			       '</Cats>',
%%%%% 			       '</Query>'],
%%%%% 			      Query),
%%%%%     execute_query(Query,Results0),
%%%%%     parse_query_results(Results0,Results),
%%%%%     format('Query = ~p~nResults0 = ~p~nParsed Results = ~p~n~n',
%%%%% 	   [Query,Results0,Results]).
%%%%% parsing_test :-
%%%%%     concatenate_items_to_atom(['<Query QueryType="isInLexicon">',
%%%%% 			       '<QueryTerm>acids</QueryTerm>',
%%%%% 			       '</Query>'],
%%%%% 			      Query),
%%%%%     execute_query(Query,Results0),
%%%%%     parse_query_results(Results0,Results),
%%%%%     format('Query = ~p~nResults0 = ~p~nParsed Results = ~p~n~n',
%%%%% 	   [Query,Results0,Results]).
%%%%% parsing_test :-
%%%%%     concatenate_items_to_atom(['<Query QueryType="isBaseInLexicon">',
%%%%% 			       '<QueryTerm>acids</QueryTerm>',
%%%%% 			       '</Query>'],
%%%%% 			      Query),
%%%%%     execute_query(Query,Results0),
%%%%%     parse_query_results(Results0,Results),
%%%%%     format('Query = ~p~nResults0 = ~p~nParsed Results = ~p~n~n',
%%%%% 	   [Query,Results0,Results]).
%%%%% parsing_test :-
%%%%%     concatenate_items_to_atom(['<Query QueryType="isBaseInLexicon">',
%%%%% 			       '<QueryTerm>acid</QueryTerm>',
%%%%% 			       '<Cats>',
%%%%% 			       '<Cat type="verb"/>',
%%%%% 			       '</Cats>',
%%%%% 			       '</Query>'],
%%%%% 			      Query),
%%%%%     execute_query(Query,Results0),
%%%%%     parse_query_results(Results0,Results),
%%%%%     format('Query = ~p~nResults0 = ~p~nParsed Results = ~p~n~n',
%%%%% 	   [Query,Results0,Results]).
%%%%% 
%%%%% 
%%%%% value_tests :-
%%%%%     format('~nValue tests:~n~n',[]),
%%%%%     value_test,
%%%%%     fail.
%%%%% value_tests.
%%%%% 
%%%%% value_test :-
%%%%%     (lex_is_a_form_ci(acids) ->
%%%%% 	OriginalResult=true,
%%%%% 	format('lex_is_a_form_ci result for acids: true~n',[])
%%%%%     ;   OriginalResult=false,
%%%%%         format('lex_is_a_form_ci result for acids: false~n',[])
%%%%%     ),
%%%%%     concatenate_items_to_atom(['<Query QueryType="isInLexicon">',
%%%%% 			       '<QueryTerm>acids</QueryTerm>',
%%%%% 			       '</Query>'],
%%%%% 			      Query),
%%%%%     execute_query(Query,Results0),
%%%%%     parse_query_results(Results0,Results),
%%%%%     extract_boolean(Results,Result),
%%%%%     format('isInLexicon query result for acids: ~p~n',[Result]),
%%%%%     compare_bool_results(OriginalResult,Result).
%%%%% value_test :-
%%%%%     lex_form_ci_vars(sleep,OriginalResult),
%%%%%     format('lex_form_ci_vars result for sleep: ~p~n',[OriginalResult]),
%%%%%     concatenate_items_to_atom(['<Query QueryType="getAllInflectionalVariants">',
%%%%% 			       '<QueryTerm>sleep</QueryTerm>',
%%%%% 			       '</Query>'],
%%%%% 			      Query),
%%%%%     execute_query(Query,Results0),
%%%%%     parse_query_results(Results0,Results),
%%%%%     extract_variants(Results,IResult,SPResult),
%%%%%     format('getAllInflectionalVariants results for sleep: ~p~n~p~n',
%%%%% 	   [IResult,SPResult]),
%%%%%     compare_var_results(OriginalResult,IResult,SPResult).
%%%%% value_test :-
%%%%%     lex_form_ci_vars(sleep,OriginalResult),
%%%%%     format('lex_form_ci_vars result for sleep: ~p~n',[OriginalResult]),
%%%%%     concatenate_items_to_atom(['<Query QueryType="getAllInflectionalVariants">',
%%%%% 			       '<QueryTerm>sleep</QueryTerm>',
%%%%% 			       '<Cats>',
%%%%% 			       '<Cat type="verb"/>',
%%%%% 			       '</Cats>',
%%%%% 			       '</Query>'],
%%%%% 			      Query),
%%%%%     execute_query(Query,Results0),
%%%%%     parse_query_results(Results0,Results),
%%%%%     extract_variants(Results,IResult,SPResult),
%%%%%     format('getAllInflectionalVariants results for sleep (verb): ~p~n~p~n',
%%%%% 	   [IResult,SPResult]),
%%%%%     compare_var_results(OriginalResult,IResult,SPResult).
%%%%% value_test :-
%%%%%     lex_form_ci_vars(apnea,OriginalResult),
%%%%%     format('lex_form_ci_vars result for apnea: ~p~n',[OriginalResult]),
%%%%%     concatenate_items_to_atom(['<Query QueryType="getAllInflectionalVariants">',
%%%%% 			       '<QueryTerm>apnea</QueryTerm>',
%%%%% 			       '</Query>'],
%%%%% 			      Query),
%%%%%     execute_query(Query,Results0),
%%%%%     parse_query_results(Results0,Results),
%%%%%     extract_variants(Results,IResult,SPResult),
%%%%%     format('getAllInflectionalVariants results for apnea: ~p~n~p~n',
%%%%% 	   [IResult,SPResult]),
%%%%%     compare_var_results(OriginalResult,IResult,SPResult).
%%%%% % the following produces identical results to the previous test
%%%%% %value_test :-
%%%%% %    lex_form_ci_vars(apnoea,OriginalResult),
%%%%% %    format('lex_form_ci_vars result for apnoea: ~p~n',[OriginalResult]),
%%%%% %    concatenate_items_to_atom(['<Query QueryType="getAllInflectionalVariants">',
%%%%% %			       '<QueryTerm>apnoea</QueryTerm>',
%%%%% %			       '</Query>'],
%%%%% %			      Query),
%%%%% %    execute_query(Query,Results0),
%%%%% %    parse_query_results(Results0,Results),
%%%%% %    format('Parsed Results = ~p~n',[Results]),
%%%%% %    extract_variants(Results,IResult,SPResult),
%%%%% %    format('getAllInflectionalVariants results for apnoea: ~p~n~p~n',
%%%%% %	   [IResult,SPResult]),
%%%%% %    compare_var_results(OriginalResult,IResult,SPResult).
%%%%% value_test :-
%%%%%     lex_form_ci_vars(gastroesophageal,OriginalResult),
%%%%%     format('lex_form_ci_vars result for gastroesophageal: ~p~n',
%%%%% 	   [OriginalResult]),
%%%%%     concatenate_items_to_atom(['<Query QueryType="getAllInflectionalVariants">',
%%%%% 			       '<QueryTerm>gastroesophageal</QueryTerm>',
%%%%% 			       '</Query>'],
%%%%% 			      Query),
%%%%%     execute_query(Query,Results0),
%%%%%     parse_query_results(Results0,Results),
%%%%%     extract_variants(Results,IResult,SPResult),
%%%%%     format('getAllInflectionalVariants results for gastroesophageal: ~p~n~p~n',
%%%%% 	   [IResult,SPResult]),
%%%%%     compare_var_results(OriginalResult,IResult,SPResult).
%%%%% value_test :-
%%%%%     lex_form_ci_vars(anesthetize,OriginalResult),
%%%%%     format('lex_form_ci_vars result for anesthetize: ~p~n',[OriginalResult]),
%%%%%     concatenate_items_to_atom(['<Query QueryType="getAllInflectionalVariants">',
%%%%% 			       '<QueryTerm>anesthetize</QueryTerm>',
%%%%% 			       '</Query>'],
%%%%% 			      Query),
%%%%%     execute_query(Query,Results0),
%%%%%     parse_query_results(Results0,Results),
%%%%%     extract_variants(Results,IResult,SPResult),
%%%%%     format('getAllInflectionalVariants results for anesthetize: ~p~n~p~n',
%%%%% 	   [IResult,SPResult]),
%%%%%     compare_var_results(OriginalResult,IResult,SPResult).
%%%%% value_test :-
%%%%%     lex_form_ci_vars(best,OriginalResult),
%%%%%     format('lex_form_ci_vars result for best: ~p~n',[OriginalResult]),
%%%%%     concatenate_items_to_atom(['<Query QueryType="getAllInflectionalVariants">',
%%%%% 			       '<QueryTerm>best</QueryTerm>',
%%%%% 			       '</Query>'],
%%%%% 			      Query),
%%%%%     execute_query(Query,Results0),
%%%%%     parse_query_results(Results0,Results),
%%%%%     extract_variants(Results,IResult,SPResult),
%%%%%     format('getAllInflectionalVariants results for best: ~p~n~p~n',
%%%%% 	   [IResult,SPResult]),
%%%%%     compare_var_results(OriginalResult,IResult,SPResult).
%%%%% value_test :-
%%%%%     lex_form_ci_vars(be,OriginalResult),
%%%%%     format('lex_form_ci_vars result for be: ~p~n',[OriginalResult]),
%%%%%     concatenate_items_to_atom(['<Query QueryType="getAllInflectionalVariants">',
%%%%% 			       '<QueryTerm>be</QueryTerm>',
%%%%% 			       '</Query>'],
%%%%% 			      Query),
%%%%%     execute_query(Query,Results0),
%%%%%     parse_query_results(Results0,Results),
%%%%%     extract_variants(Results,IResult,SPResult),
%%%%%     format('getAllInflectionalVariants results for be: ~p~n~p~n',
%%%%% 	   [IResult,SPResult]),
%%%%%     compare_var_results(OriginalResult,IResult,SPResult).
%%%%% value_test :-
%%%%%     lex_form_ci_vars(have,OriginalResult),
%%%%%     format('lex_form_ci_vars result for have: ~p~n',[OriginalResult]),
%%%%%     concatenate_items_to_atom(['<Query QueryType="getAllInflectionalVariants">',
%%%%% 			       '<QueryTerm>have</QueryTerm>',
%%%%% 			       '</Query>'],
%%%%% 			      Query),
%%%%%     execute_query(Query,Results0),
%%%%%     parse_query_results(Results0,Results),
%%%%%     extract_variants(Results,IResult,SPResult),
%%%%%     format('getAllInflectionalVariants results for have: ~p~n~p~n',
%%%%% 	   [IResult,SPResult]),
%%%%%     compare_var_results(OriginalResult,IResult,SPResult).
%%%%% value_test :-
%%%%%     lex_form_ci_vars(that,OriginalResult),
%%%%%     format('lex_form_ci_vars result for that: ~p~n',[OriginalResult]),
%%%%%     concatenate_items_to_atom(['<Query QueryType="getAllInflectionalVariants">',
%%%%% 			       '<QueryTerm>that</QueryTerm>',
%%%%% 			       '</Query>'],
%%%%% 			      Query),
%%%%%     execute_query(Query,Results0),
%%%%%     parse_query_results(Results0,Results),
%%%%%     extract_variants(Results,IResult,SPResult),
%%%%%     format('getAllInflectionalVariants results for that: ~p~n~p~n',
%%%%% 	   [IResult,SPResult]),
%%%%%     compare_var_results(OriginalResult,IResult,SPResult).
%%%%% value_test :-
%%%%%     lex_form_ci_vars(than,OriginalResult),
%%%%%     format('lex_form_ci_vars result for than: ~p~n',[OriginalResult]),
%%%%%     concatenate_items_to_atom(['<Query QueryType="getAllInflectionalVariants">',
%%%%% 			       '<QueryTerm>than</QueryTerm>',
%%%%% 			       '</Query>'],
%%%%% 			      Query),
%%%%%     execute_query(Query,Results0),
%%%%%     parse_query_results(Results0,Results),
%%%%%     extract_variants(Results,IResult,SPResult),
%%%%%     format('getAllInflectionalVariants results for than: ~p~n~p~n',
%%%%% 	   [IResult,SPResult]),
%%%%%     compare_var_results(OriginalResult,IResult,SPResult).
%%%%% value_test :-
%%%%%     lex_form_ci_vars(can,OriginalResult),
%%%%%     format('lex_form_ci_vars result for can: ~p~n',[OriginalResult]),
%%%%%     concatenate_items_to_atom(['<Query QueryType="getAllInflectionalVariants">',
%%%%% 			       '<QueryTerm>can</QueryTerm>',
%%%%% 			       '</Query>'],
%%%%% 			      Query),
%%%%%     execute_query(Query,Results0),
%%%%%     parse_query_results(Results0,Results),
%%%%%     extract_variants(Results,IResult,SPResult),
%%%%%     format('getAllInflectionalVariants results for can: ~p~n~p~n',
%%%%% 	   [IResult,SPResult]),
%%%%%     compare_var_results(OriginalResult,IResult,SPResult).
%%%%% value_test :-
%%%%%     lex_form_ci_vars(she,OriginalResult),
%%%%%     format('lex_form_ci_vars result for she: ~p~n',[OriginalResult]),
%%%%%     concatenate_items_to_atom(['<Query QueryType="getAllInflectionalVariants">',
%%%%% 			       '<QueryTerm>she</QueryTerm>',
%%%%% 			       '</Query>'],
%%%%% 			      Query),
%%%%%     execute_query(Query,Results0),
%%%%%     parse_query_results(Results0,Results),
%%%%%     extract_variants(Results,IResult,SPResult),
%%%%%     format('getAllInflectionalVariants results for she: ~p~n~p~n',
%%%%% 	   [IResult,SPResult]),
%%%%%     compare_var_results(OriginalResult,IResult,SPResult).
%%%%% % testing for category "unknown", but first predicate fails
%%%%% %value_test :-
%%%%% %    lex_form_ci_vars(doodah,OriginalResult),
%%%%% %    format('lex_form_ci_vars result for doodah: ~p~n',[OriginalResult]),
%%%%% %    concatenate_items_to_atom(['<Query QueryType="getAllInflectionalVariants">',
%%%%% %			       '<QueryTerm>doodah</QueryTerm>',
%%%%% %			       '</Query>'],
%%%%% %			      Query),
%%%%% %    execute_query(Query,Results0),
%%%%% %    parse_query_results(Results0,Results),
%%%%% %    extract_variants(Results,IResult,SPResult),
%%%%% %    format('getAllInflectionalVariants results for doodah: ~p~n~p~n',
%%%%% %	   [IResult,SPResult]),
%%%%% %    compare_var_results(OriginalResult,IResult,SPResult).
%%%%% 
%%%%% compare_bool_results(OriginalResult,Result) :-
%%%%%     (OriginalResult==Result ->
%%%%% 	format('Results are the same.~n~n',[])
%%%%%     ;   format('ERROR: Results differ.~n~n',[])
%%%%%     ),
%%%%%     !.
%%%%% 
%%%%% compare_var_results(OriginalResult,IResult,SPResult) :-
%%%%%     (OriginalResult==IResult ->
%%%%% 	format('Results are the same.~n~n',[])
%%%%%     ;   sort(OriginalResult,SOR),
%%%%% 	sort(IResult,SIR),
%%%%% 	(SIR==SOR ->
%%%%% 	    format('Results are equivalent.~n~n',[])
%%%%% 	;   append(IResult,SPResult,CombinedResult),
%%%%% 	    sort(CombinedResult,SCR),
%%%%% 	    (SCR==SOR ->
%%%%% 		format('Combined results are equivalent.~n~n',[])
%%%%% 	    ;   format('ERROR: Results differ.~n~n',[])
%%%%% 	    )
%%%%% 	)
%%%%%     ),
%%%%%     !.
%%%%% 
%%%%% 
