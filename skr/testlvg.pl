
:- module(testlvg, [
	lexAccess_find_prefix/3,
	% lexAccess_find_subterms/3,
	get_all_lexical_records/3,
	lexAccess_get_base_forms_for_form/3,
	lexAccess_get_base_forms_for_form_with_cats/4,
	lexAccess_get_dm_variants_by_category/4,
	lexAccess_get_lex_form_cats/3,
	% lexAccess_get_varlist_for_citation_form/3,
	lexAccess_get_varlist_for_form/4,
	get_varlist_for_all_forms/3
	% lexAccess_is_form/2
	% lexAccess_is_root_form/2
	% lexAccess_is_root_form_with_cats/3
   ]).

% These are the `C' lexicon functions that need to be replaced:
% qp_morph:c_dm_variants/4
  % c_dm_variants(Term, AllCats, Var1, 1)
  % lexAccess_get_dm_variants_by_category(Term, AllCats, Var1)
  % comparison in place
% qp_lexicon:c_lex_cit/7
  % lex_cit_ci_vars, which is directly called from get_variants_for_citation_form
  % lexAccess_get_varlist_for_citation_form(Term, VarList)
  % comparison in place
% qp_lexicon:c_lex_form/7
  % lex_form_ci_vars, which is directly called from get_variants_for_form
  % lexAccess_get_varlist_for_form(Term, VarList)
  % called only for dynamic variant generation
  % comparison in place
% qp_lexicon:c_lex_form_cats/6
  % lexAccess_get_lex_form_cats
  % comparison in place
% qp_lexicon:c_lex_is_a_root/5
  % lexAccess_is_root_form
% qp_lexicon:c_lex_is_a_form/5
  % lexAccess_is_form
% qp_lexicon:c_lex_is_a_root_cats/6
  % lexAccess_is_root_form
% qp_lexicon:c_lex_form_input/5
  % lexAccess_lex_form_input(Word, Records)
% qp_lexicon:c_get_varlist/4

:- use_module(lexicon(lex_access), [
	get_variants_for_citation_form/3,
	tokenize_string_for_lexical_lookup/2
   ]).

% :- use_module(lexicon(qp_fm_lexrec), [
% 	fm_lexical_record/4
%    ]).

:- use_module(skr_db(db_access), [
	db_get_lex_base_forms/2,
	db_get_lex_base_forms_with_cat/3,
	db_get_lex_cats/2,
	db_get_lex_record/2,
	db_get_lex_dm_variants/3,
	db_get_lex_varlist/2
    ]).

%% JSON printing utilities
:- use_module(skr_lib(jsonprint), [
	json_print_element/2
   ]).

%% use JSON DCG to parse JSON responses
:- use_module(skr_lib(jsonread), [
	json_array/3
   ]).

:- use_module(skr_lib(nls_strings), [
    	split_string_completely/3
   ]).

:- use_module(skr_lib(nls_system), [
    	control_value/2				    
   ]).

:- use_module(skr_lib(sicstus_utils),[
	ttyflush/0
   ]).

:- use_module(library(codesio), [
	read_from_codes/2
   ]).

:- use_module(library(lists), [
	append/2,
	keys_and_values/3
   ]).

:- use_module(library(lists3), [
	substitute/4
   ]).


%%
%% Socket read utilities
%%

get_chars_sockserv(Stream, Input) :-
	!,
	read_line(Stream, Input).

% get_chars_sockserv(Stream, Input) :-
%         get_code(Stream, Code),
%         ( Code =:= 10 ->
%           Input = []
%         ; otherwise ->
%           Input = [Code|Codes],
%           get_chars_sockserv(Stream, Codes)
%         ).

read_and_parse_lexicon_server_response(SocketStream, ServerResponse) :-
	( get_chars_sockserv(SocketStream, ServerResponseChars) ->
	  % convert JSON to list
	  json_array(ServerResponse, ServerResponseChars, [])
        ; format(user_output, 'ERROR: Unable to get server result for ~n~w~n', [Tokens]),
	  ttyflush,
	  format(SocketStream, 'ERROR: Unable to get server result for ~n~w~n', [Tokens]),
	  flush_output(SocketStream),
	  halt
        ).
	% format('StreamTerm: ~p~n', [StreamTerm]).


%% JSON Request: { "method" : "get_prefix_list", "tokens" : [ token , ... ] }
%% JSON Response (JSON array of JSON objects):
%%   [ { "prefix" : prefix0, "euilist" : [ eui , ... ], "length" : length }, ... ]
%%
lexAccess_find_prefix(Tokens, LexiconServerStream, SortedPrefixList) :-
	Tokens = [H|T],
	escape_double_quote_chars(T, H, EscapedTokens),
        % send request in JSON format.
	get_lexicon_server_response(object([pair(method,get_prefix_list),
					    pair(tokens,EscapedTokens)]),
				    LexiconServerStream,
				    Response),
	convert_json_prefix_list(Response, PrefixList),
	sort(PrefixList, SortedPrefixList).

% According to Willie, the double-quote char ('"') must be preceeded by
% three backslashes ("\") for Json to work properly.
escape_double_quote_chars([], H, [EscapedH]) :-
	add_backslashes(H, EscapedH).
escape_double_quote_chars([Next|Rest], H, [EscapedH|EscapedRest]) :-
	add_backslashes(H, EscapedH),
	escape_double_quote_chars(Rest, Next, EscapedRest).

add_backslashes(H, EscapedH) :-
	( H == '"' -> %" Fake out Emacs
	  EscapedH = '\\\"'
	; EscapedH = H
	).

convert_json_prefix_list([], []).
convert_json_prefix_list([Object|ObjectList], [HyphenTerm|HyphenTermList]) :-
	convert_json_prefix(Object, HyphenTerm),
	convert_json_prefix_list(ObjectList, HyphenTermList).

%% convert:
%%  [pair(prefix,heart),pair(length,1),pair(euilist,['E0030957'])]
%% to:
%%  heart-['E0030957']-1
%%
%% NOTE: The Prolog structure a-b is just syntactic sugar for -(a,b)
%%  a-b-c = -(-(a,b),c)   
convert_json_prefix(Object, HyphenTerm) :-
	% format(user_output, 'JSON: ~w~n', [Object]),
	memberchk(pair(prefix,Term),     Object),
	memberchk(pair(length,Length),   Object),
	memberchk(pair(euilist,EUIList), Object),
	NegLength is -Length,
	HyphenTerm=NegLength-Term-EUIList.

get_all_lexical_records(EUIList, LexiconServerStream, LexRecs) :-
	(  foreach(EUI, EUIList),
	   foreach(LexRec, LexRecList),
	   param(LexiconServerStream)
	do
	   get_lex_rec_for_EUI(EUI, LexiconServerStream, LexRec)
	),
	% append(LexRecList, MultLexRecs),
	sort(LexRecList, LexRecs).


% Generate a list of all the the parsed, MetaMap-style lexical records for
% a given base form or or an EUI!!

% The first argument of lexAccess_get_lex_recs_for_base_form/3 is
% an EUI when called during lexical lookup after find_prefix.
% This is now the only call to get_lex_rec_for_EUI.

get_lex_rec_for_EUI(EUI, LexiconServerStream, LexRec) :-
	% format(user_output, 'GLRBF: ~q~n', [EUI]), flush_output(user_output),
	% Returns an atom containing all raw lexical records for EUI
	lexAccess_get_lexical_record(EUI, LexiconServerStream, [LexRecAtom]),
	atom_codes(LexRecAtom, LexRecString),
	% The list created by split_string_completely has an empty list as its head
	% because RecordsString begins with "{base=".
	% LexRecStringList is a list of Strings, each of which is a raw lexical record
	% without the "{base=" at the beginning.
	( control_value(lexicon, db) ->
	  read_from_codes(LexRecString, LexRec)
	; LexRec = LexRecString
	),
% 	split_string_completely(LexRecString, "{base=", [[]|LexRecStringList]),
% 	(  foreach(LexRecStringWithoutBase, LexRecStringList),
% 	   foreach(MetaMapLexRec, MetaMapLexRecList)
% 	do
% 	   append("{base=", LexRecStringWithoutBase, LexRecStringWithBase),
% 	   fm_lexical_record(MetaMapLexRec, _EUI, LexRecStringWithBase, [])
% 	),
	!.

lexAccess_get_lexical_record(EUI, LexiconServerStream, Record) :-
	( control_value(lexicon, db) ->
	  db_get_lex_record(EUI, [Record])
	  % send request in JSON format.
	; get_lexicon_server_response(object([pair(method,get_lexical_records),
					      pair(baseform,EUI)]),
				      LexiconServerStream,
				      Record)
	).

% The lexicon server's response VarPairs is of the following form (for input "heart attack"):
% [['post-heart attack',adv], ['post-heart attack',adj], ['post heart attack',adv],
%  ['post heart attack',adj], ['heart attacky',adj],     ['heart attackous',adj] ... ]

% VarList needs the data in the form
% ['post-heart attack':[cat:[adv]], 'post-heart attack':[cat:[adj]],
%  'post heart attack':[cat:[adv]], 'post heart attack':[cat:[adj]],
%  'heart attacky':[cat:[adj]],     'heart attackous':[cat:[adj]] ... ]

% Generate DM Variants
gdmv :-
	LexiconServerStream = '$stream'(-230275952),
	bagof(Base-LexCat, user:base(Base,LexCat), AllPairs),
	print_all_dmv_pairs(AllPairs, LexiconServerStream).

print_all_dmv_pairs([], _LexiconServerStream).
print_all_dmv_pairs([Base-LexCat|Rest], LexiconServerStream) :-
	% format(user_output, '# ~w | ~w~n', [Base,LexCat]),
	print_dmv_pair(Base, LexCat, LexiconServerStream),
	print_all_dmv_pairs(Rest, LexiconServerStream).

print_dmv_pair(Base, LexCat, LexiconServerStream) :-
	lexAccess_get_dm_variants_by_category(Base, [LexCat], LexiconServerStream, VariantList),
	!,
	% get_lexicon_server_response(object([pair(method,get_derivational_variants_by_category),
	% 				    pair(term,Base),
	% 				    pair(categorylist,[LexCat])]),
	% 			    LexiconServerStream,
	% 			    VariantPairs),
	% format(user_output, '~w~n', [VariantPairs]),
	(  foreach(DMVariant:[cat:[DMVariantLexCat]], VariantList),
	    param([Base,LexCat,LexiconServerStream])
	do  format(user_output, '~w|~w|~w|~w~n', [Base,LexCat,DMVariant,DMVariantLexCat])
	),
	!.
print_dmv_pair(_Base, _LexCat, _LexiconServerStream).	

lexAccess_get_dm_variants_by_category(Term, CategoryList, LexiconServerStream, VariantList) :-
	CategoryList = [Category],
	( control_value(lexicon, db) ->
	  db_get_lex_dm_variants(Term, Category, VariantPairs)
	  % send request in JSON format.
	; get_lexicon_server_response(object([pair(method,get_derivational_variants_by_category),
					      pair(term,Term),
					      pair(categorylist,CategoryList)]),
				      LexiconServerStream,
				      VariantPairs)
	),
	(  foreach(VariantPair, VariantPairs),
	   foreach(VariantTerm, VariantList)
	do
	   VariantPair = [LexicalItem,LexicalCategory],
	   VariantTerm = LexicalItem:[cat:[LexicalCategory]]
	).

%% get_varlist_for_form/2
%%
%% This function is the equivalent of lex_access.pl predicate: get_variants_for_form/2.
%%
%% LVG command option for spelling variants:
%%     -f:i 
%% LVG Api Class:
%%     ToInflection 
%%
%% From Francois and Lan's explanation:
%%
%% get_variants_for_form/2: By contrast, get_variants_for_form casts a
%% broader net. This predicate first determines all the citation forms of its
%% argument, and the lexical categories of those citation forms, and then
%% generates (just as does get_variants_for_citation_form) all
%% morphological and (case-insensitive) spelling variants identified in
%% those lexical entries and that have the same lexical category/ies
%% (excluding nominalizations).
%%

get_varlist_for_all_forms([], _LexiconServerStream, []).
get_varlist_for_all_forms([FirstCitationForm|RestCitationForms], LexiconServerStream, VarList) :-
	lexAccess_get_varlist_for_form(FirstCitationForm, LexiconServerStream,
				       VarList, VarListTail),
	get_varlist_for_all_forms(RestCitationForms, LexiconServerStream, VarListTail).

lexAccess_get_varlist_for_form(Term, LexiconServerStream, VarList, VarListTail) :-
	( control_value(lexicon, db) ->
	  db_get_lex_varlist(Term, Response)
	  % send request in JSON format.
	; get_lexicon_server_response(object([pair(method,get_variant_list_for_form),
					      pair(term,Term)]),
				      LexiconServerStream,
				      Response)
	),
        % Response is a list of lists of triples, e.g.,
	% [[['heart attack',noun,base],['heart attacks',noun,plural]]]
	convert_all_to_lexicon_format(Response, VarList, VarListTail).


% takes as input a list of lists of triples, e.g.,
% [[['heart attack',noun,base],['heart attacks',noun,plural]]]
convert_all_to_lexicon_format([], LexiconFormat, LexiconFormat).
convert_all_to_lexicon_format([H|T], LexiconFormatIn, LexiconFormatOut) :-
	convert_one_to_lexicon_format(H, LexiconFormatIn, LexiconFormatNext),
	convert_all_to_lexicon_format(T, LexiconFormatNext, LexiconFormatOut).

% takes as input a list of triples, e.g.,
% [['heart attack',noun,base],['heart attacks',noun,plural]]
convert_one_to_lexicon_format([], LexiconFormat, LexiconFormat).
convert_one_to_lexicon_format([H|T], LexiconFormatIn, LexiconFormatOut) :-
	H = [Token,LexicalCategory,Feature],
	LexiconFormatElement = Token:[LexicalCategory:[Feature]],
	LexiconFormatIn = [LexiconFormatElement|LexiconFormatNext],
	convert_one_to_lexicon_format(T, LexiconFormatNext, LexiconFormatOut).

%% get_varlist_for_citation_form/2
%%
%% This function is the equivalent of lex_access.pl predicate: get_variants_for_citation_form/2.
%% 
%% LVG command option for listing citation forms: -f:Ct
%% LVG command option for spelling variants: -f:s
%% LVG Api Classes: ToCitation, ToSpellingVariants
%%
%% Francois and Lan's explanation of the function's behavior:
%% 
%% get_variants_for_citation_form/2:
%% 
%% If the first argument is not a citation form, simply fail.
%% if the first argument *is* a citation form, then
%% for each lexical entry (case insensitive) having the argument as a citation form,
%% generate all morphological and (case-insensitive) spelling variants
%% identified in those lexical entries and that have the same lexical category/ies
%% (excluding nominalizations).
%% 
%% Spelling variants are tagged as "spvar"; for example, "aging" is a spelling variant of "ageing",
%% so if one calls get_variants_for_citation_form on "ageing", one will get back
%% [aging(noun,spvar),ageing(noun,base),aging(adj,spvar),ageing(adj,base)]).
%% 
%%  Lvg Flow (Java): use ToCitation.Mutate() to determine if term is a base
%%  form, if not exit.  Otherwise, use ToSpellingVariants.Mutate() to
%%  get spelling variants for term.  Throw away any variants that are
%%  not marked as base forms.  
%%
%% get_varlist_for_citation_form('heart attack',[noun,verb]) = ['heart attack'(noun,base), 
%% 'heart attack'(noun,singular), 'heart attacks'(noun,plural)]
%%

%%% lexAccess_get_varlist_for_citation_form(Term, LexiconServerStream, VarList) :-
%%% 	get_lexicon_server_response(object([pair(method,get_variant_list_for_citation_form),
%%% 					    pair(term,Term)]),
%%% 				    LexiconServerStream,
%%% 				    Response),
%%% 	convert_one_to_lexicon_format(Response, VarList, []).

lexAccess_get_lex_form_cats(Word, LexiconServerStream, LexCats) :-
	( control_value(lexicon, db) ->
	  db_get_lex_cats(Word, LexCats)
	  % send request in JSON format.
	; get_lexicon_server_response(object([pair(method,get_lexical_form_categories),
					      pair(term,Word)]),
				      LexiconServerStream,
				      LexCats)
	).

%%% lexAccess_is_form(Word, LexiconServerStream) :-
%%% 	%% send request in JSON format.
%%% 	get_lexicon_server_response(object([pair(method,is_form),
%%% 					    pair(word,Word)]),
%%% 				    LexiconServerStream,
%%% 				    Response),
%%% 	Response=[[pair(is_form,Result)]],
%%%  	!,
%%% 	Result == true.

%%% lexAccess_is_root_form(Word, LexiconServerStream) :-
%%% 	get_lexicon_server_response(object([pair(method,is_root_form),
%%% 					    pair(word,Word)]),
%%% 				    LexiconServerStream,
%%% 				    Response),
%%% 	Response=[[pair(is_root_form,Result)]],
%%% 	!,
%%% 	Result == true.

%%% lexAccess_is_root_form_with_cats(Word, LexiconServerStream, Cats) :-
%%% 	get_lexicon_server_response(object([pair(method,is_root_form_with_categories),
%%% 					    pair(word,Word),
%%% 					    pair(categorylist,Cats)]),
%%% 				    LexiconServerStream,
%%% 				    Response),
%%% 	Response=[[pair(is_root_form_with_categories,Result)]],
%%% 	!,
%%% 	Result == true.

lexAccess_get_base_forms_for_form_with_cats(Word, CategoryList,
					    LexiconServerStream, BaseFormList) :-
	CategoryList = [Category],
	( control_value(lexicon, db) ->
	  db_get_lex_base_forms_with_cat(Word, Category, BaseFormList)
	; get_lexicon_server_response(object([pair(method,base_forms_for_form),
					      pair(word,Word),
					      pair(categorylist,CategoryList)]),
				      LexiconServerStream,
				      BaseFormList)
	).


lexAccess_get_base_forms_for_form(Word, LexiconServerStream, BaseFormList) :-
 	CategoryList = [],
	( control_value(lexicon, db) ->
	  db_get_lex_base_forms(Word, BaseFormList)
 	; get_lexicon_server_response(object([pair(method,base_forms_for_form),
					      pair(word,Word),
					      pair(categorylist,CategoryList)]),
				      LexiconServerStream,
				      BaseFormList)
	).

%%% lexAccess_get_base_forms_for_form(Word, LexiconServerStream, CitationFormList) :-
%%% 	CategoryList = [],
%%% 	get_lexicon_server_response(object([pair(method,citation_forms_for_form),
%%% 					    pair(word,Word),
%%% 					    pair(categorylist,CategoryList)]),
%%% 				    LexiconServerStream,
%%% 				    CitationFormList).
%%% 
%%% lexAccess_get_base_forms_for_form_with_cats(Word, CategoryList,
%%% 						LexiconServerStream, CitationFormList) :-
%%% 	get_lexicon_server_response(object([pair(method,citation_forms_for_form),
%%% 					    pair(word,Word),
%%% 					    pair(categorylist,CategoryList)]),
%%% 				    LexiconServerStream,
%%% 				    CitationFormList).

%%% lexAccess_find_subterms(Tokens, SocketStream, SortedPrefixList) :-
%%% 	Tokens = [H|T],
%%% 	escape_double_quote_chars(T, H, EscapedTokens),
%%% 	% send request in JSON format.
%%% 	get_lexicon_server_response(object([pair(method,get_subterm_list),
%%% 					    pair(tokens,EscapedTokens)]),
%%% 				    SocketStream,
%%% 				    Response),
%%% 	convert_json_prefix_list(Response, PrefixList),
%%% 	(  foreach(Prefix, PrefixList),
%%% 	   foreach(PrefixWithLength, KeysAndNegValues)
%%% 	do Prefix = _NegTokenCount-LexicalItem-_EUIList,
%%% 	   atom_length(LexicalItem, LexicalItemLength),
%%% 	   NegLexicalItemLength is -1 * LexicalItemLength,
%%% 	   PrefixWithLength = NegLexicalItemLength-(Prefix)
%%% 	),
%%% 	keysort(KeysAndNegValues, SortedKeysAndValues),
%%% 	keys_and_values(SortedKeysAndValues, _Keys, SortedPrefixList).

get_lexicon_server_response(Object, LexiconServerStream, Response) :-
	json_print_object(Object, LexiconServerStream),
	read_and_parse_lexicon_server_response(LexiconServerStream, Response).

json_print_object(Object, Stream) :-
	json_print_element(Object, Stream),
	format(Stream, '~n~n', []),
	flush_output(Stream).

