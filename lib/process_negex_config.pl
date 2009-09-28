% negex config compilation
% Author: Francois Lang

:- module(process_negex_config,[
	process_negex_config/0
   ]).

:- use_module(skr_lib(nls_io), [
	fget_non_ws_only_line/2
   ]).

:- use_module(skr_lib(nls_strings), [
	atom_codes_list/2,
     	split_string_completely/3,
	trim_and_compress_internal_whitespace/2
   ]).

:- use_module(library(file_systems), [
	file_exists/2
   ]).

:- use_module(skr_lib(sicstus_utils), [
	concat_atoms/2
   ]).

negex_config_file(dynamic_negex_config).

get_negex_terms(InputStream, NegExTermsIn, NegExTermsOut) :-
	( fget_non_ws_only_line(InputStream, NextLine) ->
	  add_to_negex_term_list(NextLine, NegExTermsIn, NegExTermsNext),
	  get_negex_terms(InputStream, NegExTermsNext, NegExTermsOut)
	; NegExTermsOut = NegExTermsIn
	).

add_to_negex_term_list(NextLine, NegExTermsIn, NegExTermsNext) :-
	( comment_line(NextLine) ->
	  NegExTermsNext = NegExTermsIn
	; parse_negex_config_line(NextLine, NegExTerm),
	  NegExTermsNext = [NegExTerm|NegExTermsIn]
	).

parse_negex_config_line(NegExLine, NegExTerm) :-
	trim_and_compress_internal_whitespace(NegExLine, TrimmedNegExLine),
	split_string_completely(TrimmedNegExLine, " ", StringList),
	atom_codes_list(AtomList0, StringList),
	AtomList0 = [NegExType,FirstNegExToken|RestNegExTokens],
	concat_atoms([NegExType, '_phrase_tokens'], NegExFunctor),
	NegExTerm =.. [NegExFunctor,FirstNegExToken,RestNegExTokens].

% begins with '%'
comment_line([37|_]).

assert_negex_terms([]).
assert_negex_terms([NegExTerm|RestNegExTerms]) :-
	announce_one_negex_term(NegExTerm),
	assert(compiled_negex_config:NegExTerm),
	assert_negex_terms(RestNegExTerms).

announce_one_negex_term(NegExTerm) :-
	% do not announce the default NegEx terms!
	( negex_default_term(NegExTerm) ->
	  true
	; NegExTerm =.. [NegExFunctor,FirstNegExToken,RestNegExTokens],
	  atom_codes(NegExFunctor, NegExFunctorString),
	  append(NegExTypeString, "_phrase_tokens", NegExFunctorString),
	  atom_codes(NegExType, NegExTypeString),
	  format(user_output, '~w: ~w', [NegExType,FirstNegExToken]),
	  write_rest_negex_tokens(RestNegExTokens)
	).

write_rest_negex_tokens([]) :- format(user_output, '~n', []).
write_rest_negex_tokens([H|T]) :-
	write_rest_negex_tokens_aux(T, H).

write_rest_negex_tokens_aux([], H) :-
	format(user_output, ' ~w~n', [H]).
write_rest_negex_tokens_aux([Next|T], H) :-
	format(user_output, ' ~w', [H]),
	write_rest_negex_tokens_aux(T, Next).

negex_predicate(conj_phrase_tokens,      2).
negex_predicate(nega_phrase_tokens,      2).
negex_predicate(negb_phrase_tokens,      2).
negex_predicate(pnega_phrase_tokens,     2).
negex_predicate(pnegb_phrase_tokens,     2).
negex_predicate(pseudoneg_phrase_tokens, 2).

abolish_negex_predicates :-
	negex_predicate(PredicateName, Arity),
	abolish(compiled_negex_config:PredicateName/Arity, [force(true)]),
	fail.
abolish_negex_predicates.

% From each
% negex_predicate(conj_phrase_tokens,      2).
% create a term of the form
% conj_phrase_tokens([], []).
negex_default_term(DefaultNegExTerm) :-
	negex_predicate(Functor, Arity),
	functor(DefaultNegExTerm, Functor, Arity),
	arg(1, DefaultNegExTerm, []),
	arg(2, DefaultNegExTerm, []).

negex_default_terms(NegExDefaultTerms) :-
	findall(Term, negex_default_term(Term), NegExDefaultTerms).

process_negex_config :-
	( negex_config_file(NegExConfigFile),
  	  file_exists(NegExConfigFile, read) ->
	  open(NegExConfigFile, read, InputStream),
	  negex_default_terms(NegExDefaultTerms),
	  get_negex_terms(InputStream, NegExDefaultTerms, NegExTerms),
	  close(InputStream),
	  sort(NegExTerms, SortedNegExTerms),
	  abolish_negex_predicates,
	  assert_negex_terms(SortedNegExTerms)
	; true
	).
