/* lterm.pl - writes a lexical record as a Prolog term.
*/

:- use_module(lexicon(qp_fm_lexrec), [
	fm_lexical_record/3
   ]).

:- use_module(lexicon(qp_recio), [
	read_lex_record/3
   ]).

:- use_module(library(ctypes), [
	is_endfile/1
   ]).

runtime_entry(start) :- go.
runtime_entry(abort) :- nl, halt.

:- initialization go.

go :-
    current_input(Stream),

    repeat,
	(   read_lex_record(Stream, Record) ->
		(   fm_lexical_record(Memory, Record, []) ->
			writeq(Memory), write('.'), nl,
			fail
		    ;   format(user_error, 'ERROR: Cannot convert record.~n', []),
			fail
		)
	    ;	(   is_endfile(_)
		;   format(user_error, 'ERROR: Cannot read record.~n', [])
		)
	),
    halt.
