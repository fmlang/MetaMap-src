/* lcheckp.pl - checks validity of lexical records
    and echoes to stdout.
*/

:- use_module(lexicon(qp_fm_lexrec), [
	fm_lexical_record/3
   ]).

:- use_module(lexicon(qp_mf_lexrec), [
	mf_lexical_record/3
   ]).

:- use_module(lexicon(qp_recio), [
	read_lex_record/3,
	write_file_record/2
   ]).

:- use_module(library(ctypes), [
	is_endfile/1,
	is_newline/1
   ]).

runtime_entry(start) :- go.
runtime_entry(abort) :- nl, halt.

:- initialization go.
:- dynamic ofs/2.

go :-
    current_input(Stream),

    repeat,
	character_count(Stream, CharCount),
	line_count(Stream, LineCount),
	retractall(ofs(_, _)),
	assertz(ofs(CharCount, LineCount)),

	(   read_lex_record(Stream, FR) ->
		(   fm_lexical_record(MR, FR, []) ->
			mf_lexical_record(MR, NFR, []),
			write_file_record(NFR, user_output),
			fail
		    ;   report_fm_failure(FR), fail
		)
	    ;	(   is_endfile(_)
		;   report_read_failure
		)
	),
    halt.

%%% reports a failure to convert the record
report_fm_failure(Chars) :-
    look_for_base(Chars, Base),
    !,
    ofs(CharCount, LineCount),
    format(user_error, 'ERROR: Cannot convert record for: ~a [~d|~d]~n', [Base, CharCount, LineCount]).
report_fm_failure(_Chars) :-
    ofs(CharCount, LineCount),
    format(user_error, 'ERROR: Cannot convert record for: <> [~d|~d]~n', [CharCount, LineCount]).

%%% reports a failure to read a record
report_read_failure :-
    ofs(CharCount, LineCount),
    format(user_error, 'ERROR: Cannot read record: [~d|~d]~n', [CharCount, LineCount]).

%%% looks for base
look_for_base(Chars, Base) :-
    append("{base=", List, Chars),
    chars_to_newline(BL, List, _),
    !,
    atom_codes(Base, BL).

%%% chars_to_newline - returns all characters till a newline
chars_to_newline([C|R]) -->
    [C],
    { \+ is_newline(C) },
    chars_to_newline(R).
chars_to_newline([]) --> [].
