/* qp_recio.pl - Reads in lexical records
*/

:- module(qp_recio, [
	read_lex_record/3,
	write_file_record/2
    ]).

:- use_module(skr_lib(ctypes), [
	is_newline/1,
	is_endfile/1
   ]).

%%% Reads in record at given offset from a lexicon file.
read_lex_record(Lexicon, Ofs, Rec) :-
	open(Lexicon, read, Stream, [reposition(true)]),
	seek(Stream, Ofs, bof, _New),
	read_lex_record_aux(Stream, Rec, []),
	close(Stream).

read_lex_record_aux(S) -->
    { get_code(S,C) },
    (	{ is_endfile(C) } ->
	    { !, fail }
	;   { true }
    ),
    ( { is_newline(C) } ->
	[C],
	{ get_code(S,C1) },
	(   { is_endfile(C) } ->
		{ !, fail }
	    ;	{ true }
	),
	( { is_eor(C1) } ->
	    [C1],
	    { get_code(S,C2) },
	    (	{ is_endfile(C) } ->
		    { !, fail }
		;   { true }
	    ),
	    ( { is_newline(C2) } ->
		[C2], !
	    ; [C2], !, read_lex_record_aux(S)
	    )
	; [C1], !, read_lex_record_aux(S)
	)
    ; [C], !, read_lex_record_aux(S)
    ).

is_eor(0'}).

%%% writes a record in file form to stream
write_file_record([], _Stream).
write_file_record([H|T], Stream) :-
	put_code(Stream, H),
	write_file_record(T, Stream).
