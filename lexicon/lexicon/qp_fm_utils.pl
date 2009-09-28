/* qp_fm_utils.pl - some utility predicates.
*/

:- module(qp_fm_util, [
	fm_to_newline/3,
	fm_to_comma/3,
	fm_to_pipe/3,
	fm_to_char/4,
	fm_to_rparen/3,
	fm_newline/2,
	fm_spaces/2
   ]).

:- use_module(skr_lib(ctypes), [
	is_newline/1
   ]).

%%% fm_to_newline - returns all characters till a newline
fm_to_newline([C|R]) -->
	[C],
	{ \+ is_newline(C) },
	!,
	fm_to_newline(R).
fm_to_newline([]) --> [].

%%% all characters upto ',' or newline
fm_to_comma(S) -->
	    fm_to_char(0',, S).

%%% all characters upto '|' or newline   *** due to change in lexicon syntax in 2000
fm_to_pipe(S) -->
	    fm_to_char(0'|, S).


%%% all characters upto ')' or newline
fm_to_rparen(S) -->
	    fm_to_char(0'), S).

%%% fm_to_char - All characters upto (not including) 'S' or newline
fm_to_char(S, [C|R]) -->
	[C],
	{ \+ is_newline(C), C \== S },
	!,
	fm_to_char(S, R).
fm_to_char(_, []) --> [].

%%% absorbs zero or more consecutive spaces or tabs upto a new line.
fm_spaces --> [32], !, fm_spaces.
fm_spaces --> [9], !, fm_spaces.
fm_spaces --> [], !.

%%% absorbs a newline in the input or generates one on output.
fm_newline --> [10].
