% File:	    nls_io.pl
% Module:   NLS I/O
% Author:   Lan
% Purpose:  Provide miscellaneous I/O routines.


:- module(nls_io,[
    fget_non_ws_only_line/2,
    fget_lines_until_skr_break/2
    ]).


:- use_module(skr_lib(ctypes),[
    is_space/1
    ]).

/* fget_non_ws_only_line(+Stream, -Line)

fget_non_ws_only_line/2 reads lines from Stream until it encounters a non
"blank" Line, i.e., a line with only whitespace characters (if any).
It fails at end-of-file.  */

fget_non_ws_only_line(Stream, Line) :-
	% At end of stream, peek_code returns -1
	peek_code(Stream, Code),
	Code =\= -1,
	!,
	fget_line(Stream, Line0),
	( is_ws_only(Line0) ->
	  fget_non_ws_only_line(Stream, Line)
	; Line = Line0
	).

is_ws_only([]).
is_ws_only([Code|Rest]) :-
	% tab, lf, vt, ff, cr and space
	is_space(Code),
	is_ws_only(Rest).

%%% /* fget_all_non_null_lines(+Stream, -Lines)
%%% 
%%% fget_all_non_null_lines/2 reads lines from Stream through EOF ignoring null
%%% lines. */
%%% 
%%% fget_all_non_null_lines(Stream,Lines) :-
%%%     (at_end_of_stream(Stream) ->
%%% 	Lines=[]
%%%     ;   fget_line(Stream,Line),
%%% 	(Line=="" ->
%%% 	    Lines=RestLines
%%% 	;   Lines=[Line|RestLines]
%%% 	),
%%% 	fget_all_non_null_lines(Stream,RestLines)
%%%     ),
%%%     !.


/* fget_lines_until_skr_break(+Stream, -Lines)

fget_lines_until_skr_break/2 reads Lines from Stream until it encounters
one of the following "natural" breaking points:
  a "blank" line consisting of whitespace characters only (if any) */

fget_lines_until_skr_break(Stream, []) :-
	% At end of stream, peek_code returns -1
	peek_code(Stream, Code),
	Code is -1,
	!.
fget_lines_until_skr_break(Stream, Lines) :-
	fget_line(Stream,Line),
	( is_ws_only(Line) ->
	  % add a blank space so that citation-ending AAs will be detected
	  Lines = [" "]
	; Lines = [Line|Rest],
          fget_lines_until_skr_break(Stream, Rest)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The following predicates are slightly-modified versions of code
% taken from the QP3.5 library file lineio.pl;
% The only change is to get_line_1/2, in which the line
%	(   Char =:= 10 ->
% replaces the line
%	(   Char < " ", Char =\= 9, Char =\= 5 ->
% However, since get_line_1/2 is the lowest-level predicate,
% all the higher level predicates formerly imported from lineio.pl
% are included here as well.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%   fget_line(+Stream, ?Codes)
%   reads a line from the given input Stream, and returns the characters in
%   the list Codes.  It does NOT return the line terminating character, so it
%   is useful for portable programming.  If the terminator was the end of the
%   file, it simply fails and later calls will abort.

fget_line(Stream, Codes) :-
	fget_line(Stream, Line, Terminator),
	Terminator >= 0,		% not end-of-file
	Codes = Line.

%   fget_line(+Stream, ?Codes, ?Terminator)
%   reads a line from the given input Stream, and returns the characters in
%   the list Codes, and the line terminating character in Terminator.  If the
%   terminator was end of file, it just returns it like always.  When you use
%   this routine, the last line will often be ignored if not properly ended.

fget_line(Stream, Codes, Term) :-
	get_line_1(Stream, Line, Terminator),
	Codes = Line,
	Term = Terminator.

get_line_1(Stream, Line, Terminator) :-
	get_code(Stream, Code),
	( terminator_code(Code) ->
	  Line = [],
	  Terminator = 10
	; Line = [Code|Codes],
	  get_line_1(Stream, Codes, Terminator)
	).

% NL
terminator_code(10).
% CR
terminator_code(13).
% eof in case user does not have a <CR> at the end of the file!!
terminator_code(-1).

