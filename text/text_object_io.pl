% File:     text_object_io.pl
% Module:   Text Object I/O
% Author:   Lan
% Purpose:  To I/O provide support for text objects


:- module(text_object_io,[
    to_io_global/2,
    translate_newlines_to_backslash/2,
    write_warning/4
    ]).

% Now needed for some reason for 0'\ in translate_newlines_to_backslash/2
% :- prolog_flag(character_escapes,_,off).

:- dynamic to_io_global/2.

translate_newlines_to_backslash([],[]).
translate_newlines_to_backslash([10|Rest],[0'\\|TranslatedRest]) :-
    !,
    translate_newlines_to_backslash(Rest,TranslatedRest).
translate_newlines_to_backslash([First|Rest],[First|TranslatedRest]) :-
    translate_newlines_to_backslash(Rest,TranslatedRest).

write_warning(Text0,Type,Ref,_Message) :-
    (to_io_global(warnings_stream,Stream) ->
        true
    ;   Stream=user_output
    ),
    translate_newlines_to_backslash(Text0,Text),
    format(Stream,'~s|~a||~a~n',[Text,Type,Ref]).



