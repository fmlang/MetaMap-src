% File:	    nls_signal.pl
% Module:   NLS Signal
% Author:   Lan
% Purpose:  Provide signal handling


:- module(nls_signal,[
    establish_signal_handling/0
    ]).


foreign_resource(nls_signal, ['C_establish_signal_handling']).

foreign('C_establish_signal_handling', c,
        'C_establish_signal_handling'([-integer])).

:- load_foreign_resource(nls_signal).

% :- abolish(foreign_resource/2, [force(true)]).

% :- abolish(foreign/3, [force(true)]).

/* establish_signal_handling

establish_signal_handling/0 calls C_establish_signal_handling which calls
signal() to establish signal handlers.  */

establish_signal_handling :-
    'C_establish_signal_handling'(1).
