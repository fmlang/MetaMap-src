% File:	    loader.pl
% Module:   abs
% Author:   NLS
% Purpose:  Loads abs


:- dynamic program/2.


:- use_module(disamb,[
	go/0
    ]).

:- use_module(skr_lib(sicstus_utils),[
	ttyflush/0
    ]).

:- use_module(library(file_systems),[
	close_all_streams/0
    ]).

:- use_module(skr_lib(nls_signal),[
	establish_signal_handling/0
    ]).

%:- initialization go.

:- assert(program(metamap,'MetaMap')).

runtime_entry(start) :-
    establish_signal_handling,
% Specify MetaMap processing
    retractall(program(_,_)),
    assert(program(metamap,'MetaMap')),
    go.
	
runtime_entry(abort) :-
%    format(user_output,'~nDisconnecting servers and closing files...',[]),
    ttyflush,
%    disconnect_from_all_host_servers,
    close_all_streams,
%    stop_abs,
    format(user_output,'Done~n.',[]).

