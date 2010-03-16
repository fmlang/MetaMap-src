% File:     loader.pl
% Module:   MetaMap
% Author:   Lan
% Purpose:  Loads SKR for MetaMap
%
% Note that MetaMap is now subsumed by SKR.
% In particular, metamap_fe and metamap have been replaced by skr_fe and skr.

:- use_module(mmserver,[
	main/0
	% stop_mmserver/0
    ]).

:- use_module(skr_lib(nls_signal),[
	establish_signal_handling/0
    ]).

:- use_module(library(date), [
	datime/1
   ]).

:- use_module(library(random), [
	random/1,
	setrand/1,
	random_perm2/4
    ]).

:- dynamic program/2.

:- assert(user:program(mmserver, 'MMServer')).

%%% Code provided by Mats Carlsson of SICS to FML via e-mail 03/27/2007:
%%% 
%%% There are two issues:
%%% 
%%% 1. The initial seed of the random number generator is always the
%%%    same. This is by design, so that you can reproduce the run with the
%%%    same result, which is sometimes desirable. To get different
%%%    sequences of random numbers each time, the seed has to be set
%%%    explicitly.
%%% 
%%% 2. There's a bug in maybe/[0,1] that makes it always fail the first
%%%    time, no matter what the seed is.
%%% 
%%% The piece of code below addresses both issues: it computes a random
%%% seed, and it calls maybe/0 once to compensate for the bug.
%%% --Mats
%%% 

:- initialization
	% compute a random seed based on the clock
	datime(Date),
	Date = date(A,B,C,D,E,F),
	X is A*D,
	Y is B*E,
	Z is C*F,
	setrand(random(X,Y,Z,0)),
	random(R),
	W is integer(R*(1<<30)),
	setrand(random(X,Y,Z,W)),
	% compensate for maybe/0 always failing the first time
	random_perm2(0,0,0,0).


runtime_entry(start) :-
    establish_signal_handling,
    main.
    
runtime_entry(abort) :-
    format(user_output,'~nDisconnecting servers and closing files...',[]),
    ttyflush,
    % shutdown_mmserver, 
    format(user_output,'Done.~n',[]).
