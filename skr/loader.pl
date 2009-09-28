% File:     loader.pl
% Module:   MetaMap
% Author:   Lan
% Purpose:  Loads SKR for MetaMap
%
% Note that MetaMap is now subsumed by SKR.
% In particular, metamap_fe and metamap have been replaced by skr_fe and skr.

:- use_module(skr_fe, [
	go/0,
	go/1,
	go/2
    ]).

:- use_module(skr(skr), [
	stop_skr/0
    ]).

:- use_module(skr_lib(nls_signal), [
	establish_signal_handling/0
    ]).


:- use_module(skr_lib(sicstus_utils), [
	ttyflush/0
    ]).

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

%%% SICStus version updated by Per Mildner.

initialize_random_seed :-
	datime(Date),
	Date = datime(A,B,C,D,E,F),
	X is 1 + ((A*D) mod 30000),
	Y is 1 + ((B*E) mod 30000),
	Z is 1 + ((C*F) mod 30000),
	%% high bits matters so make W big
	random(R),
	W is 1 + integer(R*(1<<30)),
	setrand(random(X,Y,Z,W)).

runtime_entry(start) :-
    establish_signal_handling,
    go.
    
runtime_entry(abort) :-
    format(user_output,'~nDisconnecting servers and closing files...',[]),
    ttyflush,
    stop_skr,
    format(user_output,'Done.~n',[]).
