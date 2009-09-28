% File:	    nls_lists.pl
% Module:   NLS Lists
% Author:   Lan
% Purpose:  Provide various list-processing predicates.


:- module(nls_lists,[
	get_from_list/3,
	first_n_or_less/3,
	truncate_list/4
    ]).


/* truncate_list(+List, -TruncatedList, +MaxN, -NTruncated)

truncate_list/4 truncates List to at most MaxN elements producing
TruncatedList. NTruncted is the number truncated. */

truncate_list(List,TruncatedList,MaxN,NTruncated) :-
    length(List,N),
    (MaxN < N ->
        NTruncated is N - MaxN,
	first_n_or_less(List,MaxN,TruncatedList)
    ;   TruncatedList=List,
	NTruncated=0
    ),
    !.


/* first_n_or_less(+List, +N, -PrefixList)

first_n_or_less/3 computes PrefixList consisting of the first N elements
of List.  If List has fewer than N elements, PrefixList is simply List,
itself.  */

first_n_or_less(List,N,PrefixList) :-
    (N=<0 ->
        PrefixList=[]
    ;   first_n_or_less_aux(List,N,PrefixList)
    ).

first_n_or_less_aux([],_,[]) :-
    !.
first_n_or_less_aux(_,0,[]) :-
    !.
first_n_or_less_aux([First|Rest],N,[First|ModifiedRest]) :-
    M is N-1,
    first_n_or_less_aux(Rest,M,ModifiedRest).


% ---------- GET_FROM_LIST ----------
% Get the next item which matches Target or get the argument from a structure
% which has Target as its functor;
% the cuts mean you only get the first match (in a flat list).

% get_from_list_nd is the NonDeterministic version of the original get_from_list;
% The semantics of the original get_from_list have not changed, because this predicate
% simply calls get_from_list_nd, and then immediately cuts.

get_from_list_nd(Target, [Target|_More], Target).
get_from_list_nd(Target, [Target:TargetList|_More], TargetList).

get_from_list_nd(Target, [Structure|_More], TargetArg) :-
        functor(Structure, Target, 1),
        arg(1, Structure, TargetArg).
get_from_list_nd(Target, [_Other|More], TargetOut) :-
        get_from_list_nd(Target, More, TargetOut).
get_from_list_nd(Target, [[EmbeddedList]|_More], TargetOut) :-
        get_from_list_nd(Target, [EmbeddedList], TargetOut).

% Deterministic version of get_from_list; the same as the get_from_list
% before the introduction of get_from_list_nd
get_from_list(Target, List, Structure) :-
	get_from_list_nd(Target, List, Structure),
	!.
