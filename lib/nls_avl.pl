% File:	    nls_avl.pl
% Module:   NLS AVL
% Author:   Lan
% Purpose:  Provide additional functionality to the AVL module


:- module(nls_avl,[
    add_to_avl/4,
    add_to_avl_once/4
    ]).


:- use_module(library(avl),[
    avl_change/5,
    avl_fetch/3,
    avl_store/4
    ]).

/* add_to_avl(+Key, +Value, +AVLIn, -AVLOut)

add_to_avl/4 adds Value to the list of values for Key in AVLIn producing
AVLOut.  */

add_to_avl(Key, Value, AVLIn, AVLOut) :-
	( avl_fetch(Key, AVLIn, Values) ->
	  avl_change(Key, AVLIn, Values, AVLOut, [Value|Values])
	; avl_store(Key, AVLIn, [Value], AVLOut)
	).

/* add_to_avl_once(+Key, +Value, +AVLIn, -AVLOut)

add_to_avl_once/4 is similar to add_to_avl/4 but only allows a single Value
for a given Key. */

add_to_avl_once(Key, Value, AVLIn, AVLOut) :-
	( avl_fetch(Key, AVLIn, _Values) ->
	  AVLOut = AVLIn
	; avl_store(Key, AVLIn, [Value], AVLOut)
	).

