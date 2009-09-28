% File:	    efficiency.pl
% Module:   Efficiency
% Author:   Lan
% Purpose:  Provide predicates for enhancing efficiency


:- module(efficiency,[
    maybe_atom_gc/2
   ]).


:- dynamic atom_gc_threshold/1.
:- dynamic saved_timeval/1.


/* maybe_atom_gc(-DidGC, -SpaceCollected)
   maybe_atom_gc(+Notify, -DidGC, -SpaceCollected)

   maybe_atom_gc/3 performs garbage_collect_atoms/0 if the amount of atom
   space in use exceeds a threshold gotten from atom_gc_threshold/1.
   It returns whether garbage collection was performed and the amount of
   space collected, if any.  Also, if Notify has value 'yes', details
   of the collection are displayed.
   maybe_atom_gc/2 calls maybe_atom_gc/3 with Notify set to 'no'.  */

maybe_atom_gc(DidGC,SpaceCollected) :-
    maybe_atom_gc(no,DidGC,SpaceCollected).

maybe_atom_gc(Notify,DidGC,SpaceCollected) :-
    atom_gc_threshold(Threshold),
    statistics(atoms,[NAtoms0,SpaceInUse0,SpaceFree0|_]),
    (SpaceInUse0 > Threshold ->
        (Notify==yes ->
            format_and_notify('~Nmaybe_atom_gc threshold = ~d~n',[Threshold]),
            TotalSpace0 is SpaceInUse0 + SpaceFree0,
            format_and_notify('~Nmaybe_atom_gc before = ~d  ~d + ~d = ~d~n',
                     [NAtoms0,SpaceInUse0,SpaceFree0,TotalSpace0])
        ;   true
        ),
        garbage_collect_atoms,
        DidGC=yes,
        statistics(atoms,[NAtoms,SpaceInUse,SpaceFree|_]),
        SpaceCollected is SpaceInUse0-SpaceInUse,
        (Notify==yes ->
            TotalSpace is SpaceInUse + SpaceFree,
            format_and_notify('~Nmaybe_atom_gc  after = ~d  ~d + ~d = ~d~n',
                     [NAtoms,SpaceInUse,SpaceFree,TotalSpace])
% temp to test need for trimcore/0
% it actually made things worse, i.e., process size grew faster, not slower
%statistics(stacks,Stacks0),
%statistics(global_stack,GStack0),
%statistics(local_stack,LStack0),
%format('Stack info before: ~p ~p ~p~n',[Stacks0,GStack0,LStack0]),
%trimcore,
%statistics(stacks,Stacks),
%statistics(global_stack,GStack),
%statistics(local_stack,LStack),
%format('Stack info after: ~p ~p ~p~n',[Stacks,GStack,LStack])
        ;   true
        ),
        NewThreshold is SpaceInUse + (SpaceFree // 2),
        (atom_gc_threshold(NewThreshold) ->
            true
        ;   retractall(atom_gc_threshold(_)),
            assert(atom_gc_threshold(NewThreshold)),
            (Notify==yes ->
                format_and_notify('~Nmaybe_atom_gc new threshold = ~d~n',
                                  [NewThreshold])
            ; true
            )
        )
    ;   DidGC=no,
        SpaceCollected = 0
    ),
    !.
maybe_atom_gc(_Notify,DidGC,SpaceCollected) :-
    format('ERROR: maybe_atom_gc/3 did not finish normally.~n',[]),
    DidGC=unknown,
    SpaceCollected = -1.


/* atom_gc_threshold(?Threshold)

atom_gc_threshold/1 is a factual predicate with one clause indicating the
threshold to be used for atom garbage collection.  It is initially 0 but
is recomputed after each garbage collection.  It is set to
                     <base> + 1/2*<free>
where <base> is the amount of atom space in use and <free> the amount free
after the garbage collection.  */

atom_gc_threshold(0).


/* format_and_notify(+Control, +Args)

format_and_notify/2 does what format does to both current_output and user_output
(if different).  */

format_and_notify(Control, Args) :-
	format(Control, Args),
	( current_output(user_output) ->
	  true
	; format(user_output,Control,Args)
	).
