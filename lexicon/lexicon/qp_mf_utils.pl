/* qp_mf_utils.pl - some utility predicates.
*/

:- module(qp_mf_utils, [mf_newline/2, mf_tab/2, mf_atom_to_chars/2]).

%%% generates a tab
mf_tab --> [9].

%%% generates a newline
mf_newline --> [10].


%%% mf_atom_to_chars - makes chars out of atoms, and 
%%%                    deals with null atoms properly

mf_atom_to_chars(MF_atom, mf_chars) :-
     atom_codes(MF_atom,mf_chars) .
