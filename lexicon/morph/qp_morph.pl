
/****************************************************************************
*
*                          PUBLIC DOMAIN NOTICE                         
*         Lister Hill National Center for Biomedical Communications
*                      National Library of Medicine
*                      National Institues of Health
*           United States Department of Health and Human Services
*                                                                         
*  This software is a United States Government Work under the terms of the
*  United States Copyright Act. It was written as part of the authors'
*  official duties as United States Government employees and contractors
*  and thus cannot be copyrighted. This software is freely available
*  to the public for use. The National Library of Medicine and the
*  United States Government have not placed any restriction on its
*  use or reproduction.
*                                                                        
*  Although all reasonable efforts have been taken to ensure the accuracy 
*  and reliability of the software and data, the National Library of Medicine
*  and the United States Government do not and cannot warrant the performance
*  or results that may be obtained by using this software or data.
*  The National Library of Medicine and the U.S. Government disclaim all
*  warranties, expressed or implied, including warranties of performance,
*  merchantability or fitness for any particular purpose.
*                                                                         
*  For full details, please see the MetaMap Terms & Conditions, available at
*  http://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/

/* qp_morph.pl - quintus prolog interface to the inflectional
	and derivational morphology modules.
*/

:- module(qp_morph, [
	dm_variants/3
]).

:- use_module(library(lists), [
	rev/2
   ]).

% foreign_file(morph('morph'), [
foreign_resource(qp_morph, [
	c_dm_variants
   ]).

foreign(c_dm_variants, c, c_dm_variants(+string, +term, -term, [-integer])).

:- load_foreign_resource(qp_morph).

% :- abolish(foreign_resource/2, [force(true)]).

% :- abolish(foreign/3, [force(true)]).

%%% returns inflectional variants in -Var
%%% +Term is the input term
%%% +Cats must be a list of 'adj', 'adv', 'noun' or 'verb'
%%%	and refers to the categories of +Term
%%%	If +Cats is [], all categories apply.
%%% +Infls must be a list of 'base', 'comparative', 'superlative',
%%%	'plural', 'present', 'past', 'ing', 'pastpart'
%%%	and refers to the *desired* inflections.
%%% -Var is a list of Var:[cat:[Cat], infl:[Infl]] terms.
%%%	The list is ordered (longest matching suffix first).


%%% returns derivational variants in -Var
%%% +Term is the input term
%%% +Cats must be a list of 'adj', 'adv', 'noun' or 'verb'
%%%	and refers to the categories of +Term
%%%	If +Cats is [], all categories apply.
%%% -Var is a list of Var:[cat:[Cat]] terms.
%%%	The list is ordered (longest matching suffix first).
dm_variants(Term, Cats, Var) :-
	get_all_cats_if_necessary(Cats, AllCats),
	c_dm_variants(Term, AllCats, Var1, 1),
	% format(user_output, '~q~n', [c_dm_variants(Term, AllCats, Var1, 1)]),
	reformat_dm_list(Var1, Var2),
	rev(Var2, Var).

get_all_cats_if_necessary([], [adj, adv, noun, verb]).
get_all_cats_if_necessary([Cat|RestCats], [Cat|RestCats]).
 
%%% changes Term(Cat) to Term:[cat:[Cat]]
reformat_dm_list([], []).
reformat_dm_list([F|R], [X|Y]) :-
	functor(F, Term, 1),
	arg(1, F, Cat),
	X = Term:[cat:[Cat]],
	reformat_dm_list(R, Y).
