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
	( Cats = [],
	  AllCats = [adj, adv, noun, verb]
	; AllCats = Cats
	),
	!,
	c_dm_variants(Term, AllCats, Var1, 1),
	reformat_dm_list(Var1, Var2),
	rev(Var2, Var).

%%% changes Term(Cat) to Term:[cat:[Cat]]
reformat_dm_list([], []).
reformat_dm_list([F|R], [X|Y]) :-
	functor(F, Term, 1),
	arg(1, F, Cat),
	X = Term:[cat:[Cat]],
	reformat_dm_list(R, Y).
