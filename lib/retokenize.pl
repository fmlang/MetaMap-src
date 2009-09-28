% File:     retokenize.pl
% Module:   retokenize
% Author:   FML, indirectly
% Purpose:  predicates to retokenize before lexical lookup to solve the "in patients" problem

:- module(retokenize,[
	retokenize/2,
	remove_null_atom_defns/2
    ]).

:- use_module(skr_lib(sicstus_utils), [
	lower/2
   ]).


/*
% ---- Retokenize
This is an ad hoc fix ultimately needed because of multi-word lexical entries.
The particular problem this addresses is the fact that "after-treatment" is a lexical entry.
Thus the prepositional phrase "after treatment" is lexicalized as the lexical entry (a noun).
Ultimately, what is needed is a switch that allows only single-word lexical entries
to be retrieved from the lexicon.
Until that is available "after treatment" is retokenized as "after '' treatment".

The list of "in ..." expressions for which this hack is done is below. Gross.
The lexical entry for '' is then removed via remove_null_atom_defn/2.

Yes, this is a hack. Any questions?

*/

retokenize([], []).
retokenize([ThisList|MoreLists], [NewList|Gap]) :-
	retokenize_list(ThisList, NewList),
	retokenize(MoreLists, Gap).

retokenize_list([], []).
retokenize_list([H|T], NewList) :-
	retokenize_list_1(T, H, NewList).

retokenize_list_1([], Last, [Last]).
retokenize_list_1([Next|Rest], First, [First|ReTokenizedRest]) :-
	lower(First, LowerFirst),
	lower(Next, LowerNext),
	retokenize_word(LowerFirst, LowerNext, ReTokenizedRest, ReTokenizedRestTail),
	retokenize_list_1(Rest, Next, ReTokenizedRestTail).

retokenize_word(after, treatment, [''|Tail], Tail) :- !.
retokenize_word(has,   been,      [''|Tail], Tail) :- !.
retokenize_word(in,    InWord,    [''|Tail], Tail) :- in_patients_in_word(InWord), !.
retokenize_word(_First, _Next,     Tail,     Tail).

in_patients_in_word('-').
in_patients_in_word(and).
in_patients_in_word(articulo).
in_patients_in_word(as).
in_patients_in_word(between).
in_patients_in_word(born).
in_patients_in_word(bred).
in_patients_in_word(built).
in_patients_in_word(cellulo).
in_patients_in_word(center).
in_patients_in_word(ceram).
in_patients_in_word(class).
in_patients_in_word(d).
in_patients_in_word(depth).
in_patients_in_word(dies).
in_patients_in_word(door).
in_patients_in_word(dwell).
in_patients_in_word(dwelled).
in_patients_in_word(dwelling).
in_patients_in_word(dwells).
in_patients_in_word(extenso).
in_patients_in_word(extremis).
in_patients_in_word(field).
in_patients_in_word(fields).
in_patients_in_word(fighting).
in_patients_in_word(folding).
in_patients_in_word(foldings).
in_patients_in_word(fundo).
in_patients_in_word(group).
in_patients_in_word(groups).
in_patients_in_word(growing).
in_patients_in_word(hospital).
in_patients_in_word(law).
in_patients_in_word(laws).
in_patients_in_word(lieu).
in_patients_in_word(memoriam).
in_patients_in_word(migrate).
in_patients_in_word(migrated).
in_patients_in_word(migrates).
in_patients_in_word(migrating).
in_patients_in_word(migration).
in_patients_in_word(migrations).
in_patients_in_word(ovo).
in_patients_in_word(part).
in_patients_in_word(patient).
in_patients_in_word(patients).
in_patients_in_word(phase).
in_patients_in_word(phaser).
in_patients_in_word(phasest).
in_patients_in_word(service).
in_patients_in_word(shoe).
in_patients_in_word(situ).
in_patients_in_word(tela).
in_patients_in_word(toed).
in_patients_in_word(toeing).
in_patients_in_word(toto).
in_patients_in_word(training).
in_patients_in_word(turned).
in_patients_in_word(utero).
in_patients_in_word(vacuo).
in_patients_in_word(vitro).
in_patients_in_word(vivo).

remove_null_atom_defns([], []).
remove_null_atom_defns([Tag:Defn|RestDefns], NewDefns) :-
	( null_atom_defn(Tag, Defn) ->
	  RestNewDefns = NewDefns
	; NewDefns = [Tag:Defn|RestNewDefns]
	),
	remove_null_atom_defns(RestDefns, RestNewDefns).

null_atom_defn(shapes,  [inputmatch:[''],features:[integer]]).
null_atom_defn(unknown, [inputmatch:['']]).
