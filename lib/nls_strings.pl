
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

% File:     nls_strings.pl
% Module:   NLS Strings
% Author:   Lan
% Purpose:  Provide miscellaneous string manipulation routines.
% Source:   strings_lra.pl


:- module(nls_strings,[
	atom_codes_list/2,
	atom_codes_list_list/2,
	concatenate_items_to_atom/2,
	concatenate_items_to_string/2,
	convert_item_list_to_string/2,
	eliminate_multiple_meaning_designator_string/2,
	% must be exported for mwi_utilities
	eliminate_nos_string/2,
	form_one_string/3,
	is_integer_string/1,
	is_print_string/1,
	normalized_syntactic_uninvert_string/2,
	number_codes_list/2,
	portray_strings_double_quoted/1,
	prep_conj_det/1,
	prep_conj_det_atom/1,
	% must be exported for mm_print and mwi_utilities 
	replace_all_substrings/4,
	replace_nonprints_in_strings/2,
	replace_tabs_in_strings/2,
	split_string/4,
	split_atom_completely/3,
	split_string_completely/3,
	split_string_backtrack/4,
	% must be exported for mm_print and mwi_utilities
	syntactic_uninvert_string/2,
	trim_and_compress_internal_whitespace/2,
	trim_whitespace/2,
	% must be exported for mwi_utilities
	trim_whitespace_left/2,
	trim_whitespace_left/3,
	trim_whitespace_left_1/4,
	% must be exported for SemRep
	trim_whitespace_right/2
    ]).


:- use_module(metamap(metamap_tokenization),[
	tokenize_text_more_lc/2
    ]).

:- use_module(skr_lib(ctypes),[
	is_alpha/1,
	is_alnum/1,
	is_print/1,
	is_digit/1,
	is_white/1,
	to_lower/2
    ]).

:- use_module(skr_lib(nls_text),[
	string_uninvert/2
    ]).

:- use_module(skr_lib(sicstus_utils),[
	concat_strings_with_separator/3,
	midstring/6
    ]).

:- use_module(lexicon(lexical),[
	concatenate_strings/3
    ]).

:- use_module(library(lists),[
	append/2,
	maplist/3,
	rev/2
    ]).


% Use this predicate if you know that none of the strings
% to be converted to atoms is > 65K chars long.

/* atom_codes_list(?Atoms, ?Strings)
atom_codes_list/2 applies atom_codes/2 to lists. */

% The cut in the first clause is necessary because atom_codes_list/2 is bi-directional.
atom_codes_list([], []) :- !.
atom_codes_list([FirstAtom|RestAtoms], [FirstString|RestStrings]) :-
	atom_codes(FirstAtom, FirstString),
	atom_codes_list(RestAtoms, RestStrings).

% convert to numbers if the atoms are a representation of numbers
number_codes_list([], []).
number_codes_list([FirstNum|RestNums], [FirstString|RestStrings]) :-
	( var(FirstNum) ->
	  ( safe_number_codes(FirstNum, FirstString) ->
	    true
	  ; atom_codes(FirstNum, FirstString)
	  )
	; number(FirstNum) ->
	  number_codes(FirstNum, FirstString)
	; atom(FirstNum) ->
	  atom_codes(FirstNum, FirstString)
	),
	number_codes_list(RestNums, RestStrings).


/* atom_codes_list_list(?AtomListList, ?StringListList)
atom_codes_list_list/2 applies atom_codes/2 to lists of lists. */

atom_codes_list_list(AtomListList,StringListList) :-
    atom_codes_list_list_aux(AtomListList,StringListList),
    !.
atom_codes_list_list(AtomListList,StringListList) :-
    (var(AtomListList) ->
	(var(StringListList) ->
	    format('~NERROR: atom_codes_list_list/2 failed (arguments uninstantiated).~n',[])
	;   format('~NERROR: atom_codes_list_list/2 failed for ~p~n.',
		   [StringListList])
	)
    ;   (var(StringListList) ->
	    format('~NERROR: atom_codes_list_list/2 failed for ~p~n.',[AtomListList])
	;   format('~NERROR: atom_codes_list_list/2 failed for ~p and ~p.~n',
		   [AtomListList,StringListList])
	)
    ),
    !,
    fail.

atom_codes_list_list_aux([], []).
atom_codes_list_list_aux([FirstAtomList|RestAtomListList],
			 [FirstStringList|RestStringListList]) :-
	atom_codes_list(FirstAtomList, FirstStringList),
	atom_codes_list_list_aux(RestAtomListList, RestStringListList).


/* concatenate_items_to_atom
Concatenate a list of items forming an atom.  The items must be strings, 
atoms or numbers. */

concatenate_items_to_atom([], '').
concatenate_items_to_atom([H|T], Atom) :-
	concatenate_items_to_string([H|T], String),
	atom_codes(Atom, String).


/* concatenate_items_to_string
Concatenate a list of items forming a string.  The items must be strings, 
atoms or numbers. */

concatenate_items_to_string([], "").
concatenate_items_to_string([Item|Rest], String) :-
	convert_item_to_string(Item, ItemString),
	append(ItemString,RestString, String),
	concatenate_items_to_string(Rest, RestString).

/* convert_item_to_string
Convert an item to a string.  The item must be a string, an atom or a number. */

convert_item_to_string(Item, String) :-
	( Item == [] ->
	  String = []
	; atom(Item) ->
	  atom_codes(Item, String)
	; number(Item) ->
	  number_codes(Item, String)
	; String = Item
	).

/* convert_item_list_to_string
Convert a list of items to a string.  The items must be a string, an atom
or a number. */

convert_item_list_to_string(ItemList, String) :-
	convert_each_item_to_string(ItemList, StringList),
	concatenate_strings(StringList, ",", String0),
	append(["[",String0,"]"], String).

convert_each_item_to_string([], []).
convert_each_item_to_string([Item|Rest], [String|ConvertedRest]) :-
	convert_item_to_string(Item, String),
	convert_each_item_to_string(Rest, ConvertedRest).

/* eliminate_multiple_meaning_designator_string(+String, -ModifiedString)

eliminate_multiple_meaning_designator_string/2 removes an expression of the
form <n> where n is an integer from String producing ModifiedString.  */

eliminate_multiple_meaning_designator_string(String, ModifiedString) :-
	split_string_backtrack(String, "<", Base, A1),
	split_string_backtrack(A1, ">", Integer, Tail),
	is_integer_string(Integer),
	trim_whitespace(Tail, ""),
	!,
	trim_whitespace(Base, ModifiedString).
eliminate_multiple_meaning_designator_string(String, String).

eliminate_nos_string(String, NormString) :-
	eliminate_nos_acros(String, NormString0),
	eliminate_nos_expansion(NormString0, NormString).

eliminate_nos_acros(String,NormString) :-
    split_string_backtrack(String,"NOS",Left,Right),
    \+begins_with_alnum(Right),
    \+ends_with_alpha(Left),
    split_a_string(String,[", NOS",
			 % "; NOS",
			   " - NOS",
			   " NOS",
			   ".NOS",
			   " - (NOS)",
			   " (NOS)",
			   "/NOS",
			   "_NOS",
			   ",NOS",
			   "-NOS",
			   ")NOS"],Left2,Substring,Right2),
    (   Substring==")NOS" ->
	append([Left2,")",Right2],NormString0)
    ;   Substring==".NOS" ->
	append([Left2,".",Right2],NormString0)
    ;   Substring==" NOS" ->
	\+abgn_form(Right2),
        append(Left2,Right2,NormString0)
    ;   Substring=="-NOS" ->
	\+short_last(Left2),
        append(Left2,Right2,NormString0)
    ;   append(Left2,Right2,NormString0)
    ),
    !,
    eliminate_nos_acros(NormString0,NormString).
eliminate_nos_acros(String,String).

eliminate_nos_expansion(String,NormString) :-
    maplist(to_lower,String,LCString),
    atom_codes(Atom,String),
    atom_codes(LCAtom,LCString),
    eliminate_nos_expansion(LCAtom,Atom,NormAtom),
    !,
    atom_codes(NormAtom,NormString).
eliminate_nos_expansion(String,String).

eliminate_nos_expansion(LCAtom,Atom,NormAtom) :-
    nos_expansion(NE),
    midstring(LCAtom,NE,_,LenA,LenB,LenC),
    !,
    midstring(Atom,_,NormAtom,LenA,LenB,LenC).

nos_expansion(', not otherwise specified').
nos_expansion('; not otherwise specified').
nos_expansion(', but not otherwise specified').
nos_expansion(' but not otherwise specified').
nos_expansion(' not otherwise specified').
nos_expansion(', not elsewhere specified').
nos_expansion('; not elsewhere specified').
nos_expansion(' not elsewhere specified').


/* 
   split_a_string(+String, +Substrings, -Left, -Substring, -Right)

split_a_string/5 does the same thing except that it returns the Left
and Right strings in addition to the matching Substring.*/

split_a_string(_String,[],_,_,_) :-
    !,
    fail.
split_a_string(String,[First|_Rest],Left,First,Right) :-
    split_string(String,First,Left,Right),
    !.
split_a_string(String,[_First|Rest],Left,Substring,Right) :-
    split_a_string(String,Rest,Left,Substring,Right).

%%% test_nos :-
%%% 	test_atom(Atom),
%%% 	atom_codes(Atom, String),
%%%         eliminate_nos_string(String, NormString),
%%% 	format('~nString: ~s~n(Norm): ~s~n', [String,NormString]),
%%% 	fail.
%%% test_nos.
%%% 
%%% test_atom('342 ACQUIRED STENOSES').
%%% test_atom('NOS1 gene product').
%%% test_atom('COLON NOS POLYPECTOMY COLONOSCOPY').
%%% test_atom('HoNOS item 1').
%%% test_atom('OPISTHOTONOS').
%%% test_atom('iNOS enzyme').
%%% test_atom('NOS').
%%% test_atom('NOS AB').
%%% test_atom('#Rib/sternum/larynx/trach.NOS').
%%% test_atom('Burning-watercr.NOS-other').
%%% test_atom('ALLERGIC REACTION (NOS)').
%%% test_atom('Lupus erythematosis (NOS) <2>').
%%% test_atom('NEUROPATHY - (NOS)').
%%% test_atom('BACTERIA:ACNC:PT:NOS:ORD:MICROSCOPY.LIGHT').
%%% test_atom('Bone/cartilage disord. OS/NOS').
%%% test_atom('Bran.cleft/preaur.anom.OS/NOS').
%%% test_atom('INFECTIOUS DISEASE OTHER/NOS').
%%% test_atom('BLEED_NOS').
%%% test_atom('BLEED_NOS PROBLEM').
%%% test_atom('Infect.endocarditis+dis EC,NOS').
%%% test_atom('ADVA-NOS-animal rider').
%%% test_atom('ANB-NOS').
%%% test_atom('HRO-NOS protein').
%%% test_atom('MVNTA-NOS').
%%% test_atom('Non-surgical biopsy (admin)NOS').
%%% test_atom('A VARIANT NOS AB').
%%% test_atom('A VARIANT NOS AG').
%%% test_atom('A VARIANT NOS ANTIBODY').
%%% test_atom('L (LITTLE U) NOS AG').
%%% test_atom('i NOS ANTIGEN <1>').
%%% test_atom('ACE inhibitor, NOS').
%%% test_atom('Ignition of clothing, NOS, from controlled fire, NOS, in building NOS').
%%% test_atom('Toxin, NOS (animal)').
%%% test_atom('10 year exam. NOS').
%%% test_atom('2-malig neop LN head/face NOS').
%%% test_atom('967-968 MALIGNANT LYMPHOMA, SPECIFIED TYPE, DIFFUSE OR NOS').
%%% test_atom('ANESTHESIA, PROC ON EYE; NOS').
%%% test_atom('Accidents NOS').
%%% test_atom('Bronchitis NOS, with tracheitis NOS').
%%% test_atom('ADDITION TO SPINAL ORTHOSIS, NOT OTHERWISE SPECIFIED').
%%% test_atom('Acute appendicitis, not otherwise specified').
%%% test_atom('FOR DIABETICS ONLY, NOT OTHERWISE SPECIFIED MODIFICATION (INCLUDING FITTING) OF OFF-THE-SHELF DEPTH-INLAY SHOE OR CUSTOM-MOLDED SHOE, PER SHOE').
%%% test_atom('IMMUNOASSAY, ANALYTE, QUANTITATIVE; NOT OTHERWISE SPECIFIED').
%%% test_atom('Anesthesia for procedures on eye; not otherwise specified').
%%% test_atom('INFECTIOUS AGENT ANTIGEN DETECTION BY IMMUNOFLUORESCENT TECHNIQUE; NOT OTHERWISE SPECIFIED, EACH ORGANISM').
%%% test_atom('Injury stated as accidentally inflicted, but not otherwise specified').
%%% test_atom('Killed, stated as accidentally inflicted but not otherwise specified').
%%% test_atom('ANESTHESIA FOR CLOSED CHEST PROCEDURES; (INCLUDING BRONCHOSCOPY) NOT OTHERWISE SPECIFIED').
%%% test_atom('Closed fracture of distal femur not otherwise specified').
%%% test_atom('1 VIEW:FINDING:POINT IN TIME:HIP.TO BE SPECIFIED ELSEWHERE:NARRATIVE:XR').
%%% test_atom('2-BUTANOL:MASS CONCENTRATION:POINT IN TIME:TO BE SPECIFIED ELSEWHERE:QUANTITATIVE').
%%% test_atom('ANTIBODY; BACTERIUM, NOT ELSEWHERE SPECIFIED').
%%% test_atom('Antibody; bacterium, not elsewhere specified').
%%% test_atom('IMMUNODIFFUSION; NOT ELSEWHERE SPECIFIED').
%%% test_atom('Immunodiffusion; not elsewhere specified').
%%% test_atom('ASSAY OF NEPHELOMETRY, EACH ANALYTE NOT ELSEWHERE SPECIFIED').
%%% test_atom('Chromatography, qualitative; thin layer, analyte not elsewhere specified').
%%% test_atom('Chromatography, quantitative, column (eg, gas liquid or HPLC); single analyte not elsewhere specified, single stationary and mobile phase').

begins_with_alnum([Char|_]) :-
    is_alnum(Char),
    !.

ends_with_alpha(String) :-
    rev(String,[Char|_]),
    is_alpha(Char),
    !.

abgn_form([0' ,0'A,Third|_]) :-  % " ANTIBODY", " ANTIGEN", " AB", " AG"
    memberchk(Third,[0'B,0'G,0'N]),
    !.

short_last(String) :-  % "HRO-NOS protein" but not "ADVA-NOS-animal rider"
    rev(String,RevString),
    (split_string(RevString," ",Last,_) ->
	length(Last,N),
	N<4
    ;   length(String,N),
	N<4
    ),
    !.


/* is_integer_string(+String)

is_integer_string/1 succeeds if String is a string of printable characters.  */

is_integer_string("") :-
	!,
	fail.
is_integer_string(String) :-
	is_integer_string_aux(String).

is_integer_string_aux("").
is_integer_string_aux([First|Rest]) :-
	is_digit(First),
	is_integer_string_aux(Rest).


/* is_print_string(+String)

is_print_string/1 succeeds if String is a string of printable characters.  */

is_print_string([]).
is_print_string([First|Rest]) :-
	nonvar(First),
	is_print(First),
	is_print_string(Rest).

/* uninvert_string(+String, -UninvString)
   normalize_string(+String, -NormString)
   syntactic_uninvert_string(+String, -UninvString)
   normalized_syntactic_uninvert_string(+String, -NormUninvString)

uninvert_string/2 simply calls lexical:string_uninvert/2.
normalized_uninvert_text/2 first normalizes  and then uninverts String.
normalize_string/2 first eliminates multiple meaning designators (<n>)
and then eliminates all forms of NOS.
syntactic_uninvert_string/2 calls uninvert_string/2 on String if it
contains ", " and does not contain a preposition or conjunction.
normalized_syntactic_uninvert_string/2 first normalizes and then
syntactically uninverts String.  */

uninvert_string(String,UninvString) :-
    string_uninvert(String,UninvString).

normalize_string(String,NormString) :-
    eliminate_multiple_meaning_designator_string(String,String1),
    eliminate_nos_string(String1,NormString).

normalized_syntactic_uninvert_string(String,NormSUninvString) :-
    normalize_string(String,NormString),
    syntactic_uninvert_string(NormString,NormSUninvString).

syntactic_uninvert_string(String,SUninvString) :-
    split_string(String,", ",_,_),
    !,
    (contains_prep_or_conj(String) ->
        SUninvString=String
    ;   uninvert_string(String,SUninvString)
    ).
syntactic_uninvert_string(String,String).

contains_prep_or_conj(String) :-
    tokenize_text_more_lc(String,LCTokens),
    contains_prep_or_conj_aux(LCTokens).

contains_prep_or_conj_aux([]) :-
    !,
    fail.
contains_prep_or_conj_aux([FirstString|_]) :-
	atom_codes(FirstAtom, FirstString),
	prep_or_conj(FirstAtom),
	!.
contains_prep_or_conj_aux([_|Rest]) :-
    contains_prep_or_conj_aux(Rest).


/* prep_or_conj(?PrepOrConj)

prep_or_conj/1 is a factual predicate of prepositions/conjunctions.
The data is derived from the 1999 lexicon and consists of any word without
special characters (blank, period, slash) that is either a preposition or
a conjunction. See .../Support/LexLab/ for details.  */

%% some clauses have been commented out to see if better results are obtained

prep_or_conj('aboard').
prep_or_conj('about').
prep_or_conj('across').
prep_or_conj('after').
prep_or_conj('against').
prep_or_conj('aka').
prep_or_conj('albeit').
prep_or_conj('along').
prep_or_conj('alongside').
prep_or_conj('although').
prep_or_conj('amid').
prep_or_conj('amidst').
prep_or_conj('among').
prep_or_conj('amongst').
prep_or_conj('and').
%%prep_or_conj('anti').
prep_or_conj('around').
prep_or_conj('as').
prep_or_conj('astride').
prep_or_conj('at').
prep_or_conj('atop').
%%prep_or_conj('bar').
prep_or_conj('because').
prep_or_conj('before').
prep_or_conj('beneath').
prep_or_conj('beside').
prep_or_conj('besides').
prep_or_conj('between').
prep_or_conj('but').
prep_or_conj('by').
prep_or_conj('circa').
prep_or_conj('contra').
prep_or_conj('despite').
%%prep_or_conj('down').
prep_or_conj('during').
prep_or_conj('ex').
prep_or_conj('except').
prep_or_conj('excluding').
prep_or_conj('failing').
prep_or_conj('following').
prep_or_conj('for').
prep_or_conj('from').
prep_or_conj('given').
prep_or_conj('if').
prep_or_conj('in').
prep_or_conj('inside').
prep_or_conj('into').
prep_or_conj('less').
prep_or_conj('lest').
%%prep_or_conj('like').
%%prep_or_conj('mid').
prep_or_conj('minus').
%%prep_or_conj('near').
prep_or_conj('nearby').
prep_or_conj('neath').
prep_or_conj('nor').
prep_or_conj('notwithstanding').
prep_or_conj('of').
%%prep_or_conj('off').
prep_or_conj('on').
prep_or_conj('once').
%%prep_or_conj('only').
prep_or_conj('onto').
prep_or_conj('or').
%%prep_or_conj('out').
%%prep_or_conj('past').
prep_or_conj('pending').
prep_or_conj('per').
%%prep_or_conj('plus').
prep_or_conj('provided').
prep_or_conj('providing').
prep_or_conj('regarding').
prep_or_conj('respecting').
%%prep_or_conj('round').
prep_or_conj('sans').
prep_or_conj('sensu').
prep_or_conj('since').
prep_or_conj('so').
prep_or_conj('suppose').
prep_or_conj('supposing').
prep_or_conj('than').
prep_or_conj('though').
prep_or_conj('throughout').
prep_or_conj('to').
prep_or_conj('toward').
prep_or_conj('towards').
prep_or_conj('under').
prep_or_conj('underneath').
prep_or_conj('unless').
prep_or_conj('unlike').
prep_or_conj('until').
prep_or_conj('unto').
prep_or_conj('upon').
prep_or_conj('upside').
prep_or_conj('versus').
prep_or_conj('vs').
prep_or_conj('w').
prep_or_conj('wanting').
prep_or_conj('when').
prep_or_conj('whenever').
prep_or_conj('where').
prep_or_conj('whereas').
prep_or_conj('wherein').
prep_or_conj('whereof').
prep_or_conj('whereupon').
prep_or_conj('wherever').
prep_or_conj('whether').
prep_or_conj('while').
prep_or_conj('whilst').
prep_or_conj('with').
prep_or_conj('within').
prep_or_conj('without').
%%prep_or_conj('worth').
prep_or_conj('yet').


/* 
   portray_strings_double_quoted(+String)

For example, portray_strings_double_quoted/1
prints strings double-quoted and fails on non-strings.  Note that the
empty list is not treated as a string.  Thus, it will be printed as
[] rather than "".  portray_strings_double_quoted/1 is suitable to be
used with addportray:add_portray/1 so that strings will be printed
double-quoted even outside the prolog and qui environments
(portray_strings_double_quoted works with qpc, whereas library(print_chars)
does not).  */

% portray_strings_double_quoted([]) :-
%     !,
%     fail.
portray_strings_double_quoted(String) :-
    is_print_string(String),
    put_print_string(String),
    !.

% Note: put_print_string/1 is at the end of this file because its syntax
% confuses emacs.


/* prep_conj_det_atom(?PrepOrConj)
/* prep_conj_det(?PrepOrConj)

prep_conj_det_atom/1 is a factual predicate of prepositions/conjunctions/
determiners. The data is derived from the 2006 lexicon and consists of any word
without special characters (blank, period, hyphen, slash) that is either a
preposition, a conjunction or a determiner. See .../Support/LexLab/ for details.
prep_conj_det/1 is the same predicate for strings. */

prep_conj_det_atom('a').
prep_conj_det_atom('aboard').
prep_conj_det_atom('about').
prep_conj_det_atom('above').
prep_conj_det_atom('across').
prep_conj_det_atom('after').
prep_conj_det_atom('against').
prep_conj_det_atom('aka').
prep_conj_det_atom('albeit').
prep_conj_det_atom('all').
prep_conj_det_atom('along').
prep_conj_det_atom('alongside').
prep_conj_det_atom('although').
prep_conj_det_atom('amid').
prep_conj_det_atom('amidst').
prep_conj_det_atom('among').
prep_conj_det_atom('amongst').
prep_conj_det_atom('an').
prep_conj_det_atom('and').
prep_conj_det_atom('another').
% prep_conj_det_atom('anti').
prep_conj_det_atom('any').
prep_conj_det_atom('around').
prep_conj_det_atom('as').
prep_conj_det_atom('astride').
prep_conj_det_atom('at').
prep_conj_det_atom('atop').
prep_conj_det_atom('bar').
prep_conj_det_atom('because').
prep_conj_det_atom('before').
prep_conj_det_atom('beneath').
prep_conj_det_atom('beside').
prep_conj_det_atom('besides').
prep_conj_det_atom('between').
prep_conj_det_atom('betwixt').
prep_conj_det_atom('beyond').
prep_conj_det_atom('both').
prep_conj_det_atom('but').
prep_conj_det_atom('by').
prep_conj_det_atom('certain').
prep_conj_det_atom('circa').
prep_conj_det_atom('contra').
prep_conj_det_atom('despite').
prep_conj_det_atom('down').
prep_conj_det_atom('during').
prep_conj_det_atom('each').
prep_conj_det_atom('either').
prep_conj_det_atom('enough').
prep_conj_det_atom('every').
prep_conj_det_atom('ex').
prep_conj_det_atom('except').
prep_conj_det_atom('excluding').
prep_conj_det_atom('failing').
prep_conj_det_atom('few').
prep_conj_det_atom('fewer').
prep_conj_det_atom('following').
prep_conj_det_atom('for').
prep_conj_det_atom('from').
prep_conj_det_atom('given').
prep_conj_det_atom('if').
prep_conj_det_atom('in').
prep_conj_det_atom('inbetween').
prep_conj_det_atom('including').
prep_conj_det_atom('inside').
prep_conj_det_atom('into').
prep_conj_det_atom('last').
prep_conj_det_atom('less').
prep_conj_det_atom('lest').
prep_conj_det_atom('like').
prep_conj_det_atom('many').
prep_conj_det_atom('mid').
prep_conj_det_atom('minus').
prep_conj_det_atom('modulo').
prep_conj_det_atom('more').
prep_conj_det_atom('most').
prep_conj_det_atom('much').
prep_conj_det_atom('near').
prep_conj_det_atom('nearby').
prep_conj_det_atom('neath').
prep_conj_det_atom('neither').
prep_conj_det_atom('no').
prep_conj_det_atom('nor').
prep_conj_det_atom('notwithstanding').
prep_conj_det_atom('of').
prep_conj_det_atom('off').
prep_conj_det_atom('on').
prep_conj_det_atom('once').
prep_conj_det_atom('only').
prep_conj_det_atom('onto').
prep_conj_det_atom('or').
prep_conj_det_atom('other').
prep_conj_det_atom('out').
prep_conj_det_atom('outwith').
prep_conj_det_atom('past').
prep_conj_det_atom('pending').
prep_conj_det_atom('per').
prep_conj_det_atom('plus').
prep_conj_det_atom('provided').
prep_conj_det_atom('providing').
prep_conj_det_atom('regarding').
prep_conj_det_atom('respecting').
prep_conj_det_atom('round').
prep_conj_det_atom('sans').
prep_conj_det_atom('sensu').
prep_conj_det_atom('several').
prep_conj_det_atom('since').
prep_conj_det_atom('so').
prep_conj_det_atom('some').
prep_conj_det_atom('such').
prep_conj_det_atom('suppose').
prep_conj_det_atom('supposing').
prep_conj_det_atom('than').
prep_conj_det_atom('that').
prep_conj_det_atom('the').
prep_conj_det_atom('therefore').
prep_conj_det_atom('these').
prep_conj_det_atom('this').
prep_conj_det_atom('those').
prep_conj_det_atom('though').
prep_conj_det_atom('through').
prep_conj_det_atom('throughout').
prep_conj_det_atom('thru').
prep_conj_det_atom('thy').
prep_conj_det_atom('to').
prep_conj_det_atom('toward').
prep_conj_det_atom('towards').
prep_conj_det_atom('under').
prep_conj_det_atom('underneath').
prep_conj_det_atom('unless').
prep_conj_det_atom('unlike').
prep_conj_det_atom('until').
prep_conj_det_atom('unto').
prep_conj_det_atom('upon').
prep_conj_det_atom('upside').
prep_conj_det_atom('upto').
prep_conj_det_atom('versus').
prep_conj_det_atom('vs').
prep_conj_det_atom('w').
prep_conj_det_atom('wanting').
prep_conj_det_atom('what').
prep_conj_det_atom('whatever').
prep_conj_det_atom('when').
prep_conj_det_atom('whenever').
prep_conj_det_atom('where').
prep_conj_det_atom('whereafter').
prep_conj_det_atom('whereas').
prep_conj_det_atom('wherefore').
prep_conj_det_atom('wherein').
prep_conj_det_atom('whereof').
prep_conj_det_atom('whereupon').
prep_conj_det_atom('wherever').
prep_conj_det_atom('whether').
prep_conj_det_atom('which').
prep_conj_det_atom('whichever').
prep_conj_det_atom('while').
prep_conj_det_atom('whilst').
prep_conj_det_atom('with').
prep_conj_det_atom('within').
prep_conj_det_atom('without').
prep_conj_det_atom('worth').
prep_conj_det_atom('yet').

prep_conj_det(String) :-
    name(Atom,String),
    prep_conj_det_atom(Atom).


/* Note: right_parenthetical/1 has been removed to nls_strings_obs.pl. */


/* replace_all_substrings(+String, +OldSubString, +NewSubString, -NewString)
replace_all_substrings/4 replaces all occurrences of OldSubString to
NewSubString in String producing NewString. */

replace_all_substrings(String,OldSubString, NewSubString, NewString) :-
	split_string(String, OldSubString, Left, Right),
	replace_all_substrings(Right, OldSubString, NewSubString, NewRight),
	split_string(NewString, NewSubString, Left, NewRight),
	!.
replace_all_substrings(String, _, _, String).

/* replace_nonprints_in_strings/2(+Strings, -ModifiedStrings)
   replace_nonprints/2(+String, -ModifiedString)

replace_nonprints_in_strings/2 uses replace_nonprints/2 to replace each
nonprint character with a space in each String.  */

replace_nonprints_in_strings([], []).
replace_nonprints_in_strings([First|Rest], [ModifiedFirst|ModifiedRest]) :-
	replace_nonprints(First, ModifiedFirst),
	replace_nonprints_in_strings(Rest, ModifiedRest).

replace_nonprints([], []).
replace_nonprints([Char|Rest], [ModifiedChar|ModifiedRest]) :-
	( Char < 127,
	  is_print(Char) ->
	  ModifiedChar = Char
	; ModifiedChar = 32
	),
	replace_nonprints(Rest, ModifiedRest).

/* replace_tabs_in_strings/2(+Strings, -ModifiedStrings)
   replace_tabs/2(+String, -ModifiedString)

replace_tabs_in_strings/2 uses replace_tabs/2 to replace each tab character
with a space in each String.  */

replace_tabs_in_strings([], []).
replace_tabs_in_strings([First|Rest], [ModifiedFirst|ModifiedRest]) :-
	replace_tabs(First, ModifiedFirst),
	replace_tabs_in_strings(Rest, ModifiedRest).

replace_tabs(String, ModifiedString) :-
	replace_all_substrings(String, [9], " ", ModifiedString).

/* split_string(?String, +Substring, ?Left, ?Right)
split_string/4 embodies the property that String is the concatenation of
Left, Substring and Right in that order.  Substring (and hence String) must be
non-null.  Backtracking is not allowed. */

split_string(String,SubString,Left,Right) :-
    \+SubString=[],
    append(SubString,Right,S1),
    append(Left,S1,String),
    !.

split_atom_completely(Atom, SplitAtom, AtomList) :-
	( Atom == [] ->
	  AtomList = []
	; atom_codes(Atom, String),
	  atom_codes(SplitAtom, SplitString),
	  split_string_completely(String, SplitString, StringList),
	  atom_codes_list(AtomList, StringList)
	).

/* split_string_completely(+String, +Subtring, -StringList)
split_string_completely/ breaks String at each occurrence of Substring
forming StringList.  */

split_string_completely(String,Substring,[Left|SplitRight]) :-
    split_string(String,Substring,Left,Right),
    !,
    split_string_completely(Right,Substring,SplitRight).
split_string_completely(String,_Substring,[String]).

/* split_string_backtrack(?String, +Substring, ?Left, ?Right)
split_string/4 embodies the property that String is the concatenation of
Left, Substring and Right in that order.  Substring (and hence String) must be
non-null.  Backtracking is allowed. */

split_string_backtrack(String, SubString, Left, Right) :-
	\+ SubString = [],
	append(SubString, Right, S1),
	append(Left, S1, String).

form_one_string(Lines, InterLeaveString, InputString) :-
	concat_strings_with_separator(Lines, InterLeaveString, InputString).

/* put_print_string(+String)
   put_print_string_aux(+String)

put_print_string/1 uses put/1 to print String in double-quoted format, i.e.,
surrounded by double quotes and doubling internal double quotes.  */

put_print_string(String) :-
	put_code(0'"), %" this is just to fake out Emacs's colorization!!
	put_print_string_aux(String),
	put_code(0'"). %" this is just to fake out Emacs's colorization!!

put_print_string_aux([]).
put_print_string_aux([0'"|Rest]) :- %" this is just to fake out Emacs's colorization!!
	!,
	put_code(0'"),    %" this is just to fake out Emacs's colorization!!
	put_code(0'"),    %" this is just to fake out Emacs's colorization!!
	put_print_string_aux(Rest).
put_print_string_aux([Char|Rest]) :-
	put_code(Char),
	put_print_string_aux(Rest).

/* trim_whitespace([+WhichEnd,] +String, -TrimmedString)

trim_whitespace_left/right/both trims blanks from one or both ends of String depending on the
value of WhichEnd (left, right, left_and_right, or all).  */

trim_whitespace(String, TrimmedString) :-
	trim_whitespace_both(String, TrimmedString).

trim_whitespace_left(String, TrimmedString, NumBlanksTrimmed) :-
	trim_whitespace_left_1(String, 0, TrimmedString, NumBlanksTrimmed).

trim_whitespace_left(String, TrimmedString) :-
	trim_whitespace_left_1(String, 0, TrimmedString, _NumBlanksTrimmed).

trim_whitespace_left_1([FirstChar|RestString], TempNumBlanksTrimmed,
		       TrimmedString, NumBlanksTrimmed) :-
	is_white(FirstChar),
	NextNumBlanksTrimmed is TempNumBlanksTrimmed + 1,
	trim_whitespace_left_1(RestString, NextNumBlanksTrimmed, TrimmedString, NumBlanksTrimmed),
	!.
trim_whitespace_left_1(String, NumBlanksTrimmed, String, NumBlanksTrimmed) :-
	!.

trim_whitespace_right(String, TrimmedString) :-
	trim_whitespace_right_1(String, TrimmedString, _NumBlanksTrimmed).

trim_whitespace_right_1(String, TrimmedString, NumBlanksTrimmed) :-
	rev(String, [FirstChar|RevString]),
	is_white(FirstChar),
	trim_whitespace_left_1(RevString, 0, TrimmedRevString, NumBlanksTrimmed),
	rev(TrimmedRevString, TrimmedString),
	!.
trim_whitespace_right_1(String, String, 0) :-
	!.

trim_whitespace_both(String, TrimmedString) :-
	trim_whitespace_both(String, TrimmedString, _NumLeftBlanksTrimmed, _NumRightBlanksTrimmed).
	
trim_whitespace_both(String, TrimmedString, NumLeftBlanksTrimmed, NumRightBlanksTrimmed) :-
	trim_whitespace_left_1(String, 0, String0, NumLeftBlanksTrimmed),
	trim_whitespace_right_1(String0, TrimmedString, NumRightBlanksTrimmed),
	!.

trim_and_compress_internal_whitespace([], []).
trim_and_compress_internal_whitespace([H|T], Compressed) :-
	trim_whitespace_both([H|T], Trimmed),
	Trimmed = [TrimmedH|TrimmedT],
	trim_and_compress_internal_whitespace_1(TrimmedT, TrimmedH, Compressed).

trim_and_compress_internal_whitespace_1([], H, [H]).
trim_and_compress_internal_whitespace_1([Next|Rest], First, Trimmed) :-
	( is_white(First),
	  is_white(Next) ->
	  RestTrimmed = Trimmed
	; Trimmed = [First|RestTrimmed]
	),
	trim_and_compress_internal_whitespace_1(Rest, Next, RestTrimmed).

% safe_number_codes(?Number, +Codes)
% The SICStus version of number_codes/2 and number_chars/2 raises an error
% if the first arg is uninstantiated and the second arg is not a valid list of number codes.
safe_number_codes(Number, Codes) :-
	is_number_code_list(Codes),
	number_codes(Number, Codes).

is_number_code_list([]).
is_number_code_list([H|T]) :-
	is_digit(H),
	is_number_code_list(T).
