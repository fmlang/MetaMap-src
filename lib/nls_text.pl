% File:	    nls_text.pl
% Module:   NLS Text
% Author:   Lan
% Purpose:  Provide simple text (atom) processing

% WARNING: This module is obsolete since midstring/? corrupts memory
%          for atoms > ~512 in size!!!


:- module(nls_text,[
	concatenate_text/3,
	eliminate_multiple_meaning_designator/2,
	is_all_graphic_text/1,
	string_uninvert/2
   ]).


:- use_module(skr_lib(nls_strings), [
	atom_codes_list/2,
	is_integer_string/1,
	split_string_backtrack/4,
	split_string_completely/3,
	trim_whitespace/2
    ]).


:- use_module(skr_lib(sicstus_utils), [
	concat_strings_with_separator/3
   ]).


:- use_module(library(lists), [
	reverse/2
   ]).

:- use_module(library(lists),[
    append/2
    ]).

/* append_text(+TextList, ?Text)

append_text/2 is analogous to append/2 for text (atoms) except that at least
one of its arguments MUST be instantiated.*/

append_text([], '').
append_text([Text], Text) :- !.
append_text(TextList, Text) :-
	atom_codes_list(TextList, StringList),
	append(StringList, String),
	atom_codes(Text, String).

/* concatenate_text(+TextList, +InsertText, -Text)

concatenate_text/3 forms Text, the concatenation of text in TextList
interposed with InsertText (often ' ').  */

concatenate_text([], _, '') :- !.
concatenate_text([Text], _, Text) :- !.
concatenate_text([First|Rest], InsertText, Text) :-
	concatenate_text(Rest, InsertText, First, Text).

concatenate_text([], _, TextIn, TextIn).
concatenate_text([First|Rest], InsertText, TextIn, TextOut) :-
	append_text([TextIn,InsertText,First], TextInOut),
	concatenate_text(Rest, InsertText, TextInOut, TextOut).

/* eliminate_multiple_meaning_designator(+Word, -ModifiedWord)

eliminate_multiple_meaning_designator/2 removes an expression of the form
<n> where n is an integer from Word (an atom) producing ModifiedWord.  */

eliminate_multiple_meaning_designator(Word, ModifiedWord) :-
	( atom_codes(Word, WordString),
	  split_string_backtrack(WordString, "<", Base, A1),
	  split_string_backtrack(A1, ">", Integer, Tail),
	  is_integer_string(Integer),
	  trim_whitespace(Tail, "") ->
	  trim_whitespace(Base, ModifiedWordString),
	  atom_codes(ModifiedWord, ModifiedWordString)
	; ModifiedWord = Word
	).

/* is_all_graphic_text(+Text)
   is_graphic_text(+Text)

is_all_graphic_text/1 succeeds if Text is an atom (INCLUDING '') consisting
entirely of graphic characters. */

is_all_graphic_text('') :-
    !.
is_all_graphic_text(Text) :-
%    midstring(Text,First,Rest,0,1),     midstring/? corrupts memory!
%    is_graphic_text(First),
%    is_all_graphic_text(Rest).
    atom_codes(Text,S),
    is_all_graphic(S).

is_all_graphic([]).
is_all_graphic([Char|Rest]) :-
    is_graphic(Char),
    is_all_graphic(Rest).

is_graphic(0'!).  % see ctypes:is_graph/1 for non-alphanumeric graphics
is_graphic(0'").  % " This comment is just to un-confuse Emacs
is_graphic(0'#).
is_graphic(0'$).
is_graphic(0'%).
is_graphic(0'&).
is_graphic(0'\').
is_graphic(0'().
is_graphic(0')).
is_graphic(0'*).
is_graphic(0';).
is_graphic(0'<).
is_graphic(0'=).
is_graphic(0'>).
is_graphic(0'?).
is_graphic(0'@).
is_graphic(0'[).
is_graphic(0'\\).
is_graphic(0']).
is_graphic(0'^).
is_graphic(0'_).
is_graphic(0'`).
is_graphic(0'{).
is_graphic(0'|).
is_graphic(0'}).
is_graphic(0'~).
is_graphic(0'+).
is_graphic(0',).
is_graphic(0'-).
is_graphic(0'.).
is_graphic(0'/).
is_graphic(0':).

% Recursively uninvert a string, i.e., injury, abdominal ==> abdominal injury.
% More generally, "S1, S2, ..., Sn" ==> Sn, ..., S2, S1".
% This is now implemented in pure prolog, thereby allowing us to retire lexical.c!

string_uninvert(String, UninvertedString) :-
	split_string_completely(String, ", ", SplitString),
	reverse(SplitString, ReverseSplitString),
	concat_strings_with_separator(ReverseSplitString, " ", UninvertedAtom),
	atom_codes(UninvertedAtom, UninvertedString).
