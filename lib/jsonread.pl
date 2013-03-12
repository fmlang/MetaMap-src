:- module(jsonread, [
	json_array/3
% 	json_object/3,
% 	json_members/3,
% 	json_pair/3,
% 	json_elements/3,
% 	json_value/3,
% 	json_string/3,
% 	json_chars/3,
% 	json_number/3,
% 	json_int/3
  ]).


% ------------
% JSON support
% ------------

json_array([]) --> "[", json_wsp, "]".
json_array([H|T]) --> "[", json_wsp, json_elements([H|T]), json_wsp, "]".

json_object([]) --> json_wsp, "{", json_wsp, "}", json_wsp.
json_object([H|T]) --> json_wsp, "{", json_wsp, json_members([H|T]), json_wsp, "}", json_wsp.

json_members([X|Y]) --> json_pair(X), json_wsp, ",", !, json_wsp, json_members(Y).
json_members([X]) --> json_pair(X).

json_pair(pair(X,Y)) --> json_string(X), json_wsp, ":", json_wsp, json_value(Y).

json_elements([X|Y]) --> json_value(X), json_wsp, ",", !, json_wsp, json_elements(Y).
json_elements([X]) --> json_value(X).

json_value(X) --> json_string(X), !.
json_value(X) --> json_number(X), !.
json_value(X) --> json_object(X), !.
json_value(X) --> json_array(X), !.
json_value(true) --> "true", !.
json_value(false) --> "false", !.
json_value(null) --> "null".

json_string(X) --> "\"", json_chars(Y), "\"", { atom_codes(X,Y) }.

json_chars([X|Y]) --> json_char(X), !, json_chars(Y).
json_chars([]) --> [].

json_char(0'") --> "\\\"". %" just to keep Emacs happy
json_char(0'\\) --> "\\\\".
json_char(0'/) --> "\\/".
json_char(0'\b) --> "\\b".
json_char(0'\f) --> "\\f".
json_char(0'\n) --> "\\n".
json_char(0'\r) --> "\\r".
json_char(0'\t) --> "\\t".
json_char(X) --> [X], {X >= 32, X \== 0'"}. %" just to keep Emacs happy

json_number(X) -->
	json_int(A),
	json_frac(B),
	json_exp(C),
	!,
	{ append(A,B,D),
	  append(D,C,E),
	  number_codes(X,E) }.
json_number(X) -->
	json_int(A),
	json_frac(B),
	!,
	{ append(A,B,C),
	  number_codes(X,C)}.
json_number(X) -->
	json_int(A),
	json_exp(B),
	!,
	{ append(A,B,C),
	  number_codes(X,C)} .
json_number(X) -->
	json_int(A),
	{ number_codes(X,A) }.

json_int([0'-,X|Y]) -->
	"-",
	json_digit(X),
	json_digits(Y),
	!.
json_int([0'-,X]) -->
	"-",
	json_digit(X),
	!.
json_int([X|Y]) -->
	json_digit(X),
	json_digits(Y),
	!.
json_int([X]) -->
	json_digit(X),
	!.

json_frac([0'.|X]) -->
	".",
	json_digits(X).

json_exp(X) -->
	json_e(A),
	json_digits(B),
	{ append(A,B,X) }.

json_digits([X|Y]) -->
	json_digit(X),
	!,
	json_digits(Y).
json_digits([]) --> [].

json_e("e+") --> "e+".
json_e("e+") --> "E+".
json_e("e-") --> "e-".
json_e("e-") --> "E-".
json_e("e+") --> "e".
json_e("e+") --> "E".

json_digit(X) -->
	[X],
	{ 0'0 =< X,
	  X =< 0'9 }.

json_wsp--> json_wschar, !, json_wsp.
json_wsp--> "//", !, json_comment, json_wsp.
json_wsp--> [].

json_wschar--> " ".
json_wschar--> "\t".
json_wschar--> "\n".
json_wschar--> "\r".

json_comment--> "\n", !.
json_comment--> "\r", !.
json_comment--> [_], !, json_comment.


% This is tested in the proof engine
% http://eulersharp.sourceforge.net/2006/02swap/euler.yap
% and the result is at
% http://lists.w3.org/Archives/Public/www-archive/2007Jan/0000.html

% fin
