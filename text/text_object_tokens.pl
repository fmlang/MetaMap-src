% File:     text_object_tokens.pl
% Module:   Text Object Tokens
% Author:   Lan
% Purpose:  To provide operations on text object tokens


:- module(text_object_tokens,[
	compute_token_position/2,
	extract_text/2,
	filter_out_field_comments/2,
	filter_out_ws_tokens/2,
	form_field_tokens/2,
	get_token_position/2,
	position_contains/2,
	position_ge/2,
	remove_bracketing/2,
	split_scope/4,
	trim_ws_tokens/2
    ]).


:- use_module(text(text_object_io), [
	write_warning/4
    ]).

:- use_module(text(text_object_util), [
	higher_order_type/1,
	lbracket/1,
	rbracket/1,
	ws_char/1,
	ws_tok/1
    ]).

:- use_module(skr_lib(nls_system), [
	control_option/1
    ]).

:- use_module(lexicon(lexical),[
	lowercase_list/2
    ]).

:- use_module(skr_lib(ctypes),[
	is_alnum/1,
	is_alpha/1,
	is_lower/1,
	is_upper/1,
	is_digit/1,
	is_punct/1
    ]).

:- use_module(skr_lib(nls_strings),[
	split_string/4
    ]).

:- use_module(skr_lib(sicstus_utils),[
	lower/2
    ]).

:- use_module(library(lists),[
	append/2,
	last/2,
	rev/2
    ]).


/* form_field_tokens(+TokenizedFields, -FieldTokens)
   form_simple_tokens(+TokenizedText, -Tokens)
   form_simple_tokens(+TokenizedText, +LCTokenizedText, +Start, -Tokens)

form_field_tokens/2 uses form_simple_tokens/2 to compute the tokens for each
field in TokenizedFields.

form_simple_tokens/2 transforms TokenizedText into simple Tokens of the form
     tok(<type>,<text>,<lctext>,<position>)
where <type> is one of
     lc - lowercase alpha
     uc - uppercase alpha
     ic - initial cap (rest lowercase)
     mc - mixed case
     an - alphanumeric
     nu - numeric
     pn - punctuation
     ws - whitespace (space, tab or newline)
     xx - unknown
and <position> is a term pos(<start>,<end>) ala TIPSTER. For example, the text
"Heart disease" would be tokenized to
     tok(ic,"Heart","heart",pos(0,5))
     tok(ws," "," ",pos(5,6))
     tok(lc,"disease","disease",pos(6,13))
Note that the length of a token is always <end>-<start> since <start> refers to
the (zero-based) position of the first character of the token and <end> refers
to the position just to the right of the last character.

Higher-order tokens (created later) are placed at the beginning of a sequence
of simple or other higher-order tokens. They are of the general form
     tok(<type>,<*>,<*>,<position>[,<original-position>])
where <type> is one of
     field - field
     label - label
     sn - sentence
     pe - parenthetical expression
     aadef - acronym/abbreviation definition
     aa - acronym/abbreviation occurrence
and <original-position> is a pos/2 term occurring in expanded tokens and
points to the position of the corresponding text in the original, unexpanded
tokenization. In this case, <position> refers to the position in the expanded
form (i.e., where acronyms/abbreviations are expanded).

field and label tokens have <text> and <lctext> arguments:
     tok(field,"TI","ti",pos(0,29))
     tok(label,"01.TI.1","01.ti.1",pos(0,24),pos(0,29))
where labels are of the form <UI>.<field>.<sentence number> where the default
field is "TX".

sn and pe tokens have [] and <order> arguments where <order> is the number of
higher-order tokens containing this token in its scope (topmost is of order 0):
     tok(sn,[],0,pos(0,29))
     tok(pe,[],1,pos(16,20))

aadef tokens have the form
     tok(aadef,<definition>,<AA>,<position>)
and aa tokens have the form
     tok(aa,<AA>,[<aadef token>],<position>)
where <definition> is the list of tokens defining the acronym/abbreviation and
<AA> is the acronym/abbreviation being associated with the definition. It
follows the definition in a parenthetical expression. So a complete definition
of an acronym/abbreviation with its definition is
     tok(aadef,[tok(ic,"Coronary","coronary",pos(0,8)),tok(ws," "," ",pos(8,9)),
                tok(lc,"artery","artery",pos(9,15))],
               [tok(uc,"CA","ca",pos(17,19))],
               pos(0,15)),
     tok(ic,"Coronary","coronary",pos(0,8)),
     tok(ws," "," ",pos(8,9)),
     tok(lc,"artery","artery",pos(9,15)),
     tok(ws," "," ",pos(15,16)),
     tok(pe,[],1,pos(16,20)),
     tok(pn,"(","(",pos(16,17)),
     tok(aa,[tok(uc,"CA","ca",pos(17,19))],
            [tok(aadef,[tok(ic,"Coronary","coronary",pos(0,8)),
                        tok(ws," "," ",pos(8,9)),
                        tok(lc,"artery","artery",pos(9,15))],
                       [tok(uc,"CA","ca",pos(17,19))],pos(0,15))],
            pos(17,19)),
     tok(uc,"CA","ca",pos(17,19)),
     tok(pn,")",")",pos(19,20))
Subsequent aa tokens will have the same [<aadef token>] argument as the example
above but with different positions for the <AA> and <position> arguments.
*/

form_field_tokens([],[]) :-
    !.
form_field_tokens([[Field,TokenizedField]|Rest],
		  [[Field,Tokens]|TokenizedRest]) :-
    form_simple_tokens(TokenizedField,Tokens),
    form_field_tokens(Rest,TokenizedRest).

form_simple_tokens(TokenizedText,Tokens) :-
    lowercase_list(TokenizedText,LCTokenizedText),
    form_simple_tokens(TokenizedText,LCTokenizedText,0,Tokens).

form_simple_tokens([], [], _, []).
form_simple_tokens([Text|RestText], [LCText|RestLCText], Start,
            [tok(Type,Text,LCText,pos(Start,End))|RestTokens]) :-
	set_token_type(Text, Type),
	length(Text,L),
	End is Start + L,
	form_simple_tokens(RestText, RestLCText, End, RestTokens).

% ws is space, tab or newline
set_token_type(Token, ws) :-
	Token = [Char],
	ws_char(Char).

% token_type(" ",ws) :-
%     !.
% token_type("	",ws) :-
%     !.
% token_type("
% ",ws) :-
%     !.

set_token_type([Char], pn) :-
	is_punct(Char),
	!.
set_token_type(Token, Type) :-
	Token \== "",
	all_alnum(Token),
	!,
	set_alnum_token_type(Token, Type).

set_token_type(Token, xx) :-
	( control_option(warnings) ->
	  write_warning(Token,wxx,'<unavailable>','Unknown token')
	; true
	).

set_alnum_token_type(Token, Type) :-
	( all_alpha(Token) ->
	  set_alpha_token_type(Token, Type)
	; all_digit(Token) ->
	  Type = nu
	; Type = an
	).


set_alpha_token_type(Token, Type) :-
	( all_lower(Token) ->
          Type = lc
	; all_upper(Token) ->
	  Type = uc
	; ( Token = [First|Rest],
	    is_upper(First),
	    all_lower(Rest)) ->
	    Type = ic
	; Type = mc
	).

all_alnum([]).
all_alnum([Char|Rest]) :-
	is_alnum(Char),
	all_alnum(Rest).

all_alpha([]).
all_alpha([Char|Rest]) :-
	is_alpha(Char),
	all_alpha(Rest).

all_lower([]).
all_lower([Char|Rest]) :-
	is_lower(Char),
	all_lower(Rest).

all_upper([]).
all_upper([Char|Rest]) :-
	is_upper(Char),
	all_upper(Rest).

all_digit([]).
all_digit([Char|Rest]) :-
	is_digit(Char),
	all_digit(Rest).


/* compute_token_position(+Tokens, -Position)
   get_token_position(+Token, -Position)

compute_token_position/2 computes the Position of Tokens by using the
positions of its first and last tokens.
get_token_position/2 returns the (previously computed) Position of Token.  */

compute_token_position([tok(_,_,_,Pos)], Pos) :-
	% singleton
	!.
compute_token_position([First|Rest], pos(Begin,End)) :-
	!,
	get_token_position(First, pos(Begin,_)),
	% reversed order of args from QP library version!
	last(Rest, Last),
	get_token_position(Last, pos(_,End)).
compute_token_position(Tokens, pos(0,0)) :-
	( control_option(warnings) ->
	  extract_text(Tokens, TText),
	  write_warning(TText, wpos, '','Cannot compute token position')
	; true
	).
get_token_position(tok(_,_,_,Pos), Pos) :- !.


/* split_scope(+Tokens, +Position, -Scope, -Rest)
   split_scope(+Tokens, +End, +AllTokens, -Scope, -Rest)

split_scope/4 splits the list of Tokens into Scope and Rest where
Scope consists of those tokens ending at the end of Position and Rest
is the remaining tokens.  Higher-order tokens cannot end Scope.  */

split_scope(Tokens,pos(_,End),Scope,Rest) :-
    split_scope(Tokens,End,Tokens,Scope,Rest).

%split_scope([],End,AllTokens,_,_) :-
%    format('ERROR: split_scope/4 failed (End=~p) for ~p~n',[End,AllTokens]),
%    !,
%    fail.
split_scope([],_End,_AllTokens,[],[]) :-
    !.
split_scope([Token|Rest],End,_AllTokens,[Token],Rest) :-
    Token=tok(Type,_,_,pos(_,End)),
    \+higher_order_type(Type),
    !.
split_scope([First|Rest],End,AllTokens,[First|RestScope],NewRest) :-
    split_scope(Rest,End,AllTokens,RestScope,NewRest).

/* remove_bracketing(+Tokens, -ModifiedTokens)

remove_bracketing/2 removes the first and last tokens from Tokens if they
are respectively left and right brackets.  */

remove_bracketing([First|Rest],ModifiedTokens) :-
    ((First=tok(pn,LB,_,_), lbracket(LB)) ->
	ModifiedTokens0=Rest
    ;   ModifiedTokens0=[First|Rest]
    ),
    rev(ModifiedTokens0,[Last|RevModifiedTokens1]),
    ((Last=tok(pn,RB,_,_), rbracket(RB)) ->
	RevModifiedTokens=RevModifiedTokens1
    ;   RevModifiedTokens=[Last|RevModifiedTokens1]
    ),
    rev(RevModifiedTokens,ModifiedTokens).


/* extract_text(+Tokens, -Text)

extract_text/2 extracts the Text of Tokens.  */

extract_text(Tokens,Text) :-
    extract_text_aux(Tokens,TokensText),
    append(TokensText,Text).

extract_text_aux([],[]).
% not currently needed
%extract_text_aux([tok(Type,_,_,_)|Rest],ExtractedRest) :-
%    higher_order_type(Type),
%    !,
%    extract_text_aux(Rest,ExtractedRest).
extract_text_aux([tok(_,Text,_,_)|Rest],[Text|ExtractedRest]) :-
    extract_text_aux(Rest,ExtractedRest).

/* filter_out_field_comments(+FieldTokens, -FilteredFieldTokens)
   filter_out_comments(+Label, +Tokens, -FilteredTokens)

filter_out_field_comments/2 uses filter_out_comments/3 to filter out unwanted
comments from FieldTokens producing FilteredFieldTokens. Fields are used to
construct the Label argument of filter_out_comments/3.

filter_out_comments/3 filters out unwanted text (currently abstract truncation
notes) from Tokens which has Label.  */

filter_out_field_comments([], []) :- !.
filter_out_field_comments([[Field,Tokens]|Rest],
			  [[Field,FilteredTokens]|FilteredRest]) :-
	lower(Field, LCField),
	append("dummy.", LCField, DummyLabel),
	filter_out_comments(DummyLabel, Tokens, FilteredTokens),
	filter_out_field_comments(Rest, FilteredRest).

filter_out_comments(Label1, Tokens, FilteredTokens) :-
	remove_abstract_truncation_note(Label1, Tokens, FilteredTokens).

remove_abstract_truncation_note(Label1, Tokens, FilteredTokens) :-
	split_string(Label1, ".ab", _, _),
	!,
	AbstractPattern=[tok(pn,"(","(",_),
			 tok(uc,"ABSTRACT","abstract",_),
			 '.', % split_tokens/4 considers this a wildcard
			 tok(uc,"TRUNCATED","truncated",_),
			 '.',
			 tok(uc,"AT","at",_),
			 '.',
			 tok(nu,'.','.',_),
			 '.',
			 tok(uc,"WORDS","words",_),
			 tok(pn,")",")",_)],
	% This code used to append TextAfterNotice, but we now discard *everything*
	% after "(ABSTRACT TRUNCATED AT 250 WORDS)".
	( split_tokens( Tokens, AbstractPattern, TextBeforeNotice, _TextAfterNotice) ->
	  FilteredTokens = TextBeforeNotice
	; FilteredTokens=Tokens
	).
remove_abstract_truncation_note(_, Tokens, Tokens).


/* split_tokens(+Tokens, +Pattern, -Left, +Right)

split_tokens/4 is similar to nls_strings:split_string/4 except that
tokens rather than strings are involved.

split_tokens/4 looks for Pattern in Tokens, where '.' matches anything,
including the token type, the token text, the token LC text,
or even the entire token.

Left and Right are the tokens that occur to the left, respectively right,
of the tokens matching Pattern.  */

split_tokens(Tokens, Pattern, [], Right) :-
	tokens_match_pattern(Pattern, Tokens, Right),
	!.
split_tokens([First|Rest], Pattern, [First|Left], Right) :-
	split_tokens(Rest, Pattern, Left, Right).

tokens_match_pattern([], Tokens, Tokens).
tokens_match_pattern([FirstPattern|RestPattern], [FirstToken|RestTokens], Right) :-
	token_matches_pattern(FirstPattern, FirstToken),
	tokens_match_pattern(RestPattern, RestTokens, Right).

token_matches_pattern('.', _).
token_matches_pattern(tok(PatType,PatText,PatLCText,_),
                      tok(TokType,TokText,TokLCText,_)) :-
	% Either PatType is the '.' wildcard,
	% or PatType and TokType are identical.
	token_component_matches_pattern_component(PatType, TokType),
	% Either PatText is the '.' wildcard,
	% or PatText and TokText are identical.
	token_component_matches_pattern_component(PatText, TokText),
	% Either PatLCText is the '.' wildcard,
	% or PatLCText and TokLCText are identical.
	token_component_matches_pattern_component(PatLCText, TokLCText).

token_component_matches_pattern_component(PatternComponent, TokenComponent) :-
	( PatternComponent == '.' ->
	  true
	; PatternComponent == TokenComponent
	).

/* filter_out_ws_tokens(+TokensIn, -TokensOut)

filter_out_ws_tokens/2 filters out whitespace tokens from TokensIn producing
TokensOut.  */

filter_out_ws_tokens([], []).
filter_out_ws_tokens([tok(ws,_,_,_)|Rest],FilteredRest) :-
	!,
	filter_out_ws_tokens(Rest, FilteredRest).
filter_out_ws_tokens([First|Rest], [First|FilteredRest]) :-
	filter_out_ws_tokens(Rest,FilteredRest).


/* trim_ws_tokens(+TokensIn, -TokensOut)

trim_ws_tokens/2 filters out whitespace tokens
from the beginning and end of TokensIn producing TokensOut.  */


trim_ws_tokens([], []) :- !.
trim_ws_tokens(TokensIn, TokensOut) :-
	trim_left_ws_tokens(TokensIn, TokensInOut),
	rev(TokensInOut, RevTokensInOut),
	trim_left_ws_tokens(RevTokensInOut, RevTokensOut),
	rev(RevTokensOut, TokensOut).

trim_left_ws_tokens([], []).
trim_left_ws_tokens([Tok|Rest], TrimmedRest) :-
	( ws_tok(Tok) ->
	  trim_left_ws_tokens(Rest, TrimmedRest)
	; TrimmedRest = [Tok|Rest]
	).

/* position_ge(+Pos1, +Pos2)

position_ge/2 computes Pos1 >= Pos2 which is true if Begin1 > Begin2 or
they're equal and End1 =< End2 (yes, less! so that tokens with larger
scope appear before those with lesser scope.) */

position_ge(pos(Begin1,_End1),pos(Begin2,_End2)) :-
    Begin1 > Begin2,
    !.
position_ge(pos(Begin,End1),pos(Begin,End2)) :-
    End1 =< End2.


/* position_contains(+Pos1, +Pos2)

position_contains/2 succeeds if Pos1 contains Pos2 which is true if
Begin1 =< Begin2 and End2 =< End1. */

position_contains(pos(Begin1,End1),pos(Begin2,End2)) :-
    Begin1 =< Begin2,
    End2 =< End1,
    !.
