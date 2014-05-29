
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

% File:     text_object_tokens.pl
% Module:   Text Object Tokens
% Author:   Lan
% Purpose:  To provide operations on text object tokens


:- module(text_object_tokens,[
	all_alnum/1,
	compute_token_position/2,
	extract_text/2,
	filter_out_field_comments/2,
	form_field_tokens/3,
	form_simple_tokens/4,
	get_token_position/2,
	position_contains/2,
	position_ge/2,
	remove_bracketing/2,
	split_scope/4,
	trim_ws_tokens/2
    ]).


:- use_module(metamap(metamap_tokenization), [
	local_alnum/1,
	local_alpha/1,
	local_digit/1,
	local_punct/1,
	local_lower/1,
	local_upper/1

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

%:- use_module(skr_lib(ctypes),[
	% is_alnum/1,
	% is_alpha/1,
	% is_lower/1,
	% is_upper/1
	% is_digit/1
	% is_punct/1
%    ]).

:- use_module(skr_lib(nls_strings),[
	split_string/4
    ]).

:- use_module(skr_lib(sicstus_utils),[
	lower/2,
	lowercase_list/2
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

form_field_tokens([], _ExtraChars, []).
form_field_tokens([[Field,TokenizedField]|Rest], ExtraCharsIn, [[Field,Tokens]|TokenizedRest]) :-
        lowercase_list(TokenizedField, LCTokenizedField),
	ExtraCharsConsumed is 0,
	CurrentPos is 0,
        form_simple_tokens_1(TokenizedField, LCTokenizedField, ExtraCharsIn,
			     ExtraCharsConsumed, CurrentPos, Tokens, ExtraCharsNext),
        form_field_tokens(Rest, ExtraCharsNext, TokenizedRest).

form_simple_tokens(Tokenized, LCTokenized, StartPos, SimpleTokens) :-
	ExtraCharsIn = [],
	ExtraCharsConsumed is 0,
	form_simple_tokens_1(Tokenized, LCTokenized, ExtraCharsIn, ExtraCharsConsumed,
			     StartPos, SimpleTokens, _ExtraCharsOut).

form_simple_tokens_1([], [], ExtraChars, _ExtraCharsConsumed, _CurrentPos, [], ExtraChars).
form_simple_tokens_1([Text|RestText], [LCText|RestLCText], ExtraCharsIn, ExtraCharsConsumedIn,
		     CurrentPos, [tok(Type,Text,LCText,pos(StartPos,EndPos))|RestTokens],
		     ExtraCharsOut) :-
        set_token_type(Text, Type),
        set_start_and_end_pos(CurrentPos, Text, ExtraCharsIn, ExtraCharsConsumedIn,
			      StartPos, EndPos, NextPos, ExtraCharsConsumedNext, ExtraCharsNext),
        form_simple_tokens_1(RestText, RestLCText, ExtraCharsNext,
			     ExtraCharsConsumedNext, NextPos, RestTokens, ExtraCharsOut).

% If CurrentPos is specified as X/Y, we want to explicitly specify
% the StartPos and EndPos of each token created as those specific values;
% otherwise, use CurrentPos as the StartPos, and StartPos + TextLength as the EndPos.

% CurrentPos is the current character position in the original UTF-8 text!

set_start_and_end_pos(CurrentPos, Text, ExtraCharsIn, ExtraCharsConsumedIn,
		      StartPos, EndPos, NextPos, ExtraCharsConsumedOut, ExtraCharsOut) :-
	  length(Text, TextLength),
	  % When is CurrentPos ever = StartPos/EndPos?!
%	( CurrentPos = StartPos/EndPos ->
%	  % format(user_input, 'HERE: ~w~n', [CurrentPos]), ttyflush, halt,
%	  NextPos = CurrentPos
%	; integer(CurrentPos),
	  % If there are no extraASCII chars to deal with
	( ExtraCharsIn == [] ->
	  StartPos is CurrentPos,
	  ExtraCharsConsumedOut is ExtraCharsConsumedIn,
	  ExtraCharsOut = ExtraCharsIn,
	  EndPos is StartPos + TextLength
	; % Are there extra ASCII chars to take into account because of a UTF-8 conversion?
	  % If so, ExtraCharsIn is a list of Pos-ExtraChars tokens, e.g., [ 4-2, 33-2, ... ]
	  set_start_end_pos_1(ExtraCharsIn, ExtraCharsConsumedIn,
			      TextLength, CurrentPos, StartPos, EndPos,
			      ExtraCharsConsumedOut, ExtraCharsOut)
	),
	NextPos is EndPos.

% This needs to be more fully documented!!

% Suppose the original UTF8 input text is
% the ⅞ Sjögren's syndrome
% 0123456789|123456789|123456

% The ASCII version will be
% the 7/8 Sjögren's syndrome
% 0123456789|123456789|123456

% and the ExtraChars will be [4-2], meaning that 2 extra ASCII chars were added at CharPos 4,
% where the UTF-8 char "⅞" is expanded to "7/8". In this case, LastExtraCharPos will be 6.

% The problem is that MetaMap never sees "⅞", because it's expanded to "7/8",
% so MetaMap needs to adjust the positional info by subtracting char positions as needed.
% The "7", "/", and "8" tokens must have StartPos = 4 and EndPos = 5, and
% the "Sjögren's" token must have StartPos = 6 and EndPos = 15.
% Moreover, after we've passed position 4, we must delete the ExtraChar info 4-2
% because it's no longer relevant.

set_start_end_pos_1(ExtraCharsIn, ExtraCharsConsumedIn, TextLength, CurrentPos,
		    StartPos, EndPos, ExtraCharsConsumedOut, ExtraCharsOut) :-
	ExtraCharsIn = [FirstExtraCharPos-NumExtraChars|RestExtraCharsIn], 
	  % Case #1:
	  % If the CurrentPos is < FirstExtraCharPos, i.e., we're strictly before the char pos
	  % of the next UTF-8 char adjustment, then do nothing different:
	  %  * Set the StartPos of the current token to CurrentPos,
	  %  * Do not increment ExtraCharsConsumed, and
	  %  * Return the ExtraChars listunchanged in ExtraCharsOut.

	  % CurrentPos is STRICTLY BEFORE first UTF=8 char; do nothing special
	( CurrentPos < FirstExtraCharPos ->
	  StartPos is CurrentPos,
	  EndPos is StartPos + TextLength,
	  ExtraCharsConsumedOut is ExtraCharsConsumedIn,
	  ExtraCharsOut = ExtraCharsIn
	  % Case #2:
	  % CurrentPos is AT first UTF=8 char, which is exactly one character long
	; CurrentPos =:= FirstExtraCharPos ->
	  % We are AT the char pos of the first UTF-8 char,
	  % so use CurrentPos as StartPos of the current ASCII token
	  % the UTF-8 char is one character long, so EndPos is StartPos + 1
	  StartPos is CurrentPos, % which is =:= FirstExtraCharPos
	  EndPos is StartPos + 1,
	  % Now it gets tricky, because there can be multiple ASCII tokens,
	  % e.g., when "⅞" is replaced by "7/8" (three tokens), or only a single token,
	  % e.g., when "â" is replaced by "a", or "ƛ" is replaced by "lambda".
	    
	  % If no extra chars have been consumed and TextLength =:= NumExtraChars,
	  % that means there's only one ASCII token (regardless of its length):
	  % Remove the first element of ExtraCharsIn, i.e.,FirstExtraCharPos-NumExtraChars,
	  % and proceed with the rest of the ExtraCharsIn list.
	  % Also reset ExtraCharsConsumed to 0, because we're done with this UTF-8 char.
          ( ExtraCharsConsumedIn =:= 0,
	    TextLength =:= NumExtraChars + 1 ->
            ExtraCharsOut = RestExtraCharsIn,
            ExtraCharsConsumedOut is 0
	    % There are multiple ASCII tokens, so we're not yet done with this UTF-8 char:
	    % Keep the ExtraChars list the same, and increment ExtraCharsConsumed by
	    % one less than the length of the current ASCII token. (One less because
	    % one char is matched by the single-char UTF-8 token).	    
          ; ExtraCharsOut = [FirstExtraCharPos-NumExtraChars|RestExtraCharsIn],
            ExtraCharsConsumedOut is ExtraCharsConsumedIn + TextLength - 1
          )
	  % Case #3:
	  % We are past the first CharPos of the UTF-8 char, which means there must be
	  % multiple ASCII tokens (e.g., when "⅞" is replaced by "7/8").
	  %  * set StartPos of current token to FirstExtraCharPos (e.g., 4);
	  %    for our example above, the "7", "/", and "8" tokens should all have 4 as StartPos;
	  %  * set EndPos of current token to StartPos+1 b/c the UTF-8 char is one character long;
	  %    Also, count the number of extra chars consumed (ExtraCharsConsumedIn + TextLength).
	  %  * If that sum is equal to NumExtraChars, we've consumed all the extra chars
	  %    ("7", "/", and "8" in our example), and this UTF-8 adjustment is done, so
	  %    pass back the rest of the ExtraChars list (RestExtraCharsIn), and
	  %    set ExtraCharsConsumed to 0 (for this UTF-8 character);
	  %    otherwise pass back the ExtraChars list unchanged in ExtraCharsOut,
	  %    and add TextLength to ExtraCharsConsumedIn to get ExtraCharsConsumedOut.

	  % CurrentPos > FirstExtraCharPos means we've passed the original UTF-8 position
	  % CurrentPos is INSIDE ASCII expansion, but not at end
	  % This condition is redundant, but included for clarity.
        ; CurrentPos > FirstExtraCharPos,
          StartPos is FirstExtraCharPos,
	  EndPos is StartPos + 1,
          ( ExtraCharsConsumedIn + TextLength =:= NumExtraChars ->
            ExtraCharsOut = RestExtraCharsIn,
            ExtraCharsConsumedOut is 0
          ; ExtraCharsOut = [FirstExtraCharPos-NumExtraChars|RestExtraCharsIn],
            ExtraCharsConsumedOut is ExtraCharsConsumedIn + TextLength
          )
        ).


% ws is space, tab or newline
set_token_type(Token, ws) :-
	Token = [Char],
	ws_char(Char),
	!.

% token_type(" ",ws) :-
%     !.
% token_type("	",ws) :-
%     !.
% token_type("
% ",ws) :-
%     !.

set_token_type([Char], Type) :-
	local_punct(Char),
	!,
	Type = pn.
set_token_type(Token, Type) :-
	Token \== "",
	all_alnum(Token),
	!,
	set_alnum_token_type(Token, Type).

% This should happen only for tokens ending in apostrophe-s, as far as I know.
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
	    local_upper(First),
	    all_lower(Rest)) ->
	    Type = ic
	; Type = mc
	).

all_alnum([]).
all_alnum([Char|Rest]) :-
	local_alnum(Char),
	all_alnum(Rest).

all_alpha([]).
all_alpha([Char|Rest]) :-
	local_alpha(Char),
	all_alpha(Rest).

all_lower([]).
all_lower([Char|Rest]) :-
	local_lower(Char),
	all_lower(Rest).

all_upper([]).
all_upper([Char|Rest]) :-
	local_upper(Char),
	all_upper(Rest).

all_digit([]).
all_digit([Char|Rest]) :-
	local_digit(Char),
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

split_scope(Tokens, pos(_,End), Scope, Rest) :-
	split_scope_aux(Tokens, End, Tokens, Scope, Rest).

%split_scope([],End,AllTokens,_,_) :-
%    fatal_error('split_scope/4 failed (End=~p) for ~p~n',[End,AllTokens]),
%    !,
%    fail.
split_scope_aux([], _End, _AllTokens, [], []).
split_scope_aux([Token|Rest], End, _AllTokens, [Token], Rest) :-
	Token = tok(Type,_,_,pos(_,End)),
	\+ higher_order_type(Type),
	!.
split_scope_aux([First|Rest], End, AllTokens, [First|RestScope], NewRest) :-
	split_scope_aux(Rest, End, AllTokens, RestScope, NewRest).

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

/* trim_ws_tokens(+TokensIn, -TokensOut)

trim_ws_tokens/2 filters out whitespace tokens
from the beginning and end of TokensIn producing TokensOut.  */


trim_ws_tokens([], []).
trim_ws_tokens([FirstToken|RestTokens], TokensOut) :-
	trim_left_ws_tokens([FirstToken|RestTokens], TokensInOut),
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
