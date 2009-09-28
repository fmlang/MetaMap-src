% File:     pos_info.pl
% Module:   SKR Positional Information
% Author:   FML
% Purpose:  Calculate positional information for tokens


:- module(pos_info,[
	create_EXP_raw_token_list/6,
	create_UNEXP_raw_token_list/5,
	get_next_token_state/3
    ]).

:- use_module(skr_lib(nls_strings),[
	trim_whitespace_left_1/4
    ]).

:- use_module(skr(skr_utilities),[
	token_template/5,
	token_template/6,
	write_token_list/3
    ]).

:- use_module(skr_lib(ctypes),[
	is_space/1
    ]).

:- use_module(skr_lib(sicstus_utils),[
	ttyflush/0
    ]).

:- use_module(text(text_object_util),[
	aa_tok/1,
	aadef_tok/1,
	ex_rbracket_tok/1,
	higher_order_or_annotation_tok/1,
	higher_order_or_annotation_type/1,
	hyphen_punc/1,
	punc_tok/1,
	token_sequence_length/4,
	ws_tok/1,
	ws_char/1
   ]).

:- use_module(library(lists),[
	append_length/3,
	sublist/4
    ]).


% one-off: aa aadef ws
% hoa: field label sn pe
% an/pn: an ic lc mc nu pn uc
create_EXP_raw_token_list([], _PreviousToken, _CurrentPos, _TokenState, _InputString, []).
create_EXP_raw_token_list([CurrentToken|RestTokensIn], PreviousToken, CurrentPos, TokenState,
		          InputStringIn, NewTokens) :-
	% CurrentToken  = tok(CurrentTokenType, _CurrentString, _CurrentLCString, _CurrentPos),
	token_template(CurrentToken, CurrentTokenType,
		       _CurrentString, _CurrentLCString, _CurrentPos),
	handle_token_type(CurrentTokenType, PreviousToken, TokenState,
			  InputStringIn,   CurrentToken, CurrentPos, RestTokensIn,
			  InputStringNext, NewTokens,    NextPos,    RestTokensNext,
			  RestNewTokens),
	get_next_token_state(TokenState, CurrentTokenType, NextTokenState),
	create_EXP_raw_token_list(RestTokensNext, CurrentToken, NextPos, NextTokenState,
				  InputStringNext, RestNewTokens).

create_UNEXP_raw_token_list([], _CurrentPos, _TokenState, _InputString, []).
create_UNEXP_raw_token_list([CurrentToken|RestTokens], CurrentPos, TokenState,
			    InputStringIn, [NewCurrentToken|NewRestTokens]) :-
	% arg(1, CurrentToken, CurrentTokenType),
	token_template(CurrentToken, CurrentTokenType, _String, _LCString, _PosInfo),
  	( CurrentTokenType == 'ws' ->
 	  % NextPos is CurrentPos + NumBlanksRemoved,
	  InputStringIn = [WhiteSpaceChar|InputStringTemp],
	  is_space(WhiteSpaceChar),
 	  add_raw_pos_info_1(CurrentToken, CurrentPos, NewCurrentToken),
	  % Handle blank spaces at the beginning of lines,
	  % e.g., in MedLine citations, that do not get tokenized
	  remove_untokenized_whitespace(RestTokens, InputStringTemp,
	  				InputStringNext, NumBlanksRemoved),
	  NextPos is CurrentPos + 1 + NumBlanksRemoved
	; higher_order_or_annotation_type(CurrentTokenType) ->
	  % CurrentToken    = tok(Type, ExpTokens, AcroTokens, Pos1),
	  % NewCurrentToken = tok(Type, ExpTokens, AcroTokens, Pos1, Pos1),
	  add_raw_pos_info_1(CurrentToken, CurrentPos, NewCurrentToken),
	  % NextTokenState is TokenState,
	  NextPos is CurrentPos,
	  InputStringNext = InputStringIn
	  % for tokens that have content (uc, lc, etc.)
	; add_raw_pos_info_2(CurrentToken,    an, _PrevTokenType, CurrentPos, InputStringIn, 
			     NewCurrentToken, NextPos,    InputStringNext),
	  test_for_adjacency(TokenState, NewCurrentToken, CurrentPos)
        ),
	% format(user_output, 'Current Token: ~q~n', [CurrentToken]),
	% format(user_output, '    New Token: ~q~n', [NewToken]),
	get_next_token_state(TokenState, CurrentTokenType, NextTokenState),
	create_UNEXP_raw_token_list(RestTokens, NextPos, NextTokenState,
				    InputStringNext, NewRestTokens).


% aadef token:
% tok(aadef,
%     [tok(lc,heart,heart,pos(0,5)),
%      tok(ws,' ',' ',pos(5,6)),
%      tok(lc,attack,attack,pos(6,12))],
%     [tok(uc,'HA',ha,pos(14,16))],
%     pos(0,12))
% 
% aa token:
% tok(aa,
%     [tok(uc,'HA',ha,pos(26,28))],
%     [tok(aadef,
%         [tok(lc,heart,heart,pos(0,5)),
%          tok(ws,' ',' ',pos(5,6)),
%          tok(lc,attack,attack,pos(6,12))],
%         [tok(uc,'HA',ha,pos(17,19))],
%         pos(0,12))],
%     pos(26,28))

% In the clauses for handle_token_type/11, the variables are the following:

% TokenType:     The current token type.
% PrevToken:     The previous token.
% InputStringIn: The input string when handle_ws_token_is called.
% CurrentPos:    The current character position.
% RestTokens:    The list containing the remaining input tokens.
% TokenState:    The current state of the token stream: see get_next_token_state/3.
	           

% InputStringNext: The input string after processing this ws token.
% NewTokens:       The output token list.
% NextPos:         The character position afteer processing this ws token.
% RestTokensNext:  The input token list on which create_EXP_raw_token_list/5 should recurse.
%		   In this simple case, RestTokensNext is simply RestTokens.
% RestNewTokens:   The output token list on which create_EXP_raw_token_list/5 should recurse.
%		   In this simple case, RestNewTokens is simply the tail of NewTokens.

% Token types ws, aadef, and aa are handled specially.

% special_token_type(ws).
special_token_type(aa).
special_token_type(aadef).

handle_token_type(TokenType, PrevToken, TokenState,
		  InputStringIn,   CurrentToken, CurrentPos, RestTokensIn,
		  InputStringNext, NewTokens,    NextPos,    RestTokensNext,
		  RestNewTokens) :-
	( TokenType == ws ->
	  handle_ws_token_type(TokenState,
			       InputStringIn,   CurrentToken, CurrentPos, RestTokensIn,
			       InputStringNext, NewTokens,    NextPos,    RestTokensNext,
			       RestNewTokens)
	; special_token_type(TokenType) ->
	  handle_special_token_type(TokenType, PrevToken,
				    InputStringIn,   CurrentToken, CurrentPos, RestTokensIn,
				    InputStringNext, NewTokens,    NextPos,    RestTokensNext,
				    RestNewTokens)
	; % aadef and aa are higher-order types, but they'll be caught by the first branch
	  higher_order_or_annotation_type(TokenType) ->
	  handle_hoa_type(InputStringIn,   CurrentToken, CurrentPos, RestTokensIn, _TokenState,
			  InputStringNext, NewTokens,    NextPos,    RestTokensNext,
			  RestNewTokens)
	; handle_an_pn_type(InputStringIn,   CurrentToken, CurrentPos, RestTokensIn, TokenState,
			    InputStringNext, NewTokens,    NextPos,    RestTokensNext,
			    RestNewTokens)
	).	  


handle_ws_token_type(_TokenState,
		     InputStringIn,   CurrentToken, CurrentPos, RestTokens,
		     InputStringNext, NewTokens,    NextPos,    RestTokensNext,
		     RestNewTokens) :-
	  InputStringIn = [WhiteSpaceChar|InputStringTemp],
	  is_space(WhiteSpaceChar),
 	  add_raw_pos_info_1(CurrentToken, CurrentPos, NewCurrentToken),
	  % Handle blank spaces at the beginning of lines,
	  % e.g., in MedLine citations, that do not get tokenized
	  remove_untokenized_whitespace(RestTokens, InputStringTemp,
	  				InputStringNext, NumBlanksRemoved),
	  NextPos is CurrentPos + 1 + NumBlanksRemoved,
	  % NewTokens is the top-level output;
	  % the top-level predicate recurses on RestNewTokens.
	  NewTokens = [NewCurrentToken|RestNewTokens],
	  % The top-level predicate recurses on RestTokensNext,
	  % which, in this case, happens to be just RestTokens.
	  RestTokensNext = RestTokens.

handle_special_token_type(aadef, PrevToken,
			  InputStringIn,   CurrentToken, CurrentPos, RestTokensIn,
			  InputStringNext, NewTokens,    NextPos,    RestTokensNext,
			  RestNewTokens) :-
	arg(2, CurrentToken, AADefTokens0),
	% Add tokens in the first aadef list, e.g.,
	% tok(lc,heart,heart,pos(0,5))
	% tok(ws,' ',' ',pos(5,6))
	% tok(lc,attack,attack,pos(6,12))
	% to the list of newly created tokens -- with the addition
	% of raw positional information.
	% RestNewTokens is the uninstantiated tail of NewTokens.
	% Also, consume the strings contained in those tokens.
	% PrevToken = tok(PrevTokenType, _PrevString, _PrevLCString, _PrevPos),
	token_template(PrevToken, PrevTokenType, _PrevString, _PrevLCString, _PrevPos),
	% This hack is intended to handle text like "urinary (u-) ..."
	remove_final_hyphen_token(AADefTokens0, AADefTokens),
	create_new_tokens_and_consume_strings(AADefTokens, PrevTokenType, RestTokensIn,
					      CurrentPos, InputStringIn,
					      NewTokens, RestNewTokens, NextPos1,
					      InputString1),
	% Then, match and remove those aadef tokens from RestTokensIn
	remove_tokens_no_consume(AADefTokens, RestTokensIn, RestTokensIn1),
	remove_tokens_up_to_ws_token(RestTokensIn1, RestTokensIn2),
	% remove all following tokens up to and including
	% the exclusive right-bracket token
	% ws*, pe, pn ("(" or "["), aa, uc*, pn (")" or "]")
	consume_tokens_through_ex_rbracket(RestTokensIn2, InputString1, NextPos1,
					   RestTokensNext, InputStringNext, NextPos),
	!.
	% fail.
handle_special_token_type(aadef, _PrevToken,
			  InputStringIn,   CurrentToken, CurrentPos, RestTokensIn,
			  InputStringNext, NewTokens,     NextPos,    RestTokensNext,
			  RestNewTokens) :-
	format(user_output, '#### WARNING: aadef token failed:~n', []),
	write_token_list([CurrentToken], 0, 1),
	ttyflush,
	InputStringNext = InputStringIn,
	RestNewTokens = NewTokens,
	NextPos is CurrentPos,
	RestTokensNext = RestTokensIn.

handle_special_token_type(aa, _PrevToken,
			  InputStringIn,   CurrentToken, CurrentPos, RestTokensIn,
			  InputStringNext, NewTokens,    NextPos,    RestTokensNext,
			  RestNewTokens) :-
	  % AADefExpansionTokenList is a list containing these three tokens:
	  % tok(lc,heart,heart,pos(0,5))
	  % tok(ws,' ',' ',pos(5,6))
	  % tok(lc,attack,attack,pos(6,12))
	  arg(2, CurrentToken, AATokenList),
	  arg(3, CurrentToken, [AADefToken|_]),
	  arg(2, AADefToken,   AADefExpansionTokenList),

	  % calculate the length of the concatenation of all the AA tokens
	  % We need to march off characters in InputStringIn
	  % skipping over blanks, until we consume every string in AATokenList
	  get_aa_tokens_start_and_end_pos(AATokenList, AAStartPos, AAEndPos),          
	  get_aa_tokens_length(AATokenList, InputStringIn, 0, 0, AALength),
	  % adjust_start_and_end_pos(InputStringIn, TempCurrentPos,
	  %			     TempAAStartPos, TempAAEndPos,
	  %			     AATokenList,
	  %			     CurrentPos, AAStartPos, AAEndPos),
	  % AALength is AAEndPos - AAStartPos,				  
	  % Create a new token for each token in the AADef Expansion
	  create_new_tokens_no_consume(AADefExpansionTokenList, CurrentPos,
	  			       AALength, AAStartPos, AAEndPos,
				       NewTokens, RestNewTokens),
	  arg(2, CurrentToken, AATokenList),
	  AATokenList = [AAH|AAT],
	  % remove from the remaining token list the AA tokens 
	  remove_aa_tokens(AAT, AAH, RestTokensIn, RestTokensNext),
	  % consume the text strings in the uc tokens from the InputString
	  consume_aa_token_strings(AAT, AAH, InputStringIn, InputStringNext),
	  % This cut can't be there because
	  % consume_aa_token_strings must be backtrackable. Ugh.
	  % !,
	  NextPos is CurrentPos + AALength.
handle_special_token_type(aa, _PrevToken,
			  InputStringIn,   CurrentToken, CurrentPos, RestTokensIn,
			  InputStringNext, NewTokens,     NextPos,    RestTokensNext,
			  RestNewTokens) :-
	format(user_output, '#### WARNING: aa token failed:~n', []),
	write_token_list([CurrentToken], 0, 1),
	ttyflush,
	InputStringNext = InputStringIn,
	RestNewTokens = NewTokens,
	NextPos is CurrentPos,
	RestTokensNext = RestTokensIn.

% Token types field, label, sn, pe are handled identically via handle_hoa_type.
% "hoa" == "higher-order or annotation type" (other than aadef and aa)
handle_hoa_type(InputStringIn,   CurrentToken, CurrentPos, RestTokensIn, _TokenState,
		InputStringNext, NewTokens,    NextPos,    RestTokensNext,
		RestNewTokens) :-
	  add_raw_pos_info_1(CurrentToken, CurrentPos, NewCurrentToken),
          NextPos is CurrentPos,
          InputStringNext = InputStringIn,
	  RestTokensNext = RestTokensIn,
	  NewTokens = [NewCurrentToken|RestNewTokens].

% Token types an, ic, lc, mc, nu, pn, uc are handled identically via handle_an_pn_type.
handle_an_pn_type(InputStringIn,   CurrentToken, CurrentPos, RestTokensIn, TokenState,
		  InputStringNext, NewTokens,    NextPos,    RestTokensNext,
		  RestNewTokens) :-
	  add_raw_pos_info_2(CurrentToken, an, _PrevTokenType,  CurrentPos, InputStringIn, 
			     NewCurrentToken, NextPos,    InputStringNext),
	  test_for_adjacency(TokenState, NewCurrentToken, CurrentPos),
	  NewTokens = [NewCurrentToken|RestNewTokens],
	  RestTokensNext = RestTokensIn.

remove_tokens_up_to_ws_token([FirstToken|RestTokens], TokensOut) :-
	( ws_tok(FirstToken) ->
	  TokensOut = RestTokens
	; remove_tokens_up_to_ws_token(RestTokens, TokensOut)
	).


remove_final_hyphen_token(AADefTokens0, AADefTokens) :-
	( append(AADefTokensPrefix, [LastAADefToken], AADefTokens0),
	  token_template(LastAADefToken, pn, HyphenString, HyphenString, _PosInfo),
	  hyphen_punc(HyphenString) ->
	  AADefTokens = AADefTokensPrefix
	; AADefTokens = AADefTokens0
	).

% If the next token is not a ws token, strip off any whitespace,
% and return the new InputString and the number of whitespace chars removed.
remove_untokenized_whitespace([], InputString, InputString, 0).
remove_untokenized_whitespace([NextToken|_], InputStringIn,
			      InputStringOut, NumBlanksRemoved) :-
	( ws_tok(NextToken) ->
	  InputStringOut = InputStringIn,
	  NumBlanksRemoved is 0
        ; remove_whitespace_chars(InputStringIn, 0, InputStringOut, NumBlanksRemoved)
        ).


% remove_parenthesized_chars([FirstToken|RestToken], InputStringIn, InputStringOut) :-
% 	( higher_order_or_annotation_tok(FirstToken) ->
% 	  remove_parenthesized_chars(RestToken, InputStringIn, InputStringOut)
% 	; arg(2, FirstToken, FirstTokenChars),
% 	  append(FirstTokenChars, InputStringNext, InputStringIn),
% 	  ( ex_rbracket_tok(FirstToken) ->
% 	    InputStringOut = InputStringNext
% 	  ; remove_parenthesized_chars(RestToken, InputStringNext, InputStringOut)
% 	  )
% 	).

% Given a list of AA tokens, which represent the unexpanded acronym, e.g.,
% [tok(uc,"HA","ha",pos(38,40)),tok(uc,"LC","lc",pos(41,43))]
% compute the length of the printed representation.
% This is done by subtracting the start position of the first AA token (38)
% from the end position of the last LC token (43). Here we get 5. Duh.

get_aa_tokens_start_and_end_pos([FirstAAToken|RestAATokens],
                                FirstAAStartPos, LastAAEndPos) :-
        arg(4, FirstAAToken, FirstPosTerm),
        arg(1, FirstPosTerm, FirstAAStartPos),
        arg(2, FirstPosTerm, FirstAAEndPos),
        get_last_aa_token_end_pos(RestAATokens, FirstAAEndPos, LastAAEndPos).

get_last_aa_token_end_pos([], LastAAEndPos, LastAAEndPos).
get_last_aa_token_end_pos([NextAAToken|RestAATokens], _PrevAAEndPos, LastAAEndPos) :-
        arg(4, NextAAToken, NextPosTerm),
        arg(2, NextPosTerm, NextAAEndPos),
        get_last_aa_token_end_pos(RestAATokens, NextAAEndPos, LastAAEndPos).

get_aa_tokens_length([], _InputStringIn, PrevNumLeftBlanksTrimmed, LengthSoFar, Length) :-
	Length is LengthSoFar - PrevNumLeftBlanksTrimmed.
get_aa_tokens_length([FirstAAToken|RestAATokens], InputStringIn, _PrevNumLeftBlanksTrimmed,
		     LengthSoFar, Length) :-
	get_one_token_pos_length(FirstAAToken, InputStringIn,
				 InputStringNext, TokenStringLength, NumLeftBlanksTrimmed),
	NextLength is LengthSoFar + TokenStringLength + NumLeftBlanksTrimmed,
	get_aa_tokens_length(RestAATokens, InputStringNext, NumLeftBlanksTrimmed, NextLength, Length).	

% calculate how many chars we peel off from InputString when we remove
% (1) the chars in TokenString, and then
% (2)  all leading blanks (i.e., trim_left)
get_one_token_pos_length(AAToken, InputStringIn,
			 InputStringOut, TokenStringLength, NumLeftBlanksTrimmed) :-
	token_template(AAToken,
		       _TokenType, TokenString, _LCTokenString, _PosInfo),
	append(TokenString, InputStringNext, InputStringIn),
	length(TokenString, TokenStringLength),
	trim_whitespace_left_1(InputStringNext, 0, InputStringOut, NumLeftBlanksTrimmed).

%%% get_aa_tokens_start_and_end_pos([FirstAAToken|RestAATokens], InputStringIn,
%%% 				FirstAAStartPos, LastAAEndPos) :-
%%% 	arg(4, FirstAAToken, FirstPosTerm),
%%% 	arg(1, FirstPosTerm, FirstAAStartPos),
%%% 	arg(2, FirstPosTerm, FirstAAEndPos),
%%% 	get_last_aa_token_end_pos(RestAATokens, FirstAAEndPos, LastAAEndPos).
%%% 
%%% get_last_aa_token_end_pos([], LastAAEndPos, LastAAEndPos).
%%% get_last_aa_token_end_pos([NextAAToken|RestAATokens], _PrevAAEndPos, LastAAEndPos) :-
%%% 	arg(4, NextAAToken, NextPosTerm),
%%% 	arg(2, NextPosTerm, NextAAEndPos),
%%% 	get_last_aa_token_end_pos(RestAATokens, NextAAEndPos, LastAAEndPos).

% remove_aa_tokens/4 is called while processing an aa token in the TokenList.
% The first argument passed to remove_aa_tokens is
% the second argument of the aa token.
% In the admittedly contrived case
% "heart attack lung cancer (HA LC) HA LC",
% the aa token is the following:

% tok(aa,
%     [tok(uc,'HA',ha,pos(33,35)),
%      tok(uc,'LC',lc,pos(37,39))],
%     [tok(aadef,
%         [tok(lc,heart,heart,pos(0,5)),
%          tok(ws,' ',' ',pos(5,6)),
%          tok(lc,attack,attack,pos(6,12)),
%          tok(ws,' ',' ',pos(12,13)),
%          tok(lc,lung,lung,pos(13,17)),
%          tok(ws,' ',' ',pos(17,18)),
%          tok(lc,cancer,cancer,pos(18,24))],
%         [tok(uc,'HA',ha,pos(26,28)),
%          tok(uc,'LC',lc,pos(29,31))],
%         pos(0,24))],
%     pos(33,39))

% We want to remove these two tokens from the TokenList:
%     tok(uc,'HA',ha,pos(33,35))
%     tok(uc,'LC',lc,pos(37,39))
% Unfortunately, the TokenList may have one or more
% intervening whitespace tokens, e.g.,
%     tok(ws," "," ",pos(49,50))
% so these must explicitly be removed via remove_next_whitespace_tokens,
% except for after the *final* AA token!!

% There are pathological cases in which an aa token intervenes between
% an aadef token and the individual expansion tokens;
% if one is encountered, just skip over it.

final_match([FirstToken|RestTokens0], LastAAToken, RestTokens) :-
	( FirstToken == LastAAToken ->
	  RestTokens = RestTokens0
	; aa_tok(FirstToken) ->
	  arg(2, FirstToken, [LastAAToken]),
	  RestTokens0 = [LastAAToken|RestTokens]
	).

remove_aa_tokens([], LastAAToken, [NextTokenIn|TempRestTokens], RestTokens) :-
	( aa_tok(NextTokenIn) ->
	  arg(2, NextTokenIn, [LastAAToken|_]),
	  final_match(TempRestTokens, LastAAToken, RestTokens)
	; aadef_tok(NextTokenIn) ->
	  arg(2, NextTokenIn, [LastAAToken|_]),
	  TempRestTokens = [LastAAToken|RestTokens]
	; LastAAToken == NextTokenIn,
	  RestTokens = TempRestTokens
	).
	
remove_aa_tokens([NextAAToken|RestAATokens], AAToken, [NextTokenIn|RestTokensIn], RestTokensOut) :-
	( aa_tok(NextTokenIn) ->
	  remove_aa_tokens([NextAAToken|RestAATokens], AAToken,RestTokensIn, RestTokensOut)
	; aadef_tok(NextTokenIn) ->
	  remove_aa_tokens([NextAAToken|RestAATokens], AAToken,RestTokensIn, RestTokensOut)
	; AAToken == NextTokenIn,
	  remove_next_whitespace_tokens(RestTokensIn, RestTokensNext),
	  remove_aa_tokens(RestAATokens, NextAAToken, RestTokensNext, RestTokensOut)
	).

remove_next_whitespace_tokens([FirstToken|RestTokensIn], RestTokensOut) :-
	( ws_tok(FirstToken) ->
	  remove_next_whitespace_tokens(RestTokensIn, RestTokensOut)
	; RestTokensOut = [FirstToken|RestTokensIn]
	).

create_new_tokens_no_consume([], _CurrPos,
			     _AATokenLength, _AAStartPos, _AAEndPos,
			     NewTokens, NewTokens).
create_new_tokens_no_consume([FirstToken|RestTokens], CurrPos,
			     AATokenLength,  AAStartPos, AAEndPos,
			     [NewFirstToken|NewTokensNext], NewTokensOut) :-
	% FirstToken = tok(TokenType, Text, LCText, _OrigPosTerm),
	token_template(FirstToken, TokenType, Text, LCText, _OrigPosTerm),
	% add_pos_term_2(T, H, CurrPos, AATokenLength, NewPosTerms),
	% FirstNewToken  = tok(TokenType, Text, LCText,
	% 		     pos(AAStartPos,AAEndPos),
	% 		     pos(CurrPos, AATokenLength)),
	token_template(NewFirstToken, TokenType, Text, LCText,
		       pos(AAStartPos,AAEndPos),
		       pos(CurrPos, AATokenLength)),
	create_new_tokens_no_consume(RestTokens, CurrPos,
				     AATokenLength,  AAStartPos, AAEndPos,
				     NewTokensNext, NewTokensOut).

% consume_aa_token_strings/4 does for the input string
% exactly what remove_aa_tokens/4 does for the token list:
% consume the text strings in the uc tokens from the InputString,
% including any intervening blank spaces that are not in the token list
consume_aa_token_strings([], LastAAToken, InputStringIn, InputStringOut) :-
	arg(2, LastAAToken, FirstTokenString),
	sublist(InputStringIn, FirstTokenString, PrefixLength, TextLength),
	% Don't allow skipping too far forward in input string!
	limit_forward_skip(LastAAToken, PrefixLength),
	NumCharsConsumed is PrefixLength + TextLength,
	append_length(InputStringOut, InputStringIn, NumCharsConsumed).
	% Can't simply append because of cases where the first token is an aa!
	% append(FirstTokenString, InputStringOut, InputStringIn).
consume_aa_token_strings([NextAAToken|RestAATokens], AAToken, InputStringIn, InputStringOut) :-
	arg(2, AAToken, AATokenString),
	sublist(InputStringIn, AATokenString, PrefixLength, TextLength),
	NumCharsConsumed is PrefixLength + TextLength,
	% Don't allow skipping too far forward in input string,
	% unless we're at the very beginning of the citation!
	limit_forward_skip(AAToken, PrefixLength),
	append_length(InputString1, InputStringIn, NumCharsConsumed),
	% append(AATokenString, InputString1, InputStringIn),
	remove_whitespace_chars(InputString1, 0, InputStringNext, _NumBlanksRemoved),
	consume_aa_token_strings(RestAATokens, NextAAToken, InputStringNext, InputStringOut).

limit_forward_skip(AAToken, PrefixLength) :-
	% AAToken = tok(_TokenType, _String, _LCString, pos(StartPos,_EndPos)),
	token_template(AAToken, _TokenType, _String, _LCString, pos(StartPos,_EndPos)),
	% If StartPos < 500, we're probably somwhere in the metadata,
	% so don't enforce a 10-character limit on PrefixLength.
	( StartPos < 500 ->
	  true
	; PrefixLength < 10
	).

remove_whitespace_chars([H|T], NumBlanksRemovedIn,
			CharsWithNoWhiteSpace, NumBlanksRemovedOut) :-
	( ws_char(H) ->
	  NumBlanksRemovedNext is NumBlanksRemovedIn + 1,
	  remove_whitespace_chars(T, NumBlanksRemovedNext,
	  			  CharsWithNoWhiteSpace, NumBlanksRemovedOut)
        ; CharsWithNoWhiteSpace = [H|T],
	  NumBlanksRemovedOut is NumBlanksRemovedIn
        ).

consume_tokens_through_ex_rbracket([FirstToken|RestTokens], InputStringIn,  PosIn,
				   NewTokenList, InputStringOut, PosOut) :-
	% Simply skip over the pe and aa tokens
	( higher_order_or_annotation_tok(FirstToken) ->
	  InputStringNext = InputStringIn,
	  PosNext = PosIn
	% Consume regular token
	; remove_untokenized_whitespace([FirstToken|RestTokens], InputStringIn,
	  				InputString1, NumBlanksRemoved1),
	  arg(2, FirstToken, CharString),
	  append(CharString, InputStringTemp, InputString1),
	  length(CharString, CharStringLength),
	  PosTemp is PosIn + CharStringLength,
	  % This is in case the acronym occurs at the beginning of a line
	  % in a citation -- after that damn whitespace that doesn't get tokenized!
	  remove_untokenized_whitespace(RestTokens, InputStringTemp,
	  				InputStringNext, NumBlanksRemoved2),
	  PosNext is PosTemp + NumBlanksRemoved1 + NumBlanksRemoved2
	),
	% Right-bracket token (")" or "]")
	( ex_rbracket_tok(FirstToken) ->
	  NewTokenList = RestTokens,
	  InputStringOut = InputStringNext,
	  PosOut is PosNext
	; consume_tokens_through_ex_rbracket(RestTokens,   InputStringNext, PosNext,
					     NewTokenList, InputStringOut,  PosOut)
	).

% remove_and_consume_ws_tokens([FirstToken|TokensIn], InputStringIn, TokensOut, InputStringOut) :-
% 	( ws_tok(FirstToken) ->
% 	  arg(2, FirstToken, WhiteSpace),
% 	  append(WhiteSpace, InputStringNext, InputStringIn),
% 	  remove_and_consume_ws_tokens(TokensIn, InputStringNext, TokensOut, InputStringOut)
% 	; TokensOut = [FirstToken|TokensIn],
% 	  InputStringOut = InputStringIn
% 	).

% There are pathological cases in which an aa token intervenes
% between an aadef token and the individual expansion tokens.

matching_tokens(FirstAAToken, NextToken, RestTokensIn1, RestTokensIn2) :-
	( FirstAAToken == NextToken ->
	  RestTokensIn2 = RestTokensIn1
	  % RestTokensIn1 = [WSToken|RestTokensInTemp],
	  % ( ws_tok(WSToken) ->
	  %   RestTokensIn2 = RestTokensInTemp
          % ; RestTokensIn2 = RestTokensIn1
	  % )
	; aa_tok(NextToken),
	  arg(2, NextToken, [FirstAAToken|_]),
	  RestTokensIn1 = [FirstAAToken|RestTokensIn2]
	).

remove_tokens_no_consume([], RestTokens, RestTokens).
remove_tokens_no_consume([FirstAADefToken|RestAADefTokens],
			 [FirstToken|RestTokensIn], RestTokensOut) :-
	( FirstAADefToken == FirstToken ->
	  remove_tokens_no_consume(RestAADefTokens, RestTokensIn, RestTokensOut)
	; aa_tok(FirstToken)->
	  arg(2, FirstToken, [FirstAAToken|RestAATokens]),
	  FirstAAToken == FirstAADefToken,
	  remove_tokens_no_consume([FirstAAToken|RestAATokens], RestTokensIn, _RestTokensOut1),
	  % NextToken is the simple token that matches the first AA token
	  % or an AA token 
	  RestTokensIn = [NextToken|RestTokensIn1],
	  matching_tokens(FirstAAToken, NextToken, RestTokensIn1, RestTokensIn2),
	  remove_tokens_no_consume(RestAADefTokens, RestTokensIn2, RestTokensOut)
	; ws_tok(FirstToken),
	  \+ ws_tok(FirstAADefToken) ->
	  remove_tokens_no_consume([FirstAADefToken|RestAADefTokens], RestTokensIn, RestTokensOut)
	; RestAADefTokens == [],
	  punc_tok(FirstAADefToken) ->
	  RestTokensOut = [FirstToken|RestTokensIn]
	).


consume_tokens_up_to_ws_tok([FirstToken|RestTokens], PrevTokenType,
			    PosIn,  InputStringIn, NewTokensIn,
			    PosOut, InputStringOut, NewTokensOut) :-
	( ws_tok(FirstToken) ->
	  PosOut is PosIn,
	  InputStringOut = InputStringIn,
	  NewTokensIn = NewTokensOut
	; add_raw_pos_info_2(FirstToken, aadef, PrevTokenType, PosIn, InputStringIn, 
			     NewToken,   PosNext, InputStringNext),
	  NewTokensIn = [NewToken|NewTokensNext],
	  consume_tokens_up_to_ws_tok(RestTokens, PrevTokenType,
				      PosNext,  InputStringNext, NewTokensNext,
				      PosOut, InputStringOut, NewTokensOut)
	).


% Create new tokens from the tokens in arg1
% and remove their corresponding strings from the InputString

% For really pathological cases such as "heart attack" (HA)
% (when the quotation marks appear in the text!!),
% we have to create new tokens for tokens occurring after the end
% of the AADef tokens and before the next whitespace token. GROSS.
create_new_tokens_and_consume_strings([], PrevTokenType, RestTokens, PosIn,
				      InputStringIn, NewTokensIn, NewTokensOut, PosOut,
				      InputStringOut) :-
	consume_tokens_up_to_ws_tok(RestTokens, PrevTokenType,
				    PosIn,  InputStringIn,  NewTokensIn,
				    PosOut, InputStringOut, NewTokensOut).
	
create_new_tokens_and_consume_strings([FirstAADefToken|RestAADefTokens], PrevTokenType,
				      [NextToken|RestTokensIn],
				      PosIn, InputStringIn,
				      NewTokensIn, NewTokensOut, PosOut,
				      InputStringOut) :-
	( FirstAADefToken == NextToken ->
	  add_raw_pos_info_2(FirstAADefToken, aadef, PrevTokenType, PosIn,   InputStringIn, 
			     NewToken,   PosNext, InputStringNext),
	  NewTokensIn = [NewToken|NewTokensNext],
	  RemainingAADefTokens = RestAADefTokens,
	  RestTokensNext = RestTokensIn
	; RestAADefTokens == [],
	  punc_tok(FirstAADefToken) ->
	  RemainingAADefTokens = [],
	  PosNext = PosIn,
	  InputStringNext = InputStringIn,
	  NewTokensNext = NewTokensIn,
	  RestTokensNext = RestTokensIn
	  % If an aa token appears in the expansion of an aadef token,
	  % then the fun begins...
	; aa_tok(NextToken),
	  arg(2, NextToken, AATokens),
	  % Ensure that the first FirstAADefToken is also the first AA token;
	  % see the example below!
	  AATokens = [FirstAAToken|RestAATokens],
	  FirstAAToken == FirstAADefToken,
	  % Can't simply append here, because RestAADefTokens can contain ws tokens
	  % that aren't in RestAATokens. GRRR.
	  match_aa_tokens(RestAATokens, RestAADefTokens, LeftOverAADefTokens),
	  match_aa_tokens(AATokens, RestTokensIn, RestTokensNext),
	  arg(3, NextToken, [AADefSubToken]),
	  arg(2, AADefSubToken, ExpansionTokens),
	  consume_strings(AATokens, 1, FirstPrefixLength, InputStringIn, InputStringNext),
	  % Can't just use PosIn here if the abstract begins with an aadef/aa,
	  % as in the example below, because PosIn will still be at the end of the title!
	  arg(4, FirstAAToken, pos(AAStartPos,_)),
	  % Not sure why the +1 was in there. I removed it because of Pawel's bug.
	  % RealPosIn is PosIn + FirstPrefixLength + 1,
	  RealPosIn is PosIn + FirstPrefixLength,
	  token_sequence_length(RestAATokens, FirstAAToken, AAStartPos, AATokensLength),
	  create_new_tokens(ExpansionTokens, RealPosIn, AATokensLength, NewTokensIn, NewTokensNext),
	  PosNext is RealPosIn + AATokensLength,
	  RemainingAADefTokens = LeftOverAADefTokens
	),
	create_new_tokens_and_consume_strings(RemainingAADefTokens, PrevTokenType, RestTokensNext,
					      PosNext, InputStringNext,
					      NewTokensNext, NewTokensOut, PosOut,
					      InputStringOut).

%%% The code above matches the tokens in the aadef (TGF ... receptor)
%%% with the following tokens. Things get complicated when the next token
%%% is an AA tokens, and even more complicated when 2 consecutive AA tokens follow.

%%% match_aa_tokens/3 is called twice when the token after the AADef is an AA.
%%% 
%%% The first call to match_aa_tokens/3 matches
%%% (1) the tail of the first arg of the first AA token (beta, 1) and
%%% (2) the tail of AADef tokens (ws, beta, ... receptor).
%%% match_aa_tokens/3 is clever enough to ignore ws tokens in its second arg,
%%% so it matches the (beta, 1) tokens in arg 1 with (ws, beta, ws, 1) in arg 2.
%%% The head of both these lists is explicitly matched by
%%% 	  FirstAAToken = FirstAADefToken,
%%% The second call to match_aa_tokens/3 matches
%%% (1) all the AA tokens (TGF, beta, 1) and
%%% (2) the remaining tokens, beginning with the second AA token.

%%% Because the first remaining token is another AA,
%%% we call match_aa_tokens_1.

%%% AADef1: transforming growth factor beta 1 (TGF beta 1)
%%% AADef2: Transforming Growth Factor beta (TGF beta)
%%% AADef3: TGF beta 1 and its receptor (TGF beta RII)
%%% leads to the following token sequence:
%%% tok(aadef,
%%%     [tok(uc,'TGF',tgf,pos(1094,1097)),
%%%      tok(ws,' ',' ',pos(1097,1098)),     <--- tail of the AADef tokens
%%%      tok(lc,beta,beta,pos(1098,1102)),
%%%      tok(ws,' ',' ',pos(1102,1103)),
%%%      tok(nu,'1','1',pos(1103,1104)),
%%%      tok(ws,' ',' ',pos(1104,1105)),
%%%      tok(lc,and,and,pos(1105,1108)),
%%%      tok(ws,' ',' ',pos(1108,1109)),
%%%      tok(lc,its,its,pos(1109,1112)),
%%%      tok(ws,' ',' ',pos(1112,1113)),
%%%      tok(lc,receptor,receptor,pos(1113,1121))],
%%%     [tok(uc,'TGF',tgf,pos(1123,1126)),
%%%      tok(lc,beta,beta,pos(1127,1131)),
%%%      tok(uc,'RII',rii,pos(1132,1135))],
%%%     pos(1094,1121))
%%% tok(aa,
%%%     [tok(uc,'TGF',tgf,pos(1094,1097)),
%%%      tok(lc,beta,beta,pos(1098,1102)),   <--- tail of first arg
%%%      tok(nu,'1','1',pos(1103,1104))],         of the first AA token
%%%     [tok(aadef,
%%%         [tok(lc,transforming,transforming,pos(8,20)),
%%%          tok(ws,' ',' ',pos(20,21)),
%%%          tok(lc,growth,growth,pos(21,27)),
%%%          tok(ws,' ',' ',pos(27,28)),
%%%          tok(lc,factor,factor,pos(28,34)),
%%%          tok(ws,' ',' ',pos(34,35)),
%%%          tok(lc,beta,beta,pos(35,39)),
%%%          tok(ws,' ',' ',pos(39,40)),
%%%          tok(nu,'1','1',pos(40,41))],
%%%         [tok(uc,'TGF',tgf,pos(43,46)),
%%%          tok(lc,beta,beta,pos(47,51)),
%%%          tok(nu,'1','1',pos(52,53))],
%%%         pos(8,41))],
%%%     pos(1094,1104))
%%% tok(aa,                                  <--- the second AA token
%%%     [tok(uc,'TGF',tgf,pos(1094,1097)),
%%%      tok(lc,beta,beta,pos(1098,1102))],
%%%     [tok(aadef,
%%%         [tok(ic,'Transforming',transforming,pos(685,697)),
%%%          tok(ws,' ',' ',pos(697,698)),
%%%          tok(ic,'Growth',growth,pos(698,704)),
%%%          tok(ws,' ',' ',pos(704,705)),
%%%          tok(ic,'Factor',factor,pos(705,711)),
%%%          tok(ws,' ',' ',pos(711,712)),
%%%          tok(lc,beta,beta,pos(712,716))],
%%%         [tok(uc,'TGF',tgf,pos(718,721)),
%%%          tok(lc,beta,beta,pos(722,726))],
%%%         pos(685,716))],
%%%     pos(1094,1102))
%%% tok(uc,'TGF',tgf,pos(1094,1097))
%%% tok(ws,' ',' ',pos(1097,1098))
%%% tok(lc,beta,beta,pos(1098,1102))
%%% tok(ws,' ',' ',pos(1102,1103))
%%% tok(nu,'1','1',pos(1103,1104))

match_aa_tokens(_, [], []) :- !.
match_aa_tokens([], RestTokens, RestTokens).
match_aa_tokens([FirstAAToken|RestAATokens], [FirstToken|RestTokens], RestTokensNext) :-
	( FirstAAToken == FirstToken ->
	  match_aa_tokens(RestAATokens, RestTokens, RestTokensNext)
	; ws_tok(FirstToken) ->
	  match_aa_tokens([FirstAAToken|RestAATokens],
			  RestTokens, RestTokensNext)
	; aa_tok(FirstToken) ->
	  arg(2, FirstToken, AATokens2),
	  match_aa_tokens_1(AATokens2, RestTokens,
			    [FirstAAToken|RestAATokens],
			    RemainingTokens, RemainingAATokens),
	  match_aa_tokens(RemainingAATokens, RemainingTokens, RestTokensNext)			    
	).

% [FirstAAToken2|RestAATokens2] are the AA tokens from the second AA
% [FirstToken|RestTokens] are the rest of the top-level tokens
% [FirstAAToken1|RestAATokens1] are the AA tokens from the first AA
match_aa_tokens_1([], RemainingTokens, RemainingAATokens, RemainingTokens, RemainingAATokens).
match_aa_tokens_1([FirstAAToken2|RestAATokens2],
		  [FirstToken|RestTokens],
		  [FirstAAToken1|RestAATokens1],
		  RemainingTokens, RemainingAATokens) :-
	( ws_tok(FirstToken) ->
	  match_aa_tokens_1([FirstAAToken2|RestAATokens2],
	  		    RestTokens,
			    [FirstAAToken1|RestAATokens1],
			    RemainingTokens, RemainingAATokens)
	; FirstAAToken1 == FirstToken,
	  FirstAAToken2 == FirstToken,
	  match_aa_tokens_1(RestAATokens2, RestTokens, RestAATokens1,
	  		    RemainingTokens, RemainingAATokens)
	).

consume_strings([], _TokenIndex, _FirstPrefixLength, InputString, InputString).
consume_strings([FirstToken|RestTokens], TokenIndex, FirstPrefixLength,
		InputStringIn, InputStringOut) :-
	arg(2, FirstToken, TokenString),
	remove_whitespace_chars(InputStringIn, 0, InputStringTemp, _NumBlanksRemoved),
	% Must change this call to append to a call to substring
	% append(TokenString, InputStringNext, InputStringTemp),
	sublist(InputStringTemp, TokenString, PrefixLength, TextLength),
	NumCharsConsumed is PrefixLength + TextLength,
	set_first_prefix_length(TokenIndex, PrefixLength, FirstPrefixLength),
	append_length(InputStringNext, InputStringTemp, NumCharsConsumed),
	NextTokenIndex is TokenIndex + 1,
	consume_strings(RestTokens, NextTokenIndex, FirstPrefixLength,
			InputStringNext, InputStringOut).

set_first_prefix_length(TokenIndex, PrefixLength, FirstPrefixLength) :-
	( TokenIndex =:= 1 ->
	  FirstPrefixLength is PrefixLength
	; true
	).

create_new_tokens([], _StartPos, _EndPos, NewTokens, NewTokens).
create_new_tokens([FirstToken|RestTokens], StartPos, EndPos,
		  [NewFirstToken|NewTokensNext], NewTokensOut) :-
	% FirstToken = tok(Type, TokenString, LCTokenString, Pos),
	token_template(FirstToken, Type, TokenString, LCTokenString, Pos),
	% NewFirstToken = tok(Type, TokenString, LCTokenString, Pos, pos(StartPos,EndPos)),
	token_template(NewFirstToken, Type, TokenString, LCTokenString, Pos, pos(StartPos,EndPos)),
	create_new_tokens(RestTokens, StartPos, EndPos, NewTokensNext, NewTokensOut).

% Test if we are in a sentence (i.e., if a alphanumeric/punctuation token
% has been encountered since the most recent sn token).
% If not, I don't care how many characters have been skipped.
% Otherwise, ensure that we haven't skipped > 10 spaces.
% This test deals with the off chance that words like "PMID" or "MEDLINE"
% (typically found in the metadata before the title/abstract)
% would appear in the actual text and mess up the character positions.
test_for_adjacency(TokenState, NewToken, CurrentPos) :-
	( TokenState =:= 0 ->
	  true
	; arg(5, NewToken, pos(StartPos, _StringLength)),
	  StartPos < CurrentPos + 5
        ).

% The token state is initialized to 0, meaning that we are NOT in a sentence.
% The state remains at 0 when a higher-order or annotation token (i.e., field,
% label, sn, pe, aa, aadef) found, but flips to 1 if a char-consuming token
% (i.e., an, ic, lc, mc, nu, pn, uc, ws), is found, because that means
% we've started a new sentence.
get_next_token_state(0, TokenType, NextTokenState) :-
	( higher_order_or_annotation_type(TokenType) ->
	  NextTokenState is 0
        ; NextTokenState is 1
         ).
% The token state flips from 1 (meaning that we are currently in a sentence)
% to 0 when an sn token is found, because that means the current sentence has ended.
% Otherwise a token state of 1 remains at 1.
get_next_token_state(1, TokenType, NextTokenState) :-
	( TokenType == sn ->
	  NextTokenState is 0
        ; NextTokenState is 1
        ).

% For tokens that consume characters (i.e., an, ic, lc, mc, nu, pn, uc, ws).
% CurrentTokenType is the type of token that is being handled when
% add_raw_pos_info_2/8 is called. Right now, it's only an (alphanumeric) or aadef.
add_raw_pos_info_2(CurrentToken, CurrentTokenType, PrevTokenType,
		   CurrentPos, InputString, 
		   NewToken,  NextPos,    RestInputString) :-
	% CurrentToken = tok(TokenType, Text, LCText, CurrentPosTerm),
	token_template(CurrentToken, TokenType, Text, LCText, CurrentPosTerm),
	% Text is a substring of InputString
	% There are PrefixLength chars in InputString before Text
	% TextLength is the length of Text
	sublist(InputString, Text, PrefixLength, TextLength),
	( CurrentTokenType == aadef,
	  PrevTokenType \== sn ->
	  PrefixLength < 20
	; true
	),
	% The call to sublist/4 must be backtrackable
	% in order to allow the token strings to match
	% multiple places in the text string!!
	% !,
	% proper_prefix_length(InputString, _Prefix, PrefixLength),
	% format(user_output, 'SKIPPED:>~q<~n', [Prefix]),
	% StartPos is the starting position of Text in InputString
	StartPos is PrefixLength + CurrentPos,	
	create_new_pos_term(CurrentPosTerm, StartPos, NewPosTerm),
	% NewToken  = tok(TokenType, Text, LCText, CurrentPosTerm, NewPosTerm),
	token_template(NewToken, TokenType, Text, LCText, CurrentPosTerm, NewPosTerm),
	% NumCharsConsumed is the number of chars to lop off
	% the beginning of InputString to get RestInputString
	NumCharsConsumed is PrefixLength + TextLength,
	NextPos is CurrentPos + PrefixLength + TextLength,
	append_length(RestInputString, InputString, NumCharsConsumed).


% I want to handle tokens from both 
% Sentences, which have only one pos(X,Y) term, e.g.,
% tok(field,"TX","tx",pos(0,12))
% tok(label,"00000000.tx.1","00000000.tx.1",pos(0,12))
% tok(sn,[],0,pos(0,12))
% and CoordSentences, which have two pos(X,Y) terms, e.g.,
% tok(field,"TX","tx",pos(0,12),pos(0,12))
% tok(label,"00000000.tx.1","00000000.tx.1",pos(0,12),pos(0,12))
% tok(sn,[],0,pos(0,12),pos(0,12))

% Regardless of whether the token has one or two pos(X,Y) terms,
% we add another one to represent the position in the raw text.
% add_raw_pos_info_1 is used for ws, sn, and higher-order and annotation types
% (field, label, sn, pe, aa, and aadef).

add_raw_pos_info_1(OrigToken, CurrentPos, NewToken) :-
	% OrigToken = tok(TokenType, Text, LCText, OrigPosTerm),
	token_template(OrigToken, TokenType, Text, LCText, OrigPosTerm),
	create_new_pos_term(OrigPosTerm, CurrentPos, NewPosTerm2),
	% NewToken  = tok(TokenType, Text, LCText, OrigPosTerm, NewPosTerm2).
	token_template(NewToken, TokenType, Text, LCText, OrigPosTerm, NewPosTerm2).

create_new_pos_term(pos(X1,Y1), CurrentPos, pos(CurrentPos, TextLength)) :-
	TextLength is Y1 - X1.

% add_pos_term_1([pos(X2,Y2)], pos(X1,Y1), CurrentPos,
% 	     pos(X1,Y1), pos(X2,Y2), pos(CurrentPos, TextLength)) :-
% 	TextLength is Y1 - X1.

% For alphanumeric tokens (an, ic, lc, mc, nu, uc)
% add_pos_term_2(pos(X1,Y1), CurrentPos, pos(CurrentPos, TextLength)).
% 	TextLength is Y1 - X1.

% add_pos_term_2([pos(X2,Y2)], pos(X1,Y1), CurrentPos, TextLength,
% 	       pos(X1,Y1), pos(X2,Y2), pos(CurrentPos, TextLength)).

% If we encounter an AA at the very beginning of a citation,
% we need to index into the text to find the real starting position.

%%% adjust_start_and_end_pos(_InputStringIn, CurrentPos,
%%% 			 AAStartPos, AAEndPos,
%%% 			 _AATokenList,
%%% 			 CurrentPos, AAStartPos, AAEndPos) :- !.
%%% 
%%% adjust_start_and_end_pos(InputStringIn, TempCurrentPos,
%%% 			 TempAAStartPos, TempAAEndPos,
%%% 			 AATokenList,
%%% 			 CurrentPos, AAStartPos, AAEndPos) :-
%%% 	( TempCurrentPos > 0 ->
%%% 	  AAStartPos is TempAAStartPos,
%%% 	  AAEndPos is TempAAEndPos
%%% 	; AATokenList = [FirstAAToken|_],
%%% 	  FirstAAToken = tok(_TokenType,TokenText,_LCText,_PosInfo),
%%% 	  sublist(InputStringIn, TokenText, PrefixLength, _TextLength),
%%% 	  AAStartPos is TempAAStartPos + PrefixLength,
%%% 	  CurrentPos is TempCurrentPos + PrefixLength,
%%% 	  AAEndPos is TempAAEndPos + PrefixLength
%%% 	).
