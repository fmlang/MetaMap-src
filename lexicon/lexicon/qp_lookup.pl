
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

/* qp_lookup.pl - lexical lookup predicates.
*/

:- module(qp_lookup, [
	assembledefns/2
   ]).

:- use_module(lexicon(qp_lexicon), [
	lex_form_ci_recs_input_5/5,
	default_lexicon_file/1,
	default_index_file/1
   ]).

:- use_module(lexicon(qp_shapes), [
	shapes/3
   ]).

:- use_module(skr_lib(ctypes), [
	is_alpha/1,
	is_punct/1
   ]).

:- use_module(library(lists), [
	append/2
   ]).


%%% top level call to lexical lookup from parser
assembledefns(Input, Recs) :-
	default_lexicon_file(Lexicon),
	default_index_file(Index),
	assembledefns_4(Input, Recs, Lexicon, Index).

assembledefns_4([], [], _Lexicon, _Index).
assembledefns_4([F|R], AllRecs, Lexicon, Index) :-
	assembledefns_aux(F, '', SomeRecs, [], Lexicon, Index),
	append(SomeRecs, MoreRecs, AllRecs),
	assembledefns_4(R, MoreRecs, Lexicon, Index).

%%% returns matching records in the form: lexicon:[lexmatch:X, inputmatch:Y, records:Z]
%%% where X is the "best" lexical item that matched, Y is a list of tokens from the
%%% input, and Z is a list of records of the form: lexrec:[base:B, ...etc]
assembledefns_aux([], _PrevToken, [], [], _Lexicon, _Index).

%%% from lexicon
assembledefns_aux([Token|MoreTokens], _PreviousToken, [Recs|MoreRecs], Rest, Lexicon, Index) :-
	lex_form_ci_recs_input_5([Token|MoreTokens], Recs, Remaining, Lexicon, Index),
	!,
	assembledefns_aux(Remaining, Token, MoreRecs, Rest, Lexicon, Index).

%%% punctuation
assembledefns_aux([Token|MoreTokens], _PreviousToken, [R|MoreRecs], Rest, Lexicon, Index) :-
	punct_token(Token, R),
	!,
	assembledefns_aux(MoreTokens, Token, MoreRecs, Rest, Lexicon, Index).

%%% from shapes
assembledefns_aux([Token|MoreTokens], _PreviousToken, Recs, Rest, Lexicon, Index) :-
	% shapes(Shapes, PreviousToken, [Token|MoreTokens], Remaining),
	shapes(Shapes, [Token|MoreTokens], Remaining),
	( Shapes = [_|_] ->
	  append(Shapes, MoreRecs, Recs)
	; Recs = [Shapes|MoreRecs]
	),
	!,
	assembledefns_aux(Remaining, Token, MoreRecs, Rest, Lexicon, Index).

%%% unknown token
assembledefns_aux([Token|MoreTokens], _PreviousToken, [R|MoreRecs], Rest, Lexicon, Index) :-
	R = unknown:[inputmatch:[Token]],
	assembledefns_aux(MoreTokens, Token, MoreRecs, Rest, Lexicon, Index).


%%% punctuation records
punct_token(Token, punctuation:[lexmatch:[Token], inputmatch:[Token], records:[punct:PunctName]]) :-
	atom_codes(Token, [C]),
	is_punct(C),
	punct_name(Token, PunctName).

% punct_token('.', punctuation:[lexmatch:['.'], inputmatch:['.'], records:[punct:[period]]]).
% punct_token(',', punctuation:[lexmatch:[','], inputmatch:[','], records:[punct:[comma]]]).
% punct_token(':', punctuation:[lexmatch:[':'], inputmatch:[':'], records:[punct:[colon]]]).
% punct_token(';', punctuation:[lexmatch:[';'], inputmatch:[';'], records:[punct:[semicolon]]]).
% punct_token('(', punctuation:[lexmatch:['('], inputmatch:['('], records:[punct:[lparen]]]).
% punct_token(')', punctuation:[lexmatch:[')'], inputmatch:[')'], records:[punct:[rparen]]]).
% punct_token('[', punctuation:[lexmatch:['['], inputmatch:['['], records:[punct:[lparen]]]).
% punct_token(']', punctuation:[lexmatch:[']'], inputmatch:[']'], records:[punct:[rparen]]]).
% punct_token('/', punctuation:[lexmatch:['/'], inputmatch:['/'], records:[punct:[slash]]]).
% punct_token('?', punctuation:[lexmatch:['?'], inputmatch:['?'], records:[punct:[question]]]).
% punct_token('!', punctuation:[lexmatch:['!'], inputmatch:['!'], records:[punct:[exclaim]]]).
% punct_token('-', punctuation:[lexmatch:['-'], inputmatch:['-'], records:[punct:[dash, hyphen]]]).

punct_name('.', [period])       :- !.
punct_name(',', [comma])        :- !.
punct_name(':', [colon])        :- !.
punct_name(';', [semicolon])    :- !.
punct_name('(', [lparen])       :- !.
punct_name(')', [rparen])       :- !.
punct_name('[', [lparen])       :- !.
punct_name(']', [rparen])       :- !.
punct_name('/', [slash])        :- !.
punct_name('?', [question])     :- !.
punct_name('!', [exclaim])      :- !.
punct_name('-', [dash, hyphen]) :- !.
punct_name(_,   [otherpunct]).

%%%%%% ------------------------

