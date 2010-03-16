
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

% File:	    metamap_parsing.pl
% Module:   MetaMap
% Author:   Lan
% Purpose:  MetaMap parsing routines


:- module(metamap_parsing,[
	demote_heads/2,
	generate_syntactic_analysis_plus/3,
	generate_syntactic_analysis_plus/4,
	collapse_syntactic_analysis/2
    ]).


:- use_module(lexicon(lex_access),[
	assemble_definitions/2,
	tokenize_string_for_lexical_lookup/2
    ]).

:- use_module(skr_lib(consulttt),[
	consult_tagged_text/5
   ]).

:- use_module(skr_lib(generate_varinfo),[
	generate_variant_info/2
    ]).


:- use_module(skr_lib(mincoman),[
	minimal_commitment_analysis/5
    ]).

:- use_module(skr_lib(retokenize),[
	remove_null_atom_defns/2,
	retokenize/2
    ]).

:- use_module(library(lists), [
	prefix/2
   ]).

:- use_module(library(lists),[
	append/2
    ]).


/* ************************************************************************
   ************************************************************************
   ************************************************************************
                          MetaMap Parsing Predicates
   ************************************************************************
   ************************************************************************
   ************************************************************************ */

generate_syntactic_analysis_plus(ListOfAscii, SyntAnalysis, Definitions) :-
	once(tokenize_string_for_lexical_lookup(ListOfAscii, Words0)),
	retokenize(Words0, Words),
	assemble_definitions(Words, Definitions0),
	remove_null_atom_defns(Definitions0, Definitions),
	once(generate_variant_info(Definitions, VarInfoList)),
	minimal_commitment_analysis(notag, Definitions, VarInfoList,
				    _LabeledText, SyntAnalysis).

generate_syntactic_analysis_plus(ListOfAscii, TempTagList, SyntAnalysis, Definitions) :-
	once(tokenize_string_for_lexical_lookup(ListOfAscii, Words0)),
	retokenize(Words0, Words),
	assemble_definitions(Words, Definitions0),
	remove_null_atom_defns(Definitions0, Definitions),
	once(generate_variant_info(Definitions, VarInfoList)),
	% ChromosomeFound is 0,
	% LeftOverWords = [],
	% PrevTagWord = '',
	% update_taglist(TempTagList, Definitions, PrevTagWord,
	% 	       ChromosomeFound, LeftOverWords, TagList),
	TagList = TempTagList,
	consult_tagged_text(Definitions, VarInfoList, TagList, LabeledText, 1),
	minimal_commitment_analysis(tag, Definitions, VarInfoList, LabeledText, SyntAnalysis).

/* collapse_syntactic_analysis(+SyntAnalysis, -CollapsedSyntAnalysis)

collapse_syntactic_analysis/2 collapses all phrases of SyntAnalysis into
one, the head being the head of the first phrase.  */

collapse_syntactic_analysis(minimal_syntax(Syntax), minimal_syntax([CollapsedSyntax])) :-
        collapse_syntactic_analysis(Syntax, CollapsedSyntax).
collapse_syntactic_analysis([], []).
collapse_syntactic_analysis([First|Rest], CollapsedSyntAnalysis) :-
	demote_all_heads(Rest, DemotedRest),
	append([First|DemotedRest], CollapsedSyntAnalysis).

demote_all_heads([], []).
demote_all_heads([First|Rest], [DemotedFirst|DemotedRest]) :-
	demote_heads(First, DemotedFirst),
	demote_all_heads(Rest, DemotedRest).

demote_heads([], []).
demote_heads([First|Rest], [NewFirst|DemotedRest]) :-
	( functor(First, head, 1) ->
	  arg(1, First, Arg),
	  functor(NewFirst, mod, 1),
  	  arg(1, NewFirst, Arg)
	; NewFirst = First
	),
	demote_heads(Rest, DemotedRest).

% This is less efficient!
% demote_heads([head(X)|Rest], [mod(X)|DemotedRest]) :-
% 	!,
% 	demote_heads(Rest, DemotedRest).
% demote_heads([First|Rest], [First|DemotedRest]) :-
% 	demote_heads(Rest, DemotedRest).
