
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

% File:	    metamap_stop_phrase.pl
% Module:   MetaMap
% Author:   Lan
% Purpose:  Improve efficiency by avoiding fully processing phrases with
%           no candidates

% Source:   NLM TC, OMED TC, NCBI TC and MMI TS

% In USAbase.2014AA, we took the top 125 entries (all those with frequency of at least 10),
% giving us 22512 of the 28616 stop phrases collected, i.e., 78.66%.

% In Full.2012AB, we took the top 379 entries (all those with frequency of at least 10),
% giving us 84383 of the 109291 stop phrases collected, i.e., 77.20%.

% In Full.2012AA, we took the top 228 entries (all those with frequency of at least 10),
% giving us 44685 of the 58176 stop phrases collected, i.e., 76.81%.

% In 2011AA.NLM, we took the top 375 entries (all those with frequency of at least 10),
% giving us 63528 of the 81485 stop phrases collected, i.e., 77.96%.

:- module(metamap_stop_phrase, [
    stop_phrase/2
    ]).

stop_phrase('and', [conj]).
stop_phrase(')', [punc]).
stop_phrase('were', [aux]).
stop_phrase('was', [aux]).
stop_phrase(',', [punc]).
stop_phrase(':', [punc]).
stop_phrase('or', [conj]).
stop_phrase('is', [aux]).
stop_phrase(').', [punc,punc]).
stop_phrase('are', [aux]).
stop_phrase('have', [aux]).
stop_phrase('we', [pron]).
stop_phrase(';', [punc]).
stop_phrase('had', [aux]).
stop_phrase('be', [aux]).
stop_phrase('been', [aux]).
stop_phrase('that', [compl]).
stop_phrase('has', [aux]).
stop_phrase('which', [pron]).
stop_phrase('but', [conj]).
stop_phrase('(', [punc]).
stop_phrase('that', [pron]).
stop_phrase('-', [punc]).
stop_phrase(')-', [punc,punc]).
stop_phrase('it', [pron]).
stop_phrase('when', [conj]).
stop_phrase('as', [conj]).
stop_phrase('d-', [aux]).
stop_phrase('can', [modal]).
stop_phrase('however,', [adv,punc]).
stop_phrase('who', [pron]).
stop_phrase('there', [adv]).
stop_phrase('[', [punc]).
stop_phrase('--', [punc,punc]).
stop_phrase('did', [aux]).
stop_phrase(']', [punc]).
stop_phrase('may', [modal]).
stop_phrase('also', [adv]).
stop_phrase('they', [pron]).
stop_phrase('while', [conj]).
stop_phrase('whether', [conj]).
stop_phrase('although', [conj]).
stop_phrase('whereas', [conj]).
stop_phrase('if', [conj]).
stop_phrase('could', [modal]).
stop_phrase('vs', [conj]).
stop_phrase('j.', [head,punc]).
stop_phrase('versus', [conj]).
stop_phrase('a', [det]).
stop_phrase('(10', [punc,shapes]).
stop_phrase('10', [shapes]).
stop_phrase('as well as', [conj]).
stop_phrase('underwent', [verb]).
stop_phrase('would', [modal]).
stop_phrase('because', [conj]).
stop_phrase('],', [punc,punc]).
stop_phrase('4,', [shapes,punc]).
stop_phrase('having', [aux]).
stop_phrase('this', [pron]).
stop_phrase('does', [aux]).
stop_phrase('and/or', [conj]).
stop_phrase('being', [aux]).
stop_phrase('should', [modal]).
stop_phrase('with', [prep]).
stop_phrase('after', [conj]).
stop_phrase('do', [aux]).
stop_phrase('since', [conj]).
stop_phrase('a.,', [det,punc,punc]).
stop_phrase('and -', [conj,punc]).
stop_phrase('will', [modal]).
stop_phrase('].', [punc,punc]).
stop_phrase('this', [det]).
stop_phrase('[35s', [punc,head]).
stop_phrase('thus,', [adv,punc]).
stop_phrase('(ii', [punc,head]).
stop_phrase('a.', [det,punc]).
stop_phrase('all', [pron]).
stop_phrase('(-4', [punc,punc,shapes]).
stop_phrase('(-6', [punc,punc,shapes]).
stop_phrase('(-7', [punc,punc,shapes]).
stop_phrase('4', [shapes]).
stop_phrase('(6-14', [punc,shapes,punc,shapes]).
stop_phrase('(7', [punc,shapes]).
stop_phrase('might', [modal]).
stop_phrase('the', [det]).
stop_phrase('undergoing', [verb]).
stop_phrase('(4', [punc,shapes]).
stop_phrase('furthermore,', [adv,punc]).
stop_phrase('= 6', [punc,shapes]).
stop_phrase('remained', [verb]).
stop_phrase('even', [adv]).
stop_phrase('s.', [aux]).
stop_phrase('both', [det]).
stop_phrase('here', [adv]).
stop_phrase('nor', [conj]).
stop_phrase('25', [shapes]).
stop_phrase('[14c', [punc,head]).
stop_phrase('cmf', [conj]).
stop_phrase('s-', [aux]).
stop_phrase('therefore,', [adv,punc]).
stop_phrase('(-8', [punc,punc,shapes]).
stop_phrase('(6', [punc,shapes]).
stop_phrase('7,', [shapes,punc]).
stop_phrase('8', [shapes]).
stop_phrase('9,', [shapes,punc]).
stop_phrase('a,', [det,punc]).
stop_phrase('c', [head]).
stop_phrase('d(', [aux]).
stop_phrase('j.,', [head,punc,punc]).
stop_phrase('remains', [verb]).
stop_phrase('still', [adv]).
stop_phrase('(1982', [punc,shapes]).
stop_phrase('(1992', [punc,shapes]).
stop_phrase('(6-13', [punc,shapes,punc,shapes]).
stop_phrase('(c', [punc,head]).
stop_phrase('6,', [shapes,punc]).
stop_phrase('7', [shapes]).
stop_phrase('8,', [shapes,punc]).
stop_phrase('became', [verb]).
stop_phrase('by', [prep]).
stop_phrase('c,', [head,punc]).
stop_phrase('catalyzes', [verb]).
stop_phrase('hypothesized', [verb]).
stop_phrase('must', [modal]).
stop_phrase('than', [prep]).
