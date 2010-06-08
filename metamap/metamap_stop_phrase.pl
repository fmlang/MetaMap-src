
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
% For 2010, we took the top 1779 entries (all those with frequency of at least 10),
% giving us 408,932 of the 472,066 stop phrases collected, i.e., 83.51%

:- module(metamap_stop_phrase, [
    stop_phrase/2
    ]).

stop_phrase('and', [conj]).
stop_phrase('was', [aux]).
stop_phrase(')', [punc]).
stop_phrase('were', [aux]).
stop_phrase('is', [aux]).
stop_phrase('or', [conj]).
stop_phrase('to', [adv]).
stop_phrase('be', [aux]).
stop_phrase('are', [aux]).
stop_phrase(',', [punc]).
stop_phrase('that', [compl]).
stop_phrase(':', [punc]).
stop_phrase('have', [aux]).
stop_phrase(').', [punc,punc]).
stop_phrase('we', [pron]).
stop_phrase('but', [conj]).
stop_phrase('had', [aux]).
stop_phrase('been', [aux]).
stop_phrase('which', [pron]).
stop_phrase('may', [modal]).
stop_phrase('has', [aux]).
stop_phrase('both', [conj]).
stop_phrase(';', [punc]).
stop_phrase('it', [pron]).
stop_phrase('that', [pron]).
stop_phrase('when', [conj]).
stop_phrase('can', [modal]).
stop_phrase('as', [conj]).
stop_phrase('however,', [adv,punc]).
stop_phrase('did', [aux]).
stop_phrase('also', [adv]).
stop_phrase('(', [punc]).
stop_phrase('there', [adv]).
stop_phrase('who', [pron]).
stop_phrase('could', [modal]).
stop_phrase('than', [prep]).
stop_phrase('either', [conj]).
stop_phrase('although', [conj]).
stop_phrase('while', [conj]).
stop_phrase('-', [punc]).
stop_phrase('of', [prep]).
stop_phrase('they', [pron]).
stop_phrase('in', [prep]).
stop_phrase('whereas', [conj]).
stop_phrase('(p less', [punc,head,adv]).
stop_phrase('(p', [punc,head]).
stop_phrase('as well as', [conj]).
stop_phrase('should', [modal]).
stop_phrase('''s', [aux]).
stop_phrase('produced', [verb]).
stop_phrase(')-', [punc,punc]).
stop_phrase('expressed', [verb]).
stop_phrase('significantly', [adv]).
stop_phrase('by', [prep]).
stop_phrase('only', [adv]).
stop_phrase('with', [prep]).
stop_phrase('does', [aux]).
stop_phrase('appears', [verb]).
stop_phrase('(n', [punc,head]).
stop_phrase('being', [aux]).
stop_phrase('this', [pron]).
stop_phrase('will', [modal]).
stop_phrase('since', [conj]).
stop_phrase('thus,', [adv,punc]).
stop_phrase('--', [punc,punc]).
stop_phrase('for', [prep]).
stop_phrase('might', [modal]).
stop_phrase('all', [pron]).
stop_phrase('after', [conj]).
stop_phrase('and/or', [conj]).
stop_phrase('in vitro', [prep,head]).
stop_phrase('do', [aux]).
stop_phrase('would', [modal]).
stop_phrase('appear', [verb]).
stop_phrase('furthermore,', [adv,punc]).
stop_phrase('more', [adv]).
stop_phrase('located', [verb]).
stop_phrase('versus', [conj]).
stop_phrase('to', [prep]).
stop_phrase('previously', [adv]).
stop_phrase('having', [aux]).
stop_phrase('to that', [prep,pron]).
stop_phrase('appeared', [verb]).
stop_phrase('produce', [verb]).
stop_phrase('(1', [punc,shapes]).
stop_phrase('because', [conj]).
stop_phrase('nor', [conj]).
stop_phrase('thus', [adv]).
stop_phrase('discussed', [verb]).
stop_phrase('whether', [compl]).
stop_phrase('if', [conj]).
stop_phrase('a', [det]).
stop_phrase('2', [shapes]).
stop_phrase('this', [det]).
stop_phrase('here', [adv]).
stop_phrase('p', [head]).
stop_phrase('less', [adv]).
stop_phrase('in vivo', [prep,head]).
stop_phrase('achieved', [verb]).
stop_phrase('therefore,', [adv,punc]).
stop_phrase('must', [modal]).
stop_phrase('in which', [prep,pron]).
stop_phrase('those', [det]).
stop_phrase('1', [shapes]).
stop_phrase('than 0.05', [prep,shapes]).
stop_phrase('seems', [verb]).
stop_phrase('neither', [conj]).
stop_phrase('some', [pron]).
stop_phrase('differ', [verb]).
stop_phrase('than that', [prep,pron]).
stop_phrase('underwent', [verb]).
stop_phrase('vs', [conj]).
stop_phrase('out', [adv]).
stop_phrase('plus', [conj]).
stop_phrase('even', [adv]).
stop_phrase('than 0.001', [prep,shapes]).
stop_phrase('moreover,', [adv,punc]).
stop_phrase('useful', [head]).
stop_phrase('in vitro.', [prep,head,punc]).
stop_phrase('3', [shapes]).
stop_phrase('each', [pron]).
stop_phrase('p less', [head,adv]).
stop_phrase('rather than', [conj]).
stop_phrase('to those', [prep,det]).
stop_phrase('(2', [punc,shapes]).
stop_phrase('generated', [verb]).
stop_phrase('recognized', [verb]).
stop_phrase('important', [head]).
stop_phrase('m', [head]).
stop_phrase('from', [prep]).
stop_phrase('than 0.01', [prep,shapes]).
stop_phrase('corresponding', [verb]).
stop_phrase('(3', [punc,shapes]).
stop_phrase('of which', [prep,pron]).
stop_phrase('before', [prep]).
stop_phrase('at least', [prep,head]).
stop_phrase('constructed', [verb]).
stop_phrase('express', [verb]).
stop_phrase('most', [pron]).
stop_phrase('and -', [conj,punc]).
stop_phrase('(+', [punc,punc]).
stop_phrase('become', [verb]).
stop_phrase('et', [conj]).
stop_phrase('than those', [prep,pron]).
stop_phrase('4,', [shapes,punc]).
stop_phrase('on', [prep]).
stop_phrase('therefore', [adv]).
stop_phrase('10', [shapes]).
stop_phrase('that,', [compl,punc]).
stop_phrase('became', [verb]).
stop_phrase('especially', [adv]).
stop_phrase('that there', [compl,adv]).
stop_phrase('comparable', [head]).
stop_phrase('influence', [verb]).
stop_phrase('necessary', [head]).
stop_phrase('the', [det]).
stop_phrase('in vivo.', [prep,head,punc]).
stop_phrase('grown', [verb]).
stop_phrase('consisted', [verb]).
stop_phrase('particularly', [adv]).
stop_phrase('discussed.', [verb,punc]).
stop_phrase('if', [compl]).
stop_phrase('1.', [shapes,punc]).
stop_phrase('expressing', [verb]).
stop_phrase('undergoing', [verb]).
stop_phrase('the possibility', [det,head]).
stop_phrase('with those', [prep,det]).
stop_phrase('specifically', [adv]).
stop_phrase('consisting', [verb]).
stop_phrase('with that', [prep,pron]).
stop_phrase('4', [shapes]).
stop_phrase('cannot', [modal]).
stop_phrase('(ii', [punc,head]).
stop_phrase('(4', [punc,shapes]).
stop_phrase('accompanied', [verb]).
stop_phrase('these', [det]).
stop_phrase('2.', [shapes,punc]).
stop_phrase('seem', [verb]).
stop_phrase('though', [conj]).
stop_phrase('(s)', [punc,head]).
stop_phrase('5', [shapes]).
stop_phrase('< 0.05', [punc,shapes]).
stop_phrase('reacted', [verb]).
stop_phrase('mainly', [adv]).
stop_phrase('the influence', [det,head]).
stop_phrase('completely', [adv]).
stop_phrase('at', [prep]).
stop_phrase('composed', [verb]).
stop_phrase('transfected', [verb]).
stop_phrase('conserved', [verb]).
stop_phrase('(10', [punc,shapes]).
stop_phrase('/l', [punc,head]).
stop_phrase('differed', [verb]).
stop_phrase('3.', [shapes,punc]).
stop_phrase('discussed', [pastpart]).
stop_phrase('2,', [shapes,punc]).
stop_phrase('coli', [head]).
stop_phrase('still', [adv]).
stop_phrase('done', [aux]).
stop_phrase('primarily', [adv]).
stop_phrase('; p less', [punc,head,adv]).
stop_phrase('influenced', [verb]).
stop_phrase('tended', [verb]).
stop_phrase('expressed', [pastpart]).
stop_phrase('consists', [verb]).
stop_phrase('of them', [prep,pron]).
stop_phrase('successfully', [adv]).
stop_phrase('usually', [adv]).
stop_phrase('closely', [adv]).
stop_phrase('partially', [adv]).
stop_phrase('the importance', [det,head]).
stop_phrase('(6', [punc,shapes]).
stop_phrase('< 0.001', [punc,shapes]).
stop_phrase('recognize', [verb]).
stop_phrase('abolished', [verb]).
stop_phrase('yielded', [verb]).
stop_phrase('1,', [shapes,punc]).
stop_phrase('from that', [prep,pron]).
stop_phrase('produces', [verb]).
stop_phrase('interact', [verb]).
stop_phrase('than .05', [prep,punc,shapes]).
stop_phrase('3,', [shapes,punc]).
stop_phrase('randomly', [adv]).
stop_phrase('in vitro,', [prep,head,punc]).
stop_phrase('before', [conj]).
stop_phrase('; p', [punc,head]).
stop_phrase('n', [head]).
stop_phrase('subsequently', [adv]).
stop_phrase('undergo', [verb]).
stop_phrase('4.', [shapes,punc]).
stop_phrase('ca(2+', [prep,shapes,punc]).
stop_phrase('despite', [conj]).
stop_phrase('greatly', [adv]).
stop_phrase('in both', [prep,det]).
stop_phrase('(1-29', [punc,shapes,punc,shapes]).
stop_phrase('m.', [head,punc]).
stop_phrase('regarding', [verb]).
stop_phrase('< 0.01', [punc,shapes]).
stop_phrase('exist', [verb]).
stop_phrase('implications', [head]).
stop_phrase('located', [pastpart]).
stop_phrase('implicated', [verb]).
stop_phrase('incorporated', [verb]).
stop_phrase('(b', [punc,head]).
stop_phrase('in e.', [prep,head,punc]).
stop_phrase('a model', [det,head]).
stop_phrase('vs.', [conj,punc]).
stop_phrase('up', [prep]).
stop_phrase('along', [prep]).
stop_phrase('b', [head]).
stop_phrase('where', [pron]).
stop_phrase('with regard', [prep,head]).
stop_phrase('(5', [punc,shapes]).
stop_phrase('he', [pron]).
stop_phrase('react', [verb]).
stop_phrase('from those', [prep,det]).
stop_phrase('similarly,', [adv,punc]).
stop_phrase('than .01', [prep,punc,shapes]).
stop_phrase('utilized', [verb]).
stop_phrase('achieve', [verb]).
stop_phrase('discussed.', [pastpart,punc]).
stop_phrase('generate', [verb]).
stop_phrase('the existence', [det,head]).
stop_phrase('us to', [pron,adv]).
stop_phrase('positively', [adv]).
stop_phrase('so', [prep]).
stop_phrase('thereby', [adv]).
stop_phrase('accumulate', [verb]).
stop_phrase('during', [prep]).
stop_phrase('exists', [verb]).
stop_phrase('many', [head]).
stop_phrase('generally', [adv]).
stop_phrase('transcribed', [verb]).
stop_phrase('grow', [verb]).
stop_phrase('widely', [adv]).
stop_phrase('6', [shapes]).
stop_phrase('also,', [adv,punc]).
stop_phrase('in vivo,', [prep,head,punc]).
stop_phrase('producing', [verb]).
stop_phrase('all', [det]).
stop_phrase('discuss', [verb]).
stop_phrase('ii', [head]).
stop_phrase('we also', [pron,adv]).
stop_phrase('(-5', [punc,punc,shapes]).
stop_phrase('(c', [punc,head]).
stop_phrase('v-', [conj]).
stop_phrase('e.g.', [conj]).
stop_phrase('in all', [prep,pron]).
stop_phrase('simultaneously', [adv]).
stop_phrase('unaffected', [verb]).
stop_phrase('additionally,', [adv,punc]).
stop_phrase('persisted', [verb]).
stop_phrase('rise', [head]).
stop_phrase('"', [punc]).
stop_phrase('8', [shapes]).
stop_phrase('detectable', [head]).
stop_phrase('distinguish', [verb]).
stop_phrase('of whom', [prep,pron]).
stop_phrase('predominantly', [adv]).
stop_phrase('seemed', [verb]).
stop_phrase('(-6', [punc,punc,shapes]).
stop_phrase('5,', [shapes,punc]).
stop_phrase('accumulated', [verb]).
stop_phrase('constitute', [verb]).
stop_phrase('once', [conj]).
stop_phrase('7', [shapes]).
stop_phrase('believe', [verb]).
stop_phrase('necessary to', [mod,adv]).
stop_phrase('pronounced', [verb]).
stop_phrase('t', [head]).
stop_phrase('(less', [punc,adv]).
stop_phrase('/', [punc]).
stop_phrase('of e.', [prep,head,punc]).
stop_phrase('9,', [shapes,punc]).
stop_phrase('al.,', [head,punc,punc]).
stop_phrase('b.', [head,punc]).
stop_phrase('differs', [verb]).
stop_phrase('of these', [prep,det]).
stop_phrase('so', [conj]).
stop_phrase('(-4', [punc,punc,shapes]).
stop_phrase('5.', [shapes,punc]).
stop_phrase('>', [punc]).
stop_phrase('al.', [head,punc]).
stop_phrase('clearly', [adv]).
stop_phrase('phosphorylated', [verb]).
stop_phrase('s.', [head,punc]).
stop_phrase('(7', [punc,shapes]).
stop_phrase('16', [shapes]).
stop_phrase('18', [shapes]).
stop_phrase('e.', [head,punc]).
stop_phrase('efficiently', [adv]).
stop_phrase('them', [pron]).
stop_phrase('/l)', [punc,head]).
stop_phrase('arise', [verb]).
stop_phrase('c', [head]).
stop_phrase('of s.', [prep,head,punc]).
stop_phrase('yield', [verb]).
stop_phrase('ii.', [head,punc]).
stop_phrase('as well as', [conj]).
stop_phrase('confined', [verb]).
stop_phrase('distinct', [head]).
stop_phrase('examination', [head]).
stop_phrase('until', [conj]).
stop_phrase('amplified', [verb]).
stop_phrase('hybridized', [verb]).
stop_phrase('11', [shapes]).
stop_phrase('12', [shapes]).
stop_phrase('<', [punc]).
stop_phrase('a.', [det,punc]).
stop_phrase('corresponds', [verb]).
stop_phrase('she', [pron]).
stop_phrase('utilizing', [verb]).
stop_phrase('(-7', [punc,punc,shapes]).
stop_phrase('(-8', [punc,punc,shapes]).
stop_phrase('immunized', [verb]).
stop_phrase('inactivated', [verb]).
stop_phrase('much', [adv]).
stop_phrase('p.', [head,punc]).
stop_phrase('(1,', [punc,shapes,punc]).
stop_phrase('distinguished', [verb]).
stop_phrase('presumably', [adv]).
stop_phrase('readily', [adv]).
stop_phrase('atp', [head]).
stop_phrase('correspond', [verb]).
stop_phrase('inoculated', [verb]).
stop_phrase('itself', [pron]).
stop_phrase('/2', [punc,shapes]).
stop_phrase('converted', [verb]).
stop_phrase('substituted', [verb]).
stop_phrase('25', [shapes]).
stop_phrase('9', [shapes]).
stop_phrase('an', [det]).
stop_phrase('both', [pron]).
stop_phrase('confer', [verb]).
stop_phrase('m,', [head,punc]).
stop_phrase('selectively', [adv]).
stop_phrase('(-', [punc,punc]).
stop_phrase('(p less', [punc,head,adv]).
stop_phrase('effectively', [adv]).
stop_phrase('of 10', [prep,shapes]).
stop_phrase('of c-fos', [prep,mod,punc,head]).
stop_phrase('elucidate', [verb]).
stop_phrase('influence', [head]).
stop_phrase('nevertheless,', [adv,punc]).
stop_phrase('recognizes', [verb]).
stop_phrase('the latter', [det,head]).
stop_phrase('which,', [pron,punc]).
stop_phrase('disappeared', [verb]).
stop_phrase('here,', [adv,punc]).
stop_phrase('of more', [prep,adv]).
stop_phrase('(-9', [punc,punc,shapes]).
stop_phrase('/dl', [punc,head]).
stop_phrase('accompanied', [pastpart]).
stop_phrase('grew', [verb]).
stop_phrase('of p.', [prep,head,punc]).
stop_phrase('15', [shapes]).
stop_phrase('6,', [shapes,punc]).
stop_phrase('7,', [shapes,punc]).
stop_phrase('already', [adv]).
stop_phrase('another', [pron]).
stop_phrase('becomes', [verb]).
stop_phrase('comprised', [verb]).
stop_phrase('for more', [prep,adv]).
stop_phrase('fully', [adv]).
stop_phrase('how', [compl]).
stop_phrase('those', [pron]).
stop_phrase('= 6', [punc,shapes]).
stop_phrase('clinically', [adv]).
stop_phrase('believed', [verb]).
stop_phrase('clarify', [verb]).
stop_phrase('conversely,', [adv,punc]).
stop_phrase('in all', [prep,det]).
stop_phrase('indistinguishable', [head]).
stop_phrase('interestingly,', [adv,punc]).
stop_phrase('of both', [prep,det]).
stop_phrase('potentiated', [verb]).
stop_phrase('preferentially', [adv]).
stop_phrase('''', [punc]).
stop_phrase('(8', [punc,shapes]).
stop_phrase('(9', [punc,shapes]).
stop_phrase('/l,', [punc,head,punc]).
stop_phrase('13', [shapes]).
stop_phrase('in whom', [prep,pron]).
stop_phrase('than 0.005', [prep,shapes]).
stop_phrase('(-3', [punc,punc,shapes]).
stop_phrase('14', [shapes]).
stop_phrase('20', [shapes]).
stop_phrase('growing', [verb]).
stop_phrase('in any', [prep,pron]).
stop_phrase('independently', [adv]).
stop_phrase('suitable', [head]).
stop_phrase('a,', [det,punc]).
stop_phrase('commonly', [adv]).
stop_phrase('comprising', [verb]).
stop_phrase('imply', [verb]).
stop_phrase('operated', [verb]).
stop_phrase('the fact', [det,head]).
stop_phrase('as well as', [conj]).
stop_phrase('negatively', [adv]).
stop_phrase('of b.', [prep,head,punc]).
stop_phrase('similarly', [adv]).
stop_phrase('such', [det]).
stop_phrase('again', [adv]).
stop_phrase('allocated', [verb]).
stop_phrase('in each', [prep,pron]).
stop_phrase('infused', [verb]).
stop_phrase('quantified', [verb]).
stop_phrase('any', [pron]).
stop_phrase('c-fos', [mod,punc,head]).
stop_phrase('consist', [verb]).
stop_phrase('exceeded', [verb]).
stop_phrase('improvement', [head]).
stop_phrase('interacts', [verb]).
stop_phrase('of these,', [prep,det,punc]).
stop_phrase('without', [prep]).
stop_phrase('(s', [punc,head]).
stop_phrase('implies', [verb]).
stop_phrase('transiently', [adv]).
stop_phrase('c.', [head,punc]).
stop_phrase('corresponded', [verb]).
stop_phrase('interfere', [verb]).
stop_phrase('normally', [adv]).
stop_phrase('reconstituted', [verb]).
stop_phrase('the accumulation', [det,head]).
stop_phrase('the implications', [det,head]).
stop_phrase('< .05', [punc,punc,shapes]).
stop_phrase('accumulation', [head]).
stop_phrase('of less', [prep,adv]).
stop_phrase('of the', [prep,det]).
stop_phrase('than 0.02', [prep,shapes]).
stop_phrase('(2,', [punc,shapes,punc]).
stop_phrase('10,', [shapes,punc]).
stop_phrase('8,', [shapes,punc]).
stop_phrase('almost', [adv]).
stop_phrase('b,', [head,punc]).
stop_phrase('cmf', [conj]).
stop_phrase('coli.', [head,punc]).
stop_phrase('continuously', [adv]).
stop_phrase('explore', [verb]).
stop_phrase('of that', [prep,pron]).
stop_phrase('surprisingly,', [adv,punc]).
stop_phrase('(17', [punc,shapes]).
stop_phrase('100%', [shapes]).
stop_phrase('complementary', [head]).
stop_phrase('emerged', [verb]).
stop_phrase('exclusively', [adv]).
stop_phrase('in those', [prep,det]).
stop_phrase('indicative', [head]).
stop_phrase('trachomatis', [head]).
stop_phrase('(-10', [punc,punc,shapes]).
stop_phrase('dqb1', [head]).
stop_phrase('eluted', [verb]).
stop_phrase('essentially', [adv]).
stop_phrase('in situ', [prep,head]).
stop_phrase('into', [prep]).
stop_phrase('kept', [verb]).
stop_phrase('l.', [head,punc]).
stop_phrase('of 1', [prep,shapes]).
stop_phrase('substantially', [adv]).
stop_phrase('than 0.0001', [prep,shapes]).
stop_phrase('0', [shapes]).
stop_phrase('= 10', [punc,shapes]).
stop_phrase('a.,', [det,punc,punc]).
stop_phrase('antagonized', [verb]).
stop_phrase('ascertained', [verb]).
stop_phrase('composed', [pastpart]).
stop_phrase('existed', [verb]).
stop_phrase('explored', [verb]).
stop_phrase('half', [head]).
stop_phrase('illustrate', [verb]).
stop_phrase('in vitro', [prep,head]).
stop_phrase('linkage', [head]).
stop_phrase('resembles', [verb]).
stop_phrase('what', [pron]).
stop_phrase('(10%', [punc,shapes]).
stop_phrase('14,', [shapes,punc]).
stop_phrase('19', [shapes]).
stop_phrase('21', [shapes]).
stop_phrase('24', [shapes]).
stop_phrase('50%', [shapes]).
stop_phrase('= 5', [punc,shapes]).
stop_phrase('= 8', [punc,shapes]).
stop_phrase('hence,', [adv,punc]).
stop_phrase('out.', [adv,punc]).
stop_phrase('resemble', [verb]).
stop_phrase('resembling', [verb]).
stop_phrase('such', [head]).
stop_phrase('whose', [pron]).
stop_phrase('/- sd', [punc,punc,head]).
stop_phrase('30', [shapes]).
stop_phrase('accordingly,', [adv,punc]).
stop_phrase('after', [prep]).
stop_phrase('confers', [verb]).
stop_phrase('discusses', [verb]).
stop_phrase('intravenously', [adv]).
stop_phrase('it also', [pron,adv]).
stop_phrase('of atp', [prep,head]).
stop_phrase('of m.', [prep,head,punc]).
stop_phrase('reacts', [verb]).
stop_phrase('yielding', [verb]).
stop_phrase('accomplished', [verb]).
stop_phrase('except', [prep]).
stop_phrase('however', [adv]).
stop_phrase('in most', [prep,pron]).
stop_phrase('of all', [prep,det]).
stop_phrase('rendered', [verb]).
stop_phrase('typical', [head]).
stop_phrase('unclear.', [head,punc]).
stop_phrase('5-trisphosphate', [shapes,punc,head]).
stop_phrase('becoming', [verb]).
stop_phrase('consistently', [adv]).
stop_phrase('correctly', [adv]).
stop_phrase('in fact,', [prep,head,punc]).
stop_phrase('in particular,', [prep,head,punc]).
stop_phrase('integrated', [verb]).
stop_phrase('perhaps', [adv]).
stop_phrase('produced', [pastpart]).
stop_phrase('several', [head]).
stop_phrase('the model', [det,head]).
stop_phrase('the usefulness', [det,head]).
stop_phrase('to which', [prep,pron]).
stop_phrase('we therefore', [pron,adv]).
stop_phrase('16,', [shapes,punc]).
stop_phrase('6.', [shapes,punc]).
stop_phrase('< 0.0001', [punc,shapes]).
stop_phrase(']', [punc]).
stop_phrase('assumed', [verb]).
stop_phrase('before', [adv]).
stop_phrase('constructed.', [verb,punc]).
stop_phrase('devoid', [head]).
stop_phrase('of c.', [prep,head,punc]).
stop_phrase('of each', [prep,pron]).
stop_phrase('so', [adv]).
stop_phrase('tend', [verb]).
stop_phrase('unless', [conj]).
stop_phrase('utilize', [verb]).
stop_phrase('22', [shapes]).
stop_phrase('arising', [verb]).
stop_phrase('as that', [prep,pron]).
stop_phrase('brought', [verb]).
stop_phrase('emphasize', [verb]).
stop_phrase('for each', [prep,pron]).
stop_phrase('in vitro.', [prep,head,punc]).
stop_phrase('m.,', [head,punc,punc]).
stop_phrase('resembled', [verb]).
stop_phrase('subsequently,', [adv,punc]).
stop_phrase('than .001', [prep,punc,shapes]).
stop_phrase('yields', [verb]).
stop_phrase('/l.', [punc,head,punc]).
stop_phrase('20%', [shapes]).
stop_phrase('accumulates', [verb]).
stop_phrase('attained', [verb]).
stop_phrase('in this', [prep,det]).
stop_phrase('minus', [conj]).
stop_phrase('no', [det]).
stop_phrase('rather,', [adv,punc]).
stop_phrase('regarded', [verb]).
stop_phrase('safe', [head]).
stop_phrase('sought', [verb]).
stop_phrase('they also', [pron,adv]).
stop_phrase('(1-44', [punc,shapes,punc,shapes]).
stop_phrase('(11', [punc,shapes]).
stop_phrase('= 0.02', [punc,shapes]).
stop_phrase('about', [adv]).
stop_phrase('by which', [prep,pron]).
stop_phrase('dqa1', [head]).
stop_phrase('expresses', [verb]).
stop_phrase('few', [pron]).
stop_phrase('from which', [prep,pron]).
stop_phrase('ii,', [head,punc]).
stop_phrase('in 4', [prep,shapes]).
stop_phrase('in a manner', [prep,det,head]).
stop_phrase('mimic', [verb]).
stop_phrase('nearly', [adv]).
stop_phrase('overcome', [verb]).
stop_phrase('since', [adv]).
stop_phrase('spontaneously', [adv]).
stop_phrase('(1991', [punc,shapes]).
stop_phrase('(50%', [punc,shapes]).
stop_phrase('(ki', [punc,head]).
stop_phrase('50', [shapes]).
stop_phrase('< .01', [punc,punc,shapes]).
stop_phrase('= 1', [punc,shapes]).
stop_phrase('c,', [head,punc]).
stop_phrase('ineffective', [head]).
stop_phrase('influencing', [verb]).
stop_phrase('namely,', [adv,punc]).
stop_phrase('no.', [det,punc]).
stop_phrase('yet', [adv]).
stop_phrase('(20%', [punc,shapes]).
stop_phrase('(33%', [punc,shapes]).
stop_phrase('(6%', [punc,shapes]).
stop_phrase('(iv', [punc,head]).
stop_phrase('*', [punc]).
stop_phrase('17', [shapes]).
stop_phrase('conferred', [verb]).
stop_phrase('generates', [verb]).
stop_phrase('illustrates', [verb]).
stop_phrase('in 5', [prep,shapes]).
stop_phrase('rather', [adv]).
stop_phrase('suggestive', [head]).
stop_phrase('undergoes', [verb]).
stop_phrase('10%', [shapes]).
stop_phrase('2-', [shapes,punc]).
stop_phrase('= 0.03', [punc,shapes]).
stop_phrase('comprises', [verb]).
stop_phrase('comprise', [verb]).
stop_phrase('connected', [verb]).
stop_phrase('construct', [verb]).
stop_phrase('devised', [verb]).
stop_phrase('extensively', [adv]).
stop_phrase('from e.', [prep,head,punc]).
stop_phrase('gradually', [adv]).
stop_phrase('in some', [prep,pron]).
stop_phrase('later', [adv]).
stop_phrase('of all', [prep,pron]).
stop_phrase('opposed', [pastpart]).
stop_phrase('out to', [adv,adv]).
stop_phrase('than 0.05', [prep,shapes]).
stop_phrase('to 10', [prep,shapes]).
stop_phrase('within', [prep]).
stop_phrase('(14', [punc,shapes]).
stop_phrase('(a', [punc,head]).
stop_phrase('= 4', [punc,shapes]).
stop_phrase('as', [prep]).
stop_phrase('believed to', [pastpart,adv]).
stop_phrase('consumed', [verb]).
stop_phrase('influences', [verb]).
stop_phrase('most', [adv]).
stop_phrase('of those', [prep,det]).
stop_phrase('originated', [verb]).
stop_phrase('postulated', [verb]).
stop_phrase('preincubation', [head]).
stop_phrase('progressively', [adv]).
stop_phrase('separately', [adv]).
stop_phrase('significantly less', [adv,adv]).
stop_phrase('ultimately', [adv]).
stop_phrase('(16%', [punc,shapes]).
stop_phrase('(6-14', [punc,shapes,punc,shapes]).
stop_phrase('(7%', [punc,shapes]).
stop_phrase('/d', [punc,head]).
stop_phrase('13,', [shapes,punc]).
stop_phrase('= 0.01', [punc,shapes]).
stop_phrase('against', [prep]).
stop_phrase('at 10', [prep,shapes]).
stop_phrase('between', [prep]).
stop_phrase('coli,', [head,punc]).
stop_phrase('constitutes', [verb]).
stop_phrase('contrary', [head]).
stop_phrase('enrolled', [verb]).
stop_phrase('genetically', [adv]).
stop_phrase('how', [conj]).
stop_phrase('in vivo', [prep,head]).
stop_phrase('in 11', [prep,shapes]).
stop_phrase('in 2', [prep,shapes]).
stop_phrase('in 3', [prep,shapes]).
stop_phrase('instead,', [adv,punc]).
stop_phrase('of 1,', [prep,shapes,punc]).
stop_phrase('of 5', [prep,shapes]).
stop_phrase('pulmonis', [head]).
stop_phrase('t.', [head,punc]).
stop_phrase('than 0.001', [prep,shapes]).
stop_phrase('themselves', [pron]).
stop_phrase('to that', [prep,pron]).
stop_phrase('whilst', [conj]).
stop_phrase('(18', [punc,shapes]).
stop_phrase('(1982', [punc,shapes]).
stop_phrase('(5%', [punc,shapes]).
stop_phrase('.', [punc]).
stop_phrase('/10', [punc,shapes]).
stop_phrase(';11', [punc,shapes]).
stop_phrase('= 2', [punc,shapes]).
stop_phrase('ascertain', [verb]).
stop_phrase('assume', [verb]).
stop_phrase('assuming', [verb]).
stop_phrase('briefly', [adv]).
stop_phrase('disclosed', [verb]).
stop_phrase('equally', [adv]).
stop_phrase('from 1', [prep,shapes]).
stop_phrase('j.', [head,punc]).
stop_phrase('likewise,', [adv,punc]).
stop_phrase('potentially', [adv]).
stop_phrase('routinely', [adv]).
stop_phrase('than 0.01', [prep,shapes]).
stop_phrase('which also', [pron,adv]).
stop_phrase('(11%', [punc,shapes]).
stop_phrase('(6-13', [punc,shapes,punc,shapes]).
stop_phrase('26', [shapes]).
stop_phrase('5%', [shapes]).
stop_phrase('= 0.0001', [punc,shapes]).
stop_phrase('[', [punc]).
stop_phrase('actively', [adv]).
stop_phrase('balb', [head]).
stop_phrase('both.', [pron,punc]).
stop_phrase('concomitantly', [adv]).
stop_phrase('demonstration', [head]).
stop_phrase('dissociated', [verb]).
stop_phrase('functionally', [adv]).
stop_phrase('in which', [prep,pron]).
stop_phrase('in more', [prep,adv]).
stop_phrase('just', [adv]).
stop_phrase('of a.', [prep,det,punc]).
stop_phrase('preincubated', [verb]).
stop_phrase('relatively', [adv]).
stop_phrase('significantly,', [adv,punc]).
stop_phrase('the feasibility', [det,head]).
stop_phrase('those who', [det,pron]).
stop_phrase('under', [prep]).
stop_phrase('v)', [conj]).
stop_phrase('(13%', [punc,shapes]).
stop_phrase('(21%', [punc,shapes]).
stop_phrase('(30%', [punc,shapes]).
stop_phrase('(sd', [punc,head]).
stop_phrase('12,', [shapes,punc]).
stop_phrase('15,', [shapes,punc]).
stop_phrase('27', [shapes]).
stop_phrase('accurately', [adv]).
stop_phrase('actually', [adv]).
stop_phrase('albeit', [conj]).
stop_phrase('covalently', [adv]).
stop_phrase('eventually', [adv]).
stop_phrase('ie,', [prep,punc]).
stop_phrase('important to', [mod,adv]).
stop_phrase('in accordance', [prep,head]).
stop_phrase('in particular', [prep,head]).
stop_phrase('ins', [prep]).
stop_phrase('it.', [pron,punc]).
stop_phrase('mimicked', [verb]).
stop_phrase('n,', [head,punc]).
stop_phrase('parapertussis', [head]).
stop_phrase('phosphorylated', [pastpart]).
stop_phrase('recognizing', [verb]).
stop_phrase('safely', [adv]).
stop_phrase('specifically,', [adv,punc]).
stop_phrase('than 0.025', [prep,shapes]).
stop_phrase('than half', [prep,head]).
stop_phrase('(14%', [punc,shapes]).
stop_phrase('(1984', [punc,shapes]).
stop_phrase('(27%', [punc,shapes]).
stop_phrase('(28%', [punc,shapes]).
stop_phrase('(ch2nh', [punc,head]).
stop_phrase('(more', [punc,adv]).
stop_phrase('1984', [shapes]).
stop_phrase('2,3,7,8-tcdd', [shapes,punc]).
stop_phrase('29', [shapes]).
stop_phrase('80%', [shapes]).
stop_phrase('; n', [punc,head]).
stop_phrase(';21', [punc,shapes]).
stop_phrase('= 7', [punc,shapes]).
stop_phrase('amplify', [verb]).
stop_phrase('am', [aux]).
stop_phrase('as to', [prep]).
stop_phrase('ascribed', [verb]).
stop_phrase('at all', [prep,pron]).
stop_phrase('behave', [verb]).
stop_phrase('beneficial', [head]).
stop_phrase('by more', [prep,adv]).
stop_phrase('convert', [verb]).
stop_phrase('differentially', [adv]).
stop_phrase('implying', [verb]).
stop_phrase('in 10', [prep,shapes]).
stop_phrase('in 12', [prep,shapes]).
stop_phrase('quantitated', [verb]).
stop_phrase('reside', [verb]).
stop_phrase('t.,', [head,punc,punc]).
stop_phrase('the possibility', [det,head]).
stop_phrase('with m.', [prep,head,punc]).
stop_phrase('(13', [punc,shapes]).
stop_phrase('(1985', [punc,shapes]).
stop_phrase('(2%', [punc,shapes]).
stop_phrase('(25%', [punc,shapes]).
stop_phrase('30%', [shapes]).
stop_phrase('= 0.04', [punc,shapes]).
stop_phrase('= 11', [punc,shapes]).
stop_phrase('= 3', [punc,shapes]).
stop_phrase('also to', [adv,adv]).
stop_phrase('at which', [prep,pron]).
stop_phrase('delineate', [verb]).
stop_phrase('elucidated', [verb]).
stop_phrase('except', [verb]).
stop_phrase('from 10', [prep,shapes]).
stop_phrase('in the', [prep,det]).
stop_phrase('indeed,', [adv,punc]).
stop_phrase('occasionally', [adv]).
stop_phrase('partly', [adv]).
stop_phrase('potentiate', [verb]).
stop_phrase('quantitate', [verb]).
stop_phrase('subcutaneously', [adv]).
stop_phrase('than 50%', [prep,shapes]).
stop_phrase('the importance', [det,head]).
stop_phrase('untreated', [verb]).
stop_phrase('with,', [prep,punc]).
stop_phrase('(12', [punc,shapes]).
stop_phrase('(18%', [punc,shapes]).
stop_phrase('(83%', [punc,shapes]).
stop_phrase('(tf1', [punc,head]).
stop_phrase('18,', [shapes,punc]).
stop_phrase('1985', [shapes]).
stop_phrase('25%', [shapes]).
stop_phrase('alternatively,', [adv,punc]).
stop_phrase('c-fos,', [mod,punc,head,punc]).
stop_phrase('coincided', [verb]).
stop_phrase('coincident', [head]).
stop_phrase('conserved', [pastpart]).
stop_phrase('emphasizes', [verb]).
stop_phrase('of a', [prep,det]).
stop_phrase('operate', [verb]).
stop_phrase('originate', [verb]).
stop_phrase('persist', [verb]).
stop_phrase('potentiates', [verb]).
stop_phrase('previously,', [adv,punc]).
stop_phrase('proceed', [verb]).
stop_phrase('shed', [verb]).
stop_phrase('slaughtered', [verb]).
stop_phrase('summarizes', [verb]).
stop_phrase('than that', [prep,pron]).
stop_phrase('the advantages', [det,head]).
stop_phrase('the remainder', [det,head]).
stop_phrase('tightly', [adv]).
stop_phrase('we previously', [pron,adv]).
stop_phrase('with more', [prep,adv]).
stop_phrase('(12%', [punc,shapes]).
stop_phrase('(15%', [punc,shapes]).
stop_phrase('(19%', [punc,shapes]).
stop_phrase('(24%', [punc,shapes]).
stop_phrase('(65%', [punc,shapes]).
stop_phrase('(67%', [punc,shapes]).
stop_phrase('*0501', [punc,shapes]).
stop_phrase('/hr', [punc,head]).
stop_phrase('2'',', [shapes,punc,punc]).
stop_phrase('60,', [shapes,punc]).
stop_phrase('7.', [shapes,punc]).
stop_phrase('< .001', [punc,punc,shapes]).
stop_phrase('= 0.001', [punc,shapes]).
stop_phrase('= 12', [punc,shapes]).
stop_phrase('absorbed', [verb]).
stop_phrase('bring', [verb]).
stop_phrase('differing', [verb]).
stop_phrase('diverged', [verb]).
stop_phrase('excised', [verb]).
stop_phrase('exist.', [verb,punc]).
stop_phrase('for which', [prep,pron]).
stop_phrase('in 13', [prep,shapes]).
stop_phrase('inactivate', [verb]).
stop_phrase('indirectly', [adv]).
stop_phrase('iv', [head]).
stop_phrase('lasted', [verb]).
stop_phrase('lasting', [verb]).
stop_phrase('of some', [prep,pron]).
stop_phrase('selective', [head]).
stop_phrase('significantly.', [adv,punc]).
stop_phrase('solely', [adv]).
stop_phrase('than those', [prep,pron]).
stop_phrase('that only', [compl,adv]).
stop_phrase('the notion', [det,head]).
stop_phrase('this,', [det,punc]).
stop_phrase('to those', [prep,det]).
stop_phrase('unclear', [head]).
stop_phrase('yet to', [adv,adv]).
stop_phrase('(1992', [punc,shapes]).
stop_phrase('(23%', [punc,shapes]).
stop_phrase('(29%', [punc,shapes]).
stop_phrase('(35%', [punc,shapes]).
stop_phrase('(60%', [punc,shapes]).
stop_phrase('(9%', [punc,shapes]).
stop_phrase('(e', [punc,head]).
stop_phrase('/c', [punc,head]).
stop_phrase('11,', [shapes,punc]).
stop_phrase('1989', [shapes]).
stop_phrase('20,', [shapes,punc]).
stop_phrase('31', [shapes]).
stop_phrase('34', [shapes]).
stop_phrase('60', [shapes]).
stop_phrase('=', [punc]).
stop_phrase('agreed', [verb]).
stop_phrase('al', [head]).
stop_phrase('any,', [pron,punc]).
stop_phrase('arose', [verb]).
stop_phrase('constituted', [verb]).
stop_phrase('cotransfected', [verb]).
stop_phrase('developmentally', [adv]).
stop_phrase('for those', [prep,det]).
stop_phrase('igm', [head]).
stop_phrase('implicate', [verb]).
stop_phrase('in less', [prep,adv]).
stop_phrase('in only', [prep,adv]).
stop_phrase('interacted', [verb]).
stop_phrase('interferes', [verb]).
stop_phrase('of l.', [prep,head,punc]).
stop_phrase('originally', [adv]).
stop_phrase('presumed', [verb]).
stop_phrase('quantify', [verb]).
stop_phrase('rather than', [conj]).
stop_phrase('solved', [verb]).
stop_phrase('than 10%', [prep,shapes]).
stop_phrase('than 10', [prep,shapes]).
stop_phrase('than previously', [prep,adv]).
stop_phrase('unaffected.', [verb,punc]).
stop_phrase('valuable', [head]).
stop_phrase('(1989', [punc,shapes]).
stop_phrase('(2+', [punc,shapes,punc]).
stop_phrase('(20', [punc,shapes]).
stop_phrase('(22%', [punc,shapes]).
stop_phrase('(26%', [punc,shapes]).
stop_phrase('(4%', [punc,shapes]).
stop_phrase('(48%', [punc,shapes]).
stop_phrase('(70%', [punc,shapes]).
stop_phrase('(8%', [punc,shapes]).
stop_phrase('(80%', [punc,shapes]).
stop_phrase('(m', [punc,head]).
stop_phrase('/dl.', [punc,head,punc]).
stop_phrase('100', [shapes]).
stop_phrase('12%', [shapes]).
stop_phrase('17,', [shapes,punc]).
stop_phrase('1990', [shapes]).
stop_phrase('23', [shapes]).
stop_phrase('32', [shapes]).
stop_phrase('7%', [shapes]).
stop_phrase('70%', [shapes]).
stop_phrase('75%', [shapes]).
stop_phrase('85%', [shapes]).
stop_phrase(':.', [punc,punc]).
stop_phrase(';19', [punc,shapes]).
stop_phrase(';22', [punc,shapes]).
stop_phrase('= 0.002', [punc,shapes]).
stop_phrase('= 14', [punc,shapes]).
stop_phrase('achieved.', [verb,punc]).
stop_phrase('almost completely', [adv,adv]).
stop_phrase('as those', [prep,det]).
stop_phrase('back', [prep]).
stop_phrase('come', [verb]).
stop_phrase('emphasized', [verb]).
stop_phrase('encompassing', [verb]).
stop_phrase('establishment', [head]).
stop_phrase('exceed', [verb]).
stop_phrase('expressed.', [verb,punc]).
stop_phrase('hbeag', [head]).
stop_phrase('hence', [adv]).
stop_phrase('imposed', [verb]).
stop_phrase('in 9', [prep,shapes]).
stop_phrase('in b.', [prep,head,punc]).
stop_phrase('itself.', [pron,punc]).
stop_phrase('of importance', [prep,head]).
stop_phrase('of such', [prep,det]).
stop_phrase('of,', [prep,punc]).
stop_phrase('off', [prep]).
stop_phrase('on the contrary,', [prep,det,head,punc]).
stop_phrase('overload', [verb]).
stop_phrase('previously.', [adv,punc]).
stop_phrase('prior to', [prep]).
stop_phrase('quantification', [head]).
stop_phrase('recruited', [verb]).
stop_phrase('sired', [verb]).
stop_phrase('such', [pron]).
stop_phrase('the yield', [det,head]).
stop_phrase('tolerated', [verb]).
stop_phrase('transcribed', [pastpart]).
stop_phrase('unexpectedly,', [adv,punc]).
stop_phrase('upon', [prep]).
stop_phrase('us', [pron]).
stop_phrase('vulgaris', [head]).
stop_phrase('whenever', [conj]).
stop_phrase('(36%', [punc,shapes]).
stop_phrase('(42%', [punc,shapes]).
stop_phrase('(51%', [punc,shapes]).
stop_phrase('(54%', [punc,shapes]).
stop_phrase('(56%', [punc,shapes]).
stop_phrase('(73%', [punc,shapes]).
stop_phrase('(t', [punc,head]).
stop_phrase('0%', [shapes]).
stop_phrase('16%', [shapes]).
stop_phrase('1988', [shapes]).
stop_phrase('4-', [shapes,punc]).
stop_phrase('64', [shapes]).
stop_phrase('= 0.05', [punc,shapes]).
stop_phrase('abolish', [verb]).
stop_phrase('abrogated', [verb]).
stop_phrase('afforded', [verb]).
stop_phrase('all,', [pron,punc]).
stop_phrase('arises', [verb]).
stop_phrase('as a model', [prep,det,head]).
stop_phrase('aureus', [head]).
stop_phrase('before,', [adv,punc]).
stop_phrase('d.', [head,punc]).
stop_phrase('down', [prep]).
stop_phrase('exceeding', [verb]).
stop_phrase('first', [pron]).
stop_phrase('from p.', [prep,head,punc]).
stop_phrase('his', [pron]).
stop_phrase('igg', [head]).
stop_phrase('in vitro,', [prep,head,punc]).
stop_phrase('in almost', [prep,adv]).
stop_phrase('it to', [pron,adv]).
stop_phrase('mimics', [verb]).
stop_phrase('originating', [verb]).
stop_phrase('overcome', [head]).
stop_phrase('recognized', [pastpart]).
stop_phrase('that there', [compl,adv]).
stop_phrase('the rise', [det,head]).
stop_phrase('thereafter,', [adv,punc]).
stop_phrase('thereafter', [adv]).
stop_phrase('these', [pron]).
stop_phrase('to m.', [prep,head,punc]).
stop_phrase('to,', [prep,punc]).
stop_phrase('totally', [adv]).
stop_phrase('translocated', [verb]).
stop_phrase('undergone', [verb]).
stop_phrase('warrant', [verb]).
stop_phrase('with any', [prep,pron]).
stop_phrase('with emphasis', [prep,head]).
stop_phrase('x', [head]).
stop_phrase('(15', [punc,shapes]).
stop_phrase('(16', [punc,shapes]).
stop_phrase('(1990', [punc,shapes]).
stop_phrase('(38%', [punc,shapes]).
stop_phrase('(57%', [punc,shapes]).
stop_phrase('(71%', [punc,shapes]).
stop_phrase('(n-6', [punc,mod,punc,shapes]).
stop_phrase('(pi', [punc,head]).
stop_phrase('/dl,', [punc,head,punc]).
stop_phrase('1982', [shapes]).
stop_phrase('28', [shapes]).
stop_phrase('3-', [shapes,punc]).
stop_phrase('60%', [shapes]).
stop_phrase('; p less', [punc,head,adv]).
stop_phrase('= 13', [punc,shapes]).
stop_phrase('a rise', [det,head]).
stop_phrase('a safe', [det,head]).
stop_phrase('achieved', [pastpart]).
stop_phrase('appearing', [verb]).
stop_phrase('argue', [verb]).
stop_phrase('as yet', [prep,adv]).
stop_phrase('by itself', [prep,pron]).
stop_phrase('carefully', [adv]).
stop_phrase('chrysogenum', [head]).
stop_phrase('for less', [prep,adv]).
stop_phrase('from each', [prep,pron]).
stop_phrase('generated', [pastpart]).
stop_phrase('in 14', [prep,shapes]).
stop_phrase('in 15', [prep,shapes]).
stop_phrase('in 8', [prep,shapes]).
stop_phrase('influenced', [pastpart]).
stop_phrase('instead', [adv]).
stop_phrase('itself,', [pron,punc]).
stop_phrase('j.,', [head,punc,punc]).
stop_phrase('more closely', [adv,adv]).
stop_phrase('necessary.', [head,punc]).
stop_phrase('neutralized', [verb]).
stop_phrase('of which', [prep,pron]).
stop_phrase('of 20', [prep,shapes]).
stop_phrase('of them.', [prep,pron,punc]).
stop_phrase('phosphorylate', [verb]).
stop_phrase('postulate', [verb]).
stop_phrase('produced.', [verb,punc]).
stop_phrase('promptly', [adv]).
stop_phrase('repressed', [verb]).
stop_phrase('sensitized', [verb]).
stop_phrase('suited', [verb]).
stop_phrase('summarized', [verb]).
stop_phrase('this model', [det,head]).
stop_phrase('through', [prep]).
stop_phrase('times that', [prep,pron]).
stop_phrase('with the', [prep,det]).
stop_phrase('(-11', [punc,punc,shapes]).
stop_phrase('(-1', [punc,punc,shapes]).
stop_phrase('(1 mm', [punc,shapes,head]).
stop_phrase('(1986', [punc,shapes]).
stop_phrase('(1987', [punc,shapes]).
stop_phrase('(47%', [punc,shapes]).
stop_phrase('(52%', [punc,shapes]).
stop_phrase('(69%', [punc,shapes]).
stop_phrase('/- 0.05', [punc,punc,shapes]).
stop_phrase('/6', [punc,shapes]).
stop_phrase('17%', [shapes]).
stop_phrase('1991', [shapes]).
stop_phrase('30,', [shapes,punc]).
stop_phrase('35%', [shapes]).
stop_phrase('35', [shapes]).
stop_phrase('38', [shapes]).
stop_phrase('42%', [shapes]).
stop_phrase('= 9', [punc,shapes]).
stop_phrase('].', [punc,punc]).
stop_phrase('agree', [verb]).
stop_phrase('at 1', [prep,shapes]).
stop_phrase('because', [adv]).
stop_phrase('by both', [prep,det]).
stop_phrase('clinically,', [adv,punc]).
stop_phrase('coexpressed', [verb]).
stop_phrase('constructs', [verb]).
stop_phrase('converts', [verb]).
stop_phrase('critically', [adv]).
stop_phrase('deal', [verb]).
stop_phrase('during,', [prep,punc]).
stop_phrase('each.', [pron,punc]).
stop_phrase('e', [head]).
stop_phrase('for linkage', [prep,head]).
stop_phrase('for the existence', [prep,det,head]).
stop_phrase('fourteen', [shapes]).
stop_phrase('from 0', [prep,shapes]).
stop_phrase('guillain-barr', [mod,punc,head]).
stop_phrase('hybridize', [verb]).
stop_phrase('importance', [head]).
stop_phrase('in vivo.', [prep,head,punc]).
stop_phrase('in 6', [prep,shapes]).
stop_phrase('in 7', [prep,shapes]).
stop_phrase('in both', [prep,pron]).
stop_phrase('in situ.', [prep,head,punc]).
stop_phrase('in the vicinity', [prep,det,head]).
stop_phrase('incorporate', [verb]).
stop_phrase('increasingly', [adv]).
stop_phrase('interfering', [verb]).
stop_phrase('km', [head]).
stop_phrase('mentioned', [verb]).
stop_phrase('namely', [adv]).
stop_phrase('noticed', [verb]).
stop_phrase('of 2', [prep,shapes]).
stop_phrase('of tf1', [prep,head]).
stop_phrase('of v.', [prep,head,punc]).
stop_phrase('or -', [conj,punc]).
stop_phrase('phosphorylates', [verb]).
stop_phrase('propagated', [verb]).
stop_phrase('reacting', [verb]).
stop_phrase('serologically', [adv]).
stop_phrase('substitute', [verb]).
stop_phrase('than 1%', [prep,shapes]).
stop_phrase('that 1', [compl,shapes]).
stop_phrase('the establishment', [det,head]).
stop_phrase('to c.', [prep,head,punc]).
stop_phrase('with both', [prep,det]).
stop_phrase('with guillain-barr', [prep,mod,punc,head]).
stop_phrase('(2-chloroethyl', [punc,shapes,punc,head]).
stop_phrase('(25', [punc,shapes]).
stop_phrase('(49%', [punc,shapes]).
stop_phrase('(55%', [punc,shapes]).
stop_phrase('(76%', [punc,shapes]).
stop_phrase('(77%', [punc,shapes]).
stop_phrase('(92%', [punc,shapes]).
stop_phrase('(csa', [punc,head]).
stop_phrase('100,', [shapes,punc]).
stop_phrase('1983', [shapes]).
stop_phrase('23%', [shapes]).
stop_phrase('3%', [shapes]).
stop_phrase('32%', [shapes]).
stop_phrase('33%', [shapes]).
stop_phrase('37', [shapes]).
stop_phrase('41', [shapes]).
stop_phrase('43%', [shapes]).
stop_phrase('45%', [shapes]).
stop_phrase('83%', [shapes]).
stop_phrase('87%', [shapes]).
stop_phrase(';14', [punc,shapes]).
stop_phrase('= .02', [punc,punc,shapes]).
stop_phrase('= 16', [punc,shapes]).
stop_phrase('= 18', [punc,shapes]).
stop_phrase('acutely', [adv]).
stop_phrase('adequately', [adv]).
stop_phrase('anticipated', [verb]).
stop_phrase('came', [verb]).
stop_phrase('capsulatum', [head]).
stop_phrase('cultivated', [verb]).
stop_phrase('deserves', [verb]).
stop_phrase('differently', [adv]).
stop_phrase('for all', [prep,det]).
stop_phrase('for all', [prep,pron]).
stop_phrase('from 3', [prep,shapes]).
stop_phrase('generating', [verb]).
stop_phrase('glycosylated', [verb]).
stop_phrase('hbsag', [head]).
stop_phrase('herein', [adv]).
stop_phrase('hydrolyzed', [verb]).
stop_phrase('in 1', [prep,shapes]).
stop_phrase('in a.', [prep,det,punc]).
stop_phrase('incorporated', [pastpart]).
stop_phrase('incorporating', [verb]).
stop_phrase('like', [prep]).
stop_phrase('maximally', [adv]).
stop_phrase('me', [pron]).
stop_phrase('model', [head]).
stop_phrase('n-', [head,punc]).
stop_phrase('n.', [head,punc]).
stop_phrase('naturally', [adv]).
stop_phrase('of 14', [prep,shapes]).
stop_phrase('of 18', [prep,shapes]).
stop_phrase('of c-fos,', [prep,mod,punc,head,punc]).
stop_phrase('of d.', [prep,head,punc]).
stop_phrase('of n.', [prep,head,punc]).
stop_phrase('otherwise', [adv]).
stop_phrase('principally', [adv]).
stop_phrase('principles', [head]).
stop_phrase('reared', [verb]).
stop_phrase('solubilized', [verb]).
stop_phrase('such as', [prep]).
stop_phrase('tentatively', [adv]).
stop_phrase('tf1', [head]).
stop_phrase('than .05', [prep,punc,shapes]).
stop_phrase('that also', [compl,adv]).
stop_phrase('to each other', [prep,pron]).
stop_phrase('truncated', [verb]).
stop_phrase('undetectable', [head]).
stop_phrase('utilizes', [verb]).
stop_phrase('weakly', [adv]).
stop_phrase('with 10', [prep,shapes]).
stop_phrase('with e.', [prep,head,punc]).
stop_phrase('(0,', [punc,shapes,punc]).
stop_phrase('(100%', [punc,shapes]).
stop_phrase('(17%', [punc,shapes]).
stop_phrase('(1988', [punc,shapes]).
stop_phrase('(19', [punc,shapes]).
stop_phrase('(3%', [punc,shapes]).
stop_phrase('(31%', [punc,shapes]).
stop_phrase('(32%', [punc,shapes]).
stop_phrase('(34%', [punc,shapes]).
stop_phrase('(37%', [punc,shapes]).
stop_phrase('(44%', [punc,shapes]).
stop_phrase('(63%', [punc,shapes]).
stop_phrase('(81%', [punc,shapes]).
stop_phrase('(ppng', [punc,head]).
stop_phrase('+', [punc]).
stop_phrase('/- 4', [punc,punc,shapes]).
stop_phrase('13%', [shapes]).
stop_phrase('14%', [shapes]).
stop_phrase('1987', [shapes]).
stop_phrase('26%', [shapes]).
stop_phrase('36', [shapes]).
stop_phrase('4%', [shapes]).
stop_phrase('73%', [shapes]).
stop_phrase('86%', [shapes]).
stop_phrase('< 0.005', [punc,shapes]).
stop_phrase('= 20', [punc,shapes]).
stop_phrase('accompanying', [verb]).
stop_phrase('achieving', [verb]).
stop_phrase('almost exclusively', [adv,adv]).
stop_phrase('an improvement', [det,head]).
stop_phrase('as is', [head]).
stop_phrase('chronically', [adv]).
stop_phrase('constitutively', [adv]).
stop_phrase('cotransfection', [head]).
stop_phrase('csf', [head]).
stop_phrase('doing', [aux]).
stop_phrase('equipped', [verb]).
stop_phrase('experimentally', [adv]).
stop_phrase('highlights', [verb]).
stop_phrase('illustrated', [verb]).
stop_phrase('important implications', [mod,head]).
stop_phrase('in 50%', [prep,shapes]).
stop_phrase('in such', [prep,det]).
stop_phrase('individually', [adv]).
stop_phrase('ki', [head]).
stop_phrase('of them', [prep,pron]).
stop_phrase('of 15', [prep,shapes]).
stop_phrase('of 17', [prep,shapes]).
stop_phrase('of 25', [prep,shapes]).
stop_phrase('of any', [prep,pron]).
stop_phrase('of t', [prep,head]).
stop_phrase('over that', [prep,pron]).
stop_phrase('persists', [verb]).
stop_phrase('postulated', [pastpart]).
stop_phrase('proceeds', [verb]).
stop_phrase('reproduced', [verb]).
stop_phrase('s.,', [head,punc]).
stop_phrase('so,', [adv,punc]).
stop_phrase('than 0.01,', [prep,shapes,punc]).
stop_phrase('that more', [compl,adv]).
stop_phrase('the demonstration', [det,head]).
stop_phrase('the dqa1', [det,head]).
stop_phrase('the improvement', [det,head]).
stop_phrase('them.', [pron,punc]).
stop_phrase('thirteen', [shapes]).
stop_phrase('tolerate', [verb]).
stop_phrase('under the influence', [prep,det,head]).
stop_phrase('unfortunately,', [adv,punc]).
stop_phrase('v.', [conj]).
stop_phrase('validated', [verb]).
stop_phrase('with 1,', [prep,shapes,punc]).
stop_phrase('with a', [prep,det]).
stop_phrase('(4,', [punc,shapes,punc]).
stop_phrase('(41%', [punc,shapes]).
stop_phrase('(43%', [punc,shapes]).
stop_phrase('(5,', [punc,shapes,punc]).
stop_phrase('(72%', [punc,shapes]).
stop_phrase('(q23', [punc,head]).
stop_phrase('*0201', [punc,shapes]).
stop_phrase('*1', [punc,shapes]).
stop_phrase('/- 0.02', [punc,punc,shapes]).
stop_phrase('/- 0.03', [punc,punc,shapes]).
stop_phrase('/- 0.3', [punc,punc,shapes]).
stop_phrase('0,', [shapes,punc]).
stop_phrase('000', [shapes]).
stop_phrase('1%', [shapes]).
stop_phrase('15%', [shapes]).
stop_phrase('1992', [shapes]).
stop_phrase('3'',', [shapes,punc,punc]).
stop_phrase('38%', [shapes]).
stop_phrase('50,', [shapes,punc]).
stop_phrase('55%', [shapes]).
stop_phrase('56', [shapes]).
stop_phrase('57', [shapes]).
stop_phrase('6-trinitrophenyl', [shapes,punc,head]).
stop_phrase('67%', [shapes]).
stop_phrase('76%', [shapes]).
stop_phrase('95%', [shapes]).
stop_phrase('< 0.01', [punc,shapes]).
stop_phrase('a decline', [det,head]).
stop_phrase('abolishes', [verb]).
stop_phrase('advantages', [head]).
stop_phrase('antagonize', [verb]).
stop_phrase('arisen', [verb]).
stop_phrase('as an adjunct', [prep,det,head]).
stop_phrase('away', [adv]).
stop_phrase('between 5', [prep,shapes]).
stop_phrase('both,', [pron,punc]).
stop_phrase('cerevisiae', [head]).
stop_phrase('crucial', [head]).
stop_phrase('deals', [verb]).
stop_phrase('delineated', [verb]).
stop_phrase('depolarized', [verb]).
stop_phrase('did.', [aux,punc]).
stop_phrase('disappearance', [head]).
stop_phrase('done.', [aux,punc]).
stop_phrase('e1', [head]).
stop_phrase('each,', [pron,punc]).
stop_phrase('efflux', [head]).
stop_phrase('emerge', [verb]).
stop_phrase('fifteen', [shapes]).
stop_phrase('for such', [prep,det]).
stop_phrase('for the', [prep,det]).
stop_phrase('if there', [compl,adv]).
stop_phrase('illustrated', [pastpart]).
stop_phrase('in 100%', [prep,shapes]).
stop_phrase('in 22', [prep,shapes]).
stop_phrase('in guillain-barr', [prep,mod,punc,head]).
stop_phrase('in p.', [prep,head,punc]).
stop_phrase('in situ,', [prep,head,punc]).
stop_phrase('indeed', [adv]).
stop_phrase('lethal', [head]).
stop_phrase('ligated', [verb]).
stop_phrase('of 30', [prep,shapes]).
stop_phrase('of csa', [prep,head]).
stop_phrase('of p-450', [prep,mod,punc,shapes]).
stop_phrase('of the e.', [prep,det,head,punc]).
stop_phrase('ogren', [head]).
stop_phrase('oxidized', [verb]).
stop_phrase('passed', [verb]).
stop_phrase('pass', [verb]).
stop_phrase('recognised', [verb]).
stop_phrase('reversibly', [adv]).
stop_phrase('seek', [verb]).
stop_phrase('simply', [adv]).
stop_phrase('sphaeroides', [head]).
stop_phrase('steadily', [adv]).
stop_phrase('summarize', [verb]).
stop_phrase('systematically', [adv]).
stop_phrase('than 0.1', [prep,shapes]).
stop_phrase('than 1', [prep,shapes]).
stop_phrase('the accumulation', [det,head]).
stop_phrase('the decline', [det,head]).
stop_phrase('them to', [pron,adv]).
stop_phrase('to 1', [prep,shapes]).
stop_phrase('to b.', [prep,head,punc]).
stop_phrase('tried', [verb]).
stop_phrase('useful to', [mod,adv]).
stop_phrase('validate', [verb]).
stop_phrase('wherein', [conj]).
stop_phrase('whether there', [compl,adv]).
stop_phrase('which we', [pron,pron]).
stop_phrase('who also', [pron,adv]).
stop_phrase('with p.', [prep,head,punc]).
stop_phrase('with which', [prep,pron]).
stop_phrase('(1%', [punc,shapes]).
stop_phrase('(1983', [punc,shapes]).
stop_phrase('(45%', [punc,shapes]).
stop_phrase('(58%', [punc,shapes]).
stop_phrase('(66%', [punc,shapes]).
stop_phrase('(hbsag', [punc,head]).
stop_phrase('/- 0.2', [punc,punc,shapes]).
stop_phrase('/- 0.4', [punc,punc,shapes]).
stop_phrase('/- 1.5', [punc,punc,shapes]).
stop_phrase('/- 3', [punc,punc,shapes]).
stop_phrase('/- 4%', [punc,punc,shapes]).
stop_phrase('/1', [punc,shapes]).
stop_phrase('/6j', [punc,head]).
stop_phrase('/d,', [punc,head,punc]).
stop_phrase('19,', [shapes,punc]).
stop_phrase('1986', [shapes]).
stop_phrase('22%', [shapes]).
stop_phrase('27%', [shapes]).
stop_phrase('28%', [shapes]).
stop_phrase('29%', [shapes]).
stop_phrase('3''-o-', [shapes,punc,punc,head,punc]).
stop_phrase('42', [shapes]).
stop_phrase('52', [shapes]).
stop_phrase('6%', [shapes]).
stop_phrase('72%', [shapes]).
stop_phrase('79%', [shapes]).
stop_phrase('8%', [shapes]).
stop_phrase(': -', [punc,punc]).
stop_phrase('= 0.008', [punc,shapes]).
stop_phrase('= 15', [punc,shapes]).
stop_phrase('= 19', [punc,shapes]).
stop_phrase('= 23', [punc,shapes]).
stop_phrase('above', [head]).
stop_phrase('accommodate', [verb]).
stop_phrase('adsorbed', [verb]).
stop_phrase('agglutinated', [verb]).
stop_phrase('appeared to', [pastpart,adv]).
stop_phrase('argued', [pastpart]).
stop_phrase('at 1,', [prep,shapes,punc]).
stop_phrase('at all.', [prep,pron,punc]).
stop_phrase('between them', [prep,pron]).
stop_phrase('clues', [head]).
stop_phrase('concomitantly,', [adv,punc]).
stop_phrase('converted', [pastpart]).
stop_phrase('dominated', [verb]).
stop_phrase('drastically', [adv]).
stop_phrase('elucidated.', [verb,punc]).
stop_phrase('emphasized', [pastpart]).
stop_phrase('entirely', [adv]).
stop_phrase('exceeds', [verb]).
stop_phrase('fewer', [head]).
stop_phrase('for both', [prep,det]).
stop_phrase('for,', [prep,punc]).
stop_phrase('from 2', [prep,shapes]).
stop_phrase('from all', [prep,pron]).
stop_phrase('from each other', [prep,pron]).
stop_phrase('from s.', [prep,head,punc]).
stop_phrase('heavily', [adv]).
stop_phrase('here.', [adv,punc]).
stop_phrase('homogenates', [verb]).
stop_phrase('igm,', [head,punc]).
stop_phrase('importantly,', [adv,punc]).
stop_phrase('inactivating', [verb]).
stop_phrase('m-1', [mod,punc,shapes]).
stop_phrase('mechanically', [adv]).
stop_phrase('more important', [adv,head]).
stop_phrase('morphologically', [adv]).
stop_phrase('much less', [adv,adv]).
stop_phrase('nonetheless,', [adv,punc]).
stop_phrase('of atp,', [prep,head,punc]).
stop_phrase('of igg', [prep,head]).
stop_phrase('of linkage', [prep,head]).
stop_phrase('of most', [prep,pron]).
stop_phrase('of stay', [prep,head]).
stop_phrase('of t.', [prep,head,punc]).
stop_phrase('of the s.', [prep,det,head,punc]).
stop_phrase('of these', [prep,pron]).
stop_phrase('predominant', [head]).
stop_phrase('previously to', [adv,adv]).
stop_phrase('rearranged', [verb]).
stop_phrase('regularly', [adv]).
stop_phrase('rely', [verb]).
stop_phrase('rescued', [verb]).
stop_phrase('s-1', [head]).
stop_phrase('segregated', [verb]).
stop_phrase('soon', [adv]).
stop_phrase('sublines', [head]).
stop_phrase('tends', [verb]).
stop_phrase('the e.', [det,head,punc]).
stop_phrase('to 50%', [prep,shapes]).
stop_phrase('to e.', [prep,head,punc]).
stop_phrase('to the', [prep,det]).
stop_phrase('v.', [head,punc]).
stop_phrase('which specifically', [pron,adv]).
stop_phrase('why', [conj]).
stop_phrase('with all', [prep,pron]).
stop_phrase('with less', [prep,adv]).
stop_phrase('(-2', [punc,punc,shapes]).
stop_phrase('(1981', [punc,shapes]).
stop_phrase('(3,', [punc,shapes,punc]).
stop_phrase('(39%', [punc,shapes]).
stop_phrase('(46%', [punc,shapes]).
stop_phrase('(53%', [punc,shapes]).
stop_phrase('(85%', [punc,shapes]).
stop_phrase('(86%', [punc,shapes]).
stop_phrase('(94%', [punc,shapes]).
stop_phrase('(a/t', [punc,head]).
stop_phrase('/- 1', [punc,punc,shapes]).
stop_phrase('1.0', [shapes]).
stop_phrase('10%,', [shapes,punc]).
stop_phrase('12-dimethylbenz', [shapes,punc,head]).
stop_phrase('1981', [shapes]).
stop_phrase('21%', [shapes]).
stop_phrase('21,', [shapes,punc]).
stop_phrase('25,', [shapes,punc]).
stop_phrase('33', [shapes]).
stop_phrase('44%', [shapes]).
stop_phrase('46,', [shapes,punc]).
stop_phrase('52%', [shapes]).
stop_phrase('56%', [shapes]).
stop_phrase('61%', [shapes]).
stop_phrase('64%', [shapes]).
stop_phrase('75,', [shapes,punc]).
stop_phrase('8-', [shapes,punc]).
stop_phrase('80', [shapes]).
stop_phrase('82%', [shapes]).
stop_phrase('9%', [shapes]).
stop_phrase('91%', [shapes]).
stop_phrase('98%', [shapes]).
stop_phrase('= 24', [punc,shapes]).
stop_phrase('> 0.05', [punc,shapes]).
stop_phrase('a more', [det,adv]).
stop_phrase('aeruginosa', [head]).
stop_phrase('again,', [adv,punc]).
stop_phrase('among those', [prep,det]).
stop_phrase('appeared', [pastpart]).
stop_phrase('as soon', [prep,adv]).
stop_phrase('at both', [prep,det]).
stop_phrase('awaiting', [verb]).
stop_phrase('b.,', [head,punc,punc]).
stop_phrase('behaved', [verb]).
stop_phrase('by p.', [prep,head,punc]).
stop_phrase('captured', [verb]).
stop_phrase('complained', [verb]).
stop_phrase('confined', [pastpart]).
stop_phrase('counteracted', [verb]).
stop_phrase('d,', [head,punc]).
stop_phrase('d-trp7,', [mod,head,punc]).
stop_phrase('d.,', [head,punc]).
stop_phrase('differ.', [verb,punc]).
stop_phrase('disappear', [verb]).
stop_phrase('distinguishes', [verb]).
stop_phrase('d', [head]).
stop_phrase('elsewhere', [adv]).
stop_phrase('emerging', [verb]).
stop_phrase('emphasis', [head]).
stop_phrase('evenly', [adv]).
stop_phrase('expansion', [head]).
stop_phrase('explores', [verb]).
stop_phrase('fischeri', [head]).
stop_phrase('for 1', [prep,shapes]).
stop_phrase('for most', [prep,pron]).
stop_phrase('from 5', [prep,shapes]).
stop_phrase('from b.', [prep,head,punc]).
stop_phrase('fsh', [head]).
stop_phrase('hcg', [head]).
stop_phrase('heterodimers', [head]).
stop_phrase('how to', [compl,adv]).
stop_phrase('however.', [adv,punc]).
stop_phrase('immunologically', [adv]).
stop_phrase('immunoreactive', [head]).
stop_phrase('in all', [prep,pron]).
stop_phrase('in 16', [prep,shapes]).
stop_phrase('in 17', [prep,shapes]).
stop_phrase('in 20%', [prep,shapes]).
stop_phrase('in 20', [prep,shapes]).
stop_phrase('in c.', [prep,head,punc]).
stop_phrase('in csf', [prep,head]).
stop_phrase('in regard', [prep,head]).
stop_phrase('in this model,', [prep,det,head,punc]).
stop_phrase('inactivates', [verb]).
stop_phrase('incorporates', [verb]).
stop_phrase('innervating', [verb]).
stop_phrase('integrate', [verb]).
stop_phrase('interfered', [verb]).
stop_phrase('internalized', [verb]).
stop_phrase('kcat', [head]).
stop_phrase('more.', [adv,punc]).
stop_phrase('of 4', [prep,shapes]).
stop_phrase('of 6', [prep,shapes]).
stop_phrase('of atp.', [prep,head,punc]).
stop_phrase('of the latter', [prep,det,head]).
stop_phrase('of this', [prep,det]).
stop_phrase('own', [verb]).
stop_phrase('particularly useful', [adv,head]).
stop_phrase('passively', [adv]).
stop_phrase('plotted', [verb]).
stop_phrase('plus', [prep]).
stop_phrase('pose', [verb]).
stop_phrase('precisely', [adv]).
stop_phrase('preclude', [verb]).
stop_phrase('relies', [verb]).
stop_phrase('shortly', [adv]).
stop_phrase('somewhat', [adv]).
stop_phrase('structurally', [adv]).
stop_phrase('substantiated', [verb]).
stop_phrase('successively', [adv]).
stop_phrase('superimposed', [verb]).
stop_phrase('s', [head]).
stop_phrase('than 0.0005', [prep,shapes]).
stop_phrase('than 80%', [prep,shapes]).
stop_phrase('that even', [compl,adv]).
stop_phrase('the existence', [det,head]).
stop_phrase('the influence', [det,head]).
stop_phrase('the breakpoint', [det,head]).
stop_phrase('the km', [det,head]).
stop_phrase('their', [pron]).
stop_phrase('those that', [det,pron]).
stop_phrase('to 2', [prep,shapes]).
stop_phrase('to 80%', [prep,shapes]).
stop_phrase('to each', [prep,pron]).
stop_phrase('tpa', [head]).
stop_phrase('typically', [adv]).
stop_phrase('warranted', [verb]).
stop_phrase('where it', [pron,pron]).
stop_phrase('where they', [pron,pron]).
stop_phrase('why', [compl]).
stop_phrase('withhold', [verb]).
stop_phrase('x,', [head,punc]).
stop_phrase('(22', [punc,shapes]).
stop_phrase('(5 x 10', [punc,shapes,mod,shapes]).
stop_phrase('(62%', [punc,shapes]).
stop_phrase('(75%', [punc,shapes]).
stop_phrase('(78%', [punc,shapes]).
stop_phrase('(84%', [punc,shapes]).
stop_phrase('(93%', [punc,shapes]).
stop_phrase('(96%', [punc,shapes]).
stop_phrase('(km', [punc,head]).
stop_phrase('(pc12', [punc,head]).
stop_phrase('/+', [punc,punc]).
stop_phrase('/- 0.5', [punc,punc,shapes]).
stop_phrase('/- 0.8', [punc,punc,shapes]).
stop_phrase('/- 2', [punc,punc,shapes]).
stop_phrase('/- 3%', [punc,punc,shapes]).
stop_phrase('/- 8', [punc,punc,shapes]).
stop_phrase('/km', [punc,head]).
stop_phrase('0.5', [shapes]).
stop_phrase('1-', [shapes,punc]).
stop_phrase('1.0,', [shapes,punc]).
stop_phrase('100%,', [shapes,punc]).
stop_phrase('18%', [shapes]).
stop_phrase('24 hr', [shapes,head]).
stop_phrase('250,', [shapes,punc]).
stop_phrase('2d3', [head]).
stop_phrase('34%', [shapes]).
stop_phrase('37%', [shapes]).
stop_phrase('43', [shapes]).
stop_phrase('45,', [shapes,punc]).
stop_phrase('45', [shapes]).
stop_phrase('48%', [shapes]).
stop_phrase('54%', [shapes]).
stop_phrase('55', [shapes]).
stop_phrase('70,', [shapes,punc]).
stop_phrase('71%', [shapes]).
stop_phrase('78%', [shapes]).
stop_phrase('92%', [shapes]).
stop_phrase(';q22', [punc,head]).
stop_phrase('< 0.001', [punc,shapes]).
stop_phrase('< 0.05', [punc,shapes]).
stop_phrase('< 0.0005', [punc,shapes]).
stop_phrase('= 30', [punc,shapes]).
stop_phrase('],', [punc,punc]).
stop_phrase('a model', [det,head]).
stop_phrase('a.m.', [det,punc,head,punc]).
stop_phrase('additionally', [adv]).
stop_phrase('also significantly', [adv,adv]).
stop_phrase('altogether,', [adv,punc]).
stop_phrase('ameliorate', [verb]).
stop_phrase('amenable', [head]).
stop_phrase('appreciated', [verb]).
stop_phrase('appropriately', [adv]).
stop_phrase('at least', [prep,head]).
stop_phrase('at 0,', [prep,shapes,punc]).
stop_phrase('at 3', [prep,shapes]).
stop_phrase('atp,', [head,punc]).
stop_phrase('behaves', [verb]).
stop_phrase('bilaterally', [adv]).
stop_phrase('burgdorferi', [head]).
stop_phrase('by any', [prep,pron]).
stop_phrase('by s.', [prep,head,punc]).
stop_phrase('c.,', [head,punc,punc]).
stop_phrase('caf', [conj]).
stop_phrase('complementation', [head]).
stop_phrase('csa', [head]).
stop_phrase('deemed', [verb]).
stop_phrase('discussed,', [pastpart,punc]).
stop_phrase('dr7', [head]).
stop_phrase('due to', [prep]).
stop_phrase('enzymatically', [adv]).
stop_phrase('eradicated', [verb]).
stop_phrase('feasible', [head]).
stop_phrase('for some', [prep,pron]).
stop_phrase('from,', [prep,punc]).
stop_phrase('furthermore', [adv]).
stop_phrase('heifers', [head]).
stop_phrase('implicated', [pastpart]).
stop_phrase('implied', [verb]).
stop_phrase('in 1980', [prep,shapes]).
stop_phrase('in 70%', [prep,shapes]).
stop_phrase('in c-fos', [prep,mod,punc,head]).
stop_phrase('in which there', [prep,pron,adv]).
stop_phrase('ineffective.', [head,punc]).
stop_phrase('informative', [head]).
stop_phrase('inherent', [head]).
stop_phrase('innervated', [verb]).
stop_phrase('ki-67,', [mod,punc,shapes,punc]).
stop_phrase('l.,', [head,punc]).
stop_phrase('like that', [prep,pron]).
stop_phrase('meanwhile,', [head,punc]).
stop_phrase('most commonly', [adv,adv]).
stop_phrase('mptp', [shapes,punc]).
stop_phrase('n'',', [head,punc,punc]).
stop_phrase('nevertheless', [adv]).
stop_phrase('nor-', [conj,punc]).
stop_phrase('o-', [head,punc]).
stop_phrase('obvious', [head]).
stop_phrase('occupationally', [adv]).
stop_phrase('of 12', [prep,shapes]).
stop_phrase('of 19', [prep,shapes]).
stop_phrase('of 3', [prep,shapes]).
stop_phrase('of hcg', [prep,head]).
stop_phrase('of improvement', [prep,head]).
stop_phrase('of it', [prep,pron]).
stop_phrase('of nmt', [prep,mod,punc,head]).
stop_phrase('of rb', [prep,head]).
stop_phrase('of what', [prep,pron]).
stop_phrase('only 2', [adv,shapes]).
stop_phrase('out,', [adv,punc]).
stop_phrase('perceived', [verb]).
stop_phrase('physically', [adv]).
stop_phrase('precluded', [verb]).
stop_phrase('prelabeled', [verb]).
stop_phrase('properly', [adv]).
stop_phrase('provided that', [conj]).
stop_phrase('quickly', [adv]).
stop_phrase('recognized.', [verb,punc]).
stop_phrase('render', [verb]).
stop_phrase('responders', [head]).
stop_phrase('reverted', [verb]).
stop_phrase('significantly more', [adv,adv]).
stop_phrase('simultaneously,', [adv,punc]).
stop_phrase('standardized', [verb]).
stop_phrase('strictly', [adv]).
stop_phrase('temporarily', [adv]).
stop_phrase('than .01', [prep,punc,shapes]).
stop_phrase('than .10', [prep,punc,shapes]).
stop_phrase('than 20%', [prep,shapes]).
stop_phrase('that specifically', [compl,adv]).
stop_phrase('the advantage', [det,head]).
stop_phrase('the luxa', [det,head]).
stop_phrase('the only', [det,adv]).
stop_phrase('the s.', [det,head,punc]).
stop_phrase('those who', [pron,pron]).
stop_phrase('to 20%', [prep,shapes]).
stop_phrase('to s.', [prep,head,punc]).
stop_phrase('topically', [adv]).
stop_phrase('unclear,', [head,punc]).
stop_phrase('underway', [verb]).
stop_phrase('v/', [conj]).
stop_phrase('with t', [prep,head]).
stop_phrase('x 10', [mod,shapes]).

