
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
*  https://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/

% File:	    metamap_stop_phrase.pl
% Module:   MetaMap
% Author:   Lan
% Purpose:  Improve efficiency by avoiding fully processing phrases with
%           no candidates

% Source:   NLM TC, OMED TC, NCBI TC and MMI TS

% In NLM.2018AA, we took the top 1821 entries (all those with frequency of at least 5),
% giving us 474428 of the 499335 stop phrases collected, i.e., 95.01%.

% In NLM.2017AA, we took the top 1057 entries (all those with frequency of at least 10),
% giving us 351620 of the 381865 stop phrases collected, i.e., 92.07%.

% In USAbase.2015AB, we took the top 1046 entries (all those with frequency of at least 10),
% giving us 353230 of the 383054 stop phrases collected, i.e., 92.21%.

% In NLM.2015AA, we took the top 1046 entries (all those with frequency of at least 10),
% giving us 353192 of the 383548 stop phrases collected, i.e., 92.08%.

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

stop_phrase('"', [punc]). % " Just to fake out Emacs
stop_phrase('"', [punc]).
stop_phrase('"phaseolicola', [punc,head]).
stop_phrase('%', [punc]).
stop_phrase('''', [punc]).
stop_phrase('''.', [punc,punc]).
stop_phrase('("', [punc,punc]). % " Just to fake out Emacs...again
stop_phrase('(', [punc]).
stop_phrase('(+', [punc,punc]).
stop_phrase('(- 9', [punc,punc,shapes]).
stop_phrase('(-', [punc,punc]).
stop_phrase('(-11', [punc,punc,shapes]).
stop_phrase('(-12', [punc,punc,shapes]).
stop_phrase('(-4', [punc,punc,shapes]).
stop_phrase('(-6', [punc,punc,shapes]).
stop_phrase('(-7', [punc,punc,shapes]).
stop_phrase('(-8', [punc,punc,shapes]).
stop_phrase('(-9', [punc,punc,shapes]).
stop_phrase('(10', [punc,shapes]).
stop_phrase('(11%', [punc,shapes]).
stop_phrase('(11', [punc,shapes]).
stop_phrase('(12%', [punc,shapes]).
stop_phrase('(12', [punc,shapes]).
stop_phrase('(13%', [punc,shapes]).
stop_phrase('(13', [punc,shapes]).
stop_phrase('(14%', [punc,shapes]).
stop_phrase('(14', [punc,shapes]).
stop_phrase('(14-27', [punc,shapes,punc,shapes]).
stop_phrase('(15%', [punc,shapes]).
stop_phrase('(15', [punc,shapes]).
stop_phrase('(16%', [punc,shapes]).
stop_phrase('(16', [punc,shapes]).
stop_phrase('(17%', [punc,shapes]).
stop_phrase('(17', [punc,shapes]).
stop_phrase('(17,', [punc,shapes,punc]).
stop_phrase('(18%', [punc,shapes]).
stop_phrase('(18', [punc,shapes]).
stop_phrase('(19%', [punc,shapes]).
stop_phrase('(19', [punc,shapes]).
stop_phrase('(1977', [punc,shapes]).
stop_phrase('(1978', [punc,shapes]).
stop_phrase('(1979', [punc,shapes]).
stop_phrase('(1980', [punc,shapes]).
stop_phrase('(1981', [punc,shapes]).
stop_phrase('(1982', [punc,shapes]).
stop_phrase('(1983', [punc,shapes]).
stop_phrase('(1984', [punc,shapes]).
stop_phrase('(1985', [punc,shapes]).
stop_phrase('(1986', [punc,shapes]).
stop_phrase('(1987', [punc,shapes]).
stop_phrase('(1988', [punc,shapes]).
stop_phrase('(1989', [punc,shapes]).
stop_phrase('(1990', [punc,shapes]).
stop_phrase('(1991', [punc,shapes]).
stop_phrase('(1992', [punc,shapes]).
stop_phrase('(21%', [punc,shapes]).
stop_phrase('(21', [punc,shapes]).
stop_phrase('(22%', [punc,shapes]).
stop_phrase('(22', [punc,shapes]).
stop_phrase('(24%', [punc,shapes]).
stop_phrase('(24', [punc,shapes]).
stop_phrase('(25%', [punc,shapes]).
stop_phrase('(25', [punc,shapes]).
stop_phrase('(26%', [punc,shapes]).
stop_phrase('(26', [punc,shapes]).
stop_phrase('(27%', [punc,shapes]).
stop_phrase('(28%', [punc,shapes]).
stop_phrase('(32%', [punc,shapes]).
stop_phrase('(32', [punc,shapes]).
stop_phrase('(33%', [punc,shapes]).
stop_phrase('(34%', [punc,shapes]).
stop_phrase('(35%', [punc,shapes]).
stop_phrase('(35', [punc,shapes]).
stop_phrase('(36%', [punc,shapes]).
stop_phrase('(37%', [punc,shapes]).
stop_phrase('(37.5%', [punc,shapes]).
stop_phrase('(38%', [punc,shapes]).
stop_phrase('(38-57', [punc,shapes,punc,shapes]).
stop_phrase('(39%', [punc,shapes]).
stop_phrase('(4', [punc,shapes]).
stop_phrase('(41%', [punc,shapes]).
stop_phrase('(42%', [punc,shapes]).
stop_phrase('(43%', [punc,shapes]).
stop_phrase('(44%', [punc,shapes]).
stop_phrase('(45%', [punc,shapes]).
stop_phrase('(46%', [punc,shapes]).
stop_phrase('(46,', [punc,shapes,punc]).
stop_phrase('(47%', [punc,shapes]).
stop_phrase('(48%', [punc,shapes]).
stop_phrase('(49%', [punc,shapes]).
stop_phrase('(51%', [punc,shapes]).
stop_phrase('(52%', [punc,shapes]).
stop_phrase('(53%', [punc,shapes]).
stop_phrase('(54%', [punc,shapes]).
stop_phrase('(56%', [punc,shapes]).
stop_phrase('(57%', [punc,shapes]).
stop_phrase('(58%', [punc,shapes]).
stop_phrase('(59%', [punc,shapes]).
stop_phrase('(6%', [punc,shapes]).
stop_phrase('(6', [punc,shapes]).
stop_phrase('(6-13', [punc,shapes,punc,shapes]).
stop_phrase('(6-14', [punc,shapes,punc,shapes]).
stop_phrase('(61%', [punc,shapes]).
stop_phrase('(62%', [punc,shapes]).
stop_phrase('(63%', [punc,shapes]).
stop_phrase('(64%', [punc,shapes]).
stop_phrase('(66%', [punc,shapes]).
stop_phrase('(66.7%', [punc,shapes]).
stop_phrase('(67%', [punc,shapes]).
stop_phrase('(7%', [punc,shapes]).
stop_phrase('(7', [punc,shapes]).
stop_phrase('(72%', [punc,shapes]).
stop_phrase('(73%', [punc,shapes]).
stop_phrase('(74%', [punc,shapes]).
stop_phrase('(75%', [punc,shapes]).
stop_phrase('(76%', [punc,shapes]).
stop_phrase('(77%', [punc,shapes]).
stop_phrase('(78%', [punc,shapes]).
stop_phrase('(79%', [punc,shapes]).
stop_phrase('(8%', [punc,shapes]).
stop_phrase('(8', [punc,shapes]).
stop_phrase('(8.3%', [punc,shapes]).
stop_phrase('(8.5%', [punc,shapes]).
stop_phrase('(81%', [punc,shapes]).
stop_phrase('(82%', [punc,shapes]).
stop_phrase('(83%', [punc,shapes]).
stop_phrase('(84%', [punc,shapes]).
stop_phrase('(85%', [punc,shapes]).
stop_phrase('(86%', [punc,shapes]).
stop_phrase('(87%', [punc,shapes]).
stop_phrase('(88%', [punc,shapes]).
stop_phrase('(89%', [punc,shapes]).
stop_phrase('(9%', [punc,shapes]).
stop_phrase('(9', [punc,shapes]).
stop_phrase('(91%', [punc,shapes]).
stop_phrase('(92%', [punc,shapes]).
stop_phrase('(93%', [punc,shapes]).
stop_phrase('(94%', [punc,shapes]).
stop_phrase('(95%', [punc,shapes]).
stop_phrase('(96%', [punc,shapes]).
stop_phrase('(98%', [punc,shapes]).
stop_phrase('(a', [punc,head]).
stop_phrase('(adh', [punc,head]).
stop_phrase('(atl', [punc,head]).
stop_phrase('(b', [punc,head]).
stop_phrase('(b)', [punc,head]).
stop_phrase('(bmax', [punc,head]).
stop_phrase('(bt2camp', [punc,head]).
stop_phrase('(c', [punc,head]).
stop_phrase('(c12u', [punc,head]).
stop_phrase('(ch2n', [punc,head]).
stop_phrase('(ch2nh', [punc,head]).
stop_phrase('(csa', [punc,head]).
stop_phrase('(e', [punc,head]).
stop_phrase('(e1', [punc,head]).
stop_phrase('(ec50', [punc,head]).
stop_phrase('(ed50', [punc,head]).
stop_phrase('(ed50,', [punc,head,punc]).
stop_phrase('(gaaagt', [punc,head]).
stop_phrase('(guillain-barr', [punc,mod,punc,head]).
stop_phrase('(hbv', [punc,head]).
stop_phrase('(ifn', [punc,head]).
stop_phrase('(igg', [punc,head]).
stop_phrase('(ii', [punc,head]).
stop_phrase('(ii)', [punc,head]).
stop_phrase('(j.', [punc,head,punc]).
stop_phrase('(ka', [punc,head]).
stop_phrase('(ki', [punc,head]).
stop_phrase('(kon', [punc,head]).
stop_phrase('(ldl', [punc,head]).
stop_phrase('(mmi', [punc,head]).
stop_phrase('(mpo', [punc,head]).
stop_phrase('(nmt1p', [punc,head]).
stop_phrase('(ovx', [punc,head]).
stop_phrase('(paco2', [punc,head]).
stop_phrase('(pcwp', [punc,head]).
stop_phrase('(pora', [punc,head]).
stop_phrase('(q21', [punc,head]).
stop_phrase('(q23', [punc,head]).
stop_phrase('(q24', [punc,head]).
stop_phrase('(q34', [punc,head]).
stop_phrase('(rgrf', [punc,head]).
stop_phrase('(rr', [punc,head]).
stop_phrase('(sd', [punc,head]).
stop_phrase('(sec', [punc,head]).
stop_phrase('(sem', [punc,head]).
stop_phrase('(t3', [punc,head]).
stop_phrase('(t4', [punc,head]).
stop_phrase('(tf1', [punc,head]).
stop_phrase('(tpo', [punc,head]).
stop_phrase('(tsh', [punc,head]).
stop_phrase('(y', [punc,head]).
stop_phrase('(z', [punc,head]).
stop_phrase(')', [punc]).
stop_phrase(')-', [punc,punc]).
stop_phrase(').', [punc,punc]).
stop_phrase(').c', [punc,punc,head]).
stop_phrase('*', [punc]).
stop_phrase('*0201', [punc,shapes]).
stop_phrase('*0301', [punc,shapes]).
stop_phrase('*0401', [punc,shapes]).
stop_phrase('*0501', [punc,shapes]).
stop_phrase('*0501,', [punc,shapes,punc]).
stop_phrase('+21,', [punc,shapes,punc]).
stop_phrase('+8,', [punc,shapes,punc]).
stop_phrase(', -', [punc,punc]).
stop_phrase(',', [punc]).
stop_phrase('-', [punc]).
stop_phrase('--', [punc,punc]).
stop_phrase('.', [punc]).
stop_phrase('/', [punc]).
stop_phrase('000', [shapes]).
stop_phrase('10', [shapes]).
stop_phrase('1000,', [shapes,punc]).
stop_phrase('106', [shapes]).
stop_phrase('11', [shapes]).
stop_phrase('11,', [shapes,punc]).
stop_phrase('110', [shapes]).
stop_phrase('1162', [shapes]).
stop_phrase('1163', [shapes]).
stop_phrase('12%', [shapes]).
stop_phrase('12', [shapes]).
stop_phrase('12,', [shapes,punc]).
stop_phrase('12-dimethylbenz', [shapes,punc,head]).
stop_phrase('120,', [shapes,punc]).
stop_phrase('13%', [shapes]).
stop_phrase('13', [shapes]).
stop_phrase('13,', [shapes,punc]).
stop_phrase('14%', [shapes]).
stop_phrase('14', [shapes]).
stop_phrase('14,', [shapes,punc]).
stop_phrase('14.', [shapes,punc]).
stop_phrase('15%', [shapes]).
stop_phrase('15%,', [shapes,punc]).
stop_phrase('15', [shapes]).
stop_phrase('15,', [shapes,punc]).
stop_phrase('16%', [shapes]).
stop_phrase('16%,', [shapes,punc]).
stop_phrase('16', [shapes]).
stop_phrase('16,', [shapes,punc]).
stop_phrase('16.', [shapes,punc]).
stop_phrase('17%', [shapes]).
stop_phrase('17', [shapes]).
stop_phrase('17,', [shapes,punc]).
stop_phrase('172', [shapes]).
stop_phrase('18', [shapes]).
stop_phrase('18,', [shapes,punc]).
stop_phrase('19', [shapes]).
stop_phrase('19,', [shapes,punc]).
stop_phrase('19.', [shapes,punc]).
stop_phrase('1978.', [shapes,punc]).
stop_phrase('1980', [shapes]).
stop_phrase('1981', [shapes]).
stop_phrase('1981,', [shapes,punc]).
stop_phrase('1982', [shapes]).
stop_phrase('1983', [shapes]).
stop_phrase('1983,', [shapes,punc]).
stop_phrase('1984', [shapes]).
stop_phrase('1984,', [shapes,punc]).
stop_phrase('1984.', [shapes,punc]).
stop_phrase('1985', [shapes]).
stop_phrase('1985,', [shapes,punc]).
stop_phrase('1985.', [shapes,punc]).
stop_phrase('1986', [shapes]).
stop_phrase('1986,', [shapes,punc]).
stop_phrase('1986.', [shapes,punc]).
stop_phrase('1987', [shapes]).
stop_phrase('1987,', [shapes,punc]).
stop_phrase('1988', [shapes]).
stop_phrase('1989', [shapes]).
stop_phrase('1989,', [shapes,punc]).
stop_phrase('1989.', [shapes,punc]).
stop_phrase('1990', [shapes]).
stop_phrase('1990,', [shapes,punc]).
stop_phrase('1990.', [shapes,punc]).
stop_phrase('1991', [shapes]).
stop_phrase('1991,', [shapes,punc]).
stop_phrase('1991.', [shapes,punc]).
stop_phrase('1992', [shapes]).
stop_phrase('200', [shapes]).
stop_phrase('21', [shapes]).
stop_phrase('21,', [shapes,punc]).
stop_phrase('22%', [shapes]).
stop_phrase('22', [shapes]).
stop_phrase('22,', [shapes,punc]).
stop_phrase('24', [shapes]).
stop_phrase('24,', [shapes,punc]).
stop_phrase('25%', [shapes]).
stop_phrase('25', [shapes]).
stop_phrase('25,', [shapes,punc]).
stop_phrase('25-', [shapes,punc]).
stop_phrase('250,', [shapes,punc]).
stop_phrase('257', [shapes]).
stop_phrase('26%', [shapes]).
stop_phrase('26', [shapes]).
stop_phrase('27%,', [shapes,punc]).
stop_phrase('27', [shapes]).
stop_phrase('27,', [shapes,punc]).
stop_phrase('28%', [shapes]).
stop_phrase('28', [shapes]).
stop_phrase('2camp', [head]).
stop_phrase('2d3', [head]).
stop_phrase('32', [shapes]).
stop_phrase('32,', [shapes,punc]).
stop_phrase('33%', [shapes]).
stop_phrase('33', [shapes]).
stop_phrase('34', [shapes]).
stop_phrase('35%', [shapes]).
stop_phrase('35', [shapes]).
stop_phrase('35,', [shapes,punc]).
stop_phrase('36', [shapes]).
stop_phrase('37%', [shapes]).
stop_phrase('37%,', [shapes,punc]).
stop_phrase('37', [shapes]).
stop_phrase('38%', [shapes]).
stop_phrase('38', [shapes]).
stop_phrase('39', [shapes]).
stop_phrase('39,', [shapes,punc]).
stop_phrase('4', [shapes]).
stop_phrase('4,', [shapes,punc]).
stop_phrase('41%,', [shapes,punc]).
stop_phrase('41', [shapes]).
stop_phrase('41,', [shapes,punc]).
stop_phrase('42%', [shapes]).
stop_phrase('42', [shapes]).
stop_phrase('43', [shapes]).
stop_phrase('43,', [shapes,punc]).
stop_phrase('44%', [shapes]).
stop_phrase('44', [shapes]).
stop_phrase('44,', [shapes,punc]).
stop_phrase('45%', [shapes]).
stop_phrase('45', [shapes]).
stop_phrase('45,', [shapes,punc]).
stop_phrase('46%', [shapes]).
stop_phrase('46', [shapes]).
stop_phrase('46,', [shapes,punc]).
stop_phrase('47,', [shapes,punc]).
stop_phrase('48', [shapes]).
stop_phrase('48,', [shapes,punc]).
stop_phrase('500,', [shapes,punc]).
stop_phrase('51,', [shapes,punc]).
stop_phrase('52%', [shapes]).
stop_phrase('52', [shapes]).
stop_phrase('53%', [shapes]).
stop_phrase('53', [shapes]).
stop_phrase('53,', [shapes,punc]).
stop_phrase('56', [shapes]).
stop_phrase('57', [shapes]).
stop_phrase('58%,', [shapes,punc]).
stop_phrase('58', [shapes]).
stop_phrase('59%', [shapes]).
stop_phrase('6%', [shapes]).
stop_phrase('6', [shapes]).
stop_phrase('6,', [shapes,punc]).
stop_phrase('6-', [shapes,punc]).
stop_phrase('6-trinitrophenyl', [shapes,punc,head]).
stop_phrase('6.', [shapes,punc]).
stop_phrase('61%', [shapes]).
stop_phrase('63%,', [shapes,punc]).
stop_phrase('64%', [shapes]).
stop_phrase('64', [shapes]).
stop_phrase('67%', [shapes]).
stop_phrase('67', [shapes]).
stop_phrase('7%', [shapes]).
stop_phrase('7', [shapes]).
stop_phrase('7,', [shapes,punc]).
stop_phrase('7-', [shapes,punc]).
stop_phrase('7.', [shapes,punc]).
stop_phrase('73%', [shapes]).
stop_phrase('75%', [shapes]).
stop_phrase('75,', [shapes,punc]).
stop_phrase('76%', [shapes]).
stop_phrase('79%', [shapes]).
stop_phrase('8%', [shapes]).
stop_phrase('8%,', [shapes,punc]).
stop_phrase('8', [shapes]).
stop_phrase('8,', [shapes,punc]).
stop_phrase('8-', [shapes,punc]).
stop_phrase('8.', [shapes,punc]).
stop_phrase('82%', [shapes]).
stop_phrase('83%,', [shapes,punc]).
stop_phrase('85%', [shapes]).
stop_phrase('85', [shapes]).
stop_phrase('86%', [shapes]).
stop_phrase('87%', [shapes]).
stop_phrase('87', [shapes]).
stop_phrase('88', [shapes]).
stop_phrase('89', [shapes]).
stop_phrase('9%', [shapes]).
stop_phrase('9', [shapes]).
stop_phrase('9,', [shapes,punc]).
stop_phrase('91%', [shapes]).
stop_phrase('94', [shapes]).
stop_phrase('98%', [shapes]).
stop_phrase(': -', [punc,punc]).
stop_phrase(':', [punc]).
stop_phrase(':.', [punc,punc]).
stop_phrase('; 12', [punc,shapes]).
stop_phrase('; 14', [punc,shapes]).
stop_phrase('; also,', [punc,adv,punc]).
stop_phrase('; however,', [punc,adv,punc]).
stop_phrase('; moreover,', [punc,adv,punc]).
stop_phrase('; rather,', [punc,adv,punc]).
stop_phrase('; therefore,', [punc,adv,punc]).
stop_phrase('; thus', [punc,adv]).
stop_phrase('; thus,', [punc,adv,punc]).
stop_phrase(';', [punc]).
stop_phrase(';11', [punc,shapes]).
stop_phrase(';14', [punc,shapes]).
stop_phrase(';19', [punc,shapes]).
stop_phrase(';21', [punc,shapes]).
stop_phrase(';22', [punc,shapes]).
stop_phrase(';7', [punc,shapes]).
stop_phrase(';?', [punc,punc]).
stop_phrase(';q21', [punc,head]).
stop_phrase(';q22', [punc,head]).
stop_phrase(';q23', [punc,head]).
stop_phrase('<', [punc]).
stop_phrase('= 11', [punc,shapes]).
stop_phrase('= 12', [punc,shapes]).
stop_phrase('= 13', [punc,shapes]).
stop_phrase('= 14', [punc,shapes]).
stop_phrase('= 15', [punc,shapes]).
stop_phrase('= 16', [punc,shapes]).
stop_phrase('= 17', [punc,shapes]).
stop_phrase('= 18', [punc,shapes]).
stop_phrase('= 19', [punc,shapes]).
stop_phrase('= 21', [punc,shapes]).
stop_phrase('= 22', [punc,shapes]).
stop_phrase('= 24', [punc,shapes]).
stop_phrase('= 26', [punc,shapes]).
stop_phrase('= 32', [punc,shapes]).
stop_phrase('= 34', [punc,shapes]).
stop_phrase('= 44', [punc,shapes]).
stop_phrase('= 46', [punc,shapes]).
stop_phrase('= 6', [punc,shapes]).
stop_phrase('= 7', [punc,shapes]).
stop_phrase('= 8', [punc,shapes]).
stop_phrase('=', [punc]).
stop_phrase('>', [punc]).
stop_phrase('>6', [punc,shapes]).
stop_phrase('[', [punc]).
stop_phrase('[125i', [punc,head]).
stop_phrase('[14c', [punc,head]).
stop_phrase('[18f', [punc,head]).
stop_phrase('[35s', [punc,head]).
stop_phrase('[75se', [punc,head]).
stop_phrase('[ala15', [punc,head]).
stop_phrase('[c', [punc,head]).
stop_phrase('[darg1,', [punc,head,punc]).
stop_phrase('[met5', [punc,head]).
stop_phrase('[tyr4', [punc,head]).
stop_phrase(']', [punc]).
stop_phrase('],', [punc,punc]).
stop_phrase(']-', [punc,punc]).
stop_phrase(']-,', [punc,punc,punc]).
stop_phrase(']-hpgrf', [punc,punc,head]).
stop_phrase('].', [punc,punc]).
stop_phrase(']atp', [punc,head]).
stop_phrase(']fbpa', [punc,head]).
stop_phrase(']hgrf', [punc,head]).
stop_phrase('a .', [det,punc]).
stop_phrase('a 1', [head]).
stop_phrase('a c2j/c2j', [det,mod,punc,head]).
stop_phrase('a safe', [det,head]).
stop_phrase('a tendency', [det,head]).
stop_phrase('a', [det]).
stop_phrase('a,', [det,punc]).
stop_phrase('a,', [head,punc]).
stop_phrase('a-56268', [det,punc,shapes]).
stop_phrase('a.', [det,punc]).
stop_phrase('a.,', [det,punc,punc]).
stop_phrase('a/', [det,punc]).
stop_phrase('a/a', [det,punc,head]).
stop_phrase('abnormally', [adv]).
stop_phrase('abolish', [verb]).
stop_phrase('abolished', [verb]).
stop_phrase('abolished.', [verb,punc]).
stop_phrase('abolishes', [verb]).
stop_phrase('about', [adv]).
stop_phrase('abrogated', [verb]).
stop_phrase('abruptly', [adv]).
stop_phrase('absolutely', [adv]).
stop_phrase('absorb', [verb]).
stop_phrase('absorbed', [verb]).
stop_phrase('accommodate', [verb]).
stop_phrase('accompanies', [verb]).
stop_phrase('accompany', [verb]).
stop_phrase('accompanying', [verb]).
stop_phrase('accomplish', [verb]).
stop_phrase('accomplished', [verb]).
stop_phrase('accordingly', [adv]).
stop_phrase('accordingly,', [adv,punc]).
stop_phrase('accumulate', [verb]).
stop_phrase('accumulated', [verb]).
stop_phrase('accumulates', [verb]).
stop_phrase('accurately', [adv]).
stop_phrase('achieve', [verb]).
stop_phrase('achieved by', [verb,prep]).
stop_phrase('achieved', [verb]).
stop_phrase('achieved.', [verb,punc]).
stop_phrase('achieving', [verb]).
stop_phrase('actively', [adv]).
stop_phrase('actually', [adv]).
stop_phrase('acutely', [adv]).
stop_phrase('additionally', [adv]).
stop_phrase('additionally,', [adv,punc]).
stop_phrase('adequately', [adv]).
stop_phrase('adg', [head]).
stop_phrase('adoptively', [adv]).
stop_phrase('adversely', [adv]).
stop_phrase('advised', [verb]).
stop_phrase('afford', [verb]).
stop_phrase('afforded', [verb]).
stop_phrase('affords', [verb]).
stop_phrase('after', [conj]).
stop_phrase('again', [adv]).
stop_phrase('again,', [adv,punc]).
stop_phrase('again.', [adv,punc]).
stop_phrase('agglutinated', [verb]).
stop_phrase('ala15', [head]).
stop_phrase('ala2,', [head,punc]).
stop_phrase('albeit', [conj]).
stop_phrase('all of them', [pron,prep,pron]).
stop_phrase('all of which', [pron,prep,pron]).
stop_phrase('all of whom', [pron,prep,pron]).
stop_phrase('all', [det]).
stop_phrase('all', [pron]).
stop_phrase('all,', [pron,punc]).
stop_phrase('alleviate', [verb]).
stop_phrase('allocated', [verb]).
stop_phrase('already', [adv]).
stop_phrase('also', [adv]).
stop_phrase('also,', [adv,punc]).
stop_phrase('alternatively', [adv]).
stop_phrase('alternatively,', [adv,punc]).
stop_phrase('although', [conj]).
stop_phrase('altogether', [adv]).
stop_phrase('altogether,', [adv,punc]).
stop_phrase('am', [aux]).
stop_phrase('am.', [aux,punc]).
stop_phrase('ameliorate', [verb]).
stop_phrase('ameliorates', [verb]).
stop_phrase('amino-', [head,punc]).
stop_phrase('among the', [prep,det]).
stop_phrase('among them,', [prep,pron,punc]).
stop_phrase('amplified', [verb]).
stop_phrase('amplifies', [verb]).
stop_phrase('amplify', [verb]).
stop_phrase('an opportunity', [det,head]).
stop_phrase('an', [det]).
stop_phrase('an,', [head,punc]).
stop_phrase('analyse', [verb]).
stop_phrase('analysed', [verb]).
stop_phrase('analysed.', [verb,punc]).
stop_phrase('analyze', [verb]).
stop_phrase('analyzes', [verb]).
stop_phrase('analyzing', [verb]).
stop_phrase('and ''', [conj]).
stop_phrase('and -', [conj,punc]).
stop_phrase('and by', [conj,prep]).
stop_phrase('and in those', [conj,prep,det]).
stop_phrase('and in', [conj,prep]).
stop_phrase('and s', [conj]).
stop_phrase('and s-', [conj,punc]).
stop_phrase('and with', [conj,prep]).
stop_phrase('and', [conj]).
stop_phrase('and-', [conj,punc]).
stop_phrase('and/or', [conj]).
stop_phrase('angus', [head]).
stop_phrase('another', [pron]).
stop_phrase('another,', [pron,punc]).
stop_phrase('another.', [pron,punc]).
stop_phrase('antagonize', [verb]).
stop_phrase('antagonized', [verb]).
stop_phrase('antagonizes', [verb]).
stop_phrase('anticipate', [verb]).
stop_phrase('any', [pron]).
stop_phrase('any,', [pron,punc]).
stop_phrase('appreciably', [adv]).
stop_phrase('appreciated', [verb]).
stop_phrase('appropriately', [adv]).
stop_phrase('approx.', [head,punc]).
stop_phrase('are', [aux]).
stop_phrase('argue', [verb]).
stop_phrase('argued', [verb]).
stop_phrase('argues', [verb]).
stop_phrase('arise', [verb]).
stop_phrase('artificially', [adv]).
stop_phrase('as if', [conj]).
stop_phrase('as to', [prep]).
stop_phrase('as well as', [conj]).
stop_phrase('as', [conj]).
stop_phrase('as-', [conj,punc]).
stop_phrase('ascertain', [verb]).
stop_phrase('ascertained', [verb]).
stop_phrase('asked', [verb]).
stop_phrase('assume', [verb]).
stop_phrase('assumes', [verb]).
stop_phrase('assuming', [verb]).
stop_phrase('assure', [verb]).
stop_phrase('at both', [prep,det]).
stop_phrase('at least', [adv]).
stop_phrase('at which', [prep,pron]).
stop_phrase('at', [prep]).
stop_phrase('atp', [head]).
stop_phrase('atp,', [head,punc]).
stop_phrase('attain', [verb]).
stop_phrase('attained', [verb]).
stop_phrase('avoid', [verb]).
stop_phrase('avoided', [verb]).
stop_phrase('avoided.', [verb,punc]).
stop_phrase('avoiding', [verb]).
stop_phrase('avoids', [verb]).
stop_phrase('awaiting', [verb]).
stop_phrase('b', [head]).
stop_phrase('b)', [head]).
stop_phrase('b,', [head,punc]).
stop_phrase('b.', [head,punc]).
stop_phrase('b.,', [head,punc,punc]).
stop_phrase('bacteriol.', [head,punc]).
stop_phrase('barely', [adv]).
stop_phrase('be', [aux]).
stop_phrase('beats', [verb]).
stop_phrase('became', [verb]).
stop_phrase('because', [conj]).
stop_phrase('become', [verb]).
stop_phrase('becomes', [verb]).
stop_phrase('becoming', [verb]).
stop_phrase('been', [aux]).
stop_phrase('before', [conj]).
stop_phrase('before', [prep]).
stop_phrase('behave', [verb]).
stop_phrase('being', [aux]).
stop_phrase('bel1', [head]).
stop_phrase('believe', [verb]).
stop_phrase('believed', [verb]).
stop_phrase('belonged', [verb]).
stop_phrase('beneficial', [head]).
stop_phrase('beneficial.', [head,punc]).
stop_phrase('between', [prep]).
stop_phrase('biol.', [head,punc]).
stop_phrase('both clinically', [det,adv]).
stop_phrase('both g1', [det,head]).
stop_phrase('both of which', [det,prep,pron]).
stop_phrase('both with', [det,prep]).
stop_phrase('both', [det]).
stop_phrase('both', [pron]).
stop_phrase('both,', [pron,punc]).
stop_phrase('both.', [pron,punc]).
stop_phrase('briefly', [adv]).
stop_phrase('bring', [verb]).
stop_phrase('bt2camp', [head]).
stop_phrase('but', [conj]).
stop_phrase('by', [prep]).
stop_phrase('c', [head]).
stop_phrase('c,', [head,punc]).
stop_phrase('c.', [head,punc]).
stop_phrase('c.,', [head,punc]).
stop_phrase('c3', [head]).
stop_phrase('c5', [head]).
stop_phrase('c6', [head]).
stop_phrase('c7', [head]).
stop_phrase('caf', [conj]).
stop_phrase('calving', [verb]).
stop_phrase('can', [modal]).
stop_phrase('cannot', [modal]).
stop_phrase('carefully', [adv]).
stop_phrase('cas', [head]).
stop_phrase('catalyzed', [verb]).
stop_phrase('catalyzes', [verb]).
stop_phrase('causally', [adv]).
stop_phrase('cbf', [head]).
stop_phrase('centrally', [adv]).
stop_phrase('cerebral', [head]).
stop_phrase('certainly', [adv]).
stop_phrase('ch,', [head,punc]).
stop_phrase('characterise', [verb]).
stop_phrase('characterised', [verb]).
stop_phrase('characteristically', [adv]).
stop_phrase('characterize', [verb]).
stop_phrase('characterizing', [verb]).
stop_phrase('charolais', [head]).
stop_phrase('chromosomally', [adv]).
stop_phrase('chronically', [adv]).
stop_phrase('circumvent', [verb]).
stop_phrase('claimed', [verb]).
stop_phrase('classically', [adv]).
stop_phrase('clearly', [adv]).
stop_phrase('clearly,', [adv,punc]).
stop_phrase('cleave', [verb]).
stop_phrase('cleaves', [verb]).
stop_phrase('cleaving', [verb]).
stop_phrase('clinically', [adv]).
stop_phrase('clinically,', [adv,punc]).
stop_phrase('closely', [adv]).
stop_phrase('cmf', [conj]).
stop_phrase('coa,', [head,punc]).
stop_phrase('collect', [verb]).
stop_phrase('collectively', [adv]).
stop_phrase('collectively,', [adv,punc]).
stop_phrase('colonize', [verb]).
stop_phrase('columbus,', [head,punc]).
stop_phrase('come', [verb]).
stop_phrase('commonly', [adv]).
stop_phrase('comparable to that', [head,prep,pron]).
stop_phrase('comparable to those', [head,prep,det]).
stop_phrase('comparable with that', [head,prep,pron]).
stop_phrase('comparable.', [head,punc]).
stop_phrase('competitively', [adv]).
stop_phrase('composed', [verb]).
stop_phrase('conceivable', [head]).
stop_phrase('conclusively', [adv]).
stop_phrase('concomitantly', [adv]).
stop_phrase('concomitantly,', [adv,punc]).
stop_phrase('concurrently', [adv]).
stop_phrase('confer', [verb]).
stop_phrase('conferred', [verb]).
stop_phrase('conferring', [verb]).
stop_phrase('confers', [verb]).
stop_phrase('confined', [verb]).
stop_phrase('confronting', [verb]).
stop_phrase('consecutively', [adv]).
stop_phrase('consequently', [adv]).
stop_phrase('consequently,', [adv,punc]).
stop_phrase('conserved', [verb]).
stop_phrase('conserved.', [verb,punc]).
stop_phrase('considerably', [adv]).
stop_phrase('considerably.', [adv,punc]).
stop_phrase('consistently', [adv]).
stop_phrase('constitute', [verb]).
stop_phrase('constituted', [verb]).
stop_phrase('constitutes', [verb]).
stop_phrase('constitutively', [adv]).
stop_phrase('consumed', [verb]).
stop_phrase('consuming', [verb]).
stop_phrase('continuously', [adv]).
stop_phrase('continuously.', [adv,punc]).
stop_phrase('conventionally', [adv]).
stop_phrase('conversely,', [adv,punc]).
stop_phrase('convert', [verb]).
stop_phrase('converted', [verb]).
stop_phrase('converts', [verb]).
stop_phrase('convey', [verb]).
stop_phrase('cooperate', [verb]).
stop_phrase('coordinately', [adv]).
stop_phrase('correctly', [adv]).
stop_phrase('correlate', [verb]).
stop_phrase('correlates', [verb]).
stop_phrase('correspond', [verb]).
stop_phrase('corresponded', [verb]).
stop_phrase('corresponding', [verb]).
stop_phrase('corroborate', [verb]).
stop_phrase('cotranslationally', [adv]).
stop_phrase('could', [modal]).
stop_phrase('counteract', [verb]).
stop_phrase('covalently', [adv]).
stop_phrase('critically', [adv]).
stop_phrase('csa', [head]).
stop_phrase('cytogenetically', [adv]).
stop_phrase('d (', [aux]).
stop_phrase('d(', [aux]).
stop_phrase('d)', [aux]).
stop_phrase('d-', [aux]).
stop_phrase('d-[', [aux]).
stop_phrase('d.', [aux]).
stop_phrase('d/', [aux]).
stop_phrase('d9s15', [head]).
stop_phrase('d9s15,', [head,punc]).
stop_phrase('d9s15.', [head,punc]).
stop_phrase('d9s5', [head]).
stop_phrase('d9s5,', [head,punc]).
stop_phrase('ddt', [head]).
stop_phrase('declined', [verb]).
stop_phrase('declined.', [verb,punc]).
stop_phrase('declines', [verb]).
stop_phrase('deduce', [verb]).
stop_phrase('deduced', [verb]).
stop_phrase('deduced.', [verb,punc]).
stop_phrase('deemed', [verb]).
stop_phrase('define', [verb]).
stop_phrase('defines', [verb]).
stop_phrase('defining', [verb]).
stop_phrase('definitely', [adv]).
stop_phrase('definitively', [adv]).
stop_phrase('degrade', [verb]).
stop_phrase('delimit', [verb]).
stop_phrase('delineate', [verb]).
stop_phrase('delineated', [verb]).
stop_phrase('delineated.', [verb,punc]).
stop_phrase('demonstrate', [verb]).
stop_phrase('demonstrated', [verb]).
stop_phrase('demonstrated.', [verb,punc]).
stop_phrase('demonstrating', [verb]).
stop_phrase('depolarization', [head]).
stop_phrase('depolarized', [verb]).
stop_phrase('depress', [verb]).
stop_phrase('derive', [verb]).
stop_phrase('describe', [verb]).
stop_phrase('describes', [verb]).
stop_phrase('describing', [verb]).
stop_phrase('deserve', [verb]).
stop_phrase('deserves', [verb]).
stop_phrase('designate', [verb]).
stop_phrase('despite the fact', [prep,det,head]).
stop_phrase('despite this,', [prep,det,punc]).
stop_phrase('despite', [prep]).
stop_phrase('destined', [verb]).
stop_phrase('detectably', [adv]).
stop_phrase('determine', [verb]).
stop_phrase('determined by', [verb,prep]).
stop_phrase('determined', [verb]).
stop_phrase('determined.', [verb,punc]).
stop_phrase('determines', [verb]).
stop_phrase('determining', [verb]).
stop_phrase('develop', [verb]).
stop_phrase('develop.', [verb,punc]).
stop_phrase('developed', [verb]).
stop_phrase('developed.', [verb,punc]).
stop_phrase('developing', [verb]).
stop_phrase('developmentally', [adv]).
stop_phrase('develops', [verb]).
stop_phrase('devised', [verb]).
stop_phrase('df', [head]).
stop_phrase('dialyzed', [verb]).
stop_phrase('did', [aux]).
stop_phrase('did.', [aux,punc]).
stop_phrase('differ', [verb]).
stop_phrase('differ.', [verb,punc]).
stop_phrase('differed', [verb]).
stop_phrase('differed.', [verb,punc]).
stop_phrase('differentially', [adv]).
stop_phrase('differently.', [adv,punc]).
stop_phrase('differing', [verb]).
stop_phrase('differs', [verb]).
stop_phrase('disappear', [verb]).
stop_phrase('disappeared', [verb]).
stop_phrase('disappeared.', [verb,punc]).
stop_phrase('disclose', [verb]).
stop_phrase('disclosed', [verb]).
stop_phrase('displace', [verb]).
stop_phrase('dissociated', [verb]).
stop_phrase('dissociates', [verb]).
stop_phrase('distinct', [head]).
stop_phrase('distinguish', [verb]).
stop_phrase('distinguished', [verb]).
stop_phrase('distinguished.', [verb,punc]).
stop_phrase('distinguishes', [verb]).
stop_phrase('distinguishing', [verb]).
stop_phrase('diverged', [verb]).
stop_phrase('divergently', [adv]).
stop_phrase('do', [aux]).
stop_phrase('does', [aux]).
stop_phrase('doing', [aux]).
stop_phrase('done', [aux]).
stop_phrase('done.', [aux,punc]).
stop_phrase('dqa1', [head]).
stop_phrase('dqa1,', [head,punc]).
stop_phrase('dqb1', [head]).
stop_phrase('dr7', [head]).
stop_phrase('dramatically', [adv]).
stop_phrase('dramatically.', [adv,punc]).
stop_phrase('drastically', [adv]).
stop_phrase('dtrp7,', [head,punc]).
stop_phrase('during', [prep]).
stop_phrase('during,', [prep,punc]).
stop_phrase('e', [head]).
stop_phrase('e.', [head,punc]).
stop_phrase('e.,', [head,punc,punc]).
stop_phrase('e.g.', [conj,punc]).
stop_phrase('e.g.', [conj]).
stop_phrase('e1', [head]).
stop_phrase('each of which', [pron,prep,pron]).
stop_phrase('each other.', [pron,punc]).
stop_phrase('each', [pron]).
stop_phrase('each,', [pron,punc]).
stop_phrase('each.', [pron,punc]).
stop_phrase('effectively', [adv]).
stop_phrase('efficiently', [adv]).
stop_phrase('eglin', [head]).
stop_phrase('elaborate', [verb]).
stop_phrase('eliminate', [verb]).
stop_phrase('eliminates', [verb]).
stop_phrase('eliminating', [verb]).
stop_phrase('elsewhere', [adv]).
stop_phrase('elucidate', [verb]).
stop_phrase('elucidated', [verb]).
stop_phrase('elucidated.', [verb,punc]).
stop_phrase('emerged', [verb]).
stop_phrase('emg', [head]).
stop_phrase('emphasis', [head]).
stop_phrase('emphasize', [verb]).
stop_phrase('emphasized', [verb]).
stop_phrase('emphasized.', [verb,punc]).
stop_phrase('emphasizes', [verb]).
stop_phrase('emphasizing', [verb]).
stop_phrase('empirically', [adv]).
stop_phrase('encompass', [verb]).
stop_phrase('encompasses', [verb]).
stop_phrase('encompassing', [verb]).
stop_phrase('encourage', [verb]).
stop_phrase('encouraged', [verb]).
stop_phrase('encourages', [verb]).
stop_phrase('enough', [adv]).
stop_phrase('entails', [verb]).
stop_phrase('entirely', [adv]).
stop_phrase('enzymatically', [adv]).
stop_phrase('equally', [adv]).
stop_phrase('eradicate', [verb]).
stop_phrase('especially', [adv]).
stop_phrase('especially,', [adv,punc]).
stop_phrase('essentially', [adv]).
stop_phrase('establishment', [head]).
stop_phrase('even', [adv]).
stop_phrase('evenly', [adv]).
stop_phrase('eventually', [adv]).
stop_phrase('ever', [adv]).
stop_phrase('evoke', [verb]).
stop_phrase('evokes', [verb]).
stop_phrase('evolutionarily', [adv]).
stop_phrase('exactly', [adv]).
stop_phrase('examine', [verb]).
stop_phrase('examines', [verb]).
stop_phrase('exceed', [verb]).
stop_phrase('exceeded', [verb]).
stop_phrase('exceeding', [verb]).
stop_phrase('exceeds', [verb]).
stop_phrase('excised', [verb]).
stop_phrase('exclusively', [adv]).
stop_phrase('expect', [verb]).
stop_phrase('experimentally', [adv]).
stop_phrase('experimentally.', [adv,punc]).
stop_phrase('explain', [verb]).
stop_phrase('explained by', [verb,prep]).
stop_phrase('explained', [verb]).
stop_phrase('explaining', [verb]).
stop_phrase('explains', [verb]).
stop_phrase('exploited', [verb]).
stop_phrase('explore', [verb]).
stop_phrase('explored', [verb]).
stop_phrase('explored.', [verb,punc]).
stop_phrase('explores', [verb]).
stop_phrase('exploring', [verb]).
stop_phrase('exponentially', [adv]).
stop_phrase('expose', [verb]).
stop_phrase('express', [verb]).
stop_phrase('expressed', [verb]).
stop_phrase('expressed.', [verb,punc]).
stop_phrase('expresses', [verb]).
stop_phrase('expressing', [verb]).
stop_phrase('extensively', [adv]).
stop_phrase('fac', [conj]).
stop_phrase('facilitate', [verb]).
stop_phrase('facilitated', [verb]).
stop_phrase('facilitates', [verb]).
stop_phrase('facilitating', [verb]).
stop_phrase('fared', [verb]).
stop_phrase('feasible', [head]).
stop_phrase('feasible.', [head,punc]).
stop_phrase('few', [pron]).
stop_phrase('finally', [adv]).
stop_phrase('finally,', [adv,punc]).
stop_phrase('for all', [prep,det]).
stop_phrase('for both', [prep,det]).
stop_phrase('for the', [prep,det]).
stop_phrase('for which', [prep,pron]).
stop_phrase('for', [prep]).
stop_phrase('formulated', [verb]).
stop_phrase('freshly', [adv]).
stop_phrase('from all', [prep,det]).
stop_phrase('from which', [prep,pron]).
stop_phrase('from', [prep]).
stop_phrase('fsh', [head]).
stop_phrase('fully', [adv]).
stop_phrase('functionally', [adv]).
stop_phrase('furthermore', [adv]).
stop_phrase('furthermore,', [adv,punc]).
stop_phrase('g1', [head]).
stop_phrase('g1,', [head,punc]).
stop_phrase('g2', [head]).
stop_phrase('g2,', [head,punc]).
stop_phrase('g2.', [head,punc]).
stop_phrase('generally', [adv]).
stop_phrase('generally,', [adv,punc]).
stop_phrase('generate', [verb]).
stop_phrase('generates', [verb]).
stop_phrase('generating', [verb]).
stop_phrase('genetically', [adv]).
stop_phrase('get', [verb]).
stop_phrase('glycosylated', [verb]).
stop_phrase('glycosylated.', [verb,punc]).
stop_phrase('going', [verb]).
stop_phrase('govern', [verb]).
stop_phrase('governing', [verb]).
stop_phrase('governs', [verb]).
stop_phrase('gradually', [adv]).
stop_phrase('grew', [verb]).
stop_phrase('grossly', [adv]).
stop_phrase('grow', [verb]).
stop_phrase('growing', [verb]).
stop_phrase('grown', [verb]).
stop_phrase('gua', [head]).
stop_phrase('guarantee', [verb]).
stop_phrase('guillain-barr', [mod,punc,head]).
stop_phrase('had', [aux]).
stop_phrase('hardly', [adv]).
stop_phrase('has', [aux]).
stop_phrase('have', [aux]).
stop_phrase('having', [aux]).
stop_phrase('he', [pron]).
stop_phrase('heavily', [adv]).
stop_phrase('hence', [adv]).
stop_phrase('hence,', [adv,punc]).
stop_phrase('here', [adv]).
stop_phrase('here,', [adv,punc]).
stop_phrase('here.', [adv,punc]).
stop_phrase('herein', [adv]).
stop_phrase('herein,', [adv,punc]).
stop_phrase('herein.', [adv,punc]).
stop_phrase('heretofore', [adv]).
stop_phrase('heterogeneously', [adv]).
stop_phrase('hgrf', [head]).
stop_phrase('highlight', [verb]).
stop_phrase('highlights', [verb]).
stop_phrase('his', [pron]).
stop_phrase('hitherto', [adv]).
stop_phrase('how', [adv]).
stop_phrase('however', [adv]).
stop_phrase('however,', [adv,punc]).
stop_phrase('however.', [adv,punc]).
stop_phrase('hundred', [head]).
stop_phrase('hybridized', [verb]).
stop_phrase('hydrolyse', [verb]).
stop_phrase('hydrolyze', [verb]).
stop_phrase('hydrolyzed', [verb]).
stop_phrase('hydrolyzes', [verb]).
stop_phrase('hyperpolarized', [verb]).
stop_phrase('hypothesize', [verb]).
stop_phrase('hypothesized', [verb]).
stop_phrase('ideally', [adv]).
stop_phrase('identifies', [verb]).
stop_phrase('identify', [verb]).
stop_phrase('identifying', [verb]).
stop_phrase('ie', [conj]).
stop_phrase('if', [conj]).
stop_phrase('ifn', [head]).
stop_phrase('ige', [head]).
stop_phrase('igg', [head]).
stop_phrase('igg,', [head,punc]).
stop_phrase('igg.', [head,punc]).
stop_phrase('igm', [head]).
stop_phrase('igm,', [head,punc]).
stop_phrase('ii', [head]).
stop_phrase('ii,', [head,punc]).
stop_phrase('ii.', [head,punc]).
stop_phrase('illustrate', [verb]).
stop_phrase('illustrated', [verb]).
stop_phrase('illustrates', [verb]).
stop_phrase('illustrating', [verb]).
stop_phrase('imidazo', [head]).
stop_phrase('immunize', [verb]).
stop_phrase('immunized', [verb]).
stop_phrase('immunohistochemically', [adv]).
stop_phrase('immunologically', [adv]).
stop_phrase('impair', [verb]).
stop_phrase('impairs', [verb]).
stop_phrase('impart', [verb]).
stop_phrase('implement', [verb]).
stop_phrase('implemented', [verb]).
stop_phrase('implemented.', [verb,punc]).
stop_phrase('implicate', [verb]).
stop_phrase('implicated', [verb]).
stop_phrase('implicates', [verb]).
stop_phrase('implicating', [verb]).
stop_phrase('implications', [head]).
stop_phrase('implied', [verb]).
stop_phrase('implies', [verb]).
stop_phrase('imply', [verb]).
stop_phrase('implying', [verb]).
stop_phrase('important', [head]).
stop_phrase('importantly,', [adv,punc]).
stop_phrase('impossible', [head]).
stop_phrase('improve', [verb]).
stop_phrase('in 15', [prep,shapes]).
stop_phrase('in 1990,', [prep,shapes,punc]).
stop_phrase('in a manner', [prep,det,head]).
stop_phrase('in addition to', [prep]).
stop_phrase('in all', [prep,det]).
stop_phrase('in all', [prep,pron]).
stop_phrase('in all,', [prep,pron,punc]).
stop_phrase('in both', [prep,det]).
stop_phrase('in determining', [prep,head]).
stop_phrase('in fact,', [prep,head,punc]).
stop_phrase('in guillain-barr', [prep,mod,punc,head]).
stop_phrase('in particular', [prep,head]).
stop_phrase('in particular,', [prep,head,punc]).
stop_phrase('in such', [prep,det]).
stop_phrase('in the', [prep,det]).
stop_phrase('in these', [prep,det]).
stop_phrase('in this', [prep,det]).
stop_phrase('in those', [prep,det]).
stop_phrase('in which', [prep,pron]).
stop_phrase('in whom', [prep,pron]).
stop_phrase('in', [prep]).
stop_phrase('inability', [head]).
stop_phrase('inactivate', [verb]).
stop_phrase('inactivated', [verb]).
stop_phrase('inactivates', [verb]).
stop_phrase('inactivating', [verb]).
stop_phrase('inadequately', [adv]).
stop_phrase('inasmuch as', [conj]).
stop_phrase('incompletely', [adv]).
stop_phrase('incorporate', [verb]).
stop_phrase('incorporated', [verb]).
stop_phrase('incorporates', [verb]).
stop_phrase('incorporating', [verb]).
stop_phrase('incorrectly', [adv]).
stop_phrase('increasingly', [adv]).
stop_phrase('indeed', [adv]).
stop_phrase('indeed,', [adv,punc]).
stop_phrase('independently', [adv]).
stop_phrase('independently.', [adv,punc]).
stop_phrase('indicate', [verb]).
stop_phrase('indicates', [verb]).
stop_phrase('indicating', [verb]).
stop_phrase('indirectly', [adv]).
stop_phrase('indirectly,', [adv,punc]).
stop_phrase('indistinguishable from that', [head,prep,pron]).
stop_phrase('indistinguishable from those', [head,prep,det]).
stop_phrase('individually', [adv]).
stop_phrase('influence', [verb]).
stop_phrase('influenced', [verb]).
stop_phrase('influences', [verb]).
stop_phrase('influencing', [verb]).
stop_phrase('infused', [verb]).
stop_phrase('infused.', [verb,punc]).
stop_phrase('innervating', [verb]).
stop_phrase('instead', [adv]).
stop_phrase('instead,', [adv,punc]).
stop_phrase('integrate', [verb]).
stop_phrase('integrated', [verb]).
stop_phrase('intensively', [adv]).
stop_phrase('interact', [verb]).
stop_phrase('interestingly,', [adv,punc]).
stop_phrase('internalized', [verb]).
stop_phrase('intimately', [adv]).
stop_phrase('intracellularly', [adv]).
stop_phrase('intramuscularly', [adv]).
stop_phrase('intraperitoneally', [adv]).
stop_phrase('intravenously', [adv]).
stop_phrase('intravenously,', [adv,punc]).
stop_phrase('intravenously.', [adv,punc]).
stop_phrase('invade', [verb]).
stop_phrase('invariably', [adv]).
stop_phrase('inversely', [adv]).
stop_phrase('involve', [verb]).
stop_phrase('involves', [verb]).
stop_phrase('is', [aux]).
stop_phrase('it', [pron]).
stop_phrase('it,', [pron,punc]).
stop_phrase('it.', [pron,punc]).
stop_phrase('itself', [pron]).
stop_phrase('itself,', [pron,punc]).
stop_phrase('itself.', [pron,punc]).
stop_phrase('j.', [head,punc]).
stop_phrase('j.,', [head,punc,punc]).
stop_phrase('junb', [head]).
stop_phrase('just', [adv]).
stop_phrase('ki', [head]).
stop_phrase('ki-67,', [head,punc]).
stop_phrase('km', [head]).
stop_phrase('know', [verb]).
stop_phrase('largely', [adv]).
stop_phrase('lastly,', [adv,punc]).
stop_phrase('leaving', [verb]).
stop_phrase('lend', [verb]).
stop_phrase('lends', [verb]).
stop_phrase('lessen', [verb]).
stop_phrase('leu11', [head]).
stop_phrase('leu14', [head]).
stop_phrase('lie', [verb]).
stop_phrase('lies', [verb]).
stop_phrase('like', [prep]).
stop_phrase('likewise', [adv]).
stop_phrase('likewise,', [adv,punc]).
stop_phrase('limousin,', [head,punc]).
stop_phrase('linearly', [adv]).
stop_phrase('locate', [verb]).
stop_phrase('located', [verb]).
stop_phrase('located.', [verb,punc]).
stop_phrase('loosely', [adv]).
stop_phrase('lose', [verb]).
stop_phrase('m)', [aux]).
stop_phrase('mainly', [adv]).
stop_phrase('manage', [verb]).
stop_phrase('many', [det]).
stop_phrase('marginally', [adv]).
stop_phrase('maximally', [adv]).
stop_phrase('maximize', [verb]).
stop_phrase('may', [modal]).
stop_phrase('me', [pron]).
stop_phrase('meanwhile,', [adv,punc]).
stop_phrase('mechanically', [adv]).
stop_phrase('medically', [adv]).
stop_phrase('mentally', [adv]).
stop_phrase('mentioned', [verb]).
stop_phrase('merely', [adv]).
stop_phrase('merits', [verb]).
stop_phrase('metabolically', [adv]).
stop_phrase('metabolize', [verb]).
stop_phrase('might', [modal]).
stop_phrase('mimic', [verb]).
stop_phrase('mimicked', [verb]).
stop_phrase('mimicking', [verb]).
stop_phrase('minimally', [adv]).
stop_phrase('minimize', [verb]).
stop_phrase('minimizing', [verb]).
stop_phrase('mmi', [head]).
stop_phrase('modestly', [adv]).
stop_phrase('molecularly', [adv]).
stop_phrase('moreover,', [adv,punc]).
stop_phrase('morphologically', [adv]).
stop_phrase('must', [modal]).
stop_phrase('namely', [adv]).
stop_phrase('namely,', [adv,punc]).
stop_phrase('natl.', [head,punc]).
stop_phrase('naturally', [adv]).
stop_phrase('necessarily', [adv]).
stop_phrase('necessary', [head]).
stop_phrase('necessary,', [head,punc]).
stop_phrase('necessary.', [head,punc]).
stop_phrase('necessitate', [verb]).
stop_phrase('necessitated', [verb]).
stop_phrase('necessitates', [verb]).
stop_phrase('necessitating', [verb]).
stop_phrase('negatively', [adv]).
stop_phrase('neither', [det]).
stop_phrase('neutralize', [verb]).
stop_phrase('neutralized', [verb]).
stop_phrase('neutralizing', [verb]).
stop_phrase('nevertheless', [adv]).
stop_phrase('nevertheless,', [adv,punc]).
stop_phrase('nle27', [head]).
stop_phrase('no', [det]).
stop_phrase('no.', [det,punc]).
stop_phrase('none of them', [pron,prep,pron]).
stop_phrase('none', [pron]).
stop_phrase('none.', [pron,punc]).
stop_phrase('nonetheless,', [adv,punc]).
stop_phrase('nor', [conj]).
stop_phrase('nor-', [conj,punc]).
stop_phrase('normally', [adv]).
stop_phrase('normally,', [adv,punc]).
stop_phrase('notably', [adv]).
stop_phrase('notably,', [adv,punc]).
stop_phrase('noteworthy', [head]).
stop_phrase('noticed.', [verb,punc]).
stop_phrase('npe10', [head]).
stop_phrase('objectively', [adv]).
stop_phrase('obscure', [verb]).
stop_phrase('obvious', [head]).
stop_phrase('obviously,', [adv,punc]).
stop_phrase('occupationally', [adv]).
stop_phrase('occupy', [verb]).
stop_phrase('of all', [prep,det]).
stop_phrase('of these', [prep,det]).
stop_phrase('of these,', [prep,det,punc]).
stop_phrase('of those', [prep,det]).
stop_phrase('of', [prep]).
stop_phrase('off', [prep]).
stop_phrase('offer', [verb]).
stop_phrase('offering', [verb]).
stop_phrase('offers', [verb]).
stop_phrase('on the contrary,', [prep,det,head,punc]).
stop_phrase('on the', [prep,det]).
stop_phrase('on', [prep]).
stop_phrase('once', [conj]).
stop_phrase('once-', [conj,punc]).
stop_phrase('optimally', [adv]).
stop_phrase('optimize', [verb]).
stop_phrase('optimized', [verb]).
stop_phrase('or -', [conj,punc]).
stop_phrase('or without', [conj,prep]).
stop_phrase('or', [conj]).
stop_phrase('orf2', [head]).
stop_phrase('originally', [adv]).
stop_phrase('other', [det]).
stop_phrase('other,', [det,punc]).
stop_phrase('other.', [det,punc]).
stop_phrase('otherwise', [adv]).
stop_phrase('outline', [verb]).
stop_phrase('outlined', [verb]).
stop_phrase('outlined.', [verb,punc]).
stop_phrase('overestimated', [verb]).
stop_phrase('overload', [verb]).
stop_phrase('own', [verb]).
stop_phrase('paradoxically,', [adv,punc]).
stop_phrase('partially', [adv]).
stop_phrase('participate', [verb]).
stop_phrase('participated', [verb]).
stop_phrase('particularly', [adv]).
stop_phrase('particularly,', [adv,punc]).
stop_phrase('partly', [adv]).
stop_phrase('passively', [adv]).
stop_phrase('pathologically', [adv]).
stop_phrase('perceive', [verb]).
stop_phrase('perfect', [verb]).
stop_phrase('perfectly', [adv]).
stop_phrase('perhaps', [adv]).
stop_phrase('permanently', [adv]).
stop_phrase('persist', [verb]).
stop_phrase('persisted for', [verb,prep]).
stop_phrase('persisted', [verb]).
stop_phrase('persisted.', [verb,punc]).
stop_phrase('perturb', [verb]).
stop_phrase('phe12', [head]).
stop_phrase('phe6', [head]).
stop_phrase('phe6,', [head,punc]).
stop_phrase('phosphorylate', [verb]).
stop_phrase('phosphorylated', [verb]).
stop_phrase('phosphorylated.', [verb,punc]).
stop_phrase('phosphorylates', [verb]).
stop_phrase('physically', [adv]).
stop_phrase('physiologically', [adv]).
stop_phrase('pose', [verb]).
stop_phrase('poses', [verb]).
stop_phrase('positively', [adv]).
stop_phrase('postinfection', [head]).
stop_phrase('postinfection.', [head,punc]).
stop_phrase('postischemia', [head]).
stop_phrase('postischemia.', [head,punc]).
stop_phrase('postoperatively', [adv]).
stop_phrase('postoperatively,', [adv,punc]).
stop_phrase('postoperatively.', [adv,punc]).
stop_phrase('postulate', [verb]).
stop_phrase('postulated', [verb]).
stop_phrase('postulated.', [verb,punc]).
stop_phrase('potentially', [adv]).
stop_phrase('potentiate', [verb]).
stop_phrase('potentiated', [verb]).
stop_phrase('potentiates', [verb]).
stop_phrase('potently', [adv]).
stop_phrase('practically', [adv]).
stop_phrase('precede', [verb]).
stop_phrase('precedes', [verb]).
stop_phrase('precisely', [adv]).
stop_phrase('preclude', [verb]).
stop_phrase('precluded', [verb]).
stop_phrase('precludes', [verb]).
stop_phrase('predict', [verb]).
stop_phrase('predictably', [adv]).
stop_phrase('predicting', [verb]).
stop_phrase('predicts', [verb]).
stop_phrase('predispose', [verb]).
stop_phrase('predominantly', [adv]).
stop_phrase('prefer', [verb]).
stop_phrase('preferentially', [adv]).
stop_phrase('prelabeled with', [verb,prep]).
stop_phrase('prematurely', [adv]).
stop_phrase('preoperatively', [adv]).
stop_phrase('preserve', [verb]).
stop_phrase('presumably', [adv]).
stop_phrase('presumably,', [adv,punc]).
stop_phrase('presumed', [verb]).
stop_phrase('pretreated', [verb]).
stop_phrase('previously', [adv]).
stop_phrase('previously,', [adv,punc]).
stop_phrase('previously.', [adv,punc]).
stop_phrase('primarily', [adv]).
stop_phrase('principally', [adv]).
stop_phrase('principles', [head]).
stop_phrase('prior to', [prep]).
stop_phrase('pro2,', [head,punc]).
stop_phrase('proceed', [verb]).
stop_phrase('produce', [verb]).
stop_phrase('produced', [verb]).
stop_phrase('produced.', [verb,punc]).
stop_phrase('produces', [verb]).
stop_phrase('producing', [verb]).
stop_phrase('profoundly', [adv]).
stop_phrase('progressively', [adv]).
stop_phrase('proliferate', [verb]).
stop_phrase('proliferated', [verb]).
stop_phrase('prolong', [verb]).
stop_phrase('prolongs', [verb]).
stop_phrase('promptly', [adv]).
stop_phrase('pronounced', [verb]).
stop_phrase('pronounced.', [verb,punc]).
stop_phrase('properly', [adv]).
stop_phrase('propose', [verb]).
stop_phrase('proposes', [verb]).
stop_phrase('prospectively', [adv]).
stop_phrase('prospectively.', [adv,punc]).
stop_phrase('protect', [verb]).
stop_phrase('protected', [verb]).
stop_phrase('protected.', [verb,punc]).
stop_phrase('protects', [verb]).
stop_phrase('proteolytically', [adv]).
stop_phrase('prove', [verb]).
stop_phrase('proved', [verb]).
stop_phrase('provided that', [conj]).
stop_phrase('proving', [verb]).
stop_phrase('provoke', [verb]).
stop_phrase('psi ch2nhleu14', [mod,head]).
stop_phrase('pursue', [verb]).
stop_phrase('qualitatively', [adv]).
stop_phrase('quantitate', [verb]).
stop_phrase('quantitated', [verb]).
stop_phrase('quantitatively', [adv]).
stop_phrase('quantitatively.', [adv,punc]).
stop_phrase('quickly', [adv]).
stop_phrase('quite', [adv]).
stop_phrase('radically', [adv]).
stop_phrase('radiographically,', [adv,punc]).
stop_phrase('raise', [verb]).
stop_phrase('raises', [verb]).
stop_phrase('raising', [verb]).
stop_phrase('randomised', [verb]).
stop_phrase('randomly', [adv]).
stop_phrase('rather than', [conj]).
stop_phrase('rather', [adv]).
stop_phrase('rather,', [adv,punc]).
stop_phrase('react', [verb]).
stop_phrase('reacted', [verb]).
stop_phrase('readily', [adv]).
stop_phrase('realize', [verb]).
stop_phrase('really', [adv]).
stop_phrase('reasonable', [head]).
stop_phrase('rebleeding', [head]).
stop_phrase('recognise', [verb]).
stop_phrase('recognised', [verb]).
stop_phrase('recognize', [verb]).
stop_phrase('recognized', [verb]).
stop_phrase('recognized.', [verb,punc]).
stop_phrase('recognizes', [verb]).
stop_phrase('recognizing', [verb]).
stop_phrase('reconstituted', [verb]).
stop_phrase('reconstruct', [verb]).
stop_phrase('reduce', [verb]).
stop_phrase('reduces', [verb]).
stop_phrase('reevaluated', [verb]).
stop_phrase('reexamined', [verb]).
stop_phrase('regarding', [verb]).
stop_phrase('regardless of', [prep]).
stop_phrase('regularly', [adv]).
stop_phrase('regulate', [verb]).
stop_phrase('regulates', [verb]).
stop_phrase('regulating', [verb]).
stop_phrase('reinforce', [verb]).
stop_phrase('relatively', [adv]).
stop_phrase('reliably', [adv]).
stop_phrase('remain', [verb]).
stop_phrase('remained', [verb]).
stop_phrase('remains', [head]).
stop_phrase('remains', [verb]).
stop_phrase('remarkable', [head]).
stop_phrase('remarkably', [adv]).
stop_phrase('remarkably,', [adv,punc]).
stop_phrase('render', [verb]).
stop_phrase('rendered', [verb]).
stop_phrase('renders', [verb]).
stop_phrase('repress', [verb]).
stop_phrase('repressed', [verb]).
stop_phrase('represses', [verb]).
stop_phrase('reproduced', [verb]).
stop_phrase('reproducibly', [adv]).
stop_phrase('require', [verb]).
stop_phrase('requires', [verb]).
stop_phrase('requiring', [verb]).
stop_phrase('rescue', [verb]).
stop_phrase('rescues', [verb]).
stop_phrase('resemble', [verb]).
stop_phrase('resembled', [verb]).
stop_phrase('resembles', [verb]).
stop_phrase('resembling', [verb]).
stop_phrase('resist', [verb]).
stop_phrase('resisted', [verb]).
stop_phrase('respectively', [adv]).
stop_phrase('respectively,', [adv,punc]).
stop_phrase('respectively.', [adv,punc]).
stop_phrase('respond', [verb]).
stop_phrase('responded', [verb]).
stop_phrase('responded.', [verb,punc]).
stop_phrase('responders', [head]).
stop_phrase('responding', [verb]).
stop_phrase('responds', [verb]).
stop_phrase('restore', [verb]).
stop_phrase('restored', [verb]).
stop_phrase('restored.', [verb,punc]).
stop_phrase('restores', [verb]).
stop_phrase('retrospectively', [adv]).
stop_phrase('retrospectively.', [adv,punc]).
stop_phrase('reversibly', [adv]).
stop_phrase('rho p21', [mod,head]).
stop_phrase('rise', [verb]).
stop_phrase('routinely', [adv]).
stop_phrase('rr', [head]).
stop_phrase('s (', [aux]).
stop_phrase('s +', [aux]).
stop_phrase('s)', [aux]).
stop_phrase('s)-', [aux]).
stop_phrase('s-', [aux]).
stop_phrase('s.', [aux]).
stop_phrase('safe', [head]).
stop_phrase('safely', [adv]).
stop_phrase('said', [verb]).
stop_phrase('satisfactorily', [adv]).
stop_phrase('satisfy', [verb]).
stop_phrase('scarcely', [adv]).
stop_phrase('secondarily', [adv]).
stop_phrase('seek', [verb]).
stop_phrase('seem', [verb]).
stop_phrase('seemed', [verb]).
stop_phrase('seems', [verb]).
stop_phrase('selectively', [adv]).
stop_phrase('sensitize', [verb]).
stop_phrase('separately', [adv]).
stop_phrase('separately,', [adv,punc]).
stop_phrase('separately.', [adv,punc]).
stop_phrase('sequentially', [adv]).
stop_phrase('serially', [adv]).
stop_phrase('seriously', [adv]).
stop_phrase('serologically', [adv]).
stop_phrase('serve', [verb]).
stop_phrase('served', [verb]).
stop_phrase('serves', [verb]).
stop_phrase('sg,', [head,punc]).
stop_phrase('she', [pron]).
stop_phrase('shed', [verb]).
stop_phrase('shortly', [adv]).
stop_phrase('should', [modal]).
stop_phrase('significantly', [adv]).
stop_phrase('significantly,', [adv,punc]).
stop_phrase('significantly.', [adv,punc]).
stop_phrase('similarly', [adv]).
stop_phrase('similarly,', [adv,punc]).
stop_phrase('similarly.', [adv,punc]).
stop_phrase('simplify', [verb]).
stop_phrase('simply', [adv]).
stop_phrase('since', [conj]).
stop_phrase('slaughtered', [verb]).
stop_phrase('sm,', [head,punc]).
stop_phrase('so', [conj]).
stop_phrase('socially', [adv]).
stop_phrase('solely', [adv]).
stop_phrase('solved', [verb]).
stop_phrase('some of them', [pron,prep,pron]).
stop_phrase('some of which', [pron,prep,pron]).
stop_phrase('some', [pron]).
stop_phrase('soon', [adv]).
stop_phrase('sought', [verb]).
stop_phrase('sparse', [head]).
stop_phrase('spatially', [adv]).
stop_phrase('specifically', [adv]).
stop_phrase('specifically,', [adv,punc]).
stop_phrase('speculate', [verb]).
stop_phrase('speculated', [verb]).
stop_phrase('spliced', [verb]).
stop_phrase('spontaneously', [adv]).
stop_phrase('stably', [adv]).
stop_phrase('standardized', [verb]).
stop_phrase('stay', [verb]).
stop_phrase('steadily', [adv]).
stop_phrase('still', [adv]).
stop_phrase('stimulate', [verb]).
stop_phrase('stimulates', [verb]).
stop_phrase('strengthen', [verb]).
stop_phrase('strictly', [adv]).
stop_phrase('strikingly', [adv]).
stop_phrase('strikingly,', [adv,punc]).
stop_phrase('strongly', [adv]).
stop_phrase('structurally', [adv]).
stop_phrase('subcutaneously', [adv]).
stop_phrase('sublines', [head]).
stop_phrase('subsequently', [adv]).
stop_phrase('subsequently,', [adv,punc]).
stop_phrase('substantially', [adv]).
stop_phrase('substantially.', [adv,punc]).
stop_phrase('substantiate', [verb]).
stop_phrase('substantiated', [verb]).
stop_phrase('substituted', [verb]).
stop_phrase('substituting', [verb]).
stop_phrase('successfully', [adv]).
stop_phrase('successively', [adv]).
stop_phrase('such as', [prep]).
stop_phrase('such', [det]).
stop_phrase('sufficiently', [adv]).
stop_phrase('summarize', [verb]).
stop_phrase('summarized', [verb]).
stop_phrase('summarized.', [verb,punc]).
stop_phrase('summarizes', [verb]).
stop_phrase('supposed', [verb]).
stop_phrase('supposing', [verb]).
stop_phrase('surprising', [verb]).
stop_phrase('surprisingly', [adv]).
stop_phrase('surprisingly,', [adv,punc]).
stop_phrase('synergistically', [adv]).
stop_phrase('synthesised', [verb]).
stop_phrase('synthesize', [verb]).
stop_phrase('synthesizes', [verb]).
stop_phrase('systematically', [adv]).
stop_phrase('systemically', [adv]).
stop_phrase('t2', [head]).
stop_phrase('t3', [head]).
stop_phrase('t3,', [head,punc]).
stop_phrase('t4', [head]).
stop_phrase('t4,', [head,punc]).
stop_phrase('tandemly', [adv]).
stop_phrase('temporally', [adv]).
stop_phrase('temporarily', [adv]).
stop_phrase('tend', [verb]).
stop_phrase('tended', [verb]).
stop_phrase('tends', [verb]).
stop_phrase('tentatively', [adv]).
stop_phrase('terminally', [adv]).
stop_phrase('tf1', [head]).
stop_phrase('tf1,', [head,punc]).
stop_phrase('than it', [prep,pron]).
stop_phrase('than that', [prep,pron]).
stop_phrase('than the', [prep,det]).
stop_phrase('than those', [prep,pron]).
stop_phrase('than', [prep]).
stop_phrase('that even', [compl,adv]).
stop_phrase('that g1', [compl,head]).
stop_phrase('that', [compl]).
stop_phrase('that', [pron]).
stop_phrase('that,', [compl,punc]).
stop_phrase('the "', [det,punc]). % " Just to fake out Emacs...again
stop_phrase('the advantages', [det,head]).
stop_phrase('the assumption', [det,head]).
stop_phrase('the contention', [det,head]).
stop_phrase('the dqa1', [det,head]).
stop_phrase('the dqb1', [det,head]).
stop_phrase('the fact', [det,head]).
stop_phrase('the feasibility of', [det,head,prep]).
stop_phrase('the g1', [det,head]).
stop_phrase('the latter', [det,head]).
stop_phrase('the luxa', [det,head]).
stop_phrase('the notion', [det,head]).
stop_phrase('the opportunity', [det,head]).
stop_phrase('the other', [det,det]).
stop_phrase('the remainder', [det,head]).
stop_phrase('the s-', [det,punc]).
stop_phrase('the', [det]).
stop_phrase('them', [pron]).
stop_phrase('them.', [pron,punc]).
stop_phrase('themselves', [pron]).
stop_phrase('themselves.', [pron,punc]).
stop_phrase('theoretically', [adv]).
stop_phrase('there', [adv]).
stop_phrase('thereafter', [adv]).
stop_phrase('thereafter,', [adv,punc]).
stop_phrase('thereafter.', [adv,punc]).
stop_phrase('thereby', [adv]).
stop_phrase('therefore', [adv]).
stop_phrase('therefore,', [adv,punc]).
stop_phrase('these facts', [det,head]).
stop_phrase('these', [det]).
stop_phrase('these', [pron]).
stop_phrase('they all', [pron,pron]).
stop_phrase('they', [pron]).
stop_phrase('thiogalactopyranoside', [head]).
stop_phrase('this fact', [det,head]).
stop_phrase('this', [det]).
stop_phrase('this', [pron]).
stop_phrase('this,', [det,punc]).
stop_phrase('this.', [det,punc]).
stop_phrase('thoroughly', [adv]).
stop_phrase('those that', [det,pron]).
stop_phrase('those which', [det,pron]).
stop_phrase('those who', [det,pron]).
stop_phrase('those who', [pron,pron]).
stop_phrase('those', [det]).
stop_phrase('those', [pron]).
stop_phrase('though', [conj]).
stop_phrase('thus', [adv]).
stop_phrase('thus,', [adv,punc]).
stop_phrase('tightly', [adv]).
stop_phrase('to ddt', [prep,head]).
stop_phrase('to that', [prep,pron]).
stop_phrase('to the', [prep,det]).
stop_phrase('to those', [prep,det]).
stop_phrase('to which', [prep,pron]).
stop_phrase('to', [prep]).
stop_phrase('tolerate', [verb]).
stop_phrase('tolerated', [verb]).
stop_phrase('tolerated.', [verb,punc]).
stop_phrase('too', [adv]).
stop_phrase('topically', [adv]).
stop_phrase('totally', [adv]).
stop_phrase('traditionally', [adv]).
stop_phrase('traditionally,', [adv,punc]).
stop_phrase('transactivates', [verb]).
stop_phrase('transcribed', [verb]).
stop_phrase('transcriptionally', [adv]).
stop_phrase('transduce', [verb]).
stop_phrase('transfect', [verb]).
stop_phrase('transfected', [verb]).
stop_phrase('transform', [verb]).
stop_phrase('transforms', [verb]).
stop_phrase('transiently', [adv]).
stop_phrase('translocate', [verb]).
stop_phrase('translocated', [verb]).
stop_phrase('tried', [verb]).
stop_phrase('trp7,', [head,punc]).
stop_phrase('truncated', [verb]).
stop_phrase('try', [verb]).
stop_phrase('trying', [verb]).
stop_phrase('tsh', [head]).
stop_phrase('tsh,', [head,punc]).
stop_phrase('typically', [adv]).
stop_phrase('ultimately', [adv]).
stop_phrase('ultrastructurally', [adv]).
stop_phrase('unambiguously', [adv]).
stop_phrase('unclear', [head]).
stop_phrase('unclear,', [head,punc]).
stop_phrase('unclear.', [head,punc]).
stop_phrase('under way', [head]).
stop_phrase('underestimate', [verb]).
stop_phrase('underestimated', [verb]).
stop_phrase('undergo', [verb]).
stop_phrase('undergoes', [verb]).
stop_phrase('undergoing', [verb]).
stop_phrase('undergone', [verb]).
stop_phrase('underlie', [verb]).
stop_phrase('underline', [verb]).
stop_phrase('underlying', [verb]).
stop_phrase('undertake', [verb]).
stop_phrase('undertaken', [verb]).
stop_phrase('undertaken.', [verb,punc]).
stop_phrase('undertook', [verb]).
stop_phrase('underway', [head]).
stop_phrase('underwent', [verb]).
stop_phrase('unequivocally', [adv]).
stop_phrase('unexpectedly,', [adv,punc]).
stop_phrase('unfortunately,', [adv,punc]).
stop_phrase('uniformly', [adv]).
stop_phrase('uniquely', [adv]).
stop_phrase('unless', [conj]).
stop_phrase('until', [conj]).
stop_phrase('us', [pron]).
stop_phrase('utilize', [verb]).
stop_phrase('utilized', [verb]).
stop_phrase('utilized.', [verb,punc]).
stop_phrase('utilizes', [verb]).
stop_phrase('utilizing', [verb]).
stop_phrase('v.', [head,punc]).
stop_phrase('validate', [verb]).
stop_phrase('validated', [verb]).
stop_phrase('variably', [adv]).
stop_phrase('varied', [verb]).
stop_phrase('varied.', [verb,punc]).
stop_phrase('vary', [verb]).
stop_phrase('varying', [verb]).
stop_phrase('versus', [conj]).
stop_phrase('virtually', [adv]).
stop_phrase('visualize', [verb]).
stop_phrase('visually', [adv]).
stop_phrase('viz.', [adv,punc]).
stop_phrase('vs', [conj]).
stop_phrase('vs-', [conj,punc]).
stop_phrase('vs.', [conj,punc]).
stop_phrase('w.', [head,punc]).
stop_phrase('w.,', [head,punc]).
stop_phrase('want', [verb]).
stop_phrase('warrant', [verb]).
stop_phrase('warranted', [verb]).
stop_phrase('warranted.', [verb,punc]).
stop_phrase('was -', [aux,punc]).
stop_phrase('was', [aux]).
stop_phrase('we', [pron]).
stop_phrase('weakly', [adv]).
stop_phrase('wear', [verb]).
stop_phrase('weighed', [verb]).
stop_phrase('were -', [aux,punc]).
stop_phrase('were', [aux]).
stop_phrase('what', [pron]).
stop_phrase('when', [conj]).
stop_phrase('whenever', [conj]).
stop_phrase('where', [adv]).
stop_phrase('whereas', [conj]).
stop_phrase('whereby', [adv]).
stop_phrase('wherein', [conj]).
stop_phrase('whether', [conj]).
stop_phrase('which it', [pron,pron]).
stop_phrase('which they', [pron,pron]).
stop_phrase('which we', [pron,pron]).
stop_phrase('which', [pron]).
stop_phrase('which,', [pron,punc]).
stop_phrase('while', [conj]).
stop_phrase('whilst', [conj]).
stop_phrase('who', [pron]).
stop_phrase('who,', [pron,punc]).
stop_phrase('whose', [pron]).
stop_phrase('why', [adv]).
stop_phrase('widely', [adv]).
stop_phrase('will', [modal]).
stop_phrase('with a', [prep,det]).
stop_phrase('with guillain-barr', [prep,mod,punc,head]).
stop_phrase('with that', [prep,pron]).
stop_phrase('with the', [prep,det]).
stop_phrase('with those', [prep,det]).
stop_phrase('with which', [prep,pron]).
stop_phrase('with', [prep]).
stop_phrase('with,', [prep,punc]).
stop_phrase('withhold', [verb]).
stop_phrase('without', [prep]).
stop_phrase('would', [modal]).
stop_phrase('x', [head]).
stop_phrase('x,', [head,punc]).
stop_phrase('y', [head]).
stop_phrase('y.', [head,punc]).
stop_phrase('y.,', [head,punc,punc]).
stop_phrase('yet', [adv]).
stop_phrase('yet,', [adv,punc]).
stop_phrase('yield', [verb]).
stop_phrase('yielded', [verb]).
stop_phrase('yielding', [verb]).
stop_phrase('yields', [verb]).
stop_phrase('you', [pron]).
stop_phrase('z', [head]).
stop_phrase('zn', [head]).
