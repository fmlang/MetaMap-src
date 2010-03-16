
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

% File:     sentence_initial_words.pl
% Module:   sentence_initial_words
% Author:   FML
% Purpose:  Define words that are known to begin a sentence
%           immediately after a full stop with no intervening whitespace.

:- module(sentence_initial_words,[
    sentence_initial_word/1
    ]).

% These are all the words in the 2008 Medline Baseline that
% * appear immediately after a "."
% * with no intervening blank space
% * at least 4 times
% * and are no more than 6 characters long.

sentence_initial_word('The').
sentence_initial_word('In').
sentence_initial_word('We').
sentence_initial_word('This').
sentence_initial_word('These').
sentence_initial_word('There').
sentence_initial_word('It').
sentence_initial_word('Our').
sentence_initial_word('To').
sentence_initial_word('Study').
sentence_initial_word('For').
sentence_initial_word('When').
sentence_initial_word('After').
sentence_initial_word('All').
% sentence_initial_word('Cl').
sentence_initial_word('Two').
sentence_initial_word('With').
sentence_initial_word('An').
sentence_initial_word('At').
sentence_initial_word('As').
sentence_initial_word('One').
sentence_initial_word('No').
sentence_initial_word('From').
sentence_initial_word('Main').
sentence_initial_word('Of').
sentence_initial_word('Three').
sentence_initial_word('On').
sentence_initial_word('Using').
sentence_initial_word('During').
sentence_initial_word('Both').
sentence_initial_word('While').
sentence_initial_word('Since').
% sentence_initial_word('Pi').
sentence_initial_word('Based').
sentence_initial_word('Data').
sentence_initial_word('Most').
sentence_initial_word('They').
sentence_initial_word('By').
% sentence_initial_word('Ig').
% sentence_initial_word('Cr').
sentence_initial_word('Among').
sentence_initial_word('If').
% sentence_initial_word('Au').
% sentence_initial_word('Cu').
% sentence_initial_word('Th').
% sentence_initial_word('Ag').
% sentence_initial_word('Aa').
% sentence_initial_word('Na').
% sentence_initial_word('Fc').
sentence_initial_word('Some').
% sentence_initial_word('Br').
sentence_initial_word('Four').
% sentence_initial_word('Ar').
sentence_initial_word('Only').
sentence_initial_word('Many').
sentence_initial_word('Taken').
sentence_initial_word('Five').
sentence_initial_word('Mean').
% sentence_initial_word('Ca').
% sentence_initial_word('Fe').
% sentence_initial_word('Tn').
sentence_initial_word('Here').
sentence_initial_word('Other').
sentence_initial_word('Serum').
sentence_initial_word('Date').
sentence_initial_word('Design').
sentence_initial_word('More').
sentence_initial_word('Plasma').
% sentence_initial_word('Sp').
% sentence_initial_word('Tg').
sentence_initial_word('Total').
% sentence_initial_word('Pt').
sentence_initial_word('Twenty').
sentence_initial_word('Recent').
% sentence_initial_word('Vi').
sentence_initial_word('Karger').
% sentence_initial_word('Ab').
sentence_initial_word('Thus').
sentence_initial_word('Early').
sentence_initial_word('Six').
sentence_initial_word('Women').
sentence_initial_word('What').
% sentence_initial_word('Mg').
sentence_initial_word('Within').
% sentence_initial_word('Xid').
sentence_initial_word('New').
sentence_initial_word('Ten').
sentence_initial_word('Their').
% sentence_initial_word('Xa').
sentence_initial_word('Blood').
sentence_initial_word('Seven').
sentence_initial_word('Over').
sentence_initial_word('About').
sentence_initial_word('Even').
sentence_initial_word('And').
sentence_initial_word('Under').
sentence_initial_word('Types').
sentence_initial_word('Human').
sentence_initial_word('Before').
sentence_initial_word('Lewis').
sentence_initial_word('Oral').
sentence_initial_word('Coli').
sentence_initial_word('Each').
sentence_initial_word('Smart').
% sentence_initial_word('Zn').
sentence_initial_word('Future').
sentence_initial_word('Given').
% sentence_initial_word('Mn').
sentence_initial_word('Cell').
sentence_initial_word('Due').
% sentence_initial_word('He').
% sentence_initial_word('Py').
sentence_initial_word('Such').
% sentence_initial_word('Pyr').
sentence_initial_word('But').
sentence_initial_word('Case').
sentence_initial_word('Eight').
sentence_initial_word('High').
sentence_initial_word('Nine').
sentence_initial_word('Part').
% sentence_initial_word('Se').
% sentence_initial_word('Sos').
sentence_initial_word('Thirty').
sentence_initial_word('Twelve').
sentence_initial_word('Twin').
sentence_initial_word('Those').
sentence_initial_word('Use').
sentence_initial_word('Out').
sentence_initial_word('Tag').
% sentence_initial_word('Wi').
sentence_initial_word('Acute').
sentence_initial_word('Grade').
sentence_initial_word('None').
% sentence_initial_word('Po').
sentence_initial_word('Though').
sentence_initial_word('Group').
sentence_initial_word('Once').
% sentence_initial_word('Ts').
% sentence_initial_word('Ak').
% sentence_initial_word('Bbd').
% sentence_initial_word('Dn').
% sentence_initial_word('Du').
% sentence_initial_word('Li').
% sentence_initial_word('Luc').
% sentence_initial_word('Pm').
sentence_initial_word('Type').
sentence_initial_word('Level').
sentence_initial_word('Normal').
sentence_initial_word('Prior').
sentence_initial_word('Rats').
sentence_initial_word('Until').
sentence_initial_word('How').
sentence_initial_word('Taking').
% sentence_initial_word('Tm').
% sentence_initial_word('Acc').
% sentence_initial_word('Cd').
sentence_initial_word('Fifty').
sentence_initial_word('Major').
sentence_initial_word('Male').
% sentence_initial_word('Sce').
sentence_initial_word('She').
sentence_initial_word('Then').
sentence_initial_word('Visual').
sentence_initial_word('Bone').
sentence_initial_word('Direct').
sentence_initial_word('Forty').
sentence_initial_word('Key').
% sentence_initial_word('Ma').
sentence_initial_word('Method').
sentence_initial_word('Almost').
% sentence_initial_word('Bil').
sentence_initial_word('Delta').
sentence_initial_word('Eleven').
sentence_initial_word('So').
% sentence_initial_word('Tec').
sentence_initial_word('Tumor').
sentence_initial_word('Any').
sentence_initial_word('Gene').
sentence_initial_word('Health').
% sentence_initial_word('Kb').
sentence_initial_word('Low').
sentence_initial_word('Mice').
sentence_initial_word('Net').
% sentence_initial_word('Ni').
% sentence_initial_word('Thy').
sentence_initial_word('Also').
sentence_initial_word('Apart').
% sentence_initial_word('Asp').
% sentence_initial_word('Da').
% sentence_initial_word('Hb').
sentence_initial_word('Its').
sentence_initial_word('Median').
sentence_initial_word('Review').
sentence_initial_word('Single').
sentence_initial_word('Skin').
sentence_initial_word('Where').
% sentence_initial_word('Ac').
sentence_initial_word('Adult').
sentence_initial_word('Age').
sentence_initial_word('Aim').
% sentence_initial_word('Al').
% sentence_initial_word('Co').
sentence_initial_word('Is').
% sentence_initial_word('Lgs').
sentence_initial_word('Trial').
sentence_initial_word('Weight').
sentence_initial_word('Whilst').
% sentence_initial_word('Young').
% sentence_initial_word('Bcgr').
sentence_initial_word('Care').
sentence_initial_word('First').
sentence_initial_word('Good').
% sentence_initial_word('Gs').
% sentence_initial_word('Hg').
% sentence_initial_word('Le').
sentence_initial_word('Like').
sentence_initial_word('Muscle').
sentence_initial_word('Pain').
% sentence_initial_word('Px').
sentence_initial_word('Simple').
sentence_initial_word('Sodium').
sentence_initial_word('Time').
% sentence_initial_word('Tsmt').
% sentence_initial_word('Ugi').
sentence_initial_word('Urine').
sentence_initial_word('Values').
% sentence_initial_word('Abeta').
sentence_initial_word('Active').
% sentence_initial_word('Arg').
sentence_initial_word('Cells').
sentence_initial_word('Female').
sentence_initial_word('Heart').
sentence_initial_word('Levels').
sentence_initial_word('Liver').
sentence_initial_word('Much').
% sentence_initial_word('Pd').
sentence_initial_word('Pooled').
% sentence_initial_word('Ps').
sentence_initial_word('Risk').
% sentence_initial_word('Ti').
% sentence_initial_word('Tlaa').
sentence_initial_word('Up').
% sentence_initial_word('Abs').
% sentence_initial_word('Bcgs').
% sentence_initial_word('Eco').
% sentence_initial_word('Eu').
% sentence_initial_word('Fab').
sentence_initial_word('Few').
sentence_initial_word('His').
sentence_initial_word('Large').
% sentence_initial_word('Ld').
% sentence_initial_word('Ln').
sentence_initial_word('Local').
sentence_initial_word('My').
sentence_initial_word('Nearly').
sentence_initial_word('Null').
% sentence_initial_word('Ow').
sentence_initial_word('Rat').
% sentence_initial_word('Sera').
sentence_initial_word('Severe').
% sentence_initial_word('Si').
sentence_initial_word('Sixty').
sentence_initial_word('Upon').
sentence_initial_word('Whole').
sentence_initial_word('Amino').
sentence_initial_word('Atrial').
sentence_initial_word('Basal').
sentence_initial_word('Cases').
% sentence_initial_word('Cg').
% sentence_initial_word('Cre').
% sentence_initial_word('Cs').
% sentence_initial_word('Ep').
% sentence_initial_word('Gel').
% sentence_initial_word('Leu').
sentence_initial_word('Not').
sentence_initial_word('Red').
sentence_initial_word('Renal').
% sentence_initial_word('Rg').
% sentence_initial_word('Sa').
% sentence_initial_word('Su').
sentence_initial_word('Thomas').
sentence_initial_word('Tissue').
sentence_initial_word('Trends').
% sentence_initial_word('Ve').
sentence_initial_word('Venous').
sentence_initial_word('Very').
% sentence_initial_word('Xs').
sentence_initial_word('Air').
sentence_initial_word('Aortic').
% sentence_initial_word('Bd').
% sentence_initial_word('Cal').
sentence_initial_word('Common').
sentence_initial_word('Dimer').
sentence_initial_word('Either').
sentence_initial_word('Every').
sentence_initial_word('Family').
sentence_initial_word('Form').
sentence_initial_word('Genta').
% sentence_initial_word('Groups').
sentence_initial_word('Growth').
% sentence_initial_word('Hha').
sentence_initial_word('Higher').
sentence_initial_word('John').
sentence_initial_word('Less').
% sentence_initial_word('Mo').
% sentence_initial_word('Nd').
sentence_initial_word('Ninety').
% sentence_initial_word('Pe').
% sentence_initial_word('Pem').
% sentence_initial_word('Pro').
sentence_initial_word('Public').
% sentence_initial_word('Ser').
sentence_initial_word('Small').
% sentence_initial_word('Sn').
sentence_initial_word('Tumors').
% sentence_initial_word('Va').
sentence_initial_word('Acid').
% sentence_initial_word('Arnt').
% sentence_initial_word('Ba').
sentence_initial_word('Brain').
% sentence_initial_word('Cad').
sentence_initial_word('Cancer').
% sentence_initial_word('Cbl').
% sentence_initial_word('Glu').
% sentence_initial_word('Ige').
% sentence_initial_word('Ighb').
% sentence_initial_word('Ighn').
sentence_initial_word('Novel').
sentence_initial_word('Proper').
sentence_initial_word('Rapid').
sentence_initial_word('Report').
sentence_initial_word('Serial').
% sentence_initial_word('Tl').
sentence_initial_word('Toxic').
sentence_initial_word('Water').
sentence_initial_word('Wound').
sentence_initial_word('Yet').
% sentence_initial_word('Ytir').
% sentence_initial_word('Asn').
% sentence_initial_word('Bm').
sentence_initial_word('Bovine').
% sentence_initial_word('Cho').
sentence_initial_word('Drug').
sentence_initial_word('Drugs').
% sentence_initial_word('Ealpha').
% sentence_initial_word('Ec').
% sentence_initial_word('Epo').
sentence_initial_word('Eye').
% sentence_initial_word('Fas').
sentence_initial_word('Fetal').
% sentence_initial_word('Focal').
% sentence_initial_word('Free').
% sentence_initial_word('Grx').
sentence_initial_word('Having').
% sentence_initial_word('Id').
sentence_initial_word('Info').
% sentence_initial_word('Km').
sentence_initial_word('Late').
sentence_initial_word('Left').
% sentence_initial_word('Lu').
% sentence_initial_word('Lx').
sentence_initial_word('Marked').
% sentence_initial_word('Max').
sentence_initial_word('Men').
sentence_initial_word('Mental').
sentence_initial_word('Modern').
sentence_initial_word('Nerve').
sentence_initial_word('Now').
sentence_initial_word('Open').
% sentence_initial_word('Ov').
sentence_initial_word('Owing').
% sentence_initial_word('Pac').
sentence_initial_word('Phase').
% sentence_initial_word('Pu').
% sentence_initial_word('Ras').
% sentence_initial_word('Ro').
% sentence_initial_word('Ru').
sentence_initial_word('Scan').
sentence_initial_word('Search').
sentence_initial_word('Short').
sentence_initial_word('Side').
sentence_initial_word('Sign').
sentence_initial_word('Spinal').
sentence_initial_word('Target').
% sentence_initial_word('Te').
sentence_initial_word('Test').
% sentence_initial_word('Thr').
sentence_initial_word('Upper').
sentence_initial_word('Virus').
sentence_initial_word('White').
% sentence_initial_word('Akt').
sentence_initial_word('Beside').
sentence_initial_word('Best').
sentence_initial_word('Beta').
sentence_initial_word('Bile').
sentence_initial_word('Bind').
sentence_initial_word('Breast').
% sentence_initial_word('Bu').
% sentence_initial_word('Crk').
sentence_initial_word('Cyclic').
sentence_initial_word('Daily').
sentence_initial_word('Day').
% sentence_initial_word('Db').
sentence_initial_word('Death').
% sentence_initial_word('Dm').
% sentence_initial_word('Epi').
% sentence_initial_word('Ex').
sentence_initial_word('Flow').
sentence_initial_word('Food').
sentence_initial_word('Full').
% sentence_initial_word('Gaw').
% sentence_initial_word('Ge').
sentence_initial_word('Gross').
% sentence_initial_word('Hop').
% sentence_initial_word('Hth').
sentence_initial_word('Index').
% sentence_initial_word('Ki').
% sentence_initial_word('Kit').
sentence_initial_word('Laser').
sentence_initial_word('Light').
sentence_initial_word('Lisa').
% sentence_initial_word('Lsa').
sentence_initial_word('Lumbar').
sentence_initial_word('Lung').
% sentence_initial_word('Lys').
sentence_initial_word('Mass').
sentence_initial_word('Mouse').
% sentence_initial_word('Mur').
sentence_initial_word('Newer').
sentence_initial_word('Often').
sentence_initial_word('Or').
% sentence_initial_word('Pf').
sentence_initial_word('Plants').
sentence_initial_word('Poly').
% sentence_initial_word('Pvu').
sentence_initial_word('Repeat').
% sentence_initial_word('Sh').
% sentence_initial_word('Shc').
sentence_initial_word('Spleen').
% sentence_initial_word('Src').
% sentence_initial_word('Stop').
sentence_initial_word('Stress').
sentence_initial_word('Swiss').
% sentence_initial_word('Tat').
% sentence_initial_word('Tb').
sentence_initial_word('Tests').
% sentence_initial_word('Tf').
sentence_initial_word('Thanks').
sentence_initial_word('That').
% sentence_initial_word('Torr').
sentence_initial_word('Trials').
sentence_initial_word('United').
sentence_initial_word('Unlike').
% sentence_initial_word('Vmax').
sentence_initial_word('Which').
% sentence_initial_word('Xe').
% sentence_initial_word('Yaa').
% sentence_initial_word('Abetao').
% sentence_initial_word('Actin').
% sentence_initial_word('Ad').
% sentence_initial_word('Ah').
sentence_initial_word('Alpha').
sentence_initial_word('Are').
sentence_initial_word('Basic').
% sentence_initial_word('Bb').
% sentence_initial_word('Bcll').
sentence_initial_word('Beyond').
% sentence_initial_word('Bmo').
% sentence_initial_word('Cb').
sentence_initial_word('Change').
sentence_initial_word('Chest').
% sentence_initial_word('Cn').
sentence_initial_word('Cohort').
sentence_initial_word('Color').
% sentence_initial_word('Con').
sentence_initial_word('Corixa').
sentence_initial_word('Cost').
sentence_initial_word('Costs').
sentence_initial_word('Crude').
sentence_initial_word('Deaths').
% sentence_initial_word('Do').
sentence_initial_word('Dogs').
sentence_initial_word('Donor').
sentence_initial_word('Double').
% sentence_initial_word('Dy').
sentence_initial_word('Effect').
sentence_initial_word('Eighty').
% sentence_initial_word('Ek').
% sentence_initial_word('End').
sentence_initial_word('Except').
sentence_initial_word('Factor').
% sentence_initial_word('Gd').
% sentence_initial_word('Glc').
sentence_initial_word('Global').
sentence_initial_word('Hair').
sentence_initial_word('Half').
sentence_initial_word('Hence').
sentence_initial_word('Hiatal').
% sentence_initial_word('Ia').
sentence_initial_word('Iron').
sentence_initial_word('Lack').
sentence_initial_word('Life').
sentence_initial_word('Ligand').
sentence_initial_word('Little').
sentence_initial_word('Loss').
sentence_initial_word('May').
sentence_initial_word('Minor').
% sentence_initial_word('Mlsa').
% sentence_initial_word('Mlx').
sentence_initial_word('Model').
sentence_initial_word('Nitric').
% sentence_initial_word('Oita').
sentence_initial_word('Older').
% sentence_initial_word('Oocyte').
sentence_initial_word('Orally').
sentence_initial_word('Oxygen').
% sentence_initial_word('Pase').
sentence_initial_word('Paul').
sentence_initial_word('Paulo').
% sentence_initial_word('Phage').
sentence_initial_word('Pilot').
sentence_initial_word('Plain').
% sentence_initial_word('Pol').
% sentence_initial_word('Raf').
sentence_initial_word('Rate').
% sentence_initial_word('Re').
sentence_initial_word('Result').
% sentence_initial_word('Rev').
sentence_initial_word('Safety').
sentence_initial_word('Saline').
% sentence_initial_word('Sc').
% sentence_initial_word('Scid').
sentence_initial_word('Sexual').
% sentence_initial_word('Sle').
sentence_initial_word('Speech').
% sentence_initial_word('Sv').
sentence_initial_word('Task').
sentence_initial_word('Today').
sentence_initial_word('Trauma').
sentence_initial_word('Travel').
% sentence_initial_word('Typhi').
sentence_initial_word('Unless').
sentence_initial_word('Uptake').
sentence_initial_word('Wistar').
% sentence_initial_word('Wt').
% sentence_initial_word('Zr').
% sentence_initial_word('Zs').
% sentence_initial_word('Aanti').
sentence_initial_word('Above').
sentence_initial_word('Across').
% sentence_initial_word('Ada').
sentence_initial_word('Adding').
sentence_initial_word('Ages').
sentence_initial_word('Aging').
% sentence_initial_word('Alb').
sentence_initial_word('Allos').
sentence_initial_word('Anemia').
sentence_initial_word('Area').
sentence_initial_word('Assays').
sentence_initial_word('Asthma').
% sentence_initial_word('Bk').
sentence_initial_word('Body').
% sentence_initial_word('Cae').
sentence_initial_word('Can').
% sentence_initial_word('Cas').
sentence_initial_word('Causes').
% sentence_initial_word('Cc').
sentence_initial_word('Close').
sentence_initial_word('Closed').
% sentence_initial_word('Csk').
sentence_initial_word('Days').
% sentence_initial_word('Del').
% sentence_initial_word('Dk').
% sentence_initial_word('Does').
% sentence_initial_word('Dot').
sentence_initial_word('Dr').
sentence_initial_word('Dual').
% sentence_initial_word('Et').
sentence_initial_word('Ethyl').
% sentence_initial_word('Exp').
sentence_initial_word('Fast').
sentence_initial_word('Focus').
sentence_initial_word('Forest').
sentence_initial_word('Frozen').
sentence_initial_word('Fusion').
% sentence_initial_word('Gal').
sentence_initial_word('Gamma').
% sentence_initial_word('Gbeta').
sentence_initial_word('German').
sentence_initial_word('Giant').
% sentence_initial_word('Glaw').
% sentence_initial_word('Gly').
sentence_initial_word('Green').
sentence_initial_word('Guinea').
% sentence_initial_word('Hae').
% sentence_initial_word('Hal').
sentence_initial_word('Hand').
sentence_initial_word('Hans').
sentence_initial_word('Head').
sentence_initial_word('Hind').
sentence_initial_word('Hip').
% sentence_initial_word('Iga').
% sentence_initial_word('Igb').
% sentence_initial_word('Ikappa').
sentence_initial_word('Infant').
sentence_initial_word('Intact').
sentence_initial_word('Joint').
sentence_initial_word('Jones').
% sentence_initial_word('Ka').
sentence_initial_word('Kappa').
% sentence_initial_word('Ku').
sentence_initial_word('Kurt').
sentence_initial_word('Larger').
sentence_initial_word('Last').
sentence_initial_word('Limb').
sentence_initial_word('Linear').
% sentence_initial_word('Lip').
sentence_initial_word('Lipid').
% sentence_initial_word('Lo').
sentence_initial_word('Load').
sentence_initial_word('Long').
sentence_initial_word('Lymph').
sentence_initial_word('Mature').
% sentence_initial_word('Meb').
sentence_initial_word('Met').
sentence_initial_word('Mild').
sentence_initial_word('Models').
% sentence_initial_word('Mono').
sentence_initial_word('Moving').
% sentence_initial_word('Msp').
sentence_initial_word('Nasal').
sentence_initial_word('Needle').
sentence_initial_word('Newly').
% sentence_initial_word('Ng').
% sentence_initial_word('Nor').
% sentence_initial_word('Np').
sentence_initial_word('Number').
sentence_initial_word('Nurses').
sentence_initial_word('Nuvelo').
sentence_initial_word('Peak').
sentence_initial_word('Pelvic').
sentence_initial_word('People').
% sentence_initial_word('Ph').
% sentence_initial_word('Plgn').
sentence_initial_word('Poor').
sentence_initial_word('Port').
sentence_initial_word('Post').
sentence_initial_word('Prompt').
sentence_initial_word('Proton').
% sentence_initial_word('Pur').
% sentence_initial_word('Pvt').
% sentence_initial_word('Pylori').
sentence_initial_word('Raised').
sentence_initial_word('Rates').
sentence_initial_word('Rectal').
% sentence_initial_word('Rgs').
% sentence_initial_word('Rh').
% sentence_initial_word('Rho').
% sentence_initial_word('Sal').
sentence_initial_word('Sample').
% sentence_initial_word('San').
sentence_initial_word('Scans').
sentence_initial_word('Second').
sentence_initial_word('Sepsis').
sentence_initial_word('Sex').
sentence_initial_word('Shock').
sentence_initial_word('Smooth').
sentence_initial_word('South').
sentence_initial_word('Sperm').
% sentence_initial_word('Spt').
% sentence_initial_word('Sss').
sentence_initial_word('State').
sentence_initial_word('Stroke').
sentence_initial_word('Strong').
sentence_initial_word('Submit').
% sentence_initial_word('Taq').
sentence_initial_word('Thin').
sentence_initial_word('Timely').
sentence_initial_word('Titles').
sentence_initial_word('Tracer').
sentence_initial_word('Train').
sentence_initial_word('True').
sentence_initial_word('Unique').
sentence_initial_word('Uracil').
% sentence_initial_word('Vch').
sentence_initial_word('Venus').
sentence_initial_word('Viable').
sentence_initial_word('Viral').
sentence_initial_word('Volume').
sentence_initial_word('Washed').
sentence_initial_word('Weekly').
sentence_initial_word('Well').
sentence_initial_word('Who').
sentence_initial_word('Wide').
% sentence_initial_word('Ws').
% sentence_initial_word('Xbal').
sentence_initial_word('Zinc').

% Added uppercase words after discussion with Lou Knecht
sentence_initial_word('CASE').
sentence_initial_word('THE').
sentence_initial_word('MAIN').
sentence_initial_word('DESIGN').
sentence_initial_word('STUDY').
sentence_initial_word('DATA').
sentence_initial_word('KEY').
sentence_initial_word('AIM').
sentence_initial_word('IN').
sentence_initial_word('MRI').
sentence_initial_word('THESE').
sentence_initial_word('IT').
sentence_initial_word('REPORT').
sentence_initial_word('SIGN').
sentence_initial_word('WE').
sentence_initial_word('HIV').
sentence_initial_word('AND').
sentence_initial_word('MIXED').
sentence_initial_word('STOP').
sentence_initial_word('FROM').
sentence_initial_word('METHOD').
sentence_initial_word('THIS').
sentence_initial_word('ACE').
sentence_initial_word('FOR').
sentence_initial_word('SYSTEM').
sentence_initial_word('WITH').
