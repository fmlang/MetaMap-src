
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

% File:	    skr_umls_info_2011AA.pl
% Module:   SKR
% Author:   Lan
% Purpose:  This module defines year-specific UMLS information such as
%           source codes. See also semtype_translationXX for semantic types.


:- module(skr_umls_info_2011AA, [
	verify_sources/1,
	verify_sts/1,
	convert_to_root_sources/2
    ]).

:- use_module(skr_lib(semtype_translation_2011AA), [
	is_abbrev_semtype/1
    ]).

/* verify_sources(+Sources)
   find_illegal_sources(+Sources, -IllegalSources)

verify_sources/1 checks to make sure all elements in the Sources list
are valid sources. It displays any that are not and then fails.
find_illegal_sources/2 uses the factual predicate umls_source/1 to find
the IllegalSources in Sources. */

verify_sources(Sources) :-
    find_illegal_sources(Sources,IllegalSources),
    (IllegalSources==[] ->
	true
    ;   format('~nFatal error: Illegal source(s) ~p~n',[IllegalSources]),
	!,
	fail
    ).

find_illegal_sources([], []).
find_illegal_sources([First|Rest], IllegalSources) :-
	umls_source(First),
	!,
	find_illegal_sources(Rest, IllegalSources).
find_illegal_sources([First|Rest], [First|RestIllegalSources]) :-
	find_illegal_sources(Rest, RestIllegalSources).

/* umls_source(?VersionedSource,?RootSource)
   umls_source(?Source)
   umls_root_source(?RootSource)
   umls_versioned_source(?VersionedSource)
   convert_to_root_sources(+Sources, -RootSources)

umls_source/2 is a factual predicate defining VersionedSource and Source
(also called the root source name). umls_root_source/1 and
umls_versioned_source/1 define either the root or versioned source names,
and umls_source/1 defines either root or versioned source names.
convert_to_root_sources/2 converts a list of Sources (either versioned or
root) to a list of RootSources, dropping any elements of Sources that are
neither. */

umls_source(Source) :-
	( umls_root_source(Source)
	; umls_versioned_source(Source)
	),
	!.

umls_root_source(RootSource) :-
	umls_source(_,RootSource),
	!.

umls_versioned_source(VersionedSource) :-
	umls_source(VersionedSource,_),
	!.

convert_to_root_sources([], []).
convert_to_root_sources([First|Rest], [First|ConvertedRest]) :-
	umls_root_source(First),
	!,
	convert_to_root_sources(Rest, ConvertedRest).
convert_to_root_sources([First|Rest], [ConvertedFirst|ConvertedRest]) :-
	umls_source(First, ConvertedFirst),
	!,
	convert_to_root_sources(Rest, ConvertedRest).
convert_to_root_sources([_First|Rest], ConvertedRest) :-
	convert_to_root_sources(Rest, ConvertedRest).

/* verify_sts(+STs)
   find_illegal_sts(+STs, -IllegalSTs)

verify_sts/1 checks to make sure all elements in the STs list
are valid semantic types. It displays any that are not and then fails.
find_illegal_sts/2 uses the factual predicate is_abbrev_semtype/1 to find
the IllegalSTs in STs. */

verify_sts(STs) :-
    find_illegal_sts(STs,IllegalSTs),
    (IllegalSTs==[] ->
	true
    ;   format('~nFatal error: Illegal semantic type(s) ~p~n',[IllegalSTs]),
	!,
	fail
    ).

find_illegal_sts([],[]) :-
    !.
find_illegal_sts([First|Rest],IllegalSTs) :-
    is_abbrev_semtype(First),
    !,
    find_illegal_sts(Rest,IllegalSTs).
find_illegal_sts([First|Rest],[First|RestIllegalSTs]) :-
    find_illegal_sts(Rest,RestIllegalSTs).


% These 2008AA sources were obtained from
% /nfsvol/nls/umls/metamorphosys2007AA/Meta-subset.noama/2007AA/META/MRSAB
% by running the command
% cut -d"|" -f3-4 MRSAB \
%     | sed -e 's/^/umls_source('\''/' \
%           -e 's/|/'\'','\''/'        \
%           -e 's/$/'\'')./'

umls_source('AIR93',                'AIR').
umls_source('ALT2009',              'ALT').
umls_source('AOD2000',              'AOD').
umls_source('AOT2003',              'AOT').
umls_source('BI98',                 'BI').
umls_source('CCPSS99',              'CCPSS').
umls_source('CCS2005',              'CCS').
umls_source('CHV2010_10',           'CHV').
umls_source('COSTAR_89-95',         'COSTAR').
umls_source('CPM2003',              'CPM').
umls_source('CPT01SP',              'CPTSP').
umls_source('CPT2005',              'CPT').
umls_source('CPT2011',              'CPT').
umls_source('CSP2006',              'CSP').
umls_source('CST95',                'CST').
umls_source('DDB00',                'DDB').
umls_source('DMDICD10_1995',        'DMDICD10').
umls_source('DMDUMD_1996',          'DMDUMD').
umls_source('DSM3R_1987',           'DSM3R').
umls_source('DSM4_1994',            'DSM4').
umls_source('DXP94',                'DXP').
umls_source('FMA3_1',               'FMA').
umls_source('GO2010_04_01',         'GO').
umls_source('GS_2011_02_08',        'GS').
umls_source('HCDT2011-2012',        'HCDT').
umls_source('HCPCS2011',            'HCPCS').
umls_source('HCPT2011',             'HCPT').
umls_source('HHC2003',              'HHC').
umls_source('HL7V2.5_2003_08_30',   'HL7V2.5').
umls_source('HL7V3.0_2006_05',      'HL7V3.0').
umls_source('HLREL_1998',           'HLREL').
umls_source('HUGO2010_05',          'HUGO').
umls_source('ICD10AE_1998',         'ICD10AE').
umls_source('ICD10AMAE_2000',       'ICD10AMAE').
umls_source('ICD10AM_2000',         'ICD10AM').
umls_source('ICD10CM_2011_01',      'ICD10CM').
umls_source('ICD10DUT_200403',      'ICD10DUT').
umls_source('ICD10PCS_2011',        'ICD10PCS').
umls_source('ICD10_1998',           'ICD10').
umls_source('ICD9CM_1998',          'ICD9CM').
umls_source('ICD9CM_2005',          'ICD9CM').
umls_source('ICD9CM_2011',          'ICD9CM').
umls_source('ICF-CY_2008',          'ICF-CY').
umls_source('ICF_2008_12_19',       'ICF').
umls_source('ICNP2_0',              'ICNP').
umls_source('ICPC2EDUT_200203',     'ICPC2EDUT').
umls_source('ICPC2EENG_200203',     'ICPC2EENG').
umls_source('ICPC2ICD10DUT_200412', 'ICPC2ICD10DUT').
umls_source('ICPC2ICD10ENG_200412', 'ICPC2ICD10ENG').
umls_source('ICPC2P_2005',          'ICPC2P').
umls_source('ICPC93',               'ICPC').
umls_source('ICPCBAQ_1993',         'ICPCBAQ').
umls_source('ICPCDAN_1993',         'ICPCDAN').
umls_source('ICPCDUT_1993',         'ICPCDUT').
umls_source('ICPCFIN_1993',         'ICPCFIN').
umls_source('ICPCFRE_1993',         'ICPCFRE').
umls_source('ICPCGER_1993',         'ICPCGER').
umls_source('ICPCHEB_1993',         'ICPCHEB').
umls_source('ICPCHUN_1993',         'ICPCHUN').
umls_source('ICPCITA_1993',         'ICPCITA').
umls_source('ICPCNOR_1993',         'ICPCNOR').
umls_source('ICPCPOR_1993',         'ICPCPOR').
umls_source('ICPCSPA_1993',         'ICPCSPA').
umls_source('ICPCSWE_1993',         'ICPCSWE').
umls_source('JABL99',               'JABL').
umls_source('KCD5_2008',            'KCD5').
umls_source('LCH90',                'LCH').
umls_source('LNC215',               'LNC').
umls_source('LNC234',               'LNC').
umls_source('LNC234_BRADEN_1988',   'LNC_BRADEN').
umls_source('LNC234_CAM_2003',      'LNC_CAM').
umls_source('LNC234_FLACC_1997',    'LNC_FLACC').
umls_source('LNC234_MDS20',         'LNC_MDS20').
umls_source('LNC234_MDS30',         'LNC_MDS30').
umls_source('LNC234_OASIS_2002',    'LNC_OASIS').
umls_source('LNC234_PHQ_9_1999',    'LNC_PHQ_9').
umls_source('LNC234_RHO_2008',      'LNC_RHO').
umls_source('LNC234_WHO_2006',      'LNC_WHO').
umls_source('MBD11',                'MBD').
umls_source('MCM92',                'MCM').
umls_source('MDDB_2011_02_02',      'MDDB').
umls_source('MDR13_1',              'MDR').
umls_source('MDRCZE13_1',           'MDRCZE').
umls_source('MDRDUT13_1',           'MDRDUT').
umls_source('MDRFRE13_1',           'MDRFRE').
umls_source('MDRGER13_1',           'MDRGER').
umls_source('MDRITA13_1',           'MDRITA').
umls_source('MDRJPN13_1',           'MDRJPN').
umls_source('MDRPOR13_1',           'MDRPOR').
umls_source('MDRSPA13_1',           'MDRSPA').
umls_source('MED11',                'MED').
umls_source('MEDCIN3_2010_12_14',   'MEDCIN').
umls_source('MEDLINEPLUS_20110108', 'MEDLINEPLUS').
umls_source('MMSL_2011_02_01',      'MMSL').
umls_source('MMX_2011_02_07',       'MMX').
umls_source('MSH2011_2011_02_14',   'MSH').
umls_source('MSHCZE2011',           'MSHCZE').
umls_source('MSHDUT2005',           'MSHDUT').
umls_source('MSHFIN2008',           'MSHFIN').
umls_source('MSHFRE2011',           'MSHFRE').
umls_source('MSHGER2011',           'MSHGER').
umls_source('MSHITA2011',           'MSHITA').
umls_source('MSHJPN2008',           'MSHJPN').
umls_source('MSHLAV2011',           'MSHLAV').
umls_source('MSHPOL2011',           'MSHPOL').
umls_source('MSHPOR2011',           'MSHPOR').
umls_source('MSHRUS2011',           'MSHRUS').
umls_source('MSHSCR2011',           'MSHSCR').
umls_source('MSHSPA2011',           'MSHSPA').
umls_source('MSHSWE2010',           'MSHSWE').
umls_source('MTH',                  'MTH').
umls_source('MTHCH2011',            'MTHCH').
umls_source('MTHFDA_2011_02_01',    'MTHFDA').
umls_source('MTHHH2011',            'MTHHH').
umls_source('MTHHL7V2.5_2003_08',   'MTHHL7V2.5').
umls_source('MTHICD9_2011',         'MTHICD9').
umls_source('MTHICPC2EAE_200203',   'MTHICPC2EAE').
umls_source('MTHICPC2ICD107B_0412', 'MTHICPC2ICD107B').
umls_source('MTHICPC2ICD10AE_0412', 'MTHICPC2ICD10AE').
umls_source('MTHMST2001',           'MTHMST').
umls_source('MTHMSTFRE_2001',       'MTHMSTFRE').
umls_source('MTHMSTITA_2001',       'MTHMSTITA').
umls_source('MTHSPL_2011_02_25',    'MTHSPL').
umls_source('NAN2004',              'NAN').
umls_source('NCBI2010_04_29',       'NCBI').
umls_source('NCI2010_02D',          'NCI').
umls_source('NCISEER_1999',         'NCISEER').
umls_source('NDDF_2011_02_16',      'NDDF').
umls_source('NDFRT_2011_03_07',     'NDFRT').
umls_source('NEU99',                'NEU').
umls_source('NIC2005',              'NIC').
umls_source('NLM-MED',              'NLM-MED').
umls_source('NOC3',                 'NOC').
umls_source('OMIM2010_04_08',       'OMIM').
umls_source('OMS2005',              'OMS').
umls_source('PCDS97',               'PCDS').
umls_source('PDQ2007_02',           'PDQ').
umls_source('PNDS2011',             'PNDS').
umls_source('PPAC98',               'PPAC').
umls_source('PSY2004',              'PSY').
umls_source('QMR96',                'QMR').
umls_source('RAM99',                'RAM').
umls_source('RCD99',                'RCD').
umls_source('RCDAE_1999',           'RCDAE').
umls_source('RCDSA_1999',           'RCDSA').
umls_source('RCDSY_1999',           'RCDSY').
umls_source('RXNORM_10AB_110307F',  'RXNORM').
umls_source('SCTSPA_2010_10_31',    'SCTSPA').
umls_source('SNM2',                 'SNM').
umls_source('SNMI98',               'SNMI').
umls_source('SNOMEDCT_2010_07_31',  'SNOMEDCT').
umls_source('SNOMEDCT_2011_01_31',  'SNOMEDCT').
umls_source('SPN2003',              'SPN').
umls_source('SRC',                  'SRC').
umls_source('TKMT2010',             'TKMT').
umls_source('ULT93',                'ULT').
umls_source('UMD2011',              'UMD').
umls_source('USPMG_2004',           'USPMG').
umls_source('UWDA173',              'UWDA').
umls_source('VANDF_2011_02_03',     'VANDF').
umls_source('WHO97',                'WHO').
umls_source('WHOFRE_1997',          'WHOFRE').
umls_source('WHOGER_1997',          'WHOGER').
umls_source('WHOPOR_1997',          'WHOPOR').
umls_source('WHOSPA_1997',          'WHOSPA').
