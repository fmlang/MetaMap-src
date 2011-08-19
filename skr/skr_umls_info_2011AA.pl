
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
	( IllegalSources == [] ->
	  true
	; format('~nFatal error: Illegal source(s) ~p~n',[IllegalSources]),
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
	umls_source(_, RootSource),
	!.

umls_versioned_source(VersionedSource) :-
	umls_source(VersionedSource, _),
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
	find_illegal_sts(STs, IllegalSTs),
	( IllegalSTs == [] ->
	  true
	; format('~nFatal error: Illegal semantic type(s) ~p~n',[IllegalSTs]),
	  !,
	  fail
	).

find_illegal_sts([], []).
find_illegal_sts([First|Rest], IllegalSTs) :-
	is_abbrev_semtype(First),
	!,
	find_illegal_sts(Rest, IllegalSTs).
find_illegal_sts([First|Rest], [First|RestIllegalSTs]) :-
	find_illegal_sts(Rest, RestIllegalSTs).

