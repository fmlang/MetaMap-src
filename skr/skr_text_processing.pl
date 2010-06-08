
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

% File:     skr_text_processing.pl
% Module:   SKR
% Author:   Lan
% Purpose:  Provide (high-level) text processing


:- module(skr_text_processing,[
	extract_sentences/6,
	get_skr_text/1,
	text_field/1
	% is_sgml_text/1,
	% moved from labeler % needed by sgml_extractor?
	% warn_remove_non_ascii_from_input_lines/2
    ]).

:- use_module(text(text_objects),[
	find_and_coordinate_sentences/5
    ]).

% :- use_module(text(sgml_extractor),[
% 	extract_fields_from_sgml/2,
% 	initialize_sgml_extractor/0
%     ]).

:- use_module(skr(skr_utilities),[
	skr_begin_write/1,
	skr_end_write/1,
	skr_write_string/1
    ]).

:- use_module(skr_lib(nls_strings),[
	replace_nonprints_in_strings/2,
	replace_tabs_in_strings/2,
	split_string/4,
	split_string_completely/3,
	trim_whitespace/2
    ]).

:- use_module(skr_lib(ctypes),[
	is_lower/1
    ]).

:- use_module(skr_lib(nls_io),[
	fget_lines_until_skr_break/2,
	fget_non_ws_only_line/2
    ]).


:- use_module(skr_lib(sicstus_utils),[
	lower/2,
	lower_chars/2,
	ttyflush/0
    ]).

:- use_module(library(lists),[
	append/2,
	rev/2
    ]).

/* get_skr_text(-Lines)
   get_skr_text(+InputStream, -Lines)

get_skr_text/1 gets Lines from current input by first skipping "blank" lines
and then reading until a "natural" breaking point. Currently, the following are
such SKR breaking points:
  a "blank" line; and
Here, a "blank" line is one containing only whitespace characters (if any).
get_skr_text/2 has input parameter InputStream. */

get_skr_text(Lines) :-
	maybe_print_prompt,
	current_input(InputStream),
	get_skr_text_1(InputStream, Lines).

get_skr_text_1(InputStream, [First|Rest]) :-
	fget_non_ws_only_line(InputStream, First),
	!,
	fget_lines_until_skr_break(InputStream,Rest).
get_skr_text_1(_, []).

% print the "|:" read prompt iff MetaMap is being used interactively,
% i.e., the user is interactively typing in input.
% This seems to be a QP/SP difference.
maybe_print_prompt :-
	( seeing(user_input) ->
	  prompt(Prompt, Prompt),
	  format(user_error, '~w', [Prompt])
	; true
	).

/*    extract_sentences(+Lines, -InputType, -Sentences, -CoordinateSentences, -AAs)

extract_sentences/4 creates Sentences and CoordinatedSentences from the strings
in Lines.
Lines can be in the following forms:
  a. MEDLINE citation
     the first tokens on the first line must be "UI" and "-"
%%%   b. loosely fielded
%%%      the first line must begin with "*"
%%%      any line beginning with "*" is a field id; field data begins a new line
%%%   d. SGML-tagged text
%%%      the first line must begin with "<"
%%%   e. Project description
%%%      the first line must begin with "--PROJECT NUMBER"
%%%   f. previously labeled utterance (for backward compatibility)
%%%      the first line must be of the form "[ label ] text"
  g. arbitrary text

The computed InputType is one of
  citation
%%%   loose
  smart
%%%   sgml
%%%   project
%%%   lutt
  simple

Sentences is a list of tokens of the form
  tok(<type>,<*>,<*>,<position>)
CoordinatedSentences is a list of terms of the form
  tok(<type>,<*>,<*>,<position>,<original-position>)
See text_object_tokens:form_field_tokens/2 for a complete description of the
tokens.

The expansion is accomplished by using text_object facilities such as
acronym/abbreviation discovery.

*/

extract_sentences(Lines0, InputType, Sentences, CoordinatedSentences, AAs, Lines) :-
	replace_tabs_in_strings(Lines0, Lines1),
	replace_nonprints_in_strings(Lines1, Lines2),
	( is_medline_citation(Lines2) ->
	  extract_coord_sents_from_citation(Lines2, Sentences, CoordinatedSentences, AAs),
	  InputType = citation
	; is_smart_fielded(Lines2) ->
	  extract_coord_sents_from_smart(Lines2, Sentences, CoordinatedSentences, AAs),
	  InputType = smart
	; form_dummy_citation(Lines2, CitationLines),
	  extract_coord_sents_from_citation(CitationLines, Sentences,
					    CoordinatedSentences, AAs),
	  InputType = simple
	),
	Lines = Lines2,
	!.

is_medline_citation([First|_]) :-
	split_string_completely(First, " ", Tokens0),
	remove_nulls(Tokens0, Tokens),
	Tokens = [String1, String2|_],
	lower_chars(String1, LowerString1),
	atom_codes(LowerAtom1, LowerString1),
	lower_chars(String2, LowerString2),
	atom_codes(LowerAtom2, LowerString2),
	medline_citation_indicator(LowerAtom1, LowerAtom2).

remove_nulls([], []).
remove_nulls([First|Rest], Result) :-
	( First == [] ->
	  RestResult = Result
	; Result = [First|RestResult]
	),
	remove_nulls(Rest, RestResult).

medline_citation_indicator('pmid-', _).
medline_citation_indicator(pmid,  '-').
medline_citation_indicator(ui,    '-').
medline_citation_indicator('ui-',   _).

is_smart_fielded([First,_|_]) :-
    append(".",_,First).

/* form_dummy_citation(+Lines, -CitationLines)

form_dummy_citation/1 turns Lines into a pseudo MEDLINE field, TX. */

form_dummy_citation([], []).
form_dummy_citation([First|Rest], ["UI  - 00000000",ModifiedFirst|PaddedRest]) :-
	append("TX  - ", First, ModifiedFirst),
	padding_string(Padding),
	pad_lines(Rest, Padding, PaddedRest),
	!.

pad_lines([], _Padding, []).
pad_lines([First|Rest], Padding, [PaddedFirst|PaddedRest]) :-
	append(Padding, First, PaddedFirst),
	pad_lines(Rest, Padding, PaddedRest).

padding_string("      ").

/* 
   extract_coord_sents_from_citation(+CitationLines, -Sentences,
                                     -CoordinatedSentences, AAs)
*/

extract_coord_sents_from_citation(CitationLines, Sentences,
				  CoordinatedSentences, AAs) :-
    extract_all_fields(CitationLines, CitationFields),
    (   member([FieldIDString,Field], CitationFields),
	lower_chars(FieldIDString, LowerFieldIDString),
	atom_codes(LowerFieldIDAtom, LowerFieldIDString), 
	medline_citation_indicator(LowerFieldIDAtom, _) ->
        extract_ui(Field, UI)
    ;   UI="00000000"
    ),
    extract_coord_sents_from_fields(UI, CitationFields, Sentences,
				    CoordinatedSentences, AAs),
    !.

%%% /* extract_utterances_from_sgml(+CitationLines, -Utterances)
%%% 
%%% extract_utterances_from_sgml/2 processes the strings CitationLines
%%% producing Utterances, a list of strings consisting of labeled utterances
%%% from the citation text fields.
%%% extract_utterances_from_sgml/3 also computes ExpandedUtterances with
%%% AAs expanded. */
%%% 
%%% extract_utterances_from_sgml(CitationLines,
%%% 			     Sentences, CoordinatedSentences, AAs, [RealText]) :-
%%%     extract_fields_from_sgml(CitationLines,CitationFields0),
%%%     standardize_field_names(CitationFields0,CitationFields),
%%% % temp
%%% %format('CitationFields:~n~p~n~n',[CitationFields]),
%%%     (select_field("DOC",CitationFields,UIField) ->
%%%         extract_ui(UIField,UI)
%%%     ;   UI="00000000"
%%%     ),
%%%     extract_real_text(CitationFields, RealText),
%%%     form_dummy_citation([RealText], RealCitationLines),
%%%     extract_coord_sents_from_citation(RealCitationLines, Sentences, CoordinatedSentences, AAs),
%%%     % extract_utterances_from_text_fields(UI,CitationFields,Utterances),
%%%     !.
%%% 
%%% extract_real_text(CitationFields, RealText) :-
%%% 	extract_real_text_from_each_field(CitationFields, TextFields),
%%% 	append(TextFields, RealText).
%%% 
%%% extract_real_text_from_each_field([], []).
%%% extract_real_text_from_each_field([H|T], [TextH|TextT]) :-
%%%         H = [_FieldType, TextHStrings],
%%% 	atom_codes_list(TextHAtoms, TextHStrings),
%%% 	concat_atom(TextHAtoms, ' ', TextHAtomsWithBlanks),
%%% 	atom_codes(TextHAtomsWithBlanks, TextH),
%%% 	extract_real_text_from_each_field(T, TextT).


%%% standardize_field_names([],[]) :-
%%%     !.
%%% standardize_field_names([[Field,Lines]|Rest],
%%% 			[[StandardField,Lines]|ModifiedRest]) :-
%%%     standard_name(Field,StandardField),
%%%     standardize_field_names(Rest,ModifiedRest).
%%% 
%%% standard_name("DOCID","UI") :- !.
%%% standard_name("MedlineID","UI") :- !.
%%% standard_name("NOINDEX","NOINDEX") :- !.
%%% standard_name("TITLE","TI") :- !.
%%% standard_name("ArticleTitle","TI") :- !.
%%% standard_name("TEXT","AB") :- !.
%%% standard_name("AbstractText","AB") :- !.
%%% standard_name(Name,Name) :- !.


/* extract_all_fields(+CitationLines, -CitationFields)
   extract_all_fields(+FieldID, +FirstFieldLine, +CitationLines, -CitationFields)

extract_all_fields/2
extract_all_fields/4
xxx
*/

extract_all_fields([], []) :- !.
extract_all_fields([FirstLine|RestLines], CitationFields) :-
	phrase(f_begins_field([FieldID,FirstFieldLine]), FirstLine),
	% atom_codes(FieldIDAtom, FieldIDString),
	% atom_codes(FirstFieldLineAtom, FirstFieldLineString),
	!,
	extract_all_fields_4(FieldID, FirstFieldLine, RestLines, CitationFields).
extract_all_fields([FirstLine|RestLines], CitationFields) :-
	format('WARNING: The following line should begin a field but does not:~n', []),
	format('~s~nIt is being ingored.~n~n', [FirstLine]),
	extract_all_fields(RestLines, CitationFields).

extract_all_fields_4(none, _FirstFieldLine, _RestLines, []) :- !.
extract_all_fields_4(FieldID, FirstFieldLine, RestLines,
                  [ExtractedFields|RestCitationFields]) :-
	extract_rest_of_field(RestLines, RestFieldLines, NewFieldID,
			      NewFirstFieldLine, NewRestLines),
	( FirstFieldLine == [] ->
	  ExtractedFields = [FieldID]
	; ExtractedFields = [FieldID,[FirstFieldLine|RestFieldLines]]
	),
	extract_all_fields_4(NewFieldID, NewFirstFieldLine, NewRestLines, RestCitationFields),
	!.

/* extract_rest_of_field(+CitationLines, -FieldLines, -NewFieldID,
                         -NewFirstFieldLine, -NewRestLines)

extract_rest_of_field/5
xxx
*/

extract_rest_of_field([], [], none, [], []) :- !.
extract_rest_of_field([First|Rest], [], NewFieldID, NewFirstFieldLine, Rest) :-
	phrase(f_begins_field([NewFieldID,NewFirstFieldLine]), First),
	!.
extract_rest_of_field([""|Rest],RestFieldLines, NewFieldID, NewFirstFieldLine, NewRestLines) :-
	extract_rest_of_field(Rest, RestFieldLines, NewFieldID, NewFirstFieldLine, NewRestLines).
extract_rest_of_field([First|Rest],[First|RestFieldLines], NewFieldID,
		      NewFirstFieldLine, NewRestLines) :-
	extract_rest_of_field(Rest, RestFieldLines, NewFieldID, NewFirstFieldLine, NewRestLines).

/*  BEGINS FIELD GRAMMAR  */

f_begins_field(FR) --> f_dense_token(F), {medline_field_data(F,_,_)},
                       f_separator(_), f_any(R),
                        {FR=[F,R]}.

f_dense_token(T) --> [Char], {\+Char==0' }, {\+Char==0'-}, f_dense_token(U),
                     {T=[Char|U]}
                 |   [Char], {\+Char==0' }, {\+Char==0'-}, {T=[Char]}.

f_separator(S) --> [0' ,0'-,0' ], f_blanks(B), !, {S=[0' ,0'-,0' |B]}
               |   [0'-,0' ], f_blanks(B), !, {S=[0'-,0' |B]}
               |   [0' ], !, f_separator(V), {S=[0' |V]}.

f_blanks(B) --> [0' ], !, f_blanks(C), {B=[0' |C]}
            |   {B=[]}.

f_any(T) --> [Char], !, f_any(U), {T=[Char|U]}
         |    {T=[]}.


/* medline_field(?Field, ?ShortDescription, ?LongDescription)

medline_field/3 is a factual predicate that defines Medline/PubMed fields.
Note that legal fields are either those defined by PubMed or additional
fields (UI, TX, QU and QT) that we use. */

medline_field_data(FieldString, ShortDescription, LongDescription) :-
	atom_codes(FieldAtom, FieldString),
	medline_field(FieldAtom, ShortDescription, LongDescription).

medline_field('UI',
	      'Unique Identifier',
	      'Unique Identifier').
medline_field('TX',
	      'Text',
	      'Text').
medline_field('QU',
	      'Query',
	      'Query').
medline_field('QT',
	      'Query Text',
	      'Query Text').
medline_field('AB',
	      'Abstract',
	      'Abstract').
medline_field('AD',
	      'Affiliation',
	      'Institutional affiliation and address of the first author, and grant numbers').
medline_field('AID',
	      'Article Identifier',
	      'Article ID values may include the pii (controlled publisher identifier) or doi (Digital Object Identifier)').
medline_field('AU',
	      'Author',
	      'Authors').
medline_field('CI',
	      'Copyright Information',
	      'Copyright statement').
medline_field('CIN',
	      'Comment In',
	      'Reference containing a comment about the article').
medline_field('CN',
	      'Corporate Author',
	      'Corporate author or group names with authorship responsibility').
medline_field('CON',
	      'Comment On',
	      'Reference upon which the article comments').
medline_field('DA',
	      'Date Created',
	      'Used for internal processing at NLM').
medline_field('DCOM',
	      'Date Completed',
	      'Used for internal processing at NLM').
medline_field('DEP',
	      'Date of Electronic Publication',
	      'Electronic publication date').
medline_field('DP',
	      'Publication Date',
	      'The date the article was published').
medline_field('EDAT',
	      'Entrez Date',
	      'The date the citation was added to PubMed').
medline_field('EFR',
	      'Erratum For',
	      'Cites the original article needing the correction').
medline_field('EIN',
	      'Erratum In',
	      'Reference containing a published erratum to the article').
medline_field('FAU',
	      'Full Author Name',
	      'Full Author Names').
medline_field('FIR',
	      'Full Investigator',
	      'Full investigator name').
medline_field('FPS',
	      'Full Personal Name as Subject',
	      'Full Personal Name of the subject of the article').
medline_field('GN',
	      'General Note',
	      'Supplemental or descriptive information related to the document').
medline_field('GR',
	      'Grant Number',
	      'Research grant numbers, contract numbers, or both that designate financial support by any agency of the US PHS (Public Health Service)').
medline_field('GS',
	      'Gene Symbol',
	      'Abbreviated gene names (used 1991 through 1996)').
medline_field('IP',
	      'Issue',
	      'The number of the issue, part, or supplement of the journal in which the article was published').
medline_field('IR',
	      'Investigator',
	      'NASA-funded principal investigator').
medline_field('IRAD',
	      'Investigator Affiliation',
	      'Affiliation of NASA-funded principal investigator').
medline_field('IS',
	      'ISSN',
	      'International Standard Serial Number of the journal').
medline_field('JID',
	      'NLM Unique ID',
	      'Unique journal ID in NLM''s catalog of books, journals, and audiovisuals').
medline_field('LA',
	      'Language',
	      'The language in which the article was published').
medline_field('LR',
	      'Last Revision Date',
	      'The date a change was made to the record during a maintenance procedure').
medline_field('MH',
	      'MeSH Terms',
	      'NLM''s controlled vocabulary').
medline_field('MHDA',
	      'MeSH Date',
	      'The date MeSH terms were added to the citation. The MeSH date is the same as the Entrez date until MeSH are added').
medline_field('OAB',
	      'Other Abstract',
	      'Abstract supplied by an NLM collaborating organization').
medline_field('OCI',
	      'Other Copyright Information',
	      'Copyright owner').
medline_field('OID',
	      'Other ID',
	      'Identification numbers provided by organizations supplying citation data').
medline_field('ORI',
	      'Original Report In',
	      'Displays on Patient Summary. Cites original article associated with the patient summary').
medline_field('OT',
	      'Other Term',
	      'Non-MeSH subject terms (keywords) assigned by an organization identified by the Other Term Owner').
medline_field('OTO',
	      'Other Term Owner',
	      'Organization that provided the Other Term data').
medline_field('OWN',
	      'Owner',
	      'Organization acronym that supplied citation data').
medline_field('PG',
	      'Pagination',
	      'The full pagination of the article').
medline_field('PHST',
	      'Publication History Status Date',
	      'History status date').
medline_field('PL',
	      'Place of Publication',
	      'Journal''s country of publication').
medline_field('PMID',
	      'PubMed Unique Identifier',
	      'Unique number assigned to each PubMed citation').
medline_field('PS',
	      'Personal Name as Subject',
	      'Individual is the subject of the article').
medline_field('PST',
	      'Publication Status',
	      'Publication status').
medline_field('PT',
	      'Publication Type',
	      'The type of material the article represents').
medline_field('PUBM',
	      'Publishing Model',
	      'Article''s model of print or electronic publishing').
medline_field('RF',
	      'Number of References',
	      'Number of bibliographic references for Review articles').
medline_field('RIN',
	      'Retraction In',
	      'Retraction of the article').
medline_field('RN',
	      'EC/RN Number',
	      'Number assigned by the Enzyme Commission to designate a particular enzyme or by the Chemical Abstracts Service for Registry Numbers').
medline_field('ROF',
	      'Retraction Of',
	      'Article being retracted').
medline_field('RPF',
	      'Republished From',
	      'Original article').
medline_field('RPI',
	      'Republished In',
	      'Corrected and republished article').
medline_field('SB',
	      'Subset',
	      'Journal/Citation Subset values representing various topic areas').
medline_field('SFM',
	      'Space Flight Mission',
	      'NASA-supplied data space flight/mission name and/or number').
medline_field('SI',
	      'Secondary Source Identifier',
	      'Identifies a secondary source that supplies information, e.g., other data sources, databanks and accession numbers of molecular sequences discussed in articles').
medline_field('SO',
	      'Source',
	      'Composite field containing bibliographic information').
medline_field('SPIN',
	      'Summary For Patients In',
	      'Cites a patient summary article').
medline_field('STAT',
	      'Status Tag',
	      'Used for internal processing at NLM').
medline_field('TA',
	      'Journal Title Abbreviation',
	      'Standard journal title abbreviation').
medline_field('TI',
	      'Title',
	      'The title of the article').
medline_field('TT',
	      'Transliterated / Vernacular Title',
	      'Non-Roman alphabet language titles are transliterated.').
medline_field('UIN',
	      'Update In',
	      'Update to the article').
medline_field('UOF',
	      'Update Of',
	      'The article being updated').
medline_field('VI',
	      'Volume', 'Journal volume').

extract_coord_sents_from_smart(SmartLines,Sentences,CoordinatedSentences, AAs) :-
    extract_all_smart_fields(SmartLines,CitationFields),
    (select_field("UI",CitationFields,UIField) ->
        extract_ui(UIField,UI)
    ;   UI="00000000"
    ),
    extract_coord_sents_from_fields(UI, CitationFields, Sentences,
				    CoordinatedSentences, AAs),
    !.

/* extract_all_smart_fields(+SmartLines, -CitationFields)
   concatenate_broken_lines(+SmartLinesIn, -SmartLinesOut)
   extract_each_smart_field(+SmartLines, -CitationFields)

extract_all_smart_fields/2
concatenate_broken_lines/2
extract_each_smart_field/2
xxx
*/

extract_all_smart_fields(SmartLines0,CitationFields) :-
    concatenate_broken_lines(SmartLines0,SmartLines),
    extract_each_smart_field(SmartLines,CitationFields),
    !.

/* Fields can be broken across lines. When this is done, an exclam is added
   to the end of the broken line and the rest of the original line continues
   on the next line (presumably it doesn't begin with ".", the field
   designator. */
concatenate_broken_lines([First,Second|Rest],Result) :-
    \+append(".",_,Second),
    !,
    rev(First,RevFirst),
    (RevFirst=[0'!|RestRevFirst] ->
        rev(RestRevFirst,RestFirst),
	append(RestFirst,Second,NewFirst)
    ;   append(First,Second,NewFirst) % maybe should warn when ! is missing
    ),
    concatenate_broken_lines([NewFirst|Rest],Result).
concatenate_broken_lines([First|Rest],[First|ModifiedRest]) :-
    !,
    concatenate_broken_lines(Rest,ModifiedRest).
concatenate_broken_lines(X,X).


extract_each_smart_field([],[]) :-
    !.
extract_each_smart_field([FirstLine|RestLines],
			 [[CitID,[Field]]|RestCitationFields]) :-
    FirstLine=[0'.,FieldChar|Field],
    FieldID=[FieldChar],
    !,
    % temp; this is awful
    ( FieldID=="I" ->
      skr_begin_write('ID'),
      skr_write_string(FirstLine),
      skr_end_write('ID')
    ; true
    ),
    smartfield_to_citationfield(FieldID,CitID),
    extract_each_smart_field(RestLines,RestCitationFields).
extract_each_smart_field([FirstLine|RestLines],CitationFields) :-
    format('WARNING: The following line should begin a field but does not:~n',
           []),
    format('~s~nIt is being ingored.~n~n',[FirstLine]),
    extract_each_smart_field(RestLines,CitationFields).

smartfield_to_citationfield("I","UI") :- !.
smartfield_to_citationfield("T","TI") :- !.
smartfield_to_citationfield("A","AB") :- !.
smartfield_to_citationfield(X,X) :- !.

/* select_field(+FieldID, +CitationFields, -Field)

select_field/3
xxx
*/

select_field(FieldID, [[FieldID,Field]|_Rest], Field) :-
	!.
select_field(FieldID, [_First|Rest], Field) :-
	select_field(FieldID, Rest, Field).


/* extract_ui(+UIField, -UI)

extract_ui/2
xxx
*/

extract_ui(Field, UI) :-
	( Field = [UI] ->
	  true
	; UI  = "00000000"
	).

extract_coord_sents_from_fields(UI, Fields, Sentences, CoordinatedSentences, AAs) :-
	extract_text_fields(Fields,TextFields0),
	padding_string(Padding),
	unpad_fields(TextFields0, Padding, TextFields1),
	TextFields = TextFields1,
	find_and_coordinate_sentences(UI, TextFields, Sentences, CoordinatedSentences, AAs),
	!.

/* text_field(?TextField)
   text_field/1 is a factual predicate of the individual textual fields.
*/

text_field('DOC').
text_field('QU').
text_field('QT').
text_field('TI').
text_field('AB').
text_field('AS').
text_field('MP').
text_field('OP').
text_field('SP').
text_field('PE').
text_field('RX').
text_field('HX').
text_field('TX').


/* extract_text_fields(+FieldsIn, -FieldsOut)

extract_text_fields/2 computes FieldsOut, those fields in FieldsIn which
satisfy text_field/1. */

extract_text_fields([], []).
extract_text_fields([[Field,Lines]|Rest], [[Field,Lines]|ExtractedRest]) :-
	atom_codes(FieldAtom, Field),
	text_field(FieldAtom),
	!,
	extract_text_fields(Rest,ExtractedRest).
extract_text_fields([_First|Rest], ExtractedRest) :-
	extract_text_fields(Rest, ExtractedRest).


unpad_fields([], _Padding, []).
unpad_fields([[Field,Lines]|Rest], Padding, [[Field,UnPaddedLines]|UnPaddedRest]) :-
	unpad_lines(Lines, Padding, UnPaddedLines),
	unpad_fields(Rest, Padding, UnPaddedRest).


unpad_lines([], _Padding, []).
unpad_lines([First|Rest], Padding, [UnPaddedFirst|UnPaddedRest]) :-
	( append(Padding, UnPaddedFirst, First) ->
	  true
	; UnPaddedFirst = First
	),
	unpad_lines(Rest, Padding, UnPaddedRest).

