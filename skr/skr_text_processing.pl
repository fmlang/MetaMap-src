% File:     skr_text_processing.pl
% Module:   SKR
% Author:   Lan
% Purpose:  Provide (high-level) text processing


:- module(skr_text_processing,[
	extract_sentences/6, % new
	get_skr_text/1
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
	prompt(Prompt, Prompt),
	format(user_error, '~w', [Prompt]),
	current_input(InputStream),
	get_skr_text_1(InputStream, Lines).

get_skr_text_1(InputStream, [First|Rest]) :-
	fget_non_ws_only_line(InputStream, First),
	!,
	fget_lines_until_skr_break(InputStream,Rest).
get_skr_text_1(_, []).


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
    replace_tabs_in_strings(Lines0,Lines1),
    replace_nonprints_in_strings(Lines1,Lines2),
    (   is_medline_citation(Lines2) ->
	extract_coord_sents_from_citation(Lines2, Sentences, CoordinatedSentences, AAs),
	InputType=citation,
	Lines = Lines2
    % ;	is_fielded_loosely(Lines2,RestLines,ID) ->  % needs modernizing
    %	extract_utterances_loosely(RestLines,ID,Utts),
    %   form_utterance_terms(Utts,UttTerms),
    %	ExpUttTerms=[],
    %	InputType=loose,
    %	Lines = Lines2
    ;	is_smart_fielded(Lines2) ->
	extract_coord_sents_from_smart(Lines2,Sentences,CoordinatedSentences),
	InputType=smart,
	Lines = Lines2
    % ;	is_sgml_text(Lines2) ->  % needs modernizing
    %	initialize_sgml_extractor,
    %	extract_utterances_from_sgml(Lines2,Sentences, CoordinatedSentences, AAs, Lines),
    %   form_utterance_terms(Utts,UttTerms),
    %	ExpUttTerms=[],
    %	InputType=sgml
    % ;	is_project_description(Lines2) ->
    %	extract_coord_sents_from_project(Lines2,Sentences,
    %					 CoordinatedSentences), % careful!
    %	% form_utterance_terms(Utterances,UttTerms),                   % careful!
    %	InputType=project,
    %	Lines = Lines2
    %   % for backward compatibility, the following handles previously
    %    % labeled text
    % ;   (Lines2=[Line|RestLines],
    %	 parse_labeled_utterance(Line,Label0,RestLine),
    %	 Label0\==[]) ->  % needs modernizing
    %    trim_whitespace(Label0,Label1),
    %    atom_codes(Label,Label1),
    % 	(RestLine==[] ->
    %	    concatenate_strings(RestLines," ",Text)
    %	;   concatenate_strings([RestLine|RestLines]," ",Text)
    %	),
    %	UttTerms=[utterance(Label,Text,_PosInfo,_ReplPos)],
    %	ExpUttTerms=[],
    %	InputType=lutt,
    %	Lines = Lines2
    ;   form_dummy_citation(Lines2,CitationLines),
	extract_coord_sents_from_citation(CitationLines, Sentences,
					  CoordinatedSentences, AAs),
	Lines = Lines2,
	InputType=simple
    ),
    !.

is_medline_citation([First|_]) :-
	split_string_completely(First, " ", Tokens0),
	remove_nulls(Tokens0, Tokens),
	Tokens = [String1, String2|_],
	lower_chars(String1, LowerString1),
	atom_codes(LowerAtom1, LowerString1),
	lower_chars(String2, LowerString2),
	atom_codes(LowerAtom2, LowerString2),
	medline_citation_indicator(LowerAtom1, LowerAtom2),
	!.

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

%%% /* parse_labeled_utterance(+InputString, -LabelString, -TextString)
%%% 
%%% parse_labeled_utterance/3 parses InputString into LabelString and TextString.  
%%% InputString is of the form <delimlabel><text> where <delimlabel>, which is
%%% optional, is of the form "[ <label> ]".  <label> is usually of the form
%%% <ui>.<field>.<n> indicating the nth utterance of <field> in citation <ui>.
%%% The form of a label is in flux. The current definition is "[ <label> ]", where
%%% the space after the left bracket is mandatory, and <label> is a string of
%%% non-right-brackets. */
%%% 
%%% parse_labeled_utterance(InputString,LabelString,TextString) :-
%%%     phrase(r_labelled_text([LabelString,TextString]),InputString).
%%% 
%%% % ---------- GRAMMAR FOR LABELLED TEXT 
%%% 
%%% r_labelled_text(LT) --> ([0' ], !, r_labelled_text(LT)
%%%                     ;    r_label(L), r_text(T), {LT=[L,T]}
%%%                     ;    r_text(T), {LT=[[],T]}
%%%                         ), !.
%%% 
%%% r_label(L) --> [0'[], [0' ], r_non_r_bracket(L), [0']].
%%% 
%%% r_text(T) --> ([0' ], !, r_text(T)
%%%           ;    r_any(T)
%%% %          ;    r_non_comment(T1), r_comment(_C), r_text(T2),
%%% %               {append([T1," ",T2],T)}
%%% %          ;    r_non_comment(T)
%%%               ), !.
%%% 
%%% r_non_r_bracket(S) --> ([Char], {\+Char==0']}, r_non_r_bracket(T), {S=[Char|T]}
%%%                    ;    {S=[]}
%%%                        ), !.
%%% 
%%% r_any(S) --> ([Char], r_any(T), {S=[Char|T]}
%%%          ;    {S=[]}
%%%              ), !.
%%% 
%%% %r_non_comment(S) --> ([Char], {\+Char==0'{}, r_non_comment(T), {S=[Char|T]}
%%% %                 ;    {S=[]}
%%% %                       ), !.
%%% 
%%% %r_comment(C) --> [0'{], r_non_r_brace(C), [0'}].
%%% 
%%% %r_non_r_brace(S) --> ([Char], {\+Char==0'}}, r_non_r_brace(T), {S=[Char|T]}
%%% %                 ;    {S=[]}
%%% %                       ), !.
%%% 
%%% 
%%% /* form_utterance_terms(+Utterances, -UtteranceTerms)
%%% 
%%% form_utterance_terms/2 transforms Utterances, a list of multi-line labeled
%%% utterances, the first line of which is of the form <label> <text>,
%%% into UtteranceTerms of the form utterance(<Label>,<Text>,<StartPos>/<Length>,<ReplPos>).  */
%%% 
%%% form_utterance_terms([],[]).
%%% form_utterance_terms([[FirstLine|RestLines]|Rest],
%%% 		     [utterance(Label,Text,_PosInfo,_ReplPos)|RestTerms]) :-
%%%     parse_labeled_utterance(FirstLine,Label0,FirstText),
%%%     trim_whitespace(Label0,Label1),
%%%     atom_codes(Label,Label1),
%%%     (FirstText=="" ->
%%%         concatenate_strings(RestLines," ",Text)
%%%     ;   concatenate_strings([FirstText|RestLines]," ",Text)
%%%     ),
%%%     form_utterance_terms(Rest,RestTerms).


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
%%% 	concat_atoms(TextHAtoms, ' ', TextHAtomsWithBlanks),
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

extract_coord_sents_from_smart(SmartLines,Sentences,CoordinatedSentences) :-
    extract_all_smart_fields(SmartLines,CitationFields),
    (select_field("UI",CitationFields,UIField) ->
        extract_ui(UIField,UI)
    ;   UI="00000000"
    ),
    extract_coord_sents_from_fields(UI, CitationFields, Sentences,
				    CoordinatedSentences, _AAs),
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

%%% /* extract_utterances_from_text_fields(+UI, +CitationFields, -Utterances)
%%% 
%%% extract_utterances_from_text_fields/3 extracts utterances from all text
%%% CitationsFields for UI producing Utterances. */
%%% 
%%% extract_utterances_from_text_fields(UI,CitationFields,Utterances) :-
%%%     text_fields(TextFields),
%%%     extract_utterances_from_text_fields_aux(TextFields,UI,CitationFields,
%%% 					    Utterances0),
%%%     append(Utterances0,Utterances),
%%%     !.
%%% 
%%% extract_utterances_from_text_fields_aux([],_UI,_CitationFields,[]) :-
%%%     !.
%%% extract_utterances_from_text_fields_aux([First|Rest],UI,CitationFields,
%%%                                         [FirstExtracted|RestExtracted]) :-
%%%     extract_utterances(First,UI,CitationFields,FirstExtracted),
%%%     extract_utterances_from_text_fields_aux(Rest,UI,CitationFields,
%%%                                             RestExtracted).

extract_coord_sents_from_fields(UI, Fields, Sentences, CoordinatedSentences, AAs) :-
	extract_text_fields(Fields,TextFields0),
	% trim_fields(TextFields0,TextFields1),
	padding_string(Padding),
	unpad_fields(TextFields0, Padding, TextFields1),
	% TextFields1 = TextFields0,
	% punctuate_fields(TextFields1,TextFields),
	TextFields = TextFields1,
	% format('TextFields: ~p~n',[TextFields]),
	%    append_fields(TextFields,TextLines),
	%    format('TextLines: ~p~n',[TextLines]),
	%    find_and_expand_aas(TextLines,Utts,ExpUtts),
	find_and_coordinate_sentences(UI, TextFields, Sentences, CoordinatedSentences, AAs),
	!.

	%    format('Sentences: ~p~n',[Sentences]),
	%    format('CoordinatedSentences: ~p~n',[CoordinatedSentences]),
	%    format('Utts: ~p~n',[Utts]),
	%    format('ExpUtts: ~p~n',[ExpUtts]),
	%    allocate_utts_to_fields(TextFields,Utts,FieldAllocation),
	%    format('FieldAllocation: ~p~n',[FieldAllocation]),
	%    construct_utt_labels(FieldAllocation,UI,"none",0,Labels),
	%    format('Labels: ~p~n',[Labels]),
	%    construct_utt_terms(Utts,Labels,UttTerms),
	%    format('UttTerms: ~p~n',[UttTerms]),
	%    construct_utt_terms(ExpUtts,Labels,ExpUttTerms),
	%    format('ExpUttTerms: ~p~n',[ExpUttTerms]),



/* text_field(?TextField)
   text_field/1 is a factual predicate of the individual textual fields.
*/

text_fields(["DOC","QU","QT","TI","AB","AS","MP","OP","SP","PE","RX","HX","TX"]).

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


%%%%% /* punctuate_fields(+FieldsIn, -FieldsOut)
%%%%%    punctuate_lines(+LinesIn, -LinesOut)
%%%%% 
%%%%% punctuate_fields/2 adds a period to the last line, if necessary, to each
%%%%% field in FieldsIn producing FieldsOut. */
%%%%% 
%%%%% punctuate_fields([],[]) :-
%%%%%     !.
%%%%% punctuate_fields([[Field,Lines]|Rest],
%%%%% 		 [[Field,PunctuatedLines]|PunctuatedRest]) :-
%%%%%     punctuate_lines(Lines,PunctuatedLines),
%%%%%     punctuate_fields(Rest,PunctuatedRest).
%%%%% 
%%%%% punctuate_lines([],[]) :-
%%%%%     !.
%%%%% punctuate_lines(Lines,PunctuatedLines) :-
%%%%%     rev(Lines,[Last|RevRest]),
%%%%%     punctuate_line(Last,PunctuatedLast),
%%%%%     rev([PunctuatedLast|RevRest],PunctuatedLines).
%%%%% 
%%%%% punctuate_line([],[]) :-
%%%%%     !.
%%%%% punctuate_line(Line,PunctuatedLine) :-
%%%%%     rev(Line,[Last|RevRest]),
%%%%%     ((is_final_punctuation(Last); Last==0',; Last==0':) ->
%%%%%         PunctuatedLine=Line
%%%%%     ;   RevPunctuatedLine=[0'.,Last|RevRest],
%%%%%         rev(RevPunctuatedLine,PunctuatedLine)
%%%%%     ).
%%%%% 
%%%%% 


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

/* trim_fields(+FieldsIn, -FieldsOut)
   trim_lines(+LinesIn, -LinesOut)

trim_fields/2 trims blanks from the lines in FieldsIn producing
FieldsOut. */

trim_fields([], []).
trim_fields([[Field,Lines]|Rest], [[Field,TrimmedLines]|TrimmedRest]) :-
	trim_lines(Lines, TrimmedLines),
	trim_fields(Rest, TrimmedRest).

trim_lines([], []).
trim_lines([First|Rest], [TrimmedFirst|TrimmedRest]) :-
	trim_whitespace(First, TrimmedFirst),
	trim_lines(Rest, TrimmedRest).

%%% /* extract_utterances(+FieldID, +UI, +CitationFields, -Utterances)
%%% 
%%% extract_utterances/4 extracts utterances for the text field, FieldID, of
%%% UI with fields CitationFields.  */
%%% 
%%% extract_utterances(FieldID,UI,CitationFields,Utterances) :-
%%%     select_field(FieldID,CitationFields,Field0),
%%%     !,
%%%     (FieldID=="AB" ->
%%% 	remove_truncation_notice(Field0,Field)
%%%     ;   Field=Field0
%%%     ),
%%%     parse_lines_into_utterances(Field,Utterances0),
%%%     lower(FieldID,LCFieldID),
%%%     label_and_format_utterances(UI,LCFieldID,1,_LastN,Utterances0,Utterances).
%%% extract_utterances(_FieldID,_UI,_CitationFields,[]).


%%% /* remove_truncation_notice(+LinesIn, -LinesOut)
%%% 
%%% remove_truncation_notice/2 removes "(ABSTRACT TRUNCATED AT nnn WORDS)" from
%%% the end of LinesIn producing LinesOut. The truncation notice cannot span
%%% more than two lines, and the last line must end with "WORDS)" (no trailing
%%% characters at all). */
%%% 
%%% remove_truncation_notice([],[]) :-
%%%     !.
%%% remove_truncation_notice([Line1,Line2],Result) :-
%%%     split_string(Line1,"(ABSTRACT",TruncatedLine1,_),
%%%     append(_,"WORDS)",Line2),
%%%     !,
%%%     (TruncatedLine1=="" ->
%%%         Result=[]
%%%     ;   Result=[TruncatedLine1]
%%%     ).
%%% remove_truncation_notice([Line],Result) :-
%%%     split_string(Line,"(ABSTRACT",TruncatedLine,RestLine),
%%%     append(_,"WORDS)",RestLine),
%%%     !,
%%%     (TruncatedLine=="" ->
%%%         Result=[]
%%%     ;   Result=[TruncatedLine]
%%%     ).
%%% remove_truncation_notice([First|Rest],[First|TruncatedRest]) :-
%%%     remove_truncation_notice(Rest,TruncatedRest).


%%% /* label_and_format_utterances(+UI, +FieldID, +FirstN, -LastN,
%%%                                +Utterances, -LFUtterances)
%%% 
%%% label_and_format_utterances/6
%%% label_and_format_utterances/5
%%% xxx
%%% */
%%% 
%%% label_and_format_utterances(_UI,_FieldID,N,N,[],[]) :-
%%%     !.
%%% label_and_format_utterances(UI,FieldID,N,LastN,[First|Rest],[LFFirst|LFRest]) :-
%%%     concatenate_items_to_string(["[ ",UI,".",FieldID,".",N," ]"],Label),
%%%     form_lines_from_words([Label|First],78,LFFirst),
%%%     !,
%%%     NewN is N+1,
%%%     label_and_format_utterances(UI,FieldID,NewN,LastN,Rest,LFRest).

%%% /* form_lines_from_words(+Words, +MaxLength, -Lines)
%%%    form_lines_from_words(+Words, +MaxLength, +LengthIn, +RevLineIn, -Lines)
%%% 
%%% form_lines_from_words/3
%%% form_lines_from_words/5
%%% xxx
%%% */
%%% 
%%% form_lines_from_words(Words,MaxLength,Lines) :-
%%%     form_lines_from_words(Words,MaxLength,0,[],Lines).
%%% 
%%% form_lines_from_words([],_,0,[],[]) :-
%%%     !.
%%% form_lines_from_words([],_,_,RevLineIn,[Line]) :-
%%%     !,
%%%     rev(RevLineIn,Line0),
%%%     append(Line0,Line).
%%% form_lines_from_words([First|Rest],MaxLength,LengthIn,RevLineIn,Lines) :-
%%%     length(First,NFirst),
%%%     (RevLineIn==[] ->
%%%         NewLengthIn is LengthIn+NFirst,
%%%         form_lines_from_words(Rest,MaxLength,NewLengthIn,[First],Lines)
%%%     |   NewLengthIn is LengthIn+NFirst+1,
%%%         NewLengthIn=<MaxLength,
%%%         form_lines_from_words(Rest,MaxLength,NewLengthIn,[First," "|RevLineIn],
%%%                               Lines)
%%%     ).
%%% form_lines_from_words(Words,MaxLength,_,RevLineIn,[Line|Lines]) :-
%%%     rev(RevLineIn,Line0),
%%%     append(Line0,Line),
%%%     form_lines_from_words(Words,MaxLength,0,[],Lines).


%%% /* extract_utterances_loosely(+Lines, +ID, -Utterances)
%%%    extract_utterances_loosely(+Lines, +FieldChar, +ID, -Utterances)
%%% 
%%% extract_utterances_loosely/3 uses extract_utterances_loosely/4 with
%%% FieldChar asterisk (*) to extract labeled Utterances from Lines.
%%% ID is used as the first component of labels.  */
%%% 
%%% extract_utterances_loosely(Lines,ID,Utterances) :-
%%%     extract_utterances_loosely(Lines,0'*,ID,Utterances).
%%% 
%%% extract_utterances_loosely(Lines,FieldChar,ID,Utterances) :-
%%%     extract_fields_from_lines(Lines,FieldChar,Fields),
%%%     form_utterances_from_fields(Fields,text,_ModeOut,FieldUtterances),
%%%     label_field_utterances(FieldUtterances,ID,Utterances).
%%% 
%%% /* extract_fields_from_lines(+Lines, +FieldChar, -Fields)
%%%    extract_fields_from_lines(+Lines, +FieldChar, +FieldID, +FieldLines,
%%%                              +FieldsIn, -FieldsOut)
%%% 
%%% extract_fields_from_lines/3 extracts Fields from Lines where the
%%% the beginning of a field is signalled by a line starting with FieldChar,
%%% e.g. *AN or *DL).  A field in Fields is of the form field(<field_id>,<lines>)
%%% where <lines> is a list of strings.  extract_fields_from_lines/6 is an
%%% auxiliary.  */
%%% 
%%% extract_fields_from_lines(Lines,FieldChar,Fields) :-
%%%     extract_fields_from_lines(Lines,FieldChar,none,[],[],Fields0),
%%%     rev(Fields0,Fields).
%%% 
%%% extract_fields_from_lines([],_FieldChar,FieldID,FieldLines,
%%% 			  FieldsIn,FieldsOut) :-
%%%     !,
%%%     (FieldLines==[] ->
%%% 	FieldsOut=FieldsIn
%%%     ;   rev(FieldLines,RevFieldLines),
%%%         FieldsOut=[field(FieldID,RevFieldLines)|FieldsIn]
%%%     ).
%%% extract_fields_from_lines([Line0|Rest],FieldChar,FieldID,FieldLines,
%%% 			  FieldsIn,FieldsOut) :-
%%%     replace_tabs(Line0,Line1),
%%%     replace_nonprints(Line1,Line),
%%%     (   Line=="" ->
%%%         extract_fields_from_lines(Rest,FieldChar,FieldID,FieldLines,
%%% 				  FieldsIn,FieldsOut)
%%%     ;   is_control_command(Line,_Mode) ->
%%%         (FieldLines==[] ->
%%%             extract_fields_from_lines(Rest,FieldChar,FieldID,[],
%%% 				      [field(Line,[])|FieldsIn],FieldsOut)
%%%         ;   rev(FieldLines,RevFieldLines),
%%%             extract_fields_from_lines(Rest,FieldChar,FieldID,[],
%%% 				      [field(Line,[]),
%%% 				       field(FieldID,RevFieldLines)|FieldsIn],
%%% 				      FieldsOut)
%%%         )
%%%     ;   begins_loose_field(Line,FieldChar,NewFieldID) ->
%%%         (FieldLines==[] ->
%%%             extract_fields_from_lines(Rest,FieldChar,NewFieldID,[],
%%% 				      FieldsIn,FieldsOut)
%%%         ;   rev(FieldLines,RevFieldLines),
%%%             extract_fields_from_lines(Rest,FieldChar,NewFieldID,[],
%%% 				      [field(FieldID,RevFieldLines)|FieldsIn],
%%% 				      FieldsOut)
%%%         )
%%%     ;   extract_fields_from_lines(Rest,FieldChar,FieldID,[Line|FieldLines],
%%% 				  FieldsIn,FieldsOut)
%%%     ).
%%% 

%%% /* is_control_command(?Chars, ?Mode)
%%% 
%%% is_control_command/1 is a factual predicate which controls how subsequent
%%% lines of text are to be labeled.  Labeler has two modes for labeling
%%% utterances: text and list.  In text mode, utterances can span several lines
%%% and are essentially detected by final punctuation.  In list mode, each
%%% line is an utterance; no final punctuation is necessary.
%%% The default mode is text except for fields *FS and *DL which are always
%%% processed in list mode.  A control command for changing modes must appear
%%% alone on a separate line.  Its effect continues until another control command
%%% is issued.  It is expected that control commands will be issued in pairs:
%%% + (to start list mode), and - (to stop list mode, returning to text mode).  */
%%% 
%%% is_control_command("+",list).  % starts list mode
%%% is_control_command("-",text).  % stops list mode (and hence starts text mode)


%%% /* begins_loose_field(+Line, +FieldChar, -FieldID)
%%% 
%%% begins_loose_field/2 determines if Line begins a "loose" field.  It does if
%%% it begins with FieldChar.  FieldID is rest of the Line lowercased.  */
%%% 
%%% begins_loose_field([FieldChar|FieldID0],FieldChar,FieldID) :-
%%%     lower(FieldID0,FieldID).


%%% /* form_utterances_from_fields(+Fields, +ModeIn, -ModeOut, -FieldUtterances)
%%%    form_utterances_from_field(+Field, +ModeIn, -ModeOut, -FieldUtterances)
%%% 
%%% form_utterances_from_fields/4 forms FieldUtterances from Fields in one of two
%%% ways: for fields which consist of lists of items, each line determines an
%%% utterance; for text fields, the same approach as for citations is used to
%%% extract the utterances.  ModeIn determines the mode (text or list) for
%%% subsequent processing.  Special command fields change the mode.  */
%%% 
%%% form_utterances_from_fields([],ModeIn,ModeIn,[]) :-
%%%     !.
%%% form_utterances_from_fields([First|Rest],ModeIn,ModeOut,FieldUtterances) :-
%%%     form_utterances_from_field(First,ModeIn,ModeInOut,FirstFieldUtterances),
%%%     (FirstFieldUtterances=[] ->
%%%         FieldUtterances=RestFieldUtterances
%%%     ;   FieldUtterances=[FirstFieldUtterances|RestFieldUtterances]
%%%     ),
%%%     form_utterances_from_fields(Rest,ModeInOut,ModeOut,RestFieldUtterances).
%%% 
%%% form_utterances_from_field(field(FieldID,_Lines),_ModeIn,NewMode,[]) :-
%%%     is_control_command(FieldID,NewMode),
%%%     !.
%%% form_utterances_from_field(field(FieldID,Lines),ModeIn,ModeIn,
%%%                            [FieldID,Utterances]) :-
%%%     is_list_field(FieldID),
%%%     !,
%%%     parse_lines_into_word_lists(Lines,Utterances).
%%% form_utterances_from_field(field(FieldID,Lines),ModeIn,ModeIn,
%%%                            [FieldID,Utterances]) :-
%%%     (ModeIn==text ->
%%%         parse_lines_into_utterances(Lines,Utterances)
%%%     ;   parse_lines_into_word_lists(Lines,Utterances)
%%%     ).
%%% 
%%% 
%%% /* is_list_field(?FieldID)
%%% 
%%% is_list_field/1 is a factual predicate indicating which of the "loose"
%%% fields consist of lists of items.  */
%%% 
%%% is_list_field("dl").
%%% is_list_field("fs").


%%% /* label_field_utterances(+FieldUtterances, +ID, -LFUtterances)
%%%    label_field_utterances(+FieldUtterances, +ID, +FirstN, -LFUtterances)
%%% 
%%% label_field_utterances/3 labels FieldUtterances using ID to produce
%%% LFUtterances (labeled, formatted utterances).  */
%%% 
%%% label_field_utterances(FieldUtterances,ID,LFUtterances) :-
%%%     label_field_utterances(FieldUtterances,ID,1,LFUtterances).
%%% 
%%% label_field_utterances([],_ID,_FirstN,[]) :-
%%%     !.
%%% label_field_utterances([[FieldID,Utterances0]|Rest],ID,FirstN,LFUtterances) :-
%%%     label_and_format_utterances(ID,FieldID,FirstN,NextN,Utterances0,
%%%                                 FirstLFUtterances),
%%%     append(FirstLFUtterances,RestLFUtterances,LFUtterances),
%%%     label_field_utterances(Rest,ID,NextN,RestLFUtterances).


%%% /* parse_lines_into_utterances(+Lines, -Utterances)
%%% 
%%% parse_lines_into_utterances/2
%%% xxx
%%% */
%%% 
%%% parse_lines_into_utterances([],[]) :-
%%%     !.
%%% parse_lines_into_utterances(Lines,Utterances) :-
%%%     parse_lines_into_word_lists(Lines,WordLists),
%%%     append(WordLists,Words),
%%%     form_utterances_from_words(Words,Utterances),
%%%     !.
%%% 
%%% /* parse_lines_into_word_lists(+Lines, -WordLists)
%%% 
%%% parse_lines_into_word_lists/2
%%% xxx
%%% */
%%% 
%%% parse_lines_into_word_lists([],[]) :-
%%%     !.
%%% parse_lines_into_word_lists([FirstLine|RestLines],
%%%                             [FirstWordList|RestWordLists]) :-
%%%     parse_line_into_word_list(FirstLine,FirstWordList),
%%%     parse_lines_into_word_lists(RestLines,RestWordLists).
%%% 
%%% parse_line_into_word_list(Line,WordList) :-
%%%     phrase(t_string(WordList),Line).


%%% /*  TOKENIZE STRING GRAMMAR  */
%%% 
%%% t_string(TS) --> [0' ], !, t_string(TS)
%%% 
%%%              |   t_token(T), {T\==[]}, t_string(S), !, {TS=[T|S]}
%%% 
%%%              |   {TS=[]}.
%%% 
%%% t_token(T) --> [Char], {\+Char==0' }, !, t_token(S), {T=[Char|S]}
%%% 
%%%            |   {T=[]}.


/* form_utterances_from_words(+Words, -Utterances)
   form_utterances_from_words(+Words, +RevUtteranceIn, -UtterancesOut)
   
form_utterances_from_words/2
form_utterances_from_words/3
xxx
*/

form_utterances_from_words(Words,Utterances) :-
    form_utterances_from_words(Words,[],Utterances).

form_utterances_from_words([],[],[]) :-
    !.
form_utterances_from_words([],RevUtteranceIn,[UtteranceIn]) :-
    !,
    rev(RevUtteranceIn,UtteranceIn).
form_utterances_from_words([Word|Rest],RevUtteranceIn,Utterances) :-
    (ends_utterance(Word,Rest) ->
        !,
        rev([Word|RevUtteranceIn],UtteranceIn),
        Utterances=[UtteranceIn|RestUtterances],
        form_utterances_from_words(Rest,[],RestUtterances)
    |   form_utterances_from_words(Rest,[Word|RevUtteranceIn],
                                   Utterances)
    ).

/* ends_utterance(+Word, +RestWords)

ends_utterance/2
*/

ends_utterance(Word,RestWords) :- % The word ends with final punctuation
    rev(Word,[Last|_]),
    is_final_punctuation(Last),
    \+is_end_utterance_exception(Last,Word,RestWords),
    !.
ends_utterance(Word,_RestWords) :-  % The word contains ".["
    split_string(Word,".[",_,_),
    !.

/* is_final_punctuation(+Char)

is_final_punctuation/1 succeeds if Char is final punctuation.  */

is_final_punctuation(0'.).
is_final_punctuation(0'?).
is_final_punctuation(0'!).
is_final_punctuation(0';).


/* is_end_utterance_exception(+FinalPunctuation, +Word, +RestWords)

is_end_utterance_exception/3
The exceptions to breaking at final punctuation are (in order of the clauses
below):
  1. The next word begins with a lowercase character;
%?  2. The word is a single character abbreviation (e.g., E.);
  3. The word is Dr.; and
  4. The word and next word are explicit exceptions, currently either
     St. Louis, St. Paul, i.e., or e.g.
xxx
*/

is_end_utterance_exception(0'.,_Word,[[NextChar|_]|_]) :-
    is_lower(NextChar).
%?is_end_utterance_exception(0'.,[Char,_],_) :-
%?    is_alpha(Char).
is_end_utterance_exception(0'.,"Dr.",[_|_]).
is_end_utterance_exception(0'.,Word,[NextWord|_]) :-
    lower(Word,LCWord),
    exception_list(LCWord,SecondWord),
    lower(NextWord,LCNextWord),
    append(SecondWord,_,LCNextWord).

/* exception_list(?FirstWord, ?SecondWord)

exception_list/2
xxx
*/

exception_list("st.","louis").
exception_list("st.","paul").
exception_list("i.","e.").
exception_list("e.","g.").


%%% extract_coord_sents_from_project(ProjectLines,Sentences,CoordinatedSentences) :-
%%%     extract_project_fields(ProjectLines,ProjectFields),
%%%     (select_field("UI",ProjectFields,UIField) ->
%%%         extract_ui(UIField,UI)
%%%     ;   UI="00000000"
%%%     ),
%%%     extract_coord_sents_from_fields(UI, ProjectFields, Sentences,
%%% 				    CoordinatedSentences, _AAs),
%%%     !.
%%% 
%%% extract_project_fields(ProjectLines,[["UI",[ProjectID]],
%%% 				     ["TI",TitleLines],
%%% 				     ["AB",DescriptionLines]]) :-
%%%     extract_project_id(ProjectLines,ProjectLines1,ProjectID),
%%%     extract_project_title(ProjectLines1,ProjectLines2,TitleLines),
%%%     extract_project_description(ProjectLines2,DescriptionLines),
%%%     !.
%%% 
%%% extract_project_id([First|Rest],Rest,ProjectID) :-
%%%     split_string(First,"--PROJECT NUMBER","",String1),
%%%     trim_left_periods(String1,String2),
%%%     (split_string(String2,"SUB:",String3,String4) ->
%%% 	trim_whitespace(String3,String5),
%%% 	trim_whitespace(String4,String6),
%%% 	append([String5," ",String6],String7)
%%%     ;   trim_whitespace(String2,String7)
%%%     ),
%%%     replace_all_substrings(String7," ",":",ProjectID).
%%% 
%%% 
%%% trim_left_periods([0'.|Rest],Result) :-
%%%     !,
%%%     trim_left_periods(Rest,Result).
%%% trim_left_periods(String,String).
%%% 
%%% extract_project_title([_TitleLine,SubTitleLine|Rest],Rest,[SubTitle]) :-
%%%     split_string(SubTitleLine,"SUB TITLE",_,SubTitle0),
%%%     !,
%%%     trim_whitespace(SubTitle0,SubTitle).
%%% extract_project_title([TitleLine|Rest],Rest,[Title]) :-
%%%     split_string(TitleLine,"TITLE",_,Title0),
%%%     trim_whitespace(Title0,Title).
%%% 
%%% extract_project_description(ProjectLines, DescriptionLines) :-
%%% 	trim_all_blanks(ProjectLines, DescriptionLines).
%%% 
%%% trim_all_blanks([], []).
%%% trim_all_blanks([First|Rest], [TrimmedFirst|TrimmedRest]) :-
%%% 	trim_whitespace(First, TrimmedFirst),
%%% 	trim_all_blanks(Rest, TrimmedRest).


%%% warn_remove_non_ascii_from_input_lines([], []).
%%% warn_remove_non_ascii_from_input_lines([H|T], [AsciiH|AsciiT]) :-
%%% 	warn_remove_non_ascii_from_string(H, H, 1, AsciiH),
%%% 	warn_remove_non_ascii_from_input_lines(T, AsciiT).

%%% warn_remove_non_ascii_from_string([], _String, _CharPos, []).
%%% warn_remove_non_ascii_from_string([H|T], String, CharPos, AsciiString) :-
%%% 	( is_ascii(H) ->
%%% 	  AsciiString = [H|RestAsciiString]
%%% 	; RestAsciiString = AsciiString,
%%% 	  warn_non_ascii_char(H, CharPos, String)
%%% 	),
%%% 	NextCharPos is CharPos + 1,
%%% 	warn_remove_non_ascii_from_string(T, String, NextCharPos, RestAsciiString).
%%% 
%%% warn_non_ascii_char(H, CharPos, String) :-
%%% 	format(user_output,
%%% 	       '~n### WARNING: Non-ASCII char ~c was removed from position ~d in input string~n###         "~s"~n',
%%% 	       [H, CharPos, String]).
%%% 