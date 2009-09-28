% File:     skr_xml.pl
% Module:   Generate XML output
% Author:   Francois
% Purpose:  Generate XML output analogous to MetaMap Machine Output

:- module(skr_xml,[
	conditionally_print_xml_header/2,
	conditionally_print_xml_footer/3,
	generate_and_print_xml/1,
	xml_header_footer_print_setting/3
    ]).

:- use_module(skr(skr_utilities), [
	replace_blanks_with_crs/4,
	verify_xml_format/2
   ]).

:- use_module(skr_lib(negex), [
	final_negation_template/6
   ]).

:- use_module(skr_lib(nls_system), [
	control_option/1,
	control_value/2
   ]).

:- use_module(skr_lib(nls_strings),[
	atom_codes_list/2,
	form_one_string/3,
	split_string_completely/3
   ]).

:- use_module(skr_lib(sicstus_utils),[
	concat_atoms/2
   ]).

:- use_module(library(system),[
	environ/2
   ]).

:- use_module(library(lists),[
	delete/3
   ]).

:- use_module(library(xml),[
	xml_parse/3
   ]).


% This code creates Prolog structures that are fed to xml:xml_parse,
% which is defined in the Quintus Prolog library.

generate_and_print_xml(AllMMO) :-
	( control_value('XML', Format) ->
	  verify_xml_format(Format, TrueOrFalse),
	  current_output(OutputStream),
	  XMLTerm = xml([], MMOXML),
	  AllMMO = [ArgsMMO,AAsMMO,NegExMMO|UtteranceMMO],
	  generate_xml_arg_term(ArgsMMO,    XMLArgTerm),
	  generate_xml_AA_term(AAsMMO,      XMLAATerm),
	  generate_xml_negex_term(NegExMMO, XMLNegExTerm),
	  generate_xml_utterances_term(UtteranceMMO, XMLUtterancesTerm),
	  create_xml_element('MMO',
			     [],
			     [XMLArgTerm,XMLAATerm,XMLNegExTerm,XMLUtterancesTerm],
			     MMOXML),
	  xml_parse(XMLChars, XMLTerm, [format(TrueOrFalse)]),
	  conditionally_increase_indent_by_one(Format, XMLChars, XMLChars1),
	  print_xml_output(OutputStream, XMLChars1)
	; true
	).

% Increase the indentation of the XML beginning with <MMO>,
% because <MMOlist> is left justified.

conditionally_increase_indent_by_one(format1, XMLChars, XMLChars1) :-
	increase_indent_by_one(XMLChars, XMLChars1).
conditionally_increase_indent_by_one(format, XMLChars, XMLChars1) :-
	increase_indent_by_one(XMLChars, XMLChars1).
conditionally_increase_indent_by_one(noformat1, XMLChars, XMLChars).
conditionally_increase_indent_by_one(noformat, XMLChars, XMLChars).

increase_indent_by_one([], []).
increase_indent_by_one([H|T], IncreasedIndent) :-
	( H =:= 10 ->
	  IncreasedIndent = [10,32|RestIncreasedIndent]
	; IncreasedIndent = [H|RestIncreasedIndent]
	),
	increase_indent_by_one(T, RestIncreasedIndent).

print_xml_output(OutputStream, XMLChars) :-
	% 0 means inner header/footer
	xml_header_footer_print_setting(0, XMLSetting, PrintSetting),
	conditionally_print_xml_header(PrintSetting, OutputStream),
	format(OutputStream, "~s", [XMLChars]),
	conditionally_print_xml_footer(PrintSetting, XMLSetting, OutputStream).

conditionally_print_CR(Format, OutputStream) :-
	( Format == format1 ->
	  format(OutputStream, "~n", [])
	; Format == format ->
	  format(OutputStream, "~n", [])
	; true
	).

count_utterance_terms([], UtteranceCount, UtteranceCount).
count_utterance_terms([H|T], UtteranceCountIn, UtteranceCountOut) :-
	( functor(H, utterance, _Arity) ->
	  UtteranceCountNext is UtteranceCountIn + 1
	; UtteranceCountNext is UtteranceCountIn
	),
	count_utterance_terms(T, UtteranceCountNext, UtteranceCountOut).

generate_xml_utterances_term(UtteranceMMO, XMLUtterancesTerm) :-
	% Count the number of terms in UtteranceMMO that are of type 'utterance'
	count_utterance_terms(UtteranceMMO, 0, UtteranceCountNumber),
	number_codes(UtteranceCountNumber, UtteranceMMOLength),
	generate_xml_utterance_list(UtteranceMMO, UtteranceList),
	create_xml_element('Utterances',
			   ['Count'=UtteranceMMOLength],
			   UtteranceList,
			   XMLUtterancesTerm).

generate_xml_utterance_list([], []).
generate_xml_utterance_list([UtteranceMMO|RestUtteranceMMO], [XMLUtterance|RestXMLUtterances]) :-
	generate_one_xml_utterance_term(UtteranceMMO, RestUtteranceMMO, XMLUtterance,
					RemainingUtteranceMMO),
	generate_xml_utterance_list(RemainingUtteranceMMO, RestXMLUtterances).

generate_one_xml_utterance_term(UtteranceMMO, RestUtteranceMMO,
				XMLUtteranceTerm, RemainingUtteranceMMO) :-
	UtteranceMMO = utterance(Label, TempMMOUtteranceString, UttStartPos/UttLength, ReplPos),
	replace_blanks_with_crs(ReplPos, TempMMOUtteranceString, UttStartPos, MMOUtteranceString),
	generate_xml_utterance_data(Label, UttStartPos, UttLength,
				    MMOUtteranceString,
				    XMLPMID, XMLUtteranceType, XMLUtteranceNumber,
				    XMLUtteranceText,
				    XMLUtteranceStartPos, XMLUtteranceLength),
	generate_xml_phrase_term(RestUtteranceMMO, XMLPhraseListTerm, RemainingUtteranceMMO),
	create_xml_element('Utterance',
			   [],
			   [XMLPMID,XMLUtteranceType,XMLUtteranceNumber,XMLUtteranceText,
			    XMLUtteranceStartPos,XMLUtteranceLength,XMLPhraseListTerm],
			   XMLUtteranceTerm).
			   
% accumulate all phrase(_), candidates(_), and mappings(_) terms
% that occur before the next utterance(_) term,
% and while we're at it, count the number of phrase(_) terms
generate_phrases_before_next_utterance([],    PhraseCount,   [],         PhraseCount,    []).
generate_phrases_before_next_utterance([H|T], PhraseCountIn, PhrasesMMO, PhraseCountOut, MMOOut) :-
	( H = utterance(_,_,_,_) ->
	  PhraseCountOut is PhraseCountIn,
	  PhrasesMMO = [],
	  MMOOut = [H|T]	  
	; H = phrase(_,_,_,_) ->
	  PhraseCountNext is PhraseCountIn + 1,
	  PhrasesMMO = [H|RestPhrases],
	  generate_phrases_before_next_utterance(T, PhraseCountNext,
						 RestPhrases, PhraseCountOut, MMOOut)
	  % catch-all for candidates and mappings terms
	; PhraseCountNext is PhraseCountIn,
	  PhrasesMMO = [H|RestPhrases],
	  generate_phrases_before_next_utterance(T, PhraseCountNext,
						 RestPhrases, PhraseCountOut, MMOOut)
	).
			       

generate_xml_phrase_term(MMOIn, XMLPhraseTerm, MMOOut) :-
	generate_phrases_before_next_utterance(MMOIn, 0, PhrasesMMO, PhrasesLength, MMOOut),
	number_codes(PhrasesLength, PhraseCount),
	generate_xml_phrase_list(PhrasesMMO, XMLPhraseList),
	create_xml_element('Phrases',
			   ['Count'=PhraseCount],
			   XMLPhraseList,
			   XMLPhraseTerm).

generate_xml_phrase_list([], []).
generate_xml_phrase_list([MMOPhraseTerm,MMOCandidatesTerm,MMOMappingsTerm|RestMMOPhrases],
			 [XMLPhrase|RestXMLPhrases]) :-
	generate_one_xml_phrase(MMOPhraseTerm, MMOCandidatesTerm, MMOMappingsTerm, XMLPhrase), 
	generate_xml_phrase_list(RestMMOPhrases, RestXMLPhrases).
				
generate_one_xml_phrase(MMOPhraseTerm, MMOCandidatesTerm, MMOMappingsTerm, XMLPhrase) :-
	MMOPhraseTerm     = phrase(TempMMOPhraseAtom,PhraseSyntax,PhraseStartPos/PhraseLength,ReplPos),
	MMOCandidatesTerm = candidates(MMOCandidatesList),
	MMOMappingsTerm   = mappings(MMOMappingsList),
	atom_codes(TempMMOPhraseAtom, TempMMOPhraseString),
	replace_blanks_with_crs(ReplPos, TempMMOPhraseString, PhraseStartPos, MMOPhraseString),
	generate_xml_phrase_text(MMOPhraseString, XMLText),
	generate_xml_phrase_syntax(PhraseSyntax, XMLSyntax),
	generate_xml_phrase_start_pos(PhraseStartPos, XMLPhraseStartPos),
	generate_xml_phrase_length(PhraseLength, XMLPhraseLength),
	% generate_xml_replacement_pos(ReplPos, XMLReplPos),
	generate_xml_phrase_candidates_term(MMOCandidatesList, XMLCandidatesTerm),
	generate_xml_phrase_mappings_term(MMOMappingsList, XMLMappingsTerm),
	create_xml_element('Phrase',
			   [],
			   [XMLText,XMLSyntax,XMLPhraseStartPos,XMLPhraseLength,
			    XMLCandidatesTerm,XMLMappingsTerm],
			   XMLPhrase).

generate_xml_phrase_candidates_term(CandidatesList, XMLCandidatesTerm) :-
	length(CandidatesList, CandidatesLength),
	number_codes(CandidatesLength, Count),
	generate_xml_candidates_list(CandidatesList, XMLCandidatesList),
	create_xml_element('Candidates',
			   ['Count'=Count],
			   XMLCandidatesList,
			   XMLCandidatesTerm).

generate_xml_candidates_list([], []).
generate_xml_candidates_list([FirstMMOCandidate|RestMMOCandidates],
			     [FirstXMLCandidate|RestXMLCandidates]) :-
	generate_one_xml_candidate(FirstMMOCandidate, FirstXMLCandidate),
	generate_xml_candidates_list(RestMMOCandidates, RestXMLCandidates).

generate_one_xml_candidate(MMOCandidate, XMLCandidate) :-
	MMOCandidate = ev(NegScore,CUI,ConceptName,PreferredName,MatchedWords,
			  SemTypes,MatchMap,IsHead,IsOverMatch,Sources,PosInfo),
	generate_xml_neg_score(NegScore, XMLNegScore),
	generate_xml_CUI(CUI, XMLCUI),
	generate_xml_concept_name(ConceptName, XMLConceptName),
	generate_xml_preferred_name(PreferredName, XMLPreferredName),
	generate_xml_matched_words(MatchedWords, XMLMatchedWords),
	generate_xml_semtypes(SemTypes, XMLSemTypes),
	generate_xml_matchmap(MatchMap, XMLMatchMap),
	generate_xml_is_head(IsHead, XMLIsHead),	
	generate_xml_is_overmatch(IsOverMatch, XMLIsOverMatch),	
	generate_xml_sources(Sources, XMLSources),
	generate_xml_pos_info(PosInfo, 'Spans', XMLPosInfo),
	create_xml_element('Candidate',
			   [],
			   [XMLNegScore,XMLCUI,XMLConceptName,XMLPreferredName,
			    XMLMatchedWords,XMLSemTypes,XMLMatchMap,XMLIsHead,
			    XMLIsOverMatch,XMLSources,XMLPosInfo],
			   XMLCandidate).


generate_xml_neg_score(NegScore, XMLNegScore) :-
	number_codes(NegScore, NegScoreString),
	create_xml_element('NegScore',
			   [],
			   [pcdata(NegScoreString)],
			   XMLNegScore).

generate_xml_CUI(CUI, XMLCUI) :-
	atom_codes(CUI, CUIString),
	create_xml_element('UMLSCUI',
			   [],
			   [pcdata(CUIString)],
			   XMLCUI).

generate_xml_concept_name(ConceptName, XMLConceptName) :-
	atom_codes(ConceptName, ConceptNameString),
	create_xml_element('UMLSConcept',
			   [],
			   [pcdata(ConceptNameString)],
			   XMLConceptName).

generate_xml_preferred_name(PreferredName, XMLPreferredName) :-
	atom_codes(PreferredName, PreferredNameString),
	create_xml_element('UMLSPreferred',
			   [],
			   [pcdata(PreferredNameString)],
			   XMLPreferredName).

generate_xml_matched_words(MatchedWords, XMLMatchedWords) :-
	generate_xml_matched_word_list(MatchedWords, XMLMatchedWordList),
	length(MatchedWords, Length),
	number_codes(Length, Count),
	create_xml_element('MatchedWords',
			   ['Count'=Count],
			   XMLMatchedWordList,
			   XMLMatchedWords).

generate_xml_matched_word_list([], []).
generate_xml_matched_word_list([FirstMatchedWordAtom|RestMatchedWordAtoms],
			       [FirstMatchedWordXML|RestMatchedWordXMLs]) :-
	generate_one_xml_matched_word(FirstMatchedWordAtom, FirstMatchedWordXML),
	generate_xml_matched_word_list(RestMatchedWordAtoms, RestMatchedWordXMLs).

generate_one_xml_matched_word(MatchedWordAtom, MatchedWordXML) :-
	atom_codes(MatchedWordAtom, MatchedWordString),
	create_xml_element('MatchedWord',
			   [],
			   [pcdata(MatchedWordString)],
			   MatchedWordXML).

generate_xml_semtypes(SemTypes, XMLSemTypes) :-
	generate_xml_semtype_list(SemTypes, XMLSemTypeList),
	length(SemTypes, Length),
	number_codes(Length, Count),
	create_xml_element('STs',
			   ['Count'=Count],
			   XMLSemTypeList,
   			   XMLSemTypes).

generate_xml_semtype_list([], []).
generate_xml_semtype_list([FirstSemTypeAtom|RestSemTypeAtoms],
			  [FirstSemTypeXML|RestSemTypeXMLs]) :-
	generate_one_xml_semtype(FirstSemTypeAtom, FirstSemTypeXML),
	generate_xml_semtype_list(RestSemTypeAtoms, RestSemTypeXMLs).

generate_one_xml_semtype(SemTypeAtom, SemTypeXML) :-
	atom_codes(SemTypeAtom, SemTypeString),
	create_xml_element('ST',
			   [],
			   [pcdata(SemTypeString)],
			   SemTypeXML).			   

generate_xml_matchmap(MatchMap, XMLMatchMap) :-
	generate_xml_matchmap_list(MatchMap, XMLMatchMapList),
	length(MatchMap, Length),
	number_codes(Length, Count),
	create_xml_element('MatchMaps',
			   ['Count'=Count],
			   XMLMatchMapList,
   			   XMLMatchMap).


generate_xml_matchmap_list([], []).
generate_xml_matchmap_list([FirstMatchMap|RestMatchMap],
			   [FirstMatchMapXML|RestMatchMapXMLs]) :-
	generate_one_xml_matchmap(FirstMatchMap, FirstMatchMapXML),
	generate_xml_matchmap_list(RestMatchMap, RestMatchMapXMLs).

generate_one_xml_matchmap(MatchMap, MatchMapXML) :-
	MatchMap = [[TWMatchPosS,TWMatchPosE],
		    [CWMatchPosS, CWMatchPosE],
		    Variation],
	number_codes(TWMatchPosS, TWMatchPosSString),
	number_codes(TWMatchPosE, TWMatchPosEString),
	number_codes(CWMatchPosS, CWMatchPosSString),
	number_codes(CWMatchPosE, CWMatchPosEString),
	number_codes(Variation,   VariationString),

	create_xml_element('TWMatchPosS',
			   [],
			   [pcdata(TWMatchPosSString)],
			   XMLTWMatchPosS),
	create_xml_element('TWMatchPosE',
			   [],
			   [pcdata(TWMatchPosEString)],
			   XMLTWMatchPosE),
	create_xml_element('CWMatchPosS',
			   [],
			   [pcdata(CWMatchPosSString)],
			   XMLCWMatchPosS),
	create_xml_element('CWMatchPosE',
			   [],
			   [pcdata(CWMatchPosEString)],
			   XMLCWMatchPosE),
	create_xml_element('Variation',
			   [],
			   [pcdata(VariationString)],
			   XMLVariation),

	create_xml_element('MatchMap',
			   [],
			   [XMLTWMatchPosS,XMLTWMatchPosE,
			    XMLCWMatchPosS,XMLCWMatchPosE,
			    XMLVariation],
			   MatchMapXML).



generate_xml_is_head(IsHead, XMLIsHead) :-
	atom_codes(IsHead, IsHeadString),
	create_xml_element('IsHead',
			   [],
			   [pcdata(IsHeadString)],
			   XMLIsHead).
generate_xml_is_overmatch(IsOverMatch, XMLIsOverMatch) :-
	atom_codes(IsOverMatch, IsOverMatchString),
	create_xml_element('IsOverMatch',
			   [],
			   [pcdata(IsOverMatchString)],
			   XMLIsOverMatch).


generate_xml_sources(Sources, XMLSources) :-
	generate_xml_sources_list(Sources, XMLSourceList),
	length(Sources, Length),
	number_codes(Length, Count),
	create_xml_element('Sources',
			   ['Count'=Count],
			   XMLSourceList,
   			   XMLSources).

generate_xml_sources_list([], []).
generate_xml_sources_list([FirstSourceAtom|RestSourceAtoms],
			  [FirstSourceXML|RestSourceXMLs]) :-
	generate_one_xml_sources(FirstSourceAtom, FirstSourceXML),
	generate_xml_sources_list(RestSourceAtoms, RestSourceXMLs).

generate_one_xml_sources(SourceAtom, SourceXML) :-
	atom_codes(SourceAtom, SourceString),
	create_xml_element('Source',
			   [],
			   [pcdata(SourceString)],
			   SourceXML).			   

generate_xml_CUI_concept(CUIConceptList, ElementName, XMLCUIConcept) :-
	generate_xml_CUI_concept_list(CUIConceptList, XMLCUIConceptList),
	length(CUIConceptList, Length),
	number_codes(Length, Count),
	create_xml_element(ElementName,
			   ['Count'=Count],
			   XMLCUIConceptList,
   			   XMLCUIConcept).

generate_xml_CUI_concept_list([], []).
generate_xml_CUI_concept_list([FirstCUIConcept|RestCUIConcepts],
			      [FirstCUIConceptXML|RestCUIConceptsXML]) :-
	generate_one_xml_CUI_concept(FirstCUIConcept, FirstCUIConceptXML),
	generate_xml_CUI_concept_list(RestCUIConcepts, RestCUIConceptsXML).


generate_one_xml_CUI_concept(CUI:Concept, CUIConceptXML) :-
	atom_codes(CUI, CUIString),
	atom_codes(Concept,  ConceptString),
	create_xml_element('NegExCUI',
			   [],
			   [pcdata(CUIString)],
			   CUIXML),
	create_xml_element('NegExConcept',
			   [],
			   [pcdata(ConceptString)],
			   ConceptXML),
	create_xml_element('CUIConcept',
			   [],
			   [CUIXML,ConceptXML],
			   CUIConceptXML).




generate_xml_pos_info(PosInfo, ElementName, XMLPosInfo) :-
	generate_xml_pos_info_list(PosInfo, XMLPosInfoList),
	length(PosInfo, Length),
	number_codes(Length, Count),
	create_xml_element(ElementName,
			   ['Count'=Count],
			   XMLPosInfoList,
   			   XMLPosInfo).

generate_xml_pos_info_list([], []).
generate_xml_pos_info_list([FirstPosInfo|RestPosInfos],
			   [FirstPosInfoXML|RestPosInfoXMLs]) :-
	generate_one_xml_pos_info(FirstPosInfo, FirstPosInfoXML),
	generate_xml_pos_info_list(RestPosInfos, RestPosInfoXMLs).


generate_one_xml_pos_info(StartPos/Length, PosInfoXML) :-
	number_codes(StartPos, StartPosString),
	number_codes(Length,   LengthString),
	create_xml_element('StartPos',
			   [],
			   [pcdata(StartPosString)],
			   StartPosXML),
	create_xml_element('SpanLen',
			   [],
			   [pcdata(LengthString)],
			   SpanLenXML),
	create_xml_element('Span',
			   [],
			   [StartPosXML,SpanLenXML],
			   PosInfoXML).

generate_xml_phrase_mappings_term(MappingsList, XMLMappingsTerm) :-
	length(MappingsList, MappingsLength),
	number_codes(MappingsLength, Count),
	generate_xml_mappings_list(MappingsList, XMLMappingsList),
	create_xml_element('Mappings',
			   ['Count'=Count],
			   XMLMappingsList,
			   XMLMappingsTerm).


generate_xml_mappings_list([], []).
generate_xml_mappings_list([FirstMMOMapping|RestMMOMappings],
			   [FirstXMLMapping|RestXMLMappings]) :-
	generate_one_xml_mapping(FirstMMOMapping, FirstXMLMapping),
	generate_xml_mappings_list(RestMMOMappings, RestXMLMappings).
			       
generate_one_xml_mapping(MMOMapping, XMLMapping) :-
	MMOMapping = map(NegScore, CandidatesList),
	number_codes(NegScore, NegScoreString),
	create_xml_element('MapNegScore',
			   [],
			   [pcdata(NegScoreString)],
			   XMLNegScore),
	generate_xml_phrase_candidates_term(CandidatesList, XMLCandidatesTerm),
	create_xml_element('Mapping',
			   [],
			   [XMLNegScore,XMLCandidatesTerm],
			   XMLMapping).

generate_xml_phrase_text(PhraseString, XMLText) :-
	create_xml_element('PText',
			  [],
			  [pcdata(PhraseString)],
			  XMLText).

generate_xml_phrase_syntax(PhraseSyntax, XMLSyntax) :-
	length(PhraseSyntax, PhraseSyntaxLength),
	number_codes(PhraseSyntaxLength, Count),
	generate_xml_syntax_list(PhraseSyntax, TagList),
	create_xml_element('Tags',
			    ['Count'=Count],
			    TagList,
			    XMLSyntax).

generate_xml_syntax_list([], []).
generate_xml_syntax_list([FirstMMOSyntaxElement|RestMMOSyntaxElements],
			 [FirstXMLSyntaxElement|RestXMLSyntaxElements]) :-
	generate_one_xml_syntax_element(FirstMMOSyntaxElement, FirstXMLSyntaxElement),
	generate_xml_syntax_list(RestMMOSyntaxElements, RestXMLSyntaxElements).

generate_one_xml_syntax_element(MMOSyntaxElement, XMLSyntaxElement) :-
	functor(MMOSyntaxElement, TypeAtom, _Arity),
	atom_codes(TypeAtom, TypeString),
	arg(1, MMOSyntaxElement, FeatureList),
	generate_xml_type_term(TypeString, XMLTypeTerm),
	generate_xml_lexmatch_term(FeatureList, XMLLexMatchTerm),
	generate_xml_inputmatch_term(FeatureList, XMLInputMatchTerm),
	generate_xml_POS_term(FeatureList, XMLPOSTerm),
	generate_xml_token_term(FeatureList, XMLTokenTerm),
	% XMLLexMatchTerm and XMLPOSTerm are optional
	% and can therefore be [], so we delete them.
	delete([XMLTypeTerm,XMLLexMatchTerm,
		XMLInputMatchTerm,XMLPOSTerm,XMLTokenTerm], [], XMLFeatureList),
	create_xml_element('Tag',
			   [],
			   XMLFeatureList,
			   XMLSyntaxElement).

generate_xml_type_term(TypeString, XMLType) :-
	create_xml_element('Type',
			   [],
			   [pcdata(TypeString)],
			   XMLType).

generate_xml_lexmatch_term(FeatureList, XMLLexMatch) :-
	( memberchk(lexmatch(LexMatchAtomList), FeatureList) ->
	  atom_codes_list(LexMatchAtomList, LexMatchStringList),
	  form_one_string(LexMatchStringList, " ", LexMatchString),
	  create_xml_element('LexMatch',
			     [],
			     [pcdata(LexMatchString)],
			     XMLLexMatch)
	; XMLLexMatch = []
	).

generate_xml_inputmatch_term(FeatureList, XMLInputMatch) :-
	memberchk(inputmatch(InputMatchAtomList), FeatureList) ->
	atom_codes_list(InputMatchAtomList, InputMatchStringList),
	form_one_string(InputMatchStringList, " ", InputMatchString),
	create_xml_element('InputMatch',
			   [],
			   [pcdata(InputMatchString)],
			   XMLInputMatch).

generate_xml_POS_term(FeatureList, XMLPOS) :-
	( memberchk(tag(LexicalCategoryAtom), FeatureList) ->
	  atom_codes(LexicalCategoryAtom, LexicalCategoryString),
	  create_xml_element('POS',
			     [],
			     [pcdata(LexicalCategoryString)],
			     XMLPOS)
	; XMLPOS = []
	).


generate_xml_token_term(FeatureList, XMLTokenTerm) :-
	memberchk(tokens(MMOTokenList), FeatureList),
	length(MMOTokenList, TokenListLength),
	number_codes(TokenListLength, TokenListLengthString),
	generate_xml_token_list(MMOTokenList, XMLTokenList),
	create_xml_element('Tokens',
			   ['Count'=TokenListLengthString],
			   XMLTokenList,
			   XMLTokenTerm).

generate_xml_token_list([], []).
generate_xml_token_list([FirstMMOTokenAtom|RestMMOTokenAtoms], [FirstXMLToken|RestXMLTokens]) :-
	atom_codes(FirstMMOTokenAtom, TokenString),
	create_xml_element('Token',
			   [],
			   [pcdata(TokenString)],
			   FirstXMLToken),
	generate_xml_token_list(RestMMOTokenAtoms, RestXMLTokens).
			     

generate_xml_phrase_start_pos(PhraseStartPos, XMLPhraseStartPos) :-
	number_codes(PhraseStartPos, PhraseStartPosString),
	create_xml_element('PStartPos',
			   [],
			   [pcdata(PhraseStartPosString)],
			   XMLPhraseStartPos).

generate_xml_phrase_length(PhraseLength, XMLPhraseLength) :-
	number_codes(PhraseLength, PhraseLengthString),
	create_xml_element('PSpanLen',
			   [],
			   [pcdata(PhraseLengthString)],
			   XMLPhraseLength).

% generate_xml_replacement_pos(ReplPos, XMLReplPos) :-
% 	generate_xml_replacement_pos_list(ReplPos, XMLReplPosList),
% 	length(ReplPos, Length),
% 	number_codes(Length, Count),
% 	create_xml_element('ReplList',
% 			   ['Count'=Count],
% 			   XMLReplPosList,
%    			   XMLReplPos).


% generate_xml_replacement_pos_list([], []).
% generate_xml_replacement_pos_list([FirstReplPos|RestReplPos],
% 				  [FirstReplPosXML|RestReplPosXMLs]) :-
% 	generate_one_xml_replacement_pos(FirstReplPos, FirstReplPosXML),
% 	generate_xml_replacement_pos_list(RestReplPos, RestReplPosXMLs).
% 
% generate_one_xml_replacement_pos(ReplPos, ReplPosXML) :-
% 	number_codes(ReplPos, ReplPosString),
% 	create_xml_element('Repl',
% 			   [],
% 			   [pcdata(ReplPosString)],
% 			   ReplPosXML).
% 

generate_xml_utterance_data(UtteranceLabel,
			    UtteranceStartPos, UtteranceLength,
			    MMOUtteranceText,
			    XMLPMID, XMLUtteranceType, XMLUtteranceNumber,
			    XMLUtteranceText,
			    XMLUtteranceStartPos, XMLUtteranceLength) :-
	atom_codes(UtteranceLabel, LabelString),
	split_string_completely(LabelString,
				".",
				[PMIDString,UtteranceTypeString,UtteranceNumberString]),
	number_codes(UtteranceStartPos, StartPosString),
	number_codes(UtteranceLength,   LengthString),
	% RealUtteranceLength is UtteranceLength - 1,
	create_xml_element('PMID',
			   [],
			   [pcdata(PMIDString)],
			   XMLPMID),
	create_xml_element('Location',
			   [],
			   [pcdata(UtteranceTypeString)],
			   XMLUtteranceType),
  	create_xml_element('SeqNo',
			   [],
			   [pcdata(UtteranceNumberString)],
			   XMLUtteranceNumber),
	create_xml_element('UText',
			   [],
			   [pcdata(MMOUtteranceText)],
			   XMLUtteranceText),
	create_xml_element('UStartPos',
			   [],
			   [pcdata(StartPosString)],
			   XMLUtteranceStartPos),
	create_xml_element('USpanLen',
			   [],
			   [pcdata(LengthString)],
			   XMLUtteranceLength).
	% generate_xml_replacement_pos(ReplPos, XMLReplPos).

generate_xml_AA_term(AAsMMO, XMLAATerm) :-
	AAsMMO = aas(AAListMMO),
	length(AAListMMO, AAListLengthAtom),
	number_codes(AAListLengthAtom, AACount),
	generate_xml_AA_list(AAListMMO, AAList),
	create_xml_element('AAs',
			   ['Count'=AACount],
			   AAList,
			   XMLAATerm).


generate_xml_AA_list([], []).
generate_xml_AA_list([Acronym*Expansion*CountList*CUIList|RestAATerms], [XMLAA|RestXMLAAs]) :-
	generate_one_xml_AA_term(Acronym, Expansion, CountList, CUIList, XMLAA),
	generate_xml_AA_list(RestAATerms, RestXMLAAs).

generate_one_xml_AA_term(Acronym, Expansion, CountList, MMOCUIList, XMLAA) :-
	CountList = [NumAATokens, AALen, NumDefnTokens, DefnLen],
	length(MMOCUIList, CUILengthAtom),
	number_codes(CUILengthAtom, CUICount),
	number_codes(NumAATokens, NumAATokensString),
	number_codes(AALen, AALenString),
	number_codes(NumDefnTokens, NumDefnTokensString),
	number_codes(DefnLen, DefnLenString),
	generate_xml_AA_CUI_list(MMOCUIList, XMLCUIList),
	create_xml_element('AAName',
			   [],
			   [pcdata(Acronym)],
			   AANameTerm),
	create_xml_element('AAExpansion',
			   [],
			   [pcdata(Expansion)],
			   AAExpansionTerm),
	create_xml_element('NumAATokens',
			   [],
			   [pcdata(NumAATokensString)],
			   NumAATokensTerm),
	create_xml_element('AALen',
			   [],
			   [pcdata(AALenString)],
			   AALenTerm),
	create_xml_element('NumDefnTokens',
			   [],
			   [pcdata(NumDefnTokensString)],
			   NumDefnTokensTerm),
	create_xml_element('DefnLen',
			   [],
			   [pcdata(DefnLenString)],
			   DefnLenTerm),
	create_xml_element('CUIs',
			   ['Count'=CUICount],
			   XMLCUIList,
			   CUIListTerm),	
	create_xml_element('AA',
			   [],
			   [AANameTerm,AAExpansionTerm,
			    NumAATokensTerm,AALenTerm,
			    NumDefnTokensTerm,DefnLenTerm,CUIListTerm],
			   XMLAA).

generate_xml_AA_CUI_list([], []).
generate_xml_AA_CUI_list([MMOCUI|RestMMOCUIs], [XMLCUI|RestXMLCUIs]) :-
	atom_codes(MMOCUI, CUIString),
	create_xml_element('CUI',
			   [],
			   [pcdata(CUIString)],
			   XMLCUI),
	generate_xml_AA_CUI_list(RestMMOCUIs, RestXMLCUIs).


generate_xml_arg_term(ArgsMMO, XMLArgTerm) :-
	ArgsMMO = args(CommandLineAtom, Options),
	atom_codes(CommandLineAtom, CommandLineString),
	length(Options, OptionsLength),
	number_codes(OptionsLength, CountString),
	generate_xml_option_list(Options, OptionList),
	create_xml_element('Command',
			   [],
			   [pcdata(CommandLineString)],
			   XMLCommandTerm),
	create_xml_element('Options',
			   ['Count'=CountString],
			   OptionList,
			   XMLOptionListTerm),	
	create_xml_element('Args',
			   [],
			   [XMLCommandTerm,XMLOptionListTerm],
			   XMLArgTerm).

				   
generate_xml_option_list([], []).
generate_xml_option_list([Option-Value|RestOptions], [XMLOption|RestXMLOptions]) :-
	generate_one_xml_option(Option, Value, XMLOption),
	generate_xml_option_list(RestOptions, RestXMLOptions).

generate_one_xml_option(OptionNameAtom, OptionValue, XMLOption) :-
	atom_codes(OptionNameAtom, OptionNameString),
	generate_xml_option_value_term(OptionValue, OptionValueTerm),
	create_xml_element('OptName',
			   [],
			   [pcdata(OptionNameString)],
			   OptionNameTerm),
	create_xml_element('Option',
			   [],
			   % OptionValueTerm can be empty
			   [OptionNameTerm|OptionValueTerm],
			   XMLOption).

% OptionValueTerm is optional (!), and it's omitted if the value is []
generate_xml_option_value_term(OptionValue, OptionValueTermList) :-
	( OptionValue == [] ->
	  OptionValueTermList = []
	; convert_to_string(OptionValue, OptionValueString),
	% ; atom_codes(OptionValue, OptionValueString),
	  create_xml_element('OptValue',
			     [],
			     [pcdata(OptionValueString)],
			     OptionValueTerm),
	  OptionValueTermList = [OptionValueTerm]
	).

convert_to_string(OptionValue, OptionValueString) :-
	( atom(OptionValue) ->
	  atom_codes(OptionValue, OptionValueString)
	; number(OptionValue) ->
	  number_codes(OptionValue, OptionValueString)
	; OptionValue == [] ->
	  OptionValueString = "[]"
	; OptionValue = [_|_] ->
	  OptionValueAtomList = OptionValue,
	  atom_codes_list(OptionValueAtomList, OptionValueStringList),
	  form_one_string(OptionValueStringList, ",", OptionValueString)
	).
			   
generate_xml_negex_term(NegExTerm, XMLNegExTerm) :-
	NegExTerm = neg_list(NegExList),
	generate_xml_NegEx_list(NegExList, XMLNegExList),
	length(NegExList, Length),
	number_codes(Length, Count),
	create_xml_element('Negations',
			   ['Count'=Count],
			   XMLNegExList,
			   XMLNegExTerm).

generate_xml_NegEx_list([], []).
generate_xml_NegEx_list([NegEx|RestNegExes], [XMLNegEx|RestXMLNegExes]) :-
	generate_one_xml_NegEx_term(NegEx, XMLNegEx),
	generate_xml_NegEx_list(RestNegExes, RestXMLNegExes).

generate_one_xml_NegEx_term(NegEx, XMLNegEx) :-
	final_negation_template(NegEx,
				NegationType, NegationTrigger, TriggerPosInfo,
				NegatedCUIConceptList,  ConceptPosInfo),
	atom_codes(NegationType, NegationTypeString),
	create_xml_element('NegType',
			   [],
			   [pcdata(NegationTypeString)],
			   XMLNegType),

	atom_codes(NegationTrigger, NegationTriggerString),
	create_xml_element('NegTrigger',
			   [],
			   [pcdata(NegationTriggerString)],
			   XMLNegTrigger),
	generate_xml_pos_info(TriggerPosInfo, 'NTSpans', XMLTriggerPosInfo),

	generate_xml_CUI_concept(NegatedCUIConceptList, 'CUIConcepts', XMLCUIConceptList),
	% atom_codes(NegatedConcept, NegationConceptString),
	% create_xml_element('NegConcept',
	% 		   [],
	% 		   [pcdata(NegationConceptString)],
	% 		   XMLNegConcept),
	generate_xml_pos_info(ConceptPosInfo, 'NCSpans', XMLConceptPosInfo),
	create_xml_element('Negation',
			   [],
			   [XMLNegType,
			    XMLNegTrigger,    XMLTriggerPosInfo,
			    XMLCUIConceptList,XMLConceptPosInfo],
			   XMLNegEx).

create_xml_element(ElementName, AttributeList, Components, XMLStructure) :-
	XMLStructure = element(ElementName, AttributeList, Components).

% xml_disclaimer('<!-- NOTE: The XML below was generated from MMO and not the original text. -->').
% 
% print_xml_disclaimer(0, _).
% print_xml_disclaimer(1, OutputStream) :-
% 	xml_disclaimer(XMLDisclaimer),
% 	format(OutputStream, '~n~w~n~n', [XMLDisclaimer]),
% 	format(OutputStream, '~n~w~n~n', [XMLDisclaimer]).
% 

conditionally_print_xml_header(PrintSetting, OutputStream) :-
	( PrintSetting =:= 1 ->
	  environ('XML_VERSION',XMLVersion),
	  environ('XML_DOCTYPE', XMLDocType),
	  environ('XML_DOCNAME', XMLDocName),
	  environ('XML_DTD', XMLDTD),
	  concat_atoms(['<!', XMLDocType, ' "', XMLDocName, '" "', XMLDTD, '">'], DocType),
	  format(OutputStream, '~w~n~w', [XMLVersion,DocType]),
	  format(OutputStream, '~n<MMOlist>', [])
	; true
	).

conditionally_print_xml_footer(PrintSetting, XMLMode, OutputStream) :-
	( PrintSetting =:= 1 ->
	  conditionally_print_CR(XMLMode, OutputStream),
	  format(OutputStream, '</MMOlist>~n~n', [])
	; true
	).

xml_header_footer_print_setting(InnerOrOuter, XMLMode, PrintSetting) :-
	( control_value('XML', XMLMode) ->
	  get_xml_format_mode(XMLMode, FormatMode),
	  % This is bitwise XOR
	  PrintSetting is \(InnerOrOuter, FormatMode)
	; PrintSetting is 0
	).

% Why can we use XOR here?
% Let's assume XML is on -- otherwise this is all irrelevant anyway.
% If we're at the outer header/footer (InnerOrOuter =:= 1)
% we want to print the header iff the format mode is 0 (format1/noformat1).
% If we're at the inner header/footer (InnerOrOuter =:= 0)
% we want to print the header iff the format mode is 1 (format/noformat).

% The overall structure of MetaMap XML output is the following.
% Note that there will be exactly one of inner and outer headers/footers.
% If the user has requested one XML document for the entire input file
% (format1/noformat1), only the outer header and footer will be printed.
% If the user has requested multiple XML documents, i.e., one per citation
% (format/noformat), only the inner headers and footers will be printed.
% 
% outer XML header                     (for format1/noformat1 only)
% 
%     inner XML header                 (for format/noformat only)
%     inner XML footer                 (for format/noformat only)
% 
%     inner XML header                 (for format/noformat only)
%     inner XML footer                 (for format/noformat only)
% 
%     inner XML header                 (for format/noformat only)
%     inner XML footer                 (for format/noformat only)
% 
% outer XML footer                     (for format1/noformat1 only)
% 

get_xml_format_mode(format1,   0).
get_xml_format_mode(noformat1, 0).
get_xml_format_mode(format,    1).
get_xml_format_mode(noformat,  1).

% :- use_module(library(addportray)).
% portray_mm_output(candidates(_)) :- write('CANDIDATES').
% portray_mm_output(mappings(_)) :- write('MAPPINGS').
% portray_mm_output(phrase(_,_,_,_)) :- write('PHRASE').
% :- add_portray(portray_mm_output).
  
% doit :-
% 	open(xml_call, read, Stream),
% 	read(Stream, Term),
% 	close(Stream),
% 	call(Term).
