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

% File:     skr_fe.pl
% Module:   SKR Front-End
% Author:   Lan
% Purpose:  Provide MetaMap, MMI and SemRep functionality


:- module(skr_fe, [
	% must be exported for SemRep
	form_expanded_sentences/3,
	% must be exported for SemRep
	form_original_sentences/7,
	go/0,
	go/1,
	go/2,
	% called by MetaMap API -- do not change signature!
	initialize_skr/4,
	% called by MetaMap API -- do not change signature!
	postprocess_sentences/9,
	% called by MetaMap API -- do not change signature!
	process_text/11
   ]).

% :- extern(skr_fe_go).
% The first two arguments of skr_text/3 are atoms representing a list of
% lines separated by newline. The last argument is a list of atoms (lines)
% :- extern(skr_text(+atom,+atom,-term)).

% These are the SemRep predicates that are loaded iff we compile SKR
% (i.e., they are NOT loaded if we compile MetaMap).
% Because they are called in the code regardless of which application we compile,
% they must still be defined, even as stubs, so as not to get a warning message.

:- use_module(skr_db(db_access), [
	default_release/1
   ]).

:- use_module(lexicon(qp_lexicon), [
	use_multi_word_lexicon/0
   ]).

:- use_module(metamap(metamap_parsing), [
	collapse_syntactic_analysis/2,
	generate_syntactic_analysis_plus/3,
	generate_syntactic_analysis_plus/4
   ]).

:- use_module(mmi(mmi), [
        do_MMI_processing/4
   ]).

:- use_module(skr(skr), [
	initialize_skr/1,
	skr_phrases/18,
	stop_skr/0
   ]).

:- use_module(skr(skr_text_processing), [
	extract_sentences/9,
	get_skr_text/1
	% is_sgml_text/1
	% warn_remove_non_ascii_from_input_lines/2
   ]).

:- use_module(skr(skr_utilities), [
	conditionally_print_end_info/0,
	conditionally_print_header_info/4,
	compare_utterance_lengths/2,
	do_formal_tagger_output/0,
	do_sanity_checking_and_housekeeping/4,
	generate_bracketed_output/2,
	generate_candidates_output/3,
	generate_EOT_output/1,
	generate_header_output/6,
	generate_mappings_output/5,
	generate_phrase_output/7,
	generate_utterance_output/6,
	generate_variants_output/2,
	get_program_name/1,
	output_should_be_bracketed/1,
	output_tagging/3,
	replace_crs_with_blanks/4,
	send_message/2,
	token_template/5,
	token_template/6,
	usage/0,
        write_MMO_terms/1,
        write_raw_token_lists/2,
        write_sentences/2
   ]).

:- use_module(skr(skr_xml), [
	conditionally_print_xml_header/2,
	conditionally_print_xml_footer/3,
	generate_and_print_xml/1,
	xml_header_footer_print_setting/3
   ]).

:- use_module(skr_lib(efficiency), [
	maybe_atom_gc/2
   ]).

:- use_module(skr_lib(negex), [
	compute_negex/4,
	generate_negex_output/1
   ]).

:- use_module(skr_lib(nls_strings), [
	eliminate_multiple_meaning_designator_string/2,
	form_one_string/3,
	normalized_syntactic_uninvert_string/2,
	split_atom_completely/3,
	trim_whitespace/2,
	trim_whitespace_left/3,
	trim_whitespace_left/2,
	trim_whitespace_right/2
   ]).

:- use_module(skr_lib(nls_system), [
	control_option/1,
	get_control_options_for_modules/2,
	get_from_iargs/4,
	interpret_args/4,
	interpret_options/4,
	parse_command_line/1,
	reset_control_options/1,
	set_control_values/2,
	toggle_control_options/1,
	update_command_line/5
   ]).

:- use_module(skr_lib(pos_info), [
	create_EXP_raw_token_list/6,
	create_UNEXP_raw_token_list/5,
	get_next_token_state/3
   ]).

:- use_module(skr_lib(sicstus_utils), [
	subchars/4,
	ttyflush/0
   ]).

:- use_module(tagger(tagger_access), [
	get_tagger_server_hosts_and_port/3,
	tag_text/7
   ]).

:- use_module(text(text_object_util), [
	higher_order_or_annotation_type/1
   ]).

:- use_module(wsd(wsdmod), [
	get_WSD_server_hosts_and_port/3
   ]).

:- use_module(library(avl), [
	empty_avl/1
   ]).

:- use_module(library(file_systems), [
	close_all_streams/0
   ]).

:- use_module(library(lists), [
	append/2,
	prefix/2,
	rev/2
   ]).

:- use_module(library(system), [
	environ/2
   ]).

/* go
   skr_fe_go
   go(+HaltFlag)
   go(+HaltFlag, +CommandLineTerm)

go/0 is the executive predicate for the SKR Front End.
go/0 uses go/1 with HaltFlag set to halt.
go/1 parses the command line and calls go/2 which controls the processing.  */

go :- go(halt).

go(HaltOption) :-
	parse_command_line(CLTerm),
	go(HaltOption, CLTerm).

go(HaltOption, command_line(Options0,Args0)) :-
	reset_control_options(metamap),
	update_command_line(Options0, Args0, metamap, Options, Args),
	get_program_name(ProgramName),
	default_release(Release),
	close_all_streams,
	( initialize_skr(Options, Args, InterpretedArgs, IOptions) ->
          ( skr_fe(InterpretedArgs, ProgramName, Release, IOptions) ->
	    true
	  ; true
	  )
	; usage
	),
	stop_skr,
	( HaltOption == halt ->
	  halt
	; true
	).

/* initialize_skr(+Options, +Args, -InterpretedArgs, +IOptions)

initialize_skr/4 interprets command line options and arguments (opening
files as necessary) and, sets and displays the SKR control options
discovered, and performs other initialization tasks including initializing
other modules by calling initialize_skr/1.  It returns InterpretedArgs
for later use (e.g., the stream associated with a file).  */

initialize_skr(InitialOptions, InitialArgs, FinalArgs, FinalOptions) :-
	get_control_options_for_modules([metamap], AllOptions),
	interpret_options(InitialOptions, AllOptions, metamap, FinalOptions),
	\+ member(iopt(help,_), FinalOptions),
	ArgSpecs=[aspec(infile,mandatory,file,read,
			user_input,
			'Input file containing labelled utterances'),
		  aspec(outfile,mandatory,file,write,
			or(['<infile>','.','out'],user_output),
			'Output file')
		 ],
	interpret_args(FinalOptions, ArgSpecs, InitialArgs, FinalArgs),
	toggle_control_options(FinalOptions),
	set_control_values(FinalOptions, FinalArgs),
	use_multi_word_lexicon,
	initialize_skr([]).


/* skr_fe(+InterpretedArgs, IOptions)

skr_fe/1 calls either process_all/1 or batch_skr/1 (which redirects
I/O streams and then calls process_all/1) depending on InterpretedArgs.  */

skr_fe(InterpretedArgs, ProgramName, FullYear, IOptions) :-
	set_tag_option_and_mode(TagOption, TagMode),
	get_tagger_server_hosts_and_port(TaggerServerHosts, TaggerForced, TaggerServerPort),
	get_WSD_server_hosts_and_port(WSDServerHosts, WSDForced, WSDServerPort),
	% XML header will be printed here (and XML footer several lines below) iff
	% (1) XML command-line option is on, and
	% (2) OutputChoice is 1 (which is manually forced)
	% (3) XML value is either XMLf1 or XMLn1
	% get_xml_settings(XMLOption, FormatMode, XMLValue),
	xml_header_footer_print_setting(1, XMLMode, PrintSetting),
	get_output_stream(OutputStream),
	conditionally_print_xml_header(PrintSetting, OutputStream),
	( InputStream = user_input,
	  get_from_iargs(infile, name, InterpretedArgs, InputStream) ->
	  % process_all is for user_input
          process_all(ProgramName, FullYear, InputStream, OutputStream, TagOption,
		      TaggerServerHosts, TaggerForced, TaggerServerPort,
		      WSDServerHosts, WSDForced, WSDServerPort,
		      InterpretedArgs, IOptions)
          % batch_skr is for batch processing
        ; batch_skr(InterpretedArgs, ProgramName, FullYear,
		    TaggerServerHosts, TaggerForced, TaggerServerPort,
		    WSDServerHosts, WSDForced, WSDServerPort,
		    TagOption, TagMode, IOptions, InputStream)
	),
	% conditionally_print_xml_footer/4 will be called here
	% only if MetaMap is in batch mode, because otherwise (in interactive use)
	% the user will have control-C-ed the program or exited it altogether.
	conditionally_print_xml_footer(PrintSetting, XMLMode, OutputStream),
	generate_EOT_output(OutputStream),
	close(OutputStream),
	close(InputStream).

get_output_stream(OutputStream) :-
	( current_stream(_File, write, OutputStream) ->
	  true
	; OutputStream = user_output
	).

set_tag_option_and_mode(TagOption, TagMode) :-
	( control_option(no_tagging) ->
	  TagOption = notag,
	  TagMode   = '(no tagging)'
	; TagOption = tag,
	  TagMode   = ''
	).

/* batch_skr(+InterpretedArgs, +ProgramName, +FullYear,
   	     +TagOption, +TagMode, +IOptions)

batch_skr/6 controls batch processing.  It gets input and output
file names from InterpretedArgs and redirects I/O to them.
Meta concepts are computed for each noun phrase in the input.  */

batch_skr(InterpretedArgs, ProgramName, FullYear,
	  TaggerServerHosts, TaggerForced, TaggerServerPort,
	  WSDServerHosts, WSDForced, WSDServerPort,
	  TagOption, TagMode, IOptions, InputStream) :-
	get_from_iargs(infile,  name,   InterpretedArgs, InputFile),
	get_from_iargs(infile,  stream, InterpretedArgs, InputStream),
	get_from_iargs(outfile, name,   InterpretedArgs, OutputFile),
	get_from_iargs(outfile, stream, InterpretedArgs, OutputStream),
	conditionally_print_header_info(InputFile, TagMode, OutputFile, TagOption),
	set_input(InputStream),
	set_output(OutputStream),
	( process_all(ProgramName, FullYear, InputStream, OutputStream,
		      TagOption, TaggerServerHosts, TaggerForced, TaggerServerPort,
		      WSDServerHosts, WSDForced, WSDServerPort,
		      InterpretedArgs, IOptions) ->
	  true
	; true
	),
	!,
	conditionally_print_end_info.

/* process_all(+TagOption, +TaggerServerHosts, +TaggerForced, +TaggerServerPort,
	       +WSDServerHosts, +WSDForced, +WSDServerPort,
   	       +InterpretedArgs, +IOptions)

process_all/9 reads utterances from user_input and performs all SKR
processing (MetaMapping, MMI processing, Semantic interpretation).
user_input and user_output may have been redirected to files.)
If TagOption is 'tag', then tagging is done. */

process_all(ProgramName, FullYear, InputStream, OutputStream,
	    TagOption, TaggerServerHosts, TaggerForced, TaggerServerPort,
	    WSDServerHosts, WSDForced, WSDServerPort,
	    InterpretedArgs, IOptions) :-
	do_sanity_checking_and_housekeeping(ProgramName, FullYear, InputStream, OutputStream),
	process_all_1(TagOption, TaggerServerHosts, TaggerForced, TaggerServerPort,
		      WSDServerHosts, WSDForced, WSDServerPort,
		      InterpretedArgs, IOptions).


process_all_1(TagOption, TaggerServerHosts, TaggerForced, TaggerServerPort,
	      WSDServerHosts, WSDForced, WSDServerPort,
	      InterpretedArgs, IOptions) :-
	get_skr_text(Strings0),
	Strings0 = [FirstString|RestStrings],
	trim_whitespace_from_last_string(RestStrings, FirstString, TrimmedStrings),
	( TrimmedStrings \== [] ->
	  process_text(TrimmedStrings,
		       TagOption, TaggerServerHosts, TaggerForced, TaggerServerPort,
		       WSDServerHosts, WSDForced, WSDServerPort,
		       ExpRawTokenList, AAs, MMResults),
	  output_should_be_bracketed(BracketedOutput),
 	  postprocess_text(TrimmedStrings, BracketedOutput, InterpretedArgs,
			   IOptions, ExpRawTokenList, AAs, MMResults),
	  process_all_1(TagOption, TaggerServerHosts, TaggerForced, TaggerServerPort,
			WSDServerHosts, WSDForced, WSDServerPort,
			InterpretedArgs, IOptions)
	; true
	),
	!.

trim_whitespace_from_last_string([], LastString, [TrimmedLastString]) :-
	trim_whitespace_right(LastString, TrimmedLastString).
trim_whitespace_from_last_string([NextString|RestStrings], FirstString, [FirstString|TrimmedStrings]) :-
	trim_whitespace_from_last_string(RestStrings, NextString, TrimmedStrings).

/* process_text(+Lines,
   		+TagOption, +TaggerServerHosts, +TaggerForced, +TaggerServerPort,
		+WSDServerHosts, +WSDForced, +WSDServerPort,
		-ExpRawTokenList, -MMResults)

   postprocess_text(+Lines, +BracketedOutputFlag, +InterpretedArgs,
   	            +IOptions, +ExpRawTokenList, +MMResults)

   postprocess_sentences(+OrigUtterances, +NegationTerms, +InterpretedArgs, +IOptions,
			 +Sentences, +CoordSentences, +BracketedOutput, +DisambMMOutput,
			 _AllMMO)
   postprocess_phrases(+MMOPhrases, +ExtractedPhrases,
                       +Sentences, +CoordSentencesIn, -CoordSentencesOut,
                       +N, +M, +Label, -PhraseMMO)

process_text/11 maps Lines (in one of the formats recognized by
extract_sentences/5) using the auxiliary process_text/3 which processes
ExpandedUtterances (expanded utterances, e.g., in which acronyms/abbreviations
are replaced with their expanded definitions) and produces MMOutput
which is packaged up with preliminary results and returned as MMResults.

Standard input and output are assumed to be redirected to files.
Tagging is done if TagOption is 'tag'.

process_text/4 does the initial MetaMap processing without writing any results.

postprocess_text/6 has two major functions:
  o it performs word sense disambiguation (WSD) on MMResults; and
  o it does any required writing of results;
    one such required writing of results is Machine Output.
It uses postprocess_sentences/9 which uses postprocess_phrases/8
to process each sentence and phrase.

Written results take account of position information in CoordSentences to
match up the text in ExpandedUtterances (expanded sentences) with the original text
in Sentences in case the original text is preferred. */

% Suppress processing of options within data
% process_text([Lines],_TagOption,reset) :-
%	process_any_new_options(Lines),
%	!.

% MMResults is created by process text, and is one term of the form
% mm_results(Lines0, TagOption, ModifiedLines, InputType,
%            Sentences, CoordSentences, OrigUtterances, MMOutput),
% 
% MMOutput is created by process_text_aux and skr_phrase, and is a list of terms of the form
%
% mm_output(ExpandedUtterance, CitationTextAtom, ModifiedText, Tagging,
%           AAs, Syntax, DisambiguatedMMOPhrases, ExtractedPhrases)
%
% DisambiguatedMMOPhrases = list of
%
% phrase(phrase(PhraseTextAtom0,Phrase,StartPos/Length,ReplacementPos),
% 	 candidates(Evaluations),
% 	 mappings(Mappings),
% 	 pwi(PhraseWordInfo),
% 	 gvcs(GVCs),
% 	 ev0(Evaluations3),
% 	 aphrases(APhrases))

%%% DO NOT MODIFY process_text without checking with the maintainer of the MetaMap API.
process_text(Text,
	     TagOption, TaggerServerHosts, TaggerForced, TaggerServerPort,
	     WSDServerHosts, WSDForced, WSDServerPort,
	     RawTokenList, AAs, MMResults) :-
	( process_text_1(Text,
			 TagOption, TaggerServerHosts, TaggerForced, TaggerServerPort,
			 WSDServerHosts, WSDForced, WSDServerPort,
			 RawTokenList, AAs, _UDAs, MMResults) ->
	  true
 	; send_message('#### ERROR: process_text/4 failed for~n~p~n', [Text]),
	  abort
	).

process_text_1(Lines0,
	       TagOption, TaggerServerHosts, TaggerForced, TaggerServerPort,
	       WSDServerHosts, WSDForced, WSDServerPort,
	       ExpRawTokenList, AAs, UDAs, MMResults) :-
	MMResults = mm_results(Lines0, TagOption, ModifiedLines, InputType,
			       Sentences, CoordSentences, OrigUtterances, MMOutput),
	ModifiedLines = modified_lines(Lines),
	% Sentences and CoordSentences are the token lists
	% CoordSentences includes the positional information for the original text
	% format(user_output, 'Processing PMID ~w~n', [PMID]),
	extract_sentences(Lines0, InputType, TextFields, NonTextFields,
			  Sentences, CoordSentences, AAs, UDAs, Lines),
	% need to update Lines
	% fail,
	% RawTokenList is the copy of Sentences that includes an extra pos(_,_) term
	% showing the position of each token in the raw, undoctored text.
	form_one_string(Lines, [10], InputStringWithCRs),
	form_one_string(Lines, " ",  InputStringWithBlanks),

	write_sentences(CoordSentences, Sentences),

	% '' is just the previous token type, which, for the initial call, is null
	TokenState is 0,
	atom_codes(CitationTextAtomWithCRs, InputStringWithCRs),
	% Here I need to change to blanks everything other than then text of text fields
 	convert_non_text_fields_to_blanks(InputType, InputStringWithBlanks,
 					  TextFields, NonTextFields,
					  TrimmedTextFieldsOnlyString, NumBlanksTrimmed),
	CurrentPos is NumBlanksTrimmed,
 	% format(user_output, '~n~s~n~n~s~n', [InputStringWithBlanks,TextFieldsOnlyString]),
	
	create_EXP_raw_token_list(Sentences, '',
				  CurrentPos, TokenState,
				  TrimmedTextFieldsOnlyString, TempExpRawTokenList),
		
				  % InputStringWithBlanks, TempExpRawTokenList),
	ExpRawTokenList = TempExpRawTokenList,
	create_UNEXP_raw_token_list(Sentences,
				    CurrentPos, TokenState,
				    TrimmedTextFieldsOnlyString, UnExpRawTokenList),
				   % InputStringWithBlanks, UnExpRawTokenList),
	!,
	% halt,
	% length(Sentences,         SentencesLength),
	% length(UnExpRawTokenList, UnExpRawTokenListLength),

	% length(CoordSentences,  CoordSentencesLength),
	% length(ExpRawTokenList, ExpRawTokenListLength),

	% format(user_output,
	%        'LENGTHS:~nSent:  ~w~nUnExp: ~w~n~nCoord: ~w~nExp:   ~w~n',
	% 	[SentencesLength,UnExpRawTokenListLength,
	% 	 CoordSentencesLength,ExpRawTokenListLength]),

	write_raw_token_lists(ExpRawTokenList, UnExpRawTokenList),
	form_original_sentences(Sentences, 1, 0, 0, CitationTextAtomWithCRs,
				UnExpRawTokenList, OrigUtterances),
	% format(user_output, '~n~n#### ~w OrigUtterances:~n', [PMID]),
	% write_utterances(OrigUtterances),
	% form_original_sentences(Sentences, OrigUtterances),
	% temp
	%    format('OrigUtterances:~n',[]),
	%    wl(OrigUtterances),
	% form_expanded_sentences(CoordSentences, 1, 0, 0, ExpRawTokenList, ExpandedUtterances),
	form_expanded_sentences(CoordSentences, OrigUtterances, ExpandedUtterances),
	% format(user_output, '~n~n#### ~w ExpandedUtterances:~n', [PMID]),
	% write_utterances(ExpandedUtterances),
	% format(user_output, '~n~n', []),
	compare_utterance_lengths(OrigUtterances, ExpandedUtterances),
	ttyflush,
	MMOutput \== '',
	ExpRawTokenList \== '',
	empty_avl(WordDataCacheIn),
	empty_avl(USCCacheIn),
	process_text_aux(ExpandedUtterances,
			 TagOption,  TaggerServerHosts, TaggerForced, TaggerServerPort,
			 WSDServerHosts, WSDForced, WSDServerPort,
			 CitationTextAtomWithCRs, AAs, UDAs,
			 ExpRawTokenList, WordDataCacheIn, USCCacheIn,
			 _RawTokenListOut, _WordDataCacheOut, _USCacheOut, MMOutput),
        !.

process_text_aux([],
		 _TagOption, _TaggerServerHosts, _TaggerForced, _TaggerServerPort,
		 _WSDServerHosts, _WSDForced, _WSDServerPort,
		 _CitationTextAtom, _AAs, _UDAs,
		 ExpRawTokenList, WordDataCache, USCCache,
		 ExpRawTokenList, WordDataCache, USCCache, []).
process_text_aux([ExpandedUtterance|Rest],
		 TagOption, TaggerServerHosts, TaggerForced, TaggerServerPort,
		 WSDServerHosts, WSDForced, WSDServerPort,
		 CitationTextAtom, AAs, UDAs,
		 ExpRawTokenListIn, WordDataCacheIn, USCCacheIn,
		 ExpRawTokenListOut, WordDataCacheOut, USCCacheOut,
		 [mm_output(ExpandedUtterance,CitationTextAtom,ModifiedText,Tagging,AAs,
			    Syntax,DisambiguatedMMOPhrases,ExtractedPhrases)|RestMMOutput]) :-
	% Construct output terms
	ModifiedText = modified_text(UtteranceText),
	Tagging = tagging(TagOption,FullTagList,TagList,HRTagStrings),
	Syntax = syntax(SyntAnalysis0, SyntAnalysis, Definitions),
	% DisambiguatedMMOPhrases and ExtractedPhrases are passed back as lists (historical)
	% Decompose input terms
	ExpandedUtterance = utterance(InputLabel,Text0,_PosInfo,_ReplacementPos),
	conditionally_announce_processing(InputLabel, Text0),
	maybe_atom_gc(_DidGC,_SpaceCollected),
	set_utterance_text(Text0, UtteranceText),
	do_syntax_processing(TagOption, TaggerServerHosts, TaggerForced, TaggerServerPort,
			     UtteranceText, FullTagList, TagList,
			     HRTagStrings, Definitions, SyntAnalysis0),
	conditionally_collapse_syntactic_analysis(SyntAnalysis0, SyntAnalysis),
	skr_phrases(InputLabel, UtteranceText, CitationTextAtom,
		    AAs, UDAs, SyntAnalysis, WordDataCacheIn, USCCacheIn, ExpRawTokenListIn,
		    WSDServerHosts, WSDForced, WSDServerPort,
		    ExpRawTokenListNext, WordDataCacheNext, USCCacheNext,
		    DisambiguatedMMOPhrases, ExtractedPhrases, _SemRepPhrases),
	process_text_aux(Rest, TagOption, TaggerServerHosts, TaggerForced, TaggerServerPort,
			 WSDServerHosts, WSDForced, WSDServerPort,
			 CitationTextAtom, AAs, UDAs,
			 ExpRawTokenListNext, WordDataCacheNext, USCCacheNext,
			 ExpRawTokenListOut, WordDataCacheOut, USCCacheOut, RestMMOutput).

postprocess_text(Lines0, BracketedOutput, InterpretedArgs,
		 IOptions,  ExpRawTokenList, AAs, MMResults) :-
	% If the phrases_only debug option is set, don't do postprocessing,
	% because we've already computed and displayed the phrase lengths,
	% and that's all we care about if this option is on.
	( control_option(phrases_only) ->
	  true
	; postprocess_text_1(Lines0, BracketedOutput, InterpretedArgs,
			     IOptions,  ExpRawTokenList, AAs, MMResults) ->
	  true
	; send_message('### ERROR: postprocess_text/2 failed for~n~p~n', [Lines0]),
	  abort
	).

postprocess_text_1(Lines0, BracketedOutput, InterpretedArgs,
		   IOptions, ExpRawTokenList, AAs, MMResults) :-
	% Decompose input
	MMResults = mm_results(Lines0, _TagOption, _ModifiedLines, _InputType,
			       Sentences, _CoordSentences, OrigUtterances, DisambMMOutput),

	compute_negex(ExpRawTokenList, Lines0, DisambMMOutput, NegationTerms),
	generate_negex_output(NegationTerms),
	postprocess_sentences(OrigUtterances, NegationTerms, InterpretedArgs, IOptions, AAs,
			      Sentences, BracketedOutput, DisambMMOutput, AllMMO),
	% All the XML output for the current citation is handled here
	generate_and_print_xml(AllMMO),
	do_MMI_processing(OrigUtterances, BracketedOutput, Sentences, DisambMMOutput),
	do_formal_tagger_output.

%%% DO NOT MODIFY process_text without checking with the maintainer of the MetaMap API.
postprocess_sentences(OrigUtterances, NegExList, IArgs, IOptions, AAs,
		      Sentences, BracketedOutput, DisambMMOutput, AllMMO) :-
	AllMMO = HeaderMMO,
	HeaderMMORest = UtteranceMMO,
	generate_header_output(IArgs, IOptions, NegExList, DisambMMOutput,
			       HeaderMMO, HeaderMMORest),
	postprocess_sentences_1(OrigUtterances, Sentences,
				BracketedOutput, 1, AAs, DisambMMOutput, UtteranceMMO, []),
	write_MMO_terms(AllMMO).

postprocess_sentences_1([], _Sentences, _BracketedOutput, _N, _AAs, [], MMO, MMO) :- !.
postprocess_sentences_1([OrigUtterance|RestOrigUtterances], Sentences,
			BracketedOutput, N, AAs, [MMOutput|RestMMOutput],
			MMOIn, MMOOut) :-
	% Decompose input
	OrigUtterance = utterance(Label, TextString, StartPos/Length, ReplPos),
	MMOutput = mm_output(_ExpandedUtterance, _Citation, _ModifiedText, Tagging,
			     _AAs, _Syntax, MMOPhrases, ExtractedPhrases),
	Tagging = tagging(_TagOption, FullTagList, _TagList, HRTagStrings),
	generate_utterance_output(Label, TextString, StartPos, Length, ReplPos, UtteranceMMO),
	% CHANGE
	MMOIn = [UtteranceMMO|MMONext1],
	output_tagging(BracketedOutput, HRTagStrings, FullTagList),
	postprocess_phrases(MMOPhrases, ExtractedPhrases,
			    Sentences, BracketedOutput, N, 1, AAs, Label, MMONext1, MMONext2),
	!,
	NextN is N + 1,
	postprocess_sentences_1(RestOrigUtterances, Sentences,
				BracketedOutput, NextN, AAs, RestMMOutput,
				MMONext2, MMOOut).
postprocess_sentences_1([FailedUtterance|_], _Sentences,
			_BracketedOutput, _N, _AAs, _MMOutput, _MMOIn, _MMOOut) :-
	FailedUtterance = utterance(UtteranceID,UtteranceText,_PosInfo,_ReplPos),
	send_message('### ERROR: postprocess_sentences/3 failed on sentence ~w:~n       "~s"~n',
		     [UtteranceID,UtteranceText]),
	abort.


postprocess_phrases([], [], _Sentences, _BracketedOutput, _N, _M, _AAs, _Label, MMO, MMO) :- !.
postprocess_phrases([MMOPhrase|RestMMOPhrases],
		    [_ExtractedPhrase|RestExtractedPhrases],
		    Sentences, BracketedOutput, N, M, AAs, Label, MMOIn, MMOOut) :-
	MMOPhrase = phrase(phrase(PhraseTextAtom0,Phrase,StartPos/Length,ReplacementPos),
			   candidates(Evaluations),
			   mappings(Mappings),
			   pwi(PhraseWordInfo),
			   gvcs(GVCs),
			   ev0(Evaluations3),
			   aphrases(APhrases)),
	PhraseTextAtom = PhraseTextAtom0,
	% format('PhraseTextAtom:~n~p~n',[PhraseTextAtom]),
	generate_phrase_output(PhraseTextAtom, Phrase, StartPos, Length, ReplacementPos,
			       BracketedOutput, PhraseMMO),
	generate_bracketed_output(BracketedOutput, PhraseWordInfo),
	generate_variants_output(GVCs, BracketedOutput),
	generate_candidates_output(Evaluations3, BracketedOutput, CandidatesMMO),
	generate_mappings_output(Mappings, Evaluations, APhrases,
				 BracketedOutput, MappingsMMO),
	% format('PhraseTextAtom:~n~p~n',[PhraseTextAtom]),
	% CHANGE
	MMOIn = [PhraseMMO,CandidatesMMO,MappingsMMO|MMONext],
	NextM is M + 1,
	postprocess_phrases(RestMMOPhrases, RestExtractedPhrases,
			    Sentences, BracketedOutput, N, NextM, AAs, Label, MMONext, MMOOut).
/*
   form_original_sentences(+Sentences, +StartPos, +EndPos, +TokenState, +CitationTextAtom,
			   +RawTokenList, -OrigUtterances)
   form_original_sentences_aux(+Sentences, +StartPos, +EndPos, +TokenState, +CitationTextAtom,
   			       +RawTokenList, +Label, +RevTexts, -OrigUtterances)

form_original_sentences/7 extracts OrigUtterances from Sentences.
(See skr_text_processing:extract_sentences/4 for a description of Sentences.)

OrigUtterances is a list of terms of the form
  utterance(<Label>,<Text>,<PosInfo>,<ReplacementPos>)
where <Label> is an atom identifying the sentence, e.g., UI.<field>.<n>,
      <Text> is a string consisting of the original sentence, and
      <PosInfo> is a StartPos/Length pair representing
          * the starting character position of the utterance in the original text and
          * the number of characters of the utterance in the original text
      <ReplacementPos> is a list of integers representing the character positions
          in Text in which <CR>s have been replaced by blanks
form_original_sentences_aux/9 is an auxiliary that keeps track of the Label to use
for each sentence and the accumulated text strings, RevTexts.

TokenState represents if we're in the middle of a sentence.
TokenState == 0 means that we have NOT YET consumed a regular token (e.g., an, ic, lc, etc.)
		in the current sentence.
TokenState == 1 means that we have ALREADY consumed a regular token in the current sentence.

The terms here are utterance/3 for historical reasons. */

form_original_sentences(Sentences, StartPos, EndPos, TokenState, CitationTextAtom,
			RawTokenList, OrigUtterances) :-
	form_original_sentences_aux(Sentences, StartPos, EndPos, TokenState, CitationTextAtom,
				    RawTokenList, '', [], OrigUtterances).

% If there are no more tokens in the sentence list (first arg == [])
% and the RevText list is empty, because we haven't been accumulating any text,
% then just terminate, and return [] as the final tail of OrigUtterances.

form_original_sentences_aux([], _StartPos, _EndPos, _TokenState, _CitationTextAtom,
			    _RawTokenList, _Label, [], []) :-
	!.
% Add StartPos/EndPos to the utterance term
% because we're at the end of the token list.
form_original_sentences_aux([], StartPos, EndPos, _TokenState, CitationTextAtom, _RawTokenList, 
			    Label, _RevTexts,
			    [utterance(Label,OrigText,StartPos/Length,ReplPos)]) :-
	!,
	% rev(RevTexts, Texts),
	% append(Texts, Text0),
	% trim_whitespace(Text0, Text),
	TempLength is EndPos - StartPos,
	subchars(CitationTextAtom, OrigTextWithCRs, StartPos, TempLength),
	replace_crs_with_blanks(OrigTextWithCRs, StartPos, OrigText, ReplPos),
	length(OrigTextWithCRs, OrigTextWithCRsLength),
	length(OrigText, OrigTextLength),
	LengthDiff is OrigTextWithCRsLength - OrigTextLength,
	Length is TempLength - LengthDiff.	
% Skip a "label" field if there is no previous RevText,
% but convert the Label text to an atom and pass it along.
% form_original_sentences_aux([tok(label,TokLabel,_,_)|Rest], StartPos, EndPos,
form_original_sentences_aux([Token|Rest], StartPos, EndPos,
			    TokenState,  CitationTextAtom,
			    [_|RestRawTokenList], _Label, [], OrigUtterances) :-
	token_template(Token, label, TokLabel, _PosInfo1, _PosInfo2),
	!,
	atom_codes(NewLabel, TokLabel),
	form_original_sentences_aux(Rest, StartPos, EndPos, TokenState, CitationTextAtom,
				    RestRawTokenList, NewLabel, [], OrigUtterances).
% Add StartPos/EndPos to the utterance term
% because the current token is a label, and thus begins the next utterance.
% form_original_sentences_aux([tok(label,TokLabel,_,_)|Rest], StartPos, EndPos,
form_original_sentences_aux([Token|Rest], StartPos, EndPos,
			    TokenState, CitationTextAtom,
			    [_|RestRawTokenList], Label, _RevTexts,
			    [utterance(Label,OrigText,StartPos/Length,ReplPos)|RestOrigUtterances]) :-
	token_template(Token, label, TokLabel, _PosInfo1, _PosInfo2),
	!,
	% rev(RevTexts, Texts),
	% append(Texts, Text0),
	% trim_whitespace(Text0, Text),
	atom_codes(NewLabel, TokLabel),

	TempLength is EndPos - StartPos,
	subchars(CitationTextAtom, OrigTextWithCRs, StartPos, TempLength),
	replace_crs_with_blanks(OrigTextWithCRs, StartPos, OrigText, ReplPos),
	length(OrigTextWithCRs, OrigTextWithCRsLength),
	length(OrigText, OrigTextLength),
	LengthDiff is OrigTextWithCRsLength - OrigTextLength,
	Length is TempLength - LengthDiff,
	form_original_sentences_aux(Rest, StartPos, EndPos, TokenState, CitationTextAtom,
				    RestRawTokenList, NewLabel, [], RestOrigUtterances).
% form_original_sentences_aux([tok(TokenType,_,_,_)|Rest], StartPos, EndPos,
form_original_sentences_aux([Token|Rest], StartPos, EndPos,
			    TokenState, CitationTextAtom,
			    [RawToken|RestRawTokens], Label, RevTexts, OrigUtterances) :-
	% skip token types
	% field, sn, pe (higher order), and
	% aa, and aadef (annnotation) altogether
	token_template(Token, TokenType, _TokenString, _PosInfo1, _PosInfo2),
	higher_order_or_annotation_type(TokenType),
	!,
	get_next_token_state(TokenState, TokenType, NextTokenState),
	consume_matching_raw_token(TokenType, RawToken, RestRawTokens, NewRestRawTokens),
	form_original_sentences_aux(Rest, StartPos, EndPos, NextTokenState, CitationTextAtom,
				    NewRestRawTokens, Label, RevTexts, OrigUtterances).
% Token here is a normal token (an, ic, uc, mc, ws, pn, etc.)
% Set the StartPos (the starting character position of the utterance) IFF
% (1) the token is not a higher_order_or_annotation_type (which is this clause), and
% (2) the TokenState is 0 (meaning we're not currently in a sentence).
% In other words, set the utterance's StartPos
% at the first normal token after an sn token.
% form_original_sentences_aux([tok(TokenType,TokenText,_,_)|Rest],
form_original_sentences_aux([Token|Rest],
			    CurrStartPos, _CurrEndPos, TokenState, CitationTextAtom,
			    [RawToken|RestRawTokenList],
			    Label, RevTexts, OrigUtterances) :-
	token_template(Token, TokenType, TokenText, _PosInfo1, _PosInfo2),
	get_next_token_state(TokenState, TokenType, NextTokenState),
	RawToken = tok(_Type, _String, _LCStr, _Pos1, pos(RawTokStartPos,RawTokLength)),
	set_utterance_start_end_pos(TokenState,
				    RawTokStartPos, RawTokLength,
				    CurrStartPos, NewStartPos, NewEndPos),
	% format(user_output, 'Orig: ~s ~w~n', [TokenText, NewEndPos]),
	form_original_sentences_aux(Rest, NewStartPos, NewEndPos, NextTokenState, CitationTextAtom,
				    RestRawTokenList, Label,
				    [TokenText|RevTexts], OrigUtterances).

consume_matching_raw_token(TokenType, RawToken, RestRawTokens, NewRestRawTokens) :-
	token_template(RawToken, TokenType, _TokenString, _LCTokenString, _PosInfo1, _PosInfo2),
	NewRestRawTokens = RestRawTokens.

set_utterance_start_end_pos(0, RawTokStartPos, RawTokLength,
			   _CurrStartPos, NewStartPos, NewEndPos) :-
	NewStartPos is RawTokStartPos,
	NewEndPos is NewStartPos + RawTokLength.
	% atom_codes(TokenAtom, TokenString).
	% RawTokStartPos is the starting character position of the utterance,
	% because it's the first printing token after an sn token,
	% i.e., when the TokenState is 0.
	% format(user_output, '~w StartPos ~w~n', [TokenAtom, NewStartPos]).


set_utterance_start_end_pos(1, RawTokStartPos, RawTokLength,
			    CurrStartPos, NewStartPos, NewEndPos) :-
	NewStartPos is CurrStartPos,
	NewEndPos is RawTokStartPos + RawTokLength.
% The calculation of NewEndPos needs some explanation:

/* form_expanded_sentences(+CoordSentences, +OrigUtterances, -ExpandedSentences)
   form_expanded_sentences(+CoordSentences, +OrigUtterances, +Label, +RevTexts,
                           -ExpandedSentences)

OrigUtterances is passed in simply to copy the positional information.

form_expanded_sentences/3 extracts ExpandedSentences from CoordSentences.
(See skr_text_processing:extract_sentences/4 for a description of
CoordSentences.

ExpandedSentences is a list of terms of the form
  utterance(<label>,<text>)
where <label> is an atom identifying the sentence, e.g., UI.<field>.<n> and
      <text> is a string consisting of the expanded sentence.

form_expanded_sentences/4 is an auxiliary that keeps track of the Label to use
for each sentence and the accumulated text strings, RevTexts.

The terms here are utterance/2 for historical reasons. */

form_expanded_sentences(CoordSentences, OrigUtterances, ExpandedSentences) :-
        form_expanded_sentences_aux(CoordSentences, OrigUtterances, '', [], ExpandedSentences).

form_expanded_sentences_aux([], [],_Label, [], []) :-
        !.
form_expanded_sentences_aux([], [utterance(Label,_OrigText,StartPos/Length,ReplPos)],
			    Label, RevTexts, [utterance(Label,Text,StartPos/Length,ReplPos)]) :-
        !,
        rev(RevTexts,Texts),
        append(Texts,Text0),
        trim_whitespace(Text0,Text).
% This is for the first label token in a citation,
% because the next-to-last argument,
% which holds the previous utterance's RevTexts, is [].
% form_expanded_sentences_aux([tok(label,Label,_LCLabel,_,_)|Rest], OrigUtterances,
form_expanded_sentences_aux([Token|Rest], OrigUtterances,
			    _NoPrevLabel, [], ExpandedSentences) :-
	token_template(Token, label, Label, _LCLabel, _PosInfo1, _PosInfo2),
        !,
        atom_codes(NewLabel, Label),
        form_expanded_sentences_aux(Rest, OrigUtterances, NewLabel, [], ExpandedSentences).
% form_expanded_sentences_aux([tok(label,NextLabel,_NextLCLabel,_,_)|Rest],
form_expanded_sentences_aux([Token|Rest],
			    [utterance(PrevLabel,_OrigText,StartPos/Length,ReplPos)
			        |RestOrigUtterances],
			    PrevLabel, RevTexts,
                           [utterance(PrevLabel,Text,StartPos/Length,ReplPos)
			        |RestExpandedSentences]) :-
	token_template(Token, label, NextLabel, _LCNextLabel, _PosInfo1, _PosInfo2),
	!,
        rev(RevTexts,Texts),
        append(Texts,Text0),
        trim_whitespace(Text0,Text),
        atom_codes(NewLabel,NextLabel),
        form_expanded_sentences_aux(Rest, RestOrigUtterances, NewLabel, [], RestExpandedSentences).
% form_expanded_sentences_aux([tok(Type,_,_,_,_)|Rest], OrigUtterances,
form_expanded_sentences_aux([Token|Rest], OrigUtterances,
			    Label, RevTexts, ExpandedSentences) :-
	token_template(Token, Type, _TokenString, _LCTokenString, _PosInfo1, _PosInfo2),
        higher_order_or_annotation_type(Type),
        !,
        form_expanded_sentences_aux(Rest, OrigUtterances, Label, RevTexts, ExpandedSentences).
% form_expanded_sentences_aux([tok(_Type,TokenText,_,_,_)|Rest], OrigUtterances,
form_expanded_sentences_aux([Token|Rest], OrigUtterances,
			    Label, RevTexts, ExpandedSentences) :-
	token_template(Token, _TokenType, TokenText, _LCTokenText, _PosInfo1, _PosInfo2),
        % temp
        %    format('  ~a:"~s"~n',[Type,TokenText]),
        form_expanded_sentences_aux(Rest, OrigUtterances,
				    Label, [TokenText|RevTexts], ExpandedSentences).

conditionally_announce_processing(InputLabel, Text0) :-
	telling(CurrentOutput),
	( CurrentOutput == user ->
	  true
	; \+ control_option(silent) ->
	  format(user_output,'~nProcessing ~a: ~s~n', [InputLabel,Text0]),
	  flush_output(user_output)
	; true
	),
	flush_output(CurrentOutput).

set_utterance_text(Text0, UtteranceText) :-
	( control_option(term_processing) ->
	  eliminate_multiple_meaning_designator_string(Text0, Text1),
	  normalized_syntactic_uninvert_string(Text1, UtteranceText)
	; UtteranceText = Text0
	).

do_syntax_processing(TagOption, TaggerServerHosts, TaggerForced, TaggerServerPort,
		     UtteranceText, FullTagList, TagList,
		     HRTagStrings, Definitions, SyntAnalysis0) :-
	( TagOption == tag ->
	  tag_text(UtteranceText,
		   TaggerServerHosts, TaggerForced, TaggerServerPort,
		   FullTagList, TagList, HRTagStrings),
	  % If tag_text succeeded, cut out the choice point
	  % left behind when the tagger was chosen
	  !,
	  generate_syntactic_analysis_plus(UtteranceText, TagList, SyntAnalysis0, Definitions)
	; FullTagList = [],
	  TagList = [],
	  HRTagStrings = [],
	  generate_syntactic_analysis_plus(UtteranceText, SyntAnalysis0, Definitions)
	).

conditionally_collapse_syntactic_analysis(SyntAnalysis0, SyntAnalysis) :-
	( control_option(term_processing) ->
	  collapse_syntactic_analysis(SyntAnalysis0, SyntAnalysis)
	; SyntAnalysis = SyntAnalysis0
	).

convert_non_text_fields_to_blanks(InputType, InputStringWithBlanks,
				  TextFields, NonTextFields,
				  TrimmedTextFieldsOnlyString, NumBlanksTrimmed) :-
	( InputType == citation ->
	  cntfb(InputStringWithBlanks, TextFields, NonTextFields, TextFieldsOnly)
	; TextFieldsOnly = [InputStringWithBlanks]
	),
	append(TextFieldsOnly, TextFieldsOnlyString0),
	trim_whitespace_right(TextFieldsOnlyString0, TextFieldsOnlyString),
	trim_whitespace_left(TextFieldsOnlyString, TrimmedTextFieldsOnlyString, NumBlanksTrimmed).


% "cntfb" == "Convert Non-Text Fields to Blanks"
cntfb([], [], [], []).
cntfb([FirstChar|RestChars], TextFieldsIn, NonTextFieldsIn, [ConvertedField|RestConvertedFields]) :-
	convert_one_field_to_blanks([FirstChar|RestChars],TextFieldsIn, NonTextFieldsIn,
				    TextFieldsNext, NonTextFieldsNext, ConvertedField, CharsNext),
	cntfb(CharsNext, TextFieldsNext, NonTextFieldsNext, RestConvertedFields).
	
convert_one_field_to_blanks(CharsIn,
			    TextFieldsIn, NonTextFieldsIn,
			    TextFieldsNext, NonTextFieldsNext,
			    ConvertedField, CharsNext) :-
	get_field_components(TextFieldsIn,
			     FirstTextField, FirstTextFieldLines, RestTextFields),
	get_field_components(NonTextFieldsIn,
			     FirstNonTextField, FirstNonTextFieldLines, RestNonTextFields),
	( FirstTextField \== [],
	  prefix(CharsIn, FirstTextField) ->
	  convert_text_field(FirstTextField, FirstTextFieldLines, CharsIn,
			     ConvertedField, CharsNext),
	  TextFieldsNext = RestTextFields,
	  NonTextFieldsNext = NonTextFieldsIn
	; FirstNonTextField \== [],
	  trim_whitespace_left(CharsIn, TrimmedCharsIn),
	  prefix(TrimmedCharsIn, FirstNonTextField) ->
	  convert_non_text_field(FirstNonTextField, FirstNonTextFieldLines, TrimmedCharsIn,
				 ConvertedField, CharsNext),
	  TextFieldsNext = TextFieldsIn,
	  NonTextFieldsNext = RestNonTextFields
	).

get_field_components([], [], [], []).
get_field_components([[FirstField,FirstFieldLines]|RestFields],
		     FirstField, FirstFieldLines, RestFields).
	
convert_text_field(TextField, TextFieldLines, CharsIn, ConvertedField, CharsNext) :-
	trim_field_name_prefix(TextField, CharsIn, TextFieldLength, NumBlanksTrimmed, CharsNext3),
	form_one_string(TextFieldLines, " ", PaddedTextFieldLines0),
	append(PaddedTextFieldLines0, " ", PaddedTextFieldLines),
	% each line has a blank space after it corresponding to the original <CR>
	% NumBlanks is TextFieldLength + NumBlanksTrimmed + TextFieldLinesLength,
	NumBlanks is TextFieldLength + NumBlanksTrimmed,
	% create a string of blanks whose length is equal to the length of the
	% field name, blanks, etc. up to the actual text
	length(BlanksList, NumBlanks),
	% format(user_output, 'LINE ~d >~s:~p~n', [NumBlanks,TextField,TextFieldLines]),
	( foreach(X, BlanksList) do X is 32 ),
	% This is where I'm missing a character?
	append(BlanksList, PaddedTextFieldLines, ConvertedField),
	walk_off_field_lines(TextFieldLines, CharsNext3, CharsNext).
	% format(user_output, 'AFTER >~s<:>~s<~n', [TextField,CharsNext]).
  
% Calculate how many characters are taken up by
%  * the text field (e.g., "TI", "AB", "PG", "OWN", "STAT", etc.,
%  * any blanks immediately after the text field,
%  * the following hyphen, and
%  * any blanks immediately after the hyphen.
% Currently, this prefix always (?) contains exactly 6 chars,
% but who knows if this might someday change!
trim_field_name_prefix(Field, CharsIn, FieldLength, NumBlanksTrimmed, CharsNext3) :-
	% trim off the text field, e.g., "TI", "AB", etc.
	append(Field, CharsNext0, CharsIn),
	% determine the length of the text field
	length(Field, FieldLength),
	% trim off any whitespace immediately after the text field
	trim_whitespace_left(CharsNext0, CharsNext1, NumBlanksTrimmed1),
	% trim off the hyphen after the blanks
	CharsNext1 = [45|CharsNext2],
	% trim off any whitespace immediately after the hyphen
	trim_whitespace_left(CharsNext2, CharsNext3, NumBlanksTrimmed2),
	% The "+1" is for the hyphen
	NumBlanksTrimmed is NumBlanksTrimmed1 + NumBlanksTrimmed2 + 1.


convert_non_text_field(NonTextField, NonTextFieldLines, CharsIn, ConvertedField, CharsNext) :-
	trim_field_name_prefix(NonTextField, CharsIn, NonTextFieldLength, NumBlanksTrimmed, CharsNext3),
	% convert_strings_to_blanks(NonTextFieldLines, BlanksLines),
	% length(BlanksLines, BlanksLinesLength),
	% must add 1 blank for each line
	% length(NonTextFieldLines, NonTextFieldLinesLength),

	form_one_string(NonTextFieldLines, " ", PaddedNonTextFieldLines0),
	append(PaddedNonTextFieldLines0, " ", PaddedNonTextFieldLines),
	length(PaddedNonTextFieldLines, PNTFLength),
	NumBlanks is NonTextFieldLength + NumBlanksTrimmed + PNTFLength,
	length(BlanksList, NumBlanks),
	% format(user_output, 'LINE ~d >~s:~p~n', [NumBlanks,NonTextField,NonTextFieldLines]),
	( foreach(X, BlanksList) do X is 32 ),
	ConvertedField = BlanksList,
	walk_off_field_lines(NonTextFieldLines, CharsNext3, CharsNext).
	% format(user_output, 'AFTER >~s<:>~s<~n', [NonTextField,CharsNext]).

walk_off_field_lines([], Chars, Chars).
walk_off_field_lines([FieldLine|RestFieldLines], CharsIn, CharsOut) :-
	append(FieldLine, CharsNext0, CharsIn),
	( CharsNext0 = [32|CharsNext1] ->
	  true
	; CharsNext1 = []
	),
	walk_off_field_lines(RestFieldLines, CharsNext1, CharsOut).

% convert each string in NonTextFieldLines to a string of blanks of the same length
convert_strings_to_blanks(NonTextFieldLines, BlanksLines) :-
	(  foreach(Line, NonTextFieldLines),
	   foreach(BlanksLine, BlanksLines)
	do length(Line, LineLength),
	   length(BlanksLine, LineLength),
	   (  foreach(Blank, BlanksLine)
	   do Blank is 32
	   )
	).
