
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

% File:	    skr_utilities.pl
% Module:   SKR
% Author:   Lan
% Purpose:  Utilities

:- module(skr_utilities,[
	compare_utterance_lengths/2,
	compute_sum/3,
	conditionally_print_end_info/0,
	conditionally_print_header_info/4,
	debug_call/2,
	debug_message/3,
	do_formal_tagger_output/0,
	do_sanity_checking_and_housekeeping/5,
	ensure_number/2,
	force_to_atoms/2,
	generate_aa_term/2,
	generate_bracketed_output/2,
	generate_candidates_output/3,
	generate_header_output/6,
	generate_mappings_output/5,
	generate_phrase_output/7,
	generate_utterance_output/6,
	generate_variants_output/2,
	generate_EOT_output/1,
	get_program_name/1,
	% called by MetaMap API -- do not change signature!
	output_should_be_bracketed/1,
	output_tagging/3,
	replace_crs_with_blanks/4,
	replace_blanks_with_crs/4,
	skr_begin_write/1,
	skr_end_write/1,
	% must be exported for mm_print
	skr_write_phrase/1,
	skr_write_string/1,
	token_template/5,
	token_template/6,
	usage/0,
	verify_xml_format/2,
	write_MMO_terms/1,
	write_raw_token_lists/3,
	write_sentences/3,
	write_token_list/3
    ]).

:- use_module(lexicon(lexical),[
	concatenate_strings/3
    ]).


:- use_module(metamap(metamap_variants),[
	write_all_variants/1
    ]).

:- use_module(metamap(metamap_tokenization),[
	tokenize_text_utterly/2
    ]).

:- use_module(metamap(metamap_utilities),[
	dump_aphrase_mappings/2,
	dump_evaluations_indented/2,
	num_dump_evaluations_indented/2,
	write_list_indented/1
    ]).

:- use_module(skr(skr_xml),[
	get_xml_format_mode/2
    ]).

:- use_module(skr_lib(nls_strings),[
	atom_codes_list/2,
	is_print_string/1,
	trim_and_compress_internal_whitespace/2
    ]).

:- use_module(skr_lib(nls_system),[
	control_option/1,
	control_value/2,
	display_control_options_for_modules/2,
	display_mandatory_metamap_options/0,
	display_current_control_options/2
    ]).

:- use_module(skr_lib(sicstus_utils),[
	concat_atom/2,
	concat_atom/3,
	concat_strings_with_separator/3,
	ttyflush/0
    ]).

:- use_module(skr(skr_umls_info09),[
	verify_sources/1,
	verify_sts/1
    ]).

:- use_module(text(text_object_util),[
	annotation_type/1,
	ex_lbracket_char/1,
	ex_rbracket_char/1,
	ex_lbracket_tok/1,
	gather_whitespace/3,
	higher_order_tok/1,
	higher_order_type/1,
	higher_order_or_annotation_type/1,
	pe_tok/1,
	ws_char/1,
	ws_tok/1
   ]).		     


:- use_module(text(text_objects),[
	extract_an_lc_strings/2,
	extract_token_strings/2
   ]).		     

:- use_module(library(avl),[
	avl_member/3,
	avl_to_list/2
    ]).

:- use_module(library(codesio),[
	with_output_to_codes/2
    ]).

:- use_module(library(lists),[
	append/2,
	last/2,
	prefix/2,
	selectchk/3
    ]).

:- use_module(library(system),[
	environ/2
    ]).

skr_begin_write(Label) :-
    format('>>>>> ~p~n',[Label]).

skr_write_list(List) :-
	format('[~n', []),
	skr_write_each(List),
	format(']~n', []).

skr_write_each([]).
skr_write_each([First|Rest]) :-
	format(' ~q', [First]),
	( Rest == [] ->
	  format('~n', [])
	; format(',~n', [])
	),
	skr_write_each(Rest).

skr_write_phrase(Phrase) :-
    skr_write_terms(Phrase).

skr_write_string(String) :-
    format('~s~n',[String]).

skr_write_strings([]).
skr_write_strings([First|Rest]) :-
    format('~s~n',[First]),
    skr_write_strings(Rest).

skr_write_terms([]).
skr_write_terms([First|Rest]) :-
    format('  ~p~n',[First]),
    skr_write_terms(Rest).

skr_end_write(Label) :-
    format('<<<<< ~p~n',[Label]).

% Output routines for debugging

write_sentences(PMID, CoordSentences, Sentences) :-
	( control_value(debug, DebugFlags),
	  memberchk(sentences, DebugFlags) ->
	  format(user_output, '#### ~w CoordSentences:~n', [PMID]),
	  write_token_list(CoordSentences, 0, 1),
	  ttyflush,
	  format(user_output, '#### ~w Sentences:~n', [PMID]),
	  write_token_list(Sentences, 0, 1),
	  ttyflush
	; true
	).

write_raw_token_lists(PMID, ExpRawTokenList, UnExpRawTokenList) :-
	( control_value(debug, DebugFlags),
	  memberchk(tokens, DebugFlags) ->
	  format(user_output, '~n~n#### ~w ExpRawTokenList:~n', [PMID]),
	  write_token_list(ExpRawTokenList, 0, 1),
	  ttyflush,
	  format(user_output, '~n~n#### ~w UnExpRawTokenList:~n', [PMID]),
	  write_token_list(UnExpRawTokenList, 0, 1),
	  ttyflush
	; true
	).


write_token_nl(0).
write_token_nl(1) :- format(user_output, '~n', []).

write_token_list([], _Indent, NL) :- write_token_nl(NL).
write_token_list([Token|RestTokens], Indent, NL) :-
	write_token(Token, Indent, NL),
	% write(Token), nl,
	write_token_list(RestTokens, Indent, NL).

write_token(Token, Indent, NL) :-
	arg(1, Token, Type),
	( annotation_type(Type) ->
	  write_complex_token(Token, Indent, NL)
	; write_simple_token(Token, Indent, NL)
	).

write_simple_token_list([SimpleToken|RestSimpleTokens], Indent) :-
	IndentP1 is Indent + 1,
	format(user_output, '~*c[', [Indent, 32]),
	write_simple_token(SimpleToken, 0, 0),
	write_rest_simple_token_list(RestSimpleTokens, IndentP1).

write_rest_simple_token_list([], _Indent) :-
	format(user_output, '],~n', []).
write_rest_simple_token_list([H|T], Indent) :-
	format(user_output, ',~n', []),
	write_simple_token(H, Indent, 0),
	write_rest_simple_token_list(T, Indent).

write_pos_term_list([H|T], Indent, NL) :-
	format(user_output, '~*c~w', [Indent, 32, H]),
	write_rest_pos_term_list(T, Indent, NL).

write_rest_pos_term_list([], _Indent, NL) :-
	write_pos_term_list_nl(NL).
write_rest_pos_term_list([H|T], Indent, NL) :-
	format(user_output, ',~n~*c~w', [Indent, 32, H]),
	write_rest_pos_term_list(T, Indent, NL).

write_pos_term_list_nl(0) :- format(user_output, ')', []).
write_pos_term_list_nl(1) :- format(user_output, ')~n', []).


write_complex_token(Token, Indent, NL) :-
	Token =.. [tok, Type, TokenList1, TokenList2|PosTerms],
	write_complex_token_1(Type, Indent, TokenList1, TokenList2, PosTerms, NL).

write_complex_token_1(aadef, Indent, TokenList1, TokenList2, PosTerms, _NL) :-
	format(user_output, '~w~n', ['tok(aadef,']),
	IndentP4 is Indent + 4,
	write_simple_token_list(TokenList1, IndentP4),
	write_simple_token_list(TokenList2, IndentP4),
	write_pos_term_list(PosTerms, IndentP4, 1).
	       
write_complex_token_1(aa, Indent, AATokenList1, [AADefToken], AAPosTermList, _NL) :-
	format(user_output, '~w~n', ['tok(aa,']),
	IndentP4 is Indent + 4,
	write_simple_token_list(AATokenList1, IndentP4),
	format(user_output, '~*c~w~n', [IndentP4, 32, '[tok(aadef,']),
	AADefToken =.. [tok, aadef, AADefTokenList1, AADefTokenList2|AADefPosTermList],
	IndentP8 is Indent + 8,
	write_simple_token_list(AADefTokenList1, IndentP8),
	write_simple_token_list(AADefTokenList2, IndentP8),
	write_pos_term_list(AADefPosTermList, IndentP8, 0),
	format(user_output, '],~n', []),
	write_pos_term_list(AAPosTermList, IndentP4, 1).

write_simple_token(Token, Indent, NL) :-
	Token =.. [tok, Type, TokenString, LCTokenString|PosTerms],
	make_atom(TokenString,   TokenAtom),
	make_atom(LCTokenString, LCTokenAtom),
	assemble_format_string(PosTerms, FormatString, NL),
	format(user_output, FormatString, [Indent, 32, Type, TokenAtom, LCTokenAtom|PosTerms]).

assemble_format_string(PosTerms, FormatString, NL) :-
	PosTerms = [H|T],
	assemble_format_atoms(T, H, NL, FormatAtoms),
	concat_atom(['~*ctok(~q,~q,~q,'|FormatAtoms], FormatString).

assemble_format_atoms([], _Last, NL, [NLFormatAtom]) :-
	assemble_format_atoms_nl(NL, NLFormatAtom).
assemble_format_atoms([NextPosTerm|RestPosTerms], _ThisPosTerm, NL, ['~w,'|RestFormatAtoms]) :-
	assemble_format_atoms(RestPosTerms, NextPosTerm, NL, RestFormatAtoms).

assemble_format_atoms_nl(0, '~w)').
assemble_format_atoms_nl(1, '~w)~n').

make_atom(String, Atom) :-
	( atomic(String) ->
	  Atom = String
	; atom_codes(Atom, String)
	).


do_sanity_checking_and_housekeeping(ProgramName, FullYear,
				    InputStream, OutputStream, SavedCurrentOutput) :-
	verify_tagger_output_settings(TaggerOutputSettings),
	verify_single_output_format(SingleOutputFormat),
	verify_xml_format_value(XMLFormatValue),
        verify_xml_settings(XMLSettings),
        verify_mmo_settings(MMOSettings),
        verify_mmi_settings(MMISettings),
	verify_sources_options(SourcesOptions),
	verify_sources_values(SourcesValues),
	verify_semantic_type_options(SemTypeOptions),
	verify_semantic_type_values(SemTypeValues),
	verify_gap_size_values(GapSizeValues),
	verify_acros_abbrs_settings(AcrosAbbrsSettings),
	verify_derivational_variants_settings(DerivationalVariantsSettings),
	verify_all_results([TaggerOutputSettings, SingleOutputFormat,
			    XMLFormatValue, XMLSettings, MMOSettings, MMISettings,
			    SourcesOptions, SourcesValues,
			    SemTypeOptions, SemTypeValues, GapSizeValues,
			    DerivationalVariantsSettings, AcrosAbbrsSettings],
			   InputStream, OutputStream, SavedCurrentOutput),
	display_current_control_options(ProgramName, FullYear).

% Error if both tagger_output and formal_tagger_output are set
verify_tagger_output_settings(Result) :-
	( control_option(tagger_output),
	  control_option(formal_tagger_output) ->
	  send_message('FATAL ERROR: Only one of --tagger_output and~n', []),
	  send_message('                         --formal_tagger_output can be specified.~n', []),
	  Result is 1
	; Result is 0
	).

% Error if both XML and machine_output are set
verify_single_output_format(Result) :-
	( control_option(machine_output),
	  control_value('XML', _) ->
	  send_message('FATAL ERROR: Only one of --machine_output and~n', []),
	  send_message('                         --xml_format     can be specified.~n', []),
	  Result is 1
	; Result is 0
	).

% Error if XML option is specified as anything other than format, format1, noformat, or noformat1
verify_xml_format_value(Result) :-
	control_value('XML', Format),
	!,
	( verify_xml_format(Format, _) ->
	  Result is 0
	; Result is 1
	).
verify_xml_format_value(0).

% verify_xml_format/2 predicate assumes that the XML option is in fact on.
verify_xml_format(Format, TrueOrFalse) :-
	( get_xml_format_mode(Format, TrueOrFalse) ->
	  true 
	; send_message('~nFATAL ERROR: XML option must be one of format1/format/noformat1/noformat.~n', []),
	  fail
	).

verify_xml_settings(Result) :-
	control_option('XML'),
	!,
	warning_check([syntax, show_cuis, negex, sources, dump_aas], 'XML'),
	fatal_error_check([hide_plain_syntax, hide_candidates, number_the_candidates,
			   hide_semantic_types, hide_mappings, show_preferrred_names_only],
			  'XML', 0, Result).
verify_xml_settings(0).

verify_mmo_settings(Result) :-
	control_option(machine_output),
	!,
	warning_check([syntax, show_cuis, negex, sources, dump_aas], machine_output),
	fatal_error_check([hide_plain_syntax, hide_candidates, number_the_candidates,
			   hide_semantic_types, hide_mappings, show_preferrred_names_only],
			  machine_output, 0, Result).
verify_mmo_settings(0).

verify_mmi_settings(Result) :-
	control_option(fielded_mmi_output),
	!,
	warning_check([show_cuis], fielded_mmi_output),
	fatal_error_check([tagger_output, hide_plain_syntax, hide_candidates,
			   number_the_candidates, hide_semantic_types,
			   show_preferrred_names_only, negex, hide_mappings,
			   sources, dump_aas],
			  fielded_mmi_output, 0, Result).
verify_mmi_settings(0).

% Error if both restrict_to_sources and exclude_sources are set
verify_sources_options(Result) :-
	( control_option(restrict_to_sources),
	  control_option(exclude_sources) ->
	  send_message('FATAL ERROR: Only one of --restrict_to_sources and~n', []),
	  send_message('                         --exclude_sources     can be specified.~n', []),
	  Result is 1
	; Result is 0
	).

% Error if either restrict_to_sources and exclude_sources is set, and invalid source is specified
verify_sources_values(Result) :-
	( control_value(restrict_to_sources,Sources)
	; control_value(exclude_sources,Sources)
	),
	!,
	( verify_sources(Sources) ->
	  Result is 0
	; Result is 1
	).
verify_sources_values(0).

% Error if both restrict_to_sts and exclude_sts are set
verify_semantic_type_options(Result) :-
	( control_option(restrict_to_sts),
	  control_option(exclude_sts) ->
	  send_message('FATAL ERROR: Only one of --restrict_to_sts and~n', []),
	  send_message('                         --exclude_sts     can be specified.~n', []),
	  Result is 1
	; Result is 0
	).

% Error if either restrict_to_sts and exclude_sts is set and invalid ST is specified
verify_semantic_type_values(Result) :-
	( control_value(restrict_to_sts,STs)
	; control_value(exclude_sts,STs)
	),
	!,
	( verify_sts(STs) ->
	  Result is 0
	; Result is 1
	).
verify_semantic_type_values(0).

% Error if invalid gap size is specified (obsolete)
verify_gap_size_values(Result) :-
	control_value(gap_size, GapSize),
	!,
	( verify_gap_size_values_aux(GapSize) ->
	  Result is 0
	; send_message('FATAL ERROR: gap_size argument must be of form Int1,Int2 ', []),
	  send_message('with Int1 >= Int2+2~n', []),
	  Result is 1
	).
verify_gap_size_values(0).

verify_gap_size_values_aux(GapSize) :-
	GapSize = [MinPhraseLength, MaxGapSize],
	integer(MinPhraseLength),
	integer(MaxGapSize),
	MinPhraseLength >= MaxGapSize + 2.


% Error if both all_acros_abbrs and unique_acros_abbrs_only are set
verify_acros_abbrs_settings(Result) :-
	( control_option(all_acros_abbrs),
	  control_option(unique_acros_abbrs_only) ->
	  send_message('FATAL ERROR: Only one of --all_acros_abbrs         and~n', []),
	  send_message('                         --unique_acros_abbrs_only can be specified.~n', []),
	  Result is 1
	; Result is 0
	).			    


% Error if both all_derivational_variants and no_derivational_variants are set
verify_derivational_variants_settings(Result) :-
	( control_option(all_derivational_variants),
	  control_option(no_derivational_variants) ->
	  send_message('FATAL ERROR: Only one of --all_derivational_variants and~n', []),
	  send_message('                         --no_derivational_variants  can be specified.~n', []),
	  Result is 1
	; Result is 0
	).			    

verify_all_results(ValuesList, InputStream, OutputStream, SavedCurrentOutput) :-
	compute_sum(ValuesList, 0, Result),
	( Result =:= 0 ->
	  true
	; set_output(SavedCurrentOutput),
	  generate_EOT_output(OutputStream),
	  close(InputStream),
	  close(OutputStream),
	  halt
	).	

warning_check([], _OutputOption).
warning_check([WarningOption|RestOptions], OutputOption) :-
	( control_option(WarningOption) ->
	  send_message('WARNING: The ~w option has no effect on ~w output.~n',
		       [WarningOption, OutputOption])
	; true
	),
	warning_check(RestOptions, OutputOption).
	  
	  
fatal_error_check([], _OutputOption, Result, Result).
fatal_error_check([FatalErrorOption|RestOptions], OutputOption, ResultIn, ResultOut) :-
	( control_option(FatalErrorOption) ->
	  send_message('FATAL_ERROR: The ~w option cannot be used with ~w output.~n',
		       [FatalErrorOption, OutputOption]),
	  ResultNext is 1
	; ResultNext is ResultIn
	),
	fatal_error_check(RestOptions, OutputOption, ResultNext, ResultOut).

send_message(Message, Format):-
	format(user_output, Message, Format),
	( current_stream(_File, write, _Output) ->
	  format(Message, Format)
	; true
	).

/* usage

usage/0 displays skr/metamap usage.  */

usage :-
    format('~nUsage: metamap [<options>] [<infile> [<outfile>]]~n~n', []),
    format('  <infile> contains text in one of several forms (default is user_input), and~n', []),
    format('  <outfile> is a file for results (default is <infile>.out).~n~n', []),
    display_mandatory_metamap_options,
    display_control_options_for_modules(metamap, []).

% format_cmd_line_for_machine_output(IArgs, IOptions, MachineOutputArgs)

% IOptions looks like
% [iopt(exclude_sts,
%       aspec(exclude_sts,mandatory,list,none,no_default,'List of semantic types to exclude.')),
%  iopt(relaxed_model,none),
%  iopt(mm_data_year,
%       aspec(mm_data_year,mandatory,none,none,no_default,'Year of MetaMap data to use.'))]

% Iargs looks like
% [iarg(exclude_sts,
%       aspec(exclude_sts,mandatory,list,none,no_default,'List of semantic types to exclude.'),
%       [value([neop,gngm])]),
%  iarg(mm_data_year,
%       aspec(mm_data_year,mandatory,none,none,no_default,'Year of MetaMap data to use.'),
%       [name('08')]),
%  iarg(infile,
%        aspec(infile,mandatory,file,read,no_default,'Input file containing labelled utterances'),
%        [name(user_input),stream(user_input)]),
%  iarg(outfile,
%       aspec(outfile,mandatory,file,write,no_default,'Output file'),
%       [name(user_output),stream(user_output)])]

% We want to create a list of the form
% [ OptionName1-OptionValue1, OptionName2-OptionValue2, ... OptionNameN-OptionValueN ]
% where the OptionNames are
% the first arg of the iarg(_) terms in Iargs and
% the first arg of the iopt(_) terms in IOptions
% and the OptionValues are extracted from
% the third  arg of the iarg(_) terms, if there is a matching iarg term, or
% the second arg of the iopt(_) terms otherwise.

format_cmd_line_for_machine_output(Options, Args, args(CommandLine,ArgsMO)) :-
	get_program_name(ProgramName),
	prolog_flag(argv, Argv),
	concat_atom([ProgramName|Argv], ' ', CommandLine),
	format_cmd_line_for_machine_output_1(Options, Args, ArgsMO).

get_program_name(ProgramName) :-
	environ('SP_APP_PATH', AbsolutePathName),
	atom_codes(AbsolutePathName, AbsolutePathNameCodes),
	basename(AbsolutePathNameCodes, ProgramNameCodes),
	atom_codes(ProgramName, ProgramNameCodes).

basename(File, Base) :-
        basename(File, S, S, Base).

% This predicate is stolen from system3.pl in the SICStus Prolog 4.1.1 library directory,
% from where it is NOT exported. Rather than modify the SP library file to export basename/4,
% I have just copied it here.
basename([], Base, [], Base).
basename([0'/|File], _, _, Base) :- !,
        basename(File, S, S, Base).
basename([C|File], S0, [C|S], Base) :- !,
        basename(File, S0, S, Base).

% format_cmd_line_for_machine_output_1(Options, IArgs, ArgsMO)
format_cmd_line_for_machine_output_1([], ArgsRest, ArgsMO) :-
	format_iargs_for_machine_output(ArgsRest, ArgsMO).

format_cmd_line_for_machine_output_1([FirstOption|RestOptions], ArgsIn, [FirstArgMO|RestArgsMO]) :-
	format_one_option(FirstOption, ArgsIn, ArgsNext, FirstArgMO),
	format_cmd_line_for_machine_output_1(RestOptions, ArgsNext, RestArgsMO).
	
% FirstArg looks like
% iarg(exclude_sts,
%      aspec(exclude_sts,mandatory,list,none,no_default,'List of semantic types to exclude.'),
%      [value([neop,gngm])])
% We want to grab the first arg (exclude_sts)
% and extract [neop,gngm] from the third arg
format_one_option(IOption, ArgsIn, ArgsNext, ArgMO) :-
	arg(1, IOption, OptionName),
	ArgMO = OptionName-OptionValue,
	( member(MatchingArg, ArgsIn),
	  arg(1, MatchingArg, OptionName) ->
	  % delete MatchingOption from IOptionsIn, leaving IOptionsNext
	  selectchk(MatchingArg, ArgsIn, ArgsNext),
	  arg(3, MatchingArg, [ArgValueTerm|_]),
	  arg(1, ArgValueTerm, OptionValue)
	; ArgsNext = ArgsIn,
	  OptionValue = []
	).
	  
format_iargs_for_machine_output([], []).
format_iargs_for_machine_output([FirstIarg|RestIargs],
				  [FirstIargName-FirstIargValue|RestIargsMO]) :-
	arg(1, FirstIarg, FirstIargName),
	arg(3, FirstIarg, [FirstIargValueTerm|_]),
	arg(1, FirstIargValueTerm, FirstIargValue),
	format_iargs_for_machine_output(RestIargs, RestIargsMO).


output_should_be_bracketed(BracketedOutput) :-
	( control_option(bracketed_output) ->
	  BracketedOutput is 1
        ; BracketedOutput is 0
	).


generate_EOT_output(OutputStream) :-
	( control_option(indicate_citation_end) ->
	  % do not use portrayed(true)
	  write_term(OutputStream, 'EOT',[quoted(true)]),
	  format(OutputStream, '.~n', [])
	; true
	).

do_formal_tagger_output :-
	( control_value('XML', _) ->
	  true
	; control_option(machine_output) ->
	  true
	; control_option(formal_tagger_output) ->
	  skr_begin_write('EOC'),
	  skr_write_string(".E"),
	  skr_end_write('EOC')
	; true
	).

generate_phrase_output(PhraseTextAtom, Phrase, StartPos, Length, ReplacementPos,
		       BracketedOutput, PhraseMMO) :-
	% Do not generate this output if machine_output, XML, or fielded_mmi_output is on!
        ( ( control_option(machine_output)
	  ; control_option('XML')
	  ; control_option(fielded_mmi_output)
	  ) ->
	  % Genrerate the MMO for the Phrase term
	  % which is needed for both MMO and XML output!
	  PhraseMMO = phrase(PhraseTextAtom,Phrase,StartPos/Length,ReplacementPos)
	; ( \+ control_option(hide_plain_syntax) ->
	    atom_codes(PhraseTextAtom,PhraseText),
	    format('~nPhrase: ~p~n',[PhraseText])
	  ; true
	  ),
	  ( control_option(syntax) ->
	    ( BracketedOutput == 1 ->
	      skr_begin_write('Syntax'),
	      format('msu~n', []),
	      skr_write_phrase(Phrase),
	      skr_end_write('Syntax')
	    ; format('~nmsu~n', []),
	      write_list_indented(Phrase)
	    )
	  ; true
	    )
	).

generate_bracketed_output(BracketedOutput, PhraseWordInfo) :-
	( control_option(fielded_mmi_output) ->
	  true
	; \+ control_option(hide_plain_syntax),
	  BracketedOutput == 1 ->
	  skr_begin_write('Phrase'),
	  PhraseWordInfo=_:pwi(wdl(_,LCWordL),_,_),
	  atom_codes_list(LCWordL,LCWordLStrings),
	  concatenate_strings(LCWordLStrings," ",LCPhraseString),
	  ( LCPhraseString == "" ->
	    true
	  ; skr_write_string(LCPhraseString)
	  ),
	  skr_end_write('Phrase')
	; true
	).

generate_variants_output(GVCs, BracketedOutput) :-
	( control_option(variants) ->
	  ( GVCs == [] ->
	    true
	  ; ( BracketedOutput == 1 ->
	      skr_begin_write('Variants')
	    ; format('~n', [])
	    ),
	    write_all_variants(GVCs),
	    ( BracketedOutput == 1 ->
	      skr_end_write('Variants')
	    ; true
	    )
	  )
	; true
	).

generate_candidates_output(Evaluations3, BracketedOutput, CandidatesMMO) :-
	% Do not generate this output if machine_output, XML, or fielded_mmi_output is on!
	( test_generate_candidate_output_control_options ->
	  % Generate the MMO for the Candidates term
	  % which is needed for both MMO and XML output!
	  CandidatesMMO = candidates(Evaluations3)
	; \+ control_option(hide_candidates),
	  Evaluations3 \== [] ->
	  conditionally_skr_begin_write(BracketedOutput),
	  conditionally_dump_evals(Evaluations3),
	  conditionally_skr_end_write(BracketedOutput)
	; true
	).

test_generate_candidate_output_control_options :-
	( control_option(machine_output) ->
	  true
	; control_option('XML') ->
	  true
	; control_option(fielded_mmi_output)
	).


conditionally_skr_begin_write(BracketedOutput) :-
	( BracketedOutput == 1 ->
	  skr_begin_write('Candidates')
	; true
	).

conditionally_skr_end_write(BracketedOutput) :-
	( BracketedOutput == 1 ->
	  skr_end_write('Candidates')
	; true
	).

conditionally_dump_evals(Evaluations3) :-
	( control_option(number_the_candidates) ->
	  num_dump_evaluations_indented(Evaluations3, 'Candidates')
	; dump_evaluations_indented(candidates(Evaluations3), 'Candidates')
	).

generate_mappings_output(Mappings, Evaluations, APhrases, BracketedOutput, MappingsMMO) :-
	% Do not generate this output if machine_output, XML, or fielded_mmi_output is on!
	( ( control_option(machine_output)
	  ; control_option('XML')
	  ; control_option(fielded_mmi_output)
	  ) ->
	  % Generate the MMO for the Mappings,
	  % which is needed for both MMO and XML output!
	  MappingsMMO = mappings(Mappings)
	; ( \+ control_option(hide_mappings),
	    Evaluations\==[] ->
            ( BracketedOutput == 1 ->
	      skr_begin_write('Mappings')
	    ; true
	    ),
	    dump_aphrase_mappings(APhrases,'Mapping'),
	    ( BracketedOutput == 1 ->
	      skr_end_write('Mappings')
	    ; true
	    )
	  ; true
	  )
	).


% Generate the MMO for Args, AAs, and NegEx.
generate_header_output(IArgs, IOptions, NegExList, DisambMMOutput,
		       HeaderMMO, HeaderMMORest) :-
	  HeaderMMO = [ArgsMMO,AAsMMO,NegExMMO|HeaderMMORest],
	  format_cmd_line_for_machine_output(IOptions, IArgs, ArgsMMO),
	  generate_aa_term(DisambMMOutput, AAsMMO),
	  % NegExList is currently hardcoded.
	  NegExMMO = neg_list(NegExList).

generate_utterance_output(Label, Text0, UttStartPos, UttLength, ReplPos, UtteranceMMO) :-
	( check_generate_utterance_output_control_options_1 ->
	  % Generate the MMO for the Utterance term,
	  % which is needed for both MMO and XML output!
	  UtteranceMMO = utterance(Label,Text0,UttStartPos/UttLength,ReplPos)
	; check_generate_utterance_output_control_options_2 ->
          format('~NProcessing ~a: ~s~n',[Label,Text0])
	; true
	).

generate_aa_term(DisambMMOutput, aas(SortedAAList)) :-
	% Exctact the AA term from the DisambMMOutput
	get_aa_term(DisambMMOutput, AAs),
	avl_to_list(AAs, AAListTokens),
	reformat_aa_list(AAListTokens, DisambMMOutput, AAList),
	sort(AAList, SortedAAList).

	
reformat_aa_list([], _DisambMMOutput, []).
reformat_aa_list([FirstAA|RestAAs], DisambMMOutput,
		 [AAString*ExpansionString*CountData*CUIList|RestReformattedAAs]) :-
	reformat_one_aa(FirstAA, DisambMMOutput,
			AAString, ExpansionString, CountData, TempCUIList),
	% 0 seeds the predicate with a NegScore
	choose_best_mappings_only(TempCUIList, 0, CUIList),
	reformat_aa_list(RestAAs, DisambMMOutput, RestReformattedAAs).

reformat_one_aa(AATokenList-[ExpansionTokenList],
		DisambMMOutput, AAString, ExpansionString, CountData, CUIList) :-
	% Get the actual strings from the AATokenList
	extract_token_strings(AATokenList, AAStringList),
	length(AATokenList, AATokenListLength),
	% Get the actual strings from the ExpansionTokenList
	extract_token_strings(ExpansionTokenList, ExpansionStringList),
	length(ExpansionTokenList, ExpansionTokenListLength),
	% Get the lowercase alphanumeric strings only from the ExpansionTokenList;
	% this will be used to match against the word lists in the ev terms
	extract_an_lc_strings(ExpansionTokenList, ANLCExpansionStringList),
	% AAStrings is for display purposes in the aas(_) term
	append(AAStringList, AAString),
	length(AAString, AAStringLength),
	append(ExpansionStringList, ExpansionString),
	length(ExpansionString, ExpansionStringLength), 
	append(ANLCExpansionStringList, ANLCExpansionString),
	CountData = [AATokenListLength,AAStringLength,ExpansionTokenListLength,ExpansionStringLength],
	atom_codes(ANLCExpansionAtom, ANLCExpansionString),
	find_matching_CUIs(DisambMMOutput, ANLCExpansionAtom, TempCUIList, []),
	sort(TempCUIList, CUIList).

% Starting with a list of terms of the form NegScore-CUI,
% remove all those terms whose NegScore is not the highest (most negative).
choose_best_mappings_only([], _PrevScore, []).
choose_best_mappings_only([FirstScore-FirstCUI|RestCUIs], PrevScore, CUIList) :-
	% If FirstScore is =< PrevScore,
	% either PrevScore is the initial 0, or the scores are the same;
	% in either case, we keep FirstCUI.
	( FirstScore =< PrevScore ->
	  CUIList = [FirstCUI|RestCUIList],
	  choose_best_mappings_only(RestCUIs, FirstScore, RestCUIList)
	% If FirstScore > PrevScore, the CUI is not one of the best mappings,
	% so we're done.
	; CUIList = []
	).	

find_matching_CUIs([], _ExpansionAtom, CUIList, CUIList).
find_matching_CUIs([FirstMMOTerm|RestMMOTerms], ExpansionAtom, CUIListIn, CUIListOut) :-
	FirstMMOTerm = mm_output(_ExpSentence, _Citation, _ModifiedText, _Tagging,
				 _AAs, _Syntax, MMOPhrases, _ExtractedPhrases),
	find_matching_CUIs_in_phrases(MMOPhrases, ExpansionAtom, CUIListIn, CUIListNext),
	find_matching_CUIs(RestMMOTerms, ExpansionAtom, CUIListNext, CUIListOut).

find_matching_CUIs_in_phrases([], _ExpansionAtom, CUIList, CUIList).
find_matching_CUIs_in_phrases([FirstPhraseTerm|RestPhraseTerms], ExpansionAtom, CUIListIn, CUIListOut) :-
	FirstPhraseTerm = phrase(_Phrase, Candidates, _Mappings, _Pwi, _Gvcs, _Ev0, _Aphrases),
	Candidates = candidates(CandidateList),
	find_matching_CUIs_in_candidates(CandidateList, ExpansionAtom, CUIListIn, CUIListNext),
	find_matching_CUIs_in_phrases(RestPhraseTerms, ExpansionAtom, CUIListNext, CUIListOut).
	
find_matching_CUIs_in_candidates([], _ExpansionAtom, CUIList, CUIList).
find_matching_CUIs_in_candidates([FirstCandidate|RestCandidates],
				 ExpansionAtom, CUIListIn, CUIListOut) :-
	( matching_CUI(FirstCandidate, ExpansionAtom, MatchingCUI) ->
	  CUIListIn = [MatchingCUI|CUIListNext]
	; CUIListNext = CUIListIn
	),
	find_matching_CUIs_in_candidates(RestCandidates, ExpansionAtom, CUIListNext, CUIListOut).

matching_CUI(Candidate, ExpansionAtomLC, CurrScore-MatchingCUI) :-
	Candidate = ev(CurrScore, MatchingCUI, _ConceptName, _PreferredName, MatchingWordList,
		       _SemTypes, _MatchMap, _HeadFlag, _OverMatch, _Sources, _PosInfo),
	concat_atom(MatchingWordList, MatchingWordAtom),
	ExpansionAtomLC == MatchingWordAtom.


get_aa_term(MMOutput, AAs) :-
	MMOutput = [FirstMMOutput|_],
	FirstMMOutput = mm_output(_ExpandedUtterance, _Citation, _ModifiedText, _Tagging,
				  AAs, _Syntax, _MMOPhrases, _ExtractedPhrases).

write_MMO_terms(MMOTerms) :-
	( control_option(machine_output) ->
	  write_MMO_terms_aux(MMOTerms)
	; true
	).

write_MMO_terms_aux([ArgsMMO,AAsMMO,NegExMMO|UtteranceMMO]) :-
	  write_args_MMO_term(ArgsMMO),
	  write_AAs_MMO_term(AAsMMO),
	  write_negex_MMO_term(NegExMMO),
	  write_all_utterance_MMO_terms(UtteranceMMO).
       
write_args_MMO_term(ArgsMMO) :-
	write_term(ArgsMMO,
		   [quoted(true),portrayed(true)]),
	format('.~n', []).

write_AAs_MMO_term(AAsMMO) :-
        write_term(AAsMMO,
		   [quoted(true),portrayed(true)]),
	format('.~n', []).

write_negex_MMO_term(NegExMMO) :-
	write_term(NegExMMO,
		   [quoted(true),portrayed(true)]),
	format('.~n', []).

write_all_utterance_MMO_terms([]).
write_all_utterance_MMO_terms([FirstMMOTerm|RestMMOTerms]) :-
	write_one_utterance_MMO_term(FirstMMOTerm, RestMMOTerms, RemainingMMOTerms),
	write_EOU_term,
	write_all_utterance_MMO_terms(RemainingMMOTerms).

write_one_utterance_MMO_term(UtteranceTerm, RestMMOTerms, RemainingMMOTerms) :-
        % this is the one place to use portrayed(true)
	UtteranceTerm = utterance(Label, UtteranceString, PosInfo, ReplPos),
	% We can no longer simply do
	% write_term(UtteranceTerm, [quoted(true),portrayed(true)])
	% because ReplPos is a list of small integers,
	% which would be interpreted as a print string by portray/1. Blarg.
	format('utterance(~q,', [Label]),
	write_term(UtteranceString, [quoted(true),portrayed(true)]),
	format(',~w,~w).~n', [PosInfo,ReplPos]),
	write_all_phrase_MMO_terms(RestMMOTerms, RemainingMMOTerms).

write_all_phrase_MMO_terms([], []).
write_all_phrase_MMO_terms([H|T], RemainingMMOTerms) :-
	( functor(H, utterance, _UtteranceArity) ->
	  RemainingMMOTerms = [H|T]
	; [H|T] = [PhraseMMOTerm,CandidateMMOTerm,MappingsTerm|RestMMOTerms],
	  write_one_phrase_MMO_term(PhraseMMOTerm, CandidateMMOTerm, MappingsTerm),
	  write_all_phrase_MMO_terms(RestMMOTerms, RemainingMMOTerms)
	).

write_one_phrase_MMO_term(PhraseMMOTerm, CandidatesMMOTerm, MappingsMMOTerm) :-
	write_phrase_MMO_component(PhraseMMOTerm),
	write_candidates_MMO_component(CandidatesMMOTerm),
	write_mappings_MMO_component(MappingsMMOTerm).

write_phrase_MMO_component(PhraseTerm) :-
	PhraseTerm = phrase(PhraseString,Syntax,PosInfo,ReplPos),
	% We can no longer simply do
	% write_term(PhraseTerm, [quoted(true), portrayed(true)]),
	% because ReplPos is a list of small integers,
	% which would be interpreted as a print string by portray/1. Blarg.
	format('phrase(', []),
	write_term(PhraseString, [quoted(true),portrayed(true)]),
	format(',~q,~w,~w).~n', [Syntax,PosInfo,ReplPos]).

write_candidates_MMO_component(CandidatesTerm) :-
	write_term(CandidatesTerm, [quoted(true)]),
	format('.~n', []).

write_mappings_MMO_component(MappingsTerm) :-
	  % do not use portrayed(true)
	  write_term(MappingsTerm,[quoted(true)]),
	  format('.~n', []).

write_EOU_term :-
        write_term('EOU',[quoted(true)]),
        format('.~n', []).


output_tagging(BracketedOutput, HRTagStrings, FullTagList) :-
	% (1a) If either tagger_output or format_tagger_output is on
	( ( control_option(formal_tagger_output)
	  ; control_option(tagger_output)
	  ) ->
	  % (2) If bracketed_output is on
	  ( BracketedOutput == 1 ->
	    skr_begin_write('Tagging')
	  ; format('~n', [])
	  ),
	  ( control_option(tagger_output) ->
	    skr_write_strings(HRTagStrings)
	  ; skr_write_list(FullTagList)
	  ),
	  ( BracketedOutput == 1 ->
	    skr_end_write('Tagging')
	  ; true
	  )
	% (1b) Otherwise, do nothing
	; true
	).

token_template(tok(TokenType,TokenString,LCTokenString,PosInfo),
	       TokenType, TokenString, LCTokenString, PosInfo).
	% format(user_output, 'TT/5: ~q~n', [tok(TokenType,TokenString,LCTokenString,PosInfo)]).

token_template(tok(TokenType,TokenString,LCTokenString,PosInfo1,PosInfo2),
	       TokenType, TokenString, LCTokenString, PosInfo1,PosInfo2).

compare_utterance_lengths([], []).
compare_utterance_lengths([FirstOrigUtterance|RestOrigUtterances],
			  [FirstExpUtterance|RestExpUtterances]) :-
	compare_one_utterance(FirstOrigUtterance, FirstExpUtterance),
	compare_utterance_lengths(RestOrigUtterances, RestExpUtterances).

compare_one_utterance(FirstOrigUtterance, FirstExpUtterance) :-
	FirstOrigUtterance = utterance(OrigID, _OrigUtterance, OrigStartPos/OrigLength, _OrigReplPos),
	FirstExpUtterance  = utterance(ExpID,  _ExpUtterance,  ExpStartPos/ExpLength,  _ExpReplPos),
	compare_one_utterance_aux(OrigID, OrigStartPos, OrigLength,
				  ExpID, ExpStartPos, ExpLength).

compare_one_utterance_aux(OrigID, OrigStartPos, OrigLength,
		   ExpID,  ExpStartPos,  ExpLength) :-
	compare_fields(OrigID, ExpID, 'OrigID',       OrigID,       'ExpID',       ExpID),
	compare_fields(OrigID, ExpID, 'OrigStartPos', OrigStartPos, 'ExpStartPos', ExpStartPos),
	compare_fields(OrigID, ExpID, 'OrigLength',   OrigLength,   'ExpLength',   ExpLength).	

compare_fields(OrigID, ExpID,
	       OrigFieldName, OrigFieldValue,
	       ExpFieldName,  ExpFieldValue) :-
	( OrigFieldValue == ExpFieldValue ->
	  true
	; format(user_output, 'In ~w/~w: ~w ~w =\\= ~w ~w~n',
		 [OrigID, ExpID,
		  OrigFieldName, OrigFieldValue,
		  ExpFieldName,  ExpFieldValue])
	).

% replace_crs_with_blanks(String, CurrPos, NewString, ReplacementIndexes)
% NewString is a copy of String, but with all <CR>s replaced by blanks.
% ReplacementIndexes is a list of the character positions
% where the replacements occurred.
% CurrPos is just the current character position in String.

replace_crs_with_blanks([], _Pos, [], []).
replace_crs_with_blanks([H|T], Pos, [NewH|NewT], ReplacementIndexes) :-
	( H =:= 10 ->
	  NewH is 32,
	  ReplacementIndexes = [Pos|RestReplacementIndexes]
	; NewH is H,
	  RestReplacementIndexes = ReplacementIndexes
	),
	NextPos is Pos + 1,
	replace_crs_with_blanks(T, NextPos, NewT, RestReplacementIndexes).

% No more blanks to replace with <CR>s
replace_blanks_with_crs([], String, _CurrPos, String).
replace_blanks_with_crs([BlankIndex|RestIndexes], [H|T], CurrPos, [NewH|NewT]) :-
	( BlankIndex =:= CurrPos,
	  H =:= 32 ->
	  NewH is 10,
	  RemainingIndexes = RestIndexes
	; NewH is H,
	  RemainingIndexes = [BlankIndex|RestIndexes]
	),
	NextPos is CurrPos + 1,
	replace_blanks_with_crs(RemainingIndexes, T, NextPos, NewT).

/* compute_sum(+Values, +SumIn, -SumOut)

compute_sum/3
xxx
*/

compute_sum([], SumIn, SumIn).
compute_sum([First|Rest], SumIn, SumOut) :-
	SumInOut is SumIn + First,
	compute_sum(Rest, SumInOut, SumOut).

debug_call(Flag, Goal) :-
	( control_value(debug, DebugFlags),
	  memberchk(Flag, DebugFlags) ->
	  call(Goal)
	; true
	).

debug_message(Flag, Control, Arguments) :-
	( control_value(debug, DebugFlags),
	  memberchk(Flag, DebugFlags) ->
	  format(user_output, Control, Arguments)
	; true
	).

ensure_number(Atom, Number) :-
	( number(Atom) ->
	  Number is Atom
	; atom_codes(Atom, AtomCodes),
	  number_codes(Number, AtomCodes)
	).

force_to_atoms([], []).
force_to_atoms([H|T], [AtomH|AtomsT]) :-
	% term_to_chars/2 will not work here, because "ii-server5" will become "-(ii,server5)"
	% term_to_chars(H, CharsH),
	with_output_to_codes(write(H), CharsH),
	atom_codes(AtomH, CharsH),
	force_to_atoms(T, AtomsT).

check_generate_utterance_output_control_options_1 :-
	( control_option(machine_output)     -> true
	; control_option(fielded_mmi_output) -> true
	; control_option('XML')
	).

check_generate_utterance_output_control_options_2 :-
	( \+ control_option(hide_plain_syntax) -> true
	; control_option(syntax)               -> true
	; control_option(variants)             -> true
	; \+ control_option(hide_candidates)   -> true
	; \+ control_option(hide_mappings)
	).

conditionally_print_header_info(InputFile, TagMode, OutputFile, TagOption) :-
	( \+ control_option(silent) ->
	  format('~n~nBeginning to process ~a ~asending output to ~a.~n',
		 [InputFile,TagMode,OutputFile]),
	  announce_tagging_mode(TagOption)
	; true
	).

announce_tagging_mode(TagOption) :-
	  ( TagOption == tag ->
	    format('Tagging will be done dynamically.~n', [])
	  ; format('No tagging will be done.~n', [])
	  ).
	
conditionally_print_end_info :-
	( \+ control_option(silent) ->
	  format('~nBatch processing is finished.~n',[])
	; true
	).
