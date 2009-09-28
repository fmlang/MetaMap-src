% File:	    skr_utilities.pl
% Module:   SKR
% Author:   Lan
% Purpose:  Utilities

:- module(skr_utilities,[
	compare_utterance_lengths/2,
	compute_original_phrase/7,
	compute_original_phrase_1/7,
	compute_sum/3,
	debug_call/2,
	debug_message/3,
	do_formal_tagger_output/0,
	do_sanity_checking_and_housekeeping/2,
	expand_all_phrase_texts/3,
	ensure_atom/2,
	ensure_number/2,
	generate_aa_term/2,
	generate_bracketed_output/2,
	generate_candidates_output/3,
	generate_header_output/6,
	generate_mappings_output/5,
	generate_phrase_output/7,
	generate_utterance_output/6,
	generate_variants_output/2,
	generate_EOT_output/0,
	get_program_name/1,
	get_token_text/2,
	output_should_be_bracketed/1,
	output_tagging/3,
	replace_crs_with_blanks/4,
	replace_blanks_with_crs/4,
	skr_begin_write/1,
	skr_begin_write2/1,
	skr_end_write/1,
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
	concat_atoms/2,
	concat_atoms_with_separator/3,
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

:- use_module(library(lists),[
	append/2,
	last/2,
	prefix/2,
	selectchk/3
    ]).

skr_begin_write(Label) :-
    format('>>>>> ~p~n',[Label]).

skr_begin_write2(Label) :-
    format('>>>>> ~p',[Label]).

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
	concat_atoms(['~*ctok(~q,~q,~q,'|FormatAtoms], FormatString).

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


do_sanity_checking_and_housekeeping(ProgramName, FullYear) :-
	% can't specify both MMO and XML
	verify_single_output_format(SingleOutputFormat),
	% the '%' flag must be followed by either 'format' or 'noformat'
	verify_negex_settings(NegExSettings),
	verify_xml_format_value(XMLFormatValue),
	verify_sources_options(SourcesOptions),
	verify_sources_values(SourcesValues),
	verify_semantic_type_options(SemTypeOptions),
	verify_semantic_type_values(SemTypeValues),
	verify_gap_size_values(GapSizeValues),
	verify_acros_abbrs_settings(AcrosAbbrsSettings),
	verify_derivational_variants_settings(DerivationalVariantsSettings),
	verify_all_results([SingleOutputFormat, NegExSettings, XMLFormatValue, SourcesOptions,
			    SourcesValues,SemTypeOptions, SemTypeValues, GapSizeValues,
			    DerivationalVariantsSettings,AcrosAbbrsSettings]),
	
	display_current_control_options(ProgramName, FullYear).

verify_all_results(ValuesList) :-
	compute_sum(ValuesList, 0, Result),
	( Result =:= 0 ->
	  true
	; % usage,
	  halt
	).	

verify_single_output_format(Result) :-
	( control_option(machine_output),
	  control_value('XML', _) ->
	  format('FATAL ERROR: Only one of --machine_output and~n', []),
	  format('                         --xml_format     can be specified.~n', []),
	  Result is 1
	; Result is 0
	).			    

verify_negex_settings(NegExSettings) :-
	% Warning only!
	NegExSettings is 0,
	( control_option(negex) ->
	  ( control_option('XML') ->
	    format('WARNING: --negex generates no output if XML option is selected~n', [])
	  ; control_option(machine_output) ->
	    format('WARNING: --negex generates no output if machine_output option is selected~n', [])
	  ; true
	  )
	; true
	).
	
verify_derivational_variants_settings(Result) :-
	( control_option(all_derivational_variants),
	  control_option(no_derivational_variants) ->
	  format('FATAL ERROR: Only one of --all_derivational_variants and~n', []),
	  format('                         --no_derivational_variants  can be specified.~n', []),
	  Result is 1
	; Result is 0
	).			    

verify_acros_abbrs_settings(Result) :-
	( control_option(all_acros_abbrs),
	  control_option(unique_acros_abbrs_only) ->
	  format('FATAL ERROR: Only one of --all_acros_abbrs         and~n', []),
	  format('                         --unique_acros_abbrs_only can be specified.~n', []),
	  Result is 1
	; Result is 0
	).			    


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
	( Format == format1 ->
	  TrueOrFalse = true
	; Format == format ->
	  TrueOrFalse = true
	; Format == noformat1 ->
	  TrueOrFalse = false
	; Format == noformat ->
	  TrueOrFalse = false
	; format('~nFatal Error: XML option must be one of format1/format/noformat1/noformat.~n', []),
	  fail
	).

verify_sources_options(Result) :-
	( control_option(restrict_to_sources),
	  control_option(exclude_sources) ->
	  format('FATAL ERROR: Only one of --restrict_to_sources and~n', []),
	  format('                         --exclude_sources     can be specified.~n', []),
	  Result is 1
	; Result is 0
	).

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

verify_semantic_type_options(Result) :-
	( control_option(restrict_to_sts),
	  control_option(exclude_sts) ->
	  format('FATAL ERROR: Only one of --restrict_to_sts and~n', []),
	  format('                         --exclude_sts     can be specified.~n', []),
	  Result is 1
	; Result is 0
	).

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

verify_gap_size_values(Result) :-
	control_value(gap_size, GapSize),
	!,
	( verify_gap_size_values_aux(GapSize) ->
	  Result is 0
	; format('FATAL ERROR: gap_size argument must be of form Int1,Int2 ', []),
	  format('with Int1 >= Int2+2~n', []),
	  Result is 1
	).
verify_gap_size_values(0).

verify_gap_size_values_aux(GapSize) :-
	GapSize = [MinPhraseLength, MaxGapSize],
	integer(MinPhraseLength),
	integer(MaxGapSize),
	MinPhraseLength >= MaxGapSize + 2.

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
	prolog_flag(argv, Argv),
	concat_atoms_with_separator(Argv, ' ', CommandLine),
	format_cmd_line_for_machine_output_1(Options, Args, ArgsMO).

% It is not obvious what's going on here:
% (1) prolog_flag(quintus_directory, QuintusDirectory)
%     will instantiate QuintusDirectory to
%     '/net/nls3/export/home/quintus/quintus3.5'
% (1) prolog_flag(runtime_directory, RuntimeDirectory)
%     will by default instantiate RuntimeDirectory to
%     '/net/nls3/export/home/quintus/quintus3.5/bin3.5/sun4-5'.
%
% However, if qsetpath has been called on the runtime binary,
% as is done in the latest version of SKRmake, i.e.,
% 
% RUNTIME_DIRECTORY=$NLS/bin/metamap
% qsetpath -r$RUNTIME_DIRECTORY a.out
% 
% then prolog_flag(runtime_directory, RuntimeDirectory)
% will instantiate RuntimeDirectory to $NLS/bin/metamap.

% So...the prefix/2 test check to see if RuntimeDirectory
% is the default, and, if so, use the default "MetaMap",
% and print a warning.

% This won't work in SICStus...duh...
get_program_name(ProgramName) :-
	prolog_flag(quintus_directory, QuintusDirectory),
	prolog_flag(runtime_directory, RuntimeDirectory),
	atom_codes(QuintusDirectory, QDChars),
	atom_codes(RuntimeDirectory, RDChars),
	( prefix(RDChars, QDChars) ->
	  ProgramName = 'MetaMap'
	; ProgramName = RuntimeDirectory
	),
	!.
get_program_name('MetaMap').


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


generate_EOT_output :-
	( control_option(indicate_citation_end) ->
	  % do not use portrayed(true)
	  write_term('EOT',[quoted(true)]),
	  format('.~n', [])
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
	( ( control_option(machine_output)
	  ; control_option(fielded_mmi_output)
	  ; control_option('XML')
	  ) ->
	  % Generate the MMO for the Utterance term,
	  % which is needed for both MMO and XML output!
	  UtteranceMMO = utterance(Label,Text0,UttStartPos/UttLength,ReplPos)
	; ( \+ control_option(hide_plain_syntax)
	  ; control_option(syntax)
	  ; control_option(variants)
	  ; \+ control_option(no_candidates)
	  ; \+ control_option(hide_mappings)
	  ) ->
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
	concat_atoms(MatchingWordList, MatchingWordAtom),
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
	( control_option(formal_tagger_output) ->
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
	; true
	).


/* compute_original_phrase(+PhraseTextAtom0, +AAs,
                           +Sentences, +CoordSentencesIn, -CoordSentencesOut,
                           -PhraseTextAtom)

compute_original_phrase/7 computes (original) PhraseTextAtom by tokenizing
(expanded) PhraseTextAtom0, sequencing through CoordSentencesIn and finding
the original matching phrase text in Sentences. 
CoordSentencesOut is the tokens leftover after finding the phrase text. */

compute_original_phrase(PhraseTextAtom0, AAs,
			Sentences, CoordSentencesIn, CoordSentencesOut,
			CoordPhraseTokens, PhraseTextAtom) :-
	( control_option(display_original_phrases) ->
	  format(user_output, 'c_o_p1: ~w~n', [PhraseTextAtom0]),
	  compute_original_phrase_1(PhraseTextAtom0, AAs,
	  			    Sentences, CoordSentencesIn, CoordSentencesOut,
				    CoordPhraseTokens, PhraseTextAtom),
	  format(user_output, 'c_o_p2: ~w~n', [PhraseTextAtom0])
        ; PhraseTextAtom = PhraseTextAtom0,
    	  CoordSentencesOut = CoordSentencesIn
        ).


% This code is dormant for now.
compute_original_phrase_1(PhraseTextAtom0, AAs,
			  Sentences, CoordSentencesIn, CoordSentencesOut,
			  CoordPhraseTokens, PhraseTextAtom) :-
	( PhraseTextAtom0 == '' ->
	  CoordSentencesOut = CoordSentencesIn,
	  CoordPhraseTokens = [],
	  PhraseTextAtom = ''
	; atom_codes(PhraseTextAtom0, PhraseText0),
	  trim_and_compress_internal_whitespace(PhraseText0, PhraseText1),
	  tokenize_text_utterly(PhraseText1, TokenizedPhraseText0),
	  find_phrase_tokens(TokenizedPhraseText0, AAs,
			     CoordSentencesIn, CoordSentencesOut,
			     CoordPhraseTokens) ->
	  extract_original_positions(CoordPhraseTokens, OrigPositions),
	  collapse_positions(OrigPositions, OrigPosition),
	  extract_original_tokens(OrigPosition, Sentences, OrigTokens),
	  extract_token_text(OrigTokens, OrigTokensText),
	  append(OrigTokensText, PhraseText),
	  atom_codes(PhraseTextAtom, PhraseText)
	; CoordPhraseTokens = [],
	  CoordSentencesOut = CoordSentencesIn,
	  format(user_output, 'ERROR: compute_original_phrase_1/5 failed on ~q.~n', [PhraseTextAtom0]),
	  PhraseTextAtom = PhraseTextAtom0
	),
	!.


expand_all_phrase_texts([], _AAs, []).
expand_all_phrase_texts([H|T], AAs, [ExpandedH|ExpandedT]) :-
	expand_one_phrase_text_if_possible(H, AAs, ExpandedH),
	expand_all_phrase_texts(T, AAs, ExpandedT).

expand_one_phrase_text_if_possible(PhraseText0, AAs, ExpandedPhraseText0) :-
	( expand_one_phrase_text(PhraseText0, AAs, ExpandedPhraseText0) ->
	  true
	; ExpandedPhraseText0 = [PhraseText0]
	).

expand_one_phrase_text(PhraseText0, AAs, ExpandedPhraseText0) :-
	avl_member(AATokens, AAs, [ExpansionTokens]),
	get_token_text(AATokens, AAAtom),
	atom_codes(AAAtom, AAString),
	AAString == PhraseText0 ->
	extract_token_strings(ExpansionTokens, ExpandedPhraseText0).

/*	
Consider the following (simplified) text:
	Spontaneous preterm birth (PTB) has risk.
	the risk of PTB.

In one of the calls to find_phrase_tokens/4,
the first argument ([TokenString|Rest]) will be

TokenizedPhraseText0 = ["of"," ","PTB","."]

CoordSentencesIn will be
  [tok(ws," "," ",pos(44,45),pos(50,51)),
   tok(lc,"of","of",pos(45,47),pos(51,53)),
   tok(ws," "," ",pos(47,48),pos(53,54)),
   tok(lc,"preterm","preterm",pos(48,55),pos(54,57)),
   tok(ws," "," ",pos(55,56),pos(54,57)),
   tok(lc,"birth","birth",pos(56,61),pos(54,57)),
   tok(pn,".",".",pos(61,62),pos(57,58))]

Unfortunately, the token "PTB" is not found in CoordSentencesIn,
so we must look in the AAs for the expansion of "PTB",
which is ["preterm", " ", "birth"]
and extract the tokens of the expansion.
*/


find_phrase_tokens([], _AA, CoordSentencesIn, CoordSentencesIn, []).
find_phrase_tokens([TokenString|Rest], AAs,
		   CoordSentencesIn, CoordSentencesOut,
		   CoordPhraseTokens) :-
	CoordSentencesIn = [FirstToken|TokensTail],
	  % skip over a non-matching space token
	( ws_tok(FirstToken),
	  TokenString = [Char|_],
	  \+ ws_char(Char) ->
	  Rest1 = [TokenString|Rest],
	  CoordSentencesNext = TokensTail,
	  RestCoordPhraseTokens = CoordPhraseTokens
	  % skip over a non-matching space char
	; TokenString = [Char],
	  ws_char(Char),
	  \+ ws_tok(FirstToken) ->
	  Rest1 = Rest,
          CoordSentencesNext = CoordSentencesIn,
	  RestCoordPhraseTokens = CoordPhraseTokens
	; higher_order_tok(FirstToken) ->
	  Rest1 = [TokenString|Rest],
	  CoordSentencesNext = TokensTail,
	  RestCoordPhraseTokens = CoordPhraseTokens	  
	  % This is to handle an AA that is in the tokens
	; match_AA_tokens(TokenString, Rest, AAs, CoordSentencesIn, CoordPhraseTokens,
			  CoordSentencesNext, RestCoordPhraseTokens, Rest1) ->
	  true
	  % This is to handle an AA that is not in the tokens
	; remove_AA_expansion_words(TokenString, Rest, AAs,
				    CoordSentencesIn, CoordSentencesNext, Rest1) ->
	  CoordSentencesNext = CoordSentencesIn,
	  RestCoordPhraseTokens = CoordPhraseTokens
	; find_one_phrase_token(CoordSentencesIn, CoordSentencesNext,
				TokenString, CoordPhraseToken) ->
	  CoordPhraseTokens = [CoordPhraseToken|RestCoordPhraseTokens],
	  Rest1 = Rest
	; expand_one_phrase_text(TokenString, AAs, ExpandedTokenString),
	  find_phrase_tokens(ExpandedTokenString, AAs,
			     CoordSentencesIn, CoordSentencesNext,
			     [ExpandedToken|RestExpandedTokens]),
	  append([ExpandedToken|RestExpandedTokens], RestCoordPhraseTokens, CoordPhraseTokens),
	  Rest1 = Rest
	),
	find_phrase_tokens(Rest1, AAs, CoordSentencesNext, CoordSentencesOut,
			   RestCoordPhraseTokens).

remove_AA_expansion_words(TokenString, Rest0, AAs,
			  _CoordSentencesIn, _CoordSentencesNext, Rest2) :-
	TokenString = [Char],
	ex_lbracket_char(Char),
	avl_member(AATokens, AAs, _ExpansionTokens),
	match_non_ws_strings(AATokens, Rest0, Rest1),
	( Rest1 = [[H2]|Rest2],
	  ex_rbracket_char(H2) ->
	  true
	; Rest2 = Rest1
	).

% TokenizedPhraseText0 = ["(","TNF"," ","alpha",","]
% CoordSentencesIn     = [
%    tok(ws," "," ",pos(82,83),pos(110,111)),
%    tok(pe,[],1,pos(83,161),pos(111,148)),
%    tok(pn,"(","(",pos(83,84),pos(111,112)),
%    tok(ic,"Tumor","tumor",pos(84,89),pos(112,121)),
%    tok(ws," "," ",pos(89,90),pos(112,121)),
%    tok(ic,"Necrosis","necrosis",pos(90,98),pos(112,121)),
%    tok(ws," "," ",pos(98,99),pos(112,121)),
%    tok(ic,"Factor","factor",pos(99,105),pos(112,121)),
%    tok(pn,"-","-",pos(105,106),pos(112,121)),
%    tok(lc,"alpha","alpha",pos(106,111),pos(112,121)),
%    tok(pn,",",",",pos(111,112),pos(121,122)),
%    tok(ws," "," ",pos(112,113),pos(122,123)),

match_AA_tokens(FirstString, RestStringsIn, AAs, CoordSentencesIn0, CoordPhraseTokens,
		 CoordSentencesNext, RestCoordPhraseTokens, RestStringsOut) :-
	optionally_remove_pe_toks(CoordSentencesIn0, FirstString, RestStringsIn,
				  CoordSentencesIn1, RestStringsNext0),
	optionally_remove_ex_lbracket_char(RestStringsNext0, RestStringsNext),
	optionally_remove_ex_lbracket_tok(CoordSentencesIn1, CoordSentencesIn2),
	avl_member(AATokens, AAs, [ExpansionTokens]),
	match_non_ws_strings(AATokens, RestStringsNext, RestStringsOut),
	match_strings(ExpansionTokens, CoordSentencesIn2, MatchingTokens, CoordSentencesNext),
	append(MatchingTokens, RestCoordPhraseTokens, CoordPhraseTokens).

optionally_remove_pe_toks(CoordSentencesIn0, FirstString, RestStringsIn,
			  CoordSentencesIn1, RestStringsNext) :-
	CoordSentencesIn0 = [FirstToken,SecondToken|RestCoordSentencesIn],
	( pe_tok(FirstToken),
	  ex_lbracket_tok(SecondToken),
	  FirstString = [Char],
	  ex_lbracket_char(Char) ->
	  CoordSentencesIn1 = RestCoordSentencesIn,
	  RestStringsNext = RestStringsIn
	; CoordSentencesIn1 = CoordSentencesIn0,
	  RestStringsNext = [FirstString|RestStringsIn]
	).

optionally_remove_ex_lbracket_char([H|T], RestStringsNext) :-
	( H = [Char],
	  ex_lbracket_char(Char),
	  RestStringsNext = T
	; RestStringsNext = [H|T]
	).

optionally_remove_ex_lbracket_tok([H|T], CoordSentencesIn2) :-
	( ex_lbracket_tok(H),
	  CoordSentencesIn2 = T
	; CoordSentencesIn2 = [H|T]
	).


match_strings([], CoordSentencesIn, [], CoordSentencesIn).
match_strings([H1|T], [H2|RestCoordPhraseTokens], [H2|RestMatchingTokens], CoordPhraseTokensTail) :-
	     token_template(H1, TokenType1, TokenString1, LCTokenString1, _H1PosInfo1),
	     token_template(H2, TokenType2, TokenString2, LCTokenString2, _H2PosInfo1, _H2PosIinfo2),
	     TokenType1 == TokenType2,
	     TokenString1 == TokenString2,
	     LCTokenString1 == LCTokenString2,
	     match_strings(T, RestCoordPhraseTokens, RestMatchingTokens, CoordPhraseTokensTail).

% FOO	

match_non_ws_strings([], RestStrings,  RestStrings).
match_non_ws_strings([FirstAAToken|RestAATokens], [FirstString|RestNext], RestOut) :-
	( FirstString = [Char],
	  ws_char(Char) ->
	  match_non_ws_strings([FirstAAToken|RestAATokens], RestNext, RestOut)
	; token_template(FirstAAToken, _TokenType, TokenString, _LCTokenString, _PosInfo),
	  TokenString == FirstString,
	  match_non_ws_strings(RestAATokens, RestNext, RestOut)
	).

token_template(tok(TokenType,TokenString,LCTokenString,PosInfo),
	       TokenType, TokenString, LCTokenString, PosInfo).
	% format(user_output, 'TT/5: ~q~n', [tok(TokenType,TokenString,LCTokenString,PosInfo)]).

token_template(tok(TokenType,TokenString,LCTokenString,PosInfo1,PosInfo2),
	       TokenType, TokenString, LCTokenString, PosInfo1,PosInfo2).

% skip over higher-order tokens
find_one_phrase_token([tok(Type,_,_,_,_)|Rest], CoordSentencesOut,
		  TokenString, CoordPhraseToken) :-
	higher_order_type(Type),
	!,
	find_one_phrase_token(Rest, CoordSentencesOut, TokenString, CoordPhraseToken).
find_one_phrase_token([Token|Rest], Rest, TokenString, Token) :-
	Token = tok(_,TokenString,_,_,_),
	!.
% skipping over a non-matching token
find_one_phrase_token([Token|Rest], [Token|CoordSentencesOut], TokenString, CoordPhraseToken) :-
	find_one_phrase_token(Rest,CoordSentencesOut, TokenString, CoordPhraseToken).


extract_original_positions([], []).
extract_original_positions([tok(_,_,_,_,OrigPos)|Rest],
			   [OrigPos|ExtractedRest]) :-
	skip_duplicate_orig_positions(Rest, OrigPos, NewRest),
	extract_original_positions(NewRest, ExtractedRest).

collapse_positions([], []).
collapse_positions(Positions, [pos(Begin,End)]) :-
	Positions = [pos(Begin,_)|_],
	% reversed order of args from QP library version!
	last(Positions, pos(_,End)).

skip_duplicate_orig_positions([tok(_,_,_,_,OrigPos)|Rest],OrigPos,Result) :-
    !,
    skip_duplicate_orig_positions(Rest,OrigPos,Result).
skip_duplicate_orig_positions(CoordPhraseTokens,_OrigPos,CoordPhraseTokens).

extract_original_tokens([],_, []) :-
    !.
% temp
%extract_original_tokens([Pos|_Rest],[Token|_RestSentences],_) :-
%    format('eot: ~p  ~p~n',[Pos,Token]),
%    fail.
extract_original_tokens(OrigPositions,[tok(Type,_,_,_)|Rest],OrigTokens) :-
    higher_order_or_annotation_type(Type),
    !,
    extract_original_tokens(OrigPositions,Rest,OrigTokens).
extract_original_tokens([Pos|Rest],[Token|RestSentences],Result) :-
    Token=tok(_,_,_,TokenPos),
    pos_within_pos(TokenPos,Pos,EndTogether),
    !,
    (EndTogether==yes ->
	NewPos=Rest,
	extract_original_wspeaa_tokens(RestSentences,WSPEAAOrigTokens),
	append([[Token],WSPEAAOrigTokens,RestOrigTokens],Result)
    ;   NewPos=[Pos|Rest],
	Result=[Token|RestOrigTokens]
    ),
    extract_original_tokens(NewPos,RestSentences,RestOrigTokens).
extract_original_tokens(OrigPositions,[_Token|RestSentences],OrigTokens) :-
    extract_original_tokens(OrigPositions,RestSentences,OrigTokens).

pos_within_pos(pos(Begin1,End1),pos(Begin2,End2),EndTogether) :-
    Begin2=<Begin1,
    End1=<End2,
    !,
    (End1=:=End2 ->
	EndTogether=yes
    ;   EndTogether=no
    ).

/* extract_original_wspeaa_tokens(+Sentences, -WSPEAATokens)

extract_original_wspeaa_tokens/2 extracts a set of initial tokens from Sentences
if Sentences begins with optional whitespace followed by a pe containing an aa.
Only lower-order tokens are extracted. If Sentences does not satisfy the
condition, WSPEAATokens is []. */

extract_original_wspeaa_tokens([], []) :-
    !.
extract_original_wspeaa_tokens(Sentences,PEAATokens) :-
    gather_whitespace(Sentences,WSTokens,RestSentences),
    extract_peaa_tokens(RestSentences,PEAATokens0),
    !,
    append(WSTokens,PEAATokens0,PEAATokens).
extract_original_wspeaa_tokens(_, []) :-
    !.

extract_peaa_tokens([PEToken|Rest],PEAATokens) :-
    PEToken=tok(pe,_,_,PETokenPos),
    !,
    extract_tokens_within_pos(Rest,PETokenPos,PEAATokens0),
    contains_aa_token(PEAATokens0),
    filter_to_lower_order_tokens(PEAATokens0,PEAATokens).

extract_tokens_within_pos([],_, []) :-
    !.
extract_tokens_within_pos([Token|Rest],Pos,[Token|ExtractedRest]) :-
    Token=tok(_,_,_,TokenPos),
    pos_within_pos(TokenPos,Pos,EndTogether),
    !,
    (EndTogether==yes ->
        ExtractedRest=[]
    ;   extract_tokens_within_pos(Rest,Pos,ExtractedRest)
    ).
extract_tokens_within_pos(_,_, []) :-
    !.

contains_aa_token([]) :-
    !,
    fail.
contains_aa_token([tok(aa,_,_,_)|_Rest]) :-
    !.
contains_aa_token([_|Rest]) :-
    contains_aa_token(Rest).

filter_to_lower_order_tokens([], []) :-
    !.
filter_to_lower_order_tokens([Token|Rest],FilteredRest) :-
    Token=tok(Type,_,_,_),
    higher_order_or_annotation_type(Type),
    !,
    filter_to_lower_order_tokens(Rest,FilteredRest).
filter_to_lower_order_tokens([Token|Rest],[Token|FilteredRest]) :-
    filter_to_lower_order_tokens(Rest,FilteredRest).

extract_token_text([], []) :-
    !.
extract_token_text([tok(_,Text,_,_)|Rest],[Text|RestText]) :-
    extract_token_text(Rest,RestText).

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

get_token_text(TokenList, TextAtom) :-
	% get_token_text_strings(TokenList, TokenTextStrings),
	extract_token_strings(TokenList, TokenTextStrings),
	append(TokenTextStrings, TextString),
	atom_codes(TextAtom, TextString).

% get_token_text_strings([], []).
% get_token_text_strings([tok(_TokenType,TokenText,_TokenLCText,_PosInfo)|RestTokens],
% 		       [TokenText|RestTokenTextStrings]) :-
% 	get_token_text_strings(RestTokens, RestTokenTextStrings).

ensure_atom(Input, InputAtom) :-
	( atom(Input) ->
	  InputAtom = Input
	; number(Input) ->
	  number_codes(Input, InputCodes),
	  atom_codes(InputAtom, InputCodes)
	; is_print_string(Input) ->
	  atom_codes(InputAtom, Input)
	).
	
ensure_number(Atom, Number) :-
	( number(Atom) ->
	  Number is Atom
	; atom_codes(Atom, AtomCodes),
	  number_codes(Number, AtomCodes)
	).
