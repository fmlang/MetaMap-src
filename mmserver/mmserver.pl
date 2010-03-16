:- module(mmserver,[main/0,list_current_options/1]).

:- use_module(library(prologbeans), [ register_query/2, start/0 ]).

:- use_module(mms_support, [
    initialize_skr/6,
    process_text/5,
    postprocess_sentences/10
    ]).

:- use_module(skr_lib(nls_system), [
	get_control_options_for_modules/2,
	reset_control_options/1,
	toggle_control_options/1,
        set_control_options/1,
	set_control_values/2,
	control_option/1,
	parse_command_line/1,
	interpret_options/4,
	interpret_args/4,
	get_from_iargs/4
    ]).

:- use_module(skr(skr_utilities),[
	compare_utterance_lengths/2,
	do_formal_tagger_output/0,
	do_sanity_checking_and_housekeeping/2,
	generate_bracketed_output/2,
	generate_candidates_output/3,
	generate_EOT_output/0,
	generate_header_output/6,
	generate_mappings_output/5,
	generate_phrase_output/7,
	generate_utterance_output/6,
	generate_variants_output/2,
	get_program_name/1,
	output_should_be_bracketed/1,
	output_tagging/3,
	replace_crs_with_blanks/4,
	usage/0,
        write_MMO_terms/1,
        write_raw_token_lists/3,
        write_sentences/3
    ]).

:- use_module(skr_lib(negex), [
        compute_negex/4,
	generate_negex_output/1
    ]).

%% Register acceptable queries and start the server (using default port)
main :-
    register_query(list_current_options(Options), list_current_options(Options)),
    register_query(get_control_options_for_modules(Module,Options),
		   get_control_options_for_modules(Module,Options)),
    register_query(set_control_options(Options),set_control_options(Options)),
    register_query(process_string(Input,Output), process_string(Input,Output)),
    % register_query(process_string(Input, Results), process_string(Input, Results)),

    ArgSpecs=[aspec(infile,mandatory,file,read,
		    user_input,
		    'Input file containing labelled utterances'),
	      aspec(outfile,mandatory,file,write,
		    or(['<infile>','.','out'],user_output),
		    'Output file')
	      ],
    Args=[],
    Options=[q],
    IOptions=[iopt(machine_output,none)],
    interpret_args(IOptions, ArgSpecs, Args, IArgs),
    % toggle_control_options(IOptions),
    % set_control_values(IOptions,IArgs),
    ProgramName = 'MetaMap',
    FullYear = 2008,
    % format(user_output,'IOptions=~w~n', [IOptions]),
    initialize_skr(Options, Args, IArgs, ProgramName, FullYear, IOptions),
    start.

list_current_options(Options) :-
	get_control_options_for_modules(metamap,Options).

set_options(Options) :-
	set_control_options(Options).

get_options(Options) :-
	get_control_options_for_modules(metamap,Options).

process_string(Input,Output) :-
	TagOption = tag,
	Strings = [Input],
	InterpretedArgs = [iarg(infile,aspec(infile,mandatory,file,read,no_default,'Input file containing labelled utterances'),
				[name(user_input),stream(user_input)]),
			   iarg(outfile,aspec(outfile,mandatory,file,write,no_default,'Output file'),
				[name(user_output),stream(user_output)])],
	process_text(Strings, TagOption, ExpRawTokenList, AAs, MMResults),
	% get_control_options_for_modules(metamap,IOptions),
	IOptions=[],
	format('IOptions: ~q~n', [IOptions]),
	output_should_be_bracketed(BracketedOutput),
	postprocess_text_mmserver(Strings, BracketedOutput, InterpretedArgs,
		 IOptions,  ExpRawTokenList, AAs, MMResults, Output).

postprocess_text_mmserver(Lines0, BracketedOutput, InterpretedArgs,
			  IOptions,  ExpRawTokenList, AAs, MMResults, AllMMO) :-
	MMResults = mm_results(Lines0, _TagOption, _ModifiedLines, _InputType,
			       Sentences, CoordSentences, OrigUtterances, DisambMMOutput),

	compute_negex(ExpRawTokenList, Lines0, DisambMMOutput, NegationTerms),
	generate_negex_output(NegationTerms),
	postprocess_sentences(OrigUtterances, NegationTerms, InterpretedArgs, IOptions, AAs,
			      Sentences, CoordSentences, BracketedOutput, DisambMMOutput,
			      AllMMO).
% 	generate_and_print_xml(AllMMO),
% 	do_MMI_processing(OrigUtterances, BracketedOutput,
% 			  Sentences, CoordSentences, DisambMMOutput),
% 	do_formal_tagger_output.
%	format('AllMMO: ~q~n', [AllMMO]).




