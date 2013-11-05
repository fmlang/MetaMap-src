:- module(mmserver,[ main/0 ]).

:- use_module(library(prologbeans), [
	register_query/2,
	register_event_listener/2,
	get_server_property/1,
	start/0,
	start/1,
	shutdown/1
   ]).
:- use_module(library(codesio), [
	 read_from_codes/2
   ]).

:- use_module(library(file_systems), [
	close_all_streams/0
   ]).

:- use_module(library(system), [
	 environ/2
   ]).

:- use_module(skr(skr_fe), [
	postprocess_sentences/11,
	initialize_skr/4,
	process_text/9
   ]).

:- use_module(skr(skr),[
	initialize_skr/1,
	stop_skr/0
   ]).

:- use_module(skr(skr_utilities),[
	output_should_be_bracketed/1
    ]).

:- use_module(skr_lib(nls_strings),[
	trim_whitespace_right/2,
	split_string/4,
	split_string_completely/3
    ]).

:- use_module(skr_lib(nls_system), [
	add_to_control_options/1,
	subtract_from_control_options/1,
	toggle_control_options/1,
	get_control_options_for_modules/2,
	set_control_options/1,        				    
	set_control_values/2,
	control_option/1,
	control_value/2,
	parse_command_line/1,
	parse_command_line/3,
	interpret_options/4,
	interpret_args/4
    ]).

:- use_module(skr_lib(server_choice), [
	get_server_streams/1,
	get_server_stream/2
   ]).

:- use_module(text(text_objects), [
	get_UDAs/1
   ]).

:- use_module(skr_lib(negex), [
	compute_negex/4,
	generate_negex_output/1
   ]).


%% Register acceptable queries and start the server (using default port)
main :-
    environ('MMSERVER_PORT', ServerPortEnv),
    atom_codes(ServerPortEnv, ServerPortChars),
    number_codes(ServerPort, ServerPortChars),
    environ('ACCEPTED_HOSTS', AcceptedHostsEnv),
    atom_codes(AcceptedHostsEnv, AcceptedHostsChars),
    append(AcceptedHostsChars, ".", AcceptedHostsCharsWithPeriod),
    read_from_codes(AcceptedHostsCharsWithPeriod, AcceptedHosts),
    ServerOptions=[port(ServerPort),accepted_hosts(AcceptedHosts)],
    format(user_error, 'Server options: ~q~N', [ServerOptions]),
    register_query(get_options(AllOptions), get_options(AllOptions)),
    register_query(process_string(Input,Output), process_string(Input,Output)),
    register_query(reset_options, reset_options),
    register_query(set_options(Options), set_options(Options)),
    register_query(unset_options(Options), unset_options(Options)),
    register_query(shutdown, shutdown_server),
    register_event_listener(server_started, server_started_listener),
    register_event_listener(server_shutdown, server_shutdown_listener),
    % ArgSpecs=[aspec(infile,mandatory,file,read,
    % 		    user_input,
    % 		    'Input file containing labelled utterances'),
    % 	      aspec(outfile,mandatory,file,write,
    % 		    or(['<infile>','.','out'],user_output),
    % 		    'Output file')
    % 	      ],
    get_server_streams(TaggerServerStream-WSDServerStream),
    AllServerStreams = TaggerServerStream-WSDServerStream,
    bb_put(all_server_streams, AllServerStreams),
    parse_command_line(CLTerm),
    CLTerm=command_line(Options,Args),
    ( \+ member(q,Options) -> append(Options, [q], OptionsFinal) ; Options=OptionsFinal),
    initialize_skr(OptionsFinal, Args, _IArgs, IOptions),
    add_to_control_options(IOptions),
    start(ServerOptions).

%% Event listener callbacks
server_started_listener :-
    get_server_property(port(Port)),
    format(user_error, 'port:~w~n', [Port]), % [PD]
    flush_output(user_error).

server_shutdown_listener :-
   format(user_error, '~Npbtest.pl: Shutdown server~n', []),
   flush_output(user_error).

shutdown_server :-
    close_all_streams,
    shutdown(now).

set_options(OptionString) :-
    append(OptionString, ".", OptionStringWithPeriod),
    read_from_codes(OptionStringWithPeriod, RawCL),
    parse_command_line(RawCL, Options, Args),
    get_control_options_for_modules([metamap], AllOptions),
    interpret_options(Options, AllOptions, metamap, IOptions),
    ArgSpecs=[aspec(infile,mandatory,file,read,
		    user_input,
		    'Input file containing labelled utterances'),
	      aspec(outfile,mandatory,file,write,
		    or(['<infile>','.','out'],user_output),
		    'Output file')
	      ],
    interpret_args(IOptions, ArgSpecs, Args, IArgs),
    ( \+ member(iopt(machine_output,none),IOptions) -> 
	append([iopt(machine_output,none)], IOptions, IOptionsFinal0) ;
	IOptions=IOptionsFinal0 ),
    %% Temporary code for use until a final lex access method is
    %% determined.
    ( \+ member(iopt(lexicon,c),IOptionsFinal0) -> 
	append([iopt(lexicon,c)], IOptionsFinal0, IOptionsFinal) ;
	IOptions=IOptionsFinal ),
    %% end Temporary code 
    add_to_control_options(IOptionsFinal),
    %% Temporary code for use until a final lex access method is
    %% determined.
    assert(control_value(lexicon,c)),
    % end Temporary code 
    set_control_values(IOptionsFinal,IArgs),
    %%
    %% If the user asks for WSD then get server stream for WSD server
    %% and add it to the blackboard.
    %%
    %%
    ( control_option(word_sense_disambiguation) ->
	%% first check to see if WSD socket is already open; skip if so.
	bb_get(all_server_streams, StoredAllServerStreams),
	StoredAllServerStreams=StoredTaggerServerStream-StoredWSDServerStream,
	( StoredWSDServerStream == '' ->
	    get_server_stream('WSD',WSDServerStream),
	    AllServerStreams = StoredTaggerServerStream-WSDServerStream,
	    bb_put(all_server_streams, AllServerStreams))).


unset_options(OptionString) :-
    append(OptionString, ".", OptionStringWithPeriod),
    read_from_codes(OptionStringWithPeriod, RawCL),
    parse_command_line(RawCL, Options, Args),
    get_control_options_for_modules([metamap], AllOptions),
    interpret_options(Options, AllOptions, metamap, IOptions),
    ArgSpecs=[aspec(infile,mandatory,file,read,
		    user_input,
		    'Input file containing labelled utterances'),
	      aspec(outfile,mandatory,file,write,
		    or(['<infile>','.','out'],user_output),
		    'Output file')
	      ],
    interpret_args(IOptions, ArgSpecs, Args, _IArgs),
    subtract_from_control_options(IOptions),
    %% Temporary code for use until a final lex access method is
    %% determined.
    assert(control_value(lexicon,c)),
    %% end Temporary code

    %% if WSD is turned off
    ( \+ control_option(word_sense_disambiguation) ->
    	bb_get(all_server_streams, StoredAllServerStreams),
    	StoredAllServerStreams=StoredTaggerServerStream-StoredWSDServerStream,
    	( StoredWSDServerStream \= '' ->
    	    close(StoredWSDServerStream),
    	    AllServerStreams = StoredTaggerServerStream-'',
    	    bb_put(all_server_streams, AllServerStreams))).

control_option_as_iopt(iopt(X,Value)) :-
	nls_system:control_value(X,Value).
control_option_as_iopt(iopt(X,none)) :-
	nls_system:control_option(X),
	\+ nls_system:control_value(X, _).

get_options(AllOptions) :-
	setof(X,control_option_as_iopt(X),AllOptions).

reset_options :-
	%% Temporary code for use until a final lex access method is
	%% determined.  This will need re-factoring to remove
	%% references to lexicon.
 	IOptions=[iopt(lexicon,c),iopt(machine_output,none)],
 	add_to_control_options(IOptions),
	assert(control_value(lexicon,c)).
        %% end Temporary code 

process_string(Input,Output) :-
	%% Temporary code for use until a final lex access method is
	%% determined.
	assert(control_value(lexicon,c)),
	%% end Temporary code 
	trim_whitespace_right(Input, TrimmedInput0),
	remove_final_CRLF(TrimmedInput0, TrimmedInput1),
	remove_final_CRLF(TrimmedInput1, TrimmedInput),
	TagOption = tag,
	split_string_completely(TrimmedInput,"\n",Strings),
	ExtraChars = [],
	get_UDAs(UDAList),
	bb_get(all_server_streams, AllServerStreams),
	process_text(Strings, "0000000", ExtraChars,
		     TagOption, AllServerStreams,
		     ExpRawTokenList, AAs, UDAList, MMResults),
	parse_command_line(CLTerm),
	CLTerm=command_line(Options,Args),
	initialize_skr(Options, Args, InterpretedArgs, IOptions),
	% get_options(IOptions),
	( \+ member(iopt(machine_output,none),IOptions) -> 
	    append([iopt(machine_output,none)], IOptions, IOptionsFinal0) ;
	    IOptions=IOptionsFinal0 ),
	%% Temporary code for use until a final lex access method is
	%% determined.
	( \+ member(iopt(lexicon,c),IOptionsFinal0) -> 
	    append([iopt(lexicon,c)], IOptionsFinal0, IOptionsFinal) ;
	    IOptions=IOptionsFinal ),
	%% end Temporary code 
	output_should_be_bracketed(BracketedOutput),
	postprocess_text_mmserver(Strings, BracketedOutput, InterpretedArgs,
		 IOptionsFinal,  ExpRawTokenList, AAs, MMResults, Output).

remove_final_CRLF(TrimmedInput0, TrimmedInput) :-
	( append(AllButLast, [Last], TrimmedInput0),
	  ( Last is 10
	  ; Last is 13
	  ) ->
	  TrimmedInput = AllButLast
	; TrimmedInput = TrimmedInput0
	).
	

postprocess_text_mmserver(Lines0, BracketedOutput, InterpretedArgs,
			  IOptions,  ExpRawTokenList, AAs, MMResults, AllMMO) :-
	MMResults = mm_results(Lines0, _TagOption, _ModifiedLines, _InputType,
			       Sentences, _CoordSentences, OrigUtterances, DisambMMOutput),

	compute_negex(ExpRawTokenList, Lines0, DisambMMOutput, NegationTerms),
	generate_negex_output(NegationTerms),
	postprocess_sentences(user_output, OrigUtterances, NegationTerms, InterpretedArgs,
			      IOptions, AAs, Sentences, BracketedOutput, DisambMMOutput,
			      0, AllMMO).
