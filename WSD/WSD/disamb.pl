:- module(disamb, [
       run_disamb/1,go/0
    ]).

:- use_module( library( addportray ),	 [ add_portray/1 ]).

%:- use_module( library( files ),	 [ close_all_streams/0 ]).

%:- use_module( library( print_chars ),	 [ ]).

:- use_module( library( strings ),	 [ atom_chars/2 ]).

:- use_module( skr_lib(nls_io),       [ fget_non_null_line/2,
                                         fget_lines_until_null_line/2 ]).

:- use_module( skr_lib( nls_strings ),[ portray_strings_double_quoted/1,
	                                   trim_whitespace/2 ]).

:- use_module( skr_lib( nls_system ), [ control_option/1,
	                                 display_control_options_for_modules/2,
	                                 display_current_control_options/0,
                                         get_control_options_for_modules/2,
                                         get_from_iargs/4,
	                                 interpret_options/4,
	                                 interpret_args/4,
	                                 parse_command_line/1,
                                         reset_control_options/1,
                                         set_control_options/1,
                                         set_control_values/2,
	                                 toggle_control_options/1,
                                         unset_control_options/1
	                                        ] ).
:- use_module(wsd(wsdmod ), [init_disamb/0,
			     stop_disamb/0,
			     wsd/1,
			     send_machine_output/1
			     ]).
		        

go :-
    go(halt).

go(HaltOption) :-
    parse_command_line( CLTerm ),
    go( HaltOption, CLTerm ).
go( _HaltOption, command_line(Options,Args) ) :-
    add_portray( portray_strings_double_quoted ),
    reset_control_options(abs),
    initialize(Options,Args,InterpretedArgs),
    run_disamb(InterpretedArgs),
    stop_disamb.
    close_all_streams.

initialize( Options, Args, InterpretedArgs ) :-
    get_control_options_for_modules( [abs], AllOptions ),
    interpret_options( Options, AllOptions, abs, IOptions),
    ArgSpec=[ aspec(infile,mandatory,file,read,
                           user_input,
                          'Input file.'),
              aspec(outfile,mandatory,file,write,
                            or(['<infile>','.',disamb], user_output),
                           'Output file.')
            ],
    interpret_args( IOptions, ArgSpec, Args, InterpretedArgs ),
    toggle_control_options( IOptions ),
    set_control_values( IOptions, InterpretedArgs ),
    stop_disamb,
    init_disamb.

run_disamb(InterpretedArgs) :- 
    get_from_iargs(infile,stream,InterpretedArgs,InputStream),
    get_from_iargs(outfile,stream,InterpretedArgs,OutputStream), 
    acquire_input_text(InputStream,InpStrList), !,
    convert_to_atoms(InpStrList,InpAtomList),
    send_machine_output(InpAtomList),
    wsd(WSDAtomOut),
    atom_chars(WSDAtomOut,WSDStringOut),
    format(OutputStream,'~s~n',[WSDStringOut]),
    run_disamb(InterpretedArgs).



acquire_input_text(InputStream,Lines) :-
    get_text(InputStream,Lines),
    ( Lines == [] -> fail ; true ).

% ---

get_text(InputStream,[First|Rest]) :-
    fget_non_null_line(InputStream,First),
    !,
    fget_lines_until_null_line(InputStream,Rest).
get_text(_,[]).

convert_to_atoms([],[]).
convert_to_atoms([String|Rest],[StrAtom|Gap]) :-
    atom_chars(StrAtom,String),
    convert_to_atoms(Rest,Gap).
