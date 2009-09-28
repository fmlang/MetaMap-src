% File:	    tagger_access.pl
% Module:   Tagger Access
% Author:   Lan
% Purpose:  Provide access to the NLS Tagger Server

:- module(tagger_access,[
	tag_text/2,
	tag_text/4
   ]).

:- use_module(skr_lib(nls_strings),[
	atom_codes_list_list/2,
	is_print_string/1,
	split_string/4
   ]).

:- use_module(skr_lib(skr_tcp),[
	establish_tcp_connection/3
   ]).

:- use_module(skr_lib(nls_system),[
	control_option/1,
	control_value/2
   ]).

:- use_module(skr_lib(sicstus_utils),[
	concat_atoms/2,
	number_to_atom/2,
	replist/3,
	ttyflush/0,
	with_input_from_chars/3
   ]).

:- use_module(library(between),[
	between/3
   ]).

:- use_module(library(lists),[
	append/2,
	rev/2
   ]).

:- use_module(library(random),[
	maybe/0
   ]).

:- use_module(library(system),[
	environ/2
    ]).

/* tag_text(+Input, -TaggedTextList)
   tag_text(+Input, -FullTaggedTextList, -TaggedTextList, -TaggedTextStrings)
   tag_text_with_options(+Input, +ModeOption, +PrologOption, -TaggedTextList)
   tag_text_aux(+OptionsAtom, +QueryAtom, -TaggedTextList)

tag_text_with_options/4 tags Input with ModeOption (syn, sem, or semonly)
and PrologOption (prolog or prologfull).  tag_text/2 calls
tag_text_with_options/4 with ModeOption syn and PrologOption prolog.
tag_text/4 tags Input producing all forms of syn output: prolog, prologfull
and normal.
TaggedTextList is list of taggings each of which is a list consisting of a
token and its tag (both atoms, even if Input is a string).
TaggedTextStrings strings consisting of the human-readable output. */

tag_text(Input, TaggedTextList) :-
        tag_text_with_options(Input, syn, prolog, TaggedTextList).

tag_text(Input, FullTaggedTextList, TaggedTextList, TaggedTextStrings) :-
	tag_text_with_options(Input, syn, prologfull, FullTaggedTextList),
	!,
	( atom_codes_list_list(FullTaggedTextList, FullTaggedTextListStrings) ->
	  form_prolog_output(FullTaggedTextListStrings, TaggedTextList),
	  form_human_readable_output(FullTaggedTextListStrings, TaggedTextStrings)
	; format(user_output,
		 'ERROR: tag_text/4 failed for ~p~n returning ~p~n',
		 [Input,FullTaggedTextList]),
	  format('ERROR: tag_text/4 failed for ~p~n returning ~p~n',
		 [Input,FullTaggedTextList]),
	  !,
	  halt
	),
	!.

tag_text_with_options([], _, _, []) :-
	!.
tag_text_with_options(Input, ModeOption, PrologOption, TaggedTextList) :-
	atom_codes(ModeOption, MOString),
	atom_codes(PrologOption, POString),
	append([MOString,"|",POString], OptionsString),
	atom_codes(Options, OptionsString),
	( atom(Input) ->
	  QueryAtom=Input,
	  tag_text_aux(Options, QueryAtom, TaggedTextList),
	  TaggedTextList \== '',
	  TaggedTextList \== end_of_file
	; is_print_string(Input) ->
	  atom_codes(QueryAtom0,Input),
	  ( var(QueryAtom0) ->
	    append(Input," ",NewInput),
	    atom_codes(QueryAtom,NewInput)
	  ; QueryAtom=QueryAtom0
	  ),
	  tag_text_aux(Options, QueryAtom, TaggedTextList),
	  !,
	  TaggedTextList \== '',
	  TaggedTextList \== end_of_file
	; TaggedTextList=[]
	),
	!.
tag_text_with_options(Input, _, _Tagger, []) :-
	format(user_output,
	       'ERROR: tagger_access:tag_text_with_options/4 failed for ~p~n',
	       [Input]),
	format('ERROR: tagger_access:tag_text_with_options/4 failed for ~p~n',
	       [Input]),
	halt.

tag_text_aux(Options, QueryAtom, TaggedTextList) :-
	call_tagger(Options, QueryAtom, TaggedTextAtom),
	atom_codes(TaggedTextAtom,TaggedTextString),
	% special case when result begins with ^J
	( TaggedTextString=[10|_] ->
	  TaggedTextList=[]
	; with_input_from_chars(read(Stream,TaggedTextList),
				Stream,
				TaggedTextString)
	).

call_tagger(Options, QueryAtom,TaggedTextAtom) :-
	% format(user_output, '~nAbout to call tagger...', []),
	( call_tagger_aux(Options, QueryAtom, TaggedTextAtom) ->
	  % format(user_output, 'Tagger call successful~n', []),
	  true
	; format(user_output, '~nERROR: Tagger (call_tagger_aux) failed on "~w"~n.', [QueryAtom]),
	  format('~nERROR: Tagger (call_tagger_aux) failed on "~w"~n.', [QueryAtom]),
	  halt
        ).

form_prolog_output([], []).
form_prolog_output([[WordString,TypeString]|Rest],
		   [[Word,ModifiedType]|ModifiedRest]) :-
	atom_codes(Word, WordString),
	( split_string(TypeString, "/", ModifiedTypeString, _) ->
	  atom_codes(ModifiedType, ModifiedTypeString)
	; atom_codes(ModifiedType,TypeString)
	),
	form_prolog_output(Rest,ModifiedRest).

form_human_readable_output([],[""]) :-
    !.
form_human_readable_output(FTTLS,Result) :-
    form_human_readable_output(FTTLS,"","",[],RevResult),
    rev(RevResult,Result).

form_human_readable_output([],CurWords,CurTypes,RRIn,RR) :-
    (CurWords=="" ->
        RR=RRIn
    ;   (RRIn==[] ->
            RR=[CurTypes,CurWords]
        ;   RR=[CurTypes,CurWords,""|RRIn]
	)
    ).
form_human_readable_output([[Word,Type]|Rest],CurWords,CurTypes,RRIn,RROut) :-
    length(Word,LWord),
    length(Type,LType),
    (   LWord > LType ->
	LNew=LWord,
	LDiff is LWord - LType,
	NewWord=Word,
	replist(0' ,LDiff,Padding),  % ' ,
        append(Type,Padding,NewType)
    ;   LType > LWord ->
	LNew=LType,
        LDiff is LType - LWord,
	NewType=Type,
	replist(0' ,LDiff,Padding),  % ' ,
        append(Word,Padding,NewWord)
    ;   LNew=LType,
        NewWord=Word,
	NewType=Type
    ),
    length(CurWords,LinePos),
    NewLinePos is LinePos + LNew + 1,
    (((NewLinePos < 78;
       CurWords=="")) ->
        % append to current lines
        (CurWords=="" ->
	    NewCurWords=NewWord,
	    NewCurTypes=NewType
	;   append([CurWords," ",NewWord],NewCurWords),
	    append([CurTypes," ",NewType],NewCurTypes)
	),
	RRInOut=RRIn
    ;   % start new lines
	NewCurWords=NewWord,
	NewCurTypes=NewType,
	(RRIn==[] ->
	    RRInOut=[CurTypes,CurWords]
	;   RRInOut=[CurTypes,CurWords,""|RRIn]
	)
    ),
    form_human_readable_output(Rest,NewCurWords,NewCurTypes,RRInOut,RROut).

choose_tagger_server_host_and_tcp_port(TaggerServerNumber, TaggerServerAddress, Tagger_TCP_PortAtom) :-
	( control_value(tagger, TaggerServerAddress) ->
	  % If it's a user-specified tagger, then just
	  % arbitrarily use the port corresponding to 0
	  tagger_TCP_port_choice(0, Tagger_TCP_PortAtom),
	  TaggerServerNumber is -9999
	; maybe ->
	  tagger_server_address(0, TaggerServerAddress),
	  tagger_TCP_port_choice(0, Tagger_TCP_PortAtom),
	  TaggerServerNumber is 0
	; 
	  tagger_server_address(1, TaggerServerAddress),
	  tagger_TCP_port_choice(1, Tagger_TCP_PortAtom),
	  TaggerServerNumber is 1
	).

tagger_server_address(TaggerServerNumber, TaggerServerAddress) :-
	number_to_atom(TaggerServerNumber, TaggerServerAtom),
	concat_atoms(['TAGGER_SERVER_NODENAME_', TaggerServerAtom], TaggerServerEnvVar),
	% from SKRenv:
	% export TAGGER_SERVER_NODENAME_0='130.14.111.65' # ind1
	% export TAGGER_SERVER_NODENAME_1='130.14.111.66' # skr1
	environ(TaggerServerEnvVar, TaggerServerAddress).

tagger_TCP_port_choice(TaggerServerNumber, Tagger_TCP_PortChoice) :-
	number_to_atom(TaggerServerNumber, TaggerServerAtom),
	concat_atoms(['TAGGER_SERVER_TCP_PORT_', TaggerServerAtom], Tagger_TCP_PortEnvVar),
	% from SKRenv:
	% export TAGGER_SERVER_TCP_PORT_0=5554
	% export TAGGER_SERVER_TCP_PORT_1=5554
	environ(Tagger_TCP_PortEnvVar, Tagger_TCP_PortChoice).

call_tagger_aux(Options, QueryAtom, TaggedTextAtom) :-
	% GIVEN:
	%   options <- Options (is an atom?)
	%   text <- QueryAtom
	%   input <- Options + NewLine + QueryAtom + NewLine + NewLine
	% Original C code
	%   strcpy(input, options);
	%   strcat(input, "\n");
	%   strcat(input, text);
	%   strcat(input, "\n");
	%   strcat(input, "\n");
	% Try to connect 100 times; if it still fails, give up!
	between(1, 100, _),
	   choose_tagger_server_host_and_tcp_port(TaggerServerNumber, TaggerServerAddress, PortAtom),
	   tagger_server_message(TaggerServerNumber, TaggerServerMessage),
	   atom_codes(NewLine, [10]), % is there a more elegant way to do this?
	   concat_atoms([Options,NewLine,QueryAtom,NewLine,NewLine], Request),
	   atom_codes(PortAtom, PortString),
	   number_codes(Port, PortString),
	   establish_tcp_connection(TaggerServerAddress, Port, SocketStream),
	   conditionally_announce_tagger_connection(TaggerServerMessage, TaggerServerAddress),
	   test_post_tagger_query(SocketStream, Request),
	   test_get_tagger_result(SocketStream, Request, Response),
	   % At this point, we know the tagger is OK,
	   % so we can cut out the choice point created by between/3 above
	   !,
	   atom_codes(TaggedTextAtom, Response),
	   close(SocketStream).

conditionally_announce_tagger_connection(TaggerServerMessage, Host) :-
	( \+ control_option(no_header_info) ->
	   format(user_output,
		  'Established connection to Tagger Server on ~w~w.~n',
		  [TaggerServerMessage, Host])
	; true
	).  


tagger_server_message(TaggerServerNumber, Message) :-
	( TaggerServerNumber >= 0 ->
	  Message = ''
        ; Message = '***USER SPECIFIED*** '
    	).

% post_Tagger_query/2
test_post_tagger_query(SocketStream, Request) :-
	( format(SocketStream, '~a~n~n^THE_END^~n', [Request]),
	  flush_output(SocketStream) ->
	  true
        ; format(user_output, 'ERROR: Unable to post Tagger query~n~w~n', [Request]),
	  ttyflush,
	  format('ERROR: Unable to post Tagger query~n~w~n', [Request]),
	  halt
        ).

% get_Tagger_result/2
test_get_tagger_result(SocketStream, Request, StreamTerm) :-
	( get_chars_tagger(SocketStream, StreamTerm) ->
	  true
        ; format(user_output, 'ERROR: Unable to get Tagger result for ~n~w~n', [Request]),
	  ttyflush,
	  format('ERROR: Unable to get Tagger result for ~n~w~n', [Request]),
	  halt
        ).

% The tagger server sends back "%%" to signal EOF. No idea why.
get_chars_tagger(Stream, Input) :-
	get_code(Stream, Code),
	( Code =:= 37,
	  peek_code(Stream, NextCode),
	  NextCode =:= 37 ->
	  Input = []
	; otherwise ->
	  Input = [Code|Codes],
	  get_chars_tagger(Stream, Codes)
	).
