:- module(skr_tcp, [
	establish_tcp_connection/3
    ]).

:- use_module(library(sockets), [
	socket_client_open/3
	% tcp_connect/2,
	% tcp_input_stream/2,
	% tcp_output_stream/2,
	% tcp_shutdown/1	
   ]).

establish_tcp_connection(TaggerServerAddress, Port, Stream) :-
	test_tcp_connect(TaggerServerAddress,Port, Stream).
	% test_tcp_input_stream(Socket,  Port, TaggerServerAddress, StreamIn),
	% test_tcp_output_stream(Socket, Port, TaggerServerAddress, StreamOut).

test_tcp_connect(TaggerServerAddress, Port, Stream) :-
	on_exception(ExceptionCode,
		     socket_client_open(inet(TaggerServerAddress,Port), Stream, [type(text)]),
		     signal_tcp_error(socket_client_open,
				      Port, TaggerServerAddress, ExceptionCode)).

%%% test_tcp_input_stream(Socket, Port, Host, StreamIn) :-
%%% 	on_exception(ExceptionCode,
%%% 		     tcp_input_stream(Socket, StreamIn),
%%% 		     signal_tcp_error(tcp_input_stream, Port, Host, ExceptionCode)).
%%% 
%%% test_tcp_output_stream(Socket, Port, Host, StreamOut) :-
%%% 	on_exception(ExceptionCode,
%%% 		     tcp_output_stream(Socket, StreamOut),
%%% 		     signal_tcp_error(tcp_output_stream, Port, Host, ExceptionCode)).

signal_tcp_error(Predicate, Port, Host, ExceptionCode) :-
	format(user_output,
	       '~n~nERROR in calling ~w for WSD Server on host ~w/port ~w:~n~w~n~n',
	       [Predicate, Host, Port, ExceptionCode]),
	format('~n~nERROR in calling ~w for WSD Server on host ~w/port ~w:~n~w~n~n',
	       [Predicate, Host, Port, ExceptionCode]),
	halt.
