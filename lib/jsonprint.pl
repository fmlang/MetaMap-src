:- module(jsonprint, [
	json_print_element/2
   ]).

:- use_module(library(lists), [
	is_list/1
]).

%%
%% JSON printing utilities
%%
json_print_element(object(ElementList), Stream) :-
	!,
	format(Stream, '{', []),	
	json_print_list_elements(ElementList, Stream),
	format(Stream, '}', []).

json_print_element(pair(Key, Value), Stream) :-
	!,
	( is_list(Value) ->
	  format(Stream, '"~s":', [Key]),
	  json_print_list(Value, Stream)
	; format(Stream, '"~s":"~s"', [Key,Value])
	).

json_print_element(Element, Stream) :-
	( is_list(Element) ->
	  json_print_list(Element, Stream)
	; format(Stream, '"~s"', [Element])
	).

json_print_list(List, Stream) :-
	  format(Stream, '[', []),
	  json_print_list_elements(List, Stream),
	  format(Stream, ']', []).

json_print_list_elements([], _Stream).
json_print_list_elements([Element|ElementList], Stream) :-
	json_print_element(Element, Stream),
	( ElementList = [] ->
	  format(Stream, '', [])
	; format(Stream, ',', [])
	),
	json_print_list_elements(ElementList, Stream).
