-module(feldapd_trace).

-export([report_event/5]).

-define(DEBUG_LEVEL, 25).

report_event(Severity, Label, Module, Line, Content) when Severity =< ?DEBUG_LEVEL ->
	% io:format("!~p > [~p:~p]\t- ~s~n", [Severity, Module, Line, Label]).
	io:format("!~p > [~p:~p]\t- ~s\t~p~n", [Severity, Module, Line, Label, Content]);
report_event(_Severity, _Label, _Module, _Line, _Content) ->
	ok.
