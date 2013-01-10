-module(feldapd_data_visualizer).

-include("feldapd_records.hrl").
-include("LDAP.hrl").

-export([visualize/3]).


visualize(Tab, DataModule, FileName) ->
	case file:open(FileName, [write]) of
		{ok, File} ->
			v(Tab, File, DataModule),
			file:close(File);
		{error, Reason} ->
			io:format("File [~p] open error: ~p~n", [FileName, Reason])
	end
.
			

v(Tab, File, DataModule) ->
	v(Tab, File, DataModule, "", 0).

v(Tab, File, DataModule, NodeKey, Level) ->
	case DataModule:read(Tab, NodeKey) of
		false -> % node not found
			w_missing(File, NodeKey, Level);
		#r_FELDAPD_Node{ children = Children, attributes = Attributes } -> % node found
			w_node(File, NodeKey, Attributes, Level),
			lists:foreach(
						fun(ChildKey) -> v(Tab, File, DataModule, feldapd_help:key_join(ChildKey, NodeKey), Level+1) end, 
						Children );
		WTF ->
			io:format(" === feldapd_data_visualizer:v WTF === ~p~n", [WTF])
	end
.

s_tabs(Count) ->
	string:copies("\t", Count).

w_missing(File, NodeKey, Level) ->
	% io:format("~s~s [MISSING]~n", [s_tabs(Level), NodeKey]),
	io:format(File, "~n~s# '~s' [MISSING]~n", [s_tabs(Level), NodeKey]).

w_node(File, NodeKey, Attributes, Level) ->
	Tabs = s_tabs(Level),
	% io:format("~s~s~n~s", [Tabs, NodeKey, s_attributes(Attributes, Tabs)]),
	io:format(File, "~n~s# '~s'~n~s", [Tabs, NodeKey, s_attributes(Attributes, Tabs)]).


s_attributes(Attributes, Tabs) ->
	s_attributes(Attributes, Tabs, []).
s_attributes([], _Tabs, Out) ->
	Out;
s_attributes([#'PartialAttribute'{type = Type, vals = Vals} | Attributes], Tabs, Out) ->
	S_Vals = string:concat("['", string:concat(string:join(Vals, "', '"), "']")),
	S = string:concat(string:join([Type, S_Vals], ":\t"), "\n"),
	NewOut = string:concat(Out, string:join([Tabs, S], "| - ")),
	s_attributes(Attributes, Tabs, NewOut).

