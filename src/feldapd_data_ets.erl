-module(feldapd_data_ets).

-include("feldapd_records.hrl").
-include("LDAP.hrl").

% read, write, load, save, initialize és search

-export([save/2, load/1]).
-export([initialize/0, read/2, write/3, delete/2]).


%% create a new empty ets table
initialize() ->
	ets:new(?MODULE, [ordered_set, private]).

normalize_key(Key) ->
	lists:filter(fun (32) -> false; (_) -> true end, Key).

%% find Tuple = {Key, _} in ets table
read(Tab, XKey) ->
	Key = normalize_key(XKey),
	case ets:lookup(Tab, Key) of
		[{Key, Value} | _] ->
			Value;
		_ -> 
			false
	end.

%% insert {Key, Value} to ets table
write(Tab, XKey, Value) ->
	Key = normalize_key(XKey),
	ets:insert(Tab, {Key, Value}).
% write(_Tab, _Key, Value) ->
	% {error, {not_Node, Value}}.


delete(Tab, XKey) ->
	Key = normalize_key(XKey),
	ets:delete(Tab, Key).

%% save ets table to file
save(Tab, FileName) ->
	case ets:tab2file(Tab, FileName) of
		{error, Reason} -> 
			io:format("File save error: ~p~n", [Reason]),
			{error, Reason};
		ok -> 
			ok
	end.

%% load ets table from file
load(null) ->
	initialize();
load(FileName) ->
	case ets:file2tab(FileName) of
		{ok, Tab} ->
			Tab;
		{error, Reason} ->
			io:format("File load error: ~p~n", [Reason]),
			initialize()
	end.
