-module(feldapd_config).

-include("feldapd_macros.hrl").
-include("feldapd_records.hrl").

-export([create/2]).

-define(DEFAULT_PORT, 389).
-define(DEFAULT_SOCKET_TYPE, ip_comm).
-define(DEFAULT_BIND_ADDRESS, any).
-define(DEFAULT_FD, undefined).

default_config() -> [	{port, ?DEFAULT_PORT},
						{socket_type, ?DEFAULT_SOCKET_TYPE},
						{bind_address, ?DEFAULT_BIND_ADDRESS},
						{fd, ?DEFAULT_FD}
					].

create(Loaded, ConfigFiles) ->
	merge(Loaded, lists:foldl(fun(ConfigFile, Config) -> merge(loadfile(ConfigFile), Config) end, default_config(), ConfigFiles)).
% create(Configs) ->
	% load(merge(Configs)).

% merge([]) ->
	% #r_FELDAPD_Config{};
% merge(Configs) ->
	% merge(Configs, []).
% merge([], MergedConfig) ->
	% MergedConfig;
% merge([ConfigHead | ConfigTail], MergedConfig) ->
	% merge(ConfigTail, merge_config(ConfigHead, MergedConfig)).

merge([], Config2) ->
	Config2;
merge([ ConfigHead = {Key, Value} | ConfigTail], Config2) ->
	case lists:keyfind(Key, 1, Config2) of
		{Key, Value} ->
			merge(ConfigTail, Config2);
		{Key, _} ->
			merge(ConfigTail, lists:keyreplace(Key, 1, Config2, ConfigHead));
		false ->
			merge(ConfigTail, [ConfigHead | Config2])
	end.

loadfile(FileName) when is_list(FileName) ->
	case file:consult(FileName) of
		{ok, Config} ->
			Config;
		{error, {Line, Mod, Term}} ->
			?hlri("Config file error in {Line: ~p, Module: ~p, Term: ~p}~n", [Line, Mod, Term]),
			[];
		{error, Reason} ->
			?hlri("Config file error: ~p~n", [Reason]),
			[]
	end.

% load(Tuples) ->
	% load(Tuples, #r_FELDAPD_Config{}).
% load(Tuples, Record) ->
	% Fields = record_info(fields, r_FELDAPD_Config),
	% load(Tuples, Record, Fields, 2).
% load(_Tuples, Config, [], _N) ->
	% Config;
% load([], Config, _Fields, _N) ->
	% Config;
% load(Tuples, Config, [Field | RestField], N) ->
	% case lists:keyfind(Field, 1, Tuples) of
		% {Field, Value} ->
			% load(lists:keydelete(Field, 1, Tuples), erlang:setelement(N, Config, Value), RestField, N+1);
		% false ->
			% load(Tuples, Config, RestField, N+1)
	% end.
