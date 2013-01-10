%%% ======================================================================
%%% Author: Ferenc Böröczki <ferenc.boroczki@gmail.com>
%%% Description: Erlang LDAP Deamon
%%% ======================================================================
-module(feldapd_supervisor).
-behaviour(supervisor).

-include("feldapd_macros.hrl").

% -export([start_link/0, start_link/1, start_link/2, init/1]).
-export([start_link/2, init/1]).
-export([stop/1]).

start_link(Args, ConfigFiles) ->
	Config = feldapd_config:create(Args, ConfigFiles),
	SupName = proplists:get_value(sup_name, Config, feldapd_name:create()),
	CorrectConfig = correct_name(Config, SupName),
	?hlrv("Starting supervisor: ", SupName),
	supervisor:start_link(SupName, ?MODULE, CorrectConfig).

stop(Pid) ->
	exit(Pid, shutdown).

init(Args) ->
	ServerName = proplists:get_value(server_name, Args),
	ServerArgs = filter_args(Args, [schema_files, data_ets_tid, data_module, data_file, root_username, root_password]),
	TransportName = proplists:get_value(transport_name, Args),
	TransportArgs = filter_args(Args, [server_name, port, socket_type, bind_address]),
	{ok, {{one_for_one, 3, 10}, [
		{feldapd_server_proc, {feldapd_server, start_link, [ServerName, ServerArgs]}, permanent, 10000, worker, [feldapd_server]},
		{feldapd_transport_proc, {feldapd_transport, start_link, [TransportName, TransportArgs]}, permanent, 10000, worker, [feldapd_transport]}
		% {feldapd_server_proc, {gen_server, start_link, [{local, proplists:get_value(server_name, Args)}, feldapd_server, Args, []]}, permanent, 10000, worker, [feldapd_server]},
		% {feldapd_transport_proc, {gen_server, start_link, [{local, proplists:get_value(transport_name, Args)}, feldapd_transport, Args, []]}, permanent, 10000, worker, [feldapd_transport]}
	]}}.


filter_args(Args, Filter) ->
	[Arg || {ArgName, _} = Arg <- Args, lists:member(ArgName, Filter)].

correct_name(Config, SupName) ->
	case proplists:is_defined(sup_name, Config) of
	false	->	correct_name([ {sup_name, SupName} | Config ], SupName);
	true	->	case proplists:is_defined(server_name, Config) of
				false	->	correct_name([ {server_name, feldapd_name:get_name(SupName, "server")} | Config ], SupName);
				true	->	case proplists:is_defined(transport_name, Config) of
							false	->	correct_name([ {transport_name, feldapd_name:get_name(SupName, "transport")} | Config ], SupName);
							true	->	Config
							end
				end
	end.

