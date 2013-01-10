-module(feldapd_transport).
-behaviour(gen_server).

-include("feldapd_macros.hrl").
-include("feldapd_records.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/2, stop/1]).
-export([do_accept_loop/3, do_receive_loop/4]).


%%% ============================================================
%%% start server
%%% ============================================================
start_link(Server, Args) ->
	gen_server:start_link(Server, ?MODULE, Args, []).

%%% ============================================================
%%% stop server
%%% ============================================================
stop(Server) ->
	gen_server:call(Server, terminate).

%%% ============================================================
%%% initialize server
%%% ============================================================
init(Args) ->
	% ?hlrd("Args: ", Args),
	process_flag(trap_exit, true),
	State = #feldapdTransportState{	socket_type = SocketType, 
									bind_address = BindAddress,
									port = Port,
									fd = Fd } = init_state(Args),
	start_prereqs(SocketType),
	case listen(SocketType, BindAddress, Port, Fd) of
		{ok, ListenSocket} ->
			{ok, State#feldapdTransportState{listen_socket = ListenSocket}, 0};
		{error, Reason} ->
			{stop, Reason}
	end.
	

%%% ============================================================
%%% initialize server state
%%% ============================================================
init_state(Args) ->
	#feldapdTransportState{
		server_name = proplists:get_value(server_name, Args),
		socket_type = proplists:get_value(socket_type, Args),
		port = proplists:get_value(port, Args),
		bind_address = proplists:get_value(bind_address, Args),
		listen_socket = undefined,
		fd = proplists:get_value(fd, Args)
	}.


% accept_loop(Args) ->
	% proc_lib:spawn(?MODULE, do_accept_loop, [self(), Args]).

%%% ============================================================
%%% waiting for incoming connectiong
%%% ============================================================
% do_accept_loop(ServerRef, Args) ->
do_accept_loop(SocketType, ListenSocket, LDAPServer) ->
	?hlrt("do_accept_loop", LDAPServer),
	{ok, Socket} = accept(SocketType, ListenSocket),
	ok = negotiate(SocketType, Socket),
	proc_lib:spawn(?MODULE, do_receive_loop, [SocketType, Socket, LDAPServer, feldapd_server:session_start(LDAPServer)]),
	do_accept_loop(SocketType, ListenSocket, LDAPServer).
	% do_receive_loop(SocketType, Socket, LDAPServer, feldapd_server:session_start(LDAPServer))
	% ok.

%%% ============================================================
%%% receive messages
%%% ============================================================
do_receive_loop(SocketType, Socket, LDAPServer, SessionId) ->
	case recv(SocketType, Socket, 0) of
        {ok, Data} ->
			% ?hlrt("LDAP RawMessage", Data),
			case feldapd_server:message(LDAPServer, SessionId, Data) of
				{reply, Responses} ->
					send_responses(SocketType, Socket, Responses);
				{noreply} ->
					ok;
				Else ->
					?hlri("WTF", Else)
			end,
			do_receive_loop(SocketType, Socket, LDAPServer, SessionId);
        {error, closed} ->
			feldapd_server:session_destroy(LDAPServer, SessionId),
			?hlrt("Connection closed", "-"),
			% if 
				% DataDirty andalso DataFile =/= null ->
					% DataModule:save(TableId, DataFile),
					% feldapd_data_visualizer:visualize(TableId, DataModule, string:join([DataFile, "vis"], "."));
				% true ->
					% ok
			% end,
            ok;
		Else ->
			?hlri("WTF", Else),
			ok
    end.

%%% ============================================================
%%% send responses
%%% ============================================================
send_responses(_SocketType, _Socket, []) ->
	ok;
send_responses(SocketType, Socket, [Response | Responses]) ->
	send(SocketType, Socket, Response),
	send_responses(SocketType, Socket, Responses).
% do_ldap_responses(SocketType, Socket, LDAPServer) ->
	% LDAPServerPid = erlang:whereis(LDAPServer),
	% receive
		% {LDAPServerPid, eor} ->
			% ok;
		% {LDAPServerPid, Response} ->
			% send(SocketType, Socket, Response),
			% do_ldap_responses(SocketType, Socket, LDAPServer);
		% Else ->
			% ?hlrt("do_ldap_responses -> receive -> WTF", Else),
			% io:format(" >> ~p << ", [Else])
	% after 10000 ->
		% ?hlrt("do_ldap_responses -> Timeout", "-"),
		% ok
	% end.
	

handle_cast(Message, State) ->
	?hlrt("handle_cast", [Message, state]),
	% io:format("~p:handle_cast(~p, ~p)~n", [?MODULE, Message, State]),
	{noreply, State}.

handle_call(Message, From, State) -> 
	?hlrt("handle_call", [Message, From, state]),
	% io:format("~p:handle_call(~p, ~p, ~p)~n", [?MODULE, Message, From, State]),
	{reply, State, State}.


handle_info(timeout, State = #feldapdTransportState{server_name = ServerName, socket_type = SocketType, listen_socket = ListenSocket}) ->
	?hlrt("handle_info", [timeout, state]),
	{noreply, State#feldapdTransportState{accept_loop_pid = proc_lib:spawn(?MODULE, do_accept_loop, [SocketType, ListenSocket, ServerName])}};
	
handle_info(Info, State) -> 
	?hlrt("handle_info", [Info, state]),
	% io:format("~p:handle_info(~p, ~p)~n", [?MODULE, Info, State]),
	{noreply, State}.

terminate(Reason, _State = #feldapdTransportState{accept_loop_pid = AcceptLoopPid}) ->
	?hlrt("terminate", [Reason, state]),
	exit(AcceptLoopPid, normal),
	% io:format("~p:terminate(~p, ~p)~n", [?MODULE, Reason, State]),
	ok.

code_change(OldVersion, State, Extra) -> 
	?hlrt("code_change", [OldVersion, state, Extra]),
	% io:format("~p:code_change(~p, ~p, ~p)~n", [?MODULE, OldVersion, State, Extra]),
	{ok, State}.


%%%

%%% ==================================================
%%% Start ssl if essl
%%% ==================================================
start_prereqs({essl, _}) ->
	start_ssl();
start_prereqs(_SocketType) ->
	ok.

start_ssl() ->
	case ssl:start() of
		ok -> ok;
		{error, {already_started, _}} -> ok;
		Else -> Else
	end.

%%% ==================================================
%%% tcp listen
%%% ==================================================
listen({essl, SSLConfig}, Addr, Port, _Fd) ->
	listen_ssl(Addr, Port, [{ssl_imp, new}, {reuseaddr, true} | SSLConfig]);
listen(ip_comm, Addr, Port, Fd) ->
	listen_ip_comm(Addr, Port, Fd).

listen_ip_comm(Addr, Port, Fd) ->
	case (catch do_listen_ip_comm(Addr, Port, Fd)) of
	{'EXIT', Reason} ->
		{error, {exit, Reason}};
	Else ->
		Else
	end.

do_listen_ip_comm(Addr, Port, Fd) ->
	{NewPort, Opts, IpFamily} = get_socket_info(Addr, Port, Fd),
	case IpFamily of
		inet6fb4 -> 
			Opts2 = [binary | Opts], 
			?hlrt("try ipv6 listen", [{port, NewPort}, {opts, Opts2}]),
			case (catch gen_tcp:listen(NewPort, Opts2)) of
				{error, Reason} when ((Reason =:= nxdomain) orelse (Reason =:= eafnosupport)) ->
					Opts3 = [binary | Opts], 
					?hlrt("ipv6 listen failed - try ipv4 instead", [{reason, Reason}, {port, NewPort}, {opts, Opts3}]),
					gen_tcp:listen(NewPort, Opts3);
				%% This is when a given hostname has resolved to a 
				%% IPv4-address. The inet6-option together with a 
				%% {ip, IPv4} option results in badarg
				{'EXIT', Reason} -> 
					Opts3 = [binary | Opts], 
					?hlrt("ipv6 listen exit - try ipv4 instead", [{reason, Reason}, {port, NewPort}, {opts, Opts3}]),
					gen_tcp:listen(NewPort, Opts3); 
				Other ->
					?hlrt("ipv6 listen done", [{other, Other}]),
					Other
			end;
		_ ->
			Opts2 = [IpFamily | Opts],
			?hlrt("listen", [{port, NewPort}, {opts, Opts2}]),
			gen_tcp:listen(NewPort, Opts2)
	end.

listen_ssl(Addr, Port, Opts0) ->
	IpFamily = ipfamily_default(Addr, Port), 
	BaseOpts = [{backlog, 128}, {reuseaddr, true} | Opts0], 
	Opts	 = sock_opts(Addr, BaseOpts),
	case IpFamily of
		inet6fb4 -> 
			Opts2 = [binary | Opts], 
			?hlrt("try ipv6 listen", [{opts, Opts2}]),
			case (catch ssl:listen(Port, Opts2)) of
				{error, Reason} when ((Reason =:= nxdomain) orelse (Reason =:= eafnosupport)) ->
					Opts3 = [binary | Opts], 
					?hlrt("ipv6 listen failed - try ipv4 instead", [{reason, Reason}, {opts, Opts3}]),
					ssl:listen(Port, Opts3);
				{'EXIT', Reason} -> 
					Opts3 = [binary | Opts], 
					?hlrt("ipv6 listen exit - try ipv4 instead", [{reason, Reason}, {opts, Opts3}]),
					ssl:listen(Port, Opts3); 
				Other ->
					?hlrt("ipv6 listen done", [{other, Other}]),
					Other
			end;
		_ ->
			Opts2 = [IpFamily | Opts],
			?hlrt("listen", [{opts, Opts2}]),
			ssl:listen(Port, Opts2)
	end.

ipfamily_default(_Addr, _Port) ->
	binary.

get_socket_info(Addr, Port, Fd0) ->
	BaseOpts		= [{backlog, 128}, {reuseaddr, true}], 
	IpFamilyDefault = ipfamily_default(Addr, Port), 
	%% The presence of a file descriptor takes precedence
	case get_fd(Port, Fd0, IpFamilyDefault) of
		{Fd, IpFamily} -> 
			{0, sock_opts(Addr, [{fd, Fd} | BaseOpts]), IpFamily};
		undefined ->
			{Port, sock_opts(Addr, BaseOpts), IpFamilyDefault}
	end.
		
get_fd(Port, undefined = _Fd, IpFamilyDefault) ->
	FdKey = list_to_atom("feldapd_" ++ integer_to_list(Port)),
	case init:get_argument(FdKey) of
		{ok, [[Value]]} ->
			case string:tokens(Value, [$|]) of
				[FdStr, IpFamilyStr] ->
					{fd_of(FdStr), ip_family_of(IpFamilyStr)};
				[FdStr] ->
					{fd_of(FdStr), IpFamilyDefault};
				_ ->
					throw({error, {bad_descriptor, Value}})
			end;
		error ->
			undefined
	end;
get_fd(_Port, Fd, IpFamilyDefault) ->
	{Fd, IpFamilyDefault}.
	
fd_of(FdStr) ->
	case (catch list_to_integer(FdStr)) of
		Fd when is_integer(Fd) ->
			Fd;
		_ ->
			throw({error, {bad_descriptor, FdStr}})
	end.

ip_family_of(IpFamilyStr) ->
	IpFamily = list_to_atom(IpFamilyStr),
	case lists:member(IpFamily, [inet, inet6, inet6fb4]) of
		true ->
			IpFamily;
		false ->
			throw({error, {bad_ipfamily, IpFamilyStr}})
	end.

%% -- sock_opts --
%% Address any comes from directive: BindAddress "*"
sock_opts(undefined, Opts) -> 
	sock_opts(Opts);
sock_opts(any = Addr, Opts) -> 
	sock_opts([{ip, Addr} | Opts]);
sock_opts(Addr, Opts) ->
	sock_opts([{ip, Addr} | Opts]).

sock_opts(Opts) ->
	[{packet, 0}, {active, false} | Opts].
	% [{packet, raw}, {packet_size, 32}, {active, false} | Opts].


%%% ==================================================
%%% tcp accept
%%% ==================================================
accept(SocketType, ListenSocket) ->
    accept(SocketType, ListenSocket, infinity).

accept(ip_comm, ListenSocket, Timeout) ->
    gen_tcp:accept(ListenSocket, Timeout);

accept({essl, _SSLConfig}, ListenSocket, Timeout) ->
    ssl:transport_accept(ListenSocket, Timeout).


%% -- negotiate --
negotiate(SocketType, ListenSocket) ->
	negotiate(SocketType, ListenSocket, infinity).
	
negotiate(ip_comm,_,_) ->
    ?hlrt("negotiate(ip_comm)", []),
    ok;
negotiate({essl, _}, Socket, Timeout) ->
    ?hlrt("negotiate(essl)", []),
    negotiate_ssl(Socket, Timeout).

negotiate_ssl(Socket, Timeout) ->
    ?hlrt("negotiate_ssl", [{socket, Socket}, {timeout, Timeout}]),
    case ssl:ssl_accept(Socket, Timeout) of
	ok ->
	    ok;
	{error, Reason} ->
	    ?hlrd("negotiate_ssl - accept failed", [{reason, Reason}]),
	    %% Look for "valid" error reasons
	    ValidReasons = [timeout, econnreset, esslaccept, esslerrssl], 
	    case lists:member(Reason, ValidReasons) of
		true ->
		    {error, normal};
		false ->
		    {error, Reason}
           end
    end.


%%% ==================================================
%%% tcp recv
%%% ==================================================
recv(ip_comm, Socket, Length) ->
	gen_tcp:recv(Socket, Length);
recv({essl, SSLConfig}, Socket, Length) ->
	case ssl:recv(Socket, Length) of 
		{ok, Data} ->
			recv({essl, SSLConfig}, Socket, Length, [Data]);
		Else ->
			Else
	end.

recv({essl, SSLConfig}, Socket, Length, DataList) ->
	case ssl:recv(Socket, Length, 10) of
		{ok, Data} ->
			recv({essl, SSLConfig}, Socket, Length, [Data | DataList]);
		{error, timeout} ->
			{ok, erlang:list_to_binary(lists:flatten(lists:foldl(fun (D, T) -> [erlang:binary_to_list(D) | T] end, "", DataList)))};
		Else ->
			Else
	end.

%%% ==================================================
%%% tcp send
%%% ==================================================
send(ip_comm, Socket, Response) ->
	gen_tcp:send(Socket, Response);
send({essl, _SSLConfig}, Socket, Response) ->
	ssl:send(Socket, Response).
