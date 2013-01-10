-module(feldapd_server).
-behaviour(gen_server).

-include("feldapd_macros.hrl").
-include("feldapd_records.hrl").
-include("LDAP.hrl").

-define(TIMEOUT, 10000).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/2, stop/1]).
-export([session_start/1, session_destroy/2]).
-export([message/3]).

start_link(Server, Args) ->
	gen_server:start_link(Server, ?MODULE, Args, []).

stop(Server) ->
	gen_server:call(feldapd_name:get_call_name(Server), terminate).

%%% Session Start
session_start(Server) ->
	gen_server:call(feldapd_name:get_call_name(Server), {session_start}, ?TIMEOUT).
%%% Session Destroy
session_destroy(Server, SessionId) ->
	gen_server:call(feldapd_name:get_call_name(Server), {session_destroy, SessionId}, ?TIMEOUT).

message(Server, SessionId, Message) ->
	gen_server:call(feldapd_name:get_call_name(Server), {message, SessionId, Message}, ?TIMEOUT).



init(Args) ->
	process_flag(trap_exit, true),
	SchemaFiles = proplists:get_value(schema_files, Args, []),
	case feldapd_schema:load_schema(SchemaFiles) of	
		{error, Reason} ->
			?hlri("Schema Load Error", Reason),
			{stop, {schema_load_error, Reason}};
		Attributes ->
			?hlrt("Schema Load OK", ""),
			DataModule = proplists:get_value(data_module, Args),
			DataFile = proplists:get_value(data_file, Args),
			DataTableId = DataModule:load(DataFile),
			feldapd_funs:write_schema(DataTableId, DataModule, Attributes),
			case feldapd_schema:load_full_schemas(SchemaFiles) of
				{ok, Schema} ->
					feldapd_schema:store(DataTableId, DataModule, Schema),
					{ok, #feldapdState{
						data_ets_tid = DataTableId,
						data_module = DataModule,
						data_file = DataFile,
						root_username = proplists:get_value(root_username, Args),
						root_password = proplists:get_value(root_password, Args)
					}};
				Error ->
					?hlri("Schema Load Error", Error),
					{stop, {schema_load_error, Error}}
			end
	end.



ldap_response(
	#'LDAPMessage'{	
		messageID = _MessageID, 
		protocolOp = ProtocolOp, 
		controls = _Controls
	} = LDAPMessage, State = #feldapdState{}, SessionId) ->
	case ldap_protocol_response(ProtocolOp, State, SessionId) of
		{ok, NewState, Responses} ->
			?hlrt(" new ServerState", NewState),
			{ok, NewState, ldap_response_prepare(LDAPMessage, Responses)};
		{error, State, Reason} ->
			?hlri(" === feldapd:ldap_response - ERROR === ", Reason),
			{error, Reason};
		WTF ->
			?hlri(" === feldapd:ldap_response - WTF === ", WTF)
	end.

% prepare LDAP response
ldap_response_prepare(_LDAPMessage, []) -> 
	[];
ldap_response_prepare(LDAPMessage, [Head | Tail]) ->
	case 'LDAP':encode('LDAPMessage', LDAPMessage#'LDAPMessage'{protocolOp = Head}) of
		{ok, LDAPResponse} ->
			[ LDAPResponse | ldap_response_prepare(LDAPMessage, Tail) ];
		Else ->
			?hlri("'LDAP':encode error", Else),
			ldap_response_prepare(LDAPMessage, Tail)
	end.

% bind request
ldap_protocol_response({bindRequest, Request}, State = #feldapdState{}, SessionId) ->
	{ok, NewState, Response} = feldapd_funs:bind(Request, State, SessionId),
	{ok, NewState, [{bindResponse, Response}]};
% unbind request
ldap_protocol_response({unbindRequest, _Request}, State = #feldapdState{}, SessionId) ->
	{ok, NewState} = feldapd_funs:unbind(State, SessionId),
	{ok, NewState, []};
% search request
ldap_protocol_response({searchRequest, Request}, State = #feldapdState{}, SessionId) ->
	{ok, NewState, Responses, FinalResponse} = feldapd_funs:search(Request, State, SessionId),
	{ok, NewState, lists:reverse([ {searchResDone, FinalResponse} | lists:map( fun (Response) -> {searchResEntry, Response} end, Responses) ])};
% add request
ldap_protocol_response({addRequest, Request}, State = #feldapdState{}, SessionId) ->
	{ok, NewState, Response} = feldapd_funs:add(Request, State, SessionId),
	{ok, NewState, [{addResponse, Response}]};
% modify request
ldap_protocol_response({modifyRequest, Request}, State = #feldapdState{}, SessionId) ->
	{ok, NewState, Response} = feldapd_funs:modify(Request, State, SessionId),
	{ok, NewState, [{modifyResponse, Response}]};
% delete request
ldap_protocol_response({delRequest, Request}, State = #feldapdState{}, SessionId) ->
	{ok, NewState, Response} = feldapd_funs:delete(Request, State, SessionId),
	{ok, NewState, [{delResponse, Response}]};
% unknown request
ldap_protocol_response(Request, State, SessionId) ->
	?hlri("??? ldap_protocol_response", [Request, State, SessionId]),
	{error, State, {unknown_request, Request}}.



%%%
%%% gen_server functions
%%%

handle_cast(Message, State) ->
	?hlri("??? handle_cast", [Message, State]),
	{noreply, State}.

handle_call({session_start}, _From, State = #feldapdState{next_session_id = SessionId, sessions = Sessions}) ->
	{reply, SessionId, State#feldapdState{next_session_id = SessionId+1, sessions = [#feldapdSession{id = SessionId} | Sessions]}};

handle_call({session_destroy, SessionId}, _From, State = #feldapdState{sessions = Sessions}) ->
	{reply, SessionId, State#feldapdState{sessions = [Session || Session = #feldapdSession{id = CurrSessionId} <- Sessions, SessionId =/= CurrSessionId]}};

handle_call({message, SessionId, Message}, _From, State) ->
	% ?hlrt("handle_call({message...})", [Message, From, State]),
	case 'LDAP':decode('LDAPMessage', Message) of
		{ok, LDAPMessage} ->
			?hlrt("'LDAP':decoded", LDAPMessage),
			case ldap_response(LDAPMessage, State, SessionId) of
				{ok, NewState, Response} ->
					{reply, {reply, Response}, NewState};
				_Else ->
					{reply, {noreply}, State}
			end;
		Else ->
			?hlri("'LDAP':decode error", Else),
			{reply, {noreply}, State}
	end;
	
handle_call(Message, From, State) -> 
	?hlri("??? handle_call", [Message, From, State]),
	{reply, State, State}.

handle_info(Info, State) -> 
	?hlri("??? handle_info", [Info, State]),
	{noreply, State}.

terminate(Reason, State = #feldapdState{data_dirty = DataDirty, data_file = DataFile, data_module = DataModule, data_ets_tid = TableId}) ->
	?hlrt("feldapd_server:terminate", [Reason, State]),
	if DataDirty andalso DataFile =/= null ->
			DataModule:save(TableId, DataFile),
			feldapd_data_visualizer:visualize(TableId, DataModule, string:join([DataFile, "vis"], ".")),
			ok;
		true ->
			ok
	end.

code_change(OldVersion, State, Extra) -> 
	?hlrt("code_change", [OldVersion, State, Extra]),
	{ok, State}.
