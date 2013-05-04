-module(feldapd_funs).

-export([check_root/2, get_root/2, save_root/3, write_schema/3]).
-export([bind/3, add/3, search/3, modify/3, delete/3]).
-export([unbind/2, is_binded/2]).

-include("LDAP.hrl").
-include("feldapd_records.hrl").
-include("feldapd_macros.hrl").


%% ======================================================================
%% < node root functions >
%% ======================================================================

check_root(Tab, DataModule) ->
	case DataModule:read(Tab, "") of
		false ->
			DataModule:write(Tab, "", #r_FELDAPD_Node{}),
			false;
		_ -> 
			true
	end.

get_root(Tab, DataModule) ->
	case DataModule:read(Tab, "") of
		false ->
			DataModule:write(Tab, "", RootNode = #r_FELDAPD_Node{}),
			RootNode;
		RootNode -> 
			RootNode
	end.

save_root(Tab, DataModule, RootNode = #r_FELDAPD_Node{}) ->
	DataModule:write(Tab, "", RootNode).

%% ======================================================================
%% </ node root functions >
%% ======================================================================


%% ======================================================================
%% < schema functions >
%% ======================================================================

write_schema(Tab, DataModule, SchemaAtributes) -> 
	SchemaKey = "cn=schema",
	% save_root(Tab, DataModule, feldapd_node:child_add(SchemaKey, get_root(Tab, DataModule))),
	DataModule:write(Tab, SchemaKey, feldapd_node:attribute_add(#r_FELDAPD_Node{baseObject = SchemaKey, attributes = SchemaAtributes}, #'PartialAttribute'{type = "objectClass", vals = ["top"]})).

%% ======================================================================
%% </ schema functions >
%% ======================================================================


%% ======================================================================
%% < LDAP functions >
%% ======================================================================

bind_session(Sessions, SessionId, Bind) ->
	lists:map(fun (Session=#feldapdSession{id = CurrSessionId}) ->  
					case CurrSessionId =:= SessionId of
						true -> Session#feldapdSession{binded = Bind};
						false -> Session
					end
				end, Sessions).

%% LDAP Bind request - to root
bind( _Request = #'BindRequest'{ name = [] },  State=#feldapdState{sessions = Sessions}, SessionId ) ->
	{ ok, State#feldapdState{sessions = bind_session(Sessions, SessionId, "")}, feldapd_resp:bind(success, "", "binded to root success") };

%% LDAP Bind - to config root user
bind(	_Request = #'BindRequest'{	version = _Version,
									name = Name, 
									authentication = {simple, Password} }, 
		State = #feldapdState{	root_username = Name,
								root_password = Password,
								sessions = Sessions },
		SessionId ) ->
		{ ok, State#feldapdState{sessions = bind_session(Sessions, SessionId, Name)}, feldapd_resp:bind(success, Name, "binded success") };
%% LDAP Bind - to somewhere
bind(	_Request = #'BindRequest'{	version = _Version,
									name = Name, 
									authentication = {simple, Password} }, 
		State = #feldapdState{	data_module = DataModule,
								data_ets_tid = Tab,
								sessions = Sessions },
		SessionId ) ->
	case DataModule:read(Tab, Name) of
		#r_FELDAPD_Node{ attributes = Attributes } ->
			case lists:keyfind("userPassword", #'PartialAttribute'.type, Attributes) of
				#'PartialAttribute'{type = "userPassword", vals = AttributeValues} ->
					case lists:member(Password, AttributeValues) of
						true ->
							{ ok, State#feldapdState{sessions = bind_session(Sessions, SessionId, Name)}, feldapd_resp:bind(success, Name, "binded success") };
						false ->
							{ ok, State, feldapd_resp:bind(operationsError, "", "bad password") }
					end;
				false ->
					{ ok, State, feldapd_resp:bind(operationsError, "", "no password for user") }
			end;
		false ->
			{ ok, State, feldapd_resp:bind(operationsError, "", "no user") }
	end.

%% LDAP UnBind - bind to root
unbind(State = #feldapdState{sessions = Sessions}, SessionId) ->
	{ok, State#feldapdState{sessions = bind_session(Sessions, SessionId, "")}}.


is_binded(_State = #feldapdState{sessions = Sessions}, SessionId) ->
	lists:foldl(fun(#feldapdSession{binded = Binded, id = CurrSessionId}, IsBinded) -> 
					case CurrSessionId =:= SessionId andalso Binded =/= "" of
						true -> true;
						_ -> IsBinded
					end
				end, false, Sessions).


%% LDAP add function
add(	_Request = #'AddRequest'{	entry = Entry,
									attributes = Attributes }, 
		State = #feldapdState{	data_module = DataModule,
								data_ets_tid = Tab},
		SessionId) ->
	case is_binded(State, SessionId) of
		false ->
			{ ok, State, feldapd_resp:result(strongerAuthRequired, "", "Permission denied") };
		true ->
			case DataModule:read(Tab, Entry) of
				false -> % az entry nincs meg
					{Me, ParentRoute} = feldapd_route:get_me_parent(Entry),
					case DataModule:read(Tab, ParentRoute) of
						false -> % nincs parent
							{ ok, State, feldapd_resp:result(operationsError, Entry, string:concat("No Parent: ", ParentRoute)) };
						ParentNode = #r_FELDAPD_Node{children = ParentNode_Children} -> % van parent
							NewNode = feldapd_node:create(Entry, Attributes),
							case feldapd_schema:check_node(NewNode, feldapd_schema:get(Tab, DataModule)) of
								ok ->
									DataModule:write(Tab, ParentRoute, ParentNode#r_FELDAPD_Node{children = [Me | ParentNode_Children]}),
									DataModule:write(Tab, Entry, NewNode),
									{ ok, State#feldapdState{data_dirty = true}, feldapd_resp:result(success, ParentRoute, "Add Success") };
								_Else ->
									{ ok, State, feldapd_resp:result(operationsError, Entry, "Invalid node (Schema error)") }
							end;
						WTF ->
							?hlri(" === feldapd_funs:add - WTF 2 === ", WTF)
					end;
				_Node -> % mar letezik amit hozza akarok adni
					{ ok, State, feldapd_resp:result(operationsError, Entry, string:concat("Exists Entry: ", Entry)) }
				% WTF ->
					% ?hlri(" === feldapd_funs:add - WTF 1 === ", WTF)
			end
	end.


%% LDAP modify function
modify(	_Request = #'ModifyRequest'{object = Object, changes = Changes}, 
		State = #feldapdState{	data_module = DataModule,
								data_ets_tid = Tab},
		SessionId) ->
	case is_binded(State, SessionId) of
		false ->
			{ ok, State, feldapd_resp:result(strongerAuthRequired, "", "Permission denied") };
		true ->
			case DataModule:read(Tab, Object) of
				false ->
					% no entry to modify
					?hlrv("object not found", Object),
					{ok, State, feldapd_resp:result(operationsError, "", "object not found")};
				Node = #r_FELDAPD_Node{} ->
					NewNode = lists:foldl(fun feldapd_node:change/2, Node, Changes),
					DataModule:write(Tab, Object, NewNode),
					{ok, State#feldapdState{data_dirty = true}, feldapd_resp:result(success, "", "") }
			end
	end.

%% LDAP delete function
delete("", State, _SessionId) ->
	{ok, State, feldapd_resp:result(operationsError, "", "root is not deletable")};
delete(	Request, 
		State = #feldapdState{	data_module = DataModule,
								data_ets_tid = Tab },
		SessionId) ->
	case is_binded(State, SessionId) of
		false ->
			{ ok, State, feldapd_resp:result(strongerAuthRequired, "", "Permission denied") };
		true ->
			case DataModule:read(Tab, Request) of
				false ->
					% no entry to delete
					?hlri("object not found", Request),
					{ok, State, feldapd_resp:result(operationsError, "", "object not found")};
				_Node = #r_FELDAPD_Node{} ->
					delete_recursive(Request, Tab, DataModule),
					{Me, ParentObject} = feldapd_route:get_me_parent(Request),
					case DataModule:read(Tab, ParentObject) of
						false ->
							?hlri("parent object not found", Request),
							{ok, State#feldapdState{data_dirty = true}, feldapd_resp:result(operationsError, "", "parent object not found")};
						ParentNode = #r_FELDAPD_Node{} ->
							DataModule:write(Tab, ParentObject, feldapd_node:child_delete(Me, ParentNode)),
							{ok, State#feldapdState{data_dirty = true}, feldapd_resp:result(success, "", "")}
					end
			end
	end.

delete_recursive(Object, Tab, DataModule) ->
	case DataModule:read(Tab, Object) of
		#r_FELDAPD_Node{children = Children} ->
			lists:foreach(fun (Child) -> delete_recursive(feldapd_help:key_join(Child, Object), Tab, DataModule) end, Children)
	end,
	DataModule:delete(Tab, Object).


%% LDAP search function
search(	Request = #'SearchRequest'{	baseObject = BaseObject,
									scope = _Scope, 
									derefAliases = _DerefAliases, 
									sizeLimit = _SizeLimit, 
									timeLimit = _TimeLimit, 
									typesOnly = _TypesOnly, 
									filter = _Filter,
									attributes = _Attributes }, 
		State = #feldapdState{	data_module = DataModule,
								data_ets_tid = Tab },
		_SessionId) ->
	case DataModule:read(Tab, BaseObject) of
		false -> 
			{ok, State, [], feldapd_resp:result(operationsError, "", "baseObject not found")};
		BaseNode = #r_FELDAPD_Node{} ->
			Nodes = search_nodes(Tab, DataModule, Request, [], [BaseNode], 0, erlang:now()),
			Responses = lists:map(fun (Node) -> feldapd_resp:search(Node) end, Nodes),
			{ok, State, Responses, feldapd_resp:result(success)}
	end;
search(Request, _State = #feldapdState{}, _SessionId) ->
	?hlri(" === feldapd_funs:search - WTF Request === ", Request).

%% ======================================================================
%% </ LDAP functions >
%% ======================================================================



%% ======================================================================
%% < node search functions >
%% ======================================================================

% #'SearchRequest'{	
	% baseObject = BaseObject,
	% scope = Scope, 
	% derefAliases = DerefAliases, 
	% sizeLimit = SizeLimit, 
	% timeLimit = TimeLimit, 
	% typesOnly = TypesOnly, 
	% filter = Filter,
	% attributes = Attributes }, 

% nothing to check
search_nodes(_Tab, _DataModule, _Request = #'SearchRequest'{}, FoundNodes, [], _Count, _StartTime ) ->
	FoundNodes;
% sizeLimit reached
search_nodes(_Tab, _DataModule, _Request = #'SearchRequest'{sizeLimit = SizeLimit}, FoundNodes, _CheckNodes, Count, _StartTime ) when SizeLimit > 0 andalso Count >= SizeLimit ->
	FoundNodes;
% scope = baseObject - ellenorzom a dolgokat, de nem teszek be semmi ujat
search_nodes(Tab, DataModule, Request = #'SearchRequest'{	baseObject = _BaseObject,
															scope = baseObject,
															derefAliases = DerefAliases, 
															filter = Filter,
															attributes = Attributes 
															}, FoundNodes, [CheckNode | CheckNodes], Count, StartTime ) ->
	case search_node__filter(CheckNode, Filter) of
		true ->
			search_nodes(Tab, DataModule, Request, [ search_node__deref_aliases( search_node__select(CheckNode, Attributes), DerefAliases) | FoundNodes ], CheckNodes, Count+1, StartTime);
		false ->
			search_nodes(Tab, DataModule, Request, FoundNodes, CheckNodes, Count, StartTime)
	end;

% scope = singleLevel -	kiszedem az ellenorizni valot, es helyette a gyerekeit teszem be, majd atvaltok scope=baseObject mukodesre, hogy azok gyerekei mar ne legyenek benne
% 						ebben az esetben az ellenorizni valo Node-ok szama csak 1 lehet, es a Count is meg 0
search_nodes(Tab, DataModule, Request = #'SearchRequest'{scope = singleLevel}, FoundNodes = [], [CheckNode], Count = 0, StartTime ) ->
	search_nodes(Tab, DataModule, Request#'SearchRequest'{scope = baseObject}, FoundNodes, feldapd_node:get_child_nodes(CheckNode, Tab, DataModule), Count, StartTime);

% scope = wholeSubTree
search_nodes(Tab, DataModule, Request = #'SearchRequest'{	baseObject = _BaseObject,
															scope = wholeSubtree,
															derefAliases = DerefAliases, 
															filter = Filter,
															attributes = Attributes
															}, FoundNodes, [CheckNode | CheckNodes], Count, StartTime ) ->
	ChildNodes = feldapd_node:get_child_nodes(CheckNode, Tab, DataModule),
	case search_node__filter(CheckNode, Filter) of
		true ->
			search_nodes(Tab, DataModule, Request, [ search_node__deref_aliases( search_node__select(CheckNode, Attributes), DerefAliases) | FoundNodes ], CheckNodes ++ ChildNodes, Count+1, StartTime);
		false ->
			search_nodes(Tab, DataModule, Request, FoundNodes, CheckNodes ++ ChildNodes, Count, StartTime)
	end.

% WTF
% search_nodes(_Tab, _DataModule, Request, FoundNodes, CheckNodes, _Count, _StartTime ) ->
	% io:format(" === feldapd_funs:search_nodes WTF === ~nRequest: ~p~nFoundNodes:~p~nCheckNodes:~p~n === === === ~n", [Request, FoundNodes, CheckNodes]).


%%%%%%%%%%
%%% Search Node Filter functions 
%%%%%%%%%%

%% node filter empty attributes
% search_node__filter( _Node = #r_FELDAPD_Node{attributes = []}, _Filter) ->
	% false;

%% node filter not
search_node__filter(	 Node = #r_FELDAPD_Node{}, 
						_Filter = {'not', Filter} ) ->
	not search_node__filter(Node, Filter);

%% node filter and
search_node__filter(	_Node = #r_FELDAPD_Node{}, 
						_Filter = {'and', []} ) ->
	true;
search_node__filter(	 Node = #r_FELDAPD_Node{}, 
						_Filter = {'and', [FilterHead | FilterTail]} ) ->
	case search_node__filter(Node, FilterHead) of
		true ->
			search_node__filter(Node, {'and', FilterTail});
		false ->
			false
	end;

%% node filter or
search_node__filter(	_Node = #r_FELDAPD_Node{}, 
						_Filter = {'or', []} ) ->
	false;
search_node__filter(	 Node = #r_FELDAPD_Node{}, 
						_Filter = {'or', [FilterHead | FilterTail]} ) ->
	case search_node__filter(Node, FilterHead) of
		true ->
			true;
		false ->
			search_node__filter(Node, {'or', FilterTail})
	end;

%% node filter equalityMatch
search_node__filter(	_Node = #r_FELDAPD_Node{ attributes = NodeAttributes }, 
						_Filter = {equalityMatch, #'AttributeValueAssertion'{attributeDesc = Attribute, assertionValue = Value}} ) ->
	search_node__filter_equalityMatch(NodeAttributes, Attribute, Value);

%% node filter present
search_node__filter(	_Node = #r_FELDAPD_Node{ attributes = NodeAttributes }, 
						_Filter = {present, Attribute}) ->
	search_node__filter_present(NodeAttributes, Attribute);

%% node filter unknown - do not apply
search_node__filter(Node, Filter) ->
	io:format(" <===== ### Unknown filter: ~p ~n =====> ~n", [Filter]),
	Node.

%%%%%%%%%%
%%% custom filters
%%%%%%%%%%

%% filter equalityMatch
search_node__filter_equalityMatch(Attributes, Attribute, Value) ->
	case lists:keyfind(string:to_lower(Attribute), #'PartialAttribute'.type, feldapd_help:attribute_key_to_lower(Attributes)) of
		% ha egy ertek van, es az pont az
		#'PartialAttribute'{vals = [Value]} ->
			true;
		% tobb ertek eseten megnezzuk, hogy ez megvan-e
		#'PartialAttribute'{vals = Values} ->
			lists:member(Value, Values);
		% kulonben
		_Else ->
			false
	end.

%% filter present
search_node__filter_present(Attributes, Attribute) ->
	lists:keymember(string:to_lower(Attribute), #'PartialAttribute'.type, feldapd_help:attribute_key_to_lower(Attributes)).


search_node__select(Node = #r_FELDAPD_Node{}, []) ->
	Node;
search_node__select(Node = #r_FELDAPD_Node{attributes = Attributes}, SelectAttributes) ->
	LowerSelectAttributes = feldapd_help:strlist_to_lower(SelectAttributes),
	Node#r_FELDAPD_Node{attributes = lists:filter(fun (_Elem = #'PartialAttribute'{type = ElemType}) -> lists:member(string:to_lower(ElemType), LowerSelectAttributes) end, Attributes)}.


%% deref aliases - ez egyelore nem tudom mit jelent
search_node__deref_aliases(Node, neverDerefAliases) ->
	Node;
search_node__deref_aliases(Node, derefAlways) ->
	Node;
search_node__deref_aliases(Node, derefInSearching) ->
	Node;
search_node__deref_aliases(Node, derefFindingBaseObj) ->
	Node;
search_node__deref_aliases(Node, DerefAliases) ->
	io:format("Unknown DerefAliases: ~p~n", DerefAliases),
	Node.


%% ======================================================================
%% </ node search functions >
%% ======================================================================


