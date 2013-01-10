-module(feldapd_node).

-include("feldapd_records.hrl").
-include("LDAP.hrl").

-export([create/2, get_child_nodes/3]).
-export([child_add/2, child_exists/2, child_delete/2]).
-export([attribute_add/2, change/2]).
-export([get_attribute/2]).

%%%%%%%%%%
%%% Node functions
%%%%%%%%%%

create(BaseObject, Attributes) ->
	#r_FELDAPD_Node{ baseObject = BaseObject, attributes = Attributes }.

get_child_nodes(_Node = #r_FELDAPD_Node{baseObject = BaseObject, children = Children}, Tab, DataModule) ->
	lists:foldl(fun (ChildKey, ChildNodes) -> 
					case DataModule:read(Tab, feldapd_help:key_join(ChildKey, BaseObject)) of
						false ->
							ChildNodes;
						ChildNode = #r_FELDAPD_Node{} ->
							[ChildNode | ChildNodes]
					end
				end, [], Children ).

get_attribute(Type, _Node = #r_FELDAPD_Node{attributes = Attributes}) ->
	attr_find_ci(Type, Attributes).
	
%%%%%%%%%%
%%% Child functions
%%%%%%%%%%

%% add child
child_add(Child, Node = #r_FELDAPD_Node{children = Children}) ->
	case child_exists(Child, Node) of
		true ->
			Node;
		false ->
			Node#r_FELDAPD_Node{children = [ Child | Children ]}
	end.

%% check child exists
child_exists(Child, #r_FELDAPD_Node{children = Children}) ->
	lists:member(Child, Children).

%% delete child
child_delete(Child, Node = #r_FELDAPD_Node{children = Children}) ->
	Node#r_FELDAPD_Node{children = lists:delete(Child, Children)}.


%%%%%%%%%%
%%% Attribute functions
%%%%%%%%%%

%% add attribute to node (merge values if exists)
attribute_add(	Node = #r_FELDAPD_Node{attributes = NodeAttributes}, 
				Attribute = #'PartialAttribute'{type = AttributeType, vals = AttributeVals}) ->
	case attr_find_ci(AttributeType, NodeAttributes) of
		{ok, #'PartialAttribute'{type = NodeAttributeType, vals = NodeAttributeVals}} ->
			% attribute exists
			Node#r_FELDAPD_Node{attributes = [#'PartialAttribute'{type = AttributeType, vals = lists_merge_uniq(NodeAttributeVals, AttributeVals)} | attr_delete_cs(NodeAttributeType, NodeAttributes)]};
		false ->
			% no this attribute yet -> add
			Node#r_FELDAPD_Node{attributes = [Attribute | NodeAttributes]}
	end.

%% modify attribute in node (replace if exists)
% foldl forditva adja mint en szeretnem
change(	Change = #'ModifyRequest_changes_SEQOF'{}, Node = #r_FELDAPD_Node{} ) ->
	change(Node, Change);
% add attribute
change(	Node = #r_FELDAPD_Node{attributes = Attributes}, 
		_Change = #'ModifyRequest_changes_SEQOF'{operation = add, modification = Attribute = #'PartialAttribute'{}} ) ->
	Node#r_FELDAPD_Node{attributes = attr_add(Attribute, Attributes)};

change(	Node = #r_FELDAPD_Node{attributes = Attributes}, 
		_Change = #'ModifyRequest_changes_SEQOF'{operation = delete, modification = #'PartialAttribute'{type = AttributeType}} ) ->
	Node#r_FELDAPD_Node{attributes = attr_delete_ci(AttributeType, Attributes)};

change(	Node = #r_FELDAPD_Node{attributes = Attributes}, 
		_Change = #'ModifyRequest_changes_SEQOF'{operation = replace, modification = Attribute = #'PartialAttribute'{}} ) ->
	Node#r_FELDAPD_Node{attributes = attr_replace_ci(Attribute, Attributes)};

change(Node, Change) ->
	io:format("Change operator is not implemented yet: ~p~n", [Change]),
	% io:format(" === feldapd_node:change(Node, Change) -> WTF? === ~nNode: ~p~nChange: ~p~n", [Node, Change]),
	Node.


%% find attribute in attribute list (case insensitive)
attr_find_ci(_Type, []) ->
	false;
attr_find_ci(Type, [Attribute = #'PartialAttribute'{type = AttributeType} | Attributes]) ->
	case string:to_lower(AttributeType) =:= string:to_lower(Type) of
		true ->
			{ok, Attribute};
		false ->
			attr_find_ci(Type, Attributes)
	end.

%% delete type from attribute (case sensitive)
attr_delete_cs(Type, Attributes) ->
	lists:keydelete(Type, #'PartialAttribute'.type, Attributes).

%% delete type from attribute (case insensitive)
attr_delete_ci(Type, Attributes) ->
	case attr_find_ci(Type, Attributes) of
		{ok, #'PartialAttribute'{type = CSType}} ->
			attr_delete_cs(CSType, Attributes);
		false ->
			Attributes
	end.


%% add Attribute to Attributes (if not exists)
attr_add(NewAttribute = #'PartialAttribute'{type = NewType}, Attributes) ->
	case attr_find_ci(NewType, Attributes) of
		{ok, _} ->
			Attributes;
		false ->
			[NewAttribute | Attributes]
	end.

%% replace Attribute in Attributes (add if not exists) (case insensitive)
attr_replace_ci(Attribute = #'PartialAttribute'{type = Type}, Attributes) ->
	attr_add(Attribute, attr_delete_ci(Type, Attributes)).


%% merge List2 to List1 to uniq
lists_merge_uniq(List1, []) ->
	List1;
lists_merge_uniq(List1, [Item | List2]) ->
	case lists:member(Item, List1) of
		true -> 
			lists_merge_uniq(List1, List2);
		false -> 
			lists_merge_uniq([Item | List1], List2)
	end.


% attributes_to_lower(Attributes) ->
	% lists:map(fun attribute_to_lower/1, Attributes).

% attribute_to_lower(Attribute = #'PartialAttribute'{type = Type}) ->
	% Attribute#'PartialAttribute'{type = string:to_lower(Type)}.

