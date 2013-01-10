-module(feldapd_schema).

-include("feldapd_records.hrl").
-include("LDAP.hrl").

-export([load/1, load_full_schemas/1]).

-export([store/3, get/2]).

-export([load_schema/1]).
-export([file_to_schema/2]).

-export([check_node/2]).


load_schema(SchemaFiles) ->
	lists:foldl( fun file_to_schema/2, [], SchemaFiles ).


file_to_schema(FileName, Schema) ->
	case read_file_content(FileName) of
		{ok, Content} ->
			parse_to_scema(Schema, Content);
		Else ->
			io:format("WARNING: file (~p) read error: ~p ~n", [FileName, Else]),
			[]
	end.


% parse_to_scema(Content) ->
	% parse_to_scema([], Content).

parse_to_scema(Attrs, []) ->
	Attrs;
parse_to_scema(Attrs, Rest) ->
	case 'LDAP_Schema':decode('AttributeType', Rest) of
		{ok, Success, NewRest} ->
			parse_to_scema(add_to("attributeTypes", Success, Attrs), NewRest);
		Else ->
			case 'LDAP_Schema':decode('AttributeTypes', Rest) of
				{ok, Success, NewRest} ->
					parse_to_scema(add_to_multiple("attributeTypes", Success, Attrs), NewRest);
				Else ->
					case 'LDAP_Schema':decode('ObjectClass', Rest) of
						{ok, Success, NewRest} ->
							parse_to_scema(add_to("objectClasses", Success, Attrs), NewRest);
						Else ->
							case 'LDAP_Schema':decode('ObjectClasses', Rest) of
								{ok, Success, NewRest} ->
									parse_to_scema(add_to_multiple("objectClasses", Success, Attrs), NewRest);
								Else ->
									case 'LDAP_Schema':decode('MatchingRule', Rest) of
										{ok, Success, NewRest} ->
											parse_to_scema(add_to("matchingRules", Success, Attrs), NewRest);
										Else ->
											case 'LDAP_Schema':decode('MatchingRules', Rest) of
												{ok, Success, NewRest} ->
													parse_to_scema(add_to_multiple("matchingRules", Success, Attrs), NewRest);
												Else ->
													case 'LDAP_Schema':decode('LdapSyntax', Rest) of
														{ok, Success, NewRest} ->
															parse_to_scema(add_to("ldapSyntaxes", Success, Attrs), NewRest);
														Else ->
															case 'LDAP_Schema':decode('LdapSyntaxes', Rest) of
																{ok, Success, NewRest} ->
																	parse_to_scema(add_to_multiple("ldapSyntaxes", Success, Attrs), NewRest);
																Else ->
																	io:format("SCHEMA PARSE ERROR: ~nParsed:~p~nRest:~p~nError:~p~n", [Attrs, Rest, Else]),
																	Else
															end
													end
											end
									end
							end
					end
			end
	end.
	
add_to(Attr, [_, _SName, _, Item, _], Attrs) ->
	StrItem = lists:flatten(Item),
	% io:format("~nAttrs: ~p~nAttr:~p~nItem: ~p~n", [Attrs, Attr, Item]),
	case lists:keyfind(Attr, #'PartialAttribute'.type, Attrs) of
		false -> % meg nincs ilyen key
			[#'PartialAttribute'{type = Attr, vals = [StrItem]} | Attrs];
		#'PartialAttribute'{type = Attr, vals = Vals} ->
			lists:keyreplace(Attr, #'PartialAttribute'.type, Attrs, #'PartialAttribute'{type = Attr, vals = [StrItem | Vals]})
	end.

add_to_multiple(Attr, [_, SName, Items], Attrs) ->
	% io:format("~nMULTIPLE~nAttrs: ~p~nAttr:~p~nItems: ~p~n", [Attrs, Attr, Items]),
	lists:foldl( fun([_, Item, _], Result) -> add_to(Attr, [[], SName, [], Item, []], Result) end, Attrs, Items ).



% - full parse -
load_full_schemas(SchemaFiles) ->
	load_full_schemas(SchemaFiles, []).

load_full_schemas([], Loaded) ->
	{ok, Loaded};
load_full_schemas([SchemaFile|SchemaFiles], Loaded) ->
	case load(SchemaFile) of
		{ok, LoadedNow} ->
			load_full_schemas(SchemaFiles, Loaded ++ LoadedNow);
		_Else ->
			io:format("Schema file error: ~p~n", SchemaFile),
			load_full_schemas(SchemaFiles, Loaded)
	end.

load(FileName) ->
	case read_file_content(FileName) of
		{ok, Content} -> 
			case parse(Content) of
				fail ->
					fail;
				{ok, Parsed, []} ->
					{ok, reformat(Parsed)};
				{ok, Parsed, Rest} ->
					{error, Parsed, Rest};
				WTF ->
					WTF
			end;
		Else ->
			Else
	end.

read_file_content(FileName) ->
	case file:open(FileName, [read]) of
		{ok, File} -> 
			Content = read_file_content(File, []),
			file:close(File),
			{ok, Content};
		Else ->
			Else
	end.

read_file_content(File, Content) ->
	case file:read_line(File) of
		eof ->
			Content;
		{ok, []} ->
			read_file_content(File, Content);
		{ok, [35 | _]} -> % #-jellel kezdodik
			read_file_content(File, Content);
		{ok, Line} ->
			read_file_content(File, Content ++ " " ++ remove_tabcrlf(Line));
		Else ->
			Else
	end.
	


parse(String) ->
	'LDAP_Schema':decode('SCHEMA', String).

% remove_tab(String) ->
	% lists:map(fun (9)->32;(Chr)->Chr end, String).
	
remove_tabcrlf(String) ->
	lists:map(fun (9)->32;(10)->32;(13)->32;(Chr)->Chr end, String).

reformat(Items) ->
	lists:flatten(lists:map(fun reformat_item/1, Items)).

reformat_name(Name) ->
	case string:to_lower(Name) of
		"attributetypes:" ->
			"attributetype";
		"objectclasses:" ->
			"objectclass";
		Else ->
			io:format("???~p~n", [Else]),
			Else
	end.


reformat_item(_Item = [ _, Name, _, [40, _, OID | Attributes], _ | [] ]) ->
	{reformat_name(Name), lists:flatten(OID), reformat_attributes(Attributes)};
	
reformat_item(_Item = [ _, Name, TheItemList | [] ]) ->
	lists:map( fun ([_, [40, _, OID | Attributes], _]) -> {reformat_name(Name), lists:flatten(OID), reformat_attributes(Attributes)} end, TheItemList).
	
reformat_attributes(Attrs) ->
	Filtered = lists:filter( fun ([]) -> false; (" ") -> false; (41) -> false; (_) -> true end, Attrs ),
	
	lists:reverse( % csak a sorrend tartas miatt tettem ide, de nem lenyeges valojaban, mert kesobb mar nem szamit a sorrend
	lists:foldl(fun	([[_, AttrName]], Formed) -> % single attribute
						[{lists:flatten(AttrName)} | Formed];
					([[_, AttrName, _, [40, _, [AttrValue | [RestAttrValues]], _, 41]]], Formed) -> % attribute with multiple values
						[{ lists:flatten(AttrName), [lists:flatten(AttrValue) | lists:map( fun	([_, 36, _, RestAttrValue]) -> lists:flatten(RestAttrValue); 
																				(RestAttrValue) -> lists:flatten(RestAttrValue)
																			end, RestAttrValues) ] } | Formed];
					([[_, AttrName, _, AttrValue]], Formed) -> % attribute with single value
						[{lists:flatten(AttrName), lists:flatten(AttrValue)} | Formed];
					(Extensions = [FirstExtension|_], Formed) when is_list(FirstExtension) ->
						lists:map(fun ([_, ExtName, _, ExtValue]) -> {lists:flatten(ExtName), lists:flatten(ExtValue)} end, Extensions) ++ Formed % @todo: itt is lehet ExtValue egy value lista, de valoszinuleg nem szamit
					% (Else, Formed) -> %% itt nem tudom mi tortenik
						% io:format(" !!! WTF WTF WTF !!! ~p~n", [Else]),
						% Formed
				end, [], Filtered)
	).

%%% =================================================
%%% = Store/Get Schema ==============================
%%% =================================================

store(Tab, DataModule, Schema) ->
	DataModule:write(Tab, "schema_loaded", Schema).
get(Tab, DataModule) ->
	DataModule:read(Tab, "schema_loaded").



%%% =================================================
%%% = Schema Checking ===============================
%%% =================================================


check_node(Node, Schema) ->
	case check_node_objectclass(Node, Schema) of
		ok ->
			ok;
		Else ->
			Else
	end.

check_node_objectclass(Node = #r_FELDAPD_Node{attributes = Attributes}, Schema) ->
	case feldapd_node:get_attribute("objectclass", Node) of
		{ok, #'PartialAttribute'{vals = ObjectClassList}} ->
			check_node_objectclass(Attributes, ObjectClassList, Schema);
		Else ->
			Else
	end.

check_node_objectclass(Attributes, ObjectClassList, Schema) ->
	% io:format("ObjectClassList: ~p~n", [ObjectClassList]),
	case get_objectclasses_musts_mays(ObjectClassList, Schema) of
		false ->
			io:format("no objectclass: ~p~n", [ObjectClassList]),
			false;
		{Musts, Mays} ->
			case check_musts(feldapd_help:attribute_key_to_lower(Attributes), feldapd_help:strlist_to_lower(Musts)) of
				{ok, RestAttribute} ->
					case check_mays(RestAttribute, feldapd_help:strlist_to_lower(Mays)) of
						ok ->
							ok;
						{error, Reason} ->
							io:format("not allowed attribute: ~p~n", [Reason]),
							{error, Reason}
					end;
				{error, Reason} ->
					io:format("missing required attribute: ~p~n", [Reason]),
					{error, Reason}
			end
	end.


% get_objectclass(ObjectClass, Schema) ->
	% lists:keyfind(ObjectClass, 1, Schema).

% get_objectclasses_musts_mays(ObjectClassList, Schema) ->
	% lists:foldl(fun	(Item, {Musts, Mays}) ->
				% end, {[], []}, ObjectClassList).

get_objectclasses_musts_mays(ObjectClassList, Schema) ->
	get_objectclasses_musts_mays(ObjectClassList, Schema, {[], []}).


% vegigmegyunk egyesevel az ObjectClassList elemein
get_objectclasses_musts_mays([], _SchemaList, MustsMays) ->
	MustsMays;
get_objectclasses_musts_mays([ObjectClass | ObjectClassTail], SchemaList, MustsMays) ->
	{NewMustsMays, NewObjectClassList} = get_objectclasses_musts_mays__oc(ObjectClass, SchemaList, MustsMays, ObjectClassTail),
	get_objectclasses_musts_mays(NewObjectClassList, SchemaList, NewMustsMays).

% adott ObjectClass-t ellenorizzuk a SchemaList elemein
get_objectclasses_musts_mays__oc(_ObjectClass, [], MustsMays, ObjectClassList) ->
	{MustsMays, ObjectClassList};
get_objectclasses_musts_mays__oc(ObjectClass, [Schema | SchemaTail], MustsMays, ObjectClassList) ->
	{NewMustsMays, NewObjectClassList} = get_objectclasses_musts_mays__oc__schema(ObjectClass, Schema, MustsMays, ObjectClassList, [], {[], []}, false),
	get_objectclasses_musts_mays__oc(ObjectClass, SchemaTail, NewMustsMays, NewObjectClassList).

% az ObjectClass-t bevizsgaljuk a Schema-n
get_objectclasses_musts_mays__oc__schema(_ObjectClass, {_SchemaType, _SchemaOID, []}, MustsMays, ObjectClassList, _AddObjectClassList, _AddMustsMays, false) ->
	{MustsMays, ObjectClassList};
get_objectclasses_musts_mays__oc__schema(_ObjectClass, {_SchemaType, _SchemaOID, []}, {Musts, Mays}, ObjectClassList, AddObjectClassList, {AddMusts, AddMays}, true) ->
	% io:format(" <<MM>> ~p ++ ~p~n", [Musts, AddMusts]),
	{{lists:usort(Musts ++ AddMusts), lists:usort(Mays ++ AddMays)}, lists:usort(ObjectClassList ++ AddObjectClassList)};
get_objectclasses_musts_mays__oc__schema(ObjectClass, {SchemaType, SchemaOID, [SchemaProp | SchemaPropTail]}, MustsMays, ObjectClassList, AddObjectClassList, {AddMusts, AddMays}, Add) ->
	% io:format(" > Checking ~p - ~p - ~p~n", [AddMusts, AddMays, SchemaProp]),
	case string:to_lower(SchemaType) of
		"objectclass" -> % csak ez az eset erdekes
			case SchemaProp of
				% ez csak egy szimpla name magaban, ertek nelkul -> nem erdekes
				{_PropName} -> 
					% io:format("  >>  1~n"),
					get_objectclasses_musts_mays__oc__schema(ObjectClass, {SchemaType, SchemaOID, SchemaPropTail}, MustsMays, ObjectClassList, AddObjectClassList, {AddMusts, AddMays}, Add);
				% tobb erteku -> egy ertekuve teszem
				{PropName, [PropVal | PropValTail]} when is_list(PropVal) -> 
					% io:format("  >>  n~n"),
					get_objectclasses_musts_mays__oc__schema(ObjectClass, {SchemaType, SchemaOID, [ {PropName, PropVal}, {PropName, PropValTail} | SchemaPropTail]}, MustsMays, ObjectClassList, AddObjectClassList, {AddMusts, AddMays}, Add);
				% ez mar egy erteku
				{_PropName, []} -> 
					get_objectclasses_musts_mays__oc__schema(ObjectClass, {SchemaType, SchemaOID, SchemaPropTail}, MustsMays, ObjectClassList, AddObjectClassList, {AddMusts, AddMays}, Add);
				{PropName, PropVal} -> 
					% io:format("  >>  ~p = ~p~n", [PropName, PropVal]),
					case string:to_lower(PropName) of
						"name" -> % ha egyezik az ObjectClass-szal, akkor veszem csak figyelembe a feldolgozott informaciokat, kulonben ugrunk is tovabb
							case string:to_lower(PropVal) =:= string:to_lower(ObjectClass) orelse string:to_lower(PropVal) =:= "'" ++ string:to_lower(ObjectClass) ++ "'" of
								true ->
									% io:format("  >>  >> FOUND~n"),
									get_objectclasses_musts_mays__oc__schema(ObjectClass, {SchemaType, SchemaOID, SchemaPropTail}, MustsMays, ObjectClassList, AddObjectClassList, {AddMusts, AddMays}, true);
								false ->
									{MustsMays, ObjectClassList}
							end;
						"sup" -> % parent objectClass 
							get_objectclasses_musts_mays__oc__schema(ObjectClass, {SchemaType, SchemaOID, SchemaPropTail}, MustsMays, ObjectClassList, [PropVal | AddObjectClassList], {AddMusts, AddMays}, Add);
						"may" -> % add to may list
							get_objectclasses_musts_mays__oc__schema(ObjectClass, {SchemaType, SchemaOID, SchemaPropTail}, MustsMays, ObjectClassList, AddObjectClassList, {AddMusts, [PropVal | AddMays]}, Add);
						"must" -> % add to must list
							get_objectclasses_musts_mays__oc__schema(ObjectClass, {SchemaType, SchemaOID, SchemaPropTail}, MustsMays, ObjectClassList, AddObjectClassList, {[PropVal | AddMusts], AddMays}, Add);
						_Else -> % valami egyeb, most nem kell
							get_objectclasses_musts_mays__oc__schema(ObjectClass, {SchemaType, SchemaOID, SchemaPropTail}, MustsMays, ObjectClassList, AddObjectClassList, {AddMusts, AddMays}, Add)
					end
			end;
		_Else ->
			{MustsMays, ObjectClassList}
	end.


% % elfogytak az objectclassok -> vege
% get_objectclasses_musts_mays([], _Schema, {Musts, Mays}, _OriginalSchema) ->
	% {Musts, Mays};
% % elfogyott a schema -> ezt az ObjectClass-t megvizsgaltuk, johet a kovetkezo (lehetne optimalisabban, hamarabb befejezni a vizsgalatat)
% get_objectclasses_musts_mays([_ | ObjectClassList], [], {Musts, Mays}, OriginSchema) -> 
	% get_objectclasses_musts_mays(ObjectClassList, OriginSchema, {Musts, Mays}, OriginSchema);
% % elfogyott egy schema osszes property-je
% get_objectclasses_musts_mays([ObjectClass | ObjectClassList], [{_OName, _OID, []} | SchemaTail], {Musts, Mays}, OriginSchema) ->
	% get_objectclasses_musts_mays([ObjectClass | ObjectClassList], SchemaTail, {Musts, Mays}, OriginSchema);
% % 
% get_objectclasses_musts_mays([ObjectClass | ObjectClassList], [{OName, OID, [Prop | PropTail]} | SchemaTail], {Musts, Mays}, OriginSchema) ->
	% case string:to_lower(OName) of
		% "objectclass" ->
			% case Prop of
				% {_PropName} -> % ez csak egy szimpla name magaban, ertek nelkul -> nem erdekes
					% get_objectclasses_musts_mays([ObjectClass | ObjectClassList], [{OName, OID, PropTail} | SchemaTail], {Musts, Mays}, OriginSchema);
				% {PropName, [PropVal | PropValTail]} when is_list(PropVal) -> % tobb erteku -> egy ertekuve teszem
					% get_objectclasses_musts_mays([ObjectClass | ObjectClassList], [{OName, OID, [ {PropName, PropVal}, {PropName, PropValTail} | PropTail]} | SchemaTail], {Musts, Mays}, OriginSchema);
				% {PropName, PropVal} -> % ez mar egy erteku
					% case string:to_lower(PropName) of
						% "sup" -> % parent objectClass 
							% get_objectclasses_musts_mays([ObjectClass, PropVal | ObjectClassList], [{OName, OID, PropTail} | SchemaTail], {Musts, Mays}, OriginSchema);
						% "may" -> % add to may list
							% get_objectclasses_musts_mays([ObjectClass | ObjectClassList], [{OName, OID, PropTail} | SchemaTail], {Musts, [PropVal | Mays]}, OriginSchema);
						% "must" -> % add to must list
							% get_objectclasses_musts_mays([ObjectClass | ObjectClassList], [{OName, OID, PropTail} | SchemaTail], {[PropVal | Musts], Mays}, OriginSchema);
						% _Else -> % valami egyeb, most nem kell
							% get_objectclasses_musts_mays([ObjectClass | ObjectClassList], [{OName, OID, PropTail} | SchemaTail], {Musts, Mays}, OriginSchema)
					% end
			% end;
		% _Else ->
			% get_objectclasses_musts_mays(ObjectClassList, SchemaTail, {Musts, Mays}, OriginSchema)
	% end.
	

% get_objectclasses_musts_mays([{_OName, _OID, []} | ObjectClassList], Schema, {Musts, Mays}) ->
	% get_objectclasses_musts_mays(ObjectClassList, Schema, {Musts, Mays});
% get_objectclasses_musts_mays([{OName, OID, [{PropName, [PropVal|PropVals] = PropValsOne} | Props]} = _OC | ObjectClassList], Schema, {Musts, Mays}) ->
	% case string:to_lower(PropName) of
		% "sup" when is_list(PropVal) -> % propval egy string -> tobb sup van felsorolva
			% get_objectclasses_musts_mays([{OName, OID, PropVals}, PropVal | ObjectClassList], Schema, {Musts, Mays});
		% "sup" -> % csak egy sup van
			% get_objectclasses_musts_mays([{OName, OID, Props}, PropValsOne | ObjectClassList], Schema, {Musts, Mays});
		% "may" when is_list(PropVal) -> % tobb
			% get_objectclasses_musts_mays([{OName, OID, Props}, ObjectClassList], Schema, {Musts, PropValsOne ++ Mays});
		% "may" -> % egy
			% get_objectclasses_musts_mays([{OName, OID, Props}, ObjectClassList], Schema, {Musts, [PropValsOne | Mays]});
		% "must" when is_list(PropVal) -> % tobb
			% get_objectclasses_musts_mays([{OName, OID, Props}, ObjectClassList], Schema, {PropValsOne ++ Musts, Mays});
		% "must" -> % egy
			% get_objectclasses_musts_mays([{OName, OID, Props}, ObjectClassList], Schema, {[PropValsOne | Musts], Mays});
		% _Else ->
			% get_objectclasses_musts_mays([{OName, OID, Props}, ObjectClassList], Schema, {Musts, Mays})
	% end.


%% itt mar lowercase az Attribute, Must es May lista is
% check musts
check_musts(Attributes, []) ->
	{ok, Attributes};
check_musts(Attributes, [Must | MustTail]) ->
	% io:format("Attributes: ~p~nMust: ~p~n", [Attributes, Must]),
	case lists:keyfind(Must, #'PartialAttribute'.type, Attributes) of
		false -> % nincs must
			{error, {must_missing, Must}};
		_ -> % megvan, akkor toroljuk a listabol
			check_musts(lists:keydelete(Must, #'PartialAttribute'.type, Attributes), MustTail)
	end.
% check mays
check_mays([], _Mays) ->
	ok;
check_mays([#'PartialAttribute'{type = AttributeType} | AttributeTail], Mays) ->
	% io:format("AttributeType: ~p~nMays: ~p~n", [AttributeType, Mays]),
	case lists:member(AttributeType, Mays) of
		true ->
			check_mays(AttributeTail, Mays);
		false ->
			{error, {attribute_is_not_enabled, AttributeType}}
	end.
