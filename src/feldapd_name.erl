-module(feldapd_name).

-export([create/0, get_name/2, get_call_name/1]).

create() ->
	{local, list_to_atom(string:join(["feldapd", integer_to_list(random:uniform(9999999999))], "_"))}.

get_name({Location, Name}, PostFix) ->
	{Location, get_name(Name, PostFix)};
get_name({via, Module, Name}, PostFix) ->
	{via, Module, get_name(Name, PostFix)};
get_name(Name, PostFix) ->
	list_to_atom(string:join([atom_to_list(Name), PostFix], "_")).

get_call_name({local, Server}) -> Server;
get_call_name(Server) -> Server.
