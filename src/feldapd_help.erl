-module(feldapd_help).

-include("feldapd_records.hrl").
-include("LDAP.hrl").

-export([key_join/2, prepare_childkeys/2, attribute_key_to_lower/1, strlist_to_lower/1, time_expired/1]).
-export([remove_false/1]).

key_join(Key1, "") ->
	Key1;
key_join("", Key2) ->
	Key2;
key_join(Key1, Key2) ->
	string:join([Key1, Key2], ",").


attribute_key_to_lower(Attributes) ->
	lists:keymap(fun(Key) -> string:to_lower(Key) end, #'PartialAttribute'.type, Attributes).
strlist_to_lower(List) ->
	lists:map(fun(Elem) -> string:to_lower(Elem) end, List).


time_expired(_StartTime = {MegaSec, Sec, Microsec}) ->
	{NowMegaSec, NowSec, NowMicrosec} = erlang:now(),
	MegaSec > NowMegaSec orelse ( MegaSec =:= NowMegaSec andalso Sec > NowSec ) orelse (MegaSec =:= NowMegaSec andalso Sec =:= NowSec andalso Microsec > NowMicrosec).



remove_false(List) ->
	lists:filter( fun (false) -> false; (_) -> true end, List ).


prepare_childkeys(ChildKeys, BaseKey) ->
	lists:map(fun (Childkey) -> key_join(Childkey, BaseKey) end, ChildKeys).
