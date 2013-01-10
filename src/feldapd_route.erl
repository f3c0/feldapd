-module(feldapd_route).

-include("feldapd_records.hrl").
-include("LDAP.hrl").

-export([get_parent/1, get_me_parent/1]).
-export([prepare_route/1, prepare_route_back/1, prepare_route_reverse/1, prepare_route_back_reverse/1]).

get_parent(RouteString) ->
	case prepare_route(RouteString) of
		[ _ | ParentRoute ] ->
			prepare_route_back(ParentRoute);
		_ ->
			""
	end.

get_me_parent(RouteString) ->
	case prepare_route(RouteString) of
		[ Me | ParentRoute ] ->
			{Me, prepare_route_back(ParentRoute)};
		_ ->
			{"",""}
	end.


%% ======================================================================
%% < route prepare functions >
%% ======================================================================

prepare_route(RouteString) ->
	string:tokens(RouteString, ",").

prepare_route_back(RouteList) ->
	string:join(RouteList, ",").

prepare_route_reverse(RouteString) ->
	lists:reverse(string:tokens(RouteString, ",")).

prepare_route_back_reverse(RouteList) ->
	string:join(lists:reverse(RouteList), ",").

%% ======================================================================
%% </ route prepare functions >
%% ======================================================================
