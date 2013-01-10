-module(feldapd_test).

-compile([export_all]).

test() ->
	test(3000).
test(Port) ->
	test("127.0.0.1", [{port, Port}]).

test_ssl() ->
	test_ssl(3000).
test_ssl(Port) ->
	test("127.0.0.1", [{port, Port}, {ssl, true}]).
	
test(Host, Options) ->
	{ok, S} = eldap:open([Host], Options),
	{error,strongerAuthRequired} = eldap:delete(S, "dc=erl"),
	test_bind(S),
	ok = eldap:delete(S, "dc=erl"),
	test_add(S),
	test_modify(S),
	test_search(S),
	eldap:close(S).

test_add(S) ->
	ok = eldap:add(S, "dc=erl", [
								{"objectClass", ["top", "dcObject"]},
								{"dc", ["erl"]}
							]),
	{error, _} = eldap:add(S, "dc=test,dc=erl", [{"objectClass", ["top", "dcObject"]}]),
	ok = eldap:add(S, "dc=alma,dc=erl", [
								{"objectClass", ["top", "dcObject"]},
								{"dc", ["alma"]}
							]),
	ok = eldap:add(S, "ou=person,dc=alma,dc=erl", [
								{"objectClass", ["organizationalUnit"]},
								{"ou", ["person"]}
							]),
	{error, _} = eldap:add(S, "cn=user1,ou=person,dc=alma,dc=erl", [
								{"objectClass", ["person"]},
								{"cn", ["user1"]}
							]),
	ok = eldap:add(S, "cn=user1,ou=person,dc=alma,dc=erl", [
								{"objectClass", ["person"]},
								{"cn", ["user1"]},
								{"sn", ["User SN"]},
								{"userPassword", ["user1pass"]}
							]),
	{error, _} = eldap:add(S, "cn=user2,ou=person,dc=alma,dc=erl", [
								{"objectClass", ["residentialPerson"]},
								{"cn", ["user2"]},
								{"sn", ["UserKetto"]},
								{"userPassword", ["user2pass"]}
							]),
	{error, _} = eldap:add(S, "cn=user2,ou=person,dc=alma,dc=erl", [
								{"objectClass", ["residentialPerson"]},
								{"cn", ["user2"]},
								{"sn", ["UserKetto"]},
								{"userPassword", ["user2pass"]},
								{"l", ["Kukutyin"]},
								{"description", ["Masodik user"]},
								{"not_enabled", ["Nem lehet ilyen attribttuma"]}
							]),
	ok = eldap:add(S, "cn=user2,ou=person,dc=alma,dc=erl", [
								{"objectClass", ["residentialPerson"]},
								{"cn", ["user2"]},
								{"sn", ["User SN"]},
								{"userPassword", ["user2pass"]},
								{"l", ["Kukutyin"]},
								{"description", ["Masodik user"]}
							]),
	ok.

test_bind(S) ->
	{error, _} = eldap:simple_bind(S, "cn=user2,ou=person,dc=alma,dc=erl", "user1pass"),
	{error, _} = eldap:simple_bind(S, "cn=userN,ou=person,dc=alma,dc=erl", "user2pass"),
	ok = eldap:simple_bind(S, "cn=user2,ou=person,dc=alma,dc=erl", "user2pass"),
	ok.

test_modify(S) ->
	ok = eldap:modify(S, "cn=user1,ou=person,dc=alma,dc=erl", [
			eldap:mod_add("description", ["Elso user description"])
		]),
	ok = eldap:modify(S, "cn=user2,ou=person,dc=alma,dc=erl", [
			eldap:mod_replace("l", ["Honululu"]),
			eldap:mod_delete("description", ["Masodik user description"]),
			eldap:mod_delete("description", ["Masodik user"])
		]),
	ok.

test_search_ok_empty(S, Options) ->
	{ok, {eldap_search_result, [], _R}} = eldap:search(S, Options),
	ok.

test_search_ok(S, Options, Count) ->
	{ok, {eldap_search_result, E, _R}} = eldap:search(S, Options),
	case length(E) of
		Count -> ok;
		_ -> false
	end.
	
test_search_ok(S, Options) ->
	{ok, {eldap_search_result, E, R}} = eldap:search(S, Options),
	io:format("Found: ~p~p~n", [E,R]),
	ok.

test_search(S) ->
	{error, _} = eldap:search(S, [
					{base, "dc=test,dc=erl"}, 
					{filter, eldap:equalityMatch("dc", "test")}
				]),
	ok = test_search_ok_empty(S, [
					{base, "dc=alma,dc=erl"}, 
					{filter, eldap:equalityMatch("dc", "test")}
				]),
	ok = test_search_ok(S, [
					{base, "dc=alma,dc=erl"}, 
					{scope, baseObject}, 
					{filter, eldap:equalityMatch("dc", "alma")}
				], 1),
	ok = test_search_ok(S, [
					{base, "dc=alma,dc=erl"}, 
					{filter, eldap:equalityMatch("dc", "alma")}
				], 1),
	ok = test_search_ok(S, [
					{base, "dc=alma,dc=erl"}, 
					{filter, eldap:equalityMatch("sn", "User SN")}
				], 2),
	ok.
