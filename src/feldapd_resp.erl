-module(feldapd_resp).

-include("LDAP.hrl").
-include("feldapd_records.hrl").

-export([bind/1, bind/2, bind/3, bind/4, bind/5]).
-export([result/1, result/2, result/3, result/4]).
-export([search/1, search/2]).

bind(ResultCode) ->
	bind(ResultCode, "").
bind(ResultCode, MatchedDN) ->
	bind(ResultCode, MatchedDN, "").
bind(ResultCode, MatchedDN, DiagnosticMessage) ->
	bind(ResultCode, MatchedDN, DiagnosticMessage, asn1_NOVALUE).
bind(ResultCode, MatchedDN, DiagnosticMessage, Referral) ->
	bind(ResultCode, MatchedDN, DiagnosticMessage, Referral, asn1_NOVALUE).
bind(ResultCode, MatchedDN, DiagnosticMessage, Referral, ServerSaslCreds) ->
	#'BindResponse'{resultCode = ResultCode, 
					matchedDN = MatchedDN, 
					diagnosticMessage = DiagnosticMessage, 
					referral = Referral, 
					serverSaslCreds = ServerSaslCreds }.

result(ResultCode) ->
	result(ResultCode, "").
result(ResultCode, MatchedDN) ->
	result(ResultCode, MatchedDN, "").
result(ResultCode, MatchedDN, DiagnosticMessage) ->
	result(ResultCode, MatchedDN, DiagnosticMessage, asn1_NOVALUE).
result(ResultCode, MatchedDN, DiagnosticMessage, Referral) ->
	#'LDAPResult'{	resultCode = ResultCode, 
					matchedDN = MatchedDN, 
					diagnosticMessage = DiagnosticMessage, 
					referral = Referral }.

search(#r_FELDAPD_Node{baseObject = BaseObject, attributes = Attributes}) ->
	search(BaseObject, Attributes);
search(ObjectName) when is_list(ObjectName) ->
	search(ObjectName, []).
search(ObjectName, Attributes) ->
	#'SearchResultEntry'{objectName = ObjectName, attributes = Attributes}.

