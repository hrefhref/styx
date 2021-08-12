-module(ory_hydra).
-export([url/0, admin_url/0, login_request/1, accept_login_request/2, consent_request/1, accept_consent_request/2, reject_consent_request/2]).

login_request(Challenge) ->
    Url = [admin_url(), "/oauth2/auth/requests/login?login_challenge=", Challenge],
    Headers = [{"accept", "application/json"}],
    api_response(hackney:request(get, Url, [], <<>>, [])).

accept_login_request(Challenge, Data) ->
    Url = [admin_url(), "/oauth2/auth/requests/login/accept?login_challenge=", Challenge],
    Headers = [{"accept", "application/json"}, {"content_type", "application/json"}],
    Json = jsone:encode(Data),
    api_response(hackney:request(put, Url, Headers, Json, [])).

consent_request(Challenge) ->
    Url = [admin_url(), "/oauth2/auth/requests/consent?consent_challenge=", Challenge],
    Headers = [{"accept", "application/json"}],
    api_response(hackney:request(get, Url, [], <<>>, [])).

accept_consent_request(Challenge, Data) ->
    Url = [admin_url(), "/oauth2/auth/requests/consent/accept?consent_challenge=", Challenge],
    Headers = [{"accept", "application/json"}, {"content_type", "application/json"}],
    Json = jsone:encode(Data),
    api_response(hackney:request(put, Url, Headers, Json, [])).

reject_consent_request(Challenge, Data) ->
    Url = [admin_url(), "/oauth2/auth/requests/consent/reject?consent_challenge=", Challenge],
    Headers = [{"accept", "application/json"}, {"content_type", "application/json"}],
    Json = jsone:encode(Data),
    api_response(hackney:request(put, Url, Headers, Json, [])).


admin_url() ->
    {ok, Value} = application:get_env(ory, hydra_admin_url),
    Value.

url() ->
    {ok, Value} = application:get_env(ory, hydra_url),
    Value.

api_response(Error = {error, Error}) ->
    logger:error("ory_kratos hackney error: ~p", [Error]),
    {error, #{<<"code">> => 503, <<"status">> => "Not Available", <<"message">> => "This service isn't available at the moment."}};
api_response({ok, 200, _, Client}) ->
    {ok, Body} = hackney:body(Client),
    {ok, jsone:decode(Body)};
api_response({ok, Code, _, Client}) ->
    {ok, Body} = hackney:body(Client),
    JSON = #{<<"error">> := Error} = jsone:decode(Body),
    logger:debug("hydra error: ~p", [JSON]),
    {error, Error}.
