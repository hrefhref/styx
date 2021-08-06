-module(ory_kratos).

-export([login_url/1, registration_url/1, settings_url/1, recovery_url/1, verification_url/1, url/0]).
-export([registration_flow/2, login_flow/2, settings_flow/2, recovery_flow/2, verification_flow/2, logout_flow/1, whoami/1, error/1]).

login_url(browser) ->
    [url(), "/self-service/login/browser"].

registration_url(browser) ->
    [url(), "/self-service/registration/browser"].

settings_url(browser) ->
    [url(), "/self-service/settings/browser"].

recovery_url(browser) ->
    [url(), "/self-service/recovery/browser"].

verification_url(browser) ->
    [url(), "/self-service/verification/browser"].

url() ->
    {ok, Value} = application:get_env(ory, kratos_url),
    Value.

registration_flow(Cookie, Id) ->
    Url = [url(), "/self-service/registration/flows?id=", Id],
    Headers = [{<<"cookie">>, Cookie}, {"accept", "application/json"}],
    api_response(hackney:request(get, Url, Headers, <<>>, [])).

login_flow(Cookie, Id) ->
    Url = [url(), "/self-service/login/flows?id=", Id],
    Headers = [{<<"cookie">>, Cookie}, {"accept", "application/json"}],
    api_response(hackney:request(get, Url, Headers, <<>>, [])).

settings_flow(Cookie, Id) ->
    Url = [url(), "/self-service/settings/flows?id=", Id],
    Headers = [{<<"cookie">>, Cookie}, {"accept", "application/json"}],
    api_response(hackney:request(get, Url, Headers, <<>>, [])).

recovery_flow(Cookie, Id) ->
    Url = [url(), "/self-service/recovery/flows?id=", Id],
    Headers = [{<<"cookie">>, Cookie}, {"accept", "application/json"}],
    api_response(hackney:request(get, Url, Headers, <<>>, [])).

verification_flow(Cookie, Id) ->
    Url = [url(), "/self-service/verification/flows?id=", Id],
    Headers = [{<<"cookie">>, Cookie}, {"accept", "application/json"}],
    api_response(hackney:request(get, Url, Headers, <<>>, [])).

logout_flow(Cookie) ->
    Url = [url(), "/self-service/logout/browser"],
    Headers = [{<<"cookie">>, Cookie}, {"accept", "application/json"}],
    api_response(hackney:request(get, Url, Headers, <<>>, [])).

whoami(Cookie) ->
    Url = [url(), "/sessions/whoami"],
    Headers = [{<<"cookie">>, Cookie}, {"accept", "application/json"}],
    api_response(hackney:request(get, Url, Headers, <<>>, [])).

error(Id) ->
    Url = [url(), "/self-service/errors?id=", Id],
    {ok, 200, _, Client} = hackney:request(get, Url, [], <<>>, []),
    {ok, Body} = hackney:body(Client),
    {ok, jsone:decode(Body)}.

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
