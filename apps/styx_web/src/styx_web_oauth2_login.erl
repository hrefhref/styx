-module(styx_web_oauth2_login).
-behaviour(cowboy_handler).
-export([init/2]).
-export([get_cookie/1, unset_cookie/1]).
-define(CHALLENGE_COOKIE, <<"_styx_oauth2_login">>).
-define(CHALLENGE_MAX_AGE, 1800).
-define(REMEMBER_MAX_AGE, 1800).

init(Req = #{method := <<"GET">>}, State) ->
    get(Req, State, styx_web:req_param(Req, <<"login_challenge">>));
init(Req, _) ->
    styx_web_error:init(Req, #{code => 404, status => <<"Not Found">>}).

get_cookie(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    case lists:keyfind(<<"lang">>, 1, Cookies) of
        {?CHALLENGE_COOKIE, Challenge} -> {ok, Challenge};
        _ -> {error, no_oauth2_challenge_cookie}
    end.

unset_cookie(Req) ->
    cowboy_req:set_resp_cookie(?CHALLENGE_COOKIE, <<"null">>, Req, #{max_age => 0, http_only => true, path => <<"/">>}).

get(Req0, State, {ok, Challenge}) ->
    case ory_hydra:login_request(Challenge) of
        {ok, Request} ->
            Req = unset_cookie(Req0),
            logger:debug("Got challenge for auth req ~p", [Request]),
            authenticate(Req, State, Request);
        {error, Error = #{<<"code">> := Code, <<"status">> := Status, <<"message">> := Msg}} ->
            styx_web_error:init(Req0, #{code => Code, status => Status, message => maps:get(<<"reason">>, Error, Msg)})
    end;
get(Req, _State, {error, {missing_param, _}}) ->
    styx_web_error:init(Req, not_found).

authenticate(Req, State, Request) ->
    Cookie = cowboy_req:header(<<"cookie">>, Req),
    case ory_kratos:whoami(Cookie) of
        {ok, Session = #{<<"active">> := true}} ->
            challenge(Req, State, Request, Session);
        {error, #{<<"code">> := 401}} ->
            challenge(Req, State, Request, undefined);
        {error, Error = #{<<"code">> := Code, <<"status">> := Status, <<"message">> := Msg}} ->
            styx_web_error:init(Req, #{code => Code, status => Status, message => maps:get(<<"reason">>, Error, Msg)})
    end.

challenge(Req0, State, #{<<"challenge">> := Challenge}, undefined) ->
    Req1 = cowboy_req:set_resp_cookie(?CHALLENGE_COOKIE, Challenge, Req0, #{max_age => ?CHALLENGE_MAX_AGE, http_only => true}),
    Req = styx_web:temporary_redirect(Req1, <<"/login">>),
    {ok, Req, State};
%% XXX: What's the point of loggin in the user again?
%%challenge(Req, State, Request = #{<<"skip">> := false}, Session) ->
challenge(Req0, State, #{<<"challenge">> := Challenge}, #{<<"active">> := true, <<"identity">> := #{<<"id">> := Id, <<"traits">> := Traits}}) ->
    Data = #{<<"subject">> => Id, <<"remember">> => true, <<"remember_for">> => ?REMEMBER_MAX_AGE, <<"context">> => Traits},
    case ory_hydra:accept_login_request(Challenge, Data) of
        {ok, #{<<"redirect_to">> := Redirect}} ->
            Req = styx_web:temporary_redirect(Req0, Redirect),
            {ok, Req, State};
        {error, Error = #{<<"code">> := Code, <<"status">> := Status, <<"message">> := Msg}} ->
            styx_web_error:init(Req0, #{code => Code, status => Status, message => maps:get(<<"reason">>, Error, Msg)})
    end.
