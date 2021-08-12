-module(styx_web_launchpad).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Cookie = cowboy_req:header(<<"cookie">>, Req0),
    case ory_kratos:whoami(Cookie) of
              {ok, Session = #{<<"active">> := true}} ->
                  logger:debug("Session ~p", [Session]),
                  oauth2_login(Req0, State, Session, styx_web_oauth2_login:get_cookie(Req0));
              {error, #{<<"code">> := 401}} ->
                  {ok, styx_web:temporary_redirect(Req0, <<"/login">>), State};
              {error, Error = #{<<"code">> := Code, <<"status">> := Status, <<"message">> := Msg}} ->
                  styx_web_error:init(Req0, #{code => Code, status => Status, message => maps:get(<<"reason">>, Error, Msg)})
          end.

oauth2_login(Req0, State, _Session, {ok, Challenge}) ->
    Req1 = styx_web_oauth2_login:unset_cookie(Req0),
    Req = styx_web:temporary_redirect(Req1, <<"/account/oauth2/login?login_challenge=", Challenge/binary>>),
    {ok, Req, State};
oauth2_login(Req, State, Session, {error, no_oauth2_challenge_cookie}) ->
    launchpad(Req, State, Session, application:get_env(styx_web, launchpad, true)).

launchpad(Req0, State, _Session, {url, Url}) ->
    Req = styx_web:temporary_redirect(Req0, Url),
    {ok, Req, State};
launchpad(Req0, State, Session, _) ->
    Html = styx_web:render(Req0, launchpad_dtl, [{"session", Session}, {"name", styx_web:identity_name(Session)}]),
    {ok, styx_web:reply_html(Req0, 200, Html), State}.
