-module(styx_web_launchpad).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Cookie = cowboy_req:header(<<"cookie">>, Req0),
    case ory_kratos:whoami(Cookie) of
              {ok, Session = #{<<"active">> := true}} ->
                  logger:debug("Session ~p", [Session]),
                  Html = styx_web:render(Req0, launchpad_dtl, [{"session", Session}, {"name", styx_web:identity_name(Session)}]),
                  {ok, styx_web:reply_html(Req0, 200, Html), State};
              {error, #{<<"code">> := 401}} ->
                  {ok, styx_web:temporary_redirect(Req0, <<"/login">>), State};
              {error, Error = #{<<"code">> := Code, <<"status">> := Status, <<"message">> := Msg}} ->
                  styx_web_error:init(Req0, #{code => Code, status => Status, message => maps:get(<<"reason">>, Error, Msg)})
          end.
