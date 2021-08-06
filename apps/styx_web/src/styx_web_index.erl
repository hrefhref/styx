-module(styx_web_index).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Cookie = cowboy_req:header(<<"cookie">>, Req0),
    Req = case ory_kratos:whoami(Cookie) of
        {ok, #{<<"active">> := true}} ->
           styx_web:temporary_redirect(Req0, <<"/launchpad">>);
        _ ->
            Html = styx_web:render(Req0, index_dtl, []),
            styx_web:reply_html(Req0, 200, Html)
    end,
    {ok, Req, State}.
