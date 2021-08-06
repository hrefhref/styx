-module(styx_web_logout).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Cookie = cowboy_req:header(<<"cookie">>, Req0),
    case ory_kratos:logout_flow(Cookie) of
        {ok, #{<<"logout_url">> := URL}} ->
            Req = styx_web:temporary_redirect(Req0, URL),
            {ok, Req, State};
        _ ->
            styx_web_error:init(Req0, #{code => 404, status => <<"Not Found">>})
    end.
