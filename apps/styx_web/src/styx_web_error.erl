-module(styx_web_error).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req, not_found) ->
    reply(Req, 404, <<"Not Found">>, undefined);
init(Req, State = #{code := Code, status := Status}) ->
    reply(Req, Code, Status, maps:get(message, State, undefined));
init(Req, oauth2) ->
    {ok, ErrorDescription} = styx_web:req_param(Req, <<"error_description">>),
    reply(Req, 500, <<"Error">>, ErrorDescription);
init(Req = #{method := <<"GET">>}, _) ->
    {ok, ErrorId} = styx_web:req_param(Req, <<"id">>),
    {ok, #{<<"error">> := #{<<"status">> := Status, <<"code">> := Code, <<"message">> := Msg}}} = ory_kratos:error(ErrorId),
    reply(Req, Code, Status, Msg).

reply(Req0, Code, Status, Msg) ->
    Assigns = [{"message", Msg}, {"status", Status}],
    Html = styx_web:render(Req0, error_dtl, Assigns),
    Req = styx_web:reply_html(Req0, Code, Html),
    {ok, Req, undefined}.
