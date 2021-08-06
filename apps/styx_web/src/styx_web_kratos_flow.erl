-module(styx_web_kratos_flow).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req = #{method := <<"GET">>}, State = #{page_title := _, template := _, getflowmf := _, initflowmf := _}) ->
    get(Req, State, styx_web:req_param(Req, <<"flow">>));
init(Req, _) ->
    styx_web_error:init(Req, #{code => 404, status => <<"Not Found">>}).

get(Req, State = #{getflowmf := {Mod, Fun}}, {ok, FlowId}) ->
    Cookie = cowboy_req:header(<<"cookie">>, Req),
    get_(Req, State, Mod:Fun(Cookie, FlowId));
get(Req0, State = #{initflowmf := {Mod, Fun}}, {error, {missing_param, _}}) ->
    Req = styx_web:temporary_redirect(Req0, Mod:Fun(browser)),
    {ok, Req, State}.

get_(Req0, State = #{page_title := PageTitle, template := Template}, {ok, Flow = #{<<"ui">> := UI}}) ->
    logger:debug("Flow = ~p", [Flow]),
    FormHtml = styx_web:render_form(UI),
    Assigns = [{"page_title", PageTitle}, {"form", FormHtml}],
    Html = styx_web:render(Req0, Template, Assigns),
    Req = styx_web:reply_html(Req0, 200, Html),
    {ok, Req, State};
get_(Req, State, {error, Error = #{<<"code">> := Code, <<"status">> := Status, <<"message">> := Msg}}) ->
    styx_web_error:init(Req, #{code => Code, status => Status, message => maps:get(<<"reason">>, Error, Msg)}).
