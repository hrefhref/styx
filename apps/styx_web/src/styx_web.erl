-module(styx_web).

-export([render/3, render_form/1, reply_html/3, reply_html/4, temporary_redirect/2, req_param/2, identity_name/1]).

identity_name(#{<<"identity">> := Identity}) ->
    identity_name(Identity);
identity_name(#{<<"traits">> := #{<<"name">> := #{<<"first">> := F, <<"last">> := L}}}) when is_binary(F), is_binary(L) ->
    [F, " ", L];
identity_name(#{<<"traits">> := #{<<"name">> := N}}) when is_binary(N) ->
    N;
identity_name(#{<<"traits">> := #{<<"username">> := U}}) when is_binary(U) ->
    U;
identity_name(#{<<"traits">> := #{<<"email">> := E}}) when is_binary(E) ->
    E;
identity_name(#{<<"id">> := Id}) ->
    Id.

render(Req, InnerModule, Assigns) ->
    {ok, InnerHtml} = InnerModule:render(Assigns),
    render_layout(Req, InnerHtml, Assigns).

render_form(UI = #{<<"action">> := Action, <<"nodes">> := Nodes}) ->
    Inputs = render_node(Nodes, []),
    Msgs = maps:get(<<"messages">>, UI, []),
    {ok, Html} = form_dtl:render([{"action", Action}, {"inputs", Inputs}, {"messages", Msgs}]),
    Html.

render_layout(_Req, InnerHtml, Assigns0) ->
    Assigns = [{"site_title", application:get_env(?MODULE, site_title, <<"Styx SSO">>)},
               {"background_image_url", application:get_env(?MODULE, background_image_url, <<"https://images.unsplash.com/photo-1505904267569-f02eaeb45a4c?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=crop&w=1908&q=80">>)},
               {"inner", InnerHtml}
              | Assigns0],
    {ok, Html} = layout_dtl:render(Assigns),
    Html.

reply_html(Req, Code, Html) ->
    reply_html(Req, Code, Html, #{}).

reply_html(Req, Code, Html, Headers0) ->
    Headers = maps:put(<<"content-type">>, <<"text/html">>, Headers0),
    cowboy_req:reply(Code, Headers, Html, Req).

temporary_redirect(Req0, Url) ->
    cowboy_req:reply(307, #{<<"location">> => Url}, Req0).

req_param(Req, Param) ->
    Qs = cowboy_req:parse_qs(Req),
    case lists:keyfind(Param, 1, Qs) of
        {_, Value} ->
            {ok, Value};
        _ ->
            {error, {missing_param, Param}}
    end.

render_node([#{<<"attributes">> := Attrs = #{<<"name">> := AttrName, <<"type">> := AttrType}, <<"type">> := <<"input">>, <<"messages">> := _Msgs, <<"meta">> := Meta} | Rest], Acc) ->
    Assigns0 = [{"input_name", AttrName},
                {"input_type", AttrType},
                {"input_value", maps:get(<<"value">>, Attrs, undefined)},
                {"input_required", maps:get(<<"required">>, Attrs, false)},
                {"input_disabled", maps:get(<<"disabled">>, Attrs, false)}],
    Assigns1 = case maps:get(<<"label">>, Meta, false) of
                #{<<"text">> := Text, <<"type">> := LType} -> [{"label", Text}, {"label_type", LType} | Assigns0];
                   _ -> Assigns0
    end,
    Assigns2 = case AttrName of
                   <<"traits.email">> -> [{"autocomplete", "email"} | Assigns1];
                   <<"password">> -> [{"autocomplete", "password"} | Assigns1];
                   _ -> Assigns1
    end,
    Assigns = Assigns2,
    {ok, Html} = case AttrType of
                     <<"submit">> -> form_submit_dtl:render(Assigns);
                    _ -> form_input_dtl:render(Assigns)
    end,
    render_node(Rest, Acc ++ Html);
render_node([], Acc) ->
    Acc.
