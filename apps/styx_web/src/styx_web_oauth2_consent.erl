-module(styx_web_oauth2_consent).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req = #{method := <<"GET">>}, State) ->
    init_(Req, State, styx_web:req_param(Req, <<"consent_challenge">>));
init(Req = #{method := <<"POST">>}, State) ->
    init_(Req, State, styx_web:req_param(Req, <<"consent_challenge">>));
init(Req, _) ->
    styx_web_error:init(Req, #{code => 404, status => <<"Not Found">>}).

init_(Req0, State, {ok, Challenge}) ->
    Req = styx_web_oauth2_login:unset_cookie(Req0),
    Cookie = cowboy_req:header(<<"cookie">>, Req),
    authentication(Req, State, Challenge, ory_kratos:whoami(Cookie));
init_(Req, _, {error, {missing_param, _}}) ->
    styx_web_error:init(Req, not_found).

authentication(Req0, State, Challenge, {ok, Session = #{<<"active">> := true}}) ->
    do(Req0, State, Session, ory_hydra:consent_request(Challenge));
authentication(Req0, State, _Challenge, Error) ->
    render_error(Req0, State, Error).

do(Req0 = #{method := <<"GET">>}, State, _Session, {ok, #{<<"challenge">> := Challenge, <<"skip">> := true, <<"requested_scope">> := Scopes}}) ->
    ConsentData = #{<<"grant_scope">> => Scopes},
    case ory_hydra:accept_consent_request(Challenge, ConsentData) of
        {ok, #{<<"redirect_to">> := Url}} ->
            Req = styx_web:temporary_redirect(Req0, Url),
            {ok, Req, State};
        Error ->
            render_error(Req0, State, Error)
    end;
do(Req0 = #{method := <<"GET">>}, State, _Session, {ok, Flow = #{<<"client">> := Client}}) ->
    %% FIXME client_name can be blank, not just undefined.
    logger:debug("oAuth request ~p", [Flow]),
    AppName = maps:get(<<"client_name">>, Client, maps:get(<<"client_id">>, Client, <<"Unnamed App">>)),
    Assigns = [{"page_title", ["Authorize ", AppName]}, {"flow", Flow}],
    Html = styx_web:render(Req0, oauth2_consent_form_dtl, Assigns),
    Req = styx_web:reply_html(Req0, 200, Html),
    {ok, Req, State};
do(Req0 = #{method := <<"POST">>}, State, Session, {ok, Flow}) ->
    {ok, Data, Req} = cowboy_req:read_urlencoded_body(Req0),
    post(Req, State, Session, Flow, Data).

post(Req0, State, Session, Flow, Data) ->
    Consent = case lists:keyfind(<<"consent">>, 1, Data) of
                  {_, <<"true">>} -> true;
                  _ -> false
    end,
    consent(Req0, State, Session, Flow, Data, Consent).


consent(Req0, State, _Session, #{<<"challenge">> := Challenge}, Data, true) ->
    ScopesFun = fun
                    ({<<"scope-", S/binary>>, <<"on">>}, Acc) -> [S | Acc];
                    (_, Acc) -> Acc
    end,
    Scopes = lists:foldl(ScopesFun, [], Data),
    ConsentData = #{<<"grant_scope">> => Scopes},
    case ory_hydra:accept_consent_request(Challenge, ConsentData) of
        {ok, #{<<"redirect_to">> := Url}} ->
            Req = styx_web:temporary_redirect(Req0, Url),
            {ok, Req, State};
        Error ->
            render_error(Req0, State, Error)
    end;
consent(Req0, State, _Session, #{<<"challenge">> := Challenge}, _Data, false) ->
    Data = #{<<"error">> => <<"User denied access.">>, <<"status_code">> => 403},
    case ory_hydra:reject_consent_request(Challenge, Data) of
        {ok, #{<<"redirect_to">> := Url}} ->
            Req = styx_web:temporary_redirect(Req0, Url),
            {ok, Req, State};
        Error ->
            render_error(Req0, State, Error)
    end.


render_error(Req, _State, {error, #{<<"code">> := Code, <<"status">> := Status, <<"message">> := Msg}}) ->
    styx_web_error:init(Req, #{code => Code, status => Status, message => Msg}).
