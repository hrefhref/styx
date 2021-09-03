%%%-------------------------------------------------------------------
%% @doc styx_web public API
%% @end
%%%-------------------------------------------------------------------

-module(styx_web_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Port = 5000,
    CowboyOpts = [{port, Port}],
    CowboyEnv = #{env => #{
                           dispatch => routes()
                          }},
    {ok, _} = cowboy:start_clear(styx_web_listener, CowboyOpts, CowboyEnv),
    logger:notice("listening on ~p", [Port]),
    styx_web_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

routes() ->
    Trails = [
              %% App
              {"/", styx_web_index, undefined},
              {"/launchpad", styx_web_launchpad, undefined},
              {"/userinfo", styx_web_userinfo, undefined},

              %% Kratos
              {"/login", styx_web_kratos_flow, #{page_title => "Login", template => login_dtl, getflowmf => {ory_kratos, login_flow}, initflowmf => {ory_kratos, login_url}}},
              {"/register", styx_web_kratos_flow, #{page_title => "Register", template => registration_dtl, getflowmf => {ory_kratos, registration_flow}, initflowmf => {ory_kratos, registration_url}}},
              {"/account", styx_web_kratos_flow, #{page_title => "Account", template => account_dtl, getflowmf => {ory_kratos, settings_flow}, initflowmf => {ory_kratos, settings_url}}},
              {"/account/recovery", styx_web_kratos_flow, #{page_title => "Recover Account", template => recovery_dtl, getflowmf => {ory_kratos, recovery_flow}, initflowmf => {ory_kratos, recovery_url}}},
              {"/account/verification", styx_web_kratos_flow, #{page_title => "Verify Account", template => verification_dtl, getflowmf => {ory_kratos, verification_flow}, initflowmf => {ory_kratos, verification_url}}},
              {"/account/error", styx_web_error, undefined},
              {"/account/logout", styx_web_logout, undefined},

              %% Hydra
              {"/account/oauth2/login", styx_web_oauth2_login, undefined},
              {"/account/oauth2/consent", styx_web_oauth2_consent, undefined},
              {"/account/oauth2/logout", styx_web_oauth2_logout, undefined},
              {"/account/oauth2/error", styx_web_error, oauth2},

              %% Static
              {"/account/app.css", cowboy_static, {priv_file, styx_web, "assets/app.css"}},
              {"/account/app.js", cowboy_static, {priv_file, styx_web, "assets/app.js"}},

              %% 404 Catch all
              {'_', styx_web_error, #{code => 404, status => <<"Not found">>}}
             ],
    trails:single_host_compile(Trails).
