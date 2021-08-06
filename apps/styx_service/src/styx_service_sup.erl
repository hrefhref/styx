%%%-------------------------------------------------------------------
%% @doc styx_service top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(styx_service_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).
-define(EXEC_TIMEOUT, 5000).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    Env = application:get_all_env(styx_service),
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = child_specs(Env, []),
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

child_specs([{watch_assets, _Options} | Rest], Acc) ->
    ExecArgs = [watch_assets, "npm --prefix /Users/href/dev/styx/assets/ run watch"],
    Spec = {watch_assets, {styx_service, start_link, ExecArgs}, permanent, 5000, worker, [styx_service]},
    child_specs(Rest, [Spec | Acc]);
child_specs([{kratos, _Options} | Rest], Acc) ->
    ExecArgs = [kratos, "kratos serve --watch-courier --config /Users/href/dev/styx/config/kratos.yml"],
    Spec = {kratos, {styx_service, start_link, ExecArgs}, permanent, 5000, worker, [styx_service]},
    child_specs(Rest, [Spec | Acc]);
child_specs([{hydra, _Options} | Rest], Acc) ->
    ExecArgs = [hydra, "hydra serve all --dangerous-allow-insecure-redirect-urls --dangerous-force-http --config /Users/href/dev/styx/config/hydra.yml"],
    Spec = {hydra, {styx_service, start_link, ExecArgs}, permanent, 5000, worker, [styx_service]},
    child_specs(Rest, [Spec | Acc]);
child_specs([], Acc) ->
    Acc.
