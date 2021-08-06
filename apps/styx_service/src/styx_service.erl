-module(styx_service).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_info/2]).

start_link(Name, Cmd) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Cmd], []).

init([Name, Cmd]) ->
    Opts = [stdout, stderr],
    {ok, Pid, OsPid} = exec:run_link(Cmd, Opts),
    logger:info("Started system process ~p '~p', system pid = ~p", [Name, Cmd, OsPid]),
    {ok, {Name, Pid, OsPid}}.

handle_info({stdout, OsPid, Binary}, {Name, _Pid, OsPid} = State) ->
    logger:info("~p[~p] ~p", [Name, OsPid, Binary]),
    {noreply, State};
handle_info({stderr, OsPid, Binary}, {Name, _Pid, OsPid} = State) ->
    logger:warning("~p[~p] ~p", [Name, OsPid, Binary]),
    {noreply, State};
handle_info(Info, {Name, _, _} = State) ->
    logger:warning("~p ~p", [Name, Info]).
