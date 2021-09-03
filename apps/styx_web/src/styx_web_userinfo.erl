-module(styx_web_userinfo).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req = #{method := <<"GET">>}, State) ->
    get(Req, State);
init(Req, _) ->
    styx_web_error:init(Req, #{code => 404, status => <<"Not Found">>}).

get(Req, State) ->
    Authorization = cowboy_req:header(<<"authorization">>, Req),
    case ory_hydra:userinfo(Authorization) of
        {ok, UserInfo} ->
            get_identity(Req, UserInfo, State)
    end.

get_identity(Req, UserInfo = #{<<"sub">> := Id}, State) ->
    case ory_kratos:get_identity(Id) of
        {ok, Identity} -> process(Req, UserInfo, Identity, State)
    end.

process(Req, UserInfo, #{<<"traits">> := Traits, <<"schema_id">> := Schema}, State) ->
    Mapper = maps:get(Schema, application:get_env(styx, openid_userinfo_mapper, #{})),
    MapFun = fun(Key, Value, Acc) -> maps:put(Key, get_value(Traits, Value), Acc) end,
    Data = maps:fold(MapFun, #{}, Mapper),
    MergedData = maps:merge(UserInfo, Data),
    Headers = #{<<"content-type">> => <<"application/json">>},
    cowboy_req:reply(200, Headers, jsone:encode(MergedData), Req).

get_value(Traits, Value) when is_binary(Value) ->
    maps:get(Value, Traits);
get_value(Traits, [Value | Acc]) ->
    get_value(maps:get(Value, Traits), Acc);
get_value(Value, []) ->
    Value.
