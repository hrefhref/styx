-module(styx).

-export([kratos_url/0, hydra_url/0]).

kratos_url() ->
    application:get_env(styx, kratos_url, undefined).

hydra_url() ->
    application:get_env(styx, hydra_url, undefined).
