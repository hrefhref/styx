styx
=====

Ory Kratos/Hydra Lightweight erlang frontend.

Made to run on your main top domain. See `config/nginx.conf` for locations.

Can supervise external `kratos` and `hydra` processes directly (especially useful for development).

## Getting Started

Dependencies:

  * NodeJS & NPM (development only, only to modify css/js)
  * Kratos and Hydra
  * Erlang, Rebar3

    rebar3 get-deps
    npm --prefix assets install

Build:

    rebar3 compile && npm --prefix assets deploy
