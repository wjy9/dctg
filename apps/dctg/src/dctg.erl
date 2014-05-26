-module(dctg).

-export([start/0, startworker/0]).

start() ->
    application:start(dctg).

startworker() ->
    application:start(dctg_worker).