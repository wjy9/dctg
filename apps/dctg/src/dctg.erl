-module(dctg).

-export([start/0, startlauncher/0]).

start() ->
    application:start(dctg).

startlauncher() ->
    application:start(dctg_client).