-module(dctg_frontend).

-export([total/1, config/1, start/1, start/2]).

% set total amount of launchers
total(Num) ->
    dctg_config_server:set_total(Num).

% start launchers of the hosts list
start(Hosts) ->
    dctg_controller:start_launchers(Hosts).

start(Hostnames, WorkerPerVM) ->
    Fun = fun(Host, AccIn) -> lists:append(AccIn, lists:duplicate(WorkerPerVM, Host)) end,
    Hosts = lists:foldl(Hostnames, fun(Host) -> [Host, Host, Host] end, []),
    dctg_controller:start_launchers(Hosts).