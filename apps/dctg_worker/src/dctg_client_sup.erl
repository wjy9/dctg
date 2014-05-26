-module(dctg_client_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE,[Args]).

init([]) ->
    SupFlags = {simple_one_for_one, 1, 2000},
    ChildSpec = [
                 {dctg_client, {dctg_client, start, []},
                  temporary, 2000, worker, [dctg_client]}
                ],

    {ok, {SupFlags, ChildSpec}}.