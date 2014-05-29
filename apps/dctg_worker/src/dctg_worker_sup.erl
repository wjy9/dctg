-module(dctg_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ClientSup = {dctg_client_sup, {dctg_client_sup, start_link, []}, permanent, 2000, supervisor, [dctg_client_sup]},
    Launcher = {dctg_launcher, {dctg_launcher, start_link, []}, transient, 2000, worker, [dctg_launcher]},
    StatCache = {dctg_stat_cache, {dctg_stat_cache, start_link, []}, transient, 2000, worker, [dctg_stat_cache]},
    {ok, { {one_for_one, 5, 10}, [ClientSup, Launcher, StatCache]} }.

