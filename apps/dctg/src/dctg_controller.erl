-module(dctg_controller).

-behaviour(gen_server).

-export([start_link/0, stop/0, start_launchers/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_launchers(Machines) ->
    gen_server:call(?MODULE, {start_launchers, Machines}, infinity).

stop() ->
    gen_server:cast(?MODULE, {stop}).

init([]) ->
    error_logger:info_msg("init~n"),
    {ok, []}.

handle_call({start_launchers, Machines}, _From, State) ->
    dctg_start_launcher:newbeams(Machines),
    {reply, ok, State}.

handle_cast({stop}, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_R, _State) ->
    ok.

code_change(_R, State, _E) ->
    {ok, State}.
