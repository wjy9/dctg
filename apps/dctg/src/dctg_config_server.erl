-module(dctg_config_server).

-behaviour(gen_server).

-export([start_link/0, get_config/1, set_total/1, set_config/1, stop/0, init_fin/1, finish/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {launcher = 0, total, config, finish = 0}).

-include("config.hrl").

-define(FINISH_DELAY, 2000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_config(Config) ->
    gen_server:cast(?MODULE, {set_config, Config}).

get_config(Node) ->
    gen_server:call({?MODULE, Node}, {get_config}).

set_total(Num) ->
    gen_server:cast(?MODULE, {set_total, Num}).

init_fin(Node) ->
    gen_server:cast({?MODULE, Node}, {init_fin}).

finish(Node) ->
    gen_server:cast({?MODULE, Node}, {finish}).

stop() ->
    gen_server:cast(?MODULE, {stop}).

init([]) ->
    {ok, #state{}}.

handle_call({get_config}, _From, State = #state{config = Config}) ->
    {reply, Config, State}.

handle_cast({set_total, Num}, State) ->
    {noreply, State#state{total = Num}};

handle_cast({set_config, Config}, State) when is_record(Config, config) ->
    {noreply, State#state{config = Config}};

handle_cast({init_fin}, State = #state{launcher = Count, total = Num}) ->
    error_logger:info_msg("WJY: config server init fin received~n"),
    if
        Count + 1 >= Num ->
            %error_logger:info_msg("WJY: enough~n"),
            dctg_start_launcher:launch_start();
        true ->
            ok
    end,
    {noreply, State#state{launcher = Count + 1}};

handle_cast({finish}, State = #state{total = Total, finish = Fin}) ->
    %error_logger:info_msg("WJY: config server: finish ~p~n", [Fin]),
    NewFin = Fin + 1,
    if
        NewFin >= Total ->
            timer:sleep(?FINISH_DELAY),
            error_logger:info_msg("WJY: config server: all finished!!!~n"),
            dctg_controller:finish(),
            {noreply, State#state{finish = 0}};
        true ->
            {noreply, State#state{finish = NewFin}}
    end;


handle_cast({stop}, _State) ->
    % WJYTODO: should change this module to gen_fsm so that the get_config call will not fail
    error_logger:info_msg("WJY: config server stop~n"),
    {noreply, #state{}}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_Old, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
