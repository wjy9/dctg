-module(dctg_stat_cache).

-behaviour(gen_server).

-export([start_link/0, put/2, start_send/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    id,
    controller,
    timestamp = 0,
    connect = 0,
    request = 0,
    packet = 0,
    total_connect = 0,
    total_request = 0,
    total_packet = 0
    }).

-define(SEND_INTERVAL, 1000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

put(Name, Val) ->
    %error_logger:info_msg("WJY: stat cache put ~p~n", [{Name, Val}]),
    gen_server:cast(?MODULE, {put, Name, Val}).

start_send(StartTime) ->
    gen_server:cast(?MODULE, {start_send, StartTime}).

init([]) ->
    %error_logger:info_msg("WJY: stat cache init~n"),
    {ok, ControllerNode} = application:get_env(dctg_worker, controller),
    ID = utils:get_id(),
    {ok, #state{id = ID, controller = ControllerNode}}.

handle_cast({put, connect, Val}, State = #state{connect = Connect, total_connect = Total}) ->
    %error_logger:info_msg("WJY: stat cache put connect~n"),
    NewConnect = Connect + Val,
    NewTotal = Total + Val,
    {noreply, State#state{connect = NewConnect, total_connect = NewTotal}};

handle_cast({put, request, Val}, State = #state{request = Request, total_request = Total}) ->
    NewRequest = Request + Val,
    NewTotal = Total + Val,
    {noreply, State#state{request = NewRequest, total_request = NewTotal}};

handle_cast({put, packet, Val}, State = #state{packet = P, total_packet = T}) ->
    NP = P + Val,
    NT = T + Val,
    {noreply, State#state{packet = NP, total_packet = NT}};

handle_cast({start_send, StartTime}, State) ->
    Time = case utils:timediff(StartTime, os:timestamp()) + ?SEND_INTERVAL of
                Num when Num < 0 ->
                    0;
                Else ->
                    erlang:trunc(Else)
            end,
    %error_logger:info_msg("WJY: stat cache start send timer ~p~n", [Time]),
    erlang:start_timer(Time, self(), send),
    {noreply, State}.

handle_call(_Call, _From, State) ->
    {reply, error, State}.

handle_info({timeout, _Ref, send}, State = #state{id = ID, controller = Node, timestamp = TimeStamp,
                                            connect = Connect,
                                            request = Request,
                                            packet = Packet,
                                            total_connect = TConn,
                                            total_request = TReq,
                                            total_packet = TPkt}) ->
    %error_logger:info_msg("WJY: stat cache timeout send~n"),
    dctg_monitor:send_stat(Node, ID, TimeStamp, {Connect, Request, Packet, TConn, TReq, TPkt}),
    %error_logger:info_msg("WJY: stat cache send ~p~n", [{Connect, ID, TimeStamp}]),
    NewTimeStamp = TimeStamp + 1,
    erlang:start_timer(?SEND_INTERVAL, self(), send),
    {noreply, State#state{timestamp = NewTimeStamp, connect = 0, request = 0, packet = 0}}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.