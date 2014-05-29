-module(dctg_launcher).

-behaviour(gen_fsm).

-include("dctg_record.hrl").

-export([start_link/0, launch/1]).
-export([init/1, launcher/2, wait/2, handle_event/3,
    handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(WARN_THRESH, 0.2).

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

launch({Node, StartTime}) ->
    error_logger:info_msg("WJY: send launch~n"),
    gen_fsm:send_event({?MODULE, Node}, {launch, StartTime}).

init([]) ->
    error_logger:info_msg("WJY: launcher init~n"),
    {ok, ControllerNode} = application:get_env(dctg_worker, controller),
    case catch dctg_config_server:get_config(ControllerNode) of
        Config when is_record(Config, config) ->
            Type = Config#config.type,
            Intensity = Config#config.intensity,
            Count = Config#config.count,
            DestList = Config#config.dutlist,
            Content = Config#config.protocol,
            if
                Intensity * 10 < 1 ->
                    Interval = 100,
                    NewIntensity = Intensity * 100;
                Intensity < 1 ->
                    NewIntensity = Intensity * 10,
                    Interval = 10;
                true ->
                    NewIntensity = Intensity * 10, % WJY using 10ms interval
                    Interval = 10
            end,
            State = #launcher{
                              intensity = NewIntensity,
                              count = Count,
                              dest = DestList,
                              interval = Interval,
                              content = Content,
                              fraction = 0,
                              round = 0,
                              nth = 1
                              },
            error_logger:info_msg("WJY: State: ~p~n", [State]),
            {ok, wait, State};
        Other ->
            error_logger:info_msg("WJY: get_config failed~n"),
            exit({error, Other})
    end.

wait({launch, StartTime}, State) ->
    error_logger:info_msg("WJY: launch start~n"),
    Time = case utils:timediff(StartTime, os:timestamp()) of
                Num when Num < 0 ->
                    0;
                Else ->
                    erlang:trunc(Else)
            end,
    gen_fsm:send_event_after(Time, {launch}),
    dctg_stat_cache:start_send(StartTime),
    {next_state, launcher, State#launcher{start_time = StartTime}}.

launcher({launch}, State=#launcher{count = Count}) when Count =< 0 ->
    {stop, normal, State};
launcher({launch}, State=#launcher{intensity = Intensity,
                                   count = Count,
                                   dest = DestList,
                                   interval = Interval,
                                   start_time = StartTime,
                                   content = Content,
                                   fraction = Frac,
                                   round = Round,
                                   nth = Nth}) ->
    error_logger:info_msg("WJY: launch, Count: ~p~n", [Count]),
    CurrentTime = os:timestamp(), % TODO: should be erlang:now()?
    TimePast = utils:timediff(StartTime, CurrentTime),
    Timer = TimePast + Interval * (Round + 1),
    gen_fsm:send_event_after(erlang:round(Timer), {launch}),
    NewIntensity = erlang:trunc(Intensity),
    NewFrac = Frac + Intensity - NewIntensity,
    if
        NewFrac > 1 ->
            NewFrac2 = NewFrac - 1,
            NewIntensity2 = NewIntensity + 1;
        true ->
            NewFrac2 = NewFrac,
            NewIntensity2 = NewIntensity
    end,
    RNum = erlang:min(NewIntensity2, Count),
    NewNth = do_launch(Content, RNum, DestList, Nth),
    {next_state, launcher, State#launcher{count = Count - RNum, fraction = NewFrac2, round = Round + 1, nth = NewNth}}.

do_launch(_, Num, _, Nth) when Num =< 0 ->
    Nth;
do_launch(Content, Num, DestList, Nth) ->
    DestIP = element(Nth, DestList),
    dctg_client_sup:start_child({DestIP, Content}),
    Size = size(DestList),
    NewNth = (Nth rem Size) + 1,
    do_launch(Content, Num - 1, DestList, NewNth).

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    slave:stop(node()),
    ok.

code_change(_Old, StateName, State, _Extra) ->
    {ok, StateName, State}.
