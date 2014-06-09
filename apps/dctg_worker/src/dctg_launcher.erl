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
    %error_logger:info_msg("WJY: send launch~n"),
    gen_fsm:send_event({?MODULE, Node}, {launch, StartTime}).

init([]) ->
    %error_logger:info_msg("WJY: launcher init~n"),
    ID = utils:get_id(),
    {ok, ControllerNode} = application:get_env(dctg_worker, controller),
    case catch dctg_config_server:get_config(ID, ControllerNode) of
        {Config, IP} when is_record(Config, config) ->
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
            case Type of
                http ->
                    Port = Content#http.port,
                    URL = Content#http.content,
                    RequestInterval = Content#http.interval,
                    State = #launcher_http{
                                        ip = IP,
                                        intensity = NewIntensity,
                                        count = Count,
                                        dest = DestList,
                                        interval = Interval,
                                        port = Port,
                                        url = URL,
                                        req_interval = RequestInterval,
                                        fraction = 0,
                                        round = 0,
                                        nth = 1
                                    },
                    {ok, wait, State};
                raw ->
                    %WJYTODO: add raw support
                    State = #launcher_raw{
                                intensity = NewIntensity,
                                count = Count,
                                interval = Interval
                    },
                    {ok, wait, State}
            end;
        Other ->
            error_logger:info_msg("WJY: get_config failed~n"),
            exit({error, Other})
    end.

wait({launch, StartTime}, State) when is_record(State, launcher_http)->
    %error_logger:info_msg("WJY: launch start~n"),
    Time = case utils:timediff(StartTime, os:timestamp()) of
                Num when Num < 0 ->
                    0;
                Else ->
                    erlang:trunc(Else)
            end,
    gen_fsm:send_event_after(Time, {launch}),
    dctg_stat_cache:start_send(StartTime),
    {next_state, launcher, State#launcher_http{start_time = StartTime}}.

launcher({launch}, State=#launcher_http{count = Count}) when Count =< 0 ->
    {stop, normal, State};
launcher({launch}, State=#launcher_http{
                                    ip = IP,
                                    intensity = Intensity,
                                    count = Count,
                                    dest = DestList,
                                    interval = Interval,
                                    start_time = StartTime,
                                    port = Port,
                                    url = URL,
                                    req_interval = RInterval,
                                    fraction = Frac,
                                    round = Round,
                                    nth = Nth
                                    }) ->
    %error_logger:info_msg("WJY: launch, Count: ~p~n", [Count]),
    CurrentTime = os:timestamp(), % TODO: should be erlang:now()?
    TimePast = utils:timediff(StartTime, CurrentTime),
    Timer = case TimePast + Interval * (Round + 1) of
                Num when Num < 0 ->
                    error_logger:info_msg("WJY: launcher: too high load!~n"),
                    0;
                Else ->
                    Else
            end,
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
    NewNth = do_launch_http(IP, Port, URL, RInterval, RNum, DestList, Nth),
    {next_state, launcher, State#launcher_http{count = Count - RNum, fraction = NewFrac2, round = Round + 1, nth = NewNth}}.

do_launch_http(_, _, _, _, Num, _, Nth) when Num =< 0 ->
    Nth;
do_launch_http(SrcIP, Port, URL, RInterval, Num, DestList, Nth) ->
    DestIP = element(Nth, DestList),
    dctg_client_sup:start_child({SrcIP, DestIP, Port, URL, RInterval}),
    Size = size(DestList),
    NewNth = (Nth rem Size) + 1,
    do_launch_http(SrcIP, Port, URL, RInterval, Num - 1, DestList, NewNth).

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    %error_logger:info_msg("WJY: launcher: test sup active: ~p~n", [dctg_client_sup:active()]),
    dctg_client_killer:kill_finish(),
    ok.

code_change(_Old, StateName, State, _Extra) ->
    {ok, StateName, State}.
