-module(dctg_monitor).

-behaviour(gen_fsm).

-export([start_link/0, send_stat/4, set_launchernum/1, stop/0]).
-export([init/1, wait/2, run/2, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {
    lau_num,
    count = 0,
    stat_arr,
    cur_time = 0
    }).

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

send_stat(Node, ID, TimeStamp, Stat) ->
    gen_fsm:send_event({?MODULE, Node}, {stat, ID, TimeStamp, Stat}).

set_launchernum(Num) ->
    gen_fsm:send_event(?MODULE, {set_launchernum, Num}).

stop() ->
    gen_fsm:send_all_state_event(?MODULE, {stop}).

init([]) ->
    %error_logger:info_msg("WJY: monitor init~n"),
    {ok, wait, #state{}}.

wait({set_launchernum, Num}, State) ->
    %error_logger:info_msg("WJY: monitor set launcher num ~p~n", [Num]),
    Array = array:new(Num),
    {next_state, run, State#state{lau_num = Num, stat_arr = Array}}.

run({stat, ID, TimeStamp, Stat}, State = #state{lau_num = Lau, count = Count, stat_arr = Array, cur_time = CurTime}) ->
    if
        TimeStamp == CurTime ->
            case array:get(ID, Array) of
                undefined ->
                    Array2 = array:set(ID, Stat, Array),
                    Count2 = Count + 1,
                    if
                        Count2 >= Lau ->
                            stat_update(Array2, TimeStamp, State);
                        true ->
                            {next_state, run, State#state{count = Count2, stat_arr = Array2}}
                    end;
                _Value ->
                    error_logger:info_msg("WJY: Error! ID duplicated! ~p~n", [{ID, TimeStamp}]),
                    {next_state, run, State}
            end;
        true ->
            error_logger:info_msg("WJY: Error! TimeStamp not consist id: ~p TimeStamp: ~p cur time: ~p~n", [ID, TimeStamp, CurTime]),
            {next_state, run, State}
    end.

stat_update(Array, Time, State) ->
    Fun = fun(_I, A, B) -> foldfun(A, B) end,
    {C, R, TC, TR} = array:foldl(Fun, {0, 0, 0, 0}, Array),
    %WJYTODO: should write result to mysql
    error_logger:info_msg("WJY: stat output ~p: ~p conn/s ~p req/s, ~p conn, ~p req~n", [Time, C, R, TC, TR]),
    Num = State#state.lau_num,
    NewArr = array:new(Num),
    {next_state, run, State#state{count = 0, stat_arr = NewArr, cur_time = Time + 1}}.

foldfun({C, R, TC, TR}, {Ac1, Ac2, Ac3, Ac4}) ->
    {Ac1 + C, Ac2 + R, Ac3 + TC, Ac4 + TR}.

handle_event({stop}, _, _State) ->
    error_logger:info_msg("WJY: monitor stop~n"),
    {next_state, wait, #state{}}.

handle_sync_event(_, _, StateName, State) ->
    {next_state, StateName, State}.

handle_info(_INfo, StateName, State) ->
    {next_state, StateName, State}.

code_change(_Old, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(_Reason, _, _State) ->
    ok.
