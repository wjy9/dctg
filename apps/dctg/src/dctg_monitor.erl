-module(dctg_monitor).

-behaviour(gen_fsm).

-export([start_link/0, send_stat/4, set_launchernum/1, stop/0]).
-export([init/1, wait/2, run/2, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {
    lau_num,
    count_array,
    stat_arr,
    cur_time = 0,
    aggstat_arr
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
    emysql:add_pool(mysql, [
    {size, 1},
    {user, "dctg"},
    {password, "dctg"},
    {database, "dctg"},
    {encoding, utf8}
    ]),
    {ok, wait, #state{stat_arr = array:new(), count_arr = array:new(), aggstat_arr = array:new()}}.

wait({set_launchernum, Num}, State) ->
    %error_logger:info_msg("WJY: monitor set launcher num ~p~n", [Num]),
    {next_state, run, State#state{lau_num = Num}};

wait({stat, ID, TimeStamp, Stat}, State) ->
    error_logger:info_msg("WJY: monitor: stat received after finish: ~p, ~p, ~p ~n", [ID, TimeStamp, Stat]),
    {next_state, wait, State}.

run({stat, ID, TimeStamp, Stat}, State = #state{lau_num = Lau,
                                            count_arr = CountArr,
                                            stat_arr = StatArr,
                                            cur_time = CurTime,
                                            aggstat_arr = AggArr}) ->
    case array:get(TimeStamp, StatArr) of
        undefine ->
            Array = array:new(Lau),
            Array2 = array:set(ID, Stat, Array),
            Count = 1,
            CountArr2 = array:set(TimeStamp, Count, CountArr),
            StatArr2 = array:set(TimeStamp, Array2, StatArr),
            if
                Count >= Lau ->
                    AggArr2 = stat_update(Array2, TimeStamp, State, AggArr),
                    CurTime2 = write_sql(AggArr2, CurTime),
                    {next_state, run, State#state{count_arr = CountArr2, stat_arr = StatArr2, aggstat_arr = AggArr2, cur_time = CurTime2}};
                true ->
                    {next_state, run, State#state{count_arr = CountArr2, stat_arr = StatArr2}}
            end;
        Array->
            case array:get(ID, Array) of
                undefined ->
                    Array2 = array:set(ID, Stat, Array),
                    Count = array:get(TimeStamp, CountArr),
                    Count2 = Count + 1,
                    CountArr2 = array:set(TimeStamp, Count2, CountArr),
                    StatArr2 = array:set(TimeStamp, Array2, StatArr),
                    if
                        Count2 >= Lau ->
                            AggArr2 =  stat_update(Array2, TimeStamp, State, AggArr),
                            CurTime2 = write_sql(AggArr2, CurTime),
                            {next_state, run, State#state{count_arr = CountArr2, stat_arr = StatArr2, aggstat_arr = AggArr2, cur_time = CurTime2}};
                        true ->
                            {next_state, run, State#state{count_arr = CountArr2, stat_arr = StatArr2}}
                    end;
                _Value ->
                    error_logger:info_msg("WJY: Error! ID duplicated! ~p~n", [{ID, TimeStamp}]),
                    {next_state, run, State}
            end
    end.

stat_update(Array, Time, State, AggArr) ->
    Fun = fun(_I, A, B) -> foldfun(A, B) end,
    {C, R, TC, TR} = array:foldl(Fun, {0, 0, 0, 0}, Array),
    array:set(Time, {C, R, TC, TR}, AggArr).

foldfun({C, R, TC, TR}, {Ac1, Ac2, Ac3, Ac4}) ->
    {Ac1 + C, Ac2 + R, Ac3 + TSC, Ac4 + TR}.

write_sql(AggArr, CurTime) ->
    case array:get(Curtime, AggArr) of
        undefine ->
            CurTime;
        {C, R, TC, TR} ->
            error_logger:info_msg("WJY: stat output ~p: ~p conn/s ~p req/s, ~p conn, ~p req~n", [CurTime, C, R, TC, TR]),
            Bc = list_to_binary(integer_to_list(C)),
            Br = list_to_binary(integer_to_list(R)),
            Btc = list_to_binary(integer_to_list(TC)),
            Btr = list_to_binary(integer_to_list(TR)),
            emysql:execute(mysql, <<"INSERT INTO stat SET connect = ", Bc/binary, ", total_connect = ", Btc/binary, ", request = ", Br/binary, ", total_request = ", Btr/binary>>),
            write_sql(AggArr, CurTime + 1)
    end.
    
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
