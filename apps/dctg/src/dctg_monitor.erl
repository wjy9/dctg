-module(dctg_monitor).

-behaviour(gen_server).

-export([start_link/0, send_stat/5, set_launchernum/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    lau_num,
    count = 0
    stat_arr,
    cur_time = 0
    }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send_stat(Node, Type, Val, ID, TimeStamp) ->
    gen_server:cast({?MODULE, Node}, {stat, Type, Val, ID, TimeStamp}).

set_launchernum(Num) ->
    gen_server:cast(?MODULE, {set_launchernum, Num}).

init([]) ->
    {ok, #state{}}.

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast({set_launchernum, Num}, State) ->
    Array = array:new(Num),
    {noreply, State#state{lau_num = Num, stat_arr = Array}};

handle_cast({stat, connect, Val, ID, TimeStamp}, State = #state{lau_num = Lau, count = Count, stat_arr = Array, cur_time = CurTime}) ->
    if
        TimeStamp == CurTime ->
            case array:get(ID, Array) of
                undefined ->
                    Array2 = array:set(ID, Val, Array),
                    Count2 = Count + 1,
                    if
                        Count2 >= Lau ->
                            stat_update(Array2, TimeStamp, State);
                        true ->
                            {noreply, State#state{count = Count2, stat_arr = Array2}}
                    end;
                _Value ->
                    error_logger:info_msg("WJY: Error! ID duplicated! ~p~n", [{Val, ID, TimeStamp}]),
                    {noreply, State}
            end;
        true ->
            error_logger:info_msg("WJY: Error! TimeStamp not consist id: ~p TimeStamp: ~p cur time: ~p~n", [ID, TimeStamp, CurTime]),
            {noreply, State}
    end.

stat_update(Array, Time, State) ->
    Acc = 0,
    Sum = array:foldl(fun(_I, A, B) -> A + B end, Acc, Array),
    %WJYTODO: should write result to mysql
    error_logger:info_msg("WJY: stat output ~p: ~p conn/s~n", [Time, Sum]),
    Num = State#state.lau_num,
    NewArr = array:new(Num),
    {noreply, State#state{count = 0, stat_arr = NewArr, cur_time = Time + 1}}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_Old, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.