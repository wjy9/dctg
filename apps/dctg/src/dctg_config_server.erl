-module(dctg_config_server).

-behaviour(gen_server).

-export([start_link/0, get_config/1, set_total/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {launcher = 0, total, config}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_config(Node) ->
    gen_server:call({?MODULE, Node}, {get_config}).

set_total(Num) ->
    gen_server:cast(?MODULE, {set_total, Num}).

init([]) ->
    {ok, #state{config = 
                    {0.1, 1000, {127, 0, 0, 1}, {http, "http://127.0.0.1:8080/"}}
                }
    }. % {Intensity, Count, Dest, Content}

handle_call({get_config}, _From, State = #state{launcher = Count, total = Num, config = Config}) ->
    error_logger:info_msg("WJY: get config ~p ~p~n", [Count, Num]),
    if
        Count + 1 >= Num ->
            error_logger:info_msg("WJY: enough~n"),
            dctg_start_launcher:launch_start();
        true ->
            ok
    end,
    {reply, Config, State#state{launcher = Count + 1}}.

handle_cast({set_total, Num}, State) ->
    {noreply, State#state{total = Num}}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_Old, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.