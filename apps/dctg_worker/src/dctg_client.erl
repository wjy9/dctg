-module(dctg_client).

-behaviour(gen_fsm).

-export([start/1]).

-export([init/1, handle_event/3, handle_sync_event/4, tcpconn/2,
        handle_info/3, terminate/3, code_change/4]).

-include("dctg_record.hrl").

start(Args) ->
    %error_logger:info_msg("WJY: start client args ~p~n", [Args]),
    gen_fsm:start_link(?MODULE, Args, []).

init({DestIP, Content}) ->
    {ok, tcpconn, {DestIP, Content}, 0}.

tcpconn(timeout, {DestIP, Content}) ->
    Port = Content#http.port,
    case gen_tcp:connect(DestIP, Port, []) of
        {ok, Sock} ->
            Cont = "GET " ++ Content#http.content ++ " HTTP/1.0\r\n\r\n",
            %error_logger:info_msg("WJY: tcp connect success ~p~n", [Cont]),
            gen_tcp:send(Sock, Cont),
            dctg_stat_cache:put(connect, 1);
        {error, Reason} ->
            error_logger:info_msg("WJY: client tcp connect fail, ~p~n", [Reason])
    end,
    {next_state, tcpconn, ok}.

handle_event(_Ev, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Ev, _From, StateName, State) ->
    {next_state, StateName, State}.

handle_info(_Info, _StateName, State) ->
    %error_logger:info_msg("WJY: received: ~p, time: ~p~n", [Info, os:timestamp()]),
    {stop, normal, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_Old, StateName, State, _Extra) ->
    {ok, StateName, State}.
