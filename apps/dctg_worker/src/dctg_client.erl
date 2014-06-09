-module(dctg_client).

-behaviour(gen_fsm).

-export([start/1]).

-export([init/1, handle_event/3, handle_sync_event/4, tcpconn/2,
        waitrecv/2,
        handle_info/3, terminate/3, code_change/4]).

-include("dctg_record.hrl").

start(Args) ->
    %error_logger:info_msg("WJY: start client args ~p~n", [Args]),
    gen_fsm:start_link(?MODULE, Args, []).

init({SrcIP, DestIP, Port, URL, Interval}) ->
    {ok, tcpconn, {SrcIP, DestIP, Port, URL, Interval, undefine}, 0}.

tcpconn(timeout, {SrcIP, DestIP, Port, URL, Interval, Sock}) ->
    case Sock of
        undefine ->
            NewSock = connect(SrcIP, DestIP, Port),
            case Interval of
                0 ->
                    send(NewSock, URL),
                    {next_state, waitrecv, NewSock};
                _ ->
                    send(NewSock, URL),
                    gen_fsm:send_event_after(Interval, timeout),
                    {next_state, tcpconn, {DestIP, Port, URL, Interval, NewSock}}
            end;
        _ ->
            send(Sock, URL),
            inet:setopts(Sock, [{active, once}]), % WJYTODO
            gen_fsm:send_event_after(Interval, timeout),
            {next_state, tcpconn, {DestIP, Port, URL, Interval, Sock}}
    end.

waitrecv(_, State) ->
    {next_state, waitrecv, State}.

connect(SrcIP, DestIP, Port) ->
    case gen_tcp:connect(DestIP, Port, [{ip, SrcIP},
                                        {active, once},
                                        {keepalive, true} % WJYTODO
                                        ]) of
        {ok, Sock} ->
            dctg_stat_cache:put(connect, 1),
            Sock;
        {error, Reason} ->
            error_logger:info_msg("WJY: client tcp connect fail, ~p~n", [Reason]),
            exit(failed)
    end.
send(Sock, URL) ->
    Cont = "GET " ++ URL ++ " HTTP/1.1\r\n\r\n",
    case gen_tcp:send(Sock, Cont) of
        ok ->
            dctg_stat_cache:put(request, 1);
        {error, Reason} ->
            error_logger:info_msg("WJY: client tcp send fail ~p~n", [Reason])
    end.

handle_event(_Ev, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Ev, _From, StateName, State) ->
    {next_state, StateName, State}.

handle_info(Info, waitrecv, Sock) ->
    gen_tcp:close(Sock),
    {stop, normal, ok};
handle_info(_Info, StateName, State) ->
    %error_logger:info_msg("WJY: received: ~p, time: ~p~n", [Info, os:timestamp()]),
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_Old, StateName, State, _Extra) ->
    {ok, StateName, State}.
