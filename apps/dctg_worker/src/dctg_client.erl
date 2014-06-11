-module(dctg_client).

-behaviour(gen_fsm).

-export([start/1]).

-export([init/1, handle_event/3, handle_sync_event/4, tcpconn/2,
        waitrecv/2,
        handle_info/3, terminate/3, code_change/4]).

-include("dctg_record.hrl").

-record(state, {
    src,
    dst,
    port,
    url,
    interval,
    sock,
    recv_count
    }).

start(Args) ->
    %error_logger:info_msg("WJY: start client args ~p~n", [Args]),
    gen_fsm:start_link(?MODULE, Args, []).

init({SrcIP, DestIP, Port, URL, Interval}) ->
    {ok, tcpconn, #state{src = SrcIP, dst = DestIP,
                        port = Port, url = URL,
                        interval = Interval, recv_count = 0}, 0}.

tcpconn(timeout, State = #state{
                        src = SrcIP,
                        dst = DestIP,
                        port = Port,
                        url = URL,
                        interval = Interval,
                        sock = Sock}) ->
    case Sock of
        undefined ->
            NewSock = connect(SrcIP, DestIP, Port),
            case Interval of
                0 ->
                    send(NewSock, URL),
                    {next_state, waitrecv, State#state{sock = NewSock}};
                _ ->
                    send(NewSock, URL),
                    gen_fsm:send_event_after(Interval, timeout),
                    {next_state, tcpconn, State#state{sock = NewSock}}
            end;
        _ ->
            send(Sock, URL),
            gen_fsm:send_event_after(Interval, timeout),
            {next_state, tcpconn, State}
    end.

waitrecv(_, State) ->
    {next_state, waitrecv, State}.

connect(SrcIP, DestIP, Port) ->
    error_logger:info_msg("src ~p, dst ~p, port ~p~n", [SrcIP, DestIP, Port]),
    % case gen_tcp:connect(DestIP, Port, [{ip, SrcIP},
    %                                     {active, 10},
    %                                     {keepalive, true} % WJYTODO
    %                                     ]) of
    case gen_tcp:connect(DestIP, Port, [{ip, SrcIP}]) of
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

handle_info(_Info, waitrecv, #state{sock = Sock}) ->
    gen_tcp:close(Sock),
    {stop, normal, ok};
handle_info(_Info, StateName, State = #state{sock = Sock, recv_count = Count}) ->
    %error_logger:info_msg("WJY: received: ~p, time: ~p~n", [Info, os:timestamp()]),
    if
        Count >= 9 ->
            inet:setopts(Sock, [{active, 10}]),
            NewCount = 0;
        true ->
            NewCount = Count + 1
    end,
    {next_state, StateName, State#state{recv_count = NewCount}}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_Old, StateName, State, _Extra) ->
    {ok, StateName, State}.
