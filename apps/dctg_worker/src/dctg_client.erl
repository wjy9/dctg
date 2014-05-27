-module(dctg_client).

-behaviour(gen_fsm).

-export([start/1]).

-export([init/1, handle_event/3, handle_sync_event/4, tcpconn/2,
        handle_info/3, terminate/3, code_change/4]).

start(Args) ->
    %error_logger:info_msg("WJY: start client args ~p~n", [Args]),
    gen_fsm:start_link(?MODULE, Args, []).

init(Content) ->
    %error_logger:info_msg("WJY: client: Content: ~p~n", [Content]),
    {ok, tcpconn, Content, 0}.

tcpconn(timeout, State) ->
    Host = "localhost",
    case gen_tcp:connect(Host, 80, []) of
        {ok, Sock} ->
            error_logger:info_msg("WJY: tcp connect success~n"),
            gen_tcp:send(Sock, State),
            ok;
        {error, Reason} ->
            error_logger:info_msg("WJY: client tcp connect fail, ~p~n", [Reason]),
            ok
    end,
    {stop, normal, State}.

handle_event(_Ev, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Ev, _From, StateName, State) ->
    {next_state, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_Old, StateName, State, _Extra) ->
    {ok, StateName, State}.
