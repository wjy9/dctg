-module(dctg_config_server).

-behaviour(gen_server).

-export([start_link/0, get_config/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_config(Node) ->
    gen_server:call({?MODULE, Node}, {get_config}).

init([]) ->
    {ok, {1, 3000, {127, 0, 0, 1}, {http, "http://127.0.0.1:8080/"}}}. % {Intensity, Count, Dest, Content}

handle_call({get_config}, _From, State) ->
    error_logger:info_msg("WJY: get config~n"),
    {reply, State, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_Old, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.