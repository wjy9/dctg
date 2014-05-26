-module(dctg_start_launcher).

-behaviour(gen_server).

-export([start_link/0, newbeams/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

newbeams(HostList) ->
    gen_server:cast(?MODULE, {newbeams, HostList}).

init([]) ->
    {ok, []}.

handle_cast({newbeams, HostList}, State) ->
    SysArgs = "-rsh ssh -detached -hidden -smp disable +P 500000 +K true -setcookie " ++ atom_to_list(erlang:get_cookie()),
    %WJYTODO PATH
    %other args: -boot xxx -boot_var path/xxx  +A 16 -kernel xxxxx
    Path1 = "~/dctg/apps/dctg/ebin/",
    Path2 = "~/dctg/apps/dctg_worker/ebin/",
    Args = SysArgs ++ " -s dctg startworker -dctg_worker controller "
        ++ atom_to_list(node()) ++ " -pa " ++ Path1 ++ " -pa " ++ Path2,
    error_logger:info_msg("Args: ~p~n", [Args]),
    {HostIDList, _A} = lists:mapfoldl(fun(Host, Acc) -> {{Host, Acc}, Acc + 1}end, 0, HostList),
    Fun = fun({Host, ID}) -> remote_launcher(Host, ID, Args) end,
    RemoteNodes = utils:pmap(Fun, HostIDList),
    {T1, T2, T3} = os:timestamp(),
    StartTime = {T1, T2 + 5, T3}, % WJY hardcoded start time, 5s after all launcher started
    error_logger:info_msg("WJY: start time: ~p~n, Nodes: ~p~n", [StartTime, RemoteNodes]),
    StartLaunchers = fun(Node) -> dctg_launcher:launch({Node, StartTime}) end,
    lists:foreach(StartLaunchers, RemoteNodes),
    {stop, normal, State}.

remote_launcher(Host, ID, Args) ->
    Name = list_to_atom("launcher" ++ integer_to_list(ID)),
    start_slave(Host, Name, Args).

start_slave(Host, Name, Args) ->
    case slave:start(Host, Name, Args) of
        {ok, Node} ->
            case net_kernel:connect_node(Node) of
                true ->
                    error_logger:info_msg("WJY: connect_node OK~n");
                _Else ->
                    error_logger:info_msg("WJY: connect_node fail~n")
            end,
            Node;
        {error, Reason} ->
            exit({slave_fail, Reason})
    end.

code_change(_Old, State, _Extra) ->
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, error, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
