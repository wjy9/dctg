-module(dctg_frontend).

-export([total/1, config/9, set_hostip/2]).

-include("config.hrl").

% set total amount of launchers
total(Num) ->
    dctg_config_server:set_total(Num).

set_hostip(HostList, IPPropList) ->
    Fun = fun(Host, AccIn) -> mapfoldfun(Host, AccIn, IPPropList) end,
    {Hosts, IPList1} = lists:mapfoldl(Fun, [], HostList),
    NewHost = lists:flatten(Hosts),
    IPList = lists:map(fun(IP) -> ipstring_to_tuple(IP) end, IPList1),
    IPArray = array:from_list(IPList),
    %error_logger:info_msg("frontend: ~p, ~p~n", [NewHost, IPArray]),
    dctg_config_server:set_hostip(NewHost, IPArray).

mapfoldfun(H, AccIn, IPPropList) ->
    IPList = proplists:get_value(H, IPPropList),
    H2 = lists:duplicate(length(IPList), H),
    AccOut = lists:append(AccIn, IPList),
    {H2, AccOut}.

ipstring_to_tuple(IP) when is_list(IP) ->
    List = string:tokens(IP, "."),
    NewList = lists:map(fun(A) -> list_to_integer(A) end, List),
    list_to_tuple(NewList).

make_iplist({IP1, IP2, IP3, IP4}, Num) ->
    make_iplist([{IP1, IP2, IP3, IP4}], Num - 1);
make_iplist(List, 0) ->
    lists:reverse(List);
make_iplist(List = [{IP1, IP2, IP3, IP4} | _Tail], Num) ->
    make_iplist([{IP1, IP2, IP3, IP4 + 1} | List], Num - 1).

config(IP, Num, Type, Intensity, Count, LaunchNum, Port, Content, Interval) when Type =:= http ->
    Http = #http{port = Port, content = Content, interval = Interval * 1000},
    Intensity2 = Intensity / 1000 / LaunchNum, % user input intensity is per second, convert it to per ms per launcher
    if
        Intensity2 > 10 ->
            dctg_config_server:set_launcher_per_ip(3),
            NewIntensity = Intensity2 / 3,
            LaunchNum2 = LaunchNum * 3,
            ok;
        Intensity2 > 6 ->
            dctg_config_server:set_launcher_per_ip(2),
            NewIntensity = Intensity2 / 2,
            LaunchNum2 = LaunchNum * 2,
            body;
        true ->
            NewIntensity = Intensity2,
            LaunchNum2 = LaunchNum
    end,
    NewCount = trunc(Count / LaunchNum2),
    CountArr1 = array:new([{size, LaunchNum2}, {fixed, true}, {default, NewCount}]),
    CountArr = calc_count_array(CountArr1, Count rem LaunchNum2, 0, LaunchNum2),
    IPT = ipstring_to_tuple(IP),
    IPList = make_iplist(IPT, Num),
    IPTuple = list_to_tuple(IPList),
    Config = #config{dut = IP, dutnum = Num, dutlist = IPTuple, type = Type, intensity = NewIntensity, protocol = Http},
    dctg_config_server:set_config(Config, CountArr).

calc_count_array(Array, Num, _I, _Size) when Num =< 0 ->
    Array;
calc_count_array(Array, Num, I, Size) when I >= Size ->
    calc_count_array(Array, Num, 0, Size);
calc_count_array(Array, Num, I, Size) ->
    Value = array:get(I, Array),
    Array2 = array:set(I, Value + 1, Array),
    calc_count_array(Array2, Num - 1, I + 1, Size).