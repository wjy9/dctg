-module(dctg_frontend).

-export([total/1, start/1, start/2, config/6, config/9, set_hostip/2]).

-include("config.hrl").

% set total amount of launchers
total(Num) ->
    dctg_config_server:set_total(Num).

set_hostip(HostList, IPPropList) ->
    Fun = fun(Host, AccIn) -> mapfoldfun(Host, AccIn, IPPropList) end,
    {Hosts, IPList} = lists:mapfoldl(Fun, [], HostList),
    NewHost = lists:flatten(Hosts),
    IPArray = array:from_list(IPList),
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
make_iplist(List = [{IP1, IP2, IP3, IP4} | Tail], Num) ->
    make_iplist([{IP1, IP2, IP3, IP4 + 1} | List], Num - 1).

config(IP, Num, Type, Intensity, Count, LaunchNum) ->
    NewIntensity = Intensity / 1000 / LaunchNum, % user input intensity is per second, convert it to per ms per launcher
    NewCount = round(Count / LaunchNum),
    IPT = ipstring_to_tuple(IP),
    IPList = make_iplist(IPT, Num),
    IPTuple = list_to_tuple(IPList),
    Config = #config{dut = IP, dutnum = Num, dutlist = IPTuple, type = Type, intensity = NewIntensity, count = NewCount},
    dctg_config_server:set_config(Config).

config(IP, Num, Type, Intensity, Count, LaunchNum, Port, Content, Interval) when Type =:= http ->
    Http = #http{port = Port, content = Content, interval = Interval * 1000},
    NewIntensity = Intensity / 1000 / LaunchNum, % user input intensity is per second, convert it to per ms per launcher
    NewCount = round(Count / LaunchNum),
    IPT = ipstring_to_tuple(IP),
    IPList = make_iplist(IPT, Num),
    IPTuple = list_to_tuple(IPList),
    Config = #config{dut = IP, dutnum = Num, dutlist = IPTuple, type = Type, intensity = NewIntensity, count = NewCount, protocol = Http},
    dctg_config_server:set_config(Config).
