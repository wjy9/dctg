-module(send_raw_packet).
-export([getMACByPing/3, getMAC/3, send/0,
        get_src_mac/1, get_ip_by_ping/1,
        make_rawpkt/3]).

-define(ETHER_BROADCAST, <<16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF>>).
-define(ETHER_UNKNOWN, <<16#00, 16#00, 16#00, 16#00, 16#00, 16#00>>).
-define(ETH_P_ARP, 16#0806). %native endian
-define(ETH_P_IP, 16#0800).  %native endian
-define(ETH_P_ALL, 16#0003). %native endian

-define(ARPOP_REQUEST, 1).
-define(ARPOP_REPLY, 2).


getMAC(SrcDev, SrcIP, DstIP) ->
    %% lookup the MAC of "eth0"
    {ok, PL} = inet:ifget(SrcDev, [hwaddr]),
    SrcMAC = list_to_binary(proplists:get_value(hwaddr, PL)),
    
    %% open a PF_PACKET raw socket with ETH_P_ARP
    {ok, SocketARP} = packet:socket(?ETH_P_ARP),
    Ifindex = packet:ifindex(SocketARP, SrcDev),

    %% use ARP to request the destination MAC
    ok = packet:send(SocketARP, Ifindex, make_arp(?ARPOP_REQUEST, SrcMAC, SrcIP, DstIP)),
    packet:close(SocketARP),

    %% lookup the destination MAC in ARP cache
    DstMAC = packet:arplookup(DstIP),

    case DstMAC of
        false -> false;
        {0,0,0,0,0,0} -> false;
        _ -> [list_to_binary(tuple_to_list(DstMAC)), SrcMAC]
    end.

getMACByPing(SrcDev, SrcIP, DstIP) ->
    %% lookup the MAC of "eth0"
    {ok, PL} = inet:ifget(SrcDev, [hwaddr]),
    SrcMAC = list_to_binary(proplists:get_value(hwaddr, PL)),
    
    %% ping DstIP, returns ok or noresponse
    PR = icmp:ping(DstIP),

    DstMAC = packet:arplookup(DstIP),

    case DstMAC of
        false -> false;
        {0,0,0,0,0,0} -> false;
        _ -> {list_to_binary(tuple_to_list(DstMAC)), SrcMAC}
    end.

get_src_mac(SrcDev) ->
    {ok, PL} = inet:ifget(SrcDev, [hwaddr]), %WJYWARN: inet:ifget/2 is a abandoned function, use getifaddrs/0 instead
    list_to_binary(proplists:get_value(hwaddr, PL)).

get_ip_by_ping(IP) ->
    icmp:ping(IP),
    case packet:arplookup(IP) of
        false -> false;
        {0,0,0,0,0,0} -> false;
        Mac -> list_to_binary(tuple_to_list(Mac))
    end.

send() ->
    %% definition of local IP and destination IP
    SrcDev = "eth0",
    SrcIP = {192,168,200,2},
    DstIP = {192,168,200,4},

    {DstMAC, SrcMAC} = getMACByPing(SrcDev, SrcIP, DstIP), 
    %% open a PF_PACKET raw socket with ETH_P_ALL
    {ok, Socket} = packet:socket(?ETH_P_ALL),
    Ifindex = packet:ifindex(Socket, SrcDev),

    %%send out packet, by either "packet:send/3" or "packet:bind/2 then procket:sendto/2)"
    %%packet:send(Socket, Ifindex, <<DstMAC:6/bytes, SrcMAC:6/bytes, ?ETH_P_IP:16, 9,8,7,6,5,4,3,2,1,16#FF,16#EE,16#DD,16#CC,16#BB,16#AA>>),
    ok = packet:bind(Socket, Ifindex),
    loop(Socket, DstMAC, SrcMAC, 10).

loop(_, _, _, N) when N<1 -> 
    ok;
loop(S, Dha, Sha, N) ->
    procket:sendto(S, <<Dha:6/bytes, Sha:6/bytes, ?ETH_P_IP:16, 9,8,7,6,5,4,3,2,1,16#FF,16#EE,16#DD,16#CC,16#BB,16#AA>>),
    loop(S, Dha, Sha, N-1).

make_rawpkt(Sha, Dha, PktData) ->
    <<
    Dha:6/bytes,                % target hardware address
    Sha:6/bytes,                % source hardware address
    ?ETH_P_IP:16,               % type, set to IP but actually raw data afterword
    PktData
    >>.

make_arp(Type, Sha, {SA1,SA2,SA3,SA4}, {DA1, DA2, DA3, DA4}) ->
    Ether = <<
    ?ETHER_BROADCAST:6/bytes,   % target hardware address set to FF:FF:FF:FF:FF:FF
    Sha:6/bytes,                % source hardware address
    ?ETH_P_ARP:16               % type
    >>,

    Arp = <<
    1:16,                       % hardware type
    ?ETH_P_IP:16,               % protocol type
    6:8,                        % hardware length
    4:8,                        % protocol length
    Type:16,                    % operation
    Sha:6/bytes,                % source hardware address
    SA1:8, SA2:8, SA3:8, SA4:8, % source IP address
    ?ETHER_UNKNOWN:6/bytes,     % target hardware address set to 00:00:00:00:00:00
    DA1:8, DA2:8, DA3:8, DA4:8  % target IP address 
    >>,

    list_to_binary([Ether, Arp]).

