-module(dctg_test).

-export([run/0, raw/0]).

run() ->
    Hosts = [
            tester1,
            tester2,
            tester3,
            tester4
    ],
    IPPropList = [{tester1, ["10.0.0.3", "10.0.0.4", "10.0.0.5"]},
                {tester2, ["10.0.0.6", "10.0.0.7", "10.0.0.8"]},
                {tester3, ["10.0.0.9", "10.0.0.10", "10.0.0.11"]},
                {tester4, ["10.0.0.12", "10.0.0.13", "10.0.0.14"]}
    ],
    DutStartIP = "10.1.0.1",
    DutNum = 8,
    Type = http,
    Intensity = 60000, % conn/s
    ConnCount = 600000,

    LauncherNum = 12,
    Port = 80,
    Content = "/a.html",
    Interval = 0,
    NumPerIP = 2,
    dctg_frontend:set_hostip(Hosts, IPPropList),
    dctg_frontend:total(LauncherNum),
    dctg_frontend:config(DutStartIP, DutNum, Type, Intensity, ConnCount, LauncherNum, Port, Content, Interval, NumPerIP),
    dctg_controller:start_launchers().

raw() ->
    SrcDev = "eth1",
    SrcMac = send_raw_packet:get_src_mac(SrcDev),
    Path = procket_mktmp:name("/tmp/procket_sock_XXXXXXXXXXXX"),
    {ok, Socket} = procket:open(0, [{protocol, procket:ntohs(16#0003)},
                                    {family, packet}, {type, raw},
                                    {pipe, Path}]),
    Ifindex = packet:ifindex(Socket, SrcDev),
    ok = packet:bind(Socket, Ifindex),
    DstIP = {192,168,1,6},
    DstMac = send_raw_packet:get_ip_by_ping(DstIP),
    procket:sendto(Socket, send_raw_packet:make_rawpkt(SrcMac, DstMac, 16#ffffabcd12345678)).