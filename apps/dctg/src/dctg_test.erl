-module(dctg_test).

-export([run/0]).

run() ->
    Hosts = [
            localhost
    ],
    IPPropList = [{localhost, ["127.0.0.1", "127.0.0.1", "127.0.0.1"]}],
    DutStartIP = "127.0.0.1",
    DutNum = 1,
    Type = http,
    Intensity = 1200, % conn/s
    ConnCount = 4800,

    LauncherNum = 3,
    Port = 80, % WJYTODO: not implemented
    Content = "/index.html", % WJYTODO: not implemented
    Interval = 60, % WJYTODO: not implemented
    dctg_frontend:set_hostip(Hosts, IPPropList),
    dctg_frontend:total(LauncherNum),
    dctg_frontend:config(DutStartIP, DutNum, Type, Intensity, ConnCount, LauncherNum, Port, Content, Interval),
    dctg_controller:start_launchers().
