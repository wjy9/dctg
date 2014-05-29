-module(dctg_test).

-export([run/0]).

run() ->
    Hosts = [
            localhost
    ],
    DutStartIP = "127.0.0.1",
    DutNum = 1,
    Type = http,
    Intensity = 300, % conn/s
    ConnCount = 1200,
    WorkerPerVM = 3,

    LauncherNum = length(Hosts) * WorkerPerVM,
    Port = 80, % WJYTODO: not implemented
    Content = "/index.html", % WJYTODO: not implemented
    Interval = 60, % WJYTODO: not implemented
    dctg_frontend:total(LauncherNum),
    dctg_frontend:config(DutStartIP, DutNum, Type, Intensity, ConnCount, LauncherNum, Port, Content, Interval),
    dctg_frontend:start(Hosts, WorkerPerVM).
