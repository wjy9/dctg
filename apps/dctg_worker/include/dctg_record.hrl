-record(launcher,
    {
    intensity, % launch client per ms
    count, % total count of clients
    dest,
    interval, % scheduled time interval
    start_time,
    content,
    fraction,
    round,
    nth % the next dest ip used is the nth of dest list
    }).

-record(config, {
    dut = {127, 0, 0, 1}, % ip of dut
    dutnum,
    dutlist, % ip list of dut
    type, % type of test, should be raw or http
    intensity, % tcp conn per ms per launcher
    count, % total number of connections
    protocol % protocol specific config
    }).

-record(http, {
    port = 80,
    content, % URL
    interval, % download interval, ms
    }).