# Data Center Based Test Traffic Generator

## compile

```
./rebar compile
```

## run

```
erl -setcookie xxx -sname xxx@localhost -pa apps/dctg/ebin -pa apps/dctg_worker/ebin -s dctg -rsh ssh
```

then in the erl shell:

```erlang
dctg_controller:start_launchers([localhost, localhost]).
dctg_launcher:launch({launcher0@localhost, os:timestamp()}).
```
