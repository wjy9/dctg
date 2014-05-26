-module(dctg_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %error_logger:tty(false),
    %error_logger:logfile({openfile, "./" ++ atom_to_list(node()) ++ ".log"}),
    error_logger:info_msg("ahahaha~n", []),
    dctg_sup:start_link().

stop(_State) ->
    ok.
