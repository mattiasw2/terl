-module(terl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(_,_) -> {'error',_} | {'ok',pid()}. %| {'ok',pid(),_}.
start(_StartType, _StartArgs) ->
    lager:start(),
    edts_log:info("terl started"),
    shc_sup:start_link().

-spec stop(_) -> 'ok'.
stop(_State) ->
    edts_log:info("terl stopped"),
    ok.
