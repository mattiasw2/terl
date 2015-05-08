%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

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
    lager:info("terl started",[]),
    terl_sup:start_link().

-spec stop(_) -> 'ok'.
stop(_State) ->
    lager:info("terl stopped",[]),
    ok.
