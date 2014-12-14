-module(terl).

-export([start/0,stop/0]).

-spec start() -> 'ok' | {'error',_}.
start() ->
    lager:start(),
    application:start(pd).

-spec stop() -> 'ok' | {'error',_}.
stop() ->
    %% there is no lager:stop(),
    application:stop(pd).
