%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(te_experiments).

-compile(export_all).

-include("../include/core_parse.hrl").

-export([]).

%%% the record definitions found at are defined at
%%% https://github.com/erlang/otp/blob/172e812c491680fbb175f56f7604d4098cdc9de4/lib/compiler/src/core_parse.hrl
%%% accessors to cerl: http://erldocs.com/17.1/compiler/cerl.html

%%% why isn't core_pp:format/1 working as I hope?
%%% http://rsaccon.blogspot.se/2008/03/diving-into-core-erlang.html
%%% https://github.com/seancribbs/erlyjs
%%% https://github.com/extend/xerl



read(std, Print) ->
    {ok, _, R} = ec_compile:erl_source_to_core_ast("std.erl"),
    case Print of
        true  -> io:format("~p",[R]), ok;
        false -> R
    end.

%% not working, create core file instead like this:
%%     X = compile:file("std.erl",[to_core]).
%%
read2(std, Print) ->
    {ok, _, R} = ec_compile:erl_source_to_core_ast("std.erl"),
    true = cerl:is_c_module(R),
    case Print of
        true  -> core_pp:format(R), io:write(R);
        false -> R
    end.
