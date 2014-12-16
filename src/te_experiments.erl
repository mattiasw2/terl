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
read2(File, Print) ->
    {ok, _, R} = ec_compile:erl_source_to_core_ast(File),
    %% true = cerl:is_c_module(R),
    case Print of
        true  -> core_pp:format(R), io:write(R);
        false -> R
    end.

%%% trying to understand the core
-spec t1(string()) -> term().
t1(File) ->
    {ok, Module} = dialyzer_utils:get_core_from_src(File),
    _Name = Module#c_module.name,
    _Exports = Module#c_module.exports,
    Attrs = Module#c_module.attrs,
    _Defs = Module#c_module.defs,
    get_spec_and_types(Attrs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% get specs and types without the c_literal stuff
get_spec_and_types(Attrs) ->
    L = lists:map(fun(Attr) -> get_spec_or_type(Attr) end, Attrs),
    L2 = lists:map(fun to_ocaml/1, L),
    L2.

%%% why is Type a list of 1 element? Can you group several -spec into one -spec?
get_spec_or_type({#c_literal{anno = _Anno,val = spec}, #c_literal{anno = _Anno2,val = [Type]}}) ->
    {spec, Type};
get_spec_or_type({#c_literal{anno = _Anno,val = type}, #c_literal{anno = _Anno2,val = [Type]}}) ->
    {type, Type};
get_spec_or_type({#c_literal{anno = _Anno,val = export_type}, #c_literal{anno = _Anno2,val = Types}}) ->
    %% -export_type([file_contract/0, plt_contracts/0]).
    {export_type, Types}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% convert record to ocaml
to_ocaml({type, {{record, _Name},_RecordType,_Fields} = Record}) -> Record;
to_ocaml({export_type, Types} = Record) -> Record;
to_ocaml({type, {_,_,_} = NamedType}) -> NamedType;
to_ocaml({spec, {{F,N}, [Type]}}) -> {spec, F, N, Type}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% eunit tests

a_test() ->
    te_experiments:t1("../samples/access1.erl").

b_test() ->
    te_experiments:t1("../samples/typed_map1.erl").

c_test() ->
    te_experiments:t1("../samples/map1.erl").

d_test() ->
    te_experiments:t1("../samples/poly1.erl").

e_test() ->
    te_experiments:t1("/home/mattias/erl-src/otp/lib/dialyzer/src/dialyzer_contracts.erl").
