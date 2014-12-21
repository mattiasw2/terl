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
    Defs = Module#c_module.defs,
    TypeAndSpecs = get_spec_and_types(Attrs),
    get_functions(Defs, TypeAndSpecs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% get the ocaml abstract code
get_functions(Defs, TAS) ->
    Defs.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% get specs and types without the c_literal stuff
get_spec_and_types(Attrs) ->
    L = lists:map(fun(Attr) -> get_spec_or_type(Attr) end, Attrs),
    L2 = lists:map(fun to_ocaml/1, L),
    L2.

%%% why is Type a list of 1 element? Can you group several -spec into one -spec?
get_spec_or_type({ #c_literal{anno = _Anno,val = spec}        , #c_literal{anno = _Anno2,val = [Type]}}) ->
    {spec, Type};
get_spec_or_type({ #c_literal{anno = _Anno,val = type}        , #c_literal{anno = _Anno2,val = [Type]}}) ->
    {type, Type};
get_spec_or_type({ #c_literal{anno = _Anno,val = export_type} , #c_literal{anno = _Anno2,val = Types }}) ->
    %% -export_type([file_contract/0, plt_contracts/0]).
    {export_type, Types}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% convert record to ocaml
%%% playing with M-x align-regexp with ',' and '=' and ') ->'
to_ocaml({type        , {{record, Name}, Fields, []}} ) -> {named_tuple, {atom, Name}, lists:map(fun generate_field/1, Fields)};
to_ocaml({type        , {Name,Type,[]}     }          ) -> {type, Name, generate_type(Type)};
to_ocaml({export_type , _Types}= Record               ) -> Record;
to_ocaml({spec        , {{F,N}, [Type]}}              ) -> {spec, F, N, generate_type(Type)}.


generate_field({typed_record_field,{record_field,_Row,{atom,_Row2, _FieldName},_Default},Type}) ->
    %% Default is {string,2,"name_default"}
    %% Do I need to bother about name? Maybe, when I refer to it?
    generate_type(Type);
generate_field({typed_record_field,{record_field,_Row,{atom,_Row2, _FieldName}         },Type}) ->
    generate_type(Type).


generate_type(Type) ->
    generate_type(Type, []).

generate_type({type, _Row, 'fun', [From, To]}, Constraints) ->
    FromType = generate_type(From, Constraints),
    ToType = generate_type(To, Constraints),
    {'->', FromType, ToType};
generate_type({type, _Row, product, Types}, Constraints) ->
    {'*', lists:map(fun(T) -> generate_type(T, Constraints) end, Types)};
generate_type({type, _Row, tuple, Types}, Constraints) ->
    maybe_named_tuple({tuple, lists:map(fun(T) -> generate_type(T, Constraints) end, Types)});
generate_type({type, _Row, record, [Type]}, Constraints) ->
    {record_type, generate_type(Type, Constraints)};
generate_type({type, _Row, list, [Type]}, Constraints) ->
    {list, generate_type(Type, Constraints)};
generate_type({type, _Row, nonempty_list, [Type]}, Constraints) ->
    {nonempty_list, generate_type(Type, Constraints)};
generate_type({type, _Row, Primitive, []}, _Constraints) ->
    %% todo: check that primitive is string, integer ....
    Primitive;
generate_type({type,_Row1,bounded_fun,[{type,_Row2,'fun',[From, To]}, Constraints]}, TopConstraints) ->
    %% I do not plan for more than one bounded_fun expression for now
    [] = TopConstraints,
    FromType = generate_type(From, Constraints),
    ToType = generate_type(To, Constraints),
    {'->',FromType, ToType};
generate_type({type,_Row,union,Types}, Constraints) ->
    {'|', lists:map(fun(T) -> generate_type(T, Constraints) end, Types)};
generate_type({type,_Row,map,Map_field_assoc}, Constraints) ->
    Types = lists:map(fun(T) -> generate_type(T, Constraints) end, Map_field_assoc),
    case is_record_like_map(Types) of
        true  -> {map_record_like, Types};
        false ->
            case length(Types) =:= 1 of
                true  -> {map_typed, Types};
                false -> {map_unsupported, Map_field_assoc}
            end
    end;
generate_type({type,_Row,map_field_assoc,From,To}, Constraints) ->
    {map_field_assoc, generate_type(From, Constraints), generate_type(To, Constraints)};
generate_type({integer, _Row, _ConstantValue}, _Constraints) ->
    %% Ignoring the actual value of the integer
    %% add more primitive constants here
    integer;
generate_type({var, _Row, Atom}, Constraints) ->
    expand_subtype(Atom, Constraints);
generate_type({atom, _Row, Atom}, _Constraints) ->
    {atom, Atom};
generate_type({remote_type, _Row, _} = Remote_type, _Constraints) ->
    %% _ [{atom,72,erl_types},{atom,72,erl_type},[]]
    {'%%%', Remote_type};
generate_type({paren_type,_Row,[Type]}, Constraints) ->
    generate_type(Type, Constraints).

is_record_like_map([]) -> false;
is_record_like_map(L) ->
    %% all of the key types must be atoms
    lists:all(fun(E) -> case E of
                            {map_field_assoc,{atom,_},_} -> true;
                            _ -> false
                        end
              end, L).


maybe_named_tuple({tuple,[{atom,Name}|Rest]}) -> {named_tuple, {atom, Name}, Rest};
maybe_named_tuple(Tuple                     ) -> Tuple.


expand_subtype(Var, Constraints) ->
    expand_subtype(Var, Constraints, Constraints).

expand_subtype('_', [], _Constraints) ->
    %% todo: recognize other term(), since then we cannot do type-checking
    any;
expand_subtype(Var, [], _Constraints) ->
    {polymorphic, Var};
expand_subtype(Var, [C|Cs], AllConstraints) ->
    case expand_subtype2(Var, C, AllConstraints) of
        error -> expand_subtype(Var, Cs, AllConstraints);
        {ok, Result} -> Result
    end.

expand_subtype2(Var, {type, _Row, constraint, SubTypes}, AllConstraints) ->
    expand_var(Var, SubTypes, AllConstraints).

expand_var(Var, [{atom,_Row,is_subtype}, [{var,_Row2,Var}, Type]], AllConstraints) ->
    {ok, generate_type(Type, AllConstraints)};
%% expand_var(_, [], _) ->
%%     error;
expand_var(_, _, _) ->
    error.


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
    %% generate_type({remote_type,72,[{atom,72,erl_types},{atom,72,erl_type},[]]},[])
    te_experiments:t1("/home/mattias/erl-src/otp/lib/dialyzer/src/dialyzer_contracts.erl").

e_read() ->
    {ok, Module} = dialyzer_utils:get_core_from_src("/home/mattias/erl-src/otp/lib/dialyzer/src/dialyzer_contracts.erl"),
    Module.
