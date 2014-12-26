%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

%% todo: more types, see http://www.erlang.org/doc/reference_manual/typespec.html
%% -spec foo(T1, T2) -> T3 ; (T4, T5) -> T6.

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


%%% trying to understand the core
-spec t1(string()) -> term().
t1(File) ->
    {ok, Module} = dialyzer_utils:get_core_from_src(File),
    _Name = Module#c_module.name,
    _Exports = Module#c_module.exports,
    Attrs = Module#c_module.attrs,
    Defs = Module#c_module.defs,
    TypeAndSpecs = get_spec_and_types(Attrs),
    Funs = get_functions(Defs, TypeAndSpecs),
    lists:map(fun(F) -> generate_ocaml(F, TypeAndSpecs) end, Funs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% get the ocaml abstract code

generate_ocamls(Cs, TAS) -> lists:map(fun(C) -> generate_ocaml(C, TAS) end, Cs).

generate_ocaml({letrec, Fun, Args, Body}, TAS) ->
    OcamlHead = make_head(Fun, Args, TAS),
    OcamlBody = generate_ocaml(Body, TAS),
    {letrec2, OcamlHead, OcamlBody};
generate_ocaml({match, {case_values, Match}, Matches}, TAS) ->
    %% todo: matches
    Matches2 = generate_matches(Matches, TAS),
    {match2, fix_variables(Match), Matches2};
generate_ocaml({'let', {'=', Vars, Arg}, Body}, TAS) ->
    {let2, {'=2', fix_variables(Vars), generate_ocaml(Arg, TAS)}, generate_ocaml(Body, TAS)};
generate_ocaml({mktuple, Args}, TAS) ->
    PV = is_polymorphic_variant_tuple(Args, TAS),
    case PV of
        {some, Res} -> Res;
        none -> {mktuple2, generate_ocamls(Args, TAS)}
    end;
generate_ocaml({variable,_Name}=V, _TAS) -> fix_variable(V);
generate_ocaml({literal,Atom}=V, _TAS) when is_atom(Atom) -> fix_polymorphic_variant(V);
generate_ocaml({literal,Number}=_V, _TAS) when is_number(Number) -> Number * 1.0;
generate_ocaml(true, _) -> true;
generate_ocaml(Keep, _TAS) ->
    {todo, Keep}.

%%% if tuple should be converted to ocaml polymorphic variant,
%%% return {some, PV}, else return none.
is_polymorphic_variant_tuple([], _) -> none;
is_polymorphic_variant_tuple([{literal, Atom}|T], TAS) when is_atom(Atom) ->
    %% todo: should I check in TAS?
    {some, {polymorphic_variant, fix_polymorphic_variant(Atom), generate_ocamls(T, TAS)}};
is_polymorphic_variant_tuple(_, _) -> none.


generate_matches(L, TAS) -> lists:map(fun(M) -> generate_match(M, TAS) end, L).

generate_match({'match|', Pattern, When, Body}, TAS) ->
    {'match|2', generate_ocaml_pattern(Pattern, TAS), generate_ocaml(When, TAS), generate_ocaml(Body, TAS)}.

generate_ocaml_pattern(Pattern, _TAS) ->
    {todo, Pattern}.









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% make casing legal ok for ocaml
fix_function_name({'/',F,N}) -> atom_to_list(F) ++ "_" ++ integer_to_list(N).

fix_variables(L) -> lists:map(fun fix_variable/1, L).
fix_variable ({variable, V}) -> make_first_lower(V);
fix_variable ({c_var,_ , V}) -> make_first_lower(V).

%%% bit unlogical, for some, I have kept the {variable / {literal stuff, sometimes not
fix_polymorphic_variant(Atom) when is_atom(Atom) -> "`" ++ make_first_upper(Atom).



name_args(Args) -> lists:map(fun name_arg/1, Args).
name_arg (Arg ) -> make_first_lower(Arg).

make_first_lower(Arg) when is_atom(Arg) -> [H|T] = atom_to_list(Arg), string:to_lower([H]) ++ T.
make_first_upper(Arg) when is_atom(Arg) -> [H|T] = atom_to_list(Arg), string:to_upper([H]) ++ T.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the head as pseudo-ocaml

make_head({'/',_F,_N}=Fun, Args, TAS) ->
    {head2, fix_function_name(Fun), name_args(Args), function_return_type(Fun, TAS)}.

function_return_type({'/',_F,_N}=_Fun, _TAS) ->
    %% todo
    [].


get_functions(Defs, TAS) ->
    lists:map(fun(Def) -> get_function(Def, TAS) end, Defs).

get_function({#c_var{name = {F, N}}, #c_fun{vars = Vars, body = Body}}, TAS) ->
    Fun = {'/', F, N},
    Args = lists:map(fun(Arg) -> get_arg(Arg, TAS) end, Vars),
    Def = compile_body(Body, TAS),
    LetRec = {'letrec', Fun, Args, Def},
    optimize_letrec(LetRec).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% convert the #c_ records to our structure
%%% (I am not sure this step should be kept, I have it now to see
%%% what/how the #c_ structure is used)

%%% get the header of the function
%%% todo: should I add the type info for args and return?
get_arg(#c_var{name = Name}, _TAS) -> Name.

compile_bodies(Bodies, TAS) ->
    lists:map(fun(Body) -> compile_body(Body, TAS) end, Bodies).

%%% compile the body into ocaml abstract code
compile_body(#c_map{arg = Map, es = Es}, TAS) ->
    %% create1() ->  #{age => 10}.
    Map2 = compile_body(Map, TAS),
    Op = lists:map(fun(E) -> compile_map_arg(E, TAS) end, Es),
    {map, Map2, Op};
compile_body(#c_case{arg = Arg, clauses = Clauses}, TAS) ->
    Res = {match, get_case_values(Arg, TAS), compile_clauses(Clauses, TAS)},
    optimize_match(Res);
compile_body(#c_let{vars = Vars, arg = Arg, body = Body}, TAS) ->
    {'let', {'=', Vars, Arg}, compile_body(Body, TAS)};
compile_body(#c_letrec{defs = _Defs, body = _Body}, _TAS) ->
    %% todo: it looks as if defs is a list var-fun pairs, and body is a normal.
    %% todo: for now, just return nil
    %% used in list_comprehension
    {literal,[]};
compile_body(#c_tuple{es = Es}, TAS) ->
    {'mktuple', compile_bodies(Es, TAS)};
compile_body(#c_cons{hd = Hd, tl = Tl}, TAS) ->
    {'mkcons', compile_body(Hd, TAS), compile_body(Tl, TAS)};
compile_body(#c_literal{val = Literal}, _TAS) ->
    %% todo: when are we going to fix conversion to ocaml casing?
    {literal, Literal};
compile_body(#c_var{name = Name}, _TAS) ->
    %% todo: when are we going to fix conversion to ocaml casing?
    {variable, Name};
compile_body(#c_call{module = Module, name = Name, args = Args}, TAS) ->
    {'call', {'/', Module#c_literal.val, Name#c_literal.val, length(Args)}, compile_bodies(Args, TAS)};
compile_body(#c_apply{op = Op, args = Args}, TAS) ->
    {'apply', compile_body(Op, TAS), compile_bodies(Args, TAS)};
compile_body(#c_try{body = Body}, TAS) ->
    %%% todo: handle try-catch for real
    compile_body(Body, TAS);
compile_body(#c_seq{}, _TAS) ->
    %%% todo: handle list comprehension, for now, return empty list
    {literal,[]}.

compile_map_arg(#c_map_pair{op = #c_literal{val = Op}, key = Key, val = Val}, TAS) ->
    Key2 = compile_body(Key, TAS),
    Val2 = compile_body(Val, TAS),
    case Op of
        exact -> {exact, Key2, Val2};           % change1(M) -> M#{age := 20}. :=, replace existing value
        assoc -> {assoc, Key2, Val2}            % upsert1(M) -> M#{name => "mattias"}.=>, add or replace
    end.

%%% the erlang core always has a top-level case
optimize_match({match,{case_values,[]},[{'match|',[],true,Body}]}) ->
    %% simplify case where no arguments
    Body;
optimize_match(Keep) -> Keep.


%%% simplify the core erlang to make the generated code more similar to the original erlang code
optimize_letrec({letrec,FunName,[cor0],
                 {match,{case_values,[{variable,cor0}]}, %added case_values [..] due to get_case_values
                  [{'match|',[{pattern_var,VarName}],true,Body}]}}) ->
    %% keep the original variable names if single clause (1 arg)
    {letrec,FunName,[VarName],Body};
optimize_letrec({letrec,FunName,[cor1,cor0],
                 {match,{case_values,[{variable,cor1},{variable,cor0}]},
                  [{'match|',[{pattern_var,VarName1},{pattern_var,VarName0}],true,Body}]}}) ->
    %% keep the original variable names if single clause (2 args)
    %% extend to 3, 4, .. args
    {letrec,FunName,[VarName1,VarName0],Body};
optimize_letrec(Keep) -> Keep.




%%% two levels to detect if this is arbitralily recursive.
%%% erlang core questions, why did you insert #c_values?
get_case_values(#c_values{es = Es},    TAS) -> {case_values, lists:map(fun(E) -> compile_body(E, TAS) end, Es)};
get_case_values(V,                     TAS) -> {case_values, [compile_body(V, TAS)]}. %ok?????



%%% compile the clauses, skip the match_fail, since we want the ocaml compiler to complain
%%% if pattern not complete.
compile_clauses([], _TAS) -> [];
compile_clauses([#c_clause{anno = [compiler_generated]}|Cs], TAS) -> compile_clauses(Cs, TAS);
compile_clauses([#c_clause{pats = Pats, guard = Guard, body = Body}|Cs], TAS) ->
    [{'match|', compile_patterns(Pats, TAS), compile_when(Guard, TAS), compile_body(Body, TAS)}
     |compile_clauses(Cs, TAS)].


compile_patterns(Pats, TAS) ->
    lists:map(fun(Pat) -> compile_pattern(Pat, TAS) end, Pats).

compile_pattern(#c_cons   {hd = Hd, tl = Tl},   TAS) -> {pattern_cons, compile_pattern(Hd, TAS), compile_pattern(Tl, TAS)};
compile_pattern(#c_literal{val = Val},         _TAS) -> {pattern_literal, Val};
compile_pattern(#c_tuple  {es = ES},            TAS) -> {pattern_tuple, compile_patterns(ES, TAS)};
compile_pattern(#c_var    {name = Name},       _TAS) -> {pattern_var, Name};
compile_pattern(#c_map    {arg = _Arg, es = _ES}, _TAS) ->
    %% access1(#{age := Age} = Person) -> Age.
    %% es :: [#c_map_pair()]
    %% todo: handle c_map_pair
    {pattern_map};
compile_pattern(#c_alias  {var = Var, pat = Pat}, TAS) ->
    %%% todo: MFA is alias in "warn_spec_missing_fun({M, F, A} = MFA, Contracts) ->"
    {alias, Var, compile_pattern(Pat, TAS)}.



compile_when(#c_literal{val = true}, _TAS) -> true;
compile_when(#c_literal{val = Guard}, TAS) -> compile_body(Guard, TAS);
compile_when(#c_call{} = Body,        TAS) -> compile_body(Body, TAS).


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
to_ocaml({type        , {{record, Name}, Fields, []}} ) -> {named_tuple, {atom, Name},
                                                            lists:map(fun generate_field/1, Fields)};
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
