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
    io:format("~n~p~n",[File]),
    {ok, Module} = dialyzer_utils:get_core_from_src(File),
    _Name = Module#c_module.name,
    _Exports = Module#c_module.exports,
    Attrs = Module#c_module.attrs,
    TypeAndSpecs = get_spec_and_types(Attrs),
    RootName = filename:rootname(filename:basename(File),".erl"),
    %% Defs = Module#c_module.defs,
    %% Funs = get_functions(Defs, TypeAndSpecs),
    %% Code=lists:map(fun(F) -> generate_ocaml(F, TypeAndSpecs) end, Funs),
    %% %% S = standard_io,
    %% {ok, S} = file:open("../ocaml/" ++ RootName ++ ".ml", [write, delayed_write]),
    %% io:put_chars(S, "let rec dummy_999() = true "), io:nl(S),
    %% lists:foreach(fun(C) -> print(S, C) end, Code),
    %% case S of
    %%     standard_io -> ok;
    %%     _ -> file:close(S)
    %% end,
    print_mli(RootName, TypeAndSpecs).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% print ocaml from syntax tree constructed by generated_ocaml

print(S, {letrec2, Head, Body}) ->
    io:put_chars(S," and "),
    print(S, Head),
    io:nl(S),
    print(S, Body),
    io:nl(S);
print(S, {head2, Name, Args, ReturnType}) ->
    print(S, Name),
    %% make tuple of args
    print_strings(S,Args,"(", ",", ")"),
    case ReturnType of
        [] -> ok;
        Chars -> io:put_chars(S," : "),
                 io:put_chars(S, Chars)
    end,
    io:put_chars(S," = ");
print(S, {let2, {'=2',Var,Expr}, Body}) ->
    io:put_chars(S, "let "),
    print(S, Var),
    io:put_chars(S, " = "),
    io:nl(S),
    print(S, Expr),
    io:put_chars(S, " in "),
    io:nl(S),
    print(S, Body);
print(S, {polymorphic_variant2, Name, Args}) ->
    print(S,Name),
    print_patterns(S,Args),
    io:nl(S);
print(S, {mktuple2, Args}) ->
    print_patterns(S,Args),
    io:nl(S);
print(S, {mknil2}) ->
    io:put_chars(S,"[]");
print(S, {mkcons2,H,T}) ->
    printsF(fun print/2, S, [H,T], "("," :: ",")");
print(S, {call_module2, M, F, Args}) ->
    print(S,M),
    io:put_chars(S,"."),
    print(S,F),
    prints(S,Args,"(",",",")");
print(S, {call2, F, Args}) ->
    print(S,F),
    prints(S,Args,"(",",",")");
print(S, {'fun_anon2', OcamlHead, OcamlBody}) ->
    %% (fun a -> a + 1)
    io:put_chars(S,"(fun "),
    print_strings(S,OcamlHead,"(",",",")"),
    io:put_chars(S," -> "),
    io:nl(S),
    print(S,OcamlBody),
    io:put_chars(S,")");
print(S, {'apply2', Op, Args}) ->
    io:put_chars(S, "(("),
    print(S, Op),
    printsF(fun print/2, S, Args, ")(",",","))");
print(S, {match2, Match, Matches}) ->
    io:put_chars(S,"(match "),
    %% print_strings not ok, since might contain call.
    prints(S,Match,"(",",",")"),
    io:put_chars(S," with"),
    io:nl(S),
    lists:foreach(fun(M) -> print_match(S,M) end, Matches),
    io:put_chars(S,")"),
    io:nl(S);
%% print(S, {variable, Name}) ->
%%     io:put_chars(S,Name);
print(S, {variable2, Name}) ->
    io:put_chars(S,Name);
%% print(S, {literal, Name}) ->
%%     io:put_chars(S,Name);
print(S, {literal2, Name}) ->
    io:put_chars(S,Name);
print(S, {literal_float2, String}) ->
    io:put_chars(S, String);
print(S, {literal_string2, String}) ->
    print_quoted_string(S, String);
print(S, {string, String}) ->
    io:put_chars(S, String);
print(S, String) when is_list(String) ->
    case String of
        []    -> io:put_chars(S,"[]");          % for safety, since I should have these
        [_|_] -> io:put_chars(S,String)
    end;
print(S, Skip) when is_tuple(Skip) ->
    io:format(S, "Skip#1 ~p~n", [element(1,Skip)]).
%% print(S, Skip) ->
%%     io:format(S, "Skip#2 ~p~n~p~n", [Skip, erlang:get_stacktrace()]).

prints(S, Strings, Begin, Delimeter, End) ->
    printsF(fun print/2, S, Strings, Begin, Delimeter, End).

print_quoted_string(S, String) ->
    io:put_chars(S,"\""),
    io:put_chars(S, re:replace(String,"\"","\\\\\"")),
    io:put_chars(S,"\"").

print_strings(S, Strings, Begin, Delimeter, End) ->
    printsF(fun io:put_chars/2, S, Strings, Begin, Delimeter, End).

print_patterns(S, Pattern) ->
    printsF(fun print_pattern/2, S, Pattern, "(",",",")").

print_pattern(S,{alias2, Var, Pattern}) ->
    End = ") as ",
    printsF(fun print_pattern/2, S, [Pattern], "((",",",End),
    print_pattern(S,Var),
    io:put_chars(S,")");
print_pattern(S,{mkcons2,H,T}) ->
    printsF(fun print_pattern/2, S, [H,T], "("," :: ",")");
print_pattern(S,{mknil2}) ->
    io:put_chars(S,"[]");
print_pattern(S,{polymorphic_variant2, Name, Args}) ->
    print_pattern(S, Name),
    print_patterns(S, Args);
print_pattern(S,{mktuple2, Args}) ->
    print_patterns(S, Args);
print_pattern(S, {literal_float2, String}) ->
    io:put_chars(S, String);
print_pattern(S, {string, String}) ->
    io:put_chars(S, String);
print_pattern(S, {literal_string2, String}) ->
    print_quoted_string(S, String);
%% print_pattern(S, String) when is_list(String) ->
%%     %% check why is this needed
%%     io:put_chars(S, String);
print_pattern(S, Float) when is_float(Float) ->
    %% maybe this is a erlang string?
    %% check why is this needed
    io:put_chars(S, mochinum:digits(Float)).


%%% generic printer of lists with delimiter and begin and end
printsF_scope(F, S, Strings, Begin, Delimeter, End, Scope) ->
    io:put_chars(S, Begin),
    printsF_scope(F, S, Strings, Delimeter, Scope),
    io:put_chars(S, End).

printsF_scope(_, _, [], _           , _Scope) -> ok;
printsF_scope(F, S, [H], _          , Scope ) -> F(S,H,Scope), ok;
printsF_scope(F, S, [H|T], Delimeter, Scope ) -> F(S,H,Scope), io:put_chars(S,Delimeter), printsF_scope(F,S,T,Delimeter,Scope).

%%% version without Scope
printsF(F, S, Strings, Begin, Delimeter, End) ->
    io:put_chars(S, Begin),
    printsF(F, S, Strings, Delimeter),
    io:put_chars(S, End).

printsF(_, _, [], _           ) -> ok;
printsF(F, S, [H], _          ) -> F(S,H), ok;
printsF(F, S, [H|T], Delimeter) -> F(S,H), io:put_chars(S,Delimeter), printsF(F,S,T,Delimeter).




%%% I need a special printer for Pattern, since stuff is not wrapped with {literal2 ...} and similar
print_match(S,{'match|2', Pattern, When, Body}) ->
    io:nl(S),
    io:put_chars(S,"| "),
    %% todo: shouldn't be list of pattern
    print_patterns(S,Pattern),
    case When of
        true -> ok;
        _    -> io:put_chars(S, " when "), prints(S,[When],"(",",",")")
    end,
    io:put_chars(S," -> "),
    io:nl(S),
    case is_tuple(Body) of
        true  -> print(S, Body);
        false ->
            %% let us hope it is a variable, need for "-> rec0" in
            %% access1_1(contact) = (match (contact) with | (`Contact(rec0,cor3)) -> rec0)
            io:put_chars(Body)
    end,
    io:nl(S).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% get the ocaml abstract code

generate_ocamls(Cs, TAS) -> lists:map(fun(C) -> generate_ocaml(C, TAS) end, Cs).

generate_ocaml({letrec, Fun, Args, Body}, TAS) ->
    OcamlHead = make_head(Fun, Args, TAS),
    OcamlBody = generate_ocaml(Body, TAS),
    {letrec2, OcamlHead, OcamlBody};
generate_ocaml({'fun_anon', Args, Def}, TAS) ->
    OcamlHead = name_args(Args),
    OcamlBody = generate_ocaml(Def, TAS),
    {'fun_anon2', OcamlHead, OcamlBody};
generate_ocaml({match, {case_values, Match}, Matches}, TAS) ->
    %% todo: matches
    Matches2 = generate_matches(Matches, TAS),
    {match2, generate_ocamls(Match,TAS), Matches2};
generate_ocaml({'let', {'=', Var, Arg}, Body}, TAS) ->
    {let2, {'=2', fix_variable(Var), generate_ocaml(Arg, TAS)}, generate_ocaml(Body, TAS)};
generate_ocaml({mktuple, Args}, TAS) ->
    case is_polymorphic_variant_tuple(Args, TAS) of
        true  -> {polymorphic_variant2, fix_polymorphic_variant(hd(Args)), generate_ocamls(tl(Args), TAS)};
        false -> {mktuple2, generate_ocamls(Args, TAS)}
    end;
generate_ocaml({mkcons, H, T}, TAS) ->
    {mkcons2, generate_ocaml(H, TAS), generate_ocaml(T, TAS)};
generate_ocaml({mknil}, _TAS) -> {mknil2};
generate_ocaml({variable,_Name}=V, _TAS) -> fix_variable(V);
generate_ocaml({literal,Atom}=_V, _TAS) when is_atom(Atom) -> fix_polymorphic_variant(Atom);
generate_ocaml({literal,Number}=_V, _TAS) when is_number(Number) ->  {literal_float2, mochinum:digits(Number * 1.0)};
generate_ocaml({literal_string, String}, _TAS) -> {literal_string2, String};
generate_ocaml({string, _}=String, _TAS) -> String;
generate_ocaml({literal_map, Map}, _TAS) -> {literal_map2, Map};
generate_ocaml(true, _) -> true;
generate_ocaml({call, Fun, Args}, TAS) -> make_call(Fun, Args, TAS);
generate_ocaml({apply, Op, Args}, TAS) ->
    {'apply2',
     generate_ocaml(Op, TAS),
     generate_ocamls(Args, TAS)};
generate_ocaml({map, Map, Changes}, _TAS) ->
    %% todo, call generate for args
    {map2, Map, Changes}.

%% generate_ocaml(Keep, _TAS) ->
%%     {todo, Keep}.

%%% if tuple should be converted to ocaml polymorphic variant,
%%% return {some, PV}, else return none.
is_polymorphic_variant_tuple([{literal, Atom}|_], _TAS) when is_atom(Atom) ->
    %% todo: should I check in TAS?
    true;
is_polymorphic_variant_tuple([{pattern_literal, Atom}|_], _TAS) when is_atom(Atom) ->
    %% todo: should I check in TAS?
    true;
is_polymorphic_variant_tuple(_, _) -> false.


generate_matches(L, TAS) -> lists:map(fun(M) -> generate_match(M, TAS) end, L).

generate_match({'match|', Pattern, When, Body}, TAS) ->
    {'match|2', generate_ocaml_patterns(Pattern, TAS), generate_ocaml(When, TAS), generate_ocaml(Body, TAS)}.

generate_ocaml_patterns(Pats, TAS) -> lists:map(fun(Pat) -> generate_ocaml_pattern(Pat, TAS) end, Pats).

generate_ocaml_pattern({pattern_literal, Atom}, _TAS) when is_atom(Atom) ->
    fix_polymorphic_variant(Atom);
generate_ocaml_pattern({pattern_literal, Number}, _TAS) when is_number(Number) ->
    Number * 1.0;
generate_ocaml_pattern({pattern_literal, Other}, _TAS) when is_list(Other) ->
    %% Am I 100% sure it is a string?
    {literal_string2, Other};
generate_ocaml_pattern({pattern_var, _Var}=V, _TAS) ->
    fix_variable(V);
generate_ocaml_pattern({pattern_cons, H, T}, TAS) ->
    {mkcons2, generate_ocaml_pattern(H, TAS), generate_ocaml_pattern(T, TAS)};
generate_ocaml_pattern({pattern_nil}, _TAS) ->
    {mknil2};
generate_ocaml_pattern({alias, Var, Expr}, TAS) ->
    {alias2, fix_variable(Var), generate_ocaml_pattern(Expr, TAS)};
generate_ocaml_pattern({pattern_tuple, Args}, TAS) ->
    case is_polymorphic_variant_tuple(Args, TAS) of
        true  -> {polymorphic_variant2, fix_polymorphic_variant(hd(Args)), generate_ocaml_patterns(tl(Args), TAS)};
        false -> {mktuple2, generate_ocaml_patterns(Args, TAS)}
    end;
generate_ocaml_pattern({pattern_map}, _TAS) ->
    %% todo: implement map pattern matching
    {mknil2}.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% make casing legal ok for ocaml
%%% todo: wrap stuff below in {string, List} to do better matching above
fix_function_name({'/',F,N}) -> {string, ocaml:fix(atom_to_list(F)) ++ "_" ++ integer_to_list(N)}.

fix_module_name(M) -> {string, (make_first_upper(M))}.


fix_variables(L) -> lists:map(fun fix_variable/1, L).

fix_variable ({variable, V}) -> {string, (make_first_lower(V))};
fix_variable ({pattern_var, V}) -> {string, (make_first_lower(V))};
fix_variable ({c_var,_ , V}) -> {string, (make_first_lower(V))}.

%%% bit unlogical, for some, I have kept the {variable / {literal stuff, sometimes not
fix_polymorphic_variant({literal,Atom}) when is_atom(Atom) ->
    {string, "`" ++ (make_first_upper(Atom))};
fix_polymorphic_variant({pattern_literal,Atom}) when is_atom(Atom) ->
    {string, "`" ++ (make_first_upper(Atom))};
fix_polymorphic_variant(Atom) when is_atom(Atom) ->
    {string, "`" ++ (make_first_upper(Atom))}.



name_args(Args) -> lists:map(fun name_arg/1, Args).
name_arg (Arg ) -> (make_first_lower(Arg)).

make_first_lower(Arg) when is_atom(Arg) -> [H|T] = ocaml:remove_op(atom_to_list(Arg)), ocaml:unreserve(string:to_lower([H]) ++ T).
make_first_upper(Arg) when is_atom(Arg) -> [H|T] = ocaml:remove_op(atom_to_list(Arg)), ocaml:unreserve(string:to_upper([H]) ++ T).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the head as pseudo-ocaml

make_head({'/',_F,_N}=Fun, Args, TAS) ->
    {head2, fix_function_name(Fun), name_args(Args), function_return_type(Fun, TAS)}.

function_return_type({'/',_F,_N}=_Fun, _TAS) ->
    %% todo
    [].

make_call({'/', M, F, N}, Args, TAS) ->
    N = length(Args),
    Fun = {'/', F, N},
    {call_module2, fix_module_name(M), fix_function_name(Fun), generate_ocamls(Args, TAS)};
make_call({'/', _F, N}=Fun, Args, TAS) ->
    N = length(Args),
    {call2, fix_function_name(Fun), generate_ocamls(Args, TAS)}.




get_functions(Defs, TAS) ->
    lists:map(fun(Def) -> get_function(Def, TAS) end, Defs).

get_function({#c_var{name = {F, N}}, #c_fun{vars = Vars, body = Body}}, TAS) ->
    Fun    = {'/', F, N},
    Args   = lists:map(fun(Arg) -> get_arg(Arg, TAS) end, Vars),
    Def    = compile_body(Body, TAS),
    LetRec = {letrec, Fun, Args, Def},
    optimize_letrec(LetRec).

%%% #c_fun are also used for anonymous functions
%%% todo: add support for named anonymous functions
get_anon_function(#c_fun{vars = Vars, body = Body}, TAS) ->
    Args   = lists:map(fun(Arg) -> get_arg(Arg, TAS) end, Vars),
    Def    = compile_body(Body, TAS),
    LetRec = {'fun_anon', Args, Def},
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
compile_body(#c_fun{} = Fun, TAS) ->
    %% anonymous function in
    %% list1(L) -> lists:map(fun(E) -> E end, L).
    get_anon_function(Fun, TAS);
compile_body(#c_map{arg = Map, es = Es}, TAS) ->
    %% create1() ->  #{age => 10}.
    Map2 = compile_body(Map, TAS),
    Op = lists:map(fun(E) -> compile_map_arg(E, TAS) end, Es),
    {map, Map2, Op};
compile_body(#c_case{arg = Arg, clauses = Clauses}, TAS) ->
    Res = {match, get_case_values(Arg, TAS), compile_clauses(Clauses, TAS)},
    optimize_match(Res);
compile_body(#c_let{vars = [Var], arg = Arg, body = Body}, TAS) ->
    %% todo: investigate what vars means, is the list only 1 single element?
    {'let', {'=', Var, compile_body(Arg, TAS)}, compile_body(Body, TAS)};
compile_body(#c_letrec{defs = _Defs, body = _Body}, _TAS) ->
    %% todo: it looks as if defs is a list var-fun pairs, and body is a normal.
    %% todo: for now, just return nil
    %% used in list_comprehension
    {mknil};
compile_body(#c_tuple{es = Es}, TAS) ->
    {'mktuple', compile_bodies(Es, TAS)};
compile_body(#c_cons{hd = Hd, tl = Tl}, TAS) ->
    {'mkcons', compile_body(Hd, TAS), compile_body(Tl, TAS)};
compile_body(#c_literal{val = []}, _TAS) ->
    {mknil};
compile_body(#c_literal{val = String}, _TAS) when is_list(String) -> % (is_list(String) and String=/=[]) ->
    {literal_string, String};
compile_body(#c_literal{val = Map}, _TAS) when is_map(Map) ->
    {literal_map, Map};
compile_body(#c_literal{val = Literal}, _TAS) ->
    true = not(is_list(Literal)),
    %% todo: when are we going to fix conversion to ocaml casing?
    {literal, Literal};
compile_body(#c_var{name = Name}, _TAS) ->
    %% todo: when are we going to fix conversion to ocaml casing?
    %% it can be a tuple like this  c_var{name={print,2}}
    %% io:format("~n c_var{name=~p}", [Name]),
    case Name of
        {F,N}   -> fix_function_name({'/',F,N});
        {M,F,N} -> fix_function_name({'/',M,F,N});
        _       -> {variable, Name}
    end;
compile_body(#c_call{module = Module, name = Name, args = Args}, TAS) ->
    {'call', {'/', Module#c_literal.val, Name#c_literal.val, length(Args)}, compile_bodies(Args, TAS)};
compile_body(#c_apply{op = Op, args = Args} = A, TAS) ->
    %% it seems that op is always a c_var, even if the name can be like {map,2}
    %% like in the recursive call in "map(F, [H|T]) -> [F(H)|map(F, T)];"
    case is_tuple(Op#c_var.name) of
        true ->
            case Op#c_var.name of
                {F,N} ->
                    %% io:format("Strange apply op = ~p, args=~p~n",[Op, Args]),
                    %% maybe an apply is added here to handle code upgrade?
                    %% {apply, {literal_string, fix_function_name({'/',F,N})}, compile_bodies(Args, TAS)};
                    {call, {'/',F,N}, compile_bodies(Args, TAS)};
                _ ->
                    throw({strange_op_in_c_apply, A})
            end;
        false -> {apply, compile_body(Op, TAS), compile_bodies(Args, TAS)}
    end;
compile_body(#c_try{body = Body}, TAS) ->
    %%% todo: handle try-catch for real
    compile_body(Body, TAS);
compile_body(#c_seq{arg=Arg, body=Body}, TAS) ->
    {'let', {'=', {c_var,[],'_'}, compile_body(Arg, TAS)}, compile_body(Body, TAS)}.

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
optimize_letrec({letrec,FunName,[Cor0],
                 {match,{case_values,[{variable,Cor0}]}, %added case_values [..] due to get_case_values
                  [{'match|',[{pattern_var,VarName}],true,Body}]}}) ->
    %% keep the original variable names if single clause (1 arg)
    {letrec,FunName,[VarName],Body};
optimize_letrec({letrec,FunName,[Cor1,Cor0],
                 {match,{case_values,[{variable,Cor1},{variable,Cor0}]},
                  [{'match|',[{pattern_var,VarName1},{pattern_var,VarName0}],true,Body}]}}) ->
    %% keep the original variable names if single clause (2 args)
    %% extend to 3, 4, .. args
    {letrec,FunName,[VarName1,VarName0],Body};
optimize_letrec({fun_anon,[Cor0],
                 {match,{case_values,[{variable,Cor0}]}, %added case_values [..] due to get_case_values
                  [{'match|',[{pattern_var,VarName}],true,Body}]}}) ->
    %% io:format("~nfun_anon ~p",[Cor0]),
    %% keep the original variable names if single clause (1 arg)
    {fun_anon,[VarName],Body};
optimize_letrec({fun_anon,[Cor1,Cor0],
                 {match,{case_values,[{variable,Cor1},{variable,Cor0}]},
                  [{'match|',[{pattern_var,VarName1},{pattern_var,VarName0}],true,Body}]}}) ->
    %% keep the original variable names if single clause (2 args)
    %% extend to 3, 4, .. args
    %% io:format("~nfun_anon ~p ~p",[Cor1,Cor0]),
    {fun_anon,[VarName1,VarName0],Body};
optimize_letrec(Keep) -> Keep.




%%% two levels to detect if this is arbitralily recursive.
%%% erlang core questions, why did you insert #c_values?
get_case_values(#c_values{es = Es},    TAS) -> {case_values, lists:map(fun(E) -> compile_body(E, TAS) end, Es)};
get_case_values(V,                     TAS) -> {case_values, [compile_body(V, TAS)]}. %ok?????



%%% compile the clauses, skip the match_fail, since we want the ocaml compiler to complain
%%% if pattern not complete.
compile_clauses([], _TAS) -> [];
compile_clauses([#c_case{}=C|_], _TAS) -> erlang:error({c_case_not_expected, C});
compile_clauses([#c_clause{anno = Anno,
                           guard = Guard,
                           body = Body,
                           pats = Pats
                          }|Cs], TAS) ->
    %% Body can be c_case
    %%io:format("~n~p",[Anno]),
    case lists:member(compiler_generated,Anno)
        and is_true(Guard)
        and skip_clause_with_body(Body) of
        true  ->
            %% io:format("~nSkipping clause 1: ~p", [C]),
            compile_clauses(Cs, TAS);
        false ->
            [{'match|', compile_patterns(Pats, TAS), compile_when(Guard, TAS), compile_body(Body, TAS)}
             |compile_clauses(Cs, TAS)]
    end.

is_true(#c_literal{val=true}) -> true;
is_true(_) -> false.

skip_clause_with_body(#c_primop{name=#c_literal{val=match_fail}}) -> true;
skip_clause_with_body(#c_call{module=#c_literal{val=erlang}, name=#c_literal{val=error},
                              args=[#c_tuple{es=[#c_literal{val=badrecord}|_]}|_]}) -> true;
skip_clause_with_body(_) -> false.

compile_patterns(Pats, TAS) ->
    lists:map(fun(Pat) -> compile_pattern(Pat, TAS) end, Pats).

compile_pattern(#c_cons   {hd = Hd, tl = Tl},   TAS) -> {pattern_cons, compile_pattern(Hd, TAS), compile_pattern(Tl, TAS)};
compile_pattern(#c_literal{val = []},         _TAS) -> {pattern_nil};
compile_pattern(#c_literal{val = Val},         _TAS) -> {pattern_literal, Val};
compile_pattern(#c_tuple  {es = ES},            TAS) -> {pattern_tuple, compile_patterns(ES, TAS)};
compile_pattern(#c_var    {name = Name},       _TAS) -> {pattern_var, Name};
compile_pattern(#c_map    {arg = _Arg, es = _ES}, _TAS) ->
    %% access1(#{age := Age} = Person) -> Age.
    %% es :: [#c_map_pair()]
    %% todo: handle c_map_pair
    {pattern_map};
compile_pattern(#c_alias  {var = #c_var{name = Name}, pat = Pat}, TAS) ->
    %%% todo: MFA is alias in "warn_spec_missing_fun({M, F, A} = MFA, Contracts) ->"
    {alias, {variable, Name}, compile_pattern(Pat, TAS)}.



compile_when(#c_literal {val = true}, _TAS) -> true;
compile_when(#c_literal {val = Guard}, TAS) -> compile_body(Guard, TAS);
compile_when(#c_call    {} = Body,     TAS) -> compile_body(Body, TAS);
compile_when(#c_let     {} = Body,     TAS) -> compile_body(Body, TAS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% print out type specifications and types

-record(mli,{
          types = [] :: [atom()]
         }).

print_mli(_File, TAS) ->
    S = standard_io,
    Scope = print_mli_scope(TAS),
    lists:foreach(fun(T) -> print_type_toplevel(S, T, Scope) end, TAS).

print_mli_scope(TAS) ->
    Types =
        [ Type || {named_tuple, {atom, Type}, _} <- TAS]
        ++ [ Type || {type, Type, _} <- TAS],
    #mli{types = Types}.

print_types(S, Types, Scope) ->
    %% comment removal needed for /home/mattias/erl-src/otp/lib/dialyzer/src/dialyzer_contracts.erl
    %% TypesNoComment = [X || X<-Types, element(1,X) =/= comment],
    printsF_scope(fun print_type/3, S, Types, "(", "*", ")", Scope).


%%% toplevel
print_type_toplevel(S, {named_tuple, {atom, Name}, _} = Top, Scope) ->
    %% todo: when will this be used?
    io:nl(S),
    io:put_chars(S, "type "),
    io:put_chars(S, atom_to_list(Name)),
    io:put_chars(S, " = ["),
    print_type(S, Top, Scope),
    io:put_chars(S, "]"),
    io:nl(S);
print_type_toplevel(S, {type, Name, Type}, Scope) ->
    %% should a type be a polymorphic variant?
    %% todo: let us try without first
    io:nl(S),
    io:put_chars(S, "type "),
    io:put_chars(S, atom_to_list(Name)),
    io:put_chars(S, " = "),
    io:nl(S),
    print_type(S, Type, Scope),
    io:nl(S);
print_type_toplevel(S, {spec, F, N, Type}, Scope) ->
    {string, Name} = fix_function_name({'/',F,N}),
    io:nl(S),
    io:put_chars(S, "val "),
    io:put_chars(S, Name),
    io:put_chars(S, " : "),
    io:nl(S),
    print_type(S, Type, Scope),
    io:nl(S);
print_type_toplevel(S, {export_type, _}, _Scope) ->
    io:put_chars(S, " () (* ********** export_type not supported *)").


%% some special cases expanded first until I really understand how typed sets and dictionaries works
%% but it seems that I should just remove {comment, {type and go inside.
%% {remote_type, I need to handle separately
print_type(S, {comment,{remote_type,_,_} = R}, Scope) ->
    print_type(S, R, Scope);
print_type(S, {type,_,Atom,[]}, Scope) ->
    print_type(S, Atom, Scope);
print_type(S, {type,_,Atom,SubTypes}, Scope) when is_list(SubTypes) ->
    io:put_chars(S, atom_to_list(Atom)),
    io:put_chars(S, "("),
    %% todo: fix proper Ocaml syntax
    %% todo: maybe I need to differentiate between 1 and 2 lebgth
    print_types(S, SubTypes, Scope),
    io:put_chars(S, ")");
print_type(S, {remote_type,_,[{atom,_,Module},{atom,_,Type},[]]}, _Scope) ->
    %% file:filename()
    %% {comment,{remote_type,83,[{atom,83,file},{atom,83,filename},[]]}}
    io:put_chars(S, atom_to_list(Module)),
    io:put_chars(S, "."),
    io:put_chars(S, atom_to_list(Type));
print_type(S, {remote_type,_,[{atom,_,Module},{atom,_,Type},[SubType]]}, Scope) ->
    %% ordsets:ordset(dial_warn_tag()),
    %% {comment,{remote_type,141,[{atom,141,ordsets},{atom,141,ordset},[{type,141,dial_warn_tag,[]}]]}}
    %% dict:dict(label(), erl_types:type_table())
    %% {comment,{remote_type,166,[{atom,166,dict},{atom,166,dict},[{type,166,label,[]},{remote_type,166,[{atom,166,erl_types},{atom,166,type_table},[]]}]]}}
    %% the Ocaml order is backward, i.e. "int sets"
    io:put_chars(S, "("),
    print_type(S, SubType, Scope),
    io:put_chars(S, " "),
    io:put_chars(S, atom_to_list(Module)),
    io:put_chars(S, "."),
    io:put_chars(S, atom_to_list(Type)),
    io:put_chars(S, ")");
print_type(S, {remote_type,_,[{atom,_,dict},{atom,_,dict},[SubTypeFrom, SubTypeTo]]}, Scope) ->
    %% todo: handle map:map too!
    %%
    %% mapping, need to define type like
    %% module MyUsers = Map.Make(String);;
    %% +
    %% # MyUsers.empty;;
    %% - : 'a MyUsers.t = <abstr>
    %% +
    %% MyUsers.add "10" "300" u;;
    %%- : string MyUsers.t = <abstr>
    %%
    %% ordsets:ordset(dial_warn_tag()),
    %% {comment,{remote_type,141,[{atom,141,ordsets},{atom,141,ordset},[{type,141,dial_warn_tag,[]}]]}}
    %% dict:dict(label(), erl_types:type_table())
    %% {comment,{remote_type,166,[{atom,166,dict},{atom,166,dict},[{type,166,label,[]},{remote_type,166,[{atom,166,erl_types},{atom,166,type_table},[]]}]]}}
    %% the Ocaml order is backward, i.e. "int MyString.t"
    %%
    %% todo: need to call print_type, and need to put this on top-level
    {type,_,SubTypeFromName,[]} = SubTypeFrom,
    MyMapName = lists:flatten(io_lib:format("My~p",[SubTypeFromName])),
    io:format("(* module ~p = Map.Make(~p) *)~n",[MyMapName,SubTypeFromName]),
    io:put_chars(S, "("),
    print_type(S, SubTypeTo, Scope),
    io:put_chars(S, " "),
    io:format("~p",[MyMapName]),
    io:put_chars(S, ")");
print_type(S, {named_tuple, {atom, Name}, Types}, Scope) ->
    {string, Poly} = fix_polymorphic_variant(Name),
    io:put_chars(S, Poly),
    io:put_chars(S, " of "),
    io:nl(S),
    print_types(S, Types, Scope);
print_type(S, {atom_single, Name}, _Scope) ->
    %% todo: this should be more generic
    {string, Name2} = fix_polymorphic_variant({literal,Name}),
    io:put_chars(S,Name2);
print_type(S, {list,Type}, Scope) ->
    printsF_scope(fun print_type/3, S, [Type], "(", ",", " list)", Scope);
print_type(S, {record_type2, Name}, _Scope) ->
    %% todo: do I need to make a polymorphica
    %% DA40, skolflygplan
    io:put_chars(S,atom_to_list(Name));
print_type(S, {'|', [{named_tuple, _, _}|_] = Types}, Scope) ->
    printsF_scope(fun print_type/3, S, Types, "[", "|", "]", Scope);
print_type(S, {'|', [{atom_single, _}|_] = Types}, Scope) ->
    printsF_scope(fun print_type/3, S, Types, "[", "|", "]", Scope);
print_type(S, {'|', [{tuple,[{atom_single,_}|_]}|_] = Types}, Scope) ->
    %% [{tuple,[{atom_single,age},integer]},{tuple,[{atom_single,name},string]}]
    printsF_scope(fun print_type/3, S, Types, "[", "|", "]", Scope);
print_type(S, {'|', [Type]}, Scope) ->
    print_type(S, Type, Scope);
print_type(S, {'|', [_,_|_] = Types}, Scope) ->
    %% Case when types are combined
    %% type mail =
    %%   Types: [{record_type2,contact},{record_type2,device}]
    %% io:format(S, "Types: ~p", [Types]),
    printsF_scope(fun print_type/3, S, Types, "(* or-types not supported by ocaml unless we can macro expand *)\n (", "|", ")", Scope);
print_type(S, {'*', Types}, Scope) ->
    printsF_scope(fun print_type/3, S, Types, "(* * *) (", "*", ")", Scope);
print_type(S, {'tuple', [{atom_single, Name}=Head|Types]}, Scope) ->
    {string, Poly} = fix_polymorphic_variant(Name),
    io:put_chars(S, Poly),
    io:put_chars(S, " of "),
    io:nl(S),
    print_types(S, Types, Scope);
print_type(S, {'tuple', Types}, Scope) ->
    printsF_scope(fun print_type/3, S, Types, "(* tuple *) (", "*", ")", Scope);
print_type(S, {'->', L, R}, Scope) ->
    printsF_scope(fun print_type/3, S, [L,R], "(", "->", ")", Scope);
print_type(S, {map_typed, Types}, Scope) ->
    %% {map_typed,[{map_field_assoc,string,integer}]}
    io:put_chars(S, "("),
    print_types(S, Types, Scope),
    io:put_chars(S, " map)");
print_type(S, {map_unsupported, _}, _Scope) ->
     io:put_chars(S, " () (* ********** maps with multiple key types is not supported unless you define a union type *)");
print_type(S, {map_field_assoc, Key, Value}, Scope) ->
    print_types(S, [Key, Value], Scope);
print_type(S, {map_record_like, Types}, Scope) ->
    %% {map_record_like,[{map_field_assoc,{atom,age},integer},{map_field_assoc,{atom,name},string}]}
    io:put_chars(S, "("),
    printsF_scope(fun print_type_map_record_like/3, S, Types, "[< ", ",", "]", Scope),
    io:put_chars(S, " set)");
print_type(S, string, _Scope) ->
    io:put_chars(S,"string");
print_type(S, integer, _Scope) ->
    io:put_chars(S,"int");
print_type(S, non_neg_integer, _Scope) ->
    io:put_chars(S,"int");
print_type(S, Atom, Scope) when is_atom(Atom) ->
    %% user-defined type
    case lists:member(Atom, Scope#mli.types) of
        true  -> io:put_chars(S,atom_to_list(Atom));
        false -> io:format(S, " ~w (* ********** unknown type ~w*) ", [Atom, Atom])
    end.





print_type_map_record_like(S, {map_field_assoc,{atom,Name},Type}, Scope) ->
    print_type(S, {named_tuple, {atom, Name}, [Type]}, Scope).














%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% get specs and types without the c_literal stuff
get_spec_and_types(Attrs) ->
    L  = lists:map(fun(Attr) -> get_spec_or_type(Attr) end, Attrs),
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
    generate_type(Type);
generate_field({record_field,_Row,{atom,_Row2, FieldName},_Default}) ->
    {comment, FieldName};
generate_field({record_field,_Row,{atom,_Row2,FieldName}}) ->
    {comment, FieldName}.




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
generate_type({type, _Row, record, [{atom, _, Type}]}, Constraints) ->
    {record_type2, Type};
generate_type({type, _Row, record, [{atom, _, Type}|_FieldTypes]}, Constraints) ->
    %% -record(c_map_pair, {anno=[],op :: #c_literal{val::'assoc'} | #c_literal{val::'exact'},key,val}).
    %% becomes {type,78,record,[{atom,78,c_literal},{type,78,field_type,[{atom,78,val},{atom,78,assoc}]}]}
    %% todo:handle FieldTypes
    {record_type2, Type};
generate_type({type, _Row, list, [Type]}, Constraints) ->
    {list, generate_type(Type, Constraints)};
generate_type({type, _Row, nonempty_list, [Type]}, Constraints) ->
    {nonempty_list, generate_type(Type, Constraints)};
generate_type({type, _Row, Primitive, []}, _Constraints) ->
    %% todo: check that primitive is string, integer ....
    Primitive;
generate_type({type,_Row1,bounded_fun,[{type,_Row2,'fun',[From, To]}, Constraints]}, TopConstraints) ->
    %% I do not plan for more than one bounded_fun expression for now
    []       = TopConstraints,
    FromType = generate_type(From, Constraints),
    ToType   = generate_type(To, Constraints),
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
    %% age in -spec get1('age' | 'name', #contact{}) -> {'age', integer()} | {'name', string()}.
    {atom_single, Atom};
generate_type({remote_type, _Row, _} = Remote_type, _Constraints) ->
    %% _ [{atom,72,erl_types},{atom,72,erl_type},[]]
    {comment, Remote_type};
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

f_test() ->
    te_experiments:t1("./te_experiments.erl").

e_read() ->
    {ok, Module} = dialyzer_utils:get_core_from_src("/home/mattias/erl-src/otp/lib/dialyzer/src/dialyzer_contracts.erl"),
    Module.
