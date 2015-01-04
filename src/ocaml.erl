-module(ocaml).

-export([fix/1,escape/1,init/0]).

-spec fix(string()) -> string().
fix(Name) ->
    Db = init(),
    fix(Name, Db).

init() ->
    ReservedWords = sets:from_list(ocaml_reserved_words:reserved_words()),
    Operators = ocaml_reserved_words:operators() ++ ocaml_reserved_words:symbols(),
    Operators2 = lists:map(fun ({op,S,L}) -> {op2, S, escape(S), L} end, Operators),
    %% we want the long one first
    Operators3 = lists:sort(fun({op2,X,_,_},{op2,Y,_,_}) -> length(X)>length(Y) end,Operators2),
    {avoid, ReservedWords, Operators3}.

fix(Name, {avoid, ReservedWords, Operators}) ->
    %% apply each replace on Name
    Result = lists:foldl(
               fun({op2,_From,Regex,To},NameTmp)-> re:replace(NameTmp,Regex,To) end,
               Name,
               Operators),
    %% convert from iolist back to erlang string
    Name2 = unicode:characters_to_list(Result),
    case sets:is_element(Name2, ReservedWords) of
        true  -> Name2 ++ "x";
        false -> Name2
    end.

escape(String) ->
    re:replace(String, "[.^$*+?()[{\\\|\s#]", "\\\\&",[global]).
