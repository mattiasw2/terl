-module(ocaml).

%%% API
-export([fix/1,unreserve/1,escape/1]).

%%% Behavior
-export([init/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API

%%% fix the Name so that is doesn't affect ocaml parsing
-spec fix(string()) -> string().
fix(Name) ->
    Db = init(),
    fix(Name, Db).

%%% make sure variable names are ok
-spec unreserve(string()) -> string().
unreserve(Name) ->
    Db = init(),
    unreserve(Name, Db).

%%% escape(String) returns a regexp string that matches exactly String and nothing else.
escape(String) ->
    re:replace(String, "[.^$*+?()[{\\\|\s#]", "\\\\&",[global]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal

%%% the necessary initialization, only needed once, the result
%%% needs to be stored for each call of fix/2. (ets or process)
%%% todo: further improvement: re:compile on the regex
init() ->
    ReservedWords = sets:from_list(ocaml_reserved_words:reserved_words()),
    Operators = ocaml_reserved_words:operators() ++ ocaml_reserved_words:symbols(),
    Operators2 = lists:map(fun ({op,S,L}) -> {op2, S, escape(S), L} end, Operators),
    %% we want the long one first
    Operators3 = lists:sort(fun({op2,X,_,_},{op2,Y,_,_}) -> length(X)>length(Y) end,Operators2),
    {avoid, ReservedWords, Operators3}.

%%% fix the Name so that is doesn't affect ocaml parsing
fix(Name, {avoid, ReservedWords, Operators} = Db) ->
    %% apply each replace on Name
    Result = lists:foldl(
               fun({op2,_From,Regex,To},NameTmp)-> re:replace(NameTmp,Regex,To) end,
               Name,
               Operators),
    %% convert from iolist back to erlang string
    Name2 = unicode:characters_to_list(Result),
    unreserve(Name2, Db).

unreserve(Name, {avoid, ReservedWords, _Operators}) ->
    case sets:is_element(Name, ReservedWords) of
        true  -> Name ++ "x";
        false -> Name
    end.
