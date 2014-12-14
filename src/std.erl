%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------

-module(std).

-export([lists_same_elements/2, lists_no_common_element/2, lists_elements_are_unique/1,
         lists_half/1,
         any_to_list/1, lists_take/2]).

%%% take N first from list
lists_take(0, _) -> [];
lists_take(_, []) -> [];
lists_take(N, [H|T]) -> [H|lists_take(N-1,T)].

lists_half(L) ->
    lists_half(L, true, [], []).

lists_half([],       _ , L, R) -> {L, R};
lists_half([H|T], true , L, R) -> lists_half(T, false, [H|L], R);
lists_half([H|T], false, L, R) -> lists_half(T, true , L, [H|R]).


%%% no element in Xs occurs twice
lists_elements_are_unique(Xs) ->
    length(Xs) =:= sets:size(sets:from_list(Xs)).

%%% Xs and Ys contains the same elements, but maybe in a different order
lists_same_elements(Xs, Ys) ->
    lists:sort(Xs) =:= lists:sort(Ys).

%%% Xs and Ys have no element in common
lists_no_common_element(Xs, Ys) ->
    0 =:= sets:size(sets:intersection(sets:from_list(Xs), sets:from_list(Ys))).

any_to_list(X) when is_float(X)   -> float_to_list(X);
any_to_list(X) when is_integer(X) -> integer_to_list(X);
any_to_list(X) when is_atom(X)    -> atom_to_list(X).
