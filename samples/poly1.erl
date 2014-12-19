%%%%

-module(poly1).

-compile(export_all).

list1(L) ->
    lists:map(fun(E) -> E end, L).

-spec applyer(Fun, A) -> B when
      Fun :: fun((A) -> B).
applyer(F,A1) ->
    F(A1).

%% -spec map(Fun, List1) -> List2 when
%%       Fun :: fun((A) -> B),
%%       List1 :: [A],
%%       List2 :: [B],
%%       A :: term(),
%%       B :: term().

%% using the original spec, I loose the relationship between A B and the Fun
-spec map(Fun, List1) -> List2 when
      Fun :: fun((A) -> B),
      List1 :: [A],
      List2 :: [B].

map(F, [H|T]) ->
    [F(H)|map(F, T)];
map(F, []) when is_function(F, 1) -> [].

-spec type1() -> [{{'map',2},[{_,_,_,_},...]},...].
type1() ->
    [{{map,2},
      [{type,8,bounded_fun,
        [{type,8,'fun',
          [{type,8,product,[{var,8,'Fun'},{var,8,'List1'}]},
           {var,8,'List2'}]},
         [{type,9,constraint,
           [{atom,9,is_subtype},
            [{var,9,'Fun'},
             {type,9,'fun',
              [{type,9,product,[{var,9,'A'}]},{var,9,'B'}]}]]},
          {type,10,constraint,
           [{atom,10,is_subtype},
            [{var,10,'List1'},{type,10,list,[{var,10,'A'}]}]]},
          {type,11,constraint,
           [{atom,11,is_subtype},
            [{var,11,'List2'},{type,11,list,[{var,11,'B'}]}]]},
          {type,12,constraint,
           [{atom,12,is_subtype},[{var,12,'A'},{type,12,term,[]}]]},
          {type,13,constraint,
           [{atom,13,is_subtype},
            [{var,13,'B'},{type,13,term,[]}]]}]]}]}].
