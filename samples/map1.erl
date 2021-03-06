-module(map1).

-compile(export_all).

create1() ->
    #{age => 10}.

change1(M) ->
    M#{age := 20}.

upsert1(M) ->
    M#{name => "mattias"}.

access1(#{age := Age} = Person) ->
    Age.

all1() ->
    M = create1(),
    M2 = change1(M),
    M3 = upsert1(M2),
    access1(M3).
