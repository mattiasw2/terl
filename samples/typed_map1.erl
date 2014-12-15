-module(typed_map1).

-compile(export_all).

-type map_t() :: #{age => integer(), name => string()}.

-spec create1() -> map_t().
create1() ->
    #{age => 10}.

-spec change1(map_t()) -> map_t().
change1(M) ->
    M#{age := 20}.

-spec upsert1(map_t()) -> map_t().
upsert1(M) ->
    M#{name => "mattias"}.

-spec access1(map_t()) -> integer().
access1(#{age := Age} = Person) ->
    Age.

-spec all1() -> map_t().
all1() ->
    M = create1(),
    M2 = change1(M),
    M3 = upsert1(M2),
    access1(M3).
