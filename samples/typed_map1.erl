-module(typed_map1).

-compile(export_all).

-type map_traditional_t() :: #{string() => integer()}.
-type map_unsupported_t() :: #{string() => integer(), float() => string()}.
-type map_record_like_t() :: #{age => integer(), name => string()}.

-spec create1() -> map_record_like_t().
create1() ->
    #{age => 10}.

-spec change1(map_record_like_t()) -> map_record_like_t().
change1(M) ->
    M#{age := 20}.

-spec upsert1(map_record_like_t()) -> map_record_like_t().
upsert1(M) ->
    M#{name => "mattias"}.

-spec access1(map_record_like_t()) -> integer().
access1(#{age := Age} = Person) -> Age.

%% -spec access2(map_record_like_t()) -> integer().
%% access2(#{age => Age} = Person) -> Age.

%% dialyzer doesn't seem to handle advanced map-spec
%% make dialyzer_sample
%% cd ~/data3/repos/terl/samples/; erlc +debug_info *.erl
%% map1.erl:14: Warning: variable 'Person' is unused
%% typed_map1.erl:20: Warning: variable 'Person' is unused
%% typed_map1.erl:24: Warning: variable 'Person' is unused
%% cd ~/data3/repos/terl/samples/; dialyzer  --fullpath --verbose -Wunderspecs -Wunmatched_returns -Werror_handling *.beam
%%   Checking whether the PLT /home/mattias/.dialyzer_plt is up-to-date... yes
%%   Proceeding with analysis... done in 0m0.75s
%% done (passed successfully)
%% mattias@ubuntu:~/data3/repos/terl$
-spec dialyzer_complain1(map_record_like_t()) -> integer().
dialyzer_complain1(#{tage := Age} = Person) ->
    Age.

-spec all1() -> integer().
all1() ->
    M = create1(),
    M2 = change1(M),
    M3 = upsert1(M2),
    access1(M3).
