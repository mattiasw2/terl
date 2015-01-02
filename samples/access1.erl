-module(access1).

-compile(export_all).

-include("rec1.hrl").

build1tuple(Name) ->
    {'contact2',Name,-1}.

-spec build1(string()) -> #contact{}.
build1(Name) ->
    #contact{name=Name}.

-spec build2a(string(),integer()) -> #contact{}.
build2a(Name, Age) ->
    #contact{name=Name,age=Age}.

-spec build2b(A,B) -> #contact{} when
      A :: string(),
      B :: integer().
build2b(Name, Age) ->
    #contact{name=Name,age=Age}.

newborn(Name) ->
    #contact{name=Name,age=0}.


access1(Contact) ->
    Contact#contact.name.

-spec get1('age' | 'name', #contact{}) -> {'age', integer()} | {'name', string()}.
get1(age,  #contact{age  = Age }) -> {age,  Age};
get1(name, #contact{name = Name}) -> {name, Name}.

%%% ( becomes parent_type
-spec get2(('age' | 'name'), #contact{}) -> {'age', integer()} | {'name', string()}.
get2(age,  #contact{age  = Age }) -> {age,  Age};
get2(name, #contact{name = Name}) -> {name, Name}.

%% to_core() ->
%%     compile:file("../samples/access1.erl",[to_core]).

same(X,X) ->
    true;
same(_,_) ->
    false.
