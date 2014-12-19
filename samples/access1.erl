-module(access1).

-compile(export_all).

-include("rec1.hrl").

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


access1(Contact) ->
    Contact#contact.name.

%% to_core() ->
%%     compile:file("../samples/access1.erl",[to_core]).
