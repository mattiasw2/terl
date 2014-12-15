-module(access1).

-compile(export_all).

-include("rec1.hrl").

-spec build1(string()) -> #contact{}.
build1(Name) ->
    #contact{name=Name}.

access1(Contact) ->
    Contact#contact.name.

%% to_core() ->
%%     compile:file("../samples/access1.erl",[to_core]).
