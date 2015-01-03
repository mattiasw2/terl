-module(ocaml_reserved_words).

-export([reserved_words/0, operators/0]).

%% ocaml 4.02 reservered words
%%
%% <<      and         as          assert      asr         begin       class
%%         constraint  do          done        downto      else        end
%%         exception   external    false       for         fun         function
%%         functor     if          in          include     inherit     initializer
%%         land        lazy        let         lor         lsl         lsr
%%         lxor        match       method      mod         module      mutable
%%         new         object      of          open        or          private
%%         rec         sig         struct      then        to          true
%%         try         type        val         virtual     when        while
%%         with
%% >>
%%   The following character sequences are also keywords:
%% <<
%%       !=    #     &     &&    '     (     )     *     +     ,     -
%%       -.    ->    .     ..    :     ::    :=    :>    ;     ;;    <
%%       <-    =     >     >]    >}    ?     [     [<    [>    [|    ]
%%       _     `     {     {<    |     |]    ||    }     ~
%% >>


reserved_words() ->
    [
     "and",
     "as",
     "assert",
     "asr",
     "begin",
     "class",
     "constraint",
     "do",
     "done",
     "downto",
     "else",
     "end",
     "exception",
     "external",
     "false",
     "for",
     "fun",
     "function",
     "functor",
     "if",
     "in",
     "include",
     "inherit",
     "initializer",
     "land",
     "lazy",
     "let",
     "lor",
     "lsl",
     "lsr",
     "lxor",
     "match",
     "method",
     "mod",
     "module",
     "mutable",
     "new",
     "object",
     "of",
     "open",
     "or",
     "private",
     "rec",
     "sig",
     "struct",
     "then",
     "to",
     "true",
     "try",
     "type",
     "val",
     "virtual",
     "when",
     "while",
     "with"].

operators() ->
    [
     {op, "!=", "neq"},
     {op, "#", "hash"},
     {op, "&", "amp"},
     {op, "&&", "ampamp"},
     {op, "'", "squote"},
     {op, "(", "lpar"},
     {op, ")", "rpar"},
     {op, "*", "times"},
     {op, "+", "plus"},
     {op, ",", "comma"},
     {op, "-", "minus"},
     {op, "-.", "fminus"},
     {op, "->", "arrow"},
     {op, ".", "dot"},
     {op, "..", "dotdot"},
     {op, ":", "colon"},
     {op, "::", "coloncolon"},
     {op, ":=", "coloneq"},
     {op, ":>", "colongr"},
     {op, ";", "semi"},
     {op, ";;", "semisemi"},
     {op, "<", "le"},
     {op, "<-", "leftarrow"},
     {op, "=", "eq"},
     {op, ">", "gr"},
     {op, ">]", "grbracket"},
     {op, ">}", "grlcurly"},
     {op, "?", "question"},
     {op, "[", "lbracket"},
     {op, "[<", "lbracketle"},
     {op, "[>", "lbracketgr"},
     {op, "[|", "lbracketvert"},
     {op, "]", "rbracket"},
     %% {op, "_", "underscore"},
     {op, "`", "backquote"},
     {op, "{", "lcurly"},
     {op, "{<", "lcurlyle"},
     {op, "|", "vert"},
     {op, "|]", "vertrbracket"},
     {op, "||", "vertvert"},
     {op, "}", "rcurly"},
     {op, "~", "approx"}
    ].
