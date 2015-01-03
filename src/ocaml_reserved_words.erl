-module(ocaml_reserved_words).

-export([reserved_words/0, symbols/0, operators/0]).

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

symbols() ->
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

%% ------------------------------------------------------
%% |    Operator     |         Initial meaning          |
%% ------------------------------------------------------
%% | +               |Integer addition.                 |
%% |- (infix)        |Integer subtraction.              |
%% |~-   - (prefix)  |Integer negation.                 |
%% |*                |Integer multiplication.           |
%% |/                |Integer division. Raise           |
%% |                 |Division_by_zero if second        |
%% |                 |argument is zero.                 |
%% |mod              |Integer modulus. Raise            |
%% |                 |Division_by_zero if second        |
%% |                 |argument is zero.                 |
%% |land             |Bitwise logical "and" on integers.|
%% |                 |                                  |
%% |lor              |Bitwise logical "or" on integers. |
%% |lxor             |Bitwise logical "exclusive or" on |
%% |                 |integers.                         |
%% |lsl              |Bitwise logical shift left on     |
%% |                 |integers.                         |
%% |lsr              |Bitwise logical shift right on    |
%% |                 |integers.                         |
%% |asr              |Bitwise arithmetic shift right on |
%% |                 |integers.                         |
%% |+.               |Floating-point addition.          |
%% |-. (infix)       |Floating-point subtraction.       |
%% |~-.   -. (prefix)|Floating-point negation.          |
%% |*.               |Floating-point multiplication.    |
%% |/.               |Floating-point division.          |
%% |**               |Floating-point exponentiation.    |
%% |@                |List concatenation.               |
%% |^                |String concatenation.             |
%% |!                |Dereferencing (return the current |
%% |                 |contents of a reference).         |
%% |:=               |Reference assignment (update the  |
%% |                 |reference given as first argument |
%% |                 |with the value of the second      |
%% |                 |argument).                        |
%% |=                |Structural equality test.         |
%% |<>               |Structural inequality test.       |
%% |==               |Physical equality test.           |
%% |!=               |Physical inequality test.         |
%% |<                |Test "less than".                 |
%% |<=               |Test "less than or equal".        |
%% |>                |Test "greater than".              |
%% |>=               |Test "greater than or equal".     |
%% |&&   &           |Boolean conjunction.              |
%% |||   or          |Boolean disjunction.              |
%% ------------------------------------------------------


operators() ->
    [
     {op,"+","plus"},          % | + |Integer addition. |
     {op,"-","minus"},         % |- (infix) |Integer subtraction. |
     {op,"~-","uminus"},       % |~- - (prefix) |Integer negation. |
     {op,"*","times"},         % |* |Integer multiplication. |
     {op,"/","div"},           % |/ |Integer division. Raise |
     {op,"mod","(mod)"},       % |mod |Integer modulus. Raise |
     {op,"land","(land)"},     % |land |Bitwise logical "and" on integers.|
     {op,"lor","(lor)"},       % |lor |Bitwise logical "or" on integers. |
     {op,"lxor","(lxor)"},     % |lxor |Bitwise logical "exclusive or" on |
     {op,"lsl","(lsl)"},       % |lsl |Bitwise logical shift left on |
     {op,"lsr","(lsr)"},       % |lsr |Bitwise logical shift right on |
     {op,"asr","(asr)"},       % |asr |Bitwise arithmetic shift right on |
     {op,"+.","fplus"},        % |+. |Floating-point addition. |
     {op,"-.","fminus"},       % |-. (infix) |Floating-point subtraction. |
     {op,"~-.","fuminus"},     % |~-. -. (prefix)|Floating-point negation. |
     {op,"*.","ftimes"},       % |*. |Floating-point multiplication. |
     {op,"/.","fdiv"},         % |/. |Floating-point division. |
     {op,"**","fexp"},         % |** |Floating-point exponentiation. |
     {op,"@","list_append"},   % |@ |List concatenation. |
     {op,"^","string_append"}, % |^ |String concatenation. |
     {op,"!","deref"},         % |! |Dereferencing (return the current |
     {op,":=","assign"},       % |:= |Reference assignment (update the |
     {op,"=","eq"},            % |= |Structural equality test. |
     {op,"<>","neq"},          % |<> |Structural inequality test. |
     {op,"==","eq"},           % |== |Physical equality test. |
     {op,"!=","neq"},          % |!= |Physical inequality test. |
     {op,"<","le"},            % |< |Test "less than". |
     {op,"<=","leq"},          % |<= |Test "less than or equal". |
     {op,">","gr"},            % |> |Test "greater than". |
     {op,">=","geq"},          % |>= |Test "greater than or equal". |
     {op,"&&","band"},         % |&& & |Boolean conjunction. |
     {op,"||","bor"},           % ||| or |Boolean disjunction. |
     {op,"&","band"},         % |&& & |Boolean conjunction. |
     {op,"or","bor"}           % ||| or |Boolean disjunction. |
    ].
