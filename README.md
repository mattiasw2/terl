terl
====

Typed Erlang Experiment - using Ocaml for type-checking

Dialyzer is nice, but I want to try a more standard Hindley-Milner type inference approach. http://okmij.org/ftp/ML/generalization.html

It has been tried before: http://homepages.inf.ed.ac.uk/wadler/papers/erlang/erlang.pdf

In order to do that, I need to restrict Erlang, for example the bang operator ! needs to know the module(s) it sends the message to.

Instead of implementing type-checking myself, version 1 will transform Erlang to an Ocaml program, and let the ocaml compiler to the verification. Erlang atoms will be mapped to Ocaml polymorphic variants, https://realworldocaml.org/v1/en/html/variants.html#polymorphic-variants

The initial goal is not to generate runnable Ocaml code, just enough to do the type-checking.
