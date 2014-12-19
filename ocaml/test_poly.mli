(* how to compile
ocamlc -c test_poly.mli test_poly.ml *)

type f_t = (int -> string) -> int -> string

val g : string -> string

val h : f_t
