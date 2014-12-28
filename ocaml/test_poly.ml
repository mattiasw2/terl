type foo_t =
  [ `A_1 of int
  | `A_2 of int*string]

type bar_t =
  [ `B_2 of int*int
  | `B_3 of int*string]

type ggg_t =
  | B_2 of int*int
  | B_3 of int*string

type f_t =
  (int->string) -> int -> string

let g (x:string) : string =
  "hej"

let h f i =
  f i

let i (x:([`A | `B of int])) = x

(* how should I inline this? maybe impossible, maybe a need a type name
let j (x:(A | B of int)) = x *)

let astest ((`Foo(a,c)) as top) = c, top

let astest2 ((a,b) as c) = c
