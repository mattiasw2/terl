type foo_t =
  [ `A_1 of int
  | `A_2 of int*string]

type bar_t =
  [ `B_2 of int*int
  | `B_3 of int*string]

type f_t =
  (int->string) -> int -> string

let g (x:string) : string =
  "hej"

let h f i =
  f i
