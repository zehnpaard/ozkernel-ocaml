type t =
  | Var of int
  | Int of int

let store = Array.make 100000 (Int 0)

let f = ref 0

let makeVar () =
  let i = !f in
  begin
    store.(i) <- Var (-1);
    f := i + 1;
    i
  end

let makeInt n
  let i = !f in
  begin
    store.(i) <- Int n;
    f := i + 1;
    i
  end

(* string -> int *)
module Env = Map.Make(string)
