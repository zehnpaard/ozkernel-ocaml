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

let stack : (Ast.t, Env.t) list ref = ref []

let runOneStep statement env = match statement with
  | Skip -> ()
  | Seq (s1, s2) ->
      stack := (s1, env) :: (s2, env) :: !stack
  | VarBind (x1, x2) -> ()
  | ValBind (x, v) -> ()
  | Declare (x, s) -> ()
  | ProcCall (x, args) -> ()

let run = match !stack with
  | [] -> ()
  | (s, e) :: stack' ->
      begin
        stack := stack';
        runOneStep s e;
        run ()
      end
