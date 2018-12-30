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

let makeInt n =
  let i = !f in
  begin
    store.(i) <- Int n;
    f := i + 1;
    i
  end

let getParent i = match store.(i) with
  | Var j when not j = (-1) ->
      begin
        p = getParent j;
        store.(i) <- Var p;
        p
      end
  | _ -> i

let bind ~parent ~child =
  store.(child) := Var !parent

(* string -> int *)
module Env = Map.Make(string)

let stack : (Ast.t, Env.t) list ref = ref []

let runOneStep statement env = match statement with
  | Ast.Skip -> ()
  | Ast.Seq (s1, s2) -> stack := (s1, env) :: (s2, env) :: !stack
  | Ast.VarBind (x1, x2) ->
      begin
        p1 = getParent (Env.find x1 env);
        p2 = getParent (Env.find x2 env);
        match (store.(p1), store.(p2)) with
          | (Var _, _) -> bind ~child:p1 ~parent:p2
          | (_, Var _) -> bind ~child:p2 ~parent:p1
          | _ -> failwith "Attempting to bind two bound variables"
      end
  | Ast.ValBind (x, v) ->
      begin
        p = getParent (Env.find x env);
        match v with
          | Int n -> bind ~child:p ~parent:(makeInt n)
      end
  | Ast.Declare (x, s) ->
      begin
        i = makeVar ();
        env' = Env.add x i env;
        stack := (s, env') :: !stack
      end
  | Ast.ProcCall (x, args) -> ()

let run = match !stack with
  | [] -> ()
  | (s, e) :: stack' ->
      begin
        stack := stack';
        runOneStep s e;
        run ()
      end
