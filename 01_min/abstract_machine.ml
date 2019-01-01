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
      let p = getParent j in
      (store.(i) <- Var p; p)
  | _ -> i

let bind ~parent ~child =
  store.(child) := Var !parent

(* string -> int *)
module Env = Map.Make(String)

let stack : (Ast.t, Env.t) list ref = ref []

let getArg env arg =
  getParent (Env.find arg env)

let getArgs env args =
  List.map (getArg env) args

exception UnboundVariable of int

let intBinOp f i j k =
  match (store.(i), store.(j)) with
     | (Int n1, Int n2) ->
         let a = makeInt (f n1 n2) in
         store.(k) := Var a
     | (Var i, _) | (_, Var i) -> UnboundVariable i
     | _ -> failwith "Non-int value passed to int binOp"

let procCall env x args =
  let args' = getArgs env args in
  match (x, args) with
    | ("+", [i; j; k]) -> intBinOp (+) i j k
    | ("-", [i; j; k]) -> intBinOp (-) i j k
    | ("*", [i; j; k]) -> intBinOp ( * ) i j k
    | ("div", [i; j; k]) -> intBinOp (/) i j k
    | ("mod", [i; j; k]) -> intBinOp mod i j k
    | _ -> failwith "Unknown procedure"

let runOneStep statement env = match statement with
  | Ast.Skip -> ()
  | Ast.Seq (s1, s2) -> stack := (s1, env) :: (s2, env) :: !stack
  | Ast.VarBind (x1, x2) ->
      let p1 = getParent (Env.find x1 env) in
      let p2 = getParent (Env.find x2 env) in
      (match (store.(p1), store.(p2)) with
         | (Var _, _) -> bind ~child:p1 ~parent:p2
         | (_, Var _) -> bind ~child:p2 ~parent:p1
         | _ -> failwith "Attempting to bind two bound variables")
  | Ast.ValBind (x, v) ->
      let p = getParent (Env.find x env) in
      let i = (match v with
        | Int n -> (makeInt n))
      in
      bind ~child:p ~parent:i
  | Ast.Declare (x, s) ->
      let env' = Env.add x (makeVar ()) env in
      stack := (s, env') :: !stack
  | Ast.ProcCall (x, args) -> procCall env x args

let run = match !stack with
  | [] -> ()
  | (s, e) :: stack' ->
      begin
        stack := stack';
        runOneStep s e;
        run ()
      end
