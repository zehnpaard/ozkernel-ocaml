(* string -> int *)
module Env = Map.Make(String)

type t =
  | Var of int
  | Int of int
  | Proc of Ast.v * Env.t

let store = Array.make 100000 (Int 0)

let f = ref 0

let makeInStore v =
  let i = !f in
  begin
    store.(i) <- v;
    f := i + 1;
    i
  end

let makeVar () = makeInStore (Var (-1))
let makeInt n = makeInStore (Int n)
let makeProc v e = match v with
  | Ast.Proc _ -> makeInStore (Proc (v, e)) (* Inefficient closure, must fix *)
  | _ -> failwith "makeProc called with non-procedure value"

let getParent i = match store.(i) with
  | Var j when not j = (-1) ->
      let p = getParent j in
      (store.(i) <- Var p; p)
  | _ -> i

let bind ~parent ~child =
  store.(child) := Var !parent

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
     | (Var i, _) | (_, Var i) -> raise (UnboundVariable i)
     | _ -> failwith "Non-int value passed to int binOp"

let callProcedureFromEnv env x args =
  let p = Env.find x env in
  match p with
    | Var i -> raise (UnboundVariable i)
    | Proc (Ast.Proc (params, s), e) ->
        if not (List.length args = List.length params) then
          failwith "Invalid number of arguments passed to procedure"
        else
          let f k a b = Some a in
          let env1 = Env.union f e env in
          let env2 = List.fold_right2 Env.add params args env1 in
          stack := (s, env2) :: !stack
    | _ -> failwith "Non-procedure in position of called procedure"

let procCall env x args =
  let args' = getArgs env args in
  match (x, args) with
    | ("+", [i; j; k]) -> intBinOp (+) i j k
    | ("-", [i; j; k]) -> intBinOp (-) i j k
    | ("*", [i; j; k]) -> intBinOp ( * ) i j k
    | ("div", [i; j; k]) -> intBinOp (/) i j k
    | ("mod", [i; j; k]) -> intBinOp mod i j k
    | _ when Env.mem x env -> callProcedureFromEnv env x args
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
