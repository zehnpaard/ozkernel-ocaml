type v =
  | Integer of int
  | Proc of string list * t
  | Record of string * (v * string) list
and t =
  | Seq of t * t
  | Skip
  | VarBind of string * string
  | ValBind of string * v
  | Declare of string * t 
  | ProcCall of string * string list

module Bound = Set.Make(String)

let rec freeVars' boundVars = function
  | Seq (s1, s2) -> (freeVars' boundVars s1) @ (freeVars' boundVars s2)
  | Skip -> []
  | VarBind (x1, x2) ->
      let f x = not (Bound.mem x boundVars) in
      List.filter f [x1; x2]
  | ValBind (x, Integer _) ->
      if Bound.mem x boundVars then [] else [x]
  | ValBind (x, Proc _ as p) ->
      let fvs = freeVarsProc' boundVars p  in
      if Bound.mem x boundVars then fvs else x :: fvs
  | Declare (x, s) ->
      freeVars' (Bound.add x boundVars) s
  | ProcCall (x, args) ->
      let f x = not (Bound.mem x boundVars) in
      List.filter f (x :: args)
and freeVarsProc' boundVars = function
  | Proc (xs, s) -> freeVars' (List.fold_right Bound.add xs boundVars) s
  | _ -> failwith "freeVarsProc' called on non-procedure"

let freeVars = freeVars' Bound.empty
let freeVarsProc = freeVarsProc' Bound.empty
