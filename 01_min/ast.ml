type v =
  | Integer of int

type t =
  | Seq of t * t
  | Skip
  | VarBind of string * string
  | ValBind of string * v
  | Declare of string * t 
  | ProcCall of string * string list
