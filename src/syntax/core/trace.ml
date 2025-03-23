type t = 
  | FuncCall of (string * Expr.t list)
  (* Can object lookup be on another thing than variables? *)
  | PropAssign of (Expr.t * Expr.t * Expr.t)
  (* Assume PropLookup in normalized code is always `<var> = <expr>[<expr>]` *)
  | PropLookup of (string * Expr.t * Expr.t)
  | RelOp of (Binrelop.t * Expr.t * Expr.t)

let to_string (x : t) =
  match x with
  | FuncCall (name, args) -> name ^ "(" ^ String.concat ", " (List.map Expr.to_string args) ^ ")"
  | PropAssign (obj, prop, v) -> Expr.to_string obj ^ "[" ^ Expr.to_string prop ^ "]" ^ " := " ^ Expr.to_string v
  | PropLookup (var, obj, prop) -> var ^ " := " ^ Expr.to_string obj ^ "[" ^ Expr.to_string prop ^ "]"
  | RelOp (op, l, r) -> Expr.to_string l  ^ " " ^ Binrelop.to_string op ^ " " ^ Expr.to_string r
