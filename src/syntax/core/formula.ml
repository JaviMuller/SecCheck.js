type t =
  (* Primeira Ordem *)
  | True 
  | False
  | Not of t
  | And of t list
  | Or of t list
  | Implication of (t * t)
  | Equiv of (t * t)
  | Exists of (string list * t)
  | Forall of (string list * t)
  (* LTL *)
  | Next of t 
  | Eventually of t
  | Always of t
  | Until of (t * t)
  | WeakUntil of (t * t)
  (* Abstraction - Predicates *) 
  | PredAssertion of (string * Expr.t)
  (* Program Trace Reasoning *)
  | RelOp of (Binrelop.t * Expr.t * Expr.t)
  | Tainted of Expr.t 
  | PropAssign of (Expr.t * Expr.t * Expr.t)
  | PropLookUp of (Expr.t * Expr.t * Expr.t)
  | Call of (Expr.t * Expr.t list)

let rec flatten (f : t) : t =
  match f with
  | Not f' -> Not (flatten f')
  | And lst ->
      let flattened_list = List.fold_left (fun acc subformula ->
                            match flatten subformula with
                            | And sublist -> acc @ sublist
                            | other -> acc @ [other]) [] lst in
      And flattened_list
  | Or lst ->
      let flattened_list = List.fold_left (fun acc subformula ->
                            match flatten subformula with
                            | Or sublist -> acc @ sublist
                            | other -> acc @ [other]) [] lst in
      Or flattened_list
  | Implication (f1, f2) -> Implication (flatten f1, flatten f2)
  | Equiv (f1, f2) -> Equiv (flatten f1, flatten f2)
  | Exists (vars, f') -> Exists (vars, flatten f')
  | Forall (vars, f') -> Forall (vars, flatten f')
  | Next f' -> Next (flatten f') 
  | Eventually f' -> Eventually (flatten f')
  | Always f' -> Always (flatten f')
  | Until (f1, f2) -> Until (flatten f1, flatten f2)
  | WeakUntil (f1, f2) -> WeakUntil (flatten f1, flatten f2)
  | _ -> f

let rec to_string_aux (f : t) : string =
  match f with
  | True -> "\u{22A4}"
  | False -> "\u{22A5}"
  | Not f' -> "\u{00AC} " ^ (to_string_aux f')
  | And fs -> "(" ^ String.concat " \u{2227} " (List.map to_string_aux fs) ^ ")"
  | Or fs -> "(" ^ String.concat " \u{2228} " (List.map to_string_aux fs) ^ ")"
  | Implication (f1, f2) -> " (" ^ (to_string_aux f1) ^ " -> " ^ (to_string_aux f2) ^ ")"
  | Equiv (f1, f2) -> "(" ^ (to_string_aux f1) ^ " <-> " ^ (to_string_aux f2) ^ ")"
  | Exists (args, f') -> "\u{2203} " ^ String.concat ", " args ^ " : " ^ (to_string_aux f') 
  | Forall (args, f') -> "\u{2200} " ^ String.concat ", " args ^ " : " ^ (to_string_aux f') 
  | Next f' -> "\u{20DD}  " ^ (to_string_aux f')
  | Eventually f' -> "\u{25C7} " ^ (to_string_aux f') 
  | Always f' -> "\u{25A1} " ^ (to_string_aux f')
  | Until (f1, f2) -> "(" ^ (to_string_aux f1) ^ "\u{1D4B0}" ^ (to_string_aux f2) ^ ")"
  | WeakUntil (f1, f2) -> "(" ^ (to_string_aux f1) ^ "\u{1D4B2}" ^ (to_string_aux f2) ^ ")"
  | PredAssertion (name, expr) -> "." ^ name ^ "(" ^ (Expr.to_string expr) ^ ")"
  | RelOp (op, e1, e2) -> (Expr.to_string e1) ^ (Binrelop.to_string op) ^ (Expr.to_string e2)
  | Tainted e -> ".tainted(" ^ (Expr.to_string e) ^ ")"
  | PropAssign (e1, e2, e3) -> "(" ^ (Expr.to_string e1) ^ "[" ^ (Expr.to_string e2) ^ "] := " ^ (Expr.to_string e3) ^ ")"
  | PropLookUp (e1, e2, e3) -> "(" ^ (Expr.to_string e1) ^ " := " ^ (Expr.to_string e2) ^ "[" ^ (Expr.to_string e3) ^ "]" ^ ")"
  | Call (f, args) -> (Expr.to_string f) ^ "(" ^ (String.concat ", " @@ List.map Expr.to_string args) ^ ")"

let to_string (f : t) : string =
  to_string_aux @@ flatten f
