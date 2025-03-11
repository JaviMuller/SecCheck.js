type t =
  | True 
  | False
  | Variable of string
  | Not of t
  | And of (t * t)
  | Or of (t * t)
  | Implication of (t * t) 
  | BicondImplication of (t * t)
  | Exists of (string list * t)
  | Forall of (string list * t)

let rec to_string (f : t) : string =
  match f with
  | True -> "⊤"
  | False -> "⊥"
  | Variable x -> x
  | Not f' -> "¬" ^ (to_string f')
  | And (f1, f2) -> " (" ^ (to_string f1) ^ " ∧ " ^ (to_string f2) ^ ") "
  | Or (f1, f2) -> " (" ^ (to_string f1) ^ " ∨ " ^ (to_string f2) ^ ") "
  | Implication (f1, f2) -> " (" ^ (to_string f1) ^ " -> " ^ (to_string f2) ^ ") "
  | BicondImplication (f1, f2) -> " (" ^ (to_string f1) ^ " <-> " ^ (to_string f2) ^ ") "
  | Exists (args, f') -> "\u{2203} " ^ String.concat ", " args ^ " ." ^ (to_string f') 
  | Forall (args, f') -> "\u{2200} " ^ String.concat ", " args ^ " ." ^ (to_string f') 

