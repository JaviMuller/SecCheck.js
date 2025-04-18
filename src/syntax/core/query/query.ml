type t =
  {
    name    : string;
    vars    : string list;
    formula : Formula.t;
    trace   : Qaction.t list;
  } [@@deriving yojson]

let next_state (_ : t) (s : int) =
  s + 1

let to_string (q : t) : string =
  "Prop " ^ q.name ^ "(" ^ (String.concat ", " q.vars) ^ ") {\n" ^
  "  " ^ Formula.to_string q.formula ^ "\n" ^
  "  ;\n" ^
  "  " ^ String.concat ", " (List.map Qaction.to_string q.trace) ^ "\n" ^
  "}"

let get_state_action ({trace; _} : t) (s : int) : Qaction.t = List.nth trace s

let get_formula ({formula; _} : t) : Formula.t =
  formula

let is_final ({trace; _} : t) (s : int) : bool =
  List.length trace = s