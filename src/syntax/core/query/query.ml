type t =
  { 
    name    : string;
    vars    : string list;
    formula : Formula.t;
    trace   : Action.t list;
  } [@@deriving yojson]

let to_string (q : t) : string =
  "Prop " ^ q.name ^ "(" ^ (String.concat ", " q.vars) ^ ") {\n" ^
  "  " ^ Formula.to_string q.formula ^ "\n" ^
  "  ;\n" ^
  "  " ^ String.concat ", " (List.map Action.to_string q.trace) ^ "\n" ^
  "}"
