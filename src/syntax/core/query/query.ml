type t =
  {
    name      : string;
    vars      : string list;
    invariant : Invformula.t;
    tracef    : Trcformula.t;
  } [@@deriving yojson]

let to_string (q : t) : string =
  "Prop " ^ q.name ^ "(" ^ (String.concat ", " q.vars) ^ ") {\n" ^
  "  " ^ Invformula.to_string q.invariant ^ "\n" ^
  "  ;\n" ^
  "  " ^ Trcformula.to_string q.tracef ^ "\n" ^
  "}"

let get_name ({name; _} : t) : string = name

let get_invariant ({invariant; _} : t) : Invformula.t = invariant

let get_trace_formula ({tracef; _} : t) : Trcformula.t = tracef