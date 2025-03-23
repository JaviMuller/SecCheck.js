type t =
  | Property of (string * string list * Predicate.t list * Trace.t list)

let to_string (s : t) : string =
  match s with
  | Property (name, vars, preds, traces) ->
      "Prop " ^ name ^ "(" ^ (String.concat ", " vars) ^ ") {\n" ^
      "  " ^ String.concat ", " (List.map Predicate.to_string preds) ^ "\n" ^
      "  ;\n" ^
      "  " ^ String.concat ", " (List.map Trace.to_string traces) ^ "\n" ^
      "}"

