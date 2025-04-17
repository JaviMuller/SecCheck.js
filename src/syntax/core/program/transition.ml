type t = {
  src    : int;
  dest   : int;
  action : Paction.t;
}[@@deriving yojson]

let to_string (t : t) : string =
  Printf.sprintf "%d -> %d : %s" t.src t.dest (Paction.to_string t.action)

let get_src (t : t) : int =
  t.src

let get_dest (t : t) : int =
  t.dest

let get_action (t : t) : Paction.t =
  t.action