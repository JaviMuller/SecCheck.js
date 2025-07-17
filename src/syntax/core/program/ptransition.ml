type t = {
  src    : int;
  dest   : int;
  action : Paction.t;
}[@@deriving yojson]

let to_string (tr: t) : string =
  Printf.sprintf "%d -> %d : %s" tr.src tr.dest (Paction.to_string tr.action)

let get_src (tr : t) : int =
  tr.src

let get_dest (tr : t) : int =
  tr.dest

let get_action (tr : t) : Paction.t =
  tr.action