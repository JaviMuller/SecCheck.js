type t = {
  src    : int;
  dest   : int;
  lbl : Paction.t;
}[@@deriving yojson]

let to_string (tr : t) : string =
  Printf.sprintf "%d -> %d : %s" tr.src tr.dest (Paction.to_string tr.lbl)

let get_src (tr : t) : int =
  tr.src

let get_dest (tr : t) : int =
  tr.dest

let get_lbl (tr : t) : Paction.t =
  tr.lbl

type eps = {
  src  : int;
  dest : int;
  lbl  : Paction.eps;
}[@@deriving yojson]

let eps_get_src (tr : eps) : int =
  tr.src

let eps_get_dest (tr : eps) : int =
  tr.dest

let eps_get_lbl (tr: eps) : Paction.eps =
  tr.lbl