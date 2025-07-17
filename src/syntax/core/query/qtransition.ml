type t =
  {
    src  : int;
    dest : int;
    lbl  : Qaction.t;
  } [@@deriving yojson]

let to_string ({src; dest; lbl}: t) : string =
  Printf.sprintf "%d -> %d : %s" src dest @@ Qaction.to_string lbl

let get_src (tr : t) : int =
  tr.src

let get_dest (tr : t) : int =
  tr.dest

let get_lbl (tr : t) : Qaction.t =
  tr.lbl
