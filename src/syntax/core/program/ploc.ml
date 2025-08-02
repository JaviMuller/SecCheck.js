type t = Loc of string

let to_string (l : t) : string =
  match l with
  | Loc s -> s

let to_yojson (l : t) : Yojson.Safe.t =
  match l with
  | Loc s -> `String s

let of_yojson (json : Yojson.Safe.t) : (t, string) result = 
  try
    match json with
    | `String s -> Ok (Loc s)
    | _ -> Error "Unexpected JSON for location"
  with
  | e ->
    Error ("Unexpected error: " ^ Printexc.to_string e)