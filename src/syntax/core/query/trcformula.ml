open Yojson.Safe.Util

let current = ref 0

let fresh_id () : string =
  let res = String.make 1 @@ Char.chr (Char.code 'a' + !current) in
  incr current;
  res

let sequence_results (results: ('a, string) result list) : ('a list, string) result =
  let rec aux acc = function
  | [] -> Ok (List.rev acc)
  | (Ok x) :: xs -> aux (x :: acc) xs
  | (Error m) :: _ -> Error m in
  aux [] results

let find_key_from_val_opt (tbl : ('a, 'b) Hashtbl.t) (v : 'b) : 'a option =
  Hashtbl.fold (fun key value acc ->
    (match acc with
    | Some k -> Some k
    | None -> if value = v then Some key else None)) tbl None

let get_predicate_of_action (tbl : (string, Qaction.t) Hashtbl.t) (a : Qaction.t) : string =
  match find_key_from_val_opt tbl a with
  | Some k -> k
  | None ->
    let k = fresh_id () in
    Hashtbl.add tbl k a;
    k

module Future = struct
  type t =
    | True
    | False
    | Action of Qaction.t
    | Not of t
    | And of t list
    | Or of t list
    | Implication of (t * t)
    | Equivalence of (t * t)
    | Sequence of t list
    | Next of t
    | WeakNext of t
    | Until of (t * t)
    | Release of (t * t)
    | Eventually of t
    | Always of t
  
  let rec to_yojson (f : t) : Yojson.Safe.t =
    match f with
    | True -> `Assoc [ ("type", `String "True") ]
    | False -> `Assoc [ ("type", `String "False")]
    | Action a -> `Assoc [ ("type", `String "Action");
                           ("value", Qaction.to_yojson a) ]
    | Not t -> `Assoc [ ("type", `String "Not");
                        ("value", to_yojson t) ]
    | And ts -> `Assoc [ ("type", `String "And");
                         ("value", `List (List.map to_yojson ts)) ]
    | Or ts -> `Assoc [ ("type", `String "Or");
                        ("value", `List (List.map to_yojson ts)) ]
    | Implication (t1, t2) -> `Assoc [ ("type", `String "Implication");
                                       ("left", to_yojson t1);
                                       ("right", to_yojson t2) ]
    | Equivalence (t1, t2) -> `Assoc [ ("type", `String "Equivalence");
                                       ("left", to_yojson t1);
                                       ("right", to_yojson t2) ]
    | Sequence ts -> `Assoc [ ("type", `String "Sequence");
                              ("value", `List (List.map to_yojson ts)) ]
    | Next t -> `Assoc [ ("type", `String "Next");
                         ("value", to_yojson t) ]
    | WeakNext t -> `Assoc [ ("type", `String "WeakNext");
                             ("value", to_yojson t) ]
    | Until (t1, t2) -> `Assoc [ ("type", `String "Until");
                                 ("left", to_yojson t1);
                                 ("right", to_yojson t2) ]
    | Release (t1, t2) -> `Assoc [ ("type", `String "Release");
                                   ("left", to_yojson t1);
                                   ("right", to_yojson t2) ]
    | Eventually t -> `Assoc [ ("type", `String "Eventually");
                               ("value", to_yojson t) ]
    | Always t -> `Assoc [ ("type", `String "Always");
                        ("value", to_yojson t) ]

  let rec of_yojson (json : Yojson.Safe.t) : (t, string) result =
    try
      let json_to_string = Yojson.Safe.Util.to_string in
      match json with
      | `Assoc _ ->
        let f_type = member "type" json |> json_to_string in
        (
          match f_type with
          | "True" -> Ok (True)
          | "False" -> Ok (False)
          | "Action" ->
            let qact = member "value" json |> Qaction.of_yojson in
            (match qact with
             | Ok a -> Ok (Action a)
             | Error m -> Error m)
          | "Not" ->
            let f = member "value" json |> of_yojson in
            (match f with
             | Ok f -> Ok (Not f)
             | Error m -> Error m)
          | "And" ->
            let fs = member "value" json |> to_list |> List.map of_yojson |> sequence_results in
            (match fs with
             | Ok fs -> Ok (And fs)
             | Error m -> Error m)
          | "Or" ->
            let fs = member "value" json |> to_list |> List.map of_yojson |> sequence_results in
            (match fs with
             | Ok fs -> Ok (Or fs)
             | Error m -> Error m)
          | "Implication" ->
            let f1 = member "left" json |> of_yojson in
            let f2 = member "right" json |> of_yojson in
            (match f1, f2 with
             | Ok f1, Ok f2 -> Ok (Implication (f1, f2))
             | Error m, _ -> Error m
             | _, Error m -> Error m)
          | "Equivalence" ->
            let f1 = member "left" json |> of_yojson in
            let f2 = member "right" json |> of_yojson in
            (match f1, f2 with
             | Ok f1, Ok f2 -> Ok (Equivalence (f1, f2))
             | Error m, _ -> Error m
             | _, Error m -> Error m)
          | "Sequence" ->
            let acts = member "value" json |> to_list |> List.map of_yojson |> sequence_results in
            (match acts with
             | Ok acts -> Ok (Sequence acts)
             | Error m -> Error m)
          | "Next" ->
            let f = member "value" json |> of_yojson in
            (match f with
             | Ok f -> Ok (Next f)
             | Error m -> Error m)
          | "WeakNext" ->
            let f = member "value" json |> of_yojson in
            (match f with
             | Ok f -> Ok (WeakNext f)
             | Error m -> Error m)
          | "Until" ->
            let f1 = member "left" json |> of_yojson in
            let f2 = member "right" json |> of_yojson in
            (match f1, f2 with
             | Ok f1, Ok f2 -> Ok (Until (f1, f2))
             | Error m, _ -> Error m
             | _, Error m -> Error m)
          | "Release" ->
            let f1 = member "left" json |> of_yojson in
            let f2 = member "right" json |> of_yojson in
            (match f1, f2 with
             | Ok f1, Ok f2 -> Ok (Release (f1, f2))
             | Error m, _ -> Error m
             | _, Error m -> Error m)
          | "Eventually" ->
            let f = member "value" json |> of_yojson in
            (match f with
             | Ok f -> Ok (Eventually f)
             | Error m -> Error m)
          | "Always" ->
            let f = member "value" json |> of_yojson in
            (match f with
             | Ok f -> Ok (Always f)
             | Error m -> Error m)
          | s ->
            Error ("Unknown formula type: " ^ s))
      | _ ->
        Error "Expected a JSON object at the top level"
    with
    | Type_error (msg, _) ->
      Error ("Type error during parsing: " ^ msg)
    | e ->
      Error ("Unexpected error: " ^ Printexc.to_string e)

  let rec flatten (f : t) : t =
    match f with
    | Not f' -> Not (flatten f')
    | And fs -> 
      let flattened_list = List.fold_left (fun acc subformula ->
                                           match flatten subformula with
                                           | And sublist -> acc @ sublist
                                           | other -> acc @ [other]) [] fs in
      And flattened_list
    | Or fs ->
      let flattened_list = List.fold_left (fun acc subformula ->
                                           match flatten subformula with
                                           | Or sublist -> acc @ sublist
                                           | other -> acc @ [other]) [] fs in
      Or flattened_list
    | Implication (f1, f2) -> Implication (flatten f1, flatten f2)
    | Equivalence (f1, f2) -> Equivalence (flatten f1, flatten f2)
    | Sequence fs ->
      let flattened_list = List.fold_left (fun acc subformula ->
                                           match flatten subformula with
                                           | Sequence sublist -> acc @ sublist
                                           | other -> acc @ [other]) [] fs in
      Sequence flattened_list
    | Next f -> Next (flatten f)
    | WeakNext f -> WeakNext (flatten f)
    | Until (f1, f2) -> Until (flatten f1, flatten f2)
    | Release (f1, f2) -> Release (flatten f1, flatten f2)
    | Eventually f -> Eventually (flatten f)
    | Always f -> Always (flatten f)
    | _ -> f

  let rec to_ltlf2dfa_aux (binds : (string, Qaction.t) Hashtbl.t) (f : t) : string =
    match f with
    | True -> "true"
    | False -> "false"
    | Action a -> 
      get_predicate_of_action binds a
    | Not f' -> "!(" ^ (to_ltlf2dfa_aux binds f') ^ ")"
    | And fs -> "(" ^ String.concat " & " (List.map (to_ltlf2dfa_aux binds) fs) ^ ")"
    | Or fs -> "(" ^ String.concat " | " (List.map (to_ltlf2dfa_aux binds) fs) ^ ")"
    | Implication (f1, f2) -> "(" ^ (to_ltlf2dfa_aux binds f1) ^ " -> " ^ (to_ltlf2dfa_aux binds f2) ^ ")"
    | Equivalence (f1, f2) -> "(" ^ (to_ltlf2dfa_aux binds f1) ^ " <-> " ^ (to_ltlf2dfa_aux binds f2) ^ ")"
    | Sequence [f'] -> to_ltlf2dfa_aux binds f'
    | Sequence (f' :: fs) -> 
      "F(" ^ (to_ltlf2dfa_aux binds f') ^ " & X(F(" ^ (seq_to_ltlf2dfa binds fs) ^ ")))"
    | Next f' -> "X(" ^ (to_ltlf2dfa_aux binds f') ^ ")"
    | WeakNext f' -> "WX(" ^ (to_ltlf2dfa_aux binds f') ^ ")"
    | Until (f1, f2) -> "(" ^ (to_ltlf2dfa_aux binds f1) ^ " U " ^ (to_ltlf2dfa_aux binds f2) ^ ")"
    | Release (f1, f2) -> "(" ^ (to_ltlf2dfa_aux binds f1) ^ " R " ^ (to_ltlf2dfa_aux binds f2) ^ ")"
    | Eventually f' -> "F(" ^ (to_ltlf2dfa_aux binds f') ^ ")"
    | Always f' -> "G(" ^ (to_ltlf2dfa_aux binds f') ^ ")"
    | _ -> ""
  and seq_to_ltlf2dfa (binds: (string, Qaction.t) Hashtbl.t) (fs : t list) : string =
    match fs with
    | [] -> ""
    | [f] -> to_ltlf2dfa_aux binds f
    | f :: fs' -> (to_ltlf2dfa_aux binds f) ^ " & X(F(" ^ (seq_to_ltlf2dfa binds fs') ^ "))"

  let to_ltlf2dfa (binds: (string, Qaction.t) Hashtbl.t) (f : t) : string =
    to_ltlf2dfa_aux binds (flatten f)
  
  let rec to_string_aux (f : t) : string =
    match f with
    | True -> "true"
    | False -> "false"
    | Action a -> Qaction.to_string a
    | Not f' -> "!(" ^ (to_string_aux f') ^ ")"
    | And fs -> "(" ^ String.concat " & " (List.map to_string_aux fs) ^ ")"
    | Or fs -> "(" ^ String.concat " | " (List.map to_string_aux fs) ^ ")"
    | Implication (f1, f2) -> "(" ^ (to_string_aux f1) ^ " -> " ^ (to_string_aux f2) ^ ")"
    | Equivalence (f1, f2) -> "(" ^ (to_string_aux f1) ^ " <-> " ^ (to_string_aux f2) ^ ")"
    | Sequence fs -> "(" ^ String.concat "; " (List.map to_string_aux fs) ^ ")"
    | Next f' -> "X(" ^ (to_string_aux f') ^ ")"
    | WeakNext f' -> "WX(" ^ (to_string_aux f') ^ ")"
    | Until (f1, f2) -> "(" ^ (to_string_aux f1) ^ " U " ^ (to_string_aux f2) ^ ")"
    | Release (f1, f2) -> "(" ^ (to_string_aux f1) ^ " R " ^ (to_string_aux f2) ^ ")"
    | Eventually f' -> "F(" ^ (to_string_aux f') ^ ")"
    | Always f' -> "G(" ^ (to_string_aux f') ^ ")"
  
  let to_string (f : t) : string =
    to_string_aux @@ flatten f
end

module Past = struct
  type t =
    | True
    | False
    | Action of Qaction.t
    | Not of t
    | And of t list
    | Or of t list
    | Implication of (t * t)
    | Equivalence of (t * t)
    | Before of t
    | WeakBefore of t
    | Since of (t * t)
    | PastRelease of (t * t)
    | Once of t
    | Historically of t

  let rec to_yojson (f : t) : Yojson.Safe.t =
    match f with
    | True -> `Assoc [ ("type", `String "True") ]
    | False -> `Assoc [ ("type", `String "False")]
    | Action a -> `Assoc [ ("type", `String "Action");
                           ("value", Qaction.to_yojson a) ]
    | Not t -> `Assoc [ ("type", `String "Not");
                        ("value", to_yojson t) ]
    | And ts -> `Assoc [ ("type", `String "And");
                         ("value", `List (List.map to_yojson ts)) ]
    | Or ts -> `Assoc [ ("type", `String "Or");
                        ("value", `List (List.map to_yojson ts)) ]
    | Implication (t1, t2) -> `Assoc [ ("type", `String "Implication");
                                       ("left", to_yojson t1);
                                       ("right", to_yojson t2) ]
    | Equivalence (t1, t2) -> `Assoc [ ("type", `String "Equivalence");
                                       ("left", to_yojson t1);
                                       ("right", to_yojson t2) ]
    | Before t -> `Assoc [ ("type", `String "Before");
                           ("value", to_yojson t) ]
    | WeakBefore t -> `Assoc [ ("type", `String "WeakBefore");
                               ("value", to_yojson t) ]
    | Since (t1, t2) -> `Assoc [ ("type", `String "Since");
                                 ("left", to_yojson t1);
                                 ("right", to_yojson t2) ]
    | PastRelease (t1, t2) -> `Assoc [ ("type", `String "PastRelease");
                                       ("left", to_yojson t1);
                                       ("right", to_yojson t2) ]
    | Once t -> `Assoc [ ("type", `String "Once");
                         ("value", to_yojson t) ]
    | Historically t -> `Assoc [ ("type", `String "Historically");
                                 ("value", to_yojson t) ]

  let rec of_yojson (json : Yojson.Safe.t) : (t, string) result =
    try
      let json_to_string = Yojson.Safe.Util.to_string in
      match json with
      | `Assoc _ ->
        let f_type = member "type" json |> json_to_string in
        (
          match f_type with
          | "True" -> Ok (True)
          | "False" -> Ok (False)
          | "Action" ->
            let qact = member "value" json |> Qaction.of_yojson in
            (match qact with
             | Ok a -> Ok (Action a)
             | Error m -> Error m)
          | "Not" ->
            let f = member "value" json |> of_yojson in
            (match f with
             | Ok f -> Ok (Not f)
             | Error m -> Error m)
          | "And" ->
            let fs = member "value" json |> to_list |> List.map of_yojson |> sequence_results in
            (match fs with
             | Ok fs -> Ok (And fs)
             | Error m -> Error m)
          | "Or" ->
            let fs = member "value" json |> to_list |> List.map of_yojson |> sequence_results in
            (match fs with
             | Ok fs -> Ok (Or fs)
             | Error m -> Error m)
          | "Implication" ->
            let f1 = member "left" json |> of_yojson in
            let f2 = member "right" json |> of_yojson in
            (match f1, f2 with
             | Ok f1, Ok f2 -> Ok (Implication (f1, f2))
             | Error m, _ -> Error m
             | _, Error m -> Error m)
          | "Equivalence" ->
            let f1 = member "left" json |> of_yojson in
            let f2 = member "right" json |> of_yojson in
            (match f1, f2 with
             | Ok f1, Ok f2 -> Ok (Equivalence (f1, f2))
             | Error m, _ -> Error m
             | _, Error m -> Error m)
          | "Before" ->
            let f = member "value" json |> of_yojson in
            (match f with
             | Ok f -> Ok (Before f)
             | Error m -> Error m)
          | "WeakBefore" ->
            let f = member "value" json |> of_yojson in
            (match f with
             | Ok f -> Ok (WeakBefore f)
             | Error m -> Error m)
          | "Since" ->
            let f1 = member "left" json |> of_yojson in
            let f2 = member "right" json |> of_yojson in
            (match f1, f2 with
             | Ok f1, Ok f2 -> Ok (Since (f1, f2))
             | Error m, _ -> Error m
             | _, Error m -> Error m)
          | "PastRelease" ->
            let f1 = member "left" json |> of_yojson in
            let f2 = member "right" json |> of_yojson in
            (match f1, f2 with
             | Ok f1, Ok f2 -> Ok (PastRelease (f1, f2))
             | Error m, _ -> Error m
             | _, Error m -> Error m)
          | "Once" ->
            let f = member "value" json |> of_yojson in
            (match f with
             | Ok f -> Ok (Once f)
             | Error m -> Error m)
          | "Historically" ->
            let f = member "value" json |> of_yojson in
            (match f with
             | Ok f -> Ok (Historically f)
             | Error m -> Error m)
          | s ->
            Error ("Unknown formula type: " ^ s))
      | _ ->
        Error "Expected a JSON object at the top level"
    with
    | Type_error (msg, _) ->
      Error ("Type error during parsing: " ^ msg)
    | e ->
      Error ("Unexpected error: " ^ Printexc.to_string e)

  let rec flatten (f : t) : t =
    match f with
    | Not f' -> Not (flatten f')
    | And fs -> 
      let flattened_list = List.fold_left (fun acc subformula ->
                                           match flatten subformula with
                                           | And sublist -> acc @ sublist
                                           | other -> acc @ [other]) [] fs in
      And flattened_list
    | Or fs ->
      let flattened_list = List.fold_left (fun acc subformula ->
                                           match flatten subformula with
                                           | Or sublist -> acc @ sublist
                                           | other -> acc @ [other]) [] fs in
      Or flattened_list
    | Implication (f1, f2) -> Implication (flatten f1, flatten f2)
    | Equivalence (f1, f2) -> Equivalence (flatten f1, flatten f2)
    | Before f -> Before (flatten f)
    | WeakBefore f -> WeakBefore (flatten f)
    | Since (f1, f2) -> Since (flatten f1, flatten f2)
    | PastRelease (f1, f2) -> PastRelease (flatten f1, flatten f2)
    | Once f -> Once (flatten f)
    | Historically f -> Historically (flatten f)
    | _ -> f

  let rec to_ltlf2dfa_aux (binds : (string, Qaction.t) Hashtbl.t) (f : t): string =
    match f with
    | True -> "true"
    | False -> "false"
    | Action a -> get_predicate_of_action binds a
    | Not f' -> "!(" ^ (to_ltlf2dfa_aux binds f') ^ ")"
    | And fs -> "(" ^ String.concat " & " (List.map (to_ltlf2dfa_aux binds) fs) ^ ")"
    | Or fs -> "(" ^ String.concat " | " (List.map (to_ltlf2dfa_aux binds) fs) ^ ")"
    | Implication (f1, f2) -> "(" ^ (to_ltlf2dfa_aux binds f1) ^ "->" ^ (to_ltlf2dfa_aux binds f2) ^ ")"
    | Equivalence (f1, f2) -> "(" ^ (to_ltlf2dfa_aux binds f1) ^ "<->" ^ (to_ltlf2dfa_aux binds f2) ^ ")"
    | Before f' -> "X(" ^ (to_ltlf2dfa_aux binds f') ^ ")"
    | WeakBefore f' -> "WX(" ^ (to_ltlf2dfa_aux binds f') ^ ")"
    | Since (f1, f2) -> "(" ^ (to_ltlf2dfa_aux binds f1) ^ "U" ^ (to_ltlf2dfa_aux binds f2) ^ ")"
    | PastRelease (f1, f2) -> "(" ^ (to_ltlf2dfa_aux binds f1) ^ "R" ^ (to_ltlf2dfa_aux binds f2) ^ ")"
    | Once f' -> "F(" ^ (to_ltlf2dfa_aux binds f') ^ ")"
    | Historically f' -> "G(" ^ (to_ltlf2dfa_aux binds f') ^ ")"

  let to_ltlf2dfa (binds : (string, Qaction.t) Hashtbl.t) (f : t) : string =
    to_ltlf2dfa_aux binds (flatten f)

  let rec to_string_aux (f : t): string =
    match f with
    | True -> "true"
    | False -> "false"
    | Action a -> Qaction.to_string a
    | Not f' -> "!(" ^ (to_string_aux f') ^ ")"
    | And fs -> "(" ^ String.concat " & " (List.map to_string_aux fs) ^ ")"
    | Or fs -> "(" ^ String.concat " | " (List.map to_string_aux fs) ^ ")"
    | Implication (f1, f2) -> "(" ^ (to_string_aux f1) ^ "->" ^ (to_string_aux f2) ^ ")"
    | Equivalence (f1, f2) -> "(" ^ (to_string_aux f1) ^ "<->" ^ (to_string_aux f2) ^ ")"
    | Before f' -> "X(" ^ (to_string_aux f') ^ ")"
    | WeakBefore f' -> "WX(" ^ (to_string_aux f') ^ ")"
    | Since (f1, f2) -> "(" ^ (to_string_aux f1) ^ "U" ^ (to_string_aux f2) ^ ")"
    | PastRelease (f1, f2) -> "(" ^ (to_string_aux f1) ^ "R" ^ (to_string_aux f2) ^ ")"
    | Once f' -> "F(" ^ (to_string_aux f') ^ ")"
    | Historically f' -> "G(" ^ (to_string_aux f') ^ ")"

  let to_string (f : t) : string =
    to_string_aux (flatten f)
end

type t = 
  | FutureFormula of Future.t
  | PastFormula of Past.t

let to_yojson (f : t) : Yojson.Safe.t =
  match f with
  | FutureFormula f -> `Assoc [ ("type", `String "FutureFormula");
                                ("value", Future.to_yojson f) ]
  | PastFormula f -> `Assoc [ ("type", `String "PastFormula");
                              ("value", Past.to_yojson f) ]

let of_yojson (json : Yojson.Safe.t) : (t, string) result =
  try
    let json_to_string = Yojson.Safe.Util.to_string in
    match json with
    | `Assoc _ ->
      let f_type = member "type" json |> json_to_string in
      (
        match f_type with
        | "FutureFormula" ->
          let f = member "value" json |> Future.of_yojson in
          (match f with
           | Ok f -> Ok (FutureFormula f)
           | Error m -> Error m)
        | "PastFormula" ->
          let f = member "value" json |> Past.of_yojson in
          (match f with
           | Ok f -> Ok (PastFormula f)
           | Error m -> Error m)
        | s ->
          Error ("Unknown formula type: " ^ s))
    | _ ->
      Error "Expected a JSON object at the top level"
  with
  | Type_error (msg, _) ->
    Error ("Type error during parsing: " ^ msg)
  | e ->
    Error ("Unexpected error: " ^ Printexc.to_string e)

let to_ltlf2dfa (binds : (string, Qaction.t) Hashtbl.t) (f : t) : string =
  match f with
  | FutureFormula f' -> Future.to_ltlf2dfa binds f'
  | PastFormula f' -> Past.to_ltlf2dfa binds f'

let to_string (f : t) : string =
  match f with
  | FutureFormula f' -> Future.to_string f'
  | PastFormula f' -> "(PAST) " ^ Past.to_string f'