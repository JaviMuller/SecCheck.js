type t =
  | Action of Paction.t
  | Seq of t list
  | Opt of t list
  | KStar of t

module LocMap = Map.Make (struct
  type t = Ploc.t
  let compare = compare
end)

module IntMap = Map.Make (Int)

let t_to_eps_tbl (trc : t) : (int, Ptransition.eps list) Hashtbl.t =
  let tbl : (int, Ptransition.eps list) Hashtbl.t = Hashtbl.create 500 in
  let rec aux (trc : t) (in_state : int) (stack : (int * int * int list) LocMap.t) (n_state : int) : int * (int * int * int list) LocMap.t * int =
    match trc with
    | Action a ->
      (match a with
       | FuncCall (f, _) ->
         (match LocMap.find_opt f stack with
          (* Recursive Call -> Find first call and add a transition to the first call body *)
          | Some (body, _, _) -> 
            let tr : Ptransition.eps = { src = in_state; dest = body; lbl = Action a } in
            if Hashtbl.mem tbl in_state then
              let prev_trs = Hashtbl.find tbl in_state in
              Hashtbl.replace tbl in_state (tr :: prev_trs)
            else 
              Hashtbl.add tbl in_state [tr];
            let stack' = LocMap.update f (fun sf ->
              (match sf with
              | Some (s_bdy, s_ret, oth_calls) -> Some (s_bdy, s_ret, n_state :: oth_calls) 
              | None -> None)) stack in
            (n_state, stack', n_state + 1)
          (* Non-recusive Call -> Add a new frame to stack, add a transition to the next state. Create a return state *)
          | None ->
            let s_body = n_state in
            let s_ret = n_state + 1 in
            let n_state' = n_state + 2 in
            let tr : Ptransition.eps = { src = in_state; dest = s_body; lbl = Action a } in
            if Hashtbl.mem tbl in_state then
              let prev_trs = Hashtbl.find tbl in_state in
              Hashtbl.replace tbl in_state (tr :: prev_trs)
            else 
              Hashtbl.add tbl in_state [tr];
            let stack' = LocMap.add f (s_body, s_ret, []) stack in
            (n_state, stack', n_state'))
       | FuncRet (f, _) ->
          (match LocMap.find_opt f stack with
           | Some (_, s_ret, oth_calls) ->
             let tr : Ptransition.eps = { src = in_state; dest = s_ret; lbl = Action a } in
             if Hashtbl.mem tbl in_state then
               let prev_trs = Hashtbl.find tbl in_state in
               Hashtbl.replace tbl in_state (tr :: prev_trs)
             else 
               Hashtbl.add tbl in_state [tr];
             List.iter (fun s ->
               let tr : Ptransition.eps = { src = in_state; dest = s; lbl = Action a } in
               if Hashtbl.mem tbl in_state then
                 let prev_trs = Hashtbl.find tbl in_state in
                 Hashtbl.replace tbl in_state (tr :: prev_trs)
               else
                 Hashtbl.add tbl in_state [tr]) oth_calls;
             let stack' = LocMap.remove f stack in
             (s_ret, stack', n_state)
           | None ->
             failwith @@ "Return (" ^ Paction.to_string a ^ ") not matching Call")
       | _ ->
         let tr : Ptransition.eps = { src = in_state; dest = n_state; lbl = Action a } in
         if Hashtbl.mem tbl in_state then
           let prev_trs = Hashtbl.find tbl in_state in
           Hashtbl.replace tbl in_state (tr :: prev_trs)
         else
           Hashtbl.add tbl in_state [tr];
         (n_state, stack, n_state + 1))
    | Seq ts ->
      List.fold_left (fun (acc : int * (int * int * int list) LocMap.t * int) (tr : t) ->
        let in_state, stack, n_state = acc in
        aux tr in_state stack n_state) (in_state, stack, n_state) ts
    | Opt ts ->
      let out_states, n_state' = List.fold_left (fun (acc : (int list * int)) (tr : t) ->
        let out_states, n_state = acc in
        let tr' : Ptransition.eps = { src = in_state; dest = n_state; lbl = Epsilon } in
        if Hashtbl.mem tbl in_state then
          let prev_trs = Hashtbl.find tbl in_state in
          Hashtbl.replace tbl in_state (tr' :: prev_trs)
        else
          Hashtbl.add tbl in_state [tr'];
        let out_s, _, n_state' = aux tr n_state stack (n_state + 1) in
        (out_s :: out_states, n_state')) ([], n_state) ts in
      let _ = List.fold_left (fun _ s ->
        let tr : Ptransition.eps = { src = s; dest = n_state'; lbl = Epsilon } in
        if Hashtbl.mem tbl s then
          let prev_trs = Hashtbl.find tbl s in
          Hashtbl.replace tbl s (tr :: prev_trs)
        else
          Hashtbl.add tbl s [tr]) () out_states in
      (n_state', stack, n_state' + 1)
    | KStar t ->
      let s_out = n_state in
      let s_in = n_state + 1 in
      let n_state = n_state + 2 in
      let tr_out : Ptransition.eps = { src = in_state; dest = s_out; lbl = Epsilon } in
      if Hashtbl.mem tbl in_state then
        let prev_trs = Hashtbl.find tbl in_state in
        Hashtbl.replace tbl in_state (tr_out :: prev_trs)
      else
        Hashtbl.add tbl in_state [tr_out];
      let tr_in : Ptransition.eps = { src = in_state; dest = s_in; lbl = Epsilon } in
      if Hashtbl.mem tbl in_state then
        let prev_trs = Hashtbl.find tbl in_state in
        Hashtbl.replace tbl in_state (tr_in :: prev_trs)
      else
        Hashtbl.add tbl in_state [tr_in];
      let out_state, _, n_state' = aux t s_in stack n_state in
      let tr_rep : Ptransition.eps = { src = out_state; dest = in_state; lbl = Epsilon } in
      if Hashtbl.mem tbl out_state then
        let prev_trs = Hashtbl.find tbl out_state in
        Hashtbl.replace tbl out_state (tr_rep :: prev_trs)
      else
        Hashtbl.add tbl out_state [tr_rep];
      (s_out, stack, n_state') in
  let _ = aux trc 1 LocMap.empty 2 in
  tbl

let eps_closure (s : int) (tbl : (int, Ptransition.eps list) Hashtbl.t) : int list =
  let rec aux (s : int) (tbl : (int, Ptransition.eps list) Hashtbl.t) (acc : int list) : int list =
    if List.mem s acc then
      acc
    else
      match Hashtbl.find_opt tbl s with
      | None -> s :: acc
      | Some trs ->
        let acc' = List.fold_left (fun acc tr ->
          match Ptransition.eps_get_lbl tr, Ptransition.eps_get_dest tr with
          | Epsilon, dest ->
            aux dest tbl acc
          | _, _ -> acc) acc trs in
        if List.mem s acc' then
          acc'
        else
          s :: acc' in
  aux s tbl []

let reach (s : int) (tbl : (int, Ptransition.t list) Hashtbl.t) : int list =
  let rec aux (s : int) (tbl : (int, Ptransition.t list) Hashtbl.t) (acc : int list) =
    if List.mem s acc then
      acc
    else
      match Hashtbl.find_opt tbl s with
      | None -> s :: acc
      | Some trs ->
        let acc' = List.fold_left (fun acc tr ->
          let dest = Ptransition.get_dest tr in
          aux dest tbl acc) acc trs in
        if List.mem s acc' then
          acc'
        else
          s :: acc' in
  aux s tbl []

let eps_tbl_to_conc_tbl (tbl : (int, Ptransition.eps list) Hashtbl.t) : (int, Ptransition.t list) Hashtbl.t =
  let res : (int, Ptransition.t list) Hashtbl.t = Hashtbl.create 500 in
  (* Epsilon-removal *)
  let states = Hashtbl.fold (fun k v acc -> 
    let acc' = List.fold_left (fun acc tr -> 
      let dest = Ptransition.eps_get_dest tr in
      if dest > acc then dest else acc) acc v in
    if k > acc' then k else acc') tbl 0 in
  for i = 1 to states do
    (* Find the epsilon-closure *)
    let e_cl = eps_closure i tbl in
    (* Remove epsilon transitions at the current state *)
    match Hashtbl.find_opt tbl i with
    | None -> ()
    | Some trs -> Hashtbl.add res i @@ List.filter_map (fun tr : Ptransition.t option ->
          match Ptransition.eps_get_lbl tr with
          | Epsilon -> None
          | Action a -> Some { src = i; dest = Ptransition.eps_get_dest tr; lbl = a }) trs;
    (* Copy non-epsilon transitions of the closure states *)
    List.iter (fun s ->
      match Hashtbl.find_opt tbl s with
      | None -> ()
      | Some trs -> Hashtbl.add res i @@ List.filter_map (fun tr : Ptransition.t option ->
          match Ptransition.eps_get_lbl tr with
          | Epsilon -> None
          | Action a -> Some { src = i; dest = Ptransition.eps_get_dest tr; lbl = a }) trs) e_cl
  done;
  (* Unreachable removal and state renaming *)
  let s_list = List.sort_uniq compare @@ reach 1 res in
  let _, map = List.fold_left (fun acc s ->
    let n_state, map = acc in
    (n_state + 1, IntMap.add s n_state map)) (1, IntMap.empty) s_list in
  Hashtbl.filter_map_inplace (fun k v ->
    match IntMap.find_opt k map with
    | None -> None
    | Some _ -> Some (List.map (fun tr : Ptransition.t -> 
      { src = IntMap.find (Ptransition.get_src tr) map;
        dest = IntMap.find (Ptransition.get_dest tr) map;
        lbl = Ptransition.get_lbl tr}) v)) res;
  res