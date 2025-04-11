let () = 
  if Array.length Sys.argv <> 2 then
    print_endline "Usage: dune exec trusted-cpg <filename>"
  else 
    let filename = Sys.argv.(1) in
    let f = TcpgSemantics.Parsing.parse_query_from_file filename in
    let pretty_json = Yojson.Safe.pretty_to_string @@ TcpgSyntax.Query.to_yojson f in
    (* print_endline @@ TcpgSyntax.Query.to_string f *)
    print_endline pretty_json
