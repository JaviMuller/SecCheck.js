let () = 
  if Array.length Sys.argv <> 2 then
    print_endline "Usage: dune exec trusted-cpg <filename>"
  else 
    let filename = Sys.argv.(1) in
    let f = TcpgSemantics.Parsing.parse_formula_from_file filename in
    print_endline @@ TcpgSyntax.Formula.to_string f
