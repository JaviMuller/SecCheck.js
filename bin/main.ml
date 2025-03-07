let () = 
  let f = TcpgSemantics.Parsing.parse_formula_from_file "banana.txt" in
  print_endline @@ TcpgSyntax.Formula.to_string f
