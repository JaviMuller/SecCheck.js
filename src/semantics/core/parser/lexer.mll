(* ========== OCaml Utilities ========== *)

{
  open Lexing
  open Parser

  let keywords = Hashtbl.of_seq @@ List.to_seq
          [
            (* Language values *)
            "True"         , TRUE;
            "False"        , FALSE;
            
            (* Quantifier operators *)
            "exists"       , QEXISTS;
            "∃"            , QEXISTS;
            (* "forall"       , qforall;
            "∀"            , qforall; *)

            (* Classical logical operators *)
            "¬"            , LNOT;
            "~"            , LNOT;
            "∧"            , LAND;
            "&"            , LAND;
            "∨"            , LOR;
            "|"            , LOR;
            "→"            , LIMPLIES;
            "->"           , LIMPLIES;
            "↔"            , LBICONDIMPL;
            "<->"          , LBICONDIMPL;

            (* LTL operators *)
            "X"            , NEXT;
            "○"            , NEXT;
            "F"            , EVENTUALLY;
            "◊"            , EVENTUALLY;
            "G"            , ALWAYS;
            "□"            , ALWAYS;
            "U"            , UNTIL;
            "𝒰"            , UNTIL;
            "W"            , WEAKUNTIL;
            "𝒲"            , WEAKUNTIL;

            (* CTL operators *)
            (*"E"            , CTLEXISTS;*)
            (*"A"            , CTLALL;*)
          ]

  exception Syntax_error of string

  let create_string (lexbuf : Lexing.lexbuf) (read_string : Lexing.lexbuf -> token) : token =
    let start_p = lexbuf.lex_start_p in
    let token = read_string lexbuf in
    lexbuf.lex_start_p <- start_p;
    token

  let create_syntax_error ?(eof=false) (msg : string) (lexbuf : Lexing.lexbuf) : exn =
    let c = Lexing.lexeme lexbuf in
    let formatted_msg = (
      match eof with
      | true  -> Printf.sprintf "%s. Line number: %d." msg (lexbuf.lex_curr_p.pos_lnum)
      | false -> Printf.sprintf "%s: %s. Line number: %d." msg c (lexbuf.lex_curr_p.pos_lnum)
    ) in (Syntax_error formatted_msg)
}


(* ========== Regular expressions ========== *)

let digit        = ['0' - '9']
let letter       = ['a' - 'z' 'A' - 'Z']
(* let int          = '-'? digit+  *)
let id           = (letter | '_') (letter | digit | '_')* '\''*
let white        = (' ' | '\t')+
let newline      = '\r' | '\n' | "\r\n"


(* ========== Lexical rules ========== *)

rule read =
  parse
  | white            { read lexbuf }
  | newline          { new_line lexbuf; read lexbuf }
  | ','              { COMMA }
  | '.'              { DOT }
  | '('              { LPAREN }
  | ')'              { RPAREN }
  | '['              { LBRACK }
  | ']'              { RBRACK }
  | '='              { DEFEQ }
  | id as x          { try Hashtbl.find keywords x with Not_found -> ID x }
  | "//"             { read_line_comment lexbuf }
  | "/*"             { read_block_comment lexbuf }
  | _                { raise (create_syntax_error "Unexpected char" lexbuf) }
  | eof              { EOF }


(* ========== String reader ========== *)

and read_line_comment =
  parse
  | newline          { new_line lexbuf; read lexbuf }
  | _                { read_line_comment lexbuf }

and read_block_comment =
  parse
  | "*/"             { read lexbuf }
  | newline          { new_line lexbuf; read_block_comment lexbuf }
  | _                { read_block_comment lexbuf }
  | eof              { raise (create_syntax_error ~eof:true "Comment is not terminated" lexbuf) }





