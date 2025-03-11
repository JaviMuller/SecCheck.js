(* ========== OCaml Utilities ========== *)

{
  open Lexing
  open Parser

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
  | white                        { read lexbuf }
  | newline                      { new_line lexbuf; read lexbuf }
  | ','                          { COMMA }
  | '.'                          { DOT }
  | '('                          { LPAREN }
  | ')'                          { RPAREN }
  | "True"                       { TRUE }
  | '\xE2' '\x8A' '\xA4'         { TRUE }
  | "False"                      { FALSE}
  | '\x22' '\xA5'                { FALSE }
  | '\xE2' '\x88' '\x83'         { QEXISTS }
  | '\xE2' '\x88' '\xA4'         { QFORALL }
  | '\xC2' '\xAC'                { LNOT }
  | '!'                          { LNOT }
  | '\xE2' '\x88' '\xA7'         { LAND }
  | '&'                          { LAND }
  | '\xE2' '\x88' '\xA8'         { LOR }
  | '|'                          { LOR }
  | 'X'                          { NEXT }
  | '\xE2' '\x97' '\x8B'         { NEXT }
  | 'F'                          { EVENTUALLY }
  | '\xE2' '\x97' '\x8A'         { EVENTUALLY }
  | 'G'                          { ALWAYS }
  | '\xE2' '\x96' '\xA1'         { ALWAYS }
  | 'U'                          { UNTIL }
  | '\xF0' '\x9D' '\x92' '\xB0'  { UNTIL }
  | 'W'                          { WEAKUNTIL }
  | '\xF0' '\x9D' '\x92' '\xB2'  { WEAKUNTIL }
  | "->"                         { LIMPLIES }
  | "<->"                        { LBICONDIMPL }
  | "exists"                     { QEXISTS }
  | "forall"                     { QFORALL }
  | id as x                      { ID x }
  | "//"                         { read_line_comment lexbuf }
  | "/*"                         { read_block_comment lexbuf }
  | _                            { raise (create_syntax_error "Unexpected char" lexbuf) }
  | eof                          { EOF }


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





