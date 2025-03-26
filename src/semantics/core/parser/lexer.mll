(* ========== OCaml Utilities ========== *)

{
  open Lexing
  open Parser

  let keywords = Hashtbl.of_seq @@ List.to_seq
    [
        "Prop"                   , PROP;
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

let digit         = ['0' - '9']
let letter        = ['a' - 'z' 'A' - 'Z']
let int           = '-'? digit+
let frac          = '.' digit*
let exp           = ['e' 'E'] ['-' '+']? digit+
let float         = digit+ frac? exp? | frac exp? | "nan" | "inf"
let bool          = "true" | "false"
let id            = ('_') (letter | digit | '_')+ '\''* | (letter) (letter | digit | '_')* '\''*
let gid           = '|' (id) '|'
let symbol        = '\'' (id | int)
let white         = (' ' | '\t')+
let newline       = '\r' | '\n' | "\r\n"
let three_d       = digit digit digit
let char_code     = '\\' three_d


(* ========== Lexical rules ========== *)

rule read =
  parse
  (* General symbols *)
  | white                        { read lexbuf }
  | newline                      { new_line lexbuf; read lexbuf }
  | ','                          { COMMA }
  | "..."                        { THREEDOTS }
  | '.'                          { DOT }
  | ';'                          { SEMICOLON }
  | '('                          { LPAREN }
  | ')'                          { RPAREN }
  | '{'                          { LBRACE }
  | '}'                          { RBRACE }
  (* Abstraction symbols *)
  | bool                         { BOOL (bool_of_string @@ Lexing.lexeme lexbuf) }
  | int                          { INT (int_of_string @@ Lexing.lexeme lexbuf) }
  | float                        { FLOAT (float_of_string @@ Lexing.lexeme lexbuf) }
  | '"'                          { create_string lexbuf @@ read_string (Buffer.create 16) }
  | '['                          { LBRACK }
  | ']'                          { RBRACK }
  | "=="                         { EQ }
  | "!="                         { NEQ }
  | "<"                          { LT }
  | "<="                         { LEQ }
  | ">"                          { GT }
  | ">="                         { GEQ }
  | ":="                         { DEFEQ }
  | id as x                      { try Hashtbl.find keywords x with Not_found -> ID x }
  | '_'                          { UNDERSCORE }
  | "//"                         { read_line_comment lexbuf }
  | "/*"                         { read_block_comment lexbuf }
  | _                            { raise (create_syntax_error "Unexpected char" lexbuf) }
  | eof                          { EOF }


(* ========== String reader ========== *)

and read_string buf =
  parse
  | '"'                   { STRING (Buffer.contents buf)                         }
  | '\\' '\\'             { Buffer.add_char buf '\\';    read_string buf lexbuf  }
  | '\\' 'b'              { Buffer.add_char buf '\b';    read_string buf lexbuf  }
  | '\\' 'n'              { Buffer.add_char buf '\n';    read_string buf lexbuf  }
  | '\\' 'r'              { Buffer.add_char buf '\r';    read_string buf lexbuf  }
  | '\\' 't'              { Buffer.add_char buf '\t';    read_string buf lexbuf  }
  | '\\' '0'              { Buffer.add_char buf '\000';  read_string buf lexbuf  }
  | '\\' 'v'              { Buffer.add_char buf '\011';  read_string buf lexbuf  }
  | '\\' 'f'              { Buffer.add_char buf '\012';  read_string buf lexbuf  }
  | '\\' '\"'             { Buffer.add_char buf '\"';    read_string buf lexbuf  }
  | '\\' (three_d as c)   {
                            Buffer.add_char buf (Char.chr (int_of_string c));
                            read_string buf lexbuf
                          }
  | char_code char_code   {
                            let s = Lexing.lexeme lexbuf in
                            let s' = "\"" ^ s ^ "\"" in
                            let s'' = Scanf.sscanf s' "%S" (fun s -> s) in
                            Buffer.add_string buf s'';
                            read_string buf lexbuf
                          }
  | [^ '"' '\\']+         {
                            Buffer.add_string buf (Lexing.lexeme lexbuf);
                            read_string buf lexbuf
                          }
  | _                     { raise (create_syntax_error "Illegal string character" lexbuf) }
  | eof                   { raise (create_syntax_error ~eof:true "String is not terminated" lexbuf) }

(* ========== Comment reader ========== *)

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





