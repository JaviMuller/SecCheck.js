(* Copyright (C) 2022-2025 formalsec programmers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *)

open Prelude
open SecCheckJSBase
open SecCheckJSSyntax
include Parsing_utils

type 'a start = Lexing.position -> 'a Parser.MenhirInterpreter.checkpoint

type token = [%import: Parser.token] [@@deriving show]

let lexer (last_token : token ref) (lexbuf : Lexing.lexbuf) =
  let token = Lexer.read lexbuf in
  last_token := token;
  token

let parser (start : 'a start) (lexbuf : Lexing.lexbuf) : Query.t =
  let module Core_TCPGMI = Parser.MenhirInterpreter in
  let last_token = ref Parser.EOF in
  Core_TCPGMI.loop_handle
    (fun result -> result)
    (function
      | Core_TCPGMI.Rejected -> Log.fail "Parser rejected input"
      | Core_TCPGMI.HandlingError _e ->
        Log.stderr "%a, last token: %s: %s.@." print_position lexbuf
          (show_token !last_token) "Error message found";
        raise Parser.Error
      | _ -> Log.fail "Unexpected state in failure handler!" )
    (Core_TCPGMI.lexer_lexbuf_to_supplier (lexer last_token) lexbuf)
    (start lexbuf.Lexing.lex_curr_p)

let parse_query (str : string) : Query.t =
  let lexbuf = init_lexbuf "" str in
  Parser.entry_sec_prop_target Lexer.read lexbuf
  
let parse_query_from_file (fname : string) : Query.t =
  let str = Code_utils.load_file_contents fname in
  parse_query str
