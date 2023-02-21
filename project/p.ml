open Ast

let () =
  let lex_buf = Lexing.from_channel stdin in
  let expr = Parser.program Scanner.token lex_buf in
  let res = Ast.string_of_program expr in
  print_endline res;;

