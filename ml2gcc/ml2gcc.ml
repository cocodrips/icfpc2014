open Expr
open Compile
open Gcc

let _ =
  let expr = (Parser.expr Lexer.token (Lexing.from_channel stdin)) in
  let (main, other) = compile expr [] in
  print_gccs (main @ [GRtn] @ other)
