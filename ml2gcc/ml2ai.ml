open Expr
open Compile
open Gcc

let _ =
  let expr = (Parser.expr Lexer.token (Lexing.from_channel stdin)) in
  match expr with
	| EFun (init, main) -> print_gccs (
	  let (main, other) = compile main init in
	  main @ [GRtn] @ other)
