open Expr
open Compile
open Gcc

let _ =
  let expr = (Parser.expr Lexer.token (Lexing.from_channel stdin)) in
  match expr with
	| ELetIn ([("main", body)], EConst 0) ->
	  let (args, body) =
		(match body with
		  | EFun (args, funbody) -> (args, funbody)
		  | _ -> ([], body)) in
	  let (gcc_body, gcc_funs) = compile body args in
	  print_gccs (gcc_body @ [GRtn] @ gcc_funs)
	|_ -> raise (Compile_error "Program must be of the form: let main ... = ... in 0.")
