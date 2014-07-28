open Expr
open Compile
open Gcc

let _ =
  let (_, expr) = (Parser.expr Lexer.token (Lexing.from_channel stdin)) in
  match expr with
	| ELetIn ([("main", (p, body))], (_, EConst 0)) ->
	  let (args, body) =
		(match body with
		  | EFun (args, funbody) -> (args, funbody)
		  | _ -> ([], (p, body))) in
	  let (gcc_body, gcc_funs) = compile body args in
	  print_gccs (gcc_body @ [GRtn] @ gcc_funs)
	|_ -> raise (Compile_error "Program must be of the form: let main ... = ... in 0.")
