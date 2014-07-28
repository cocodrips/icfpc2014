open Expr
open Gcc
open List
open Pattern_match

exception Compile_error of string

let id_num = ref 0

let next_id () = (id_num := !id_num + 1; ("_" ^ (string_of_int !id_num)))

let closures = ref []

let rec make_ld env name p n i = match env with
  | e::es -> (match e with
	  | s::ss -> if s = name
		then GLd (n, i)
		else make_ld (ss::es) name p n (i + 1)
	  | [] -> make_ld es name p (n + 1) 0)
  | [] ->
	let mess =  (string_of_pos p) ^ "Variable not found: " ^ name ^ "." in
	raise (Compile_error mess)

let rec range start last =
  if start == last then []
  else if start < last then start::(range (start + 1) last)
  else start::(range (start - 1) last)

let rec compile2 (pos, exp) env tail =
  let cp exp = compile2 exp env false in
  match exp with
	| EConst value -> [GLdc value]
	| EAdd (a, b) -> (cp a) @ (cp b) @ [GAdd]
	| ESub (a, b) -> (cp a) @ (cp b) @ [GSub]
	| EMul (a, b) -> (cp a) @ (cp b) @ [GMul]
	| EDiv (a, b) -> (cp a) @ (cp b) @ [GDiv]
	| EEq (a, b) -> (cp a) @ (cp b) @ [GCeq]
	| EGt (a, b) -> (cp a) @ (cp b) @ [GCgt]
	| EGte (a, b) -> (cp a) @ (cp b) @ [GCgte]
	| ECons (a, b) -> (cp a) @ (cp b) @ [GCons]
	| ECar a -> (cp a) @ [GCar]
	| ECdr a -> (cp a) @ [GCdr]
	| EAtom a -> (cp a) @ [GAtom]
	| EDebug a -> (cp a) @ [GDbug; GLdc 0]
	| EFun (names, body) ->
	  let label = next_id () in
	  let body = (compile2 body (names::env) true) in
	  (closures := (!closures) @ [GLabel label] @ body @ [GRtn];
	   [GLdf label])
	| EVar name -> [make_ld env name pos 0 0]
	| EApp (a, b) -> (flatten (map cp b)) @ (cp a)
	  @ [if tail then GTap (length b) else GAp (length b)]
	| ELetIn (lets, result) ->
	  let gccs = map (fun x -> cp (snd x)) lets in
	  let label = next_id () in
	  let body = (compile2 result ((map fst lets)::env) true) in
	  (closures := (!closures) @ [GLabel label] @ body @ [GRtn];
	   (flatten gccs) @ [GLdf label; if tail then (GTap (length lets)) else (GAp (length lets))])
	| ERecIn (lets, result) ->
	  let let_count = length lets in
	  let new_env = (map fst lets)::env in
	  let label = next_id () in
	  let body = (compile2 result new_env true) in
	  let dummy_env = List.map (fun _ -> GLdc 0) lets in
	  let enter_body = [GLdf label; if tail then (GTap let_count) else (GAp let_count)] in
	  let rec_compile x = compile2 (snd x) new_env false in
	  let funs_gcc = List.flatten (List.map rec_compile lets) in
	  let rewrite_env = List.map (fun i -> GSt (0, i - 1)) (range let_count 0) in
	  (closures := (!closures) @ [GLabel label] @ funs_gcc @ rewrite_env @ body @ [GRtn];
	   dummy_env @ enter_body)
	| EIf (a, t, f) ->
	  let t_label = next_id () in
	  let f_label = next_id () in
	  let (sel, join) = if tail then (GTsel(t_label, f_label), GRtn)
		else (GSel(t_label, f_label), GJoin) in
	  (closures := (!closures)
	   @ [GLabel t_label] @ (compile2 t env tail) @ [join]
	   @ [GLabel f_label] @ (compile2 f env tail) @ [join];
	   (cp a) @ [sel])
	| EMatch (target, matches) ->
	  compile2 (match_to_expr target matches) env tail
	| EStop -> [GStop]

let compile exp toplevel =
  let _ = (closures := []) in
  let gcc = compile2 exp [toplevel] false in
  (gcc, !closures)
