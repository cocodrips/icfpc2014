open Expr

let rec to_expr target (p, pattern) body next =
  match pattern with
	| PAny -> body
	| PVar name -> (p, ELetIn ([(name, target)], body))
	| PConst x -> (p, EIf ((p, EAtom target),
						   (p, EIf ((p, EEq ((p, EConst x), target)), body, next)), next))
	| PUnit -> (p, EIf ((p, EAtom target), (to_expr target (p, PConst 0) body next), next))
	| PCons (a, b) ->
	  (p, EIf ((p, EAtom target), next,
			   to_expr (p, ECar target) a (to_expr (p, ECdr target) b body next) next))

let rec match_to_expr (p, target) matches = match matches with
  | [] -> (p, EStop)
  | (pattern, body)::ms -> to_expr (p, target) pattern body (match_to_expr (p, target) ms)
