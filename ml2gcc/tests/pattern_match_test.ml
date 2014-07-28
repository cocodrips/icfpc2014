let main =
  let rec sum ls = match ls with
	| [] -> 0
	| x::xs -> x + sum xs in
  let ls = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
  sum ls
in 0
