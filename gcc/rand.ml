fun init enemy ->
  let mod x y = x - ((x / y) * y) in
  let rand seed = (mod (seed * 229 + 3571) 7919) in
  let step seed world =
	((rand seed) :: (mod seed 4)) in
  (1 :: step)
