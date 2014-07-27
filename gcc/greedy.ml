let main world0 ghosts =

  let rec get_item list k =
    if (k = 0) then (car list) else (get_item (cdr list) (k - 1))
  in

  let rec get_cell map xy =
    let x = (car xy) and
        y = (cdr xy) in
    if (y = 0) then (get_item (car map) x) else (get_cell (cdr map) (x :: (y - 1)))
  in

  let eval_cell cell fruit_state =
    if (cell >= 5) then
      1
    else if (cell = 4) then
      if (fruit_state > 0) then 4 else 1
    else
      cell
  in

  (*
  let rec find_best_index values k best_k best_value =
    if (atom values) then
      best_k
    else if (best_value >= (car values)) then
      find_best_index (cdr values) (k + 1) best_k best_value
    else
      find_best_index (cdr values) (k + 1) k (car values)
  in
  *)

  let find_best_index values =
    let a = (car values) and
        b = (car (cdr values)) and
        c = (car (cdr (cdr values))) and
        d = (car (cdr (cdr (cdr values)))) in
    if ((a >= b) && (a >= c) && (a >= d)) then
      0
    else if ((b >= a) && (b >= c) && (b >= d)) then
      1
    else if ((c >= a) && (c >= b) && (c >= d)) then
      2
    else if ((d >= a) && (d >= b) && (d >= c)) then
      3
    else
      -1
  in

  let decide world =
    let x = (car (car (cdr (car (cdr world))))) and
        y = (cdr (car (cdr (car (cdr world))))) and
        map = (car world) in
    (* TODO(yuizumi): Get fruit_state. *)
    find_best_index [(eval_cell (get_cell map (x, y - 1)) 0);
                     (eval_cell (get_cell map (x + 1, y)) 0);
                     (eval_cell (get_cell map (x, y + 1)) 0);
                     (eval_cell (get_cell map (x - 1, y)) 0)]
  in

  let step state world =
    (0 :: (decide world))
  in

  (0 :: step)
in 0
