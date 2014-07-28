let main world_0 ghost_roms =

  (*----------------------------------------------------------------------
    Constants
  ----------------------------------------------------------------------*)

  let kWall       = 0 and
      kEmpty      = 1 and
      kPill       = 2 and
      kPowerPill  = 3 and
      kFruitPos   = 4 and
      kLambdaHome = 5 and
      kGhostHome  = 6 and

      kU = 0 and
      kR = 1 and
      kD = 2 and
      kL = 3 and

      kMaxInt = 2147483647
  in

  (*----------------------------------------------------------------------
    Basic Functions
  ----------------------------------------------------------------------*)

  let abs x = if x >= 0 then x else (-x) in

  let rec mapfun func xs =
    if (atom xs) then [] else ((func (car xs)) :: (mapfun func (cdr xs)))
  in

  let rec nth xs n =
    if (n = 0) then (car xs) else (nth (cdr xs) (n - 1))
  in

  let get map x y = (nth (nth map y) x) in

  (*----------------------------------------------------------------------
    Internal States
  ----------------------------------------------------------------------*)

  let init_state map =
    let inner row = mapfun (fun cell -> cell = kWall) row in
    (mapfun inner map)
  in

  let next_state state lx ly =
    let rec inner row x y =
      if (atom row) then
        []
      else
        let new_value =
          if (x = lx) && (y = ly) then
            let n = (get state x (y - 1)) and e = (get state (x + 1) y) and
                s = (get state x (y + 1)) and w = (get state (x - 1) y) in
            if ((n + e + s + w) >= 3) then 1 else -1
          else
            if (car row) > 0 then (car row) else 0
        in
        (new_value :: (inner (cdr row) (x + 1) y))
    in
    let rec outer rows y =
      if (atom rows) then
        []
      else
        ((inner (car rows) 0 y) :: (outer (cdr rows) (y + 1)))
    in
    (outer state 0)
  in

  (*----------------------------------------------------------------------
    Evaluations
  ----------------------------------------------------------------------*)

  let find_dest_by_cell_type map lx ly target =
    let dist x y = (abs (x - lx)) + (abs (y - ly)) in

    let rec inner row x y best_xy =
      if (atom row) then
        best_xy
      else
        let update = ( ((car row) = target) &&
                       ((dist x y) < (dist (car best_xy) (cdr best_xy))) ) in
        inner (cdr row) (x + 1) y (if update then (x, y) else best_xy)
    in

    let rec outer map y best_xy =
      if (atom map) then
        best_xy
      else
        outer (cdr map) (y + 1) (inner (car map) 0 y best_xy)
    in

    (outer map 0 (65535, 65535))
  in

  (* TODO(yuizumi): Ghosts. *)
  let decide_dest world =
    let lx = (car (car (cdr (car (cdr world))))) and
        ly = (cdr (car (cdr (car (cdr world))))) and
        map = (car world) and
        fruit = (cdr (cdr (cdr world))) in
    let target = (if fruit then kFruitPos else kPill) in
    (find_dest_by_cell_type map lx ly target)
  in

  let choose_action state world actions =
    let lx = (car (car (cdr (car (cdr world))))) and
        ly = (cdr (car (cdr (car (cdr world))))) in
    let next_x action =
      if (action = kL) then (lx - 1) else if (action = kR) then (lx + 1) else lx
    in
    let next_y action =
      if (action = kU) then (ly - 1) else if (action = kD) then (ly + 1) else ly
    in
    let rec choose actions =
      if (atom actions) then
        99
      else
        let action = (car actions) in
        if (get state (next_x action) (next_y action)) then
          choose (cdr actions)
        else
          action
    in
    (choose actions)
  in

  let decide_action state world =
    let dest = (decide_dest world) in
    let dx = (car dest) - (car (car (cdr (car (cdr world))))) and
        dy = (cdr dest) - (cdr (car (cdr (car (cdr world))))) in
    if ((abs dx) >= (abs dy)) then
      choose_action state world [(if (dx > 0) then kR else kL);
                                 (if (dy > 0) then kD else kU);
                                 (if (dy > 0) then kU else kD);
                                 (if (dx > 0) then kL else kR)]
    else
      choose_action state world [(if (dy > 0) then kD else kU);
                                 (if (dx > 0) then kR else kL);
                                 (if (dx > 0) then kL else kR);
                                 (if (dy > 0) then kU else kD)]
  in

  (*----------------------------------------------------------------------
    Entry Point
  ----------------------------------------------------------------------*)

  let step state world =
    let lx = (car (car (cdr (car (cdr world))))) and
        ly = (cdr (car (cdr (car (cdr world))))) in
    ((next_state state lx ly), (decide_action state world))
  in

  ((init_state (car world_0)), step)

in 0
