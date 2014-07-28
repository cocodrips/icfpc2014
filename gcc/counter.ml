let main world_0 ghosts =

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

      (* Counted in the init function. *)
      kPenaltyWall = 65536 and

      (* Counted in the step function. *)
      kPenaltyFore  = -1      and
      kPenaltyBack  = 4       and
      kPenaltyPill  = -16     and
      kPenaltyGhost = 1048576 and
      kPenaltyRisky = 131072  and

      kMaxInt = 2147483647
  in


  (*----------------------------------------------------------------------
    Basic Functions
  ----------------------------------------------------------------------*)

  let rec mapfun func list =
    if (atom list) then [] else ((func (car list)) :: (mapfun func (cdr list)))
  in

  let rec nth list n =
    if (n = 0) then (car list) else (nth (cdr list) (n - 1))
  in

  let lambda_pos world = car (cdr (car (cdr world))) in

  (*----------------------------------------------------------------------
    Internal States
  ----------------------------------------------------------------------*)

  (* TODO(yuizumi): Detect dead ends. *)
  let init_state map =
    let inner row =
      mapfun (fun cell -> if cell = kWall then kPenaltyWall else 0) row
    in
    (mapfun inner map)
  in

  let next_state state lx ly =
    let rec inner row x y =
      if (atom row) then
        []
      else
        (((car row) + (if (x = lx) && (y = ly) then 1 else 0)) :: (inner (cdr row) (x + 1) y))
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

  let find_best_index values =
    let rec find values k best_k min_value =
      if (atom values) then
        best_k
      else if ((car values) >= min_value) then
        find (cdr values) (k + 1) best_k min_value
      else
        find (cdr values) (k + 1) k (car values)
    in
    (find values 0 99 kMaxInt)
  in

  let rec ghost_penalty ghosts x y =
    let calc_penalty ghost =
      let gx = (car (car (cdr ghost))) and
          gy = (cdr (car (cdr ghost))) and
          vitality = (car ghost) in
      if (vitality != 0) then
        0
      else if (x = gx) && (y = gy) then
        kPenaltyGhost
      else if (x = gx) && ((y = gy - 1) || (y = gy + 1)) then
        kPenaltyRisky
      else if (y = gy) && ((x = gx - 1) || (x = gx + 1)) then
        kPenaltyRisky
      else
        0
    in
    if (atom ghosts) then
      0
    else
      (calc_penalty (car ghosts)) + (ghost_penalty (cdr ghosts) x y)
  in

  (* TODO(yuizumi): Consider fruits and ghosts. *)
  (* NOTE(yuizumi): Smaller is better. *)
  let evaluate state world dx dy fore_dir back_dir =
    let x = (car (lambda_pos world)) + dx and
        y = (cdr (lambda_pos world)) + dy and
        lamb_dir = (car (cdr (cdr (car (cdr world))))) and
        map = (car world) and
        ghosts = (car (cdr (cdr world))) in
    let cell = (nth (nth map y) x) in
    (nth (nth state y) x)  (* base penalty *)
    + (ghost_penalty ghosts x y)
    + (if (cell = kPill || cell = kPowerPill) then kPenaltyPill else 0)
    + (if (lamb_dir = fore_dir) then kPenaltyFore else 0)
    + (if (lamb_dir = back_dir) then kPenaltyBack else 0)
  in

  let decide_action state world =
    find_best_index [(evaluate state world 0 (-1) 0 2);   (* 0: ↑ *)
                     (evaluate state world 1 0 1 3);      (* 1: → *)
                     (evaluate state world 0 1 2 0);      (* 2: ↓ *)
                     (evaluate state world (-1) 0 3 1)]   (* 3: ← *)
  in

  (*----------------------------------------------------------------------
    Entry Point
  ----------------------------------------------------------------------*)

  let step state world =
    let lx = (car (lambda_pos world)) and
        ly = (cdr (lambda_pos world)) in
    ((next_state state lx ly), (decide_action state world))
  in

  ((init_state (car world_0)), step)

in 0  (* End of main. *)
