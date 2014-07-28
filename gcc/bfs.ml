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

      kMaxDistance = 16 and

      kMaxInt = 2147483647
  in

  (*----------------------------------------------------------------------
    Basic Functions
  ----------------------------------------------------------------------*)

  let rec mapfun func xs =
    if (atom xs) then [] else ((func (car xs)) :: (mapfun func (cdr xs)))
  in

  let rec nth xs n =
    if (n = 0) then (car xs) else (nth (cdr xs) (n - 1))
  in

  let get map x y = (nth (nth map y) x) in

  (*----------------------------------------------------------------------
    Evaluations
  ----------------------------------------------------------------------*)

  let build_basemap map lx ly pred_cell =
    let eval_cell cell x y =
      if ((x = lx) && (y = ly)) then
        (-2)
      else if (cell = kWall) then
        (-2)
      else
        if (pred_cell cell) then 0 else (-1)
    in
    let rec inner row x y =
      if (atom row) then
        []
      else
        ((eval_cell (car row) x y) :: (inner (cdr row) (x + 1) y))
    in
    let rec outer map y =
      if (atom map) then
        []
      else
        ((inner (car map) 0 y) :: (outer (cdr map) (y + 1)))
    in
    (outer map 0)
  in

  (* basemap --> 0: Target, -1: Empty, else: Wall *)
  let build_distmap basemap =
    let rec for_x o u r d l counter =
      let new_value =
        if (car o) != -1 then
          (car o)
        else
          let has_adj = ((car u) >= 0) || ((car r) >= 0) || ((car d) >= 0) || ((car l) >= 0) in
          if has_adj then counter else (-1)
      in
      if (atom r) then
        [new_value]
      else
        (new_value :: (for_x (cdr o) (cdr u) (cdr r) (cdr d) (cdr l) counter))
    in

    let rec for_y curr prev next counter =
      let new_values = (for_x (car curr)
                              (car prev)
                              (cdr (car curr))
                              (car (if (atom next) then curr else next))
                              (-3 :: (car curr))
                              counter) in
      if (atom next) then
        [new_values]
      else
        (new_values :: (for_y next curr (cdr next) counter))
    in

    let rec for_n map n =
      if n >= kMaxDistance then
        map
      else
        (for_n (for_y map ((car map) :: map) (cdr map) n) (n + 1))
    in

    (for_n basemap 1)
  in

  let find_best_index values =
    let rec find values k best_k min_value =
      if (atom values) then
        best_k
      else if (((car values) >= min_value) || ((car values) < 0)) then
        find (cdr values) (k + 1) best_k min_value
      else
        find (cdr values) (k + 1) k (car values)
    in
    (find values 0 99 kMaxInt)
  in

  let shrink_map map lx ly =
    let sx = if (lx < kMaxDistance) then 0 else (lx - kMaxDistance) and
        sy = if (ly < kMaxDistance) then 0 else (ly - kMaxDistance) in
    let rec inner row x =
      if (atom row) then
        []
      else if x > (sx + 2 * kMaxDistance) then
        []
      else if x >= sx then
        ((car row) :: (inner (cdr row) (x + 1)))
      else
        (inner (cdr row) (x + 1))
    in
    let rec outer map y =
      if (atom map) then
        []
      else if y > (sy + 2 * kMaxDistance) then
        []
      else if y >= sy then
        ((inner (car map) 0) :: (outer (cdr map) (y + 1)))
      else
        (outer (cdr map) (y + 1))
    in
    ((outer map 0), (sx, sy))
  in

  let decide_action world =
    let lx = (car (car (cdr (car (cdr world))))) and
        ly = (cdr (car (cdr (car (cdr world))))) and
        map = (car world) in
    let shrinked = (shrink_map map lx ly) in
    let sx = (car (cdr shrinked)) and
        sy = (cdr (cdr shrinked)) and
        smap = (car shrinked) in
    let basemap_pills =
      (build_basemap smap (lx - sx) (ly - sy) (fun c -> (c = kPill || c = kPowerPill))) in
    let distmap_pills = (build_distmap basemap_pills) in
    find_best_index [(get distmap_pills (lx - sx) (ly - sy - 1));     (* 0: ↑ *)
                     (get distmap_pills (lx - sx + 1) (ly - sy));     (* 1: → *)
                     (get distmap_pills (lx - sx) (ly - sy + 1));     (* 2: ↓ *)
                     (get distmap_pills (lx - sx - 1) (ly - sy))]     (* 3: ← *)
  in

  (*----------------------------------------------------------------------
    Entry Point
  ----------------------------------------------------------------------*)

  (0, (fun state world -> (0, (decide_action world))))

in 0
