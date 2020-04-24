type figure =
    Point
  | Circle of int
  | Rectangle of int * int
  | Square of int

let similar x y =
  match (x, y) with
    (Point, Point) | (Circle _, Circle _) | (Square _, Square _) -> true
  | (Rectangle (l1, l2), Rectangle (l3, l4)) -> (l3 * l2 - l4 * l1) = 0
  | (Rectangle (a, b), Square _) | (Square _, Rectangle (a, b)) -> a = b
  | _ -> false

type 'a with_location = {loc_x: float; loc_y: float; body: 'a}

let distance p q =
  let dist_x = p.loc_x -. q.loc_x and dist_y = p.loc_y -. q.loc_y in
  sqrt (dist_x *. dist_x +. dist_y *. dist_y)

let overlap_rectangles p q =
  match (p, q) with
    ({body = Rectangle (px, py)}, {body = Rectangle (qx, qy)})
      -> abs_float (p.loc_x -. q.loc_x) < float_of_int (px + qx) /. 2.
        && abs_float (p.loc_x -. q.loc_x) < float_of_int (px + qx) /. 2.
  | (_, _) -> false

let overlap_point_rectangle point rect =
  let (rx, ry) = match rect with
      {body = Rectangle (x, y)} -> (x, y)
    | _ -> (0, 0)
  in
  let x_half = float_of_int rx /. 2. and y_half = float_of_int ry /. 2. in
  (rect.loc_x -. x_half < point.loc_x && point.loc_x < rect.loc_x +. x_half)
  && (rect.loc_y -. y_half < point.loc_y && point.loc_y < rect.loc_y +. y_half)

let overlap_circle_rectangle cir rect =
  let (cr, rx, ry) = match (cir, rect) with
      ({body = Circle r}, {body = Rectangle (x, y)}) -> (r, x, y)
    | (_, _) -> (0, 0, 0)
  in
  let x_half = float_of_int rx /. 2. and y_half = float_of_int ry /. 2. in
  let r_float = float_of_int cr in
  let left_top = (rect.loc_x -. x_half, rect.loc_y +. y_half) in
  let left_bot = (rect.loc_x -. x_half, rect.loc_y -. y_half) in
  let right_top = (rect.loc_x +. x_half, rect.loc_y +. y_half) in
  let right_bot = (rect.loc_x +. x_half, rect.loc_y -. y_half) in
  let points_overlap = List.exists (fun (x, y) ->
      distance cir {loc_x = x; loc_y = y; body = Point} < r_float)
      [left_top; left_bot; right_top; right_bot]
  in
  points_overlap
    || overlap_point_rectangle cir {rect with body = Rectangle (rx + cr, ry)}
    || overlap_point_rectangle cir {rect with body = Rectangle (rx, ry + cr)}


let overlap p q =
  match (p, q) with
    ({body = Point}, {body = Point}) -> p.loc_x = q.loc_x && p.loc_y = q.loc_y
  | ({body = Circle r}, {body = Point}) | ({body = Point}, {body = Circle r})
      -> let r_float = float_of_int r in
      distance p q -. r_float < 0.
  | (({body = Point} as point), ({body = Rectangle _} as rect))
    | (({body = Rectangle _} as rect), ({body = Point} as point))
      -> overlap_point_rectangle point rect
  | (({body = Point} as point), ({body = Square a} as sq))
    | (({body = Square a} as sq), ({body = Point} as point))
      -> overlap_point_rectangle point {sq with body = Rectangle (a, a)}
  | ({body = Circle r1}, {body = Circle r2})
      -> let r1_float = float_of_int r1 and r2_float = float_of_int r2 in
      distance p q -. r1_float -. r2_float < 0.
  | ({body = Rectangle _}, {body = Rectangle _}) -> overlap_rectangles p q
  | (({body = Rectangle _} as rect), ({body = Square a} as sq))
    | (({body = Square a} as sq), ({body = Rectangle _} as rect))
      -> overlap_rectangles rect {sq with body = Rectangle (a, a)}
  | ({body = Square a}, {body = Square b})
      -> overlap_rectangles
         {p with body = Rectangle (a, a)}
         {q with body = Rectangle (b, b)}
  | (({body = Circle _} as cir), ({body = Rectangle _} as rect))
    | (({body = Rectangle _} as rect), ({body = Circle _} as cir))
      -> overlap_circle_rectangle cir rect
  | (({body = Circle _} as cir), ({body = Square a} as sq))
    | (({body = Square a} as sq), ({body = Circle _} as cir))
      -> overlap_circle_rectangle cir {sq with body = Rectangle (a, a)}
