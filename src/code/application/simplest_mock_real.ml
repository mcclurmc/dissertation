(* file: simplest_mock_real.ml  *)
module type TURTLE =
  sig
    type t
    type direction = Left | Right
    val make : unit -> t
    val turn : direction -> t -> t
    val move : int -> t -> t
    val pen_up : t -> t
    val pen_down : t -> t
    val (>>) : t -> (t -> t) -> t
  end

(* The *real* TURTLE implementation *)
module Turtle : TURTLE = struct
  type direction = Left | Right
  type position = int * int
  type orientation = North | South | East | West
  type pen = Up | Down
  type t = {
    pos : position;
    pen : pen;
    dir : orientation;
  }
  let (>>) t f = f t
  let make () = { pos = (0,0); pen = Up; dir = North }
  let turn d t = match d with
    | Left ->
       let d = match t.dir with
               | North -> West | West -> South
               | South -> East | East -> North
       in { t with dir = d }
    | Right ->
       let d = match t.dir with
                | North -> East | West -> North
                | South -> West | East -> South
       in { t with dir = d }
  (* draw_line uses some graphics library to draw a line
   * between points a and b. *)
  let draw_line a b = ()
  let move i t =
    let orig = (t.pos) in
    let x, y = t.pos in
    let t =
      match t.dir with
      | North -> { t with pos = (x, y+i) }
      | South -> { t with pos = (x, y-i) }
      | East  -> { t with pos = (x+i, y) }
      | West  -> { t with pos = (x-i, y) }
    in
    if t.pen = Down
    then ( draw_line orig t.pos; t )
    else t
  let pen_up t = { t with pen = Up }
  let pen_down t = { t with pen = Down }
end
