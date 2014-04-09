(* Sig to mock *)
module type TURTLE =
  sig
    type t
    type direction = Left | Right
    type position = int * int
    val make : unit -> t
    val get_position : t -> position
    val turn : direction -> t -> t
    val move_forward : int -> t -> t
    val toggle_pen : t -> t
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
  let get_position t = t.pos
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
  let move_forward i t =
    let x, y = t.pos in
    match t.dir with
    | North -> { t with pos = (x, y+1) }
    | South -> { t with pos = (x, y-1) }
    | East  -> { t with pos = (x+1, y) }
    | West  -> { t with pos = (x-1, y) }
  let toggle_pen = function
    | { pen = Up } as t -> { t with pen = Down }
    | { pen = Down } as t -> { t with pen = Up }

end

(* A partial mock of TURTLE (only mock's turn function). This works by
   including the to-be-mocked module in the mock itself, and
   reimplementing the functions we want to mock. This won't work if
   initialising the to-be-mocked module will have unwanted side
   effects. *)
module PartialTurtleMock = struct
  include Turtle

  (* We have to do this for every function we want to mock. *)
  let turn_args = ref []
  let turn_return : (direction -> t -> t) option ref = ref None
  let turn_count () = List.length !turn_args

  let turn t d =
    turn_args := (t,d) :: !turn_args;
    match !turn_return with
    | None -> turn t d
    | Some f -> f t d

end

(* Our system under test *)
module Draw(T : TURTLE) = struct

  (* Draw a simple shape *)
  let draw () =
    let open T in
    make ()
    >> turn Left
    >> toggle_pen
    >> move_forward 1
    >> turn Right
    >> move_forward 2
    >> toggle_pen
    |> ignore

end

let string_of_direction = function
  | Turtle.Left -> "Left"
  | Turtle.Right -> "Right"

(* A test case which uses the mock module *)
let test =
  (* Set up the test *)
  let module D = Draw(PartialTurtleMock) in
  let open PartialTurtleMock in

  (* Can provide indirect inputs to SUT *)
  turn_return := Some (fun d t ->
                       Printf.printf "Turning %s\n" (string_of_direction d);
                       Turtle.turn d t);

  (* Run the test *)
  print_endline "* Excercising SUT";
  D.draw ();

  (* Analyse the indirect outputs from the SUT *)
  print_endline "* Analysing outputs";
  Printf.printf "Turn was called %d times\n" (turn_count ());
  Printf.printf "Last direction was: %s\n"
                (!turn_args |> List.hd |> fst |> string_of_direction)

