open Simplest_mock_real

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
