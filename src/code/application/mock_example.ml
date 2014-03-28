(* Manual mock examlple in OCaml *)

(* Turtle example from Growing *)
module type TURTLE =
  sig
    type t
    type direction = Left | Right
    type position = int * int
    val make : unit -> t
    val get_position : t -> position
    val move_forward : t -> int -> unit
    val turn : t -> direction -> unit
    val pen_up : t -> unit
    val pen_down : t -> unit
  end

(* We might use these later in the generation phase so that we can
   just include them in generated mocks and save some code gen. *)
(* module MockHelpers = struct end *)
(* module ExpectationHelpers = struct end *)

(* We don't have run-time reflection and instrumentation, so we need
   to generate all of our expetation and mocking code statically. *)
module TurtleExpectations = struct end

module TurtleMock = struct

  (* The mock's "context" (actual type TBD).  *)
  type context = {
    expectations : unit list;
    invocations  : unit list;
  }

  let empty_context = { expectations = []; invocations = [] }

  (* Instead of having the context be mutable state that we hide
     in an enclosing module, we should make the M module be a functor
     that returns a TURTLE, and which takes a module of type CONTEXT,
     which will contain just the context. This is like what Xapi's log
     module did. *)
  module type CONTEXT = sig val ctx : context end

  (* We need to put the mock's types into an extra module so that they
    can be referred to by both the expectation and mock modules. Is
    there a better way to do this? *)
  module TurtleTypes = struct
    type t
    type direction = Left | Right
    type position = int * int
  end

  (* Expectations for this mock *)
  module E = struct
    (* Function names *)
    type funs = [ `get_position | `move_forward | `turn | `pen_up | `pen_down ]

    (* Shorthand for the function names *)
    let get_position = `get_position
    and move_forward = `move_forward
    and turn = `turn
    and pen_up = `pen_up
    and pen_down = `pen_down

    (* XXX can GADTs help us here to simplify these argument types? We
       could also move these to another module so we can reuse the
       orignal function names. The functions would have the same
       parameter types as the original, but we would return these
       variants instead. *)
    (* Function argument types *)
    type args = [ `get_position of TurtleTypes.t
                | `move_forward of TurtleTypes.t * int
                | `turn of TurtleTypes.t * TurtleTypes.direction
                | `pen_up of TurtleTypes.t
                | `pen_down of TurtleTypes.t ]

    (* let invoke_get_position t = `get_position t *)
    (* and invoke_move_forward t i = `move_forward (t, i) *)
    (* and invoke_turn t d = `turn (t, d) *)
    (* and invoke_pen_up t = `pen_up t *)
    (* and invoke_pen_down t = `pen_down t *)

    (* Lookup a function's expectations and record its invocations *)
    let invoke (ctx : context) (f : funs) (args : args) = Obj.magic f
    and lookup (ctx : context) (f : funs) (args : args) = Obj.magic f
  end

  (* Mock implementation of TURTLE, to be used as test double *)
  module M (C:CONTEXT) : TURTLE = struct

    (* types are simply duplicated *)
    (* type t *)
    (* type direction = Left | Right *)
    (* type position = int * int *)
    include TurtleTypes

    (* Implement each function *)
    let get_position t =
      (* Record invocation *)
      E.invoke C.ctx E.get_position (`get_position t) |> ignore;
      (* Lookup expectations *)
      E.lookup C.ctx E.get_position (`get_position t)

    (* TBD *)
    let make u = Obj.magic u    (* Fudged this one... *)
    let move_forward t i = ()
    let turn t d = ()
    let pen_up t = ()
    let pen_down t = ()

  end

end

(* Using TurtleMock *)
let example () =
  let module C = struct let ctx = TurtleMock.empty_context end in
  let module Turtle = TurtleMock.M(C) in
  let t = Turtle.make () in
  ignore (Turtle.get_position t)

let main =
  example ();
  print_endline "Hello Mocks"
