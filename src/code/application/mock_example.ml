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

(* Because I might want to monadify this... It's looking an awefully
   lot like the state monad. *)
module type MONAD =
  sig
    type 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
end;;

(* We might use these later in the generation phase so that we can
   just include them in generated mocks and save some code gen. *)
(* module MockHelpers = struct end *)
(* module ExpectationHelpers = struct end *)

(* We don't have run-time reflection and instrumentation, so we need
   to generate all of our expetation and mocking code statically. *)
module TurtleExpectations = struct end

module TurtleMock = struct

  (* We need to put the mock's types into an extra module so that they
     can be referred to by both the expectation and mock modules. Is
     there a better way to do this? *)
  module TurtleTypes = struct
    type t
    type direction = Left | Right
    type position = int * int
  end

  (* Mocked function names *)
  type funs = [ `get_position | `move_forward | `turn | `pen_up | `pen_down ]

  (* XXX can GADTs help us here to simplify these argument types? We
     could also move these to another module so we can reuse the
     orignal function names. The functions would have the same
     parameter types as the original, but we would return these
     variants instead. *)
  (* Function argument types *)
  and args = [ `get_position of TurtleTypes.t
             | `move_forward of TurtleTypes.t * int
             | `turn of TurtleTypes.t * TurtleTypes.direction
             | `pen_up of TurtleTypes.t
             | `pen_down of TurtleTypes.t ]

  and rets = [ `get_position of TurtleTypes.position
             | `move_forward of unit
             | `turn of unit
             | `pen_up of unit
             | `pen_down of unit
             (* XXX we *definitely* need GADTs! *)
             | `unit | `position of TurtleTypes.position ]

  module type EXPECTATIONS =
    sig
      type t
      type action
      type occurance
      val return : action
      val raises : action
      val will : action -> rets -> (funs * t list) -> (funs * t list)
      val once : occurance
      val never : occurance
      val at_least : int -> occurance
      val at_most : int -> occurance
      val between : int -> int -> occurance
      val occurs : occurance -> (funs * t list) -> (funs * t list)
      val (>>) : 'a -> ('a -> 'a) -> 'a
    end
  module Expectations : EXPECTATIONS =
    struct
      (* Here is probably a prime candidate for GADTs. We've basically
         got dependent types here, where what we return or throw
         depends on what function the action is being performed. *)
      type t = Action of action
             | Occurance of occurance

      and action = Return | Raises

      and occurance =
        Once | Never | AtMost of int | AtLeast of int | Between of (int * int)

      let return = Return
      and raises = Raises

      (* XXX This won't work for throwing exceptions. Need to either
         have ret also contain exceptions, or have action (Return |
         Raises) carry it's value and remove ret. *)
      let will action ret (f,e) = f, e (* C.ctx *)

      let once = Once
      and never = Never
      and at_most i = AtMost i
      and at_least i = AtLeast i
      and between i j = Between (i,j)

      let occurs o (f,e) =
        (* Retrieve f from hashtbl, prepend o to it, and store it *)
        (* XXX shit, do we not have access to the context now? of
           course not, because we need these definitions to define the
           context >:( Looks like the only solution is to pass a pair
           of f and its expectations along, and then store them at the
           end. *)
        f,e

      (* Combinator-based expectation language. We bind together
         expectations with (>>), which simply carries the function
         name over throughout the sequence of expectation creation
         functions. ($) is used to string together expectations. I
         can't think of a nicer-looking way to do this... *)
      let (>>) e f = f e
    end

  (* The mock's "context" (actual type TBD). *)
  type context = {
    expectations : (funs, Expectations.t list) Hashtbl.t;   (* TODO: figure out type *)
    mutable invocations  : args list;
  }

  let empty_context = { expectations = Hashtbl.create 10; invocations = [] }
  let make_context () = empty_context

  module type CONTEXT = sig val ctx : context end

  (* Generated content *)
  module type GENEXPS =
    sig
      include EXPECTATIONS
      val save : (funs * t list) -> unit
      (* Generated *)
      val get_position : funs * t list
      val move_forward : funs * t list
      val turn : funs * t list
      val pen_up : funs * t list
      val pen_down :funs * t list
    end
  module E (C : CONTEXT) : GENEXPS =
    struct
      include Expectations
      let save (f, ctx) = ()    (* XXX *)
      (* Generated: shorthand for the function names *)
      (* XXX if we return [], then if we "redo" a functions
         expectation, it will delete older expectations. If we first
         look up the current expectation in the context, we will add
         on to the current expectations. *)
      let get_position = `get_position, []
      and move_forward = `move_forward, []
      and turn = `turn, []
      and pen_up = `pen_up, []
      and pen_down = `pen_down, []
    end

  (* Record function invocations *)
  let invoke (ctx : context) (f : funs) (args : args) =
    ctx.invocations <- args :: ctx.invocations

  (* Lookup a function's expectations *)
  and lookup (ctx : context) (f : funs) (args : args) = Obj.magic f

  (* Mock implementation of TURTLE, to be used as test double *)
  module M (C:CONTEXT) : TURTLE = struct
    (* Include the types from the mock module. We do this so that we
       can access these types from the enclosing module. I'm not sure
       if the types will ultimately equate; there may be a better way
       to do this. *)
    include TurtleTypes

    let get_position t =
      (* Record invocation *)
      invoke C.ctx `get_position (`get_position t);
      (* Lookup expectations *)
      lookup C.ctx `get_position (`get_position t)

    (* TBD *)
    let make u = Obj.magic u    (* Fudged this one... *)
    let move_forward t i = ()
    let turn t d = ()
    let pen_up t = ()
    let pen_down t = ()

  end

end

(* module C = struct let ctx = TurtleMock.make_context () end *)
(* module E = TurtleMock.E(C) *)
(* module Turtle = TurtleMock.M(C) *)

(* Using TurtleMock *)
let example () =
  (* Create the mock's context, the expectation module, and the mock itself *)
  let module C = struct let ctx = TurtleMock.make_context () end in
  let module E = TurtleMock.E(C) in
  let module Turtle = TurtleMock.M(C) in

  (let open E in
    (* XXX Couple problems here: 1) 'occurs never >> will return ...'
       should fail somehow, I'd think. Need to check how it works in
       JMock. 2) turn can only return unit, but here it returns
       position. THIS is what GADTs are for! *)
    turn >> occurs never >> will return (`position (4,2)) |> save;
    get_position >> occurs once >> will return (`position (0,1)) |> save;
  );

  (* Exercise the SUT *)
  let t = Turtle.make () in
  Turtle.get_position t |> ignore

let main =
  example ();
  print_endline "Hello Mocks"
