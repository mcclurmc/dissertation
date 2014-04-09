(* Manual mock examlple in OCaml *)

(* XXX TODO: Really need to think hard about how we'll represent
   expectations before we do any more development. *)

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

  and exc = [ `Exception ]

  and fmatchers = [ `get_position of (TurtleTypes.t -> TurtleTypes.position)
                  | `turn of (TurtleTypes.t -> TurtleTypes.direction -> unit) ]

  module type EXPECTATIONS =
    sig
      type t
      type action
      type occurance
      type invocation
      type verify_result
      exception Unsatisfied_expectation
      val return : rets -> action
      val raises : exc -> action
      (* val will : action -> rets -> (funs * t list) -> (funs * t list) *)
      (* val will : action -> (funs * t list) -> (funs * t list) *)
      val will : action -> t -> t
      val once : occurance
      val never : occurance
      val allowing : occurance
      val exactly : int -> occurance
      val at_least : int -> occurance
      val at_most : int -> occurance
      val between : int -> int -> occurance
      (* val occurs : occurance -> (funs * t list) -> (funs * t list) *)
      val occurs : occurance -> t -> t
      val with_args : args -> t -> t
      val with_fun : fmatchers -> t -> t
      val (>>) : 'a -> ('a -> 'a) -> 'a
      val verify : t list -> t list option
      val verify_one : args -> t list -> bool
    end
  module Expectations (* : EXPECTATIONS *) =
    struct
      (* Here is probably a prime candidate for GADTs. We've basically
         got dependent types here, where what we return or throw
         depends on what function the action is being performed. *)
      (* type t = Action of action *)
      (*        | Occurance of occurance *)

      type t = {
        fname : funs;
        fargs : args option;
        fmatcher : fmatchers option;
        occurs : occurance;
        action : action option;
        seq_num : int;
        mutable count : int;
        (* Set this to true as soon as we satisfy, so that we can skip
           it if there is another potentially matching invocation
           later. *)
        mutable is_satisfied : bool;
      }

       and action = Return of rets | Raises of exc

       and occurance =
         | Allowing | Once | Never | AtMost of int | AtLeast of int
         | Exactly of int | Between of (int * int)

       and invocation = rets * occurance

       (* These need some work *)
       and verify_result =
         | Invalid_count of invocation * invocation
         | Out_of_order of invocation * invocation

      exception Unsatisfied_expectation

      let return v = Return v
      and raises e = Raises e

      (* XXX This won't work for throwing exceptions. Need to either
         have ret also contain exceptions, or have action (Return |
         Raises) carry it's value and remove ret. *)
      (* let will action (f,e) = f, e (\* C.ctx *\) *)
      let will action e = { e with action = Some action }

      let once = Once
      and never = Never
      and allowing  = Allowing
      and exactly i = Exactly i
      and at_most i = AtMost i
      and at_least i = AtLeast i
      and between i j = Between (i,j)

      (* let occurs o (f,e) = *)
      let occurs occurs e = { e with occurs = occurs }
        (* Retrieve f from hashtbl, prepend o to it, and store it *)
        (* XXX shit, do we not have access to the context now? of
           course not, because we need these definitions to define the
           context >:( Looks like the only solution is to pass a pair
           of f and its expectations along, and then store them at the
           end. *)

      let with_fun f e = { e with fmatcher = Some f }
      and with_args a e = { e with fargs = Some a }

      (* Combinator-based expectation language. We bind together
         expectations with (>>), which simply carries the function
         name over throughout the sequence of expectation creation
         functions. *)
      let (>>) e f = f e

      let is_satisfied e = match e.occurs with
        | Allowing -> true
        | Never -> false
        | Once -> e.count = 1
        | Exactly i -> e.count = i
        | AtLeast i -> e.count >= i
        | AtMost i -> e.count <= i
        | Between (i, j) -> (e.count >= i) && (e.count <= j)

      (* Verify a single invocation against the list of
         expectations. This is meant to be called from the mock
         implementation module. Return true if we match an expecation,
         not if we satisfy an expecation (that may happen later, don't
         care if it happens now). *)
      let rec verify_one invoke = function
        | [] -> false
        | e::es ->
           if e.is_satisfied || (Some invoke <> e.fargs)
           then verify_one invoke es (* if satisfied then skip *)
           else
             begin
               e.count <- e.count + 1;
               e.is_satisfied <- is_satisfied e;
               true
             end

      (* Verify that all of the expectations in the list have been satisfied. *)
      let verify exps = (* verify [] exps *)
        let exps = List.filter (fun e -> e.is_satisfied) exps in
        if exps = []
        then None
        else Some exps

    end

  (* The mock's "context" (actual type TBD). *)
  type context = {
    mutable expectations : Expectations.t list;
    mutable invocations  : args list;
  }

  (* let empty_context = { expectations = Hashtbl.create 10; invocations = [] } *)
  let empty_context = { expectations = []; invocations = [] }
  let make_context () = empty_context
  let clear_context ctx = ctx.expectations <- []; ctx.invocations <- []

  module type CONTEXT = sig val ctx : context end

  (* Generated content *)
  module type GENEXPS =
    sig
      include EXPECTATIONS
      (* val save : (funs * t list) -> unit *)
      val save : t -> unit
      (* Generated *)
      (* val get_position : funs * t list *)
      val get_position : t
      val move_forward : t
      val turn : t
      val pen_up : t
      val pen_down : t

      val get_position_ : TurtleTypes.t -> t
      val move_forward_ : TurtleTypes.t -> int -> t
      val turn_ : TurtleTypes.t -> TurtleTypes.direction -> t
      val pen_up_ : TurtleTypes.t -> t
      val pen_down_ : TurtleTypes.t -> t

      val get_position_f : (TurtleTypes.t -> TurtleTypes.position) -> t
      val turn_f : (TurtleTypes.t -> TurtleTypes.direction -> unit) -> t
    end
  module E (C : CONTEXT) : GENEXPS =
    struct
      include Expectations
      let save e =
        let e = { e with seq_num = List.length C.ctx.expectations } in
        C.ctx.expectations <- C.ctx.expectations @ [e] (* append should be okay... *)

      let make ?args ?fmatcher fn = {
        fname = fn;
        fargs = args;
        fmatcher = fmatcher;
        occurs = Allowing;
        action = None;
        seq_num = 0;
        count = 0;
        is_satisfied = false;
      }

      (* Generated: shorthand for the function names *)
      let get_position = make `get_position
      and move_forward = make `move_forward
      and turn = make `turn
      and pen_up = make `pen_up
      and pen_down = make `pen_down

      let get_position_ a1 = make ~args:(`get_position a1) `get_position
      and move_forward_ a1 a2 = make ~args:(`move_forward (a1,a2)) `move_forward
      and turn_ a1 a2 = make ~args:(`turn (a1,a2)) `turn
      and pen_up_ a1 = make ~args:(`pen_up a1) `pen_up
      and pen_down_ a1 = make ~args:(`pen_down a1) `pen_down

      (* Take functions as argument matchers. Maybe this should be the default... *)
      let get_position_f f = make ~fmatcher:(`get_position f) `get_position
      and turn_f f = make ~fmatcher:(`turn f) `turn
    end

  exception Unimplemented

  (* let find fname = function *)
  (*   | [] -> None *)
  (*   | e::es -> *)
  (*      if e.fname = fname *)
  (*      then Some e *)
  (*      else  *)

  (* XXX we really need to GADT this thing before we go further... *)
  (* Record function invocations *)
  let invoke (ctx : context) (f : funs) (args : args) =
    ctx.invocations <- args :: ctx.invocations

  (* XXX invoke should just do return as well... *)
  (* Lookup a function's expectations *)
  (* and lookup (ctx : context) (f : funs) (args : args) = *)
  (*   let is_return = function *)
  (*     | _ -> true in *)
  (*   let exps = List.filter (fun e -> e.fname = f) ctx.expectations in *)
  (*   if exps = [] then raise Unimplemented; *)
  (*   let ret = List.find is_return es in *)
  (*   Obj.magic ret *)
  and lookup ctx f args = Obj.magic f

  (* TODO *)
  (* let assert_verify ctx = *)
  (*   match Expectations.verify ctx.expectations ctx.invocations with *)
  (*   | None -> () *)
  (*   | Some _ -> raise Expectations.Unsatisfied_expectation *)

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

(* Create the mock's context, the expectation module, and the mock itself *)
module C = struct let ctx = TurtleMock.make_context () end
module E = TurtleMock.E(C)
module Turtle = TurtleMock.M(C)

(* Using TurtleMock *)
let example () =
  (* Create the mock's context, the expectation module, and the mock itself *)
  (* let module C = struct let ctx = TurtleMock.make_context () end in *)
  (* let module E = TurtleMock.E(C) in *)
  (* let module Turtle = TurtleMock.M(C) in *)

  (* let t = Turtle.make () in *)

  (let open E in
    (* XXX Couple problems here: 1) 'occurs never >> will return ...'
       should fail somehow, I'd think. Need to check how it works in
       JMock. 2) turn can only return unit, but here it returns
       position. THIS is what GADTs are for! *)

    turn
    >> occurs (exactly 2)
    >> with_fun (`turn (fun _ _ -> ()))
    >> will (return @@ `position (4,2))
    |> save;

    get_position
    >> occurs once
    >> will (return @@ `position (0,1))
    |> save;
  );

  (* Exercise the SUT *)
  let t = Turtle.make () in
  Turtle.get_position t |> ignore;

  (* Verify *)
  (* TurtleMock.assert_verify C.ctx; *)
  ()

let main =
  example ();
  print_endline "Hello Mocks"
