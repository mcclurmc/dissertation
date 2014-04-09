(* XXX it may be simpler at first to keep all definitions in one
module, but in the long run it will be more difficult to keep track of
all the fresh names we have to generate. Although we will still have
to keep track of fresh name collitions in our expectation language
module. Perhaps we should place the names collected from the
to-be-mocked interface in a sub-module of the expectation language
module (so we'd have E.(M.mock_function arg1 arg2 >> occurs once >>
...) ) *)

(* Sig to mock *)
module type TURTLE =
  sig
    type t
    type direction = Left | Right
    type position = int * int
    val make : unit -> t
    val get_position : t -> position
    val turn : t -> direction -> unit
  end

(* Do we have enough info here to define this sig? *)
module type EXPECTATIONS =
  sig
    type e
    type occurance
    type args
    type funs
    val once : occurance
    val with_args : args -> e -> e
    val with_fun : funs -> e -> e
    val (>>) : 'a -> ('a -> 'a) -> 'a
  end

module type MOCK =
  sig
    module M : TURTLE
    module E : EXPECTATIONS
  end

module Mock (* : MOCK *) = struct

  module Impl = struct
    (* Implement the mock and the expectation language *)

    (* Carry over type definitions *)
    (* type t *)
    type t
    type direction = Left | Right
    type position = int * int

    (* Preliminary definitions *)
    exception Unimplemented

    (* Generated definitions *)
    type name = [ `make | `turn | `get_position ]
    type args = [ `make of unit | `turn of (t * direction) | `get_position of t ]
    type funs = [ `make of (unit -> t)
                | `turn of (t -> direction -> unit)
                | `get_position of (t -> position) ]

    (* Implement mock functions *)
    let make () = raise Unimplemented
    let get_position a1 = raise Unimplemented
    let turn a1 a2 = raise Unimplemented

    (* Predefinied expectation language *)
    type action = Return | Raise
    type occurance = Allowing | Once | Never
    let once = Once
    let (>>) e f = f e

    type _ mock_gadt =
      | F_get_position : unit -> unit mock_gadt
      | F_make : unit -> unit mock_gadt
      | F_turn : unit -> unit mock_gadt

    type _ return_gadt =
      (* | R_get_position : position -> position return_gadt *)
      (* | R_make : t -> t return_gadt *)
      (* | R_turn : unit -> unit return_gadt *)
      | R_get_position :  position return_gadt
      | R_make : t return_gadt
      | R_turn : unit return_gadt

    (* Type of expectations *)
    type e = {
      name                 : name;
      args                 : args option;
      ffun                 : funs option;
      occurs               : occurance;
      action               : action option;
      return               : 'a. 'a return_gadt option;
      seq_num              : int;
      mutable count        : int;
      mutable is_satisfied : bool;
    }

    type 'a e' = {
      name                 : name;
      args                 : args option;
      ffun                 : funs option;
      occurs               : occurance;
      action               : action option;
      (* return               : 'a return_gadt option; *)
      return               : 'a option;
      seq_num              : int;
      mutable count        : int;
      mutable is_satisfied : bool;
    }

    (* let lookup : type a. a return_gadt -> a = function *)
    (*   | R_get_position a -> a *)
    (*   | R_make a -> a *)
    (*   | R_turn a -> a *)

    (* let will_return : type r. r -> e -> e = fun r e -> *)
    (* let will_return : type r. r return_gadt -> r -> r e' -> r e' = fun t r e -> *)
    let will_return : 'r return_gadt -> 'r -> 'r e' -> 'r e' = fun t r e ->
      match t with
      | R_get_position -> { e with return = Some r }
      (* | R_make -> { e with return = Some r } *)
      | _ -> e
      (* match e.name with *)
      (* | `get_position -> { e with return = (Some (R_get_position r)) } *)
      (* | `make -> { e with return = (Some (R_make r)) } *)
      (* | `turn -> e *)

    (* Generated expecation language *)
    let with_args f t = t
    let with_fun f t = t
  end

  (* When we generate names, we'll have to do this for all names. We should default  *)
  module M : TURTLE = Impl
  module E : EXPECTATIONS = Impl
end
