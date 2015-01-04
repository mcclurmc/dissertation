(* Example expectations *)

(* module Simple = struct *)
(*   let add a b = a + b ;; *)
(*   let incr a = a + 1;; *)
(* end *)

module Simple_ORIGINAL = Simple (* <generated/> *)

module type SIMPLE = module type of Simple

module type SIMPLEMOCK =
  sig
    include SIMPLE
    module Expect : sig
      type fname
      type ftype
      type fret
      type farg
      type exp
      type t
      (* val one_of : fname -> 'a -> exp *)
      (* val never : fname -> 'a -> exp *)
      (* val times : fname -> int -> 'a -> exp *)
      (* val before : fname -> fname -> 'a -> exp *)
      (* val will : fname -> ftype -> 'a -> exp *)
      (* val returns : fname -> fret -> 'a -> exp *)
      (* val one_of : fname -> 'a t *)
      (* val never : fname -> 'a t *)
      (* val times : fname -> int -> 'a t *)
      (* val before : fname -> fname -> 'a t *)
      (* val will : fname -> ftype -> 'a t *)
      (* val returns : fname -> fret -> 'a t *)
      (* <generated> *)
      val add : fname
      val incr : fname
      (* </generated> *)
      type 'a e
      val pure : 'a -> 'a e
    end
    (* val return : exp -> t *)
    (* val bind : t -> exp -> unit *)
    (* val ( >>= ) : t -> exp -> unit *)
    (* val return : exp -> t *)
    (* val bind : t -> exp -> unit *)
    (* val ( >>= ) : t -> exp -> unit *)
    val init : unit -> unit
    val verify : unit -> [ `Success | `Failure ]
  end

module Simple (* : SIMPLEMOCK *) = struct
  module Expect = struct

    (** Generated **)

    (* Mocked function names *)
    type fname = [ `add | `incr ]
    (* Function types *)
    type ftype =
      [ `add of (int -> int -> int)
      | `incr of (int -> int) ]
    (* Function arg types *)
    type farg =
      [ `add of (int * int)
      | `incr of int ]
    (* Function return types *)
    type fret =
      [ `add of int
      | `incr of int ]
    type fact =
      [ `f_do of ftype
      | `f_ret of fret ]

    (** Static **)

    (* Expectation langauge type *)
    type exp = ..

    type exp +=

      (* Counting *)
      | One_of of fname
      | Never of fname
      | Times of (int * fname)

      (* Order *)
      | Before of (fname * fname)

      (* Action *)
      | Will of (fname * ftype)
      | Returns of (fname * fret)


    (* Mock type *)
    type t = {
      fcalls  : farg list ref;
      actions : (fname, fact) Hashtbl.t;
    }

    exception Expect_violation of exp * farg

    (* XXX let's try applicative functors *)
    (* type 'a e = 'a list *)
    (* let pure a = [a] *)
    (* let app f a = List.append a b *)
    (* let ( $ ) = app *)

    (** Generated **)

    let add_proxy m a b =
      m.fcalls := !(m.fcalls) @ [`add (a, b)];
      if Hashtbl.mem m.actions `add
      then match Hashtbl.find m.actions `add with
           | `f_do f  -> (match f with
                          | `add f' -> f' a b
                          | _ -> failwith "Impossible")
           | `f_ret x -> (match x with
                          | `add x' -> x'
                          | _ -> failwith "Impossible")
      else Simple.add a b

    let incr_proxy m a =
      m.fcalls := !(m.fcalls) @ [`incr a];
      if Hashtbl.mem m.actions `incr
      then match Hashtbl.find m.actions `incr with
           | `f_do f  -> (match f with
                          | `incr f' -> f' a
                          | _ -> failwith "Impossible")
           | `f_ret x -> (match x with
                          | `incr x' -> x'
                          | _ -> failwith "Impossible")
      else Simple.incr a

    (* Function name identifiers *)
    let add = (`add : fname)
    and incr = (`incr : fname)

    (** Static **)

    (* Monad definitions *)
    let empty = {
      fcalls  = ref [];
      actions = Hashtbl.create 10;
    }
    (* return : a -> m a *)
    (* let return a = [a] *)
    (* let return x = { empty with expects = ref [x] } *)
    (* bind : m a -> (a -> m b) -> m b *)
    (* let bind x f = x.expects := !(x.expects) @ [x] *)
    (* let (>>=) = bind *)

    let doit = function
      | `add -> fun f -> `add f
      | `incr -> fun f -> `incr f

    (* Expectation langauge operations *)
    let one_of fn = One_of fn
    let never fn = Never fn
    let times fn n = Times (n, fn)
    let before fn1 fn2 = Before (fn1, fn2)
    let ( >> ) = before
    let will fn f = Will (fn, f)
    type exp +=
       | Will_add of (int -> int -> int)
       | Will_incr of (int -> int)
    (* let will' = function *)
    (*   | `add -> fun f -> Will_add f *)
    (*   | `incr -> fun f -> Will_incr f *)
    let ( *> ) = will
    let returns fn r = Returns (fn, r)
    (* let one_of fn = return (One_of fn) *)
    (* let never fn = return (Never fn) *)
    (* let times fn n = return (Times (n, fn)) *)
    (* let before fn1 fn2 = return (Before (fn1, fn2)) *)
    (* let ( >> ) = before *)
    (* let will fn f = return (Will (fn, f)) *)
    (* let ( *> ) = will *)
    (* let returns fn r = return (Returns (fn, r)) *)

    (* <generated> *)
    let add_will f = Will (`add, `add f)
    let incr_will f = Will (`incr, `incr f)
    let add_returns x = Returns (`add, `add x)
    let incr_returns x = Returns (`incr, `incr x)
    (* </generated> *)

    (* Compile the expectations *)
    let compile es = print_endline "compiling."; empty
    (* Verify the expectations after running *)
    let verify () =
      let r = print_endline "verifying." in
      if r = ()
      then `Success
      else `Failure "foo"

  end

  (* module Mock(E : sig val e : Expect.exp list end) = struct *)
  module Mock(E : sig val e : Expect.t end) = struct

    (* let m = ref Expect.empty *)

    (** <generated> **)

    (* Mocked functions *)
    let add a b = Expect.add_proxy E.e a b
    let incr a = Expect.incr_proxy E.e a

    (** </generated> **)

  end

end

module SUT(M : SIMPLE) = struct
  open M
  let doit x = add x 42 |> incr
end

let test_simple_mock () =
  (* Build a module E with a single value e, containing the compiled
     expectations. *)
  let module E = struct
    open Simple.Expect
    (* At this point, expectations are just a list. *)
    let e =
      [ one_of add
      ; add >> incr
      ; incr *> `incr (fun x -> x+2)
      ; incr_will (fun x -> x+2)
      ; incr_returns 42
      ; returns add (`add 42)
      ; add_returns 42 ]
      (* Here we compile the expectations into a form that can be verified
         by our mocked functions. *)
      |> compile
  end in
  (* Construct the mock using our compiled expectations. *)
  let module M = Simple.Mock(E) in
  (* Inject the mock module into the SUT *)
  let module T = SUT(M) in
  (* Exercise the mocked functions. *)
  T.doit 1 |> ignore
  (* M.( *)
  (*   add 1 2 |> incr |> ignore *)
  (* ) *)
  (* | `Success -> print_endline "success." *)
  (* | `Failure _ -> print_endline "failure." *)

let () =
  test_simple_mock ();
  print_endline "good."
