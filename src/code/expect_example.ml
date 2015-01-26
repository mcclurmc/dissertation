(* Example expectations *)

(* Copy this into utop before #use'ing this file:
module Simple = struct
  let add a b = a + b ;;
  let incr a = a + 1;;
end
*)

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

    (* XXX *)
    type action = ..
    type action +=
       | Will of (fname * ftype)
       | Returns of (fname * fret)

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

      (* TODO: move actions into 'action' type *)
      (* Action *)
      | Will of (fname * ftype)
      | Returns of (fname * fret)

      (* TODO: would also like to implement: *)
      (* | Raises of exception *)
      (* How do we encapsulate exceptions? *)

    module Fname = struct
      type t = fname
      let compare = compare
    end

    module ExpectMap = Map.Make(Fname)

    module FSet = Set.Make(Fname)

    type count =
      | Exactly of int
      | GTE of int
      | LTE of int

    type compiled_expect = {
      (* TODO probably not the right type here... *)
      action     : fact option;
      count      : count option;
      dominators : FSet.t;
    }

    (* Mock type *)
    type t = {
      (* fcalls  : (fname * farg) list ref;  (\* XXX change this to mutable *\) *)
      (* mutable fcounts : FSet.t; (\* XXX figure out how to incorporate this *\) *)
      (* actions : (fname, fact) Hashtbl.t; *)
      mutable fcalls : (fname * farg) list;
      expects        : compiled_expect ExpectMap.t;
    }

    type expect_violation =
      | Count_violation of (count * int) (* expected, actual *)
      | Order_violation of fname         (* function we expected in call list *)
      | Action_violation
      | Violation_msg of string

    exception Expect_violation of (farg * expect_violation)

    (** Generated **)

    let add_proxy m a b =
      (* Record this function invocation *)
      m.fcalls <- m.fcalls @ [`add, `add (a, b)];
      if ExpectMap.mem `add m.expects
      then
        let ncalls = List.(filter
                             (fun (f,_) -> f = `add)
                             m.fcalls
                           |> length) in
        let e = ExpectMap.find `add m.expects in

        (* Test for a violated exact count expectation *)
        (match e.count with
        | Some (Exactly count) ->
           if ncalls > count
           then raise (Expect_violation
                         (`add (a, b),
                          Count_violation ((Exactly count), ncalls)));
        | _ -> ());

        (* Test for a violated order expectation *)
        FSet.iter
          (fun d ->
           if not (List.exists (fun (f,_) -> f = d) m.fcalls)
           then raise (Expect_violation (`add (a, b), Order_violation d)))
          e.dominators;

        (* Perform any actions *)
        match e.action with
        | Some (`f_do f)  -> (match f with
                              | `add f' -> f' a b
                              | _ -> failwith "Impossible")
        | Some (`f_ret x) -> (match x with
                              | `add x' -> x'
                              | _ -> failwith "Impossible")
        | None -> Simple.add a b
      else Simple.add a b

    (* This isn't a complete proxy; see add_proxy instead. *)
    let incr_proxy m a =
      m.fcalls <- m.fcalls @ [`incr, `incr a];
      if ExpectMap.mem `incr m.expects
      then match (ExpectMap.find `incr m.expects).action with
           | Some (`f_do f)  -> (match f with
                                 | `incr f' -> f' a
                                 | _ -> failwith "Impossible")
           | Some (`f_ret x) -> (match x with
                                 | `incr x' -> x'
                                 | _ -> failwith "Impossible")
           | None -> Simple.incr a
      else Simple.incr a

    (* Function name identifiers *)
    let add = (`add : fname)
    and incr = (`incr : fname)

    (** Static **)

    (* Monad definitions *)
    let empty = {
      fcalls  = [];
      expects = ExpectMap.empty;
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
    (* let will' = function *)
    (*   | `add -> fun f -> Will_add f *)
    (*   | `incr -> fun f -> Will_incr f *)
    let ( *> ) = will
    let returns fn r = Returns (fn, r)
    let ( *-> ) = returns
    (* let one_of fn = return (One_of fn) *)
    (* let never fn = return (Never fn) *)
    (* let times fn n = return (Times (n, fn)) *)
    (* let before fn1 fn2 = return (Before (fn1, fn2)) *)
    (* let ( >> ) = before *)
    (* let will fn f = return (Will (fn, f)) *)
    (* let ( *> ) = will *)
    (* let returns fn r = return (Returns (fn, r)) *)

    (* <generated> *)
    type exp +=
       | Will_add of (int -> int -> int)
       | Will_incr of (int -> int)
    let add_will f = Will (`add, `add f)
    let incr_will f = Will (`incr, `incr f)
    let add_returns x = Returns (`add, `add x)
    let incr_returns x = Returns (`incr, `incr x)
    (* </generated> *)

    exception Compile_exception of string

    (* TODO translate expect list to ExpectMap *)
    (* Compile the expectations *)
    let compile es =
      print_endline "compiling.";
      (* XXX *)
      (* type compiled_expect = { *)
      (*   action     : fact; *)
      (*   count      : int; *)
      (*   dominators : FSet.t; *)
      (* } *)

      let empty = {
        action     = None;
        count      = None;
        dominators = FSet.empty;
      } in
      let get_expect f m =
        if ExpectMap.mem f m
        then ExpectMap.find f m
        else empty in
      let add_fcall f i t =
        let e  = get_expect f t.expects in
        match e.count with
        | None   -> { t with expects = ExpectMap.add f { e with count = Some i } t.expects }
        | Some _ -> raise (Compile_exception "Count already set")
      (* XXX TODO *)
      and add_dominator f1 f2 t = t
        (* let e = get_expect f2 t in *)
        (* { t with expects = FSet.add f1 e.dominators *)
        (* if FSet.mem f1 e.dominators *)
        (* then begin *)
        (*     let *)

        (*   end *)
        (* else FSet.add f1 *)
      and add_action f a t = t
      and add_return f r t = t
      in

      let rec compile t = function
        | []    -> t
        | e::es ->
           match e with
           | One_of f       -> compile (add_fcall f (GTE 1) t) es
           | Never f        -> compile (add_fcall f (Exactly 0) t) es
           | Times (i,f)    -> compile (add_fcall f (Exactly i) t) es
           | Before (f1,f2) -> compile (add_dominator f1 f2 t) es
           | Will (f,a)     -> compile (add_action f a t) es
           | Returns (f,r)  -> compile (add_return f r t) es
           | _ -> failwith "Impossible"
      in

      compile { fcalls = []; expects = ExpectMap.empty } es

    (* TODO Verify the expectations after running *)
    let verify () =
      let r = print_endline "verifying." in
      if r = ()
      then `Success
      else `Failure "foo"

  end

  (* module Mock(E : sig val e : Expect.exp list end) = struct *)
  (* module Mock(E : sig val e : Expect.compiled_expect Expect.ExpectMap.t end) = struct *)
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
      ; before add incr
      ; add >> incr
      ; will incr (`incr (fun x -> x+2))
      ; incr *> `incr (fun x -> x+2)
      ; incr_will (fun x -> x+2)
      ; incr_returns 42
      ; returns add (`add 42)
      ; add *-> (`add 42)
      ; add_returns 42 ]
      (* Here we compile the expectations into a form that can be
         verified by our mocked functions. *)
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
