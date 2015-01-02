(* Example expectations *)

module Simple = struct
  let add a b = a + b ;;
  let incr a = a + 1;;
end

module Mock = struct
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

    (* Expectation langauge type *)
    type exp =

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
      expects : exp list ref;
      actions : (fname, fact) Hashtbl.t;
    }

    (* Gotta be a way to do this with GADTs... *)
    (* let f_proxy fcalls factions fname fargs = *)
    (*   fcalls := !fcalls @ [fargs]; *)
    (*   if Hashtbl.mem factions fname *)
    (*   then match Hashtbl.find fname with *)
    (*        | `f_do f  -> f a b *)
    (*        | `f_ret x -> x *)

    (* let f_proxy : type a. t -> fname -> a expect  = fun m f args -> *)

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

    (* Expectation langauge type *)
    (* type exp = *)

    (*   (\* Counting *\) *)
    (*   | One_of of fname *)
    (*   | Never of fname *)
    (*   | Times of (int * fname) *)

    (*   (\* Order *\) *)
    (*   | Before of (fname * fname) *)

    (*   (\* Action *\) *)
    (*   | Will of (fname * ftype) *)
    (*   | Returns of (fname * fret) *)

    (* Monad definitions *)
    let return a = [a]
    let bind m f = List.(concat @@ map f m)
    let (>>=) m f = bind m f

    (* Expectation langauge operations *)
    let one_of fn m = One_of fn
    let never fn m = Never fn
    let times fn n m = Times (n, fn)
    let before fn1 fn2 m = Before (fn1, fn2)
    let will fn f m = Will (fn, f)
    let returns fn r m = Returns (fn, r)

  end

  (** Static **)

  (* Mock type *)
  (* type t = { *)
  (*   fcalls  : Expect.farg list ref; *)
  (*   expects : Expect.exp list ref; *)
  (*   actions : (Expect.fname, Expect.fact) Hashtbl.t; *)
  (* } *)

  let m = ref (None : Expect.t option)

  (* Initalize the mock *)
  let init () = m := Some Expect.({
    fcalls = ref [];
    expects = ref [];
    actions = Hashtbl.create 10;
  })

  (* Verify the expectations after running *)
  let verify m e = `Success (* or `Failure <reason> *)

  (** Generated **)

  (* Mocked functions *)
  let add a b =
    let rec f () = match !m with
    | Some m -> Expect.add_proxy m a b
    | None -> init (); f ()
    in f () ;;
  let incr a =
    let rec f () = match !m with
      | Some m -> Expect.incr_proxy m a
      | None -> init (); f ()
    in f () ;;

end

(* let test () = *)
(*   let open Mock.Expect in *)
(*   [%monad *)
(*       return `foo ] *)
    (* return `foo; *)
    (* one_of add; *)
    (* one_of cons ] *)

(*
let test_simple () =
  let module M = Mock(Simple) in
  let moudle SUT = SUT.Make(M) in

  let open M.Expect in
  let expect = create () in
  expect |> one_of cons ;
  expect |> times 2 add ;
  expect |> before add cons ;
  expect |> that add (fun _ _ -> 42)

let test_turtle () =
  let module M = Mock(Turtle) in
  let moudle SUT = SUT.Make(M) in

  let open M.Expect in
  let expect = create () in

  expect |> one_of pen_down ;
  expect |> one_of pen_up ;
  expect |> before pen_down move
 *)
