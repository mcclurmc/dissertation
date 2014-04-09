(* Prototype GADT implementation of expectations *)

module M = struct
  let fun1 i = string_of_int i
  let fun2 s = int_of_string s
  let fun3 i f = Printf.sprintf "%d, %f" i f
end

(* Represent the whole function *)
type _ ty_fun =
  | Fun1 : (int -> string) ty_fun
  | Fun2 : (string -> int) ty_fun
  | Fun3 : (int -> float -> string) ty_fun

type fun_vals =
  | Fun1 of (int -> string)
  | Fun2 of (string -> int)
  | Fun3 of (int -> float -> string)

(* Represent the return value *)
type _ ret =
  | Fun1 : string ret
  | Fun2 : int ret
  | Fun3 : string ret

(* Represent the arguements *)
type _ args =
  | Fun1 : int args
  | Fun2 : string args
  | Fun3 : (int * float) args

type ret_vals =
  | Fun1 of string
  | Fun2 of int
  | Fun3 of string

type arg_vals =
  | Fun1 of int
  | Fun2 of string
  | Fun3 of int * float

(* Simple expectation type *)
type ('a,'r) e = {
  ty_ret  : 'r ret;
  ty_args : 'a args;
  args    : arg_vals option;
  return  : ret_vals option;
}

let returns: type a r. r -> (a,r) e -> (a,r) e = fun r e ->
  match e.ty_ret with
  | Fun1 -> { e with return = Some (Fun1 r : ret_vals) }
  | Fun2 -> { e with return = Some (Fun2 r : ret_vals) }
  | Fun3 -> { e with return = Some (Fun3 r : ret_vals) }

let mk_fun1 a1 = Fun1 a1
and mk_fun2 a1 = Fun2 a1
and mk_fun3 (a1,a2) = Fun3 (a1,a2)

let takes: type a r. a -> (a,r) e -> (a,r) e = fun a e ->
  match e.ty_args with
  | Fun1 -> { e with args = Some (mk_fun1 a) }
  | Fun2 -> { e with args = Some (mk_fun2 a) }
  | Fun3 -> { e with args = Some (mk_fun3 a) }

let fun1 = { ty_ret = Fun1; ty_args = Fun1; args = None; return = None }
and fun2 = { ty_ret = Fun2; ty_args = Fun2; args = None; return = None }
and fun3 = { ty_ret = Fun3; ty_args = Fun3; args = None; return = None }

let (>>) e f = f e

(* Regular variant types *)
let returns_ r e = { e with return = Some r }
let _ = returns_ (Fun1 "hello")
let _ = returns_ (Fun2 42)

(* With GADTs *)
let _ = fun1 >> takes 42 >> returns "hello"
let _ = fun2 >> takes "hello" >> returns 42
(* Tuples are good enough for now, but we'd really like to take
   function arguement syntax, so we can accept labeled args, etc. *)
let _ = fun3 >> takes (1, 1.) >> returns "1, 1.0"

(***********************************)
(* Varargs implemented with GADTs? *)
(***********************************)

type _ varargs =
  | F1 : ('a -> 'b) varargs
  | F2 : ('a -> 'b -> 'c) varargs
type ('a, 'b, 'c) funvals =
  | F1 of ('a -> 'b)
  | F2 of ('a -> 'b -> 'c)

let takes: type a r. a -> (a,r) e -> (a,r) e = fun a e ->
  match e.ty_args with
  | Fun1 -> { e with args = Some (Fun1 a : arg_vals) }
  | Fun2 -> { e with args = Some (Fun2 a : arg_vals) }
  | Fun3 -> { e with args = Some (Fun3 (fst a, snd a) : arg_vals) }

type _ funty =
  | Fun : ('a -> 'r funty) -> ('a * 'r) funty
  (* | Fun : ('a * 'r funty) -> ('a * 'r) funty *)
  | Ret : 'r -> 'r funty

(* let f1 = Fun (4, Fun ('a', Ret 2)) *)
let f2 r = Fun (fun a -> Fun (fun b -> Fun (fun c -> Ret r)))

let rec make i =
  if i = 1
  then Fun (fun a -> Ret a)
  else Fun (fun a -> Ret a)
(* Fun (fun a -> make (i-1)) *)

(* let rec takes: type a t. a -> (a -> t funty) funty = fun a -> *)
(*                  fun b -> b *)

(* let takes: type t. t varargs -> *)


type _ va =
  | R : _ va
  | F : int -> _ va

(* let rec f : type t. t va -> (t -> 'r) = function *)
(*   | R -> (fun r -> r) *)
(*   | F i -> (fun r -> r) *)
