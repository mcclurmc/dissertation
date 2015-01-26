module M = struct
  let add a b = a + b
  let str i = Format.sprintf "%d" i
end

type f = [ `add | `str ]

(*
type _ e = ..

(* Static *)
type _ e +=
   | One_of : f -> f e
   | Never : f -> f e

let one_of f = One_of f
let never f = Never f

(* Generated *)
type _ e +=
   | Will_add : (int -> int -> int) -> (int -> int -> int) e
   | Will_str : (int -> string) -> (int -> string) e
   | Returns_add : int -> int e
   | Returns_str : string -> string e

let get_ret : type a. a e -> a =
    function
    | Returns_add i -> i
    | Returns_str s -> s
    | _ -> failwith "Not a return encoding"

let get_fun : type a. a e -> a =
    function
    | Will_add f -> f
    | Will_str f -> f
    | _ -> failwith "Not a function encoding"

*)

(* type (f,r) ty = { *)
(*   f : f; *)
(*   r : r; *)
(* } *)

let add = `add
let str = `str

type _ e = ..
type _ e +=
  | One_of : f -> f e
  | Returns : f * 'a -> 'a e
  | Will : f * 'a -> 'a e
(* type _ e + = *)
(*   | Returns_add : 'a -> 'a e *)
(*   | Returns_str : 'a -> 'a e *)
(*   | Will_add : f * 'a -> 'a e *)
(*   | Will_str : f * 'a -> 'a e *)

(* type _ e = .. *)
(* type _ e += *)
(*   | One_of of f *)
(* type _ e += *)
(*   | Returns_add : int -> int e *)
(*   | Returns_str : string -> string e *)
(*   | Will_add : (int -> int -> int) -> (int -> int -> int) e *)
(*   | Will_str : (int -> string) -> (int -> string) e *)

let one_of f = One_of f

let returns : type a. f -> a -> a e =
    function
    | `add -> (fun f -> Returns (`add, f))
    | `str -> (fun f -> Returns (`str, f))

(* let returns_ (\* : type a. f -> a -> a e *\) *)
(*   = function *)
(*     | `add -> (fun r -> Returns_add r) *)
(*     | `str -> (fun r -> Returns_str r) *)

let ( *> ) = returns

let will : type a. f -> a -> a e =
    function
    | `add -> (fun f -> Will (`add, f))
    | `str -> (fun f -> Will (`str, f))

(* let will_ : type a. f -> a -> a e = *)
(*     function *)
(*     | `add -> (fun f -> Will_add (`add, f)) *)
(*     | `str -> (fun f -> Will_str (`str, f)) *)

let ( *-> ) = will

(* let test1 = *)
(*   let r1 = returns str "hello" *)
(*   and r2 = returns add 42 *)
(*   and w1 = will str (fun i -> Format.sprintf "Num: %d" i) *)
(*   and w2 = will add (fun i j -> 42 + i - j) *)
(*   in *)
(*   let r = match r1,r2 with *)
(*     | (Returns (_,r1), Returns (_,r2)) -> *)
(*        (r1, r2) *)
(*     | _ -> failwith "unsupported" in *)
(*   let w = match w1,w2 with *)
(*     | (Will (_,f1), Will (_,f2)) -> *)
(*        (f1, f2) *)
(*     | _ -> failwith "unsupported" in *)
(*   (r,w) *)

let test2 (* : 'a e list  *)=
  [
  add *-> (fun i j -> j - i) ;
  (* str *> "hello" ; *)
  ]

(* let returns : type a. f -> 't -> _ e = *)
(*     function *)
(*     | `add -> (fun r -> Returns_add r) *)
(*     | `str -> (fun r -> Returns_str r) *)

(* let will : type a. f -> _ -> _ e = *)
(*     function *)
(*     | `add -> (fun f -> Will_add f) *)
(*     | `str -> (fun f -> Will_str f) *)
