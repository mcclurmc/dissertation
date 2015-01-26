module type SIMPLE_S =
  sig
    val add  : int -> int -> int
    val incr : int -> int
  end [@mock]

(* 'Simple' module does not have any type inforamtion, so
   there is nothing to work with when generating a mock. *)
module Simple = struct
  let add a b = a + b ;;
  let incr a = a + 1;;
end [@mock]

(* We still have no type information, so this extension is
   not useful to us either. *)
(* module SimpleMock_struct = [%mock Simple] *)

(* Simlarly, a recovered signature is represented in the AST
   as just an identifier that points to the original module
   -- there is no type information available to us. *)
module type SIMPLE = module type of Simple
(* module SimpleMock_recovered_sig = [%mock SIMPLE] *)
