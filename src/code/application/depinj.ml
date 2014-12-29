(* Examples of depenency injection in OCaml *)

(* We have some dependency DOC *)
module DOC = struct
  let f x = x + 1
end

(* And a SUT which depends upon DOC *)
module SUT = struct
  let g x = DOC.f x
end

(* BUT we want to be able to use a different version of DOC, so we
   have to functorise SUT and specialise it with a new version of the
   DOC. *)

(* Create the module signature of DOC  *)
module type S = module type of DOC

(* SUT_functor now creates a version of the SUT which uses D instead of DOC *)
module SUT_functor(D : S) = struct
  let g x = D.f x
end

(* This is equivalent to the original SUT *)
module Original_SUT = SUT_functor(DOC)

(* This is a new SUT, with a different DOC *)
module New_SUT = SUT_functor(struct let f x = x + 42 end)

(* Downsides to functorisation: We have to do significant refactoring,
especially if the module in quesiton is at the toplevel of a
compilation unit, because ml files cannot be functors. A pattern for
"lifting" a toplevel module into a functor is to surround the whole
file with a new module calld "Make": 'module Make (D : DOC) = struct
...', and at the end of the file, after we end the Make module, add
'include Make(DOC)', which will modify this module so that it contains
both a functor to create a new module, and the original contents of
the module. If the ml file is restricted by an mli file, it may be
simpler to just move the functor to a new ml file (say,
"SUT_func.ml"), and have the original ml file contain only the line
'include SUT_func.Make(DOC)'. *)

(* Alternatively, instead of using functors we could use first class
modules. We have a couple different options. We can introduce a first
class module at the toplevel of the SUT. Or we could intruduce a new
module parameter to the function which we wish to test. *)

module SUT_fc = struct

  (* Extract the module signature from the DOC *)
  module type S = module type of DOC

  (* First option: optionally pass a new DOC into the function we want
     to test. The DOC module is an optional argument, so we haven't  *)
  let g ?(m=(module DOC : S)) x =
    let module M = (val m : S) in
    M.f x

  let m = ref (module DOC : S)

  let h x =
    let module M = (val !m) in
    M.f x

end

(* Downsides to this method are that each function which needs to have
the option of dependency injection needs to be refactored to install
the new dependency. This may or may not be less invasive than
functorising the entire module. There is also a very slight
performance cost to this method, but it is so low an overhead that it
should be ignored for most purposes. *)

(* Another option for managing dependency injection using first class
modules would be to create a "module factory." Client modules could
ask this factory to create a particular module, and the module factory
would check whether it should return the production dependency or the
test dependency, depending on perhaps a command-line flag or some
configuration file. A major benefit to this method is that it removes
the need to refactor each of the functions that access the first class
module, because we can set this module at module initialisation
time. *)

module Factory = struct
  (* Always return the "fake" module, for simplicity *)
  module type S = module type of DOC
  let get_module () = (module struct let f x = x + 42 end : S)
end

module SUT_factory = struct
  module DOC = (val Factory.get_module ())
  let g x = DOC.f x
end

let main =
  (* Using the original SUT *)
  Printf.printf "Original:    g 42 = %d\n" (SUT.g 42);
  Printf.printf "Functor:     g 42 = %d\n" (New_SUT.g 42);
  Printf.printf "First-class: g 42 = %d\n" (SUT_fc.g 42);
  Printf.printf "First-class: g ~m:(module struct \
                 let f x = x + 42 end) 42 = %d\n"
                (SUT_fc.g ~m:(module struct let f x = x + 42 end) 42);
  Printf.printf "Factory:     g 42 = %d\n" (SUT_factory.g 42);
  ()
