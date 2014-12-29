(* We have some dependency DOC *)
module DOC = struct
  let f x = x + 1
end

(* And a SUT which depends upon DOC *)
module SUT = struct
  let g x = DOC.f x
end

(* BUT we want to be able to use a different version of DOC,
   so we have to functorise SUT and specialise it with a new
   version of the DOC. *)

(* Create the module signature of DOC  *)
module type S = module type of DOC

(* SUT_functor now creates a version of the SUT which uses D
   instead of DOC *)
module SUT_functor(D : S) = struct
  let g x = D.f x
end

(* This is equivalent to the original SUT *)
module Original_SUT = SUT_functor(DOC)

(* This is a new SUT, with a different DOC *)
module New_SUT = SUT_functor(struct let f x = x + 42 end)
