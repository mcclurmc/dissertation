module Factory = struct
  (* Always return the "fake" module, for simplicity *)
  module type S = module type of DOC
  let get_module () = (module struct let f x = x + 42 end : S)
end

module SUT_factory = struct
  module DOC = (val Factory.get_module ())
  let g x = DOC.f x
end
