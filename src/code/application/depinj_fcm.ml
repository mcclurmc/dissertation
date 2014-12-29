module SUT_fc = struct

  (* If we haven't already defined one, we can extract the
     module signature from the DOC *)
  module type S = module type of DOC

  (* First option: optionally pass a new DOC into the
     function we want to test. The DOC module is an optional
     argument, so we aren't forced to modify callers. *)
  let g ?(m=(module DOC : S)) x =
    let module M = (val m : S) in
    M.f x

  (* Second option: create a reference which stores the
     current DOC. This module is dereferenced in the calling
     function, as below in 'h'. *)
  let m = ref (module DOC : S)

  let h x =
    let module M = (val !m) in
    M.f x

end
