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
