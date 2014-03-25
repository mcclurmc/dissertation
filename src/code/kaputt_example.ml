open Kaputt.Abbreviations

(* This is our SUT *)
let divider x y = x / y

(* A simplistic test case *)
let test_div_by_zero =
  Test.make_simple_test
    ~title:"test division by zero"
    (fun () ->
     Assert.make_raises
       (function Division_by_zero -> true | _ -> false)
       Printexc.to_string
       (fun () -> divider 1 0))

let () = Test.run_tests [ test_div_by_zero ]
