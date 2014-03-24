open OUnit

(* This is our SUT *)
let divider x y = x / y

(* A pair of simplistic test cases *)
let test_div_by_zero () =
  assert_raises
    Division_by_zero
    (fun () -> divider 1 0)

let test_1_div_2_eq_0 () =
  assert_equal
    ~msg:"Test int division"
    (divider 1 2)
    0

(* Group our test cases into a suite *)
let suite =
  "Example suite" >::: [
    "test_div_by_zero"  >:: test_div_by_zero;
    "test_1_div_2_eq_0" >:: test_1_div_2_eq_0
  ]

(* Our main function runs the test suite *)
let _ = run_test_tt_main suite
