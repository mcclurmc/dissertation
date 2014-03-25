open Kaputt.Abbreviations

(* Precondition *)
let is_int : int Spec.predicate = fun _ -> true

(* Postcondition *)
let one_greater (a,b) = b = a+1

let spec_example =
  Test.make_random_test
    ~title:"Test succ function"
    ~nb_runs:128
    Gen.int
    succ                        (* SUT *)
    [ is_int => one_greater ]   (* pre- implies post-condition *)
