open Kaputt.Abbreviations

let mock_example = Test.make_simple_test
    (fun () ->
     (* eq_int_list tests that two lists are equal *)
     let eq_int_list = Assert.make_equal_list (=) string_of_int in
     (* Mock the standard library function 'succ' *)
     let f = Mock.from_function succ in
     (* Test input *)
     let i = [0; 1; 2; 0] in
     (* Exercise the mock *)
     let o = List.map (Mock.func f) i in
     (* Our expected outputs *)
     let o' = [1; 2; 3; 1] in
     (* Actual outputs match expected outputs *)
     eq_int_list o' o;
     (* Actual inputs match the inputs the mock received *)
     eq_int_list i (Mock.calls f);
     (* Mock was called the correct number of times *)
     Assert.equal_int (List.length i) (Mock.total f))
