(* The original DOC, which will have the Expect and Mock
   inner modules generated by the framework. *)
module DOC = struct
  let add a b = a + b
  let incr x = x + 1
end

let test () =
  (* Create our expectation module E using module DOC, the
     generated mock module. We will use E later in the
     construction of a mocked DOC module. *)
  let module E = struct
    open DOC.Expect
    (* At this point, expectations are just a list. *)
    let e =
      [ one_of add
      ; before add incr
      ; add >> incr             (* add `before` incr *)
      ; will incr (`incr (fun x -> x+2))
      ; incr *> `incr (fun x -> x+2) (* incr `will` ... *)
      ; returns add (`add 42)
      ; add *-> (`add 42)
      (* Here we compile the expectations into a form that
         can be verified by our mocked functions. *)
      |> compile
  end in

  (* Construct the mock using our expectations in the E module *)
  let module M = DOC.Mock(E) in

  (* Continue with the test... *)