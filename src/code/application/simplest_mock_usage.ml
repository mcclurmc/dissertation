open Simplest_mock_real

let draw =
  let open Turtle in
  make ()
  >> turn Left
  >> pen_down
  >> move 1
  >> turn Right
  >> move 2
  >> pen_up
  >> move 1
  |> ignore
