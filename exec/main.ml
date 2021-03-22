open! Core
open Src

let mat = Matrix.init ~h:512 ~w:512 ~f:(fun row col -> row + col)

let () =
  let m1 = Matrix.mult_normal mat mat in
  print_endline "done with normal";
  let m2 = Matrix.mult_stras mat mat 3 in
  print_endline "done with strassen";
  printf "%b\n" (Matrix.equal m1 m2)
