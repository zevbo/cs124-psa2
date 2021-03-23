open! Core
open Src

let mat = Matrix.init ~h:100 ~w:100 ~f:(fun row col -> row + col)

let gen_matrix h w min_val max_val =
  Matrix.init ~h ~w ~f:(fun _row _col ->
      min_val + Random.int (max_val - min_val))

let test_speed mult_f t1 t2 =
  let time_0 = Unix.gettimeofday () in
  let _result = mult_f t1 t2 in
  Unix.gettimeofday () -. time_0

let () =
  let mat1, mat2 = Read_input.read "test_matricies.txt" in
  Matrix.print mat1;
  Matrix.print mat2;
  let m1 = Matrix.mult_normal mat mat in
  print_endline "done with normal";
  let m2 = Matrix.mult_stras 3 mat mat in
  print_endline "done with strassen";
  printf "%b\n" (Matrix.equal m1 m2)
