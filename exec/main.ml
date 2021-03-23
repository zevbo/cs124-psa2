open! Core
open Src

let gen_matrix h w min_val max_val =
  Matrix.init ~h ~w ~f:(fun _row _col ->
      min_val + Random.int (1 + max_val - min_val))

let test_speed mult_f t1 t2 =
  let time_0 = Unix.gettimeofday () in
  let _result = mult_f t1 t2 in
  Unix.gettimeofday () -. time_0

let test_speed_w_dim dim min_val max_val crossover =
  let gen () = gen_matrix dim dim min_val max_val in
  let t1 = gen () in
  let t2 = gen () in
  test_speed (Matrix.mult_stras crossover) t1 t2

let test_speed_w_dim_avg num_trials dim min_val max_val crossover =
  let trials =
    List.map (List.range 0 num_trials) ~f:(fun _ ->
        test_speed_w_dim dim min_val max_val crossover)
  in
  let sum = List.sum (module Float) trials ~f:(fun a -> a) in
  sum /. Float.of_int (List.length trials)

let analye_crossover dim min_val max_val num_trials_per =
  (* lc = logged crossover *)
  let test lc =
    test_speed_w_dim_avg num_trials_per dim min_val max_val
      (Int.of_float (2. ** Float.of_int lc))
  in
  let max_lc =
    Int.of_float (Float.round_up (log (Float.of_int dim) /. log 2.))
  in
  let min_lc = 0 in
  List.map (List.range min_lc (max_lc + 1)) ~f:(fun lc -> (lc, test lc))

let () =
  let result = analye_crossover 200 (-1) 1 5 in
  List.iter result ~f:(fun (lc, time) -> printf "lc, time: %n, %f\n" lc time);
  ()
