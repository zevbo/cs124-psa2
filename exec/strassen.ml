open! Core

let strassen_cutoff = 44

let gen_matrix h w min_val max_val =
  Matrix.init ~h ~w ~f:(fun _row _col ->
      min_val + Random.int (1 + max_val - min_val))

let gen_adacency_matrix size p =
  Matrix.init ~h:size ~w:size ~f:(fun row col ->
      if (not (row = col)) && Float.(Random.float_range 0.0 1.0 < p) then 1
      else 0)

let find_triangles size p =
  let adacency_matrix = gen_adacency_matrix size p in
  let cubed = Matrix.exp adacency_matrix 3 strassen_cutoff in
  List.sum (module Int) (List.range 0 size) ~f:(fun i -> Matrix.get cubed i i)
  / 6

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

let rec analye_crossover dim ?(on = dim) ?(min_cross = 30) ?(max_cross = dim)
    min_val max_val num_trials_per =
  (* lc = logged crossover *)
  let analye_crossover () =
    analye_crossover dim ~min_cross ~max_cross min_val max_val num_trials_per
      ~on:(on / 2)
  in
  let on = if on % 2 = 0 then on else on + 1 in
  if on < min_cross then []
  else if on > max_cross then analye_crossover ()
  else
    let curr_speed =
      test_speed_w_dim_avg num_trials_per dim min_val max_val on
    in
    (on, curr_speed) :: analye_crossover ()

let get_range result =
  let min, _ =
    Option.value_exn
      (List.min_elt result ~compare:(fun (_cross1, t1) (_cross2, t2) ->
           Float.compare t1 t2))
  in
  (min / 2, min * 2)

let analyze_many_crossovers min_cross max_cross stride min_num max_num
    trials_per =
  let inc_constant = 8 in
  List.map (List.range min_cross max_cross ~stride) ~f:(fun cross ->
      analye_crossover (inc_constant * cross)
        ~max_cross:((2 * cross) + 1)
        ~min_cross:(cross - 1) min_num max_num trials_per)

let () =
  let result = analyze_many_crossovers 42 43 2 (-5) 5 2 in
  List.iter result ~f:(fun vals ->
      let i1, f1 = List.nth_exn vals 0 in
      let i2, f2 = List.nth_exn vals 1 in
      printf "%n, %f | %n, %f\n" i2 f2 i1 f1);
  ()
