open! Core

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

let rec analye_crossover dim ?(on = dim) ?(min_cross = 15) min_val max_val
    num_trials_per =
  (* lc = logged crossover *)
  let on = if on % 2 = 0 then on else on + 1 in
  if on <= min_cross then []
  else
    let curr_speed =
      test_speed_w_dim_avg num_trials_per dim min_val max_val on
    in
    (on, curr_speed)
    :: analye_crossover dim min_val max_val num_trials_per ~on:(on / 2)

let get_range result =
  let min, _ =
    Option.value_exn
      (List.min_elt result ~compare:(fun (_cross1, t1) (_cross2, t2) ->
           Float.compare t1 t2))
  in
  (min / 2, min * 2)

let overlap (min1, max1) (min2, max2) =
  printf "%d, %d\n" min2 max2;
  if min1 = -1 && max1 = -1 then (min2, max2)
  else
    let new_min = max min1 min2 in
    let new_max = min max1 max2 in
    if new_max < new_min then (0, 0) else (new_min, new_max)

let create_small_range min_dim max_dim stride min_val max_val num_trials_per =
  List.fold (List.range min_dim max_dim ~stride) ~init:(-1, -1)
    ~f:(fun (min, max) dim ->
      overlap (min, max)
        (get_range
           (analye_crossover dim ~min_cross:min min_val max_val num_trials_per)))

let () =
  let m1 = gen_matrix 1024 1024 0 1 in
  let a = Matrix.mult_stras 44 m1 m1 in
  let _b = a in
  ()
