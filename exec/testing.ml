open! Core

let strassen_even_cutoff = 50

let strassen_odd_cutoff = 71

let gen_matrix h w min_val max_val =
  Matrix.init ~h ~w ~f:(fun _row _col ->
      min_val + Random.int (1 + max_val - min_val))

let gen_adacency_matrix size p =
  let og_matrix =
    Matrix.init ~h:size ~w:size ~f:(fun row col ->
        if (not (row = col)) && Float.(Random.float_range 0.0 1.0 < p) then 1
        else 0)
  in
  let undirected =
    Matrix.init ~h:size ~w:size ~f:(fun row col ->
        if row >= col then Matrix.get og_matrix row col
        else Matrix.get og_matrix col row)
  in
  undirected

let find_triangles size p =
  let adacency_matrix = gen_adacency_matrix size p in
  assert (adacency_matrix.w = size);
  assert (adacency_matrix.h = size);
  let cubed =
    Matrix.exp adacency_matrix 3 strassen_even_cutoff strassen_odd_cutoff
  in
  assert (cubed.w = size);
  assert (cubed.h = size);
  let v =
    List.sum (module Int) (List.range 0 size) ~f:(fun i -> Matrix.get cubed i i)
    / 6
  in
  if v < 0 then assert false;
  v

let test_speed mult_f t1 t2 =
  let time_0 = Unix.gettimeofday () in
  let _result = mult_f t1 t2 in
  Unix.gettimeofday () -. time_0

let test_speed_w_dim dim min_val max_val crossover =
  Random.init (Int.of_float (Unix.time ()));
  let gen () = gen_matrix dim dim min_val max_val in
  let t1 = gen () in
  let t2 = gen () in
  test_speed (Matrix.mult_stras crossover crossover) t1 t2

let test_speed_w_dim_avg num_trials dim min_val max_val crossover =
  let trials =
    List.map (List.range 0 num_trials) ~f:(fun _ ->
        test_speed_w_dim dim min_val max_val crossover)
  in
  let sum = List.sum (module Float) trials ~f:(fun a -> a) in
  sum /. Float.of_int (List.length trials)

let rec analyze_crossover dim ?(on = dim) ?(min_cross = 30) ?(max_cross = dim)
    min_val max_val num_trials_per =
  (* lc = logged crossover *)
  let analyze_crossover () =
    analyze_crossover dim ~min_cross ~max_cross min_val max_val num_trials_per
      ~on:(on / 2)
  in
  let on = if on % 2 = 0 then on else on + 1 in
  if on < min_cross then []
  else if on > max_cross then analyze_crossover ()
  else
    let curr_speed =
      test_speed_w_dim_avg num_trials_per dim min_val max_val on
    in
    (on, curr_speed) :: analyze_crossover ()

let analyze_many_crossovers min_cross max_cross stride min_num max_num
    trials_per min_dim odd =
  let cross_to_dim cross =
    let exp =
      Float.round_up (log (Float.of_int min_dim /. Float.of_int cross) /. log 2.)
    in
    if odd then ((cross * 2) - 1) * Int.of_float (2. **. (exp -. 1.))
    else Int.of_float (2. **. exp) * cross
  in
  List.map (List.range min_cross max_cross ~stride) ~f:(fun cross ->
      analyze_crossover (cross_to_dim cross)
        ~max_cross:((2 * cross) + 1)
        ~min_cross:(cross - 1) min_num max_num trials_per)

let print_result result =
  List.iter result ~f:(fun vals ->
      let i1, f1 = List.nth_exn vals 0 in
      let i2, f2 = List.nth_exn vals 1 in
      printf "%n, %f | %n, %f\n" i2 f2 i1 f1);
  List.iter result ~f:(fun vals ->
      let i2, _f2 = List.nth_exn vals 1 in
      printf "%n\n" i2);
  printf "----\n";
  List.iter result ~f:(fun vals ->
      let _i2, f2 = List.nth_exn vals 1 in
      printf "%f\n" f2);
  printf "----\n";
  List.iter result ~f:(fun vals ->
      let i1, _f1 = List.nth_exn vals 0 in
      printf "%n\n" i1);
  printf "----\n";
  List.iter result ~f:(fun vals ->
      let _i1, f1 = List.nth_exn vals 0 in
      printf "%f\n" f1);
  print_endline ""

let test_specific_multiplication mat1 mat2 =
  let m1 = Matrix.mult_normal mat1 mat2 in
  let m2 = Matrix.mult_stras 2 2 mat1 mat2 in
  if not (Matrix.equal m1 m2) then (
    printf "(%n, %n), (%n, %n)\n" m1.h m1.w m2.h m2.w;
    Matrix.print m1;
    Matrix.print m2;
    assert false )

let test_multiplication () =
  let mat1, mat2 = Read_matrix.read "test_matricies.txt" in
  test_specific_multiplication mat1 mat2;
  test_specific_multiplication (gen_matrix 101 91 0 5) (gen_matrix 91 73 0 5);
  ()

let () = ()
