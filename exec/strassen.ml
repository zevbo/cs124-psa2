open! Core

let strassen_cutoff = 25

let print_diagnol (m : int Matrix.t) =
  List.iter
    (List.range 0 (min m.h m.w))
    ~f:(fun n -> printf "%n\n" (Matrix.get m n n))

let () =
  Testing.test_multiplication ();
  (* let args = Sys.get_argv () in
     let filename = args.(3) in
     let m1, m2 = Read_matrix.read filename in
     let result = Matrix.mult_stras strassen_cutoff m1 m2 in
     print_diagnol result; *)
  let p_list = [ 0.01 ] in
  let run_triangles trials p =
    let expected = Float.of_int (1024 * 1023 * 1022 / 6) *. (p **. 3.) in
    let results =
      List.map (List.range 0 trials) ~f:(fun _ -> Testing.find_triangles 1024 p)
    in
    let errors =
      List.map results ~f:(fun result ->
          printf "result, expected: %d ,  %f\n" result expected;
          Float.abs (Float.of_int result -. expected) /. expected)
    in
    printf "Avg: %f\n"
      ( ( List.sum (module Float) results ~f:(fun i -> Float.of_int i)
          /. Float.of_int trials
        -. expected )
      /. expected );
    List.sum (module Float) errors ~f:(fun n -> n) /. Float.of_int trials
  in
  let trials = 4 in
  List.iter p_list ~f:(fun p -> printf "%f\n" (run_triangles trials p))
