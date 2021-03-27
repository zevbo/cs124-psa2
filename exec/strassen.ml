open! Core

let print_diagnol (m : int Matrix.t) =
  List.iter
    (List.range 0 (min m.h m.w))
    ~f:(fun n -> printf "%n\n" (Matrix.get m n n))

let () =
  Testing.test_multiplication ();
  let args = Sys.get_argv () in
  if String.equal args.(1) "0" then
    let filename = args.(3) in
    let m1, m2 = Read_matrix.read filename in
    let result =
      Matrix.mult_stras Testing.strassen_even_cutoff Testing.strassen_odd_cutoff
        m1 m2
    in
    print_diagnol result
  else if String.equal args.(1) "1" then
    let p_list = [ 0.001 ] in
    let run_triangles trials p =
      let expected = Float.of_int (1024 * 1023 * 1022 / 6) *. (p **. 3.) in
      let results =
        List.map (List.range 0 trials) ~f:(fun _ ->
            Testing.find_triangles 1024 p)
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
    let trials = 12 in
    List.iter p_list ~f:(fun p -> printf "%f\n" (run_triangles trials p))
  else if String.equal args.(1) "2" then (
    print_endline "Starting Crossover Analysis";
    let analyze_many_crossovers min_cross odd =
      Testing.print_result
        (Testing.analyze_many_crossovers 14 50 2 (-2) 2 50 min_cross odd)
    in
    analyze_many_crossovers 80 true;
    analyze_many_crossovers 160 true;
    analyze_many_crossovers 320 true )
