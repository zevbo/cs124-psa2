open! Core

let read file =
  let lines =
    Array.of_list
      (List.map (In_channel.read_lines file) ~f:(fun line ->
           Int.of_string (List.nth_exn (String.split line ~on:" ".[0]) 0)))
  in
  let d = Int.of_float (sqrt (Float.of_int (Array.length lines / 2))) in
  let make_matrix starting_row =
    Matrix.init ~h:d ~w:d ~f:(fun row col ->
        lines.(((row + starting_row) * d) + col))
  in
  let m1 = make_matrix 0 in
  let m2 = make_matrix d in
  (m1, m2)
