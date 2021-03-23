open! Core
open Src

let read file =
  let lines =
    Array.of_list
      (List.map (In_channel.read_lines file) ~f:(fun line ->
           Array.of_list (String.split line ~on:" ".[0])))
  in
  let make_matrix starting_row =
    Matrix.init
      ~h:(Array.length lines / 2)
      ~w:(Array.length lines.(0))
      ~f:(fun row col -> Int.of_string lines.(row + starting_row).(col))
  in
  let m1 = make_matrix 0 in
  let m2 = make_matrix (Array.length lines / 2) in
  (m1, m2)
