open! Core
open Src

let m = Matrix.init ~h:10 ~w:10 ~f:(fun row col -> row + col)

let () = Matrix.print m
