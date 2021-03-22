open! Core
open Src

let m1 = Matrix.init ~h:2 ~w:2 ~f:(fun row col -> row + col)

let m2 = Matrix.init ~h:2 ~w:4 ~f:(fun row col -> row + col)

let () = Matrix.print (Matrix.mult_normal m1 m2)
