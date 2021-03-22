open! Core

type 'a t = {h:int; w:int; m: 'a array array}

let create ~h ~w ~default =
  {h = h; w = w; m = Array.make_matrix ~dimx:w ~dimy:h default} 

let set t row col v = 
  Array.set t.m.(col) row v
let get t row col = 
  t.m.(col).(row)
(* like create, except rather than a singular value, it takes a function of form row -> col -> val *)
let init ~h ~w ~f = 
  let init_column col = 
    Array.init h ~f:(fun row -> f row col) 
  in
  Array.init w ~f:init_column
  
exception Unimplmented of string

let check_add_compatible t1 t2 = 
  assert (t1.w = t2.w && t1.h = t2.h)

let check_mult_compatible t1 t2 =
  assert (t1.w = t2.h)
let add t1 t2 = 
  check_add_compatible t1 t2;
  init ~h:t1.h ~w:t1.w ~f:(fun row col -> (get t1 row col) + (get t2 row col))
let mult_normal t1 t2 = 
  check_mult_compatible t1 t2;
  let calculate_cell row col = 
    List.sum (module Int) (List.range 0 t1.w) ~f:(fun i -> (get t1 row i) * (get t2 i col))
  in
  init ~h:t1.h ~w:t2.w ~f:calculate_cell
let mult_stras t1 t2 _min_size = 
  check_mult_compatible t1 t2;
  raise (Unimplmented "mult_stras not implemented") 