open! Core

type 'a t = {h:int; w:int; m: 'a array array}

let create ~h ~w ~default =
  {h = h; w = w; m = Array.make_matrix ~dimx:w ~dimy:h default} 
let set t row col v = 
  Array.set t.m.(col) row v
let get t row col = 
  t.m.(col).(row)
exception Unimplmented of string

let check_compatible t1 t2 =
  assert (t1.w = t2.h)
let mult_normal t1 t2 = 
  check_compatible t1 t2;
  let size = t1.w in
  let final_matrix = create ~h:t1.h ~w:t1.w ~default:0 in
  let calculate_cell row col = 
    List.fold_left (List.range 0 size) ~init:0 ~f:(fun sum i -> sum + (get t1 row i) * (get t2 col i))
  in
  let calc_and_set_cell (row, col) = 
    set final_matrix row col (calculate_cell row col)
  in
  List.iter (List.cartesian_product (List.range 0 t1.h) (List.range 0 t2.w))
    ~f:calc_and_set_cell;
  final_matrix
let mult_stras t1 t2 _min_size = 
  check_compatible t1 t2;
  raise (Unimplmented "mult_stras not implemented") 