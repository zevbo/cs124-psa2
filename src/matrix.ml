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
  {h; w; m = Array.init w ~f:init_column}

let check_add_compatible t1 t2 = 
  assert (t1.w = t2.w && t1.h = t2.h)

let check_mult_compatible t1 t2 =
  assert (t1.w = t2.h)
let add t1 t2 = 
  check_add_compatible t1 t2;
  init ~h:t1.h ~w:t1.w ~f:(fun row col -> (get t1 row col) + (get t2 row col))
let subtract t1 t2 = 
  check_add_compatible t1 t2;
  init ~h:t1.h ~w:t1.w ~f:(fun row col -> (get t1 row col) - (get t2 row col))
let mult_normal t1 t2 = 
  check_mult_compatible t1 t2;
  let calculate_cell row col = 
    List.sum (module Int) (List.range 0 t1.w) ~f:(fun i -> (get t1 row i) * (get t2 i col))
  in
init ~h:t1.h ~w:t2.w ~f:calculate_cell
let submatrix t row1 row2 col1 col2 =
  (*Returns the submatrix of t from h1,w1 (inclusive) to h2,w2 (exclusive)*)
  let h = row2 - row1 in 
  let w = col2 - col1 in 
  init ~h ~w ~f:(fun row col -> get t (row + row1) (col + col1))
let make_from_submatrices t11 t12 t21 t22 = 
  (*Returns a matrix composed of submatrices*)
  assert (t11.h = t12.h);
  assert (t21.h = t22.h);
  assert (t11.w = t21.w);
  assert (t12.w = t22.w);
  assert (t11.w + t12.w = t21.w + t22.w);
  assert (t11.h + t21.h = t12.h + t22.h);
  let h = t11.h + t21.h in 
  let w = t11.w + t12.w in 
  let get_val row col = 
    let top_row = row < t11.h in 
    let top_col = col < t11.w in
    let row0 = if top_row then 0 else t11.h in
    let col0 = if top_col then 0 else t11.w in
    let mat_to_use = 
      if top_row then
        if top_col then t11 else t12 
      else 
        if top_col then t21 else t12
    in 
    get mat_to_use (row - row0) (col - col0)
  in
  init ~h ~w ~f:get_val
  
let rec mult_stras t1 t2 _min_size = 
  check_mult_compatible t1 t2;
  let mult_stras t1 t2 = mult_stras t1 t2 _min_size in
  (*Assumes that matrices are square and have sizes that are powers of 2*)
  if t1.h <= _min_size then mult_normal t1 t2 else (
    let a = submatrix t1 0 (t1.h/2) 0 (t1.w/2) in
    let b = submatrix t1 0 (t1.h/2) (t1.w/2) t1.w in
    let c = submatrix t1 (t1.h/2) t1.h 0 (t1.w/2) in
    let d = submatrix t1 (t1.h/2) t1.h (t1.w/2) t1.w in
    let e = submatrix t2 0 (t2.h/2) 0 (t2.w/2) in
    let f = submatrix t2 0 (t2.h/2) (t2.w/2) t2.w in
    let g = submatrix t2 (t2.h/2) t2.h 0 (t2.w/2) in
    let h = submatrix t2 (t2.h/2) t2.h (t2.w/2) t2.w in

    let p1 = mult_stras a (subtract f h) in
    let p2 = mult_stras (add a b) h in
    let p3 = mult_stras (add c d) e in
    let p4 = mult_stras d (subtract g e) in
    let p5 = mult_stras (add a d) (add e h) in
    let p6 = mult_stras (subtract b d) (add g h) in
    let p7 = mult_stras (subtract a c) (add e f) in
    let t11 = add (add p5 p4) (subtract p6 p2) in
    let t12 = add p1 p2 in
    let t21 = add p3 p4 in
    let t22 = subtract (add p5 p1) (add p3 p7) in
    make_from_submatrices t11 t12 t21 t22
  )