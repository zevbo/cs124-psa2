open! Core

type pos = { row : int; col : int }

let create_pos row col = { row; col }

type 'a t = {
  h : int;
  w : int;
  pad_h : int;
  pad_w : int;
  m : 'a array array;
  tl : pos;
}

let create ~h ~w ~default =
  {
    h;
    w;
    pad_h = 0;
    pad_w = 0;
    m = Array.make_matrix ~dimx:w ~dimy:h default;
    tl = create_pos 0 0;
  }

exception Matrix_outofbounds of string

let get t row col =
  let check_in_bounds v max =
    if v < 0 || v >= max then
      raise
        (Matrix_outofbounds
           (String.concat
              [
                "Attempted to get (";
                Int.to_string row;
                ", ";
                Int.to_string col;
                ") from matrix of size ";
                Int.to_string t.h;
                " x ";
                Int.to_string t.w;
              ]))
  in
  check_in_bounds row t.h;
  check_in_bounds col t.w;
  if row + t.pad_h < t.h && col + t.pad_w < t.w then
    t.m.(col + t.tl.col).(row + t.tl.row)
  else 0

(* like create, except rather than a singular value, it takes a function of form row -> col -> val *)
let init ~h ~w ~f =
  let init_column col = Array.init h ~f:(fun row -> f row col) in
  {
    h;
    w;
    pad_h = 0;
    pad_w = 0;
    m = Array.init w ~f:init_column;
    tl = create_pos 0 0;
  }

let submatrix_to_regular t =
  init ~h:t.h ~w:t.w ~f:(fun row col -> get t row col)

let transpose t = init ~h:t.w ~w:t.h ~f:(fun row col -> get t col row)

let check_add_compatible t1 t2 = assert (t1.w = t2.w && t1.h = t2.h)

let check_mult_compatible t1 t2 = assert (t1.w = t2.h)

let add t1 t2 =
  check_add_compatible t1 t2;
  init ~h:t1.h ~w:t1.w ~f:(fun row col -> get t1 row col + get t2 row col)

let subtract t1 t2 =
  check_add_compatible t1 t2;
  init ~h:t1.h ~w:t1.w ~f:(fun row col -> get t1 row col - get t2 row col)

let long_arithmetic t0 ts_and_sign =
  init ~h:t0.h ~w:t0.w ~f:(fun row col ->
      List.fold ts_and_sign ~init:(get t0 row col) ~f:(fun acc (t, sign) ->
          acc + (sign * get t row col)))

let equal t1 t2 =
  let rows_equal = Array.equal ( = ) in
  t1.h = t2.h && t1.w = t2.w && Array.equal rows_equal t1.m t2.m

let mult_normal t1 t2 =
  check_mult_compatible t1 t2;
  let calculate_cell row col =
    List.sum
      (module Int)
      (List.range 0 t1.w)
      ~f:(fun i -> get t1 row i * get t2 i col)
  in
  init ~h:t1.h ~w:t2.w ~f:calculate_cell

let submatrix t row1 row2 col1 col2 =
  (*Returns the submatrix of t from h1,w1 (inclusive) to h2,w2 (exclusive)*)
  let h = row2 - row1 in
  let w = col2 - col1 in
  let tl = create_pos row1 col1 in
  { t with h; w; tl }

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
      if top_row then if top_col then t11 else t12
      else if top_col then t21
      else t22
    in
    get mat_to_use (row - row0) (col - col0)
  in
  init ~h ~w ~f:get_val

let split_on_point t row col =
  ( submatrix t 0 row 0 col,
    submatrix t 0 row col t.w,
    submatrix t row t.h 0 col,
    submatrix t row t.h col t.w )

let pad_even t =
  let round_up v = if v % 2 = 0 then v else v + 1 in
  let h = round_up t.h in
  let w = round_up t.w in
  { t with h; w; pad_h = t.pad_h + h - t.h; pad_w = t.pad_w + w - t.w }

let print t =
  let t = submatrix_to_regular t in
  let t = transpose t in
  let num_size n =
    let extra_one = if n < 0 then 1 else 0 in
    let n = abs n in
    extra_one
    +
    if n < 2 then 1
    else Int.of_float (Float.round_up (log (Float.of_int n) /. log 9.99))
  in
  let max_num_size_row row =
    let max = Array.max_elt (Array.map row ~f:num_size) ~compare:Int.compare in
    Option.value max ~default:0
  in
  let max_num_size =
    Option.value
      (Array.max_elt (Array.map t.m ~f:max_num_size_row) ~compare:Int.compare)
      ~default:0
  in
  let horizontal = String.make ((t.h * (max_num_size + 1)) + 3) "-".[0] in
  let print_correct n =
    let msg =
      String.concat
        [
          String.make (max_num_size - num_size n) " ".[0]; Int.to_string n; " ";
        ]
    in
    print_string msg
  in
  let print_line row =
    printf "| ";
    Array.iter row ~f:print_correct;
    printf "|\n"
  in
  print_endline horizontal;
  Array.iter t.m ~f:print_line;
  print_endline horizontal

let rec mult_stras_2_power min_size t1_og t2_og =
  check_mult_compatible t1_og t2_og;
  let mult_stras = mult_stras_2_power min_size in
  (*Assumes that matrices are square and have sizes that are powers of 2*)
  if t1_og.h <= min_size then mult_normal t1_og t2_og
  else
    let t1 = pad_even t1_og in
    let t2 = pad_even t2_og in
    let a, b, c, d = split_on_point t1 (t1.h / 2) (t1.w / 2) in
    let e, f, g, h = split_on_point t2 (t2.h / 2) (t2.w / 2) in

    let p1 = mult_stras a (subtract f h) in
    let p2 = mult_stras (add a b) h in
    let p3 = mult_stras (add c d) e in
    let p4 = mult_stras d (subtract g e) in
    let p5 = mult_stras (add a d) (add e h) in
    let p6 = mult_stras (subtract b d) (add g h) in
    let p7 = mult_stras (subtract a c) (add e f) in
    let t11 = long_arithmetic p5 [ (p4, 1); (p6, 1); (p2, -1) ] in
    let t12 = add p1 p2 in
    let t21 = add p3 p4 in
    let t22 = long_arithmetic p5 [ (p1, 1); (p3, -1); (p7, -1) ] in
    let result = make_from_submatrices t11 t12 t21 t22 in
    submatrix result 0 t1_og.h 0 t2_og.w

let mult_stras min_size t1 t2 = mult_stras_2_power min_size t1 t2

let id_matrix dim =
  init ~h:dim ~w:dim ~f:(fun row col -> if row = col then 1 else 0)

let exp t n cutoff =
  assert (t.w = t.h);
  List.fold (List.range 0 n) ~init:(id_matrix t.w) ~f:(fun prod _i ->
      mult_stras cutoff prod t)
