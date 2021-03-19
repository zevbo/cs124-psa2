open! Core

type 'a t = {h:int; w:int; m: 'a array array}

let create ~h ~w ~default =
  {h = h; w = w; m = Array.make_matrix ~dimx:w ~dimy:h default} 

exception Unimplmented of string

let check_compatible t1 t2 =
  assert (t1.w = t2.h)
let mult_normal t1 t2 = 
  check_compatible t1 t2;
  raise (Unimplmented "mutl_normal not implemented") 
let mult_stras t1 t2 _min_size = 
  check_compatible t1 t2;
  raise (Unimplmented "mult_stras not implemented") 