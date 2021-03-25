
type pos = { row : int; col : int }

type 'a t = { h : int; w : int; m : 'a array array; tl : pos }
val create : h:int -> w:int -> default:'a -> 'a t
val init : h:int -> w:int -> f:(int -> int -> 'a) -> 'a t
val mult_normal : int t -> int t -> int t
val mult_stras : int -> int t -> int t -> int t
val exp : int t -> int -> int -> int t

val equal : int t -> int t -> bool

val get : 'a t -> int -> int -> 'a
val set : 'a t -> int -> int -> 'a -> unit

val print : int t -> unit