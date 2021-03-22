type 'a t = {h:int; w:int; m: 'a array array}

val create : h:int -> w:int -> default:'a -> 'a t
val init : h:int -> w:int -> f:(int -> int -> 'a) -> 'a t
val mult_normal : int t -> int t -> int t
val mult_stras : int t -> int t -> int -> int t

val set : 'a t -> int -> int -> 'a -> unit

val print : int t -> unit