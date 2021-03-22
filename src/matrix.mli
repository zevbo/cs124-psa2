type 'a t = {h:int; w:int; m: 'a array array}

val create : h:int -> w:int -> default:'a -> 'a t
val mult_normal : 'a t -> 'a t -> int -> 'a t
val mult_stras : 'a t -> 'a t -> int -> 'a t