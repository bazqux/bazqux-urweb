type t
(* сделать [type t a] не выходит -- жалуется на
   Unsupported type constructor FFI(HashMapFfi.t) {...}
 *)

val new : transaction t
val clear : t -> transaction {}
val insert : a ::: Type -> t -> string -> a -> transaction {}
val delete : t -> string -> transaction {}
val lookup : a ::: Type -> t -> string -> transaction (option a)
val lookupDefault : a ::: Type -> t -> a -> string -> transaction a
val toList : a ::: Type -> t -> transaction (list (string * a))
val size : t -> transaction int
