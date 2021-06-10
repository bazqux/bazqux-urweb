(* functor Make(M : sig *)
(*                  con a *)
(*              end) = struct *)

    datatype t a = X of HashMapFfi.t

    fun new [a] () : transaction (t a) = m <- HashMapFfi.new; return (X m)
    fun clear [a] ((X m) : t a) = HashMapFfi.clear m
    fun insert [a] ((X m) : t a) (key : string) (x : a) : transaction {} =
        HashMapFfi.insert m key x
    fun delete [a] ((X m) : t a) (key : string) : transaction {} =
        HashMapFfi.delete m key
    fun lookup [a] ((X m) : t a) (key : string) : transaction (option a) =
        HashMapFfi.lookup m key
    fun lookupDefault [a] ((X m) : t a) (d : a) (key : string) : transaction a =
        HashMapFfi.lookupDefault m d key
    fun toList [a] ((X m) : t a) : transaction (list (string * a)) =
        HashMapFfi.toList m
    fun size [a] ((X m) : t a) = HashMapFfi.size m
(* end *)
