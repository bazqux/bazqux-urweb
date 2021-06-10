datatype t = X of HashMapFfi.t

fun new () : transaction t = m <- HashMapFfi.new; return (X m)
fun clear ((X m) : t) = HashMapFfi.clear m
fun insert ((X m) : t) (key : string) : transaction {} =
    HashMapFfi.insert m key True
fun delete ((X m) : t) (key : string) : transaction {} =
    HashMapFfi.delete m key
fun member ((X m) : t) (key : string) : transaction bool =
    r <- HashMapFfi.lookup m key;
    return (Option.isSome (r : option bool))
fun size ((X m) : t) = HashMapFfi.size m
