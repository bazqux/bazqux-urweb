open Binary_ffi

datatype either a b = Left of a | Right of b

type assoc_list a b = list (a * b)

type put a = putBuf -> a -> putBuf
type get a = getBuf -> (getBuf * a)

fun get_string b = (advanceGetBuf b (get_int_ b + 8), get_string_ b)
fun get_xhead  b = (advanceGetBuf b (get_int_ b + 8), get_xhead_ b)
fun get_xbody  b = (advanceGetBuf b (get_int_ b + 8), get_xbody_ b)
fun get_page   b = (advanceGetBuf b (get_int_ b + 8), get_page_ b)
fun get_id     b = (advanceGetBuf b (get_int_ b + 8), get_id_ b)
fun get_url    b = (advanceGetBuf b (get_int_ b + 8), get_url_ b)
fun get_blob   b = (advanceGetBuf b (get_int_ b + 8), get_blob_ b)
fun get_int    b = (advanceGetBuf b               8 , get_int_ b)
fun get_char   b = (advanceGetBuf b               1 , get_char_ b)
fun get_bool   b = (advanceGetBuf b               1 , get_char_ b <> chr 0)
fun get_time   b = (advanceGetBuf b              16 , get_time_ b)
fun get_unit   b = (b, ())

fun put_bool b x = put_char b (if x then chr 1 else chr 0)
fun put_unit b () = b

fun get_list [a] (g : get a) b : (getBuf * list a) =
    let val (b', len) = get_int b
        fun getL n acc b =
            if n <= 0 then
                (b, List.rev acc)
            else
                let val (b', elt) = g b
                in
                    getL (n-1) (elt :: acc) b'
                end
    in
        getL len [] b'
    end

fun put_list [a] (p : put a) b l : putBuf =
    let val b' = put_int b (List.length l)
        fun pl l b =
            case l of
                [] => b
              | x::xs => pl xs (p b x)
    in
        pl l b'
    end

fun put_option [a] (p : put a) b o : putBuf =
    case o of
        None   => put_char b (chr 0)
      | Some s => p (put_char b (chr 1)) s

fun get_option [a] (g : get a) b : (getBuf * option a) =
    let val (b', c) = get_char b
    in
        if c = chr 0 then (b', None)
        else
            let val (b'', s) = g b'
            in (b'', Some s) end
    end

fun put_either [a] [b] (pa : put a) (pb : put b) b e : putBuf =
    case e of
        Left l  => pa (put_char b (chr 0)) l
      | Right r => pb (put_char b (chr 1)) r

fun get_either [a] [b] (ga : get a) (gb : get b) b : (getBuf * either a b) =
    let val (b', c) = get_char b
    in
        if c = chr 0 then
            let val (b'', s) = ga b'
            in (b'', Left s) end
        else
            let val (b'', s) = gb b'
            in (b'', Right s) end
    end

signature BINARY = sig
    class binary
    val toHaskell :   a ::: Type -> binary a -> a -> string
    val fromHaskell : a ::: Type -> binary a -> string -> a
    val mkBinary : a ::: Type -> put a -> get a -> binary a
    val binary_unit : binary {}
    val binary_int : binary int
    val binary_bool : binary bool
    val binary_char : binary char
    val binary_time : binary time
    val binary_string : binary string
    val binary_url : binary url
    val binary_xhead : binary xhead
    val binary_xbody : binary xbody
    val binary_page : binary page
    val binary_id : binary Basis.id
    val binary_blob : binary blob
    val binary_either : a ::: Type -> b ::: Type -> binary a -> binary b -> binary (either a b)
    val binary_tuple : a ::: Type -> b ::: Type -> binary a -> binary b -> binary (a * b)
    val binary_tuple3 : a ::: Type -> b ::: Type -> c ::: Type -> binary a -> binary b -> binary c -> binary (a * b * c)
    val binary_tuple4 : a ::: Type -> b ::: Type -> c ::: Type -> d ::: Type -> binary a -> binary b -> binary c -> binary d -> binary (a * b * c * d)
    val binary_tuple5 : a ::: Type -> b ::: Type -> c ::: Type -> d ::: Type -> e ::: Type -> binary a -> binary b -> binary c -> binary d -> binary e -> binary (a * b * c * d * e)
    val binary_list : a ::: Type -> binary a -> binary (list a)
    val binary_option : a ::: Type -> binary a -> binary (option a)
end

structure Binary : BINARY = struct
    con binary a = { Put : put a, Get : get a }

    fun toHaskell [a] p x = putBufString (p.Put (mkPutBuf ()) x)
    fun fromHaskell [a] p s = (p.Get (mkGetBuf s)).2
    fun mkBinary [a] p g = { Put = p, Get = g }
    val binary_int : binary int = { Put = put_int, Get = get_int }
    val binary_bool : binary bool = { Put = put_bool, Get = get_bool }
    val binary_char : binary char = { Put = put_char, Get = get_char }
    val binary_time : binary time = { Put = put_time, Get = get_time }
    val binary_unit : binary {} = { Put = put_unit, Get = get_unit }
    val binary_string : binary string = { Put = put_string, Get = get_string }
    val binary_url : binary url = { Put = put_url, Get = get_url }
    val binary_xhead : binary xhead = { Put = put_xhead, Get = get_xhead }
    val binary_xbody : binary xbody = { Put = put_xbody, Get = get_xbody }
    val binary_page : binary page = { Put = put_page, Get = get_page }
    val binary_blob : binary blob = { Put = put_blob, Get = get_blob }
    val binary_id : binary Basis.id = { Put = put_id, Get = get_id }
    val binary_either [a] [b] (ba : binary a) (bb : binary b) : binary (either a b) =
        { Put = fn p e => put_either ba.Put bb.Put p e
        , Get = fn g => get_either ba.Get bb.Get g
        }
    val binary_tuple [a] [b] (ba : binary a) (bb : binary b) : binary (a*b) =
        { Put = fn p (a,b) => bb.Put (ba.Put p a) b
        , Get = fn g =>
                   let val (g, a) = ba.Get g
                       val (g, b) = bb.Get g
                   in (g, (a, b)) end }
    val binary_tuple3 [a] [b] [c] (ba : binary a) (bb : binary b) (bc : binary c) : binary (a*b*c) =
        { Put = fn p (a,b,c) => bc.Put (bb.Put (ba.Put p a) b) c
        , Get = fn g =>
                   let val (g, a) = ba.Get g
                       val (g, b) = bb.Get g
                       val (g, c) = bc.Get g
                   in (g, (a, b, c)) end }
    val binary_tuple4 [a] [b] [c] [d] (ba : binary a) (bb : binary b) (bc : binary c) (bd : binary d) : binary (a*b*c*d) =
        { Put = fn p (a,b,c,d) => bd.Put (bc.Put (bb.Put (ba.Put p a) b) c) d
        , Get = fn g =>
                   let val (g, a) = ba.Get g
                       val (g, b) = bb.Get g
                       val (g, c) = bc.Get g
                       val (g, d) = bd.Get g
                   in (g, (a, b, c, d)) end }
    val binary_tuple5 [a] [b] [c] [d] [e] (ba : binary a) (bb : binary b) (bc : binary c) (bd : binary d) (be : binary e) : binary (a*b*c*d*e) =
        { Put = fn p (a,b,c,d,e) => be.Put (bd.Put (bc.Put (bb.Put (ba.Put p a) b) c) d) e
        , Get = fn g =>
                   let val (g, a) = ba.Get g
                       val (g, b) = bb.Get g
                       val (g, c) = bc.Get g
                       val (g, d) = bd.Get g
                       val (g, e) = be.Get g
                   in (g, (a, b, c, d, e)) end }
    val binary_list [a] (b : binary a) : binary (list a) =
        { Put = put_list b.Put, Get = get_list b.Get }
    val binary_option [a] (b : binary a) : binary (option a) =
        { Put = put_option b.Put, Get = get_option b.Get }
end

open Binary

(* ^ а почему тут не видит??? *)
