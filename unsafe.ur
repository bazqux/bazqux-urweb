open Datatypes

ffi id jsFunc "w.id" : string -> Basis.id
ffi fromXml jsFunc "w.id" : xbody -> string
ffi toXml jsFunc "w.id" : string -> xbody
ffi fromXbodyString jsFunc "w.id" : Binary_ffi.xbodyString -> string
ffi xbodyStringToXbody jsFunc "w.id" : Binary_ffi.xbodyString -> xbody
ffi toStyle jsFunc "w.id" : string -> css_style

ffi boolTransactionToSignal jsFunc "unsafeTransactionToSignal" : transaction bool -> signal bool

ffi stringSource jsFunc "unsafeSource" : string -> string -> source string
ffi boolSource jsFunc "unsafeSource" : string -> bool -> source bool
ffi optionTimeSource jsFunc "unsafeSource" : string -> option time -> source (option time)
ffi intSource jsFunc "unsafeSource" : string -> int -> source int
ffi floatSource jsFunc "unsafeSource" : string -> float -> source float
ffi xbodySource jsFunc "unsafeSource" : string -> xbody -> source xbody
ffi countersSource jsFunc "unsafeSource" : string -> counters -> source counters
ffi msgTreeViewModeSource jsFunc "unsafeSource" : string -> msgTreeViewMode -> source msgTreeViewMode
ffi optionTransactionSource jsFunc "unsafeSource" : string -> option (transaction {}) -> source (option (transaction {}))
ffi transactionListSource jsFunc "unsafeSource" : string -> list (transaction {}) -> source (list (transaction {}))
ffi bgActionListSource jsFunc "unsafeSource" : string -> list (int * bgAction) -> source (list (int * bgAction))

ffi scrollModeSource jsFunc "unsafeSource" : string -> scrollMode -> source scrollMode
ffi lvmSource jsFunc "unsafeSource" : string -> listViewMode -> source listViewMode
ffi mrmSource jsFunc "unsafeSource" : string -> markReadMode -> source markReadMode

ffi intStorageSource_ jsFunc "unsafeStorageSource"
    : (string -> int) -> (int -> string) -> string -> int -> source int
ffi stringStorageSource_ jsFunc "unsafeStorageSource"
    : (string -> string) -> (string -> string) -> string -> string -> source string
ffi boolStorageSource_ jsFunc "unsafeStorageSource"
    : (string -> bool) -> (bool -> string) -> string -> bool -> source bool
fun intStorageSource n d =
    intStorageSource_
        (fn x => Option.get d (read x))
        (fn x => show x ^ "")
        (* ^ иначе передает js-функцию ts() вместо ur-байткода *)
        n d
val stringStorageSource = stringStorageSource_ (fn x => x) (fn x => x)
val boolStorageSource = boolStorageSource_ (fn x => x = "true") (fn x => if x then "true" else "false")
(* передаем userId *)
ffi initStorageSources : string -> transaction {}

(* signature ENUM = sig *)
(*     class enum *)
(*     val toEnum :   a ::: Type -> enum a -> int -> a *)
(*     val fromEnum : a ::: Type -> enum a -> a -> int *)
(*     val mkEnum : a ::: Type -> (int -> a) -> (a -> int) -> enum a *)
(*     val enum_bool : enum bool *)
(* end *)

(* structure Enum : ENUM = struct *)
(*     con enum a = { To : int -> a, From : a -> int } *)

(*     fun toEnum [a] e x = e.To x *)
(*     fun fromEnum [a] e x = e.From x *)
(*     fun mkEnum [a] t f = { To = t, From = f } *)
(*     val enum_bool = { To = fn i => i <> 0, From = fn b => if b then 1 else 0 } *)
(* end *)

(* open Enum *)

(* ffi polyStorageSource_ jsFunc "unsafeStorageSource" *)
(*     : a ::: Type -> (string -> a) -> (a -> string) -> string -> a -> source a *)

(* fun enumStorageSource [a] [e : enum a] n (d : a) = *)
(*     polyStorageSource_ *)
(*         (fn x => toEnum [e] (Option.get 0 (read x))) *)
(*         (fn x => show (fromEnum [e] x) ^ "") *)
(*         (\* ^ иначе передает js-функцию ts() вместо ur-байткода *\) *)
(*         n (fromEnum [e] d) *)
