con putBuf
type xhead = xml head [] []

val mkPutBuf : {} -> putBuf
val putBufString : putBuf -> string
val put_char   : putBuf -> char -> putBuf
val put_int    : putBuf -> int -> putBuf
val put_time   : putBuf -> time -> putBuf
val put_string : putBuf -> string -> putBuf
val put_url : putBuf -> url -> putBuf
val put_xhead : putBuf -> xhead -> putBuf
val put_xbody : putBuf -> xbody -> putBuf
val put_page : putBuf -> page -> putBuf
val put_blob : putBuf -> blob -> putBuf
val put_id   : putBuf -> Basis.id -> putBuf

con getBuf

val mkGetBuf   : string -> getBuf
val advanceGetBuf : getBuf -> int -> getBuf
val get_char_   : getBuf -> char
val get_int_    : getBuf -> int
val get_time_   : getBuf -> time
val get_string_ : getBuf -> string
val get_url_    : getBuf -> url
val get_xhead_  : getBuf -> xhead
val get_xbody_  : getBuf -> xbody
val get_page_   : getBuf -> page
val get_blob_   : getBuf -> blob
val get_id_     : getBuf -> Basis.id
