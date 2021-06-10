val whoami : transaction page
val r : string -> transaction page
val login : {} -> transaction page
val signUp : {} -> transaction page
val please_log_in_again : transaction page
val opml : transaction page
val order_completed : string -> transaction page
val check_order : string -> transaction page
val add : option queryString -> transaction page
val renew : option queryString -> transaction page
val fetcher : transaction page
val apps : transaction page
val search_hints : transaction page
val how_to_import_my_feeds : transaction page
val media_proxy : transaction page
val clearSubscriptions : transaction page
val deleteAccount : transaction page

val activate_account : string -> transaction page
val password_reset : string -> transaction page
val change_email : string -> transaction page
val restore_access : string -> transaction page

val logo : xbody
val htmlHeadMain : xhead
val externalLoginButtons : Datatypes.externalLoginAction -> xbody
val helpText : (string -> transaction {}) -> xbody
val buyText : string -> source Datatypes.paidTill -> transaction xbody
val loginUI : { Login : string, Password : string } -> option string -> bool -> Datatypes.externalLoginAction -> transaction page
val infoPage' : css_class -> string -> xhead -> xbody -> transaction page
val infoPage : string -> xbody -> transaction page
val errorPage : xbody -> transaction page
val addToPocket : string -> string -> transaction Datatypes.okErrorRedirect

val switch_to_user : transaction page
