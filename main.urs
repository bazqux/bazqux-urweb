val whoami : transaction page
val main : {} -> transaction page
val demo : string -> transaction page
val opml : {} -> transaction page
val importOPML_ : {} -> transaction page
val facebookToken : transaction page
val order_notification : postBody -> transaction page
val order_completed : string -> transaction page
val check_order : string -> transaction page
val add : option queryString -> transaction page
val activeImports : transaction page
val fetcher : {} -> transaction page
val clearSubscriptions : transaction page
val getUserIdBySession : transaction page
