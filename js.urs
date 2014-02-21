val scrollToElement : Basis.id -> string -> transaction {} -> transaction {}
val fitMessage : Basis.id -> transaction {}
val collapseComments : int -> Basis.id -> string -> transaction {} -> transaction {}
val expandComments : int -> Basis.id -> string -> transaction {} -> transaction {}
val collapseMessage : int -> Basis.id -> transaction {} -> transaction {}
val expandMessage : int -> Basis.id -> transaction {} -> transaction {}
val moveUpDown : (int -> int) -> Basis.id -> string -> transaction {} -> transaction {}
(* val getIntProp : Basis.id -> string -> transaction int *)
(* val getBoolProp : Basis.id -> string -> transaction bool *)
(* val setIntProp : Basis.id -> string -> int -> transaction unit *)

val offsetTopLeft : Basis.id -> { Top : int, Left : int }
val offsetBottomRight : Basis.id -> { Top : int, Left : int }
val offsetParentScrollTop : Basis.id -> transaction int
val setOffsetParentScrollTop : Basis.id -> int -> transaction {}
val offsetTop : Basis.id -> transaction int
val scrollTop : Basis.id -> transaction int
val scrollLeft : Basis.id -> transaction int
val scrollHeight : Basis.id -> transaction int
val clientHeight : Basis.id -> transaction int
val complete : Basis.id -> transaction bool

val setScrollTop : Basis.id -> int -> transaction {}
val setScrollLeft : Basis.id -> int -> transaction {}

val showId : Basis.id -> string
val forceImpure : transaction {} -> transaction {}
(* ^ нельзя делать полиморфным, не работает Unsubscribe и прочие глюки появляются, см TODO *)

val showTime : time -> string
val showAgo : time -> string
(* val showTime' : Datatypes.subscriptionsList -> string *)

val activeElementIsEditable : transaction bool
val getEventKeyIdentifier : transaction string
val mkKeyIdentifier : int -> string
(* val toXml : string -> xbody *)
                         (* ^ опасная ф-я *)
val fromXml : xbody -> string
val messageImage : string -> xbody -> option xbody
val authorPicImg : url -> xbody -> transaction xbody
val preprocessMessageText : string -> xbody
val preprocessAuthorText : string -> xbody
val preprocessSubjectText : string -> xbody
val subjectDuplicatesMessage : xbody -> string -> bool
val authorIsFoundInSubject : xbody -> xbody -> bool
val strGt : string -> string -> bool
val showTags : string -> string

val encodeURIComponent : string -> string
val decodeURIComponent : string -> string
val getLocationHash : transaction string
val setLocationHash : string -> transaction {}
val setDocumentTitle : string -> transaction {}
val fixChildWidth : Basis.id -> Basis.id -> transaction {}
val setTimeout : string -> transaction {} -> int -> transaction {}
val setOnload : Basis.id -> transaction {} -> transaction {}
val setOninput : Basis.id -> transaction {} -> transaction {}
val eq_id : Basis.id -> Basis.id -> bool
val focus : Basis.id -> transaction {}
val blur : Basis.id -> transaction {}
val inputValue : Basis.id -> transaction string
val select : Basis.id -> transaction {}
val setReadOnly : Basis.id -> bool -> transaction {}
(* в web-app openLink открывает в том же окне, использовать только
   в клавиатурных сокращениях -- их нет в iOS *)
val openLink : url -> transaction {}
val openLinkInBackgroud : url -> transaction {}
val stopPropagationLink : css_class -> string -> url -> xbody -> xbody
val noModifiers : transaction bool
val reloadPage : transaction {}
val opmlForm : xbody -> xbody
val opmlUpload : transaction {} -> transaction {}
val jsInit : transaction {}
val loadGoogleAnalytics : bool -> transaction {}
val trackEvent : string -> string -> string -> transaction {}
val msgOnClick : string -> (string -> mouseEvent -> transaction {}) -> xbody -> xbody
val scannedPercentGradientStyle : int -> css_style
con counters
  = { ReadPosts : int
    , ReadComments : int
    , TotalPosts : int
    , TotalComments : int
    , Scanning : int
    , ScanningComments : int
    , Error : int
    , Feed : int
    , ScannedPercent : int
    }
val showUnread : counters -> bool -> string
val groupSubInfoListByFolder : a ::: Type -> list a -> (list (string * list a) * list a)
(* a = (string * source subInfo) *)
val commentSubjectNotNeeded : option url -> string -> bool

val revAppend : a ::: Type -> (list a * list a) -> list a
val reverse : a ::: Type -> list a -> list a
val lookupS : a ::: Type -> (string * list (string * a)) -> option a

val setSubItems : a ::: Type -> (bool * list a) -> bool
val setOnSetFeed : (int -> transaction {}) -> transaction {}
val setOnToggleFolder : (int -> transaction {}) -> transaction {}
val selectSubItem : int -> transaction {}
val getSubItem : a ::: Type -> int -> a
val getSubItems : a ::: Type -> int -> list a
val getSubItemByUrl : a ::: Type -> string -> option a
val getSubItemByHash : a ::: Type -> string -> option a
val updateCounters_ : a ::: Type -> (a * int * int) -> transaction bool
val updateExpandedComments : int -> bool -> transaction {}
val retryScanning : int -> transaction {}
val hideSubItems : a ::: Type -> list a -> bool
val setFeedLink : int -> xbody -> xbody
val adjustMenuHeight : css_class -> transaction {}

val attachmentXml : a ::: Type -> a -> xbody
val setupSearchAutocomplete : Basis.id -> list string -> transaction {} -> transaction {}
val updateSearchAutocomplete : string -> transaction {}
val fromFolderIcon : option string -> xbody
val setupSubscriptionAutocomplete : string -> Basis.id -> transaction {}

val strIndexOf : string -> string -> int

val passwordHash : string -> transaction string
val feverApiKey : string -> string -> transaction string
val checkLogin : string -> bool
val toLowerCase : string -> string
val makePasswordInput : Basis.id -> transaction {}
val faviconStyle : string -> string
val prompt : string -> string -> transaction string
val checkTagName : string -> transaction (option string)

val setupCountryAutocomplete : Basis.id -> (string -> transaction {}) -> transaction {}
val setupTagAutocomplete : Basis.id -> transaction {}
val isAutocompleteActive : Basis.id -> transaction bool
val getTagsList : Basis.id -> transaction (bool * list string)
val getUsedTags : transaction (list string)
val selTag : string -> transaction {}

val identity : a ::: Type -> a -> a
val mw : a ::: Type -> {} -> a
val setmw : a ::: Type -> a -> bool
val uim : a ::: Type -> Basis.id -> a
val setuim : a ::: Type -> Basis.id -> a -> bool
val clearuims : {} -> bool
val getCharCode : transaction int

val alwaysFalse : {} -> bool

con readCounters = list (string * int * int * int * int)

val updateAndSaveReadCounters :
    readCounters -> readCounters -> transaction {}
val getReadCounters : transaction readCounters
val clearChangedReadCountersSet : transaction {}
val markChangedReadCounters : string -> transaction {}
val registerUpdateSubscriptions : (bool -> transaction {}) -> transaction {}
val getChangedReadCounters : transaction (list string)
val registerOnDragAndDrop : a ::: Type -> (a -> transaction {}) -> transaction {}
val registerOnDragAndDropStart : transaction {} -> transaction {}
val isDragAndDropActive : transaction bool
val setExactUnreadCounts : bool -> transaction {}
val setOnSubscriptionRightClick : (int -> transaction (option Basis.id)) -> transaction {}
val getFromLocalStorage : string -> string -> int -> transaction int
val saveToLocalStorage : string -> string -> int -> transaction {}
val feedKeyboardAction : int -> string -> transaction {}
val cleanDiscoveryQuery : string -> string
val countryNameFromCountryCode : string -> string
val countryCodeFromCountryName : string -> string
val discoveryQueryLooksLikeUrl : string -> bool
val hasOnscreenKeyboard : {} -> bool
val set_setDiscoveryFeed : a ::: Type -> (string -> string -> option string -> option string -> option a -> transaction {}) -> transaction {}
val set_subscribeDiscoveryFeed : (string -> transaction {}) -> transaction {}
val set_discoveryHide : transaction {} -> transaction {}
val discoveryClearSelection : transaction {}

val windowClose : transaction {}
