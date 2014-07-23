ffi setTimeout jsFunc "setTimeoutUr" : string -> transaction {} -> int -> transaction {}
ffi eq_id jsFunc "eq" : Basis.id -> Basis.id -> bool
ffi forceImpure jsFunc "execF" : transaction {} -> transaction {}
(* ^ нельзя делать полиморфным, не работает Unsubscribe и прочие глюки появляются, см TODO *)

ffi scrollToElement : Basis.id -> string -> transaction {} -> transaction {}
ffi fitMessage : Basis.id -> transaction {}
ffi collapseComments : int -> Basis.id -> string -> transaction {} -> transaction {}
ffi expandComments : int -> Basis.id -> string -> transaction {} -> transaction {}
ffi collapseMessage : int -> Basis.id -> transaction {} -> transaction {}
ffi expandMessage : int -> Basis.id -> transaction {} -> transaction {}
ffi moveUpDown : (int -> int) -> Basis.id -> string -> transaction {} -> transaction {}
(* ffi getIntProp : Basis.id -> string -> transaction int *)
(* ffi getBoolProp : Basis.id -> string -> transaction bool *)
(* ffi setIntProp : Basis.id -> string -> int -> transaction unit *)

ffi offsetTopLeft effectful : Basis.id -> { Top : int, Left : int }
ffi offsetBottomRight effectful : Basis.id -> { Top : int, Left : int }
ffi offsetParentScrollTop : Basis.id -> transaction int
ffi setOffsetParentScrollTop : Basis.id -> int -> transaction {}
ffi offsetTop : Basis.id -> transaction int
ffi scrollTop : Basis.id -> transaction int
ffi scrollLeft : Basis.id -> transaction int
ffi scrollHeight : Basis.id -> transaction int
ffi clientHeight : Basis.id -> transaction int
ffi complete : Basis.id -> transaction bool

ffi setScrollTop : Basis.id -> int -> transaction {}
ffi setScrollLeft : Basis.id -> int -> transaction {}

ffi showId : Basis.id -> string

ffi showTime : time -> string
ffi showAgo : time -> string
(* ffi showTime' : Datatypes.subscriptionsList -> string *)

ffi activeElementIsEditable : transaction bool
ffi getEventKeyIdentifier : transaction string
ffi mkKeyIdentifier : int -> string
(* ffi toXml : string -> xbody *)
                         (* ^ опасная ф-я *)
ffi fromXml : xbody -> string
ffi messageImage : string -> xbody -> option xbody
ffi authorPicImg : url -> xbody -> transaction xbody
ffi preprocessMessageText : string -> xbody
ffi preprocessAuthorText : string -> xbody
ffi preprocessSubjectText : string -> xbody
ffi subjectDuplicatesMessage : xbody -> string -> bool
ffi authorIsFoundInSubject : xbody -> xbody -> bool
ffi strGt : string -> string -> bool
ffi showTags : string -> string

ffi encodeURIComponent : string -> string
ffi decodeURIComponent : string -> string
ffi getLocationHash : transaction string
ffi setLocationHash : string -> transaction {}
ffi setDocumentTitle : string -> transaction {}
ffi fixChildWidth : Basis.id -> Basis.id -> transaction {}
ffi setOnload : Basis.id -> transaction {} -> transaction {}
ffi setOninput : Basis.id -> transaction {} -> transaction {}
ffi focus : Basis.id -> transaction {}
ffi blur : Basis.id -> transaction {}
ffi inputValue : Basis.id -> transaction string
ffi select : Basis.id -> transaction {}
ffi setReadOnly : Basis.id -> bool -> transaction {}
(* в web-app openLink открывает в том же окне, использовать только
   в клавиатурных сокращениях -- их нет в iOS *)
ffi openLink : url -> transaction {}
ffi openLinkInBackgroud : url -> transaction {}
ffi stopPropagationLink : css_class -> string -> url -> xbody -> xbody
ffi noModifiers : transaction bool
ffi reloadPage : transaction {}
ffi opmlForm : xbody -> xbody
ffi opmlUpload : transaction {} -> transaction {}
ffi jsInit : transaction {}
ffi loadGoogleAnalytics : bool -> transaction {}
ffi trackEvent : string -> string -> string -> transaction {}
ffi msgOnClick : string -> (string -> mouseEvent -> transaction {}) -> xbody -> xbody
ffi scannedPercentGradientStyle : int -> css_style
ffi commentSubjectNotNeeded : option url -> string -> bool

ffi revAppend : a ::: Type -> (list a * list a) -> list a
ffi reverse : a ::: Type -> list a -> list a
ffi lookupS : a ::: Type -> (string * list (string * a)) -> option a

ffi setOnSetFeed : (int -> transaction {}) -> transaction {}
ffi setOnToggleFolder : (int -> transaction {}) -> transaction {}
ffi selectSubItem : int -> transaction {}
ffi updateExpandedComments : int -> bool -> transaction {}
ffi retryScanning : int -> transaction {}
ffi setFeedLink : int -> xbody -> xbody
ffi adjustMenuHeight : css_class -> transaction {}

ffi setupSearchAutocomplete : Basis.id -> list string -> transaction {} -> transaction {}
ffi updateSearchAutocomplete : string -> transaction {}
ffi fromFolderIcon : option string -> xbody
ffi setupSubscriptionAutocomplete : string -> Basis.id -> transaction {}

ffi strIndexOf : string -> string -> int

ffi passwordHash : string -> transaction string
ffi feverApiKey : string -> string -> transaction string
ffi checkLogin : string -> bool
ffi toLowerCase : string -> string
ffi makePasswordInput : Basis.id -> transaction {}
ffi faviconStyle : string -> string
ffi prompt : string -> string -> transaction string
ffi checkName : string -> string -> transaction (option string)

ffi setupCountryAutocomplete : Basis.id -> (string -> transaction {}) -> transaction {}
ffi setupTagAutocomplete : Basis.id -> transaction {}
ffi isAutocompleteActive : Basis.id -> transaction bool
ffi getTagsList : Basis.id -> transaction (bool * list string)
ffi getUsedTags : transaction (list string)
ffi selTag : string -> transaction {}

ffi identity : a ::: Type -> a -> a
ffi clearuims effectful : {} -> bool
ffi getCharCode : transaction int

ffi alwaysFalse : {} -> bool

con readCounters = list (string * int * int * int * int)

ffi updateAndSaveReadCounters :
    readCounters -> readCounters -> transaction {}
ffi getReadCounters : transaction readCounters
ffi clearChangedReadCountersSet : transaction {}
ffi markChangedReadCounters : string -> transaction {}
ffi registerUpdateSubscriptions : (bool -> transaction {}) -> transaction {}
ffi getChangedReadCounters : transaction (list string)
ffi registerOnDragAndDropStart : transaction {} -> transaction {}
ffi isDragAndDropActive : transaction bool
ffi setExactUnreadCounts : bool -> transaction {}
ffi setOnSubscriptionRightClick : (int -> transaction (option Basis.id)) -> transaction {}
ffi saveToLocalStorage : string -> string -> int -> transaction {}
ffi feedKeyboardAction : int -> string -> transaction {}
ffi cleanDiscoveryQuery : string -> string
ffi countryNameFromCountryCode : string -> string
ffi countryCodeFromCountryName : string -> string
ffi discoveryQueryLooksLikeUrl : string -> bool
ffi hasOnscreenKeyboard effectful : {} -> bool
ffi set_subscribeDiscoveryFeed : (string -> transaction {}) -> transaction {}
ffi set_discoveryHide : transaction {} -> transaction {}
ffi discoveryClearSelection : transaction {}

ffi windowClose : transaction {}
ffi isFullScreen : transaction bool
ffi clearPrevScrollTop : transaction {}
ffi getUrls : int -> transaction readCounters
ffi getUrlsOnly : int -> transaction (list string)
ffi subItemHasQueryFeeds effectful : int -> list (string * bool) -> list string -> bool
ffi displayQueryFeeds effectful : list (string * bool) -> xbody
ffi editQueryFeeds : list (string * bool) -> transaction { Xml : xbody, SelectAll : transaction {}, SelectNone : transaction {}, GetFeeds : transaction (list string), UpdateFolders : transaction {} }
