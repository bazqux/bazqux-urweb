ffi setTimeout jsFunc "setTimeoutUr" : string -> transaction {} -> int -> transaction {}
ffi requestAnimationFrameOnce jsFunc "requestAnimationFrameOnceUr" : string -> transaction {} -> transaction {}
ffi requestAnimationFrame jsFunc "requestAnimationFrameUr" : transaction {} -> transaction {}
ffi eq_id jsFunc "w.eq" : Basis.id -> Basis.id -> bool
ffi eq_css_class jsFunc "w.eq" : Basis.css_class -> Basis.css_class -> bool
ffi forceImpure jsFunc "w.execF" : transaction {} -> transaction {}
(* ^ нельзя делать полиморфным, не работает Unsubscribe и прочие глюки появляются, см TODO *)
ffi precompute jsFunc "w.id" : xbody -> transaction xbody
ffi scrollTo : Basis.id -> string -> float -> transaction {} -> transaction {}
ffi fitMessage : Basis.id -> transaction {}
ffi collapseComments : int -> Basis.id -> string -> transaction {} -> transaction {}
ffi expandComments : int -> Basis.id -> string -> transaction {} -> transaction {}
ffi moveUpDown : (float -> float) -> Basis.id -> string -> transaction {} -> transaction {}

ffi offsetBottomRight effectful : Basis.id -> css_class -> { Top : int, Left : int }
ffi offsetBottomLeft effectful : Basis.css_class -> { Top : int, Left : int }
ffi isVisible : Basis.css_class -> transaction bool
ffi scrollTop : Basis.id -> transaction float
ffi scrollHeight : Basis.id -> transaction float
ffi clientHeight : Basis.id -> transaction float
ffi positionTop : Basis.id -> transaction float
ffi boundingClientRectTop : Basis.id -> transaction float
ffi boundingClientRectHeight : Basis.id -> transaction float
con rect = { Top : float, Left : float, Right : float, Bottom : float }
ffi boundingClientRect : Basis.id -> transaction rect
ffi complete : Basis.id -> transaction bool

ffi setScrollTop : Basis.id -> float -> transaction {}
ffi setScrollTopNoOnScroll : Basis.id -> float -> transaction {}

ffi updateBrowserScale : transaction {}
ffi roundScrollTop effectful : float -> float

ffi showId jsFunc "w.id" : Basis.id -> string
ffi show_css_class jsFunc "w.id" : Basis.css_class -> string

ffi showTimeAndAgo : time -> string
ffi showAgo : time -> string
(* ffi showTime' : Datatypes.subscriptionsList -> string *)

ffi activeElementIsEditable : transaction bool
ffi getEventKeyIdentifier : transaction string
ffi mkKeyIdentifier : int -> string
(* ffi toXml : string -> xbody *)
                         (* ^ опасная ф-я *)
ffi backgroundImage : url -> css_style
ffi authorPicImg : option url -> string -> transaction xbody
ffi preloadImages : xbody -> transaction {}
ffi strGt : string -> string -> bool
ffi showTags : string -> string

ffi encodeURIComponent jsFunc "w.encodeURIComponent": string -> string
ffi decodeURIComponent jsFunc "w.decodeURIComponent" : string -> string
ffi getInterfacePath : transaction string
ffi pushInterfacePath : string -> transaction {}
ffi replaceInterfacePath : string -> transaction {}
ffi locationHost : {} -> string
ffi setDocumentTitle : string -> transaction {}
ffi fixChildWidth : Basis.id -> Basis.id -> transaction {}
ffi focus : Basis.id -> transaction {}
ffi blur : Basis.id -> transaction {}
ffi blurActiveElement : transaction {}
ffi select : Basis.id -> transaction {}
ffi setReadOnly : Basis.id -> bool -> transaction {}
(* в web-app openLink открывает в том же окне, использовать только
   в клавиатурных сокращениях -- их нет в iOS *)
ffi openLink : url -> transaction {}
ffi openLinkInBackgroud : url -> transaction {}
ffi noModifiers : transaction bool
ffi reloadPage : transaction {}
ffi opmlForm : xbody -> xbody
ffi opmlUpload : transaction {} -> transaction {}
ffi jsInit : transaction {}
ffi updateButtonsColor : string -> string -> transaction {}
ffi eventTargetClasses : transaction (list string)
ffi eventTargetHasLink : transaction bool
ffi scannedPercentGradientStyle : int -> css_style

ffi setOnSetFeed : (int -> transaction {}) -> transaction {}
ffi setOnToggleFolder : (int -> transaction {}) -> transaction {}
ffi selectSubItem : int -> transaction {}
ffi updateExpandedComments : int -> bool -> transaction {}
ffi retryScanning : int -> transaction {}
ffi setFeedLink : css_class -> int -> xbody -> xbody
ffi adjustMenuPosition : transaction {}
ffi hideContextMenu : transaction {} -> transaction {}
ffi setSmoothHide : bool -> transaction {}

ffi setupSearchAutocomplete : Basis.id -> list string -> transaction {} -> transaction {}
ffi updateSearchAutocomplete : string -> transaction {}
ffi fromFeedIcon : option string -> xbody
ffi setupSubscriptionAutocomplete : string -> Basis.id -> transaction {}

ffi checkLogin : string -> bool
ffi toLowerCase : string -> string
ffi faviconStyle : string -> bool -> string
ffi faviconUrl : string -> bool -> url
ffi prompt : string -> string -> transaction string
ffi checkName : string -> string -> transaction (option string)

ffi setupCountryAutocomplete : Basis.id -> (string -> transaction {}) -> transaction {}
ffi setupTagAutocomplete : Basis.id -> transaction {}
ffi isAutocompleteActive : Basis.id -> transaction bool
ffi getTagsList : Basis.id -> transaction (bool * list string)
ffi getUsedTags : transaction (list string)

ffi clearuims effectful : {} -> bool
ffi getCharCode : transaction int

ffi alwaysFalse : {} -> bool

con readCounters = list (int * int * int * int * int)

ffi updateReadCounters : readCounters -> transaction {}
ffi isWindowActive : transaction bool
ffi registerUpdateSubscriptions : transaction {} -> transaction {}
ffi registerBackgroundRpcFlush : transaction {} -> transaction {}
ffi isDragAndDropActive : transaction bool
ffi setExactUnreadCounts : bool -> transaction {}
ffi setOnSubscriptionRightClick : (int -> transaction {}) -> transaction {}
ffi feedKeyboardAction : int -> string -> transaction {}
ffi cleanDiscoveryQuery : string -> string
ffi countryNameFromCountryCode : string -> string
ffi countryCodeFromCountryName : string -> string
ffi discoveryQueryLooksLikeUrl : string -> bool
ffi hasOnscreenKeyboard effectful : {} -> bool
ffi set_subscribeDiscoveryFeed : (string -> transaction {}) -> transaction {}
ffi set_discoveryHide : transaction {} -> transaction {}
ffi set_popupsHide : transaction {} -> transaction {}
ffi discoveryClearSelection : transaction {}
ffi isLeftPanelVisible : transaction bool
ffi isLeftPanelMovable0 : transaction bool
ffi set_showLeftPanel : (bool -> transaction {}) -> transaction {}
ffi enableMenuTransitions : transaction {}

ffi windowClose : transaction {}
ffi isFullScreen : transaction bool
ffi clearPrevScrollTop : transaction {}
ffi getUrls : int -> transaction readCounters
ffi getUrlsOnly : int -> transaction (list string)
ffi getSubItemGRIds : int -> transaction (list int)
ffi getFeedsCount : transaction int
ffi subItemHasQueryFeeds effectful : int -> list int -> list int -> bool
ffi displayQueryFeeds effectful : list int -> xbody
ffi editQueryFeeds : list int -> transaction { Xml : xbody, SelectAll : transaction {}, SelectNone : transaction {}, GetFeedGRIds : transaction (list int), UpdateFolders : transaction {} }

ffi logTime : string -> transaction bool -> transaction bool

ffi strReplace : string -> string -> string -> string
ffi menuFavicon : url -> xbody

(* ffi unsafeTransaction : a ::: Type -> string -> transaction a -> a *)
(* ffi unsafeTransaction : a ::: Type -> string -> transaction (source a) -> source a *)

ffi textareaReplaceAndSelectText : Basis.id -> string -> string -> transaction {}
ffi trim : string -> string
ffi strftime jsFunc "w.strftime" : string -> time -> string

ffi css : Basis.id -> string -> string -> transaction {}
ffi addClass : string -> css_class -> transaction {}
ffi removeClass : string -> css_class -> transaction {}
ffi saveLoadedImgSizes : Basis.id -> transaction {}
ffi uw_mouseEvent jsFunc "w.uw_mouseEvent" : transaction mouseEvent
ffi isMobile jsFunc "isMobileF" : {} -> bool
ffi isWin jsFunc "isWinF" : {} -> bool
ffi windowInnerHeight : transaction float
ffi viewportWidth : transaction float
ffi viewportHeight : transaction float
ffi windowDevicePixelRatio : transaction float

ffi getUrlUsernameAndPassword : string -> (string * string)
ffi setUrlUsernameAndPassword : (string * string) -> string -> string

ffi titleFromUrl : string -> string

ffi fixFontSize : css_class -> css_class
ffi clickedInPopup : transaction bool
ffi withDummyOnclick : xbody -> xbody
ffi fromDatetimeUtc : int -> int -> int -> int -> int -> int -> time

ffi copyToClipboardEnabled : transaction bool
ffi copyToClipboard : Basis.id -> transaction {}
ffi registerOnPopstate : transaction {} -> transaction {}
ffi setMediaVariable : string -> string -> string -> transaction {}
ffi setMediaVariableTo : string -> string -> string -> string -> transaction {}
ffi setVariable : string -> string -> transaction {}
ffi setVariableTo : string -> string -> string -> transaction {}
ffi getRuleValue jsFunc "cssParser.getRuleValue" : string -> string -> transaction string
ffi getRuleValueF jsFunc "cssParser.getRuleValue" : string -> string -> string
ffi updateBaselineCompensation : css_class -> css_class -> transaction {}
ffi parseInt jsFunc "w.parseInt" : string -> int
ffi setImageProxy : string -> transaction {}
ffi preprocessImg : Binary_ffi.xbodyString -> xbody
ffi fontsLoading : transaction bool
ffi registerOnFontsLoaded : transaction {} -> transaction {}
ffi toFixed : float -> int -> string
ffi dirAuto : string

type shareData = { Title : string, Text : string, Url : url }
ffi canShare : shareData -> bool
ffi share : shareData -> transaction {}
