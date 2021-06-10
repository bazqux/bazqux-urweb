val readUserSettings : string -> transaction (option Datatypes.userSettings)

val cachedReadUserSettings : string -> transaction (option Datatypes.userSettings)

val cachedNothingReadUserSettings : string -> transaction (option Datatypes.userSettings)

val mergeWriteUserSettings : Datatypes.userSettings -> transaction ({})

val deleteUserSettings : Datatypes.userSettings -> transaction ({})

val readManyUserSettingss : list string -> transaction (list (option Datatypes.userSettings))

val cachedReadManyUserSettingss : list string -> transaction (list (option Datatypes.userSettings))

val cachedNothingReadManyUserSettingss : list string -> transaction (list (option Datatypes.userSettings))

val writeManyUserSettingss : list Datatypes.userSettings -> transaction ({})

val readSession : string -> transaction (option Datatypes.session)

val cachedReadSession : string -> transaction (option Datatypes.session)

val cachedNothingReadSession : string -> transaction (option Datatypes.session)

val mergeWriteSession : Datatypes.session -> transaction ({})

val deleteSession : Datatypes.session -> transaction ({})

val readManySessions : list string -> transaction (list (option Datatypes.session))

val cachedReadManySessions : list string -> transaction (list (option Datatypes.session))

val cachedNothingReadManySessions : list string -> transaction (list (option Datatypes.session))

val writeManySessions : list Datatypes.session -> transaction ({})

val readMsg : Datatypes.msgKey -> transaction (option Datatypes.msg)

val cachedReadMsg : Datatypes.msgKey -> transaction (option Datatypes.msg)

val cachedNothingReadMsg : Datatypes.msgKey -> transaction (option Datatypes.msg)

val mergeWriteMsg : Datatypes.msg -> transaction ({})

val deleteMsg : Datatypes.msg -> transaction ({})

val readManyMsgs : list Datatypes.msgKey -> transaction (list (option Datatypes.msg))

val cachedReadManyMsgs : list Datatypes.msgKey -> transaction (list (option Datatypes.msg))

val cachedNothingReadManyMsgs : list Datatypes.msgKey -> transaction (list (option Datatypes.msg))

val writeManyMsgs : list Datatypes.msg -> transaction ({})

val loginGetForwardUrl : Datatypes.externalLoginType -> string -> Datatypes.externalLoginAction -> url -> transaction (url)

val loginCallback : Datatypes.externalLoginType -> string -> url -> string -> transaction ((Datatypes.loginType * Datatypes.loginAccessToken * Datatypes.externalLoginAction * option string))

val userSubscribe : string -> string -> option string -> list string -> transaction (string)

val userDiscoverySubscribe : string -> string -> string -> string -> option string -> list string -> transaction (string)

val userRenameSubscription : string -> string -> string -> transaction ({})

val userRenameFolder : string -> string -> string -> transaction (string)

val userEditSubscriptionFolders : string -> string -> string -> bool -> transaction ({})

val userUnsubscribe : string -> list string -> transaction ({})

val userRetrySubscription : string -> string -> transaction ({})

val deleteFilter : string -> int -> transaction ({})

val deleteSmartStream : string -> string -> transaction ({})

val checkQuerySyntax : string -> transaction (option string)

val addFilter : string -> string -> bool -> list int -> transaction ({})

val editFilter : string -> int -> string -> bool -> list int -> transaction ({})

val addSmartStream : string -> string -> string -> list int -> transaction ({})

val editSmartStream : string -> string -> string -> list int -> transaction ({})

val userOPML : bool -> string -> transaction (string)

val opmlSubscriptions : blob -> string -> transaction ({})

val subscriptionsAndRenames : string -> bool -> Datatypes.updateFilters -> time -> string -> string -> string -> string -> transaction ((option (Binary_ffi.xbodyString * string * list string) * string * list Datatypes.subItemRpc * option ((list (int * Datatypes.filterQueryRpc) * list (string * Datatypes.filterQueryRpc)) * string) * list (time * string * string)))

val subscriptionsAndSettings : string -> bool -> bool -> string -> transaction (((option (Binary_ffi.xbodyString * string * list string) * string * list Datatypes.subItemRpc * option ((list (int * Datatypes.filterQueryRpc) * list (string * Datatypes.filterQueryRpc)) * string) * list (time * string * string)) * (list string * bool * Datatypes.userSettings) * (Datatypes.welcomeState * list (time * string * string) * int * int)))

val orderNotification : string -> transaction ((string * Datatypes.payment))

val orderNotificationNew : string -> string -> transaction ({})

val checkOrder : string -> transaction ((string * Datatypes.payment))

val getPaidTill : string -> transaction (Datatypes.paidTill)

val getUserAccountTypeAndRenewalUrl : string -> transaction ((string * string))

val getFeedDetails : string -> string -> string -> transaction ((string * option string * option string * Datatypes.msgTreeViewMode))

val performBgActions : string -> list Datatypes.bgAction -> transaction (string)

val tagsForest : Datatypes.apiMode -> string -> option (list Datatypes.itemTag) -> Datatypes.msgTreeViewMode -> transaction ((Datatypes.markReq * list (int * int * int * int * int) * Datatypes.msgForest))

val folderForest : string -> option string -> Datatypes.feedsOrDiscovery -> list Datatypes.postsReq -> Datatypes.msgTreeViewMode -> string -> string -> transaction ((Datatypes.markReq * list (int * int * int * int * int) * Datatypes.msgForest))

val getTree : Datatypes.apiMode -> string -> Datatypes.msgTreeViewMode -> list Datatypes.treeReq -> transaction (list (option Datatypes.msgForest))

val filterForest : string -> option string -> string -> option string -> Datatypes.feedsOrDiscovery -> Datatypes.msgTreeViewMode -> string -> string -> transaction (Either.either string (Datatypes.markReq * list (int * int * int * int * int) * Datatypes.filterResults))

val filterTagsForest : string -> string -> option (list Datatypes.itemTag) -> Datatypes.msgTreeViewMode -> string -> string -> transaction (Either.either string (Datatypes.markReq * list (int * int * int * int * int) * Datatypes.filterResults))

val smartStreamForest : Datatypes.apiMode -> string -> string -> list (int * int * int * int * int) -> Datatypes.msgTreeViewMode -> transaction ((Datatypes.markReq * list (int * int * int * int * int) * Datatypes.msgForest))

val markReqReadCounters : string -> Datatypes.msgTreeViewMode -> Datatypes.markReq -> list Datatypes.msgId -> transaction (list (int * int * int * int * int))

val pageFromFile : string -> transaction (page)

val addWebpackScripts : string -> transaction (string)

val webpackStyles : transaction (xhead)

val blessId : string ->  (Basis.id)

val parseQueryStringUtf8Only : string ->  (list (string * string))

val userEmail : string -> transaction (option Datatypes.emailAddress)

val buyPage : string -> string -> option Datatypes.emailAddress -> transaction (page)

val invoiceLink : string ->  (string)

val prettyUID : string -> transaction (string)

val xbodyStringToString : Binary_ffi.xbodyString ->  (string)

val xbodyStringToXbody : Binary_ffi.xbodyString ->  (xbody)

val escapeXbody : xbody ->  (xbody)

val hyphenatePage : page ->  (page)

val hyphenateXbody : xbody ->  (xbody)

val toLowerCase : string ->  (string)

val addTwitterScreenName : string -> string -> string -> transaction ({})

val newSessionJunk : Datatypes.loginType -> Datatypes.loginAccessToken -> list (string * string) -> transaction (Datatypes.session)

val getUserByLogin : Datatypes.loginType -> option string -> transaction (option string)

val clearSession : string -> string -> transaction ({})

val userEvent : string -> string -> string -> transaction ({})

val runTasks : transaction ({})

val runApiServer : transaction ({})

val reloadBrowserPage : transaction ({})

val logOutAllSessions : string -> list string -> transaction ({})

val getFullText : string -> bool -> string -> string -> Datatypes.msgKey -> transaction (Either.either string Binary_ffi.xbodyString)

val getUrTime : transaction (time)

val setUsername : option string -> string -> string -> transaction (bool)

val setPassword : option string -> string -> string -> transaction ({})

val tryRemoveAssociatedAccount : option string -> string -> Datatypes.loginType -> transaction (bool)

val tryAddAssociatedAccount : option string -> string -> Datatypes.loginType -> transaction (bool)

val tryGetFeverUser : string -> transaction (option string)

val enablePublicFeed : Datatypes.publicFeedType -> string -> transaction (list (string * bool * option string))

val disablePublicFeed : Datatypes.publicFeedType -> string -> transaction (list (string * bool * option string))

val generateNewPublicFeed : Datatypes.publicFeedType -> string -> transaction (list (string * bool * option string))

val discover : string -> string -> string -> string -> transaction (option (xbody * list (string * Datatypes.msgTreeViewMode)))

val restoreSubscriptionsFromBackup : string -> transaction ({})

val isUserExists : string -> transaction (bool)

val deleteAccount : bool -> string -> transaction ({})

val recordWebUsage : string -> option string -> transaction ({})

val readMsgAndApplyFixes : string -> string -> Datatypes.msgKey -> transaction (option Datatypes.msg)

val parseRenewalUserId : string -> transaction (string)

val passwordResetEmail : string -> transaction (Either.either string (string * string))

val sendSignUpEmail : string -> string -> string -> transaction (bool)

val sendPasswordResetEmail : string -> string -> string -> transaction (bool)

val sendChangeEmailEmail : string -> string -> string -> transaction (bool)

val verifySignUpToken : string -> transaction (option string)

val verifyPasswordResetToken : string -> string -> string -> transaction (option string)

val verifyChangeEmailToken : string -> string -> transaction (option string)

val verifyRestoreAccessToken : string -> string -> string -> transaction (option string)

val validateEmail : string ->  (option string)

val maskEmail : string ->  (string)

val userAddToPocket : string -> string -> url -> string -> string -> transaction (Datatypes.okErrorRedirect)

val userAuthorizeAndAddToPocket : string -> transaction ({})

val logT : string -> transaction ({})

val findUsersLeft : int -> transaction ((list Datatypes.feedbackUserInfo * list Datatypes.feedbackUserInfo))

val updateFeedbackUserInfo : Datatypes.feedbackUserInfo -> transaction ({})

