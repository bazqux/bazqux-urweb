val init : transaction {}
val readUserSettings_ : string -> transaction string
val cachedReadUserSettings_ : string -> transaction string
val cachedNothingReadUserSettings_ : string -> transaction string
val mergeWriteUserSettings_ : string -> transaction string
val deleteUserSettings_ : string -> transaction string
val readManyUserSettingss_ : string -> transaction string
val cachedReadManyUserSettingss_ : string -> transaction string
val cachedNothingReadManyUserSettingss_ : string -> transaction string
val writeManyUserSettingss_ : string -> transaction string
val readSession_ : string -> transaction string
val cachedReadSession_ : string -> transaction string
val cachedNothingReadSession_ : string -> transaction string
val mergeWriteSession_ : string -> transaction string
val deleteSession_ : string -> transaction string
val readManySessions_ : string -> transaction string
val cachedReadManySessions_ : string -> transaction string
val cachedNothingReadManySessions_ : string -> transaction string
val writeManySessions_ : string -> transaction string
val readMsg_ : string -> transaction string
val cachedReadMsg_ : string -> transaction string
val cachedNothingReadMsg_ : string -> transaction string
val mergeWriteMsg_ : string -> transaction string
val deleteMsg_ : string -> transaction string
val readManyMsgs_ : string -> transaction string
val cachedReadManyMsgs_ : string -> transaction string
val cachedNothingReadManyMsgs_ : string -> transaction string
val writeManyMsgs_ : string -> transaction string
val loginGetForwardUrl_ : string -> string -> string -> string -> transaction string
val loginCallback_ : string -> string -> string -> string -> transaction string
val fbTokenGetForwardUrl_ : string -> string -> transaction string
val fbTokenCallback_ : string -> string -> string -> transaction string
val importFromGoogleReaderGetForwardUrl_ : string -> string -> transaction string
val importFromGoogleReaderCallback_ : string -> string -> string -> string -> transaction string
val importStarredAndTaggedItemsFromGoogleReaderCallback_ : string -> string -> string -> string -> transaction string
val userSubscribe_ : string -> string -> string -> string -> transaction string
val userDiscoverySubscribe_ : string -> string -> string -> string -> string -> string -> transaction string
val userRenameSubscription_ : string -> string -> string -> transaction string
val userRenameFolder_ : string -> string -> string -> transaction string
val userEditSubscriptionFolders_ : string -> string -> string -> string -> transaction string
val userUnsubscribe_ : string -> string -> transaction string
val userRetrySubscription_ : string -> string -> transaction string
val userOPML_ : string -> transaction string
val opmlSubscriptions_ : string -> string -> transaction string
val userSubscriptionsAndRenames_ : string -> string -> string -> string -> string -> string -> transaction string
val userSubscriptionsAndSettings_ : string -> string -> transaction string
val orderNotification_ : string -> transaction string
val checkOrder_ : string -> transaction string
val getPaidTill_ : string -> transaction string
val activeGRImportsCount_ : string -> transaction string
val activeGRImportNames_ : string -> transaction string
val getFeedDetails_ : string -> string -> transaction string
val tagsMsgForest_ : string -> string -> string -> string -> transaction string
val folderMsgForest_ : string -> string -> string -> string -> string -> transaction string
val userGetTree_ : string -> string -> string -> string -> transaction string
val performBgActions_ : string -> string -> transaction string
val searchMsgForest_ : string -> string -> string -> string -> transaction string
val searchTagsMsgForest_ : string -> string -> string -> string -> transaction string
val htmlHead_ : string -> transaction string
val htmlHeadMain_ : string -> transaction string
val htmlHeadMainNoTranslate_ : string -> transaction string
val htmlLikeButtons_ : string -> transaction string
val htmlLandingScripts_ : string -> transaction string
val htmlOpenIdSignInButton_ : string -> transaction string
val htmlConversionLogin_ : string -> transaction string
val version_ : string -> transaction string
val blessId_ : string -> transaction string
val parseQueryStringUtf8Only_ : string -> transaction string
val buyLink_ : string -> string -> transaction string
val encodeURIComponent_ : string -> transaction string
val prettyUID_ : string -> transaction string
val textToXbody_ : string -> transaction string
val newSession_ : string -> string -> transaction string
val clearSession_ : string -> transaction string
val userEvent_ : string -> string -> string -> transaction string
val initMailer_ : string -> transaction string
val initApiServer_ : string -> transaction string
val readFullTextCache_ : string -> transaction string
val cachedReadFullTextCache_ : string -> transaction string
val cachedNothingReadFullTextCache_ : string -> transaction string
val mergeWriteFullTextCache_ : string -> transaction string
val deleteFullTextCache_ : string -> transaction string
val readManyFullTextCaches_ : string -> transaction string
val cachedReadManyFullTextCaches_ : string -> transaction string
val cachedNothingReadManyFullTextCaches_ : string -> transaction string
val writeManyFullTextCaches_ : string -> transaction string
val getFullText_ : string -> transaction string
val getUrTime__ : string -> transaction string
val setMobileLogin_ : string -> string -> string -> string -> transaction string
val tryGetFeverUser_ : string -> transaction string
val userEnablePublicFeed_ : string -> string -> transaction string
val userDisablePublicFeed_ : string -> string -> transaction string
val userGenerateNewPublicFeed_ : string -> string -> transaction string
val searchSubscriptions_ : string -> string -> string -> transaction string
