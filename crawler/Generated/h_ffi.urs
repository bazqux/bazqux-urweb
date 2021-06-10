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
val userSubscribe_ : string -> string -> string -> string -> transaction string
val userDiscoverySubscribe_ : string -> string -> string -> string -> string -> string -> transaction string
val userRenameSubscription_ : string -> string -> string -> transaction string
val userRenameFolder_ : string -> string -> string -> transaction string
val userEditSubscriptionFolders_ : string -> string -> string -> string -> transaction string
val userUnsubscribe_ : string -> string -> transaction string
val userRetrySubscription_ : string -> string -> transaction string
val deleteFilter_ : string -> string -> transaction string
val deleteSmartStream_ : string -> string -> transaction string
val checkQuerySyntax_ : string -> transaction string
val addFilter_ : string -> string -> string -> string -> transaction string
val editFilter_ : string -> string -> string -> string -> string -> transaction string
val addSmartStream_ : string -> string -> string -> string -> transaction string
val editSmartStream_ : string -> string -> string -> string -> transaction string
val userOPML_ : string -> string -> transaction string
val opmlSubscriptions_ : string -> string -> transaction string
val subscriptionsAndRenames_ : string -> string -> string -> string -> string -> string -> string -> string -> transaction string
val subscriptionsAndSettings_ : string -> string -> string -> string -> transaction string
val orderNotification_ : string -> transaction string
val orderNotificationNew_ : string -> string -> transaction string
val checkOrder_ : string -> transaction string
val getPaidTill_ : string -> transaction string
val getUserAccountTypeAndRenewalUrl_ : string -> transaction string
val getFeedDetails_ : string -> string -> string -> transaction string
val performBgActions_ : string -> string -> transaction string
val tagsForest_ : string -> string -> string -> string -> transaction string
val folderForest_ : string -> string -> string -> string -> string -> string -> string -> transaction string
val getTree_ : string -> string -> string -> string -> transaction string
val filterForest_ : string -> string -> string -> string -> string -> string -> string -> string -> transaction string
val filterTagsForest_ : string -> string -> string -> string -> string -> string -> transaction string
val smartStreamForest_ : string -> string -> string -> string -> string -> transaction string
val markReqReadCounters_ : string -> string -> string -> string -> transaction string
val pageFromFile_ : string -> transaction string
val addWebpackScripts_ : string -> transaction string
val webpackStyles_ : transaction string
val blessId_ : string -> string
val parseQueryStringUtf8Only_ : string -> string
val userEmail_ : string -> transaction string
val buyPage_ : string -> string -> string -> transaction string
val invoiceLink_ : string -> string
val prettyUID_ : string -> transaction string
val xbodyStringToString_ : string -> string
val xbodyStringToXbody_ : string -> string
val escapeXbody_ : string -> string
val hyphenatePage_ : string -> string
val hyphenateXbody_ : string -> string
val toLowerCase_ : string -> string
val addTwitterScreenName_ : string -> string -> string -> transaction string
val newSessionJunk_ : string -> string -> string -> transaction string
val getUserByLogin_ : string -> string -> transaction string
val clearSession_ : string -> string -> transaction string
val userEvent_ : string -> string -> string -> transaction string
val runTasks_ : transaction string
val runApiServer_ : transaction string
val reloadBrowserPage_ : transaction string
val logOutAllSessions_ : string -> string -> transaction string
val getFullText_ : string -> string -> string -> string -> string -> transaction string
val getUrTime_ : transaction string
val setUsername_ : string -> string -> string -> transaction string
val setPassword_ : string -> string -> string -> transaction string
val tryRemoveAssociatedAccount_ : string -> string -> string -> transaction string
val tryAddAssociatedAccount_ : string -> string -> string -> transaction string
val tryGetFeverUser_ : string -> transaction string
val enablePublicFeed_ : string -> string -> transaction string
val disablePublicFeed_ : string -> string -> transaction string
val generateNewPublicFeed_ : string -> string -> transaction string
val discover_ : string -> string -> string -> string -> transaction string
val restoreSubscriptionsFromBackup_ : string -> transaction string
val isUserExists_ : string -> transaction string
val deleteAccount_ : string -> string -> transaction string
val recordWebUsage_ : string -> string -> transaction string
val readMsgAndApplyFixes_ : string -> string -> string -> transaction string
val parseRenewalUserId_ : string -> transaction string
val passwordResetEmail_ : string -> transaction string
val sendSignUpEmail_ : string -> string -> string -> transaction string
val sendPasswordResetEmail_ : string -> string -> string -> transaction string
val sendChangeEmailEmail_ : string -> string -> string -> transaction string
val verifySignUpToken_ : string -> transaction string
val verifyPasswordResetToken_ : string -> string -> string -> transaction string
val verifyChangeEmailToken_ : string -> string -> transaction string
val verifyRestoreAccessToken_ : string -> string -> string -> transaction string
val validateEmail_ : string -> string
val maskEmail_ : string -> string
val userAddToPocket_ : string -> string -> string -> string -> string -> transaction string
val userAuthorizeAndAddToPocket_ : string -> transaction string
val logT_ : string -> transaction string
val findUsersLeft_ : string -> transaction string
val updateFeedbackUserInfo_ : string -> transaction string
