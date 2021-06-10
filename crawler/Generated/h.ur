open Binary
open H_ffi
open Datatypes

open DatatypesBinary

task initialize = fn () => H_ffi.init

fun readUserSettings (x1 : string) : transaction (option userSettings) = 
  r <- readUserSettings_ (toHaskell x1) ; return (fromHaskell r)
fun cachedReadUserSettings (x1 : string) : transaction (option userSettings) = 
  r <- cachedReadUserSettings_ (toHaskell x1) ; return (fromHaskell r)
fun cachedNothingReadUserSettings (x1 : string) : transaction (option userSettings) = 
  r <- cachedNothingReadUserSettings_ (toHaskell x1) ; return (fromHaskell r)
fun mergeWriteUserSettings (x1 : userSettings) : transaction ({}) = 
  r <- mergeWriteUserSettings_ (toHaskell x1) ; return (fromHaskell r)
fun deleteUserSettings (x1 : userSettings) : transaction ({}) = 
  r <- deleteUserSettings_ (toHaskell x1) ; return (fromHaskell r)
fun readManyUserSettingss (x1 : list string) : transaction (list (option userSettings)) = 
  r <- readManyUserSettingss_ (toHaskell x1) ; return (fromHaskell r)
fun cachedReadManyUserSettingss (x1 : list string) : transaction (list (option userSettings)) = 
  r <- cachedReadManyUserSettingss_ (toHaskell x1) ; return (fromHaskell r)
fun cachedNothingReadManyUserSettingss (x1 : list string) : transaction (list (option userSettings)) = 
  r <- cachedNothingReadManyUserSettingss_ (toHaskell x1) ; return (fromHaskell r)
fun writeManyUserSettingss (x1 : list userSettings) : transaction ({}) = 
  r <- writeManyUserSettingss_ (toHaskell x1) ; return (fromHaskell r)
fun readSession (x1 : string) : transaction (option session) = 
  r <- readSession_ (toHaskell x1) ; return (fromHaskell r)
fun cachedReadSession (x1 : string) : transaction (option session) = 
  r <- cachedReadSession_ (toHaskell x1) ; return (fromHaskell r)
fun cachedNothingReadSession (x1 : string) : transaction (option session) = 
  r <- cachedNothingReadSession_ (toHaskell x1) ; return (fromHaskell r)
fun mergeWriteSession (x1 : session) : transaction ({}) = 
  r <- mergeWriteSession_ (toHaskell x1) ; return (fromHaskell r)
fun deleteSession (x1 : session) : transaction ({}) = 
  r <- deleteSession_ (toHaskell x1) ; return (fromHaskell r)
fun readManySessions (x1 : list string) : transaction (list (option session)) = 
  r <- readManySessions_ (toHaskell x1) ; return (fromHaskell r)
fun cachedReadManySessions (x1 : list string) : transaction (list (option session)) = 
  r <- cachedReadManySessions_ (toHaskell x1) ; return (fromHaskell r)
fun cachedNothingReadManySessions (x1 : list string) : transaction (list (option session)) = 
  r <- cachedNothingReadManySessions_ (toHaskell x1) ; return (fromHaskell r)
fun writeManySessions (x1 : list session) : transaction ({}) = 
  r <- writeManySessions_ (toHaskell x1) ; return (fromHaskell r)
fun readMsg (x1 : msgKey) : transaction (option msg) = 
  r <- readMsg_ (toHaskell x1) ; return (fromHaskell r)
fun cachedReadMsg (x1 : msgKey) : transaction (option msg) = 
  r <- cachedReadMsg_ (toHaskell x1) ; return (fromHaskell r)
fun cachedNothingReadMsg (x1 : msgKey) : transaction (option msg) = 
  r <- cachedNothingReadMsg_ (toHaskell x1) ; return (fromHaskell r)
fun mergeWriteMsg (x1 : msg) : transaction ({}) = 
  r <- mergeWriteMsg_ (toHaskell x1) ; return (fromHaskell r)
fun deleteMsg (x1 : msg) : transaction ({}) = 
  r <- deleteMsg_ (toHaskell x1) ; return (fromHaskell r)
fun readManyMsgs (x1 : list msgKey) : transaction (list (option msg)) = 
  r <- readManyMsgs_ (toHaskell x1) ; return (fromHaskell r)
fun cachedReadManyMsgs (x1 : list msgKey) : transaction (list (option msg)) = 
  r <- cachedReadManyMsgs_ (toHaskell x1) ; return (fromHaskell r)
fun cachedNothingReadManyMsgs (x1 : list msgKey) : transaction (list (option msg)) = 
  r <- cachedNothingReadManyMsgs_ (toHaskell x1) ; return (fromHaskell r)
fun writeManyMsgs (x1 : list msg) : transaction ({}) = 
  r <- writeManyMsgs_ (toHaskell x1) ; return (fromHaskell r)
fun loginGetForwardUrl (x1 : externalLoginType)(x2 : string)(x3 : externalLoginAction)(x4 : url) : transaction (url) = 
  r <- loginGetForwardUrl_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)
fun loginCallback (x1 : externalLoginType)(x2 : string)(x3 : url)(x4 : string) : transaction ((loginType * loginAccessToken * externalLoginAction * option string)) = 
  r <- loginCallback_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)
fun userSubscribe (x1 : string)(x2 : string)(x3 : option string)(x4 : list string) : transaction (string) = 
  r <- userSubscribe_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)
fun userDiscoverySubscribe (x1 : string)(x2 : string)(x3 : string)(x4 : string)(x5 : option string)(x6 : list string) : transaction (string) = 
  r <- userDiscoverySubscribe_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) (toHaskell x5) (toHaskell x6) ; return (fromHaskell r)
fun userRenameSubscription (x1 : string)(x2 : string)(x3 : string) : transaction ({}) = 
  r <- userRenameSubscription_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)
fun userRenameFolder (x1 : string)(x2 : string)(x3 : string) : transaction (string) = 
  r <- userRenameFolder_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)
fun userEditSubscriptionFolders (x1 : string)(x2 : string)(x3 : string)(x4 : bool) : transaction ({}) = 
  r <- userEditSubscriptionFolders_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)
fun userUnsubscribe (x1 : string)(x2 : list string) : transaction ({}) = 
  r <- userUnsubscribe_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)
fun userRetrySubscription (x1 : string)(x2 : string) : transaction ({}) = 
  r <- userRetrySubscription_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)
fun deleteFilter (x1 : string)(x2 : int) : transaction ({}) = 
  r <- deleteFilter_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)
fun deleteSmartStream (x1 : string)(x2 : string) : transaction ({}) = 
  r <- deleteSmartStream_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)
fun checkQuerySyntax (x1 : string) : transaction (option string) = 
  r <- checkQuerySyntax_ (toHaskell x1) ; return (fromHaskell r)
fun addFilter (x1 : string)(x2 : string)(x3 : bool)(x4 : list int) : transaction ({}) = 
  r <- addFilter_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)
fun editFilter (x1 : string)(x2 : int)(x3 : string)(x4 : bool)(x5 : list int) : transaction ({}) = 
  r <- editFilter_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) (toHaskell x5) ; return (fromHaskell r)
fun addSmartStream (x1 : string)(x2 : string)(x3 : string)(x4 : list int) : transaction ({}) = 
  r <- addSmartStream_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)
fun editSmartStream (x1 : string)(x2 : string)(x3 : string)(x4 : list int) : transaction ({}) = 
  r <- editSmartStream_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)
fun userOPML (x1 : bool)(x2 : string) : transaction (string) = 
  r <- userOPML_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)
fun opmlSubscriptions (x1 : blob)(x2 : string) : transaction ({}) = 
  r <- opmlSubscriptions_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)
fun subscriptionsAndRenames (x1 : string)(x2 : bool)(x3 : updateFilters)(x4 : time)(x5 : string)(x6 : string)(x7 : string)(x8 : string) : transaction ((option (Binary_ffi.xbodyString * string * list string) * string * list subItemRpc * option ((list (int * filterQueryRpc) * list (string * filterQueryRpc)) * string) * list (time * string * string))) = 
  r <- subscriptionsAndRenames_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) (toHaskell x5) (toHaskell x6) (toHaskell x7) (toHaskell x8) ; return (fromHaskell r)
fun subscriptionsAndSettings (x1 : string)(x2 : bool)(x3 : bool)(x4 : string) : transaction (((option (Binary_ffi.xbodyString * string * list string) * string * list subItemRpc * option ((list (int * filterQueryRpc) * list (string * filterQueryRpc)) * string) * list (time * string * string)) * (list string * bool * userSettings) * (welcomeState * list (time * string * string) * int * int))) = 
  r <- subscriptionsAndSettings_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)
fun orderNotification (x1 : string) : transaction ((string * payment)) = 
  r <- orderNotification_ (toHaskell x1) ; return (fromHaskell r)
fun orderNotificationNew (x1 : string)(x2 : string) : transaction ({}) = 
  r <- orderNotificationNew_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)
fun checkOrder (x1 : string) : transaction ((string * payment)) = 
  r <- checkOrder_ (toHaskell x1) ; return (fromHaskell r)
fun getPaidTill (x1 : string) : transaction (paidTill) = 
  r <- getPaidTill_ (toHaskell x1) ; return (fromHaskell r)
fun getUserAccountTypeAndRenewalUrl (x1 : string) : transaction ((string * string)) = 
  r <- getUserAccountTypeAndRenewalUrl_ (toHaskell x1) ; return (fromHaskell r)
fun getFeedDetails (x1 : string)(x2 : string)(x3 : string) : transaction ((string * option string * option string * msgTreeViewMode)) = 
  r <- getFeedDetails_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)
fun performBgActions (x1 : string)(x2 : list bgAction) : transaction (string) = 
  r <- performBgActions_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)
fun tagsForest (x1 : apiMode)(x2 : string)(x3 : option (list itemTag))(x4 : msgTreeViewMode) : transaction ((markReq * list (int * int * int * int * int) * msgForest)) = 
  r <- tagsForest_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)
fun folderForest (x1 : string)(x2 : option string)(x3 : feedsOrDiscovery)(x4 : list postsReq)(x5 : msgTreeViewMode)(x6 : string)(x7 : string) : transaction ((markReq * list (int * int * int * int * int) * msgForest)) = 
  r <- folderForest_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) (toHaskell x5) (toHaskell x6) (toHaskell x7) ; return (fromHaskell r)
fun getTree (x1 : apiMode)(x2 : string)(x3 : msgTreeViewMode)(x4 : list treeReq) : transaction (list (option msgForest)) = 
  r <- getTree_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)
fun filterForest (x1 : string)(x2 : option string)(x3 : string)(x4 : option string)(x5 : feedsOrDiscovery)(x6 : msgTreeViewMode)(x7 : string)(x8 : string) : transaction (Either.either string (markReq * list (int * int * int * int * int) * filterResults)) = 
  r <- filterForest_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) (toHaskell x5) (toHaskell x6) (toHaskell x7) (toHaskell x8) ; return (fromHaskell r)
fun filterTagsForest (x1 : string)(x2 : string)(x3 : option (list itemTag))(x4 : msgTreeViewMode)(x5 : string)(x6 : string) : transaction (Either.either string (markReq * list (int * int * int * int * int) * filterResults)) = 
  r <- filterTagsForest_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) (toHaskell x5) (toHaskell x6) ; return (fromHaskell r)
fun smartStreamForest (x1 : apiMode)(x2 : string)(x3 : string)(x4 : list (int * int * int * int * int))(x5 : msgTreeViewMode) : transaction ((markReq * list (int * int * int * int * int) * msgForest)) = 
  r <- smartStreamForest_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) (toHaskell x5) ; return (fromHaskell r)
fun markReqReadCounters (x1 : string)(x2 : msgTreeViewMode)(x3 : markReq)(x4 : list msgId) : transaction (list (int * int * int * int * int)) = 
  r <- markReqReadCounters_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)
fun pageFromFile (x1 : string) : transaction (page) = 
  r <- pageFromFile_ (toHaskell x1) ; return (fromHaskell r)
fun addWebpackScripts (x1 : string) : transaction (string) = 
  r <- addWebpackScripts_ (toHaskell x1) ; return (fromHaskell r)
val webpackStyles  : transaction (xhead) = 
  r <- webpackStyles_ ; return (fromHaskell r)
fun blessId (x1 : string) :  (Basis.id) = 
  fromHaskell (blessId_ (toHaskell x1) )
fun parseQueryStringUtf8Only (x1 : string) :  (list (string * string)) = 
  fromHaskell (parseQueryStringUtf8Only_ (toHaskell x1) )
fun userEmail (x1 : string) : transaction (option emailAddress) = 
  r <- userEmail_ (toHaskell x1) ; return (fromHaskell r)
fun buyPage (x1 : string)(x2 : string)(x3 : option emailAddress) : transaction (page) = 
  r <- buyPage_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)
fun invoiceLink (x1 : string) :  (string) = 
  fromHaskell (invoiceLink_ (toHaskell x1) )
fun prettyUID (x1 : string) : transaction (string) = 
  r <- prettyUID_ (toHaskell x1) ; return (fromHaskell r)
fun xbodyStringToString (x1 : Binary_ffi.xbodyString) :  (string) = 
  fromHaskell (xbodyStringToString_ (toHaskell x1) )
fun xbodyStringToXbody (x1 : Binary_ffi.xbodyString) :  (xbody) = 
  fromHaskell (xbodyStringToXbody_ (toHaskell x1) )
fun escapeXbody (x1 : xbody) :  (xbody) = 
  fromHaskell (escapeXbody_ (toHaskell x1) )
fun hyphenatePage (x1 : page) :  (page) = 
  fromHaskell (hyphenatePage_ (toHaskell x1) )
fun hyphenateXbody (x1 : xbody) :  (xbody) = 
  fromHaskell (hyphenateXbody_ (toHaskell x1) )
fun toLowerCase (x1 : string) :  (string) = 
  fromHaskell (toLowerCase_ (toHaskell x1) )
fun addTwitterScreenName (x1 : string)(x2 : string)(x3 : string) : transaction ({}) = 
  r <- addTwitterScreenName_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)
fun newSessionJunk (x1 : loginType)(x2 : loginAccessToken)(x3 : list (string * string)) : transaction (session) = 
  r <- newSessionJunk_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)
fun getUserByLogin (x1 : loginType)(x2 : option string) : transaction (option string) = 
  r <- getUserByLogin_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)
fun clearSession (x1 : string)(x2 : string) : transaction ({}) = 
  r <- clearSession_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)
fun userEvent (x1 : string)(x2 : string)(x3 : string) : transaction ({}) = 
  r <- userEvent_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)
val runTasks  : transaction ({}) = 
  r <- runTasks_ ; return (fromHaskell r)
val runApiServer  : transaction ({}) = 
  r <- runApiServer_ ; return (fromHaskell r)
val reloadBrowserPage  : transaction ({}) = 
  r <- reloadBrowserPage_ ; return (fromHaskell r)
fun logOutAllSessions (x1 : string)(x2 : list string) : transaction ({}) = 
  r <- logOutAllSessions_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)
fun getFullText (x1 : string)(x2 : bool)(x3 : string)(x4 : string)(x5 : msgKey) : transaction (Either.either string Binary_ffi.xbodyString) = 
  r <- getFullText_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) (toHaskell x5) ; return (fromHaskell r)
val getUrTime  : transaction (time) = 
  r <- getUrTime_ ; return (fromHaskell r)
fun setUsername (x1 : option string)(x2 : string)(x3 : string) : transaction (bool) = 
  r <- setUsername_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)
fun setPassword (x1 : option string)(x2 : string)(x3 : string) : transaction ({}) = 
  r <- setPassword_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)
fun tryRemoveAssociatedAccount (x1 : option string)(x2 : string)(x3 : loginType) : transaction (bool) = 
  r <- tryRemoveAssociatedAccount_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)
fun tryAddAssociatedAccount (x1 : option string)(x2 : string)(x3 : loginType) : transaction (bool) = 
  r <- tryAddAssociatedAccount_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)
fun tryGetFeverUser (x1 : string) : transaction (option string) = 
  r <- tryGetFeverUser_ (toHaskell x1) ; return (fromHaskell r)
fun enablePublicFeed (x1 : publicFeedType)(x2 : string) : transaction (list (string * bool * option string)) = 
  r <- enablePublicFeed_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)
fun disablePublicFeed (x1 : publicFeedType)(x2 : string) : transaction (list (string * bool * option string)) = 
  r <- disablePublicFeed_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)
fun generateNewPublicFeed (x1 : publicFeedType)(x2 : string) : transaction (list (string * bool * option string)) = 
  r <- generateNewPublicFeed_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)
fun discover (x1 : string)(x2 : string)(x3 : string)(x4 : string) : transaction (option (xbody * list (string * msgTreeViewMode))) = 
  r <- discover_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)
fun restoreSubscriptionsFromBackup (x1 : string) : transaction ({}) = 
  r <- restoreSubscriptionsFromBackup_ (toHaskell x1) ; return (fromHaskell r)
fun isUserExists (x1 : string) : transaction (bool) = 
  r <- isUserExists_ (toHaskell x1) ; return (fromHaskell r)
fun deleteAccount (x1 : bool)(x2 : string) : transaction ({}) = 
  r <- deleteAccount_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)
fun recordWebUsage (x1 : string)(x2 : option string) : transaction ({}) = 
  r <- recordWebUsage_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)
fun readMsgAndApplyFixes (x1 : string)(x2 : string)(x3 : msgKey) : transaction (option msg) = 
  r <- readMsgAndApplyFixes_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)
fun parseRenewalUserId (x1 : string) : transaction (string) = 
  r <- parseRenewalUserId_ (toHaskell x1) ; return (fromHaskell r)
fun passwordResetEmail (x1 : string) : transaction (Either.either string (string * string)) = 
  r <- passwordResetEmail_ (toHaskell x1) ; return (fromHaskell r)
fun sendSignUpEmail (x1 : string)(x2 : string)(x3 : string) : transaction (bool) = 
  r <- sendSignUpEmail_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)
fun sendPasswordResetEmail (x1 : string)(x2 : string)(x3 : string) : transaction (bool) = 
  r <- sendPasswordResetEmail_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)
fun sendChangeEmailEmail (x1 : string)(x2 : string)(x3 : string) : transaction (bool) = 
  r <- sendChangeEmailEmail_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)
fun verifySignUpToken (x1 : string) : transaction (option string) = 
  r <- verifySignUpToken_ (toHaskell x1) ; return (fromHaskell r)
fun verifyPasswordResetToken (x1 : string)(x2 : string)(x3 : string) : transaction (option string) = 
  r <- verifyPasswordResetToken_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)
fun verifyChangeEmailToken (x1 : string)(x2 : string) : transaction (option string) = 
  r <- verifyChangeEmailToken_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)
fun verifyRestoreAccessToken (x1 : string)(x2 : string)(x3 : string) : transaction (option string) = 
  r <- verifyRestoreAccessToken_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)
fun validateEmail (x1 : string) :  (option string) = 
  fromHaskell (validateEmail_ (toHaskell x1) )
fun maskEmail (x1 : string) :  (string) = 
  fromHaskell (maskEmail_ (toHaskell x1) )
fun userAddToPocket (x1 : string)(x2 : string)(x3 : url)(x4 : string)(x5 : string) : transaction (okErrorRedirect) = 
  r <- userAddToPocket_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) (toHaskell x5) ; return (fromHaskell r)
fun userAuthorizeAndAddToPocket (x1 : string) : transaction ({}) = 
  r <- userAuthorizeAndAddToPocket_ (toHaskell x1) ; return (fromHaskell r)
fun logT (x1 : string) : transaction ({}) = 
  r <- logT_ (toHaskell x1) ; return (fromHaskell r)
fun findUsersLeft (x1 : int) : transaction ((list feedbackUserInfo * list feedbackUserInfo)) = 
  r <- findUsersLeft_ (toHaskell x1) ; return (fromHaskell r)
fun updateFeedbackUserInfo (x1 : feedbackUserInfo) : transaction ({}) = 
  r <- updateFeedbackUserInfo_ (toHaskell x1) ; return (fromHaskell r)
