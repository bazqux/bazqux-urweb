open Binary
open Ur_ffi
open Datatypes

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

fun loginGetForwardUrl (x1 : loginType)(x2 : string)(x3 : string)(x4 : url) : transaction (url) = 
  r <- loginGetForwardUrl_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)

fun loginCallback (x1 : loginType)(x2 : string)(x3 : url)(x4 : string) : transaction ((uID * option string)) = 
  r <- loginCallback_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)

fun fbTokenGetForwardUrl (x1 : string)(x2 : url) : transaction (url) = 
  r <- fbTokenGetForwardUrl_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)

fun fbTokenCallback (x1 : string)(x2 : url)(x3 : string) : transaction (string) = 
  r <- fbTokenCallback_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)

fun importFromGoogleReaderGetForwardUrl (x1 : string)(x2 : url) : transaction (url) = 
  r <- importFromGoogleReaderGetForwardUrl_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)

fun importFromGoogleReaderCallback (x1 : string)(x2 : string)(x3 : string)(x4 : string) : transaction ({}) = 
  r <- importFromGoogleReaderCallback_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)

fun importStarredAndTaggedItemsFromGoogleReaderCallback (x1 : string)(x2 : string)(x3 : string)(x4 : string) : transaction ({}) = 
  r <- importStarredAndTaggedItemsFromGoogleReaderCallback_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)

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

fun userDeleteFilter (x1 : string)(x2 : string)(x3 : bool) : transaction ({}) = 
  r <- userDeleteFilter_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)

fun userDeleteSmartStream (x1 : string)(x2 : string) : transaction ({}) = 
  r <- userDeleteSmartStream_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)

fun userAddFilter (x1 : string)(x2 : string)(x3 : bool)(x4 : list string) : transaction ({}) = 
  r <- userAddFilter_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)

fun userEditFilter (x1 : string)(x2 : string)(x3 : bool)(x4 : string)(x5 : bool)(x6 : list string) : transaction ({}) = 
  r <- userEditFilter_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) (toHaskell x5) (toHaskell x6) ; return (fromHaskell r)

fun userAddSmartStream (x1 : string)(x2 : string)(x3 : string)(x4 : list string) : transaction ({}) = 
  r <- userAddSmartStream_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)

fun userEditSmartStream (x1 : string)(x2 : string)(x3 : string)(x4 : list string) : transaction ({}) = 
  r <- userEditSmartStream_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)

fun userOPML (x1 : bool)(x2 : string) : transaction (string) = 
  r <- userOPML_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)

fun opmlSubscriptions (x1 : blob)(x2 : string) : transaction ({}) = 
  r <- opmlSubscriptions_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)

fun userSubscriptionsAndRenames (x1 : bool)(x2 : time)(x3 : string)(x4 : string)(x5 : list string)(x6 : string) : transaction ((option (xbody * string * list string) * string * list subItemRpc * bool * list (time * string * string))) = 
  r <- userSubscriptionsAndRenames_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) (toHaskell x5) (toHaskell x6) ; return (fromHaskell r)

fun userSubscriptionsAndSettings (x1 : string)(x2 : string) : transaction ((option (xbody * string * list string) * string * list subItemRpc * (bool * bool * list (time * string * string) * list string * userSettings) * (option welcomeState * list filterQuery * list (string * list filterQuery)))) = 
  r <- userSubscriptionsAndSettings_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)

fun userGetFiltersAndSmartStreams (x1 : string) : transaction ((list filterQuery * list (string * list filterQuery))) = 
  r <- userGetFiltersAndSmartStreams_ (toHaskell x1) ; return (fromHaskell r)

fun orderNotification (x1 : string) : transaction (payment) = 
  r <- orderNotification_ (toHaskell x1) ; return (fromHaskell r)

fun checkOrder (x1 : string) : transaction (payment) = 
  r <- checkOrder_ (toHaskell x1) ; return (fromHaskell r)

fun getPaidTill (x1 : string) : transaction (paidTill) = 
  r <- getPaidTill_ (toHaskell x1) ; return (fromHaskell r)

fun activeGRImportsCount (x1 : {}) : transaction (int) = 
  r <- activeGRImportsCount_ (toHaskell x1) ; return (fromHaskell r)

fun activeGRImportNames (x1 : {}) : transaction (xbody) = 
  r <- activeGRImportNames_ (toHaskell x1) ; return (fromHaskell r)

fun getFeedDetails (x1 : string)(x2 : string) : transaction ((string * option string * option string * msgTreeViewMode)) = 
  r <- getFeedDetails_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)

fun tagsMsgForest (x1 : apiMode)(x2 : string)(x3 : option (list itemTag))(x4 : msgTreeViewMode) : transaction (msgForest) = 
  r <- tagsMsgForest_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)

fun folderMsgForest (x1 : apiMode)(x2 : string)(x3 : list (string * int * int * int * int))(x4 : list postsReq)(x5 : msgTreeViewMode) : transaction ((list (string * int * int * int * int) * msgForest)) = 
  r <- folderMsgForest_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) (toHaskell x5) ; return (fromHaskell r)

fun userGetTree (x1 : apiMode)(x2 : string)(x3 : msgTreeViewMode)(x4 : list treeReq) : transaction (list (option msgForest)) = 
  r <- userGetTree_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)

fun performBgActions (x1 : string)(x2 : list bgAction) : transaction (string) = 
  r <- performBgActions_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)

fun filterMsgForest (x1 : string)(x2 : option string)(x3 : string)(x4 : list (string * int * int * int * int))(x5 : msgTreeViewMode) : transaction ((list (string * int * int * int * int) * filterResults)) = 
  r <- filterMsgForest_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) (toHaskell x5) ; return (fromHaskell r)

fun filterTagsMsgForest (x1 : string)(x2 : string)(x3 : option (list itemTag))(x4 : msgTreeViewMode) : transaction (filterResults) = 
  r <- filterTagsMsgForest_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)

fun smartStreamMsgForest (x1 : apiMode)(x2 : string)(x3 : string)(x4 : list (string * int * int * int * int))(x5 : msgTreeViewMode) : transaction ((list (string * int * int * int * int) * msgForest)) = 
  r <- smartStreamMsgForest_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) (toHaskell x5) ; return (fromHaskell r)

fun htmlHead (x1 : {}) : transaction (xhead) = 
  r <- htmlHead_ (toHaskell x1) ; return (fromHaskell r)

fun htmlHeadMain (x1 : {}) : transaction (xhead) = 
  r <- htmlHeadMain_ (toHaskell x1) ; return (fromHaskell r)

fun htmlHeadMainNoTranslate (x1 : {}) : transaction (xhead) = 
  r <- htmlHeadMainNoTranslate_ (toHaskell x1) ; return (fromHaskell r)

fun htmlLikeButtons (x1 : {}) : transaction (xbody) = 
  r <- htmlLikeButtons_ (toHaskell x1) ; return (fromHaskell r)

fun htmlLandingScripts (x1 : {}) : transaction (page) = 
  r <- htmlLandingScripts_ (toHaskell x1) ; return (fromHaskell r)

fun htmlOpenIdSignInButton (x1 : {}) : transaction (xbody) = 
  r <- htmlOpenIdSignInButton_ (toHaskell x1) ; return (fromHaskell r)

fun htmlConversionLogin (x1 : {}) : transaction (xbody) = 
  r <- htmlConversionLogin_ (toHaskell x1) ; return (fromHaskell r)

fun version (x1 : {}) : transaction (string) = 
  r <- version_ (toHaskell x1) ; return (fromHaskell r)

fun blessId (x1 : string) : transaction (Basis.id) = 
  r <- blessId_ (toHaskell x1) ; return (fromHaskell r)

fun parseQueryStringUtf8Only (x1 : string) : transaction (list (string * string)) = 
  r <- parseQueryStringUtf8Only_ (toHaskell x1) ; return (fromHaskell r)

fun buyLink (x1 : string)(x2 : string) : transaction (string) = 
  r <- buyLink_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)

fun encodeURIComponent (x1 : string) : transaction (string) = 
  r <- encodeURIComponent_ (toHaskell x1) ; return (fromHaskell r)

fun prettyUID (x1 : string) : transaction (string) = 
  r <- prettyUID_ (toHaskell x1) ; return (fromHaskell r)

fun textToXbody (x1 : string) : transaction (xbody) = 
  r <- textToXbody_ (toHaskell x1) ; return (fromHaskell r)

fun newSession (x1 : uID)(x2 : list (string * string)) : transaction (session) = 
  r <- newSession_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)

fun getUserByMobileLogin (x1 : string)(x2 : string) : transaction (uID) = 
  r <- getUserByMobileLogin_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)

fun clearSession (x1 : string) : transaction ({}) = 
  r <- clearSession_ (toHaskell x1) ; return (fromHaskell r)

fun userEvent (x1 : string)(x2 : string)(x3 : string) : transaction ({}) = 
  r <- userEvent_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)

fun initMailer (x1 : {}) : transaction ({}) = 
  r <- initMailer_ (toHaskell x1) ; return (fromHaskell r)

fun initApiServer (x1 : {}) : transaction ({}) = 
  r <- initApiServer_ (toHaskell x1) ; return (fromHaskell r)

fun readFullTextCache (x1 : url) : transaction (option fullTextCache) = 
  r <- readFullTextCache_ (toHaskell x1) ; return (fromHaskell r)

fun cachedReadFullTextCache (x1 : url) : transaction (option fullTextCache) = 
  r <- cachedReadFullTextCache_ (toHaskell x1) ; return (fromHaskell r)

fun cachedNothingReadFullTextCache (x1 : url) : transaction (option fullTextCache) = 
  r <- cachedNothingReadFullTextCache_ (toHaskell x1) ; return (fromHaskell r)

fun mergeWriteFullTextCache (x1 : fullTextCache) : transaction ({}) = 
  r <- mergeWriteFullTextCache_ (toHaskell x1) ; return (fromHaskell r)

fun deleteFullTextCache (x1 : fullTextCache) : transaction ({}) = 
  r <- deleteFullTextCache_ (toHaskell x1) ; return (fromHaskell r)

fun readManyFullTextCaches (x1 : list url) : transaction (list (option fullTextCache)) = 
  r <- readManyFullTextCaches_ (toHaskell x1) ; return (fromHaskell r)

fun cachedReadManyFullTextCaches (x1 : list url) : transaction (list (option fullTextCache)) = 
  r <- cachedReadManyFullTextCaches_ (toHaskell x1) ; return (fromHaskell r)

fun cachedNothingReadManyFullTextCaches (x1 : list url) : transaction (list (option fullTextCache)) = 
  r <- cachedNothingReadManyFullTextCaches_ (toHaskell x1) ; return (fromHaskell r)

fun writeManyFullTextCaches (x1 : list fullTextCache) : transaction ({}) = 
  r <- writeManyFullTextCaches_ (toHaskell x1) ; return (fromHaskell r)

fun userGetFullText (x1 : string)(x2 : msgKey) : transaction (either string string) = 
  r <- userGetFullText_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)

fun getUrTime_ (x1 : {}) : transaction (time) = 
  r <- getUrTime__ (toHaskell x1) ; return (fromHaskell r)

fun setMobileLogin (x1 : string)(x2 : string)(x3 : string)(x4 : string) : transaction (bool) = 
  r <- setMobileLogin_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) ; return (fromHaskell r)

fun tryGetFeverUser (x1 : string) : transaction (option string) = 
  r <- tryGetFeverUser_ (toHaskell x1) ; return (fromHaskell r)

fun userEnablePublicFeed (x1 : publicFeedType)(x2 : string) : transaction (list (string * bool * option string)) = 
  r <- userEnablePublicFeed_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)

fun userDisablePublicFeed (x1 : publicFeedType)(x2 : string) : transaction (list (string * bool * option string)) = 
  r <- userDisablePublicFeed_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)

fun userGenerateNewPublicFeed (x1 : publicFeedType)(x2 : string) : transaction (list (string * bool * option string)) = 
  r <- userGenerateNewPublicFeed_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)

fun userSearchSubscriptions (x1 : string)(x2 : string)(x3 : string) : transaction (option (xbody * list (string * msgTreeViewMode))) = 
  r <- userSearchSubscriptions_ (toHaskell x1) (toHaskell x2) (toHaskell x3) ; return (fromHaskell r)

fun userRestoreSubscriptionsFromBackup (x1 : string) : transaction ({}) = 
  r <- userRestoreSubscriptionsFromBackup_ (toHaskell x1) ; return (fromHaskell r)

fun isUserExists (x1 : string) : transaction (bool) = 
  r <- isUserExists_ (toHaskell x1) ; return (fromHaskell r)

fun userDeleteAccount (x1 : bool)(x2 : string) : transaction ({}) = 
  r <- userDeleteAccount_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)

fun recordWebUsage (x1 : string)(x2 : option string) : transaction ({}) = 
  r <- recordWebUsage_ (toHaskell x1) (toHaskell x2) ; return (fromHaskell r)

fun userAddToPocket (x1 : string)(x2 : string)(x3 : url)(x4 : string)(x5 : string) : transaction (okErrorRedirect) = 
  r <- userAddToPocket_ (toHaskell x1) (toHaskell x2) (toHaskell x3) (toHaskell x4) (toHaskell x5) ; return (fromHaskell r)

fun userAuthorizeAndAddToPocket (x1 : string) : transaction ({}) = 
  r <- userAuthorizeAndAddToPocket_ (toHaskell x1) ; return (fromHaskell r)

