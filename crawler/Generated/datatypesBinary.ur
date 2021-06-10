open Datatypes
open Binary

fun get_subscriptionParentUrl (b : getBuf) : (getBuf * subscriptionParentUrl) = 
  let val (b, c) = get_byte b in case c of
      0 => 
        let val (b, _Url) = get_string b  in
          (b,
           SpuRedirect
           { Url = _Url
           })
        end
    | 1 => 
        let val (b, _Url) = get_string b  in
        let val (b, _Debug) = get_string b  in
          (b,
           SpuHtml
           { Url = _Url
           , Debug = _Debug
           })
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize SubscriptionParentUrl ({[n]} is out of range)</xml>
  end
fun get_subscriptionState (b : getBuf) : (getBuf * subscriptionState) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, SSAdded)
    | 1 => 
        let val (b, _StartTime) = get_time b  in
          (b,
           SSScanning
           { StartTime = _StartTime
           })
        end
    | 2 => 
        let val (b, _Message) = get_string b  in
          (b,
           SSError
           { Message = _Message
           })
        end
    | 3 => 
        let val (b, _Url) = get_string b  in
          (b,
           SSFeed
           { Url = _Url
           })
        end
    | 4 => 
        let val (b, _Message) = get_string b  in
        let val (b, _Path) = get_list (get_subscriptionParentUrl) b  in
          (b,
           SSErrorPath
           { Message = _Message
           , Path = _Path
           })
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize SubscriptionState ({[n]} is out of range)</xml>
  end
fun get_subscription (b : getBuf) : (getBuf * subscription) = 
  case 0 of
      0 => 
        let val (b, _Url) = get_string b  in
        let val (b, _State) = get_subscriptionState b  in
        let val (b, _EditsCount) = get_int b  in
        let val (b, _Title) = get_option (get_string) b  in
        let val (b, _Folders) = get_list (get_string) b  in
          (b,
           { Url = _Url
           , State = _State
           , EditsCount = _EditsCount
           , Title = _Title
           , Folders = _Folders
           })
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize Subscription ({[n]} is out of range)</xml>
fun get_postsViewMode (b : getBuf) : (getBuf * postsViewMode) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, PVMShort)
    | 1 => 
          (b, PVMFull)
    | 2 => 
          (b, PVMMagazine)
    | 3 => 
          (b, PVMMosaic)
    | n => error <xml>Oh, shi -- can’t deserialize PostsViewMode ({[n]} is out of range)</xml>
  end
fun get_mTVMEx (b : getBuf) : (getBuf * mTVMEx) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, MTVMFolderCollapsed)
    | 1 => 
          (b, MTVMFolderExpanded)
    | 2 => 
        let val (b, _FolderExpanded) = get_bool b  in
        let val (b, _GroupByFeed) = get_bool b  in
        let val (b, _Reserved1) = get_bool b  in
        let val (b, _Reserved2) = get_int b  in
          (b,
           MTVMEx
           { FolderExpanded = _FolderExpanded
           , GroupByFeed = _GroupByFeed
           , Reserved1 = _Reserved1
           , Reserved2 = _Reserved2
           })
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize MTVMEx ({[n]} is out of range)</xml>
  end
fun get_msgTreeViewMode (b : getBuf) : (getBuf * msgTreeViewMode) = 
  case 0 of
      0 => 
        let val (b, _Ascending) = get_bool b  in
        let val (b, _UnreadOnly) = get_bool b  in
        let val (b, _ExpandedComments) = get_bool b  in
        let val (b, _Posts) = get_postsViewMode b  in
        let val (b, _Ex) = get_mTVMEx b  in
        let val (b, _NoOverride) = get_bool b  in
          (b,
           { Ascending = _Ascending
           , UnreadOnly = _UnreadOnly
           , ExpandedComments = _ExpandedComments
           , Posts = _Posts
           , Ex = _Ex
           , NoOverride = _NoOverride
           })
        end
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize MsgTreeViewMode ({[n]} is out of range)</xml>
fun get_payment (b : getBuf) : (getBuf * payment) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, PReserved)
    | 1 => 
        let val (b, _OrderId) = get_string b  in
        let val (b, _OrderType) = get_string b  in
        let val (b, _OrderTime) = get_time b  in
          (b,
           PFastSpring
           { OrderId = _OrderId
           , OrderType = _OrderType
           , OrderTime = _OrderTime
           })
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize Payment ({[n]} is out of range)</xml>
  end
fun get_paidTill (b : getBuf) : (getBuf * paidTill) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, PTUnknown)
    | 1 => 
        let val (b, _Till) = get_time b  in
          (b,
           PTPaid
           { Till = _Till
           })
        end
    | 2 => 
        let val (b, _Till) = get_time b  in
          (b,
           PTFreeTrial
           { Till = _Till
           })
        end
    | 3 => 
        let val (b, _Till) = get_time b  in
          (b,
           PTFreeTrialFinished
           { Till = _Till
           })
        end
    | 4 => 
        let val (b, _Till) = get_time b  in
          (b,
           PTPaidFinished
           { Till = _Till
           })
        end
    | n => error <xml>Oh, shi -- can’t deserialize PaidTill ({[n]} is out of range)</xml>
  end
fun get_scrollMode (b : getBuf) : (getBuf * scrollMode) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, SMNormal)
    | 1 => 
          (b, SMQuick)
    | 2 => 
          (b, SMImmediate)
    | n => error <xml>Oh, shi -- can’t deserialize ScrollMode ({[n]} is out of range)</xml>
  end
fun get_listViewMode (b : getBuf) : (getBuf * listViewMode) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, LVMCompact)
    | 1 => 
          (b, LVMTwoLines)
    | n => error <xml>Oh, shi -- can’t deserialize ListViewMode ({[n]} is out of range)</xml>
  end
fun get_markReadMode (b : getBuf) : (getBuf * markReadMode) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, MRMOnScroll)
    | 1 => 
          (b, MRMManual)
    | 2 => 
          (b, MRMOnScrollEverywhere)
    | n => error <xml>Oh, shi -- can’t deserialize MarkReadMode ({[n]} is out of range)</xml>
  end
fun get_publicFeedType (b : getBuf) : (getBuf * publicFeedType) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, PFTAll)
    | 1 => 
        let val (b, _Folder) = get_string b  in
          (b,
           PFTFolder
           { Folder = _Folder
           })
        end
    | 2 => 
        let val (b, _TagName) = get_string b  in
          (b,
           PFTTag
           { TagName = _TagName
           })
        end
    | 3 => 
          (b, PFTStarred)
    | 4 => 
          (b, PFTAllTags)
    | 5 => 
        let val (b, _StreamName) = get_string b  in
          (b,
           PFTSmartStream
           { StreamName = _StreamName
           })
        end
    | n => error <xml>Oh, shi -- can’t deserialize PublicFeedType ({[n]} is out of range)</xml>
  end
fun get_loginAccessToken (b : getBuf) : (getBuf * loginAccessToken) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, LATNone)
    | 1 => 
        let val (b, _AccessToken) = get_string b  in
          (b,
           LATFacebook
           { AccessToken = _AccessToken
           })
        end
    | 2 => 
        let val (b, _Credentials) = get_list ((fn (b : getBuf) =>let val (b, _1) = get_string b  in
        let val (b, _2) = get_string b  in
          (b,(_1, _2))
        end
        end
        )) b  in
          (b,
           LATTwitter
           { Credentials = _Credentials
           })
        end
    | n => error <xml>Oh, shi -- can’t deserialize LoginAccessToken ({[n]} is out of range)</xml>
  end
fun get_apiKeys (b : getBuf) : (getBuf * apiKeys) = 
  case 0 of
      0 => 
        let val (b, _Pocket) = get_option ((fn (b : getBuf) =>let val (b, _1) = get_string b  in
        let val (b, _2) = get_string b  in
          (b,(_1, _2))
        end
        end
        )) b  in
        let val (b, _PocketRequest) = get_option ((fn (b : getBuf) =>let val (b, _1) = get_string b  in
        let val (b, _2) = get_string b  in
        let val (b, _3) = get_string b  in
          (b,(_1, _2, _3))
        end
        end
        end
        )) b  in
        let val (b, _Reserved10) = get_option (get_int) b  in
        let val (b, _FacebookAccessToken) = get_option ((fn (b : getBuf) =>let val (b, _1) = get_time b  in
        let val (b, _2) = get_string b  in
          (b,(_1, _2))
        end
        end
        )) b  in
        let val (b, _TwitterAccessToken) = get_option ((fn (b : getBuf) =>let val (b, _1) = get_time b  in
        let val (b, _2) = get_list ((fn (b : getBuf) =>let val (b, _1) = get_string b  in
        let val (b, _2) = get_string b  in
          (b,(_1, _2))
        end
        end
        )) b  in
          (b,(_1, _2))
        end
        end
        )) b  in
        let val (b, _Reserved13) = get_bool b  in
        let val (b, _Reserved14) = get_bool b  in
        let val (b, _Reserved15) = get_bool b  in
        let val (b, _Reserved16) = get_bool b  in
        let val (b, _Reserved17) = get_bool b  in
        let val (b, _Reserved2) = get_int b  in
        let val (b, _Reserved3) = get_int b  in
        let val (b, _Reserved4) = get_int b  in
          (b,
           { Pocket = _Pocket
           , PocketRequest = _PocketRequest
           , Reserved10 = _Reserved10
           , FacebookAccessToken = _FacebookAccessToken
           , TwitterAccessToken = _TwitterAccessToken
           , Reserved13 = _Reserved13
           , Reserved14 = _Reserved14
           , Reserved15 = _Reserved15
           , Reserved16 = _Reserved16
           , Reserved17 = _Reserved17
           , Reserved2 = _Reserved2
           , Reserved3 = _Reserved3
           , Reserved4 = _Reserved4
           })
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize ApiKeys ({[n]} is out of range)</xml>
fun get_userExperiment (b : getBuf) : (getBuf * userExperiment) = 
  case 0 of
      0 => 
          (b, UENo9)
    | n => error <xml>Oh, shi -- can’t deserialize UserExperiment ({[n]} is out of range)</xml>
fun get_customShareAction (b : getBuf) : (getBuf * customShareAction) = 
  case 0 of
      0 => 
        let val (b, _Id) = get_int b  in
        let val (b, _Title) = get_string b  in
        let val (b, _UrlFormat) = get_string b  in
        let val (b, _Shorten) = get_bool b  in
          (b,
           { Id = _Id
           , Title = _Title
           , UrlFormat = _UrlFormat
           , Shorten = _Shorten
           })
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize CustomShareAction ({[n]} is out of range)</xml>
fun get_shareAction (b : getBuf) : (getBuf * shareAction) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, SAEMail)
    | 1 => 
          (b, SATwitter)
    | 2 => 
          (b, SAFacebook)
    | 3 => 
          (b, SAGooglePlus)
    | 4 => 
          (b, SATumblr)
    | 5 => 
          (b, SAEvernote)
    | 6 => 
          (b, SADelicious_discontinued)
    | 7 => 
          (b, SAPinboard)
    | 8 => 
          (b, SAPocket)
    | 9 => 
          (b, SAReadability_discontinued)
    | 10 => 
          (b, SAInstapaper)
    | 11 => 
          (b, SATranslate)
    | 12 => 
          (b, SABlogger)
    | 13 => 
          (b, SAWordpress)
    | 14 => 
          (b, SALinkedIn)
    | 15 => 
          (b, SAPinterest)
    | 16 => 
          (b, SAVK)
    | 17 => 
          (b, SASkype)
    | 18 => 
          (b, SAReddit)
    | 19 => 
          (b, SAStumbleUpon)
    | 20 => 
          (b, SADigg)
    | 21 => 
          (b, SAScoopIt)
    | 22 => 
          (b, SAFlipboard)
    | 23 => 
          (b, SABuffer)
    | 24 => 
          (b, SANewsVine)
    | 25 => 
          (b, SADiigo)
    | 26 => 
          (b, SARememberTheMilk)
    | 27 => 
          (b, SAGoogleBookmarks)
    | 28 => 
          (b, SAWallabag)
    | 29 => 
          (b, SAWakelet)
    | 30 => 
        let val (b, _CustomShareAction) = get_customShareAction b  in
          (b,
           SACustom
           { CustomShareAction = _CustomShareAction
           })
        end
    | 31 => 
          (b, SASystem)
    | n => error <xml>Oh, shi -- can’t deserialize ShareAction ({[n]} is out of range)</xml>
  end
fun get_msgButton (b : getBuf) : (getBuf * msgButton) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, MBKeepUnread)
    | 1 => 
          (b, MBStar)
    | 2 => 
          (b, MBTag)
    | 3 => 
          (b, MBShare)
    | 4 => 
        let val (b, _ShareAction) = get_shareAction b  in
          (b,
           MBShareAction
           { ShareAction = _ShareAction
           })
        end
    | n => error <xml>Oh, shi -- can’t deserialize MsgButton ({[n]} is out of range)</xml>
  end
fun get_emailContact (b : getBuf) : (getBuf * emailContact) = 
  case 0 of
      0 => 
        let val (b, _EMail) = get_string b  in
        let val (b, _FullName) = get_string b  in
        let val (b, _Groups) = get_list (get_string) b  in
        let val (b, _Avatar) = get_option (get_string) b  in
        let val (b, _Stats) = get_option ((fn (b : getBuf) =>let val (b, _1) = get_time b  in
        let val (b, _2) = get_int b  in
          (b,(_1, _2))
        end
        end
        )) b  in
          (b,
           { EMail = _EMail
           , FullName = _FullName
           , Groups = _Groups
           , Avatar = _Avatar
           , Stats = _Stats
           })
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize EmailContact ({[n]} is out of range)</xml>
fun get_sharingSettings (b : getBuf) : (getBuf * sharingSettings) = 
  case 0 of
      0 => 
        let val (b, _ShareMenuButtons) = get_option (get_list (get_msgButton)) b  in
        let val (b, _MsgButtons) = get_option (get_list (get_msgButton)) b  in
        let val (b, _ListViewButtons) = get_option (get_list (get_msgButton)) b  in
        let val (b, _CustomShareActions) = get_list (get_customShareAction) b  in
        let val (b, _EMailUsingMailto) = get_bool b  in
        let val (b, _ReplyToEMail) = get_option ((fn (b : getBuf) =>let val (b, _1) = get_string b  in
        let val (b, _2) = get_string b  in
          (b,(_1, _2))
        end
        end
        )) b  in
        let val (b, _Contacts) = get_list (get_emailContact) b  in
        let val (b, _Reserved1) = get_int b  in
          (b,
           { ShareMenuButtons = _ShareMenuButtons
           , MsgButtons = _MsgButtons
           , ListViewButtons = _ListViewButtons
           , CustomShareActions = _CustomShareActions
           , EMailUsingMailto = _EMailUsingMailto
           , ReplyToEMail = _ReplyToEMail
           , Contacts = _Contacts
           , Reserved1 = _Reserved1
           })
        end
        end
        end
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize SharingSettings ({[n]} is out of range)</xml>
fun get_loginType (b : getBuf) : (getBuf * loginType) = 
  let val (b, c) = get_byte b in case c of
      0 => 
        let val (b, _Email) = get_string b  in
          (b,
           LTGoogle
           { Email = _Email
           })
        end
    | 1 => 
        let val (b, _Email) = get_string b  in
          (b,
           LTFacebook
           { Email = _Email
           })
        end
    | 2 => 
        let val (b, _Id) = get_string b  in
          (b,
           LTTwitter
           { Id = _Id
           })
        end
    | 3 => 
        let val (b, _URL) = get_string b  in
          (b,
           LTOpenId
           { URL = _URL
           })
        end
    | 4 => 
        let val (b, _Email) = get_string b  in
          (b,
           LTEmail
           { Email = _Email
           })
        end
    | 5 => 
        let val (b, _Username) = get_string b  in
          (b,
           LTUsername
           { Username = _Username
           })
        end
    | 6 => 
        let val (b, _ApiKey) = get_string b  in
          (b,
           LTFeverApiKey
           { ApiKey = _ApiKey
           })
        end
    | n => error <xml>Oh, shi -- can’t deserialize LoginType ({[n]} is out of range)</xml>
  end
fun get_userSettingsEx (b : getBuf) : (getBuf * userSettingsEx) = 
  case 0 of
      0 => 
        let val (b, _LastWhatsNewTime) = get_time b  in
        let val (b, _PasswordHash) = get_option (get_string) b  in
        let val (b, _Reserved1_1) = get_option (get_int) b  in
        let val (b, _Reserved1_2) = get_option (get_int) b  in
        let val (b, _Reserved1_3) = get_option (get_int) b  in
        let val (b, _Reserved1_4) = get_option (get_int) b  in
        let val (b, _Reserved1_5) = get_option (get_int) b  in
        let val (b, _Reserved1_6) = get_option (get_int) b  in
        let val (b, _Reserved1_7) = get_option (get_int) b  in
        let val (b, _AssociatedAccounts) = get_list (get_loginType) b  in
        let val (b, _AssociatedAccountNames) = get_list ((fn (b : getBuf) =>let val (b, _1) = get_loginType b  in
        let val (b, _2) = get_string b  in
          (b,(_1, _2))
        end
        end
        )) b  in
        let val (b, _Reserved4) = get_int b  in
        let val (b, _Reserved5) = get_int b  in
        let val (b, _Reserved6) = get_int b  in
        let val (b, _Reserved7) = get_int b  in
        let val (b, _Reserved8) = get_int b  in
        let val (b, _Reserved9) = get_int b  in
        let val (b, _Reserved10) = get_int b  in
        let val (b, _Reserved11) = get_int b  in
        let val (b, _Reserved12) = get_int b  in
        let val (b, _Reserved13) = get_int b  in
        let val (b, _Reserved14) = get_int b  in
        let val (b, _Reserved15) = get_int b  in
          (b,
           { LastWhatsNewTime = _LastWhatsNewTime
           , PasswordHash = _PasswordHash
           , Reserved1_1 = _Reserved1_1
           , Reserved1_2 = _Reserved1_2
           , Reserved1_3 = _Reserved1_3
           , Reserved1_4 = _Reserved1_4
           , Reserved1_5 = _Reserved1_5
           , Reserved1_6 = _Reserved1_6
           , Reserved1_7 = _Reserved1_7
           , AssociatedAccounts = _AssociatedAccounts
           , AssociatedAccountNames = _AssociatedAccountNames
           , Reserved4 = _Reserved4
           , Reserved5 = _Reserved5
           , Reserved6 = _Reserved6
           , Reserved7 = _Reserved7
           , Reserved8 = _Reserved8
           , Reserved9 = _Reserved9
           , Reserved10 = _Reserved10
           , Reserved11 = _Reserved11
           , Reserved12 = _Reserved12
           , Reserved13 = _Reserved13
           , Reserved14 = _Reserved14
           , Reserved15 = _Reserved15
           })
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize UserSettingsEx ({[n]} is out of range)</xml>
fun get_userSettings (b : getBuf) : (getBuf * userSettings) = 
  case 0 of
      0 => 
        let val (b, _User) = get_string b  in
        let val (b, _EditsCount) = get_int b  in
        let val (b, _ScrollMode) = get_scrollMode b  in
        let val (b, _ListViewMode) = get_listViewMode b  in
        let val (b, _ShowFavicons) = get_bool b  in
        let val (b, _MarkReadMode) = get_markReadMode b  in
        let val (b, _UltraCompact) = get_bool b  in
        let val (b, _Reserved) = get_option (get_string) b  in
        let val (b, _ExactUnreadCounts) = get_bool b  in
        let val (b, _PublicFeeds) = get_option (get_list ((fn (b : getBuf) =>let val (b, _1) = get_publicFeedType b  in
        let val (b, _2) = get_list ((fn (b : getBuf) =>let val (b, _1) = get_string b  in
        let val (b, _2) = get_bool b  in
        let val (b, _3) = get_option (get_string) b  in
          (b,(_1, _2, _3))
        end
        end
        end
        )) b  in
          (b,(_1, _2))
        end
        end
        ))) b  in
        let val (b, _Country) = get_option (get_string) b  in
        let val (b, _ApiKeys) = get_option (get_apiKeys) b  in
        let val (b, _Experiments) = get_option (get_list (get_userExperiment)) b  in
        let val (b, _SharingSettings_) = get_option (get_sharingSettings) b  in
        let val (b, _Ex) = get_option (get_userSettingsEx) b  in
          (b,
           { User = _User
           , EditsCount = _EditsCount
           , ScrollMode = _ScrollMode
           , ListViewMode = _ListViewMode
           , ShowFavicons = _ShowFavicons
           , MarkReadMode = _MarkReadMode
           , UltraCompact = _UltraCompact
           , Reserved = _Reserved
           , ExactUnreadCounts = _ExactUnreadCounts
           , PublicFeeds = _PublicFeeds
           , Country = _Country
           , ApiKeys = _ApiKeys
           , Experiments = _Experiments
           , SharingSettings_ = _SharingSettings_
           , Ex = _Ex
           })
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize UserSettings ({[n]} is out of range)</xml>
fun get_uID (b : getBuf) : (getBuf * uID) = 
  let val (b, c) = get_byte b in case c of
      0 => 
        let val (b, _Id) = get_string b  in
          (b,
           EMail
           { Id = _Id
           })
        end
    | 1 => 
        let val (b, _Id) = get_string b  in
          (b,
           Url
           { Id = _Id
           })
        end
    | n => error <xml>Oh, shi -- can’t deserialize UID ({[n]} is out of range)</xml>
  end
fun get_session (b : getBuf) : (getBuf * session) = 
  case 0 of
      0 => 
        let val (b, _Key) = get_string b  in
        let val (b, _Expire) = get_time b  in
        let val (b, _Cleared) = get_bool b  in
        let val (b, _User) = get_string b  in
          (b,
           { Key = _Key
           , Expire = _Expire
           , Cleared = _Cleared
           , User = _User
           })
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize Session ({[n]} is out of range)</xml>
fun get_attachment (b : getBuf) : (getBuf * attachment) = 
  let val (b, c) = get_byte b in case c of
      0 => 
        let val (b, _Url) = get_url b  in
        let val (b, _Width) = get_option (get_int) b  in
        let val (b, _Height) = get_option (get_int) b  in
        let val (b, _Title) = get_option (get_string) b  in
          (b,
           AImage
           { Url = _Url
           , Width = _Width
           , Height = _Height
           , Title = _Title
           })
        end
        end
        end
        end
    | 1 => 
        let val (b, _Url) = get_url b  in
        let val (b, _Mime) = get_string b  in
        let val (b, _FileSize) = get_option (get_int) b  in
        let val (b, _Duration) = get_option (get_int) b  in
        let val (b, _Title) = get_option (get_string) b  in
          (b,
           AAudio
           { Url = _Url
           , Mime = _Mime
           , FileSize = _FileSize
           , Duration = _Duration
           , Title = _Title
           })
        end
        end
        end
        end
        end
    | 2 => 
        let val (b, _Url) = get_url b  in
        let val (b, _Mime) = get_string b  in
        let val (b, _FileSize) = get_option (get_int) b  in
        let val (b, _Duration) = get_option (get_int) b  in
        let val (b, _Title) = get_option (get_string) b  in
        let val (b, _Width) = get_option (get_int) b  in
        let val (b, _Height) = get_option (get_int) b  in
        let val (b, _Poster) = get_option (get_url) b  in
          (b,
           AVideo
           { Url = _Url
           , Mime = _Mime
           , FileSize = _FileSize
           , Duration = _Duration
           , Title = _Title
           , Width = _Width
           , Height = _Height
           , Poster = _Poster
           })
        end
        end
        end
        end
        end
        end
        end
        end
    | 3 => 
        let val (b, _Url) = get_url b  in
        let val (b, _Xml) = get_string b  in
        let val (b, _Duration) = get_option (get_int) b  in
        let val (b, _Title) = get_option (get_string) b  in
          (b,
           AIframe
           { Url = _Url
           , Xml = _Xml
           , Duration = _Duration
           , Title = _Title
           })
        end
        end
        end
        end
    | 4 => 
        let val (b, _Url) = get_url b  in
        let val (b, _Mime) = get_string b  in
        let val (b, _FileSize) = get_option (get_int) b  in
          (b,
           AOther
           { Url = _Url
           , Mime = _Mime
           , FileSize = _FileSize
           })
        end
        end
        end
    | 5 => 
        let val (b, _Feed) = get_url b  in
        let val (b, _Guid) = get_string b  in
        let val (b, _StreamTitle) = get_string b  in
        let val (b, _HtmlUrl) = get_string b  in
          (b,
           AGrOrigin
           { Feed = _Feed
           , Guid = _Guid
           , StreamTitle = _StreamTitle
           , HtmlUrl = _HtmlUrl
           })
        end
        end
        end
        end
    | 6 => 
        let val (b, _Url) = get_url b  in
        let val (b, _Mime) = get_string b  in
        let val (b, _FileSize) = get_option (get_int) b  in
        let val (b, _Duration) = get_option (get_int) b  in
        let val (b, _Title) = get_option (get_string) b  in
        let val (b, _Width) = get_option (get_int) b  in
        let val (b, _Height) = get_option (get_int) b  in
        let val (b, _Poster) = get_option (get_url) b  in
        let val (b, _Loop) = get_bool b  in
          (b,
           AVideo2
           { Url = _Url
           , Mime = _Mime
           , FileSize = _FileSize
           , Duration = _Duration
           , Title = _Title
           , Width = _Width
           , Height = _Height
           , Poster = _Poster
           , Loop = _Loop
           })
        end
        end
        end
        end
        end
        end
        end
        end
        end
    | 7 => 
        let val (b, _Url) = get_url b  in
          (b,
           AThumbnail
           { Url = _Url
           })
        end
    | n => error <xml>Oh, shi -- can’t deserialize Attachment ({[n]} is out of range)</xml>
  end
fun get_msgKey (b : getBuf) : (getBuf * msgKey) = 
  case 0 of
      0 => 
        let val (b, _BlogFeedUrl) = get_string b  in
        let val (b, _PostGuid) = get_option (get_string) b  in
        let val (b, _CommentGuid) = get_option (get_string) b  in
          (b,
           { BlogFeedUrl = _BlogFeedUrl
           , PostGuid = _PostGuid
           , CommentGuid = _CommentGuid
           })
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize MsgKey ({[n]} is out of range)</xml>
fun get_msg (b : getBuf) : (getBuf * msg) = 
  case 0 of
      0 => 
        let val (b, _Key) = get_msgKey b  in
        let val (b, _Attachments) = get_list (get_attachment) b  in
        let val (b, _Author) = get_string b  in
        let val (b, _AuthorUri) = get_option (get_url) b  in
        let val (b, _AuthorEmail) = get_string b  in
        let val (b, _AuthorPic) = get_option (get_url) b  in
        let val (b, _Link) = get_option (get_url) b  in
        let val (b, _Subject) = get_string b  in
        let val (b, _Tags) = get_list (get_string) b  in
        let val (b, _Time) = get_option (get_time) b  in
        let val (b, _DlTime) = get_time b  in
        let val (b, _Text) = get_xbodyString b  in
        let val (b, _ShortText) = get_string b  in
        let val (b, _ShorterText) = get_string b  in
          (b,
           { Key = _Key
           , Attachments = _Attachments
           , Author = _Author
           , AuthorUri = _AuthorUri
           , AuthorEmail = _AuthorEmail
           , AuthorPic = _AuthorPic
           , Link = _Link
           , Subject = _Subject
           , Tags = _Tags
           , Time = _Time
           , DlTime = _DlTime
           , Text = _Text
           , ShortText = _ShortText
           , ShorterText = _ShorterText
           })
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize Msg ({[n]} is out of range)</xml>
fun get_msgHeader (b : getBuf) : (getBuf * msgHeader) = 
  case 0 of
      0 => 
        let val (b, _Guid) = get_string b  in
        let val (b, _ContentHash) = get_string b  in
        let val (b, _Author) = get_string b  in
        let val (b, _AuthorPic) = get_option (get_url) b  in
        let val (b, _Subject) = get_string b  in
        let val (b, _Time) = get_option (get_time) b  in
        let val (b, _DlTime) = get_time b  in
        let val (b, _ShortText) = get_string b  in
          (b,
           { Guid = _Guid
           , ContentHash = _ContentHash
           , Author = _Author
           , AuthorPic = _AuthorPic
           , Subject = _Subject
           , Time = _Time
           , DlTime = _DlTime
           , ShortText = _ShortText
           })
        end
        end
        end
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize MsgHeader ({[n]} is out of range)</xml>
fun get_commentsKey (b : getBuf) : (getBuf * commentsKey) = 
  case 0 of
      0 => 
        let val (b, _BlogFeedUrl) = get_string b  in
        let val (b, _PostGuid) = get_string b  in
          (b,
           { BlogFeedUrl = _BlogFeedUrl
           , PostGuid = _PostGuid
           })
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize CommentsKey ({[n]} is out of range)</xml>
fun get_itemTag (b : getBuf) : (getBuf * itemTag) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, ITStarred)
    | 1 => 
        let val (b, _TagName) = get_string b  in
          (b,
           ITTag
           { TagName = _TagName
           })
        end
    | n => error <xml>Oh, shi -- can’t deserialize ItemTag ({[n]} is out of range)</xml>
  end
fun get_filterQueryRpc (b : getBuf) : (getBuf * filterQueryRpc) = 
  case 0 of
      0 => 
        let val (b, _Query) = get_string b  in
        let val (b, _Negate) = get_bool b  in
        let val (b, _FeedGRIds) = get_list (get_int) b  in
          (b,
           { Query = _Query
           , Negate = _Negate
           , FeedGRIds = _FeedGRIds
           })
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize FilterQueryRpc ({[n]} is out of range)</xml>
fun get_searchError (b : getBuf) : (getBuf * searchError) = 
  let val (b, c) = get_byte b in case c of
      0 => 
        let val (b, _ErrorMessage) = get_string b  in
          (b,
           SESyntaxError
           { ErrorMessage = _ErrorMessage
           })
        end
    | 1 => 
        let val (b, _ErrorMessage) = get_string b  in
          (b,
           SESystemError
           { ErrorMessage = _ErrorMessage
           })
        end
    | n => error <xml>Oh, shi -- can’t deserialize SearchError ({[n]} is out of range)</xml>
  end
fun get_apiMode (b : getBuf) : (getBuf * apiMode) = 
  let val (b, c) = get_byte b in case c of
      0 => 
        let val (b, _HostName) = get_string b  in
        let val (b, _AcceptLanguage) = get_string b  in
          (b,
           AMNormal
           { HostName = _HostName
           , AcceptLanguage = _AcceptLanguage
           })
        end
        end
    | 1 => 
        let val (b, _Fetch) = get_bool b  in
        let val (b, _Count) = get_int b  in
        let val (b, _Continuation) = get_option (get_option (get_msgKey)) b  in
        let val (b, _MinDlTime) = get_option (get_time) b  in
        let val (b, _MaxDlTime) = get_option (get_time) b  in
        let val (b, _MaxTime) = get_option (get_time) b  in
        let val (b, _ExcludeTags) = get_list (get_itemTag) b  in
        let val (b, _IncludeTags) = get_list (get_itemTag) b  in
        let val (b, _ReadOnly) = get_bool b  in
        let val (b, _MsgLinkParams) = get_list ((fn (b : getBuf) =>let val (b, _1) = get_string b  in
        let val (b, _2) = get_string b  in
          (b,(_1, _2))
        end
        end
        )) b  in
        let val (b, _FromUI) = get_bool b  in
        let val (b, _MaxMsgTextLength) = get_option (get_int) b  in
          (b,
           AMGRIdsOnly
           { Fetch = _Fetch
           , Count = _Count
           , Continuation = _Continuation
           , MinDlTime = _MinDlTime
           , MaxDlTime = _MaxDlTime
           , MaxTime = _MaxTime
           , ExcludeTags = _ExcludeTags
           , IncludeTags = _IncludeTags
           , ReadOnly = _ReadOnly
           , MsgLinkParams = _MsgLinkParams
           , FromUI = _FromUI
           , MaxMsgTextLength = _MaxMsgTextLength
           })
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
    | 2 => 
        let val (b, _HostName) = get_string b  in
        let val (b, _AcceptLanguage) = get_string b  in
        let val (b, _Url) = get_string b  in
          (b,
           AMDiscovery
           { HostName = _HostName
           , AcceptLanguage = _AcceptLanguage
           , Url = _Url
           })
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize ApiMode ({[n]} is out of range)</xml>
  end
fun get_msgTreePoint (b : getBuf) : (getBuf * msgTreePoint) = 
  case 0 of
      0 => 
        let val (b, _ParentId) = get_int b  in
        let val (b, _Time) = get_time b  in
        let val (b, _Id) = get_int b  in
          (b,
           { ParentId = _ParentId
           , Time = _Time
           , Id = _Id
           })
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize MsgTreePoint ({[n]} is out of range)</xml>
fun get_postsReq (b : getBuf) : (getBuf * postsReq) = 
  case 0 of
      0 => 
        let val (b, _FeedId) = get_int b  in
        let val (b, _MsgTreePoint) = get_msgTreePoint b  in
        let val (b, _TotalPosts) = get_int b  in
        let val (b, _TotalComments) = get_int b  in
          (b,
           { FeedId = _FeedId
           , MsgTreePoint = _MsgTreePoint
           , TotalPosts = _TotalPosts
           , TotalComments = _TotalComments
           })
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize PostsReq ({[n]} is out of range)</xml>
fun get_commentsReq (b : getBuf) : (getBuf * commentsReq) = 
  case 0 of
      0 => 
        let val (b, _Key) = get_commentsKey b  in
        let val (b, _PostId) = get_int b  in
        let val (b, _MsgTreePoint) = get_msgTreePoint b  in
        let val (b, _TotalComments) = get_int b  in
          (b,
           { Key = _Key
           , PostId = _PostId
           , MsgTreePoint = _MsgTreePoint
           , TotalComments = _TotalComments
           })
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize CommentsReq ({[n]} is out of range)</xml>
fun get_msgId (b : getBuf) : (getBuf * msgId) = 
  case 0 of
      0 => 
        let val (b, _FeedId) = get_int b  in
        let val (b, _PostId) = get_int b  in
        let val (b, _CommentId) = get_option (get_int) b  in
          (b,
           { FeedId = _FeedId
           , PostId = _PostId
           , CommentId = _CommentId
           })
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize MsgId ({[n]} is out of range)</xml>
fun get_longMsgId (b : getBuf) : (getBuf * longMsgId) = 
  case 0 of
      0 => 
        let val (b, _MsgKey) = get_msgKey b  in
        let val (b, _MsgId) = get_msgId b  in
          (b,
           { MsgKey = _MsgKey
           , MsgId = _MsgId
           })
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize LongMsgId ({[n]} is out of range)</xml>
fun get_treeReq (b : getBuf) : (getBuf * treeReq) = 
  let val (b, c) = get_byte b in case c of
      0 => 
        let val (b, _Reqs) = get_list (get_postsReq) b  in
          (b,
           TRPosts
           { Reqs = _Reqs
           })
        end
    | 1 => 
        let val (b, _Tags) = get_option (get_list (get_itemTag)) b  in
        let val (b, _MaxTag) = get_option ((fn (b : getBuf) =>let val (b, _1) = get_time b  in
        let val (b, _2) = get_msgId b  in
          (b,(_1, _2))
        end
        end
        )) b  in
        let val (b, _LastMsg) = get_option ((fn (b : getBuf) =>let val (b, _1) = get_time b  in
        let val (b, _2) = get_msgId b  in
          (b,(_1, _2))
        end
        end
        )) b  in
          (b,
           TRTags
           { Tags = _Tags
           , MaxTag = _MaxTag
           , LastMsg = _LastMsg
           })
        end
        end
        end
    | 2 => 
        let val (b, _OnExpand) = get_bool b  in
        let val (b, _Req) = get_commentsReq b  in
          (b,
           TRComments
           { OnExpand = _OnExpand
           , Req = _Req
           })
        end
        end
    | 3 => 
        let val (b, _OnExpand) = get_bool b  in
        let val (b, _StreamName) = get_string b  in
        let val (b, _Req) = get_commentsReq b  in
          (b,
           TRCommentsS
           { OnExpand = _OnExpand
           , StreamName = _StreamName
           , Req = _Req
           })
        end
        end
        end
    | 4 => 
        let val (b, _StreamName) = get_string b  in
        let val (b, _Reqs) = get_list (get_postsReq) b  in
          (b,
           TRSmartStream
           { StreamName = _StreamName
           , Reqs = _Reqs
           })
        end
        end
    | 5 => 
        let val (b, _Query) = get_string b  in
        let val (b, _FeedMasksKey) = get_string b  in
        let val (b, _Reqs) = get_list (get_postsReq) b  in
          (b,
           TRSearchPosts
           { Query = _Query
           , FeedMasksKey = _FeedMasksKey
           , Reqs = _Reqs
           })
        end
        end
        end
    | 6 => 
        let val (b, _StreamName) = get_string b  in
        let val (b, _Query) = get_string b  in
        let val (b, _FeedMasksKey) = get_string b  in
        let val (b, _Reqs) = get_list (get_postsReq) b  in
          (b,
           TRSearchSmartStream
           { StreamName = _StreamName
           , Query = _Query
           , FeedMasksKey = _FeedMasksKey
           , Reqs = _Reqs
           })
        end
        end
        end
        end
    | 7 => 
        let val (b, _Query) = get_string b  in
        let val (b, _IdsKey) = get_string b  in
        let val (b, _Tags) = get_option (get_list (get_itemTag)) b  in
        let val (b, _MaxTag) = get_option ((fn (b : getBuf) =>let val (b, _1) = get_time b  in
        let val (b, _2) = get_msgId b  in
          (b,(_1, _2))
        end
        end
        )) b  in
        let val (b, _LastMsg) = get_option ((fn (b : getBuf) =>let val (b, _1) = get_time b  in
        let val (b, _2) = get_msgId b  in
          (b,(_1, _2))
        end
        end
        )) b  in
          (b,
           TRSearchTags
           { Query = _Query
           , IdsKey = _IdsKey
           , Tags = _Tags
           , MaxTag = _MaxTag
           , LastMsg = _LastMsg
           })
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize TreeReq ({[n]} is out of range)</xml>
  end
fun get_msgView (b : getBuf) : (getBuf * msgView) = 
  let val (b, c) = get_byte b in case c of
      0 => 
        let val (b, _Msg) = get_msg b  in
          (b,
           MVFull
           { Msg = _Msg
           })
        end
    | 1 => 
        let val (b, _Header) = get_msgHeader b  in
        let val (b, _CachedMsg) = get_option (get_msg) b  in
          (b,
           MVShort
           { Header = _Header
           , CachedMsg = _CachedMsg
           })
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize MsgView ({[n]} is out of range)</xml>
  end
fun get_msgItem (b : getBuf) : (getBuf * msgItem) = 
  case 0 of
      0 => 
        let val (b, _MsgView) = get_msgView b  in
        let val (b, _MsgKey) = get_msgKey b  in
        let val (b, _MsgId) = get_msgId b  in
        let val (b, _Read) = get_bool b  in
        let val (b, _Tags) = get_list (get_itemTag) b  in
        let val (b, _SmartStreams) = get_list (get_int) b  in
        let val (b, _ReadLocked) = get_bool b  in
        let val (b, _Full) = get_bool b  in
        let val (b, _SearchResult) = get_bool b  in
          (b,
           { MsgView = _MsgView
           , MsgKey = _MsgKey
           , MsgId = _MsgId
           , Read = _Read
           , Tags = _Tags
           , SmartStreams = _SmartStreams
           , ReadLocked = _ReadLocked
           , Full = _Full
           , SearchResult = _SearchResult
           })
        end
        end
        end
        end
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize MsgItem ({[n]} is out of range)</xml>
fun get_msgForest (b : getBuf) : (getBuf * msgForest) = 
  case 0 of
      0 => 
        let val (b, _TotalCount) = get_int b  in
        let val (b, _UnreadCount) = get_int b  in
        let val (b, _TotalResultsCount) = get_int b  in
        let val (b, _UnreadResultsCount) = get_int b  in
        let val (b, _SmartStreamUnreadCounts) = get_list ((fn (b : getBuf) =>let val (b, _1) = get_int b  in
        let val (b, _2) = get_int b  in
          (b,(_1, _2))
        end
        end
        )) b  in
        let val (b, _SmartStreamUnreadResultCounts) = get_list ((fn (b : getBuf) =>let val (b, _1) = get_int b  in
        let val (b, _2) = get_int b  in
          (b,(_1, _2))
        end
        end
        )) b  in
        let val (b, _TagTotalCounts) = get_list ((fn (b : getBuf) =>let val (b, _1) = get_option (get_itemTag) b  in
        let val (b, _2) = get_int b  in
          (b,(_1, _2))
        end
        end
        )) b  in
        let val (b, _TagUnreadCounts) = get_list ((fn (b : getBuf) =>let val (b, _1) = get_option (get_itemTag) b  in
        let val (b, _2) = get_int b  in
          (b,(_1, _2))
        end
        end
        )) b  in
        let val (b, _TagUnreadResultCounts) = get_list ((fn (b : getBuf) =>let val (b, _1) = get_option (get_itemTag) b  in
        let val (b, _2) = get_int b  in
          (b,(_1, _2))
        end
        end
        )) b  in
        let val (b, _List) = get_list ((fn (b : getBuf) =>let val (b, _1) = get_msgItem b  in
        let val (b, _2) = get_msgForest b  in
          (b,(_1, _2))
        end
        end
        )) b  in
        let val (b, _NextReq) = get_option (get_treeReq) b  in
          (b,
           MsgForest
           { TotalCount = _TotalCount
           , UnreadCount = _UnreadCount
           , TotalResultsCount = _TotalResultsCount
           , UnreadResultsCount = _UnreadResultsCount
           , SmartStreamUnreadCounts = _SmartStreamUnreadCounts
           , SmartStreamUnreadResultCounts = _SmartStreamUnreadResultCounts
           , TagTotalCounts = _TagTotalCounts
           , TagUnreadCounts = _TagUnreadCounts
           , TagUnreadResultCounts = _TagUnreadResultCounts
           , List = _List
           , NextReq = _NextReq
           })
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize MsgForest ({[n]} is out of range)</xml>
fun get_externalLoginType (b : getBuf) : (getBuf * externalLoginType) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, Google)
    | 1 => 
          (b, Facebook)
    | 2 => 
          (b, Twitter)
    | 3 => 
        let val (b, _URL) = get_string b  in
          (b,
           OpenId
           { URL = _URL
           })
        end
    | n => error <xml>Oh, shi -- can’t deserialize ExternalLoginType ({[n]} is out of range)</xml>
  end
fun get_externalLoginAction (b : getBuf) : (getBuf * externalLoginAction) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, ELALogin)
    | 1 => 
        let val (b, _URL) = get_string b  in
          (b,
           ELAAddUrl
           { URL = _URL
           })
        end
    | 2 => 
          (b, ELAAddAssociatedAccount)
    | n => error <xml>Oh, shi -- can’t deserialize ExternalLoginAction ({[n]} is out of range)</xml>
  end
fun get_counters (b : getBuf) : (getBuf * counters) = 
  case 0 of
      0 => 
        let val (b, _ReadPosts) = get_int b  in
        let val (b, _ReadComments) = get_int b  in
        let val (b, _TotalPosts) = get_int b  in
        let val (b, _TotalComments) = get_int b  in
        let val (b, _Scanning) = get_int b  in
        let val (b, _ScanningComments) = get_int b  in
        let val (b, _Error) = get_int b  in
        let val (b, _Feed) = get_int b  in
        let val (b, _ScannedPercent) = get_int b  in
          (b,
           { ReadPosts = _ReadPosts
           , ReadComments = _ReadComments
           , TotalPosts = _TotalPosts
           , TotalComments = _TotalComments
           , Scanning = _Scanning
           , ScanningComments = _ScanningComments
           , Error = _Error
           , Feed = _Feed
           , ScannedPercent = _ScannedPercent
           })
        end
        end
        end
        end
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize Counters ({[n]} is out of range)</xml>
fun get_subItemType (b : getBuf) : (getBuf * subItemType) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, SITAll)
    | 1 => 
        let val (b, _Query) = get_string b  in
          (b,
           SITSearch
           { Query = _Query
           })
        end
    | 2 => 
        let val (b, _Folder) = get_string b  in
          (b,
           SITFolder
           { Folder = _Folder
           })
        end
    | 3 => 
        let val (b, _Subscription) = get_subscription b  in
        let val (b, _FeedLink) = get_option (get_string) b  in
        let val (b, _PointAllDesc) = get_option (get_msgTreePoint) b  in
          (b,
           SITFeed
           { Subscription = _Subscription
           , FeedLink = _FeedLink
           , PointAllDesc = _PointAllDesc
           })
        end
        end
        end
    | 4 => 
        let val (b, _TagName) = get_string b  in
          (b,
           SITTag
           { TagName = _TagName
           })
        end
    | 5 => 
        let val (b, _StreamName) = get_string b  in
        let val (b, _StreamFeedSirs) = get_list (get_int) b  in
          (b,
           SITSmartStream
           { StreamName = _StreamName
           , StreamFeedSirs = _StreamFeedSirs
           })
        end
        end
    | 6 => 
          (b, SITStarred)
    | 7 => 
          (b, SITAllTags)
    | n => error <xml>Oh, shi -- can’t deserialize SubItemType ({[n]} is out of range)</xml>
  end
fun get_subItemRpc (b : getBuf) : (getBuf * subItemRpc) = 
  case 0 of
      0 => 
        let val (b, _Path) = get_string b  in
        let val (b, _Index) = get_int b  in
        let val (b, _Title) = get_string b  in
        let val (b, _SIType) = get_subItemType b  in
        let val (b, _Counters) = get_counters b  in
        let val (b, _ViewMode) = get_msgTreeViewMode b  in
        let val (b, _ParentFolders) = get_list (get_int) b  in
        let val (b, _DomIds) = get_list (get_int) b  in
        let val (b, _FaviconStyle) = get_option (get_string) b  in
        let val (b, _GRId) = get_int b  in
          (b,
           { Path = _Path
           , Index = _Index
           , Title = _Title
           , SIType = _SIType
           , Counters = _Counters
           , ViewMode = _ViewMode
           , ParentFolders = _ParentFolders
           , DomIds = _DomIds
           , FaviconStyle = _FaviconStyle
           , GRId = _GRId
           })
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize SubItemRpc ({[n]} is out of range)</xml>
fun get_welcomeState (b : getBuf) : (getBuf * welcomeState) = 
  case 0 of
      0 => 
        let val (b, _HasPrevAccount) = get_bool b  in
        let val (b, _HasPrevSubs) = get_bool b  in
        let val (b, _StarredRestored) = get_bool b  in
        let val (b, _TaggedRestored) = get_bool b  in
          (b,
           { HasPrevAccount = _HasPrevAccount
           , HasPrevSubs = _HasPrevSubs
           , StarredRestored = _StarredRestored
           , TaggedRestored = _TaggedRestored
           })
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize WelcomeState ({[n]} is out of range)</xml>
fun get_updateFilters (b : getBuf) : (getBuf * updateFilters) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, UFNone)
    | 1 => 
          (b, UFChanged)
    | 2 => 
          (b, UFAll)
    | n => error <xml>Oh, shi -- can’t deserialize UpdateFilters ({[n]} is out of range)</xml>
  end
fun get_browserType (b : getBuf) : (getBuf * browserType) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, BTUnknown)
    | 1 => 
          (b, BTAndroid)
    | 2 => 
          (b, BTIPhone)
    | 3 => 
          (b, BTIPad)
    | 4 => 
          (b, BTIPod)
    | 5 => 
          (b, BTChrome)
    | 6 => 
          (b, BTIE)
    | 7 => 
          (b, BTIEMobile)
    | 8 => 
          (b, BTSafari)
    | 9 => 
          (b, BTOpera)
    | 10 => 
          (b, BTOperaMini)
    | 11 => 
          (b, BTFirefox)
    | 12 => 
          (b, BTVivaldi)
    | 13 => 
          (b, BTEdge)
    | n => error <xml>Oh, shi -- can’t deserialize BrowserType ({[n]} is out of range)</xml>
  end
fun get_appType (b : getBuf) : (getBuf * appType) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, ATUnknown)
    | 1 => 
          (b, ATFeeddler)
    | 2 => 
          (b, ATMrReader)
    | 3 => 
          (b, ATReeder)
    | 4 => 
          (b, ATSlowFeeds)
    | 5 => 
          (b, ATJustReader)
    | 6 => 
          (b, ATNewsPlus)
    | 7 => 
          (b, ATPress)
    | 8 => 
          (b, ATVienna)
    | 9 => 
          (b, ATReadKit)
    | 10 => 
          (b, ATNewsJet)
    | 11 => 
          (b, ATAmber)
    | 12 => 
          (b, ATgzip)
    | 13 => 
          (b, ATUnread)
    | 14 => 
          (b, ATFeedMe)
    | 15 => 
          (b, ATFieryFeeds)
    | 16 => 
          (b, ATLire)
    | 17 => 
          (b, ATWebSubscriber)
    | 18 => 
          (b, ATReadably)
    | 19 => 
          (b, ATokhttp)
    | 20 => 
          (b, ATFluentReader)
    | 21 => 
          (b, ATRavenReader)
    | 22 => 
          (b, ATFocusReader)
    | 23 => 
          (b, ATNetNewsWire)
    | n => error <xml>Oh, shi -- can’t deserialize AppType ({[n]} is out of range)</xml>
  end
fun get_operatingSystem (b : getBuf) : (getBuf * operatingSystem) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, OSUnknown)
    | 1 => 
          (b, OSWindows)
    | 2 => 
          (b, OSMac)
    | 3 => 
          (b, OSLinux)
    | 4 => 
          (b, OSAndroid)
    | 5 => 
          (b, OSIOS)
    | 6 => 
          (b, OSChromeOS)
    | n => error <xml>Oh, shi -- can’t deserialize OperatingSystem ({[n]} is out of range)</xml>
  end
fun get_usageFlag (b : getBuf) : (getBuf * usageFlag) = 
  let val (b, c) = get_byte b in case c of
      0 => 
        let val (b, _BrowserType) = get_browserType b  in
        let val (b, _OperatingSystem) = get_operatingSystem b  in
          (b,
           UFWeb
           { BrowserType = _BrowserType
           , OperatingSystem = _OperatingSystem
           })
        end
        end
    | 1 => 
        let val (b, _AppType) = get_appType b  in
        let val (b, _OperatingSystem) = get_operatingSystem b  in
          (b,
           UFApp
           { AppType = _AppType
           , OperatingSystem = _OperatingSystem
           })
        end
        end
    | 2 => 
        let val (b, _ShareAction) = get_shareAction b  in
          (b,
           UFShareAction
           { ShareAction = _ShareAction
           })
        end
    | 3 => 
          (b, UFOPML)
    | 4 => 
          (b, UFAddSubscription)
    | 5 => 
          (b, UFSearchSubscriptions)
    | 6 => 
          (b, UFDiscoverySubscription)
    | 7 => 
          (b, UFAddDiscoverySubscription)
    | 8 => 
          (b, UFUnsubscribe)
    | 9 => 
          (b, UFRetrySubscription)
    | 10 => 
          (b, UFRenameSubscription)
    | 11 => 
          (b, UFRenameFolder)
    | 12 => 
          (b, UFEditSubscriptionFolders)
    | 13 => 
          (b, UFDragAndDrop)
    | 14 => 
          (b, UFSearch)
    | 15 => 
          (b, UFSearchTags)
    | 16 => 
          (b, UFSkip)
    | 17 => 
          (b, UFIgnore)
    | 18 => 
          (b, UFKeepUnread)
    | 19 => 
          (b, UFMarkAllAsRead)
    | 20 => 
          (b, UFStar)
    | 21 => 
          (b, UFTag)
    | 22 => 
          (b, UFReadability)
    | 23 => 
          (b, UFSetUsername)
    | 24 => 
          (b, UFEnablePublicFeed)
    | 25 => 
          (b, UFDisablePublicFeed)
    | 26 => 
          (b, UFGenerateNewPublicFeed)
    | 27 => 
          (b, UFDeleteAccount)
    | 28 => 
          (b, UFExportOPML)
    | 29 => 
        let val (b, _OlderThan) = get_int b  in
          (b,
           UFMarkAllAsReadD
           { OlderThan = _OlderThan
           })
        end
    | 30 => 
        let val (b, _OlderThan) = get_int b  in
          (b,
           UFMarkSearchAsReadD
           { OlderThan = _OlderThan
           })
        end
    | 31 => 
          (b, UFFilterApply)
    | 32 => 
          (b, UFFilterHide)
    | 33 => 
          (b, UFNewSmartStream)
    | 34 => 
          (b, UFEditFilter)
    | 35 => 
          (b, UFEditSmartStream)
    | 36 => 
          (b, UFDeleteFilter)
    | 37 => 
          (b, UFDeleteSmartStream)
    | 38 => 
        let val (b, _Time) = get_time b  in
          (b,
           UFWhatsNewClick
           { Time = _Time
           })
        end
    | 39 => 
        let val (b, _Time) = get_time b  in
          (b,
           UFWhatsNewClose
           { Time = _Time
           })
        end
    | 40 => 
        let val (b, _ThemeName) = get_string b  in
          (b,
           UFThemeChange
           { ThemeName = _ThemeName
           })
        end
    | 41 => 
        let val (b, _FontName) = get_string b  in
          (b,
           UFFontChange
           { FontName = _FontName
           })
        end
    | 42 => 
        let val (b, _Size) = get_int b  in
          (b,
           UFFontSizeChange
           { Size = _Size
           })
        end
    | 43 => 
        let val (b, _Pixels) = get_int b  in
        let val (b, _FontSize) = get_int b  in
          (b,
           UFLineHeightChange
           { Pixels = _Pixels
           , FontSize = _FontSize
           })
        end
        end
    | 44 => 
          (b, UFSetPassword)
    | 45 => 
          (b, UFSetEmail)
    | 46 => 
          (b, UFMarkReadAbove)
    | 47 => 
          (b, UFMarkReadBelow)
    | 48 => 
          (b, UFUnstarAbove)
    | 49 => 
          (b, UFUnstarBelow)
    | 50 => 
          (b, UFUntagAbove)
    | 51 => 
          (b, UFUntagBelow)
    | n => error <xml>Oh, shi -- can’t deserialize UsageFlag ({[n]} is out of range)</xml>
  end
fun get_markReq (b : getBuf) : (getBuf * markReq) = 
  let val (b, c) = get_byte b in case c of
      0 => 
        let val (b, _FeedTcs) = get_list ((fn (b : getBuf) =>let val (b, _1) = get_int b  in
        let val (b, _2) = (fn (b : getBuf) =>let val (b, _1) = get_int b  in
        let val (b, _2) = get_int b  in
          (b,(_1, _2))
        end
        end
        ) b  in
          (b,(_1, _2))
        end
        end
        )) b  in
          (b,
           MRPosts
           { FeedTcs = _FeedTcs
           })
        end
    | 1 => 
        let val (b, _Tags) = get_option (get_list (get_itemTag)) b  in
        let val (b, _MaxTag) = get_option ((fn (b : getBuf) =>let val (b, _1) = get_time b  in
        let val (b, _2) = get_msgId b  in
          (b,(_1, _2))
        end
        end
        )) b  in
          (b,
           MRTags
           { Tags = _Tags
           , MaxTag = _MaxTag
           })
        end
        end
    | 2 => 
        let val (b, _StreamName) = get_string b  in
        let val (b, _FeedTcs) = get_list ((fn (b : getBuf) =>let val (b, _1) = get_int b  in
        let val (b, _2) = (fn (b : getBuf) =>let val (b, _1) = get_int b  in
        let val (b, _2) = get_int b  in
          (b,(_1, _2))
        end
        end
        ) b  in
          (b,(_1, _2))
        end
        end
        )) b  in
          (b,
           MRSmartStream
           { StreamName = _StreamName
           , FeedTcs = _FeedTcs
           })
        end
        end
    | 3 => 
        let val (b, _Query) = get_string b  in
        let val (b, _FeedMasksKey) = get_string b  in
        let val (b, _FeedTcs) = get_list ((fn (b : getBuf) =>let val (b, _1) = get_int b  in
        let val (b, _2) = (fn (b : getBuf) =>let val (b, _1) = get_int b  in
        let val (b, _2) = get_int b  in
          (b,(_1, _2))
        end
        end
        ) b  in
          (b,(_1, _2))
        end
        end
        )) b  in
          (b,
           MRSearchPosts
           { Query = _Query
           , FeedMasksKey = _FeedMasksKey
           , FeedTcs = _FeedTcs
           })
        end
        end
        end
    | 4 => 
        let val (b, _Query) = get_string b  in
        let val (b, _IdsKey) = get_string b  in
        let val (b, _Tags) = get_option (get_list (get_itemTag)) b  in
        let val (b, _MaxTag) = get_option ((fn (b : getBuf) =>let val (b, _1) = get_time b  in
        let val (b, _2) = get_msgId b  in
          (b,(_1, _2))
        end
        end
        )) b  in
          (b,
           MRSearchTags
           { Query = _Query
           , IdsKey = _IdsKey
           , Tags = _Tags
           , MaxTag = _MaxTag
           })
        end
        end
        end
        end
    | 5 => 
        let val (b, _StreamName) = get_string b  in
        let val (b, _Query) = get_string b  in
        let val (b, _FeedMasksKey) = get_string b  in
        let val (b, _FeedTcs) = get_list ((fn (b : getBuf) =>let val (b, _1) = get_int b  in
        let val (b, _2) = (fn (b : getBuf) =>let val (b, _1) = get_int b  in
        let val (b, _2) = get_int b  in
          (b,(_1, _2))
        end
        end
        ) b  in
          (b,(_1, _2))
        end
        end
        )) b  in
          (b,
           MRSearchSmartStream
           { StreamName = _StreamName
           , Query = _Query
           , FeedMasksKey = _FeedMasksKey
           , FeedTcs = _FeedTcs
           })
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize MarkReq ({[n]} is out of range)</xml>
  end
fun get_markReadDirection (b : getBuf) : (getBuf * markReadDirection) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, MRDAll)
    | 1 => 
        let val (b, _Point) = (fn (b : getBuf) =>let val (b, _1) = get_time b  in
        let val (b, _2) = (fn (b : getBuf) =>let val (b, _1) = get_bool b  in
        let val (b, _2) = get_msgId b  in
          (b,(_1, _2))
        end
        end
        ) b  in
        let val (b, _3) = (fn (b : getBuf) =>let val (b, _1) = get_bool b  in
        let val (b, _2) = get_msgId b  in
          (b,(_1, _2))
        end
        end
        ) b  in
          (b,(_1, _2, _3))
        end
        end
        end
        ) b  in
          (b,
           MRDAbove
           { Point = _Point
           })
        end
    | 2 => 
        let val (b, _Point) = (fn (b : getBuf) =>let val (b, _1) = get_time b  in
        let val (b, _2) = (fn (b : getBuf) =>let val (b, _1) = get_bool b  in
        let val (b, _2) = get_msgId b  in
          (b,(_1, _2))
        end
        end
        ) b  in
        let val (b, _3) = (fn (b : getBuf) =>let val (b, _1) = get_bool b  in
        let val (b, _2) = get_msgId b  in
          (b,(_1, _2))
        end
        end
        ) b  in
          (b,(_1, _2, _3))
        end
        end
        end
        ) b  in
          (b,
           MRDBelow
           { Point = _Point
           })
        end
    | n => error <xml>Oh, shi -- can’t deserialize MarkReadDirection ({[n]} is out of range)</xml>
  end
fun get_bgAction (b : getBuf) : (getBuf * bgAction) = 
  let val (b, c) = get_byte b in case c of
      0 => 
        let val (b, _MsgId) = get_msgId b  in
        let val (b, _Read) = get_bool b  in
        let val (b, _TotalComments) = get_int b  in
          (b,
           BGMarkMsgRead
           { MsgId = _MsgId
           , Read = _Read
           , TotalComments = _TotalComments
           })
        end
        end
        end
    | 1 => 
        let val (b, _LongMsgId) = get_longMsgId b  in
        let val (b, _Tag) = get_itemTag b  in
          (b,
           BGAddTag
           { LongMsgId = _LongMsgId
           , Tag = _Tag
           })
        end
        end
    | 2 => 
        let val (b, _LongMsgId) = get_longMsgId b  in
        let val (b, _Tag) = get_itemTag b  in
          (b,
           BGRemoveTag
           { LongMsgId = _LongMsgId
           , Tag = _Tag
           })
        end
        end
    | 3 => 
        let val (b, _MsgId) = get_msgId b  in
        let val (b, _TotalComments) = get_int b  in
          (b,
           BGSkipComments
           { MsgId = _MsgId
           , TotalComments = _TotalComments
           })
        end
        end
    | 4 => 
        let val (b, _MsgId) = get_msgId b  in
        let val (b, _TotalComments) = get_int b  in
          (b,
           BGIgnorePost
           { MsgId = _MsgId
           , TotalComments = _TotalComments
           })
        end
        end
    | 5 => 
        let val (b, _Direction) = get_markReadDirection b  in
        let val (b, _OlderThan) = get_int b  in
        let val (b, _ViewMode) = get_msgTreeViewMode b  in
        let val (b, _Posts) = get_list ((fn (b : getBuf) =>let val (b, _1) = get_time b  in
        let val (b, _2) = get_msgId b  in
          (b,(_1, _2))
        end
        end
        )) b  in
        let val (b, _MarkReq) = get_markReq b  in
          (b,
           BGMarkRead
           { Direction = _Direction
           , OlderThan = _OlderThan
           , ViewMode = _ViewMode
           , Posts = _Posts
           , MarkReq = _MarkReq
           })
        end
        end
        end
        end
        end
    | 6 => 
        let val (b, _Above) = get_bool b  in
        let val (b, _Tags) = get_option (get_list (get_itemTag)) b  in
        let val (b, _ViewMode) = get_msgTreeViewMode b  in
        let val (b, _TreeReqs) = get_list (get_treeReq) b  in
          (b,
           BGRemoveTagFromTree
           { Above = _Above
           , Tags = _Tags
           , ViewMode = _ViewMode
           , TreeReqs = _TreeReqs
           })
        end
        end
        end
        end
    | 7 => 
        let val (b, _Tags) = get_option (get_list (get_itemTag)) b  in
        let val (b, _OlderThan) = get_int b  in
          (b,
           BGRemoveTagD
           { Tags = _Tags
           , OlderThan = _OlderThan
           })
        end
        end
    | 8 => 
        let val (b, _Value) = get_bool b  in
          (b,
           BGSetOnlyUpdatedSubscriptions
           { Value = _Value
           })
        end
    | 9 => 
        let val (b, _Folder) = get_string b  in
        let val (b, _ViewMode) = get_msgTreeViewMode b  in
          (b,
           BGSetFolderViewMode
           { Folder = _Folder
           , ViewMode = _ViewMode
           })
        end
        end
    | 10 => 
        let val (b, _Url) = get_string b  in
        let val (b, _ViewMode) = get_msgTreeViewMode b  in
          (b,
           BGSetSubscriptionViewMode
           { Url = _Url
           , ViewMode = _ViewMode
           })
        end
        end
    | 11 => 
          (b, BGClearAllSubscriptions)
    | 12 => 
        let val (b, _Query) = get_string b  in
          (b,
           BGSaveFilterQuery
           { Query = _Query
           })
        end
    | 13 => 
        let val (b, _ScrollMode) = get_scrollMode b  in
          (b,
           BGSetScrollMode
           { ScrollMode = _ScrollMode
           })
        end
    | 14 => 
        let val (b, _ListViewMode) = get_listViewMode b  in
          (b,
           BGSetListViewMode
           { ListViewMode = _ListViewMode
           })
        end
    | 15 => 
        let val (b, _MarkReadMode) = get_markReadMode b  in
          (b,
           BGSetMarkReadMode
           { MarkReadMode = _MarkReadMode
           })
        end
    | 16 => 
        let val (b, _UltraCompact) = get_bool b  in
          (b,
           BGSetUltraCompact
           { UltraCompact = _UltraCompact
           })
        end
    | 17 => 
        let val (b, _What) = get_subItemType b  in
        let val (b, _InsertAfter) = get_option (get_subItemType) b  in
        let val (b, _SourceFolder) = get_option (get_string) b  in
        let val (b, _TargetFolder) = get_option (get_string) b  in
          (b,
           BGDragAndDrop
           { What = _What
           , InsertAfter = _InsertAfter
           , SourceFolder = _SourceFolder
           , TargetFolder = _TargetFolder
           })
        end
        end
        end
        end
    | 18 => 
        let val (b, _Value) = get_bool b  in
          (b,
           BGSetExactUnreadCounts
           { Value = _Value
           })
        end
    | 19 => 
          (b, BGSortAllFeedsAndFolders)
    | 20 => 
        let val (b, _Folder) = get_string b  in
          (b,
           BGSortFolder
           { Folder = _Folder
           })
        end
    | 21 => 
          (b, BGSortTags)
    | 22 => 
        let val (b, _ShareAction) = get_shareAction b  in
          (b,
           BGShareAction
           { ShareAction = _ShareAction
           })
        end
    | 23 => 
        let val (b, _Country) = get_string b  in
          (b,
           BGSetCountry
           { Country = _Country
           })
        end
    | 24 => 
        let val (b, _Time) = get_time b  in
          (b,
           BGWhatsNewClick
           { Time = _Time
           })
        end
    | 25 => 
        let val (b, _Time) = get_time b  in
          (b,
           BGWhatsNewClose
           { Time = _Time
           })
        end
    | n => error <xml>Oh, shi -- can’t deserialize BgAction ({[n]} is out of range)</xml>
  end
fun get_feedsOrDiscovery (b : getBuf) : (getBuf * feedsOrDiscovery) = 
  let val (b, c) = get_byte b in case c of
      0 => 
        let val (b, _ReadCounters) = get_list ((fn (b : getBuf) =>let val (b, _1) = get_int b  in
        let val (b, _2) = get_int b  in
        let val (b, _3) = get_int b  in
        let val (b, _4) = get_int b  in
        let val (b, _5) = get_int b  in
          (b,(_1, _2, _3, _4, _5))
        end
        end
        end
        end
        end
        )) b  in
          (b,
           FODFeeds
           { ReadCounters = _ReadCounters
           })
        end
    | 1 => 
        let val (b, _APIMode) = get_apiMode b  in
        let val (b, _Feeds) = get_list (get_string) b  in
          (b,
           FODFeedsApi
           { APIMode = _APIMode
           , Feeds = _Feeds
           })
        end
        end
    | 2 => 
        let val (b, _Url) = get_string b  in
          (b,
           FODDiscovery
           { Url = _Url
           })
        end
    | n => error <xml>Oh, shi -- can’t deserialize FeedsOrDiscovery ({[n]} is out of range)</xml>
  end
fun get_filterResults (b : getBuf) : (getBuf * filterResults) = 
  case 0 of
      0 => 
        let val (b, _TotalPosts) = get_int b  in
        let val (b, _TotalComments) = get_int b  in
        let val (b, _UnreadPosts) = get_int b  in
        let val (b, _UnreadComments) = get_int b  in
        let val (b, _Took) = get_int b  in
        let val (b, _TookReal) = get_int b  in
        let val (b, _MsgForest) = get_msgForest b  in
          (b,
           { TotalPosts = _TotalPosts
           , TotalComments = _TotalComments
           , UnreadPosts = _UnreadPosts
           , UnreadComments = _UnreadComments
           , Took = _Took
           , TookReal = _TookReal
           , MsgForest = _MsgForest
           })
        end
        end
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize FilterResults ({[n]} is out of range)</xml>
fun get_emailAddress (b : getBuf) : (getBuf * emailAddress) = 
  case 0 of
      0 => 
        let val (b, _Email) = get_string b  in
        let val (b, _FirstName) = get_string b  in
        let val (b, _LastName) = get_string b  in
          (b,
           { Email = _Email
           , FirstName = _FirstName
           , LastName = _LastName
           })
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize EmailAddress ({[n]} is out of range)</xml>
fun get_okErrorRedirect (b : getBuf) : (getBuf * okErrorRedirect) = 
  let val (b, c) = get_byte b in case c of
      0 => 
          (b, OEROK)
    | 1 => 
        let val (b, _Error) = get_string b  in
          (b,
           OERError
           { Error = _Error
           })
        end
    | 2 => 
        let val (b, _Url) = get_string b  in
          (b,
           OERRedirect
           { Url = _Url
           })
        end
    | n => error <xml>Oh, shi -- can’t deserialize OkErrorRedirect ({[n]} is out of range)</xml>
  end
fun get_linkInfo (b : getBuf) : (getBuf * linkInfo) = 
  case 0 of
      0 => 
        let val (b, _Url) = get_url b  in
        let val (b, _Title) = get_string b  in
        let val (b, _Description) = get_string b  in
        let val (b, _Image) = get_option (get_url) b  in
        let val (b, _Avatar) = get_option (get_url) b  in
          (b,
           { Url = _Url
           , Title = _Title
           , Description = _Description
           , Image = _Image
           , Avatar = _Avatar
           })
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize LinkInfo ({[n]} is out of range)</xml>
fun get_feedbackEmail (b : getBuf) : (getBuf * feedbackEmail) = 
  case 0 of
      0 => 
        let val (b, _Address) = get_emailAddress b  in
        let val (b, _Time) = get_time b  in
        let val (b, _Subject) = get_string b  in
        let val (b, _Text) = get_string b  in
        let val (b, _Reserved1) = get_int b  in
        let val (b, _Reserved2) = get_int b  in
          (b,
           { Address = _Address
           , Time = _Time
           , Subject = _Subject
           , Text = _Text
           , Reserved1 = _Reserved1
           , Reserved2 = _Reserved2
           })
        end
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize FeedbackEmail ({[n]} is out of range)</xml>
fun get_feedbackUserInfo (b : getBuf) : (getBuf * feedbackUserInfo) = 
  case 0 of
      0 => 
        let val (b, _Id) = get_string b  in
        let val (b, _Who) = get_option (get_string) b  in
        let val (b, _PaidTill) = get_paidTill b  in
        let val (b, _Country) = get_string b  in
        let val (b, _UsageFlags) = get_list (get_usageFlag) b  in
        let val (b, _LastUsedTime) = get_time b  in
        let val (b, _Deleted) = get_bool b  in
        let val (b, _Payments) = get_list ((fn (b : getBuf) =>let val (b, _1) = get_time b  in
        let val (b, _2) = get_string b  in
        let val (b, _3) = get_string b  in
        let val (b, _4) = get_emailAddress b  in
          (b,(_1, _2, _3, _4))
        end
        end
        end
        end
        )) b  in
        let val (b, _FeedsCount) = get_int b  in
        let val (b, _ErrorFeedsCount) = get_int b  in
        let val (b, _ProcessedAt) = get_option (get_time) b  in
        let val (b, _MailSent) = get_option (get_feedbackEmail) b  in
        let val (b, _RepliedAt) = get_option (get_time) b  in
        let val (b, _Tags) = get_list (get_string) b  in
        let val (b, _Notes) = get_string b  in
        let val (b, _Reserved1) = get_int b  in
        let val (b, _Reserved2) = get_int b  in
        let val (b, _Reserved3) = get_int b  in
        let val (b, _Reserved4) = get_int b  in
          (b,
           { Id = _Id
           , Who = _Who
           , PaidTill = _PaidTill
           , Country = _Country
           , UsageFlags = _UsageFlags
           , LastUsedTime = _LastUsedTime
           , Deleted = _Deleted
           , Payments = _Payments
           , FeedsCount = _FeedsCount
           , ErrorFeedsCount = _ErrorFeedsCount
           , ProcessedAt = _ProcessedAt
           , MailSent = _MailSent
           , RepliedAt = _RepliedAt
           , Tags = _Tags
           , Notes = _Notes
           , Reserved1 = _Reserved1
           , Reserved2 = _Reserved2
           , Reserved3 = _Reserved3
           , Reserved4 = _Reserved4
           })
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can’t deserialize FeedbackUserInfo ({[n]} is out of range)</xml>


fun put_subscriptionParentUrl (b : putBuf) (x : subscriptionParentUrl) : putBuf = 
  case x of
      SpuRedirect x => 
        let val b = put_byte b 0 in
        let val b = put_string b x.Url in
          b
        end
        end
    | SpuHtml x => 
        let val b = put_byte b 1 in
        let val b = put_string b x.Url in
        let val b = put_string b x.Debug in
          b
        end
        end
        end
  
fun put_subscriptionState (b : putBuf) (x : subscriptionState) : putBuf = 
  case x of
      SSAdded => 
        let val b = put_byte b 0 in
          b
        end
    | SSScanning x => 
        let val b = put_byte b 1 in
        let val b = put_time b x.StartTime in
          b
        end
        end
    | SSError x => 
        let val b = put_byte b 2 in
        let val b = put_string b x.Message in
          b
        end
        end
    | SSFeed x => 
        let val b = put_byte b 3 in
        let val b = put_string b x.Url in
          b
        end
        end
    | SSErrorPath x => 
        let val b = put_byte b 4 in
        let val b = put_string b x.Message in
        let val b = put_list (put_subscriptionParentUrl) b x.Path in
          b
        end
        end
        end
  
fun put_subscription (b : putBuf) (x : subscription) : putBuf = 
  case x of
    |  x => 

        let val b = put_string b x.Url in
        let val b = put_subscriptionState b x.State in
        let val b = put_int b x.EditsCount in
        let val b = put_option (put_string) b x.Title in
        let val b = put_list (put_string) b x.Folders in
          b
        end
        end
        end
        end
        end
  
fun put_postsViewMode (b : putBuf) (x : postsViewMode) : putBuf = 
  case x of
      PVMShort => 
        let val b = put_byte b 0 in
          b
        end
    | PVMFull => 
        let val b = put_byte b 1 in
          b
        end
    | PVMMagazine => 
        let val b = put_byte b 2 in
          b
        end
    | PVMMosaic => 
        let val b = put_byte b 3 in
          b
        end
  
fun put_mTVMEx (b : putBuf) (x : mTVMEx) : putBuf = 
  case x of
      MTVMFolderCollapsed => 
        let val b = put_byte b 0 in
          b
        end
    | MTVMFolderExpanded => 
        let val b = put_byte b 1 in
          b
        end
    | MTVMEx x => 
        let val b = put_byte b 2 in
        let val b = put_bool b x.FolderExpanded in
        let val b = put_bool b x.GroupByFeed in
        let val b = put_bool b x.Reserved1 in
        let val b = put_int b x.Reserved2 in
          b
        end
        end
        end
        end
        end
  
fun put_msgTreeViewMode (b : putBuf) (x : msgTreeViewMode) : putBuf = 
  case x of
    |  x => 

        let val b = put_bool b x.Ascending in
        let val b = put_bool b x.UnreadOnly in
        let val b = put_bool b x.ExpandedComments in
        let val b = put_postsViewMode b x.Posts in
        let val b = put_mTVMEx b x.Ex in
        let val b = put_bool b x.NoOverride in
          b
        end
        end
        end
        end
        end
        end
  
fun put_payment (b : putBuf) (x : payment) : putBuf = 
  case x of
      PReserved => 
        let val b = put_byte b 0 in
          b
        end
    | PFastSpring x => 
        let val b = put_byte b 1 in
        let val b = put_string b x.OrderId in
        let val b = put_string b x.OrderType in
        let val b = put_time b x.OrderTime in
          b
        end
        end
        end
        end
  
fun put_paidTill (b : putBuf) (x : paidTill) : putBuf = 
  case x of
      PTUnknown => 
        let val b = put_byte b 0 in
          b
        end
    | PTPaid x => 
        let val b = put_byte b 1 in
        let val b = put_time b x.Till in
          b
        end
        end
    | PTFreeTrial x => 
        let val b = put_byte b 2 in
        let val b = put_time b x.Till in
          b
        end
        end
    | PTFreeTrialFinished x => 
        let val b = put_byte b 3 in
        let val b = put_time b x.Till in
          b
        end
        end
    | PTPaidFinished x => 
        let val b = put_byte b 4 in
        let val b = put_time b x.Till in
          b
        end
        end
  
fun put_scrollMode (b : putBuf) (x : scrollMode) : putBuf = 
  case x of
      SMNormal => 
        let val b = put_byte b 0 in
          b
        end
    | SMQuick => 
        let val b = put_byte b 1 in
          b
        end
    | SMImmediate => 
        let val b = put_byte b 2 in
          b
        end
  
fun put_listViewMode (b : putBuf) (x : listViewMode) : putBuf = 
  case x of
      LVMCompact => 
        let val b = put_byte b 0 in
          b
        end
    | LVMTwoLines => 
        let val b = put_byte b 1 in
          b
        end
  
fun put_markReadMode (b : putBuf) (x : markReadMode) : putBuf = 
  case x of
      MRMOnScroll => 
        let val b = put_byte b 0 in
          b
        end
    | MRMManual => 
        let val b = put_byte b 1 in
          b
        end
    | MRMOnScrollEverywhere => 
        let val b = put_byte b 2 in
          b
        end
  
fun put_publicFeedType (b : putBuf) (x : publicFeedType) : putBuf = 
  case x of
      PFTAll => 
        let val b = put_byte b 0 in
          b
        end
    | PFTFolder x => 
        let val b = put_byte b 1 in
        let val b = put_string b x.Folder in
          b
        end
        end
    | PFTTag x => 
        let val b = put_byte b 2 in
        let val b = put_string b x.TagName in
          b
        end
        end
    | PFTStarred => 
        let val b = put_byte b 3 in
          b
        end
    | PFTAllTags => 
        let val b = put_byte b 4 in
          b
        end
    | PFTSmartStream x => 
        let val b = put_byte b 5 in
        let val b = put_string b x.StreamName in
          b
        end
        end
  
fun put_loginAccessToken (b : putBuf) (x : loginAccessToken) : putBuf = 
  case x of
      LATNone => 
        let val b = put_byte b 0 in
          b
        end
    | LATFacebook x => 
        let val b = put_byte b 1 in
        let val b = put_string b x.AccessToken in
          b
        end
        end
    | LATTwitter x => 
        let val b = put_byte b 2 in
        let val b = put_list ((fn (b : putBuf) t => let val b = put_string b t.1 in
        let val b = put_string b t.2 in
          b
        end
        end
        )) b x.Credentials in
          b
        end
        end
  
fun put_apiKeys (b : putBuf) (x : apiKeys) : putBuf = 
  case x of
    |  x => 

        let val b = put_option ((fn (b : putBuf) t => let val b = put_string b t.1 in
        let val b = put_string b t.2 in
          b
        end
        end
        )) b x.Pocket in
        let val b = put_option ((fn (b : putBuf) t => let val b = put_string b t.1 in
        let val b = put_string b t.2 in
        let val b = put_string b t.3 in
          b
        end
        end
        end
        )) b x.PocketRequest in
        let val b = put_option (put_int) b x.Reserved10 in
        let val b = put_option ((fn (b : putBuf) t => let val b = put_time b t.1 in
        let val b = put_string b t.2 in
          b
        end
        end
        )) b x.FacebookAccessToken in
        let val b = put_option ((fn (b : putBuf) t => let val b = put_time b t.1 in
        let val b = put_list ((fn (b : putBuf) t => let val b = put_string b t.1 in
        let val b = put_string b t.2 in
          b
        end
        end
        )) b t.2 in
          b
        end
        end
        )) b x.TwitterAccessToken in
        let val b = put_bool b x.Reserved13 in
        let val b = put_bool b x.Reserved14 in
        let val b = put_bool b x.Reserved15 in
        let val b = put_bool b x.Reserved16 in
        let val b = put_bool b x.Reserved17 in
        let val b = put_int b x.Reserved2 in
        let val b = put_int b x.Reserved3 in
        let val b = put_int b x.Reserved4 in
          b
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
  
fun put_userExperiment (b : putBuf) (x : userExperiment) : putBuf = 
  case x of
    | UENo9 => 

          b
  
fun put_customShareAction (b : putBuf) (x : customShareAction) : putBuf = 
  case x of
    |  x => 

        let val b = put_int b x.Id in
        let val b = put_string b x.Title in
        let val b = put_string b x.UrlFormat in
        let val b = put_bool b x.Shorten in
          b
        end
        end
        end
        end
  
fun put_shareAction (b : putBuf) (x : shareAction) : putBuf = 
  case x of
      SAEMail => 
        let val b = put_byte b 0 in
          b
        end
    | SATwitter => 
        let val b = put_byte b 1 in
          b
        end
    | SAFacebook => 
        let val b = put_byte b 2 in
          b
        end
    | SAGooglePlus => 
        let val b = put_byte b 3 in
          b
        end
    | SATumblr => 
        let val b = put_byte b 4 in
          b
        end
    | SAEvernote => 
        let val b = put_byte b 5 in
          b
        end
    | SADelicious_discontinued => 
        let val b = put_byte b 6 in
          b
        end
    | SAPinboard => 
        let val b = put_byte b 7 in
          b
        end
    | SAPocket => 
        let val b = put_byte b 8 in
          b
        end
    | SAReadability_discontinued => 
        let val b = put_byte b 9 in
          b
        end
    | SAInstapaper => 
        let val b = put_byte b 10 in
          b
        end
    | SATranslate => 
        let val b = put_byte b 11 in
          b
        end
    | SABlogger => 
        let val b = put_byte b 12 in
          b
        end
    | SAWordpress => 
        let val b = put_byte b 13 in
          b
        end
    | SALinkedIn => 
        let val b = put_byte b 14 in
          b
        end
    | SAPinterest => 
        let val b = put_byte b 15 in
          b
        end
    | SAVK => 
        let val b = put_byte b 16 in
          b
        end
    | SASkype => 
        let val b = put_byte b 17 in
          b
        end
    | SAReddit => 
        let val b = put_byte b 18 in
          b
        end
    | SAStumbleUpon => 
        let val b = put_byte b 19 in
          b
        end
    | SADigg => 
        let val b = put_byte b 20 in
          b
        end
    | SAScoopIt => 
        let val b = put_byte b 21 in
          b
        end
    | SAFlipboard => 
        let val b = put_byte b 22 in
          b
        end
    | SABuffer => 
        let val b = put_byte b 23 in
          b
        end
    | SANewsVine => 
        let val b = put_byte b 24 in
          b
        end
    | SADiigo => 
        let val b = put_byte b 25 in
          b
        end
    | SARememberTheMilk => 
        let val b = put_byte b 26 in
          b
        end
    | SAGoogleBookmarks => 
        let val b = put_byte b 27 in
          b
        end
    | SAWallabag => 
        let val b = put_byte b 28 in
          b
        end
    | SAWakelet => 
        let val b = put_byte b 29 in
          b
        end
    | SACustom x => 
        let val b = put_byte b 30 in
        let val b = put_customShareAction b x.CustomShareAction in
          b
        end
        end
    | SASystem => 
        let val b = put_byte b 31 in
          b
        end
  
fun put_msgButton (b : putBuf) (x : msgButton) : putBuf = 
  case x of
      MBKeepUnread => 
        let val b = put_byte b 0 in
          b
        end
    | MBStar => 
        let val b = put_byte b 1 in
          b
        end
    | MBTag => 
        let val b = put_byte b 2 in
          b
        end
    | MBShare => 
        let val b = put_byte b 3 in
          b
        end
    | MBShareAction x => 
        let val b = put_byte b 4 in
        let val b = put_shareAction b x.ShareAction in
          b
        end
        end
  
fun put_emailContact (b : putBuf) (x : emailContact) : putBuf = 
  case x of
    |  x => 

        let val b = put_string b x.EMail in
        let val b = put_string b x.FullName in
        let val b = put_list (put_string) b x.Groups in
        let val b = put_option (put_string) b x.Avatar in
        let val b = put_option ((fn (b : putBuf) t => let val b = put_time b t.1 in
        let val b = put_int b t.2 in
          b
        end
        end
        )) b x.Stats in
          b
        end
        end
        end
        end
        end
  
fun put_sharingSettings (b : putBuf) (x : sharingSettings) : putBuf = 
  case x of
    |  x => 

        let val b = put_option (put_list (put_msgButton)) b x.ShareMenuButtons in
        let val b = put_option (put_list (put_msgButton)) b x.MsgButtons in
        let val b = put_option (put_list (put_msgButton)) b x.ListViewButtons in
        let val b = put_list (put_customShareAction) b x.CustomShareActions in
        let val b = put_bool b x.EMailUsingMailto in
        let val b = put_option ((fn (b : putBuf) t => let val b = put_string b t.1 in
        let val b = put_string b t.2 in
          b
        end
        end
        )) b x.ReplyToEMail in
        let val b = put_list (put_emailContact) b x.Contacts in
        let val b = put_int b x.Reserved1 in
          b
        end
        end
        end
        end
        end
        end
        end
        end
  
fun put_loginType (b : putBuf) (x : loginType) : putBuf = 
  case x of
      LTGoogle x => 
        let val b = put_byte b 0 in
        let val b = put_string b x.Email in
          b
        end
        end
    | LTFacebook x => 
        let val b = put_byte b 1 in
        let val b = put_string b x.Email in
          b
        end
        end
    | LTTwitter x => 
        let val b = put_byte b 2 in
        let val b = put_string b x.Id in
          b
        end
        end
    | LTOpenId x => 
        let val b = put_byte b 3 in
        let val b = put_string b x.URL in
          b
        end
        end
    | LTEmail x => 
        let val b = put_byte b 4 in
        let val b = put_string b x.Email in
          b
        end
        end
    | LTUsername x => 
        let val b = put_byte b 5 in
        let val b = put_string b x.Username in
          b
        end
        end
    | LTFeverApiKey x => 
        let val b = put_byte b 6 in
        let val b = put_string b x.ApiKey in
          b
        end
        end
  
fun put_userSettingsEx (b : putBuf) (x : userSettingsEx) : putBuf = 
  case x of
    |  x => 

        let val b = put_time b x.LastWhatsNewTime in
        let val b = put_option (put_string) b x.PasswordHash in
        let val b = put_option (put_int) b x.Reserved1_1 in
        let val b = put_option (put_int) b x.Reserved1_2 in
        let val b = put_option (put_int) b x.Reserved1_3 in
        let val b = put_option (put_int) b x.Reserved1_4 in
        let val b = put_option (put_int) b x.Reserved1_5 in
        let val b = put_option (put_int) b x.Reserved1_6 in
        let val b = put_option (put_int) b x.Reserved1_7 in
        let val b = put_list (put_loginType) b x.AssociatedAccounts in
        let val b = put_list ((fn (b : putBuf) t => let val b = put_loginType b t.1 in
        let val b = put_string b t.2 in
          b
        end
        end
        )) b x.AssociatedAccountNames in
        let val b = put_int b x.Reserved4 in
        let val b = put_int b x.Reserved5 in
        let val b = put_int b x.Reserved6 in
        let val b = put_int b x.Reserved7 in
        let val b = put_int b x.Reserved8 in
        let val b = put_int b x.Reserved9 in
        let val b = put_int b x.Reserved10 in
        let val b = put_int b x.Reserved11 in
        let val b = put_int b x.Reserved12 in
        let val b = put_int b x.Reserved13 in
        let val b = put_int b x.Reserved14 in
        let val b = put_int b x.Reserved15 in
          b
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
  
fun put_userSettings (b : putBuf) (x : userSettings) : putBuf = 
  case x of
    |  x => 

        let val b = put_string b x.User in
        let val b = put_int b x.EditsCount in
        let val b = put_scrollMode b x.ScrollMode in
        let val b = put_listViewMode b x.ListViewMode in
        let val b = put_bool b x.ShowFavicons in
        let val b = put_markReadMode b x.MarkReadMode in
        let val b = put_bool b x.UltraCompact in
        let val b = put_option (put_string) b x.Reserved in
        let val b = put_bool b x.ExactUnreadCounts in
        let val b = put_option (put_list ((fn (b : putBuf) t => let val b = put_publicFeedType b t.1 in
        let val b = put_list ((fn (b : putBuf) t => let val b = put_string b t.1 in
        let val b = put_bool b t.2 in
        let val b = put_option (put_string) b t.3 in
          b
        end
        end
        end
        )) b t.2 in
          b
        end
        end
        ))) b x.PublicFeeds in
        let val b = put_option (put_string) b x.Country in
        let val b = put_option (put_apiKeys) b x.ApiKeys in
        let val b = put_option (put_list (put_userExperiment)) b x.Experiments in
        let val b = put_option (put_sharingSettings) b x.SharingSettings_ in
        let val b = put_option (put_userSettingsEx) b x.Ex in
          b
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
  
fun put_uID (b : putBuf) (x : uID) : putBuf = 
  case x of
      EMail x => 
        let val b = put_byte b 0 in
        let val b = put_string b x.Id in
          b
        end
        end
    | Url x => 
        let val b = put_byte b 1 in
        let val b = put_string b x.Id in
          b
        end
        end
  
fun put_session (b : putBuf) (x : session) : putBuf = 
  case x of
    |  x => 

        let val b = put_string b x.Key in
        let val b = put_time b x.Expire in
        let val b = put_bool b x.Cleared in
        let val b = put_string b x.User in
          b
        end
        end
        end
        end
  
fun put_attachment (b : putBuf) (x : attachment) : putBuf = 
  case x of
      AImage x => 
        let val b = put_byte b 0 in
        let val b = put_url b x.Url in
        let val b = put_option (put_int) b x.Width in
        let val b = put_option (put_int) b x.Height in
        let val b = put_option (put_string) b x.Title in
          b
        end
        end
        end
        end
        end
    | AAudio x => 
        let val b = put_byte b 1 in
        let val b = put_url b x.Url in
        let val b = put_string b x.Mime in
        let val b = put_option (put_int) b x.FileSize in
        let val b = put_option (put_int) b x.Duration in
        let val b = put_option (put_string) b x.Title in
          b
        end
        end
        end
        end
        end
        end
    | AVideo x => 
        let val b = put_byte b 2 in
        let val b = put_url b x.Url in
        let val b = put_string b x.Mime in
        let val b = put_option (put_int) b x.FileSize in
        let val b = put_option (put_int) b x.Duration in
        let val b = put_option (put_string) b x.Title in
        let val b = put_option (put_int) b x.Width in
        let val b = put_option (put_int) b x.Height in
        let val b = put_option (put_url) b x.Poster in
          b
        end
        end
        end
        end
        end
        end
        end
        end
        end
    | AIframe x => 
        let val b = put_byte b 3 in
        let val b = put_url b x.Url in
        let val b = put_string b x.Xml in
        let val b = put_option (put_int) b x.Duration in
        let val b = put_option (put_string) b x.Title in
          b
        end
        end
        end
        end
        end
    | AOther x => 
        let val b = put_byte b 4 in
        let val b = put_url b x.Url in
        let val b = put_string b x.Mime in
        let val b = put_option (put_int) b x.FileSize in
          b
        end
        end
        end
        end
    | AGrOrigin x => 
        let val b = put_byte b 5 in
        let val b = put_url b x.Feed in
        let val b = put_string b x.Guid in
        let val b = put_string b x.StreamTitle in
        let val b = put_string b x.HtmlUrl in
          b
        end
        end
        end
        end
        end
    | AVideo2 x => 
        let val b = put_byte b 6 in
        let val b = put_url b x.Url in
        let val b = put_string b x.Mime in
        let val b = put_option (put_int) b x.FileSize in
        let val b = put_option (put_int) b x.Duration in
        let val b = put_option (put_string) b x.Title in
        let val b = put_option (put_int) b x.Width in
        let val b = put_option (put_int) b x.Height in
        let val b = put_option (put_url) b x.Poster in
        let val b = put_bool b x.Loop in
          b
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
    | AThumbnail x => 
        let val b = put_byte b 7 in
        let val b = put_url b x.Url in
          b
        end
        end
  
fun put_msgKey (b : putBuf) (x : msgKey) : putBuf = 
  case x of
    |  x => 

        let val b = put_string b x.BlogFeedUrl in
        let val b = put_option (put_string) b x.PostGuid in
        let val b = put_option (put_string) b x.CommentGuid in
          b
        end
        end
        end
  
fun put_msg (b : putBuf) (x : msg) : putBuf = 
  case x of
    |  x => 

        let val b = put_msgKey b x.Key in
        let val b = put_list (put_attachment) b x.Attachments in
        let val b = put_string b x.Author in
        let val b = put_option (put_url) b x.AuthorUri in
        let val b = put_string b x.AuthorEmail in
        let val b = put_option (put_url) b x.AuthorPic in
        let val b = put_option (put_url) b x.Link in
        let val b = put_string b x.Subject in
        let val b = put_list (put_string) b x.Tags in
        let val b = put_option (put_time) b x.Time in
        let val b = put_time b x.DlTime in
        let val b = put_xbodyString b x.Text in
        let val b = put_string b x.ShortText in
        let val b = put_string b x.ShorterText in
          b
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
  
fun put_msgHeader (b : putBuf) (x : msgHeader) : putBuf = 
  case x of
    |  x => 

        let val b = put_string b x.Guid in
        let val b = put_string b x.ContentHash in
        let val b = put_string b x.Author in
        let val b = put_option (put_url) b x.AuthorPic in
        let val b = put_string b x.Subject in
        let val b = put_option (put_time) b x.Time in
        let val b = put_time b x.DlTime in
        let val b = put_string b x.ShortText in
          b
        end
        end
        end
        end
        end
        end
        end
        end
  
fun put_commentsKey (b : putBuf) (x : commentsKey) : putBuf = 
  case x of
    |  x => 

        let val b = put_string b x.BlogFeedUrl in
        let val b = put_string b x.PostGuid in
          b
        end
        end
  
fun put_itemTag (b : putBuf) (x : itemTag) : putBuf = 
  case x of
      ITStarred => 
        let val b = put_byte b 0 in
          b
        end
    | ITTag x => 
        let val b = put_byte b 1 in
        let val b = put_string b x.TagName in
          b
        end
        end
  
fun put_filterQueryRpc (b : putBuf) (x : filterQueryRpc) : putBuf = 
  case x of
    |  x => 

        let val b = put_string b x.Query in
        let val b = put_bool b x.Negate in
        let val b = put_list (put_int) b x.FeedGRIds in
          b
        end
        end
        end
  
fun put_searchError (b : putBuf) (x : searchError) : putBuf = 
  case x of
      SESyntaxError x => 
        let val b = put_byte b 0 in
        let val b = put_string b x.ErrorMessage in
          b
        end
        end
    | SESystemError x => 
        let val b = put_byte b 1 in
        let val b = put_string b x.ErrorMessage in
          b
        end
        end
  
fun put_apiMode (b : putBuf) (x : apiMode) : putBuf = 
  case x of
      AMNormal x => 
        let val b = put_byte b 0 in
        let val b = put_string b x.HostName in
        let val b = put_string b x.AcceptLanguage in
          b
        end
        end
        end
    | AMGRIdsOnly x => 
        let val b = put_byte b 1 in
        let val b = put_bool b x.Fetch in
        let val b = put_int b x.Count in
        let val b = put_option (put_option (put_msgKey)) b x.Continuation in
        let val b = put_option (put_time) b x.MinDlTime in
        let val b = put_option (put_time) b x.MaxDlTime in
        let val b = put_option (put_time) b x.MaxTime in
        let val b = put_list (put_itemTag) b x.ExcludeTags in
        let val b = put_list (put_itemTag) b x.IncludeTags in
        let val b = put_bool b x.ReadOnly in
        let val b = put_list ((fn (b : putBuf) t => let val b = put_string b t.1 in
        let val b = put_string b t.2 in
          b
        end
        end
        )) b x.MsgLinkParams in
        let val b = put_bool b x.FromUI in
        let val b = put_option (put_int) b x.MaxMsgTextLength in
          b
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
    | AMDiscovery x => 
        let val b = put_byte b 2 in
        let val b = put_string b x.HostName in
        let val b = put_string b x.AcceptLanguage in
        let val b = put_string b x.Url in
          b
        end
        end
        end
        end
  
fun put_msgTreePoint (b : putBuf) (x : msgTreePoint) : putBuf = 
  case x of
    |  x => 

        let val b = put_int b x.ParentId in
        let val b = put_time b x.Time in
        let val b = put_int b x.Id in
          b
        end
        end
        end
  
fun put_postsReq (b : putBuf) (x : postsReq) : putBuf = 
  case x of
    |  x => 

        let val b = put_int b x.FeedId in
        let val b = put_msgTreePoint b x.MsgTreePoint in
        let val b = put_int b x.TotalPosts in
        let val b = put_int b x.TotalComments in
          b
        end
        end
        end
        end
  
fun put_commentsReq (b : putBuf) (x : commentsReq) : putBuf = 
  case x of
    |  x => 

        let val b = put_commentsKey b x.Key in
        let val b = put_int b x.PostId in
        let val b = put_msgTreePoint b x.MsgTreePoint in
        let val b = put_int b x.TotalComments in
          b
        end
        end
        end
        end
  
fun put_msgId (b : putBuf) (x : msgId) : putBuf = 
  case x of
    |  x => 

        let val b = put_int b x.FeedId in
        let val b = put_int b x.PostId in
        let val b = put_option (put_int) b x.CommentId in
          b
        end
        end
        end
  
fun put_longMsgId (b : putBuf) (x : longMsgId) : putBuf = 
  case x of
    |  x => 

        let val b = put_msgKey b x.MsgKey in
        let val b = put_msgId b x.MsgId in
          b
        end
        end
  
fun put_treeReq (b : putBuf) (x : treeReq) : putBuf = 
  case x of
      TRPosts x => 
        let val b = put_byte b 0 in
        let val b = put_list (put_postsReq) b x.Reqs in
          b
        end
        end
    | TRTags x => 
        let val b = put_byte b 1 in
        let val b = put_option (put_list (put_itemTag)) b x.Tags in
        let val b = put_option ((fn (b : putBuf) t => let val b = put_time b t.1 in
        let val b = put_msgId b t.2 in
          b
        end
        end
        )) b x.MaxTag in
        let val b = put_option ((fn (b : putBuf) t => let val b = put_time b t.1 in
        let val b = put_msgId b t.2 in
          b
        end
        end
        )) b x.LastMsg in
          b
        end
        end
        end
        end
    | TRComments x => 
        let val b = put_byte b 2 in
        let val b = put_bool b x.OnExpand in
        let val b = put_commentsReq b x.Req in
          b
        end
        end
        end
    | TRCommentsS x => 
        let val b = put_byte b 3 in
        let val b = put_bool b x.OnExpand in
        let val b = put_string b x.StreamName in
        let val b = put_commentsReq b x.Req in
          b
        end
        end
        end
        end
    | TRSmartStream x => 
        let val b = put_byte b 4 in
        let val b = put_string b x.StreamName in
        let val b = put_list (put_postsReq) b x.Reqs in
          b
        end
        end
        end
    | TRSearchPosts x => 
        let val b = put_byte b 5 in
        let val b = put_string b x.Query in
        let val b = put_string b x.FeedMasksKey in
        let val b = put_list (put_postsReq) b x.Reqs in
          b
        end
        end
        end
        end
    | TRSearchSmartStream x => 
        let val b = put_byte b 6 in
        let val b = put_string b x.StreamName in
        let val b = put_string b x.Query in
        let val b = put_string b x.FeedMasksKey in
        let val b = put_list (put_postsReq) b x.Reqs in
          b
        end
        end
        end
        end
        end
    | TRSearchTags x => 
        let val b = put_byte b 7 in
        let val b = put_string b x.Query in
        let val b = put_string b x.IdsKey in
        let val b = put_option (put_list (put_itemTag)) b x.Tags in
        let val b = put_option ((fn (b : putBuf) t => let val b = put_time b t.1 in
        let val b = put_msgId b t.2 in
          b
        end
        end
        )) b x.MaxTag in
        let val b = put_option ((fn (b : putBuf) t => let val b = put_time b t.1 in
        let val b = put_msgId b t.2 in
          b
        end
        end
        )) b x.LastMsg in
          b
        end
        end
        end
        end
        end
        end
  
fun put_msgView (b : putBuf) (x : msgView) : putBuf = 
  case x of
      MVFull x => 
        let val b = put_byte b 0 in
        let val b = put_msg b x.Msg in
          b
        end
        end
    | MVShort x => 
        let val b = put_byte b 1 in
        let val b = put_msgHeader b x.Header in
        let val b = put_option (put_msg) b x.CachedMsg in
          b
        end
        end
        end
  
fun put_msgItem (b : putBuf) (x : msgItem) : putBuf = 
  case x of
    |  x => 

        let val b = put_msgView b x.MsgView in
        let val b = put_msgKey b x.MsgKey in
        let val b = put_msgId b x.MsgId in
        let val b = put_bool b x.Read in
        let val b = put_list (put_itemTag) b x.Tags in
        let val b = put_list (put_int) b x.SmartStreams in
        let val b = put_bool b x.ReadLocked in
        let val b = put_bool b x.Full in
        let val b = put_bool b x.SearchResult in
          b
        end
        end
        end
        end
        end
        end
        end
        end
        end
  
fun put_msgForest (b : putBuf) (x : msgForest) : putBuf = 
  case x of
    | MsgForest x => 

        let val b = put_int b x.TotalCount in
        let val b = put_int b x.UnreadCount in
        let val b = put_int b x.TotalResultsCount in
        let val b = put_int b x.UnreadResultsCount in
        let val b = put_list ((fn (b : putBuf) t => let val b = put_int b t.1 in
        let val b = put_int b t.2 in
          b
        end
        end
        )) b x.SmartStreamUnreadCounts in
        let val b = put_list ((fn (b : putBuf) t => let val b = put_int b t.1 in
        let val b = put_int b t.2 in
          b
        end
        end
        )) b x.SmartStreamUnreadResultCounts in
        let val b = put_list ((fn (b : putBuf) t => let val b = put_option (put_itemTag) b t.1 in
        let val b = put_int b t.2 in
          b
        end
        end
        )) b x.TagTotalCounts in
        let val b = put_list ((fn (b : putBuf) t => let val b = put_option (put_itemTag) b t.1 in
        let val b = put_int b t.2 in
          b
        end
        end
        )) b x.TagUnreadCounts in
        let val b = put_list ((fn (b : putBuf) t => let val b = put_option (put_itemTag) b t.1 in
        let val b = put_int b t.2 in
          b
        end
        end
        )) b x.TagUnreadResultCounts in
        let val b = put_list ((fn (b : putBuf) t => let val b = put_msgItem b t.1 in
        let val b = put_msgForest b t.2 in
          b
        end
        end
        )) b x.List in
        let val b = put_option (put_treeReq) b x.NextReq in
          b
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
  
fun put_externalLoginType (b : putBuf) (x : externalLoginType) : putBuf = 
  case x of
      Google => 
        let val b = put_byte b 0 in
          b
        end
    | Facebook => 
        let val b = put_byte b 1 in
          b
        end
    | Twitter => 
        let val b = put_byte b 2 in
          b
        end
    | OpenId x => 
        let val b = put_byte b 3 in
        let val b = put_string b x.URL in
          b
        end
        end
  
fun put_externalLoginAction (b : putBuf) (x : externalLoginAction) : putBuf = 
  case x of
      ELALogin => 
        let val b = put_byte b 0 in
          b
        end
    | ELAAddUrl x => 
        let val b = put_byte b 1 in
        let val b = put_string b x.URL in
          b
        end
        end
    | ELAAddAssociatedAccount => 
        let val b = put_byte b 2 in
          b
        end
  
fun put_counters (b : putBuf) (x : counters) : putBuf = 
  case x of
    |  x => 

        let val b = put_int b x.ReadPosts in
        let val b = put_int b x.ReadComments in
        let val b = put_int b x.TotalPosts in
        let val b = put_int b x.TotalComments in
        let val b = put_int b x.Scanning in
        let val b = put_int b x.ScanningComments in
        let val b = put_int b x.Error in
        let val b = put_int b x.Feed in
        let val b = put_int b x.ScannedPercent in
          b
        end
        end
        end
        end
        end
        end
        end
        end
        end
  
fun put_subItemType (b : putBuf) (x : subItemType) : putBuf = 
  case x of
      SITAll => 
        let val b = put_byte b 0 in
          b
        end
    | SITSearch x => 
        let val b = put_byte b 1 in
        let val b = put_string b x.Query in
          b
        end
        end
    | SITFolder x => 
        let val b = put_byte b 2 in
        let val b = put_string b x.Folder in
          b
        end
        end
    | SITFeed x => 
        let val b = put_byte b 3 in
        let val b = put_subscription b x.Subscription in
        let val b = put_option (put_string) b x.FeedLink in
        let val b = put_option (put_msgTreePoint) b x.PointAllDesc in
          b
        end
        end
        end
        end
    | SITTag x => 
        let val b = put_byte b 4 in
        let val b = put_string b x.TagName in
          b
        end
        end
    | SITSmartStream x => 
        let val b = put_byte b 5 in
        let val b = put_string b x.StreamName in
        let val b = put_list (put_int) b x.StreamFeedSirs in
          b
        end
        end
        end
    | SITStarred => 
        let val b = put_byte b 6 in
          b
        end
    | SITAllTags => 
        let val b = put_byte b 7 in
          b
        end
  
fun put_subItemRpc (b : putBuf) (x : subItemRpc) : putBuf = 
  case x of
    |  x => 

        let val b = put_string b x.Path in
        let val b = put_int b x.Index in
        let val b = put_string b x.Title in
        let val b = put_subItemType b x.SIType in
        let val b = put_counters b x.Counters in
        let val b = put_msgTreeViewMode b x.ViewMode in
        let val b = put_list (put_int) b x.ParentFolders in
        let val b = put_list (put_int) b x.DomIds in
        let val b = put_option (put_string) b x.FaviconStyle in
        let val b = put_int b x.GRId in
          b
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
  
fun put_welcomeState (b : putBuf) (x : welcomeState) : putBuf = 
  case x of
    |  x => 

        let val b = put_bool b x.HasPrevAccount in
        let val b = put_bool b x.HasPrevSubs in
        let val b = put_bool b x.StarredRestored in
        let val b = put_bool b x.TaggedRestored in
          b
        end
        end
        end
        end
  
fun put_updateFilters (b : putBuf) (x : updateFilters) : putBuf = 
  case x of
      UFNone => 
        let val b = put_byte b 0 in
          b
        end
    | UFChanged => 
        let val b = put_byte b 1 in
          b
        end
    | UFAll => 
        let val b = put_byte b 2 in
          b
        end
  
fun put_browserType (b : putBuf) (x : browserType) : putBuf = 
  case x of
      BTUnknown => 
        let val b = put_byte b 0 in
          b
        end
    | BTAndroid => 
        let val b = put_byte b 1 in
          b
        end
    | BTIPhone => 
        let val b = put_byte b 2 in
          b
        end
    | BTIPad => 
        let val b = put_byte b 3 in
          b
        end
    | BTIPod => 
        let val b = put_byte b 4 in
          b
        end
    | BTChrome => 
        let val b = put_byte b 5 in
          b
        end
    | BTIE => 
        let val b = put_byte b 6 in
          b
        end
    | BTIEMobile => 
        let val b = put_byte b 7 in
          b
        end
    | BTSafari => 
        let val b = put_byte b 8 in
          b
        end
    | BTOpera => 
        let val b = put_byte b 9 in
          b
        end
    | BTOperaMini => 
        let val b = put_byte b 10 in
          b
        end
    | BTFirefox => 
        let val b = put_byte b 11 in
          b
        end
    | BTVivaldi => 
        let val b = put_byte b 12 in
          b
        end
    | BTEdge => 
        let val b = put_byte b 13 in
          b
        end
  
fun put_appType (b : putBuf) (x : appType) : putBuf = 
  case x of
      ATUnknown => 
        let val b = put_byte b 0 in
          b
        end
    | ATFeeddler => 
        let val b = put_byte b 1 in
          b
        end
    | ATMrReader => 
        let val b = put_byte b 2 in
          b
        end
    | ATReeder => 
        let val b = put_byte b 3 in
          b
        end
    | ATSlowFeeds => 
        let val b = put_byte b 4 in
          b
        end
    | ATJustReader => 
        let val b = put_byte b 5 in
          b
        end
    | ATNewsPlus => 
        let val b = put_byte b 6 in
          b
        end
    | ATPress => 
        let val b = put_byte b 7 in
          b
        end
    | ATVienna => 
        let val b = put_byte b 8 in
          b
        end
    | ATReadKit => 
        let val b = put_byte b 9 in
          b
        end
    | ATNewsJet => 
        let val b = put_byte b 10 in
          b
        end
    | ATAmber => 
        let val b = put_byte b 11 in
          b
        end
    | ATgzip => 
        let val b = put_byte b 12 in
          b
        end
    | ATUnread => 
        let val b = put_byte b 13 in
          b
        end
    | ATFeedMe => 
        let val b = put_byte b 14 in
          b
        end
    | ATFieryFeeds => 
        let val b = put_byte b 15 in
          b
        end
    | ATLire => 
        let val b = put_byte b 16 in
          b
        end
    | ATWebSubscriber => 
        let val b = put_byte b 17 in
          b
        end
    | ATReadably => 
        let val b = put_byte b 18 in
          b
        end
    | ATokhttp => 
        let val b = put_byte b 19 in
          b
        end
    | ATFluentReader => 
        let val b = put_byte b 20 in
          b
        end
    | ATRavenReader => 
        let val b = put_byte b 21 in
          b
        end
    | ATFocusReader => 
        let val b = put_byte b 22 in
          b
        end
    | ATNetNewsWire => 
        let val b = put_byte b 23 in
          b
        end
  
fun put_operatingSystem (b : putBuf) (x : operatingSystem) : putBuf = 
  case x of
      OSUnknown => 
        let val b = put_byte b 0 in
          b
        end
    | OSWindows => 
        let val b = put_byte b 1 in
          b
        end
    | OSMac => 
        let val b = put_byte b 2 in
          b
        end
    | OSLinux => 
        let val b = put_byte b 3 in
          b
        end
    | OSAndroid => 
        let val b = put_byte b 4 in
          b
        end
    | OSIOS => 
        let val b = put_byte b 5 in
          b
        end
    | OSChromeOS => 
        let val b = put_byte b 6 in
          b
        end
  
fun put_usageFlag (b : putBuf) (x : usageFlag) : putBuf = 
  case x of
      UFWeb x => 
        let val b = put_byte b 0 in
        let val b = put_browserType b x.BrowserType in
        let val b = put_operatingSystem b x.OperatingSystem in
          b
        end
        end
        end
    | UFApp x => 
        let val b = put_byte b 1 in
        let val b = put_appType b x.AppType in
        let val b = put_operatingSystem b x.OperatingSystem in
          b
        end
        end
        end
    | UFShareAction x => 
        let val b = put_byte b 2 in
        let val b = put_shareAction b x.ShareAction in
          b
        end
        end
    | UFOPML => 
        let val b = put_byte b 3 in
          b
        end
    | UFAddSubscription => 
        let val b = put_byte b 4 in
          b
        end
    | UFSearchSubscriptions => 
        let val b = put_byte b 5 in
          b
        end
    | UFDiscoverySubscription => 
        let val b = put_byte b 6 in
          b
        end
    | UFAddDiscoverySubscription => 
        let val b = put_byte b 7 in
          b
        end
    | UFUnsubscribe => 
        let val b = put_byte b 8 in
          b
        end
    | UFRetrySubscription => 
        let val b = put_byte b 9 in
          b
        end
    | UFRenameSubscription => 
        let val b = put_byte b 10 in
          b
        end
    | UFRenameFolder => 
        let val b = put_byte b 11 in
          b
        end
    | UFEditSubscriptionFolders => 
        let val b = put_byte b 12 in
          b
        end
    | UFDragAndDrop => 
        let val b = put_byte b 13 in
          b
        end
    | UFSearch => 
        let val b = put_byte b 14 in
          b
        end
    | UFSearchTags => 
        let val b = put_byte b 15 in
          b
        end
    | UFSkip => 
        let val b = put_byte b 16 in
          b
        end
    | UFIgnore => 
        let val b = put_byte b 17 in
          b
        end
    | UFKeepUnread => 
        let val b = put_byte b 18 in
          b
        end
    | UFMarkAllAsRead => 
        let val b = put_byte b 19 in
          b
        end
    | UFStar => 
        let val b = put_byte b 20 in
          b
        end
    | UFTag => 
        let val b = put_byte b 21 in
          b
        end
    | UFReadability => 
        let val b = put_byte b 22 in
          b
        end
    | UFSetUsername => 
        let val b = put_byte b 23 in
          b
        end
    | UFEnablePublicFeed => 
        let val b = put_byte b 24 in
          b
        end
    | UFDisablePublicFeed => 
        let val b = put_byte b 25 in
          b
        end
    | UFGenerateNewPublicFeed => 
        let val b = put_byte b 26 in
          b
        end
    | UFDeleteAccount => 
        let val b = put_byte b 27 in
          b
        end
    | UFExportOPML => 
        let val b = put_byte b 28 in
          b
        end
    | UFMarkAllAsReadD x => 
        let val b = put_byte b 29 in
        let val b = put_int b x.OlderThan in
          b
        end
        end
    | UFMarkSearchAsReadD x => 
        let val b = put_byte b 30 in
        let val b = put_int b x.OlderThan in
          b
        end
        end
    | UFFilterApply => 
        let val b = put_byte b 31 in
          b
        end
    | UFFilterHide => 
        let val b = put_byte b 32 in
          b
        end
    | UFNewSmartStream => 
        let val b = put_byte b 33 in
          b
        end
    | UFEditFilter => 
        let val b = put_byte b 34 in
          b
        end
    | UFEditSmartStream => 
        let val b = put_byte b 35 in
          b
        end
    | UFDeleteFilter => 
        let val b = put_byte b 36 in
          b
        end
    | UFDeleteSmartStream => 
        let val b = put_byte b 37 in
          b
        end
    | UFWhatsNewClick x => 
        let val b = put_byte b 38 in
        let val b = put_time b x.Time in
          b
        end
        end
    | UFWhatsNewClose x => 
        let val b = put_byte b 39 in
        let val b = put_time b x.Time in
          b
        end
        end
    | UFThemeChange x => 
        let val b = put_byte b 40 in
        let val b = put_string b x.ThemeName in
          b
        end
        end
    | UFFontChange x => 
        let val b = put_byte b 41 in
        let val b = put_string b x.FontName in
          b
        end
        end
    | UFFontSizeChange x => 
        let val b = put_byte b 42 in
        let val b = put_int b x.Size in
          b
        end
        end
    | UFLineHeightChange x => 
        let val b = put_byte b 43 in
        let val b = put_int b x.Pixels in
        let val b = put_int b x.FontSize in
          b
        end
        end
        end
    | UFSetPassword => 
        let val b = put_byte b 44 in
          b
        end
    | UFSetEmail => 
        let val b = put_byte b 45 in
          b
        end
    | UFMarkReadAbove => 
        let val b = put_byte b 46 in
          b
        end
    | UFMarkReadBelow => 
        let val b = put_byte b 47 in
          b
        end
    | UFUnstarAbove => 
        let val b = put_byte b 48 in
          b
        end
    | UFUnstarBelow => 
        let val b = put_byte b 49 in
          b
        end
    | UFUntagAbove => 
        let val b = put_byte b 50 in
          b
        end
    | UFUntagBelow => 
        let val b = put_byte b 51 in
          b
        end
  
fun put_markReq (b : putBuf) (x : markReq) : putBuf = 
  case x of
      MRPosts x => 
        let val b = put_byte b 0 in
        let val b = put_list ((fn (b : putBuf) t => let val b = put_int b t.1 in
        let val b = (fn (b : putBuf) t => let val b = put_int b t.1 in
        let val b = put_int b t.2 in
          b
        end
        end
        ) b t.2 in
          b
        end
        end
        )) b x.FeedTcs in
          b
        end
        end
    | MRTags x => 
        let val b = put_byte b 1 in
        let val b = put_option (put_list (put_itemTag)) b x.Tags in
        let val b = put_option ((fn (b : putBuf) t => let val b = put_time b t.1 in
        let val b = put_msgId b t.2 in
          b
        end
        end
        )) b x.MaxTag in
          b
        end
        end
        end
    | MRSmartStream x => 
        let val b = put_byte b 2 in
        let val b = put_string b x.StreamName in
        let val b = put_list ((fn (b : putBuf) t => let val b = put_int b t.1 in
        let val b = (fn (b : putBuf) t => let val b = put_int b t.1 in
        let val b = put_int b t.2 in
          b
        end
        end
        ) b t.2 in
          b
        end
        end
        )) b x.FeedTcs in
          b
        end
        end
        end
    | MRSearchPosts x => 
        let val b = put_byte b 3 in
        let val b = put_string b x.Query in
        let val b = put_string b x.FeedMasksKey in
        let val b = put_list ((fn (b : putBuf) t => let val b = put_int b t.1 in
        let val b = (fn (b : putBuf) t => let val b = put_int b t.1 in
        let val b = put_int b t.2 in
          b
        end
        end
        ) b t.2 in
          b
        end
        end
        )) b x.FeedTcs in
          b
        end
        end
        end
        end
    | MRSearchTags x => 
        let val b = put_byte b 4 in
        let val b = put_string b x.Query in
        let val b = put_string b x.IdsKey in
        let val b = put_option (put_list (put_itemTag)) b x.Tags in
        let val b = put_option ((fn (b : putBuf) t => let val b = put_time b t.1 in
        let val b = put_msgId b t.2 in
          b
        end
        end
        )) b x.MaxTag in
          b
        end
        end
        end
        end
        end
    | MRSearchSmartStream x => 
        let val b = put_byte b 5 in
        let val b = put_string b x.StreamName in
        let val b = put_string b x.Query in
        let val b = put_string b x.FeedMasksKey in
        let val b = put_list ((fn (b : putBuf) t => let val b = put_int b t.1 in
        let val b = (fn (b : putBuf) t => let val b = put_int b t.1 in
        let val b = put_int b t.2 in
          b
        end
        end
        ) b t.2 in
          b
        end
        end
        )) b x.FeedTcs in
          b
        end
        end
        end
        end
        end
  
fun put_markReadDirection (b : putBuf) (x : markReadDirection) : putBuf = 
  case x of
      MRDAll => 
        let val b = put_byte b 0 in
          b
        end
    | MRDAbove x => 
        let val b = put_byte b 1 in
        let val b = (fn (b : putBuf) t => let val b = put_time b t.1 in
        let val b = (fn (b : putBuf) t => let val b = put_bool b t.1 in
        let val b = put_msgId b t.2 in
          b
        end
        end
        ) b t.2 in
        let val b = (fn (b : putBuf) t => let val b = put_bool b t.1 in
        let val b = put_msgId b t.2 in
          b
        end
        end
        ) b t.3 in
          b
        end
        end
        end
        ) b x.Point in
          b
        end
        end
    | MRDBelow x => 
        let val b = put_byte b 2 in
        let val b = (fn (b : putBuf) t => let val b = put_time b t.1 in
        let val b = (fn (b : putBuf) t => let val b = put_bool b t.1 in
        let val b = put_msgId b t.2 in
          b
        end
        end
        ) b t.2 in
        let val b = (fn (b : putBuf) t => let val b = put_bool b t.1 in
        let val b = put_msgId b t.2 in
          b
        end
        end
        ) b t.3 in
          b
        end
        end
        end
        ) b x.Point in
          b
        end
        end
  
fun put_bgAction (b : putBuf) (x : bgAction) : putBuf = 
  case x of
      BGMarkMsgRead x => 
        let val b = put_byte b 0 in
        let val b = put_msgId b x.MsgId in
        let val b = put_bool b x.Read in
        let val b = put_int b x.TotalComments in
          b
        end
        end
        end
        end
    | BGAddTag x => 
        let val b = put_byte b 1 in
        let val b = put_longMsgId b x.LongMsgId in
        let val b = put_itemTag b x.Tag in
          b
        end
        end
        end
    | BGRemoveTag x => 
        let val b = put_byte b 2 in
        let val b = put_longMsgId b x.LongMsgId in
        let val b = put_itemTag b x.Tag in
          b
        end
        end
        end
    | BGSkipComments x => 
        let val b = put_byte b 3 in
        let val b = put_msgId b x.MsgId in
        let val b = put_int b x.TotalComments in
          b
        end
        end
        end
    | BGIgnorePost x => 
        let val b = put_byte b 4 in
        let val b = put_msgId b x.MsgId in
        let val b = put_int b x.TotalComments in
          b
        end
        end
        end
    | BGMarkRead x => 
        let val b = put_byte b 5 in
        let val b = put_markReadDirection b x.Direction in
        let val b = put_int b x.OlderThan in
        let val b = put_msgTreeViewMode b x.ViewMode in
        let val b = put_list ((fn (b : putBuf) t => let val b = put_time b t.1 in
        let val b = put_msgId b t.2 in
          b
        end
        end
        )) b x.Posts in
        let val b = put_markReq b x.MarkReq in
          b
        end
        end
        end
        end
        end
        end
    | BGRemoveTagFromTree x => 
        let val b = put_byte b 6 in
        let val b = put_bool b x.Above in
        let val b = put_option (put_list (put_itemTag)) b x.Tags in
        let val b = put_msgTreeViewMode b x.ViewMode in
        let val b = put_list (put_treeReq) b x.TreeReqs in
          b
        end
        end
        end
        end
        end
    | BGRemoveTagD x => 
        let val b = put_byte b 7 in
        let val b = put_option (put_list (put_itemTag)) b x.Tags in
        let val b = put_int b x.OlderThan in
          b
        end
        end
        end
    | BGSetOnlyUpdatedSubscriptions x => 
        let val b = put_byte b 8 in
        let val b = put_bool b x.Value in
          b
        end
        end
    | BGSetFolderViewMode x => 
        let val b = put_byte b 9 in
        let val b = put_string b x.Folder in
        let val b = put_msgTreeViewMode b x.ViewMode in
          b
        end
        end
        end
    | BGSetSubscriptionViewMode x => 
        let val b = put_byte b 10 in
        let val b = put_string b x.Url in
        let val b = put_msgTreeViewMode b x.ViewMode in
          b
        end
        end
        end
    | BGClearAllSubscriptions => 
        let val b = put_byte b 11 in
          b
        end
    | BGSaveFilterQuery x => 
        let val b = put_byte b 12 in
        let val b = put_string b x.Query in
          b
        end
        end
    | BGSetScrollMode x => 
        let val b = put_byte b 13 in
        let val b = put_scrollMode b x.ScrollMode in
          b
        end
        end
    | BGSetListViewMode x => 
        let val b = put_byte b 14 in
        let val b = put_listViewMode b x.ListViewMode in
          b
        end
        end
    | BGSetMarkReadMode x => 
        let val b = put_byte b 15 in
        let val b = put_markReadMode b x.MarkReadMode in
          b
        end
        end
    | BGSetUltraCompact x => 
        let val b = put_byte b 16 in
        let val b = put_bool b x.UltraCompact in
          b
        end
        end
    | BGDragAndDrop x => 
        let val b = put_byte b 17 in
        let val b = put_subItemType b x.What in
        let val b = put_option (put_subItemType) b x.InsertAfter in
        let val b = put_option (put_string) b x.SourceFolder in
        let val b = put_option (put_string) b x.TargetFolder in
          b
        end
        end
        end
        end
        end
    | BGSetExactUnreadCounts x => 
        let val b = put_byte b 18 in
        let val b = put_bool b x.Value in
          b
        end
        end
    | BGSortAllFeedsAndFolders => 
        let val b = put_byte b 19 in
          b
        end
    | BGSortFolder x => 
        let val b = put_byte b 20 in
        let val b = put_string b x.Folder in
          b
        end
        end
    | BGSortTags => 
        let val b = put_byte b 21 in
          b
        end
    | BGShareAction x => 
        let val b = put_byte b 22 in
        let val b = put_shareAction b x.ShareAction in
          b
        end
        end
    | BGSetCountry x => 
        let val b = put_byte b 23 in
        let val b = put_string b x.Country in
          b
        end
        end
    | BGWhatsNewClick x => 
        let val b = put_byte b 24 in
        let val b = put_time b x.Time in
          b
        end
        end
    | BGWhatsNewClose x => 
        let val b = put_byte b 25 in
        let val b = put_time b x.Time in
          b
        end
        end
  
fun put_feedsOrDiscovery (b : putBuf) (x : feedsOrDiscovery) : putBuf = 
  case x of
      FODFeeds x => 
        let val b = put_byte b 0 in
        let val b = put_list ((fn (b : putBuf) t => let val b = put_int b t.1 in
        let val b = put_int b t.2 in
        let val b = put_int b t.3 in
        let val b = put_int b t.4 in
        let val b = put_int b t.5 in
          b
        end
        end
        end
        end
        end
        )) b x.ReadCounters in
          b
        end
        end
    | FODFeedsApi x => 
        let val b = put_byte b 1 in
        let val b = put_apiMode b x.APIMode in
        let val b = put_list (put_string) b x.Feeds in
          b
        end
        end
        end
    | FODDiscovery x => 
        let val b = put_byte b 2 in
        let val b = put_string b x.Url in
          b
        end
        end
  
fun put_filterResults (b : putBuf) (x : filterResults) : putBuf = 
  case x of
    |  x => 

        let val b = put_int b x.TotalPosts in
        let val b = put_int b x.TotalComments in
        let val b = put_int b x.UnreadPosts in
        let val b = put_int b x.UnreadComments in
        let val b = put_int b x.Took in
        let val b = put_int b x.TookReal in
        let val b = put_msgForest b x.MsgForest in
          b
        end
        end
        end
        end
        end
        end
        end
  
fun put_emailAddress (b : putBuf) (x : emailAddress) : putBuf = 
  case x of
    |  x => 

        let val b = put_string b x.Email in
        let val b = put_string b x.FirstName in
        let val b = put_string b x.LastName in
          b
        end
        end
        end
  
fun put_okErrorRedirect (b : putBuf) (x : okErrorRedirect) : putBuf = 
  case x of
      OEROK => 
        let val b = put_byte b 0 in
          b
        end
    | OERError x => 
        let val b = put_byte b 1 in
        let val b = put_string b x.Error in
          b
        end
        end
    | OERRedirect x => 
        let val b = put_byte b 2 in
        let val b = put_string b x.Url in
          b
        end
        end
  
fun put_linkInfo (b : putBuf) (x : linkInfo) : putBuf = 
  case x of
    |  x => 

        let val b = put_url b x.Url in
        let val b = put_string b x.Title in
        let val b = put_string b x.Description in
        let val b = put_option (put_url) b x.Image in
        let val b = put_option (put_url) b x.Avatar in
          b
        end
        end
        end
        end
        end
  
fun put_feedbackEmail (b : putBuf) (x : feedbackEmail) : putBuf = 
  case x of
    |  x => 

        let val b = put_emailAddress b x.Address in
        let val b = put_time b x.Time in
        let val b = put_string b x.Subject in
        let val b = put_string b x.Text in
        let val b = put_int b x.Reserved1 in
        let val b = put_int b x.Reserved2 in
          b
        end
        end
        end
        end
        end
        end
  
fun put_feedbackUserInfo (b : putBuf) (x : feedbackUserInfo) : putBuf = 
  case x of
    |  x => 

        let val b = put_string b x.Id in
        let val b = put_option (put_string) b x.Who in
        let val b = put_paidTill b x.PaidTill in
        let val b = put_string b x.Country in
        let val b = put_list (put_usageFlag) b x.UsageFlags in
        let val b = put_time b x.LastUsedTime in
        let val b = put_bool b x.Deleted in
        let val b = put_list ((fn (b : putBuf) t => let val b = put_time b t.1 in
        let val b = put_string b t.2 in
        let val b = put_string b t.3 in
        let val b = put_emailAddress b t.4 in
          b
        end
        end
        end
        end
        )) b x.Payments in
        let val b = put_int b x.FeedsCount in
        let val b = put_int b x.ErrorFeedsCount in
        let val b = put_option (put_time) b x.ProcessedAt in
        let val b = put_option (put_feedbackEmail) b x.MailSent in
        let val b = put_option (put_time) b x.RepliedAt in
        let val b = put_list (put_string) b x.Tags in
        let val b = put_string b x.Notes in
        let val b = put_int b x.Reserved1 in
        let val b = put_int b x.Reserved2 in
        let val b = put_int b x.Reserved3 in
        let val b = put_int b x.Reserved4 in
          b
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
        end
  


val binary_subscriptionParentUrl : binary subscriptionParentUrl = mkBinary put_subscriptionParentUrl get_subscriptionParentUrl
val binary_subscriptionState : binary subscriptionState = mkBinary put_subscriptionState get_subscriptionState
val binary_subscription : binary subscription = mkBinary put_subscription get_subscription
val binary_postsViewMode : binary postsViewMode = mkBinary put_postsViewMode get_postsViewMode
val binary_mTVMEx : binary mTVMEx = mkBinary put_mTVMEx get_mTVMEx
val binary_msgTreeViewMode : binary msgTreeViewMode = mkBinary put_msgTreeViewMode get_msgTreeViewMode
val binary_payment : binary payment = mkBinary put_payment get_payment
val binary_paidTill : binary paidTill = mkBinary put_paidTill get_paidTill
val binary_scrollMode : binary scrollMode = mkBinary put_scrollMode get_scrollMode
val binary_listViewMode : binary listViewMode = mkBinary put_listViewMode get_listViewMode
val binary_markReadMode : binary markReadMode = mkBinary put_markReadMode get_markReadMode
val binary_publicFeedType : binary publicFeedType = mkBinary put_publicFeedType get_publicFeedType
val binary_loginAccessToken : binary loginAccessToken = mkBinary put_loginAccessToken get_loginAccessToken
val binary_apiKeys : binary apiKeys = mkBinary put_apiKeys get_apiKeys
val binary_userExperiment : binary userExperiment = mkBinary put_userExperiment get_userExperiment
val binary_customShareAction : binary customShareAction = mkBinary put_customShareAction get_customShareAction
val binary_shareAction : binary shareAction = mkBinary put_shareAction get_shareAction
val binary_msgButton : binary msgButton = mkBinary put_msgButton get_msgButton
val binary_emailContact : binary emailContact = mkBinary put_emailContact get_emailContact
val binary_sharingSettings : binary sharingSettings = mkBinary put_sharingSettings get_sharingSettings
val binary_loginType : binary loginType = mkBinary put_loginType get_loginType
val binary_userSettingsEx : binary userSettingsEx = mkBinary put_userSettingsEx get_userSettingsEx
val binary_userSettings : binary userSettings = mkBinary put_userSettings get_userSettings
val binary_uID : binary uID = mkBinary put_uID get_uID
val binary_session : binary session = mkBinary put_session get_session
val binary_attachment : binary attachment = mkBinary put_attachment get_attachment
val binary_msgKey : binary msgKey = mkBinary put_msgKey get_msgKey
val binary_msg : binary msg = mkBinary put_msg get_msg
val binary_msgHeader : binary msgHeader = mkBinary put_msgHeader get_msgHeader
val binary_commentsKey : binary commentsKey = mkBinary put_commentsKey get_commentsKey
val binary_itemTag : binary itemTag = mkBinary put_itemTag get_itemTag
val binary_filterQueryRpc : binary filterQueryRpc = mkBinary put_filterQueryRpc get_filterQueryRpc
val binary_searchError : binary searchError = mkBinary put_searchError get_searchError
val binary_apiMode : binary apiMode = mkBinary put_apiMode get_apiMode
val binary_msgTreePoint : binary msgTreePoint = mkBinary put_msgTreePoint get_msgTreePoint
val binary_postsReq : binary postsReq = mkBinary put_postsReq get_postsReq
val binary_commentsReq : binary commentsReq = mkBinary put_commentsReq get_commentsReq
val binary_msgId : binary msgId = mkBinary put_msgId get_msgId
val binary_longMsgId : binary longMsgId = mkBinary put_longMsgId get_longMsgId
val binary_treeReq : binary treeReq = mkBinary put_treeReq get_treeReq
val binary_msgView : binary msgView = mkBinary put_msgView get_msgView
val binary_msgItem : binary msgItem = mkBinary put_msgItem get_msgItem
val binary_msgForest : binary msgForest = mkBinary put_msgForest get_msgForest
val binary_externalLoginType : binary externalLoginType = mkBinary put_externalLoginType get_externalLoginType
val binary_externalLoginAction : binary externalLoginAction = mkBinary put_externalLoginAction get_externalLoginAction
val binary_counters : binary counters = mkBinary put_counters get_counters
val binary_subItemType : binary subItemType = mkBinary put_subItemType get_subItemType
val binary_subItemRpc : binary subItemRpc = mkBinary put_subItemRpc get_subItemRpc
val binary_welcomeState : binary welcomeState = mkBinary put_welcomeState get_welcomeState
val binary_updateFilters : binary updateFilters = mkBinary put_updateFilters get_updateFilters
val binary_browserType : binary browserType = mkBinary put_browserType get_browserType
val binary_appType : binary appType = mkBinary put_appType get_appType
val binary_operatingSystem : binary operatingSystem = mkBinary put_operatingSystem get_operatingSystem
val binary_usageFlag : binary usageFlag = mkBinary put_usageFlag get_usageFlag
val binary_markReq : binary markReq = mkBinary put_markReq get_markReq
val binary_markReadDirection : binary markReadDirection = mkBinary put_markReadDirection get_markReadDirection
val binary_bgAction : binary bgAction = mkBinary put_bgAction get_bgAction
val binary_feedsOrDiscovery : binary feedsOrDiscovery = mkBinary put_feedsOrDiscovery get_feedsOrDiscovery
val binary_filterResults : binary filterResults = mkBinary put_filterResults get_filterResults
val binary_emailAddress : binary emailAddress = mkBinary put_emailAddress get_emailAddress
val binary_okErrorRedirect : binary okErrorRedirect = mkBinary put_okErrorRedirect get_okErrorRedirect
val binary_linkInfo : binary linkInfo = mkBinary put_linkInfo get_linkInfo
val binary_feedbackEmail : binary feedbackEmail = mkBinary put_feedbackEmail get_feedbackEmail
val binary_feedbackUserInfo : binary feedbackUserInfo = mkBinary put_feedbackUserInfo get_feedbackUserInfo

