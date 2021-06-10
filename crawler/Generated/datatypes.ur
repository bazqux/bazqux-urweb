datatype subscriptionParentUrl
  = SpuRedirect of
    { Url   : string
    }
  | SpuHtml of
    { Url   : string
    , Debug : string
    }

datatype subscriptionState
  = SSAdded
  | SSScanning of
    { StartTime : time
    }
  | SSError of
    { Message   : string
    }
  | SSFeed of
    { Url       : string
    }
  | SSErrorPath of
    { Message   : string
    , Path      : list subscriptionParentUrl
    }

con subscription :: Type
  = { Url        : string
    , State      : subscriptionState
    , EditsCount : int
    , Title      : option string
    , Folders    : list string
    }

datatype postsViewMode
  = PVMShort
  | PVMFull
  | PVMMagazine
  | PVMMosaic

datatype mTVMEx
  = MTVMFolderCollapsed
  | MTVMFolderExpanded
  | MTVMEx of
    { FolderExpanded : bool
    , GroupByFeed    : bool
    , Reserved1      : bool
    , Reserved2      : int
    }

con msgTreeViewMode :: Type
  = { Ascending        : bool
    , UnreadOnly       : bool
    , ExpandedComments : bool
    , Posts            : postsViewMode
    , Ex               : mTVMEx
    , NoOverride       : bool
    }

datatype payment
  = PReserved
  | PFastSpring of
    { OrderId   : string
    , OrderType : string
    , OrderTime : time
    }

datatype paidTill
  = PTUnknown
  | PTPaid of
    { Till : time
    }
  | PTFreeTrial of
    { Till : time
    }
  | PTFreeTrialFinished of
    { Till : time
    }
  | PTPaidFinished of
    { Till : time
    }

datatype scrollMode
  = SMNormal
  | SMQuick
  | SMImmediate

datatype listViewMode
  = LVMCompact
  | LVMTwoLines

datatype markReadMode
  = MRMOnScroll
  | MRMManual
  | MRMOnScrollEverywhere

datatype publicFeedType
  = PFTAll
  | PFTFolder of
    { Folder     : string
    }
  | PFTTag of
    { TagName    : string
    }
  | PFTStarred
  | PFTAllTags
  | PFTSmartStream of
    { StreamName : string
    }

datatype loginAccessToken
  = LATNone
  | LATFacebook of
    { AccessToken : string
    }
  | LATTwitter of
    { Credentials : list (string * string)
    }

con apiKeys :: Type
  = { Pocket              : option (string * string)
    , PocketRequest       : option (string * string * string)
    , Reserved10          : option int
    , FacebookAccessToken : option (time * string)
    , TwitterAccessToken  : option (time * list (string * string))
    , Reserved13          : bool
    , Reserved14          : bool
    , Reserved15          : bool
    , Reserved16          : bool
    , Reserved17          : bool
    , Reserved2           : int
    , Reserved3           : int
    , Reserved4           : int
    }

datatype userExperiment
  = UENo9

con customShareAction :: Type
  = { Id        : int
    , Title     : string
    , UrlFormat : string
    , Shorten   : bool
    }

datatype shareAction
  = SAEMail
  | SATwitter
  | SAFacebook
  | SAGooglePlus
  | SATumblr
  | SAEvernote
  | SADelicious_discontinued
  | SAPinboard
  | SAPocket
  | SAReadability_discontinued
  | SAInstapaper
  | SATranslate
  | SABlogger
  | SAWordpress
  | SALinkedIn
  | SAPinterest
  | SAVK
  | SASkype
  | SAReddit
  | SAStumbleUpon
  | SADigg
  | SAScoopIt
  | SAFlipboard
  | SABuffer
  | SANewsVine
  | SADiigo
  | SARememberTheMilk
  | SAGoogleBookmarks
  | SAWallabag
  | SAWakelet
  | SACustom of
    { CustomShareAction : customShareAction
    }
  | SASystem

datatype msgButton
  = MBKeepUnread
  | MBStar
  | MBTag
  | MBShare
  | MBShareAction of
    { ShareAction : shareAction
    }

con emailContact :: Type
  = { EMail    : string
    , FullName : string
    , Groups   : list string
    , Avatar   : option string
    , Stats    : option (time * int)
    }

con sharingSettings :: Type
  = { ShareMenuButtons   : option (list msgButton)
    , MsgButtons         : option (list msgButton)
    , ListViewButtons    : option (list msgButton)
    , CustomShareActions : list customShareAction
    , EMailUsingMailto   : bool
    , ReplyToEMail       : option (string * string)
    , Contacts           : list emailContact
    , Reserved1          : int
    }

datatype loginType
  = LTGoogle of
    { Email    : string
    }
  | LTFacebook of
    { Email    : string
    }
  | LTTwitter of
    { Id       : string
    }
  | LTOpenId of
    { URL      : string
    }
  | LTEmail of
    { Email    : string
    }
  | LTUsername of
    { Username : string
    }
  | LTFeverApiKey of
    { ApiKey   : string
    }

con userSettingsEx :: Type
  = { LastWhatsNewTime       : time
    , PasswordHash           : option string
    , Reserved1_1            : option int
    , Reserved1_2            : option int
    , Reserved1_3            : option int
    , Reserved1_4            : option int
    , Reserved1_5            : option int
    , Reserved1_6            : option int
    , Reserved1_7            : option int
    , AssociatedAccounts     : list loginType
    , AssociatedAccountNames : list (loginType * string)
    , Reserved4              : int
    , Reserved5              : int
    , Reserved6              : int
    , Reserved7              : int
    , Reserved8              : int
    , Reserved9              : int
    , Reserved10             : int
    , Reserved11             : int
    , Reserved12             : int
    , Reserved13             : int
    , Reserved14             : int
    , Reserved15             : int
    }

con userSettings :: Type
  = { User              : string
    , EditsCount        : int
    , ScrollMode        : scrollMode
    , ListViewMode      : listViewMode
    , ShowFavicons      : bool
    , MarkReadMode      : markReadMode
    , UltraCompact      : bool
    , Reserved          : option string
    , ExactUnreadCounts : bool
    , PublicFeeds       : option (list (publicFeedType * list (string * bool * option string)))
    , Country           : option string
    , ApiKeys           : option apiKeys
    , Experiments       : option (list userExperiment)
    , SharingSettings_  : option sharingSettings
    , Ex                : option userSettingsEx
    }

datatype uID
  = EMail of
    { Id : string
    }
  | Url of
    { Id : string
    }

con session :: Type
  = { Key     : string
    , Expire  : time
    , Cleared : bool
    , User    : string
    }

datatype attachment
  = AImage of
    { Url         : url
    , Width       : option int
    , Height      : option int
    , Title       : option string
    }
  | AAudio of
    { Url         : url
    , Mime        : string
    , FileSize    : option int
    , Duration    : option int
    , Title       : option string
    }
  | AVideo of
    { Url         : url
    , Mime        : string
    , FileSize    : option int
    , Duration    : option int
    , Title       : option string
    , Width       : option int
    , Height      : option int
    , Poster      : option url
    }
  | AIframe of
    { Url         : url
    , Xml         : string
    , Duration    : option int
    , Title       : option string
    }
  | AOther of
    { Url         : url
    , Mime        : string
    , FileSize    : option int
    }
  | AGrOrigin of
    { Feed        : url
    , Guid        : string
    , StreamTitle : string
    , HtmlUrl     : string
    }
  | AVideo2 of
    { Url         : url
    , Mime        : string
    , FileSize    : option int
    , Duration    : option int
    , Title       : option string
    , Width       : option int
    , Height      : option int
    , Poster      : option url
    , Loop        : bool
    }
  | AThumbnail of
    { Url         : url
    }

con msgKey :: Type
  = { BlogFeedUrl : string
    , PostGuid    : option string
    , CommentGuid : option string
    }

con msg :: Type
  = { Key         : msgKey
    , Attachments : list attachment
    , Author      : string
    , AuthorUri   : option url
    , AuthorEmail : string
    , AuthorPic   : option url
    , Link        : option url
    , Subject     : string
    , Tags        : list string
    , Time        : option time
    , DlTime      : time
    , Text        : Binary_ffi.xbodyString
    , ShortText   : string
    , ShorterText : string
    }

con msgHeader :: Type
  = { Guid        : string
    , ContentHash : string
    , Author      : string
    , AuthorPic   : option url
    , Subject     : string
    , Time        : option time
    , DlTime      : time
    , ShortText   : string
    }

con commentsKey :: Type
  = { BlogFeedUrl : string
    , PostGuid    : string
    }

datatype itemTag
  = ITStarred
  | ITTag of
    { TagName : string
    }

con filterQueryRpc :: Type
  = { Query     : string
    , Negate    : bool
    , FeedGRIds : list int
    }

datatype searchError
  = SESyntaxError of
    { ErrorMessage : string
    }
  | SESystemError of
    { ErrorMessage : string
    }

datatype apiMode
  = AMNormal of
    { HostName         : string
    , AcceptLanguage   : string
    }
  | AMGRIdsOnly of
    { Fetch            : bool
    , Count            : int
    , Continuation     : option (option msgKey)
    , MinDlTime        : option time
    , MaxDlTime        : option time
    , MaxTime          : option time
    , ExcludeTags      : list itemTag
    , IncludeTags      : list itemTag
    , ReadOnly         : bool
    , MsgLinkParams    : list (string * string)
    , FromUI           : bool
    , MaxMsgTextLength : option int
    }
  | AMDiscovery of
    { HostName         : string
    , AcceptLanguage   : string
    , Url              : string
    }

con msgTreePoint :: Type
  = { ParentId : int
    , Time     : time
    , Id       : int
    }

con postsReq :: Type
  = { FeedId        : int
    , MsgTreePoint  : msgTreePoint
    , TotalPosts    : int
    , TotalComments : int
    }

con commentsReq :: Type
  = { Key           : commentsKey
    , PostId        : int
    , MsgTreePoint  : msgTreePoint
    , TotalComments : int
    }

con msgId :: Type
  = { FeedId    : int
    , PostId    : int
    , CommentId : option int
    }

con longMsgId :: Type
  = { MsgKey : msgKey
    , MsgId  : msgId
    }

datatype treeReq
  = TRPosts of
    { Reqs         : list postsReq
    }
  | TRTags of
    { Tags         : option (list itemTag)
    , MaxTag       : option (time * msgId)
    , LastMsg      : option (time * msgId)
    }
  | TRComments of
    { OnExpand     : bool
    , Req          : commentsReq
    }
  | TRCommentsS of
    { OnExpand     : bool
    , StreamName   : string
    , Req          : commentsReq
    }
  | TRSmartStream of
    { StreamName   : string
    , Reqs         : list postsReq
    }
  | TRSearchPosts of
    { Query        : string
    , FeedMasksKey : string
    , Reqs         : list postsReq
    }
  | TRSearchSmartStream of
    { StreamName   : string
    , Query        : string
    , FeedMasksKey : string
    , Reqs         : list postsReq
    }
  | TRSearchTags of
    { Query        : string
    , IdsKey       : string
    , Tags         : option (list itemTag)
    , MaxTag       : option (time * msgId)
    , LastMsg      : option (time * msgId)
    }

datatype msgView
  = MVFull of
    { Msg       : msg
    }
  | MVShort of
    { Header    : msgHeader
    , CachedMsg : option msg
    }

con msgItem :: Type
  = { MsgView      : msgView
    , MsgKey       : msgKey
    , MsgId        : msgId
    , Read         : bool
    , Tags         : list itemTag
    , SmartStreams : list int
    , ReadLocked   : bool
    , Full         : bool
    , SearchResult : bool
    }

datatype msgForest
  = MsgForest of
    { TotalCount                    : int
    , UnreadCount                   : int
    , TotalResultsCount             : int
    , UnreadResultsCount            : int
    , SmartStreamUnreadCounts       : list (int * int)
    , SmartStreamUnreadResultCounts : list (int * int)
    , TagTotalCounts                : list (option itemTag * int)
    , TagUnreadCounts               : list (option itemTag * int)
    , TagUnreadResultCounts         : list (option itemTag * int)
    , List                          : list (msgItem * msgForest)
    , NextReq                       : option treeReq
    }

datatype externalLoginType
  = Google
  | Facebook
  | Twitter
  | OpenId of
    { URL : string
    }

datatype externalLoginAction
  = ELALogin
  | ELAAddUrl of
    { URL : string
    }
  | ELAAddAssociatedAccount

con counters :: Type
  = { ReadPosts        : int
    , ReadComments     : int
    , TotalPosts       : int
    , TotalComments    : int
    , Scanning         : int
    , ScanningComments : int
    , Error            : int
    , Feed             : int
    , ScannedPercent   : int
    }

datatype subItemType
  = SITAll
  | SITSearch of
    { Query          : string
    }
  | SITFolder of
    { Folder         : string
    }
  | SITFeed of
    { Subscription   : subscription
    , FeedLink       : option string
    , PointAllDesc   : option msgTreePoint
    }
  | SITTag of
    { TagName        : string
    }
  | SITSmartStream of
    { StreamName     : string
    , StreamFeedSirs : list int
    }
  | SITStarred
  | SITAllTags

con subItemRpc :: Type
  = { Path          : string
    , Index         : int
    , Title         : string
    , SIType        : subItemType
    , Counters      : counters
    , ViewMode      : msgTreeViewMode
    , ParentFolders : list int
    , DomIds        : list int
    , FaviconStyle  : option string
    , GRId          : int
    }

con welcomeState :: Type
  = { HasPrevAccount  : bool
    , HasPrevSubs     : bool
    , StarredRestored : bool
    , TaggedRestored  : bool
    }

datatype updateFilters
  = UFNone
  | UFChanged
  | UFAll

datatype browserType
  = BTUnknown
  | BTAndroid
  | BTIPhone
  | BTIPad
  | BTIPod
  | BTChrome
  | BTIE
  | BTIEMobile
  | BTSafari
  | BTOpera
  | BTOperaMini
  | BTFirefox
  | BTVivaldi
  | BTEdge

datatype appType
  = ATUnknown
  | ATFeeddler
  | ATMrReader
  | ATReeder
  | ATSlowFeeds
  | ATJustReader
  | ATNewsPlus
  | ATPress
  | ATVienna
  | ATReadKit
  | ATNewsJet
  | ATAmber
  | ATgzip
  | ATUnread
  | ATFeedMe
  | ATFieryFeeds
  | ATLire
  | ATWebSubscriber
  | ATReadably
  | ATokhttp
  | ATFluentReader
  | ATRavenReader
  | ATFocusReader
  | ATNetNewsWire

datatype operatingSystem
  = OSUnknown
  | OSWindows
  | OSMac
  | OSLinux
  | OSAndroid
  | OSIOS
  | OSChromeOS

datatype usageFlag
  = UFWeb of
    { BrowserType     : browserType
    , OperatingSystem : operatingSystem
    }
  | UFApp of
    { AppType         : appType
    , OperatingSystem : operatingSystem
    }
  | UFShareAction of
    { ShareAction     : shareAction
    }
  | UFOPML
  | UFAddSubscription
  | UFSearchSubscriptions
  | UFDiscoverySubscription
  | UFAddDiscoverySubscription
  | UFUnsubscribe
  | UFRetrySubscription
  | UFRenameSubscription
  | UFRenameFolder
  | UFEditSubscriptionFolders
  | UFDragAndDrop
  | UFSearch
  | UFSearchTags
  | UFSkip
  | UFIgnore
  | UFKeepUnread
  | UFMarkAllAsRead
  | UFStar
  | UFTag
  | UFReadability
  | UFSetUsername
  | UFEnablePublicFeed
  | UFDisablePublicFeed
  | UFGenerateNewPublicFeed
  | UFDeleteAccount
  | UFExportOPML
  | UFMarkAllAsReadD of
    { OlderThan       : int
    }
  | UFMarkSearchAsReadD of
    { OlderThan       : int
    }
  | UFFilterApply
  | UFFilterHide
  | UFNewSmartStream
  | UFEditFilter
  | UFEditSmartStream
  | UFDeleteFilter
  | UFDeleteSmartStream
  | UFWhatsNewClick of
    { Time            : time
    }
  | UFWhatsNewClose of
    { Time            : time
    }
  | UFThemeChange of
    { ThemeName       : string
    }
  | UFFontChange of
    { FontName        : string
    }
  | UFFontSizeChange of
    { Size            : int
    }
  | UFLineHeightChange of
    { Pixels          : int
    , FontSize        : int
    }
  | UFSetPassword
  | UFSetEmail
  | UFMarkReadAbove
  | UFMarkReadBelow
  | UFUnstarAbove
  | UFUnstarBelow
  | UFUntagAbove
  | UFUntagBelow

datatype markReq
  = MRPosts of
    { FeedTcs      : list (int * (int * int))
    }
  | MRTags of
    { Tags         : option (list itemTag)
    , MaxTag       : option (time * msgId)
    }
  | MRSmartStream of
    { StreamName   : string
    , FeedTcs      : list (int * (int * int))
    }
  | MRSearchPosts of
    { Query        : string
    , FeedMasksKey : string
    , FeedTcs      : list (int * (int * int))
    }
  | MRSearchTags of
    { Query        : string
    , IdsKey       : string
    , Tags         : option (list itemTag)
    , MaxTag       : option (time * msgId)
    }
  | MRSearchSmartStream of
    { StreamName   : string
    , Query        : string
    , FeedMasksKey : string
    , FeedTcs      : list (int * (int * int))
    }

datatype markReadDirection
  = MRDAll
  | MRDAbove of
    { Point : (time * (bool * msgId) * (bool * msgId))
    }
  | MRDBelow of
    { Point : (time * (bool * msgId) * (bool * msgId))
    }

datatype bgAction
  = BGMarkMsgRead of
    { MsgId         : msgId
    , Read          : bool
    , TotalComments : int
    }
  | BGAddTag of
    { LongMsgId     : longMsgId
    , Tag           : itemTag
    }
  | BGRemoveTag of
    { LongMsgId     : longMsgId
    , Tag           : itemTag
    }
  | BGSkipComments of
    { MsgId         : msgId
    , TotalComments : int
    }
  | BGIgnorePost of
    { MsgId         : msgId
    , TotalComments : int
    }
  | BGMarkRead of
    { Direction     : markReadDirection
    , OlderThan     : int
    , ViewMode      : msgTreeViewMode
    , Posts         : list (time * msgId)
    , MarkReq       : markReq
    }
  | BGRemoveTagFromTree of
    { Above         : bool
    , Tags          : option (list itemTag)
    , ViewMode      : msgTreeViewMode
    , TreeReqs      : list treeReq
    }
  | BGRemoveTagD of
    { Tags          : option (list itemTag)
    , OlderThan     : int
    }
  | BGSetOnlyUpdatedSubscriptions of
    { Value         : bool
    }
  | BGSetFolderViewMode of
    { Folder        : string
    , ViewMode      : msgTreeViewMode
    }
  | BGSetSubscriptionViewMode of
    { Url           : string
    , ViewMode      : msgTreeViewMode
    }
  | BGClearAllSubscriptions
  | BGSaveFilterQuery of
    { Query         : string
    }
  | BGSetScrollMode of
    { ScrollMode    : scrollMode
    }
  | BGSetListViewMode of
    { ListViewMode  : listViewMode
    }
  | BGSetMarkReadMode of
    { MarkReadMode  : markReadMode
    }
  | BGSetUltraCompact of
    { UltraCompact  : bool
    }
  | BGDragAndDrop of
    { What          : subItemType
    , InsertAfter   : option subItemType
    , SourceFolder  : option string
    , TargetFolder  : option string
    }
  | BGSetExactUnreadCounts of
    { Value         : bool
    }
  | BGSortAllFeedsAndFolders
  | BGSortFolder of
    { Folder        : string
    }
  | BGSortTags
  | BGShareAction of
    { ShareAction   : shareAction
    }
  | BGSetCountry of
    { Country       : string
    }
  | BGWhatsNewClick of
    { Time          : time
    }
  | BGWhatsNewClose of
    { Time          : time
    }

datatype feedsOrDiscovery
  = FODFeeds of
    { ReadCounters : list (int * int * int * int * int)
    }
  | FODFeedsApi of
    { APIMode      : apiMode
    , Feeds        : list string
    }
  | FODDiscovery of
    { Url          : string
    }

con filterResults :: Type
  = { TotalPosts     : int
    , TotalComments  : int
    , UnreadPosts    : int
    , UnreadComments : int
    , Took           : int
    , TookReal       : int
    , MsgForest      : msgForest
    }

con emailAddress :: Type
  = { Email     : string
    , FirstName : string
    , LastName  : string
    }

datatype okErrorRedirect
  = OEROK
  | OERError of
    { Error : string
    }
  | OERRedirect of
    { Url   : string
    }

con linkInfo :: Type
  = { Url         : url
    , Title       : string
    , Description : string
    , Image       : option url
    , Avatar      : option url
    }

con feedbackEmail :: Type
  = { Address   : emailAddress
    , Time      : time
    , Subject   : string
    , Text      : string
    , Reserved1 : int
    , Reserved2 : int
    }

con feedbackUserInfo :: Type
  = { Id              : string
    , Who             : option string
    , PaidTill        : paidTill
    , Country         : string
    , UsageFlags      : list usageFlag
    , LastUsedTime    : time
    , Deleted         : bool
    , Payments        : list (time * string * string * emailAddress)
    , FeedsCount      : int
    , ErrorFeedsCount : int
    , ProcessedAt     : option time
    , MailSent        : option feedbackEmail
    , RepliedAt       : option time
    , Tags            : list string
    , Notes           : string
    , Reserved1       : int
    , Reserved2       : int
    , Reserved3       : int
    , Reserved4       : int
    }

