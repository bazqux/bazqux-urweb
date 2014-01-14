open Binary

open Ur_ffi

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

con subscription
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

con msgTreeViewMode
  = { Ascending        : bool
    , UnreadOnly       : bool
    , ExpandedComments : bool
    , Posts            : postsViewMode
    , FolderExpanded   : bool
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
    { Folder  : string
    }
  | PFTTag of
    { TagName : string
    }
  | PFTStarred
  | PFTAllTags

con userSettings
  = { User              : string
    , EditsCount        : int
    , ScrollMode        : scrollMode
    , ListViewMode      : listViewMode
    , ShowFavicons      : bool
    , MarkReadMode      : markReadMode
    , UltraCompact      : bool
    , MobileLogin       : option string
    , ExactUnreadCounts : bool
    , PublicFeeds       : option (assoc_list publicFeedType (list (string * bool * option string)))
    , Country           : option string
    , Reserved6         : option string
    , Reserved7         : option string
    , Reserved8         : option string
    , Reserved9         : option string
    }

datatype uID
  = EMail of
    { Id : string
    }
  | Url of
    { Id : string
    }

con session
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

con msgKey
  = { BlogFeedUrl : string
    , PostGuid    : option string
    , CommentGuid : option string
    }

con msg
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
    , Text        : string
    , ShortText   : string
    , Debug       : string
    }

con msgHeader
  = { Guid        : string
    , ContentHash : string
    , Author      : string
    , AuthorPic   : option url
    , Subject     : string
    , Time        : option time
    , DlTime      : time
    , ShortText   : string
    }

con commentsKey
  = { BlogFeedUrl : string
    , PostGuid    : string
    }

datatype itemTag
  = ITStarred
  | ITTag of
    { TagName : string
    }

datatype apiMode
  = AMNormal
  | AMGRIdsOnly of
    { Fetch        : bool
    , Count        : int
    , Continuation : option (option msgKey)
    , MinDlTime    : option time
    , MaxDlTime    : option time
    , MaxTime      : option time
    , ExcludeTags  : option itemTag
    , IncludeTags  : option itemTag
    , ReadOnly     : bool
    }
  | AMDiscovery of
    { Url          : string
    }

con msgTreePoint
  = { ParentId : int
    , Time     : time
    , Id       : int
    }

con postsReq
  = { BlogFeedUrl   : string
    , MsgTreePoint  : msgTreePoint
    , TotalPosts    : int
    , TotalComments : int
    }

con commentsReq
  = { Key           : commentsKey
    , PostId        : int
    , MsgTreePoint  : msgTreePoint
    , TotalComments : int
    }

datatype treeReq
  = TRPosts of
    { Reqs        : list postsReq
    }
  | TRComments of
    { OnExpand    : bool
    , Req         : commentsReq
    }
  | TRSearch of
    { Query       : string
    , Feeds       : list (string * int * int)
    , PostTime    : time
    , BlogFeedUrl : string
    , PostGuid    : string
    }
  | TRTags of
    { LastMsg     : option msgKey
    , Tags        : option (list itemTag)
    }

datatype msgView
  = MVFull of
    { Msg       : msg
    }
  | MVShort of
    { Header    : msgHeader
    , CachedMsg : option msg
    }

con msgId
  = { MsgKey    : msgKey
    , FeedId    : int
    , PostId    : int
    , CommentId : option int
    }

con msgItem
  = { MsgView    : msgView
    , MsgId      : msgId
    , Read       : bool
    , Starred    : bool
    , Tags       : list string
    , ReadLocked : bool
    }

datatype msgForest
  = MsgForest of
    { ResultsCount : int
    , UnreadCount  : int
    , List         : list (msgItem * msgForest)
    , NextReq      : option treeReq
    }

datatype loginType
  = Google
  | Facebook
  | Twitter
  | OpenId of
    { URL : string
    }

con counters
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
    { Query        : string
    }
  | SITFolder of
    { Folder       : string
    }
  | SITFeed of
    { Subscription : subscription
    , FeedLink     : option string
    , PointAllDesc : option msgTreePoint
    }
  | SITTag of
    { TagName      : string
    }
  | SITStarred
  | SITAllTags

con subItemRpc
  = { Hash          : string
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

datatype shareAction
  = SAEMail
  | SATwitter
  | SAFacebook
  | SAGooglePlus
  | SATumblr
  | SAEvernote
  | SADelicious
  | SAPinboard
  | SAPocket
  | SAReadability
  | SAInstapaper
  | SATranslate

datatype bgAction
  = BGMarkMsgRead of
    { MsgId         : msgId
    , Read          : bool
    , TotalComments : int
    }
  | BGAddTag of
    { MsgId         : msgId
    , Tag           : itemTag
    }
  | BGRemoveTag of
    { MsgId         : msgId
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
  | BGMarkBlogRead of
    { BlogFeedUrl   : string
    , TotalPosts    : int
    , TotalComments : int
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

con searchResults
  = { Total     : int
    , Took      : int
    , TookReal  : int
    , MsgForest : msgForest
    }

con fullTextCache
  = { Url       : url
    , Text      : either string string
    , Time      : time
    , Reserved1 : bool
    , Reserved2 : bool
    }

fun recurseGets () = ()
and get_subscriptionState b : (getBuf * subscriptionState) = 
  let val (b, c) = get_char b in case ord c of
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
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
  end
and get_subscription b : (getBuf * subscription) = 
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
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
and get_postsViewMode b : (getBuf * postsViewMode) = 
  let val (b, c) = get_char b in case ord c of
      0 => 
          (b, PVMShort)
    | 1 => 
          (b, PVMFull)
    | 2 => 
          (b, PVMMagazine)
    | 3 => 
          (b, PVMMosaic)
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
  end
and get_msgTreeViewMode b : (getBuf * msgTreeViewMode) = 
  case 0 of
      0 => 
        let val (b, _Ascending) = get_bool b  in
        let val (b, _UnreadOnly) = get_bool b  in
        let val (b, _ExpandedComments) = get_bool b  in
        let val (b, _Posts) = get_postsViewMode b  in
        let val (b, _FolderExpanded) = get_bool b  in
        let val (b, _NoOverride) = get_bool b  in
          (b,
           { Ascending = _Ascending
           , UnreadOnly = _UnreadOnly
           , ExpandedComments = _ExpandedComments
           , Posts = _Posts
           , FolderExpanded = _FolderExpanded
           , NoOverride = _NoOverride
           })
        end
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
and get_payment b : (getBuf * payment) = 
  let val (b, c) = get_char b in case ord c of
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
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
  end
and get_paidTill b : (getBuf * paidTill) = 
  let val (b, c) = get_char b in case ord c of
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
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
  end
and get_scrollMode b : (getBuf * scrollMode) = 
  let val (b, c) = get_char b in case ord c of
      0 => 
          (b, SMNormal)
    | 1 => 
          (b, SMQuick)
    | 2 => 
          (b, SMImmediate)
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
  end
and get_listViewMode b : (getBuf * listViewMode) = 
  let val (b, c) = get_char b in case ord c of
      0 => 
          (b, LVMCompact)
    | 1 => 
          (b, LVMTwoLines)
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
  end
and get_markReadMode b : (getBuf * markReadMode) = 
  let val (b, c) = get_char b in case ord c of
      0 => 
          (b, MRMOnScroll)
    | 1 => 
          (b, MRMManual)
    | 2 => 
          (b, MRMOnScrollEverywhere)
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
  end
and get_publicFeedType b : (getBuf * publicFeedType) = 
  let val (b, c) = get_char b in case ord c of
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
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
  end
and get_userSettings b : (getBuf * userSettings) = 
  case 0 of
      0 => 
        let val (b, _User) = get_string b  in
        let val (b, _EditsCount) = get_int b  in
        let val (b, _ScrollMode) = get_scrollMode b  in
        let val (b, _ListViewMode) = get_listViewMode b  in
        let val (b, _ShowFavicons) = get_bool b  in
        let val (b, _MarkReadMode) = get_markReadMode b  in
        let val (b, _UltraCompact) = get_bool b  in
        let val (b, _MobileLogin) = get_option (get_string) b  in
        let val (b, _ExactUnreadCounts) = get_bool b  in
        let val (b, _PublicFeeds) = get_option (get_list ((fn b =>let val (b, _1) = get_publicFeedType b  in
        let val (b, _2) = get_list ((fn b =>let val (b, _1) = get_string b  in
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
        let val (b, _Reserved6) = get_option (get_string) b  in
        let val (b, _Reserved7) = get_option (get_string) b  in
        let val (b, _Reserved8) = get_option (get_string) b  in
        let val (b, _Reserved9) = get_option (get_string) b  in
          (b,
           { User = _User
           , EditsCount = _EditsCount
           , ScrollMode = _ScrollMode
           , ListViewMode = _ListViewMode
           , ShowFavicons = _ShowFavicons
           , MarkReadMode = _MarkReadMode
           , UltraCompact = _UltraCompact
           , MobileLogin = _MobileLogin
           , ExactUnreadCounts = _ExactUnreadCounts
           , PublicFeeds = _PublicFeeds
           , Country = _Country
           , Reserved6 = _Reserved6
           , Reserved7 = _Reserved7
           , Reserved8 = _Reserved8
           , Reserved9 = _Reserved9
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
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
and get_uID b : (getBuf * uID) = 
  let val (b, c) = get_char b in case ord c of
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
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
  end
and get_session b : (getBuf * session) = 
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
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
and get_attachment b : (getBuf * attachment) = 
  let val (b, c) = get_char b in case ord c of
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
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
  end
and get_msgKey b : (getBuf * msgKey) = 
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
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
and get_msg b : (getBuf * msg) = 
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
        let val (b, _Text) = get_string b  in
        let val (b, _ShortText) = get_string b  in
        let val (b, _Debug) = get_string b  in
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
           , Debug = _Debug
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
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
and get_msgHeader b : (getBuf * msgHeader) = 
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
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
and get_commentsKey b : (getBuf * commentsKey) = 
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
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
and get_itemTag b : (getBuf * itemTag) = 
  let val (b, c) = get_char b in case ord c of
      0 => 
          (b, ITStarred)
    | 1 => 
        let val (b, _TagName) = get_string b  in
          (b,
           ITTag
           { TagName = _TagName
           })
        end
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
  end
and get_apiMode b : (getBuf * apiMode) = 
  let val (b, c) = get_char b in case ord c of
      0 => 
          (b, AMNormal)
    | 1 => 
        let val (b, _Fetch) = get_bool b  in
        let val (b, _Count) = get_int b  in
        let val (b, _Continuation) = get_option (get_option (get_msgKey)) b  in
        let val (b, _MinDlTime) = get_option (get_time) b  in
        let val (b, _MaxDlTime) = get_option (get_time) b  in
        let val (b, _MaxTime) = get_option (get_time) b  in
        let val (b, _ExcludeTags) = get_option (get_itemTag) b  in
        let val (b, _IncludeTags) = get_option (get_itemTag) b  in
        let val (b, _ReadOnly) = get_bool b  in
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
    | 2 => 
        let val (b, _Url) = get_string b  in
          (b,
           AMDiscovery
           { Url = _Url
           })
        end
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
  end
and get_msgTreePoint b : (getBuf * msgTreePoint) = 
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
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
and get_postsReq b : (getBuf * postsReq) = 
  case 0 of
      0 => 
        let val (b, _BlogFeedUrl) = get_string b  in
        let val (b, _MsgTreePoint) = get_msgTreePoint b  in
        let val (b, _TotalPosts) = get_int b  in
        let val (b, _TotalComments) = get_int b  in
          (b,
           { BlogFeedUrl = _BlogFeedUrl
           , MsgTreePoint = _MsgTreePoint
           , TotalPosts = _TotalPosts
           , TotalComments = _TotalComments
           })
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
and get_commentsReq b : (getBuf * commentsReq) = 
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
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
and get_treeReq b : (getBuf * treeReq) = 
  let val (b, c) = get_char b in case ord c of
      0 => 
        let val (b, _Reqs) = get_list (get_postsReq) b  in
          (b,
           TRPosts
           { Reqs = _Reqs
           })
        end
    | 1 => 
        let val (b, _OnExpand) = get_bool b  in
        let val (b, _Req) = get_commentsReq b  in
          (b,
           TRComments
           { OnExpand = _OnExpand
           , Req = _Req
           })
        end
        end
    | 2 => 
        let val (b, _Query) = get_string b  in
        let val (b, _Feeds) = get_list ((fn b =>let val (b, _1) = get_string b  in
        let val (b, _2) = get_int b  in
        let val (b, _3) = get_int b  in
          (b,(_1, _2, _3))
        end
        end
        end
        )) b  in
        let val (b, _PostTime) = get_time b  in
        let val (b, _BlogFeedUrl) = get_string b  in
        let val (b, _PostGuid) = get_string b  in
          (b,
           TRSearch
           { Query = _Query
           , Feeds = _Feeds
           , PostTime = _PostTime
           , BlogFeedUrl = _BlogFeedUrl
           , PostGuid = _PostGuid
           })
        end
        end
        end
        end
        end
    | 3 => 
        let val (b, _LastMsg) = get_option (get_msgKey) b  in
        let val (b, _Tags) = get_option (get_list (get_itemTag)) b  in
          (b,
           TRTags
           { LastMsg = _LastMsg
           , Tags = _Tags
           })
        end
        end
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
  end
and get_msgView b : (getBuf * msgView) = 
  let val (b, c) = get_char b in case ord c of
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
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
  end
and get_msgId b : (getBuf * msgId) = 
  case 0 of
      0 => 
        let val (b, _MsgKey) = get_msgKey b  in
        let val (b, _FeedId) = get_int b  in
        let val (b, _PostId) = get_int b  in
        let val (b, _CommentId) = get_option (get_int) b  in
          (b,
           { MsgKey = _MsgKey
           , FeedId = _FeedId
           , PostId = _PostId
           , CommentId = _CommentId
           })
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
and get_msgItem b : (getBuf * msgItem) = 
  case 0 of
      0 => 
        let val (b, _MsgView) = get_msgView b  in
        let val (b, _MsgId) = get_msgId b  in
        let val (b, _Read) = get_bool b  in
        let val (b, _Starred) = get_bool b  in
        let val (b, _Tags) = get_list (get_string) b  in
        let val (b, _ReadLocked) = get_bool b  in
          (b,
           { MsgView = _MsgView
           , MsgId = _MsgId
           , Read = _Read
           , Starred = _Starred
           , Tags = _Tags
           , ReadLocked = _ReadLocked
           })
        end
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
and get_msgForest b : (getBuf * msgForest) = 
  case 0 of
      0 => 
        let val (b, _ResultsCount) = get_int b  in
        let val (b, _UnreadCount) = get_int b  in
        let val (b, _List) = get_list ((fn b =>let val (b, _1) = get_msgItem b  in
        let val (b, _2) = get_msgForest b  in
          (b,(_1, _2))
        end
        end
        )) b  in
        let val (b, _NextReq) = get_option (get_treeReq) b  in
          (b,
           MsgForest
           { ResultsCount = _ResultsCount
           , UnreadCount = _UnreadCount
           , List = _List
           , NextReq = _NextReq
           })
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
and get_loginType b : (getBuf * loginType) = 
  let val (b, c) = get_char b in case ord c of
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
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
  end
and get_counters b : (getBuf * counters) = 
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
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
and get_subItemType b : (getBuf * subItemType) = 
  let val (b, c) = get_char b in case ord c of
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
          (b, SITStarred)
    | 6 => 
          (b, SITAllTags)
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
  end
and get_subItemRpc b : (getBuf * subItemRpc) = 
  case 0 of
      0 => 
        let val (b, _Hash) = get_string b  in
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
           { Hash = _Hash
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
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
and get_shareAction b : (getBuf * shareAction) = 
  let val (b, c) = get_char b in case ord c of
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
          (b, SADelicious)
    | 7 => 
          (b, SAPinboard)
    | 8 => 
          (b, SAPocket)
    | 9 => 
          (b, SAReadability)
    | 10 => 
          (b, SAInstapaper)
    | 11 => 
          (b, SATranslate)
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
  end
and get_bgAction b : (getBuf * bgAction) = 
  let val (b, c) = get_char b in case ord c of
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
        let val (b, _MsgId) = get_msgId b  in
        let val (b, _Tag) = get_itemTag b  in
          (b,
           BGAddTag
           { MsgId = _MsgId
           , Tag = _Tag
           })
        end
        end
    | 2 => 
        let val (b, _MsgId) = get_msgId b  in
        let val (b, _Tag) = get_itemTag b  in
          (b,
           BGRemoveTag
           { MsgId = _MsgId
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
        let val (b, _BlogFeedUrl) = get_string b  in
        let val (b, _TotalPosts) = get_int b  in
        let val (b, _TotalComments) = get_int b  in
          (b,
           BGMarkBlogRead
           { BlogFeedUrl = _BlogFeedUrl
           , TotalPosts = _TotalPosts
           , TotalComments = _TotalComments
           })
        end
        end
        end
    | 6 => 
        let val (b, _Value) = get_bool b  in
          (b,
           BGSetOnlyUpdatedSubscriptions
           { Value = _Value
           })
        end
    | 7 => 
        let val (b, _Folder) = get_string b  in
        let val (b, _ViewMode) = get_msgTreeViewMode b  in
          (b,
           BGSetFolderViewMode
           { Folder = _Folder
           , ViewMode = _ViewMode
           })
        end
        end
    | 8 => 
        let val (b, _Url) = get_string b  in
        let val (b, _ViewMode) = get_msgTreeViewMode b  in
          (b,
           BGSetSubscriptionViewMode
           { Url = _Url
           , ViewMode = _ViewMode
           })
        end
        end
    | 9 => 
          (b, BGClearAllSubscriptions)
    | 10 => 
        let val (b, _Query) = get_string b  in
          (b,
           BGSaveFilterQuery
           { Query = _Query
           })
        end
    | 11 => 
        let val (b, _ScrollMode) = get_scrollMode b  in
          (b,
           BGSetScrollMode
           { ScrollMode = _ScrollMode
           })
        end
    | 12 => 
        let val (b, _ListViewMode) = get_listViewMode b  in
          (b,
           BGSetListViewMode
           { ListViewMode = _ListViewMode
           })
        end
    | 13 => 
        let val (b, _MarkReadMode) = get_markReadMode b  in
          (b,
           BGSetMarkReadMode
           { MarkReadMode = _MarkReadMode
           })
        end
    | 14 => 
        let val (b, _UltraCompact) = get_bool b  in
          (b,
           BGSetUltraCompact
           { UltraCompact = _UltraCompact
           })
        end
    | 15 => 
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
    | 16 => 
        let val (b, _Value) = get_bool b  in
          (b,
           BGSetExactUnreadCounts
           { Value = _Value
           })
        end
    | 17 => 
          (b, BGSortAllFeedsAndFolders)
    | 18 => 
        let val (b, _Folder) = get_string b  in
          (b,
           BGSortFolder
           { Folder = _Folder
           })
        end
    | 19 => 
          (b, BGSortTags)
    | 20 => 
        let val (b, _ShareAction) = get_shareAction b  in
          (b,
           BGShareAction
           { ShareAction = _ShareAction
           })
        end
    | 21 => 
        let val (b, _Country) = get_string b  in
          (b,
           BGSetCountry
           { Country = _Country
           })
        end
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
  end
and get_searchResults b : (getBuf * searchResults) = 
  case 0 of
      0 => 
        let val (b, _Total) = get_int b  in
        let val (b, _Took) = get_int b  in
        let val (b, _TookReal) = get_int b  in
        let val (b, _MsgForest) = get_msgForest b  in
          (b,
           { Total = _Total
           , Took = _Took
           , TookReal = _TookReal
           , MsgForest = _MsgForest
           })
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>
and get_fullTextCache b : (getBuf * fullTextCache) = 
  case 0 of
      0 => 
        let val (b, _Url) = get_url b  in
        let val (b, _Text) = get_either (get_string) (get_string) b  in
        let val (b, _Time) = get_time b  in
        let val (b, _Reserved1) = get_bool b  in
        let val (b, _Reserved2) = get_bool b  in
          (b,
           { Url = _Url
           , Text = _Text
           , Time = _Time
           , Reserved1 = _Reserved1
           , Reserved2 = _Reserved2
           })
        end
        end
        end
        end
        end
    | n => error <xml>Oh, shi -- can't deserialize ({[n]} is out of range)</xml>


fun recursePuts () = ()
and put_subscriptionState b (x : subscriptionState) = 
  case x of
      SSAdded => 
        let val b = put_char b (chr 0) in
          b
        end
    | SSScanning x => 
        let val b = put_char b (chr 1) in
        let val b = put_time b x.StartTime in
          b
        end
        end
    | SSError x => 
        let val b = put_char b (chr 2) in
        let val b = put_string b x.Message in
          b
        end
        end
    | SSFeed x => 
        let val b = put_char b (chr 3) in
        let val b = put_string b x.Url in
          b
        end
        end
  
and put_subscription b (x : subscription) = 
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
  
and put_postsViewMode b (x : postsViewMode) = 
  case x of
      PVMShort => 
        let val b = put_char b (chr 0) in
          b
        end
    | PVMFull => 
        let val b = put_char b (chr 1) in
          b
        end
    | PVMMagazine => 
        let val b = put_char b (chr 2) in
          b
        end
    | PVMMosaic => 
        let val b = put_char b (chr 3) in
          b
        end
  
and put_msgTreeViewMode b (x : msgTreeViewMode) = 
  case x of
    |  x => 

        let val b = put_bool b x.Ascending in
        let val b = put_bool b x.UnreadOnly in
        let val b = put_bool b x.ExpandedComments in
        let val b = put_postsViewMode b x.Posts in
        let val b = put_bool b x.FolderExpanded in
        let val b = put_bool b x.NoOverride in
          b
        end
        end
        end
        end
        end
        end
  
and put_payment b (x : payment) = 
  case x of
      PReserved => 
        let val b = put_char b (chr 0) in
          b
        end
    | PFastSpring x => 
        let val b = put_char b (chr 1) in
        let val b = put_string b x.OrderId in
        let val b = put_string b x.OrderType in
        let val b = put_time b x.OrderTime in
          b
        end
        end
        end
        end
  
and put_paidTill b (x : paidTill) = 
  case x of
      PTUnknown => 
        let val b = put_char b (chr 0) in
          b
        end
    | PTPaid x => 
        let val b = put_char b (chr 1) in
        let val b = put_time b x.Till in
          b
        end
        end
    | PTFreeTrial x => 
        let val b = put_char b (chr 2) in
        let val b = put_time b x.Till in
          b
        end
        end
    | PTFreeTrialFinished x => 
        let val b = put_char b (chr 3) in
        let val b = put_time b x.Till in
          b
        end
        end
    | PTPaidFinished x => 
        let val b = put_char b (chr 4) in
        let val b = put_time b x.Till in
          b
        end
        end
  
and put_scrollMode b (x : scrollMode) = 
  case x of
      SMNormal => 
        let val b = put_char b (chr 0) in
          b
        end
    | SMQuick => 
        let val b = put_char b (chr 1) in
          b
        end
    | SMImmediate => 
        let val b = put_char b (chr 2) in
          b
        end
  
and put_listViewMode b (x : listViewMode) = 
  case x of
      LVMCompact => 
        let val b = put_char b (chr 0) in
          b
        end
    | LVMTwoLines => 
        let val b = put_char b (chr 1) in
          b
        end
  
and put_markReadMode b (x : markReadMode) = 
  case x of
      MRMOnScroll => 
        let val b = put_char b (chr 0) in
          b
        end
    | MRMManual => 
        let val b = put_char b (chr 1) in
          b
        end
    | MRMOnScrollEverywhere => 
        let val b = put_char b (chr 2) in
          b
        end
  
and put_publicFeedType b (x : publicFeedType) = 
  case x of
      PFTAll => 
        let val b = put_char b (chr 0) in
          b
        end
    | PFTFolder x => 
        let val b = put_char b (chr 1) in
        let val b = put_string b x.Folder in
          b
        end
        end
    | PFTTag x => 
        let val b = put_char b (chr 2) in
        let val b = put_string b x.TagName in
          b
        end
        end
    | PFTStarred => 
        let val b = put_char b (chr 3) in
          b
        end
    | PFTAllTags => 
        let val b = put_char b (chr 4) in
          b
        end
  
and put_userSettings b (x : userSettings) = 
  case x of
    |  x => 

        let val b = put_string b x.User in
        let val b = put_int b x.EditsCount in
        let val b = put_scrollMode b x.ScrollMode in
        let val b = put_listViewMode b x.ListViewMode in
        let val b = put_bool b x.ShowFavicons in
        let val b = put_markReadMode b x.MarkReadMode in
        let val b = put_bool b x.UltraCompact in
        let val b = put_option (put_string) b x.MobileLogin in
        let val b = put_bool b x.ExactUnreadCounts in
        let val b = put_option (put_list ((fn b t =>let val b = put_publicFeedType b t.1 in
        let val b = put_list ((fn b t =>let val b = put_string b t.1 in
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
        let val b = put_option (put_string) b x.Reserved6 in
        let val b = put_option (put_string) b x.Reserved7 in
        let val b = put_option (put_string) b x.Reserved8 in
        let val b = put_option (put_string) b x.Reserved9 in
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
  
and put_uID b (x : uID) = 
  case x of
      EMail x => 
        let val b = put_char b (chr 0) in
        let val b = put_string b x.Id in
          b
        end
        end
    | Url x => 
        let val b = put_char b (chr 1) in
        let val b = put_string b x.Id in
          b
        end
        end
  
and put_session b (x : session) = 
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
  
and put_attachment b (x : attachment) = 
  case x of
      AImage x => 
        let val b = put_char b (chr 0) in
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
        let val b = put_char b (chr 1) in
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
        let val b = put_char b (chr 2) in
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
        let val b = put_char b (chr 3) in
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
        let val b = put_char b (chr 4) in
        let val b = put_url b x.Url in
        let val b = put_string b x.Mime in
        let val b = put_option (put_int) b x.FileSize in
          b
        end
        end
        end
        end
    | AGrOrigin x => 
        let val b = put_char b (chr 5) in
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
  
and put_msgKey b (x : msgKey) = 
  case x of
    |  x => 

        let val b = put_string b x.BlogFeedUrl in
        let val b = put_option (put_string) b x.PostGuid in
        let val b = put_option (put_string) b x.CommentGuid in
          b
        end
        end
        end
  
and put_msg b (x : msg) = 
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
        let val b = put_string b x.Text in
        let val b = put_string b x.ShortText in
        let val b = put_string b x.Debug in
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
  
and put_msgHeader b (x : msgHeader) = 
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
  
and put_commentsKey b (x : commentsKey) = 
  case x of
    |  x => 

        let val b = put_string b x.BlogFeedUrl in
        let val b = put_string b x.PostGuid in
          b
        end
        end
  
and put_itemTag b (x : itemTag) = 
  case x of
      ITStarred => 
        let val b = put_char b (chr 0) in
          b
        end
    | ITTag x => 
        let val b = put_char b (chr 1) in
        let val b = put_string b x.TagName in
          b
        end
        end
  
and put_apiMode b (x : apiMode) = 
  case x of
      AMNormal => 
        let val b = put_char b (chr 0) in
          b
        end
    | AMGRIdsOnly x => 
        let val b = put_char b (chr 1) in
        let val b = put_bool b x.Fetch in
        let val b = put_int b x.Count in
        let val b = put_option (put_option (put_msgKey)) b x.Continuation in
        let val b = put_option (put_time) b x.MinDlTime in
        let val b = put_option (put_time) b x.MaxDlTime in
        let val b = put_option (put_time) b x.MaxTime in
        let val b = put_option (put_itemTag) b x.ExcludeTags in
        let val b = put_option (put_itemTag) b x.IncludeTags in
        let val b = put_bool b x.ReadOnly in
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
    | AMDiscovery x => 
        let val b = put_char b (chr 2) in
        let val b = put_string b x.Url in
          b
        end
        end
  
and put_msgTreePoint b (x : msgTreePoint) = 
  case x of
    |  x => 

        let val b = put_int b x.ParentId in
        let val b = put_time b x.Time in
        let val b = put_int b x.Id in
          b
        end
        end
        end
  
and put_postsReq b (x : postsReq) = 
  case x of
    |  x => 

        let val b = put_string b x.BlogFeedUrl in
        let val b = put_msgTreePoint b x.MsgTreePoint in
        let val b = put_int b x.TotalPosts in
        let val b = put_int b x.TotalComments in
          b
        end
        end
        end
        end
  
and put_commentsReq b (x : commentsReq) = 
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
  
and put_treeReq b (x : treeReq) = 
  case x of
      TRPosts x => 
        let val b = put_char b (chr 0) in
        let val b = put_list (put_postsReq) b x.Reqs in
          b
        end
        end
    | TRComments x => 
        let val b = put_char b (chr 1) in
        let val b = put_bool b x.OnExpand in
        let val b = put_commentsReq b x.Req in
          b
        end
        end
        end
    | TRSearch x => 
        let val b = put_char b (chr 2) in
        let val b = put_string b x.Query in
        let val b = put_list ((fn b t =>let val b = put_string b t.1 in
        let val b = put_int b t.2 in
        let val b = put_int b t.3 in
          b
        end
        end
        end
        )) b x.Feeds in
        let val b = put_time b x.PostTime in
        let val b = put_string b x.BlogFeedUrl in
        let val b = put_string b x.PostGuid in
          b
        end
        end
        end
        end
        end
        end
    | TRTags x => 
        let val b = put_char b (chr 3) in
        let val b = put_option (put_msgKey) b x.LastMsg in
        let val b = put_option (put_list (put_itemTag)) b x.Tags in
          b
        end
        end
        end
  
and put_msgView b (x : msgView) = 
  case x of
      MVFull x => 
        let val b = put_char b (chr 0) in
        let val b = put_msg b x.Msg in
          b
        end
        end
    | MVShort x => 
        let val b = put_char b (chr 1) in
        let val b = put_msgHeader b x.Header in
        let val b = put_option (put_msg) b x.CachedMsg in
          b
        end
        end
        end
  
and put_msgId b (x : msgId) = 
  case x of
    |  x => 

        let val b = put_msgKey b x.MsgKey in
        let val b = put_int b x.FeedId in
        let val b = put_int b x.PostId in
        let val b = put_option (put_int) b x.CommentId in
          b
        end
        end
        end
        end
  
and put_msgItem b (x : msgItem) = 
  case x of
    |  x => 

        let val b = put_msgView b x.MsgView in
        let val b = put_msgId b x.MsgId in
        let val b = put_bool b x.Read in
        let val b = put_bool b x.Starred in
        let val b = put_list (put_string) b x.Tags in
        let val b = put_bool b x.ReadLocked in
          b
        end
        end
        end
        end
        end
        end
  
and put_msgForest b (x : msgForest) = 
  case x of
    | MsgForest x => 

        let val b = put_int b x.ResultsCount in
        let val b = put_int b x.UnreadCount in
        let val b = put_list ((fn b t =>let val b = put_msgItem b t.1 in
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
  
and put_loginType b (x : loginType) = 
  case x of
      Google => 
        let val b = put_char b (chr 0) in
          b
        end
    | Facebook => 
        let val b = put_char b (chr 1) in
          b
        end
    | Twitter => 
        let val b = put_char b (chr 2) in
          b
        end
    | OpenId x => 
        let val b = put_char b (chr 3) in
        let val b = put_string b x.URL in
          b
        end
        end
  
and put_counters b (x : counters) = 
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
  
and put_subItemType b (x : subItemType) = 
  case x of
      SITAll => 
        let val b = put_char b (chr 0) in
          b
        end
    | SITSearch x => 
        let val b = put_char b (chr 1) in
        let val b = put_string b x.Query in
          b
        end
        end
    | SITFolder x => 
        let val b = put_char b (chr 2) in
        let val b = put_string b x.Folder in
          b
        end
        end
    | SITFeed x => 
        let val b = put_char b (chr 3) in
        let val b = put_subscription b x.Subscription in
        let val b = put_option (put_string) b x.FeedLink in
        let val b = put_option (put_msgTreePoint) b x.PointAllDesc in
          b
        end
        end
        end
        end
    | SITTag x => 
        let val b = put_char b (chr 4) in
        let val b = put_string b x.TagName in
          b
        end
        end
    | SITStarred => 
        let val b = put_char b (chr 5) in
          b
        end
    | SITAllTags => 
        let val b = put_char b (chr 6) in
          b
        end
  
and put_subItemRpc b (x : subItemRpc) = 
  case x of
    |  x => 

        let val b = put_string b x.Hash in
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
  
and put_shareAction b (x : shareAction) = 
  case x of
      SAEMail => 
        let val b = put_char b (chr 0) in
          b
        end
    | SATwitter => 
        let val b = put_char b (chr 1) in
          b
        end
    | SAFacebook => 
        let val b = put_char b (chr 2) in
          b
        end
    | SAGooglePlus => 
        let val b = put_char b (chr 3) in
          b
        end
    | SATumblr => 
        let val b = put_char b (chr 4) in
          b
        end
    | SAEvernote => 
        let val b = put_char b (chr 5) in
          b
        end
    | SADelicious => 
        let val b = put_char b (chr 6) in
          b
        end
    | SAPinboard => 
        let val b = put_char b (chr 7) in
          b
        end
    | SAPocket => 
        let val b = put_char b (chr 8) in
          b
        end
    | SAReadability => 
        let val b = put_char b (chr 9) in
          b
        end
    | SAInstapaper => 
        let val b = put_char b (chr 10) in
          b
        end
    | SATranslate => 
        let val b = put_char b (chr 11) in
          b
        end
  
and put_bgAction b (x : bgAction) = 
  case x of
      BGMarkMsgRead x => 
        let val b = put_char b (chr 0) in
        let val b = put_msgId b x.MsgId in
        let val b = put_bool b x.Read in
        let val b = put_int b x.TotalComments in
          b
        end
        end
        end
        end
    | BGAddTag x => 
        let val b = put_char b (chr 1) in
        let val b = put_msgId b x.MsgId in
        let val b = put_itemTag b x.Tag in
          b
        end
        end
        end
    | BGRemoveTag x => 
        let val b = put_char b (chr 2) in
        let val b = put_msgId b x.MsgId in
        let val b = put_itemTag b x.Tag in
          b
        end
        end
        end
    | BGSkipComments x => 
        let val b = put_char b (chr 3) in
        let val b = put_msgId b x.MsgId in
        let val b = put_int b x.TotalComments in
          b
        end
        end
        end
    | BGIgnorePost x => 
        let val b = put_char b (chr 4) in
        let val b = put_msgId b x.MsgId in
        let val b = put_int b x.TotalComments in
          b
        end
        end
        end
    | BGMarkBlogRead x => 
        let val b = put_char b (chr 5) in
        let val b = put_string b x.BlogFeedUrl in
        let val b = put_int b x.TotalPosts in
        let val b = put_int b x.TotalComments in
          b
        end
        end
        end
        end
    | BGSetOnlyUpdatedSubscriptions x => 
        let val b = put_char b (chr 6) in
        let val b = put_bool b x.Value in
          b
        end
        end
    | BGSetFolderViewMode x => 
        let val b = put_char b (chr 7) in
        let val b = put_string b x.Folder in
        let val b = put_msgTreeViewMode b x.ViewMode in
          b
        end
        end
        end
    | BGSetSubscriptionViewMode x => 
        let val b = put_char b (chr 8) in
        let val b = put_string b x.Url in
        let val b = put_msgTreeViewMode b x.ViewMode in
          b
        end
        end
        end
    | BGClearAllSubscriptions => 
        let val b = put_char b (chr 9) in
          b
        end
    | BGSaveFilterQuery x => 
        let val b = put_char b (chr 10) in
        let val b = put_string b x.Query in
          b
        end
        end
    | BGSetScrollMode x => 
        let val b = put_char b (chr 11) in
        let val b = put_scrollMode b x.ScrollMode in
          b
        end
        end
    | BGSetListViewMode x => 
        let val b = put_char b (chr 12) in
        let val b = put_listViewMode b x.ListViewMode in
          b
        end
        end
    | BGSetMarkReadMode x => 
        let val b = put_char b (chr 13) in
        let val b = put_markReadMode b x.MarkReadMode in
          b
        end
        end
    | BGSetUltraCompact x => 
        let val b = put_char b (chr 14) in
        let val b = put_bool b x.UltraCompact in
          b
        end
        end
    | BGDragAndDrop x => 
        let val b = put_char b (chr 15) in
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
        let val b = put_char b (chr 16) in
        let val b = put_bool b x.Value in
          b
        end
        end
    | BGSortAllFeedsAndFolders => 
        let val b = put_char b (chr 17) in
          b
        end
    | BGSortFolder x => 
        let val b = put_char b (chr 18) in
        let val b = put_string b x.Folder in
          b
        end
        end
    | BGSortTags => 
        let val b = put_char b (chr 19) in
          b
        end
    | BGShareAction x => 
        let val b = put_char b (chr 20) in
        let val b = put_shareAction b x.ShareAction in
          b
        end
        end
    | BGSetCountry x => 
        let val b = put_char b (chr 21) in
        let val b = put_string b x.Country in
          b
        end
        end
  
and put_searchResults b (x : searchResults) = 
  case x of
    |  x => 

        let val b = put_int b x.Total in
        let val b = put_int b x.Took in
        let val b = put_int b x.TookReal in
        let val b = put_msgForest b x.MsgForest in
          b
        end
        end
        end
        end
  
and put_fullTextCache b (x : fullTextCache) = 
  case x of
    |  x => 

        let val b = put_url b x.Url in
        let val b = put_either (put_string) (put_string) b x.Text in
        let val b = put_time b x.Time in
        let val b = put_bool b x.Reserved1 in
        let val b = put_bool b x.Reserved2 in
          b
        end
        end
        end
        end
        end
  


val binary_subscriptionState : binary subscriptionState = mkBinary put_subscriptionState get_subscriptionState
val binary_subscription : binary subscription = mkBinary put_subscription get_subscription
val binary_postsViewMode : binary postsViewMode = mkBinary put_postsViewMode get_postsViewMode
val binary_msgTreeViewMode : binary msgTreeViewMode = mkBinary put_msgTreeViewMode get_msgTreeViewMode
val binary_payment : binary payment = mkBinary put_payment get_payment
val binary_paidTill : binary paidTill = mkBinary put_paidTill get_paidTill
val binary_scrollMode : binary scrollMode = mkBinary put_scrollMode get_scrollMode
val binary_listViewMode : binary listViewMode = mkBinary put_listViewMode get_listViewMode
val binary_markReadMode : binary markReadMode = mkBinary put_markReadMode get_markReadMode
val binary_publicFeedType : binary publicFeedType = mkBinary put_publicFeedType get_publicFeedType
val binary_userSettings : binary userSettings = mkBinary put_userSettings get_userSettings
val binary_uID : binary uID = mkBinary put_uID get_uID
val binary_session : binary session = mkBinary put_session get_session
val binary_attachment : binary attachment = mkBinary put_attachment get_attachment
val binary_msgKey : binary msgKey = mkBinary put_msgKey get_msgKey
val binary_msg : binary msg = mkBinary put_msg get_msg
val binary_msgHeader : binary msgHeader = mkBinary put_msgHeader get_msgHeader
val binary_commentsKey : binary commentsKey = mkBinary put_commentsKey get_commentsKey
val binary_itemTag : binary itemTag = mkBinary put_itemTag get_itemTag
val binary_apiMode : binary apiMode = mkBinary put_apiMode get_apiMode
val binary_msgTreePoint : binary msgTreePoint = mkBinary put_msgTreePoint get_msgTreePoint
val binary_postsReq : binary postsReq = mkBinary put_postsReq get_postsReq
val binary_commentsReq : binary commentsReq = mkBinary put_commentsReq get_commentsReq
val binary_treeReq : binary treeReq = mkBinary put_treeReq get_treeReq
val binary_msgView : binary msgView = mkBinary put_msgView get_msgView
val binary_msgId : binary msgId = mkBinary put_msgId get_msgId
val binary_msgItem : binary msgItem = mkBinary put_msgItem get_msgItem
val binary_msgForest : binary msgForest = mkBinary put_msgForest get_msgForest
val binary_loginType : binary loginType = mkBinary put_loginType get_loginType
val binary_counters : binary counters = mkBinary put_counters get_counters
val binary_subItemType : binary subItemType = mkBinary put_subItemType get_subItemType
val binary_subItemRpc : binary subItemRpc = mkBinary put_subItemRpc get_subItemRpc
val binary_shareAction : binary shareAction = mkBinary put_shareAction get_shareAction
val binary_bgAction : binary bgAction = mkBinary put_bgAction get_bgAction
val binary_searchResults : binary searchResults = mkBinary put_searchResults get_searchResults
val binary_fullTextCache : binary fullTextCache = mkBinary put_fullTextCache get_fullTextCache

