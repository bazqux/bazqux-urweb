open Datatypes
open Utils

con subItem
  = { Path          : string
    , Index         : int
    , Title         : string
    , SIType        : subItemType
    , Counters      : source counters
    , ViewMode      : source msgTreeViewMode
    , ParentFolders : list int
    , DomIds        : list int
    , FaviconStyle  : option string
    }

fun subItemPath (sit : subItem) : string = sit.Path
fun subItemTitle (si : subItem) = si.Title
fun isFeed (si : subItem) =
    case si.SIType of
      | SITFeed _ => True
      | _ => False
fun subItemUrl (si : subItem) =
    case si.SIType of
      | SITFeed s => Some s.Subscription.Url
      | _ => None

ffi getSubItem jsFunc "getSubItem" effectful : int -> subItem
ffi getSubItems jsFunc "getSubItems" effectful : int -> list subItem
ffi getSubItemByUrl jsFunc "getSubItemByUrl" : string -> option subItem
ffi getSubItemByGRId jsFunc "getSubItemByGRId" : int -> option subItem
ffi getSubItemByTag jsFunc "getSubItemByTag" : string -> option subItem
ffi getSubItemByPath jsFunc "getSubItemByPath" effectful : string -> transaction (option subItem)
ffi getSubItemByPath' jsFunc "getSubItemByPath" effectful : string -> option subItem
fun getSmartStreamByName name : option subItem =
    getSubItemByPath' ("smartstream/" ^ Js.encodeURIComponent name)
fun getSubItemByItemTag t = case t of
  | None => getSubItemByPath' "tags"
  | Some ITStarred => getSubItemByPath' "starred"
  | Some (ITTag { TagName = n }) => getSubItemByTag n
ffi setSubItems jsFunc "setSubItems" effectful : bool -> list subItemRpc -> transaction {}
ffi hideSubItems jsFunc "hideSubItems" : list subItem -> transaction {}
ffi updateCounters' jsFunc "updateCounters" : subItem -> int -> int -> transaction {}
ffi isCommentCountsVisible effectful : subItem -> bool
ffi showUnread effectful : counters -> bool -> string

(* Если в процессе запроса updateSubscriptions обновились счетчики,
   то мы отправим новый запрос
 *)
val needRetryPendingUpdates = Unsafe.boolSource "needRetryPendingUpdates" False
val updateCountersEnabled = Unsafe.boolSource "updateCountersEnabled" True
fun feedUrl grId = Option.get "-" (bind (getSubItemByGRId grId) subItemUrl)
fun blockUpdateCounters b =
    set updateCountersEnabled (not b)

val retryPendingUpdates = set needRetryPendingUpdates True
val clearRetryPendingUpdates = set needRetryPendingUpdates False

fun updateReadCounters uc =
    when (notNull uc)
         (retryPendingUpdates;
          Js.updateReadCounters uc)
fun updateCounters si p c =
    case si.SIType of
      | SITSearch _ =>
        updateCounters' si p c
        (* у поиска отдельный счетчик, не блокируем,
           т.к. он иначе не обновится *)
      | _ =>
        e <- get updateCountersEnabled;
        when e
             (updateCounters' si p c;
              when (p <> 0 || c <> 0) retryPendingUpdates)

val defaultMsgTreeViewMode : msgTreeViewMode =
    { Ascending = False, UnreadOnly = True
    , ExpandedComments = False, Posts = PVMFull
    , Ex = MTVMEx { FolderExpanded = True, GroupByFeed = False, Reserved1 = False, Reserved2 = 0 }
    , NoOverride = True
    }
fun mtvmGroupByFeed (vm : msgTreeViewMode) =
    case vm.Ex of
      | MTVMEx f => f.GroupByFeed
      | _ => False
fun notMtvmGroupByFeed (vm : msgTreeViewMode) =
    let fun notg ex =
          case ex of
            | MTVMFolderCollapsed => MTVMEx { FolderExpanded = False, GroupByFeed = True, Reserved1 = False, Reserved2 = 0 }
            | MTVMFolderExpanded => MTVMEx { FolderExpanded = True, GroupByFeed = True, Reserved1 = False, Reserved2 = 0 }
            | MTVMEx f => MTVMEx (modifyF [#GroupByFeed] not f)
    in
        modifyF [#Ex] notg vm
    end

val emptyCounters : counters =
    { ReadPosts = 0
    , ReadComments = 0
    , TotalPosts = 0
    , TotalComments = 0
    , Scanning = 0
    , ScanningComments = 0
    , Error = 0
    , Feed = 0
    , ScannedPercent = 100
    }

val _defaultSubItemCounters = Unsafe.countersSource "SubItem._defaultSubItemCounters" emptyCounters
val _defaultSubItemViewMode = Unsafe.msgTreeViewModeSource "SubItem._defaultSubItemViewMode" defaultMsgTreeViewMode

val defaultSubItem : subItem =
    { Path          = "folder/"
    , Index         = -1
    , Title         = ""
    , SIType        = SITFolder { Folder = "" }
    , Counters      = _defaultSubItemCounters
    , ViewMode      = _defaultSubItemViewMode
    , ParentFolders = []
    , DomIds        = []
    , FaviconStyle  = None
    }

ffi _unsafeSubItemSource jsFunc "unsafeSource" : string -> subItem -> source subItem

val currentFeed = _unsafeSubItemSource "SubItem.currentFeed" defaultSubItem
val currentSearchFeed = _unsafeSubItemSource "SubItem.currentSearchFeed" defaultSubItem

val currentFeedS =
    cf <- signal currentFeed;
    case cf.SIType of
      | SITSearch _ => signal currentSearchFeed
      | _ => return cf
val getCurrentFeed = current currentFeedS
val isSearch =
    cf <- signal currentFeed;
    return <| case cf.SIType of
      | SITSearch _ => True
      | _ => False
fun isTagFeed (f : subItem) = case f.SIType of
    | SITStarred => (True, Some (ITStarred :: []))
    | SITAllTags => (True, None)
    | SITTag t => (True, Some (ITTag { TagName = t.TagName } :: []))
    | _ => (False, None)

val msgTreeViewMode =
    cf <- signal currentFeed;
    signal cf.ViewMode
val getMsgTreeViewMode = current msgTreeViewMode
val isUnreadOnly =
    m <- getMsgTreeViewMode;
    return m.UnreadOnly

fun viewModeByMsgKey (mkey:msgKey) =
    mtvm0 <- getMsgTreeViewMode;
    if mtvm0.NoOverride then
        case getSubItemByUrl mkey.BlogFeedUrl of
          | None =>
(*         debug ("si not found " ^ mkey.BlogFeedUrl); *)
            return (* defaultMsgTreeViewMode *)mtvm0
          | Some si => get si.ViewMode
    else
        return mtvm0

fun getUrls (si : subItem) : transaction Js.readCounters =
    Js.getUrls si.Index
fun getUrlsOnly si : transaction (list string) =
    Js.getUrlsOnly si.Index
fun getSubItemGRIds si : transaction (list int) =
    Js.getSubItemGRIds si.Index
