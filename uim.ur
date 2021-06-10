open Datatypes
open SubItem

datatype readabilityView
  = RVNone of option xbody
  | RVLoading
  | RVError of string
  | RVReadability of xbody

datatype addToPocketState
  = ATPSNone
  | ATPSAdding
  | ATPSAdded of Basis.id

datatype uim
  = UIM of
    { Mi : msgItem
    , Mv : source msgView
    , Tags : source (list itemTag)
    , ReadabilityView : source readabilityView
    , AddToPocketState : source addToPocketState
    , ShowText : source bool
    , Depth : int
    , GrowId : id
    , FrameId : id
    , LoadingChildren : source (option (transaction {})) (* rollOut действие *)
    , Read : source bool
    , KeepUnread : source bool
    , Selected : source bool
    , Collapsed : source collapsed
    , SubForest : uiForest
    , Parents : list uiForest
    , Parent : option uim
    , Prev : list uim
    , Next : source (option uim)
    , HasSeparator : source bool
    }
and uiForest
  = UIForest of
    { TotalCount  : int
    , UnreadCount  : source int
    , TotalResultsCount  : int
      (* ^ различаются только при поиске *)
    , UnreadResultsCount : source int
    , SmartStreamUnreadCounts : source (list (int * int))
    , SmartStreamUnreadResultCounts : source (list (int * int))
    , TagTotalCounts : source (list (option itemTag * int))
    , TagUnreadCounts : source (list (option itemTag * int))
    , TagUnreadResultCounts : source (list (option itemTag * int))
    , FirstChild : source (option uim)
    , Children : source (list uim) (* а uim содержит uiForest *)
      (* список в обратном порядке, uim.Prev для следующего uim *)
    , NextReqId  : source (option id)
    }
and forestParams
  = ForestParams of
    { Depth : int
    , Parents : list uiForest
    , ParentUIM : option uim
    , Forest : uiForest
    }
and appendReq
  = AppendReq of
    { Id : Basis.id
    , Params : forestParams
    , TreeReq : treeReq
    , InsertPoint : option (source xbody)
    }
and collapsed
  = Collapsed of list appendReq
  | Expanded

ffi getuim jsFunc "uim" effectful : Basis.id -> uim
ffi setuim jsFunc "setuim" effectful : Basis.id -> uim -> bool

ffi pagerGlobal jsFunc "unsafeGlobal" : string -> transaction Pager.t -> Pager.t
ffi labelIdMapGlobal jsFunc "unsafeGlobal" : string -> transaction (HashMap.t Basis.id) -> HashMap.t Basis.id

val _feedLabelIds = labelIdMapGlobal "Uim.feedLabelIds" (HashMap.new ())
val clearFeedLabelIds = HashMap.clear _feedLabelIds
fun getFeedLabelId bfu = HashMap.lookup _feedLabelIds bfu
fun setFeedLabelId bfu x = HashMap.insert _feedLabelIds bfu x

val isMobile = Js.isMobile ()

val msgsBoundingRect =
    rt <- Js.boundingClientRect Settings.topId;
    ih <- Js.windowInnerHeight;
    return { Left = rt.Left, Top = rt.Bottom, Right = rt.Right, Bottom = ih }

val msgsClientHeight =
    r <- msgsBoundingRect;
    return (r.Bottom - r.Top)

val msgsScrollTopOffset =
    (* Т.к. прокручивается body, то все position начинаются не от
       начала сообщений (subscriptionTitle), а от начала body,
       т.е. включают поле, соответствующее по высоте #topId.
       В desktop msgsPadder-у требуется position:relative, по-этому
       у него offsetTop идет от subscriptionTitle
       (а без position:relative также от самого верха)
     *)
    if not isMobile then return 0.0 else
    rt <- Js.boundingClientRect Settings.topId;
    return rt.Bottom
val msgsScrollTop =
    st <- Js.scrollTop Settings.msgDivId;
    o <- msgsScrollTopOffset;
    return (st + o)

val pager = pagerGlobal "Uim.pager" (Pager.new Settings.msgDivId msgsBoundingRect)

fun uimAuthorAndShortText (UIM uim) = case uim.Mi.MsgView of
      | MVFull f => (f.Msg.Author, f.Msg.ShortText)
      | MVShort s => (s.Header.Author, s.Header.ShortText)
fun uimSubjectAndShortText (UIM uim) = case uim.Mi.MsgView of
      | MVFull f => (f.Msg.Subject, f.Msg.ShortText)
      | MVShort s => (s.Header.Subject, s.Header.ShortText)

fun msgHeader (m : msg) : msgHeader =
    { Guid = ""
    , ContentHash = ""
    , Author = m.Author
    , AuthorPic = m.AuthorPic
    , Subject = m.Subject
    , Time = m.Time
    , DlTime = m.DlTime
    , ShortText = m.ShortText
    }
fun mhTime (mh : msgHeader) =
    case mh.Time of
        None => mh.DlTime
      | Some t => t

fun hasFirstChild (UIForest f) =
    fc <- get f.FirstChild;
    return (Option.isSome fc)
fun hasChildren (UIForest f) =
    fc <- get f.FirstChild;
    nri <- get f.NextReqId;
    return (Option.isSome fc || Option.isSome nri)
fun uifUnreadResultsCount (UIForest f) = f.UnreadResultsCount
fun uifTotalCount (UIForest f) = f.TotalCount
fun uifUnreadCount (UIForest f) = f.UnreadCount
fun uifSmartStreamUnreadCounts (UIForest f) = f.SmartStreamUnreadCounts
fun uifTagTotalCounts (UIForest f) = f.TagTotalCounts
fun uifTagUnreadCounts (UIForest f) = f.TagUnreadCounts
fun uifChildren (UIForest f) = f.Children
fun uimMsgHeader (UIM uim) : msgHeader =
    case uim.Mi.MsgView of
        MVFull { Msg = m } => msgHeader m
      | MVShort { Header = mh, ... } => mh
val uimTime = mhTime <<< uimMsgHeader
fun uimRead (UIM uim) = uim.Read
fun uimMi (UIM uim) = uim.Mi

fun forestParamsParentUIM (ForestParams p) = p.ParentUIM

val eq_itemTag : eq itemTag =
    mkEq (fn a b => case (a,b) of
      | (ITStarred, ITStarred) => True
      | (ITTag { TagName = t1 }, ITTag { TagName = t2 }) => t1 = t2
      | _ => False)
val eq_ord : ord itemTag =
    let fun o t = case t of ITStarred => None | ITTag { TagName = n } => Some n
    in
      mkOrd
        { Lt = fn a b => o a < o b
        , Le = fn a b => o a <= o b
        }
    end

fun uimTags (UIM uim) = uim.Tags
fun uimStarred u =
    Monad.mp (elem ITStarred) (signal (uimTags u))
fun uimITTags u =
    Monad.mp
      (mapMaybe (fn t => case t of
        | ITTag { TagName = n } => Some n
        | ITStarred => None))
      (signal (uimTags u))
fun itTag n = ITTag { TagName = n }
fun isITTag t = case t of ITTag _ => True | _ => False

fun uimMsgId (UIM uim) = uim.Mi.MsgId
fun uimMsgKey (UIM uim) = uim.Mi.MsgKey
fun uimLongMsgId (UIM uim) : longMsgId =
    { MsgKey = uim.Mi.MsgKey, MsgId = uim.Mi.MsgId }
fun showMsgKey (k : msgKey) = k.BlogFeedUrl ^ "/" ^ show k.PostGuid ^ "/" ^ show k.CommentGuid
fun uimCachedMsg (UIM uim) =
    case uim.Mi.MsgView of
        MVFull m => Some m.Msg
      | MVShort { CachedMsg = cm, ... } => cm
      | _ => None
fun uimMsgLink (UIM uim) =
    case uim.Mi.MsgView of
        MVFull m => m.Msg.Link
      | MVShort { CachedMsg = Some m, ... } => m.Link
      | _ => None
fun uimMsgText (UIM uim) =
    case uim.Mi.MsgView of
        MVFull m => Unsafe.fromXbodyString m.Msg.Text
      | MVShort { CachedMsg = Some m, ... } => Unsafe.fromXbodyString m.Text
      | MVShort { Header = h, ... } => h.ShortText
fun uimAttachments (UIM uim) =
    case uim.Mi.MsgView of
        MVFull m => m.Msg.Attachments
      | MVShort { CachedMsg = Some m, ... } => m.Attachments
      | _ => []
fun uimReadabilityMode (UIM uim) =
    rv <- get uim.ReadabilityView;
    return (case rv of RVReadability _ => True | _ => False)

fun uimSubItem u = getSubItemByUrl (uimMsgKey u).BlogFeedUrl
fun uimMv (UIM uim) = uim.Mv
fun uimFrameId (UIM uim) = uim.FrameId
fun uimSelected (UIM uim) = uim.Selected
fun uimSubForest (UIM uim) = uim.SubForest
fun uimCollapsed (UIM uim) = uim.Collapsed
fun uimDepth (UIM uim) = uim.Depth
fun uimPrev (UIM uim) = uim.Prev
fun uimNext (UIM uim) = uim.Next
fun uimViewMode u = viewModeByMsgKey (uimMsgKey u)
fun isCompact (UIM uim) =
    vm <- uimViewMode (UIM uim);
    lvm <- get Settings.listViewMode;
    return (case (vm.Posts, lvm, uim.Depth) of
             | (PVMShort, LVMCompact, 0) => True
             | _ => False)
fun uimFeedLabelHeight uim =
    li <- getFeedLabelId (uimMsgKey uim).BlogFeedUrl;
    maybe (return 0.0)
          (fn x =>
              h <- Pager.idClientRectHeight pager x;
              return (h - 2.0))
          li
fun uimBorderTopWidth (UIM uim) =
    vm <- uimViewMode (UIM uim);
    hasSep <- get uim.HasSeparator;
    mv <- get uim.Mv;
    return (if hasSep then 0.0 else
            case (mv, vm.Posts) of
              | (MVFull _, _) => 1.0
              | (_, PVMMosaic) => 0.0
              | (_, PVMMagazine) => 0.0
              | _ => 1.0)
fun uimTop (UIM uim) =
    border <- uimBorderTopWidth (UIM uim);
    li <- getFeedLabelId (uimMsgKey (UIM uim)).BlogFeedUrl;
    ft <- Pager.idPositionTop pager uim.FrameId;
    case li of
    | None =>
      return (Js.roundScrollTop (ft + border))
    | Some labelId =>
      h <- Pager.idClientRectHeight pager labelId;
      return (Js.roundScrollTop (ft + border - h))
(* при сворачивании сообщение может оказаться выше экрана, в таких случаях
   прокручиваем к нему. Иначе ничего не делаем, т.к. сообщение и так видно
 *)
fun snapToIfAbove' force offs uim =
    ot <- Monad.mp (plus offs) (uimTop uim);
    st <- msgsScrollTop;
    when (ot < st || force)
         (o <- msgsScrollTopOffset;
          Js.setScrollTop Settings.msgDivId (ot-o))
val snapToIfAbove = snapToIfAbove' False 0.0
val snapTo = snapToIfAbove' True
fun uimOnValidPage (UIM uim) =
    Pager.isIdOnValidPage pager uim.FrameId

fun authorPicXml author authorPic =
    <xml>
      <div class={Css.authorPic}>
        <active code={Js.authorPicImg authorPic author} />
      </div>
    </xml>

fun uimAuthorPic (UIM uim) =
    case uim.Mi.MsgView of
      | MVFull f => f.Msg.AuthorPic
      | MVShort s => s.Header.AuthorPic

fun uimAuthorPicXml uim =
    authorPicXml (uimAuthorAndShortText uim).1 (uimAuthorPic uim)

fun uimSubjectOrShortText uim =
    let val (subject, shortText) = uimSubjectAndShortText uim
    in
        if subject = "" then shortText else subject
    end

fun addMsgTag t (UIM uim) =
    Js.forceImpure (BackgroundRpc.addAction
                        (BGAddTag { LongMsgId = uimLongMsgId (UIM uim), Tag = t }))
fun removeMsgTag t (UIM uim) =
    Js.forceImpure (BackgroundRpc.addAction
                        (BGRemoveTag { LongMsgId = uimLongMsgId (UIM uim), Tag = t }))

(* навигация *)

fun isSearchResult (mi:msgItem) = mi.SearchResult
fun isResult (mi:msgItem) = mi.Full

fun smartStreamUnreadCounters uc mi =
    List.mp (fn ssi => (ssi, uc)) mi.SmartStreams
fun itemTags u =
    ts <- get (uimTags u);
    return <| (if any isITTag ts then cons None else id)
      <| List.mp some (List.sort gt ts)
fun tagUnreadCounters uc u =
    Monad.mp (List.mp (fn t => (t, uc))) (itemTags u)

fun negateUnreadCounts [a] (cs : list (a * int)) : list (a * int) =
    List.mp (fn (s,c) => (s,-c)) cs

(* обновление SmartStreamUnreadCounters, TagUnreadCounters *)
fun modifyUnreadCounts [a] (_ : eq a) (_ : ord a)
      (src : source (list (a * int))) (unreadCounts : list (a * int)) =
    modify src <| fn uc0 =>
      List.filter (fn (_,c) => c > 0)
        (* ^ на случай, если внезапно появится новый элемент
           smart stream-а, которого в начальном запросе не было
           (например, из-за time:[...]),
           чтобы не было отрицательных (а заодно и нулевых)
           счетчиков.
           Также для тегов это важно
         *)
        <| mergeWith plus uc0 (negateUnreadCounts unreadCounts)

fun fixResultsCount' (UIForest f) =
    let fun go rc ssrc trc uims = case uims of
              | [] =>
                rc0 <- get f.UnreadResultsCount;
                set f.UnreadResultsCount rc;
                ssrc0 <- get f.SmartStreamUnreadResultCounts;
                set f.SmartStreamUnreadResultCounts ssrc;
                trc0 <- get f.TagUnreadResultCounts;
                set f.TagUnreadResultCounts trc;
                modify f.UnreadCount (plus (rc - rc0));
                modifyUnreadCounts f.SmartStreamUnreadCounts
                    (mergeWith plus ssrc0 (negateUnreadCounts ssrc));
                modifyUnreadCounts f.TagUnreadCounts
                    (mergeWith plus trc0 (negateUnreadCounts trc));
                return (rc, ssrc, trc)
              | (UIM uim) :: uims' =>
                (frc, fssrc, ftrc) <- fixResultsCount' uim.SubForest;
                read <- get uim.Read;
                let val ur = isResult uim.Mi && not read
                in
                    tc <- (if ur then
                               tagUnreadCounters 1 (UIM uim)
                           else return []);
                    go (rc + frc + if ur then 1 else 0)
                       (mergeWith plus fssrc <| mergeWith plus ssrc
                         <| if ur then []
                            else smartStreamUnreadCounters 1 uim.Mi)
                       (mergeWith plus ftrc <| mergeWith plus trc tc)
                       uims'
                end
    in
        ch <- get f.Children;
        go 0 [] [] ch
    end
fun fixResultsCount f = void (fixResultsCount' f)

fun parentMsg (UIM uim) : transaction (option uim) =
    return uim.Parent
fun postMsg (UIM uim) =
    case uim.Parent of
      | Some p => postMsg p
      | None => UIM uim
fun isPost u = Option.isNone (uimMsgId u).CommentId
fun checkUIM checkNotCollapsed resultsOnly (UIM m) f =
    if resultsOnly then
        (if isResult m.Mi || m.Depth = 0 then
             (* посты всегда можно выделять *)
             return (Some (UIM m))
         else
             f checkNotCollapsed resultsOnly (UIM m))
    else
        return (Some (UIM m))

fun whenNotCollapsed' checkNotCollapsed (UIM uim) act =
    if checkNotCollapsed then
        c <- get uim.Collapsed;
        case c of Collapsed _ => return None | Expanded => act uim.SubForest
    else
        act uim.SubForest
fun findFirst' cc ro (UIForest f) =
    m <- get f.FirstChild;
    case m of
      | None => return None
      | Some uim =>
        checkUIM cc ro uim nextMsg'
and nextMsg' cc ro uim =
    f <- whenNotCollapsed' cc uim (findFirst' cc ro);
    case f of Some m => return f | None => nextSibling' cc ro uim
and nextSibling' cc ro (UIM uim) =
    n <- get uim.Next;
    case n of
      | Some next =>
        checkUIM cc ro next nextMsg'
      | None =>
        (case uim.Parent of
           | Some (UIM p) =>
             (case p.SubForest of UIForest uif =>
              nri <- get uif.NextReqId;
              (case nri of
                 | None   => nextSibling' cc ro (UIM p)
                 | Some _ => return None))
              (* сначала должен выполниться запрос, и только потом,
               повторно вызываться next.
               *)
           | None => return None (* закончились *))
fun findLast' cc ro (UIForest f) =
    ch <- get f.Children;
    case ch of
      | [] => return None
      | last :: _ =>
        m <- whenNotCollapsed' cc last (findLast' cc ro);
        (case m of
           | Some _ => return m
           | None =>
             checkUIM cc ro last prevMsg')
and prevMsg' cc ro (UIM uim) =
    case uim.Prev of
      | prev :: _ =>
        m <- whenNotCollapsed' cc prev (findLast' cc ro);
        (case m of
           | Some _ => return m
           | None   => checkUIM cc ro prev prevMsg')
      | [] =>
        (case uim.Parent of
           | Some p => checkUIM cc ro p prevMsg'
           | None   => return None (* закончились *))

fun whenNotCollapsed u a = whenNotCollapsed' True u a
val findFirst = findFirst' True
val findLast = findLast' True
val nextMsg = nextMsg' True
val prevMsg = prevMsg' True
