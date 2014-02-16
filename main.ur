open UrCalls
open Datatypes
open Css
open Utils

val apiUrl = "https://github.com/bazqux/bazqux-api"

fun hrefLink t u =
    <xml><a href={bless u} target="_blank">{t}</a></xml>
fun hrefLinkStopPropagation (t : xbody) (u : string) =
    Js.stopPropagationLink null "" (bless u) t
fun apiLink t = hrefLink t apiUrl

cookie sid : string
cookie sid_beta : string
cookie freshSignIn : {}
cookie referrer : string

fun logAction' action details uid msec =
    p <- prettyUID uid;
    let val m = show msec
        val l = strlen m
        val (s,ms) =
            if l > 3 then (substring m 0 (l-3), substring m (l-3) 3)
            else if l = 3 then ("0", m)
            else if l = 2 then ("0", "0" ^ m)
            else ("0", "00" ^ m)
    in
        debug ("-- " ^ action ^ " " ^ p ^ " " ^ details ^ " " ^ s ^ "." ^ ms)
    end
fun logAction [a] action uid (act : transaction a) : transaction a =
    t1 <- getUrTime_ ();
    (* оказывается, now в серверной части возвращает время только с точностью
     до секунды
     *)
    r <- act;
    t2 <- getUrTime_ ();
    logAction' action "" uid (diffInMilliseconds t1 t2);
    return r

val getHost =
    h <- getHeader (blessRequestHeader "Host");
    return (case h of
      | None => "bazqux.com"
      | Some "www.bazqux.com" => "bazqux.com"
      | Some "www.beta.bazqux.com" => "beta.bazqux.com"
      | Some h => h)
val isBeta =
    h <- getHost;
    return (h = "beta.bazqux.com")

val sessionCookie =
    b <- isBeta;
    return (if b then sid_beta else sid)

fun getUserBySession clear k =
    s <- cachedReadSession k;
    (case s of
       | Some s =>
         e <- isUserExists s.User;
         if s.Cleared || not e then
             when clear (clearCookie sid); deleteSession s; return None
             (* вынести как ф-ю не получается -- не компилится *)
         else
             return (Some s.User)
       | _ =>
         when clear (clearCookie sid); return None)

fun getUser action =
(*     return (Some "1") *)
    sid <- sessionCookie;
    c <- getCookie sid;
    case c of
      | Some k =>
        u <- getUserBySession True k;
        (case u of
           | Some u =>
             when (action <> "subscriptions")
                  (* обновление подписок использованием не считаем *)
                  (ua <- getHeader (blessRequestHeader "User-Agent");
                   recordWebUsage u ua);
             return (Some u)
           | None => return None)
      | _ => return None

val whoami =
    u <- getUser "";
    case u of
      | Some uid =>
        infoPage "Who am I" <xml>
          <p>Your user ID is:
          <div class="errorText">{[uid]}</div></p>
        </xml>
      | _ => error <xml>Not logged in</xml>

fun withUser [r] action (f : string -> transaction r) (l : list bgAction)
    : transaction r =
    u <- getUser action;
    case u of
      | Some u =>
        t1 <- getUrTime_ ();
        d <- performBgActions u l;
        r <- f u;
        t2 <- getUrTime_ ();
        logAction' (action ^ if notNull l then "/bg" else "") d u (diffInMilliseconds t1 t2);
        return r
      | None => error <xml>Not logged in</xml>
val bgactions = withUser "" (fn u => return ())

fun mobileLogin l p k = withUser "mobileLogin"
                        (fn userId => setMobileLogin userId l p k)

fun opml () = withUser "OPML" (fn userId =>
    o <- userOPML True userId;
    setHeader (blessResponseHeader "Content-Disposition")
              "attachment; filename=bazqux-reader-subscriptions.xml";
    returnBlob (textBlob o) (blessMime "text/x-opml")) []

fun isResultMv mv =
    case mv of
      | MVFull _ => True
      | MVShort { CachedMsg = Some _, ... } => True
      | _ => False
fun isResult mi = isResultMv mi.MsgView

datatype readabilityView
  = RVNone of option xbody
  | RVLoading
  | RVError of string
  | RVReadability of xbody

datatype uim
  = UIM of
    { Mi : msgItem
    , Mv : source msgView
    , Starred : source bool
    , Tags : source (list string)
    , ReadabilityView : source readabilityView
    , ShowText : source bool
    , Depth : int
    , SnapId : id
    , GrowId : id
    , FrameId : id
    , TagId : id
    , LoadingChildren : source bool
    , Read : source bool
    , KeepUnread : source bool
    , Selected : source bool
    , Collapsed : source collapsed
    , SubForest : uiForest
    , Parents : list uiForest
    , Parent : option uim
    , Prev : list uim
    , Next : source (option uim)
    }
and uiForest
  = UIForest of
    { ResultsCount : source int
    , UnreadCount  : source int
      (* ^ различаются только при поиске *)
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
    , InsertPoint : source xbody
    }
and collapsed
  = Collapsed of list appendReq
  | Expanded

con mw =
    {FullUIM : source (option uim),
     ListViewMode : source Datatypes.listViewMode, MsgDivId : id,
     LastScrollPos : source int, GetScrollMode : transaction string,
     ScrollAfter : source (option (int * bool)),
     ToggleRead : uim -> transaction {},
     BackgroundRpc : backgroundRpc bgAction,
     GetMsgTreeViewMode : transaction msgTreeViewMode,
     ToggleStarred : uim -> transaction {},
     AddTag : uim -> transaction {},
     RemoveITTag : string -> uim -> transaction {},
     MarkRead : uim -> transaction {},
     TryMarkRead : uim -> transaction {},
     Po : popups,
     SharePopup : source xbody
    }

fun uimAuthorAndShortText (UIM uim) = case uim.Mi.MsgView of
      | MVFull f => (f.Msg.Author, f.Msg.ShortText)
      | MVShort s => (s.Header.Author, s.Header.ShortText)
fun uimSubjectAndShortText (UIM uim) = case uim.Mi.MsgView of
      | MVFull f => (f.Msg.Subject, f.Msg.ShortText)
      | MVShort s => (s.Header.Subject, s.Header.ShortText)

val show_id : show id = mkShow Js.showId

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

fun readability key = withUser "readability" (fn u => userGetFullText u key)
fun msg key = withUser "msg" (fn _ => readMsg key)

val defaultMsgTreeViewMode : msgTreeViewMode =
    { Ascending = False, UnreadOnly = True
    , ExpandedComments = False, Posts = PVMFull
    , FolderExpanded = True, NoOverride = True
    }

con subItem
  = { Hash          : string
    , Index         : int
    , Title         : string
    , SIType        : subItemType
    , Counters      : source counters
    , ViewMode      : source msgTreeViewMode
    , ParentFolders : list int
    , DomIds        : list int
    , FaviconStyle  : option string
    }

fun getMW () : mw = Js.mw ()
fun getSubItem idx : subItem = Js.getSubItem idx
fun getSubItems idx : list subItem = Js.getSubItems idx
fun getSubItemByUrl url : option subItem = Js.getSubItemByUrl url
fun getSubItemByHash url : transaction (option subItem) =
    a <- source (Js.getSubItemByHash url);
    (* без этого urweb делает reorder и вызвает ф-ю, когда нужен результат,
       а не тогда, когда ее вызвали *)
    get a
fun hideSubItems (sis : list subItem) : transaction {} =
    when (not (Js.hideSubItems sis)) (alert "hideSubItems")
fun updateCounters (si : subItem) p c =
    x <- Js.updateCounters_ (si, p, c);
    when x (debug "just forced to eval")

fun hasChildren (UIForest f) =
    fc <- get f.FirstChild;
    nri <- get f.NextReqId;
    return (Option.isSome fc || Option.isSome nri)
fun uimMsgKey (UIM uim) = uim.Mi.MsgId.MsgKey
fun uimMsgLink (UIM uim) =
    case uim.Mi.MsgView of
        MVFull m => m.Msg.Link
      | MVShort { CachedMsg = Some m, ... } => m.Link
      | _ => None
fun uimMsgText (UIM uim) =
    case uim.Mi.MsgView of
        MVFull m => m.Msg.Text
      | MVShort { CachedMsg = Some m, ... } => m.Text
      | MVShort { Header = h, ... } => h.ShortText
fun uimAttachments (UIM uim) =
    case uim.Mi.MsgView of
        MVFull m => m.Msg.Attachments
      | MVShort { CachedMsg = Some m, ... } => m.Attachments
      | _ => []
fun uimReadabilityMode (UIM uim) =
    rv <- get uim.ReadabilityView;
    return (case rv of RVReadability _ => True | _ => False)

fun viewModeByMsgKey getMsgTreeViewMode mkey =
    mtvm0 <- getMsgTreeViewMode;
    if mtvm0.NoOverride then
        case Js.getSubItemByUrl mkey.BlogFeedUrl of
          | None =>
(*         debug ("si not found " ^ mkey.BlogFeedUrl); *)
            return (* defaultMsgTreeViewMode *)mtvm0
          | Some (si : subItem) => get si.ViewMode
    else
        return mtvm0
fun uimViewMode gmv u = viewModeByMsgKey gmv (uimMsgKey u)
fun snapToTop uim =
    ot <- Js.offsetTop uim.SnapId;
    st <- Js.offsetParentScrollTop uim.SnapId;
    when (ot < st) (Js.setOffsetParentScrollTop uim.SnapId ot)

fun isCompact (UIM uim) gmv listViewMode =
    vm <- uimViewMode gmv (UIM uim);
    lvm <- get listViewMode;
    return (case (vm.Posts, lvm, uim.Depth) of
             | (PVMShort, LVMCompact, 0) => True
             | _ => False)

fun fitCompact lastScrollPos msgDivId (UIM uim) =
    let fun setScrollPos ot =
            Js.setOffsetParentScrollTop uim.SnapId ot;
            set lastScrollPos ot
    in
        ot <- Js.offsetTop uim.SnapId;
        st <- Js.offsetParentScrollTop uim.SnapId;
        if ot < st then setScrollPos ot
        else
            chf <- Js.clientHeight uim.FrameId;
            h <- Js.clientHeight msgDivId;
            when (ot+chf > st+h)
                 (setScrollPos (if chf >= h then ot else ot+chf-h))
    end

fun toggleFull toggleCollapsed select (mw : mw) (UIM uim) =
    let fun collapse andSelect (UIM uim) m =
            let val afterScroll =
                set uim.Mv (MVShort { Header = msgHeader m
                                    , CachedMsg = Some m });
                f <- get mw.FullUIM;
                (case f of
                   | Some (UIM uimf) => when (Js.eq_id uimf.SnapId uim.SnapId)
                                             (set mw.FullUIM None)
                   | _ => return ());
                when andSelect
                     (snapToTop uim;
                      select)
            val afterCollapse =
                Js.collapseMessage 60 uim.SnapId afterScroll
(*                 ot <- Js.offsetTop uim.SnapId; *)
(*                 st <- Js.offsetParentScrollTop uim.SnapId; *)
(*                 if ot < st then Js.scrollToElement uim.SnapId afterScroll *)
(*                 else afterScroll *)
            in
                c <- get uim.Collapsed;
                vm <- viewModeByMsgKey mw.GetMsgTreeViewMode m.Key;
                case (c, uim.Depth, vm.Posts) of
                  | (_,_, PVMFull) => afterCollapse
                  | (Expanded, 0, _ (* PVMMosaic *)) =>
                    toggleCollapsed afterCollapse (UIM uim)
                    (* во всех коротких видах сворачивание поста
                       сворачивает комменты *)
                  | _ => afterCollapse
            end

        val afterScroll =
            select;
            r <- get uim.Read;
            k <- get uim.KeepUnread;
            when (not r && not k) (mw.ToggleRead (UIM uim));
            set mw.ScrollAfter None
        fun setFull uim m =
            compact <- isCompact (UIM uim) mw.GetMsgTreeViewMode mw.ListViewMode;
            set mw.ScrollAfter (if compact then None else Some (0, False));
            ch0 <- Js.clientHeight uim.FrameId;
            sm <- mw.GetScrollMode;
            set uim.Mv (MVFull { Msg = m });
            Js.expandMessage ch0 uim.SnapId (
            if isResult uim.Mi then
(*                 ot <- Js.offsetTop uim.SnapId; *)
(*                 Js.setOffsetParentScrollTop uim.SnapId ot; *)
                (if compact then
                    f <- get mw.FullUIM;
                    (case f of
                       | Some (UIM uimf) =>
                         mvf <- get uimf.Mv;
                         (case mvf of
                            | MVFull { Msg = m } =>
                              collapse False (UIM uimf) m
                            | _ => return ())
                       | _ => return ());
(*                     Js.scrollToElement uim.SnapId "immediate" afterScroll; *)
                    (* гуглоридер скролит только если сообщение не влезает
                       снизу (чтобы нижняя граница совпадала)
                       или сверху (чтобы верхняя граница совпадала)
                     *)
                    set mw.FullUIM (Some (UIM uim));
                    fitCompact mw.LastScrollPos mw.MsgDivId (UIM uim);
                    afterScroll
                 else
                    Js.scrollToElement uim.SnapId sm afterScroll
                )
            else
                afterScroll)
    in
    mv <- get uim.Mv;
    case mv of
      | MVFull { Msg = m } =>
        collapse True (UIM uim) m
      | MVShort s =>
        (
         case s.CachedMsg of
           | Some m => setFull uim m
           | None =>
             queueCRpcB mw.BackgroundRpc
                        (fn l => rpc (msg (uimMsgKey (UIM uim)) l))
                       (fn mo =>
                           case mo of
                                | Some m => setFull uim m
                                | None   => return ())
        )
    end

fun mvAuthorPic mv =
    case mv of
      | MVFull f => f.Msg.AuthorPic
      | MVShort s => s.Header.AuthorPic
fun noSubject subject (UIM uim) =
    subject = ""
    || (uim.Depth > 0 &&
        Js.commentSubjectNotNeeded
            (mvAuthorPic uim.Mi.MsgView)
            (uimMsgKey (UIM uim)).BlogFeedUrl)
(*     || Js.subjectDuplicatesMessage subject mh.ShortText *)
(*     || Js.authorIsFoundInSubject subject author *)

fun mailLink (UIM uim) link =
    let val (subject, shortText) = uimSubjectAndShortText (UIM uim)
        val s = if noSubject subject (UIM uim)
                then shortText else subject
                (* ^ не хорошо, что этот код дублирует код из вызова shareMenu *)
    in
        Js.openLink
            (bless ("mailto:?subject=" ^
             Js.encodeURIComponent s ^
             "&body=" ^ Js.encodeURIComponent (show link)))
    end

fun getCachedFullText link default =
    ft <- readFullTextCache link;
    return (case ft of
              | Some ft =>
                (case (ft.Text : either string string) of
                   | Right t => t
                   | _ => default)
              | _ => default)

fun msgContents key readability qs =
    m <- readMsg key;
    case m of
       | None => error <xml>Article not found</xml>
       | Some m =>
         text <- (case (readability, m.Link) of
                    | (True, Some l) =>
                      getCachedFullText l m.Text
                    | _ =>
                      return m.Text);
         t <- textToXbody text;
         pageNoBody' "https://bazqux.com" <xml/> m.Subject <xml>
           <body class={Css.msgContents}><div class="post">
             {case m.AuthorPic of
                | None => <xml/>
                | Some u => <xml><div class={Css.authorPic}>
                  <div class="authorPicCenter">
                    <img class="authorPic" src={u}></img>
                  </div>
                </div></xml>}
             <div class="msgBody">
               <div class="msubject">{textWithLink m.Link (txt m.Subject)}</div>
               {case m.Tags of
                  | [] => <xml/>
                  | t => <xml><div class="mtags">{["tags: " ^ intercalate ", " m.Tags]}</div></xml>}
               {if m.Author <> "" then <xml><div class="mauthor">{textWithLink m.AuthorUri (txt m.Author)}</div></xml>
                else <xml/>}
               <div class="mtextPad"></div>
               <div class="mtext">{t}</div>
             </div></div>
           </body></xml>

fun translateLink readability (key : msgKey) =
    "http://translate.google.com/translate?u=" ^ Js.encodeURIComponent ("http://bazqux.com" ^ show (effectfulUrl (msgContents key readability)))

fun trackShareAction backgroundRpc a =
    backgroundRpc.AddAction (BGShareAction { ShareAction = a })

fun shareMenu tp backgroundRpc msgDivId shareId subject link readability key =
    let fun b c a n u : xbody = tp.LLiI (trackShareAction backgroundRpc a) c n u
        val s = Js.encodeURIComponent subject
        val l = Js.encodeURIComponent (show link)
    in
        tp.NewIdPosMenu shareId (Js.offsetBottomRight shareId) Css.shareMenu <xml>
          {b Css.btnEMail SAEMail "E-mail" ("mailto:?subject=" ^ s ^ "&body=" ^ l)}
          <hr/>
          {b Css.btnTwitter SATwitter "Twitter" ("https://twitter.com/share?url=" ^ l ^ "&text=" ^ s
                            (* ^ "&via=BazQuxReader" *))}
          {b Css.btnFacebook SAFacebook "Facebook" ("https://www.facebook.com/sharer.php?u=" ^ l)}
          {b Css.btnGooglePlus SAGooglePlus "Google Plus" ("https://plus.google.com/share?url=" ^ l)}
          {b Css.btnTumblr SATumblr "Tumblr" ("https://www.tumblr.com/share?v=3&u=" ^ l ^ "&t=" ^ s)}
(*           https://bufferapp.com/add?url=http%3A%2F%2Flambda-the-ultimate.org%2Fnode%2F4846&text=Call%20for%20Participation:%20Programming%20Languages%20Mentoring%20Workshop *)
          <hr/>
          {b Css.btnPocket SAPocket "Pocket" ("https://getpocket.com/save?url=" ^ l ^ "&title=" ^ s)}
          {b Css.btnInstapaper SAInstapaper "Instapaper" ("https://www.instapaper.com/add?url=" ^ l ^ "&title=" ^ s)}
          {b Css.btnReadability SAReadability "Readability" ("https://www.readability.com/save?url=" ^ l ^ "&title=" ^ s)}
(*           <hr/> *)
          {b Css.btnPinboard SAPinboard "Pinboard" ("https://pinboard.in/add?url=" ^ l ^ "&title=" ^ s)}
          {b Css.btnEvernote SAEvernote "Evernote" ("https://www.evernote.com/clip.action?url=" ^ l ^ "&title=" ^ s)}
          {b Css.btnDelicious SADelicious "Delicious" ("https://delicious.com/save?v=5&noui&jump=close&url=" ^ l ^ "&title=" ^ s)}
          <hr/>
          {b Css.btnTranslate SATranslate "Translate" (translateLink readability key)}
        </xml>
    end

fun msgNodeNew ultraCompact imgLoadCheck children showFeedTitles authorStyle toggleCollapsed select (mw : mw) (UIM uim)
    : transaction xbody =
    noSelect <- source False;
    hasComments <- hasChildren uim.SubForest;
    let val subForest = case uim.SubForest of UIForest f => f
        val (mh : msgHeader) = case uim.Mi.MsgView of
            MVFull { Msg = m } => msgHeader m
          | MVShort { Header = mh, ... } => mh
        val mkey = uimMsgKey (UIM uim)
        val subject = (* Js.preprocessSubjectText *) mh.Subject
(*         val subject = if mh.Subject = "" && depth = 0 then *)
(*                           <xml>(no subject)</xml> else subject' *)
        val author = Js.preprocessAuthorText mh.Author
        val authorComma = if mh.Author = "" then "" else ","
        val time =
            case mh.Time of
                None => mh.DlTime
              | Some t => t
        fun markReadNoSelectLink cls title link t =
            case link of
              | None => <xml><div class={cls} title={title}>{[t]}</div></xml>
              | Some l =>
                Js.msgOnClick
                    "" (fn _ _ => set noSelect True; mw.MarkRead (UIM uim))
                    (* если оставить stopPropagation,
                     то ctrl+click не работает в FF, по-этому noSelect *)
                    <xml><a class={cls} title={title} href={l} target={"_blank"}>{[t]}</a></xml>
        fun timeLink link =
            markReadNoSelectLink mtime (Js.showTime time) link (Js.showAgo time)
        fun rmTag t _ = stopPropagation; mw.RemoveITTag t (UIM uim)
        fun tags header = dyn_
          (ts <- signal uim.Tags;
           return (if ts = [] then <xml/> else <xml><div class="mtagsList">{
           List.mapX (fn t => <xml>
             <div class="mtag">
             <div class="mtagName"
                  onclick={fn _ => when (not header) (Js.selTag t)}>{[t]}</div>{
             if header then <xml/> else
              <xml><div class="mtagRm" onclick={rmTag t}>x</div></xml>
             }</div>
           </xml>)
           ts}</div></xml>))
        val starButton =
            symbolButton "Star"(* Css.buttonLeft *) Css.btnStar
                     (* "" *)(* "Keep unread" *)
                     "Star/unstar article. \nKeyboard shortcut: 's'"
                     (stopPropagation; mw.ToggleStarred (UIM uim))
        val tagButton = <xml><span id={uim.TagId}>{
            symbolButton "Tag" (* buttonT' Css.buttonLeft *) Css.btnTag
                     (* "" *)(* "Keep unread" *)
                     "Edit tags. \nKeyboard shortcut: 't'"
                     (stopPropagation; mw.AddTag (UIM uim))
            }</span></xml>
    in
    mtvm <- viewModeByMsgKey mw.GetMsgTreeViewMode mkey;
    lvm <- get mw.ListViewMode;
    let fun grOrigin acc as =
            case as of
              | (AGrOrigin o) :: as' => (Some o, revAppend acc as')
              | a :: as' => grOrigin (a :: acc) as'
              | [] => (None, reverse acc)
        fun ffLV style_ title =
            <xml><div class={Css.fromFolderLV}>{Js.fromFolderIcon style_} {[title]}</div></xml>
        fun ff inner =
            <xml>
              <div class={Css.fromFolder}>
                <div class={Css.fromFolderFrom}>from </div>
                <div class={Css.fromFolderFolder}>{inner}</div>
              </div>
            </xml>
        fun fromFolder' icon =
            if uim.Depth > 0 then <xml/> else
            case (showFeedTitles,
                  grOrigin [] (uimAttachments (UIM uim)),
                  Js.getSubItemByUrl mkey.BlogFeedUrl) of
              | (_, (Some o,_), _) => (* origin показываем всегда *)
                if icon then
                    ffLV (Some (Js.faviconStyle o.HtmlUrl)) o.StreamTitle
                else
                    ff <xml><a href={bless o.HtmlUrl} target={"_blank"}>{txt o.StreamTitle} <div class={Css.fromFolderImported}>{[if o.Guid <> "" then "(imported)" else "(unsubscribed)"]}</div></a></xml>
              | (True, _, Some (si : subItem)) =>
                if icon then
                    ffLV si.FaviconStyle si.Title
                else
                    ff (Js.setFeedLink si.Index (txt si.Title))
              | _ => <xml/>
        val fromFolder = fromFolder' False
        fun text m = Js.preprocessMessageText m.Text
        fun attachments m =
            let val as = (grOrigin [] m.Attachments).2 in
            if uim.Depth > 0 || isNull as then <xml/> else <xml>
              {case as of
(*                  | (AImage _) :: [] => <xml/> *)
(*                    (\* не пишем "Attachment:", если у нас только один image *\) *)
                 | _ => <xml>
                   <div class="attachmentsLabel">
(*                      {["Attachment" ^ *)
(*                        if List.length as > 1 then "s:" else ":"]} *)
                   </div></xml>}
              {List.mapX (fn a => <xml><div class="attachment">
                {Js.attachmentXml a}</div></xml>) as}
            </xml>
            end
        val noSubject = noSubject subject (UIM uim)
        val compact =
            case (mtvm.Posts, uim.Depth, lvm) of
              | (PVMShort, 0, LVMCompact) => True
              | _ => False
        val authorPicHidden =
            compact ||
            (uim.Depth = 0 &&
             (case mtvm.Posts of
                | PVMMagazine => True | PVMMosaic => True | _ => False))
        val expandable =
            uim.Depth = 0 && (case mtvm.Posts of PVMFull => False | _ => True)
        fun shortClass mv c =
            case mv of
                MVFull _ => c
              | MVShort _ =>
                (case (mtvm.Posts, uim.Depth) of
                   | (PVMMagazine, 0) => classes Css.magazine c
                   | (PVMMosaic, 0) => classes Css.mosaic c
                   | _ =>
                     classes (if compact then Css.compact else Css.short) c)
        val post = uim.Depth = 0
        fun postClass c =
            classes
            (if uim.Depth = 0 then Css.post
             else if uim.Depth = 1 then Css.depth1
             else Css.depth2) c
(*             if post then classes Css.post c else c *)
        fun emptyAuthorPic s = <xml>
          <div class={Css.emptyAuthorPic}>
            <div class={classes Css.emptyAuthorPicChar s}>A</div>
          </div>
          </xml>
        val commentsButton =
            if post && hasComments then <xml>
              <div class={postClass Css.msgFooter}>
              <a class={classes Css.button Css.msgCommentsButton}
                    onclick={fn _ => (* set noScroll True; *)select True False (UIM uim); stopPropagation; toggleCollapsed (return ()) (UIM uim)}
                    title={"Unread comments count. \nClick to collapse/expand comments. \nKeyboard shortcut: 'o'"}
            >{dyn_ (rc <- signal subForest.ResultsCount;
                  c <- signal uim.Collapsed;
                  return (txt ((if rc > 500 then "500+" else show rc)
                               ^ " comment" ^ (if rc <> 1 then "s" else "")
                               ^ (case c of Expanded => ""
                                          | Collapsed _ => " ...")
             )))}
              </a></div></xml>
            else <xml/>
        fun keepUnreadButton cls =
            if uim.Mi.ReadLocked then <xml/> else
            symbolButton "Keep unread" (* cls *)
                     Css.btnKeepUnread
                     (* "" *) (* "Keep unread" *)
                     "Mark message read/unread. \nKeyboard shortcut: 'm'"
                     (stopPropagation; mw.ToggleRead (UIM uim))
        fun msgButtons shareId link =
            <xml>{tagButton}{
            case link of
              | None => keepUnreadButton Css.buttonRight
              | Some l =>
                <xml><span id={shareId} class={Css.share}>{
                  symbolButton "Share" (* Css.buttonMiddle *) Css.btnShare(*  "" *) "Share or bookmark"
                          (r <- uimReadabilityMode (UIM uim);
                           mw.Po.Toggle mw.SharePopup
                               (shareMenu mw.Po mw.BackgroundRpc mw.MsgDivId shareId
                                (if noSubject
                                 then mh.ShortText else subject)
                                l r mkey))
                  }{(* все-таки разрываем, иначе я не цепляюсь глазами
                      за значок прочитанности в процессе прокрутки *)
                   keepUnreadButton Css.buttonRight
                  }</span></xml>}</xml>
        val imageView = case mtvm.Posts of
                          | PVMMagazine => True
                          | PVMMosaic => True
                          | _ => False
        val toggleFull' =
            toggleFull toggleCollapsed
                       (Js.forceImpure (select False False (UIM uim)))
                       mw (UIM uim)
    in
    shareId <- fresh;
    let fun msgBodyAndFooter commentsButton buttons body = <xml>
        <div class={Css.msgButtons}>{buttons}</div>
        <div class="msgBody">
          {body}
(*           {if uim.Depth = 0 && not (isResult uim.Mi) then <xml> *)
(*             <span class="newCommentsHint">New comments in post</span></xml> *)
(*           else <xml/>} *)

          <div class="clearBoth"></div>
        </div> (* msgBody *)
        {commentsButton}
(*         {if not imageView then commentsButton else <xml/>} *)
(*           <div class="msgButtons"> *)
(*             {keepUnreadButton} *)
(*             {share} *)
(*           </div> *)
      </xml>
      val authorPic =
          <xml><div class={Css.authorPic}>
            <div class="authorPicCenter">
              {case mvAuthorPic uim.Mi.MsgView of
                | Some u =>
                  <xml><active code={Js.authorPicImg u (emptyAuthorPic authorStyle)} /></xml>
                | None =>
                  emptyAuthorPic authorStyle}
            </div>
          </div></xml>
    in
    return <xml>
        {Js.msgOnClick "clicker"
                       (fn cls e =>
                        ns <- get noSelect;
                        if ns(*  || e.CtrlKey || e.ShiftKey *)
(*                            || e.AltKey || e.MetaKey = False || *)
(*                            (case e.Button of Left => False | _ => True) *) then
                            set noSelect False
                        else (
(*                         debug ("clicker: " ^ cls); *)
                        if (expandable &&
                              Js.strIndexOf "postHeader" cls = -1 &&
                              (Js.strIndexOf "clicker" cls <> -1 ||
                               Js.strIndexOf "msgFrame" cls <> -1 ||
                               Js.strIndexOf "msgFooter" cls <> -1)) then
                             toggleFull'
                        else
                            select True True (UIM uim))) <xml>
        <div dynClass={r <- signal uim.Read;
                       s <- signal uim.Selected;
                       mv <- signal uim.Mv;
                       st <- signal uim.Starred;
                       return (ifClass st Css.starred
                              (ifClass r Css.read
                              (ifClass expandable Css.expandable
                              (classes (if s
                                        then Css.selected else Css.unselected)
                                       (shortClass mv
                                              (postClass Css.msgFrame))))))
                      }
             style={prop1 "margin-left"
                          (show (2.75*float (if uim.Depth > 2 then 2 else uim.Depth)) ^ "em")}
             id={uim.FrameId}
             >
        <div class={ifClass compact Css.compact Css.snapPoint} id={uim.SnapId}></div>
        {if compact then let val from = fromFolder' True in
          Js.msgOnClick "" (fn _ e =>
                               ns <- get noSelect;
                               when (not ns)
                                    (if e.ShiftKey then
                                         mw.ToggleRead (UIM uim)
                                     else
                                         toggleFull'))
          <xml><div class={ifClass ultraCompact Css.ultra Css.postHeader}
               >{
            timeLink (uimMsgLink (UIM uim))
            }<div class="mlineLeft"
              >{starButton}{from}</div><div class="mline" title={""}
              >{tags True}<div class="msubject" title={""}>{[subject]}</div>{
               if mh.ShortText <> "" || subject = "" then
                   <xml><div class="mtext" title={""}> - {[mh.ShortText]}</div></xml>
               else <xml/>
            }</div><div class="mlineOver"></div></div></xml> end else <xml/>}
        {if authorPicHidden then <xml/> else authorPic}
        <dyn signal={
          mv <- signal uim.Mv;
          let val link = case mv of
                  | MVFull { Msg = m } => m.Link
                  | MVShort { CachedMsg = Some m, ... } => m.Link
                  | _ => None
              val buttons = <xml>
                {if compact then <xml/> else
                 <xml>{timeLink link}{starButton}</xml>
                }{msgButtons shareId link}
                </xml>
              val subj =
                  if noSubject then <xml/>
                  else markReadNoSelectLink msubject "" link subject
              val anysubj =
                  if noSubject then
                      markReadNoSelectLink msubject "" link mh.ShortText
                  else subj
              fun postImage src =
                  <xml><div class={Css.postImage}>
                  <div class={Css.postImageCenter}>
                  {case src of
                    | Some s => s
                    | None => <xml/>}
                  </div></div></xml>
          in
          return (case mv of
            | MVFull { Msg = m } => <xml>
              {if authorPicHidden then authorPic else <xml/>}
              {msgBodyAndFooter commentsButton buttons <xml>
                {subj}{tags False}
                {fromFolder}
                {case m.Tags of
                      _ :: _ => if uim.Depth = 0 then
                                    <xml><div class="mtags">
                                    {["tags: " ^ intercalate ", " m.Tags]}
                                    </div></xml>
                                else <xml/>
                    | [] => <xml/>
                }
              <div class="mauthor">
                {textWithLink' ""(* authorComma *) m.AuthorUri author}
              </div>
              {case (uim.Parent, uim.Prev) of
                 | (Some (UIM p), _ :: _) =>
                   (* если есть родитель,
                      и сообщение не первое в цепочке ответов *)
                   if uim.Depth < 2 then <xml/> else
                   (case uimAuthorAndShortText (UIM p) of (author, shortText) =>
                   <xml><div class="inReplyTo" title={shortText}
                             onclick={fn _ => stopPropagation;
                                              select False True (UIM p)}>{
                     buttonSymbol Css.btnInReplyTo
                     }<div class="mauthor">
                       {[if author = "" then
                        (if p.Depth = 0 then "post" else "_") else author]}
                     </div></div>
                   </xml>)
                 | _ => <xml/>
              }
              <div class="mtextPad"></div>
              <div class="mtext">{
                if uim.Depth > 0 then text m else
                <xml>
                  {dyn_ (rv <- signal uim.ReadabilityView;
                         return (case rv of
                           | RVNone _ => <xml/>
                           | RVError e => <xml>
                             <div class="readabilityError">
(*                                Can't get full post text:<br/> *)
                               {Js.preprocessMessageText e}
                             </div></xml>
                           | RVLoading => <xml>
                             <div class="readabilityLoading">
                               <span class="loadingGif"></span> Retrieving full text...
                             </div></xml>
                           | RVReadability x => x
                     ))}
                  {displayIf uim.ShowText (<xml>{text m}{attachments m}</xml>)}
                </xml>
              }</div>
            </xml>}
            </xml>
          | MVShort s =>
            if compact then <xml/> else
            Js.msgOnClick "" (fn _ _ =>
(*                               set noScroll (not (isResult uim.Mi)); *)
                              ns <- get noSelect;
                              when (not ns) (
                              stopPropagation;
                              (* в list view все-таки скролим *)
                              toggleFull'))
            (case (uim.Depth, mtvm.Posts, s.CachedMsg) of
               | (0, PVMMagazine, Some m) =>
                 let val imgsrc = Js.messageImage m.Text (attachments m) in
                     msgBodyAndFooter <xml/> buttons <xml>
                       {postImage imgsrc}
                       <div class="magazineText">
                       {subj}{tags False}
                       {fromFolder}
                       <div class="mtextPad"></div>
                       <div class="mtext">{[mh.ShortText]}</div>
                       {commentsButton}
                       </div>
                     </xml>
                 end
               | (0, PVMMosaic, Some m) =>
                 let val imgsrc = Js.messageImage m.Text (attachments m) in
                     msgBodyAndFooter <xml/> <xml/> <xml>
                       {postImage imgsrc}
                       {anysubj}(* {tags} *)
                       {fromFolder}
                       <div class="mtextPad"></div>
                       <div class="mtext">{[mh.ShortText]}</div>
                     </xml>
                 end
               | _ => msgBodyAndFooter commentsButton
                                       (if uim.Depth = 0 then buttons
                                        else keepUnreadButton null)
                                       <xml>
              {subj}{tags False}
(*               {if noSubject then <xml/> else *)
(*                <xml><div class="msubject">{subject}</div></xml>} *)
              {fromFolder}
              <div class="mauthor">{author}{[authorComma]}</div>
              <div class="mtext">{[mh.ShortText]}</div></xml>)
          ) end } />
      </div> (* msgFrame *)
      </xml>} (* {Js.msgOnClick <xml>... *)

      {if hasComments then
           <xml>
             <div dynClass={lc <- signal uim.LoadingChildren;
                           return (ifClass (not lc) Css.displayNone
                                           loadingExpanded)}>
                 <span class="loadingGif"></span> Loading...
             </div>
             <div id={uim.GrowId}
                  dynClass={c <- signal uim.Collapsed;
                               return (case c of
                                 | Expanded => Css.commentsGrow
                                 | Collapsed _ =>
                                   classes Css.commentsGrow Css.collapsed)}>
             {children}
           </div></xml>
       else <xml/>}
    </xml>
    end end end

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

val defaultSubItem : transaction subItem =
    c <- source emptyCounters;
    m <- source defaultMsgTreeViewMode;
    return
        { Hash          = "folder/"
        , Index         = -1
        , Title         = ""
        , SIType        = SITFolder { Folder = "" }
        , Counters      = c
        , ViewMode      = m
        , ParentFolders = []
        , DomIds        = []
        , FaviconStyle  = None
        }
fun subItemHash (sit : subItem) : string = sit.Hash
fun subItemTitle (si : subItem) = si.Title
fun isFeed (si : subItem) =
    case si.SIType of
      | SITFeed _ => True
      | _ => False
fun subItemUrl (si : subItem) =
    case si.SIType of
      | SITFeed s => Some s.Subscription.Url
      | _ => None

fun subscriptions tagsOnly t h siv crc = withUser "subscriptions" (userSubscriptionsAndRenames tagsOnly t h siv crc)
val subscriptionsAndSettings = withUser "subscriptionsAndSettings" (userSubscriptionsAndSettings "")
val restoreSubscriptions = withUser "restoreSubscriptions"
    (fn userId =>
        userRestoreSubscriptionsFromBackup userId;
        t <- getUrTime_ ();
        userSubscriptionsAndRenames False t "" "" [] userId)

fun addSubscription t url l =
    if url = "" then error <xml>Empty feed URL</xml>
    else withUser "addSubscription"
    (fn userId =>
        h <- userSubscribe userId url None [];
        s <- userSubscriptionsAndRenames False t "" "" [] userId;
        return (case s of (x,siv,s,i,r) => (h,x,i,siv,s,r))) l
fun addDiscoverySubscription t url country query l =
    if url = "" then error <xml>Empty feed URL</xml>
    else withUser "addDiscoverySubscription"
    (fn userId =>
        h <- userDiscoverySubscribe userId url country query None [];
        s <- userSubscriptionsAndRenames False t "" "" [] userId;
        return (case s of (x,siv,s,i,r) => (h,x,i,siv,s,r))) l
fun renameSubscription t url to l =
    if url = "" then error <xml>Empty feed URL</xml>
    else withUser "renameSubscription"
    (fn userId =>
        userRenameSubscription userId url to;
        userSubscriptionsAndRenames False t "" "" [] userId) l
fun renameFolder t from to l =
    if to = "" then error <xml>Empty folder name</xml>
    else withUser "renameFolder"
    (fn userId =>
        h <- userRenameFolder userId from to;
        s <- userSubscriptionsAndRenames False t "" "" [] userId;
        return (case s of (x,siv,s,i,r) => (h,x,siv,s,i,r))) l
fun editSubscriptionFolders t url f add l =
    if url = "" || f = "" then error <xml>Empty feed URL or folder</xml>
    else withUser "editSubscriptionFolders"
    (fn userId =>
        userEditSubscriptionFolders userId url f add;
        userSubscriptionsAndRenames False t "" "" [] userId) l
fun retrySubscription url =
    withUser "retrySubscription"
             (fn userId => userRetrySubscription userId url)
fun removeSubscriptions t (urls : list string) =
    withUser "removeSubscriptions"
             (fn userId =>
                 userUnsubscribe userId urls;
                 userSubscriptionsAndRenames False t "" "" [] userId)
fun getTree mtvm reqs =
    withUser "getTree"
             (fn userId => userGetTree AMNormal userId mtvm reqs)
fun getTreeD url mtvm reqs =
    withUser "getTree"
             (fn userId => userGetTree (AMDiscovery { Url = url })
                                       userId mtvm reqs)

fun toggleFolder backgroundRpc si =
    vm <- get si.ViewMode;
    let val vm' = modifyF [#FolderExpanded] not vm
    in
        set si.ViewMode vm';
        updateCounters si 0 0 (* обновляет CSS-класс *);
        case si.SIType of
          | SITFolder { Folder = name } =>
            backgroundRpc.AddAction (BGSetFolderViewMode
                                         { Folder = name, ViewMode = vm' })
          | SITAllTags =>
            backgroundRpc.AddAction (BGSetFolderViewMode
                                         { Folder = ",SITAllTags", ViewMode = vm' })
          | _ => return ()
    end
fun renameDialog ps popup what name act =
    tid <- fresh;
    text <- source name;
    let val ren = t <- get text; when (t <> "") (act t; ps.Hide) in
    d <- ps.New Css.renameDialog <xml>
      Rename{[what]} "{[name]}" to<br/>
        <ctextbox id={tid}
          class="renameInput" source={text} size={30}
          onkeydown={fn k => if k.KeyCode = 13 then ren else
(*                              if k.KeyCode = 27 then ps.Hide else *)
                             return()}
          />{textButton "OK" ren}</xml>;
    ps.Toggle popup d;
    Js.select tid;
    Js.focus tid
    end
fun unsubscribeLiI ps unsubscribe setFeed title act (sis : list subItem) : xbody =
    ps.LiI Css.btnEmpty title (
    c <- (case sis of
          | si :: [] =>
            confirm ("Are you sure you want to unsubscribe from \""
              ^ si.Title ^ "\"?")
          | _ => confirm ("Are you sure you want to unsubscribe from "
              ^ show (List.length sis) ^ " subscriptions and delete the folder? This action can not be undone."));
    when c ((* Js.trackEvent "UI" "Unsubscribe" uid; *)
            act;
            Js.setLocationHash "folder/";
            dsi <- defaultSubItem;
            setFeed dsi;
            unsubscribe sis))

val tupleEq : eq (string * string) =
    mkEq (fn (a1,a2) (b1,b2) => a1=b1 && a2=b2)

val eqPFT : eq publicFeedType =
    mkEq (fn a b => case (a,b) of
      | (PFTAll, PFTAll) => True
      | (PFTStarred, PFTStarred) => True
      | (PFTAllTags, PFTAllTags) => True
      | (PFTTag { TagName = t1 }, PFTTag { TagName = t2 }) => t1 = t2
      | (PFTFolder { Folder = f1 }, PFTFolder { Folder = f2 }) => f1 = f2
      | _ => False)

fun enablePublicFeed t = withUser "enablePublicFeed" (userEnablePublicFeed t)
fun disablePublicFeed t = withUser "disablePublicFeed" (userDisablePublicFeed t)
fun generateNewPublicFeed t = withUser "generateNewPublicFeed" (userGenerateNewPublicFeed t)

fun publicFeedDialog typ (ps : popups) (popup : source xbody) publicFeeds backgroundRpc : transaction {} =
    tid <- fresh;
    let fun disabled t =
            <xml><span class="publicFeedDisabled">No feed</span><br/></xml>
        fun sel tid =
            Js.select tid;
            Js.setReadOnly tid True;
            Js.focus tid
        fun e act =
            queueRpcB backgroundRpc
                      act
                      (fn pfs =>
                          modify publicFeeds (fn l => case List.assoc typ l of
                              | None => (typ, pfs) :: l
                              | Some _ => List.mp (fn (t,p) =>
                                                      if t = typ then (t,pfs)
                                                      else (t,p)) l);
                          sel tid
                      )
        val enable =
            <xml>{textButton "Enable" (e (fn l => rpc (enablePublicFeed typ l)))}</xml>
        fun feedsList l =
            <xml><p>{l}</p></xml>
(*             <xml><div class="publicFeedsList">{l}</div></xml> *)
    in
    d <- ps.New Css.publicFeedDialog <xml>
      <p>Public feed for
      {[case typ of
          | PFTAll => "Latest"
          | PFTStarred => "Starred items"
          | PFTAllTags => "Tags"
          | PFTTag { TagName = t } => "tag \"" ^ t ^ "\""
          | PFTFolder { Folder = f } => "folder \"" ^ f ^ "\""
      ]}:</p>
      {dyn_ (pf <- signal publicFeeds;
             return (case List.assoc typ pf of
               | Some l => <xml>
                 {feedsList (List.mapX (fn (f,e,_) =>
                                let val a = "https://bazqux.com/feed/" ^ f
                                in
                                    if e then
(*                                         <xml><ctextbox id={tid} *)
(*           class="renameInput" value={a} size={30} /><br/></xml> *)
                                        <xml>{hrefLinkStopPropagation (txt a) a}<br/></xml>
                                    else
                                        disabled a
                                end) l)}
                 {if List.all (fn (_,e,_) => not e) l then enable else <xml><p>
                    {textButton "Disable" (e (fn l => rpc (disablePublicFeed typ l)))}
                    {textButton "Generate new"
                                (ok <- confirm "Current feed will be deleted. Are you sure you want to generate new feed address?";
                                 when ok (e (fn l => rpc (generateNewPublicFeed typ l))))}</p>
                    <p>{hrefLinkStopPropagation
                            (txt "New IFTTT recipe")
                            "https://ifttt.com/myrecipes/personal/new"}
                      (select Feed channel and paste feed URL)</p>
                 </xml>}
                 </xml>
               | None => <xml>{feedsList (disabled "No feed")}{enable}</xml>))}
      </xml>;
    ps.Toggle popup d;
    sel tid
    end

fun feedAddressDialog f ps popup  =
    let val a = f.Subscription.Url in
    d <- ps.New Css.renameDialog <xml>
      Feed address:<br/>
      {hrefLinkStopPropagation (txt a) a}
    </xml>;
    ps.Toggle popup d
    end

val discoverySubItemIndex = -100
fun discoverySubItemUrl currentFeed =
    cf <- signal currentFeed;
    return (case (cf.Index = discoverySubItemIndex, cf.SIType) of
      | (True, SITFeed f) =>
        Some f.Subscription.Url
      | _ =>
        None)

fun subscriptionsWidget currentFeed updateCurrentFeed (onUpdateSubInfo : subItem -> subItem -> transaction {}) setFeed backgroundRpc ps popup onlyUpdatedSubscriptions exactUnreadCounts subscribeDiscoveryFeed clearMtvm refresh =
    Js.setOnSetFeed (fn i => setFeed (getSubItem i));
    Js.setOnToggleFolder (fn i => toggleFolder backgroundRpc (getSubItem i));
    selected <- source (None : option subItem);
    html <- source <xml/>;
    lastRenTime <- source minTime;
    updatePending <- source False;
    titleHash <- source "";
    subItemsVersion <- source "";
    updateCount <- source 0;
    queueUpdateSrc <- source (return ());
    folders <- source [];
    tagsImported <- source False;
    rpcErrorsCount <- source 0;
    lastChangedReadCounters <- source [];
    bgRefresh <- source False;
    publicFeeds <- source [];
    let val queueUpdate =
            q <- get queueUpdateSrc; q
            (* если в updateSubscriptions' делать
               updateSetTimeout (queueSubscriptions updateSubscriptions'),
               почему-то копится env. По-этому разнес вызов через source.
             *)
        fun tryRename u subUrlRenames =
            let fun findRen u s =
                    case s of
                       | [] => u
                       | (_,f,t) :: rs => if f = u then t else findRen u rs
                fun go u prev =
                    (* rename-ов может быть несколько за раз, ищем до упора
                     *)
                    let val r = findRen u subUrlRenames
                    in
                        if elem r prev then r else go r (r::prev)
                    end
            in
                go u (u::[])
            end
        fun updateSubscriptions' tagsOnly (x, siv, sirs, ti, subUrlRenames) = (* withSetFeed *)
(*             t1 <- now; *)
            modify updateCount succ;
            set tagsImported ti;
            hash0 <- Js.getLocationHash;
            sitSel0 <- getSubItemByHash hash0;
            let val hash = tryRename hash0 subUrlRenames
            in
            when (hash <> hash0) (Js.setLocationHash hash);
            (case x of
               | None => return ()
               | Some (x, th, fs) =>
                 set titleHash th;
                 set folders fs;
                 set html x);
            when (not tagsOnly || Option.isSome x)
                 (set subItemsVersion siv);
            when (Js.setSubItems (Option.isNone x, sirs)) (
            crc <- Js.getChangedReadCounters;
            set lastChangedReadCounters crc;
            (case subUrlRenames of
                 (t,_,_) :: _ => set lastRenTime t | _ => return ());
            (* проверяем, а не обновился ли текущий выделенный фид *)
            sitSel1 <- getSubItemByHash hash;
            (case (sitSel0, sitSel1) of
               | ( Some si0, Some si1) =>
                 c0 <- get si0.Counters;
                 c1 <- get si1.Counters;
(*                  debug (show (isFeed si1) ^ *)
(*                         " " ^ show c0.ScannedPercent ^ " -> " ^ show c1.ScannedPercent ^ *)
(*                         " " ^ show c0.TotalComments ^ " -> " ^ show c1.TotalComments); *)

                 when (isFeed si1 && c0.Scanning = 1 && c1.Scanning = 0)
                      (setFeed si1);
                 onUpdateSubInfo si0 si1
               | _ => return ());
            updateCurrentFeed;
            c <- get (getSubItem 0).Counters;
            when (c.Scanning > 0 || c.ScanningComments > 0)
                 queueUpdate
            (* или getSubItem 1 и update tagsOnly для импорта тегов *)
(*             t2 <- now; *)
(*             debug ("updateSubscriptions': " ^ *)
(*                    show (diffInMilliseconds t1 t2) ^ " ms in total" *)
(*                   ) *)
            )
            end
        fun updateSubscriptions tagsOnly =
            (* не делаем queueRpcB, т.к. это блокирует остальные rpc
               и дико тормозит прокрутка и добавление к дереву в процессе
               подписки.
               Есть потенциальная проблема, что из двух параллельно работающих
               RPC, один успеет сделать endGetList, а второй обломится
               и некоторые BGActions пропадут. Но это редкий случай, и при
               проблемах с сетью неудивительно, если что-то отвалится.
               А в целом, надо не (set activeList []), а удалять из него
               совпадающие bgactions (чтобы не сравнивать лишний раз,
               можно нумеровать и чистить/не чистить до последнего
               обработанного).

               Еще проблема (существующая и в последовательном режиме) --
               пока идет запрос на обновление подписок, что-то может уже
               прочитаться и счетчики будут не совпадать.
               Правда это исправится само собой на следующем обновлении.
               Т.е. реально этот глюк может проявиться только если что-то
               будет прочитано в процессе последнего обновления текущей подписки
               (шанс есть, но где-то 1 из 100).
               Чтобы его поправить, надо хранить не UnreadCount, а Read+Total.
               Read устанавливается при первоначальной загрузке и обновляется
               только в UI, а Total может обновляться с сервера.
             *)
            uc <- get updateCount;
            s <- withBGRpcList backgroundRpc
                (fn l =>
                    t <- get lastRenTime;
                    h <- get titleHash;
                    siv <- get subItemsVersion;
                    crc <- get lastChangedReadCounters;
                    bg <- get bgRefresh;
                    set bgRefresh False;
                    Js.clearChangedReadCountersSet;
                    tryRpc (subscriptions tagsOnly t h
                                          (if bg then "bg" ^ siv else siv)
                                          crc l));
            case s of
              | Some s =>
                set rpcErrorsCount 0;
                uc' <- get updateCount;
                when (uc = uc') (updateSubscriptions' tagsOnly s)
                (* ^ не обновляем подписки,
                   если во время запроса их уже обновили *)
              | None =>
                ec <- get rpcErrorsCount;
                set rpcErrorsCount (ec+1);
(*                 debug ("subscriptions request failed (" ^ show (ec+1) ^ " times)"); *)
                queueUpdate
        val queueUpdate_ =
            p <- get updatePending;
            when (not p)
                 (set updatePending True;
                  ec <- get rpcErrorsCount;
                  Js.setTimeout "queueUpdate_"
                                (set updatePending False;
                                 updateSubscriptions False)
                                (min 60000 (3000 + ec*3000)))
        fun addSub u =
            when (u <> "")
            (queueRpcB backgroundRpc
                       (fn l =>
                           t <- get lastRenTime;
                           rpc (addSubscription t u l))
                       (fn (h, x, i, siv, s, rens) =>
                           h0 <- Js.getLocationHash;
                           let val h' = tryRename h rens
                           in
                           Js.setLocationHash h';
                           updateSubscriptions' False (x,siv, s,i,rens);
                           when (h0 = h') refresh
                           (* подписались из discovery, хеш не менялся *)
                           end
                       ))
        fun addDiscoverySub u country query =
            when (u <> "")
            (queueRpcB backgroundRpc
                       (fn l =>
                           t <- get lastRenTime;
                           rpc (addDiscoverySubscription t u country query l))
                       (fn (h, x, i, siv, s, rens) =>
                           h0 <- Js.getLocationHash;
                           updateSubscriptions' False (x,siv, s,i,rens);
                           if h <> h0 then
                               Js.setLocationHash h
                           else
                               refresh))
        fun editFolders (si : subItem) folder add =
            case subItemUrl si of
              | Some u =>
                queueRpcB backgroundRpc
                          (fn l =>
                              t <- get lastRenTime;
                              rpc (editSubscriptionFolders t u folder add l))
                          (updateSubscriptions' False)
              | _ =>
                return ()
        fun rename (si : subItem) newTitle =
            case subItemUrl si of
              | Some u =>
                queueRpcB backgroundRpc
                          (fn l =>
                              t <- get lastRenTime;
                              rpc (renameSubscription t u newTitle l))
                          (updateSubscriptions' False)
              | _ =>
                return ()
        fun renameF (folder : string) (to : string) =
            queueRpcB backgroundRpc
                      (fn l =>
                          t <- get lastRenTime;
                          rpc (renameFolder t folder to l))
                      (fn (h, x, siv, s, i, rens) =>
                          h0 <- Js.getLocationHash;
                          when (h0 = "folder/" ^ folder)
                               (Js.setLocationHash h);
                          updateSubscriptions' False (x,siv, s,i,rens))
        fun retryScanning (si : subItem) =
            modify updateCount succ; (* уже ручками обновили *)
            (* TODO: по идее, нельзя обновлять пока retry не прошел,
               а не просто пропускать следующий update
             *)
            case subItemUrl si of
              | Some u =>
                queueRpcB backgroundRpc
                          (fn l => rpc (retrySubscription u l))
                          (fn _ => return ());
                queueUpdate
              | None => return ()
        fun unsubscribe (sis : list subItem) =
            Js.forceImpure (hideSubItems sis);
            queueRpcB backgroundRpc
                      (fn l =>
                          t <- get lastRenTime;
                          rpc (removeSubscriptions t
                                   (List.mapPartial subItemUrl sis) l))
                      (fn r =>
                          List.app clearMtvm (List.mapPartial subItemUrl sis);
                          updateSubscriptions' False r)
        fun onDragAndDrop dd =
            backgroundRpc.AddAction (BGDragAndDrop dd);
            set titleHash "";
            updateSubscriptions False
        fun sort act q =
            c <- confirm q;
            when c
                 (backgroundRpc.AddAction act;
                  updateSubscriptions False)
        fun publicFeedLiI typ =
            ps.LiI Css.btnEmpty "Public feed"
                   (publicFeedDialog typ ps popup publicFeeds backgroundRpc)
   in
    allTagsMenu <- ps.NewMenu Css.foldersMenu <xml>
       {ps.LiI Css.btnEmpty "Sort tags"
                  (sort BGSortTags
                        "Are you sure you want to reset tags ordering? All your drag and drop customizations will be cleared. This operation can't be undone.")}
       {publicFeedLiI PFTAllTags}
    </xml>;
    subMenu <- ps.NewMenu Css.subscriptionsMenu <xml>
      <dyn signal={
        ous <- signal onlyUpdatedSubscriptions;
        euc <- signal exactUnreadCounts;
        let fun checkButtons c =
                if c then (Css.btnEmpty,Css.btnCheck)
                else (Css.btnCheck,Css.btnEmpty)
            val (al,u) = checkButtons ous
            val (c500,exact) = checkButtons euc
            fun hideRead v =
                set onlyUpdatedSubscriptions v;
                backgroundRpc.AddAction (BGSetOnlyUpdatedSubscriptions
                                             { Value = v })
            fun setExact e =
                Js.setExactUnreadCounts e;
                set exactUnreadCounts e;
                backgroundRpc.AddAction (BGSetExactUnreadCounts { Value = e })
        in
        return <xml>
          {ps.LiI al "Show all" (hideRead False)}
          {ps.LiI u  "Show updated" (hideRead True)}
          <hr/>
          {ps.LiI c500  "500+ unread counts" (setExact False)}
          {ps.LiI exact "Exact unread counts" (setExact True)}
          <hr/>
          {ps.LiI Css.btnEmpty "Sort feeds and folders"
                  (sort BGSortAllFeedsAndFolders
                        "Are you sure you want to reset subscriptions ordering? All your drag and drop customizations will be cleared. This operation can't be undone.")
          }
          {ps.LiI Css.btnEmpty "Sort top level only"
                  (sort (BGSortFolder { Folder = "" })
                        "Are you sure you want to reset top level ordering (ordering inside folders will not be changed)? Your drag and drop customizations will be cleared. This operation can't be undone.")
          }
          <hr/>
          {publicFeedLiI PFTAll}
          <a link={opml ()} onclick={fn _ =>
(*                                         Js.trackEvent "UI" "ExportOPML" uid; *)
                                        ps.Hide; redirect (bless "/opml")}>
            <li>{buttonSymbol Css.btnEmpty}Export OPML</li></a>
        </xml>
        end} />
    </xml>;
    newFolder <- source "";
    let fun foldersMenuContents cf =
        case cf.SIType of
          | SITFeed { Subscription = { Folders = sfs, ... }, ... } =>
            dyn_ (fs <- signal folders;
            let val new =
                    ps.Hide;
                    nf <- get newFolder;
                    set newFolder "";
                    when (nf <> "")
                         (editFolders cf nf True)
            in
            return <xml>
              {List.mapX
               (fn f =>
                    if elem f sfs then
                        ps.LiI Css.btnCheck f (editFolders cf f False)
                    else
                        ps.LiI Css.btnEmpty f (editFolders cf f True))
               fs}
              <div class={Css.newFolder}>
              New folder<br/>
              <ctextbox class="newFolderInput" source={newFolder} size={20}
              onkeydown={fn k => if k.KeyCode = 13
                                 then new
(*                                  else if k.KeyCode = 27 then ps.Hide *)
                                 else return()}
              />{textButton "Add" new}
              </div>
            </xml>
            end)
          | _ => <xml/>
    in
    foldersMenu <- ps.NewMenu Css.foldersMenu <xml>
      <dyn signal={
        cf <- signal currentFeed;
        return (foldersMenuContents cf)
        } />
    </xml>;
    let fun pfMenuContents typ = Some (publicFeedLiI typ)
        fun subItemMenuContents context si = case si.SIType of
              | SITAll => pfMenuContents PFTAll
              (* <xml><hr/> *)
(*                 {ps.LiI Css.btnEmpty "Unsubscribe" clearSubscriptions : xbody}</xml> *)
              | SITStarred => pfMenuContents PFTStarred
              | SITAllTags => pfMenuContents PFTAllTags
              | SITTag t => pfMenuContents (PFTTag t)
              (* TODO: переименование тегов *)
              | SITSearch _ => None
              | SITFolder { Folder = f } =>
                if f = "" then None else Some <xml>
                {ps.LiI Css.btnEmpty "Rename folder"
                       (renameDialog ps popup " folder" f (renameF f)) : xbody}
                {unsubscribeLiI ps unsubscribe setFeed "Delete folder"
                             (Js.forceImpure (hideSubItems (si :: [])))
                             (Js.getSubItems si.Index) : xbody}
                {displayIfC context
                 (ps.LiI Css.btnEmpty "Sort folder"
                  (sort (BGSortFolder { Folder = f })
                        "Are you sure you want to reset folder ordering? Drag and drop customizations will be cleared. This operation can't be undone."))
                }
                <hr/>
                {publicFeedLiI (PFTFolder { Folder = f })}
                </xml>
              | SITFeed f =>
                Some <xml>
                {if si.Index = discoverySubItemIndex then
                  ps.LiI Css.btnEmpty "Subscribe"
                      (subscribeDiscoveryFeed f.Subscription.Url)
                 else <xml>
                {ps.LiI Css.btnEmpty "Rename"
                       (renameDialog ps popup "" si.Title (rename si)) : xbody}
                (* TODO: rename c клавы, так всегда можно посмореть
                   полное название текущего фида
                 *)
                {displayIfNotC context
                 <xml><span class="foldersMenuItem">
                   {foldersMenu.2}
                   {ps.LiSubI Css.btnEmpty "Folders"}
                 </span></xml>}
                {unsubscribeLiI ps unsubscribe setFeed
                                "Unsubscribe" (return ()) (si :: []) : xbody}
                </xml>}
                {displayIfNotC context <xml><hr/></xml>}
                {ps.LiI Css.btnEmpty "Feed address"
                        (feedAddressDialog f ps popup)}
                {displayIfC context <xml>
                  {dyn_ (f <- signal folders;
                         return (if notNull f then <xml><hr/>
                                   <div class="menuLabel">Folders</div></xml>
                                 else <xml/>))}
                  {foldersMenuContents si}
                  </xml>}
                </xml>

        fun showMenu menu =
            ps.Hide;
            ps.Toggle popup menu;
            return (Some menu.1)
        fun showPublicFeedMenu t =
            menu <- ps.NewMenu Css.foldersMenu (publicFeedLiI t);
            showMenu menu
        fun onSubscriptionRightClick id =
            case (getSubItem id).SIType of
              | SITAll => showMenu subMenu
              | SITAllTags => showMenu allTagsMenu
              | SITTag t => showPublicFeedMenu (PFTTag t)
              | SITStarred => showPublicFeedMenu PFTStarred
              | _ =>
                (case subItemMenuContents True (getSubItem id) of
                   | Some m =>
                     menu <- ps.NewMenu Css.foldersMenu m;
                     showMenu menu
                   | None => return None)
    in
        Js.setOnSubscriptionRightClick onSubscriptionRightClick;
        Js.registerOnDragAndDrop onDragAndDrop;
        set queueUpdateSrc queueUpdate_;
        Js.registerUpdateSubscriptions
            (fn bg =>
                set bgRefresh bg;
                queueUpdate_);
        return
            { Html = <xml><dyn signal={signal html} /></xml>
            , PublicFeeds = publicFeeds
            , AddSubscription = addSub
            , AddDiscoverySubscription = addDiscoverySub
            , UpdateSubscriptions  = updateSubscriptions
            , UpdateSubscriptions_ = updateSubscriptions'
            , Rename = rename
            , RenameFolder = renameF
            , Unsubscribe = unsubscribe
            , RetryScanning = retryScanning
            , EditFolders = editFolders
            , Folders = folders
            , TagsImported = tagsImported
            , SubMenu = subMenu
            , SubItemMenuContents = subItemMenuContents
            }
    end end end

val postsViewModeEq : eq postsViewMode =
    let fun pn pvm =
            case pvm of
              | PVMShort => 0
              | PVMFull => 1
              | PVMMagazine => 2
              | PVMMosaic => 3
    in
        mkEq (fn a b => pn a = pn b)
    end
val scrollModeEq : eq scrollMode =
    let fun sn sm =
            case sm of
              | SMNormal => 0
              | SMQuick => 1
              | SMImmediate => 2
    in
        mkEq (fn a b => sn a = sn b)
    end
val listViewModeEq : eq listViewMode =
    let fun sn sm =
            case sm of
              | LVMCompact => 0
              | LVMTwoLines => 1
    in
        mkEq (fn a b => sn a = sn b)
    end
val markReadModeEq : eq markReadMode =
    let fun sn sm =
            case sm of
              | MRMOnScroll => 0
              | MRMManual => 1
              | MRMOnScrollEverywhere => 3
    in
        mkEq (fn a b => sn a = sn b)
    end
val msgKeyEq : eq msgKey =
    mkEq (fn a b => a.BlogFeedUrl = b.BlogFeedUrl
                    && a.PostGuid = b.PostGuid
                    && a.CommentGuid = b.CommentGuid)
val msgIdEq : eq msgId =
    mkEq (fn a b => a.MsgKey = b.MsgKey
                    && a.PostId = b.PostId && a.CommentId = b.CommentId)
(* убирает повторные отмечания одного и того же сообщения прочитанным
   и обращает список
 *)
fun preprocessBGActions l =
    let fun go marks acc l =
            case l of
                [] => acc
              | x :: xs =>
                (case x of
                   | BGMarkMsgRead { MsgId = key, ... } =>
                     if elem key marks then go marks acc xs
                     (* уже есть, оставляем последний *)
                     else go (key :: marks) (x :: acc) xs
                   | _ => revAppend xs (x :: acc)
                          (* ничего больше не трогаем, т.к. сложно
                             проанализировать, что можно подчистить, а что нет
                           *)
                )
    in
        go [] [] l
    end

fun handleBGActions (l : list bgAction) =
    rpc (bgactions l)

val emptyMF = MsgForest { ResultsCount = 0, UnreadCount = 0
                        , List = [], NextReq = None }
fun mkUIForest (MsgForest f) =
    resultsCount <- source f.ResultsCount;
    unreadCount <- source f.UnreadCount;
    firstChild <- source None;
    children <- source [];
    nextReqId <- source None;
    return (UIForest
                { ResultsCount = resultsCount
                , UnreadCount = unreadCount
                , FirstChild = firstChild
                , Children = children
                , NextReqId = nextReqId
           })

fun feedKeyboardActionCF currentFeed currentSearchFeed act =
    cf <- get currentFeed;
    csi <- (case cf.SIType of
              | SITSearch _ => get currentSearchFeed
              | _ => return cf);
    Js.feedKeyboardAction csi.Index act

fun msgsWidget msgDivId po popup sharePopup backgroundRpc currentFeed currentSearchFeed getMsgTreeViewMode updateTags loading =
    scrollMode <- source SMNormal;
    ultraCompact <- source False;
    listViewMode <- source LVMCompact(* LVMTwoLines *);
    markReadMode <- source MRMOnScroll(* MRMOnScrollEverywhere *);
    selectedUIM <- source (None : option uim);
    fullUIM <- source (None : option uim);
    defaultUIForest <- mkUIForest emptyMF;
    uiForest <- source defaultUIForest;
    authors <- source (Css.authorStyles, [], []);
    appending <- source False;
    appendingRpc <- source False;
    loadingComments <- source False;
    appendRequests <- source ([] : list appendReq);
    scrolling <- source 0;
    lastScrollPos <- source 0;
    scrollAfter <- source None;
    lastPostsViewMode <- source PVMFull;
    afterAppendAction <- source (return ());
    toggleCollapsed_ <- source (fn _ _ => return ());
    (* ^  заглушка, чтобы не плодить кучу fun .. and .. and *)
    let val getScrollMode =
            sm <- get scrollMode;
            return (case sm of
                      | SMNormal => "normal"
                      | SMQuick => "quick"
                      | SMImmediate => "immediate")
        fun moveUpDown immediate f =
            set scrollAfter (Some (0, False));
            modify scrolling succ;
            sm <- getScrollMode;
            Js.moveUpDown f msgDivId (if immediate then "immediate" else sm)
                          (set scrollAfter None; modify scrolling pred)
        fun checkUIM resultsOnly (UIM m) f =
            if resultsOnly then
                (mv <- get m.Mv;
                 if isResultMv mv || m.Depth = 0 then
                     (* посты всегда можно выделять *)
                     return (Some (UIM m))
                 else
                     f resultsOnly (UIM m))
            else
                return (Some (UIM m))
        fun whenNotCollapsed (UIM uim) act =
            c <- get uim.Collapsed;
            case c of Collapsed _ => return None | Expanded => act uim.SubForest
        fun findFirst ro (UIForest f) =
            m <- get f.FirstChild;
            case m of
              | None => return None
              | Some uim =>
                checkUIM ro uim nextMsg
(*             let fun go ch = *)
(*                     case ch of *)
(*                      | [] => return None *)
(*                      | (UIM f) :: [] => *)
(*                        mv <- get f.Mv; *)
(*                        if isFullMv mv then return (Some (UIM f)) *)
(*                        else nextMsg (UIM f) *)
(*                      | _ :: ch => go ch *)
(*             in *)
(*                 ch <- get f.Children; *)
(*                 go ch *)
(*             end *)
        and nextMsg ro uim =
            f <- whenNotCollapsed uim (findFirst ro);
            case f of Some m => return f | None => nextSibling ro uim
        and nextSibling ro (UIM uim) =
            n <- get uim.Next;
            case n of
              | Some next =>
                checkUIM ro next nextMsg
              | None =>
                (case uim.Parent of
                   | Some (UIM p) =>
                     (case p.SubForest of UIForest uif =>
                      nri <- get uif.NextReqId;
                      (case nri of
                         | None   => nextSibling ro (UIM p)
                         | Some _ => return None))
                      (* сначала должен выполниться запрос, и только потом,
                       повторно вызываться next.
                       *)
                   | None => return None (* закончились *))
        fun findLast ro (UIForest f) =
            ch <- get f.Children;
            case ch of
              | [] => return None
              | last :: _ =>
                m <- whenNotCollapsed last (findLast ro);
                (case m of
                   | Some _ => return m
                   | None =>
                     checkUIM ro last prevMsg)
        and prevMsg ro (UIM uim) =
            case uim.Prev of
              | prev :: _ =>
                m <- whenNotCollapsed prev (findLast ro);
                (case m of
                   | Some _ => return m
                   | None   => checkUIM ro prev prevMsg)
              | [] =>
                (case uim.Parent of
                   | Some p => checkUIM ro p prevMsg
                   | None   => return None (* закончились *))
        fun parentMsg (UIM uim) =
            return uim.Parent
        fun postMsg (UIM uim) =
            case uim.Parent of
              | Some p => postMsg p
              | None => UIM uim
        fun skipMsgComments (UIM uim) : transaction (int*int*int) =
            read <- get uim.Read;
            set uim.Read True;
            set uim.KeepUnread False;
            case uim.SubForest of UIForest f =>
            rc <- get f.ResultsCount;
            uc <- get f.UnreadCount;
            set f.ResultsCount 0;
            set f.UnreadCount 0;
            ch <- get f.Children;
            List.app (fn c => r <- skipMsgComments c; return ()) ch;
            return (if not read then 1 else 0,
                    if isResult uim.Mi && not read then rc+1 else rc,
                    if not read then uc+1 else uc)
        fun toggleMsgRead (UIM uim) =
            if uim.Mi.ReadLocked then return (0,0,0) else
            toggle uim.Read;
            r <- get uim.Read;
            set uim.KeepUnread (not r);
            (* устанавливается после изменений пользователя,
             а не по-умолчанию для непрочитанного сообщения *)
            let val uc = if r then 1 else -1 in
                return (uc, if isResult uim.Mi then uc else 0, uc)
            end
        fun processRead mark act (UIM uim) =
            c <- mark (UIM uim);
            item <- get currentFeed;
            (* TODO: ничего не делать в поиске? *)
            let val (up, rc, uc) = c
                val mid = uim.Mi.MsgId
                val bfu = mid.MsgKey.BlogFeedUrl
                fun tryCur cf =
                    case cf.SIType of
                      | SITSearch _ =>
                        ss <- get currentSearchFeed;
                        tryCur ss
                      | SITFeed f =>
                        if f.Subscription.Url = bfu then
                            return (Some cf)
                        else
                            return (getSubItemByUrl bfu)
                      | _ => return (getSubItemByUrl bfu)
            in
            List.app (fn (UIForest f) =>
                         modify f.ResultsCount (fn c => c-rc);
                         modify f.UnreadCount (fn c => c-uc)
                     ) uim.Parents;
            Js.markChangedReadCounters bfu;
            si <- tryCur item;
            case si : option subItem of
                None => return ()
              | Some si =>
                (case mid.CommentId of
                  | None =>
                     updateCounters si (-up) (up-uc)
                   | _ =>
                     updateCounters si 0 (-uc));
                c <- get si.Counters;
                Js.forceImpure (backgroundRpc.AddAction
                                    (act mid c.TotalComments uc))
                (* после того, как сделал управление с клавы, опять
                   перестал вызываться updateSubInfo,
                   после перестановки местами заработал updateSubInfo,
                   но не работает backgroundRpc.AddAction при skipC=False,
                   и даже без skipC (с отдельными ф-ями) все равно не работает.
                   TODO: какая-то проблема с оптимизатором, он выкидывает
                   вызовы updateSubInfo или addaction.
                   вылечилось деланием toggleMsgRead "рекурсивной".
                   Сделал в mono_reduce impure всегда true, не помогло.
                 *)
            end
        val toggleRead =
            processRead toggleMsgRead
                (fn mid tc uc => BGMarkMsgRead
                                      { MsgId = mid, Read = uc=1
                                      , TotalComments = tc })
        fun addMsgTag t (UIM uim) =
            Js.forceImpure (backgroundRpc.AddAction
                                (BGAddTag { MsgId = uim.Mi.MsgId, Tag = t }))
        fun removeMsgTag t (UIM uim) =
            Js.forceImpure (backgroundRpc.AddAction
                                (BGRemoveTag { MsgId = uim.Mi.MsgId, Tag = t }))
        fun toggleStarred (UIM uim) =
            s <- get uim.Starred;
            set uim.Starred (not s);
            (if s then
                 removeMsgTag ITStarred (UIM uim)
             else
                 addMsgTag ITStarred (UIM uim));
            updateTags
        fun addITTag t (UIM uim) =
            ts <- get uim.Tags;
            when (not (elem t ts || t = ""))
                 (addMsgTag (ITTag { TagName = t }) (UIM uim);
                  set uim.Tags (List.append ts (t :: [])));
            updateTags
        fun removeITTag t (UIM uim) =
            ts <- get uim.Tags;
            set uim.Tags (List.filter (fn tn => tn <> t) ts);
            removeMsgTag (ITTag { TagName = t }) (UIM uim);
            updateTags
        fun replaceITTags ts' (UIM uim) =
            ts <- get uim.Tags;
            set uim.Tags ts';
            List.app
                (fn t => removeMsgTag (ITTag { TagName = t }) (UIM uim)) ts;
            List.app
                (fn t => addMsgTag (ITTag { TagName = t }) (UIM uim)) ts';
            updateTags
        fun editTags (UIM uim) =
            ts <- get uim.Tags;
            text <- source (intercalate ", " ts);
            tid <- fresh;
            let val edit =
                    tl <- Js.getTagsList tid;
                    when (tl.1)
                         (po.Hide;
                          replaceITTags tl.2 (UIM uim))
            in
            d <- po.NewMenu Css.tagsDialog <xml>
              Enter tags separated by comma<br/>
                <ctextbox id={tid}
                  class="tagsInput" source={text} size={30}
                  onkeydown={fn k =>
                                a <- Js.isAutocompleteActive tid;
                                if k.KeyCode = 13 && not a then
                                    stopPropagation;
                                    (* а то list view раскрывает *)
                                    edit else
(*                                 if k.KeyCode = 27 then po.Hide else *)
                                return()}
                  />{textButton "OK" edit}</xml>;
            po.Toggle popup d;
            Js.select tid;
            Js.focus tid;
            Js.setupTagAutocomplete tid
            end
        fun addTag (UIM uim) =
            tid <- fresh;
            text <- source "";
            let val new =
                    t <- get text;
                    tn <- Js.checkTagName t;
                    case tn of
                      | Some t =>
                        ts <- get uim.Tags;
                        if t = "" then
                            alert "Tag name can't be empty."
                        else
                            po.Hide;
                            addITTag t (UIM uim)
                      | None =>
                        return ()
            in
            ts <- get uim.Tags;
            usedTags <- Js.getUsedTags;
            po.Toggle sharePopup
                    (po.NewIdPosMenu uim.TagId
                     (Js.offsetBottomRight uim.TagId) Css.tagsMenu <xml>
              {List.mapX
               (fn t =>
                    if elem t ts then
                        po.LiI Css.btnCheck t (removeITTag t (UIM uim))
                    else
                        po.LiI Css.btnEmpty t (addITTag t (UIM uim)))
               usedTags}
              <div class={Css.newFolder}>
              New tag<br/>
              <ctextbox class="tagsMenuInput" source={text} size={20}
              onkeydown={fn k => if k.KeyCode = 13
                                 then stopPropagation; new
(*                               else if k.KeyCode = 27 then po.Hide *)
                                 else return()}
              />{textButton "Add" new}
              </div>
            </xml>);
            Js.adjustMenuHeight Css.tagsMenu
            end
        fun removeAppendReqById rqacc id =
            let fun go acc ars = case ars of
                        [] => (rqacc, Js.reverse acc)
                      | (AppendReq ar) :: ars =>
                        if Js.eq_id ar.Id id then
                            (AppendReq ar :: rqacc, revAppend acc ars)
                        else go (AppendReq ar :: acc) ars
            in
                ars <- get appendRequests;
                case go [] ars of (r, ars') =>
                set appendRequests ars';
                return r
            end
        (* удаляет AppendReq-и из всех нижних поддеревьев *)
        fun removeUIMAppendReqs (UIM uim) rqacc  =
            case uim.SubForest of UIForest uif =>
            nri <- get uif.NextReqId;
            r <- (case nri of
                None => return rqacc
              | Some id =>
                set uif.NextReqId None;
                removeAppendReqById rqacc id);
            ch <- get uif.Children;
            List.foldlM removeUIMAppendReqs r ch
            (* TODO: ^ потенциально медленно *)
        fun restoreUIMAppendReqs (reqs : list appendReq) =
            modify appendRequests (revAppend reqs);
            List.app (fn (AppendReq { Id = id
                                    , Params = ForestParams
                                      { Forest =
                                        UIForest { NextReqId = nri, ...}, ... }
                                    , ...}) => set nri (Some id)) reqs
        fun markRead (UIM uim) =
            r <- get uim.Read;
            k <- get uim.KeepUnread;
            v <- get uim.Mv;
            if not r && not k then
                toggleRead (UIM uim)
            else return ()
        fun tryMarkRead (UIM uim) =
(*             case v of MVShort _ => return () | _ => *)
            if isResult uim.Mi then
                mrm <- get markReadMode;
                case mrm of
                  | MRMManual => return ()
                  | MRMOnScrollEverywhere =>
                    markRead (UIM uim)
                  | MRMOnScroll =>
                    vm <- viewModeByMsgKey getMsgTreeViewMode (uimMsgKey (UIM uim));
                    (* lvm <- get listViewMode; *)
                    (case (vm.Posts(* , lvm *)) of
                       | (PVMShort(* , LVMCompact *)) =>
                         return ()
                       | _ =>
                         markRead (UIM uim))
            else
                return ()
        val mw : mw = Js.mw ()
        fun toggleCollapsed a b =
            tc <- get toggleCollapsed_;
            tc a b
        fun tryToggleFull (UIM uim) =
            mv <- get uim.Mv;
            case mv of
              | MVShort _ =>
                toggleFull toggleCollapsed (return ()) mw (UIM uim)
              | _ => return ()
        fun selectS tryFull andMarkRead scroll (UIM uim) =
(*             debug "selectS"; *)
            compact <- isCompact (UIM uim) getMsgTreeViewMode listViewMode;
            s <- get uim.Selected;
            if s then
                when andMarkRead (markRead (UIM uim));
                when tryFull (tryToggleFull (UIM uim))
            else
            (let val sel =
(*                      debug "sel"; *)
                     (if andMarkRead
                      (* если пользователь щелкнул по сообщению *)
                      then markRead (UIM uim) else tryMarkRead (UIM uim));
                     old <- get selectedUIM;
                     (case old of
                        | Some (UIM u) => set u.Selected False
                        | None => return ());
                     set uim.Selected True;
                     set selectedUIM (Some (UIM uim))
                 fun scrollMore ro n (UIM uim) =
                     if n = 0 then selectS tryFull False scroll (UIM uim) else
                     if n < 0 then
                         p <- prevMsg ro (UIM uim);
                         case p of
                           | Some p =>
                             tryMarkRead (UIM uim);
                             scrollMore ro (n+1) p
                           | None => selectS tryFull False scroll (UIM uim)
                     else
                         nm <- nextMsg ro (UIM uim);
                         case nm of
                           | Some nm =>
                             tryMarkRead (UIM uim);
                             scrollMore ro (n-1) nm
                           | None => selectS tryFull False scroll (UIM uim)
             in
                 if compact then
                     sel;
                     when tryFull (tryToggleFull (UIM uim));
                     when scroll (fitCompact lastScrollPos msgDivId (UIM uim))
                 else if scroll then
                     (modify scrolling succ;
                      ot <- Js.offsetTop uim.SnapId;
                      set lastScrollPos ot;
                      set scrollAfter (Some (0, False));
                      sm <- getScrollMode;
                      Js.scrollToElement uim.SnapId sm
                          (sel;
                           sa <- get scrollAfter;
                           set scrollAfter None;
                           modify scrolling pred;
                           case sa of
                             | Some (0,_) => return ()
                             | None => return ()
                             | Some (n,ro) => scrollMore ro (min 10 (max (-10) n)) (UIM uim)
                          ))
                 else
                     sel
             end)
        val select = selectS False False True
        fun whenNotScrolling act =
            sa <- get scrollAfter;
            (case sa of
              | Some _ => return ()
              | None => act)
        fun scrollOrAccum resultsOnly x act =
            sa <- get scrollAfter;
            case sa of
              | Some (n,_) => set scrollAfter (Some (n+x, resultsOnly))
              | None => act
        fun trySelectOr ro tryFull f other =
            uimo <- get selectedUIM;
            m' <-
              (case uimo of
                | None => uif <- get uiForest; findFirst ro uif
                | Some m => f ro m);
            case m' of
              | Some m => selectS tryFull False True m
              | None => other
        fun trySelect ro tryFull f = trySelectOr ro tryFull f (return ())
        fun withSelected f =
            uimo <- get selectedUIM;
            withSome f uimo
        fun authorStyle mv =
            (* по хорошему бы этих авторов параметром/результатом fs прогонять *)
            let val author =
                    case mv of
                      | MVFull m => m.Msg.Author
                      | MVShort s => s.Header.Author
            in
                if author = "" then return Css.authorUnknown else
                a <- get authors;
                case a of (s, rs, as) =>
                (case lookupS author as of
                  | Some s => return s
                  | None =>
                    let fun getS s rs =
                            case (s,rs) of
                              | ([], []) => (Css.authorUnknown, [], [])
                              (* ??? *)
                              | (st :: s, rs) => (st, s, st :: rs)
                              | ([], rs) => getS (Js.reverse rs) []
                        val (st, s, rs) = getS s rs
                    in
                        set authors (s, rs, (author, st) :: as);
                        return st
                    end)
            end
        fun setLoadCheckTimeout imgLoadCheck =
            toCheck <- get imgLoadCheck;
            when (notNull toCheck)
                 (Js.setTimeout "setLoadCheckTimeout"
                                (List.app (fn (picId, loaded) =>
                                              c <- Js.complete picId;
                                              when c (set loaded True))
                                          toCheck) 0)
        val topAppendRequest =
            arsOrig <- get appendRequests;
            offsArs <- List.mapM (fn (AppendReq ar) => Js.offsetTop ar.Id)
                                 arsOrig;
            let fun min m l = case l of [] => m
                                      | x::xs => min (if x < m then x else m) xs
            in
                return (case offsArs of [] => None | m::ms => Some (min m ms))
            end
(*         val getNext = *)
(*             uimo <- get selectedUIM; *)
(*             case uimo of *)
(*                 | None => uif <- get uiForest; findFirst uif *)
(*                 | Some m => nextMsg m *)
        fun uimTop (UIM uim) = Js.offsetTop uim.SnapId
        fun findScroll st (h:int) m =
            top <- uimTop m;
            let fun sel m = selectS False False False m
            in
                if top = st then sel m
                else if top > st then (* стоит посмотреть, есть ли что выше *)
                    (p <- prevMsg False m;
                     case p of
                       | None => sel m
                       | Some pm =>
                         pt <- uimTop pm;
                         if pt >= st then
                             (tryMarkRead m; findScroll st h pm)
                         else if top >= st+h/2 then
                             (tryMarkRead m; sel pm) else sel m)
                else (* смотрим ниже *)
                    (n <- nextMsg False m;
                     case n of
                       | None => sel m
                       | Some nm =>
                         ntop <- uimTop nm;
                         if ntop < st then
                             (tryMarkRead m; findScroll st h nm)
                         else if ntop >= st+h/2 then sel m else
                             (tryMarkRead m; sel nm))
            end
        val onScroll =
            st <- Js.scrollTop msgDivId;
            lsp <- get lastScrollPos;
            if st = lsp then return () else
            (* дабы не выделяло первое сообщение при выборе фида *)
            set lastScrollPos st;
            h <- Js.clientHeight msgDivId;
            uimo <- get selectedUIM;
            case uimo of
              | None => uif <- get uiForest; m <- findFirst False uif;
                withSome (findScroll st h) m
              | Some m => findScroll st h m
        fun fs imgLoadCheck (ForestParams p) (MsgForest mf) =
            let val (UIForest f) = p.Forest
                val parents' = p.Forest :: p.Parents
                fun showFeedTitlesT f =
                    case f.SIType of
                      | SITAll => return True
                      | SITFolder _ => return True
                      | SITFeed _ => return False
                      | SITStarred => return True
                      | SITAllTags => return True
                      | SITTag _ => return True
                      | SITSearch _ =>
                        sf <- get currentSearchFeed;
                        showFeedTitlesT sf
            in
            cf <- get currentFeed;
            showFeedTitles <- showFeedTitlesT cf;
            uc <- get ultraCompact;
(*             mtvm <- getMsgTreeViewMode; *)
            x <- List.mapXM (fn (mi, subForest) =>
                           read <- source mi.Read;
                           starred <- source mi.Starred;
                           tags <- source mi.Tags;
                           keepUnread <- source False;
                           selected <- source False;
                           collapsed <- source Expanded;
                           growId <- fresh;
                           frameId <- fresh;
                           tagId <- fresh;
                           loadingChildren <- source False;
                           authorStyle <- authorStyle mi.MsgView;
                           sf <- mkUIForest subForest;
                           prev <- get f.Children;
                           next <- source None;
                           mv <- source mi.MsgView;
                           rv <- source (RVNone None);
                           st <- source True;
                           id <- fresh;
                           when (not (Js.setuim id
                                  (UIM
                                   { Mi = mi
                                   , Starred = starred
                                   , Tags = tags
                                   , Mv = mv
                                   , ReadabilityView = rv
                                   , ShowText = st
                                   , Depth = p.Depth
                                   , SnapId = id
                                   , GrowId = growId
                                   , FrameId = frameId
                                   , TagId = tagId
                                   , LoadingChildren = loadingChildren
                                   , Read = read
                                   , KeepUnread = keepUnread
                                   , Selected = selected
                                   , Collapsed = collapsed
                                   , SubForest = sf
                                   , Parents = parents'
                                   , Parent = p.ParentUIM
                                   , Prev = prev
                                   , Next = next
                                   }))) (alert "setuim?");
                           let val uim : uim = Js.uim id
                           in
                               (case prev of
                                  | (UIM p) :: _ =>
                                    set p.Next (Some uim)
                                  | _ => return ());
                               set f.Children (uim :: prev);
                               fc <- get f.FirstChild;
                               (case fc of
                                  | None => set f.FirstChild (Some uim)
                                  | _ => return ());
                               ch <- fs imgLoadCheck
                                        (ForestParams
                                         { Depth = p.Depth+1
                                         , Parents = parents'
                                         , ParentUIM = Some uim
                                         , Forest = sf }) subForest;
                               x <- msgNodeNew uc imgLoadCheck ch showFeedTitles authorStyle toggleCollapsed (selectS False) mw uim;
                               if p.Depth > 0 then
                                   return x
                               else
                                   vm <- viewModeByMsgKey getMsgTreeViewMode (uimMsgKey uim);
                                   lpvm <- get lastPostsViewMode;
                                   let val pvm = vm.Posts
                                   in
                                       set lastPostsViewMode pvm;
                                       if pvm <> lpvm &&
                                          (pvm = PVMMosaic || pvm = PVMMagazine)
                                       then
                                           return <xml>
                                             <div class="viewModeSeparator">
                                             </div>
                                             <span class="noBorderTop">{x}</span></xml>
                                       else
                                           return x
                                   end
                           end
                       )
                      mf.List;
            case mf.NextReq of
              | None =>
                set f.NextReqId None;
                return x
              | Some rq =>
                id <- fresh;
                tail <- source <xml><span class="insertPoint"
                                          id={id} (* положение *)/></xml>;
                set f.NextReqId (Some id);
                let val areq =
                        AppendReq { Id = id, Params = ForestParams p,
                                    TreeReq = rq, InsertPoint = tail }
                in
                    (case (rq, p.ParentUIM) of
                       | ( TRComments { OnExpand = True, ... }
                         , Some (UIM uim) ) =>
                         set uim.Collapsed (Collapsed (areq :: []))
                       | _ =>
                         modify appendRequests (cons areq));
                return <xml>{x}{dyn_ (signal tail)}</xml>
                end
            end
        and append afterAppend onAppend =
            a <- get appending;
            withSome (set afterAppendAction) afterAppend;
            if a then
                onAppend False
            else
            arsOrig <- get appendRequests;
            let fun depth (ForestParams fp) = fp.Depth
                val offsArs =
                    List.mp
                        (fn (AppendReq ar) =>
                            ((Js.offsetTopLeft ar.Id).Top, depth ar.Params, AppendReq ar))
                            arsOrig
                val offsArsSorted =
                    List.sort (fn (oa,da,_) (ob,db,_) =>
                                  oa > ob || (oa = ob && da <= db))
                              (* начиная от max depth, Left одинаковый *)
                              offsArs
                val ars = List.mp (fn a => a.3) offsArsSorted
            in
(*             debug ("L: " ^ intercalate "," (List.mp (fn (o,d,_) => "(" ^ show o ^ "," ^ show d ^ ")") offsArsSorted)); *)
(*             debug ("append " ^ show (List.length arsOrig) ^ " ars=" *)
(*                     ^ show (List.length ars)); *)
            if isNull ars then
                onAppend False; set afterAppendAction (return ())
            else
            set appending True;
            set appendRequests [];
            queueRpcB backgroundRpc
                (fn l =>
                    set appendingRpc True;
                    mtvm <- getMsgTreeViewMode;
                    du <- current (discoverySubItemUrl currentFeed);
                    let val arg = (List.mp (fn (AppendReq a) => a.TreeReq) ars)
                    in
                        case du of
                          | Some u => rpc (getTreeD u mtvm arg l)
                          | _ => rpc (getTree mtvm arg l)
                    end)
                (fn mfs =>
                    imgLoadCheck <- source [];
                    let fun go mfs ars =
                            case (mfs,ars) of
                              | ((Some mf)::mfs, ((AppendReq ar)::ars)) =>
                                toAppend <- fs imgLoadCheck ar.Params mf;
                                set ar.InsertPoint toAppend;
                                go mfs ars
                              | (None::mfs, ars) =>
                                modify appendRequests (revAppend ars)
                                (* без изменений *)
                              | _ => return ()
                    in
                        withScrollSaved msgDivId (go mfs ars);
                        (* ^ в firefox иногда при append-ах
                           scroll улетает вверх
                           на тормоза вроде не влияет
                         *)
                        setLoadCheckTimeout imgLoadCheck;
(*                         withSome id afterAppend; *)
                        aa <- get afterAppendAction;
                        set afterAppendAction (return ());
                        aa;
                        set appendingRpc False;
                        Js.setTimeout "onAppend"
                                      (set appending False;
                                       onAppend True)
                                      200
                        (* ограничиваем частоту запросов *)
                    end)
            end
        and checkAppend limit callOnScroll after =
(*             appending <- get msgsWidget.Appending; *)
(*             if appending then after False else *)
            if limit <= 0 then after False else
            top <- topAppendRequest;
            case top of
              | None => after False (* нечего добавлять *)
              | Some top =>
(*                 debug ("topAppendRequest: " ^ show top ^ "px"); *)
(*                 n <- msgsWidget.GetNext; *)
(*                 (case n of *)
(*                    | None => msgsWidget.Append after *)
(*                    | Some (UIM next) => *)
                     st <- Js.scrollTop msgDivId;
                     h <- Js.clientHeight msgDivId;
(*                 top <- Js.clientHeight innerMessagesId; *)
                     if st + 3*h > top
(*                         || Js.offsetTop next.SnapId >= top *)
                     then
                        (when callOnScroll onScroll;
                         append None
                             (fn a =>
                                 if a then
                                     checkAppend (limit - 1) callOnScroll after
                                 else
                                     after False))
                     else after False
        and toggleCollapsedReal after (UIM uim) = whenNotScrolling (
            c <- get uim.Collapsed;
            mv <- get uim.Mv;
            mtvm <- uimViewMode getMsgTreeViewMode (UIM uim);
            case (mtvm.Posts, mv, c) of
              | (PVMMosaic, MVShort _, Collapsed _) => after
              | _ => (
            set scrollAfter (Some (0, False));
            compact <- isCompact (UIM uim) getMsgTreeViewMode listViewMode;
            case c of
              | Expanded =>
                ars <- removeUIMAppendReqs (UIM uim) [];
                sm <- getScrollMode;
                Js.collapseComments 0 uim.GrowId
                    (if compact then "immediate" else sm)
                    (set uim.Collapsed (Collapsed ars);
                     set scrollAfter None;
                     after;
                     checkAppend 1 False (fn _ => return ()))
              | Collapsed ars =>
                ch <- (case uim.SubForest of UIForest f => get f.Children);
                restoreUIMAppendReqs ars;
(*                 tryMarkRead (UIM uim); *)
                let val rollOut =
                        set uim.Collapsed Expanded;
                        sm <- getScrollMode;
                        Js.expandComments 20 uim.GrowId
                                          (if compact then "immediate" else sm)
                                          (set scrollAfter None;
                                           after;
                                           checkAppend 1 False
                                                       (fn _ => return ()))
                in
                case (ars, ch) of
                  | ( (AppendReq
                       { TreeReq = TRComments { OnExpand = True, ... }
                       , InsertPoint = ip, ... }) :: []
                    , [] ) =>
                    set loadingComments True;
                    set uim.LoadingChildren True;
                    checkAppend 1 False
                        (fn _ =>
                            set loadingComments False;
                            set uim.LoadingChildren False;
                            rollOut)
                  | _ =>
                    rollOut
                end))
        fun trySelectNext ro tryFull =
            uimo <- get selectedUIM;
            case uimo of
              | None =>
                uif <- get uiForest; m <- findFirst ro uif;
                withSome (selectS tryFull False True) m
              | Some m =>
                n <- nextMsg ro m;
                (case n of
                   | None =>
                     append (Some (trySelectNext ro tryFull))
                            (fn a => checkAppend 2 False (fn _ => return ()))
                   | Some m =>
                     modify scrolling pred;
                     (* ^ чтобы onscroll все-таки отработал и добавил сообщения
                        после skip *)
                     selectS tryFull False True m;
                     modify scrolling succ)
        fun withSelectedPost f =
            withSelected (fn s => let val p = postMsg s in
                                      selectS False False False p; f p end)
        fun selectFirstOr f =
            uimo <- get selectedUIM;
            case uimo of
              | None => trySelectNext False True
              | Some _ => f
        fun collapseAndTrySelectNext (UIM uim) =
            sm <- getScrollMode;
            Js.collapseComments 0 uim.GrowId sm
            (
            set uim.Collapsed (Collapsed []);
            trySelectNext False True;
            st <- Js.scrollTop msgDivId;
            set lastScrollPos st (* дабы избежать лишнего выделения сообщения *))
        fun skipComments (UIM uim) =
            x <- removeUIMAppendReqs (UIM uim) []; (* убираем с концами *)
            processRead skipMsgComments
                (fn mid tc uc =>
                    BGSkipComments { MsgId = mid, TotalComments = tc })
                (UIM uim);
            collapseAndTrySelectNext (UIM uim)
        fun ignorePost (UIM uim) =
            (* пока обрабатываем также, как и skip *)
            x <- removeUIMAppendReqs (UIM uim) []; (* убираем с концами *)
            processRead skipMsgComments
                (fn mid tc uc =>
                    BGIgnorePost { MsgId = mid, TotalComments = tc })
                (UIM uim);
            collapseAndTrySelectNext (UIM uim)
        fun later (UIM s) =
            r <- get s.Read;
            when (not r) (toggleRead (UIM s));
            (* чтобы точно галочка keep unread появилась *)
            toggleRead (UIM s);
            c <- get s.Collapsed;
            case c of Collapsed _ => trySelectNext False True
                    | Expanded => toggleCollapsed (trySelectNext False True) (UIM s)
        val pageUp = moveUpDown False (fn ch => 30-ch)
        val pageDown = moveUpDown False (fn ch => ch-30)
        val feedKBA = feedKeyboardActionCF currentFeed currentSearchFeed
        val prevUnreadFeed = feedKBA "prevUnreadFeed"
        val nextUnreadFeed = feedKBA "nextUnreadFeed"
        val loadingComplete =
            l <- get loading;
            a <- get appending;
            sa <- get scrollAfter;
            ars <- get appendRequests;
            return (not l && not a && Option.isNone sa && isNull ars)
        val nextOrPageDown =
            uimo <- get selectedUIM;
            st <- Js.scrollTop msgDivId;
            ch <- Js.clientHeight msgDivId;
            sh <- Js.scrollHeight msgDivId;
            lc <- loadingComplete;
            if st + ch >= sh then
                when lc nextUnreadFeed
            else
            case uimo of
              | None => trySelectNext False True
              | Some (UIM uim) =>
                ot <- Js.offsetTop uim.FrameId;
                chf <- Js.clientHeight uim.FrameId;
                if ot + chf > st + ch then
                    (* прокручиваем текущее сообщение,
                       если оно полностью не видно *)
                    set lastScrollPos (st+ch-30);
                    (* для отключения авто выделения *)
                    pageDown
(*                     moveUpDown False *)
(*                                (fn ch => min (ch-30) *)
(*                                              (ot + chf - (st+ch) + ch/2 - 1)) *)
                else
                    n <- nextMsg False (UIM uim);
                    (if Option.isNone n && lc then
                         nextUnreadFeed
(*                          pageDown *)
                     (* нет следующего и ничего не добавляется
                        и нельзя добавить, идем вниз, чтобы на следующем пробеле
                        перейти на следующий фид
                      *)
                     else
                         trySelectNext False True)
        val prevOrPageUp =
            uimo <- get selectedUIM;
            l <- get loading;
            case uimo of
              | None => when (not l) prevUnreadFeed
              | Some (UIM uim) =>
                ot <- Js.offsetTop uim.SnapId;
                st <- Js.scrollTop msgDivId;
                ch <- Js.clientHeight msgDivId;
                pm <- prevMsg False (UIM uim);
                if st = 0 && Option.isNone pm then
                    when (not l) prevUnreadFeed
                else if ot < st && ot > st - ch then
                    moveUpDown False (fn ch => ot - st)
(*                     pageUp (\* текущее сообщение не полностью видно сверху *\) *)
                else
                    (case pm of
                       | None => pageUp
                       | Some (UIM p) =>
                         otp <- Js.offsetTop p.SnapId;
                         if otp < st - ch then
                             pageUp (* предыдущее сообщение дальше чем экран *)
                         else
                             trySelect False True prevMsg)
        fun nextOrNextFeed ro act =
            uimo <- get selectedUIM;
            lc <- loadingComplete;
            next <- (case uimo of
              | None =>
                uif <- get uiForest;
                findFirst ro uif
              | Some uim => nextMsg ro uim);
            if Option.isNone next && lc then
                nextUnreadFeed
            else
                act
        fun prevOrPrevFeed ro act =
            uimo <- get selectedUIM;
            l <- get loading;
            prev <- (case uimo of
              | None => return None
              | Some uim => prevMsg ro uim);
            if Option.isNone prev && not l then
                prevUnreadFeed
            else
                act
        val scrollToMsgTop =
            uimo <- get selectedUIM;
            case uimo of
              | None => return ()
              | Some (UIM uim) =>
                ot <- Js.offsetTop uim.SnapId;
                st <- Js.scrollTop msgDivId;
                when (ot < st)
                     (moveUpDown False (fn ch => ot - st))
        fun toggleFullText (UIM uim) =
(*             if uim.Depth > 0 then return () *)
(*             else *)
            mv <- get uim.Mv;
            (case mv of
              | MVShort _ =>
                toggleFull toggleCollapsed (return ()) mw (UIM uim)
              | _ => return ()
(*                 Js.scrollToElement uim.SnapId (return ()) *)
                (* а toggle full и так будет скролить *)
            );
            let val scrollIfNeeded =
                    case mv of
                      | MVFull _ =>
                        sm <- getScrollMode;
                        Js.scrollToElement uim.SnapId sm (return ())
                        (* ^ как при разворачивании MVShort *)
                      | _ => return ()
            in
            rv <- get uim.ReadabilityView;
            case (rv, mv) of
              | (RVLoading, _) =>
                return () (* и так уже грузится *)
              | (RVReadability _, MVShort _) =>
                return () (* при разворачивании оставляем readability *)
              | (RVError _, MVShort _) =>
                return () (* при разворачивании оставляем readability *)
              | (RVReadability x, MVFull _) =>
                snapToTop uim; (* как при сворачивании *)
                set uim.ReadabilityView (RVNone (Some x));
                set uim.ShowText True
              | (RVError _, MVFull _) =>
                snapToTop uim;
                set uim.ReadabilityView (RVNone None)
              | (RVNone (Some x), MVShort _) =>
                set uim.ShowText False;
                set uim.ReadabilityView (RVReadability x)
              | (RVNone (Some x), MVFull _) =>
                scrollIfNeeded;
                set uim.ShowText False;
                set uim.ReadabilityView (RVReadability x)
              | (RVNone None, _) =>
                set uim.ReadabilityView RVLoading;
                (* не Cancelable, а то можно несколько раз нажать,
                 пока запрос идет и обломиться
                 TODO: нельзя позволять более одного запроса
                 *)
                r <- rpc (readability (uimMsgKey (UIM uim)) []);
                scrollIfNeeded;
                (case r of
                  | Left e =>
                    set uim.ReadabilityView (RVError e)
                  | Right t =>
                    set uim.ShowText False;
                    set uim.ReadabilityView
                        (RVReadability <xml>
                          <div class="readabilityMode">
                            {textButton "Close"
                                        (toggleFullText (UIM uim))}
                          </div>
                          {Js.preprocessMessageText t}
                        </xml>)
                           )
            end
    in
        when (not (Js.setmw
            { FullUIM = fullUIM
            , ListViewMode = listViewMode
            , MsgDivId = msgDivId
            , LastScrollPos = lastScrollPos
            , GetScrollMode = getScrollMode
            , ScrollAfter = scrollAfter
            , ToggleRead = toggleRead
            , BackgroundRpc = backgroundRpc
            , GetMsgTreeViewMode = getMsgTreeViewMode
            , ToggleStarred = toggleStarred
            , AddTag = addTag
            , RemoveITTag = removeITTag
            , MarkRead = markRead
            , TryMarkRead = tryMarkRead
            , Po = po
            , SharePopup = sharePopup
(*             , Select = selectS False *)
(*             , ToggleCollapsed = toggleCollapsed *)
            }
             )) (alert "setmw?");
        html <- source <xml/>;
        set toggleCollapsed_ toggleCollapsedReal;
        return
        { Html = <xml><span
          dynClass={sm <- signal scrollMode;
                    return (case sm of
                              | SMImmediate => null
                              | _ => smoothScrolling)}
          ><dyn signal={signal html}/></span></xml>
        , OnScroll = onScroll
        , Scrolling = scrolling
        , Appending = appending
        , LoadingComments = loadingComments
        , AppendingRpc = appendingRpc
(*         , GetNext = getNext *)
        , TopAppendRequest = topAppendRequest
        , AppendRequests = appendRequests
        , CheckAppend = fn after => checkAppend 3 True after
        , AfterAppendAction = afterAppendAction
        , SetForest = fn mf =>
            set lastPostsViewMode PVMFull;
            set lastScrollPos 0;
            set scrollAfter None;
            set appending False;
            set appendRequests [];
            set authors (Css.authorStyles, [], []);
            set selectedUIM None;
            set fullUIM None;
            when (not (Js.clearuims ())) (alert "clearuims?");
            f <- mkUIForest mf;
            set uiForest f;
            imgLoadCheck <- source [];
            x <- fs imgLoadCheck
                    (ForestParams
                     { Depth = 0, Parents = [], ParentUIM = None, Forest = f })
                    mf;
            setLoadCheckTimeout imgLoadCheck;
            set html x;
            checkAppend 3 False (fn _ => return ())
        , IsForestEmpty =
            uif <- signal uiForest;
            fc <- signal (case uif of UIForest f => f.FirstChild);
            return (Option.isNone fc)
        , SelectedUIM = selectedUIM
        , ToggleRead = whenNotScrolling (withSelected mw.ToggleRead)
        , ToggleStarred = whenNotScrolling (withSelected mw.ToggleStarred)
        , EditTags = whenNotScrolling (withSelected editTags)
        , SkipComments = whenNotScrolling (selectFirstOr (withSelected skipComments))
        , SkipPost = whenNotScrolling (selectFirstOr (withSelectedPost skipComments))
        , PageUp = whenNotScrolling pageUp
        , PageDown = whenNotScrolling pageDown
        , LineUp = whenNotScrolling (moveUpDown True (fn _ => -40))
        , LineDown = whenNotScrolling (moveUpDown True (fn _ => 40))
        , Next = scrollOrAccum True 1 (nextOrNextFeed True (trySelectNext True False))
        , Prev = scrollOrAccum True (-1) (prevOrPrevFeed True (trySelect True False prevMsg))
        , NextTryFull = scrollOrAccum False 1 (nextOrNextFeed False (trySelectNext False True))
        , PrevTryFull = scrollOrAccum False (-1) (prevOrPrevFeed False (trySelect False True prevMsg))
        , NextOrPageDown = scrollOrAccum False 1 nextOrPageDown
        , PrevOrPageUp = scrollOrAccum False (-1) prevOrPageUp
        , Up = whenNotScrolling (selectFirstOr (trySelectOr False True (fn _ => parentMsg) scrollToMsgTop))
        , Later = whenNotScrolling (selectFirstOr (withSelected later))
        , LaterPost = whenNotScrolling (selectFirstOr (withSelectedPost later))
        , IgnorePost = whenNotScrolling (selectFirstOr (withSelectedPost ignorePost))
        , ToggleCollapsed = whenNotScrolling (withSelected (toggleCollapsed (return ())))
        , ToggleFull = whenNotScrolling (withSelected
                           (fn s => toggleFull toggleCollapsed (select s) mw s))
        , TryMakeShort =
          whenNotScrolling (withSelected
           (fn (UIM uim) =>
               mv <- get uim.Mv;
               case mv of
                 | MVFull _ =>
                   toggleFull toggleCollapsed (select (UIM uim)) mw (UIM uim)
                 | _ =>
                   trySelectOr False True (fn _ => parentMsg) scrollToMsgTop))
        , ToggleFullText =
          whenNotScrolling (withSelectedPost toggleFullText)
        , JumpToLink = fn openFunc => whenNotScrolling (withSelected
          (fn (UIM s) =>
              mv <- get s.Mv;
              case mv of
                | MVShort { CachedMsg = Some m, ... } =>
                  withSome (openFunc (UIM s)) m.Link
                | MVShort _ =>
                  toggleFull toggleCollapsed (return ()) mw (UIM s)
                | MVFull { Msg = m } =>
                  withSome (openFunc (UIM s)) m.Link))
        , JumpToTranslate = fn openFunc => whenNotScrolling (withSelected
          (fn uim =>
              r <- uimReadabilityMode uim;
              trackShareAction backgroundRpc SATranslate;
              openFunc uim (bless (translateLink r (uimMsgKey uim)))))
        , ScrollMode = scrollMode
        , ListViewMode = listViewMode
        , UltraCompact = ultraCompact
        , MarkReadMode = markReadMode
        }
    end

val discoveryTopics =
       "news"
    :: "tech"
    :: "comics"
    :: "apple"
    :: "photo"
    :: "music"
    :: "sports"
    :: "games"
    :: "science"
    :: "politics"
    :: "finance"
    :: "fun"
    :: "food"
    :: "cooking"
    :: "humor"
    :: "design"
(*     :: [] *)
(* val discoveryTopics2 = *)
    :: "podcasts"
    :: "programming"
    :: "business"
    :: "travel"
    :: "software"
    :: "mac"
    :: "android"
    :: "linux"
    :: "books"
    :: "entertainment"
    :: "health"
    :: "security"
    :: "economics"
    :: "art"
    :: "fashion"
    :: "fitness"
    :: "beauty"
    :: "productivity"
    :: "history"
    :: "philosophy"
    :: "education"
    :: []

fun discover country query =
    withUser "discover" (fn userId => userSearchSubscriptions userId country query) []
fun feedDetails url =
    withUser "feedDetails" (fn userId => getFeedDetails userId url) []

fun discoveryWidget addSub opmlUploadClick discoveryTextBoxId displayDiscovery ps popup =
    text <- source "";
    country <- source "RU";
    contents <- source <xml/>;
    showImport <- source True;
    cache <- source [];
    contentsId <- fresh;
    looksLikeUrl <- source False;
    lastQuery <- source " ";(* чтобы первый запрос отработал и показал топики *)
    nonEmptyText <- source False;
    mtvms <- source [];
    let fun modMtvm m =
            modify mtvms m;
            modify cache (List.mp (fn (k,(t,r)) =>
                                      (k,(t,case r of
                                              | Some (x,vms) => Some (x, m vms)
                                              | None => None))))
        fun setMtvm url mtvm =
            modMtvm (fn x => (url,mtvm) :: List.filter (fn (u,_) => u <> url) x)
        fun clearMtvm url =
            modMtvm (List.filter (fn (u,_) => u <> url))
        fun lookupMtvm url =
            ms <- get mtvms;
            return (Option.get (modifyF [#UnreadOnly] (const False)
                                        defaultMsgTreeViewMode)
                               (List.assoc url ms))
        val getQuery =
            v <- Js.inputValue discoveryTextBoxId;
            set nonEmptyText (v <> "");
            return (Js.cleanDiscoveryQuery v)
        fun lookupCache co query =
            ca <- get cache;
            time <- now;
            return (case List.assoc (co, query) ca of
              | Some (t, r) =>
                if diffInSeconds t time >= 3600 then None else Some r
              | _ => None)
        fun insertCache co query r =
            ca <- get cache;
            time <- now;
            set cache (((co, query), (time, r)) ::
                       List.take 14
                       (List.filter (fn (k,(t,_)) =>
                                        k <> (co, query) &&
                                        diffInSeconds t time < 3600) ca))
        val hide =
            stopPropagation; preventDefault;
            Js.blur discoveryTextBoxId;
            set displayDiscovery False;
            Js.discoveryClearSelection
        val selectCountry =
            tid <- fresh;
            d <- ps.NewMenu Css.selectSubscriptionDialog <xml>
              Select country for subscriptions search<br/>
                <ctextbox id={tid} class="selectSubscriptionInput" size={30}
                /></xml>;
            ps.Toggle popup d;
            Js.setupCountryAutocomplete tid
                (fn co =>
                    set country co;
                    ps.Hide;
                    handleBGActions (BGSetCountry { Country = co } :: []));
            Js.select tid;
            Js.focus tid
        fun setContents (c,ms) =
            set contents c;
            Js.setScrollTop contentsId 0;
            set mtvms ms
        fun topicsList topics =
            List.mapX (fn topic => <xml>
              <div onclick={fn e =>
(*                                    case e.Button of *)
(*                                      | Basis.Left => *)
                                       set text ("#" ^ topic);
                                       when (not (Js.hasOnscreenKeyboard ()))
                                            (* ^ на мобилах не выделяем *)
                                            (Js.select discoveryTextBoxId);
                                       search ()
(*                                      | _ => return () *)
                           }
                   class="discoveryTopic">{[topic]}</div></xml>)
              topics
        and default () = <xml>
          <div class={Css.discoveryTopics}>Topics</div>
            {topicsList discoveryTopics}
          <div class={Css.discoveryCountry}>Country</div>
          <div class={Css.discoveryCountryName}>
             {dyn_ (co <- signal country;
                    return (txt (Js.countryNameFromCountryCode co)))}
          </div>
          <div class={Css.discoveryCountryButton}>
             {textButton "Change" selectCountry}
          </div>
        </xml>
        and cleanSearch () =
            set text "";
            search ()
        and add () =
            u <- getQuery;
            addSub u;
            hide;
            cleanSearch ()
        and setSearchResult r =
            set showImport False;
            q <- getQuery;
            case r of
              | Some x =>
                setContents x
              | None =>
                setContents (<xml><div class="discoveryNotFound">
                 No feeds found.
                 {if Js.discoveryQueryLooksLikeUrl q then
                      <xml>Click "Add" or press Enter to find the feed on site.</xml>
                  else
                      <xml>Try enter the full feed or site URL.</xml>}
                 </div></xml>, [])
        and search () =
            query <- getQuery;
            set looksLikeUrl (Js.discoveryQueryLooksLikeUrl query);
            lq <- get lastQuery;
            when (query <> lq)
            (set lastQuery query;
             if query <> "" then
                co <- get country;
                cr <- lookupCache co query;
                case cr of
                  | Some x =>
                    setSearchResult x
                  | _ =>
                    setContents (<xml/>,[]);
                    set showImport False;
                    r <- tryRpc (discover co query);
                    (case r of
                       | Some x =>
                         insertCache co query x;
                         q' <- getQuery;
                         when (q' = query) (setSearchResult x)
                       | None =>
                         setContents (<xml/>,[]))
            else
                set showImport True;
                setContents (default (),[]))
        fun displayClass id c inner =
            <xml><div id={id} dynClass=
            {dd <- signal displayDiscovery;
             si <- signal showImport;
             return (classes c
                      (classes (if dd then Css.displayBlock else Css.displayNone)
                      (ifClass si Css.discoveryShowImport null)))}>
              {inner}
            </div></xml>
        val tryAdd =
            q <- getQuery;
            if Js.discoveryQueryLooksLikeUrl q then
                add ()
            else
                search ()
    in
    dummy <- fresh;
    Js.setTimeout "setOninput" (search (); Js.setOninput discoveryTextBoxId (search ())) 0;
    return { Hide = hide
           , Country = country
           , GetQuery = getQuery
           , GetCountry = get country
           , SetMtvm = setMtvm
           , ClearMtvm = clearMtvm
           , LookupMtvm = lookupMtvm
           , Html = <xml>
      {displayClass dummy Css.discoveryHeader <xml>
      <div class="discoveryHeaderInner">
      <div class="discoveryHeaderHint">Enter URL, title or #topic</div>
      <div dynClass={ne <- signal nonEmptyText;
                     return (ifClass ne
                                     Css.discoveryNonEmptyText
                                     Css.discoverySearchBox)}>
      <span dynClass={lu <- signal looksLikeUrl;
                      return (if lu then null else Css.discoveryAddDisabled)}>{
        textButton "Add" tryAdd
      }</span>
      <span class="discoverySearchInput">
      <div class="discoveryCleanButton"
           onclick={fn _ => cleanSearch ();
                    when (not (Js.hasOnscreenKeyboard ()))
                         (Js.focus discoveryTextBoxId)}>×</div>
      <ctextbox id={discoveryTextBoxId}
         class="feedUrlInput" source={text} size={30}
         onchange={search ()}
(*          onkeyup={fn k => search ()} *)
         onkeydown={fn k => if k.KeyCode = 13
                            then
                                Js.setTimeout "tryAdd" tryAdd 0
                            else if k.KeyCode = 27 then
                                hide
                            else
                                Js.setTimeout "discoverySearch" (search ()) 0
                   }
         /></span></div>
     {displayIf showImport (Js.opmlForm
          (textButton "Import OPML" opmlUploadClick))}
(*      {displayIfNot showImport *)
(*           (textButton "Back" (set text ""; search ()))} *)
     </div></xml>}
    {displayClass contentsId Css.discoveryContents
      (dyn_ (signal contents))}
    </xml> }
    end

cookie openIdURL : string

fun addUrlAndRedirect user url =
    h <- userSubscribe user url None [];
    redirect (bless ("/#" ^ h))

fun callback lt addUrl (qs : option queryString) : transaction page =
    case qs of
        None => error <xml>Empty query string for sign in callback</xml>
      | Some qs =>
        h <- getHost;
        u <- loginCallback lt h (effectfulUrl (callback lt addUrl)) (show qs);
(*         debug ("User authenticated " ^ u); *)
        httpRef <- getCookie referrer;
        withSome (fn _ => clearCookie referrer) httpRef;
        params <-
            Monad.mp (List.append
                          (("Who", Option.get "-" u.2) ::
                           ("Referrer", Option.get "-" httpRef) :: []))
                     (List.mapM
                          (fn n =>
                              p <- getHeader (blessRequestHeader n);
                              return (n, Option.get "-" p))
                          ("Country" :: "Region" :: "City"
                                     :: "IP" :: "User-Agent" :: []));
        s <- newSession u.1 params;
        sid <- sessionCookie;
        setCookie sid {Value = s.Key, Expires = Some s.Expire,
                       Secure = False};
        setCookie freshSignIn {Value = (), Expires = None, Secure = False};
        let fun add url =
                addUrlAndRedirect (case u.1 of EMail e => e.Id | Url u => u.Id)
                                  url
        in
            qs <- parseQueryStringUtf8Only (show qs);
            case (lt, List.assoc "state" qs) of
                (* Google не позволяет задавать произвольный return_url,
                   передаем url на подписку в параметре state
                 *)
              | (Google, Some url) =>
                if url = "" then redirectToMain else add url
              | _ =>
                (case addUrl of
                   | None => redirectToMain
                   | Some url => add url)
        end

fun sign_in lt addUrl : transaction page =
    h <- getHost;
    u <- loginGetForwardUrl lt h (case addUrl of Some u => u | None => "")
                            (effectfulUrl (callback lt addUrl));
    redirect u

fun sign_in_openid (addUrl : option string) h =
    setCookie openIdURL {Value = h.URL, Expires = None, Secure = False};
    debug ("OpenID URL: " ^ h.URL);
    sign_in (OpenId h) addUrl

fun sign_out _ =
    sid <- sessionCookie;
    s <- getCookie sid;
    (case s of
       | Some k =>
         sess <- cachedReadSession k;
         clearSession k;
         (case sess of
           | Some s =>
             logAction "sign_out" s.User (userEvent s.User "Sign out" "")
           | None => return ())
       | None => return ());
    clearCookie sid;
    redirectToMain

fun clearSubscriptionsHandler x =
    withUser "clearSubscriptionsHandler" (fn _ => return ())
             (BGClearAllSubscriptions :: []);
    redirectToMain

val clearSubscriptions : transaction page =
    infoPage "Clear subscriptions" <xml>
      <p>Are you really sure you want to clear all your subscriptions?</p>
      <p>This operation can NOT be undone!</p>
      <form>
        <submit value={"Clear subscriptions"}
                action={clearSubscriptionsHandler}/>
      </form>
      <br/>
    </xml>

fun deleteAccountHandler f =
    withUser "deleteAccuntHandler" (userDeleteAccount True) [];
    x <- getUser ""; (* чтобы cookie удалить *)
    infoPage "Account deleted" <xml>
      <p>Your account was deleted. You can come back any time and start a new free trial.</p>
      <br/>
    </xml>

val deleteAccount : transaction page =
    c <- fresh;
    infoPage "Delete account" <xml>
      <p>Are you really sure you want to delete your subscriptions, starred and tagged items, read states, payments, settings, public feeds and mobile login?</p>
      <p>This operation can NOT be undone!</p>
      <form>
(*         <label for={c}<checkbox{#ImSure} id={c}/>I'm really sure</label> *)
        <submit value={"Delete account"} action={deleteAccountHandler}/>
      </form>
      <br/>
    </xml>

val backToMain =
    <xml><a href={bless "/"}>Home</a><p/></xml>

fun landingPage title head content =
    ls <- htmlLandingScripts ();
    pageNoBody' "" head ("BazQux Reader" ^ title) <xml>
      <body class="landing noTouch">
        <div class={Css.landingPage}>
          <h1><a href={bless "/"}>{logo ""}</a></h1>
          {content}
        </div>
      </body>
      {ls}
    </xml>

    (* Взято из
     https://www.2checkout.com/blog/2checkout-blog/sample-privacy-policy-and-refund-policy/ *)
val privacyPolicyText : xbody =
    <xml>
      <p> (* This policy covers how we use your personal information. *)
      We take your privacy seriously at BazQux Reader and we protect your personal information.</p>
      <p>Any personal information received will only be used to fill your order.
      We will not sell or redistribute your information to anyone. In fact we do not even keep order information on our servers, only your order ID.</p>
      <p>The only information we know about you is your email address (when you sign in with Facebook or Google), your subscriptions list and a list of starred and tagged items.</p>
      <p class="signOutLink">At any moment you can export your feeds in OPML-format or <a link={deleteAccount}>opt out</a> from our service.</p>
    </xml>

fun privacy () =
    landingPage " Privacy Policy" <xml/> <xml>
      <h2>Privacy Policy</h2>
      {privacyPolicyText}
      {backToMain}
    </xml>

fun refund () =
    landingPage " Refund Policy" <xml/> <xml>
      <h2>Refund Policy</h2>
      {refundPolicyText}
      {backToMain}
    </xml>
fun qaX q (a : xbody) : xbody =
     <xml>
      (* <div class="qa">Q.</div> *)<div class="question">{[q]}</div>
      (* <div class="qa">A.</div> *)<div class="answer">{a}</div>
     </xml>

fun qa q a = qaX q (txt a)

fun feeddler f = f (txt "Feeddler") "http://www.chebinliu.com/projects/iphone/feeddler-rss-reader/" (* "https://itunes.apple.com/app/feeddler-rss-reader-pro/id365710282" *)
fun mrReader f = f  <xml>Mr.&nbsp;Reader</xml> "http://www.curioustimes.de/mrreader/" (* "https://itunes.apple.com/app/mr.-reader/id412874834" *)
fun justReader f = f (txt "JustReader") "http://justreader.net"(* "https://play.google.com/store/apps/details?id=ru.enacu.myreader" *)
fun newsPlus f = f (txt "News+") "http://newsplus.co"
fun viennaRss f = f <xml>Vienna&nbsp;RSS</xml> "http://www.vienna-rss.org"
fun slowFeeds f = f <xml>Slow&nbsp;Feeds</xml> "http://zoziapps.ch/slowfeeds/"
fun reeder f = f <xml>Reeder</xml> "http://reeder.ch/"
fun press f = f <xml>Press</xml> "http://twentyfivesquares.com/press/"
fun readKit f = f <xml>ReadKit</xml> "http://readkitapp.com"
fun amberRssReader f = f <xml>Amber RSS</xml> "https://play.google.com/store/apps/details?id=com.reindeercrafts.deerreader"
(* fun iRSSMac f = f <xml>iRSS</xml> "https://itunes.apple.com/us/app/irss/id795983486?ls=1&mt=12" *)

val subscribeBookmarklet = "javascript:{void(window.open('https://bazqux.com/add?url='+encodeURIComponent(location.href)));}"

fun faqText (link : xbody -> string -> xbody) =
    <xml>
      {qaX "Are there any mobile apps?"
          <xml>Try {mrReader link} on iPad, {feeddler link} or {slowFeeds link} on iPhone/iPad, {newsPlus link} (gReader) and {justReader link} on Android. BazQux Reader can be used as Fever server in {reeder link}, {press link} and {readKit link}.<br/>Tell developers of your favorite mobile app to support BazQux Reader! Its {link (txt "API") apiUrl} is a copy of Google Reader API so it's very simple to integrate it.</xml>}

      {qaX "How to import my feeds?"
           <xml>Read our knowledge base {link <xml>article</xml> "http://bazqux.uservoice.com/knowledgebase/articles/282171"}.</xml>}

      {qa "How do I log in from 3rd party client app?"
          "Go to Settings (icon in the top right corner) » Mobile login and set your login & password."}

(*       {qa "How do I hide comments?" *)
(*           "There are view mode buttons above the messages list. All except first are with comments collapsed. You can also press '2'."} *)

      {qa "How do I turn off smooth scrolling?"
          "Settings (icon in the top right corner) » Transitions » Immediate."}

      {qa "How to unsubscribe from feed?"
          "Select feed and choose Unsubscribe in the drop-down menu above the messages lists or use right-click menu."}

      {qa "How do I assign a feed to folder?"
          "Select the feed and choose Folders » Add in the drop-down menu above the messages lists. Or use right-click menu or drag and drop."}

      {qa "How long do you keep items unread?"
          "Unread items are kept forever. Although there is a limit of 500 items per feed."}

      {qaX "Is there a place to vote about new features?"
           <xml>Yes. Visit our {link (txt "UserVoice") "https://bazqux.uservoice.com/"}.</xml>}

      {qaX "Who are behind the BazQux Reader?"
           <xml>BazQux Reader is a {link (txt "one man") "https://plus.google.com/+VladimirShabanov/about"} project.</xml>}

      {qa "Will you add free accounts?"
          "No. I need the money to guarantee a continued service. I don't want to close down like the free Reader from Google did."}

      {qaX "Is there an URL to add subscription?"
       <xml>Yes, use https://bazqux.com/add?url=%s in the RSS Subscription Extension for Chrome, or use {link (txt "SubToMe") "https://www.subtome.com/"}. Or drag the {link (txt "Subscribe") subscribeBookmarklet} bookmarklet to your bookmarks bar.</xml>}

      {qa "Can I export my feeds?"
          "Yes, you can both upload and download OPML to add or backup your feeds."}

      {qaX "What does 'BazQux' mean?"
       <xml>Just {link (txt "nothing") "https://en.wikipedia.org/wiki/Metasyntactic_variable#English"}.</xml>}

      {qaX "How do you pronounce 'Qux'?"
       <xml>Something like "cooks". Although hackers often pronounce "qux" as "kwucks". Didn't know that when I picked the name ;)</xml>}

      {qa "Free trial length?"
          "30 days."}

      {qa "Pricing?"
          "You can choose any price from $9 to $29 annually."}

      {qa "What's the difference between prices?"
          "No difference. It's a way to donate more if you like the service."}

      {qa "How can I pay?"
          "Credit card or PayPal through our authorized reseller FastSpring.com."}

      {qaX "Are refunds possible?"
          <xml>Yes, we have a {link (txt "refund policy") (show (url (refund ())))}.</xml>}

      {qaX "Is it possible to delete account?"
          <xml>Yes, at any time you can {link (txt "delete") (show (url deleteAccount))} your account.</xml>}

(*       {qaX "Another RSS reader?" *)
(*        <xml>It allows you to read comments the same way as messages.<br/>It remembers what comments you read and display only new comments next time.<br/>It supports comments trees, user avatars and tags where available.<br/>It even allows you to read Facebook pages and Google+ blogs.<br/>We just don't know any other RSS feed reader with the same feature set.</xml>} *)

(*       {qa "Search?" *)
(*           "Yes. You can search in all or only new messages, text, subject, author, tags and even search for messages with images only. Subscription to searches is planned. Beware that search currently is at beta stage."} *)

(*       {qa "I'm signed in using Google and then logged in using Facebook and subscriptions disappear." *)
(*           "We use your Google/Facebook/Twitter identifier as user name (we don't know your email) so when you sign in with different vendors you are a different user for us."} *)

      {qa "Can I post comments directly from Reader?"
          "No. Posting is not implemented and is not planned in the near future."}

      {qa "Why not all posts are shown in some Facebook pages?"
          "Unfortunately, Facebook Graph API returns all posts only on public (official) pages, not the personal ones."}

      {qa "Is BazQux Reader really written in Haskell and Ur/Web?"
          "Yes. We love to use the best tools available."}

      {qaX "I have another question!"
          <xml>Don't hesitate to ask by email {link (txt "hello@bazqux.com") "mailto:hello@bazqux.com"}</xml>}
    </xml>

fun helpText addSub =
    let fun kb button text : xbody =
            <xml><span class="helpKbButton">{[button]}</span>{[text]}<br/></xml>
        fun l t u = hrefLinkStopPropagation (txt t) u
    in
    <xml>
      <p>Keyboard shortcuts:</p>
      <div class="kbShortcuts">
      {kb "j or n" "next item"}
      {kb "k or p" "previous item"}
      {kb "space" "next item or page"}
      {kb "<Shift> + space" "previous item or page"}
      {kb "u" "parent item or scroll to item top"}
      {kb "g or ; or ]" "get full text of post with Readability"}
      {kb "x" "ignore post (do not show new comments)"}
      {kb "i" "skip current post and all its comments"}
      {kb "<Shift> + i" "skip current comment and all its replies"}
      {kb "l" "skip current post and keep it unread"}
      {kb "<Shift> + l" "skip current comment and keep it unread"}
      {kb "s" "star/unstar item"}
      {kb "t" "edit item tags"}
      {kb "m" "mark item as read/unread"}
      {kb "<Shift> + a" "mark all as read"}
      {kb "o" "expand/collapse comments"}
      {kb "enter" "expand/collapse item"}
      {kb "escape" "collapse item"}
      {kb "v" "view original"}
      {kb "b" "open in background (Chrome/Safari only)"}
      {kb "<Shift> + v" "view translation"}
      {kb "<Shift> + b" "open translation in background (Chrome/Safari only)"}
      {kb "e" "mail article link (via default mail agent)"}
      {kb "a" "add subscription"}
      {kb "r" "refresh subscriptions"}
      {kb "<Shift> + j or n" "next subscription"}
      {kb "<Shift> + k or p" "previous subscription"}
      {kb "<Shift> + x" "expand folder"}
      {kb "<Shift> + u" "select parent folder"}
      {kb "d then a" "display latest items"}
      {kb "d then s" "display starred items"}
      {kb "d then d" "open subscription selector"}
      {kb "d then u" "open feed selector"}
      {kb "d then f" "open folder selector"}
      {kb "d then t" "open tag selector"}
      {kb "/" "search"}
      {kb "- or =" "change article font size (saved per browser)"}
      {kb "_ or +" "change reader font size (saved per browser)"}
      {kb "f" "fullscreen"}
      {kb "1" "expanded view"}
      {kb "2" "expanded view without comments"}
      {kb "3" "magazine view"}
      {kb "4" "mosaic view"}
      {kb "5" "list view"}
      {kb "6" "toggle compact/normal list view mode"}
      {kb "0" "default subscription view modes in starred and tagged items"}
      {kb "h" "help"}
      <p/>
      <p>Mouse hints:</p>
      <b>Right click</b> on subscription to show the context menu<br/>
      <b>Shift+Click</b> on header in list view to mark as read/unread<br/>
      <p/>
      <p>Filter hints:</p>
      <b>img:true</b> to show only messages with images<br/>
      <b>author:john</b> to filter specific author<br/>
      <b>subject:bazqux</b> for messages with subjects containing "bazqux"<br/>
      <b>tag:reader</b> if you need to filter by tag<br/>
      <b>comment:true</b> or <b>comment:false</b> to show comments or posts only<br/>
      <b>(bazqux OR rss) AND NOT google</b> if you need complex query<br/>
      <br/>
      <br/>
      <p>Resources:</p>
      {l "Blog" "http://blog.bazqux.com"}
      {textButton "subscribe" (addSub "http://blog.bazqux.com/feeds/posts/default")}<br/>
      {l "Twitter" "https://twitter.com/BazQuxReader"}
      {textButton "subscribe" (addSub "https://twitter.com/BazQuxReader")}
      {l "follow" "https://twitter.com/intent/user?screen_name=BazQuxReader"}<br/>
      {l "Google+ community" "https://plus.google.com/communities/108331945866753583767"}<br/>
      {l "Facebook community" "https://www.facebook.com/BazQuxReader"}<br/>
      {l "UserVoice" "http://bazqux.uservoice.com"} for feature requests.<br/>
      {l "hello@bazqux.com" "mailto:hello@bazqux.com"} for any questions.<br/>
      <br/>
      {l "Subscribe" subscribeBookmarklet} bookmarklet (drag it to your bookmarks bar).<br/>
      <br/>
      <br/>
      <p>FAQ</p>
      {faqText hrefLinkStopPropagation}
      </div>
    </xml>
    end

fun faq () =
    landingPage " FAQ" <xml/> <xml>
      <h2>FAQ</h2>

      {faqText hrefLink}

      {backToMain}
    </xml>

fun fetcher () =
    landingPage " Fetcher" <xml/> <xml>
      <h2>Fetcher</h2>
      <p>BazQux Fetcher is how BazQux Reader grabs RSS/Atom feeds and comments when users choose to subscribe to your blog in BazQux Reader. Fetcher collects and periodically refreshes these user-initiated feeds. Find answers below to some of the most commonly asked questions about how this user-controlled feeds and comments grabber works.</p>

      {qaX "How do I request that BazQux not retrieve some or all of my site's feeds?"
           <xml>When users subscribe to your feed, BazQux fetcher attempts to obtain the content of the feed in order to display it. Since fetcher requests come from explicit action by human users, and not from automated crawlers, fetcher does not follow robots.txt guidelines.<br/>
             If your feed is publicly available, BazQux can't restrict users from accessing it. One solution is to configure your site to serve a 404, 410, or other error status message to user-agent BazQux<br/>
             If your feed is provided by a blog or site hosting service, please work directly with that service to restrict access to your feed.<br/></xml>}

      {qa "How often will Fetcher retrieve my feeds?"
          "Fetcher shouldn't retrieve feeds from most sites more than once every hour on average. Some frequently updated sites may be refreshed more often. Note, however, that due to network delays, it's possible that Fetcher may briefly appear to retrieve your feeds more frequently."}

      {qa "Why is Fetcher trying to download incorrect links from my server, or from a server that doesn't exist?"
          "Fetcher retrieves feeds at the request of users who have subscribed to them in BazQux Reader. It is possible that a user has requested a feed URL location that does not exist."}

      {qa "Why is Fetcher downloading information from our \"secret\" web server?"
          "Fetcher retrieves feeds at the request of users who have added them to their BazQux. It is possible that the request came from a user who knows about your \"secret\" server or typed it in by mistake."}

      {qa "Why isn't Fetcher obeying my robots.txt file?"
          "Fetcher retrieves feeds only after users have explicitly subscribed to them in BazQux Reader. Fetcher behaves as a direct agent of the human user, not as a robot, so it ignores robots.txt entries. Fetcher does have one special advantage, though: because it's acting as the agent of multiple users, it conserves bandwidth by making requests for common feeds only once for all users."}

      {qa "Why are there hits from multiple machines at bazqux.com, all with user-agent BazQux?"
          "Fetcher was designed to be distributed on several machines to improve performance and scale as the web grows."}

      {qa "Can you tell me the IP addresses from which Fetcher makes requests so that I can filter my logs?"
          "The IP addresses used by Fetcher change from time to time. The best way to identify accesses by Fetcher is to use its identifiable user-agent: BazQux."}

      {qa "Why is Fetcher downloading the same page on my site multiple times?"
          "In general, Fetcher should only download one copy of each file from your site during a given feed retrieval. Very occasionally, the machines are stopped and restarted, which may cause it to again retrieve pages that it's recently visited."}

      {qa "Do you support push technology?"
          "Yes. BazQux Reader support push hubs. If your feeds advertise a push hub, Fetcher will subscribe for updates and reduce the number of polls to three times a day."}

      {qa "My Fetcher question isn't answered here. Where can I get more help?"
          "If you're still having trouble, try posting your question to support at bazqux.com."}

      {backToMain}
    </xml>

fun signInUI addUrl =
    logAction "main" "-" (
    r <- getHeader (blessRequestHeader "Referer");
    rc <- getCookie referrer;
    (case (r, rc) of
      | (Some r, None) =>
        setCookie referrer {Value = r, Expires = None, Secure = False}
      | _ => return ());
    Hacks.clear_script_header;
    (* ^ дабы на главной странице не было скриптов *)
    likeButtons <- htmlLikeButtons ();
    headMain <- htmlHeadMain ();
    c <- getCookie openIdURL;
    oisb <- htmlOpenIdSignInButton ();
    ois <- blessId "openidSignIn";
    oisu <- blessId "openidSignInUrl";
    landingPage ""(* "RSS reader with comments" *) headMain <xml>
      {case addUrl of
         | None => <xml/>
         | Some u => <xml><div class="pleaseSignInToAddSubscription">
           Please sign in to add subscription.
           </div></xml>
      }
      <p class="motto">RSS reader that shows comments to posts</p>

      <img class="screenShot" width={510} height={288}
           src={bless "/images/screenshot_v4.png"} />

      <div class="signIn"><div class="signInInner">
        <p class="startToday">Start your free trial today!</p>
        <a class="signInWith facebook" link={sign_in Facebook addUrl}>
        </a><a class="signInWith twitter" link={sign_in Twitter addUrl}>
        </a><a class="signInWith google" link={sign_in Google addUrl}>
        </a>{oisb}
        <div id={ois} class="openid">OpenID URL<br />
        <form>
          <textbox{#URL} id={oisu} class="openidUrl" value={Option.get "" c}/><br/>
          <submit class="openidSubmit" action={sign_in_openid addUrl}/>
        </form>
        </div>
(*         <p class="free30">It's FREE for 30 days.</p> *)
      </div></div>

      <div class="landingText">
        <div class="column1">
          <h3 class="first">Read all discussions in one place</h3>
          <p>BazQux Reader shows blog posts and comments
          in one seamless stream, tracks what was read
          and displays only new discussions next time.</p>
(*  and marks read discussions. *)
          <p>Comments from Reddit, Livejournal, blogs
          with comment feeds, Disqus and Facebook widgets are supported.</p>

(*           <h3>Filter</h3> *)
(*           Apply filter and read only things interesting to you. *)
          <h3>Mobile and desktop apps</h3>
          <p>Thanks to our Google Reader compatible API, BazQux Reader is supported by {mrReader hrefLink}, {feeddler hrefLink}, {slowFeeds hrefLink}, {justReader hrefLink}, {newsPlus hrefLink} and {viennaRss hrefLink}.</p>
          <p>{reeder hrefLink}, {press hrefLink} and {readKit hrefLink} can be used with BazQux Reader as a Fever server.</p>
(*           Thanks to Google Reader compatible API BazQux is already supported *)
(*           by Mr.&nbsp;Reader, Feeddler and JustReader apps. *)

          <h3>Full-text articles</h3>
          Read the text of feeds inside BazQux Reader, powered by Readability.
(*           Read full distilled post content right inside rss reader thanks to great Readability service. *)

          <h3>Photoblogs</h3>
          A mosaic or magazine view mode to quickly skim through picture rich blogs.

        </div>
        <div class="column2">
          <h3 class="first">Enjoy the speed</h3>
          Thanks to our powerful servers, BazQux Reader is one of the fastest feed readers. BazQux Reader gives you a fast user interface, fast feed updates and fast sync with apps.
(*           Thanks to our powerful servers, BazQux Reader is fast *)
(*           both in terms of feed updates and user interface. *)

          <h3>Subscribe to Google+ and Facebook pages</h3>
          Just enter the URL in the "Add subscription"
          dialog to add the page like a regular feed.

          <h3>Share and read later</h3>
          Share articles via E-mail, Facebook, Google+ or Twitter. Save them to Evernote, Pinboard, Pocket or Instapaper.
(*           Any post or comment can be shared via E-mail, *)
(*           Twitter, Facebook, Google+, Tumblr or saved to Pinboard, Pocket, Evernote or Instapaper. *)

          <h3>Tag and star</h3>
          Organize articles with stars or tags.

          <h3>Search</h3>
          Search in feeds, folders or tagged items.
(*           <h3>Advanced search</h3> *)
(*           It looks like filter. You still see discussions trees *)
(*           and reader remembers what you've seen. *)
(*           You can search by text, subject, author, tags *)
(*           and even search for messages with images only. *)

(*           <h3>Clean user interface.</h3> *)
(*           Nothing disturbs you from reading. *)

(*           <h3>OPML</h3> *)
(*           You can both upload and download OPML to add or backup your feeds. *)

          <h3>Quick start</h3>
(*           Sign in, add feeds and start reading. You can add your backup (Takeout.zip or OPML) from Google Reader. *)
          Sign in, import OPML or Takeout.zip file and start reading.
        </div>
      </div>
      <span class={clearBoth}></span>

      <div class="mainFooter"><div class="mainFooterInner">
(*         <p>{likeButtons}</p> *)

        <a href={bless "http://blog.bazqux.com"}>Blog</a> ·
        <a href={bless "https://twitter.com/BazQuxReader"}>Twitter</a> ·
        <a href={bless "mailto:hello@bazqux.com"}>Contact</a> ·
        <a link={privacy ()}>Privacy</a> ·
        <a link={faq ()}>FAQ</a> ·
        {apiLink (txt "API")}(*  — © 2011-2012 BazQux. *)
      </div></div>
    </xml>)

fun buyOpt r id (name : string) price = <xml><label for={id}>
  <div class="buyOption">
    <span class="buyOptionName">{r}<span class="dollarSign">$</span>{[price]}</span>
(*     {r}<span class="buyOptionName">{[name]}</span> *)
(*     <span class="buyOptionPrice"> *)
(*       <span class="dollarSign">$</span>{price} *)
(*     </span> *)
(*     <br/><span class="buyOptionDesc">{desc}</span> *)
  </div>
</label></xml>

fun add qs =
(*     u <- currentUrl; (\* вызывает crash? *\) *)
(*     u <- getUser; *)
(*     case u of *)
(*       | Some u => *)
    case qs of
        None => error <xml>No URL specified</xml>
      | Some q =>
        let val u = show q in
        params <- parseQueryStringUtf8Only u;
        (case params of
           | ("url", url) :: [] =>
             if url = "" then
                 error <xml>Empty URL specified</xml>
             else
                 u <- getUser "";
                 (case u of
                    | None =>
                      logAction "add" "-" (signInUI (Some url))
                    | Some u =>
                      logAction "add" u (addUrlAndRedirect u url))
           | _ =>
             error <xml>No URL specified.<br/><br/>
             Use <b>{["https://bazqux.com/add?url={url}"]}</b> format.</xml>
        )
        end


and main () =
    u <- getUser "";
    case u of
      | Some uid =>
        if uid = "demo" then signInUI None
        else
        (* page "commented out" <xml/> *)
        (p <- getPaidTill uid;
         logAction "main" uid (userUI p uid))
      | None     =>
        signInUI None

and demo s =
    logAction "demo" "demo" (
    let val init =
            s <- newSession (Url { Id = "demo" }) [];
            sid <- sessionCookie;
            setCookie sid {Value = s.Key, Expires = Some s.Expire,
                           Secure = False};
            redirect (bless "/demo")
            (* FF не любит перенаправления на самого себя, а у urweb какие-то
               проблемы с подписыванием куки для demo *)
    in
    u <- getUser "";
    case u of
      | Some uid =>
        if uid = "demo" then
(*             (if s <> "" then redirect (bless "/demo") else *)
            t <- now;
            userUI (PTFreeTrial { Till = addSeconds t (30*84600-1) }) "demo"
        else
            init
      | _ => init
    end)

and buyText paidTill =
    o1 <- fresh;
    o2 <- fresh;
    o3 <- fresh;
    return <xml>
         <h1>Buy now</h1>
         <p>{[case paidTill of
               | PTFreeTrialFinished _ =>
                 "We are very pleased to see that you decided to buy the subscription."
               | PTPaid _ =>
                 "We are very pleased that you decided to continue the subscription."
               | PTPaidFinished _ =>
                 "We are very pleased that you decided to continue the subscription."
               | _ =>
                 "We are very pleased that you decided to finish free trial and buy the subscription."
            ]}</p>
         <p>As a customer you get first-priority support and help us improve BazQux Reader.</p>
         <p>Choose any price you want and enjoy!</p>
(*          <p>What option would you like to take?</p> *)
         <form>
           <radio{#Option}>
             {buyOpt <xml><radioOption value={"readeryear"} id={o3} checked={True}/></xml> o3
                  "Good money" "29/year"
                  (* <xml>less than $2<sup>50</sup>/month</xml> *)}
             {buyOpt <xml><radioOption value={"readeryear19"} id={o2} /></xml> o2
                  "Standard" "19/year"
                  (* <xml>less than $2/month</xml> *)}
             {buyOpt <xml><radioOption value={"readeryear9"} id={o1} /></xml> o1
                  "Some money" "9/year"
                  (* <xml>only 75&cent;/month</xml> *)}
           </radio>
           <submit action={buy}
                   value={"Complete your purchase at FastSpring.com"} />
         </form>
(*          <p>There is no difference in service </p> *)
         <hr/>

         <h3>Refund policy</h3>
         {refundPolicyText}

         <h3>Contact information</h3>
         <p>We're always happy to hear your questions at
           <a href={bless "mailto:support@bazqux.com"}>support@bazqux.com</a></p>

         <h3>Privacy policy</h3>
         {privacyPolicyText}
       </xml>

and mobileLoginBox ps popup login =
    tid <- fresh;
    pid <- fresh;
    text <- source "";
    password <- source "";
    forceLoginEdit <- source False;
    loginError <- source (None : option string);
    let val loginOk =
            l <- signal text;
            return (Js.checkLogin l)
        val passwordOk =
            p <- signal password;
            return (p <> "")
        val save : transaction {} =
            lok <- current loginOk;
            pok <- current passwordOk;
            if not lok then
                set loginError (Some "Invalid login. Must only contain english letters or digits, 4 characters minimum.")
            else if not pok then
                set loginError (Some "Invalid password. Must be non-empty.")
            else
                l <- get text;
                p <- get password;
                ph <- Js.passwordHash p;
                fak <- Js.feverApiKey l p;
                ok <- rpc (mobileLogin l ph fak []);
                if not ok then
                    set loginError
                        (Some "This login is busy. Try another one.")
                else
                    set login (Some (Js.toLowerCase l));
                    ps.Hide
        fun keydown k : transaction {} =
            if k.KeyCode = 13 then save else
(*             if k.KeyCode = 27 then ps.Hide else *)
            return ()
        val init =
            set loginError None;
            set password "";
            Js.makePasswordInput pid;
            Js.select tid;
            Js.focus tid
    in
    box <- ps.New Css.mobileLoginBox <xml>
      <h1>Mobile login</h1>
      <p>Set your login &amp; password to access reader from client apps.</p>
      {dyn_ (le <- signal loginError;
             return (case le of
               | Some e => <xml><p class={Css.mobileLoginError}>{[e]}</p></xml>
               | _ => <xml/>))}
      {dyn_ (l <- signal login;
             f <- signal forceLoginEdit;
             return (case (f, l) of
               | (False, Some l) =>
                 <xml><p>Your login is <b>{[l]}</b></p>
                 <p>{textButton "Change"
                                (set text l;
                                 set forceLoginEdit True;
                                 init)}</p></xml>
               | _ =>
                 <xml>
                   <p>Login<br/>
                   <ctextbox id={tid} class="mlLogin" source={text} size={20}
                             onkeydown={keydown} />
                   {dyn_ (lok <- loginOk;
                          return (if lok then <xml/> else
                          txt "english letters or digits, 4 characters minimum"))}
                   </p>
                   <p>Password<br/>
                   <ctextbox id={pid} class="mlPassword"
                             source={password} size={20}
                             onkeydown={keydown} />
                   {dyn_ (pok <- passwordOk;
                          return (if pok then <xml/> else
                                  txt "must be non-empty"))}</p>
                 <p>{textButton "OK" save}
                    {textButton "Cancel" ps.Hide}</p></xml>))
      }
      <br/>
      <p>Use {mrReader hrefLink} on iPad, {feeddler hrefLink} or {slowFeeds hrefLink} on iPhone/iPad, {newsPlus hrefLink} (gReader), {justReader hrefLink} and {amberRssReader hrefLink} on Android and {viennaRss hrefLink} on Mac.</p>
      <p>{reeder hrefLink} on iPhone, {press hrefLink} on Android and {readKit hrefLink} on Mac can be used by setting bazqux.com as a Fever server (see <a target={"_blank"} href={bless "http://blog.bazqux.com/2013/09/reeder-press-and-readkit-via-fever-api.html"}>how to</a>).</p>
      <p>BazQux Reader supports Google Reader API.</p>
      <p>Please tell developers of your favorite client app to add support
      for BazQux Reader!</p>
      <p>API documentation is {apiLink (txt "here")}.</p>
    </xml>;
    return (set forceLoginEdit False;
            ps.Toggle popup box;
            init)
    end

and userUI paidTill uid : transaction page =
    searchBoxId <- fresh; (* чтобы всегда был одинаковый *)
    dragMarkId <- fresh;
    fl <- getCookie freshSignIn;
    withSome (fn _ => clearCookie freshSignIn) fl;
    likeButtons <- htmlLikeButtons ();
    headMain <- htmlHeadMainNoTranslate ();
    (* Google Chrome иногда пытается перевести RSS-фид и убивает читалку *)
    buyT <- buyText paidTill;
    curTime <- now;
    let fun mainPage () =
    dsi <- defaultSubItem;
    currentFeed <- source dsi;
    currentSearchFeed <- source dsi;
    currentFeedUrl <- source "";
    currentTagHash <- source "";
    msgDivId <- fresh;
    dummyId <- fresh;
    innerMessagesId <- fresh;
    loading <- source True;
    popup <- source <xml/>;
    sharePopup <- source <xml/>;
    ps <- popups;
    Js.registerOnDragAndDropStart ps.Hide;
    backgroundRpc <- backgroundRpc preprocessBGActions handleBGActions;
    onlyUpdatedSubscriptions <- source False;
    exactUnreadCounts <- source False;
    setFeedSrc <- source (fn _ => return ());
    onUpdateSubInfoSrc <- source (fn _ _ => return ());
    buyBox <- ps.New Css.buyBox buyT;
    login <- source None;
    toggleMobileLoginBox <- mobileLoginBox ps popup login;
    subscribeDiscoveryFeedSrc <- source (fn _ => return ());
    clearMtvmSrc <- source (fn _ => return ());
    refreshSrc <- source (return ());
    viewModeJustSet <- source False;
    let fun subscribeDiscoveryFeed u =
            s <- get subscribeDiscoveryFeedSrc;
            s u
        fun clearMtvm u =
            c <- get clearMtvmSrc;
            c u
        val refresh =
            r <- get refreshSrc; r;
            set viewModeJustSet False
        fun selectSubscription typ =
            i <- fresh;
            d <- ps.NewMenu Css.selectSubscriptionDialog <xml>
              Select {[typ]}<br/>
                <ctextbox id={i} class="selectSubscriptionInput" size={30}
                /></xml>;
            ps.Toggle popup d;
            Js.setupSubscriptionAutocomplete typ i;
            Js.select i;
            Js.focus i
        fun redirect u =
            backgroundRpc.OnUnload;
            Basis.redirect u
        val buyClick =
            Js.trackEvent "UI" "BuyClick" uid;
            ps.Toggle popup buyBox
(*             redirect (bless "/buy") *)
        val greaderImportClick =
            Js.trackEvent "UI" "ImportGReader" uid;
            redirect (effectfulUrl importFromGoogleReader)
        val greaderImportStarredClick =
(*             Js.trackEvent "UI" "ImportGReader" uid; *)
            redirect (effectfulUrl importStarredAndTaggedItemsFromGoogleReader)
        val opmlUploadClick =
            Js.trackEvent "UI" "UploadOPML" uid;
            Js.opmlUpload backgroundRpc.OnUnload
        val msgTreeViewMode =
            cf <- signal currentFeed;
            signal cf.ViewMode
        val getMsgTreeViewMode = current msgTreeViewMode
        fun setFeed si = sf <- get setFeedSrc; sf si
        fun onUpdateSubInfo si si2 = o <- get onUpdateSubInfoSrc; o si si2
        fun searchQueryAndSubItem hash =
            if isPrefixOf "search/" hash then
                case strindex (strsuffix hash 7) #"/" of
                  | Some c =>
                    si <- getSubItemByHash (strsuffix hash (7+c+1));
                    (case si of
                       | Some si =>
                         return (Some (Js.decodeURIComponent (substring hash 7 c), si))
                       | None => return None)
                  | None => return None
            else
                return None
        val updateCurrentFeed =
            c <- get (getSubItem 0).Counters;
            if c.Feed = 0 && c.Error = 0 && c.Scanning = 0 then
                return () (* ничего не делаем, если у нас пусто *)
            else
            hash <- Js.getLocationHash;
            sq <- searchQueryAndSubItem hash;
            case sq of
              | Some (q, si) => set currentSearchFeed si
              | None =>
                si <- getSubItemByHash hash;
                (case si of
                   | Some si => set currentFeed si
                   | None => return ())
    in
    ssWidget <- subscriptionsWidget
        currentFeed updateCurrentFeed onUpdateSubInfo setFeed backgroundRpc
        ps popup onlyUpdatedSubscriptions exactUnreadCounts
        subscribeDiscoveryFeed clearMtvm refresh;
    msgsWidget <- msgsWidget msgDivId ps popup sharePopup
                             backgroundRpc
                             currentFeed currentSearchFeed getMsgTreeViewMode
                             (ssWidget.UpdateSubscriptions True) loading;
    infoMessage <- infoMessageAtTheTop;
    searchQuery <- source "";
    searchCounters <- source emptyCounters;
    searchResults <- source (None : option searchResults);
    settingHash <- source 0;
    subscribeUrlId <- fresh;
    endDivId <- fresh;
    scrollTimeoutActive <- source False;
    fullscreen <- source False;
    lastCounters <- source emptyCounters;
    scannedPercent <- source <xml/>;
    refreshAvailable <- source False;
    msgTreeSpacerText <- source <xml/>;
    exiting <- source False;
    helpClickTracked <- source False;
    welcomeState <- source None;
    msgScale <- source Css.msgScale0;
    bodyScale <- source Css.bodyScale0;
    displayDiscovery <- source False;
    discoveryTextBoxId <- fresh;
    let fun addSub u =
(*             u <- what; *)
            when (u <> "")
                 (ssWidget.AddSubscription u;
                  Js.trackEvent "UI" "AddSubscription" uid;
                  ps.Hide)
        fun toggleDiscovery fromWelcome =
            dd <- get displayDiscovery;
            set displayDiscovery (not dd);
            when (not dd && not (fromWelcome && Js.hasOnscreenKeyboard ()))
                 (* на iPad показывается сначала верх, потом низ, пока
                    клавиатура выплывает.
                    А через setTimeout не работает, т.к. focus() на iPad
                    работает только внутри user-initiated events
                  *)
                 (Js.select discoveryTextBoxId;
                  Js.focus discoveryTextBoxId)
    in
    discovery <- discoveryWidget addSub opmlUploadClick discoveryTextBoxId displayDiscovery ps popup;
    let val msgScales =
            Css.msgScale0 :: Css.msgScale1 :: Css.msgScale2 ::
            Css.msgScale3 :: Css.msgScale4 :: Css.msgScale5 :: []
        val bodyScales =
            Css.bodyScale0 :: Css.bodyScale1 :: Css.bodyScale2 ::
            Css.bodyScale3 :: Css.bodyScale4 :: Css.bodyScale5 :: []
        fun changeScale what scales src f =
            s <- Js.getFromLocalStorage uid (what ^ "Scale") 0;
            let val s' = f s
            in
                Js.saveToLocalStorage uid (what ^ "Scale") s';
                set src (Option.get null (List.nth scales s'))
            end
        val changeBodyScale = changeScale "body" bodyScales bodyScale
        val changeMsgScale = changeScale "msg" msgScales msgScale
        fun incScale n = min 5 (n+1)
        fun incBodyScale n = min 4 (n+1)
        fun decScale n = max 0 (n-1)
        val trackHelpClick =
            t <- get helpClickTracked;
            when (not t)
                 (set helpClickTracked True;
                  Js.trackEvent "UI" "HelpClick" uid)
        val snInfoMsgScanning = <xml>
              <div class="subInfoScanning">
                <span class="loadingGif"></span> Subscribing to new feed...
              </div>
            </xml>
        fun scannedPercentXml text = <xml><div class={Css.scannedPercent}>
             <span class="loadingGif"></span>
             {dyn_ (ls <- signal lastCounters;
                   return <xml><span class="percent">{[ls.ScannedPercent]}%</span></xml>)}
             {[text]}
             {displayIf refreshAvailable (textButton "Refresh" refresh)}
            </div></xml>
        fun scannedPercent100Xml text = <xml><div class={Css.scannedPercent}>
             {[text]} {textButton "Refresh" refresh}
            </div></xml>
(*         val snImportTags = <xml><div class={Css.scannedPercent}> *)
(*              {textButton "Import starred and tagged items" *)
(*                          greaderImportStarredClick} *)
(*             </div></xml> *)
        val snScanningComments = scannedPercentXml "Scanning comments..."
        val snAllCommentsScanned = scannedPercent100Xml "All comments scanned."
(*         val snImportingTags = scannedPercentXml "Importing starred and tagged items..." *)
(*         val snAllTagsImported = scannedPercent100Xml "All starred and tagged items imported." *)
        val snInfoMsg = <xml><dyn signal={
            si <- (signal currentFeed : signal subItem);
            c <- signal si.Counters;
            case si.SIType of
              | SITSearch _ => return <xml/>
              | SITFeed f =>
                return <xml/>
              | _ => (* SITFolder _ | SITAll *)
                return <xml/>
                (* исчезает, когда сканирование заканчивается *)
(*                 (if c.Scanning > 0 then <xml> *)
(*                   <div class="subInfoScanning"> *)
(*                     <span class="loadingGif"></span> Subscribing to new feeds... *)
(*                     {textButton "Refresh" refresh} *)
(*                   </div> *)
(*                 </xml> *)
(*                 else <xml/>) *)
                        }/></xml>
        fun modifyMsgTreeViewMode vmName (f : msgTreeViewMode -> msgTreeViewMode) =
            set viewModeJustSet True;
            cf <- get currentFeed;
            let fun updSub si = case si.SIType of
                    | SITFeed feed =>
                      mtvm0 <- get si.ViewMode;
                      let val mtvm = f mtvm0 in
                      set si.ViewMode mtvm;
                      discovery.SetMtvm feed.Subscription.Url mtvm;
                      when (mtvm0.ExpandedComments <> mtvm.ExpandedComments)
                           (Js.updateExpandedComments si.Index
                                                      mtvm.ExpandedComments);
                      backgroundRpc.AddAction (BGSetSubscriptionViewMode
                                                   { Url = feed.Subscription.Url, ViewMode = mtvm });
                      return True
                      end
                    | _ => return False
                fun updFolder' si name =
                    mtvm <- Monad.mp f (get si.ViewMode);
                    set si.ViewMode mtvm;
                    backgroundRpc.AddAction (BGSetFolderViewMode
                                                 { Folder = name, ViewMode = mtvm })
                fun updFolder si name = case vmName of
                    | None =>
                      (* выбор ascending/unread *)
                      updFolder' si name;
                      return True
                    | Some vm => (* режим просмотра  *)
                      c <- confirm ("Do you really want to set " ^ vm ^
                                    " for all feeds" ^
                                    (if name <> "" then " in \"" ^ name ^ "\""
                                     else "") ^
                                    "? \nAll per-feed settings will be cleared.");
                      if c then
                          updFolder' si name;
                          (* TODO: тут надо бы все папки обновлять,
                             если это корень *)
                          List.app (fn s => x <- updSub s; return ())
                                   (getSubItems si.Index);
                          return True
                      else
                          return False
                fun upd si =
                    case si.SIType of
                        SITAll => updFolder si ""
                      | SITFolder f => updFolder si f.Folder
                      | SITFeed f => updSub si
                      | SITStarred => updFolder' si ",SITStarred"; return True
                      | SITAllTags => updFolder' si ",SITAllTags"; return True
                      | SITTag t => updFolder' si t.TagName; return True
                      | SITSearch _ =>
                        csf <- get currentSearchFeed;
                        upd csf
            in
                upd cf
            end
(*         fun isFolder si = *)
(*             case si.SIType of *)
(*                 SITAll => return True *)
(*               | SITFolder _ => return True *)
(*               | SITFeed _ => return False *)
(*               | SITSearch _ => *)
(*                 csf <- signal currentSearchFeed; *)
(*                 isFolder csf *)
        fun setUnreadOnly x =
            m <- modifyMsgTreeViewMode None (setF [#UnreadOnly] x);
            when m refresh
        fun setAscending x =
            m <- modifyMsgTreeViewMode None (setF [#Ascending] x);
            when m refresh
        fun isTagSi si =
            case si.SIType of
              | SITStarred => return True
              | SITAllTags => return True
              | SITTag _ => return True
              | SITSearch _ =>
                csf <- get currentSearchFeed;
                isTagSi csf
              | _ => return False
        fun isTagSiS si =
            case si.SIType of
              | SITStarred => return True
              | SITAllTags => return True
              | SITTag _ => return True
              | SITSearch _ =>
                csf <- signal currentSearchFeed;
                isTagSiS csf
              | _ => return False
        val isTag =
            si <- get currentFeed;
            isTagSi si
        fun setViewMode name expanded posts f =
            t <- isTag;
            m <- modifyMsgTreeViewMode (Some name)
                (fn vm =>
                    setF [#NoOverride] (f (not t))
                    (setF2 [#ExpandedComments] [#Posts] expanded posts vm));
            when m refresh
        val withCommentsVM =
            ( vmWithComments
            , fn vm => vm.ExpandedComments
            , setViewMode "expanded view" True PVMFull id
            , "Expanded view")
        val fullVM =
            ( vmFull
            , fn vm => not vm.ExpandedComments && vm.Posts = PVMFull
            , setViewMode "expanded view without comments" False PVMFull id
            , "Expanded view without comments")
        val shortVM =
            ( vmShort
            , fn vm => not vm.ExpandedComments && vm.Posts = PVMShort
            , setViewMode "list view" False PVMShort id
            , "List view")
        val magazineVM =
            ( vmMagazine
            , fn vm => not vm.ExpandedComments && vm.Posts = PVMMagazine
            , setViewMode "magazine view" False PVMMagazine id
            , "Magazine view")
        val mosaicVM =
            ( vmMosaic
            , fn vm => not vm.ExpandedComments && vm.Posts = PVMMosaic
            , setViewMode "mosaic view" False PVMMosaic id
            , "Mosaic view" )
        val combinedVM =
            ( vmCombined
            , fn vm => vm.NoOverride
            , setViewMode "default subscription view" False PVMFull (fn _ => True)
            , "Use subscription view modes" )
        val subscriptionTitle =
            dyn_ (f <- signal currentFeed; return (txt f.Title))
        fun setForest f =
            msgsWidget.SetForest f;
            st <- Js.scrollTop msgDivId;
            when (st <> 0) (Js.setScrollTop msgDivId 0)
            (* прокручиваем в начало, т.к. firefox/opera не сбрасывают scroll
             *)
        fun setDocumentTitle si =
            Js.setDocumentTitle ("bq | " ^ subItemTitle si)
            (* иногда оставляет в заголовке окна только концовку,
               и непонятно, что это вообще за окно
             *)
        fun setCurrentFeed si =
            msgsWidget.SetForest emptyMF;
            set currentFeed si;
            set currentSearchFeed dsi;
            setDocumentTitle si;
            set searchResults None;
            set scannedPercent <xml/>;
            set msgTreeSpacerText <xml/>;
            set currentFeedUrl "";
            set currentTagHash "";
            set refreshAvailable False;
            Js.updateAndSaveReadCounters [] [] (* чистим счетчики *)
        fun updateHash si =
            let val h' = subItemHash si in
            h <- Js.getLocationHash;
            when (h <> h')
                 (modify settingHash succ;
                  Js.setLocationHash h')
            (* Js.setTimeout (set settingHash False) 0
               ^ почему-то в Firefox все равно событие выстреливает
             *)
            (* onhashchange вызывается асинхронно, сносим флажок после него *)
            end
        val loadingIndicator =
            dyn_ (a <- signal msgsWidget.AppendingRpc;
                  l <- signal msgsWidget.LoadingComments;
                  (* loadingComments -- уже есть индикатор при expand *)
                  return (if a && not l then
                              <xml><div class={Css.loading}>
                                <span class="loadingGif"></span> Loading...
                              </div></xml>
                          else <xml/>))
(*         fun msgForest u tp tc vm = *)
(*             withUser "msgForest" (fn userId => feedMsgForest AMNormal userId u tp tc vm) *)
        fun tagsForest ts vm =
            withUser "tagsForest" (fn userId => tagsMsgForest AMNormal userId ts vm)
        fun folderForest feeds vm =
            withUser "folderForest"
                     (fn userId =>
                         folderMsgForest
                             (case feeds of
                                | (feed, rp, rc, -1, tc) :: [] =>
                                  AMDiscovery { Url = feed }
                                | _ => AMNormal)
                             userId feeds [] vm)
        fun getUrls (onlyUnread : bool) (si : subItem) =
            Monad.mp (List.sort (fn a b => a.1 > b.1))
                     (List.mapPartialM (fn s =>
                c <- get s.Counters;
                return (case s.SIType of
                  | SITFeed { Subscription = { State = SSFeed f, ... }, ... } =>
                    if not onlyUnread ||
                       c.ReadPosts <> c.TotalPosts ||
                       c.ReadComments <> c.TotalComments then
                        Some (show f.Url, c.ReadPosts, c.ReadComments, c.TotalPosts, c.TotalComments)
                    else
                        None
                  | _ => None)) (getSubItems si.Index))
         fun setFolderForest si feeds vm =
             if isNull feeds then
                 c <- get si.Counters;
                 set msgTreeSpacerText
                 (case si.SIType of
                     | SITFolder { Folder = "" } => <xml/>
                     | SITFeed f => (* snInfoMsg *)
                (if c.Scanning > 0 then
                     snInfoMsgScanning
                 else if c.Error > 0 then
                     <xml>
                       <div class="subInfoError"><h1>Error</h1>
                         {Js.preprocessMessageText
                          (case f.Subscription.State of
                             | SSError e => e.Message | _ => "")}
                       </div>
                       <div class="subInfoErrorButtons">
                         {textButton "Unsubscribe"
                                     (setFeed dsi;
                                      Js.trackEvent "UI" "UnsubscribeErr" uid;
                                      ssWidget.Unsubscribe (si :: []))}
                         {textButton "Retry"
                                     (Js.trackEvent "UI" "RetryScan" uid;
                                      Js.retryScanning si.Index;
                                      refresh;
(*                                       updateCurrentFeed; *)
                                      ssWidget.RetryScanning si)}
                       </div>
                     </xml>
                 else
                     <xml/>)
                     | _ =>
                       if c.Scanning > 0 then <xml>
                         <div class="subInfoScanning">
                           <span class="loadingGif"></span>
                           Subscribing to new feeds...
                           {textButton "Refresh" refresh}
                         </div>
                       </xml>
                       else <xml>
                         <div class="emptySetFeedResult">
                           <h3>{[if c.Error = 0 then
                                     "No subscriptions added."
                                 else "No feeds."]}
                           </h3></div>
                       </xml>)
             else
             queueCRpcB backgroundRpc
             (fn l =>
                 set loading True;
                 showInfo null "Loading..." infoMessage
                          (rpc (folderForest feeds vm l)))
             (fn (uc,(MsgForest f)) =>
                 (case (feeds,uc) of
                    | ((feed, _,_,-1,_) :: [],
                       (ufeed, rp,rc,tp,tc) :: []) =>
                      when (feed = ufeed)
                           (c <- get si.Counters;
                            set si.Counters
                            (c -- #ReadPosts -- #ReadComments
                               -- #TotalPosts -- #TotalComments
                             ++ { ReadPosts = rp, ReadComments = rc
                                , TotalPosts = tp, TotalComments = tc })
                           )
                    | _ => return ());
                 Js.updateAndSaveReadCounters feeds uc;
                 setForest (MsgForest f);
                 set loading False;
                 if not (isNull f.List) then
                     set msgTreeSpacerText loadingIndicator
                 else
                     c <- get si.Counters;
                     set msgTreeSpacerText <xml>
                       <div class="emptySetFeedResult">
                         {if c.TotalPosts = 0 && c.TotalComments = 0 then <xml>
                           <h3>"{subscriptionTitle}" is empty (no posts found).</h3>
                         </xml> else <xml>
                           <h3>"{subscriptionTitle}" has no unread items.</h3>
                           {displayIfSig (mtvm <- msgTreeViewMode;
                                          return mtvm.UnreadOnly)
                                         (textButton "View all items"
                                                     (setUnreadOnly False))}
                         </xml>}
                       </div>
                     </xml>
             )
         fun setFolder si vm =
             feeds <- getUrls False (*vm.UnreadOnly*) si;
             setFolderForest si feeds vm
(*              if rp <> tp || (vm.ExpandedComments && rc <> tc) || *)
(*                 (vm.UnreadOnly = False && (tp <> 0 || tc <> 0)) then *)
(*                  (feeds <- getUrls False (\*vm.UnreadOnly*\) si; *)
(*                  ) *)
        fun setTags si ts c =
            set currentTagHash si.Hash;
(*             ti <- get ssWidget.TagsImported; *)
(*             (if c.ScannedPercent <> 100 then *)
(*                  set lastCounters c; *)
(*                  set scannedPercent snImportingTags *)
(*             else when (not ti) (set scannedPercent snImportTags)); *)
            set msgTreeSpacerText <xml/>;
            ujs <- get viewModeJustSet;
            when (not ujs)
                 (m <- modifyMsgTreeViewMode None (setF [#UnreadOnly] False);
                  return ());
            set viewModeJustSet False;
            vm <- getMsgTreeViewMode;
            queueCRpcB backgroundRpc
                  (fn l =>
                   set loading True;
                   showInfo null "Loading..." infoMessage (rpc
                      (tagsForest ts vm l)))
                  (fn mf =>
                      setForest mf;
                      set msgTreeSpacerText loadingIndicator;
                      set loading False)
        fun setFeed_ si =
            ps.Hide;
            case paidTill of PTFreeTrialFinished _ => return () | PTPaidFinished _ => return () | _ =>
            set searchQuery "";
            Js.selectSubItem si.Index;
            updateHash si;
            setCurrentFeed si;
            c <- get si.Counters;
            vm <- getMsgTreeViewMode;
            case (si.SIType, c) of
               | (SITFolder _, _) => setFolder si vm
               | (SITAll, _) => setFolder si vm
               | (SITStarred, _) => setTags si (Some (ITStarred :: [])) c
               | (SITAllTags, _) => setTags si None c
               | (SITTag t, _) =>
                 setTags si (Some (ITTag { TagName = t.TagName } :: [])) c
               | (SITFeed
                      { Subscription = { State = SSFeed f, ... }, ... },
                  { ReadPosts = rp, ReadComments = rc
                  , TotalPosts = tp, TotalComments = tc
                  , ScannedPercent = sp, ... }) =>
                      set currentFeedUrl f.Url;
                      when (sp <> 100)
                           (set lastCounters c;
                            set scannedPercent snScanningComments);
                      let val feeds = ((show f.Url), rp, rc, tp, tc) :: [] in
                      setFolderForest si feeds vm
(*                       if rp <> tp || (vm.ExpandedComments && rc <> tc) || *)
(*                          (vm.UnreadOnly = False && (tp <> 0 || tc <> 0)) then *)
                      end
               | (SITFeed _, _) => setFolderForest si [] vm
               | _ => return ()
        fun setDiscoveryFeed url title feedLink faviconStyle mbmtvm =
            c <- source (
                 modifyF [#ReadPosts] (const (-1))
                 (modifyF [#TotalPosts] (const (-1)) emptyCounters));
            mtvm <- (case mbmtvm of
              | Some m => return m
              | None => discovery.LookupMtvm url);
            m <- source mtvm;
            setFeed
                ({ Hash          = "subscription/" ^ Js.encodeURIComponent url
                 , Index         = discoverySubItemIndex
                 , Title         = title
                 , SIType        = SITFeed { Subscription =
                                               { State = SSFeed { Url = url }
                                               , Url = url
                                               , EditsCount = 0
                                               , Title = None
                                               , Folders = [] }
                                           , FeedLink = feedLink
                                           , PointAllDesc = None
                                           }
                 , Counters      = c
                 , ViewMode      = m
                 , ParentFolders = []
                 , DomIds        = []
                 , FaviconStyle  = faviconStyle
                 })
        fun subscribeDiscoveryFeed url =
            discovery.Hide;
            c <- discovery.GetCountry;
            q <- discovery.GetQuery;
            ssWidget.AddDiscoverySubscription url c q
        fun searchForest q feeds vm =
            withUser "searchForest"
                     (fn userId => searchMsgForest userId q feeds vm)
        fun searchTagsForest q tags vm =
            withUser "searchTagsForest"
                     (fn userId => searchTagsMsgForest userId q tags vm)
        fun setEmptyResult x =
            set msgTreeSpacerText
               <xml><div class="emptySetFeedResult">{x}
               </div></xml>
        fun search' (q : string) (csi : subItem) =
            case paidTill of PTFreeTrialFinished _ => return () | PTPaidFinished _ => return () | _ =>
            let val searchInAllItemsBtn =
                    textButton "Search in all items"
                           (m <- modifyMsgTreeViewMode None (setF [#UnreadOnly] False);
                            search' q csi)
                val si =
                    { Hash          =
                       "search/" ^ Js.encodeURIComponent q ^ "/" ^ csi.Hash
                    , Index         = -1
                    , Title         = q
                    , SIType        = SITSearch { Query = q }
                    , Counters      = searchCounters
                    , ViewMode      = csi.ViewMode
                    , ParentFolders = []
                    , DomIds        = []
                    , FaviconStyle  = None
                    }
            in
            when (q <> "")
            (Js.updateSearchAutocomplete q;
             set searchCounters emptyCounters;
             sq <- get searchQuery;
             when (sq <> q) (set searchQuery q);
             Js.selectSubItem csi.Index;
             updateHash si;
             setCurrentFeed si;
             set currentSearchFeed csi;
             vm <- getMsgTreeViewMode;
             tags <- return (case csi.SIType of
               | SITStarred => (True, Some (ITStarred :: []))
               | SITAllTags => (True, None)
               | SITTag t => (True, Some (ITTag { TagName = t.TagName } :: []))
               | _ => (False, None));
             du <- current (discoverySubItemUrl currentSearchFeed);
             feeds <- (case du of
               | Some u => return ((u, 0, 0, 1000000000, 1000000000) :: [])
               | _ => getUrls vm.UnreadOnly csi);
             if isNull feeds && not tags.1 then
                 (if isNull (getSubItems csi.Index) then
                      setEmptyResult <xml>
                        <h3>No subscriptions to search.</h3>
                      </xml>
                  else
                      setEmptyResult <xml>
                        <h3>You have no unread items to search.</h3>
                        {searchInAllItemsBtn}
                      </xml>);
                 backgroundRpc.AddAction (BGSaveFilterQuery { Query = q })
             else
             (queueCRpcB backgroundRpc
                       (fn l =>
                        set loading True;
                        showInfo Css.searching "Searching..." infoMessage
                           (if tags.1 then
                                rpc (searchTagsForest q tags.2 vm l)
                            else rpc (searchForest q feeds vm l)))
             (fn sr =>
                 let val (pc,cc) =
                         case sr.MsgForest of
                           | MsgForest mf =>
                             List.foldl
                                 (fn (mi, MsgForest mf) (pc,cc) =>
                                     (if isResult mi then pc+1 else pc,
                                      if notNull mf.List then
                                          cc + mf.ResultsCount else cc))
                                 (0,0)
                                 mf.List
                 in
                     set searchCounters
                         { ReadPosts = 0, TotalPosts = pc
                         , ReadComments = 0, TotalComments = cc
                         , Scanning = 0, ScanningComments = 0
                         , Error = 0, Feed = 1, ScannedPercent = 100 };
                     set searchResults (Some sr);
                     setForest sr.MsgForest;
                     set loading False;
                     if pc+cc = 0 && sr.Total <> 0 && vm.UnreadOnly then
                        setEmptyResult <xml>
                          <h3>Nothing found in unread items.</h3>
                          {searchInAllItemsBtn}
                        </xml>
                     else if pc+cc = 0 then
                        setEmptyResult <xml>
                          <h3>Nothing found, sorry.</h3>
                        </xml>
                     else
                        set msgTreeSpacerText loadingIndicator
                 end)))
            end
        val search =
            q <- get searchQuery;
            cf <- get currentFeed;
            csi <- (case cf.SIType of
                     | SITSearch _ => get currentSearchFeed
                     | _ => return cf);
            search' q csi
        fun reloadFeed () =
            hash <- Js.getLocationHash;
            qsi <- searchQueryAndSubItem hash;
            case qsi of
              | Some (q, sit) => search' q sit
              | _ =>
                si <- getSubItemByHash hash;
                cf <- get currentFeed;
                (case si of
                   | Some si => setFeed si
                   | None =>
                     when (isPrefixOf "subscription/" hash)
                          (let val url = Js.decodeURIComponent (strsuffix hash (strlen "subscription/"))
                           in
                           if cf.Hash = hash then
                               mtvm <- get cf.ViewMode;
                               setDiscoveryFeed
                                   url
                                   cf.Title
                                   (case cf.SIType of
                                      | SITFeed f => f.FeedLink
                                      | _ => None)
                                   cf.FaviconStyle
                                   (Some mtvm)
                           else
                               d <- tryRpc (feedDetails url);
                               (* не круто, что запрос фида пойдет после,
                                  ну и ладно
                                *)
                               (case d of
                                  | Some (title, link, favicon, mtvm) =>
                                    setDiscoveryFeed url title link
                                                     favicon (Some mtvm)
                                  | _ =>
                                    setDiscoveryFeed url "" None None None)
(*                                cf <- get currentFeed; *)
(*                                (case (d, cf.Hash = hash) of *)
(*                                   | (Some (title, link, favicon), True) => *)
(*                                     let val cf' = *)
(*                                     (cf *)
(*                                      -- #Title -- #FaviconStyle -- #SIType *)
(*                                      ++ { Title = title *)
(*                                         , FaviconStyle = favicon *)
(*                                         , SIType = *)
(*                                           case cf.SIType of *)
(*                                             | SITFeed f => *)
(*                                               SITFeed (modifyF [#FeedLink] *)
(*                                                                (const link) f) *)
(*                                             | _ => cf.SIType *)
(*                                         }) *)
(*                                     in *)
(*                                         set currentFeed cf; *)
(*                                         setDocumentTitle cf *)
(*                                     end *)
(*                                   | _ => return ()) *)
                               end))
        fun onhashchange () =
            sh <- get settingHash;
            if sh > 0 then
                modify settingHash pred
            else
                (* refresh *) reloadFeed ()
        fun feedKeyboardAction act =
            feedKeyboardActionCF currentFeed currentSearchFeed act
        val markAllRead =
            si <- get currentFeed;
            cnt <- get si.Counters;
            let val up = cnt.TotalPosts - cnt.ReadPosts in
            ok <- (if up > 50 then
                       confirm ("Do you really want mark all "
                                ^ show up ^ " posts as read?")
                   else
                       return True);
            if not ok then return () else
                urls <- Js.getReadCounters;
(*                 urls <- getUrls True si; *)
                case urls of
                    [] => return ()
                  | _ =>
                    (msgsWidget.SetForest emptyMF;
(*                      List.app (fn si => *)
(*                                   c <- get si.Counters; *)
(*                                   updateCounters si *)
(*                                       (c.ReadPosts - c.TotalPosts) *)
(*                                       (c.ReadComments - c.TotalComments)) *)
(*                               (getSubItems si.Index); *)
                     (* правильно вычитать сохраненные Total-ы *)
                     (* сразу обнуляем ручками, обновлять SubInfo нужно
                        только при флажке auto update
                      *)
                     List.app
                         (fn (u, _, _, tp, tc) =>
                             Js.markChangedReadCounters u;
                             backgroundRpc.AddAction
                                 (BGMarkBlogRead ({ BlogFeedUrl = u
                                                  , TotalPosts = tp
                                                  , TotalComments = tc
                                                  })))
                         urls;
                     (* refresh *)reloadFeed ()
(*                      queueCRpcB backgroundRpc *)
(*                                 (fn l => rpc (bgactions l)) *)
(*                                 (fn _ => return ()) *)
(*                      (\* ^ дабы отменить возможную загрузку фида *\) *)
                    )
            end
        fun err e =
            ex <- get exiting;
            when (not ex)
                 (infoMessage.Error e;
                  backgroundRpc.OnError;
                  set msgsWidget.Appending False;
                  set msgsWidget.AppendingRpc False;
                  set msgsWidget.AfterAppendAction (return ());
                  set msgsWidget.LoadingComments False;
                  set scrollTimeoutActive False;
                  set loading False)
        fun checkAppendLoop () =
            Js.setTimeout "checkAppendLoop"
                          (msgsWidget.CheckAppend (fn _ => return ());
                           checkAppendLoop ()) 1000
        val scroll =
            s <- get msgsWidget.Scrolling;
            a <- get scrollTimeoutActive;
            if a || s > 0 then return () else
                 (set scrollTimeoutActive True;
                  Js.setTimeout "msgsWidget.CheckAppend"
                      (msgsWidget.CheckAppend
                           (fn _ =>
                               set scrollTimeoutActive False;
                               s <- get msgsWidget.Scrolling;
                               when (s <= 0) msgsWidget.OnScroll))
                      150)
        fun onUpdateSubInfo si1 si2 =
            cfu <- get currentFeedUrl;
            (case subItemUrl si2 of
              | Some u =>
                when (u = cfu)
                     (c1 <- get si1.Counters;
                      c2 <- get si2.Counters;
                      if c1.ScannedPercent <> 100 && c2.ScannedPercent = 100
                      then
                          set scannedPercent snAllCommentsScanned
                      else
                          lc <- get lastCounters;
                          when (lc.ScannedPercent <> c2.ScannedPercent
                               || lc.TotalComments <> c2.TotalComments
                               || lc.TotalPosts <> c2.TotalPosts)
                               (set refreshAvailable True;
                                set lastCounters c2))
              | _ => return ())
(*             cth <- get currentTagHash; *)
(*             when (cth = si1.Hash) *)
(*                  (c1 <- get si1.Counters; *)
(*                   c2 <- get si2.Counters; *)
(*                   if c1.ScannedPercent <> 100 && c2.ScannedPercent = 100 *)
(*                   then *)
(*                       set scannedPercent snAllTagsImported *)
(*                   else *)
(*                       lc <- get lastCounters; *)
(*                       when (lc.ScannedPercent <> c2.ScannedPercent) *)
(*                            (set lastCounters c2); *)
(*                       when (lc.TotalPosts <> c2.TotalPosts) *)
(*                            (set refreshAvailable True; *)
(*                             set lastCounters c2)) *)
        fun setListViewMode mode =
            set msgsWidget.ListViewMode mode;
            backgroundRpc.AddAction
                (BGSetListViewMode { ListViewMode = mode });
            (* refresh *)reloadFeed ()
        val toggleListViewMode =
            m <- get msgsWidget.ListViewMode;
            setListViewMode (case m of LVMCompact => LVMTwoLines
                                     | LVMTwoLines => LVMCompact)
        fun displayOr d f a =
            if d then (Js.setLocationHash f; reloadFeed ()) else a
        fun init helpBox welcomeBox =
            withSome (fn _ => Js.trackEvent "User" "Login" uid) fl;
            Js.trackEvent "UI" "MainPage" uid;
            Js.jsInit;
            Js.set_subscribeDiscoveryFeed subscribeDiscoveryFeed;
            Js.set_setDiscoveryFeed setDiscoveryFeed;
            Js.set_discoveryHide discovery.Hide;
            set subscribeDiscoveryFeedSrc subscribeDiscoveryFeed;
            set clearMtvmSrc discovery.ClearMtvm;
            changeMsgScale id;
            changeBodyScale id;
            set setFeedSrc setFeed_;
            set onUpdateSubInfoSrc onUpdateSubInfo;
            set refreshSrc (reloadFeed ());
            onConnectFail (err "Can't connect to the server");
            onDisconnect (err "Disconnected from server");
            onServerError (fn e => err ("Server error:\n" ^ e));
            onFail (fn e => err ("Failure:\n" ^ e));
            onError (fn _ => err "Error");
            ps.Setup;
            dPressed <- source False;
            setupKeydown (fn k check =>
              (* символы *)
              d <- get dPressed;
              dd <- get displayDiscovery;
              when d (set dPressed False);
              let fun fkbaOr act a =
                      if k.ShiftKey then
                          when (not dd) (feedKeyboardAction act)
                      else
                          a
                  val nextFeedOr = fkbaOr "nextFeed"
                  val prevFeedOr = fkbaOr "prevFeed"
                  val toggleFolderOr = fkbaOr "toggleFolder"
                  val parentFolderOr = fkbaOr "parentFolder"
              in
              return (
              if check (78 (* N *) :: 110 :: 1058 :: 1090 :: []) then
                  Some (nextFeedOr msgsWidget.Next)
              else if check (74 (* J *) ::106 :: 1054 :: 1086 :: []) then
                  Some (nextFeedOr msgsWidget.NextTryFull)
              else if check (80 (* P *) :: 112 :: 1047 :: 1079 :: []) then
                  Some (prevFeedOr msgsWidget.Prev)
              else if check (75 (* K *) :: 107 :: 1051 :: 1083 :: []) then
                  Some (prevFeedOr msgsWidget.PrevTryFull)
              else if check (85 (* U *) :: 117 :: 1043 :: 1075 :: []) then
                  Some (if d then selectSubscription "feed" else
                        parentFolderOr msgsWidget.Up)
              else if check (86 (* V *) :: 118 :: 1052 :: 1084 :: []) then
                  Some ((if k.ShiftKey
                         then msgsWidget.JumpToTranslate
                         else msgsWidget.JumpToLink) (fn _ => Js.openLink))
              else if check (66 (* B *) :: 98 :: 1048 :: 1080 :: []) then
                  Some ((if k.ShiftKey
                         then msgsWidget.JumpToTranslate
                         else msgsWidget.JumpToLink)
                        (fn _ => Js.openLinkInBackgroud))
              else if check (69 (* E *) :: 101 :: 1059 :: 1091 :: []) then
                  Some (trackShareAction backgroundRpc SAEMail;
                        msgsWidget.JumpToLink mailLink)
              else if check (73 (* I *) :: 105 :: 1064 :: 1096 :: []) then
                  Some (if k.ShiftKey
                        then msgsWidget.SkipComments
                        else msgsWidget.SkipPost)
              else if check (76 (* L *) :: 108 :: 1044 :: 1076 :: []) then
                  Some (if k.ShiftKey
                        then msgsWidget.Later
                        else msgsWidget.LaterPost)
              else if check (88 (* X *) :: 120 :: 1063 :: 1095 :: []) then
                  Some (toggleFolderOr msgsWidget.IgnorePost)
              else if check (71 (* G *) :: 103 :: 1055 :: 1087 ::
                             93 (* ] *) :: 1066 :: 1098 ::
                             59 (* ; *) :: 1046 :: 1078 ::
                             []) then
                  Some msgsWidget.ToggleFullText
              else if check (77 (* M *) :: 109 :: 1068 :: 1100 :: []) then
                  Some msgsWidget.ToggleRead
              else if check (79 (* O *) :: 111 :: 1065 :: 1097 :: []) then
                  Some msgsWidget.ToggleCollapsed
              else if check (49 (* 1 *) :: []) then
                  Some (ps.Hide; withCommentsVM.3)
              else if check (50 (* 2 *) :: []) then
                  Some (ps.Hide; fullVM.3)
              else if check (51 (* 3 *) :: []) then
                  Some (ps.Hide; magazineVM.3)
              else if check (52 (* 4 *) :: []) then
                  Some (ps.Hide; mosaicVM.3)
              else if check (53 (* 5 *) :: []) then
                  Some (ps.Hide; shortVM.3)
              else if check (54 (* 6 *) :: []) then
                  Some (ps.Hide; toggleListViewMode)
              else if check (48 (* 0 *) :: []) then
                  Some (ps.Hide; combinedVM.3)
              else if check (83 (* S *) :: 115 :: 1067 :: 1099 :: []) then
                  Some (displayOr d "starred" msgsWidget.ToggleStarred)
              else if check (84 (* T *) :: 116 :: 1045 :: 1077 :: []) then
                  Some (if d then selectSubscription "tag" else
                        msgsWidget.EditTags)
              else if check (47 (* / *) :: 46 (* . *) :: []) then
                  Some (Js.select searchBoxId; Js.focus searchBoxId)
              else if check (70 (* F *) :: 102 :: 1040 :: 1072 :: []) then
                  Some (if d then selectSubscription "folder" else
                        toggle fullscreen)
              else if check (65 (* A *) :: 97 :: 1060 :: 1092 :: []) then
                  Some (displayOr d ""
                        (if k.ShiftKey
                         then markAllRead
                         else toggleDiscovery False))
              else if check (82 (* R *) :: 114 :: 1050 :: 1082 :: []) then
                  Some (ssWidget.UpdateSubscriptions False;
                        (* refresh *)reloadFeed ())
              else if check (68 (* D *) :: 100 :: 1042 :: 1074 :: []) then
                  Some (if d then selectSubscription "subscription" else
                        set dPressed True)
              else if check (63 (* ? *) :: 72 (* H *) :: 104 :: 1056 :: 1088 :: [])(*  || (k.ShiftKey && k.KeyCode = 191 (\* для firefox *\)) *) then
                  Some (trackHelpClick;
                        ps.Toggle popup helpBox.2)
              else if check (45 (* - *) :: []) then
                  Some (changeMsgScale decScale)
              else if check (61 (* = *) :: []) then
                  Some (changeMsgScale incScale)
              else if check (95 (* _ *) :: []) then
                  Some (changeBodyScale decScale)
              else if check (43 (* + *) :: []) then
                  Some (changeBodyScale incBodyScale)
              else
                  None)
              end) (fn k check =>
              (* специальные клавиши *)
              return (
              if check (27 :: []) then
                  Some (a <- ps.IsActive;
                        dd <- get displayDiscovery;
                        if a then
                            ps.Hide
                        else if dd then
                            discovery.Hide
                        else msgsWidget.TryMakeShort)
              else if check (32 :: []) then
                  Some (if k.ShiftKey then msgsWidget.PrevOrPageUp else msgsWidget.NextOrPageDown)
              else if check (13 :: []) then
                  Some msgsWidget.ToggleFull
              else if check (33 (* PgUp *) :: []) then
                  Some msgsWidget.PageUp
              else if check (34 (* PgDown *) :: []) then
                  Some msgsWidget.PageDown
              else if check (36 (* Home *) :: 35 (* End *) :: []) then
                  Some (return ())
              else if check (38 (* Up *) :: []) then
                  Some msgsWidget.LineUp
              else if check (40 (* Down *) :: []) then
                  Some msgsWidget.LineDown
              else
                  None));

            queueRpcB backgroundRpc (fn l => rpc (subscriptionsAndSettings l))
                (fn (x,siv, s,(ous, ti, renames,searches,us), ws) =>
                    set onlyUpdatedSubscriptions ous;
                    set welcomeState ws;
                    set msgsWidget.ScrollMode us.ScrollMode;
                    set msgsWidget.ListViewMode us.ListViewMode;
                    set msgsWidget.UltraCompact us.UltraCompact;
                    set msgsWidget.MarkReadMode us.MarkReadMode;
                    let val co = Option.get "-" us.Country in
                        set discovery.Country
                            (if Js.countryNameFromCountryCode co =
                                "International" then "-" else co)
                        (* бывают всякие EU/A1, которые должны быть "-" *)
                    end;
                    set ssWidget.PublicFeeds (Option.get [] us.PublicFeeds);
                    Js.setExactUnreadCounts us.ExactUnreadCounts;
                    set exactUnreadCounts us.ExactUnreadCounts;
                    set login us.MobileLogin;
                    Js.setupSearchAutocomplete searchBoxId searches search;
                    ssWidget.UpdateSubscriptions_ False (x, siv, s, ti, renames));
            set loading False;

            (* ^ может так не будет вылезать
                 "Unknown Ur expression kind undefined"? *)
            c <- get (getSubItem 0).Counters;
            if c.Feed = 0 && c.Error = 0 && c.Scanning = 0 then
                 (case paidTill of
                    | PTFreeTrialFinished _ => return ()
                    | PTPaidFinished _ => return ()
                    | _ =>
                      ps.ToggleNoPrevent popup welcomeBox;
                      toggleDiscovery True
                 );
                Js.setTimeout "loadGA"
                              (Js.loadGoogleAnalytics True) 0
            else
                Js.setTimeout "onhashchange"
                              (onhashchange ();
                               Js.loadGoogleAnalytics True) 0
        val clearSubscriptions =
            c <- confirm "Are you really sure you want to unsubscribe from all your subscriptions?";
            when c
                 (Js.trackEvent "UI" "ClearSubscriptions" uid;
                  backgroundRpc.AddAction BGClearAllSubscriptions;
                  backgroundRpc.OnUnload;
                  Js.reloadPage)
    in
    msgMenu <- ps.NewMenu Css.msgMenu <xml>
      <dyn signal=
           {vm <- msgTreeViewMode;
            sr <- signal searchResults;
            let val (sn,so) = if vm.Ascending then (Css.btnEmpty, Css.btnCheck) else (Css.btnCheck, Css.btnEmpty)
                val (vn,va) = if vm.UnreadOnly then (Css.btnCheck, Css.btnEmpty) else (Css.btnEmpty, Css.btnCheck)
            in
                return <xml>
                  {if not vm.UnreadOnly then
                       ps.LiI Css.btnEmpty "Show new" (setUnreadOnly True)
                   else
                       ps.LiI Css.btnEmpty "Show all" (setUnreadOnly False)}
                  {if Option.isSome sr then
                       <xml/> (* у поиска не будет сортировки
                                 и пометки всех прочитанными *)
                   else <xml>
                     <hr/>
                     {ps.LiI sn "Sort by newest" (setAscending False)}
                     {ps.LiI so "Sort by oldest" (setAscending True) : xbody}
(*                      <hr/> *)
(*                      {p.LiI Css.btnEmpty "Mark all as read" markAllRead : xbody} *)
                   </xml>}
                </xml>
            end}  />
      <dyn signal=
           {si <- signal currentFeed;
            return (case ssWidget.SubItemMenuContents False si of
              | Some m => <xml><hr/>{m}</xml>
              | _ => <xml/>)
           } />
    </xml>;
    helpBox <- ps.NewInfoBox "Help" (helpText ssWidget.AddSubscription);
    recommendBox <- ps.NewInfoBox "Recommend" <xml>
      <p>Like the reader? Please spread the word:</p>
      <p>{hrefLinkStopPropagation <xml>Vote</xml> "http://alternativeto.net/software/bazqux-reader/"} on alternativeto.net<br/>
      {hrefLinkStopPropagation <xml>Tweet</xml> "https://twitter.com/intent/tweet?text=I'm%20going%20to%20%23replacereader%20with%20%23BazQuxReader&url=https%3A%2F%2Fbazqux.com"} on replacereader.com<br/>
      {hrefLinkStopPropagation <xml>Post</xml> "https://plus.google.com/share?url=https%3A%2F%2Fbazqux.com"} on Google+<br/>
      {hrefLinkStopPropagation <xml>Share</xml> "https://www.facebook.com/sharer/sharer.php?u=https%3A%2F%2Fbazqux.com"} on Facebook<br/>
      {hrefLinkStopPropagation <xml>Mail</xml> "mailto:?subject=BazQux%20Reader&body=Try%20this%20great%20RSS%20reader%20https%3A%2F%2Fbazqux.com"} a friend<br/></p>
      {likeButtons}
    </xml>;
    scrollingMenu <- ps.NewMenu Css.scrollingMenu
      (dyn_ (sm <- signal msgsWidget.ScrollMode;
             let fun smItem mode name =
                     ps.LiI (if mode = sm then Css.btnCheck else Css.btnEmpty)
                            name
                            (set msgsWidget.ScrollMode mode;
                             backgroundRpc.AddAction
                                 (BGSetScrollMode { ScrollMode = mode }))
             in
                 return <xml>
                   {smItem SMNormal "Normal"}
                   {smItem SMQuick  "Quick"}
                   {smItem SMImmediate "Immediate"}
                 </xml>
             end));
    listViewMenu <- ps.NewMenu Css.scrollingMenu
      (dyn_ (sm <- signal msgsWidget.ListViewMode;
             uc <- signal msgsWidget.UltraCompact;
             let fun item mode name =
                     ps.LiI (if mode = sm then Css.btnCheck else Css.btnEmpty)
                            name
                            (setListViewMode mode)
                 fun itemc mode c name =
                     ps.LiI (if mode = sm && c = uc
                             then Css.btnCheck else Css.btnEmpty)
                            name
                            (set msgsWidget.UltraCompact c;
                             backgroundRpc.AddAction
                                 (BGSetUltraCompact { UltraCompact = c });
                             setListViewMode mode)
             in
                 return <xml>
                   {itemc LVMCompact True "Ultra-compact"}
                   {itemc LVMCompact False "Compact"}
                   {item LVMTwoLines "Normal"}
                 </xml>
             end));
    markReadMenu <- ps.NewMenu (classes Css.markReadMenu Css.scrollingMenu)
      (dyn_ (sm <- signal msgsWidget.MarkReadMode;
             let fun item mode name =
                     ps.LiI (if mode = sm then Css.btnCheck else Css.btnEmpty)
                            name
                            (set msgsWidget.MarkReadMode mode;
                             backgroundRpc.AddAction
                                 (BGSetMarkReadMode { MarkReadMode = mode }))
             in
                 return <xml>
                   {item MRMOnScrollEverywhere "On scroll"}
                   {item MRMOnScroll "On scroll & click in List view"}
                   {item MRMManual "On click"}
                 </xml>
             end));
    optMenu <- ps.NewMenu Css.optMenu <xml>
      {ps.Li "Help" (trackHelpClick;
                    ps.Toggle popup helpBox.2)}
(*       {ps.LiInfoBox popup helpBox : xbody} *)
      <dyn signal={return
      (ps.LLi "Feedback"
              ("mailto:hello@bazqux.com?subject=[Feedback]&body="
               ^ Js.encodeURIComponent ("\n-----\nUser ID:\n" ^ uid)))} />
      {ps.LLi "UserVoice" "http://bazqux.uservoice.com"}
      {ps.LiInfoBox popup recommendBox : xbody}
      <hr/>
      {ps.Li "Mobile login" toggleMobileLoginBox}
      <span class="subscriptionsMenuItem">
         {ssWidget.SubMenu.2}
         {ps.LiSub "Subscriptions"}
      </span>
      <span class="scrollingMenuItem">
         {scrollingMenu.2}
         {ps.LiSub "Transitions"}
      </span>
      <span class="scrollingMenuItem">
         {listViewMenu.2}
         {ps.LiSub "List view"}
      </span>
      <span class="scrollingMenuItem">
         {markReadMenu.2}
         {ps.LiSub "Mark read"}
      </span>
      <hr/>
      {ps.Li "Sign out" (Js.trackEvent "User" "Logout" uid;
                        set exiting True;
                        redirect (effectfulUrl sign_out)) : xbody}
    </xml>;
    feed <- source "";
    twitter <- source "";
    facebook <- source "";
    gplus <- source "";
    welcomeBox <- ps.New Css.welcomeBox (dyn_ (wso <- signal welcomeState;
      return (case wso of None => <xml/> | Some ws => <xml>
      <h1>Welcome{[if ws.HasPrevAccount then " back" else ""]}!</h1>
(*       <p>To start using BazQux Reader please</p> *)
(*       <p>{linkButton "import your subscriptions" greaderImportClick} from Google Reader</p> *)
      {displayIfC ws.HasPrevAccount
       <xml><p>Your previous free trial has expired more than a month ago. New free trial has just started!</p></xml>}
      {displayIfC (ws.StarredRestored || ws.TaggedRestored)
       <xml><p>We have restored your
         {[case (ws.StarredRestored, ws.TaggedRestored) of
             | (True, True) => "starred and tagged"
             | (True, False) => "starred"
             | (False, True) => "tagged"
             | _ => ""
         ]} items.</p></xml>}
      {let val importOpml = dyn_ (return (Js.opmlForm (linkButton "import an OPML file" (stopPropagation; opmlUploadClick))))
           val restore =
               ps.Hide;
               set loading True;
               showInfo Css.restoring "Restoring..." infoMessage
               (x <- rpc (restoreSubscriptions []);
                set loading False;
                ssWidget.UpdateSubscriptions_ False x;
                discovery.Hide);
               refresh
       in
           if ws.HasPrevSubs then <xml>
             <p>You can {linkButton "restore previous subscriptions" restore} or {importOpml}</p>
             <p>Or just search for sites you love to read and add them to the reader.</p>
           </xml>
           else <xml>
             <p>Search for sites you love to read and add them to the reader.</p>
             <p>You can also {importOpml}</p>
           </xml>
       end
      }
(*       <p class="howToImportMyFeeds">{hrefLinkStopPropagation <xml>How to get my OPML file?</xml> "http://bazqux.uservoice.com/knowledgebase/articles/282171"}</p> *)
(*       <p></p> *)
(*       <p>Hint: You can download your OPML from {hrefLinkStopPropagation (txt "Feedly") "http://cloud.feedly.com/#opml"}, {hrefLinkStopPropagation (txt "The Old Reader") "https://theoldreader.com/reader/subscriptions/export"} or {hrefLinkStopPropagation (txt "NewsBlur") "http://newsblur.com/import/opml_export"}.</p> *)
(*       <p>or <a href={bless "/importFromGoogleReader"} *)
(*               onclick={fn _ => redirect (effectfulUrl importFromGoogleReader)}> *)
(*           import your subscriptions from Google Reader</a *)
(*          </p> *)
    </xml>)));

    return
        { Oninit =
            showInfo null "Loading..." infoMessage
                             (init helpBox welcomeBox)
        , Onunload = set exiting True; backgroundRpc.OnUnload
        , Onresize =
            msgsWidget.CheckAppend (fn _ => return ());
            Js.adjustMenuHeight Css.foldersMenu
        , Onhashchange = preventDefault; onhashchange ()
        , Xml = <xml>
      <span dynClass={
          s <- signal bodyScale;
          f <- signal fullscreen;
          return (ifClass f Css.fullscreen (classes s Css.bodyScaler))}>
      <div class="top">
        <div class="logoContainer"><a link={main ()} class={Css.logo}>{logo ""}
        </a></div><span class="searchToolBar"
          (* Так можно сделать autocomplete
             http://stackoverflow.com/questions/8400269/browser-native-autocomplete-for-ajaxed-forms
           *)
          ><ctextbox source={searchQuery} (* placeholder="Search..." *)
              id={searchBoxId}
              onkeydown={fn k => if k.KeyCode = 13 then search else
                                 if k.KeyCode = 27 then stopPropagation; preventDefault (* без этого в Safari все равно вызывается обработчик по-умолчанию *); Js.blur searchBoxId
                                 else return()}
          /><a onclick={fn _ => search} class={Css.button}>Filter</a>(* {buttonChar "D"} *)
        </span>
        <span class="topToolBar">
          {let fun buyButton mode t =
                   let val ds = diffInSeconds curTime t
                       val days = min (ds / 86400 + 1) 30
                       val (c,text) =
                           if ds < 0 then
                               (Css.unpaid, mode ^ " finished. ")
                           else
                               (if days <= 7 then Css.weekLeft
                                else Css.nDaysLeft,
                                show days ^ " day" ^
                                (if days = 1 then "" else "s") ^ " left. ")
                   in <xml>
                     <span class="buyNow">
                       (* <span class={c}>{[text]}</span> *)
                       {textButton (text ^ "Buy now!") buyClick}
                     </span></xml>
                   end
           in
               case paidTill of
                 | PTUnknown => <xml/>
                 | PTFreeTrial { Till = t } => buyButton "free trial" t
                 | PTFreeTrialFinished { Till = t } => <xml/>
                 | PTPaidFinished { Till = t } => <xml/>
                 | PTPaid { Till = t } =>
                   if diffInSeconds curTime t < 15*86400 then
                       buyButton "subscription" t
                   else <xml/>
           end
          }
          {symbolButton "Options" Css.btnOptions "" (ps.Toggle popup optMenu)}
(*           {button Css.btnOptions "" (ps.Toggle popup optMenu)} *)
        </span>
      </div>
      <div class="subToolBar">
        {button Css.btnAddSubscription "Add subscription" (toggleDiscovery False)}
      </div>
      <div class="msgToolBar">
        <span class="msgComboBox">
          <dyn signal={
             si <- signal currentFeed;
             cnt <- signal si.Counters;
             vm <- msgTreeViewMode;
             return <xml>
               <a onclick={fn _ => ps.Toggle popup msgMenu;
                              Js.adjustMenuHeight Css.foldersMenu}
                  class={Css.button}><span class={Css.buttonText}>
                  {if vm.UnreadOnly then
                       <xml>{[Js.showUnread
                                  cnt vm.ExpandedComments]}&nbsp;New</xml>
                  else <xml>All items</xml>}</span>{buttonSymbol Css.btnDown}
               </a>
             </xml> } />
          <dyn signal={
             si <- signal currentFeed;
             vm <- msgTreeViewMode;
             isTag <- isTagSiS si;
             let fun vmBtn (cls, active, setvm, name) sep hint =
                    <xml><span class={ifClass sep rightSeparator
                                      (ifClass (if isTag then
                                                    (if hint = "0" then
                                                         vm.NoOverride
                                                     else
                                                         vm.NoOverride = False
                                                         && active vm)
                                                else
                                                    active vm) vmButtonActive
                                               vmButton)}
                               title={name ^ ". \nKeyboard shortcut: '" ^ hint ^ "'"}
                               onclick={fn _ => Js.forceImpure ps.Hide; setvm}
                         ><span class={cls}></span></span></xml>
             in
             return <xml>
               <span class="vmButtons">
                 {vmBtn withCommentsVM True "1"}{
                  vmBtn fullVM True "2"}{
                  vmBtn magazineVM True "3"}{
                  vmBtn mosaicVM True "4"}{
                  vmBtn shortVM isTag "5"}{
                  if isTag then vmBtn combinedVM False "0" else <xml/>
                 }
               </span>
             </xml> end } />
          {buttonT Css.btnCheck "Mark all as read" "Mark all messages and comments as read" markAllRead}
        </span>
(*         {buttonT "w" "Mark all as read" "Mark all messages and comments as read" *)
(*          markAllRead} *)
        {
         buttonT' Css.buttonLeft   Css.btnSkip "Skip" "Mark current post and its comments read and view next post. \nKeyboard shortcut: 'i'" msgsWidget.SkipPost
         }{
        buttonT' Css.buttonRight  Css.btnIgnore "Ignore" "Skip current post and do not show new comments for it. \nKeyboard shortcut: 'x'" msgsWidget.IgnorePost}
(* подумать еще над расположением кнопок
 *)
         {buttonT2' ((* classes Css.buttonLeft *) Css.buttonReadability) Css.btnReadabilityBW "Readability" "Get full post text with Readability. \nKeyboard shortcut: 'g'" msgsWidget.ToggleFullText}
         {
(*          buttonT' Css.buttonLeft   Css.btnUp "Up" "Parent message. \nKeyboard shortcut: 'u'" msgsWidget.Up}{ *)
         buttonT' Css.buttonLeft Css.btnPrev "Prev" "Previous message. \nKeyboard shortcut: 'k'" msgsWidget.PrevTryFull}{
         buttonT' Css.buttonRight  Css.btnNext "Next" "Next message. \nKeyboard shortcut: 'j'" msgsWidget.NextTryFull}
      </div>
      <div dynClass={Monad.mp (classes Css.left)
                              (ifS displayDiscovery Css.displayNone Css.displayBlock)}>
(*       <div class="left"> *)
        <div dynClass={Monad.mp (classes Css.subscriptions)
                                (ous <- signal onlyUpdatedSubscriptions;
                                 return (if ous
                                         then Css.onlyUnreadSubscriptions
                                         else null))}>
          <div class={Css.dragMark} id={dragMarkId}>
            <div class={Css.dragMarkFill}></div>
          </div>
          {ssWidget.Html}
        </div>
      </div>
      {discovery.Html}
      <div id={msgDivId} class="right"
        onscroll={scroll}>
        <div id={innerMessagesId}
             dynClass={s <- signal msgScale;
                       return (classes s Css.innerMsgs)} >
        <div class="msgsPadder">
        <div class={Css.subscriptionTitle}>
          {dyn_ (f <- signal currentFeed;
                 return (case f.SIType of
                           | SITFeed feed =>
                             (case feed.FeedLink of
                                | Some l =>
                                  textWithLink (Some (bless l)) (txt (f.Title ^ " »"))
                                | None => txt f.Title)
                           | _ => txt f.Title))}
        </div>
        {dyn_ (du <- discoverySubItemUrl currentFeed;
               return (case du of
                 | Some u => <xml><div class={Css.subscribeDiscoveryFeed}>
                    {textButton "Subscribe" (subscribeDiscoveryFeed u)}
                 </div></xml>
                 | None => <xml/>))}
        {snInfoMsg}
        {dyn_ (signal scannedPercent)}
        <dyn signal=
             {sr <- signal searchResults;
              return (case sr of
                  None => <xml/>
                | Some sr => <xml>
(*                   <div class="searchFound"> *)
(*                     Found {[sr.Total]} *)
(*                   </div> *)
                    (* {[if sr.Total > 100 then "100+" else show sr.Total]} *)
                  <div class="searchTime">Found {[sr.Total]} ({[sr.TookReal]} ms)</div></xml>)
             } />
        {msgsWidget.Html}
        <div class="clearBoth"></div>
        </div>
        </div>
        <dyn signal={signal sharePopup}/>

        <div dynClass=
            {emf <- msgsWidget.IsForestEmpty;
             ar <- signal msgsWidget.AppendRequests;
             a <- signal msgsWidget.AppendingRpc;
             return (ifClass (isNull ar && not a) Css.noMoreItemsText
                    (ifClass emf Css.emptyMsgForest Css.msgTreeSpacer))
            }>
            {dyn_ (signal msgTreeSpacerText)}
        </div>
      </div>
      {infoMessage.Html}
      <dyn signal={signal popup}/>
      {case paidTill of
         | PTFreeTrialFinished _ =>
           <xml><div class="blackout"></div><div class="freeTrialFinishedBox">
             <h1>Hello</h1>
             <p>Your 30 days free trial has expired. Subscribe to keep reading all the interesting posts and discussions in your favorite blogs!</p>
             <p class="buyNowLink">
               {linkButton "Buy now!" buyClick}
             </p>
             <p class="signOutLink">
               <a link={opml ()}>Export OPML</a> ·
               <a link={deleteAccount}>Delete account</a> ·
               {linkButton "Sign out" (redirect (effectfulUrl sign_out))}
             </p>
           </div></xml>
         | PTPaidFinished _ =>
           <xml><div class="blackout"></div><div class="freeTrialFinishedBox">
             <h1>Hello</h1>
             <p>Your year subscription has expired. Subscribe to keep reading all the interesting posts and discussions in your favorite blogs!</p>
             <p class="buyNowLink">
               {linkButton "Buy now!" buyClick}
             </p>
             <p class="signOutLink">
               <a link={opml ()}>Export OPML</a> ·
               <a link={deleteAccount}>Delete account</a> ·
               {linkButton "Sign out" (redirect (effectfulUrl sign_out))}
             </p>
           </div></xml>
         | _ => <xml/>}
    </span></xml> }
    end end end
    fun runSrc s = h <- get s; withSome id h
    in
        inner <- source <xml/>;
        unload <- source None;
        resize <- source None;
        hashchange <- source None;
        conv <- (if Option.isNone fl then return <xml/> else
                 htmlConversionLogin ());
        pageNoBody' "" headMain "BazQux Reader" <xml>
          <body
             class={classes Css.noTouch Css.mainPage}
             onload={p <- mainPage ();
                     set unload (Some p.Onunload);
                     set resize (Some p.Onresize);
                     set hashchange (Some p.Onhashchange);
                     set inner p.Xml;
                     p.Oninit}
             onunload={runSrc unload}
             onresize={runSrc resize}
             onhashchange={runSrc hashchange}
             >
             {dyn_ (signal inner)}
             {conv}
        </body>
        </xml>
    end


and buy r = withUser "buy" (fn userId =>
    case r.Option of
      | Some pid =>
        l <- buyLink pid userId;
        redirect (bless l)
      | None => error <xml>No payment option selected</xml>) []

and addSubscriptions src f : transaction page =
    withUser src (fn userId =>
    f userId;
    redirectToMain) []

and importOPML r : transaction page =
    addSubscriptions "importOPML" (opmlSubscriptions (fileData r.OPML))
and importOPML_ () : transaction page =
    page "Import OPML" <xml>
      <form>
        <p>Upload OPML<br/>
          <upload{#OPML}/><br/>
          <submit action={importOPML}/>
        </p>
      </form>
    </xml>

and importingFromGoogleReader (qs : option queryString) : transaction page =
    case qs of
        None => error <xml>Empty query string for import callback</xml>
      | Some qs =>
        h <- getHost;
        addSubscriptions "importingFromGoogleReader"
                         (importFromGoogleReaderCallback h
                              (show (effectfulUrl importingFromGoogleReader))
                              (show qs))

and importFromGoogleReader _ : transaction page =
    h <- getHost;
    u <- importFromGoogleReaderGetForwardUrl h
             (effectfulUrl importingFromGoogleReader);
    redirect u

and importingStarredAndTaggedItemsFromGoogleReader (qs : option queryString) : transaction page =
    case qs of
        None => error <xml>Empty query string for import callback</xml>
      | Some qs =>
        h <- getHost;
        withUser "importingStarredAndTaggedItemsFromGoogleReader" (fn userId =>
        importStarredAndTaggedItemsFromGoogleReaderCallback h
            (show (effectfulUrl importingStarredAndTaggedItemsFromGoogleReader))
            (show qs) userId;
        redirect (bless "/#starred")) []

and importStarredAndTaggedItemsFromGoogleReader _ : transaction page =
    h <- getHost;
    u <- importFromGoogleReaderGetForwardUrl h
             (effectfulUrl importingStarredAndTaggedItemsFromGoogleReader);
    redirect u

fun facebookTokenCallback (qs : option queryString) : transaction page =
    case qs of
        None => error <xml>Empty query string for sign in callback</xml>
      | Some qs =>
        h <- getHost;
        u <- fbTokenCallback h (effectfulUrl facebookTokenCallback) (show qs);
        return <xml>{[u]}</xml>

val facebookToken : transaction page =
    h <- getHost;
    u <- fbTokenGetForwardUrl h (effectfulUrl facebookTokenCallback);
    redirect u

task initialize = fn () =>
    Ur_ffi.init;
    initMailer ();
    initApiServer ();
    return ()

fun order_completed oid : transaction page =
    p <- checkOrder oid;
    redirectToMain

fun check_order oid : transaction page =
    pm <- checkOrder oid;
    infoPage "Check order" <xml>
      <p>Order {[oid]} processed:
        <div class="errorText">{case pm of
          | PReserved => <xml/>
          | PFastSpring fs => <xml>
            type: {[fs.OrderType]}<br/>
            time: {[fs.OrderTime]}
          </xml>
        }</div></p>
    </xml>

fun order_notification (pb : postBody) : transaction page =
(*     debug ("postType: " ^ postType pb); *)
(*     debug ("postData: " ^ postData pb); *)
        (* TODO: top.ur:postFields проверяет "application/x-www-form-urlencoded", а приходит "application/x-www-form-urlencoded; charset=UTF-8" *)
        (* а еще Invalid escaped URL byte starting at: .&amp *)
    p <- orderNotification (postData pb);
    return <xml/>

val activeImports : transaction page =
    c <- activeGRImportNames ();
    infoPage "Active imports" <xml>
      <p>Active imports:
        <div class="errorText">{c}</div></p>
    </xml>

val getUserIdBySession : transaction page =
    s <- getHeader (blessRequestHeader "Session");
    case s of
       | Some k =>
         fu <- tryGetFeverUser k;
         (case fu of
             | Some u =>
               returnBlob (textBlob u) (blessMime "text/plain")
             | None =>
               u <- getUserBySession False k;
               returnBlob (textBlob (Option.get "" u)) (blessMime "text/plain"))
       | _ =>
         error <xml>No Session header specified</xml>
