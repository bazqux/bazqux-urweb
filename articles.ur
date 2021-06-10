open Css
open Uim
open Share
open Feeds
structure P = Popups

val noTransitions = Unsafe.boolSource "noTransitions" False
val lastScrollTop = Unsafe.floatSource "Articles.lastScrollTop" 0.0
    (* установка lastScrollTop = scrollTop отменяет onScroll при выборе фида,
       и после прокрутки на выбранное сообщение
     *)

con mw =
    {FullUIM : source (option uim),
     SelectedUIM : source (option uim),
     Select : bool -> bool -> uim -> transaction {},
     Blocked : source bool,
     ToggleCollapsed : transaction {} -> uim -> transaction {},
     ToggleRead : bool -> uim -> transaction {},
     ToggleStarred : uim -> transaction {},
     RemoveITTag : string -> uim -> transaction {},
     MarkRead : uim -> transaction {},
     TryMarkRead : uim -> transaction {},
     MarkIfDown : float -> option uim -> uim -> transaction {},
     CheckAppend : transaction {},
     UpdateFeedMark : transaction {},
     SaveSelectedUIMPosition : transaction {},
     RestoreSelectedUIMPosition : transaction {}
    }

ffi setmw effectful : mw -> bool
ffi getmw jsFunc "mw" effectful : {} -> mw

fun scrollTo t a =              (* select/toggleFull/toggleFullText *)
    sm <- Settings.getScrollMode;
    o <- msgsScrollTopOffset;
    Js.scrollTo Settings.msgDivId sm (t-o) (Pager.tryScroll pager; a)

val preventOnScroll =
    st <- msgsScrollTop;
    set lastScrollTop st

fun setScrollTop ot =
    o <- msgsScrollTopOffset;
    Js.setScrollTop Settings.msgDivId (ot-o)

fun setScrollTopNoOnscroll ot =
    setScrollTop ot;
    preventOnScroll

fun fitCompact mw (UIM uim) =
    flh <- uimFeedLabelHeight (UIM uim);
    ot <- uimTop (UIM uim);
    st <- msgsScrollTop;
    if ot < st then setScrollTopNoOnscroll ot
    else
        otf <- Pager.idPositionTop pager uim.FrameId;
        vis <- Pager.isIdOnVisiblePage pager uim.FrameId;
        h <- msgsClientHeight;
        chf <-
            (if not vis then return (h*0.67) else
             Pager.idClientRectHeight pager uim.FrameId);
        when (otf+chf > st+(h-flh))
             (setScrollTopNoOnscroll (if chf >= h-flh then ot else otf+chf-h))

fun toggleFull select (mw : mw) (UIM uim) =
    compact <- isCompact (UIM uim);
    vis <- Pager.isIdOnVisiblePage pager uim.FrameId;
    if not compact && not vis then
        return ()
        (* Не меняем размер невидимых сообщений, т.к. не сможем
           корректно обновить размер страницы pager-а.
           В большинстве случаев сообщение будет видно,
           но, если pager не успел обновить видимость, и пользователь
           нажал enter/escape возможно разворачивание невидимого сообщения.
           См. также toggleCollapsed
         *)
    else
    Pager.resetPageHeight pager uim.FrameId;
    let fun collapse andSelect (UIM uim) m =
            let val afterScroll =
                Js.saveLoadedImgSizes uim.FrameId;
                withScrollSaved Settings.msgDivId
                    (set uim.Mv (MVShort { Header = msgHeader m
                                         , CachedMsg = Some m }));
                when compact (Pager.restoreIdHeight pager uim.FrameId);
                f <- get mw.FullUIM;
                (case f of
                   | Some (UIM uimf) => when (uimf.FrameId = uim.FrameId)
                                             (set mw.FullUIM None)
                   | _ => return ());
                when andSelect
                     (snapToIfAbove (UIM uim);
                      preventOnScroll;
                      (* чтобы в mosaic не выделял первое сообщение в строке *)
                      select);
                mw.CheckAppend
                (* перед разворачиванием сообщение может быть внизу,
                   а после сворачивания уже наверху, но при этом scroll-а нет
                 *)
            in
                c <- get uim.Collapsed;
                vm <- viewModeByMsgKey m.Key;
                case (c, uim.Depth, vm.Posts) of
                  | (_,_, PVMFull) => afterScroll
                  | (Expanded, 0, _ (* PVMMosaic *)) =>
                    (getmw ()).ToggleCollapsed afterScroll (UIM uim)
                    (* во всех коротких видах сворачивание поста
                       сворачивает комменты *)
                  | _ => afterScroll
            end
        val afterScroll =
            set mw.Blocked False;
            select;
            r <- get uim.Read;
            k <- get uim.KeepUnread;
            when (not r && not k) (mw.ToggleRead False (UIM uim))
        fun setFull uim m =
            set mw.Blocked (not compact);
            when compact (Pager.saveIdHeight pager uim.FrameId);
            withScrollSaved Settings.msgDivId
                (set uim.Mv (MVFull { Msg = m }));
            (* ^ FF 66 прокручивает вниз при раскрытии magazine *)
            if isResult uim.Mi then
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
                    fitCompact mw (UIM uim);
                    afterScroll
                 else
                     sel0 <- get mw.SelectedUIM;
                     t <- uimTop (UIM uim);
                     scrollTo t
                              (mw.MarkIfDown t sel0 (UIM uim);
                               afterScroll)
                )
            else
                afterScroll
    in
    mv <- get uim.Mv;
    case mv of
      | MVFull { Msg = m } =>
        collapse True (UIM uim) m
      | MVShort s =>
        (case s.CachedMsg of
           | Some m => setFull uim m
           | None =>
             queueCancellableRpc
                 (fn l => rpc (Rpcs.msg (uimMsgKey (UIM uim)) l))
                 (fn mo =>
                     case mo of
                        | Some m => setFull uim m
                        | None   => return ())
        )
    end

con fragmentPosition
  = { Id : option Basis.id
    , Offset : option float }

fun scrollToFragment (UIM uim) pos =
    (* делаем сообщение видимым и развернутым *)
    vis <- Pager.isIdOnVisiblePage pager uim.FrameId;
    when (not vis)
         (snapTo 0.0 (UIM uim);
          Pager.tryScroll pager);
    mv <- get uim.Mv;
    (case mv of
       | MVShort s =>
         toggleFull (return ()) (getmw ()) (UIM uim)
       | _ => return());
    (* и прокручиваем на #элемент *)
    case pos of
      | { Id = Some id, ... } =>
        ft <- Js.boundingClientRectTop uim.FrameId;
        it <- Js.boundingClientRectTop id;
        snapTo (it - ft) (UIM uim)
      | { Offset = Some o, ... } =>
        snapTo o (UIM uim)
      | _ => return ()

fun uimScrollOffset u =
    t <- uimTop u;
    st <- msgsScrollTop;
    return (st - t)

fun grOrigin acc as =
    case as of
      | (AGrOrigin o) :: as' => (Some o, List.revAppend acc as')
      | a :: as' => grOrigin (a :: acc) as'
      | [] => (None, List.rev acc)

fun grOriginLinkText fmt o = fmt <xml>{txt o.StreamTitle} <span class={Css.fromFeedImported}>{[if o.Guid <> "" then "(imported)" else "(unsubscribed)"]}</span></xml>
fun grOriginLink fmt o = clsHrefLink Css.fromFeedFolder (grOriginLinkText fmt o) o.HtmlUrl

ffi uimOnClick : css_class -> (uim -> transaction {}) -> transaction {}
ffi uimOnMiddleclick : css_class -> (uim -> transaction {}) -> transaction {}
ffi uimOnContextmenu : css_class -> (uim -> transaction {}) -> transaction {}
ffi uimOnMiddlemousedown : css_class -> (uim -> transaction {}) -> transaction {}
ffi uimOnLongtap : css_class -> (uim -> transaction {}) -> transaction {}
ffi registerScrollToFragment : (uim -> fragmentPosition -> transaction {}) -> transaction {}
ffi registerUimScrollOffset : (uim -> transaction float) -> transaction {}
ffi set_showUimContextMenu : (uim -> P.position -> transaction {}) -> transaction {}

fun toggleFull' uim =
    toggleFull ((getmw ()).Select False False uim) (getmw ()) uim

fun isShort mv =
    case mv of
      | MVShort _ => True
      | _ => False
fun expandable mtvm (UIM uim) mv =
    isShort mv ||
    (uim.Depth = 0 && (case mtvm.Posts of PVMFull => False | _ => True))

fun msgFrameMiddleclick uim =
    hasLink <- Js.eventTargetHasLink;
    when hasLink ((getmw ()).MarkRead uim)

fun msgFrameClick uim =
    hasLink <- Js.eventTargetHasLink;
    if hasLink then
        (getmw ()).MarkRead uim
    else
    pa <- P.isActive;
    if pa then
        P.hide (* чтобы не сворачивал пост, если хочется убрать меню *)
    else
    (* Google Reader фокусировал сообщение при щелчке по ссылке,
       но это мешает работе #anchor-ссылок (перемещаемся на начало
       сообщения, а не ссылку). Для обычных ссылок тоже неудобно,
       т.к. открывается новая вкладка, а в фоне незаметно происходит
       прокрутка и непонятно где мы, когда возвращаемся на вкладку.
     *)
    mm <- P.mouseMovedAfterMouseDown;
    when (not mm) (
    mv <- get (uimMv uim);
    cls <- Js.eventTargetClasses;
    mtvm <- uimViewMode uim;
    let val expandable = expandable mtvm uim mv in
    e <- Js.uw_mouseEvent;
    if expandable && (elem "postHeader" cls || elem "magazineImage" cls || elem "mosaicImage" cls)
    then
        if e.ShiftKey then
            (getmw ()).ToggleRead True uim
        else
            toggleFull' uim
    else if expandable &&
            (isShort mv || not (elem "msgCollapseMargin" cls))
            (* все, что внутри margin-а не ведет к сворачиванию  *)
    then
        toggleFull' uim
    else
        (getmw ()).Select True True uim
    end)

fun commentsButtonClick uim =
    (getmw ()).Select True False uim;
    (getmw ()).ToggleCollapsed (return ()) uim

fun msgNodeNew setFeed children showFeedTitles separator numMosaic (mw : mw) (UIM uim)
    : transaction xbody =
    hasComments <- hasChildren uim.SubForest;
    uc <- get Settings.ultraCompact;
    mtvm <- uimViewMode (UIM uim);
    lvm <- get Settings.listViewMode;
    uo <- isUnreadOnly;
    let val subForest = case uim.SubForest of UIForest f => f
        val mh = uimMsgHeader (UIM uim)
        val mkey = uimMsgKey (UIM uim)
        val subject = mh.Subject
(*         val subject = if mh.Subject = "" && depth = 0 then *)
(*                           <xml>(no subject)</xml> else subject' *)
        val author = mh.Author
        val shorterText = maybe mh.ShortText (fn m => m.ShorterText)
            <| uimCachedMsg (UIM uim)
        val time = mhTime mh
        val link = uimMsgLink (UIM uim)
        fun markReadNoSelectLink cls title inner =
            case link of
              | None => <xml><div class={cls} title={title} dir="auto">{inner}</div></xml>
              | Some l =>
                <xml><a class={cls} title={title} href={l}
                  dir="auto" rel="noopener" target="_blank"
                  (* onclick для mtime/msubject вызываем через uimOnClick *)
(*                   onclick={fn _ => *)
(*                               mw.MarkRead (UIM uim); *)
(*                               stopPropagation} *)
                  >{inner}</a></xml>
        val ago = Js.showAgo time
        fun timeLink f cls =
            markReadNoSelectLink cls (Js.showTimeAndAgo time) (txt (f ago))
        fun rmTag t _ = stopPropagation; mw.RemoveITTag t (UIM uim)
        fun tags header =
            if header then
              <xml><div dynClass={ts <- uimITTags (UIM uim); return (if ts = [] then displayNone else postHeaderTags)}>{
                dyn_ (ts <- uimITTags (UIM uim);
                      return (if ts = [] then <xml/> else
                      List.mapX (fn t =>
                      <xml>
                        <div class="postHeaderTag">{[t]}</div>
                      </xml>) ts))
              }</div></xml>
           else
              dyn_
              (ts <- uimITTags (UIM uim);
               return (if ts = [] then <xml/> else
                 <xml><div class="mtagsList">{
                 List.mapX (fn t => <xml>
                   <div class="mtag">
                   <div class="mtagName" dir="auto"
                        onclick={fn _ => when (not header)
                            (withSome setFeed (getSubItemByTag t))}
                   >{[t]}</div><div class="mtagRm" onclick={rmTag t}>x</div></div>
                 </xml>)
                 ts}</div></xml>))
        val starButton =
            iconButtonNoOnclick Css.iconStar "Star/unstar article. \nKeyboard shortcut: s"
        val tagButton =
            iconButtonNoOnclick Css.iconMsgTag "Edit tags. \nKeyboard shortcut: t"
        fun ffLV style_ title =
            <xml>{Js.fromFeedIcon style_}<span class="postHeaderFeed" dir="auto">{[title]}</span></xml>
        fun ff inner =
            <xml>
              <div class={ifClass (author <> "") Css.hasAuthor
                                  Css.fromFeed}>
                <div class={Css.fromFeedFrom}>from</div>
                {inner}
              </div>
            </xml>
        fun fromFeed' icon =
            if uim.Depth > 0 then <xml/> else
            case (showFeedTitles,
                  grOrigin [] (uimAttachments (UIM uim)),
                  getSubItemByUrl mkey.BlogFeedUrl) of
              | (True, (Some o,_), _) =>
                if icon then
                    ffLV (Some (Js.faviconStyle o.HtmlUrl False)) o.StreamTitle
                else
                    ff (grOriginLink id o)
              | (True, _, Some si) =>
                if icon then
                    ffLV si.FaviconStyle si.Title
                else
                    ff (Js.setFeedLink Css.fromFeedFolder si.Index (txt si.Title))
              | _ => <xml/>
        val fromFeed = fromFeed' False
        fun text m = Js.preprocessImg m.Text
        val noSubject = subject = ""
        val compact =
            case (mtvm.Posts, uim.Depth, lvm) of
              | (PVMShort, 0, LVMCompact) => True
              | _ => False
        val normalLV =
            case (mtvm.Posts, uim.Depth, lvm) of
              | (PVMShort, 0, LVMTwoLines) => True
              | _ => False
        val authorPicHidden =
            compact ||
            (uim.Depth = 0 &&
             (case mtvm.Posts of
              | PVMMagazine => True | PVMMosaic => True | _ => False))
        fun shortClass mv c =
            case mv of
                MVFull _ => classes Css.expanded c
              | MVShort _ =>
                (case (mtvm.Posts, uim.Depth) of
                   | (PVMMagazine, 0) => classes Css.magazine c
                   | (PVMMosaic, 0) =>
                     classes Css.mosaic
                     (ifClass (mod numMosaic 2 = 0) Css.mosaic2
                     (ifClass (mod numMosaic 3 = 0) Css.mosaic3
                     (ifClass (mod numMosaic 4 = 0) Css.mosaic4
                     (ifClass (mod numMosaic 5 = 0) Css.mosaic5
                     (ifClass (mod numMosaic 6 = 0) Css.mosaic6
                      c)))))
                   | _ =>
                     classes (if compact then Css.compact else Css.short) c)
        val post = uim.Depth = 0
        fun postClass c =
            classes
            (if uim.Depth = 0 then Css.post
             else if uim.Depth = 1 then Css.depth1
             else Css.depth2) c
(*             if post then classes Css.post c else c *)
        fun mobileTime prefix =
            <xml><div class={Css.mobileTime}>{[prefix]}{
              timeLink Js.toLowerCase Css.mtime
            }</div></xml>
        val commentsButton =
            if post && hasComments then <xml>
              <div class={postClass Css.msgFooter}><div class="fontSizeSmaller">
              {Js.withDummyOnclick
              <xml><a class={classes Css.button Css.msgCommentsButton}
                 title={"Click to collapse/expand comments. \nButton shows unread comments count. \nKeyboard shortcut: o"}
                >{dyn_ <|
                  rc <- (if uo then signal subForest.UnreadResultsCount else
                         return subForest.TotalResultsCount);
                  c <- signal uim.Collapsed;
                  return (txt ((if rc > 500 then "500+" else show rc)
                               ^ " " ^ plural rc "comment"
                               ^ (case c of Expanded => ""
                                          | Collapsed _ => "…")
                 ))}</a></xml>}</div></div></xml>
            else <xml/>
        val keepUnreadButton =
            if uim.Mi.ReadLocked then <xml/> else
            iconButtonNoOnclick Css.iconKeepUnread
            "Mark message read/unread. \nKeyboard shortcut: m"
        val msgButtons =
            <xml>{tagButton}{
            case link of
              | None => keepUnreadButton
              | Some l =>
                <xml>{
                  iconButtonNoOnclick Css.iconShare "Share or bookmark"
                  }{(* все-таки разрываем, иначе я не цепляюсь глазами
                      за значок прочитанности в процессе прокрутки *)
                   keepUnreadButton
                  }</xml>}</xml>
        val imageView = case mtvm.Posts of
                          | PVMMagazine => True
                          | PVMMosaic => True
                          | _ => False
        fun msgBodyAndFooter commentsButton buttons body = <xml>
        <div class={Css.msgButtons}>{buttons}</div>
        <div class="msgCollapseMargin">
          <div class="msgBody">
            {body}
(*           {if uim.Depth = 0 && not (isResult uim.Mi) then <xml> *)
(*             <span class="newCommentsHint">New comments in article</span></xml> *)
(*           else <xml/>} *)
            <div class="clearBoth"></div>
          </div> (* msgBody *)
(*         {if not imageView then commentsButton else <xml/>} *)
(*           <div class="msgButtons"> *)
(*             {keepUnreadButton} *)
(*             {share} *)
(*           </div> *)
        </div> (* msgCollapseMargin *)
        {commentsButton}
      </xml>
      val buttons = <xml>
        {if compact then <xml/> else
         <xml>{timeLink id Css.mtime}{starButton}</xml>
        }{msgButtons}
        </xml>
      fun subjLink s = markReadNoSelectLink msubject "" <xml><span class="msubjectStrut">_</span>{txt s}</xml>
      val subj' = subjLink subject
      val subj =
          if noSubject then <xml/>
          else if normalLV then
            (* чтобы ссылка открывалась только при клике на заголовок,
               а в остальных случаях происходило разворачивание элемента
             *)
            <xml><div class="normalListViewClickWrapper">{subj'}</div></xml>
          else if compact then
            (* иначе, в узком режиме заголовок уходит вниз
               из-за margin-left: -Npx в ellipsis() *)
            <xml><div class="msubjectWrapper">{subj'}</div></xml>
          else
            subj'
      val mosaicSubj =
          if noSubject then
              <xml/>
(*               <xml><div class={Css.msubject}></div></xml> *)
              (* для отступа *)
          else
              markReadNoSelectLink Css.msubject "" (txt subject)
      val postImage =
          let fun go a = case a of
                | [] => None
                | (AThumbnail { Url = u }) :: _ => Some u
                | _ :: as => go as
              fun p c m = case go (uimAttachments (UIM uim)) of
                | None =>
                  <xml><div class={c}></div></xml>
                | Some s =>
                  <xml><div class={c} style={Js.backgroundImage s}></div></xml>
          in
              case (uim.Depth, mtvm.Posts, uimCachedMsg (UIM uim)) of
                | (0, PVMMagazine, Some m) => p Css.magazineImage m
                | (0, PVMMosaic, Some m) => p Css.mosaicImage m
                | _ => <xml/>
          end
      val articleTags =
          let val tags = case uim.Mi.MsgView of
                  | MVFull { Msg = m } => m.Tags
                  | MVShort { CachedMsg = Some m, ... } => m.Tags
                  | _ => []
          in
              case tags of
                | _ :: _ =>
                  if uim.Depth = 0 then
                      <xml><div class="mtags" dir="auto">
                        {["tags: " ^ intercalate ", " tags]}
                      </div></xml>
                  else <xml/>
                | [] => <xml/>
          end
      val authorPic = uimAuthorPicXml (UIM uim)
      val msgFrameClassConst =
          ifClass (not noSubject) Css.hasSubject
          <| ifClass compact Css.compactListViewItem
          <| ifClass hasComments Css.hasComments
          <| ifClass (Option.isSome separator) Css.hasSeparator
          <| postClass Css.msgFrame
      val msgFrameClass =
          atps <- signal uim.AddToPocketState;
          mv <- signal uim.Mv;
          st <- uimStarred (UIM uim);
          r <- signal uim.Read;
          s <- signal uim.Selected;
          return (classes (if s
                           then Css.selected else Css.unselected)
                 (ifClass r Css.read
                 (ifClass st Css.starred
                 (ifClass (expandable mtvm (UIM uim) mv) Css.expandable
                 (classes (case atps of
                             | ATPSNone => null
                             | ATPSAdding => Css.addingToPocket
                             | ATPSAdded _ => Css.addedToPocket)
                          (shortClass mv msgFrameClassConst))))))
      val header =
          if compact then let val from = fromFeed' True in
          <xml><div class={ifClass uc Css.ultra Css.postHeader}>
            <div class="postHeaderLeft">{starButton}{from}</div>
            <div class="postHeaderRight">
              {tags True}
              <span class={ifClass (not noSubject) Css.hasSubject
                           (ifClass (mh.ShortText <> "" || noSubject)
                              Css.hasText
                              Css.postHeaderText)}
                    dir="auto">{
                if not noSubject then
                    <xml><span class="postHeaderSubject">{[subject]}</span></xml> else <xml/>
              }{if mh.ShortText <> "" then
                    <xml><span class="postHeaderShortText"><span class="postHeaderShortTextWide">{[mh.ShortText]}</span><span class="postHeaderShortTextNarrow">{[shorterText]}</span></span></xml>
                else if noSubject then
                    <xml><span class="postHeaderShortText">-</span></xml>
                else <xml/>}</span>
              {timeLink id (classes Css.postHeaderTime
               (if strlen ago > 3 then
                    Css.postHeaderTimeLong
                else
                    Css.postHeaderTimeShort))}
          </div></div></xml> end else
          if authorPicHidden then <xml/> else authorPic
      fun readabilityView rv =
          case rv of
            | RVNone _ => <xml/>
            | RVError e => <xml>
              <div class="readabilityError">{[e]}</div>
              </xml>
            | RVLoading => <xml>
              <div class="readabilityLoading">
                <span class="spinner"></span>
                Retrieving full text…
              </div></xml>
            | RVReadability x => x
      fun withMagazinePadder inner =
          case (uim.Depth, mtvm.Posts) of
             | (0, PVMMagazine) =>
               <xml><div dynClass={
                 mv <- signal uim.Mv;
                 return (case mv of
                   | MVFull _ => null
                   | MVShort _ => magazinePadder)}>{inner}</div></xml>
             | _ =>
               inner
    in
    header <- Js.precompute header;
    return <xml>
        {case separator of Some s => s | _ => <xml/>}
        {withMagazinePadder <xml><div dynClass={msgFrameClass}
(*              style={prop1 "margin-left" *)
(*                           (show (2.75*float (if uim.Depth > 2 then 2 else uim.Depth)) ^ "em")} *)
             id={uim.FrameId}
             >
        {header}
        <dyn signal={
          mv <- signal uim.Mv;
          return (case mv of
          | MVFull { Msg = m } => <xml>
              {if authorPicHidden then authorPic else <xml/>}
              {msgBodyAndFooter commentsButton buttons <xml>
                <div class="msgBodyHeader">
                  {subj}
                  {tags False}
                  {fromFeed}
                  {articleTags}
                  {if author <> "" then
                     <xml>
                       <div class="mauthor" dir="auto">{
                           textWithLink m.AuthorUri (txt author)
                       }</div>
                     </xml>
                   else
                     <xml/>}
                  {case (uim.Parent, uim.Prev) of
                     | (Some (UIM p), _ :: _) =>
                       (* если есть родитель,
                          и сообщение не первое в цепочке ответов *)
                       if uim.Depth < 2 then <xml/> else
                       (case uimAuthorAndShortText (UIM p) of (author, shortText) =>
                       <xml><div class="inReplyTo" title={shortText}
                                 onclick={fn _ => stopPropagation;
                                             mw.Select False True (UIM p)}>
                         → (* еще вариант ↦ *)
                         <div class="mauthor" dir="auto">
                           {[if author = "" then
                            (if p.Depth = 0 then "article" else "_") else author]}
                         </div></div>
                       </xml>)
                     | _ => <xml/>
                  }
                  {mobileTime (if author <> "" then " - " else "")}
                  (* '·' слишком высоко *)
                </div>
                <div class="mtext">
                  {dyn_ (Monad.mp readabilityView (signal uim.ReadabilityView))}
                  {displayIf uim.ShowText (text m)}
                </div>
              </xml>}
            </xml>
          | MVShort s =>
            if compact then <xml/> else
            case (uim.Depth, mtvm.Posts, s.CachedMsg) of
              | (0, PVMMagazine, Some m) =>
                <xml>
                  <div class={Css.msgButtons}>{buttons}</div>
                  {postImage}
                  <div class="magazineDescription">
                    {subj}{tags False}
                    {fromFeed}
                    <div class="magazineText" dir="auto">{[m.ShortText]}</div>
                    <div class="magazineTextShorter" dir="auto">{[m.ShorterText]}</div>
                    {commentsButton}
                  </div>
                </xml>
              | (0, PVMMosaic, Some m) =>
                <xml>
                  {postImage}
                  <div class="mosaicDescription">
                    {mosaicSubj}
                    {tags False}
                    {fromFeed}
                    <div class="mosaicText" dir="auto">{[mh.ShortText]}</div>
                  </div>
                </xml>
              | _ =>
                msgBodyAndFooter commentsButton
                 (* хотя msgCollapseMargin тут не нужен, он все равно
                    добавляется, чтобы текст заголовка не скакал
                    при сворачивании/разворачивании *)
                 (if uim.Depth = 0 then buttons
                  else keepUnreadButton)
                 <xml>
              <div class="msgBodyHeader">
              {subj}{tags False}
              {fromFeed}
              <span dir="auto">
              {if author <> "" then
                   <xml><div class="mauthor">{[author]},</div></xml>
               else
                   <xml/>}
              <div class="mtext">{[mh.ShortText]}</div>
              </span>
              </div></xml>
          )} />
      </div></xml>} (* msgFrame *)

      {if hasComments then
           <xml>
             <div dynClass={lc <- signal uim.LoadingChildren;
                           return (ifClass (Option.isNone lc) Css.displayNone
                                           loadingExpanded)}>
                 <span class="spinner"></span> Loading…
             </div>
             <div id={uim.GrowId}
                  dynClass={c <- signal uim.Collapsed;
                            lc <- signal uim.LoadingChildren;
                            return (ifClass (Option.isSome lc)
                                            Css.commentsLoading (case c of
                              | Expanded => Css.commentsGrow
                              | Collapsed _ =>
                                classes Css.commentsGrow Css.collapsed))}>
             {children}
           </div></xml>
       else <xml/>}
    </xml>
    end

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

val emptyMF =
    MsgForest
    { TotalCount = 0, UnreadCount = 0
    , TotalResultsCount = 0, UnreadResultsCount = 0
    , SmartStreamUnreadCounts = [], SmartStreamUnreadResultCounts = []
    , TagTotalCounts = []
    , TagUnreadCounts = [], TagUnreadResultCounts = []
    , List = []
    , NextReq = None
    }
fun mkUIForest (MsgForest f) =
    unreadCount <- source f.UnreadCount;
    unreadResultsCount <- source f.UnreadResultsCount;
    smartStreamUnreadCounts <- source f.SmartStreamUnreadCounts;
    smartStreamUnreadResultCounts <- source f.SmartStreamUnreadResultCounts;
    tagTotalCounts <- source f.TagTotalCounts;
    tagUnreadCounts <- source f.TagUnreadCounts;
    tagUnreadResultCounts <- source f.TagUnreadResultCounts;
    firstChild <- source None;
    children <- source [];
    nextReqId <- source None;
    return (UIForest
                { TotalCount = f.TotalCount
                , UnreadCount = unreadCount
                , TotalResultsCount = f.TotalResultsCount
                , UnreadResultsCount = unreadResultsCount
                , SmartStreamUnreadCounts = smartStreamUnreadCounts
                , SmartStreamUnreadResultCounts = smartStreamUnreadResultCounts
                , TagTotalCounts = tagTotalCounts
                , TagUnreadCounts = tagUnreadCounts
                , TagUnreadResultCounts = tagUnreadResultCounts
                , FirstChild = firstChild
                , Children = children
                , NextReqId = nextReqId
           })

fun feedKeyboardAction act =
    csi <- getCurrentFeed;
    Js.feedKeyboardAction csi.Index act

datatype feedMark
  = FMNone
  | FMTop of float * xbody
  | FMMoving of float * xbody

datatype nextLower
  = NLNone
  | NLLastLine of uim * list uim
  | NLLower of list uim * float * uim

val emptyMarkReq = MRPosts { FeedTcs = [] }

fun isEmptyMarkReq c =
    case c of
      | MRPosts { FeedTcs = [] } => True
      | _ => False

(* Результат mark read/skip/ignore:
   число прочитанных сообщений, результатов поиска и сообщений smart stream-ов
 *)
con countersChange
  = { Posts : int
    , Comments : int
    , RPosts : int
    , RComments : int
    , SSPosts : list (int * int)
    , SSComments : list (int * int)
    , SSRComments : list (int * int)
    , TPosts : list (option itemTag * int)
    , TComments : list (option itemTag * int)
    , TRComments : list (option itemTag * int)
    }

val emptyCountersChange : countersChange =
    { Posts = 0
    , Comments = 0
    , RPosts = 0
    , RComments = 0
    , SSPosts = []
    , SSComments = []
    , SSRComments = []
    , TPosts = []
    , TComments = []
    , TRComments = []
    }



fun msgsWidget updateSubscriptions updateMarkReqReadCounters loadingFeed setFeed =
    selectedUIM <- source (None : option uim);
    selectedUIMPosition <- source 0.0;
    fullUIM <- source (None : option uim);
    defaultUIForest <- mkUIForest emptyMF;
    uiForest <- source defaultUIForest;
    markReq <- source emptyMarkReq;
    loadingAppendRequests <- source False;
    loadingComments <- source False;
    appendRequests_ <- source ([] : list appendReq);
    bulkMarkReadPerformed <- source False;
    lastBfu <- source "";
    blocked <- source False;
    (* на время прокрутки/загрузки комментариев все действия связанные
       с прокруткой/выделением постов блокируются
     *)
    lastPostsViewMode <- source PVMFull;
    toggleCollapsed_ <- source (fn _ _ => return ());
    (* ^  заглушка, чтобы не плодить кучу fun .. and .. and *)
    feedLabels <- source [];
    feedMarkTop <- source None;
    feedMarkMoving <- source None;
    currentPageSize <- source { Count = 0, Mosaic = 0 };
    pagerInsertPointId <- fresh;
    checkAppendActive <- source False;
    trySelectNextAfterCheckAppend <- source None;
    shareIconsPreloaded <- source False;
    actionHintNumber <- source 0;
    actionHintMessage <- source (None : option string);
    let fun setFeedMark fm = case fm of
            | FMNone => set feedMarkTop None; set feedMarkMoving None
            | FMTop x => setNeq feedMarkTop (Some x); set feedMarkMoving None
            | FMMoving x => set feedMarkTop None; setNeq feedMarkMoving (Some x)
            (* небольшая оптимизация, чтобы не менять две ноды сразу,
               у нас две feedMark ноды, одна внутри .top (чтобы работал
               position:absolute и не было проблем в antialiasing из-за
               position:fixed) и одна внутри .right
             *)
        val onError =
            set checkAppendActive False;
            set loadingAppendRequests False;
            set trySelectNextAfterCheckAppend None;
            set loadingComments False;
            set blocked False
        fun actionHint u h =
            return ()
            (* пока отключил, мешает больше, чем помогает.
               Стоит вернуть только для клавиатуры и только когда не видно
               заголовка поста с кнопками
            *)
(*             vm <- uimViewMode u; *)
(*             st <- msgsScrollTop; *)
(*             t <- uimTop uim; *)
(*             c <- isCompact u; *)
(*             set actionHintMessage (Some h); *)
(*             modify actionHintNumber succ *)
(*             n <- get actionHintNumber; *)
(*             spawn (sleep 1000; *)
(*                    n' <- get actionHintNumber; *)
(*                    when (n' = n) (set actionHintMessage None)) *)
        val actionHintUI =
            dynS (maybe <xml/> (divClass Css.actionHint <<< txt))
              actionHintMessage
        val resetCurrentPageSize =
            set currentPageSize { Count = 0, Mosaic = 0 }
        fun insertPoint id =
            <xml><span class={Css.insertPoint} id={id} (* положение *)/></xml>
        fun moveUpDown noOnScroll immediate f =
            set blocked True;
            sm <- Settings.getScrollMode;
            Js.moveUpDown f Settings.msgDivId (if immediate then "immediate" else sm)
                          (set blocked False;
                           Pager.tryScroll pager;
                           (* обновляем pager здесь, т.к.,
                              если нажать и держать PgUp, то будет постоянный
                              scrolling, из-за которого не вызовется onScroll
                              и Pager.scroll
                            *)
                           when noOnScroll preventOnScroll
                          )
        fun expandedInTag uim tagSubItem =
            tvm <- get tagSubItem.ViewMode;
            vm <- (if tvm.NoOverride then uimViewMode uim else return tvm);
            return vm.ExpandedComments
        fun setUimTags (UIM uim) ts' =
            ts <- get uim.Tags;
            set uim.Tags ts';
            r <- get uim.Read;
            let val removed = difference ts ts'
                val added = difference ts' ts
                val hadTags = notNull <| difference ts (ITStarred :: [])
                val hasTags = notNull <| difference ts' (ITStarred :: [])
                val ucs =
                    List.sort gt <|
                    (None, if hadTags = hasTags then 0
                           else if hadTags then 1 else -1) ::
                    List.append
                      (List.mp (fn t => (Some t,1)) removed)
                      (List.mp (fn t => (Some t,-1)) added)
                val rMult = if r then 1 else 0
                fun needUpdateTags (t, n) =
                    if n = 0 then return False else
                    case getSubItemByItemTag t of
                      | None => return True (* нужно добавить новый тег *)
                      | Some si =>
                        c <- get si.Counters;
                        return (c.TotalPosts = 0 && c.TotalComments = 0 &&
                                t <> Some ITStarred)
                        (* нужно удалить пустой тег *)
                val sf = uimSubForest (UIM uim)
                val tc = uifTotalCount sf
                val result = isResult uim.Mi
            in
                uc <- get (uifUnreadCount sf);
                ttc <- get (uifTagTotalCounts sf);
                tuc <- get (uifTagUnreadCounts sf);
                when (not r)
                     (List.app (fn (UIForest f) =>
                         modifyUnreadCounts f.TagUnreadCounts ucs;
                         when result
                           (modifyUnreadCounts f.TagUnreadResultCounts ucs))
                       uim.Parents);
                List.app (fn (UIForest f) =>
                  modifyUnreadCounts f.TagTotalCounts ucs) uim.Parents;
                postTags <- itemTags (postMsg (UIM uim));
                forM_ ucs (fn (t, c) => flip withSome (getSubItemByItemTag t)
                  (fn si =>
                    e <- expandedInTag (UIM uim) si;
                    retryPendingUpdates;
                    modify si.Counters
                      (if isPost (UIM uim) then
                        modifyF [#TotalPosts] (plus (-c)) <<<
                        modifyF [#ReadPosts] (plus ((-c) * rMult)) <<<
                        (if e then
                         let val dupTC = Option.get 0 (List.assoc t ttc)
                             val dupUC = Option.get 0 (List.assoc t tuc)
                             val tc' = tc - dupTC
                         in
(*                              trace ("dupTC = " ^ show dupTC ^ "; dupUC = " ^ show dupUC ^ "; tc = " ^ show tc ^ "; tc' = " ^ show tc') <<< *)
                             modifyF [#TotalComments] (plus ((-c) * tc')) <<<
                             modifyF [#ReadComments]
                               (plus ((-c) * (tc' - (uc - dupUC))))
                         end
                         else id)
                       else if e && elem t postTags then
                        (* если пост уже помечен этим тегом и отображается
                           в этом теге как expanded, то его комментарии
                           и так уже считаются *)
                        id
                       else
                        modifyF [#TotalComments] (plus (-c)) <<<
                        modifyF [#ReadComments] (plus ((-c) * rMult)));
                    updateCounters si 0 0));
                u <- List.findM needUpdateTags ucs;
                ue <- get updateCountersEnabled;
                when (Option.isSome u && ue)
                     (* не вызываем updateSubscriptions в untag above/below *)
                     updateSubscriptions
            end
        fun addTag t u =
            ts <- get (uimTags u);
            when (not (elem t ts || t = itTag ""))
                 (addMsgTag t u;
                  setUimTags u (List.append ts (t :: [])))
        fun removeTag t u =
            removeMsgTag t u;
            ts <- get (uimTags u);
            setUimTags u (List.filter (ne t) ts)
        val addITTag = addTag <<< itTag
        val removeITTag = removeTag <<< itTag
        fun toggleStarred u =
            s <- current (uimStarred u);
            (if s then
                 actionHint u "Unstarred";
                 removeTag ITStarred u
             else
                 actionHint u "Starred";
                 addTag ITStarred u)
        fun replaceITTags tagNames u =
            ts <- get (uimTags u);
            let val ts' = (if elem ITStarred ts then cons ITStarred else id)
                          (List.mp itTag tagNames)
            in
            List.app (flip removeMsgTag u) (difference ts ts');
            List.app (flip addMsgTag u)    (difference ts' ts);
            setUimTags u ts'
            end
        fun skipMsgComments (UIM uim) : transaction countersChange =
            read <-
              (if uim.Mi.ReadLocked then
                   return True
               else
                   r <- get uim.Read;
                   set uim.Read True;
                   set uim.KeepUnread False;
                   return r);
            case uim.SubForest of UIForest f =>
            urc <- get f.UnreadResultsCount;
            uc <- get f.UnreadCount;
            ssuc <- get f.SmartStreamUnreadCounts;
            ssurc <- get f.SmartStreamUnreadResultCounts;
            tuc <- get f.TagUnreadCounts;
            turc <- get f.TagUnreadResultCounts;
            set f.UnreadResultsCount 0;
            set f.UnreadCount 0;
            set f.SmartStreamUnreadCounts [];
            set f.SmartStreamUnreadResultCounts [];
            set f.TagUnreadCounts [];
            set f.TagUnreadResultCounts [];
            ch <- get f.Children;
            List.app (fn c => r <- skipMsgComments c; return ()) ch;
            tucU <-
              (if not read then tagUnreadCounters 1 (UIM uim) else return []);
            let val p = isPost (UIM uim)
                fun ifP [a] (t:a) (e:a) : a = if p then t else e
                val nr = if not read then 1 else 0
                val nrr = if isResult uim.Mi then nr else 0
                val ssucU =
                    if not read then smartStreamUnreadCounters 1 uim.Mi else []
                val ssurcU =
                    if isResult uim.Mi then ssucU else []
                val turcU =
                    if isResult uim.Mi then tucU else []
            in
                return
                    { Posts      = ifP nr 0
                    , Comments   = ifP uc (uc+nr)
                    , RPosts     = ifP nrr 0
                    , RComments  = ifP urc (urc+nrr)
                    , SSPosts    = ifP ssucU []
                    , SSComments = ifP ssuc (mergeWith plus ssuc ssucU)
                    , SSRComments = ifP ssurc (mergeWith plus ssurc ssurcU)
                    , TPosts     = ifP tucU []
                    , TComments  = ifP tuc (mergeWith plus tuc tucU)
                    , TRComments = ifP turc (mergeWith plus turc turcU)
                    }
            end
        fun toggleMsgRead withActionHint (UIM uim) : transaction countersChange =
            if uim.Mi.ReadLocked then return emptyCountersChange else
            toggle uim.Read;
            r <- get uim.Read;
            when withActionHint
              (actionHint (UIM uim) (if r then "Read" else "Unread"));
            set uim.KeepUnread (not r);
            (* устанавливается после изменений пользователя,
             а не по-умолчанию для непрочитанного сообщения *)
            let val uc = if r then 1 else -1
            in
                tc <- tagUnreadCounters uc (UIM uim);
            let val p = isPost (UIM uim)
                fun ifP [a] (t:a) (e:a) : a = if p then t else e
                val rc = if isResult uim.Mi then uc else 0
                val ss = smartStreamUnreadCounters uc uim.Mi
                val ssr = if isResult uim.Mi then ss else []
                val tcr = if isResult uim.Mi then tc else []
            in
                return
                    { Posts       = ifP uc 0
                    , Comments    = ifP 0 uc
                    , RPosts      = ifP rc 0
                    , RComments   = ifP 0 rc
                    , SSPosts     = ifP ss []
                    , SSComments  = ifP [] ss
                    , SSRComments = ifP [] ssr
                    , TPosts      = ifP tc []
                    , TComments   = ifP [] tc
                    , TRComments  = ifP [] tcr
                    }
            end end
        fun processRead (mark : uim -> transaction countersChange) act (UIM uim) =
            c <- mark (UIM uim);
            let val mid = uim.Mi.MsgId
                val bfu = uim.Mi.MsgKey.BlogFeedUrl
                fun updCounters si up uc =
                    updateCounters si (-up) (-uc)
                val ssupc = mergeWith (fn (p,_) (_,c) => (p,c))
                            (List.mp (fn (s,p) => (s,(p,0))) c.SSPosts)
                            (List.mp (fn (s,c) => (s,(0,c))) c.SSComments)
                val tupc = mergeWith (fn (p,_) (_,c) => (p,c))
                            (List.mp (fn (s,p) => (s,(p,0))) c.TPosts)
                            (List.mp (fn (s,c) => (s,(0,c))) c.TComments)
                fun withSubItem act =
                    case getSubItemByUrl bfu of
                      | None =>
                        du <- current discoverySubItemUrl;
                        (case du of
                           | Some u =>
                             when (u = bfu) (cf <- getCurrentFeed; act cf)
                           | None => return ())
                      | Some si =>
                        act si
                fun allRead c =
                    c.ReadPosts = c.TotalPosts
                    && c.ReadComments = c.TotalComments
            in
            List.app (fn (UIForest f) =>
              (* обновляем только счетчики комментариев, т.к. общие счетчики
                 у root MsgForest нигде не используются
                 (subItems обновляются отдельно)
               *)
              modify f.UnreadResultsCount (fn rc => rc - c.RComments);
              modify f.UnreadCount (fn uc => uc - c.Comments);
              modifyUnreadCounts f.SmartStreamUnreadCounts c.SSComments;
              modifyUnreadCounts f.SmartStreamUnreadResultCounts c.SSRComments;
              modifyUnreadCounts f.TagUnreadCounts c.TComments;
              modifyUnreadCounts f.TagUnreadResultCounts c.TRComments
              ) uim.Parents;
            cf <- get currentFeed;
            allRead0 <- Monad.mp allRead (get cf.Counters);
            (case cf.SIType of
               | SITSearch _ =>
                 updCounters cf c.RPosts c.RComments
                 (* для поиска создается временный subItem,
                    обновляем его счетчики отдельно *)
               | _ => return ());
            List.app
                (fn (s,(p,c)) =>
                    withSome (fn ss => updCounters ss p c) (getSubItemByGRId s))
                ssupc;
            List.app
                (fn (t,(p,c)) =>
                    withSome (fn ts => updCounters ts p c) (getSubItemByItemTag t))
                tupc;
            postTags <- itemTags (postMsg (UIM uim));
            List.app
                (fn t => flip withSome (getSubItemByItemTag t) <| fn ts =>
                    e <- expandedInTag (UIM uim) ts;
                    when e <| updCounters ts 0 <|
                      c.Comments - Option.get 0 (List.assoc t c.TComments))
                postTags;
            withSubItem (fn si =>
                updCounters si c.Posts c.Comments;
                sc <- get si.Counters;
                Js.forceImpure (maybe (return ()) BackgroundRpc.addAction
                                    (act mid sc.TotalComments c))
                (* TODO: какая-то проблема с оптимизатором, он выкидывает
                   вызовы updateSubInfo или addaction в конце ф-ий
                 *));
            allRead' <- Monad.mp allRead (get cf.Counters);
            when (allRead' && not allRead0)
                BackgroundRpc.flushInBackground
            end
        fun toggleRead withActionHint =
            processRead (toggleMsgRead withActionHint)
                (fn mid tc c => Some (BGMarkMsgRead
                                           { MsgId = mid
                                           , Read = c.Posts + c.Comments = 1
                                           , TotalComments = tc }))
        val toggleReadNoBgAction =
            processRead (toggleMsgRead False) (fn _ _ _ => None)
        fun editTags (UIM uim) =
            ts <- current (uimITTags (UIM uim));
            text <- source (intercalate ", " ts);
            tid <- fresh;
            let val edit =
                    (ok,tags) <- Js.getTagsList tid;
                    when ok
                         (P.hide;
                          replaceITTags tags (UIM uim))
            in
            d <- P.newBox "Enter tags separated by comma"
              (oneLineInputAndOkButton tid text "" "OK" edit);
            P.toggle d;
            Js.select tid;
            Js.focus tid;
            Js.setupTagAutocomplete tid
            end
        fun addTagMenuContents uim =
            tid <- fresh;
            let val new =
                    stopPropagation;
                    (* а то list view раскрывает *)
                    (ok,tags) <- Js.getTagsList tid;
                    when ok
                         (P.hide;
                          replaceITTags tags uim)
            in
            ts <- current (uimITTags uim);
            text <- source (intercalate ", " ts);
            usedTags <- Js.getUsedTags;
            return <xml>
              <div class={Css.newFolder}>
                <div class="newFolderLabel">Edit tags</div>
                {oneLineInputAndOkButton tid text "New tag, …" "OK" new}
              </div>
              {List.mapX
               (fn t =>
                    if elem t ts then
                        P.lii Css.iconCheck t <|
                          actionHint uim ("Untagged “" ^ t ^ "”");
                          removeITTag t uim
                    else
                        P.lii Css.iconEmpty t <|
                          actionHint uim ("Tagged “" ^ t ^ "”");
                          addITTag t uim)
               usedTags}
            </xml>
            end
        fun addTagMenuUnderButton (UIM uim) =
            m <- addTagMenuContents (UIM uim);
            P.toggleContextMenu
                (Unsafe.id "tagsMenu")
                (Some (Right (Js.offsetBottomRight uim.FrameId Css.iconMsgTag)))
                m
        fun addTagMenu p u =
            m <- addTagMenuContents u;
            P.showContextMenu (Some (Left p)) m
        fun modifyAppendRequests f =
            a0 <- get appendRequests_;
            let val a' = f a0
                fun setAppending x l =
                    List.app (fn (AppendReq ar) =>
                                 Pager.setAppendingId pager ar.Id x) l
            in
                set appendRequests_ a';
                setAppending False a0;
                setAppending True a'
            end
        fun removeAppendReqById rqacc id =
            let fun go acc ars = case ars of
                        [] => (rqacc, List.rev acc)
                      | (AppendReq ar) :: ars =>
                        if ar.Id = id then
                            (AppendReq ar :: rqacc, List.revAppend acc ars)
                        else go (AppendReq ar :: acc) ars
            in
                (r, ars) <- Monad.mp (go []) (get appendRequests_);
                modifyAppendRequests (const ars);
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
            modifyAppendRequests (List.revAppend reqs);
            List.app (fn (AppendReq { Id = id
                                    , Params = ForestParams
                                      { Forest =
                                        UIForest { NextReqId = nri, ...}, ... }
                                    , ...}) => set nri (Some id)) reqs
        fun markRead (UIM uim) =
            r <- get uim.Read;
            if r then return () else
            k <- get uim.KeepUnread;
            if k then return () else
            toggleRead False (UIM uim)
        fun tryMarkRead (UIM uim) =
(*             case v of MVShort _ => return () | _ => *)
            if isResult uim.Mi then
                mrm <- get Settings.markReadMode;
                case mrm of
                  | MRMManual => return ()
                  | MRMOnScrollEverywhere =>
                    markRead (UIM uim)
                  | MRMOnScroll =>
                    vm <- uimViewMode (UIM uim);
                    (case (vm.Posts) of
                       | PVMShort =>
                         return ()
                       | _ =>
                         markRead (UIM uim))
            else
                return ()
        val mw = getmw ()
        fun toggleCollapsed a b =
            tc <- get toggleCollapsed_;
            tc a b
        fun tryToggleFull (UIM uim) =
            mv <- get uim.Mv;
            case mv of
              | MVShort _ =>
                toggleFull (return ()) mw (UIM uim)
              | _ => return ()
        fun markPrevRead (UIM u) (UIM uimTo) =
            when (u.FrameId <> uimTo.FrameId)
                 (tryMarkRead (UIM u);
                  n <- nextMsg False (UIM u);
                  withSome (fn x => markPrevRead x (UIM uimTo)) n)
        fun markIfDown ot (sel0 : option uim) (uimTo : uim) : transaction {} =
            sel0 <- (case sel0 of
              | None =>
                uif <- get uiForest; findFirst False uif
              | x => return x);
            (* помечаем все предыдущие сообщения прочитанными
               только если прокрутка идет вниз.
               Если выделяемое сообщение выше текущего, значит
               текущее - compact list view и тут сложно сказать,
               что должно быть прочитанным, а что нет
             *)
            withSome (fn s =>
            ot0 <- uimTop s;
            (* важно getmw, т.к. mw содержит эту ф-ю и при его инициализации
               mw внутри этой ф-ии будет равен null
             *)
            when (ot0 < ot) (markPrevRead s uimTo)) sel0
        fun whenNotBlocked act =
            Js.updateBrowserScale; (* перед любой командой прокрутки *)
            b <- get blocked;
            set trySelectNextAfterCheckAppend None;
            (* любое действие отменяет выделение следующего сообщения
               (в т.ч. навигация, прокрутка и выделение сообщения)
             *)
            when (not b) act
        val saveSelectedUIMPosition =
(*             debug "saveSelectedUIMPosition"; *)
            s <- get selectedUIM;
            case s of
              | Some (UIM u) =>
                st <- msgsScrollTop;
                t <- uimTop (UIM u);
                h <- Pager.idClientRectHeight pager u.FrameId;
(*                 debug ("st = " ^ show st ^ "; t = " ^ show t ^ "; h = " ^ show h); *)
                set selectedUIMPosition
                    (if t >= st then t - st else (t - st) / h)
                (* позиционирование пропорционально высоте хорошо работает
                   с чистым текстом/изображениями/видео,
                   но плохо работает со смесью текста/видео,
                   т.к. видео при увеличении ширины увеличивается в высоте,
                   а не уменьшается как текст из-за чего происходят
                   случайные сдвиги.
                   можно находить ближайший class=p/i и считать смещение
                   от него, но пока этого делать не будем
                 *)
              | None =>
                set selectedUIMPosition 0.0
        val restoreSelectedUIMPosition =
            s <- get selectedUIM;
            fl <- Js.fontsLoading;
(*             debug ("restoreSelectedUIMPosition " ^ show (Option.isSome s, fl)); *)
            case (s, fl) of
              | (Some (UIM u), False) =>
                p <- get selectedUIMPosition;
                st <- msgsScrollTop;
                t <- uimTop (UIM u);
                h <- Pager.idClientRectHeight pager u.FrameId;
                let val diff =
                        if p >= 0.0 then (t - st) - p
                        else t - (p * h + st)
                in
(*                     debug ("pos = " ^ show p ^ "; diff = " ^ show diff); *)
                    when (diff <> 0.0)
                      (st <- msgsScrollTop;
                       o <- msgsScrollTopOffset;
                       setScrollTop (max o (st + diff)))
                end
              | _ => return ()
        fun selectS forceSelect tryFull andMarkRead scroll (UIM uim) =
            whenNotBlocked (
(*             debug "selectS"; *)
            compact <- isCompact (UIM uim);
            s <- get uim.Selected;
            if s && not forceSelect then
                when andMarkRead (markRead (UIM uim));
                when tryFull (tryToggleFull (UIM uim))
            else
            (let val sel =
(*                      debug "sel"; *)
                     (if andMarkRead
                      (* если пользователь щелкнул по сообщению *)
                      then markRead (UIM uim) else tryMarkRead (UIM uim));
                     when (not s)
                       (old <- get selectedUIM;
                        (case old of
                           | Some (UIM u) => set u.Selected False
                           | None => return ());
                        set uim.Selected True;
                        set selectedUIM (Some (UIM uim)));
                     saveSelectedUIMPosition
             in
                 if compact then
                     sel;
                     when tryFull (tryToggleFull (UIM uim));
                     when scroll (fitCompact mw (UIM uim))
                 else if scroll then
                     (ot <- uimTop (UIM uim);
                      set blocked True;
                      sel0 <- get selectedUIM;
                      scrollTo ot
                          (sel;
                           preventOnScroll;
                           markIfDown ot sel0 (UIM uim);
                           (* toggleFull делает прокрутку, но не делает
                              помечания сообщений
                            *)
                           set blocked False
                          ))
                 else
                     sel
             end))
        val select = selectS False False False True
        val scrollToMsgTop =
            uimo <- get selectedUIM;
            case uimo of
              | None => return ()
              | Some uim =>
                ot <- uimTop uim;
                st <- msgsScrollTop;
                when (ot < st)
                     (moveUpDown False False (fn ch => ot - st))
        fun trySelectPrev' f ro tryFull =
            uimo <- get selectedUIM;
            m' <-
              (case uimo of
                | None => uif <- get uiForest; findFirst ro uif
                | Some m => f ro m);
            case m' of
              | Some m =>
                v <- uimOnValidPage m;
                if v then selectS False tryFull False True m else scrollToMsgTop
                (* прокручиваем наверх, чтобы отобразилась предыдущая страница *)
              | None => scrollToMsgTop
        val trySelectPrev = trySelectPrev' prevMsg
        fun withSelected f =
            uimo <- get selectedUIM;
            withSome f uimo
        fun uimCompactFull (UIM uim) =
            comp <- isCompact (UIM uim);
            mv <- get uim.Mv;
            return (comp, case mv of MVFull _ => True | _ => False)
        val mobileCursorMode =
            (* когда курсор не виден, сообщение выделяется при пересечении
               им верхней границы
             *)
(*             fa <- get Settings.feedAlign; *)
            vw <- Js.viewportWidth;
            return (vw <= 900.0(*  || fa = Settings.FACenter *))
        fun findScroll' st (h:float) m =
            suim <- get selectedUIM;
            sh <- Js.boundingClientRectHeight Settings.snapHeightId;
            mc <- mobileCursorMode;
            let fun compactSelectMargin down =
                    (* максимально долго держим выделенным раскрытый compact,
                       но вверх на него переходим как на обычное сообщение
                     *)
                    if down then
                        if mc then 0.0 else 15.0
                    else
                        h/2.0
                fun selectMargin down =
                    (* 0.0 *) (* 8.0*sh *) (* 8*0.25em = 2em -- msgBody:margin-bottom *)
                    if mc && down then
                        0.0
                    else
                        h/2.0
                fun sel m = tryMarkRead m; selectS False False False False m
                fun mark s = List.app tryMarkRead s
                fun nextLower top acc m =
                    n <- nextMsg False m;
                    case n of
                      | None =>
                        return (case acc of
                                  | [] => NLNone
                                  | (l::ls) => NLLastLine (l, ls))
                      | Some nm =>
                        ntop <- uimTop nm;
                        if ntop = top then
                            nextLower top (nm :: acc) nm
                        else
                            return (NLLower (acc, ntop, nm))
                fun selFirst top m =
                    p <- prevMsg False m;
                    case p of
                      | None => sel m
                      | Some pm =>
                        ptop <- uimTop pm;
                        if ptop = top then
                            tryMarkRead m;
                            selFirst top pm
                        else
                            sel m
                fun go m top compact full =
                if top = st then sel m
                else if top > st then (* стоит посмотреть, есть ли что выше *)
                    when (not compact || Option.isNone suim)
                         (* у выделенного list view item
                              курсор вверх не перемещаем *)
                    (p <- prevMsg False m;
                     case p of
                       | None => sel m
                       | Some pm =>
                         ptop <- uimTop pm;
                         (pcompact, pfull) <- uimCompactFull pm;
                         if ptop >= st then
                             (tryMarkRead m;
                              if pcompact then
                                  sel pm (* останавливаемся на первом compact *)
                              else
                                  go pm ptop pcompact pfull)
                         else
                         let val margin =
                                 if pcompact && pfull
                                 then compactSelectMargin False
                                 else selectMargin False
                         in
                             if margin > 0.0 then
                                 (if top >= st + margin then
                                      tryMarkRead m; selFirst ptop pm
                                  else
                                      sel m)
                             else
                                 tryMarkRead m; selFirst ptop pm
                         end)
                else (* смотрим ниже *)
                    (n <- nextLower top [] m;
                     case n of
                       | NLNone => sel m
                       | NLLastLine (last, sameTop) =>
                         (* в последней строке mosaic выделяем последний
                            элемент, а не первый *)
                         mark (m :: last :: sameTop);
                         sel last
                       | NLLower (sameTop, ntop, nm) =>
(*                          debug ("compact: " ^ show compact ^ *)
(*                                 "; full: " ^ show full ^ *)
(*                                 "; ntop: " ^ show ntop ^ *)
(*                                 "; st: " ^ show st); *)
(*                          (ncompact,nfull) <- uimCompactFull nm; *)
                         if ntop <= st then
                             mark (m :: sameTop);
                             (nc,nf) <- uimCompactFull nm;
                             go nm ntop nc nf
                         else
                         let val margin =
                                 if compact && full
                                 then compactSelectMargin True
(*                                  на свернутых должно работать как раньше *)
                                 else selectMargin True
                         in
                         if margin > 0.0 then
                             if ntop >= st + margin then
                                 sel m
                             else
                                 mark (m :: sameTop); sel nm
                         else
                             sel m
                         end)
            in
                top <- uimTop m;
                (* uimTop очень ресурсоемок, не вызываем его дважды *)
                (compact, full) <- uimCompactFull m;
                go m top compact full
            end
        (* предварительный findScroll, отсекающий элементы находящиеся
           полностью за пределами экрана
         *)
        fun findScroll st (h:float) m =
            r <- msgsBoundingRect;
            let fun orig m = findScroll' st h m
                val up = Some True
                val down = Some False
                fun go direction prev m =
                    t <- Pager.idClientRectTop'
                        (fn r t b => return (t < r.Bottom && b > r.Top))
                        pager (uimFrameId m);
                    if t >= r.Bottom then (* вне экрана *)
                        (if direction = down then
                             (* перелетели экран.
                                может быть случай, когда уменьшилась
                                высота по сравнению с сохраненной
                                (не успели загрузиться картинки и/или их
                                 высота не была сохранена),
                                тогда возможен вариант, что t_1 >= Bottom,
                                а t_0 + h_0 < Top и мы начинаем вечно
                                ходить вверх/вниз, чтобы этого избежать
                                запоминаем направление
                              *)
                             orig prev
                         else
                             c <- isCompact m;
                             if c then orig prev(* m *) else
                             pm <- prevMsg False m;
                             case pm of
                               | None => orig prev(* m *)
                               | Some pm =>
(*                                  debug "tryMarkRead and go up"; *)
                                 tryMarkRead m; go up m pm)
                    else
                        h <- Pager.idClientRectHeight pager (uimFrameId m);
                        if t + h < r.Top then
                            (if direction = up then
                                 orig prev
                             else
                                 nm <- nextMsg False m;
                                 case nm of
                                   | None => orig prev(* m *)
                                   | Some nm =>
(*                                      debug ("tryMarkRead and go down (t = " ^ show t ^ ", h = " ^ show h ^ ", r.Top = " ^ show r.Top ^ ", uimFrameId m = " ^ show (uimFrameId m)); *)
                                     tryMarkRead m; go down m nm)
                        else
                            orig prev(* m *)
                            (* orig всегда с prev, т.к. может заехать вниз,
                               в видимую область,
                               а потом пойти наверх и выделить предыдущий
                               элемент, пометив следующий прочитанным
                             *)
            in
(*                 orig m *)
                go None m m
            end
        fun feedLabel l = <xml>
          <div class="groupByFeedLabel" id={l.LabelId}>
            {l.Text}
          </div>
        </xml>
        fun onScroll selectOnly = (* measure ("onScroll" ^ if selectOnly then " selectOnly" else "") *)
            st0 <- msgsScrollTop;
            ps <- (if selectOnly then return False else
                   (* measure "Pager.scroll" *) (Pager.scroll pager));
            st <- (if ps then
                       st <- msgsScrollTop;
                       lsp <- get lastScrollTop;
                       when (st0 = lsp) (set lastScrollTop st);
                       (* учитываем, что Pager мог поменять scrollTop,
                          и обновляем lastScrollTop, если нужно
                        *)
                       return st
                   else
                       return st0);
            lsp <- get lastScrollTop;
(*             debug ("onScroll st = " ^ show st ^ "; lsp = " ^ show lsp); *)
            fs <- Js.isFullScreen;
            if st = lsp || fs then return () else whenNotBlocked (
            (* whenNotBlocked здесь, чтобы не сбрасывало
               trySelectNextAfterCheckAppend при сворачивании комментариев *)
            (* дабы не выделяло первое сообщение при выборе фида *)
            set lastScrollTop st;
            h <- msgsClientHeight;
            uimo <- get selectedUIM;
            (case uimo of
              | None => uif <- get uiForest; m <- findFirst False uif;
                withSome (fn m =>
                             t <- uimTop m;
                             mc <- mobileCursorMode;
                             when (st >= t || not mc) (findScroll st h m)) m
              | Some m => (* measure "findScroll" *) (findScroll st h m)));
            saveSelectedUIMPosition
        fun findFeedMark (st : float) prev ls =
            case ls of
              | [] => setFeedMark FMNone
              | (l::ls') =>
                lastTop <- source None;
                let fun checkTop r t b =
                        set lastTop (Some t);
                        return (t < r.Bottom && b >= r.Top)
                    fun setTop t =
                        r <- msgsBoundingRect;
                        setFeedMark (FMTop (r.Top, t))
                in
                t <- Pager.idPositionTop' checkTop pager l.LabelId;
                if Js.roundScrollTop t < st then
(*                     debug ("round ot = " ^ show (round ot) ^ *)
(*                            "; round rt = " ^ show (round rt) ^ *)
(*                            "; ot = " ^ show ot ^ "; rt = " ^ show rt *)
(*                           ); *)
                    h0 <- Pager.idClientRectHeight pager l.LabelId;
                    lt <- get lastTop;
                    let val checkPrevTop = case lt of
                            | Some lt =>
                              fn r t b => return (t < r.Bottom && b > lt)
                            | None => checkTop
                        val h = h0 - 1.0
                                  (* ^ закрываем border-top следующей метки *)
                    in
                        case prev of
                          | Some p =>
                            pt <- Pager.idPositionTop' checkPrevTop pager p.LabelId;
                            if Js.roundScrollTop (pt - h) < st
                            then (* следующая наезжает *)
                                let val movingTop = pt - h
                                in
(*                                     debug ("pt = " ^ show pt ^ "; h = " ^ show h ^ "; st = " ^ show st ^ "; lt = " ^ show lt ^ "; movingTop = " ^ show movingTop); *)
                                    setFeedMark
                                        (FMMoving (movingTop, l.Text))
                                end
                            else
                                setTop l.Text
                          | None =>
                            setTop l.Text
                    end
                else
                    findFeedMark st (Some l) ls'
                end
        val updateFeedMark =
            groupByFeed <- Monad.mp mtvmGroupByFeed getMsgTreeViewMode;
            fs <- Js.isFullScreen;
            Js.updateBrowserScale;
            when (not fs && groupByFeed) (
            ls <- get feedLabels;
            st <- msgsScrollTop;
            (* measure "findFeedMark" *) (findFeedMark st None ls)
            )
        fun onExpandReq rq =
            case rq of
              | TRComments r => r.OnExpand
              | TRCommentsS r => r.OnExpand
              | _ => False
        fun fs (ForestParams p) (MsgForest mf) =
            let val (UIForest f) = p.Forest
                val parents' = p.Forest :: p.Parents
                fun griFeed bfu =
                    strsindex bfu "https://bazqux.com/gri/" = Some 0
                fun showFeedTitlesT f =
                    case f.SIType of
                      | SITAll => return True
                      | SITFolder _ => return True
                      | SITFeed f => return (griFeed f.Subscription.Url)
                      | SITStarred => return True
                      | SITAllTags => return True
                      | SITTag _ => return True
                      | SITSmartStream _ => return True
                      | SITSearch _ =>
                        sf <- get currentSearchFeed;
                        showFeedTitlesT sf
            in
            cf <- get currentFeed;
            groupByFeed <- Monad.mp mtvmGroupByFeed getMsgTreeViewMode;
            showFeedTitles <- Monad.mp (fn s => s && not groupByFeed)
                                       (showFeedTitlesT cf);
            x <- List.mapXM (fn (mi, subForest) =>
                read <- source mi.Read;
                tags <- source mi.Tags;
                keepUnread <- source False;
                selected <- source False;
                collapsed <- source Expanded;
                growId <- fresh;
                frameId <- fresh;
                loadingChildren <- source None;
                sf <- mkUIForest subForest;
                prev <- get f.Children;
                next <- source None;
                mv <- source mi.MsgView;
                rv <- source (RVNone None);
                atps <- source ATPSNone;
                st <- source True;
                hasSep <- source False;
                when (not (setuim frameId
                       (UIM
                        { Mi = mi
                        , Tags = tags
                        , Mv = mv
                        , ReadabilityView = rv
                        , AddToPocketState = atps
                        , ShowText = st
                        , Depth = p.Depth
                        , GrowId = growId
                        , FrameId = frameId
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
                        , HasSeparator = hasSep
                        }))) (alert "setuim?");
                let val uim : uim = getuim frameId
                    val separator = <xml>
                      <div class="viewModeSeparator"></div>
                    </xml>
                    val bfu0 = (uimMsgKey uim).BlogFeedUrl
                    val gri = griFeed bfu0
                    val mkGroupByFeedLabel =
                        if not groupByFeed || p.Depth > 0 then
                            return None else
                        lbfu <- get lastBfu;
                        let val bfu = if gri then "gri" else bfu0 in
                        if lbfu = bfu then
                            when gri
                                 (fls <- get feedLabels;
                                  case fls of
                                    | x::xs =>
                                      setFeedLabelId bfu0 x.LabelId
                                    | _ => return ());
                            return None
                        else
                        labelId <- fresh;
                        uc <- get Settings.ultraCompact;
                        let fun formatLabel icon (x : xbody) =
                                <xml><div class={ifClass uc Css.ultra Css.postHeader}>
                                  (* postHeader чтобы высота правильнее
                                     масштабировалась *)
                                  <span class="feedLabelLeft">{Js.fromFeedIcon icon}</span><span class={Css.feedLabelFeed}>{x}</span>
                                </div></xml>
                            val text =
                                if gri then
                                    formatLabel None (txt "Imported from Google Reader")
                                else
                                    case ( grOrigin [] (uimAttachments uim)
                                         , getSubItemByUrl bfu) of
                                      | ((Some o,_), _) =>
                                        grOriginLinkText (formatLabel (Some (Js.faviconStyle o.HtmlUrl False))) o
                                      | (_, Some si) =>
                                        formatLabel si.FaviconStyle (txt si.Title)
                                      | _ => txt "?"
                            val l = { LabelId = labelId,
                                      Text = text
                                    } in
                        modify feedLabels (cons l);
                        set lastBfu bfu;
                        setFeedLabelId bfu labelId;
                        Pager.append pager (feedLabel l);
                        Pager.appendId pager labelId;
                        return (Some l)
                        end end
                    fun mkSeparator groupByFeedLabel =
                        if p.Depth > 0 then
                            return None
                        else
                        vm <- uimViewMode uim;
                        lpvm <- get lastPostsViewMode;
                        let val pvm = vm.Posts
                        in
                            set lastPostsViewMode pvm;
                            return
                            (if (pvm = PVMMosaic || pvm = PVMMagazine) &&
                                (* добавляем разделитель, которого
                                   нет у свернутых mosaic/magazine *)
                                (Option.isSome groupByFeedLabel
                                 (* после разделителя груп *)
                                 || pvm <> lpvm)
                                 (* или при смене режима просмотра *)
                            then
                                Some separator
                            else
                                None)
                        end
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
                    groupByFeedLabel <- mkGroupByFeedLabel;
                    separator <- mkSeparator groupByFeedLabel;
                    set hasSep (Option.isSome separator);
                    (if p.Depth = 0 then
                         Pager.appendId pager frameId
                     else
                         withSome (fn (UIM p) =>
                             Pager.appendChildId pager p.FrameId frameId)
                             p.ParentUIM);
                    ch <- fs (ForestParams
                              { Depth = p.Depth+1
                              , Parents = parents'
                              , ParentUIM = Some uim
                              , Forest = sf }) subForest;
                    cp <- get currentPageSize;
                    x <- msgNodeNew setFeed ch (showFeedTitles || gri) separator cp.Mosaic mw uim;
                    if p.Depth = 0 then
                        Pager.append pager x;
                        vm <- uimViewMode uim;
                        uo <- isUnreadOnly;
                        let val (c0, m0) = case vm.Posts of
                                | PVMMosaic => (cp.Count + 1, cp.Mosaic+1)
                                | PVMMagazine => (cp.Count + 3, 0)
                                | PVMFull =>
                                  (cp.Count + 2
                                   + String.length (uimMsgText uim) / 2000, 0)
                                (* 5kb без картинок -- примерно страница
                                   0.5-1.5kb -- посты share на facebook
                                   200-400b -- твиты, новости lenta.ru
                                 *)
                                | PVMShort => (cp.Count+1, 0)
                            val (sfList, sfCount) = case subForest of
                                | MsgForest mf =>
                                  (mf.List,
                                   if uo then mf.UnreadResultsCount
                                   else mf.TotalResultsCount)
                            val (c, m) =
                                if notNull sfList then
                                  (c0 + sfCount, 0)
                                (* развернутые комментарии могут быть только
                                   у expanded, по-этому mosaic = 0
                                 *)
                                else
                                  (c0 + sfCount / 5, m0)
                                (* на случай, если комментарии развернут,
                                   чтобы не было несколько постов
                                   по 100 комментов на одной странице
                                   (500 все равно может быть и подтормаживает,
                                    но чтобы это исправить нужно реализовать
                                    вложенные страницы)
                                 *)
                        in
(*                             debug ("text length = " ^ show (String.length (uimMsgText uim))); *)
                            if c >= 15 && mod m 60 = 0 then
(*                                 debug ("c = " ^ show c ^ "; m = " ^ show m); *)
                                Pager.newPage pager;
                                resetCurrentPageSize
                            else
                                set currentPageSize { Count = c, Mosaic = m }
                        end;
                        return <xml></xml>
                    else
                        return x
                end)
                mf.List;
            let fun addAppendReq id rq ip =
                    set f.NextReqId (Some id);
                    let val areq =
                            AppendReq { Id = id, Params = ForestParams p,
                                        TreeReq = rq, InsertPoint = ip }
                        fun addCollapsed (UIM uim) =
                            set uim.Collapsed (Collapsed (areq :: []))
                    in
                        withSome
                            (fn parent =>
                                Pager.appendChildId pager
                                    (uimFrameId (postMsg parent)) id)
                            p.ParentUIM;
                        case (onExpandReq rq, p.ParentUIM) of
                          | (True, Some u) =>
                            addCollapsed u
                          | _ =>
                            modifyAppendRequests (cons areq)
                    end
            in
                case (mf.NextReq, p.Depth) of
                  | (None, _) =>
                    set f.NextReqId None;
                    return x
                  | (Some rq, 0) =>
                    addAppendReq pagerInsertPointId rq None;
                    return x
                  | (Some rq, _) =>
                    id <- fresh;
                    tail <- source (insertPoint id);
                    addAppendReq id rq (Some tail);
                    return <xml>{x}{dyn_ (signal tail)}</xml>
            end end
        val topAppendRequests =
            arsOrig <- get appendRequests_;
            let fun depth (ForestParams fp) = fp.Depth in
            offsArs <- List.mapM
               (fn (AppendReq ar) =>
                   t <- Pager.idPositionTop'
                       (fn d t b => return (t <= d.Bottom + Pager.reserved))
                       pager ar.Id;
                   return (t, depth ar.Params, AppendReq ar))
               arsOrig;
            return
               (List.sort (fn (oa,da,_) (ob,db,_) =>
                              oa > ob || (oa = ob && da <= db))
                          (* начиная от max depth, Left одинаковый *)
                          offsArs)
            end
        fun performAppend areqs0 next =
            let val areqs = maybe areqs0 (flip cons []) <|
                    List.find (fn (AppendReq a) => onExpandReq a.TreeReq) areqs0
                    (* если есть запрос разворачивания комментариев,
                       то выполняем только его, иначе для 1 комментария
                       будет еще 14 постов грузить
                     *)
            in
            queueCancellableRpc
                (fn l =>
(*                     debug "performAppend"; *)
(*                     List.app (fn (_,_, AppendReq a) => debug (show a.Id)) ars; *)
                    set loadingAppendRequests True;
                    mtvm <- getMsgTreeViewMode;
                    du <- current discoverySubItemUrl;
                    let val rqs = List.mp (fn (AppendReq a) => a.TreeReq) areqs
                    in
                        set bulkMarkReadPerformed False;
                        case du of
                          | Some u => rpc (Rpcs.getTreeD u mtvm rqs l)
                          | _ => rpc (Rpcs.getTree mtvm rqs l)
                    end)
                (fn mfs =>
                    b <- get bulkMarkReadPerformed;
                    if b then
(*                         debug "cancelling performAppend"; *)
                        next True
                        (* отменяем обработку запроса после
                           skip/mark/untag above/below,
                           т.к. могут прийти старые непрочитанные
                           сообщения вместо уже прочитанных.
                           checkAppend в next повторит запрос
                           с более актуальными appendRequests_
                           (не будет снова запрашивать комментарии
                           к свернутому посту)
                         *)
                    else
(*                     debug "appending"; *)
                    a0 <- get appendRequests_;
                    let fun hasAreq (AppendReq a) =
                            any (fn (AppendReq b) => a.Id = b.Id)
                        fun go mfs ars =
                            case (mfs,ars) of
                              | ((Some mf)::mfs, ((AppendReq ar)::ars)) =>
                                (if hasAreq (AppendReq ar) a0 then
                                  toAppend <- fs ar.Params mf;
(*                                   debug ("inserting " ^ show ar.Id); *)
                                  withSome (fn p => set p toAppend)
                                    ar.InsertPoint;
                                  withSome (fn (UIM u) =>
                                      c <- get u.LoadingChildren;
                                      withSome id c
                                      (* выполняем rollOut-действие *))
                                    (forestParamsParentUIM ar.Params)
                                 else
(*                                    debug ("req " ^ show ar.Id ^ " was removed"); *)
                                   (* при сворачивании комментариев запросы
                                      убираются. Не обрабатываем их, чтобы
                                      не добавлять сотни комментариев внутрь
                                      свернутого поста (а также чтобы избежать
                                      повторного добавления после
                                      restoreUIMAppendReqs).
                                      Если пост успели свернуть и развернуть
                                      обратно, то комментарии добавятся
                                    *)
                                   return ());
                                go mfs ars
                              | (None::mfs, ars) =>
                                modifyAppendRequests
                                  <| List.revAppend
                                  <| List.filter (flip hasAreq a0) ars
                                (* без изменений *)
                              | _ => return ()
                    in
                        modifyAppendRequests
                            (List.filter (fn ar => not (hasAreq ar areqs)));
                            (* оставляем AppendReq, которые могли появиться
                               за время работы performAppend *)
                        (* measure "append" *)
                        (withScrollSaved Settings.msgDivId (go mfs areqs));
                        (* ^ в firefox иногда при append-ах
                           scroll улетает вверх
                           (особенно при инерционной прокрутке,
                            когда видно loading индикатор,
                            если медленно крутить, то все более-менее)
                           на тормоза вроде не влияет.

                           scrollTop вызывает layout, который может и лишний
                           (хотя, все равно будет layout), но без него FF
                           нормально не работает
                         *)
                        set loadingAppendRequests False;
                        next False
                    end)
            end
        (* проверяет необходимость добавления новых элементов
           и вызывает after после (возможного) добавления.
           Если добавление уже в процессе, то after будет вызван
           после следующего check append
         *)
        fun checkAppendLoop () =
            ch <- get checkAppendActive;
            when (not ch) (
            set checkAppendActive True;
            Js.requestAnimationFrameOnce "performAppend" (
            (* поскольку offsetTop в topAppendRequest и scrollTop вызывают
               перерисовку, делаем проверку с таймаутом.
               Особого смысла нет, но Chrome Timeline не показывает
               "Forced synchronous layout is a possible performance bottleneck."
             *)
            ca <- get checkAppendActive; (* вдруг reset прошел *)
            if not ca then return () else
            let fun finalize recheck cancelled =
                    when (not cancelled)
                         (tsn <- get trySelectNextAfterCheckAppend;
                          withSome (fn s =>
(*                             debug "trySelectNextAfterCheckAppend"; *)
                            s) tsn;
                          set trySelectNextAfterCheckAppend None);
                    set checkAppendActive False;
                    when recheck
                         (if cancelled then
                              check ()
                              (* сразу же пускаем повторный запрос *)
                          else
                              sleep 200;    (* небольшой rate limit *)
                              checkAppendLoop ())
                and check () =
                    ars <- (* measure "topAppendRequests" *) topAppendRequests;
                    case ars of
                      | [] => finalize False False (* нечего добавлять *)
                      | (top,_,_) :: _ =>
                        st <- msgsScrollTop;
                        h <- msgsClientHeight;
        (*                 debug ("topAppendRequest: " ^ show top ^ "px; st = " ^ show st ^ "; h = " ^ show h); *)
                        tsn <- get trySelectNextAfterCheckAppend;
                        lc <- get loadingComments;
                        if top < st + h + Pager.reserved || Option.isSome tsn || lc
                        then
                            onScroll True;
                            performAppend
                                (List.mp (fn a => a.3) ars) (finalize True)
                        else
                            finalize False False
            in
                check ()
            end
            ))
        val checkAppend = checkAppendLoop ()
        fun toggleCollapsedReal after (UIM uim) = whenNotBlocked (
            compact <- isCompact (UIM uim);
            vis <- Pager.isIdOnVisiblePage pager uim.FrameId;
            if not compact && not vis then
                return ()
                (* не трогаем сообщения на скрытых страницах, см. toggleFull *)
            else
            Pager.resetPageHeight pager uim.FrameId;
            c <- get uim.Collapsed;
            mv <- get uim.Mv;
            mtvm <- uimViewMode (UIM uim);
            case (mtvm.Posts, mv, c) of
              | (PVMMosaic, MVShort _, Collapsed _) => after
              | _ => (
            set blocked True;
            (* ^ за счет этого одновременно два toggleCollapsed не запустятся *)
            case c of
              | Expanded =>
                ars <- removeUIMAppendReqs (UIM uim) [];
                sm <- Settings.getScrollMode;
                Js.saveLoadedImgSizes uim.GrowId;
                Js.collapseComments 0 uim.GrowId
                    (if compact then "immediate" else sm)
                    (withScrollSaved Settings.msgDivId
                         (set uim.Collapsed (Collapsed ars));
                     (* FF 66 стал прокручивать при раскрытии комментариев *)
                     set blocked False;
                     after;
                     checkAppend)
              | Collapsed ars =>
                ch <- (case uim.SubForest of UIForest f => get f.Children);
                restoreUIMAppendReqs ars;
                let val rollOut =
                        withScrollSaved Settings.msgDivId
                            (set uim.Collapsed Expanded);
                        sm <- Settings.getScrollMode;
                        Js.expandComments 20 uim.GrowId
                                          (if compact then "immediate" else sm)
                                          (set blocked False;
                                           after;
                                           checkAppend)
                    val expand =
                        withScrollSaved Settings.msgDivId
                          (set loadingComments True;
                           set uim.LoadingChildren
                             (Some
                               (withScrollSaved Settings.msgDivId
                                 (set loadingComments False;
                                  set uim.LoadingChildren None);
                                rollOut)));
                        checkAppend
                in
                case (ars, ch) of
                  | ((AppendReq ar) :: [], []) =>
                     if onExpandReq ar.TreeReq then expand else rollOut
                  | _ =>
                    (* если children уже добавлены (или нет append requests),
                       то сразу разворачиваем без loading…
                     *)
                    rollOut
                end))
        fun trySelectNext ro tryFull =
            uimo <- get selectedUIM;
            case uimo of
              | None =>
                uif <- get uiForest; m <- findFirst ro uif;
                withSome (selectS False tryFull False True) m
              | Some m =>
                v <- uimOnValidPage m;
                if not v then
                    selectS False tryFull False True m
                    (* позволяем перейти на первое сообщение на невалидной
                       странице, чтобы показать ее,
                       а дальше игнорируем (но выделяем сообщение
                       на всякий случай). если у нас следующий комментарий
                       и несколько прочитанных перед ним, то прокрутит неточно,
                       но это относительно редкий случай (смена шрифта
                       или ширины окна)
                     *)
                else
                n <- nextMsg ro m;
                (case n of
                   | None =>
                     (* возможен длинный пост и scroll не будет
                        добавлять новые данные
                      *)
                     set trySelectNextAfterCheckAppend
                         (Some (trySelectNext ro tryFull));
                     checkAppend
                   | Some m =>
                     selectS False tryFull False True m)
        fun withSelectedPost f =
            withSelected (fn s => let val p = Uim.postMsg s in
                                      selectS False False False False p; f p end)
        fun selectFirstOr f =
            uimo <- get selectedUIM;
            case uimo of
              | None => trySelectNext False True
              | Some _ => f
        fun collapseAndTrySelectNext (UIM uim) =
            c <- get uim.Collapsed;
            case c of
              | Expanded =>
                Pager.resetPageHeight pager uim.FrameId;
                Js.saveLoadedImgSizes uim.GrowId;
                ars <- removeUIMAppendReqs (UIM uim) [];
                sm <- Settings.getScrollMode;
                Js.collapseComments 0 uim.GrowId sm
                  (set uim.Collapsed (Collapsed ars);
                   trySelectNext False True;
                   preventOnScroll
                   (* дабы избежать лишнего выделения сообщения *))
              | Collapsed _ =>
                trySelectNext False True;
                preventOnScroll
        fun skipComments (UIM uim) =
            set bulkMarkReadPerformed True;
            processRead skipMsgComments
                (fn mid tc uc =>
                    Some (BGSkipComments { MsgId = mid, TotalComments = tc }))
                (UIM uim);
            collapseAndTrySelectNext (UIM uim)
        fun ignorePost (UIM uim) =
            set bulkMarkReadPerformed True;
            processRead skipMsgComments
                (fn mid tc uc =>
                    Some (BGIgnorePost { MsgId = mid, TotalComments = tc }))
                (UIM uim);
            collapseAndTrySelectNext (UIM uim)
        fun later (UIM s) =
            r <- get s.Read;
            when (not r) (toggleRead False (UIM s));
            (* чтобы точно галочка keep unread появилась *)
            toggleRead False (UIM s);
            c <- get s.Collapsed;
            case c of Collapsed _ => trySelectNext False True
                    | Expanded => toggleCollapsed (trySelectNext False True) (UIM s)
        val firstFeedLabelHeight =
            (* первая, чтобы все page up/page down одинаково работали *)
            uif <- get uiForest;
            first <- findFirst False uif;
            maybe (return 0.0) uimFeedLabelHeight first
        val pageUp =
            flh <- firstFeedLabelHeight;
            moveUpDown False False (fn ch => flh+30.0-ch)
        fun pageDown noOnScroll =
            flh <- firstFeedLabelHeight;
            moveUpDown noOnScroll False (fn ch => ch-30.0-flh)
        val prevUnreadFeed = feedKeyboardAction "prevUnreadFeed"
        val nextUnreadFeed = feedKeyboardAction "nextUnreadFeed"
        val loadingCompleteS =
            l <- signal loadingFeed;
            b <- signal blocked;
            ars <- signal appendRequests_;
            return (not l && not b && isNull ars)
        val loadingComplete = current loadingCompleteS
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
            l <- get loadingFeed;
            prev <- (case uimo of
              | None => return None
              | Some uim => prevMsg ro uim);
            if Option.isNone prev && not l then
                prevUnreadFeed
            else
                act
        val nextOrPageDown =
            uimo <- get selectedUIM;
            st <- msgsScrollTop;
            ch <- msgsClientHeight;
            sh <- Js.scrollHeight Settings.msgDivId;
            lc <- loadingComplete;
            case uimo of
              | None => nextOrNextFeed False (trySelectNext False True)
              | Some (UIM uim) =>
                flh <- uimFeedLabelHeight (UIM uim);
                ot <- Pager.idPositionTop pager uim.FrameId;
                chf <- Pager.idClientRectHeight pager uim.FrameId;
                if ot + chf > st + ch then
                    (* прокручиваем текущее сообщение,
                       если оно полностью не видно *)
                    pageDown True (* без автовыделения *)
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
            l <- get loadingFeed;
            case uimo of
              | None => when (not l) prevUnreadFeed
              | Some uim =>
                ot <- uimTop uim;
                st <- msgsScrollTop;
                ch <- msgsClientHeight;
                pm <- prevMsg False uim;
                if st = 0.0 && Option.isNone pm then
                    when (not l) prevUnreadFeed
                else if ot < st && ot > st - ch then
                    moveUpDown False False (fn ch => ot - st)
(*                     pageUp (\* текущее сообщение не полностью видно сверху *\) *)
                else
                    (case pm of
                       | None => pageUp
                       | Some p =>
                         otp <- uimTop p;
                         if otp < st - ch then
                             pageUp (* предыдущее сообщение дальше чем экран *)
                         else
                             trySelectPrev False True)
        fun toggleFullText (UIM uim) =
(*             if uim.Depth > 0 then return () *)
(*             else *)
            mv <- get uim.Mv;
            (case mv of
              | MVShort _ =>
                toggleFull (return ()) mw (UIM uim)
              | _ => return ()
(*                 Js.scrollToElement uim.SnapId (return ()) *)
                (* а toggle full и так будет скролить *)
            );
            let val scrollIfNeeded =
                    case mv of
                      | MVFull _ =>
                        t <- uimTop (UIM uim);
                        scrollTo t (return ())
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
                snapToIfAbove (UIM uim); (* как при сворачивании *)
                set uim.ReadabilityView (RVNone (Some x));
                set uim.ShowText True
              | (RVError _, MVFull _) =>
                snapToIfAbove (UIM uim);
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
                r <- rpc (Rpcs.readability (uimMsgKey (UIM uim)) []);
                scrollIfNeeded;
                (case r of
                  | Left e =>
                    set uim.ReadabilityView (RVError e)
                  | Right t =>
                    set uim.ShowText False;
                    set uim.ReadabilityView
                        (RVReadability (Js.preprocessImg t))
                );
                Pager.resetPageHeight pager uim.FrameId
            end
        fun feedMarkUI addClass cls markVar =
            <xml><div dynClass={fm <- signal markVar;
                                addClass (classes
                                            (if Option.isSome fm then
                                                 cls
                                             else
                                                 Css.displayNone)
                                            Css.feedMark)
                               }
                      dynStyle={fm <- signal markVar;
                                return (case fm of
                                  | None => noStyle
                                  | Some (y,_) =>
                                    prop1 "top" (show y ^ "px"))} >
              {dyn_ (Monad.mp (fn s => case s of
                                         | None => <xml/> | Some (_,t) => t)
                              (signal markVar))}
            </div></xml>
        val flush =
            (* на мобилах сразу же запоминаем состояние прочитанности
               по любому клику, т.к. может открыться вкладка в браузере
               и iOS заморозит home screen app или же просто пользователь
               выключит устройство и клик по keep unread не сохранится
               (а вот помечанию прочитанности по onscroll позволяем потерять
               последние 2 секунды, не так страшно, зато меньше запросов к сети)
             *)
            when isMobile BackgroundRpc.flush
        fun setForest mf mr =
            Js.clearPrevScrollTop;
            set lastPostsViewMode PVMFull;
            o <- msgsScrollTopOffset;
            set lastScrollTop o;
            set lastBfu "";
            onError;
            set appendRequests_ [];
            set markReq mr;
            set selectedUIM None;
            set selectedUIMPosition 0.0;
            set fullUIM None;
            set feedLabels [];
            setFeedMark FMNone;
            clearFeedLabelIds;
            when (not (Js.clearuims ())) (alert "clearuims?");
            f <- mkUIForest mf;
            set uiForest f;
            Pager.reset pager;
            resetCurrentPageSize;
            resetHabrSpoilers;
            _ <- fs (ForestParams
                     { Depth = 0, Parents = [], ParentUIM = None, Forest = f })
                    mf;
            p <- get shareIconsPreloaded;
            when (not p && notNull (case mf of MsgForest f => f.List))
                 (preloadShareIcons;
                  set shareIconsPreloaded True);
            checkAppend
        fun linkClick u = markRead u; P.hide; flush
        fun bglinkClick u =
            linkClick u;
            withSome Js.openLinkInBackgroud (uimMsgLink u)
        (* bfu -> [postId] *)
        val loadedPostsIds : transaction (HashMap.t (list int)) =
            m <- HashMap.new ();
            uif <- get uiForest;
            case uif of UIForest { Children = ch, ... } =>
            children <- get ch;
            List.app
              (fn (UIM { Mi = { MsgKey = { BlogFeedUrl = bfu, ... }
                              , MsgId =
                                { PostId = pid
                                , CommentId = cid, ... }, ... }, ... }) =>
                when (Option.isNone cid)
                  (prev <- HashMap.lookupDefault m [] bfu;
                   HashMap.insert m bfu (pid :: prev)))
              children;
            return m
        val loadedPostsIdsList =
            bind loadedPostsIds HashMap.toList
        val isSearchOrSmartStream =
            cf <- get currentFeed;
            return <| case cf.SIType of
              | SITSearch _ => True
              | SITSmartStream _ => True
              | _ => False
        fun markAboveOrBelow' postsOnly dir mark mkReq s =
            blockUpdateCounters True;
            set bulkMarkReadPerformed True;
            search <- isSearchOrSmartStream;
            (* Cвернутые комментарии (даже если они не загружены)
               помечаются прочитанными везде (в фидах/тегах/поиске).

               В поиске/smart stream помечаются только результаты поиска.
               Свернутые комментарии (при поиске только по постам) считаются
               результатом поиска.
             *)
            let val (above, next, skipFirst) = case dir of
                  | Above =>
                    (True
                    ,prevMsg' postsOnly False
                    ,prevMsg' postsOnly False)
                  | Below =>
                    (False
                    ,nextMsg' postsOnly False
                    ,nextMsg' True False)
                     (* пропускаем первые свёрнутые комментарии самого поста *)
                fun addPost' (UIM uim) ps =
                    case ps of
                      | (UIM up) :: _ =>
                        if up.FrameId = uim.FrameId then
                            ps
                        else
                            UIM uim :: ps
                      | [] => UIM uim :: []
                fun addPost u ps =
                    if uimDepth u = 0 then addPost' u ps else ps
                fun go postsAcc selected first s =
                    case s of
                      | None =>
                        mtvm <- getMsgTreeViewMode;
                        mkReq above search (List.rev postsAcc)
                            selected first mtvm
                      | Some (UIM uim) =>
                        when (not postsOnly || uim.Depth = 0)
                            (mark search (UIM uim));
                        p <- next (UIM uim);
                        go (addPost (UIM uim) postsAcc) selected first p
            in
                mbf <- skipFirst s;
                case mbf of
                  | Some f =>
                    go (if dir = Below then postMsg f :: [] else [])
                        (* добавляем пост первого сообщения, т.к. иначе мы
                           его пропустим, если мы начали с комментария
                           и пошли вниз
                         *)
                       s f mbf
                  | None => return ()
            end;
            blockUpdateCounters False
        fun markAboveOrBelow postsOnly dir mark mkReq =
            s <- get selectedUIM;
            withSome (markAboveOrBelow' postsOnly dir mark mkReq) s
        val unreadSearchResultsCount =
            let fun go p c uims = case uims of
                       | [] => return (p, c)
                       | (UIM uim) :: uims' =>
                         vm <- uimViewMode (UIM uim);
                         read <- get uim.Read;
                         rc <- get (uifUnreadResultsCount uim.SubForest);
                         go (if isSearchResult uim.Mi && not read
                             then p+1 else p)
                            (if vm.ExpandedComments then rc+c else c)
                            uims'
            in
                UIForest f <- get uiForest;
                c <- get f.Children;
                go 0 0 c
            end
        val confirmMarkReadPostCount = 50
        val mkUntaggedCheck =
            si <- getCurrentFeed;
            let fun andUntagged u n =
                    if n > 0 then
                        "(and " ^ show n ^ " " ^ u ^ " "
                        ^ plural n "article" ^ ") "
                    else ""
                fun no u f =
                    (andUntagged u
                    ,fn uim => Monad.mp (not <<< any f) (get (uimTags uim)))
            in
            return <| case si.SIType of
              | SITTag t   => no "untagged" (eq (ITTag t))
              | SITStarred => no "unstarred" (eq ITStarred)
              | SITAllTags => no "untagged" isITTag
              | _ => (const "", const (return False))
            end
        fun numUnreadAndUntagged uims =
            (_, isUntagged) <- mkUntaggedCheck;
            ss <- isSearchOrSmartStream;
            let val result = if ss then isSearchResult else isResult
            in
                List.foldlM
                  (fn u (ur, ut) =>
                      r <- get (uimRead u);
                      untagged <- isUntagged u;
                      return <| if not r && isPost u && result (uimMi u)
                        then (if untagged then (ur, ut+1) else (ur+1, ut))
                        else (ur, ut))
                  (0, 0) uims
            end
        fun numUnreadAndUntaggedPostsAbove countFirst s =
            numUnreadAndUntagged
              (if uimDepth s > 0 then
                   postMsg s :: uimPrev (postMsg s)
               else if countFirst then
                   s :: uimPrev s
               else
                   uimPrev s)
        fun numUnreadAndUntaggedPostsBelow s =
            let fun nextPosts acc u =
                    nu <- get (uimNext u);
                    case nu of
                      | None => return (List.rev acc)
                      | Some n => nextPosts (n :: acc) n
            in
                n <- nextPosts [] (postMsg s);
                numUnreadAndUntagged n
            end
        val numUnreadPosts =
            si <- get currentFeed;
            cnt <- get si.Counters;
            return <| cnt.TotalPosts - cnt.ReadPosts
        val markReadPosts = List.mp (fn u => (uimTime u, uimMsgId u))
        fun markAboveOrBelowAsRead' dir s =
            lc <- loadingComplete;
            (na, naUntagged) <- numUnreadAndUntaggedPostsAbove (dir = Below) s;
            (untagged, _) <- mkUntaggedCheck;
            up <- numUnreadPosts;
            (* подтверждение считает только посты, а в случае с тегами
               на верхнем уровне могут быть и комментарии, но чтобы их
               отличать от просто комментариев потребуются отдельные счетчики
             *)
            sRead <- get (uimRead s);
            (dirS, numPosts, numUntagged) <-
              (if dir = Above then
                   return ("above", na, naUntagged)
               else
                   (_, nbUntagged) <- numUnreadAndUntaggedPostsBelow s;
                   return ("below", up - na, nbUntagged));
            ok <- (if numPosts > confirmMarkReadPostCount then
              confirm <| "Do you really want to mark " ^ show numPosts
                ^ " articles " ^ untagged numUntagged ^ dirS ^ " as read?"
            else
              return True);
            when ok (
            markAboveOrBelow'
              False dir
              (fn searchOrSmartStream (UIM uim) =>
                  (* если комментарий свернутый, то всё внутри надо пометить,
                     т.к. там может быть незагруженное поддерево
                     т.е. делаем skip? не совсем, в поиске только результаты
                     помечаем
                   *)
                  when (if searchOrSmartStream then
                            isSearchResult uim.Mi
                        else
                            isResult uim.Mi)
                       (set uim.KeepUnread False;
                        r <- get uim.Read;
                        when (not r)
                             (toggleReadNoBgAction (UIM uim))))
              (fn above searchOrSmartStream posts selected first mtvm =>
                  cf <- get currentFeed;
                  search <- current isSearch;
                  (* ^ отдельные счетчики *)
                  (* Комментарии могут быть еще не загружены
                     (свернули до окончания загрузки,
                      вообще не загружали (не expanded view),
                      или mark below с комментария)
                     и счетчик “N comments” останется > 0.
                     По-этому считаем, сколько загруженных осталось
                     непрочитанными (не ставим 0 из-за mark below).
                   *)
                  List.app
                    (fn uim =>
                        let val f = uimSubForest uim
                        in
                            vm <- uimViewMode uim;
                            rc <- get (uifUnreadResultsCount f);
                            (rc', _, _) <- fixResultsCount' f;
(*                             debug ("rc = " ^ show rc ^ "; rc' = " ^ show rc'); *)
                            when (search && vm.ExpandedComments)
                                 (updateCounters cf 0 (rc' - rc))
                        end)
                    (if above && uimDepth selected > 0
                     (* если идем вверх от комментария, то не трогаем
                        первый пост, т.к. у него могут остаться не загружены
                        действительно непрочитанные комментарии
                      *)
                     then List.drop 1 posts else posts);
                    (* в случае поиска считаем, сколько осталось
                       непрочитанных результатов поиска и обновляем
                       счетчик (у поиска он отдельный и не
                       обновится при updateSubscriptions)
                     *)
                  when (search && not above)
                    ((p,c) <- unreadSearchResultsCount;
                     modify
                       cf.Counters
                       (fn (cn : counters) =>
                           setF2 [#ReadPosts] [#ReadComments]
                                 (cn.TotalPosts - p) (cn.TotalComments - c) cn)
                    );
                  mr <- get markReq;
                  let fun rMid uim = (uimDepth uim = 0, uimMsgId uim)
                      val point =
                            { Point =
                                (uimTime (postMsg selected)
                                ,rMid selected
                                ,rMid first) }
                  in
                      BackgroundRpc.addAction
                        (BGMarkRead
                         { Direction =
                             if above then MRDAbove point else MRDBelow point
                         , ViewMode = mtvm
                         , OlderThan = 0
                         , Posts =
                             markReadPosts
                             (if not above && uimDepth selected > 0
                              then List.drop 1 posts else posts)
                             (* сам пост при mark below из комментария не помечаем *)
                         , MarkReq = mr
                         });
                      blockUpdateCounters False;
                      updateMarkReqReadCounters mtvm mr
                        (List.mp uimMsgId posts)
                  end)
              s)
        fun markAboveOrBelowAsRead dir =
            s <- get selectedUIM;
            withSome (markAboveOrBelowAsRead' dir) s
        fun untagAboveOrBelow postsOnly dir mark mkReq =
            s <- get selectedUIM;
            withSome (markAboveOrBelow' postsOnly dir mark mkReq) s
        fun clearTags' tags d reload =
            let val rmTags =
                    case tags of
                      | Some (t :: _) => removeTag t
                      | _ => replaceITTags []
            in
            blockUpdateCounters False;
            case d of
              | ASDirection (dir, from) =>
                f <- maybe (get selectedUIM) (const <| return from) from;
                withSome (markAboveOrBelow' True dir (const rmTags)
                  <| fn above _ _ _ _ mtvm =>
                    rqs <- (if above then return [] else get appendRequests_);
                    BackgroundRpc.addAction
                      (BGRemoveTagFromTree
                       { Above = above
                       , Tags = tags
                       , ViewMode = mtvm
                       , TreeReqs = List.mp (fn (AppendReq a) => a.TreeReq) rqs
                       });
                    updateSubscriptions) f
              | ASOlderThan d =>
                BackgroundRpc.addAction
                    (BGRemoveTagD { Tags = tags, OlderThan = d });
                updateSubscriptions; reload
            end
        fun clearTags tags t d reload =
            askBefore
              ("Are you sure you want to " ^
                (case tags of
                  | Some (ITStarred :: []) => "unstar"
                  | Some ((ITTag t) :: []) =>
                    "remove tag “" ^ t.TagName ^ "” from"
                  | None => "remove tags from"
                  | _ => "untag" (* can’t happen? *)
                  )
                ^ " " ^ Js.toLowerCase t ^ "?\nWARNING: This operation can NOT be undone!")
              (clearTags' tags d reload)
        fun markAllAsRead t d reloadOrUpdate =
            mr <- get markReq;
            if isEmptyMarkReq mr then
                return () (* идет загрузка фида или пустой фид *)
            else
            up <- numUnreadPosts;
            uif <- get uiForest;
            ch <- get (uifChildren uif);
            (untagged, _) <- mkUntaggedCheck;
            (_, numUntagged) <- numUnreadAndUntagged ch;
            ok <-
              (if up > confirmMarkReadPostCount && d = 0 then
                   confirm ("Do you really want to mark all "
                            ^ show up ^ " articles "
                            ^ untagged numUntagged ^ "as read?")
               else if d > 0 then
                   confirm ("Do you really want to mark all "
                            ^ Js.toLowerCase t ^ " as read?")
               else
                   return True);
            when ok
              (mtvm <- getMsgTreeViewMode;
               uif <- get uiForest;
               ch <- get (uifChildren uif);
               BackgroundRpc.addAction
                   (BGMarkRead
                    { Direction = MRDAll
                    , ViewMode = mtvm
                    , OlderThan = d
                    , Posts = markReadPosts ch
                    , MarkReq = mr
                    });
               setForest emptyMF emptyMarkReq;
               reloadOrUpdate
              )
        fun showUimContextMenu uim pos =
            p <- Monad.mp Option.isSome (prevMsg True uim);
            n <- Monad.mp Option.isSome (nextMsg True uim);
            parent <- parentMsg uim;
            comments <- hasChildren (uimSubForest uim);
            vm <- uimViewMode uim;
            (tagged, tags) <- Monad.mp isTagFeed getCurrentFeed;
            P.showContextMenu (Some (Left pos)) <xml>
              {displayIfNotC (uimMi uim).ReadLocked <|
               dyn_ <|
                r <- signal (uimRead uim);
                return <| if r then
                  spanClass Css.read
                    <| P.lii Css.iconKeepUnread "Unread"
                    <| toggleRead True uim
                else
                  P.lii Css.iconKeepUnread "Read" (toggleRead True uim)}
              {dyn_ <|
                s <- uimStarred uim;
                return <| if s then
                  spanClass Css.starred
                    <| P.lii Css.iconStar "Unstar" (toggleStarred uim)
                else
                  P.lii Css.iconStar "Star" (toggleStarred uim)}
              {P.lii Css.iconMsgTag "Tag"
                    (cp <- P.currentMousePosition;
                     addTagMenu (setF [#Top] cp.Top pos) uim)}
              {case uimMsgLink uim of
                 | Some l => <xml>
                   {P.lii Css.iconShare "Share"
                     (c <- shareMenuContents uim l;
                      cp <- P.currentMousePosition;
                      P.showContextMenu (Some (Left (setF [#Top] cp.Top pos))) c)}
                   {P.llii Css.iconEmpty "Open link" l}
                   </xml>
                 | None => <xml/>}
              {displayIfC (uimDepth uim = 0
                           && (case vm.Posts of PVMFull => False | _ => True))
                <| dyn_ <|
                  mv <- signal (uimMv uim);
                  return (case mv of
                    | MVFull _ =>
                      P.lii Css.iconEmpty "Collapse"
                        (toggleFull (select uim) mw uim)
                    | _ => <xml/>)}
              {case parent of
                 | Some p => P.lii Css.iconEmpty "Parent"
                   (selectS True False False True p)
                 | None => <xml/>}
              {displayIfC (not (isPost uim) && comments)
                (dynS
                  (fn c =>
                    P.lii Css.iconEmpty
                      (case c of
                        | Expanded => "Collapse comments"
                        | Collapsed _ => "Expand comments")
                      (toggleCollapsedReal (return ()) uim))
                  (uimCollapsed uim))}
              {let fun mark p icon what dir =
                       displayIfC p
                         <| P.lii icon ("Mark" ^ what ^ " as read")
                         <| markAboveOrBelowAsRead' dir uim
               in
                 displayIfC (p || n) <xml>
                   <hr/>
                   {mark p Css.iconPrev " above" Above}
                   {mark n Css.iconNext " below" Below}
                   </xml>
               end}
            </xml>
        fun contextMenu uim =
            hasLink <- Js.eventTargetHasLink;
            cls <- Js.eventTargetClasses;
            if hasLink
               || elem "msgCollapseMargin" cls
               || (elem "magazineDescription" cls && notElem "msgFooter" cls)
               || elem "mosaicDescription" cls
               || elem "msgCommentsButton" cls then
                (* ничего не делаем внутри сообщения, при клике по заголовку,
                   времени или кнопке N comments *)
                return ()
            else
                pos <- P.currentMousePosition;
                showUimContextMenu uim pos;
                preventDefault
        val jsInit =
            uimOnClick Css.iconKeepUnread (fn u => toggleRead True u; P.hide; flush);
            uimOnClick Css.iconStar (fn u => toggleStarred u; P.hide);
            uimOnClick Css.iconMsgTag addTagMenuUnderButton;
            uimOnClick Css.iconShare toggleShareMenu;
            uimOnClick Css.mtime linkClick;
            uimOnClick Css.postHeaderTime linkClick;
            uimOnClick Css.msubject linkClick;
            uimOnMiddleclick Css.mtime linkClick;
            uimOnMiddleclick Css.postHeaderTime linkClick;
            uimOnMiddleclick Css.msubject linkClick;
            uimOnMiddleclick Css.magazineImage bglinkClick;
            uimOnMiddleclick Css.mosaicImage bglinkClick;
            uimOnMiddleclick Css.postHeader bglinkClick;
            uimOnMiddlemousedown Css.magazineImage (const preventDefault);
            uimOnMiddlemousedown Css.mosaicImage (const preventDefault);
            uimOnMiddlemousedown Css.postHeader (const preventDefault);
            uimOnContextmenu Css.magazineImage contextMenu;
            uimOnContextmenu Css.mosaicImage contextMenu;
            uimOnContextmenu Css.postHeader contextMenu;
            uimOnContextmenu Css.msgFrame contextMenu;
            uimOnContextmenu Css.msgFooter contextMenu;
            uimOnLongtap Css.magazineImage contextMenu;
            uimOnLongtap Css.mosaicImage contextMenu;
            uimOnLongtap Css.postHeader contextMenu;
            (* чтобы в Firefox не появлялся scroller там, где
               может быть обработчик *)
            uimOnClick Css.msgCommentsButton (fn u => commentsButtonClick u; P.hide);
            uimOnClick Css.msgFrame (fn u => msgFrameClick u; P.hide; flush);
            uimOnMiddleclick Css.msgFrame (fn u =>
                msgFrameMiddleclick u; P.hide; flush);
            set_showUimContextMenu showUimContextMenu
    in
        when (not (setmw
            { FullUIM = fullUIM
            , SelectedUIM = selectedUIM
            , Select = selectS False False
            , Blocked = blocked
            , ToggleRead = toggleRead
            , ToggleStarred = toggleStarred
            , RemoveITTag = removeITTag
            , MarkRead = markRead
            , TryMarkRead = tryMarkRead
            , MarkIfDown = markIfDown
            , ToggleCollapsed = toggleCollapsed
            , CheckAppend = checkAppend
            , UpdateFeedMark = updateFeedMark
            , SaveSelectedUIMPosition = saveSelectedUIMPosition
            , RestoreSelectedUIMPosition = restoreSelectedUIMPosition
            }
             )) (alert "setmw?");
        set toggleCollapsed_ toggleCollapsedReal;
        return
        { JsInit = jsInit
        , FeedMarkTopHtml =
            fn addClass => feedMarkUI addClass Css.feedMarkTop feedMarkTop
        , Html = <xml><span
          dynClass={sm <- signal Settings.scrollMode;
                    nt <- signal noTransitions;
                    return (case (sm, nt) of
                              | (SMImmediate, _) => null
                              | (_, True) => null
                              | _ => smoothCursorTransitions)}>
            {actionHintUI}
            {feedMarkUI return Css.feedMarkMoving feedMarkMoving}
            {Pager.html pager}
            {insertPoint pagerInsertPointId}
          </span></xml>
        , HtmlPre = <xml>
            <div class="snapHeight" id={Settings.snapHeightId}></div>
          </xml>
        , OnScroll = onScroll False
        , LoadingComments = signal loadingComments
        , LoadingAppendRequests = signal loadingAppendRequests
        , AppendRequests = signal appendRequests_
        , OnError = onError
        , CheckAppend = checkAppend
        , SetForest = setForest
        , IsForestEmpty =
            UIForest f <- signal uiForest;
            fc <- signal f.FirstChild;
            return (Option.isNone fc)
        , SelectedUIM = selectedUIM
        , ToggleRead = fn ah => whenNotBlocked (withSelected (mw.ToggleRead ah))
        , ToggleStarred = whenNotBlocked (withSelected toggleStarred)
        , ClearStarred =
          whenNotBlocked (withSelected (fn u =>
                                             s <- current (uimStarred u);
                                             when s (toggleStarred u)))
        , EditTags = whenNotBlocked (withSelected editTags)
        , SkipComments = whenNotBlocked (selectFirstOr (withSelected skipComments))
        , SkipPost = whenNotBlocked (selectFirstOr (withSelectedPost skipComments))
        , PageUp = whenNotBlocked pageUp
        , PageDown = whenNotBlocked (pageDown False)
        , LineUp = whenNotBlocked (moveUpDown False True (fn _ => -40.0))
        , LineDown = whenNotBlocked (moveUpDown False True (fn _ => 40.0))
        , Home = whenNotBlocked (st <- msgsScrollTop;
                                   moveUpDown False False (fn _ => -st))
        , End = whenNotBlocked (st <- msgsScrollTop;
                                  sh <- Js.scrollHeight Settings.msgDivId;
                                  moveUpDown False False (fn ch => sh-ch-st))
        , Next = whenNotBlocked (nextOrNextFeed True (trySelectNext True False))
        , Prev = whenNotBlocked (prevOrPrevFeed True (trySelectPrev True False))
        , NextTryFull = whenNotBlocked (nextOrNextFeed False (trySelectNext False True))
        , PrevTryFull = whenNotBlocked (prevOrPrevFeed False (trySelectPrev False True))
        , NextOrPageDown = whenNotBlocked nextOrPageDown
        , PrevOrPageUp = whenNotBlocked prevOrPageUp
          (* scrollOrAccum в space/shift+space неправильно, т.к. он сообщения
             выделяет, а не страницы
           *)
        , Up = whenNotBlocked (selectFirstOr (trySelectPrev' (fn _ => parentMsg) False True))
        , Later = whenNotBlocked (selectFirstOr (withSelected later))
        , LaterPost = whenNotBlocked (selectFirstOr (withSelectedPost later))
        , IgnorePost = whenNotBlocked (selectFirstOr (withSelectedPost ignorePost))
        , ToggleCollapsed = whenNotBlocked (withSelected (toggleCollapsed (return ())))
        , ToggleFull =
(*           measure "toggleFull" *)
          (whenNotBlocked (withSelected
             (fn s => toggleFull (select s) mw s)))
        , TryMakeShort =
          whenNotBlocked (withSelected
           (fn (UIM uim) =>
               mv <- get uim.Mv;
               case mv of
                 | MVFull _ =>
                   toggleFull (select (UIM uim)) mw (UIM uim)
                 | _ =>
                   trySelectPrev' (fn _ => parentMsg) False True))
        , ToggleFullText =
          whenNotBlocked (withSelected toggleFullText)
        , JumpToLink = fn openFunc => whenNotBlocked (withSelected
          (fn (UIM s) =>
              mv <- get s.Mv;
              case mv of
                | MVShort { CachedMsg = Some m, ... } =>
                  withSome (openFunc (UIM s)) m.Link
                | MVShort _ =>
                  toggleFull (return ()) mw (UIM s)
                | MVFull { Msg = m } =>
                  withSome (openFunc (UIM s)) m.Link))
        , JumpToTranslate =
          fn openFunc =>
             whenNotBlocked (withSelected (translate openFunc))
        , HasAbove =
          s <- signal selectedUIM;
          case s of
            | Some uim =>
              Unsafe.boolTransactionToSignal
                  (Monad.mp Option.isSome (prevMsg True uim))
            | None => return False
        , HasBelow =
          s <- signal selectedUIM;
          case s of
            | Some uim =>
              Unsafe.boolTransactionToSignal
                  (Monad.mp Option.isSome (nextMsg True uim))
              (* по-правильному нужно учитывать только appendRequests
                 ниже selectedUIM
               *)
            | None =>
              return False
        , MarkAboveOrBelowAsRead = markAboveOrBelowAsRead
        , MarkAllAsRead = markAllAsRead
        , ClearTags = clearTags
        }
    end
