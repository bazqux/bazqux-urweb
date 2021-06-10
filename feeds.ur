open Datatypes
open Css
open Utils
open SubItem
open Either
open Uim
structure P = Popups

fun tryPushInterfacePath p =
    p0 <- Js.getInterfacePath;
    when (p0 <> p) (Js.pushInterfacePath p)

fun toggleFolder si =
    vm <- get si.ViewMode;
    let fun notfe ex =
            case ex of
              | MTVMFolderCollapsed => MTVMFolderExpanded
              | MTVMFolderExpanded => MTVMFolderCollapsed
              | MTVMEx f => MTVMEx (modifyF [#FolderExpanded] not f)
        val vm' = modifyF [#Ex] notfe vm
    in
        set si.ViewMode vm';
        updateCounters si 0 0 (* обновляет CSS-класс *);
        case si.SIType of
          | SITFolder { Folder = name } =>
            BackgroundRpc.addAction (BGSetFolderViewMode
                                         { Folder = name, ViewMode = vm' })
          | SITAllTags =>
            BackgroundRpc.addAction (BGSetFolderViewMode
                                         { Folder = ",SITAllTags", ViewMode = vm' })
          | _ => return ()
    end
fun renameDialog what name act =
    tid <- fresh;
    text <- source name;
    let val ren =
            t <- get text;
            if t = name then
                P.hide
            else
                c <- Js.checkName what t;
                withSome (fn t => act t; P.hide) c
    in
    d <- P.newBox ("Rename " ^ what ^ " “" ^ name ^ "” to")
      (oneLineInputAndOkButton tid text "" "OK" ren);
    P.toggle d;
    Js.select tid;
    Js.focus tid
    end
fun setDefaultFeed setFeed =
    tryPushInterfacePath "folder/";
    setFeed defaultSubItem
fun unsubscribeLiI unsubscribe setFeed title act (sis : list subItem) : xbody =
    P.lii Css.iconEmpty title (
    askBefore
      (case sis of
        | { SIType = SITSmartStream s, ... } :: [] =>
          "Are you sure you want to delete smart stream “"
            ^ s.StreamName ^ "”?"
        | si :: [] =>
          "Are you sure you want to unsubscribe from “"
            ^ si.Title ^ "”?"
        | _ => "Are you sure you want to unsubscribe from "
            ^ show (List.length sis) ^ " subscriptions and delete the folder? This action can not be undone.")
      (act;
       setDefaultFeed setFeed;
       unsubscribe sis (return ())))

datatype aboveBelow = Above | Below

val eq_aboveBelow : eq aboveBelow =
    mkEq (fn a b => case (a,b) of
      | (Above, Above) => True
      | (Below, Below) => True
      | _ => False)

datatype articlesSelector
  = ASOlderThan of int
  | ASDirection of aboveBelow * option uim

fun articlesOlderThanMenu searchS hasAboveBelow m titlePrefix beforeOlderThan =
    dyn_ <|
    search <- searchS;
    let val a = if search then "Results" else "Articles"
        fun lii i t s = P.lii i t (m (a ^ " older than " ^ t) s)
        fun li t d = lii Css.iconEmpty t (ASOlderThan d)
        fun lio t d = lii Css.iconEmpty t (ASOlderThan d)
    in
    return <xml>
      {case hasAboveBelow of
         | Some (hasAbove, hasBelow) => <xml>
           {disabledIf (Monad.mp not hasAbove)
             (lii Css.iconPrev (a ^ " above current") (ASDirection (Above, None)))}
           {disabledIf (Monad.mp not hasBelow)
             (lii Css.iconNext (a ^ " below current") (ASDirection (Below, None)))}
           </xml>
         | None => <xml/>}
      {li (if search then "All search results" else "All articles") 0}
      {beforeOlderThan}
      {P.menuLabel (titlePrefix ^ " " ^ Js.toLowerCase a ^ " older than…")}
      {lio "1 day" 1}
      {lio "2 days" 2}
      {lio "3 days" 3}
      {lio "1 week" 7}
      {lio "2 weeks" 14}
      {lio "a month" 30}
      {lio "a year" 365}
    </xml>
    end

val eqPFT : eq publicFeedType =
    mkEq (fn a b => case (a,b) of
      | (PFTAll, PFTAll) => True
      | (PFTStarred, PFTStarred) => True
      | (PFTAllTags, PFTAllTags) => True
      | (PFTTag { TagName = t1 }, PFTTag { TagName = t2 }) => t1 = t2
      | (PFTFolder { Folder = f1 }, PFTFolder { Folder = f2 }) => f1 = f2
      | (PFTSmartStream { StreamName = s1 }, PFTSmartStream { StreamName = s2 }) => s1 = s2
      | _ => False)

fun clipboardCopyInput a = activeXml
    (e <- Js.copyToClipboardEnabled;
     if e then
         tid <- fresh;
         text <- source a;
         spawn (Js.setReadOnly tid True);
         return (oneLineInputAndOkButton tid text "" "Copy"
             (Js.copyToClipboard tid))
     else
         return (hrefLinkStopPropagation (txt a) a))

fun publicFeedDialog typ publicFeeds : transaction {} =
    let fun disabled t =
            <xml><span class="publicFeedDisabled">No feed</span><br/></xml>
        fun e act =
            BackgroundRpc.queueRpcB
                act
                (fn pfs =>
                    modify publicFeeds (fn l => case List.assoc typ l of
                        | None => (typ, pfs) :: l
                        | Some _ => List.mp (fn (t,p) =>
                                                if t = typ then (t,pfs)
                                                else (t,p)) l)
                )
        val enable =
            <xml>{textButton "Enable" (e (fn l => rpc (Rpcs.enablePublicFeed typ l)))}</xml>
    in
    d <- P.newBox
      ("Public feed for " ^
       (case typ of
          | PFTAll => "Latest"
          | PFTStarred => "Starred items"
          | PFTAllTags => "Tags"
          | PFTTag { TagName = t } => "tag “" ^ t ^ "”"
          | PFTFolder { Folder = f } => "folder “" ^ f ^ "”"
          | PFTSmartStream { StreamName = s } => "smart stream “" ^ s ^ "”"
      ) ^ ":")
      (dyn_ (pf <- signal publicFeeds;
             return (case List.assoc typ pf of
               | Some l => <xml>
                 {List.mapX (fn (f,e,_) =>
                     let val a = "https://bazqux.com/feed/" ^ f
                     in
                         <xml>{
                         if e then
                             clipboardCopyInput a
                         else
                             disabled a
                         }</xml>
                     end) l}
                 {if List.all (fn (_,e,_) => not e) l then enable else <xml><p></p><p>
                    {textButton "Disable" (e (fn l => rpc (Rpcs.disablePublicFeed typ l)))}
                    {textButton "Generate new"
                      (askBefore
                        "Current feed will be deleted. Are you sure you want to generate new feed address?"
                        (e (fn l => rpc (Rpcs.generateNewPublicFeed typ l))))}</p>
                    {hrefLinkStopPropagation
                         (txt "New IFTTT applet") "https://ifttt.com/create"}
                    (select “RSS Feed” service and paste public feed URL)<br/>
                    {hrefLinkStopPropagation
                         (txt "New Zap in Zapier")
                         "https://zapier.com/webintent/create-zap"}
                    (select “RSS by Zapier” app)<br/>
                    {hrefLinkStopPropagation
                         (txt "New Integromat scenario")
                         "https://www.integromat.com/scenario/add"}
                    (select “RSS” module)<br/>
                 </xml>}
                 </xml>
               | None => <xml><p>{disabled "No feed"}</p>{enable}</xml>)));
    P.toggle d
    end

fun feedAddressDialog f =
    d <- P.newBox "Feed address:" (clipboardCopyInput f.Subscription.Url);
    P.toggle d

ffi set_setDiscoveryFeed : (string -> string -> option string -> option string -> option msgTreeViewMode -> transaction {}) -> transaction {}
con dragAndDropInfo =
    { What          : subItemType
    , InsertAfter   : option subItemType
    , SourceFolder  : option string
    , TargetFolder  : option string
    }
ffi registerOnDragAndDrop : (dragAndDropInfo -> transaction {}) -> transaction {}
ffi registerMTVMFolderExpanded : mTVMEx -> transaction {}

ffi resetHabrSpoilers : transaction {}

val discoverySubItemIndex = -100
val discoverySubItemUrl =
    cf <- currentFeedS;
    return (case (cf.Index = discoverySubItemIndex, cf.SIType) of
      | (True, SITFeed f) =>
        Some f.Subscription.Url
      | _ =>
        None)

con filterAction = (option string -> transaction {}) -> transaction {}

con filtersAndSmartStreams =
    list (int * filterQueryRpc) * list (string * filterQueryRpc)

con ssWidget =
    {Html : xbody,
     PublicFeeds : source (list (Datatypes.publicFeedType *
                                 list (string * bool * option string))),
     AddSubscription : string -> transaction {},
     AddDiscoverySubscription : string -> string -> string -> transaction {},
     UpdateSubscriptionsIgnoringErrors : transaction {},
     (* ^ обновление подписок в фоне с игнорированием ошибок *)
     UpdateSubscriptionsThen : transaction {} -> transaction {},
     (* обновление подписок с обработкой ошибок и вызовом пользовательской
        ф-ии в случае успеха
        Если фоновое обновление уже идет, запрашивает повторное обновление
        и выходит, не вызывая пользовательскую ф-ю, чтобы не было перегрузки
      *)
     UpdateSubscriptions_ :
     ((option (Binary_ffi.xbodyString * string * (list string)))
      * string
      * list subItemRpc
      * option (filtersAndSmartStreams * string)
      * list (time * string * string))
     -> transaction {},
     UpdateMarkReqReadCounters : msgTreeViewMode -> markReq -> list msgId -> transaction {},
     OnError : transaction {},
     Rename : subItem -> string -> transaction {},
     Unsubscribe : list subItem -> transaction {} -> transaction {},
     RetryScanning : subItem -> transaction {},
     SubMenu : P.popup,
     SubItemMenuContents : bool -> subItem -> option xbody,
     FiltersAndSmartStreams : source filtersAndSmartStreams,
     DeleteFilter : int -> filterAction,
     DeleteSmartStream : string -> filterAction,
     AddSmartStream : string -> string -> list int -> filterAction,
     EditSmartStream : string -> string -> list int -> filterAction,
     AddFilter : string -> bool -> list int -> filterAction,
     EditFilter : int -> string -> bool -> list int -> filterAction,
     (* для queueCancellableCustomUpdate *)
     CustomUpdatesRunning : source int,
     MarkReqReadCountersSeqNum : source int,
     QueueUpdate : transaction {},
    }

fun queueCancellableCustomUpdate' [a] seqNum queueUpdate customUpdatesRunning (f : list Datatypes.bgAction -> transaction a) (r: a -> transaction {}) : transaction {} =
    modify customUpdatesRunning succ;
    BackgroundRpc.queueCRpcB seqNum
        f
        (fn x =>
            modify customUpdatesRunning pred;
            r x)
        (queueUpdate;
         modify customUpdatesRunning pred)

val setFeedSeqNum = Unsafe.intSource "Feeds.setFeedSeqNum" 0

(* вынес отдельно, т.к. Ur/Web не работает с полиморфными полями в записи *)
fun queueCancellableCustomUpdate [a] updateSubscriptionsIfCancelled (ssWidget : ssWidget) (f : list Datatypes.bgAction -> transaction a) (r: a -> transaction {}) : transaction {} =
    modify ssWidget.MarkReqReadCountersSeqNum succ;
    (* ^ при новом setFeed отменяем все updateMarkReqReadCounters *)
    queueCancellableCustomUpdate'
        setFeedSeqNum
        (when updateSubscriptionsIfCancelled ssWidget.QueueUpdate
         (* при отмене setFeed после markAllAsRead  *))
        ssWidget.CustomUpdatesRunning
        f r

fun queueCancellableRpc [a] (f : list Datatypes.bgAction -> transaction a) (r: a -> transaction {}) : transaction {} =
    BackgroundRpc.queueCRpcB_ setFeedSeqNum f r (return ())

fun subscriptionsWidget updateCurrentFeed (onUpdateSubInfo : subItem -> subItem -> transaction {}) setFeed onlyUpdatedSubscriptions exactUnreadCounts subscribeDiscoveryFeed clearMtvm reload editStreamDialog updatePaidTill hasAbove hasBelow clearTags : transaction ssWidget =
    selected <- source (None : option subItem);
    html <- source <xml/>;
    lastRenTime <- source minTime;
    updatePending <- source False;
    updateRunning <- source False;
    customUpdatesRunning <- source 0;
    markReqReadCountersSeqNum <- source 0;
    titleHash <- source "";
    subItemsVersion <- source "";
    updateCount <- source 0;
    queueUpdateSrc <- source (return ());
    folders <- source [];
    publicFeeds <- source [];
    filtersAndSmartStreams <- source (([], []) : filtersAndSmartStreams);
    filtersAndSmartStreamsHash <- source "";
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
                    (* rename-ов может быть несколько за раз:
                       url -> normalizedUrl
                       normalizedUrl -> subscribedUrl
                       ищем до упора
                     *)
                    let val r = findRen u subUrlRenames
                    in
                        if elem r prev then r else go r (r::prev)
                    end
            in
                go u (u::[])
            end
        fun updateSubscriptions' (x, siv, sirs, fss, subUrlRenames) = (* withSetFeed *)
            (*             t1 <- now; *)
            r <- get needRetryPendingUpdates;
            if r then
                queueUpdate
                (* ^ перезапускаем запрос, если во время запроса подписки уже
                     обновили или изменились счетчики *)
            else
            retryPendingUpdates; (* если шло обновление, то перезапустим *)
            path0 <- Js.getInterfacePath;
            sitSel0 <- getSubItemByPath path0;
            let val path = tryRename path0 subUrlRenames
            in
            when (path <> path0) (Js.replaceInterfacePath path);
            (case x of
               | None => return ()
               | Some (x, th, fs) =>
                 set titleHash th;
                 f <- get folders;
                 when (fs <> f)
                      (set folders fs);
                 set html (Unsafe.xbodyStringToXbody x));
            set subItemsVersion siv;
            setSubItems (Option.isNone x) sirs;
            withSome (fn (fs, h) =>
                         set filtersAndSmartStreams fs;
                         set filtersAndSmartStreamsHash h)
                     fss;
            when (Option.isNone fss && Option.isSome x)
                 (* "обновим" фильтры, т.к. заголовки поменялись *)
                 (fs <- get filtersAndSmartStreams;
                  set filtersAndSmartStreams ([], []);
                  (* ^ чтобы точно обновилось *)
                  set filtersAndSmartStreams fs);
            (case subUrlRenames of
                 (t,_,_) :: _ => set lastRenTime t | _ => return ());
            (* проверяем, а не обновился ли текущий выделенный фид *)
            sitSel1 <- getSubItemByPath path;
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
            end
        fun updateSubscriptionsIgnoringErrors updateFilters =
            (* не делаем queueRpcB, т.к. это блокирует остальные rpc
               и дико тормозит прокрутка и добавление к дереву в процессе
               подписки.
             *)
            r <- get updateRunning;
            cur <- get customUpdatesRunning;
            if r || cur > 0 then
                queueUpdate
            (* дабы не пускать слишком много запросов,
             особенно, если “s” нажать и держать *) else
            set updateRunning True;
            s <- BackgroundRpc.tryWithBGRpcList
                (fn l =>
                    clearRetryPendingUpdates;
                    t <- get lastRenTime;
                    fssh <- get filtersAndSmartStreamsHash;
                    h <- get titleHash;
                    siv <- get subItemsVersion;
                    a <- Js.isWindowActive;
                    r <- (if updateFilters then
                      tryRpc (Rpcs.subscriptions (not a) t h siv fssh l)
                    else
                      tryRpc (Rpcs.subscriptions_ (not a) t h siv fssh l));
                    case r of
                      | Some (r', pt) =>
                        updatePaidTill pt;
                        return (Some r')
                      | None => return None
                );
            set updateRunning False;
            (case s of
              | Some s =>
                cur' <- get customUpdatesRunning;
                when (cur' = 0) (updateSubscriptions' s)
                (* не делаем queueUpdate, если был запрошен customUpdate,
                   т.к. он будет избыточным (подписки и так обновятся полностью,
                   а после setFeed лучше фид качать, чем подписки обновлять,
                   при отмене setFeed после mark all as read
                   queueUpdate и так делается)
                 *)
              | None =>
(*                 debug ("subscriptions request failed (" ^ show (ec+1) ^ " times)"); *)
                queueUpdate)
        val queueUpdate_ =
            p <- get updatePending;
            when (not p)
                 (set updatePending True;
                  ec <- BackgroundRpc.getRpcErrorsCount;
                  Js.setTimeout "queueUpdate_"
                                (set updatePending False;
                                 updateSubscriptionsIgnoringErrors True)
                                (min 60000 (3000 + ec*3000)))
        fun queueCustomUpdate [a] (f : time -> string -> list Datatypes.bgAction -> transaction a) (r: a -> transaction {}) : transaction {} =
            modify customUpdatesRunning succ;
            BackgroundRpc.queueRpcB
              (fn l =>
                  t <- get lastRenTime;
                  fssh <- get filtersAndSmartStreamsHash;
                  clearRetryPendingUpdates;
                  f t fssh l)
              (fn x =>
                  modify customUpdatesRunning pred;
                  r x)
        (* вызовет возвращенное действие только если не менялся фид *)
        fun queueCustomUpdateSameFeed [a] (f : time -> string -> list Datatypes.bgAction -> transaction a) (r: a -> transaction (transaction {})) : transaction {} =
            sn <- get setFeedSeqNum;
            queueCustomUpdate
              f
              (fn x =>
                  a <- r x;
                  sn' <- get setFeedSeqNum;
                  when (sn = sn') a)
        fun updateMarkReqReadCounters vm mr mids =
            queueCancellableCustomUpdate'
              markReqReadCountersSeqNum
              queueUpdate customUpdatesRunning
              (fn l =>
                  clearRetryPendingUpdates;
                  rpc (Rpcs.markReqReadCounters vm mr mids l))
              (fn rc =>
                  r <- get needRetryPendingUpdates;
                  if r then
                      queueUpdate
                  else
                      updateReadCounters rc)
        fun updateSubscriptionsThen (a : transaction {}) : transaction {} =
            cus <- get customUpdatesRunning;
            if cus > 0 then queueUpdate else
            queueCustomUpdate
                (fn t fssh l =>
                    h <- get titleHash;
                    siv <- get subItemsVersion;
                    a <- Js.isWindowActive;
                    rpc (Rpcs.subscriptions_ (not a) t h siv fssh l))
                (fn (r, pt) =>
                    updatePaidTill pt;
                    updateSubscriptions' r;
                    a)
        fun addSub u =
            when (u <> "")
            (queueCustomUpdateSameFeed
                 (fn t fssh l =>
                     rpc (Rpcs.addSubscription t fssh u l))
                 (fn (h, r) =>
                     updateSubscriptions' r;
                     return
                       (let val (_,_,_,_,rs) = r
                            val h' = tryRename h rs
                            val pu = "subscription/" ^ Js.encodeURIComponent u
                        in
                            p <- Js.getInterfacePath;
                            when (p = pu)
                                 (Js.replaceInterfacePath h';
                                  reload)
                        end
                 )))
        fun addDiscoverySub u country query =
            when (u <> "")
            (queueCustomUpdateSameFeed
                 (fn t fssh l =>
                     rpc (Rpcs.addDiscoverySubscription t fssh u country query l))
                 (fn (h, r) =>
                     updateSubscriptions' r;
                     return
                       (tryPushInterfacePath h;
                        reload)))
        fun editFolders (si : subItem) folder add =
            case subItemUrl si of
              | Some u =>
                queueCustomUpdate
                    (fn t fssh l =>
                        rpc (Rpcs.editSubscriptionFolders t fssh u folder add l))
                    (fn r =>
                        updateSubscriptions' r;
                        cf <- getCurrentFeed;
                        case cf.SIType of
                          | SITFolder f =>
                            when (f.Folder = folder) reload
                          | _ => return ())
              | _ =>
                return ()
        fun rename (si : subItem) newTitle =
            case subItemUrl si of
              | Some u =>
                queueCustomUpdate
                    (fn t fssh l =>
                        rpc (Rpcs.renameSubscription t fssh u newTitle l))
                    (fn r =>
                        updateSubscriptions' r;
                        cf <- getCurrentFeed;
                        case cf.SIType of
                          | SITTag _ => reload
                          | SITAllTags => reload
                          | SITStarred => reload
                            (* пока нет счетчиков тегов считаем, что у фида
                               есть помеченные элементы
                             *)
                          | _ =>
                            urls <- getUrlsOnly cf;
                            when (elem u urls)
                                 reload)
              | _ =>
                return ()
        fun renameF (si : subItem) (folder : string) (to : string) =
            queueCustomUpdate
                (fn t fssh l =>
                    rpc (Rpcs.renameFolder t fssh folder to l))
                (fn (p, r) =>
                    p0 <- Js.getInterfacePath;
                    when (si.Path = p0)
                         (* если переименовывали текущий выделенный объект *)
                         (Js.replaceInterfacePath p);
                    updateSubscriptions' r;
                    when (si.Path = p0
                          || p = p0
                             (* переименовали что-то в текущий фид *)
                          || (case si.SIType of
                               | SITTag _ => True
                                 (* всегда перегружаем, т.к. в любом фиде
                                    может быть помеченный этим тегом объект
                                  *)
                               | _ => False))
                          reload
                )
        fun retryScanning (si : subItem) =
            Js.retryScanning si.Index;
            reload;
            retryPendingUpdates; (* уже ручками обновили *)
            (* TODO: по идее, нельзя обновлять пока retry не прошел,
               а не просто пропускать следующий update
             *)
            case subItemUrl si of
              | Some u =>
                BackgroundRpc.queueRpcB
                    (fn l => rpc (Rpcs.retrySubscription u l))
                    (fn _ => return ());
                queueUpdate
              | None => return ()
        fun unsubscribe (sis : list subItem) after =
            Js.forceImpure (hideSubItems sis);
            queueCustomUpdate
                (fn t fssh l =>
                    rpc (Rpcs.removeSubscriptions t fssh
                         (List.mapPartial subItemUrl sis) l))
                (fn r =>
                    List.app clearMtvm (List.mapPartial subItemUrl sis);
                    updateSubscriptions' r;
                    after)
        fun onDragAndDrop dd =
            BackgroundRpc.addAction (BGDragAndDrop dd);
            set titleHash "";
            updateSubscriptionsThen (
            cf <- getCurrentFeed;
            groupByFeed <- Monad.mp mtvmGroupByFeed getMsgTreeViewMode;
            case cf.SIType of
              | SITFolder f =>
                let val sf = Some f.Folder in
                    when ((sf = dd.SourceFolder || sf = dd.TargetFolder)
                          &&
                          (dd.SourceFolder <> dd.TargetFolder || groupByFeed))
                        reload
                end
              | SITAll =>
                when groupByFeed reload
              | _ => return ())
        fun sort act q =
            askBefore q
              (BackgroundRpc.addAction act;
               updateSubscriptionsThen (return ()))
        fun publicFeedLiI typ =
            P.lii Css.iconEmpty "Public feed"
                   (publicFeedDialog typ publicFeeds)
        fun filterAction f callback =
            queueCustomUpdate
                (fn t fssh l =>
                    h <- get titleHash;
                    siv <- get subItemsVersion;
                    f t h siv fssh l)
                (fn r =>
                    case r of
                      | Left e => callback (Some e)
                      | Right s =>
                        updateSubscriptions' s;
                        callback None)
        fun deleteSS name =
            filterAction (fn t h siv fssh l => rpc (Rpcs.deleteSmartStream name t h siv fssh l))
   in
    Js.setOnSetFeed (fn i => setFeed (getSubItem i));
    Js.setOnToggleFolder (fn i =>
                             retryPendingUpdates;
                             (* чтобы папку обратно не развернуло *)
                             toggleFolder (getSubItem i));
    subMenuContents <- return <xml>
      <dyn signal={
        ous <- signal onlyUpdatedSubscriptions;
        euc <- signal exactUnreadCounts;
        let fun checkButtons c =
                if c then (Css.iconEmpty,Css.iconCheck)
                else (Css.iconCheck,Css.iconEmpty)
            val (al,u) = checkButtons ous
            val (c500,exact) = checkButtons euc
            fun hideRead v =
                set onlyUpdatedSubscriptions v;
                BackgroundRpc.addAction (BGSetOnlyUpdatedSubscriptions
                                             { Value = v })
            fun setExact e =
                Js.setExactUnreadCounts e;
                set exactUnreadCounts e;
                BackgroundRpc.addAction (BGSetExactUnreadCounts { Value = e })
        in
        return <xml>
          {P.lii al "Show all" (hideRead False)}
          {P.lii u  "Show updated" (hideRead True)}
          <hr/>
          {P.lii c500  "500+ unread counts" (setExact False)}
          {P.lii exact "Exact unread counts" (setExact True)}
          <hr/>
          {P.lii Css.iconEmpty "Sort feeds and folders"
                  (sort BGSortAllFeedsAndFolders
                        "Are you sure you want to reset subscriptions ordering? All your drag and drop customizations will be cleared. This operation can’t be undone.")
          }
          {P.lii Css.iconEmpty "Sort top level only"
                  (sort (BGSortFolder { Folder = "" })
                        "Are you sure you want to reset top level ordering (ordering inside folders will not be changed)? Your drag and drop customizations will be cleared. This operation can’t be undone.")
          }
          <hr/>
          {publicFeedLiI PFTAll}
          <a link={Pages.opml} onclick={fn _ =>
                                           P.hide; redirect (bless "/opml")}>
            <li>{buttonSymbol Css.iconEmpty}Export OPML</li></a>
          <a link={Pages.clearSubscriptions}>
            <li>{buttonSymbol Css.iconEmpty}Clear subscriptions</li></a>
        </xml>
        end} />
    </xml>;
    subMenu <- P.newMenu null "" subMenuContents;
    newFolder <- source "";
    dummyNewFolderTid <- fresh;
    let fun foldersMenuContents cf =
        case cf.SIType of
          | SITFeed { Subscription = { Folders = sfs, ... }, ... } =>
            dyn_ (fs <- signal folders;
            let val new =
                    stopPropagation;  (* а то list view раскрывает *)
                    nf <- get newFolder;
                    c <- Js.checkName "folder" nf;
                    withSome (fn nf =>
                                 P.hide;
                                 set newFolder "";
                                 editFolders cf nf True) c
            in
            return <xml>
              <div class={Css.newFolder}>
                <div class="newFolderLabel">Folders</div>
                {oneLineInputAndOkButton dummyNewFolderTid newFolder "New folder" "Add" new}
              </div>
              {List.mapX
               (fn f =>
                    if elem f sfs then
                        P.lii Css.iconCheck f (editFolders cf f False)
                    else
                        P.lii Css.iconEmpty f (editFolders cf f True))
               fs}
            </xml>
            end)
          | _ => <xml/>
        fun clrTagsMenuContents context what tags =
            articlesOlderThanMenu (return False)
              (if context then None else Some (hasAbove, hasBelow))
              (fn t d => clearTags tags t d reload)
              what <xml/>
        fun clrTag context what tag = <xml>
              {P.menuLabel what}
              {clrTagsMenuContents context what tag}
            </xml>
        fun subItemMenuContents context si = case si.SIType of
              | SITAll => Some (publicFeedLiI PFTAll)
              | SITStarred => Some <xml>
                {publicFeedLiI PFTStarred}
                {clrTag context "Unstar" (Some (ITStarred :: []))}
                </xml>
              | SITAllTags => Some <xml>
                {displayIfC context
                 (P.lii Css.iconEmpty "Sort tags"
                  (sort BGSortTags
                        "Are you sure you want to reset tags ordering? All your drag and drop customizations will be cleared. This operation can’t be undone."))}
                {publicFeedLiI PFTAllTags}
                {clrTag context "Untag" None}
                </xml>
              | SITTag t => Some <xml>
                {P.lii Css.iconEmpty "Rename tag"
                        (renameDialog "tag" t.TagName (renameF si t.TagName)) : xbody}
                {publicFeedLiI (PFTTag t)}
                {clrTag context "Untag" (Some (ITTag t :: []))}
                </xml>
              | SITSmartStream { StreamName = s, ... } =>
                Some <xml>
                {P.lii Css.iconEmpty "Edit stream"
                       (editStreamDialog s) : xbody}
                {P.lii Css.iconEmpty "Rename stream"
                       (renameDialog "smart stream" s (renameF si s)) : xbody}
                {publicFeedLiI (PFTSmartStream { StreamName = s })}
                {unsubscribeLiI (fn _ _ => deleteSS s (const <| return ()))
                                setFeed "Delete stream"
                             (Js.forceImpure (hideSubItems (si :: [])))
                             (si :: []) : xbody}
                </xml>
              | SITSearch _ => None
              | SITFolder { Folder = f } =>
                if f = "" then None else Some <xml>
                {displayIfC context
                 (P.lii Css.iconEmpty "Sort folder"
                  (sort (BGSortFolder { Folder = f })
                        "Are you sure you want to reset folder ordering? Drag and drop customizations will be cleared. This operation can’t be undone."))
                }
                {P.lii Css.iconEmpty "Rename folder"
                       (renameDialog "folder" f (renameF si f)) : xbody}
                {publicFeedLiI (PFTFolder { Folder = f })}
                {unsubscribeLiI unsubscribe setFeed "Delete folder"
                             (Js.forceImpure (hideSubItems (si :: [])))
                             (getSubItems si.Index) : xbody}
                </xml>
              | SITFeed f =>
                Some
                (if si.Index = discoverySubItemIndex then <xml>
                  {P.lii Css.iconEmpty "Subscribe"
                      (subscribeDiscoveryFeed f.Subscription.Url)}
                  {P.lii Css.iconEmpty "Feed address"
                      (feedAddressDialog f)}
                </xml> else <xml>
                {P.lii Css.iconEmpty "Rename"
                       (renameDialog "" si.Title (rename si)) : xbody}
                (* TODO: rename c клавы, так всегда можно посмореть
                   полное название текущего фида
                 *)
                {P.lii Css.iconEmpty "Feed address" (feedAddressDialog f)}
                {unsubscribeLiI unsubscribe setFeed
                                "Unsubscribe" (return ()) (si :: []) : xbody}
                {foldersMenuContents si}
                </xml>)
        fun onSubscriptionRightClick id =
            case (getSubItem id).SIType of
              | SITAll => P.showContextMenu None subMenuContents
              | _ =>
                withSome (P.showContextMenu None)
                  (subItemMenuContents True (getSubItem id))
    in
        Js.setOnSubscriptionRightClick onSubscriptionRightClick;
        registerOnDragAndDrop onDragAndDrop;
        registerMTVMFolderExpanded MTVMFolderExpanded;
        set queueUpdateSrc queueUpdate_;
        Js.registerUpdateSubscriptions queueUpdate_;
        return
            { Html = <xml><dyn signal={signal html} /></xml>
            , PublicFeeds = publicFeeds
            , AddSubscription = addSub
            , AddDiscoverySubscription = addDiscoverySub
            , UpdateSubscriptionsIgnoringErrors =
              updateSubscriptionsIgnoringErrors False
            , UpdateSubscriptionsThen = updateSubscriptionsThen
            , UpdateSubscriptions_ = updateSubscriptions'
            , CustomUpdatesRunning = customUpdatesRunning
            , UpdateMarkReqReadCounters  = updateMarkReqReadCounters
            , MarkReqReadCountersSeqNum = markReqReadCountersSeqNum
            , QueueUpdate = queueUpdate
            , OnError = set customUpdatesRunning 0
            , Rename = rename
            , Unsubscribe = unsubscribe
            , RetryScanning = retryScanning
            , SubMenu = subMenu
            , SubItemMenuContents = subItemMenuContents
            , FiltersAndSmartStreams = filtersAndSmartStreams
            , DeleteFilter =
                fn fid =>
                  filterAction (fn t h siv fssh l =>
                    rpc (Rpcs.deleteFilter fid t h siv fssh l))
            , DeleteSmartStream = deleteSS
            , AddSmartStream =
                fn name query feeds =>
                  filterAction (fn t h siv fssh l =>
                    rpc (Rpcs.addSmartStream name query feeds t h siv fssh l))
            , EditSmartStream =
                fn name query feeds =>
                  filterAction (fn t h siv fssh l =>
                    rpc (Rpcs.editSmartStream name query feeds t h siv fssh l))
            , AddFilter =
                fn query negate feeds =>
                  filterAction (fn t h siv fssh l =>
                    rpc (Rpcs.addFilter query negate feeds t h siv fssh l))
            , EditFilter =
                fn fid query negate feeds =>
                  filterAction (fn t h siv fssh l =>
                    rpc (Rpcs.editFilter fid query negate feeds t h siv fssh l))
            }
    end end
