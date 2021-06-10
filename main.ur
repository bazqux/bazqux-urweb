open Datatypes
open Css
open Utils
open Payments
open Import
open SubItem
open Share
open Uim
open Feeds
open Discovery
open Articles
open Filters
open Appearance
open Account

structure P = Popups
val _ = Settings.imagesWidthEq
val _ = Settings.feedAlignEq

val showLeftPanel = Unsafe.boolSource "Main.showLeftPanel" False
val toggleLeftPanel =
    s <- get showLeftPanel;
    Js.blurActiveElement;
    Js.enableMenuTransitions;
    P.toggleExternalPopup showLeftPanel

val userExperimentEq : eq userExperiment =
    mkEq (fn a b => case (a,b) of
      | (UENo9, UENo9) => True
         )

val tupleEq : eq (string * string) =
    mkEq (fn (a1,a2) (b1,b2) => a1=b1 && a2=b2)

val eqPaidTill : eq paidTill =
    mkEq (fn a b => case (a,b) of
      | (PTUnknown, PTUnknown) => True
      | (PTFreeTrial a, PTFreeTrial b) => a.Till = b.Till
      | (PTFreeTrialFinished a, PTFreeTrialFinished b) => a.Till = b.Till
      | (PTPaid a, PTPaid b) => a.Till = b.Till
      | (PTPaidFinished a, PTPaidFinished b) => a.Till = b.Till
      | _ => False)

val feedPasswordWarning =
    "WARNING: username and password are kept in pain text in feed URL in standard HTTP basic access authentication format:
https://USERNAME:PASSWORD@example.com/feed

Please, if possible, use unique password! Username and password will be visible in OPML, in web server logs (some apps make requests with feed address) and, unlike usual cases when only password hash is kept, this password needs to be kept as is, so there are many unpredictable ways it could leak.

Only HTTP basic authentication is supported. If after entering right username and password feed still returns “HTTP 401 Unauthorized” then it could be misconfigured or require another authentication method."

val defaultWelcomeState : welcomeState =
    { HasPrevAccount = False
    , HasPrevSubs = False
    , StarredRestored = False
    , TaggedRestored = False }

fun welcomeText ws paidTill curTime opmlUploadClick restoreSubscriptions : (string * xbody) =
    let val importOpml =
            dyn_ (return (Js.opmlForm
                              (linkButton "import an OPML file"
                                          (stopPropagation; opmlUploadClick))))
        val addSub = <xml>
          {buttonName "Add subscription"} button to add new feeds.
        </xml>
        val clickOn = <xml>the
          <span class="displayIfLeftPanelStatic">{addSub}</span>
          <span class="displayIfLeftPanelMovable">{buttonSymbol Css.iconHamburger} hamburger button <span class="displayIfTouch">(or swipe right)</span>
            and then {addSub}</span>
        </xml>
        val title = "Welcome" ^ (if ws.HasPrevAccount then " back" else "") ^ "!"
    in
        ("", <xml>
          <div class={Css.welcomeText}>
            <h2>{[title]}</h2>
(*             <p>To start using BazQux Reader please</p> *)
(*             <p>{linkButton "import your subscriptions" greaderImportClick} from Google Reader</p> *)
            {displayIfC ws.HasPrevAccount
              (dyn_ (
               pt <- signal paidTill;
               ct <- signal curTime;
               return (case pt of
                | PTFreeTrial { Till = t } =>
                  (* сообщаем о предыдущем аккаунте только в первый день
                     free trial-а (не пишем после оплаты о начале нового free
                     trial)
                   *)
                  displayIfC (diffInSeconds ct t / 86400 + 1 > 29) <xml>
                    <p>Your previous account has expired more than a month ago. New free trial has just started!</p>
                    {displayIfC (ws.StarredRestored || ws.TaggedRestored) <xml>
                      <p>We have restored your
                        {[case (ws.StarredRestored, ws.TaggedRestored) of
                            | (True, True) => "starred and tagged"
                            | (True, False) => "starred"
                            | (False, True) => "tagged"
                            | _ => ""
                        ]} items.</p></xml>}
                  </xml>
                | _ => <xml/>)))}
            {if ws.HasPrevSubs then <xml>
               <p>You can {linkButton "restore your previous subscriptions" restoreSubscriptions} in one click!</p>
               <p>Alternatively, you can click on {clickOn}</p></xml>
             else <xml>
               <p>Click on {clickOn}</p></xml>
            }
            <p>Search for sites you love to read and add them to BazQux Reader or {importOpml}.</p>
            <p>You could read more about {hrefLink' (txt "how to import your feeds") (show (url Pages.how_to_import_my_feeds))} from other feed readers.</p>
(*             <p>After adding your feeds don’t forget to play with view modes (they can be set per feed), set username and password in Account setting and try some of the apps, search (and maybe add some filters!), and enjoy fast.</p> *)
(*             <p>Hint: You can download your OPML from {hrefLinkStopPropagation (txt "Feedly") "http://cloud.feedly.com/#opml"}, {hrefLinkStopPropagation (txt "The Old Reader") "https://theoldreader.com/reader/subscriptions/export"} or {hrefLinkStopPropagation (txt "NewsBlur") "http://newsblur.com/import/opml_export"}.</p> *)
(*             <p>or *)
(*               <a href={bless "/importFromGoogleReader"} *)
(*                  onclick={fn _ => redirect (effectfulUrl importFromGoogleReader)}> *)
(*                  import your subscriptions from Google Reader</a> *)
(*             </p> *)
          </div></xml>)
    end

fun whatsNew lastWhatsNewTime = dyn_ (
     lto <- signal lastWhatsNewTime;
     return (case lto of None => <xml/> | Some lt =>
     let fun item year month day title link =
             let val time = Js.fromDatetimeUtc year (month-1) day 0 0 0
                 (* у JavaScript Date() месяцы начинаются с 0 *)
                 val act =
                     BackgroundRpc.addAction (BGWhatsNewClick { Time = time })
             in
                 (time, actHrefLink act (txt title) link)
             end
         val wnList =
             item 2020 7 27 "Mark above or below as read and article menu" "https://blog.bazqux.com/2020/07/mark-above-below-article-menu.html" ::
             item 2019 11 20 "Email registration and account management" "https://blog.bazqux.com/2019/11/email-registration-account-management.html" ::
             item 2019 6 18 "Themes, typography and image proxy" "https://blog.bazqux.com/2019/06/themes-typography-image-proxy.html" ::
             item 2018 7 23 "Mobile web interface" "https://blog.bazqux.com/2018/07/mobile-web-interface.html" ::
             []
         val wn = List.filter (fn (t,_) => t > lt) wnList
         val close = case wn of
               | (t,_) :: _ =>
                 BackgroundRpc.addAction (BGWhatsNewClose { Time = t });
                 set lastWhatsNewTime (Some t)
               | _ =>
                 return ()
     in
         if isNull wn then <xml/>
         else <xml>
           <div class={Css.whatsNew}>
             {dialog "New in reader:" close
              (List.mapX (fn (_,x) => <xml><p>{x}</p></xml>) wn)}
           </div>
         </xml>
     end))
val dragMarkId = Unsafe.id "Main.dragMarkId"
fun userUI paidTill_ uid : transaction page =
    hrUid_ <- Session.humanReadableUID uid;
    hrUid <- source hrUid_;
    searchBoxId <- fresh; (* чтобы всегда был одинаковый *)
    experiments <- source [];
    paidTill <- source paidTill_;
    payments <- source [];
    maxPaidSubscriptions <- source 0;
    maxFiltersOrSmartStreams <- source 0;
    curTime_ <- now;
    curTime <- source curTime_;
    beta <- Session.isBeta;
    local <- Session.isLocal;
    buyT0 <- Pages.buyText "" paidTill;
    buyT <- (* hyphenateXbody *) return buyT0;
    let val testing = beta || local
        fun mainPage () =
    currentFeedUrl <- source "";
    currentTagPath <- source "";
    dummyId <- fresh;
    loadingFeed <- source True;
    onlyUpdatedSubscriptions <- source False;
    exactUnreadCounts <- source False;
    setFeedStub <- source (fn _ => return ());
    onUpdateSubInfoStub <- source (fn _ _ => return ());
    associatedAccounts <- source [];
    associatedAccountNames <- source [];
    passwordSet <- source False;
    subscribeDiscoveryFeedStub <- source (fn _ => return ());
    clearMtvmStub <- source (fn _ => return ());
    reloadStub <- source (return ());
    filteringIsInProgress <- source None;
    addSubscriptionStub <- source (fn _ => return ());
    let fun subscribeDiscoveryFeed u =
            s <- get subscribeDiscoveryFeedStub;
            s u
        fun clearMtvm u =
            c <- get clearMtvmStub;
            c u
        val reload =
            r <- get reloadStub;
            r
        fun addSubscription u =
            a <- get addSubscriptionStub;
            a u
        fun selectSubscription typ =
            i <- fresh;
            d <- P.newBox ("Select " ^ typ)
              <xml><ctextbox id={i} class="selectSubscriptionInput" size={30} dir={Js.dirAuto} /></xml>;
            P.toggle d;
            Js.setupSubscriptionAutocomplete typ i;
            Js.select i;
            Js.focus i
(*         val greaderImportClick = *)
(*             redirect (effectfulUrl Import.importFromGoogleReader) *)
(*         val greaderImportStarredClick = *)
(*             redirect (effectfulUrl Import.importStarredAndTaggedItemsFromGoogleReader) *)
        val opmlUploadClick =
            Js.opmlUpload BackgroundRpc.flush
        fun setFeed si = sf <- get setFeedStub; sf si
        fun onUpdateSubInfo si si2 = o <- get onUpdateSubInfoStub; o si si2
        fun searchQueryAndPath path =
            if isPrefixOf "search/" path then
                case strindex (strsuffix path 7) #"/" of
                  | Some c =>
                    Some (Js.decodeURIComponent (substring path 7 c),
                          strsuffix path (7+c+1))
                  | None => None
            else
                None
        val updateCurrentFeed =
            c <- get (getSubItem 0).Counters;
            if c.Feed = 0 && c.Error = 0 && c.Scanning = 0 then
                return () (* ничего не делаем, если у нас пусто *)
            else
            path <- Js.getInterfacePath;
            case searchQueryAndPath path of
              | Some (_, p) =>
                si <- getSubItemByPath p;
                withSome (set currentSearchFeed) si
              | None =>
                si <- getSubItemByPath path;
                withSome (set currentFeed) si
        fun updatePaidTill (pt, ct) =
            pt0 <- get paidTill;
            when (pt <> pt0) (set paidTill pt);
            set curTime ct
    in
    updateSubscriptionsStub <- source (return ());
    updateMarkReqReadCountersStub <- source (fn _ _ _ => return ());
    msgsWidget <- msgsWidget
        (u <- get updateSubscriptionsStub; u)
        (fn vm mr mids => u <- get updateMarkReqReadCountersStub; u vm mr mids)
        loadingFeed setFeed;
    editStreamDialog <- source (fn _ => return ());
    ssWidget <- subscriptionsWidget
        updateCurrentFeed onUpdateSubInfo setFeed
        onlyUpdatedSubscriptions exactUnreadCounts
        subscribeDiscoveryFeed clearMtvm reload
        (fn n => e <- get editStreamDialog; e n) updatePaidTill
        msgsWidget.HasAbove msgsWidget.HasBelow msgsWidget.ClearTags
    ;
    set updateSubscriptionsStub ssWidget.UpdateSubscriptionsIgnoringErrors;
    set updateMarkReqReadCountersStub ssWidget.UpdateMarkReqReadCounters;
    set editStreamDialog (editSmartStreamDialog ssWidget (fn () => reload));
    toggleAccountBox <- accountBox associatedAccounts associatedAccountNames passwordSet paidTill payments maxPaidSubscriptions hrUid buyT ssWidget maxFiltersOrSmartStreams;
    infoMessage <- infoMessageAtTheTop;
    searchQuery <- source "";
    searchCounters <- source emptyCounters;
    searchResults <- source (None : option filterResults);
    path <- bind Js.getInterfacePath source;
    subscribeUrlId <- fresh;
    endDivId <- fresh;
    scrollTimeoutActive <- source False;
    fullscreen <- source False;
    lastCounters <- source emptyCounters;
    scannedPercent <- source <xml/>;
    reloadAvailable <- source False;
    msgTreeSpacerText <- source <xml/>;
    welcomeState <- source defaultWelcomeState;
    displayDiscovery <- source False;
    searchBarActive <- source False;
    appearanceDialog <- newAppearanceDialog;
    lastWhatsNewTime <- source None;
    ht <- (* hyphenateXbody *) return (Pages.helpText addSubscription);
    helpBox <- P.newBigBox "Help" ht;
    discovery <- discoveryWidget addSubscription opmlUploadClick displayDiscovery;
    let fun toggleDiscovery fromWelcome =
            lv <- Js.isLeftPanelVisible;
            dd <- (if not lv then return False else get displayDiscovery);
            set displayDiscovery (not dd);
            when (not dd && not lv) toggleLeftPanel;
            when (not dd && not (fromWelcome && Js.hasOnscreenKeyboard ()))
                 (* на iPad показывается сначала верх, потом низ, пока
                    клавиатура выплывает.
                    А через setTimeout не работает, т.к. focus() на iPad
                    работает только внутри user-initiated events
                  *)
                 (Js.select discoveryTextBoxId;
                  Js.focus discoveryTextBoxId)
        fun subInfoMsg t = <xml>
              <div class="subInfoScanning">
                <span class="spinner"></span> {[t]}
              </div>
            </xml>
        val snInfoMsgAdding = subInfoMsg "Adding new feed…"
        val snInfoMsgScanning = subInfoMsg "Fetching new feed…"
        fun scannedPercentXml text = <xml><div class={Css.scannedPercent}>
             <span class="spinner"></span>
             {dyn_ (ls <- signal lastCounters;
                   return <xml><span class="percent">{[ls.ScannedPercent]}%</span></xml>)}
             {[text]}
             {ifDynClass (Monad.mp not (signal reloadAvailable))
               Css.visibilityHidden (textButton "Refresh" reload)}
            </div></xml>
        fun scannedPercent100Xml text = <xml><div class={Css.scannedPercent}>
             {[text]} {textButton "Refresh" reload}
            </div></xml>
(*         val snImportTags = <xml><div class={Css.scannedPercent}> *)
(*              {textButton "Import starred and tagged items" *)
(*                          greaderImportStarredClick} *)
(*             </div></xml> *)
        val snScanningComments = scannedPercentXml "Fetching comments…"
        val snAllCommentsScanned = scannedPercent100Xml "All comments fetched."
        val snInfoMsg = <xml><dyn signal={
            si <- signal currentFeed;
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
(*                     <span class="spinner"></span> Fetching new feeds… *)
(*                     {textButton "Refresh" reload} *)
(*                   </div> *)
(*                 </xml> *)
(*                 else <xml/>) *)
                        }/></xml>
        fun modifyMsgTreeViewMode vmName (f : msgTreeViewMode -> msgTreeViewMode) =
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
                      BackgroundRpc.addAction (BGSetSubscriptionViewMode
                                                   { Url = feed.Subscription.Url, ViewMode = mtvm });
                      return True
                      end
                    | _ => return False
                fun updFolder' si name =
                    mtvm <- Monad.mp f (get si.ViewMode);
                    set si.ViewMode mtvm;
                    BackgroundRpc.addAction (BGSetFolderViewMode
                                                 { Folder = name, ViewMode = mtvm })
                fun updFolder si name = case vmName of
                    | None =>
                      (* выбор ascending/unread/group by feed *)
                      updFolder' si name;
                      return True
                    | Some vm => (* режим просмотра  *)
                      c <- confirm ("Do you really want to set " ^ vm ^
                                    " for all feeds" ^
                                    (if name <> "" then " in “" ^ name ^ "”"
                                     else "") ^
                                    "? \nAll per-feed settings will be cleared.");
                      (* Js.logTime "update" *) (if c then
                          updFolder' si name;
                          (* TODO: тут надо бы все папки обновлять,
                             если это корень *)
                          List.app (fn s => x <- updSub s; return ())
                                   (getSubItems si.Index);
                          return True
                      else
                          return False)
                fun upd si =
                    case si.SIType of
                        SITAll => updFolder si ""
                      | SITFolder f => updFolder si f.Folder
                      | SITFeed f => updSub si
                      | SITStarred => updFolder' si ",SITStarred"; return True
                      | SITAllTags => updFolder' si ",SITAllTags"; return True
                      | SITTag t => updFolder' si t.TagName; return True
                      | SITSmartStream s =>
                        updFolder' si s.StreamName; return True
                      | SITSearch _ =>
                        csf <- get currentSearchFeed;
                        upd csf
            in
                m <- upd cf;
                when m retryPendingUpdates;
                (* фоновое обновление может вернуть старый режим просмотра *)
                return m
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
            when m reload
        fun setAscending x =
            m <- modifyMsgTreeViewMode None (setF [#Ascending] x);
            when m reload
        val toggleGroupByFeed =
            m <- modifyMsgTreeViewMode None notMtvmGroupByFeed;
            when m reload
        fun isMixedViewSi si =
            case si.SIType of
              | SITStarred => return True
              | SITAllTags => return True
              | SITTag _ => return True
              | SITSmartStream _ => return True
              | SITSearch _ =>
                csf <- signal currentSearchFeed;
                isMixedViewSi csf
              | _ => return False
        val isMixedView =
            si <- get currentFeed;
            current (isMixedViewSi si)
        fun setViewMode name expanded posts f =
            mixed <- isMixedView;
            m <- modifyMsgTreeViewMode (Some name)
                (fn vm =>
                    setF [#NoOverride] (f (not mixed))
                    (setF2 [#ExpandedComments] [#Posts] expanded posts vm));
            when m reload
        val withCommentsVM =
            ( iconFullViewWithComments
            , fn vm => vm.ExpandedComments
            , setViewMode "expanded view with expanded comments" True PVMFull id
            , "Expanded view with expanded comments")
        val fullVM =
            ( iconFullView
            , fn vm => not vm.ExpandedComments && vm.Posts = PVMFull
            , setViewMode "expanded view" False PVMFull id
            , "Expanded view")
        val shortVM =
            ( iconListView
            , fn vm => not vm.ExpandedComments && vm.Posts = PVMShort
            , setViewMode "list view" False PVMShort id
            , "List view")
        val magazineVM =
            ( iconMagazineView
            , fn vm => not vm.ExpandedComments && vm.Posts = PVMMagazine
            , setViewMode "magazine view" False PVMMagazine id
            , "Magazine view")
        val mosaicVM =
            ( iconMosaicView
            , fn vm => not vm.ExpandedComments && vm.Posts = PVMMosaic
            , setViewMode "mosaic view" False PVMMosaic id
            , "Mosaic view" )
        val mixedVM =
            ( iconMixedView
            , fn vm => vm.NoOverride
            , setViewMode "mixed view" False PVMFull (fn _ => True)
            , "Mixed view (using each feed view modes)" )
        val subscriptionTitle =
            dyn_ (f <- signal currentFeed;
                  return <xml><span dir="auto">{[f.Title]}</span></xml>)
        fun setForest f mr =
            msgsWidget.SetForest f mr
        fun setDocumentTitle si =
            Js.setDocumentTitle ("bq | " ^ subItemTitle si)
            (* иногда оставляет в заголовке окна только концовку,
               и непонятно, что это вообще за окно
             *)
        fun updatePath si =
            let val h' = subItemPath si in
            set path h';
            tryPushInterfacePath h'
            (* не трогаем историю при back, чтобы можно было сделать forward *)
            end
        fun setCurrentFeed si =
            updatePath si;
            Js.setScrollTop Settings.msgDivId 0.0;
            (* прокручиваем в начало до SetForest,
               почему-то, если делать это после, firefox не сбрасывает scroll
               (или пытается восстановить его?)
             *)
            setForest emptyMF emptyMarkReq;
            Js.discoveryClearSelection;
            set currentFeed si;
            set currentSearchFeed defaultSubItem;
            setDocumentTitle si;
            set searchResults None;
            set scannedPercent <xml/>;
            set msgTreeSpacerText <xml/>;
            set currentFeedUrl "";
            set currentTagPath "";
            set reloadAvailable False;
            l <- get showLeftPanel;
            when l toggleLeftPanel
        fun mkDiscoveryFeed url title feedLink faviconStyle mbmtvm =
            c <- source emptyCounters;
            mtvm <- (case mbmtvm of
              | Some m => return m
              | None => discovery.LookupMtvm url);
            m <- source mtvm;
            return
                ({ Path          = "subscription/" ^ Js.encodeURIComponent url
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
        fun setFeedAdding si =
            setCurrentFeed (setF [#Index] (discoverySubItemIndex - 1) si);
            set msgTreeSpacerText snInfoMsgAdding
        fun setFeedUrlAdding url =
            si <- mkDiscoveryFeed url (Js.titleFromUrl url) None None None;
            setFeedAdding si
        val loadingIndicator =
            dyn_ (a <- msgsWidget.LoadingAppendRequests;
                  l <- msgsWidget.LoadingComments;
                  (* loadingComments -- уже есть индикатор при expand *)
                  return (if a && not l then
                              <xml><div class={Css.loading}>
                                <span class="spinner"></span> Loading…
                              </div></xml>
                          else <xml/>))
         fun subInfoErrorText m =
             <xml><div class={Css.subInfoErrorText}>{[m]}</div></xml>
         fun feedAuthInput si url e parents =
             if strsindex e "HTTP 401 " = Some 0 then
                 activeXml
                 let val (username, password) = Js.getUrlUsernameAndPassword url
                 in
                     uSrc <- source username;
                     pSrc <- source password;
                 let val hasPwd = username <> "" || password <> ""
                     val rUrl = case parents of
                         | (SpuRedirect r) :: _ => Some r.Url
                         | (SpuHtml h) :: _ => Some h.Url
                         | _ => None
                     val subscribe =
                         u <- get uSrc;
                         p <- get pSrc;
                         if u = "" && p = "" then
                             alert "Please, enter username and password."
                         else
                         let val url' = Js.setUrlUsernameAndPassword
                                          (u,p) (Option.get url rUrl)
                         in
                             if url' <> url then
                                 setFeedUrlAdding url';
                                 ssWidget.Unsubscribe
                                     (si :: [])
                                     (ssWidget.AddSubscription url')
                             else
                                 ssWidget.RetryScanning si
                         end
                     val keydown = onEnter subscribe
                     val msg =
                         case (hasPwd, rUrl) of
                           | (False, None) =>
                             "This feed is password protected.\n\nPlease, enter authentication information:"
                           | (True, None) =>
                             "Invalid username or password.\n\nPlease, enter authentication information:"
                           | (False, Some u) =>
                             "Feed was redirected to\n" ^ u ^ "\nand this feed is password protected.\n\nPlease, enter authentication information:"
                           | (True, Some u) =>
                             "Feed was redirected to\n" ^ u ^ "\nand this feed is password protected too. You could use the same username and password or enter new ones:"
                 in
                     return <xml>
                       <p>
                         {subInfoErrorText msg}
                       </p>
                       <p>Username<br/>
                         <ctextbox class="mlLogin" source={uSrc} size={20}
                                   onkeydown={keydown} dir={Js.dirAuto} />
                       </p>
                       <p>Password<br/>
                         <cpassword class="mlPassword" source={pSrc} size={20}
                                    onkeydown={keydown} />
                       </p>
                       <p>{textButton "Subscribe" subscribe}</p>

                       {subInfoErrorText feedPasswordWarning}
                     </xml>
                 end end
             else
                 <xml></xml>
         fun subErrorText si s =
             let val (m, p) = case s.State of
                 | SSError e => (e.Message, [])
                 | SSErrorPath e => (e.Message, e.Path)
                 | _ => ("", [])
             in
                 <xml>
                   {subInfoErrorText (Js.strReplace "<br/>" "\n" m)}
                   {feedAuthInput si s.Url m p}
                 </xml>
             end
         val restoreSubscriptions =
             P.hide;
             set loadingFeed True;
             showInfo "Restoring…" infoMessage
                      (x <- rpc (Rpcs.restoreSubscriptions []);
                       set loadingFeed False;
                       ssWidget.UpdateSubscriptions_ x;
                       discovery.Hide);
             reload
         fun setEmptyResult x =
             set msgTreeSpacerText
                <xml><div class="emptySetFeedResult">{x}
                </div></xml>
         fun feedsOrDiscovery discoveryUrl (si : subItem) =
             case discoveryUrl of
               | Some u => return (FODDiscovery { Url = u })
               | None =>
                 rcs <- getUrls si;
                 return (FODFeeds { ReadCounters = rcs })
         fun emptyFod fod = case fod of
               | FODFeeds { ReadCounters = [] } => True
               | _ => False
         fun fodFeeds fod = case fod of
               | FODFeeds { ReadCounters = rcs } => rcs
               | _ => []
         fun viewAllButton t =
             displayIfSig (mtvm <- msgTreeViewMode;
                           return mtvm.UnreadOnly)
                          (textButton t (setUnreadOnly False))
         val showLoading = infoMessage.Show "Loading…"
         val hideLoading = infoMessage.Hide
         fun setFolderForest uic name si fod vm =
             if emptyFod fod then
                 c <- get si.Counters;
                 case si.SIType of
                     | SITFolder { Folder = "" } => set msgTreeSpacerText <xml/>
                     | SITFeed f => set msgTreeSpacerText (* snInfoMsg *)
                       (if c.Scanning > 0 then
                            snInfoMsgScanning
                        else if c.Error > 0 then
                            <xml>
                              <div class="subInfoError"><h1>Error</h1>
                                {subErrorText si f.Subscription}
                              </div>
                              <div class="subInfoErrorButtons">
                                {textButton "Unsubscribe"
                                  (setFeed defaultSubItem;
                                   ssWidget.Unsubscribe (si :: []) (return ()))}
                                {textButton "Retry"
                                  (ssWidget.RetryScanning si)}
                              </div>
                            </xml>
                        else
                            <xml/>)
                     | SITSmartStream s =>
                       setEmptyResult <xml><h2>No feeds in smart stream
                         “{[s.StreamName]}”</h2></xml>
                     | _ =>
                       if c.Scanning > 0 then set msgTreeSpacerText <xml>
                         <div class="subInfoScanning">
                           <span class="spinner"></span>
                           Fetching new feeds…
                           {textButton "Refresh" reload}
                         </div>
                       </xml>
                       else if c.Error = 0 then
                           ws <- get welcomeState;
                           let val (title, text) = welcomeText ws
                                      paidTill curTime
                                      opmlUploadClick restoreSubscriptions
                           in
                               cf <- get currentFeed;
                               set currentFeed (setF [#Title] title cf);
                               set msgTreeSpacerText text
                           end
                       else
                           setEmptyResult <xml><h2>No feeds.</h2></xml>
             else
             showLoading;
             queueCancellableCustomUpdate uic ssWidget
             (fn l =>
                 set loadingFeed True;
                 case si.SIType of
                   | SITSmartStream s =>
                     rpc (Rpcs.smartStreamForest s.StreamName (fodFeeds fod) vm l)
                   | _ =>
                     rpc (Rpcs.folderForest name fod vm l))
             (fn (markReq,uc,(MsgForest f)) =>
                 hideLoading;
                 (case (fod,uc) of
                    | (FODDiscovery _,
                       (_, rp,rc,tp,tc) :: []) =>
                      (* обновляем счетчики discovery фида *)
                      c <- get si.Counters;
                      set si.Counters
                      (c -- #ReadPosts -- #ReadComments
                         -- #TotalPosts -- #TotalComments
                       ++ { ReadPosts = rp, ReadComments = rc
                          , TotalPosts = tp, TotalComments = tc })
                    | _ => return ());
                 updateReadCounters uc;
                 setForest (MsgForest f) markReq;
                 set loadingFeed False;
                 if not (isNull f.List) then
                     set msgTreeSpacerText loadingIndicator
                 else
                     c <- get si.Counters;
                     setEmptyResult <|
                         if c.TotalPosts = 0 && c.TotalComments = 0 then <xml>
                           <h2>“{subscriptionTitle}” is empty (feed exists but contains no articles).</h2>
                         </xml> else if vm.UnreadOnly then <xml>
                           <h2>“{subscriptionTitle}” has no unread articles.</h2>
                           (* без subscriptionTitle непонятно, что за фид в
                                Feed has no unread articles,
                              а если сделать
                                No unread articles,
                              то непонятно, почему это нет непрочитанных,
                              если в других фидах они есть
                            *)
                           {viewAllButton "View all articles"}
                         </xml> else <xml>
                           <h2>“{subscriptionTitle}” has all articles filtered out (go to {buttonName "Filters & streams"} settings to adjust filters).</h2>
                         </xml>
             )
         fun setFolder uic name si vm =
             fod <- feedsOrDiscovery None si;
             setFolderForest uic name si fod vm
        fun setTags uic si ts c =
            set currentTagPath si.Path;
            set msgTreeSpacerText <xml/>;
            vm <- getMsgTreeViewMode;
            showLoading;
            queueCancellableCustomUpdate uic ssWidget
                (fn l =>
                    set loadingFeed True;
                    rpc (Rpcs.tagsForest ts vm l))
                (fn (markReq,uc,mf) =>
                    hideLoading;
                    updateReadCounters uc;
                    setForest mf markReq;
                    set loadingFeed False;
                    emf <- current msgsWidget.IsForestEmpty;
                    c <- get si.Counters;
                    if emf then
                        setEmptyResult <| case ts of
                          | Some (ITStarred :: []) =>
                            if c.TotalPosts = 0 && c.TotalComments = 0 then
                                <xml><h2>There are no starred items yet.
                                  Please, star some articles first.</h2></xml>
                            else
                                <xml><h2>There are no unread starred items.</h2>
                                  {viewAllButton "Show all starred items"}
                                </xml>
                          | Some ((ITTag { TagName = n }) :: []) =>
                            if c.TotalPosts = 0 && c.TotalComments = 0 then
                                <xml><h2>There are no items tagged “{[n]}”.
                                  Perhaps you’ve untagged articles in another
                                  browser window or app (try reload page
                                  or press “r” to remove nonexistent tag).
                                </h2></xml>
                            else
                                <xml><h2>There are no unread items tagged “{[n]}”.</h2>
                                  {viewAllButton "Show all tagged items"}
                                </xml>
                          | _ =>
                            if c.TotalPosts = 0 && c.TotalComments = 0 then
                                <xml><h2>There are no tagged items.
                                  Please, tag some articles first.</h2></xml>
                            else
                                <xml><h2>There are no unread tagged items.</h2>
                                  {viewAllButton "Show all tagged items"}
                                </xml>
                    else
                        set msgTreeSpacerText loadingIndicator)
        fun setFeed_ hide uic si =
            when hide P.hide;
            set searchBarActive False;
            set searchQuery "";
            Js.selectSubItem si.Index;
            setCurrentFeed si;
            c <- get si.Counters;
            vm <- getMsgTreeViewMode;
            case (si.SIType, c) of
               | (SITFolder f, _) => setFolder uic (Some f.Folder) si vm
               | (SITAll, _) => setFolder uic None si vm
               | (SITSmartStream s, _) => setFolder uic None si vm
               | (SITStarred, _) => setTags uic si (Some (ITStarred :: [])) c
               | (SITAllTags, _) => setTags uic si None c
               | (SITTag t, _) =>
                 setTags uic si (Some (ITTag { TagName = t.TagName } :: [])) c
               | (SITFeed
                      { Subscription = { State = SSFeed f, ... }, ... },
                  { ScannedPercent = sp, ... }) =>
                      set currentFeedUrl f.Url;
                      when (sp <> 100)
                           (set lastCounters c;
                            set scannedPercent snScanningComments);
                      du <- current discoverySubItemUrl;
                      fod <- feedsOrDiscovery du si;
                      setFolderForest uic None si fod vm
               | (SITFeed _, _) =>
                 setFolderForest uic None si (FODFeeds { ReadCounters = [] }) vm
               | _ => return ()
        fun setDiscoveryFeed url title feedLink faviconStyle mbmtvm =
            df <- mkDiscoveryFeed url title feedLink faviconStyle mbmtvm;
            setFeed df
        fun addSubscription u =
            when (u <> "")
                 (P.hide;
                  setFeedUrlAdding u;
                  ssWidget.AddSubscription u)
        fun subscribeDiscoveryFeed url =
            c <- discovery.GetCountry;
            q <- discovery.GetQuery;
            discovery.Hide;
            si <- get currentFeed;
            setFeedAdding si;
            ssWidget.AddDiscoverySubscription url c q
        fun search' uic (q : string) (csi : subItem) =
            let val searchInAllArticlesIcon =
                    textButton "Search in all articles"
                           (m <- modifyMsgTreeViewMode None (setF [#UnreadOnly] False);
                            search' uic q csi)
                val si =
                    { Path          =
                       "search/" ^ Js.encodeURIComponent q ^ "/" ^ csi.Path
                    , Index         = -1
                    , Title         = q
                    , SIType        = SITSearch { Query = q }
                    , Counters      = searchCounters
                    , ViewMode      = csi.ViewMode
                    , ParentFolders = []
                    , DomIds        = []
                    , FaviconStyle  = None
                    }
                val folder = case csi.SIType of
                  | SITFolder { Folder = f } => Some f
                  | _ => None
            in
            when (q <> "")
            (Js.updateSearchAutocomplete q;
             set searchCounters emptyCounters;
             sq <- get searchQuery;
             when (sq <> q) (set searchQuery q);
             Js.selectSubItem csi.Index;
             setCurrentFeed si;
             set currentSearchFeed csi;
             vm <- getMsgTreeViewMode;
             (taggedFeed, tags) <- return (isTagFeed csi);
             du <- current discoverySubItemUrl;
             fod <- feedsOrDiscovery du csi;
             if emptyFod fod && not taggedFeed then
                 (if isNull (getSubItems csi.Index) then
                      setEmptyResult <xml>
                        <h2>No feeds to search. Please, add feeds using {buttonName "Add subscription"} panel first.</h2>
                      </xml>
                  else
                      setEmptyResult <xml>
                        <h2>You have no unread articles to search.</h2>
                        {searchInAllArticlesIcon}
                      </xml>);
                 BackgroundRpc.addAction (BGSaveFilterQuery { Query = q })
             else
             (showLoading;
              queueCancellableCustomUpdate uic ssWidget
              (fn l =>
                  set loadingFeed True;
                  if taggedFeed then
                      rpc (Rpcs.filterTagsForest q tags vm l)
                  else
                      case csi.SIType of
                       | SITSmartStream ss =>
                         rpc (Rpcs.filterSmartStreamForest ss.StreamName q (fodFeeds fod) vm l)
                       | _ =>
                         rpc (Rpcs.filterForest q folder fod vm l)
              )
              (fn (r : either string (markReq * Js.readCounters * filterResults)) =>
               hideLoading;
               case r of
                | Left e =>
                  set msgTreeSpacerText <xml>
                    <div class="subInfoError">
                      <h1>Syntax error (please, edit your search query):</h1>
                      {subInfoErrorText e}
                    </div>
                  </xml>
                | Right (markReq,uc,sr) =>
                  updateReadCounters uc;
                  let val (up,uc) = (sr.UnreadPosts, sr.UnreadComments)
                      val (tp,tc) = (sr.TotalPosts, sr.TotalComments)
                  in
                      set searchCounters
                          { ReadPosts = tp-up, TotalPosts = tp
                          , ReadComments = tc-uc, TotalComments = tc
                          , Scanning = 0, ScanningComments = 0
                          , Error = 0, Feed = 1, ScannedPercent = 100 };
                      set searchResults (Some sr);
                      setForest sr.MsgForest markReq;
                      set loadingFeed False;
                      if vm.UnreadOnly && up+uc = 0 && tp+tc > 0 then
                         setEmptyResult <xml>
                           <h2>Nothing found in unread articles.</h2>
                           {searchInAllArticlesIcon}
                         </xml>
                      else if tp+tc = 0 then
                         setEmptyResult <xml>
                           <h2>Nothing found, sorry.</h2>
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
            search' False q csi
        fun reloadFeed' hide updateIfCancelled () =
            path0 <- Js.getInterfacePath;
            if isPrefixOf "account" path0 then
                Js.replaceInterfacePath "";
                toggleAccountBox;
                reloadFeed' False False ()
            else
            let val (setF, cf, path) = case searchQueryAndPath path0 of
                  | Some (q, p) => (search' updateIfCancelled q, currentSearchFeed, p)
                  | _ => (setFeed_ hide updateIfCancelled, currentFeed, path0)
            in
                si <- getSubItemByPath path;
                cf <- get cf;
                (case si of
                   | Some si => setF si
                   | None =>
                     if isPrefixOf "subscription/" path then
                         (let val url = Js.decodeURIComponent (strsuffix path (strlen "subscription/"))
                              val (uname, pwd) = Js.getUrlUsernameAndPassword url
                          in
                           if pwd <> "" then
                               setDefaultFeed setFeed
                               (* фид с паролем, от которого отписались *)
                           else if cf.Path = path then
                               mtvm <- get cf.ViewMode;
                               df <- mkDiscoveryFeed
                                   url
                                   cf.Title
                                   (case cf.SIType of
                                      | SITFeed f => f.FeedLink
                                      | _ => None)
                                   cf.FaviconStyle
                                   (Some mtvm);
                               setF df
                           else
                               d <- tryRpc (Rpcs.feedDetails url);
                               (* не круто, что запрос фида пойдет после,
                                  ну и ладно
                                *)
                               df <- (case d of
                                  | Some (title, link, favicon, mtvm) =>
                                    mkDiscoveryFeed url title link
                                                     favicon (Some mtvm)
                                  | _ =>
                                    mkDiscoveryFeed url "" None None None);
                               setF df
                          end)
                     else if isPrefixOf "tag" path then
                         setDefaultFeed setFeed (* тег удалили *)
                     else
                         return ())
            end
        fun reloadFeed () = reloadFeed' True False ()
        val onPopstate =
            p0 <- get path;
            p <- Js.getInterfacePath;
(*             debug ("onPopstate p0 = " ^ p0 ^ "; p = " ^ p); *)
            when (p <> p0)
                 (preventDefault;
                  reloadFeed ())
        fun markAllRead t d =
            msgsWidget.MarkAllAsRead t d (reloadFeed' True True ())
        fun err caption details =
            ex <- get exiting;
            when (not ex)
                 (infoMessage.Error caption details;
                  BackgroundRpc.onError;
                  set filteringIsInProgress None;
                  msgsWidget.OnError;
                  ssWidget.OnError;
                  set scrollTimeoutActive False;
                  set loadingFeed False)
        val scroll' =
            fs <- Js.isFullScreen;
            (* при проигрывании html5 video игнорируем scrolling *)
            (getmw ()).UpdateFeedMark;
(*             st <- msgsScrollTop; *)
(*             debug ("onscroll, scrollTop = " ^ show st); *)
            a <- get scrollTimeoutActive;
            if fs || a then return () else
                 (set scrollTimeoutActive True;
                  Js.setTimeout "Main.onscroll"
                      (set scrollTimeoutActive False;
                       Js.requestAnimationFrameOnce "scroll'"
                       ((* debug "onscroll"; *)
                        msgsWidget.OnScroll;
                        msgsWidget.CheckAppend))
                      150)
        val scroll =
            Js.requestAnimationFrameOnce "scroll" scroll'
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
                               (set reloadAvailable True;
                                set lastCounters c2))
              | _ => return ())
        fun setListViewMode mode =
            set Settings.listViewMode mode;
            BackgroundRpc.addAction
                (BGSetListViewMode { ListViewMode = mode });
            reloadFeed ()
        fun displayOr d f a =
            if d then
                tryPushInterfacePath f;
                reloadFeed ()
            else
                a
        val focusInSearchBox =
            Js.select searchBoxId;
            Js.focus searchBoxId
        fun init filtersBox =
            showLoading;
            msgsWidget.JsInit;
            setRoundedVariables appearanceBoxClass "16" 1.5;
            Js.registerBackgroundRpcFlush BackgroundRpc.flush;
            Js.registerOnPopstate onPopstate;
            Js.registerOnFontsLoaded (getmw ()).RestoreSelectedUIMPosition;
            registerScrollToFragment scrollToFragment;
            registerUimScrollOffset uimScrollOffset;
            Js.set_showLeftPanel
                (fn x =>
                    x0 <- get showLeftPanel;
                    when (x <> x0) toggleLeftPanel);
            Js.set_popupsHide P.hide;
            Js.set_subscribeDiscoveryFeed subscribeDiscoveryFeed;
            set_setDiscoveryFeed setDiscoveryFeed;
            Js.set_discoveryHide discovery.Hide;
            set subscribeDiscoveryFeedStub subscribeDiscoveryFeed;
            set clearMtvmStub discovery.ClearMtvm;
            set setFeedStub (setFeed_ True False);
            set onUpdateSubInfoStub onUpdateSubInfo;
            set reloadStub (reloadFeed ());
            set addSubscriptionStub addSubscription;
            onConnectFail (err "Can’t connect to the server" "");
            (* ^ раньше вызывалось при ошибке в RPC, теперь не вызывается,
             *)
            onDisconnect (err "Disconnected from server" "");
            (* ^ если проблемы с каналом *)
            onServerError (fn e => err "Server error" e);
            onFail (fn e => err "Exception" e);
            (* ^ exception *)
            onError (fn e => err "Error" (Unsafe.fromXml e));
            (* ^ Division by zero *)
            P.setup;
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
                  Some (msgsWidget.JumpToLink mailLink)
              else if check (73 (* I *) :: 105 :: 1064 :: 1096 :: []) then
                  Some (if k.ShiftKey
                        then msgsWidget.SkipComments
                        else msgsWidget.SkipPost)
              else if check (76 (* L *) :: 108 :: 1044 :: 1076 :: []) then
                  Some (if k.ShiftKey
                        then msgsWidget.Later
                        else msgsWidget.LaterPost)
              else if check (87 (* W *) :: 119 :: 1062 :: 1094 :: []) then
                  Some (P.toggle filtersBox)
              else if check (81 (* Q *) :: 113 :: 1049 :: 1081 :: []) && k.ShiftKey then
                  Some (msgsWidget.MarkAboveOrBelowAsRead Above)
              else if check (90 (* Z *) :: 122 :: 1103 :: 1071 :: []) && k.ShiftKey then
                  Some (msgsWidget.MarkAboveOrBelowAsRead Below)
              else if check (88 (* X *) :: 120 :: 1063 :: 1095 :: []) then
                  Some (toggleFolderOr msgsWidget.IgnorePost)
              else if check (71 (* G *) :: 103 :: 1055 :: 1087 ::
                             93 (* ] *) :: 1066 :: 1098 ::
                             59 (* ; *) :: 1046 :: 1078 ::
                             []) then
                  Some msgsWidget.ToggleFullText
              else if check (77 (* M *) :: 109 :: 1068 :: 1100 :: []) then
                  Some (msgsWidget.ToggleRead True)
              else if check (79 (* O *) :: 111 :: 1065 :: 1097 :: []) then
                  Some msgsWidget.ToggleCollapsed
              else if check (49 (* 1 *) :: []) then
                  Some (P.hide; withCommentsVM.3)
              else if check (50 (* 2 *) :: []) then
                  Some (P.hide; fullVM.3)
              else if check (51 (* 3 *) :: []) then
                  Some (P.hide; magazineVM.3)
              else if check (52 (* 4 *) :: []) then
                  Some (P.hide; mosaicVM.3)
              else if check (53 (* 5 *) :: []) then
                  Some (P.hide; shortVM.3)
              else if check (48 (* 0 *) :: []) then
                  Some (P.hide; mixedVM.3)
              else if check (67 (* C *) :: 99 :: 1057 :: 1089 :: []) then
                  Some (msgsWidget.ClearStarred;
                        msgsWidget.JumpToLink (fn _ => Js.openLinkInBackgroud);
                        msgsWidget.Next)
              else if check (83 (* S *) :: 115 :: 1067 :: 1099 :: []) then
                  Some (displayOr d "starred" msgsWidget.ToggleStarred)
              else if check (84 (* T *) :: 116 :: 1045 :: 1077 :: []) then
                  Some (if d then selectSubscription "tag" else
                        if k.ShiftKey
                        then P.toggle appearanceDialog
                        else msgsWidget.EditTags)
              else if check (47 (* / *) :: 46 (* . *) :: []) then
                  Some (v <- Js.isVisible buttonOpenSearchBar;
                        when v (set searchBarActive True);
                        focusInSearchBox)
              else if check (70 (* F *) :: 102 :: 1040 :: 1072 :: []) then
                  Some (if d then selectSubscription "folder" else
                        (withLayoutChange (withNoTransitions (toggle fullscreen))))
              else if check (65 (* A *) :: 97 :: 1060 :: 1092 :: []) then
                  Some (displayOr d ""
                        (if k.ShiftKey
                         then markAllRead "" 0
                         else toggleDiscovery False))
              else if check (82 (* R *) :: 114 :: 1050 :: 1082 :: []) then
                  Some (showLoading;
                        ssWidget.UpdateSubscriptionsThen
                        (hideLoading;
                         (* reload *)reloadFeed' False False ()))
              else if check (68 (* D *) :: 100 :: 1042 :: 1074 :: []) then
                  Some (if d then selectSubscription "subscription" else
                        set dPressed True)
              else if check (63 (* ? *) :: 72 (* H *) :: 104 :: 1056 :: 1088 :: [])(*  || (k.ShiftKey && k.KeyCode = 191 (\* для firefox *\)) *) then
                  Some (P.toggle helpBox)
              else if check (45 (* - *) :: []) then
                  Some decArticleFontSize
              else if check (61 (* = *) :: []) then
                  Some incArticleFontSize
              else if check (95 (* _ *) :: []) then
                  Some decReaderFontSize
              else if check (43 (* + *) :: []) then
                  Some incReaderFontSize
              else
                  Some (when (k.CtrlKey || k.AltKey || k.MetaKey || k.ShiftKey)
                             handleLayoutChange))
              end) (fn k check =>
              (* специальные клавиши *)
              return (
              if check (27 :: []) then
                  Some (dd <- get displayDiscovery;
                        lv <- Js.isLeftPanelVisible;
                        lm <- Js.isLeftPanelMovable0;
                        sa <- P.isSubPopupActive;
                        a <- P.isActive;
                        sba <- get searchBarActive;
                        if lm && lv && dd then
                            (* видимый popup и есть левая панель *)
                            discovery.Hide
                        (* во всех других случаях закрываем сначала popup,
                           а потом уже discovery *)
                        else if sa then
                            P.hideSubPopup
                        else if a then
                            P.hide
                        else if lv && dd then
                            discovery.Hide
                        else if sba then
                            set searchBarActive False
                        else
                            msgsWidget.TryMakeShort)
              else if check (32 :: []) then
                  Some (if k.ShiftKey then msgsWidget.PrevOrPageUp else msgsWidget.NextOrPageDown)
              else if check (13 :: []) then
                  Some msgsWidget.ToggleFull
              else if check (33 (* PgUp *) :: []) then
                  Some msgsWidget.PageUp
              else if check (34 (* PgDown *) :: []) then
                  Some msgsWidget.PageDown
              else if check (36 (* Home *) :: []) then
                  Some msgsWidget.Home
              else if check (35 (* End *) :: []) then
                  Some msgsWidget.End
              else if check (38 (* Up *) :: []) then
                  Some msgsWidget.LineUp
              else if check (40 (* Down *) :: []) then
                  Some msgsWidget.LineDown
              else
                  None));

            BackgroundRpc.queueRpcB
                (fn l => rpc (Rpcs.subscriptionsAndSettings l))
                (fn ( upd
                    , (searches, ous, us)
                    , (ws, ps, maxSubs, maxFilters)) =>
                    set onlyUpdatedSubscriptions ous;
                    set welcomeState ws;
                    set payments ps;
                    set maxPaidSubscriptions maxSubs;
                    set maxFiltersOrSmartStreams maxFilters;
                    set Settings.scrollMode us.ScrollMode;
                    set Settings.listViewMode us.ListViewMode;
                    set Settings.ultraCompact us.UltraCompact;
                    set Settings.markReadMode us.MarkReadMode;
                    set experiments (Option.get [] us.Experiments);
                    set lastWhatsNewTime
                        (Option.mp (getF [#LastWhatsNewTime]) us.Ex);
                    let val co = Option.get "-" us.Country in
                        set discovery.Country
                            (if Js.countryNameFromCountryCode co =
                                "International" then "-" else co)
                        (* бывают всякие EU/A1, которые должны быть "-" *)
                    end;
                    set ssWidget.PublicFeeds (Option.get [] us.PublicFeeds);
                    Js.setExactUnreadCounts us.ExactUnreadCounts;
                    set exactUnreadCounts us.ExactUnreadCounts;
                    set associatedAccounts
                        (maybe [] (getF [#AssociatedAccounts]) us.Ex);
                    set associatedAccountNames
                        (maybe [] (getF [#AssociatedAccountNames]) us.Ex);
                    set passwordSet
                        (maybe None (getF [#PasswordHash]) us.Ex <> None);
                    Js.setupSearchAutocomplete searchBoxId searches search;
                    ssWidget.UpdateSubscriptions_ upd
                );
            set loadingFeed False;

            (* ^ может так не будет вылезать
                 "Unknown Ur expression kind undefined"? *)
            c <- get (getSubItem 0).Counters;
(*             if c.Feed = 0 && c.Error = 0 && c.Scanning = 0 then *)
(*                 pt <- get paidTill; *)
(*                 ws <- get welcomeState; *)
(*                 (case (ws, pt) of *)
(*                   | (_, PTFreeTrialFinished _) => return () *)
(*                   | (_, PTPaidFinished _) => return () *)
(*                   | (Some ws, _) => *)
(*                     b <- welcomeBox ws; *)
(*                     P.toggle' P.popupSource False b; *)
(*                     toggleDiscovery True *)
(*                   | _ => return () *)
(*                  ) *)
(*             else *)
            hideLoading;
            reloadFeed' False False ()
        val clearSubscriptions =
            askBefore
              "Are you really sure you want to unsubscribe from all your subscriptions?"
              (BackgroundRpc.addAction BGClearAllSubscriptions;
               BackgroundRpc.flush;
               Js.reloadPage)
        val openSearchBar =
            set searchBarActive True;
            focusInSearchBox
        fun showMarkAllAsReadMenu cls =
            let fun m t as =
                    case as of
                      | ASDirection (dir, _) =>
                        msgsWidget.MarkAboveOrBelowAsRead dir
                      | ASOlderThan d =>
                        markAllRead t d
                fun btn cls icon title act =
                    <xml><span class="cls">{P.lii icon title act}</span></xml>
            in
            P.toggle
              <| P.newIdPosMenu (Unsafe.id "markAllReadMenu")
                "" (Left (Js.offsetBottomLeft cls))
                Css.markAllAsReadMenu "Mark as read…"
              <| articlesOlderThanMenu
                isSearch
                (Some (msgsWidget.HasAbove, msgsWidget.HasBelow)) m
                "Mark"
                <xml><div class="toolbarButtonsInMenu">
                  <hr/>
                  {btn buttonOpenSearchBar iconSearch "Search" openSearchBar}
                  {btn buttonSkip iconSkip "Skip comments" msgsWidget.SkipPost}
                  {btn buttonIgnore iconIgnore "Ignore new comments" msgsWidget.IgnorePost}
                  {btn buttonSelectSubscription iconEmpty "Select subscription"
                    (selectSubscription "subscription")}
                </div></xml>
            end
        val viewModeButtons =
            si <- signal currentFeed;
            vm <- msgTreeViewMode;
            mixed <- isMixedViewSi si;
            let fun vmIcon (cls, active, setvm, name) sep hint =
                    buttonNoT'
                        (ifClass sep rightSeparator
                         (ifClass (if mixed then
                                       (if hint = "0" then
                                            vm.NoOverride
                                        else
                                            vm.NoOverride = False && active vm)
                                   else
                                       active vm)
                                  vmButtonActive
                                  vmButton))
                        cls
                        (name ^ ". \nKeyboard shortcut: " ^ hint)
                        (P.hide; setvm)
            in
                return <xml><span class="vmButtons">{
                     vmIcon withCommentsVM True "1"}{
                     vmIcon fullVM True "2"}{
                     vmIcon magazineVM True "3"}{
                     vmIcon mosaicVM True "4"}{
                     vmIcon shortVM mixed "5"}{
                     if mixed then vmIcon mixedVM False "0" else <xml/>
                    }</span></xml>
            end
        fun toggleFeedOptionsMenu cls =
            P.toggle (P.newIdPosMenu (Unsafe.id "feedOptionsMenu") ""
                        (Left (Js.offsetBottomLeft cls)) Css.feedOptionsMenu
                        "Feed options" <xml>
      <dyn signal=
           {vm <- msgTreeViewMode;
            si <- currentFeedS;
            let val canGroup =
                    case si.SIType of
                     | SITFeed _ => False
                     | _ => True
                fun checks x = if x then (Css.iconEmpty, Css.iconCheck) else (Css.iconCheck, Css.iconEmpty)
                val (sn,so) = checks vm.Ascending
                val (ng,gbf) = checks (mtvmGroupByFeed vm)
            in
                return <xml>
                  <span class="displayIfLeftPanelMovable">
                  {P.lii Css.iconEmpty "Appearance" (P.toggle appearanceDialog)}
                  </span>
                  {if not vm.UnreadOnly then
                       P.lii Css.iconEmpty "Show new" (setUnreadOnly True)
                   else
                       P.lii Css.iconEmpty "Show all" (setUnreadOnly False)}
                  <hr/>
                  {P.lii sn "Sort by newest" (setAscending False)}
                  {P.lii so "Sort by oldest" (setAscending True) : xbody}
                  {displayIfC canGroup <xml>
                    <hr/>
                    {P.lii ng  "No grouping" toggleGroupByFeed}
                    {P.lii gbf "Group by feed" toggleGroupByFeed}
                  </xml>}
                  <div class="vmButtonsInMenu">
                    {P.menuLabel "View mode"}
                    {P.indentedLine (dyn_ viewModeButtons)}
                  </div>
                </xml>
            end}  />
      <dyn signal=
           {si <- signal currentFeed;
            return (case ssWidget.SubItemMenuContents False si of
              | Some m => <xml><hr/>{m}</xml>
              | _ => <xml/>)
           } />
             </xml>)
        fun feedOptionsButton cls unreadOnly allArticles = <xml>
          <a onclick={fn _ => toggleFeedOptionsMenu cls} class={classes Css.button cls}
             title="Toggle feed options menu">
          {dyn_ (
             si <- signal currentFeed;
             cnt <- signal si.Counters;
             vm <- msgTreeViewMode;
             return (if vm.UnreadOnly then
                         unreadOnly cnt (isCommentCountsVisible si)
                     else
                         allArticles))}</a></xml>
        val buyNowText =
          pt <- signal paidTill;
          ct <- signal curTime;
          let fun text mode t =
                  let val ds = diffInSeconds ct t
                      val days = min (ds / 86400 + 1) 30
                  in
                      if ds < 0 then
                          mode ^ " finished. Buy now!"
                      else
                          show days ^ " day" ^
                          (if days = 1 then "" else "s") ^ " left. Buy now!"
                  end
          in
              return (case pt of
                | PTUnknown => None
                | PTFreeTrial { Till = t } => Some (text "free trial" t)
                | PTFreeTrialFinished { Till = t } => None
                | PTPaidFinished { Till = t } => None
                | PTPaid { Till = t } =>
                  if diffInSeconds ct t < 14*86400 then
                      Some (text "subscription" t)
                  else None)
          end
        val buyNowButton = <xml>
          <div dynClass={t <- buyNowText;
                         return (if Option.isSome t then Css.buyNow else Css.displayNone)} onclick={fn _ => toggleBuyBox buyT paidTill}>
            {dyn_ (Monad.mp (maybe <xml/> txt) buyNowText)}
          </div></xml>
        val searchButtonHint =
            "Search for articles in current feed and create filters or smart streams. \nKeyboard shortcut: /"
        val feedAlignClass =
            fa <- Settings.feedAlignClass;
            f <- signal fullscreen;
            return (if f then Css.feedAlignCenter else fa)
    in
    recommendBox <- P.newBigBox "Recommend" <xml>
      <p>Love the reader? Please spread the word:</p>
      <p>
        Write about BazQux Reader in your blog!<br/>
        Leave comments when you see discussion about feed readers.<br/>
        {hrefLinkStopPropagation <xml>Mail</xml> "mailto:?subject=BazQux%20Reader&body=Try%20this%20great%20RSS%20reader%20https%3A%2F%2Fbazqux.com"} a friend<br/>
        {hrefLinkStopPropagation <xml>Tweet</xml> "https://twitter.com/intent/tweet?text=I%20love%20this%20wonderful%20feed%20reader&url=https%3A%2F%2Fbazqux.com"} about the reader<br/>
        Share on {hrefLinkStopPropagation <xml>Facebook</xml> "https://www.facebook.com/sharer/sharer.php?u=https%3A%2F%2Fbazqux.com"}<br/>
        Like BazQux Reader on {hrefLinkStopPropagation <xml>AlternativeTo</xml> "http://alternativeto.net/software/bazqux-reader/"}<br/>
        Give it 5 stars on the {hrefLinkStopPropagation <xml>Chrome Web Store</xml> "https://chrome.google.com/webstore/detail/bazqux-reader-your-rss-fe/ojgfbobcblfebofgadefjfggnlclddko"}
      </p>
    </xml>;
    scrollingMenu <- P.newMenu null ""
      (dyn_ (sm <- signal Settings.scrollMode;
             let fun smItem mode name =
                     P.lii (if mode = sm then Css.iconCheck else Css.iconEmpty)
                            name
                            (set Settings.scrollMode mode;
                             BackgroundRpc.addAction
                                 (BGSetScrollMode { ScrollMode = mode }))
             in
                 return <xml>
                   {smItem SMNormal "Normal"}
                   {smItem SMQuick  "Quick"}
                   {smItem SMImmediate "Immediate"}
                 </xml>
             end));
    listViewMenu <- P.newMenu null ""
      (dyn_ (sm <- signal Settings.listViewMode;
             uc <- signal Settings.ultraCompact;
             let fun item mode name =
                     P.lii (if mode = sm then Css.iconCheck else Css.iconEmpty)
                            name
                            (setListViewMode mode)
                 fun itemc mode c name =
                     P.lii (if mode = sm && c = uc
                             then Css.iconCheck else Css.iconEmpty)
                            name
                            (set Settings.ultraCompact c;
                             BackgroundRpc.addAction
                                 (BGSetUltraCompact { UltraCompact = c });
                             setListViewMode mode)
             in
                 return <xml>
                   {itemc LVMCompact True "Ultra-compact"}
                   {itemc LVMCompact False "Compact"}
                   {item LVMTwoLines "Normal"}
                 </xml>
             end));
    markReadMenu <- P.newMenu null ""
      (dyn_ (sm <- signal Settings.markReadMode;
             let fun item mode name =
                     P.lii (if mode = sm then Css.iconCheck else Css.iconEmpty)
                            name
                            (set Settings.markReadMode mode;
                             BackgroundRpc.addAction
                                 (BGSetMarkReadMode { MarkReadMode = mode }))
             in
                 return <xml>
                   {item MRMOnScrollEverywhere "On scroll"}
                   {item MRMOnScroll "On scroll & click in list view"}
                   {item MRMManual "On click"}
                 </xml>
             end));
    filtersBox <- filtersBox ssWidget (setFeed_ False False) (reloadFeed' False False);
    settingsMenu <- P.newMenu Css.settingsMenu "Settings" <xml>
(*       {P.menuLabel "Settings"} *)
      {P.li "Help" (P.toggle helpBox)}
      {dyn_ (u <- signal hrUid;
             return (P.lli "Feedback"
             (bless
                  ("mailto:hello@bazqux.com?subject=[Feedback]&body="
                   ^ Js.encodeURIComponent ("\n-----\nUser ID:\n" ^ u)))))}
      {P.lli "Community" (bless "https://discourse.bazqux.com")}
      {P.lli "UserVoice" (bless "https://bazqux.uservoice.com")}
      {P.li "Recommend" (P.toggle recommendBox)}
      <hr/>
      {P.li "Account" toggleAccountBox}
      {P.li "Appearance" (P.toggle appearanceDialog)}
      {P.li "Filters & streams" (P.toggle filtersBox)}
      {P.liSub "Subscriptions" (P.popupXbody ssWidget.SubMenu)}
      {P.liSub "Transitions" (P.popupXbody scrollingMenu)}
      {P.liSub "List view" (P.popupXbody listViewMenu)}
      {P.liSub "Mark read" (P.popupXbody markReadMenu)}
      <hr/>
      {P.li "Log out" logOutAction : xbody}
    </xml>;
    settingsButton <- return
      (buttonNoT' Css.buttonSettings Css.iconSettings "Settings"
                  (P.toggle settingsMenu));
    showLeftPanelButton <- return
      (buttonNoT' Css.showLeftPanelButton Css.iconHamburger "Show feeds list"
                  toggleLeftPanel);
    vw <- bind Js.viewportWidth source;
    vh <- bind Js.viewportHeight source;
    return
        { Oninit = init filtersBox
        , Onunload = set exiting True; BackgroundRpc.flush
        , Onresize =
            msgsWidget.CheckAppend;
            Js.adjustMenuPosition;
            updateImageSizes;
            vw' <- Js.viewportWidth;
            vh' <- Js.viewportHeight;
            vwc <- get vw;
            vhc <- get vh;
(*             debug ("onresize " ^ show vw' ^  "x" ^ show vh' ^ " (was " ^ show vwc ^ "x" ^ show vhc ^ ")"); *)
            let val t = 900.0  in
            when (vwc <> vw' || (vh' > t && vhc <= t) || (vh' <= t && vhc > t))
                 (set vw vw';
                  set vh vh';
                  (* восстанавливаем позицию только если изменилась ширина,
                     а то при прокрутке на мобиле видны скачки,
                     когда появляется или скрывается адресная строка.
                     Также ловим переход 900px, на которых меняется высота
                     magazine/mosaic.
                     Мобильные устройства обычно либо меньше 900px,
                     либо ощутимо больше (1024px), будем надеяться, что
                     не будет скачков из-за отображения адресной строки.
                   *)
                  handleLayoutChange)
            end
        , Onscroll = scroll
        , Xml = <xml>
      <span dynClass={
              s <- Settings.readerFontSizeClass;
              tid <- signal Settings.themeId;
              f <- signal fullscreen;
              sl <- signal showLeftPanel;
              (font, _) <- fontAndLineHeight;
              return (
                  ifClass font.SupportsSuper Css.fontSupportsSuper (
                  ifClass font.SupportsSub Css.fontSupportsSub (
                  ifClass (tid <> "") Css.overrideColors (
                  ifClass sl Css.showLeftPanel
                  (classes (if f then Css.fullscreen else Css.noFullscreen)
                           (classes s Css.bodyScaler))))))
          }
      >
      {fontCSS}
      <div id={Settings.topId}
           dynClass={ifClassS searchBarActive Css.searchBarActive (return Css.top)}>
        <div class="topToolbar">
          {showLeftPanelButton}
          {divClass Css.oneLineInputAndButtonBorder <xml>
          <ctextbox source={searchQuery} (* placeholder="Search…" *)
              id={searchBoxId}
              class="oneLineInputAndButtonInput" dir={Js.dirAuto}
              onkeydown={fn k => if k.KeyCode = 13 then search else
                                 if k.KeyCode = 27 then stopPropagation; preventDefault (* без этого в Safari все равно вызывается обработчик по-умолчанию *); Js.blur searchBoxId
                                 else return()} /></xml>}
          {textButton'' Css.oneLineInputAndButtonButton "Search" searchButtonHint search}
          {buttonNoT' Css.buttonSearch Css.iconSearch "Search" search}
          {buttonNoT' Css.buttonCloseSearchBar Css.iconCloseSearchBar "Close search bar" (set searchBarActive False)}
          <div class="flexSpacer"></div>
          {buyNowButton}
          {settingsButton}
        </div> (* topToolbar *)
        <div class="msgToolbar">
          {showLeftPanelButton}
          {let fun inner t = <xml>
            {buttonText t}
            {buttonSymbol (classes Css.iconDown Css.iconDownComboBox)}
            </xml>
           in
           feedOptionsButton Css.buttonFeedOptions
             (fn cnt expandedComments =>
                 let val unread = showUnread cnt expandedComments
                 in
                     inner (if strlen unread > 4 then
                                unread
                             else
                                unread ^ " New")
                 end)
             (inner "All articles")
           end}
          {feedOptionsButton Css.buttonFeedOptionsSmall
             (fn cnt expandedComments =>
                 let val p = cnt.TotalPosts - cnt.ReadPosts
                     val c =
                         if expandedComments then
                             cnt.TotalComments - cnt.ReadComments
                         else
                             0
                 in
                     buttonText
                         (* пытаемся уместить счетчик в 3 символа и косую черту
                          *)
                         (if p > 50 then
                              "50+"
                          else if c = 0 then
                              show p
                          else if p = 0 then
                              if c >= 100 then
                                  "/99+"
                              else
                                  "/" ^ show c
                          else if (p >= 10 && c < 10) || (p < 10 && c < 100) then
                              (* в сумме укладываемся в три символа  *)
                              show p ^ "/" ^ show c
                          else
                              show p ^ "+")
                 end)
             (buttonText "All")}
          {dyn_ viewModeButtons}
          {buttonNoT' Css.buttonMarkAllAsReadSmall Css.iconCheck "Mark all as read menu" (showMarkAllAsReadMenu Css.buttonMarkAllAsReadSmall)}
          <div class="buttonMarkAllAsRead">
            {buttonT' Css.buttonLeft Css.iconCheck "Mark all as read" "Mark all messages and comments as read. \nKeyboard shortcut: Shift + A" (markAllRead "" 0)}
            {buttonT' Css.buttonRight Css.iconDown "Mark all as read menu" "" (showMarkAllAsReadMenu (classes Css.buttonMarkAllAsRead Css.buttonLeft))}
          </div>
          <div class="flexSpacer"></div>
          {buttonNoT'
              Css.buttonOpenSearchBar Css.iconSearch
              searchButtonHint
              openSearchBar}
          {buttonT' (classes Css.buttonSkip Css.buttonLeft)   Css.iconSkip "Skip" "Mark current article and its comments read and view next article. \nKeyboard shortcut: i" msgsWidget.SkipPost}
          {buttonT' (classes Css.buttonIgnore Css.buttonRight)  Css.iconIgnore "Ignore" "Mark current article and its comments read and view next article. \nNew article comments will be ignored and never shown \n(unless you reset ‘ignore’ flag by marking article unread). \nKeyboard shortcut: x" msgsWidget.IgnorePost}
          {buttonNoT' Css.buttonReadability Css.iconReadabilityBW "Get full article text. \nKeyboard shortcut: g" msgsWidget.ToggleFullText}
(*          buttonT' Css.buttonLeft   Css.iconUp "Up" "Parent message. \nKeyboard shortcut: u" msgsWidget.Up}{ *)
          {buttonT' (classes Css.buttonPrev Css.buttonLeft) Css.iconPrev "Prev" "Previous message. \nKeyboard shortcut: k" msgsWidget.PrevTryFull}
          {buttonT' (classes Css.buttonNext Css.buttonRight)  Css.iconNext "Next" "Next message. \nKeyboard shortcut: j" msgsWidget.NextTryFull}
        </div> (* msgToolbar *)
      </div> (* top *)

      {msgsWidget.FeedMarkTopHtml
         (fn c =>
             fs <- Settings.articleFontSizeClass;
             fa <- feedAlignClass;
             return (classes c (classes fs fa)))}

      {infoMessage.Html}

      (* Левая панель *)

      <div class="leftPanelShadow"
           onclick={fn _ => P.hide}
           (* ^ требуется на iPad, почему-то до document.onclick не доходит
              (видимо из-за того, что fixed элемент)  *)
      ></div>
      <div dynClass={
            ifClassS displayDiscovery Css.displayDiscovery
            (return Css.left)}>
        <div class="topToolbar"><a href={bless "/"} class={Css.logo}>{Pages.logo}</a></div>
        <div class="msgToolbar">
          {buttonT' Css.buttonAddSubscription Css.iconAddSubscription "Add subscription" "Toggle feed discovery panel to add feeds by URL, title or #topic. \nKeyboard shortcut: a" (toggleDiscovery False)}
          {buttonNoT' Css.buttonCloseAddSubscription Css.iconCloseSearchBar "Close “Add subscription” panel"
                      (toggleDiscovery False)}
          {settingsButton}
        </div>
        <div dynClass={
             ifClassS onlyUpdatedSubscriptions Css.onlyUnreadSubscriptions
             (return (classes Css.subscriptions flexFullHeight))}>
          <div class={Css.dragMark} id={dragMarkId}>
            <div class={Css.dragMarkFill}></div>
          </div>
          {ssWidget.Html}
          <div class="subscriptionsPadder"></div>
        </div>
        {whatsNew lastWhatsNewTime}
        {buyNowButton}
        {discovery.Html}
      </div> (* left *)

      (* popup *)
      {P.embed P.popupSource}

      (* Blackout *)
      {dyn_ (
       pt <- signal paidTill;
       let fun blackout till daysMore reason =
           activeXml (
           popup <- source <xml/>;
           box <- P.newBox "Hello!" <xml>
             <p>{[reason]}</p>
             <p class="buyNowLink">
               {linkButton "Buy now!" (P.hide; toggleBuyBox' (P.toggle' popup True) buyT paidTill)}
             </p>
             {exportDeleteLogOut}
             <p class="accountExpirationNote">
               Can’t pay now? No worries, your account will be active till
               {formatTimeOnClient' "%B %e, %Y"
                 (addSeconds till (86400*daysMore))}.
               Afterwards, it will be archived
               (read/unread state will be lost
               but you can return anytime and restore your feeds
               and starred/tagged items).
             </p>
           </xml>;
           return <xml>
             <div class={classes Css.blackout Css.popup}>
               {P.popupXbody box}
               {P.embed popup}
             </div>
           </xml>)
       in
       return (case pt of
         | PTFreeTrialFinished t =>
           blackout t.Till 30 "Your 30 days free trial has expired. Subscribe to keep reading all the interesting articles in your favorite blogs!"
         | PTPaidFinished t =>
           blackout t.Till 60 "Your year subscription has expired. Subscribe to keep reading all the interesting articles in your favorite blogs!"
         | _ =>
           <xml/>)
       end)}

      (* Сообщения *)
      <div class="articles"
           id={if isMobile then Unsafe.id "dummyMsgDivId" else Settings.msgDivId}
           onscroll={scroll}
      >
        <div dynClass={s <- Settings.articleFontSizeClass;
                       fa <- feedAlignClass;
                       iw <- Settings.imagesWidthClass;
                       p <- Settings.imageProxyClass;
                       return (classList (
                           iw :: fa :: s :: p :: Css.msgsPadder :: []))}>
        {msgsWidget.HtmlPre}
        <div class="msgsHeader">
        <div class={Css.subscriptionTitle} dir="auto">
          {dyn_ (f <- signal currentFeed;
                 return (case f.SIType of
                           | SITFeed feed =>
                             (case feed.FeedLink of
                                | Some l =>
                                  textWithLink (Some (bless l)) (txt f.Title)
                                | None => txt f.Title)
                           | _ => txt f.Title))}
        </div>
        <div>
        (* Safari (macOS/iOS) на узком экране падает и перезагружает страницу
           при обновлении dyn-элементов (например, при Scanning comments).
           Но, если все dyn-элементы поместить внутри div, Safari начинает
           нормально работать.
           Интересно еще, что без этого div, при изменении ширины экрана
           searchTime/searchButtons перестают переключаться из двухстрочного
           в однострочный режим.
         *)
        {dyn_ (du <- discoverySubItemUrl;
               return (case du of
                 | Some u => <xml><div class={Css.subscribeDiscoveryFeed}>
                    {textButton "Subscribe" (subscribeDiscoveryFeed u)}
                 </div></xml>
                 | None => <xml/>))}
        {snInfoMsg}
        {dyn_ (signal scannedPercent)}
        <dyn signal=
             {sr <- signal searchResults;
              cf <- signal currentFeed;
              sf <- signal currentSearchFeed;
              c <- signal searchCounters;
              (fs,ss) <- signal ssWidget.FiltersAndSmartStreams;
              m <- signal maxFiltersOrSmartStreams;
              let val total = List.length fs + List.length ss
                  fun checkLimit what a =
                      if total >= m then
                          alert ("Can’t create " ^ what
                                 ^ ": too many filters or smart streams ("
                                 ^ show total ^ " of " ^ show m
                                 ^ " allowed, you can see more details in “Status” section of Account settings).\n\nPlease, remove some filters or smart streams first.\n\nYou can join filter queries using OR (see more in “Search hints” section in Help).")
                      else
                          a
                  val canFilter =
                      sf.Index <> discoverySubItemIndex
                      &&
                      case sf.SIType of
                        | SITAll => True
                        | SITFolder _ => True
                        | SITFeed _ => True
                        | _ => False
                  val query =
                      case cf.SIType of
                        | SITSearch s => s.Query
                        | _ => ""
                  val newSmartStream =
                      checkLimit "smart stream" <|
                      text <- source "";
                      tid <- fresh;
                      let val edit =
                              t <- get text;
                              sn <- Js.checkName "smart stream" t;
                              case sn of
                                | Some name =>
                                  P.hide;
                                  set filteringIsInProgress (Some "Creating new smart stream");
                                  u <- getSubItemGRIds sf;
                                  ssWidget.AddSmartStream name query u <| fn _ =>
                                  set filteringIsInProgress None;
                                  tryPushInterfacePath ("smartstream/" ^ Js.encodeURIComponent name);
                                  reloadFeed ()
                                | None => return ()
                      in
                      d <- P.newBox "Enter stream name" <xml>
                        {oneLineInputAndOkButton tid text "" "OK" edit}
                        <span class="dialogHint">
                        <p>Smart stream is a virtual feed containing articles matching your search query. Use it to monitor new interesting content in selected feeds.</p>
                        <p>You can edit or delete smart streams in settings.</p>
                        </span>
                      </xml>;
                      P.toggle d;
                      Js.focus tid
                      end
                  fun addF negate message =
                      P.hide;
(*                       c <- confirm (message ^ " Current and new articles in selected feeds will be filtered. You can edit or delete created filter in settings."); *)
(*                       when c *)
                           (set filteringIsInProgress (Some "Creating filter");
                              u <- getSubItemGRIds sf;
                              ssWidget.AddFilter query negate u <| fn _ =>
                              set filteringIsInProgress None;
                              sf <- get currentSearchFeed;
                              setFeed sf)
                  val newFilter =
                      checkLimit "filter" <|
                      d <- P.newBox "Select filter type" <xml>
                        {textButton "Hide found articles" (addF True "")}
                        {textButton "Show found articles only" (addF False "")}<br/>

                        <span class="dialogHint">
                        <p>Filters let you automatically hide articles matching (or not matching) your search query. Use them to read only interesting content in selected feeds.</p>
                        <p>Hidden articles are not marked as read and will appear again if&nbsp;you delete the filter. You can edit or delete filters in settings.</p>
                        </span>
                      </xml>;
                      P.toggle d
              in
              return (case sr of
                  None => <xml/>
                | Some sr => <xml>
                  <div class="searchTime">
                    <span title={"search took " ^ show sr.Took ^ " ms"}>
                      <active code={
                       c <- get searchCounters;
                       (* чтобы не обновлять текст при каждом помечании
                          сообщения как прочитанного *)
                       return (txt (
                        let val t = c.TotalPosts + c.TotalComments
                            val u = sr.UnreadPosts + sr.UnreadComments
                        in
                            "Found " ^ show t ^ " " ^ plural t "article"
                            ^ (if u > 0 then " (" ^ show u ^ " unread)" else "")
                        end))} />
                    </span>
                  </div>
                  <div class="searchButtons">
                    {dyn_ (
                     ip <- signal filteringIsInProgress;
                     return (case ip of
                       | Some p =>
                         <xml><div class="searchFiltering"><span class="spinner"></span> {[p]}…</div></xml>
                       | None => <xml>
                         {textButton "Back" (setFeed sf)}
                         {displayIfC canFilter <xml>
                           {textButton "New filter" newFilter}
                           {textButton "New smart stream" newSmartStream}
                         </xml>}
                         </xml>))}
                  </div>
                  </xml>)
              end
             } />
        </div> (* dyns *)
        </div> (* msgsHeader *)
        {msgsWidget.Html}
        </div> (* articleFontSize / msgsPadder *)

        <div dynClass=
            {emf <- msgsWidget.IsForestEmpty;
             ar <- msgsWidget.AppendRequests;
             a <- msgsWidget.LoadingAppendRequests;
             groupByFeed <- Monad.mp mtvmGroupByFeed msgTreeViewMode;
             uc <- signal Settings.ultraCompact;
             return (ifClass (isNull ar && not a && not emf) Css.noMoreArticlesText
                    (ifClass emf Css.emptyMsgForest
                    (ifClass uc Css.ultra
                    (ifClass groupByFeed Css.groupByFeed
                    Css.msgTreeSpacer))))
            }>
            <div dynClass={Monad.mp (classes Css.msgsPadder) Settings.articleFontSizeClass}>
              <div class="msgsHeader">
                {dyn_ (signal msgTreeSpacerText)}
              </div>
            </div>
        </div>
      </div> (* articles *)
    </span></xml> }
    end end
    fun runSrc s = h <- get s; withSome id h
    in
        inner <- source <xml/>;
        unload <- source None;
        resize <- source None;
        scroll <- source None;
        let fun onload () =
                p <- mainPage ();
                set unload (Some p.Onunload);
                set resize (Some p.Onresize);
                set scroll (Some p.Onscroll);
                set inner p.Xml;
                p.Oninit
        in
        pageNoBody'
          <xml>
            {Pages.htmlHeadMain}
          </xml> "BazQux Reader" <xml>
          <body
             class={Css.mainPage}
             onload={Unsafe.initStorageSources uid; Js.jsInit; onload ()}
             onunload={runSrc unload}
             onresize={runSrc resize}
             onscroll={runSrc scroll}
             >
             {dyn_ (signal inner)}
        </body>
        </xml>
    end end

fun trySetReferrer (qs : option queryString) =
    case H.parseQueryStringUtf8Only (maybe "" show qs) of
      | ("referrer", "bing") :: ("querystring", q) :: ("keyword", k)
         :: ("adid", addid) :: _ =>
        r <- getHeader (blessRequestHeader "Referer");
        Session.setReferrer
            ("PaidBing \"" ^ q ^ "\" (" ^ k ^ " / " ^ addid
             ^ maybe "" (fn x => if x <> "" then " / " ^ x else "") r
             ^ ")");
        redirect (bless "/") (* убираем query string *)
      | ("referrer", "google") :: ("keyword", k)
         :: ("matchtype", m) :: ("network", n) :: ("creative", c) :: _ =>
        r <- getHeader (blessRequestHeader "Referer");
        Session.setReferrer
            ("PaidGoogle \"" ^ k ^ "\" ("
             ^ (case m of
                | "e" => "exact"
                | "b" => "broad"
                | "p" => "phrase"
                | _ => m)
             ^ " / "
             ^ (case n of
                | "g" => "Google search"
                | "s" => "search partner"
                | "d" => "display network"
                | "u" => "Smart Shopping Campaign"
                | "ytv" => "YouTube videos"
                | "vp" => "Google video partners"
                | _ => n)
             ^ " / " ^ c
             ^ maybe "" (fn x => if x <> "" then " / " ^ x else "") r
             ^ ")");
        redirect (bless "/") (* убираем query string *)
      | _ =>
        return ()

fun main (qs : option queryString) =
    Rpcs.rmWWW (
    u <- Session.getUser "";
    case u of
      | Some uid =>
        if uid = "demo" then
            t <- now;
            userUI (PTFreeTrial { Till = addSeconds t (30*84600-1) }) "demo"
        else
        (* page "commented out" <xml/> *)
        (p <- H.getPaidTill uid;
         Session.logAction "main" uid (userUI p uid))
      | None     =>
        trySetReferrer qs;
        Pages.loginUI { Login = "", Password = "" } None False ELALogin)

fun i qs = main qs

fun demo s =
    Session.logAction "demo" "demo" (
    Session.sessionLogOut;
    s <- H.newSessionJunk (LTUsername { Username = "demo"}) LATNone [];
    sc <- Session.sessionCookie;
    secure <- Session.secureCookie;
    setCookie sc {Value = s.Key, Expires = Some s.Expire, Secure = secure};
    (* FF не любит перенаправления на самого себя, а у urweb какие-то
       проблемы с подписыванием куки для demo *)
    redirectToMain)

task initialize = fn () =>
    H.runTasks;
    H.runApiServer
(*     H.reloadBrowserPage *)

val getUserIdBySession = Session.getUserIdBySession
