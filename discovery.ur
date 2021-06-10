open Datatypes
open Utils
open SubItem

structure P = Popups

val discoveryTextBoxId = Unsafe.id "Discovery.discoveryTextBoxId"

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

con mtvmMap = { MtvmMap : {} }
ffi newMtvmMap : list (string * msgTreeViewMode) -> transaction mtvmMap
ffi updateMtvmMap : string -> msgTreeViewMode -> mtvmMap -> transaction {}
ffi clearMtvmMap : string -> mtvmMap -> transaction {}
ffi lookupMtvmMap : string -> mtvmMap -> transaction (option msgTreeViewMode)

fun discoveryWidget addSub opmlUploadClick displayDiscovery =
    text <- source "";
    country <- source "RU";
    contents <- source <xml/>;
    showImport <- source True;
    cache <- source [];
    contentsId <- fresh;
    looksLikeUrl <- source False;
    lastQuery <- source " ";(* чтобы первый запрос отработал и показал топики *)
    nonEmptyText <- source False;
    mtvms <- bind (newMtvmMap []) source;
    let fun modMtvm m =
            bind (get mtvms) m;
            bind (get cache) (List.app (fn (_,(_,r)) =>
                                           case r of
                                              | Some (x,vms) => m vms
                                              | None => return ()))
            (* TODO: по хорошему, не надо обновлять режимы просмотра,
             а использовать те, что уже есть в основном окне.
             Тогда и setMtvm не понадобится. А то, при забитом кеше в discovery
             на 2.5k подписок работает где-то 2.5сек (вместо 0.3 без setMtvm)
             *)
        fun setMtvm url mtvm =
            set cache [];
            (*  ^ тупо убиваем кеш, чтобы не тормозило на куче подписок *)
            modMtvm (updateMtvmMap url mtvm)
(*                 (fn x => (url,mtvm) :: List.filter (fn (u,_) => u <> url) x) *)
        fun clearMtvm url =
            modMtvm (clearMtvmMap url) (* (List.filter (fn (u,_) => u <> url)) *)
        fun lookupMtvm url =
            ms <- get mtvms;
            mtvm <- lookupMtvmMap url ms(* List.assoc url ms *);
            return (Option.get (modifyF [#UnreadOnly] (const False)
                                        defaultMsgTreeViewMode) mtvm)
        val getQuery =
            v <- get text;
            set nonEmptyText (v <> "");
            return (Js.cleanDiscoveryQuery v)
        fun lookupCache co query =
            ca <- get cache;
            time <- now;
            return (case List.assoc (co, query) ca of
              | Some (t, r) =>
                if diffInSeconds t time >= 3600 then None else Some r
              | _ => None)
        fun insertCache co query (r : option (xbody * mtvmMap)) =
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
            d <- P.newBox
              "Select country for subscriptions search"
              <xml><ctextbox id={tid} class="selectSubscriptionInput" size={30}
                dir={Js.dirAuto} /></xml>;
            P.toggle d;
            Js.setupCountryAutocomplete tid
                (fn co =>
                    set country co;
                    P.hide;
                    BackgroundRpc.handleBGActions
                        (BGSetCountry { Country = co } :: []));
            Js.select tid;
            Js.focus tid
        fun setContents (c,ms : transaction mtvmMap) =
            set contents c;
            Js.setScrollTop contentsId 0.0;
            bind ms (set mtvms)
        fun topicsList topics =
            List.mapX (fn topic => <xml>
              <div onclick={fn e =>
                               set text ("#" ^ topic);
                               when (not (Js.hasOnscreenKeyboard ()))
                                    (* ^ на мобилах не выделяем *)
                                    (Js.select discoveryTextBoxId);
                               search ()
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
            hide;
            cleanSearch ();
            addSub u
        and setSearchResult (r : option (xbody * mtvmMap)) =
            set showImport False;
            q <- getQuery;
            case r of
              | Some (x,ms) =>
                setContents (x, return ms)
              | None =>
                setContents (<xml><div class="discoveryNotFound">
                 {if Js.discoveryQueryLooksLikeUrl q then
                      <xml>Click “Add” or press Enter key to find the feed on site.</xml>
                  else
                      <xml>Try enter the full feed or site URL.</xml>}
                 </div></xml>, newMtvmMap [])
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
                    r <- tryRpc (Rpcs.discover co query);
                    (case r of
                       | Some x0 =>
                         x <- (case x0 of
                                   None => return None
                                 | Some (x,msl) =>
                                   ms <- newMtvmMap msl;
                                   return (Some (x,ms)));
                         insertCache co query x;
                         q' <- getQuery;
                         when (q' = query) (setSearchResult x)
                       | None =>
                         return ())
            else
                set showImport True;
                setContents (default (), newMtvmMap []))
        val tryAdd =
            q <- getQuery;
            if Js.discoveryQueryLooksLikeUrl q then
                add ()
            else
                search ()
    in
    spawn (search ());
    dummy <- fresh;
    return { Hide = hide
           , Country = country
           , GetQuery = getQuery
           , GetCountry = get country
           , SetMtvm = setMtvm
           , ClearMtvm = clearMtvm
           , LookupMtvm = lookupMtvm
           , Html = <xml>
      <div class="discoveryHeader">
      <div class="discoveryHeaderHint">Enter URL, title or #topic</div>
      <div dynClass={ne <- signal nonEmptyText;
                     return (ifClass ne
                                     Css.discoveryNonEmptyText
                                     Css.oneLineInputAndButton)}>
        {divClass Css.oneLineInputAndButtonBorder <xml>
        <ctextbox id={discoveryTextBoxId}
           class="feedUrlInput oneLineInputAndButtonInput" source={text} size={30}
           dir={Js.dirAuto}
           oninput={search ()}
           onkeydown={onEnter (Js.setTimeout "tryAdd" tryAdd 0)}
           />
        </xml>}
        <div class="discoveryCleanButton"
          onclick={fn _ => cleanSearch ();
                   when (not (Js.hasOnscreenKeyboard ()))
                        (Js.focus discoveryTextBoxId)}>×</div>
        <div dynClass={ifClassS looksLikeUrl Css.discoveryAddDisabled
                       (return (classes Css.textButton Css.oneLineInputAndButtonButton))}
             onclick={fn _ => tryAdd}>
          {buttonText "Add"}
        </div>
      </div>
      {displayIf showImport <xml>
        <div class="discoveryImportButton">
          {Js.opmlForm (textButton "Import OPML" opmlUploadClick)}
        </div></xml>}
    </div>
    <div id={contentsId} class="discoveryContents flexFullHeight">
      {dyn_ (signal contents)}
      <div class="subscriptionsPadder"></div>
    </div>
    </xml> }
    end
