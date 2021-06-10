open Datatypes
open Css
open Utils
open Feeds
structure P = Popups

fun initEditQueryBox toggle what fullWhat q edit =
    i <- fresh;
    e <- Js.editQueryFeeds q.FeedGRIds;
    modifying <- source None;
    query <- source q.Query;
    let val close =
            stopPropagation;
            a <- P.isSubPopupActive;
            if a then P.hideSubPopup else P.hide
        val focus =
            Js.select i;
            Js.focus i
        val ok =
            m <- get modifying;
            when (Option.isNone m) (
            q <- get query;
            f <- e.GetFeedGRIds;
(*             if q = "" then *)
(*                 alert "Please enter non-empty query" *)
(*             else if List.length f = 0 then *)
(*                 alert "Please select at least one feed" *)
            set modifying
                (Some
                <xml><div class="searchFiltering">
                  <span class="spinner"></span> Modifying {[what]}…
                </div></xml>);
            edit q f <| fn e => case e of
              | Left err =>
                set modifying None;
                alert err;
                focus
              | Right a =>
                close;
                a)
        fun queryLine title x =
            <xml><div class="editQueryLine">
              <div class="editQueryTitle">{[title]}</div>
              {x}
            </div></xml>
    in
    b <- P.newBigBox
      ("Filters & streams > Edit " ^ fullWhat) (noHyphens
       <| disabledIf (Monad.mp Option.isSome <| signal modifying) <xml>
        <div class="editQueryDialogContents">
        {dyn_ (Monad.mp (compose (queryLine "Query") (Option.get (<xml>
            <ctextbox id={i} source={query} class="editQueryInput" size={30}
              dir={Js.dirAuto} onkeydown={onEnter ok} />
            {textButton "OK" ok}
            {textButton "Cancel" close}
          </xml>))) (signal modifying))}
        {queryLine "Feeds" <xml>
          {textButton "Select all" e.SelectAll}
          {textButton "Select none" e.SelectNone}
        </xml>}
        <div class="editQueryFeeds">
          {e.Xml}
        </div>
        </div>
      </xml>);
    toggle b;
    e.UpdateFolders;
    focus
    end

fun handle pre onOk post =
    pre <| fn e => post <| case e of
      | None => Right onOk
      | Some e => Left e

fun editSmartStreamAction (ssWidget : ssWidget) reloadFeed name =
 fn fqr q f =>
    handle
      (ssWidget.EditSmartStream name q f)
      (cf <- getCurrentFeed;
       when (Js.subItemHasQueryFeeds cf.Index fqr.FeedGRIds f)
            (reloadFeed ()))
fun editSmartStreamDialog (ssWidget : ssWidget) reloadFeed name =
    (_,ss) <- get ssWidget.FiltersAndSmartStreams;
    case List.find (fn (n,_) => n = name) ss of
      | Some (_, q) =>
        initEditQueryBox P.toggle "smart stream"
                         ("smart stream “" ^ name ^ "”") q
                         (editSmartStreamAction
                              ssWidget reloadFeed name q)
      | _ => return ()

fun filtersBox (ssWidget : ssWidget) setFeed reloadFeed0 =
    inProgress <- source False;
    let fun reloadFeed () =
            spawn (reloadFeed0 ())
            (* запускаем обновление в фоне, чтобы возможная ошибка в поиске
               не подвешивала диалог (заодно ускоряем редактирование за счет
               того, что не ждем загрузки фида).
             *)
        fun subline l r =
            <xml><div class="filterSubLine">
              <div class="filterLeft">{[l]}
              </div><div class="filterRight">{r}</div>
            </div></xml>
        fun line toggle what fullWhat editAction deleteAction q prefixXml =
            removing <- source False;
            return (dyn_ (r <- signal removing; return <xml>
              <div class="filterLine">
                {prefixXml}
                {subline "In" (Js.displayQueryFeeds q.FeedGRIds)}
                {subline ""
                  (disabledIf (signal inProgress) <|
                   if r then
                       <xml><div class="searchFiltering">
                         <span class="spinner"></span> Deleting {[what]}…
                       </div></xml>
                   else <xml>{
                       textButton "edit"
                          (initEditQueryBox toggle what fullWhat q editAction)}
                      {textButton "delete"
                          (askBefore
                             ("Are you sure you want to delete this " ^ what ^ "?")
                             (set inProgress True;
                              set removing True;
                              deleteAction (set inProgress False)))
                          }</xml>)}
              </div></xml>))
    in
    P.newBigBoxWithSubPopup "Filters & streams"
    (fn toggle =>
        dyn_ ((fs, ss) <- signal ssWidget.FiltersAndSmartStreams;
              return <xml><active code={
              filters <-
                List.mapXM (fn (fid, f) =>
                  line toggle "filter" "filter"
                       (fn q feeds =>
                           handle
                             (ssWidget.EditFilter fid q f.Negate feeds)
                             (cf <- getCurrentFeed;
                              when (Js.subItemHasQueryFeeds cf.Index f.FeedGRIds feeds)
                                   (reloadFeed ()))
                       )
                       (fn after =>
                          ssWidget.DeleteFilter fid <| fn _ =>
                          after;
                          cf <- getCurrentFeed;
                          when (Js.subItemHasQueryFeeds cf.Index f.FeedGRIds [])
                               (reloadFeed ())
                       )
                       f <xml>
                    {subline (if f.Negate then "Hide" else "Show")
                     <xml><div class="filterQuery" dir="auto">{[f.Query]}</div></xml>}
                  </xml>) fs;
              smartStreams <-
                List.mapXM (fn (name, q) =>
                  line toggle "smart stream"
                       ("smart stream “" ^ name ^ "”")
                       (editSmartStreamAction ssWidget reloadFeed name q)
                       (fn after =>
                        cf <- getCurrentFeed;
                        (case cf.SIType of
                          | SITSmartStream s =>
                            when (s.StreamName = name)
                                 (setDefaultFeed setFeed)
                          | _ => return ());
                        ssWidget.DeleteSmartStream name <| fn _ =>
                        after;
                        cf <- getCurrentFeed;
                        when (Js.subItemHasQueryFeeds cf.Index q.FeedGRIds [])
                             (reloadFeed ())
                       )
                       q <xml>
                    {subline "Stream"
                      <xml><div class="smartStreamName" dir="auto">{[name]}</div></xml>}
                    {subline "Query"
                      <xml><div class="filterQuery" dir="auto">{[q.Query]}</div></xml>}
                  </xml>) ss;
              return <xml>
                <h1>Filters</h1>
                {case fs of
                   | [] => <xml><p class="gray">You have no active filters. Search for the things you’d like to hide or show and click “New filter” button.</p></xml>
                   | _ => noHyphens filters}
                <h1 class="smartStreamsPadding">Smart streams</h1>
                {case ss of
                   | [] => <xml><p class="gray">You have no active smart streams. Search for the things you’d like to monitor and click “New smart stream” button.</p></xml>
                   | _ => <xml>{noHyphens smartStreams}<p></p></xml>}
                <p class="gray">You could read more about filters and smart streams in our {hrefLinkStopPropagation (txt "blog post") "https://blog.bazqux.com/2014/04/filters-and-smart-streams.html"}.</p>
                <p class="gray">Read about search syntax in our {hrefLinkStopPropagation' (txt "search hints") (show (url Pages.search_hints))} page.</p>
              </xml> } /></xml>))
    end
