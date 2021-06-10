open Uim
open Either

structure P = Popups

datatype share
  = SLink of { Title : string, UrlFormat : string, Shorten : bool }
  | SPocket
  | SSystem
  | SDiscontinued

fun shareActionToShare sa =
    let fun sl t u = SLink { Title = t, UrlFormat = u, Shorten = True }
        fun l t u = SLink { Title = t, UrlFormat = u, Shorten = False }
    in
        case sa of
  | SACustom c => SLink (c.CustomShareAction -- #Id)
  | SASystem => SSystem
  | SAEMail => l "Email" "mailto:?subject=[TITLE]&body=[URL]"
  | SATwitter => sl "Twitter" "https://twitter.com/share?url=[URL]&text=[TITLE]"(* "&via=BazQuxReader" *)
  | SAFacebook => sl "Facebook"
    (* TODO: когда появится custom sharing, сделать SAFacebook appId,
       и передавать его с сервера в UserSettings.ShareSettings,
       хотя через Facebook делятся всего несколько человек
     *)
    "https://www.facebook.com/dialog/share?app_id=468204013193852&href=[URL]"
    (* &display=popup *)
    (* "https://www.facebook.com/sharer.php?u=[URL]" *)
  | SAGooglePlus => sl "Google+" "https://plus.google.com/share?url=[URL]"
  | SATumblr => sl "Tumblr" (* "https://www.tumblr.com/share?v=3&u=[URL]&t=[TITLE]" *)
                "https://www.tumblr.com/share/link?url=[URL]&name=[TITLE]"(* &description=[CONTENT] *)
  | SAEvernote => l "Evernote" "https://www.evernote.com/clip.action?url=[URL]&title=[TITLE]"
  | SADelicious_discontinued => SDiscontinued
  | SAPinboard => l "Pinboard" "https://pinboard.in/add?url=[URL]&title=[TITLE]"
  | SAPocket => SPocket
  | SAReadability_discontinued => SDiscontinued
(*   | SAInstapaper => l "Instapaper" "https://www.instapaper.com/add?url=[URL]&title=[TITLE]" *)
  | SAInstapaper => l "Instapaper" "https://www.instapaper.com/hello2?u=[URL]&title=[TITLE]"
  | SATranslate => l "Translate" "https://translate.google.com/translate?u=[MSG_URL]"
  | SABlogger => sl "Blogger" "https://www.blogger.com/blog_this.pyra?t&u=[URL]&n=[TITLE]"
  | SAWordpress => sl "Wordpress" "https://wordpress.com/press-this.php?u=[URL]&t=[TITLE]" (* &s=[CONTENT]&i=[post-img]" *)
  | SALinkedIn => sl "LinkedIn" "https://www.linkedin.com/shareArticle?mini=true&url=[URL]&title=[TITLE]"
  | SAPinterest => sl "Pinterest" "https://pinterest.com/pin/create/bookmarklet/?url=[URL]&description=[TITLE]" (* media=[post-img]&is_video=[is_video] *)
  | SAVK => sl "VK" "https://vk.com/share.php?url=[URL]"
  | SASkype => l "Skype" "https://web.skype.com/share?url=[URL]"
  | SAReddit => sl "Reddit" "https://reddit.com/submit?url=[URL]&title=[TITLE]"
  | SAStumbleUpon => sl "StumbleUpon" "https://www.stumbleupon.com/submit?url=[URL]&title=[TITLE]"
  | SADigg => sl "Digg" "https://digg.com/submit?url=[URL]&title=[TITLE]"
  | SAScoopIt => sl "Scoop.it" "https://www.scoop.it/bookmarklet?fs=1&url=[URL]"
  | SAFlipboard => sl "Flipboard" "https://share.flipboard.com/bookmarklet/popout?v=2&title=[TITLE]&url=[URL]"
  | SABuffer => sl "Buffer" "https://buffer.com/add?url=[URL]&text=[TITLE]"
  | SANewsVine => sl "Newsvine" "https://www.newsvine.com/_tools/seed&save?u=[URL]&h=[TITLE]"
                  (* вроде это не bookmarking, а social journalizm *)
  | SADiigo => l "Diigo" "https://www.diigo.com/post?url=[URL]&title=[TITLE]"
  | SARememberTheMilk => l "Remember The Milk" "https://www.rememberthemilk.com/services/ext/addtask.rtm?t=[TITLE]+[URL]"
  | SAGoogleBookmarks => l "Google Bookmarks" "https://www.google.com/bookmarks/mark?op=edit&bkmk=[URL]&title=[TITLE]" (* &annotation=" *)
  | SAWallabag => l "Wallabag" "https://app.wallabag.it/bookmarklet?url=[URL]"
  | SAWakelet => l "Wakelet" "https://wakelet.com/save?media=[URL]"
(*   | SABaidu => sl "百度" "http://apps.hi.baidu.com/share/?url=[URL]&title=[TITLE]" не работает *)
(*   | SAWeibo => sl "微博" "http://service.weibo.com/share/share.php?url=[URL]&title=[TITLE]" *)
(*   | SARenren => sl "人人网" "http://share.renren.com/share/buttonshare.do?link=[URL]&title=[TITLE]" *)
    end
val pocketLink = "https://getpocket.com"

fun trackShareAction a =
    BackgroundRpc.addAction (BGShareAction { ShareAction = a })

fun msgContents key readability qs =
    h <- Session.getHost;
    al <- acceptLanguage;
    m <- H.readMsgAndApplyFixes h al key;
    case m of
       | None => error <xml>Article not found</xml>
       | Some m =>
         text <-
             (if readability then
                  al <- acceptLanguage;
                  r <- H.getFullText "translate" True h al key;
                  return (case r of Left _ => m.Text | Right t => (t : Binary_ffi.xbodyString))
              else
                  return m.Text);
         scrollable <- H.pageFromFile "web/bazqux/scrollable.html";
         pageNoBody' <xml/> m.Subject <xml>
           <body class={Css.msgContents}>
             <div class="post">
             {case m.AuthorPic of
                | None => <xml/>
                | Some u => <xml>
                  <div class="authorPic">
                    <img class="authorPic" src={u} />
                  </div>
                </xml>}
             <div class="msgCollapseMargin">
             <div class="msgBody">
               <div class="msgBodyHeader">
                 <div class="msubject">{textWithLink m.Link (txt m.Subject)}</div>
                 {case m.Tags of
                    | [] => <xml/>
                    | t => <xml><div class="mtags">{["tags: " ^ intercalate ", " t]}</div></xml>}
                 {if m.Author <> "" then <xml><div class="mauthor">{textWithLink m.AuthorUri (txt m.Author)}</div></xml>
                  else <xml/>}
               </div>
               <div class="mtext">{H.xbodyStringToXbody text}</div>
             </div></div></div>
           </body>
           {scrollable}
         </xml>

fun substUrlFormat fmt subject (link:url) key readability =
    let val title = Js.encodeURIComponent subject
        val url = Js.encodeURIComponent (show link)
        val msg_url = Js.encodeURIComponent ("https://" ^ Js.locationHost () ^ show (effectfulUrl (msgContents key readability)))
    in
        Js.strReplace "[TITLE]" title
        (Js.strReplace "[URL]" url
        (Js.strReplace "[MSG_URL]" msg_url fmt))
    end

fun translateLink readability (key : msgKey) =
    case shareActionToShare SATranslate of
        SLink l => substUrlFormat l.UrlFormat "" (bless "") key readability
      | _ => "translateLink: not SLink?"

val saveServices = SAPocket :: SAInstapaper :: SAWallabag :: SAPinboard :: SAEvernote :: SAWakelet :: []
val shareServices = SATwitter :: SAFacebook :: SABuffer :: SATumblr :: SAVK :: []

val uimShareText = Js.strReplace "­" "" <<< uimSubjectOrShortText
(* без soft hyphen, а то в Twitter получаются переносы без дефиса *)

fun shareMenuContents (UIM uim) (link:url) =
    readability <- uimReadabilityMode (UIM uim);
    let val key = uimMsgKey (UIM uim)
        val subject = uimShareText (UIM uim)
        val shareData = { Title = subject, Text = "", Url = link }
        val saveToPocket =
            ps <- get uim.AddToPocketState;
            case ps of ATPSAdding => return () | _ =>
            set uim.AddToPocketState ATPSAdding;
            x <- tryRpc (Pages.addToPocket (show link) subject);
            let val clear = set uim.AddToPocketState ATPSNone in
            clear;
            case x of
              | Some OEROK =>
                f <- fresh;
                set uim.AddToPocketState (ATPSAdded f);
                Js.setTimeout "atp"
                              (s <- get uim.AddToPocketState;
                               case s of
                                 | ATPSAdded f' =>
                                   when (f = f') clear
                                 | _ => return ())
                              5000
              | Some (OERError e) =>
                alert e.Error
              | Some (OERRedirect r) =>
                a <- P.newBox "Add to Pocket" <xml>
                  <p>{hrefLink (txt "Authorize to Pocket") r.Url}
                    to add an article and enable one-click article saving.</p>
                </xml>;
                P.toggle a
              | None =>
                alert "Can’t connect to the server"
            end
        fun b sa : xbody = case shareActionToShare sa of
            | SLink l =>
              P.llif (trackShareAction sa) l.Title
                     (bless (substUrlFormat l.UrlFormat subject link key readability))
            | SPocket =>
              P.lif (bless pocketLink) "Pocket" saveToPocket
            | SSystem =>
              P.lii Css.iconShare "System…"
                <| trackShareAction sa; Js.share shareData
            | SDiscontinued =>
              <xml/>
    in
      return <xml>
          {b SAEMail}
          <hr/>
          {List.mapX b saveServices}
          <hr/>
          {displayIfC (Js.canShare shareData) (b SASystem)}
          {List.mapX b shareServices}
          <hr/>
          {b SATranslate}
(*           <hr/> *)
(*           {List.mapX b (SABlogger *)
(*  :: SAWordpress *)
(*  :: SALinkedIn *)
(*  :: SAPinterest *)
(*  :: SAVK *)
(*  :: SASkype *)
(*  :: SAReddit *)
(*  :: SAStumbleUpon *)
(*  :: SADigg *)
(*  :: SAScoopIt *)
(*  :: SAFlipboard *)
(*  :: SABuffer *)
(*  :: SANewsVine *)
(*  :: SADiigo *)
(*  :: SARememberTheMilk *)
(*  :: SAGoogleBookmarks :: [] *)
(* )} *)
        </xml>
    end


fun toggleShareMenu uim =
    withSome
        (fn l =>
            c <- shareMenuContents uim l;
            P.toggleContextMenu
                (Unsafe.id "shareMenu")
                (Some (Right (Js.offsetBottomRight (uimFrameId uim) Css.iconShare)))
                c)
        (uimMsgLink uim)

val preloadShareIcons =
    Js.preloadImages
        (List.mapX
             (fn sa => <xml>
               <img src={Js.faviconUrl
                             (case shareActionToShare sa of
                              | SLink l => l.UrlFormat
                              | SPocket => pocketLink
                              | SSystem => ""
                              | SDiscontinued => "") True}></img>
             </xml>)
             (List.append saveServices
              (List.append shareServices (SATranslate :: []))))

fun mailLink uim (link:url) =
    (* нехорошо, что этот код дублирует код из вызова shareMenu *)
    trackShareAction SAEMail;
    Js.openLink
        (bless ("mailto:?subject=" ^
                Js.encodeURIComponent (uimShareText uim) ^
                "&body=" ^ Js.encodeURIComponent (show link)))

fun translate openFunc uim =
    trackShareAction SATranslate;
    r <- uimReadabilityMode uim;
    openFunc uim (bless (translateLink r (uimMsgKey uim)))
