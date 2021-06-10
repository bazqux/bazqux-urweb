open Utils
open Session
open HtmlTags
open HtmlUtils
open Either

structure P = Popups

val opml : transaction page = Rpcs.withUser "OPML" (fn userId =>
    o <- H.userOPML True userId;
    setHeader (blessResponseHeader "Content-Disposition")
              "attachment; filename=bazqux-reader-subscriptions.xml";
    returnBlob (textBlob o) (blessMime "text/x-opml")) []

val backToMain =
    <xml><p class="homeLink"><a href={bless "/"}>Home</a></p></xml>

val logo : xbody = <xml>
  <span class="logoBaz">Baz</span><span class="logoQux">Qux</span>
  <span class="logoReader">Reader</span></xml>

val supportEmail = noHyphens <xml><a href={bless "mailto:support@bazqux.com"}>support@bazqux.com</a></xml>

fun landingPage' s title head content : transaction page =
    ls <- H.pageFromFile "web/bazqux/landing_scripts.html";
    win <- isWindows;
    pageNoBody'
      <xml>{head}
        <link rel="preload" as="font" type="font/woff2" href="/images/fonts/5742946/b94a1c08-b8bd-4d11-b916-37dfb2073306.latin.woff2" /> (* Kievit bold *)
        <link rel="preload" as="font" type="font/woff2" href="/images/fonts/5743071/ca33c897-5e7a-42a2-a70d-d134d07d08b6.latin.woff2" /> (* Kievit regular *)
        <link rel="preload" as="font" type="font/woff2"
          href={bless (if win
            then "/images/fonts/5327262/9eb66b61-093b-4e16-8787-ed765ec564b1.woff2" (* ITC Charter regular *)
            else "/images/fonts/5682404/6f7f55b5-d511-474f-9652-affbdac0a19e.latin.woff2" (* PT ITC Charter regular *))} />
      </xml>
      ("BazQux Reader" ^ (if title <> "" then " — " ^ title else "")) <xml>
      <body class="landing">
        <div class={classes Css.landingPage s}>
          <h1><a href={bless "/"}>{logo}</a></h1>
          {content}
        </div>
      </body>
      {ls}
    </xml>
val landingPage = landingPage' null

fun infoPage' cls title head (t : xbody) =
    landingPage' cls title head <xml>
      <h2>{[title]}</h2>
      {t}
      {backToMain}
    </xml>
fun infoPage t = infoPage' Css.infoPage t <xml/>

(*     pageNoBody title <xml><body class="errorPage"> *)
(*       <h1>{[title]}</h1> *)
(*       {t} *)
(*       <p><a href={bless "/"}>Home</a></p> *)
(*     </body></xml> *)

fun errorPage (e : xbody) =
    infoPage' Css.errorPage "Error happened"
    <xml>
      <link rel="preload" as="font" type="font/woff2" href="/images/fonts/FiraMono-Regular.woff2" />
    </xml>
    <xml>
      <p>There was an error in handling your request:
      <pre class="errorText">{
         H.escapeXbody e (* реально тут приходит просто текст как есть *)
      }</pre></p>
    </xml>

val whoami =
    Rpcs.withUser "" (fn userId =>
    uid <- humanReadableUID userId;
    infoPage' Css.errorPage "Who am I" <xml/> <xml>
      (* тоже errorPage, чтобы errorText был пошире *)
      <p>Your user ID is:
        <div class="errorText">{[uid]}</div></p>
    </xml>) []

fun newSessionAndDelayedRedirect path email title reason =
    newSessionForEmail email;
    infoPage title <xml>
      <p>{reason}; you will be redirected to the home page.</p>
      <p class="homeLink"><a class="grayButton" href={bless path}>Continue to BazQux Reader</a></p>
      {activeCode (Js.setTimeout "redirect" (redirect (bless path)) 4000)}
    </xml>

fun activate_account_ f =
    e <- H.verifySignUpToken f.Token;
    case e of
      | Some email =>
        newSessionAndDelayedRedirect "/" email "Welcome to BazQux Reader"
          (txt "Your new account is confirmed")
      | None =>
        infoPage "Account confirmation error" <xml>
          <p>Sorry, this account confirmation link is no longer valid. Perhaps your account is already active?</p>
        </xml>

fun activate_account token =
    infoPage "Welcome to BazQux Reader" <xml>
      <form class="loginForm">
        <hidden{#Token} value={token} />
        <submit value={"Click here to activate your account"}
          class="greenButton" action={activate_account_}/>
      </form>
    </xml>

fun validateEmail retry login action =
    if login = "" then
        retry "Please, enter email address."
    else
        case H.validateEmail login of
          | None =>
            retry "Please, enter valid email address."
          | Some email =>
            action email

fun login () = redirectToMain
(* если оставить val, то инлайнит /login в /Utils/redirectToMain *)

fun password_reset_ f =
    e <- H.passwordResetEmail f.Login;
    case e of
      | Left er =>
        passwordResetForm f er
      | Right (uid, email) =>
        h <- getHost;
        sent <- H.sendPasswordResetEmail h uid email;
        if sent then
            infoPage "Password reset" <xml>
              <p>We sent email with instructions on how to reset your password
                to {emailX (if Option.isSome (H.validateEmail f.Login)
                            then email else H.maskEmail email)}.
                You should receive it shortly.
              </p>
              <p>If it doesn’t arrive, check your spam folder.</p>
            </xml>
        else
            infoPage "Password reset error" <xml>
              <p>Too many password reset requests for {emailX email}. Please, check your spam folder.</p>
            </xml>

and passwordResetForm f reason =
    i <- fresh;
    infoPage "Password reset" <xml>
      <p class="errorMessage">{[reason]}</p>
      <p>Enter your email, username or order ID</p>
      <form class="loginForm">
        <textbox{#Login} class="mlLogin" value={f.Login} id={i}
          placeholder="Email, username or order ID" />
        <submit value={"Reset password"} class="greenButton"
          action={password_reset_}/>
      </form>
      {autoFocus i}
    </xml>

fun newPassword_ f =
    if f.Password = "" then
      newPassword f "Password can’t be empty."
    else
      h <- getHost;
      e <- H.verifyPasswordResetToken h f.Token f.Password;
      case e of
        | Some email =>
          newSessionAndDelayedRedirect "/" email "Password reset"
            (txt "Your password was reset")
        | None =>
          infoPage "Password reset" <xml>
            <p>Sorry, that password change link is no longer valid. Please, <a link={password_reset ""}>try again</a>.</p>
          </xml>

and newPassword f reason =
    i <- fresh;
    infoPage "Password reset" <xml>
      <p class="errorMessage">{[reason]}</p>
      <p>Choose a password</p>
      <form class="loginForm">
        <hidden{#Token} value={f.Token} />
        <password{#Password} class="mlPassword" value={f.Password} id={i}
          placeholder="Password" />
        <submit value={"Set password"} class="greenButton"
          action={newPassword_}/>
      </form>
      {autoFocus i}
    </xml>

and password_reset token =
    if token = "" then
      passwordResetForm { Login = "" } ""
    else
      newPassword { Token = token, Password = "" } ""

fun change_email_ f =
    h <- getHost;
    e <- H.verifyChangeEmailToken h f.Token;
    case e of
      | Some email =>
        newSessionAndDelayedRedirect accountUrl email "Account email changed"
            <xml>Your account email is set to {emailX email}</xml>
      | None =>
        infoPage "Account email confirmation error" <xml>
          <p>Sorry, this email change link is no longer valid.
            Please, try to change email again in Account settings.
          </p>
        </xml>

fun change_email token =
    infoPage "Account email change" <xml>
      <form class="loginForm">
        <hidden{#Token} value={token} />
        <submit value={"Click here to change your email"}
          class="greenButton" action={change_email_}/>
      </form>
    </xml>

(* очень похоже на newPassword, но текст другой *)
fun restore_access_ f =
    if f.Password = "" then
      restore_access' f "Password can’t be empty."
    else
      h <- getHost;
      e <- H.verifyRestoreAccessToken h f.Token f.Password;
      case e of
        | Some email =>
          newSessionAndDelayedRedirect accountUrl email "Restore access"
            (txt "Access to your account was restored")
        | None =>
          infoPage "Restore access failed" <xml>
            <p>Sorry, that access restore link is no longer valid.
              Please, mail {supportEmail} to restore access to your account.</p>
          </xml>

and restore_access' f reason =
    i <- fresh;
    infoPage "Restore access" <xml>
      <p class="errorMessage">{[reason]}</p>
      <p>You need to change your password since it could be compromised. After that we will restore your account email, remove username and associated accounts (if any) and log out all other sessions so nobody could access your account except you.</p>
      <p>Choose a password</p>
      <form class="loginForm">
        <hidden{#Token} value={f.Token} />
        <password{#Password} class="mlPassword" value={f.Password} id={i}
          placeholder="Password" />
        <submit value={"Change password and restore access"} class="greenButton"
          action={restore_access_}/>
      </form>
      {autoFocus i}
    </xml>

and restore_access token =
    restore_access' { Token = token, Password = "" } ""

fun openIdLoginRedirect la h =
    if h.URL = "" then
      login_with (OpenId { URL = "" }) la
    else
      secure <- secureCookie;
      setCookie openIdURL {Value = h.URL, Expires = None, Secure = secure};
      H.logT ("OpenID URL: " ^ h.URL);
      loginRedirect (OpenId h) la

and login_with t a =
    case t of
      | OpenId { URL = "" } =>
        i <- fresh;
        c <- getCookie openIdURL;
        infoPage "Log in with OpenID" <xml>
          <p>Enter your OpenID URL</p>
          <form class={Css.loginForm}>
            <textbox{#URL} class={Css.openIdUrl} id={i} value={Option.get "" c}
              placeholder="https://..." />
            <submit value={"Log in"} action={openIdLoginRedirect a}/>
          </form>
          {autoFocus i}
        </xml>
      | _ =>
        loginRedirect t a

fun clearSubscriptionsHandler x =
    Rpcs.withUser "clearSubscriptionsHandler" (fn _ => return ())
             (BGClearAllSubscriptions :: []);
    redirectToMain

val clearSubscriptions : transaction page =
    infoPage "Clear subscriptions" <xml>
      <p>Are you really sure you want to unsubscribe from all your subscriptions?</p>
      <p>This operation can NOT be undone!</p>
      <form>
        <submit value={"Clear subscriptions"} class="redButton"
                action={clearSubscriptionsHandler}/>
      </form>
      <br/>
    </xml>

fun deleteAccountHandler f =
    Rpcs.withUser "deleteAccuntHandler" (H.deleteAccount True) [];
    x <- getUser ""; (* чтобы cookie удалить *)
    infoPage "Account deleted" <xml>
      <p>Your account was deleted. Come back any time. We are always happy to see you again!</p>
    </xml>

val deleteAccount : transaction page =
    c <- fresh;
    infoPage "Delete account" <xml>
      <p>Are you really sure you want to delete your subscriptions, starred and tagged articles, payments, settings, filters, smart streams and public feeds?</p>
      <p>This operation can NOT be undone!</p>
      <form>
(*         <label for={c}<checkbox{#ImSure} id={c}/>I’m really sure</label> *)
        <submit value={"Delete account"} class="redButton"
                action={deleteAccountHandler}/>
      </form>
      <br/>
    </xml>

    (* Взято из
     https://www.2checkout.com/blog/2checkout-blog/sample-privacy-policy-and-refund-policy/ *)
val refundPolicyText : xbody =
    <xml>
      <p>If you are not 100% satisfied with your purchase,
      within 30 days from the purchase date,
      we will fully refund the cost of your order.</p>
    </xml>

val bazQuxReader = noHyphens (txt "BazQux Reader")

val privacyPolicyText : xbody =
    let fun l w u = hrefLinkStopPropagation (txt w) u in
    <xml>
      <p> (* This policy covers how we use your personal information. *)
      We take your privacy seriously at {bazQuxReader} and we protect your personal information.</p>
      <p>Any personal information received will only be used to fill your order.
      We will not sell or redistribute your information to anyone. In fact we do not even keep order information on our servers, only your order ID.</p>
      <p>The only information we know about you is your email address (including when you log in with Facebook or Google), your subscriptions, filters and smart streams list, list of starred and tagged articles and service usage statistics.</p>
      <p>To understand how service is used and to improve it we collect usage statistics without personal details (e.g., which share/save service was used but not for which post, click on “Mark all as read”, creation of filters, changing view modes but without details what feeds or filters was changed, just the fact that buttons were clicked).</p>
      <p>To keep you authorized we keep authorization (session) cookie.</p>
      <p>For debug purposes we keep webserver logs which contain your IP address for a week. After that all logs are permanently removed.</p>
      <p>Our main servers are located in Germany (Hetzner). There are few servers in USA (Linode) used as proxy (for sites that require GDPR consent even for feeds, sites that return German versions even if we asking for English ones and for sites that block access from Hetzner servers) and for backups.</p>
      <p>We don’t use any external analytics service like Google Analytics and trying to further reduce tracking by using image proxy and loading videos only when you play them. External services like YouTube, Facebook, SoundCloud, etc. which host videos or other &lt;iframe&gt;-content may track you. Please, refer to corresponding service’s privacy policy.</p>
      <p>External APIs used:
        <ul>
          <li>Google OAuth for “Log in with Google” ({l "Google Privacy Policy" "https://www.google.com/policies/privacy"});</li>
          <li>Facebook OAuth for “Log in with Facebook” ({l "Facebook Privacy Policy" "https://www.facebook.com/about/privacy"});</li>
          <li>Twitter OAuth for “Log in with Twitter” ({l "Twitter Privacy Policy" "https://twitter.com/privacy"});</li>
          <li>Facebook Public Page API for fetching Facebook public pages (your access token will be used to speed up fetches if you logged in with Facebook so Facebook may track your Facebook feeds);</li>
          <li>Twitter API for fetching Twitter accounts, lists and searches (your access token will be used to speed up fetches if you logged in with Twitter so Twitter may track your Twitter feeds);</li>
          <li>Reddit API for fetching subreddits and comments (no authorization, Reddit can’t track your feeds);</li>
          <li>VK API for fetching VK walls, groups and topics (no authorization, VK can’t track your feeds).</li>
          <li>{l "YouTube API Services" "https://developers.google.com/youtube/terms/developer-policies#definition-youtube-api-services"} for fetching YouTube feeds and video descriptions (no authorization, no {l "authorized data" "https://developers.google.com/youtube/terms/developer-policies#definition-authorized-data"} is used, YouTube can’t track your feeds but can track what you watch; by subscribing to YouTube feeds or watching YouTube video you’re agreeing to {l "YouTube’s Terms of Service" "https://www.youtube.com/t/terms"});</li>
          <li>Vimeo oEmbed API for fetching Vimeo video descriptions (Vimeo won’t track you till you start watch).</li>
        </ul>
      </p>
      <p>At any moment you can <a link={opml}>export</a> your feeds in OPML-format or <a link={deleteAccount}>opt out</a> from our service.</p>
    </xml>
    end

val privacy =
    infoPage "Privacy policy" <xml>
      {privacyPolicyText}
    </xml>

val refund =
    infoPage "Refund policy" <xml>
      {refundPolicyText}
    </xml>

fun qaXX (q : xbody) (a : xbody) : xbody =
     <xml>
      (* <div class="qa">Q.</div> *)<div class="question">{q}</div>
      (* <div class="qa">A.</div> *)<div class="answer">{a}</div>
     </xml>
fun qaX q a = qaXX (txt q) a
fun qa  q a = qaXX (txt q) (txt a)

val apps =
    let val g' = spanClass Css.gray
        val g = g' <<< txt
        fun l name link = hrefLink (txt name) link
        fun list t l = <xml>
          <h3>{[t]}</h3>
          <p>
            <ul>
              {List.mapX (fn i => <xml><li>{i}</li></xml>) l}
            </ul>
          </p>
        </xml>
    in
    infoPage "Mobile and desktop apps" <xml>
      {list "iOS & macOS"
        (   l "Reeder" "https://reederapp.com/"
         :: <xml>{
            l "ReadKit" "https://readkit.app"} {g "(via Fever on macOS)"}</xml>
         :: l "Fiery Feeds" "http://cocoacake.net/apps/fiery/"
            (* https://itunes.apple.com/us/app/fiery-feeds/id1158763303 *)
         :: l "Lire" "https://lireapp.com/"
            (* https://itunes.apple.com/app/lire/id550441545 *)
         :: [])}

      {list "iOS"
        (   l "Feeddler" "https://itunes.apple.com/app/feeddler-rss-reader-pro-2/id919056339"
           (* "http://www.chebinliu.com/projects/iphone/feeddler-rss-reader/"
               ссылка на приложение битая *)
         :: <xml>{
            l "Unread" "https://www.goldenhillsoftware.com/unread/"}
            {g "(via Fever)"}</xml>
         :: l "Slow Feeds" "https://zoziapps.ch/slowfeeds/"
         :: l "Web Subscriber" "https://zoziapps.ch/web-subscriber/"
         :: [])}

      {list "macOS"
        (   l "NetNewsWire" "https://netnewswire.com/"
         :: l "Vienna RSS" "https://www.vienna-rss.com"
         :: [])}

      {list "Android"
        (   l "FeedMe" "https://play.google.com/store/apps/details?id=com.seazon.feedme"
         :: l "FocusReader" "https://play.google.com/store/apps/details?id=allen.town.focus.reader"
         :: l "Gravity" "https://play.google.com/store/apps/details?id=de.mobileways.gravity"
         :: l "NewsJet" "https://play.google.com/store/apps/details?id=mobi.newsjet.rss"
(*          :: l "Press" "https://play.google.com/store/apps/details?id=com.twentyfivesquares.press" *)
         (* ^ давно не обновлялся *)
         :: [])}

      {list "Windows"
        (<xml>{l "Fluent Reader" "https://hyliu.me/fluent-reader/"} {g "(via Fever)"}</xml>
         :: [])}

      {list "Text console (Linux, BSD, …)"
        (<xml>{l "Newsboat" "https://newsboat.org/"}
          {g' <xml>(see {hrefLink (txt "how to")
            "https://newsboat.org/releases/2.20/docs/newsboat.html#_bazqux"
            })</xml>}</xml>
         :: [])}

      <h3>Fever API</h3>
      <p>
        Apps that support Fever API (like Unread, ReadKit for macOS and Fluent Reader) can be used by setting {noHyphens (txt "bazqux.com")} as a Fever server (see {hrefLink (txt "how to") "https://blog.bazqux.com/2013/09/reeder-press-and-readkit-via-fever-api.html"}).
      </p>

      <h3>API</h3>
      <p>{bazQuxReader} supports both Google Reader and Fever APIs.</p>
      <p>Please, tell developers of your favorite client app to add support
      for {bazQuxReader}!</p>
      <p>API documentation is {Settings.apiLink (txt "here")}.</p>
    </xml>
    end


val searchHints =
(*     let fun h' q e = <xml>{q} &mdash; {[e]}<br/></xml> *)
    let fun h' q e = divClass Css.searchHint <xml>
      <div class="searchHintQuery">{q}</div>
      <div class="searchHintExplanation">{[e]}</div>
    </xml>
        fun h q e = h' (txt q) e
    in
    <xml><p>
      {h "subject:\"bazqux reader\""
         "articles with subjects containing phrase “bazqux reader”"}
      {h "author:john"
         "filter by specific author"}
      {h "tag:interesting"
         "filter by tag"}
      {h "img:true"
         "articles with images"}
      {h "gif:true"
         "articles with GIF images"}
      {h "audio:true\nvideo:true"
         "articles containing audio or video"}
      {h "link:bazqux.com"
         "articles containing specified link"}
      {h "item_link:example.com/*"
         "articles linking to specified URL"}
      {h "*"
         "search for everything (to mute feed with hide-filter)"}
      {h "baz*q?x"
         "wildcard searches (leading wildcards are not allowed)"}
      {h "comment:true\ncomment:false"
         "show comments or articles only"}
      {h "bazqux OR rss OR reader"
         "match any of keywords"}
      {h "bazqux AND rss AND reader"
         "match all keywords"}
      {h "NOT google"
         "match anything except keyword"}
      {h "(bazqux OR \"rss reader\") AND NOT google"
         "combine queries with parens"}
      {h "(bazqux || \"rss reader\") && -google"
         "if you prefer this syntax"}
      {h "time:[* now-1d]"
         "articles older than a day"}
      {h "time:[now-1w *]"
         "articles from the last week (use “M” for months, “y” for years, “h” for hours)"}
      </p>
      <p>
      Punctuation is currently ignored. So “[sponsor]” works the same way as “sponsor”.
      </p>
      <p>
      To search in Chinese (and other writing systems that aren’t based on alphabet) you may need to put your keywords in double quotes ({noHyphens <xml>"keyword1" "keyword2"</xml>}) to search for keywords only and not for all words with symbols from your keywords.
      </p>
    </xml>
    end

val search_hints =
    infoPage "Search hints" searchHints

val how_to_import_my_feeds =
    let fun l name link =
            <xml><li>{[name]} &mdash; {hrefLink (txt (stripHttpWww link)) link}</li></xml>
    in
    infoPage "How to import my feeds?" <xml>
      <p>You need to download OPML file (your subscriptions list)
        from your current feed reader and upload it to BazQux Reader
        (click {buttonName "Add subscription"} ⇒ {buttonName "Import OPML"}).
      </p>

      <p>Here are the OPML file download links for some readers:
        <ul>
          {l "Feedly" "https://feedly.com/i/opml"}
          {l "InoReader" "https://www.inoreader.com/reader/subscriptions/export?download=1"}
          {l "The Old Reader" "https://theoldreader.com/reader/subscriptions/export"}
          {l "Feedbin" "https://feedbin.me/subscriptions.xml"}
          {l "NewsBlur" "http://newsblur.com/import/opml_export"}
        </ul>
      </p>

      <p>And of course you can upload your Google Reader Takeout.zip file.</p>

      <p>To export feeds from BazQux Reader click settings icon
        ⇒ {buttonName "Subscriptions"} ⇒ {buttonName "Export OPML"}
        or click {hrefLink (txt "bazqux.com/opml") "/opml"}.
      </p>
    </xml>
    end

val subscribeBookmarklet = "javascript:{void(window.open('https://bazqux.com/add?url='+encodeURIComponent(location.href)));}"

val blogBazQuxFeed = "https://blog.bazqux.com/feed"

fun nobr (x : string) : xbody = <xml><span class={Css.nobr}>{[x]}</span></xml>

fun faqText (link : xbody -> string -> xbody) =
    <xml>
      {qaX "How to import my feeds?"
           <xml>Read our knowledge base {link <xml>article</xml> (show (url how_to_import_my_feeds))}.</xml>}

      {qaX "Are there any mobile apps?"
          <xml><p>{bazQuxReader} has wonderful, best in class {link <xml>mobile web interface</xml> "https://blog.bazqux.com/2018/07/mobile-web-interface.html"}. If you still prefer app (or want offline access) choose app in our {link (txt "apps") (show (url apps))} page.</p></xml>}

      {qa "How do I log in from 3rd party client app?"
          "Go to Settings (icon in the top right corner) ⇒ Account, and set your username and password."}

      {qa "How do I open original article when there is no subject?"
          "Click on the article time."}

(*       {qa "How do I hide comments?" *)
(*           "There are view mode buttons above the articles list. All except first are with comments collapsed. You can also press “2”."} *)

      {qa "How do I turn off smooth scrolling?"
          "Settings (icon in the top right corner) ⇒ Transitions ⇒ Immediate."}

      {qa "How to unsubscribe from feed?"
          "Select feed and choose Unsubscribe in the drop-down menu above the articles list or use right-click menu."}

      {qa "How do I assign a feed to folder?"
          "Select the feed and choose Folders ⇒ Add in the drop-down menu above the articles list. Or use right-click menu or drag and drop."}

      {qa "How long do you keep articles unread?"
          "Unread articles are kept forever. Although there is a limit of 500 total (both read and unread) articles per feed."}

      {qaX "How often feeds are updated?"
           <xml>Refresh rate depends on the time of the last article in feed: few hours ago—each {nobr "15–45 minutes"}, less than a week—each {nobr "1–2 hours"}, less than a month—each {nobr "2–3 hours"} and {nobr "3+ hours"} for others. Feeds that support {link (txt "PubSubHubbub") "https://en.wikipedia.org/wiki/PubSubHubbub"} are updated in realtime.</xml>}

      {qa "Mobile app suddenly stopped working. What happened?"
          "Perhaps your year subscription has expired. Login to the website to check."}

      {qaX "I’ve logged in and see no feeds. How to restore them?"
           <xml>Perhaps you’ve logged into wrong account (check the {link (txt "who am I") "/whoami"} page). Try another provider (Twitter/FB/Google) or log in with username and password if you’ve set them up.</xml>}

      {qaX "How to remove replies and retweets from Twitter feed?"
           <xml>Subscribe to {noHyphens <xml>twitter.com/&lt;account&gt;<wbr/>?exclude_replies<wbr/>&amp;exclude_retweets</xml>}</xml>}

      {qaX "Do you support password protected feeds?"
           <xml>Yes, feeds with HTTP {link (txt "basic access authentication") "https://en.wikipedia.org/wiki/Basic_access_authentication"} are supported.</xml>}

      {qa "How many feeds I can subscribe to?"
          "You can subscribe or import up to 3000 feeds."}

      {qa "How many filters or smart streams I can create?"
          "You can create up to 500 filters or smart streams combined."}

      {qaX "Is there an URL to add subscription?"
       <xml>Yes, use {noHyphens (txt "https://bazqux.com/add?url=%s")} in the RSS Subscription Extension for Chrome, or use {link (txt "SubToMe") "https://www.subtome.com/"}. Or drag the {link (txt "Subscribe") subscribeBookmarklet} bookmarklet to your bookmarks bar.</xml>}

      {qa "Can I export my feeds?"
          "Yes, you can both upload and download OPML to add or backup your feeds."}

      {qaX "Could you tell more about image proxy?"
       <xml>Yes, {link (txt "read this post") "https://blog.bazqux.com/2019/06/themes-typography-image-proxy.html#image-proxy"} in our blog.</xml>}

      {qaX "Is there a place to vote about new features?"
           <xml>Yes. Visit our {link (txt "UserVoice") "https://bazqux.uservoice.com/"}.</xml>}

      {qaXX <xml>Who are behind the {bazQuxReader}?</xml>
           <xml>{bazQuxReader} is a {link (txt "one man") "https://www.linkedin.com/in/vshabanov/"} project.</xml>}

      {qa "Will you add free accounts?"
          "No. I need the money to guarantee a continued service. I don’t want to close down like the free readers from Google/AOL/Digg done."}

      {qaXX <xml>What does {noHyphens (txt "“BazQux”")} mean?</xml>
       <xml>Just {link (txt "nothing") "https://en.wikipedia.org/wiki/Metasyntactic_variable#English"}.</xml>}

      {qaX "How do you pronounce “Qux”?"
       <xml>Something like “cooks”. Although hackers often pronounce “qux” as “kwucks”. Didn’t know that when I picked the name ;) Perhaps “quicks” is the best option.</xml>}

      {qa "Free trial length?"
          "30 days."}

      {qa "Pricing?"
          "You can pay $30 or $50 annually, or $249 for a lifetime subscription. Depending on your country taxation rules you may need to pay VAT/GST above the selected price."}

      {qa "What’s the difference between prices?"
          "No difference. It’s a way to support developer more if you like the service."}

      {qaX "How can I pay?"
          <xml>Credit card, {noHyphens <xml>PayPal or Amazon Payments</xml>} through our authorized reseller {noHyphens <xml>FastSpring.com</xml>}. You can look what payment methods are supported in your country {link (txt "here") "https://fastspring.com/docs/payment-methods-accepted-by-fastspring"}.</xml>}

      {qaX "Are refunds possible?"
          <xml>Yes, we have a {link (txt "refund policy") (show (url refund))}.</xml>}

      {qaX "Is it possible to delete account?"
          <xml>Yes, at any time you can {link (txt "delete") (show (url deleteAccount))} your account.</xml>}

      {qa "Can I post comments directly from Reader?"
          "No. Posting is not implemented and is not planned in the near future."}

(*       {qa "Why not all articles are shown in some Facebook pages?" *)
(*           "Unfortunately, Facebook Graph API returns all articles only on public (official) pages, not the personal ones."} *)

      {qaXX <xml>Is {bazQuxReader} really written in Haskell and Ur/Web?</xml>
          (txt "Yes. I love to use the best tools available.")}

      {qaX "I have another question!"
          <xml>Don’t hesitate to ask by email {link (noHyphens (txt "hello@bazqux.com")) "mailto:hello@bazqux.com"}</xml>}
    </xml>

fun helpText addSub =
    let fun kb key descr : xbody =
            <xml><div class="kbShortcut">
              <span class="kbShortcutKey">{[key]}
              </span><span class="kbShortcutDescr">{[descr]}</span>
            </div></xml>
        fun l t u = hrefLinkStopPropagation (txt t) u
        fun h t = <xml><h3>{[t]}</h3></xml>
    in
    <xml>
      {h "Keyboard shortcuts"}
      <p>
      {kb "j or n" "next article"}
      {kb "k or p" "previous article"}
      {kb "space" "next article or page"}
      {kb "shift + space" "previous article or page"}
      {kb "g or ; or ]" "get full text of the current article or the link in social media feeds (Twitter, Reddit, Facebook, Telegram and VK) using Five Filters script"}
      {kb "m" "mark article as read/unread"}
      {kb "shift + a" "mark all as read"}
      {kb "shift + q" "mark above as read"}
      {kb "shift + z" "mark below as read"}
      {kb "shift + click" "mark list/magazine/mosaic view article as read/unread"}
      {kb "s" "star/unstar article"}
      {kb "t" "edit article tags"}
      {kb "u" "parent article or scroll to article top"}
      {kb "x" "ignore article (do not show new comments)"}
      {kb "i" "skip current article and all its comments"}
      {kb "shift + i" "skip current comment and all its replies"}
      {kb "l" "mark article as unread and go to the next article (closing comments if expanded)"}
      {kb "shift + l" "mark current comment as unread go the next comment or article"}
      {kb "o" "expand/collapse comments"}
      {kb "enter" "expand/collapse article"}
      {kb "escape" "collapse article"}
      {kb "v" "open link"}
      {kb "b" "open link in background tab (Firefox/Chrome only)"}
      {kb "c" "open link in background tab, unstar article and move to next article"}
      {kb "shift + v" "view translation"}
      {kb "shift + b" "open translation in background tab (Firefox/Chrome only)"}
      {kb "e" "mail article link (via default mail agent)"}
      {kb "a" "add subscription"}
      {kb "r" "refresh subscriptions"}
      {kb "shift + j or n" "next subscription"}
      {kb "shift + k or p" "previous subscription"}
      {kb "shift + x" "expand folder"}
      {kb "shift + u" "select parent folder"}
      {kb "d then a" "display latest articles"}
      {kb "d then s" "display starred articles"}
      {kb "d then d" "open subscription selector"}
      {kb "d then u" "open feed selector"}
      {kb "d then f" "open folder selector"}
      {kb "d then t" "open tag selector"}
      {kb "/" "search"}
      {kb "- or =" "change article font size (saved per browser)"}
      {kb "_ or +" "change reader font size (saved per browser)"}
      {kb "f" "fullscreen"}
      {kb "1" "expanded view with expanded comments"}
      {kb "2" "expanded view"}
      {kb "3" "magazine view"}
      {kb "4" "mosaic view"}
      {kb "5" "list view"}
      {kb "0" "mixed view mode (in starred/tagged articles and smart streams)"}
      {kb "w" "edit filters and smart streams"}
(*       {kb "shift + t" "edit appearance (colors, fonts, etc.)"} *)
      {kb "h" "help"}
      </p>
      {h "Mouse hints"}
      <p>
      <b>right click</b> on subscription to show the context menu<br/>
      <b>middle click</b> or <b>ctrl + click</b> (<b>cmd + click</b> in macOS) on header in list view or on image in magazine/mosaic view to open article in background tab and mark it as read (Firefox/Chrome only)<br/>
      <b>shift + click</b> on header in list view or on image in magazine/mosaic view to mark article as read/unread<br/>
      <b>click</b> on the left side of the expanded article (list, magazine, mosaic view) to collapse it (aim slightly to the left of the text edge).
      </p>
      {h "Search hints"}
      {searchHints}
      {h "Resources"}
      <p>
      {l "Blog" "https://blog.bazqux.com"}
      {textButton "subscribe" (addSub blogBazQuxFeed)}<br/>
      {l "Community" "https://discourse.bazqux.com"}
      {textButton "subscribe" (addSub "https://discourse.bazqux.com/posts.rss")}<br/>
      {l "Twitter" "https://twitter.com/BazQuxReader"}
      {textButton "subscribe" (addSub "https://twitter.com/BazQuxReader")}
      {l "follow" "https://twitter.com/intent/user?screen_name=BazQuxReader"}<br/>
      {l "Facebook page" "https://www.facebook.com/BazQuxReader"}
      {textButton "subscribe" (addSub "https://www.facebook.com/414051538639341")}<br/>
      {l "UserVoice" "https://bazqux.uservoice.com"} for feature requests.<br/>
      {l "hello@bazqux.com" "mailto:hello@bazqux.com"} for any questions.
      </p><p>
      {l "Chrome extension" "https://chrome.google.com/webstore/detail/bazqux-notifier/fgoenlfbfnofepaodjepdhkoepoogedb"} (displays number of unread articles)<br/>
      {l "Opera extension" "https://addons.opera.com/extensions/details/bazqux-notifier"} (displays number of unread articles)<br/>
      {l "Chrome app" "https://chrome.google.com/webstore/detail/bazqux-reader-your-rss-fe/ojgfbobcblfebofgadefjfggnlclddko"} (adds a button to your apps page)
      </p><p>
      {l "Subscribe" subscribeBookmarklet} bookmarklet (drag it to your bookmarks bar).<br/>
      {l "Feed Hawk" "https://www.goldenhillsoftware.com/feed-hawk/"} iOS app for subscribing to feeds from within Safari.<br/>
      {l "Full-Text RSS" "https://ftr.bazqux.com"}, {l "Feed Creator" "https://createfeed.bazqux.com"} and {l "PDF Newspaper" "https://pdf.bazqux.com"} services from Five Filters. These instances are hosted on {bazQuxReader} servers and are free for you.
      </p>
      {h "FAQ"}
      {faqText hrefLinkStopPropagation'}
    </xml>
    end

val faq =
    win <- isWindows;
    infoPage' Css.infoPage "FAQ" <xml>
      <link rel="preload" as="font" type="font/woff2"
        href={bless (if win
          then "/images/fonts/5179514/58cdaee3-47e9-41f2-acfd-8a2333d1c960.woff2" (* ITC Charter italic *)
          else "/images/fonts/5687277/089fcb44-dfaf-40ec-96f6-f0aaefdedffc.latin.woff2" (* PT ITC Charter italic *))} />
    </xml>
    <xml>
      {faqText hrefLink'}
    </xml>

val fetcher =
    infoPage "Fetcher" <xml>
      <p>BazQux Fetcher is how BazQux Reader grabs RSS/Atom feeds and comments when users choose to subscribe to your blog in BazQux Reader. Fetcher collects and periodically refreshes these user-initiated feeds. Find answers below to some of the most commonly asked questions about how this user-controlled feeds and comments grabber works.</p>

      {qaX "How do I request that BazQux not retrieve some or all of my site’s feeds?"
           <xml>When users subscribe to your feed, BazQux fetcher attempts to obtain the content of the feed in order to display it. Since fetcher requests come from explicit action by human users, and not from automated crawlers, fetcher does not follow robots.txt guidelines.<br/>
             If your feed is publicly available, BazQux can’t restrict users from accessing it. One solution is to configure your site to serve a 404, 410, or other error status message to user-agent BazQux<br/>
             If your feed is provided by a blog or site hosting service, please, work directly with that service to restrict access to your feed.<br/></xml>}

      {qa "How often will Fetcher retrieve my feeds?"
          "Fetcher shouldn’t retrieve feeds from most sites more than once every hour on average. Some frequently updated sites may be refreshed more often. Note, however, that due to network delays, it’s possible that Fetcher may briefly appear to retrieve your feeds more frequently."}

      {qa "Why is Fetcher trying to download incorrect links from my server, or from a server that doesn’t exist?"
          "Fetcher retrieves feeds at the request of users who have subscribed to them in BazQux Reader. It is possible that a user has requested a feed URL location that does not exist."}

      {qa "Why is Fetcher downloading information from our “secret” web server?"
          "Fetcher retrieves feeds at the request of users who have added them to their BazQux. It is possible that the request came from a user who knows about your “secret” server or typed it in by mistake."}

      {qa "Why isn’t Fetcher obeying my robots.txt file?"
          "Fetcher retrieves feeds only after users have explicitly subscribed to them in BazQux Reader. Fetcher behaves as a direct agent of the human user, not as a robot, so it ignores robots.txt entries. Fetcher does have one special advantage, though: because it’s acting as the agent of multiple users, it conserves bandwidth by making requests for common feeds only once for all users."}

      {qa "Why are there hits from multiple machines at bazqux.com, all with user-agent BazQux?"
          "Fetcher was designed to be distributed on several machines to improve performance and scale as the web grows."}

      {qa "Can you tell me the IP addresses from which Fetcher makes requests so that I can filter my logs?"
          "The IP addresses used by Fetcher change from time to time. The best way to identify accesses by Fetcher is to use its identifiable user-agent: BazQux."}

      {qa "Why is Fetcher downloading the same page on my site multiple times?"
          "In general, Fetcher should only download one copy of each file from your site during a given feed retrieval. Very occasionally, the machines are stopped and restarted, which may cause it to again retrieve pages that it’s recently visited."}

      {qa "Do you support push technology?"
          "Yes. BazQux Reader support push hubs. If your feeds advertise a push hub, Fetcher will subscribe for updates and reduce the number of polls to three times a day."}

      {qa "My Fetcher question isn’t answered here. Where can I get more help?"
          "If you’re still having trouble, try posting your question to support at bazqux.com."}
    </xml>

val media_proxy =
    infoPage "Media proxy" <xml>
      <p>BazQux Reader Media Proxy is how BazQux Reader grabs article images, audio and video when users read your blog in BazQux Reader and have secure proxying enabled. It’s also used for showing article thumbnails, fetching site descrption and favicons. All this access is user-initiated from user browsers. Find answers below to some of the most commonly asked questions about how this user-controlled proxy works.</p>

      {qa "Why isn’t Media Proxy obeying my robots.txt file?"
          "Media Proxy makes requests only when user’s browser tries to download media from your site. Media Proxy behaves as a direct agent of the human user, not as a robot, so it ignores robots.txt entries (as web browsers do). Media Proxy does have one special advantage, though: because it’s acting as the agent of multiple users, it conserves bandwidth by making requests for common resources only once for all users."}

      {qaX "How do I request that Media Proxy not retrieve some or all of my site’s resources?"
           <xml>When users read your blog or your page linked from blog, BazQux proxy attempts to obtain the content of the page in order to display it. Since proxy requests come from explicit action by human users, and not from automated crawlers, proxy does not follow robots.txt guidelines.<br/>
             If your site is publicly available, BazQux can’t restrict users from accessing it. One solution is to configure your site to serve a 404, 410, or other error status message to user-agent BazQuxReaderMediaProxy<br/>
             If your site is provided by a blog or site hosting service, please, work directly with that service to restrict access to your feed.<br/></xml>}

      {qa "How often will Media Proxy access my site resources?"
          "Each time users read your article in BazQux Reader but not more often that once an hour for each resource (and even less frequent if you have set up Expires/Cache-Control HTTP headers)."} (*  In general proxy will lessen load to your site since instead of direct download from all readers of your blog it makes single request and serves cached version by itself. *)

      {qa "Why is Media Proxy trying to download incorrect links from my server, or from a server that doesn’t exist?"
          "Media Proxy retrieves resources as described in articles from blogs or social media which can contain mistakes."}

      {qa "Why is Media Proxy downloading information from our “secret” web server?"
          "BazQux Reader retrieves feeds at the request of users who have added them to their BazQux. It is possible that the request came from a user who knows about your “secret” server or typed it in by mistake."}

      {qa "Why are there hits from multiple machines at bazqux.com, all with user-agent BazQuxReaderMediaProxy?"
          "Media Proxy was designed to be distributed on several machines to improve performance and scale as the web grows."}

      {qa "Can you tell me the IP addresses from which Media Proxy makes requests so that I can filter my logs?"
          "The IP addresses used by Media Proxy change from time to time. The best way to identify accesses by Media Proxy is to use its identifiable user-agent: BazQuxReaderMediaProxy."}

      {qa "Why is Media Proxy downloading the same resource on my site multiple times?"
          "In general, Media Proxy should only download one copy of each file from your site and cache it for an hour (or more as specified in your site configuration). Very occasionally, the machines are stopped and restarted, which may cause it to again retrieve resources that it’s recently visited."}

      {qa "My Media Proxy question isn’t answered here. Where can I get more help?"
          "If you’re still having trouble, try posting your question to support at bazqux.com."}
    </xml>

val png = ".png"
val webp = ".webp"

fun screenshotUrl' dark ext v =
    "/images/screenshot" ^ (if dark then "-night" else "")
    ^ ".v" ^ show v ^ ext
fun screenshotUrl dark ext = screenshotUrl' dark ext 7

(* Описания для распространения в соцсетях,
   нужны только в незалогиненных страницах *)
fun htmlHeadSocial (hostName : string) time : xhead =
    let val name = "BazQux Reader"
        val url = "https://" ^ hostName
        val screenshot = url ^ screenshotUrl' False png (toSeconds time)
        (* twitter не любит, когда на одну и ту же картинку ссылаются с разных
           страниц, так что добавляем постоянно меняющуюся версию
           А также twitter предпочитает og:image вместо twitter:img,
           (хотя качает оба), так что меняем все возможные ссылки на скриншот
         *)
        val description = "Your friend for reading feeds."
    in
    <xml>
      <meta name="description" content={description} />

      <meta itemprop="name" content={name} />
      <meta itemprop="image" content={screenshot} />
      <meta itemprop="description" content={description} />

      <meta property="fb:app_id" content="468204013193852" />
      <meta property="og:type" content="website"/>
      <meta property="og:url" content={url} />
      <meta property="og:title" content={name}/>
      <meta property="og:image" content={screenshot} />
      <meta property="og:site_name" content={name}/>
      <link rel="image_src" href={bless screenshot} />
      <meta property="og:description" content={description} />

      <meta property="twitter:card" content="summary_large_image" />
      <meta property="twitter:url" content={url} />
      <meta property="twitter:site" content="@BazQuxReader" />
      <meta property="twitter:title" content={name} />
      <meta property="twitter:description" content={description} />
      <meta property="twitter:image" content={screenshot} />
      (* property, а не name, т.к. Ur/Web не позволяет двоеточия в name (src/settings.sml checkMeta)
       *)
    </xml>
    end

val htmlHeadMain : xhead =
    <xml>
      <link href="/images/apple-touch-icon.png" rel="apple-touch-icon" sizes="180x180" />
      <link rel="mask-icon" href="/images/safari-pinned-tab.svg" color="#3aafdd" />

      <meta name="application-name" content="BazQux Reader" />
      <meta name="apple-mobile-web-app-title" content="BazQux Reader" />
      <meta name="apple-mobile-web-app-capable" content="yes" />
      <meta name="apple-mobile-web-app-status-bar-style" content="default" />

      <link rel="chrome-webstore-item" href="https://chrome.google.com/webstore/detail/ojgfbobcblfebofgadefjfggnlclddko" /> (* app icon *)
      <link rel="chrome-webstore-item" href="https://chrome.google.com/webstore/detail/fgoenlfbfnofepaodjepdhkoepoogedb" /> (* notifier *)
      <link rel="chrome-webstore-item" href="https://chrome.google.com/webstore/detail/dfoegpibjpjpchgmjnmomelfnclbijnm" /> (* open links in background *)

      <link rel="yandex-tableau-widget" href="/web/yandex-tableau.json" />
      <link rel="manifest" href="/web/manifest.json" />
      <meta name="msapplication-config" content="/web/browserconfig.xml" />
      <meta name="referrer" content="origin" />
      (* ^ отправляет bazqux.com без #fragment как referrer при переходе на не-HTTPS сайты *)
    </xml>

fun loginButton cls name t la =
    <xml><a class={classes Css.logInWith cls} link={login_with t la}
      >(* Log in with *)<span class="loginProvider">{[name]}</span></a></xml>

fun externalLoginButtons loginAction =
    <xml><div class={Css.logInWithButtons}>
      {loginButton Css.google "Google" Google loginAction}
      {loginButton Css.facebook "Facebook" Facebook loginAction}
      {loginButton Css.twitter "Twitter" Twitter loginAction}
      {loginButton Css.openId "OpenID" (OpenId {URL = ""}) loginAction}
    </div></xml>

fun _signUp change resend f =
    let fun retry f er =
            if change then
                signUpChangeEmail er f
            else
                loginUI f (Some er) False ELALogin
    in
    validateEmail (retry f) f.Login (fn email =>
    if f.Password = "" then
        retry (setF [#Login] email f) "Password can’t be empty"
    else
        let val lt = LTEmail { Email = email } in
        u <- H.getUserByLogin lt None;
        case u of
          | Some u =>
            retry (setF [#Login] email f) ("This email is busy. Try to log in or try another email.")
          | None =>
            h <- getHost;
            sent <- H.sendSignUpEmail h email f.Password;
            if sent then
              if resend then
                infoPage "Sign up" <xml>
                  <p>We sent another activation mail to {emailX email}.
                    It might take a few minutes for it to arrive;
                    be sure to check your spam folder.
                  </p>
                </xml>
              else
                infoPage "Sign up" <xml>
                  <p>You’re almost done!
                    We sent an activation mail to {emailX email}.
                    Please, follow the instructions in the mail
                    to activate your account.
                  </p>
                  <p>If it doesn’t arrive, check your spam folder.</p>

                  <div class="buttonForms">
                    <form>
                      <hidden{#Login} value={email} />
                      <hidden{#Password} value={f.Password} />
                      <submit value={"Resend activation email"}
                         class="greenButton"
                         action={_signUp False True}/>
                    </form><form>
                      <hidden{#Login} value={email} />
                      <hidden{#Password} value={f.Password} />
                      <submit value={"Change email address"}
                         action={signUpChangeEmail ""}/>
                    </form>
                  </div>
                </xml>
            else
              infoPage "Sign up" <xml>
                <p>We already send several activation mails to
                  {emailX email}.
                  It might take a few minutes for it to arrive;
                  be sure to check your spam folder.
                </p>
              </xml>
        end)
    end
and signUpChangeEmail reason f =
    i <- fresh;
    infoPage "Edit email" <xml>
      <p class="errorMessage">{[reason]}</p>
      <p>Provide a new address and we’ll resend your confirmation email.</p>
      <form class="loginForm">
        <textbox{#Login} value={f.Login} class="mlLogin" id={i} placeholder="Email" />
        <hidden{#Password} value={f.Password} />
        <submit value={"Update email address"} class="greenButton" action={_signUp True False}/>
      </form>
      {autoFocus i}
    </xml>

(* страница-заглушка, чтобы работал /_signUp,
   вызываемый через JavaScript с главной страницы *)
and signUp () =
    infoPage "Sign up" <xml>
      <form class="loginForm">
        <textbox{#Login} class="mlLogin" placeholder="Email" />
        <password{#Password} class="mlPassword" placeholder="Password" /><br/>
        <submit value={"Sign up"} class="greenButton"
          action={_signUp False False}/>
      </form>
    </xml>

and _login loginAction f =
    let val lt = LTEmail { Email = f.Login } in
        u <- H.getUserByLogin lt (Some f.Password);
        case u of
          | Some u =>
            handleLoginAction lt LATNone loginAction None
          | None =>
            loginUI f (Some ("Invalid "
                        ^ (if Option.isSome (H.validateEmail f.Login)
                           then "email" else "username")
                        ^ " or password.")) False loginAction
    end

and loginButtons f loginAction =
    <xml>
      <form class="loginForm">
        <textbox{#Login} class="mlLogin" placeholder="Email or username"
          value={f.Login} />
        <password{#Password} class="mlPassword" placeholder="Password"
          value={f.Password} />
        <div class="forgotPassword">
          <a link={password_reset ""}>I forgot my password</a>
        </div>
        <div class={Css.loginButtons}>
          <submit class="buttonLogIn" value={"Log in"} action={_login loginAction} />
        </div>
      </form>
      <div class="orLogInWith">
        or log in with
      </div>
      {externalLoginButtons loginAction}
    </xml>

and loginUI f loginError pleaseLogInAgain loginAction =
    logAction "main" "-" (
    r <- getHeader (blessRequestHeader "Referer");
    rc <- getCookie referrer;
    (case (r, rc) of
      | (Some r, None) =>
        setReferrer r
      | _ => return ());
    Hacks.clear_script_header;
    (* ^ дабы на главной странице не было скриптов *)
    host <- getHost;
    time <- now;
    landingPage ""
      <xml>
        {htmlHeadMain}
        {htmlHeadSocial host time}
        <link rel="alternate" type="application/rss+xml" title="BazQux Reader news" href={bless blogBazQuxFeed} />
      </xml> <xml>
      {case (pleaseLogInAgain, loginAction) of
         | (True, _) =>
           divClass Css.pleaseLogIn (txt "Please, log in again.")
         | (_, ELAAddUrl u) =>
           divClass Css.pleaseLogIn (txt "Please, log in to add subscription.")
         | (_, _) => <xml/>
      }
      <h2 class="motto">Your friend for reading feeds, <span class="price"><span class="dollarSign">$</span>30 a year</span></h2>
      (* после добавления цены стало примерно на 40% меньше логинов,
         ну и ладно *)
(*       <p class="motto">Fast, clean and unique feed reader</p> *)
(*       <p class="motto">Fast and clean feed reader, <span class="dollarSign">$</span>30 a year.</p> *)

      {case loginError of
        | Some e =>
          pClass Css.invalidLogin (txt e)
        | None =>
          <xml/>}
      <div class="landingButtonsAndScreenshot">
        <div class="loginPanel">
          <p class="startToday">Start free 30-day trial:</p>
          {loginButtons f loginAction}
        </div>
        <picture>
          {sourceTypeMediaSrcset "image/webp" "(prefers-color-scheme: light)"
            (screenshotUrl False webp)}
          {sourceTypeMediaSrcset "image/png" "(prefers-color-scheme: light)"
            (screenshotUrl False png)}
          {sourceTypeMediaSrcset "image/webp" "(prefers-color-scheme: dark)"
            (screenshotUrl True webp)}
          {sourceTypeMediaSrcset "image/png" "(prefers-color-scheme: dark)"
            (screenshotUrl True png)}
          <img class="screenshot" width={610} height={343}
            src={bless (screenshotUrl False png)} />
        </picture>
      </div>

      <div class="landingText">
      {let fun combine (ta, xa) (tb, xb) = (ta, <xml>{xa}
                <h3>{[tb]}</h3>
                {xb}
               </xml>)
           val column1 =
               ("One place to follow everything"
               ,<xml><p>
                 Follow the news from your favorite sites in one place.
(*                  Follow all the sites you love in one place. *)
(*                  Build your own feed without ads, algorithms and tracking. *)
                 Subscribe to sites instead of visiting them.
                 Read without ads and tracking.
(*                  No algorithms, no ads, privacy. *)
(*                  You’re client, not a product there. *)
                </p></xml>) ::

               ("Read Twitter, Facebook, Telegram and VK"
               ,<xml><p>
                 Monitor social media pages together with websites.
                 Just enter the page URL and follow it like a regular blog.
                </p></xml>) ::

               ("Full-text articles"
               ,<xml><p>
                 Read the full content of truncated articles
                 or social media links inside the reader.
                 (* {hrefLink (txt "Convert") "https://ftr.bazqux.com"} *)
                 Convert summary feeds to full-text ones.
                </p></xml>) ::

               ("Read the comments"
               ,<xml><p>
                 No need to leave the reader to check article comments.
                 They’re here. And like with articles you can follow new ones.
                </p></xml>) ::

               combine
               ("Efficiency"
               ,<xml><p>
                 Fast and clean interface keeps you focused on reading.
                 No ads, no superfluous buttons or borders,
                 raw speed and keyboard shortcuts&mdash;everything
                 is designed to save your time.
                </p></xml>)

               ("Search and filter"
               ,<xml><p>
                 Search in feeds or tagged articles.
                 Hide what you don’t like and monitor must reads
                 with {hrefLink (txt "filters and smart streams") "https://blog.bazqux.com/2014/04/filters-and-smart-streams.html"}.
                </p></xml>) ::

               ("Mixed view mode"
               ,<xml><p>
                 Choose optimal view for each feed:
                 list, mosaic, magazine, expanded.
                 Read feeds together in unique mixed view mode.
                </p></xml>) ::
               []
           val column2 =
               ("Themes and typography"
               ,<xml><p>
                 Choose your colors, fonts and layout.
                 Attention to {hrefLink (txt "typography") "https://blog.bazqux.com/2019/06/themes-typography-image-proxy.html"} makes your reading faster and more enjoyable.
                </p></xml>) ::

               ("Star, tag, share and read later"
               ,<xml><p>
                 Bookmark your favorite articles with stars or tags.
                 Share via email, Twitter or Facebook.
                 Save to Pocket, Instapaper, Evernote or Pinboard.
                </p></xml>) ::

               ("Mobile and desktop"
               ,<xml><p>
                 Enjoy both efficient desktop and marvelous
                 {hrefLink (txt "mobile") "https://blog.bazqux.com/2018/07/mobile-web-interface.html"} web interfaces.
                 Use {pageLink (txt "apps") apps} on iOS, macOS and Android when you need offline mode.
                </p></xml>) ::

               ("Secure"
               ,<xml><p>
                 Read your feeds without third party tracking
                 thanks to our secure {hrefLink (txt "image proxy") "https://blog.bazqux.com/2019/06/themes-typography-image-proxy.html#image-proxy"}.
                </p></xml>) ::

               ("Not free, not freemium"
               ,<xml><p>
                 Being paid service reader updates feeds of paying
                 and trial users only.
                 It doesn’t get rate limited by site owners
                 for fetching millions of free users feeds
                 and updates your feeds in time.
                 It designed for the needs of paying clients
                 not for the nice screenshots and reviews.
                 And you get real customer support here.
                 </p><p>
                 You’re client, not product.
                </p></xml>) ::

               ("Quick start"
               ,<xml><p>
                 Log in, find your favorite feeds or import OPML
                 and start reading!
                </p></xml>) ::
               []

       in <xml>
         <div class="landingTextOneColumn">
         {List.mapX
              (fn (t,c) => <xml>
                <h3>{[t]}</h3>
                {c}
              </xml>)
              (List.append column1 column2)}
         </div>
         {List.mapX (fn ((t1,c1), (t2,c2)) =>
            <xml>
              <div class="landingTextTwoColumns">
                <div class={Css.column1}>
                  <h3>{[t1]}</h3>
                  {c1}
                </div>
                <div class={Css.column2}>
                  <h3>{[t2]}</h3>
                  {c2}
                </div>
              </div>
            </xml>) (zip column1 column2)}
         </xml>
       end}
      </div> (* landingText *)
      <div class="loginPanelAtTheBottom">
        <p class="startToday">Start your free 30-day trial:</p>
        {loginButtons f loginAction}
      </div>
      <div class="mainFooter">
        <a href={bless "https://blog.bazqux.com"}>Blog</a> ·
        <a href={bless "https://discourse.bazqux.com"}>Community</a> ·
        <a href={bless "https://twitter.com/BazQuxReader"}>Twitter</a> ·
        <a href={bless "mailto:hello@bazqux.com"}>Contact</a> ·
        <a link={privacy}>Privacy</a> ·
        <a link={faq}>FAQ</a> ·
        {Settings.apiLink (txt "API")}
      </div>
    </xml>)

val please_log_in_again : transaction page =
    loginUI { Login = "", Password = "" } None True ELALogin

fun add (qs : option queryString) : transaction page =
    case qs of
        None => error <xml>No URL specified</xml>
      | Some q =>
        let val u = show q in
        (case H.parseQueryStringUtf8Only u of
           | ("url", url) :: [] =>
             if url = "" then
                 error <xml>Empty URL specified</xml>
             else
                 u <- getUser "";
                 (case u of
                    | None =>
                      logAction "add" "-"
                                (loginUI { Login = "", Password = "" }
                                         None False (ELAAddUrl { URL = url }))
                    | Some u =>
                      logAction "add" u (addUrlAndRedirect u url))
           | _ =>
             error <xml>No URL specified. Use https://bazqux.com/add?url={["{url}"]} format.</xml>
        )
        end

fun buyOpt r id price = <xml><label for={id}>
  <div class="buyOption">
    {r}
    <span class="buyOptionName"><span class="dollarSign">$</span>{[price]}</span>
  </div>
</label></xml>

fun buyText userId paidTill =
    o1 <- fresh;
    o2 <- fresh;
    o3 <- fresh;
    return <xml><span class={Css.buyText}>
         <p>{dyn_ (pt <- signal paidTill;
                   return (txt (case pt of
               | PTFreeTrialFinished _ =>
                 "We are very pleased to see that you decided to buy the subscription!"
               | PTPaid _ =>
                 if Payments.lifetime pt then
                 "Thank you for being a lifetime client! We are very pleased to see that you decided to donate even more to help."
                 else
                 "We are very pleased to see that you decided to continue the subscription!"
               | PTPaidFinished _ =>
                 "We are very pleased to see that you decided to continue the subscription!"
               | _ =>
                 "We are very pleased to see that you decided to finish free trial and buy the subscription!"
            )))}</p>
         <p>As a client you get first-priority support and help us improve {bazQuxReader}.</p>
         <p>Choose any price you want and enjoy:</p>
(*          <p>What option would you like to take?</p> *)
         <form>
           <radio{#Option}>
             {buyOpt <xml><radioOption value={"readeryear50"} id={o1} checked={True} /></xml> o1
               "50 a year if you like to help more!"}
             {buyOpt <xml><radioOption value={"readeryear30"} id={o2} /></xml> o2
               "30 a year"}
             {buyOpt <xml><radioOption value={"reader-lifetime249"} id={o3} /></xml> o3
               "249 lifetime subscription"}
           </radio>
           <submit action={Payments.buy userId} class="greenButton"
                   value={"Complete your purchase on FastSpring.com"} />
         </form>
(*          <p>There is no difference in service </p> *)
         <hr/>

         <h3>Refund policy</h3>
         {refundPolicyText}

         <h3>Contact information</h3>
         <p>We’re always happy to hear your questions at {supportEmail}.</p>

         <h3>Privacy policy</h3>
         {privacyPolicyText}
       </span></xml>
(*
https://bazqux.com/renew/example@gmail.com
https://bazqux.com/renew/twitter_id-740431896
nginx делает escape для пути,
а также сливает двойные слеши в один,
так что надо
https://bazqux.com/renew?openid=http%3A%2F%2Fvshabanov.livejournal.com
а у ur/web '.' -- это для escapeing, так что nginx меняет renew/ на renew?
а openid так и остается параметром
 *)
fun renew_subscription uid =
    t <- now;
    pt <- source (PTPaid { Till = t });
    bt <- buyText uid pt;
    infoPage "Subscription Renewal" <xml>
      {bt}
    </xml>
fun renew (qs : option queryString) =
    uid <- H.parseRenewalUserId (Option.get "" (Option.mp show qs));
    renew_subscription uid

fun check_order' pageTitle showProcessedTime oid : transaction page =
    pm <- H.checkOrder oid;
    pt <- H.getPaidTill pm.1;
    at <- H.getUserAccountTypeAndRenewalUrl pm.1;
    user0 <- getUser "";
    user <- (case user0 of
     | Some u =>
       if u <> pm.1 then
           sessionLogOut;
           (* если был залогинен под другим пользователем,
              чтобы ссылка back to reader вела куда надо *)
           return None
       else
           return user0
     | None => return user0);
    let fun ot t =
            <xml><span class="checkOrderTime">{formatTimeOnClient t}</span></xml>
    in
    landingPage' Css.infoPage pageTitle <xml/> <xml>
      <h2>Thank you for your purchase!</h2>
      <p>Your order
        {hrefLink (txt oid) (H.invoiceLink oid)}
        was successfully processed{
        displayIfC showProcessedTime <xml> at
          {case pm.2 of
            | PReserved => <xml/>
            | PFastSpring fs => ot fs.OrderTime
            }</xml>
        }.</p>
      {case pt of
        | PTPaid paid =>
          <xml><p>Your subscription is
            {displayIfNotC showProcessedTime (txt "now ")}active
            {if datetimeYear paid.Till > 2100 then
                 txt "forever! Thank you again for choosing a lifetime option!"
             else
                 <xml>till {ot paid.Till}.</xml>}
          </p>
          <p class="homeLink"><a href={bless "/"}>Back to the reader</a>
            {displayIfC (Option.isNone user)
                        <xml>(use your {[at.1]} to log in)</xml>}</p>
          </xml>
        | PTPaidFinished paid =>
          <xml><p>Your subscription was active till {ot paid.Till}.
          Perhaps it’s time to <a link={renew_subscription pm.1}>renew</a>?
          </p></xml>
(*         | PTFreeTrialFinished _ => <xml/> *)
(*         | PTFreeTrial _ => <xml/> *)
(*         | PTUnknown => <xml/> *)
        | _ => backToMain
        }
    </xml>
    end

fun check_order oid : transaction page = check_order' "Check Order" True oid

fun order_completed oid : transaction page = check_order' "Order Completed" False oid

fun authorizeAndAddToPocket (qs : option queryString) =
    Rpcs.withUser "authorizeAndAddToPocket"
    (fn u =>
        H.userAuthorizeAndAddToPocket u;
        landingPage' Css.infoPage "Added to Pocket" <xml/> <xml>
          <h2>Added to Pocket</h2>
          <p>You have authorized to Pocket, added an article and enabled one-click article saving.</p>
          <p>{textButton "Close" Js.windowClose}</p>
        </xml>) []
fun addToPocket link title =
    Rpcs.withUser "addToPocket"
    (fn u =>
        h <- getHost;
        H.userAddToPocket u h (effectfulUrl authorizeAndAddToPocket) link title)
    []

fun _switch_to_user f =
    adminOnly (
    let val lt = LTEmail { Email = f.Login } in
        u <- H.getUserByLogin lt None;
        case u of
          | Some u =>
            sessionLogOut;
            _ <- setNewSessionCookie lt LATNone None;
            redirectToMain
          | None =>
            error <xml>Invalid email or username</xml>
    end)

val switch_to_user =
    adminOnly (
    i <- fresh;
    infoPage "Switch to user" <xml>
      <p>Enter email or username</p>
      <form class={Css.loginForm}>
        <textbox{#Login} class="mlLogin" id={i}
          placeholder="Email or username" />
        <submit value={"Switch to user"} class="greenButton"
          action={_switch_to_user} />
      </form>
      {autoFocus i}
    </xml>)

fun r referral =
    setReferrer referral;
    redirectToMain
