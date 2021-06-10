open Utils

cookie sid : string
cookie sid_beta : string
cookie sid_local : string
cookie referrer : string

val getHost =
    h <- getHeader (blessRequestHeader "Host");
    return (case h of
      | None => "bazqux.com"
      | Some "www.bazqux.com" => "bazqux.com"
      | Some "www.beta.bazqux.com" => "beta.bazqux.com"
      | Some "www.local.bazqux.com" => "local.bazqux.com"
      | Some h => h)
fun rmWWW act =
    h <- getHeader (blessRequestHeader "Host");
    case bind h (stripPrefix "www.") of
      | Some d =>
        redirect (bless ("https://" ^ d))
      | None =>
        act
val isBeta =
    h <- getHost;
    return (h = "beta.bazqux.com")
val isLocal =
    h <- getHost;
    return (h = "local.bazqux.com")
val secureCookie =
    h <- getHeader (blessRequestHeader "Non-HTTPS-Connection");
    (* тестовые соединения по IP работают по HTTP и требуют не secure cookie *)
    return (case h of
      | Some "true" => False
      | _ => True)

val sessionCookie =
    b <- isBeta;
    l <- isLocal;
    return (if b then sid_beta else if l then sid_local else sid)

fun getUserBySession clear k =
    s <- (if k <> "" then H.cachedReadSession k else return None);
    sc <- sessionCookie;
    (case s of
       | Some s =>
         e <- H.isUserExists s.User;
(*          debug ("user " ^ s.User ^ "; exists = " ^ show e); *)
         if s.Cleared || not e then
             when clear (clearCookie sc); H.deleteSession s; return None
             (* вынести как ф-ю не получается -- не компилится *)
         else
             return (Some s.User)
       | _ =>
(*          debug ("no session " ^ k); *)
         when clear (clearCookie sc); return None)

fun getUser action =
(*     return (Some "1") *)
    sc <- sessionCookie;
    c <- getCookie sc;
    case c of
      | Some k =>
        u <- getUserBySession True k;
        (case u of
           | Some u =>
             when (action <> "subscriptions")
                  (* обновление подписок использованием не считаем *)
                  (ua <- getHeader (blessRequestHeader "User-Agent");
                   H.recordWebUsage u ua);
             return (Some u)
           | None => return None)
      | _ => return None

val isWindows =
    mbua <- getHeader (blessRequestHeader "User-Agent");
    case mbua of
      | Some ua =>
        return (Option.isSome (strsindex (H.toLowerCase ua) "win"))
      | None =>
        return False

val getUserIdBySession : transaction page =
    s <- getHeader (blessRequestHeader "Session");
    case s of
       | Some k =>
         fu <- H.tryGetFeverUser k;
         (case fu of
             | Some u =>
               returnBlob (textBlob u) (blessMime "text/plain")
             | None =>
               u <- getUserBySession False k;
               returnBlob (textBlob (Option.get "" u)) (blessMime "text/plain"))
       | _ =>
         error <xml>No Session header specified</xml>

val clearReferrer =
    httpRef <- getCookie referrer;
    withSome (fn _ => clearCookie referrer) httpRef;
    return httpRef

fun setReferrer r =
    secure <- secureCookie;
    t <- now;
    setCookie referrer
      {Value = r, Expires = Some (addSeconds t (180*86400)), Secure = secure}

fun setNewSessionCookie lt lat who =
    httpRef <- clearReferrer;
    params <-
        Monad.mp (List.append
                      (("LoginType", case lt of
                        | LTGoogle g => "Google " ^ g.Email
                        | LTFacebook f => "Facebook " ^ f.Email
                        | LTTwitter t =>
                          "Twitter " ^ t.Id ^ " @" ^ Option.get "-" who
                        | LTOpenId o => "OpenID " ^ o.URL
                        | LTEmail e => "Email " ^ e.Email
                        | LTUsername u => "Username " ^ u.Username
                        | LTFeverApiKey k => "FeverApiKey " ^ k.ApiKey) ::
                       ("Who", Option.get "-" who) ::
                       ("Referrer", Option.get "-" httpRef) :: []))
                 (List.mapM
                      (fn n =>
                          p <- getHeader (blessRequestHeader n);
                          return (n, Option.get "-" p))
                      ("Country" :: "User-Agent" :: []));
    s <- H.newSessionJunk lt lat params;
    sc <- sessionCookie;
    secure <- secureCookie;
    H.logT ("User authenticated " ^ s.User);
    setCookie sc {Value = s.Key, Expires = Some s.Expire, Secure = secure};
    return s.User

fun newSessionForEmail email =
    void (setNewSessionCookie (LTEmail { Email = email }) LATNone None)

cookie openIdURL : string

fun addUrlAndRedirect user url =
    h <- H.userSubscribe user url None [];
    redirect (bless ("/i/" ^ h))

val accountUrl = "/i/account"

fun handleLoginAction lt lat la who =
    case la of
      | ELALogin =>
        _ <- setNewSessionCookie lt lat who;
        redirectToMain
      | ELAAddUrl a =>
        uid <- setNewSessionCookie lt lat who;
        addUrlAndRedirect uid a.URL
      | ELAAddAssociatedAccount =>
        u <- getUser "callback";
        case u of
          | None => error (txt "Not logged in. Please, log in to add associated account.")
          | Some user =>
            h <- getHost;
            (case lat of
              | LATTwitter { Credentials = c } =>
                (case (List.assoc "user_id" c, List.assoc "screen_name" c) of
                   | (Some i, Some n) =>
                     H.addTwitterScreenName user i n
                     (* ^ чтобы в письме "New associated account was added"
                          было правильное название twitter handle  *)
                   | _ => return ())
              | _ => return ());
            a <- H.tryAddAssociatedAccount (Some h) user lt;
            if not a then
                fail "Can’t add associated account.\nIt is the last login method of another account.\nPlease, login to that account and add new login method or remove that account."
            else
                _ <- setNewSessionCookie lt lat who;
                (* чтобы lat запомнить *)
                redirect (bless accountUrl)

fun callback elt (qs : option queryString) : transaction page =
    case qs of
        None => error <xml>Empty query string for login callback</xml>
      | Some qs =>
        h <- getHost;
        (lt,lat,la,who) <- H.loginCallback elt h (effectfulUrl (callback elt)) (show qs);
        handleLoginAction lt lat la who

fun loginRedirect elt la : transaction page =
    h <- getHost;
    u <- H.loginGetForwardUrl elt h la (effectfulUrl (callback elt));
    redirect u

fun humanReadableUID uid =
    p <- H.prettyUID uid;
    return (if p <> uid then uid ^ " (" ^ p ^ ")" else uid)

fun logAction' action details uid msec =
    p <- humanReadableUID uid;
    let val m = show msec
        val l = strlen m
        val (s,ms) =
            if l > 3 then (substring m 0 (l-3), substring m (l-3) 3)
            else if l = 3 then ("0", m)
            else if l = 2 then ("0", "0" ^ m)
            else ("0", "00" ^ m)
    in
        H.logT ("-- " ^ action ^ " " ^ p ^ " " ^ details ^ " " ^ s ^ "." ^ ms)
    end
fun logAction [a] action uid (act : transaction a) : transaction a =
    t1 <- H.getUrTime;
    (* оказывается, now в серверной части возвращает время только с точностью
     до секунды
     *)
    r <- act;
    t2 <- H.getUrTime;
    logAction' action "" uid (diffInMilliseconds t1 t2);
    return r

val sessionLogOut : transaction {} =
    sid <- sessionCookie;
    s <- getCookie sid;
    (case s of
       | Some k => when (k <> "")
         (sess <- H.cachedReadSession k;
         H.clearSession "log out" k;
         (case sess of
           | Some s =>
             logAction "log out" s.User (H.userEvent s.User "Log out" "")
           | None => return ()))
       | None => return ());
    clearCookie sid

val logOutOtherSessions : transaction {} =
    sc <- sessionCookie;
    c <- getCookie sc;
    withSome (fn s =>
                 u <- getUserBySession True s;
                 withSome (fn uid => H.logOutAllSessions uid (s :: [])) u) c

fun logOut (_ : option queryString) =
    sessionLogOut;
    redirectToMain

fun adminOnly [a] (act : transaction a) : transaction a =
    u <- getUser "";
    if u <> Some "1" then fail "Admin only" else act
