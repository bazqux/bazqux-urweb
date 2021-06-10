open Session
open Either

fun withUser [r] action (f : string -> transaction r) (l : list bgAction)
    : transaction r =
    u <- getUser action;
    case u of
      | Some u =>
        t1 <- H.getUrTime;
        d <- H.performBgActions u l;
        r <- f u;
        t2 <- H.getUrTime;
        logAction' (action ^ if notNull l then "/bg" else "") d u (diffInMilliseconds t1 t2);
        return r
      | None => error <xml>Not logged in. Please, log in on the home page first.</xml>
val bgactions = withUser "" (fn u => return ())

fun readAssociatedAccounts u =
    us <- H.readUserSettings u;
    return (maybe [] (fn s => maybe [] (getF [#AssociatedAccounts]) s.Ex) us)
fun setEmail e : list Datatypes.bgAction -> transaction (either xbody (option string)) =
    withUser "setEmail"
             (fn userId =>
                 case H.validateEmail e of
                   | Some email =>
                     u <- H.getUserByLogin (LTEmail { Email = email}) None;
                     (case u of
                       | Some euid =>
                         if userId = euid then
                             return (Right None)
                         else
                             return (Left <xml>The email
                               {emailX email} is busy.
                               Try another one or log into that account
                               and change its email  or delete it.</xml>)
                       | None =>
                         h <- getHost;
                         ok <- H.sendChangeEmailEmail h userId email;
                         return (if ok then Right (Some email) else Left (txt
                           "Too many confirmation emails. Please, check your spam folder.")))
                   | None =>
                     return (Left (txt "Please, enter valid email")))
fun setUsername u =
    withUser "setUsername"
             (fn userId =>
                 h <- getHost;
                 ok <- H.setUsername (Some h) userId u;
                 if ok then
                     logOutOtherSessions;
                     aa <- readAssociatedAccounts userId;
                     return (Some aa)
                 else
                     return None)
fun setPassword p =
    withUser "setPassword"
             (fn userId =>
                 logOutOtherSessions;
                 h <- getHost;
                 H.setPassword (Some h) userId p)
fun removeAssociatedAccount a =
    withUser "removeAssociatedAccount"
             (fn userId =>
                 logOutOtherSessions;
                 h <- getHost;
                 ok <- H.tryRemoveAssociatedAccount (Some h) userId a;
                 if ok then
                     Monad.mp some (readAssociatedAccounts userId)
                 else
                     return None)

fun readability key =
    withUser "readability"
             (fn u =>
                 h <- getHost;
                 al <- acceptLanguage;
                 H.getFullText u False h al key)
fun msg key = withUser "msg" (fn _ => H.readMsg key)

fun subscriptions' n uf bg t h siv fssh = withUser n
    (fn userId =>
        host <- getHost;
        r <- H.subscriptionsAndRenames host bg uf t h siv fssh userId;
        p <- H.getPaidTill userId;
        ct <- now;
        return (r, (p,ct)))
val subscriptions = subscriptions' "subscriptions" UFAll
val subscriptions_ = subscriptions' "subscriptions_" UFNone
val subscriptionsAndSettings = withUser "subscriptionsAndSettings"
    (fn userId =>
        host <- getHost;
        H.subscriptionsAndSettings host True True userId)
val restoreSubscriptions = withUser "restoreSubscriptions"
    (fn userId =>
        H.restoreSubscriptionsFromBackup userId;
        t <- H.getUrTime;
        host <- getHost;
        H.subscriptionsAndRenames host False UFChanged t "" "" "" userId)

fun markReqReadCounters vm mr mids = withUser "markReqReadCounters"
    (fn userId =>
        H.markReqReadCounters userId vm mr mids)

fun addSubscription t fssh url l =
    if url = "" then error <xml>Empty feed URL</xml>
    else withUser "addSubscription"
    (fn userId =>
        h <- H.userSubscribe userId url None [];
        host <- getHost;
        r <- H.subscriptionsAndRenames host False UFChanged t "" "" fssh userId;
        return (h,r)) l
fun addDiscoverySubscription t fssh url country query l =
    if url = "" then error <xml>Empty feed URL</xml>
    else withUser "addDiscoverySubscription"
    (fn userId =>
        h <- H.userDiscoverySubscribe userId url country query None [];
        host <- getHost;
        r <- H.subscriptionsAndRenames host False UFChanged t "" "" fssh userId;
        return (h,r)) l
fun renameSubscription t fssh url to l =
    if url = "" then error <xml>Empty feed URL</xml>
    else withUser "renameSubscription"
    (fn userId =>
        H.userRenameSubscription userId url to;
        host <- getHost;
        H.subscriptionsAndRenames host False UFNone t "" "" fssh userId) l
fun renameFolder t fssh from to l =
    if to = "" then error <xml>Empty folder name</xml>
    else withUser "renameFolder"
    (fn userId =>
        h <- H.userRenameFolder userId from to;
        host <- getHost;
        r <- H.subscriptionsAndRenames host False UFNone t "" "" fssh userId;
        return (h,r)) l
fun editSubscriptionFolders t fssh url f add l =
    if url = "" || f = "" then error <xml>Empty feed URL or folder</xml>
    else withUser "editSubscriptionFolders"
    (fn userId =>
        H.userEditSubscriptionFolders userId url f add;
        host <- getHost;
        H.subscriptionsAndRenames host False UFNone t "" "" fssh userId) l
fun retrySubscription url =
    withUser "retrySubscription"
             (fn userId => H.userRetrySubscription userId url)
fun removeSubscriptions t fssh (urls : list string) =
    withUser "removeSubscriptions"
             (fn userId =>
                 H.userUnsubscribe userId urls;
                 host <- getHost;
                 H.subscriptionsAndRenames host False UFChanged t "" "" fssh userId)
fun getTree mtvm reqs =
    withUser "getTree"
             (fn userId =>
                 h <- getHost;
                 al <- acceptLanguage;
                 H.getTree (AMNormal { HostName = h, AcceptLanguage = al })
                           userId mtvm reqs)
fun getTreeD url mtvm reqs =
    withUser "getTree"
             (fn userId =>
                 h <- getHost;
                 al <- acceptLanguage;
                 H.getTree (AMDiscovery { HostName = h, AcceptLanguage = al,
                                          Url = url })
                           userId mtvm reqs)
fun editFilters name q f t h siv fssh =
    withUser name
      (fn userId =>
          r <- maybe (return None) H.checkQuerySyntax q;
          case r of
            | None =>
              f userId;
              host <- getHost;
              s <- H.subscriptionsAndRenames host False UFChanged t h siv fssh userId;
              return (Right s)
            | Some e =>
              return (Left e))

fun addFilter query negate feeds =
    editFilters "addFilter" (Some query)
      (fn u => H.addFilter u query negate feeds)
fun addSmartStream name query feeds =
    editFilters "addSmartStream" (Some query)
      (fn u => H.addSmartStream u name query feeds)
fun deleteFilter fid =
    editFilters "deleteFilter" None (fn u => H.deleteFilter u fid)
fun deleteSmartStream name =
    editFilters "deleteSmartStream" None (fn u => H.deleteSmartStream u name)
fun editFilter fid query negate feeds =
    editFilters "editFilter" (Some query)
      (fn u => H.editFilter u fid query negate feeds)
fun editSmartStream name query feeds =
    editFilters "editSmartStream" (Some query)
      (fn u => H.editSmartStream u name query feeds)

fun enablePublicFeed t = withUser "enablePublicFeed" (H.enablePublicFeed t)
fun disablePublicFeed t = withUser "disablePublicFeed" (H.disablePublicFeed t)
fun generateNewPublicFeed t = withUser "generateNewPublicFeed" (H.generateNewPublicFeed t)

fun discover country query =
    withUser "discover"
             (fn userId =>
                 host <- getHost;
                 H.discover host userId country query) []
fun feedDetails url =
    withUser "feedDetails"
             (fn userId =>
                 host <- getHost;
                 H.getFeedDetails host userId url) []

fun tagsForest ts vm =
    withUser "tagsForest"
             (fn userId =>
                 h <- getHost;
                 al <- acceptLanguage;
                 H.tagsForest
                     (AMNormal { HostName = h, AcceptLanguage = al })
                     userId ts vm)

fun folderForest name feedsOrDiscovery vm =
    withUser "folderForest"
             (fn userId =>
                 h <- getHost;
                 al <- acceptLanguage;
                 H.folderForest userId name feedsOrDiscovery [] vm h al)
fun smartStreamForest name feeds vm =
    withUser "smartStreamForest"
             (fn userId =>
                 h <- getHost;
                 al <- acceptLanguage;
                 H.smartStreamForest
                     (AMNormal { HostName = h, AcceptLanguage = al })
                     userId name feeds vm)

fun filterForest q folder feedsOrDiscovery vm =
    withUser "filterForest"
             (fn userId =>
                 h <- getHost;
                 al <- acceptLanguage;
                 H.filterForest userId None q folder feedsOrDiscovery vm h al)
fun filterSmartStreamForest n q feeds vm =
    withUser "filterSmartStreamForest"
             (fn userId =>
                 h <- getHost;
                 al <- acceptLanguage;
                 H.filterForest userId (Some n) q None
                     (FODFeeds { ReadCounters = feeds }) vm h al)
fun filterTagsForest q tags vm =
    withUser "filterTagsForest"
             (fn userId =>
                 h <- getHost;
                 al <- acceptLanguage;
                 H.filterTagsForest userId q tags vm h al)
