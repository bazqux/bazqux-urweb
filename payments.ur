open Datatypes

val lifetimeYear = Js.fromDatetimeUtc 2100 0 1 0 0 0
fun lifetime pt = case pt of
    | PTPaid { Till = t } => t > lifetimeYear
    | _ => False

fun buy (uid : string) (r : { Option : option string }) : transaction page =
    let fun go userId =
        case r.Option of
          | Some pid =>
            e <- H.userEmail userId;
            H.buyPage pid userId e
          | None => error <xml>No payment option selected</xml>
    in
        if uid <> "" then
            (* /renew задает пользователя, главная страница нет
               (в принципе, тоже может задавать, только зачем)
             *)
            e <- H.isUserExists uid;
            if e then
                go uid
            else
                error <xml>Invalid user ID: {[uid]}</xml>
        else
            Rpcs.withUser "buy" go []
    end

fun order_notification (pb : postBody) : transaction page =
(*     debug ("postType: " ^ postType pb); *)
(*     debug ("postData: " ^ postData pb); *)
        (* TODO: top.ur:postFields проверяет "application/x-www-form-urlencoded", а приходит "application/x-www-form-urlencoded; charset=UTF-8" *)
        (* а еще Invalid escaped URL byte starting at: .&amp *)
    p <- H.orderNotification (postData pb);
    return <xml/>

fun order_notification_new (pb : postBody) : transaction page =
    h <- getHeader (blessRequestHeader "X-FS-Signature");
    case h of
      | None => error (txt "Not signed")
      | Some s =>
        p <- H.orderNotificationNew (postData pb) s;
        return <xml/>
