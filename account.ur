open Datatypes
open Css
open Utils
open Either

structure P = Popups

val exiting = Unsafe.boolSource "exiting" False
fun flushRedirect u =
    BackgroundRpc.flush;
    Basis.redirect u
val logOutAction =
    set exiting True;
    flushRedirect (effectfulUrl Session.logOut)
val exportDeleteLogOut =
    <xml><p class="gray grayLink">
      <a link={Pages.opml}>Export OPML</a> ·
      <a link={Pages.deleteAccount}>Delete account</a> ·
      <a href={effectfulUrl Session.logOut}
        onclick={fn _ => logOutAction; preventDefault}>Log out</a>
    </p></xml>

fun buyNow pt = if Payments.lifetime pt then "Donate" else "Buy now"

fun toggleBuyBox' toggle buyT paidTill =
    pt <- get paidTill;
    buyBox <- P.newBigBox (buyNow pt) buyT;
    toggle buyBox
val toggleBuyBox = toggleBuyBox' P.toggle

fun accountBox (associatedAccounts : source (list loginType)) associatedAccountNames passwordSet paidTill payments maxPaidSubscriptions hrUid buyT (ssWidget : Feeds.ssWidget) maxFiltersOrSmartStreams =
    addAssociatedAccountEnabled <- source False;
    let fun till n t =
            if t > Payments.lifetimeYear then
                txt "Lifetime subscription"
            else
                <xml>{[n]} till {formatTimeOnClient t}</xml>
        fun editBox
                (password : bool)
                (verify : string -> option string)
                (saveEdit : string -> transaction (option xbody))
                (whenNotEdit : (string -> transaction {}) -> xbody) =
            tid <- fresh;
            text <- source "";
            edit <- source False;
            saving <- source False;
            saveError <- source None;
            let fun startEdit t =
                    set text t;
                    set edit True;
                    set saving False;
                    Js.select tid;
                    Js.focus tid
                val cancel =
                    set edit False;
                    set saveError None
                val save =
                    t <- get text;
                    when (Option.isNone (verify t))
                         (set saving True;
                          e <- saveEdit t;
                          set saving False;
                          set saveError e;
                          if Option.isSome e then
                            startEdit t
                          else
                            set edit False)
                fun keydown k =
                    onEnter save k;
                    onEscape cancel k
            in
                return (dynS (fn e =>
                  if e then <xml><p>
                    {dynS (fn err => case err of
                      | None => <xml/>
                      | Some e =>
                        <xml><p class="editBoxError">{e}</p></xml>
                      )
                      saveError}
                    {if password then
                        <xml><cpassword id={tid} source={text}
                          onkeydown={keydown} /></xml>
                     else
                        <xml><ctextbox id={tid} source={text}
                          onkeydown={keydown} /></xml>}
                    {disabledIf
                      (t <- signal text;
                       s <- signal saving;
                       return (s || Option.isSome (verify t)))
                      (textButton "OK" save)
                    }{textButton "Cancel" cancel}
                    {dynS (fn t => case verify t of
                      | None => <xml/>
                      | Some e => txt e) text}
                    </p></xml>
                  else
                    whenNotEdit startEdit) edit)
            end
        fun rmAssociatedAccount a =
            r <- rpc (Rpcs.removeAssociatedAccount a []);
            case r of
              | Some aa' =>
                set associatedAccounts aa'
              | None =>
                alert "Can’t remove last login method. Please add another login method before removing this one."
        val aaType = spanClass Css.associatedAccountType
        val aaId = spanClass Css.associatedAccountId
        fun associatedAccount an a =
            let fun aa typ id =
                    let val name = Option.get id (List.assoc a an) in
                    Some <xml>
                      <div>{aaType (txt typ)}</div>
                      <div>{aaId (txt name)}
                        {textButton "Remove"
                          (askBefore
                            ("Are you sure you want to remove associated account\n"
                             ^ typ ^ " " ^ name ^ " ?")
                            (rmAssociatedAccount a))}</div>
                    </xml>
                    end
            in
              case a of
                | LTGoogle g => aa "Google" g.Email
                | LTFacebook f => aa "Facebook" f.Email
                | LTTwitter t => aa "Twitter" t.Id
                | LTOpenId o => aa "OpenID" o.URL
(*                 | LTEmail e => aa "Email" e.Email *)
(*                 | LTUsername u => aa "Username" u.Username *)
                | _ => None
            end
    in
    emailSet <- source None;
    emailBox <- editBox False (fn e => None)
      (fn em =>
        s <- rpc (Rpcs.setEmail em []);
        case s of
          | Right (e : option string) =>
            set emailSet e;
            return None
          | Left (er : xbody) =>
            return (Some er))
      (fn edit =>
        (dyn_ (aa <- signal associatedAccounts;
               es <- signal emailSet;
               let val e =
                       findF (fn a => case a of
                         | LTEmail u => Some u.Email
                         | _ => None) aa
               in
                   return (case (es, e) of
                     | (Some em, _) => <xml>
                       <p>Confirmation email was sent to
                         <b>{noHyphens (txt em)}</b>.
                       </p>
                       {textButton "Change" (edit em)}
                       </xml>
                     | (_, Some em) => <xml>
                       {aaId (txt em)}
                       {textButton "Change" (edit em)}
                       </xml>
                     | (_, None) =>
                       textButton "Set email" (edit ""))
               end)));
    usernameBox <- editBox False
      (fn l => if Js.checkLogin l then None else
        Some "english letters or digits, 4 characters minimum")
      (fn un =>
        r <- rpc (Rpcs.setUsername un []);
        case r of
          | Some aa' =>
            set associatedAccounts aa';
            return None
          | None =>
            return (Some (txt "This username is busy. Try another one.")))
      (fn edit =>
        (dyn_ (aa <- signal associatedAccounts;
               let val n =
                       findF (fn a => case a of
                         | LTUsername u => Some u.Username
                         | _ => None) aa
               in
                   return (case n of
                     | Some un => <xml>
                       {aaId (txt un)}
                       {textButton "Change" (edit un)
                       }{textButton "Remove"
                         (askBefore
                           ("Are you sure you want to remove your username “"
                             ^ un ^ "”?")
                           (rmAssociatedAccount
                             (LTUsername { Username = un })))}
                       </xml>
                     | None =>
                       textButton "Set username" (edit ""))
               end)));
    passwordBox <- editBox True
      (fn p => if p <> "" then None else Some "must be non-empty")
      (fn p =>
        rpc (Rpcs.setPassword p []);
        set passwordSet True;
        return None)
      (fn edit =>
        dynS (fn s => <xml><p>
               {textButton
                 (if s then "Change password" else "Set password")
                 (edit "")}</p></xml>)
             passwordSet);
    box <- P.newBigBox "Account" <xml>
      <h3>Email</h3>
      {emailBox}
      <h3>Username</h3>
      {usernameBox}
      <h3>Password</h3>
      {passwordBox}
      <p>Both email and username can be used in client {pageLink (txt "apps") Pages.apps}. Changing of password, email or username will log out all other sessions.</p>
      <h3>Associated accounts</h3>
      {dyn_ (aa <- signal associatedAccounts;
             an <- signal associatedAccountNames;
             return (case List.mapPartial (associatedAccount an) aa of
               | [] => <xml/>
               | l => spanClass Css.associatedAccountsList (List.mapX id l)))}
      <p>{textButton "Add" (toggle addAssociatedAccountEnabled)}</p>
      <p>{displayIf addAssociatedAccountEnabled
                    (Pages.externalLoginButtons ELAAddAssociatedAccount)}</p>
      <p>Adding Twitter or Facebook account will help to fetch your Twitter or Facebook feeds faster.</p>
      <hr/>
      <h3>Status</h3>
      <p>{dyn_ (pt <- signal paidTill;
             return (case pt of
                | PTUnknown => txt "unknown?"
                | PTFreeTrial { Till = t } => till "Free trial" t
                | PTFreeTrialFinished { Till = t } => till "Free trial" t
                | PTPaidFinished { Till = t } => till "Paid" t
                | PTPaid { Till = t } => till "Paid" t))}</p>
      <p>{activeXml (Monad.mp txt Js.getFeedsCount)}
        of {dynS txt maxPaidSubscriptions} feeds</p>

      <p>
        {dyn_ ((fs, ss) <- signal ssWidget.FiltersAndSmartStreams;
               m <- signal maxFiltersOrSmartStreams;
               let val nf = List.length fs
                   val ns = List.length ss
                   val t = show (nf + ns) ^ " of " ^ show m
                           ^ " filters or smart streams"
                           ^ if nf + ns > 0 then
                                 " (" ^ show nf ^ plural nf " filter"
                                 ^ ", " ^ show ns ^ plural ns " smart stream"
                                 ^ ")"
                             else
                                 ""
               in
                   return (txt t)
               end)}</p>

      <h3>Payments</h3>
      {dyn_ (ps <- signal payments;
             return (case ps of
               | [] => <xml/>
               | l => spanClass Css.paymentsList
                 (List.mapX (fn (time, typ, invoice) => <xml>
                   {divClass Css.paymentDate
                     (formatTimeOnClient' "%Y-%m-%d" time)}
                   <div class={Css.paymentDescription}>
                     {[typ]} [{hrefLink (txt "invoice") invoice}]
                   </div>
                 </xml>) (List.rev ps))))}
      <p>{dyn_ (pt <- signal paidTill;
                return (textButton (buyNow pt) (toggleBuyBox buyT paidTill)))}</p>
      <h3>More</h3>
      {exportDeleteLogOut}
    </xml>;
    return (P.toggle box)
    end
