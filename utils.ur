open UrCalls
open Css

fun min a b = if a < b then a else b
fun max a b = if a > b then a else b

fun modifyF [nm :: Name] [t] [r] [[nm] ~ r] (f : t -> t) (x : $([nm = t] ++ r)) : $([nm = t] ++ r) = x -- nm ++ { nm = f x.nm }
fun setF [nm :: Name] [t] [r] [[nm] ~ r] (v : t) (x : $([nm = t] ++ r)) : $([nm = t] ++ r) = x -- nm ++ { nm = v }
fun setF2 [nm1 :: Name] [nm2 :: Name] [t1] [t2] [r] [[nm1] ~ [nm2]] [[nm1, nm2] ~ r] (v1 : t1) (v2 : t2) (x : $([nm1 = t1, nm2 = t2] ++ r)) : $([nm1 = t1, nm2 = t2] ++ r) = x -- nm1 -- nm2 ++ { nm1 = v1, nm2 = v2 }

fun measure [a] (s : string) (act : transaction a) : transaction a =
    a <- now;
    r <- act;
    b <- now;
    debug (s ^ ": " ^ show (diffInMilliseconds a b) ^ "ms");
    return r

fun trace [a] s (x : a) : a =
    if naughtyDebug s = 0 then x else error <xml>trace???</xml>

fun flip [a] [b] [c] (f:a -> b -> c) (b:b) (a:a) : c = f a b
fun const [a] [b] (x:a) (y:b) = x
fun id [a] (x:a) = x
fun succ x = x+1
fun pred x = x-1
fun sequence_ (l : list (transaction {})) : transaction {} = List.app id l
fun fst [a] [b] (x : a * b) = x.1
fun snd [a] [b] (x : a * b) = x.2
fun cons [a] (x : a) (xs : list a) = x :: xs
fun when cond act =
    if cond then act else return ()
fun maybe [a] [b] (x : b) (f : a -> b) (o : option a) =
    case o of
      | None => x
      | Some x => f x
fun withSome [a] (f : a -> transaction {}) (o : option a) =
    case o of
      | None => return ()
      | Some x => f x
fun modify [a] (s : source a) (f : a -> a) =
    x <- get s;
    set s (f x)
fun toggle s = modify s not
fun elem [t] (_ : eq t) (e : t) = List.exists (fn x => e = x)
fun sortStrs [a] (f : a -> string) (l : list a) : list a =
    List.sort (fn a b => Js.strGt (f a) (f b)) l
fun findM [a] (f : a -> transaction bool) (ls : list a)
    : transaction (option a) =
    case ls of
        [] => return None
      | x :: ls =>
        c <- f x;
        if c then
            return (Some x)
        else
            findM f ls
fun notNull [a] (l : list a) = case l of [] => False | _ => True
fun isNull [a] (l : list a) = case l of [] => True | _ => False
fun reverse [a] (l : list a) : list a = Js.reverse l
fun revAppend [a] (a : list a) (b : list a) : list a = Js.revAppend (a, b)
fun lookupS [a] (k : string) (l : list (string * a)) : option a = Js.lookupS (k, l)

fun assocInsert [a] [b] (_ : eq a) (a:a) (b:b) (l : list (a*b)) =
    let fun go acc l =
            case l of
                [] => (a,b) :: List.rev acc
              | (k,x) :: ls =>
                if k = a then List.revAppend acc ((a,b) :: ls)
                else go ((k,x)::acc) ls
    in
        go [] l
    end
fun isPrefixOf p s = strlen p <= strlen s && substring s 0 (strlen p) = p
fun showT2 [a] [b] (_:show a) (_:show b) : show (a*b) =
    mkShow (fn (a,b) => "(" ^ show a ^ "," ^ show b ^ ")")
fun showT3 [a] [b] [c] (_:show a) (_:show b) (_:show c) : show (a*b*c) =
    mkShow (fn (a,b,c) => "(" ^ show a ^ "," ^ show b ^ "," ^ show c ^ ")")
fun withScrollSaved [a] (eltId : Basis.id) (f : transaction a) : transaction a =
    st <- Js.scrollTop eltId;
    sl <- Js.scrollLeft eltId;
    r <- f;
    st' <- Js.scrollTop eltId;
    sl' <- Js.scrollLeft eltId;
    when (st' <> st) (Js.setScrollTop eltId st);
    when (sl' <> sl) (Js.setScrollLeft eltId sl);
    return r
fun prop1 n v =
    oneProperty noStyle (value (property n) (atom v))
fun prop2 n1 v1 n2 v2 =
    oneProperty (prop1 n1 v1) (value (property n2) (atom v2))

fun setupKeydown (character : keyEvent -> (list int -> bool) -> transaction (option (transaction {}))) (special : keyEvent -> (list int -> bool) -> transaction (option (transaction {}))) =
    let fun keyDown' aee f k ki =
            let fun check list =
                    elem k.KeyCode list ||
                    elem ki (List.mp Js.mkKeyIdentifier list)
            in
            if aee then
                return None
            else
                f k check
            end
        fun keyDown f k =
            aee <- Js.activeElementIsEditable;
            noMod <- Js.noModifiers;
            ki <- Js.getEventKeyIdentifier;
            a <- keyDown' ((aee && k.KeyCode <> 27) || not noMod) f k ki;
            case a of
              | Some act =>
                preventDefault; stopPropagation;
                (* важно вызывать preventDefault сначала, т.к. потом может
                   произойти rpc, после которой uw_event снесётся
                   и будет ошибка *)
                act;
                return True
              | None => return False
    in
        onKeypress (fn k =>
                       ch <- Js.getCharCode;
                       when (ch <> 0)
                            (h <- keyDown character k; return ()));
        (* в firefox надо charCode использовать, а то keycode f5=T, f4=S *)
        onKeydown (fn k => h <- keyDown special k; return ())
(*         keydownHandled <- source False; *)
(*         onKeydown (fn key => *)
(*             (\* надо перехватывать именно keydown, т.к. type-to-navigate *)
(*                отключает onkeypress на время таймаута и получается *)
(*                отрабатывать только каждую вторую кнопку. *\) *)
(*             h <- keyDown key; *)
(* (\*             alert ("onkeydown " ^ show key ^ " h=" ^ show h); *\) *)
(*             set keydownHandled h *)
(*             ) *)
        (* что-то теперь в safari сначала вызывается keypress,
           а потом keydown.
           а в opera что-то не работает autorepeat вне зависимости
           от того, что выбрано keydown или keypress
         *)
(*         onKeypress (fn key => *)
(*             (\* также приходится перехватывать onkeypress, *)
(*                т.к. auto-repeat на всех браузерах, кроме safari/chrome, *)
(*                вызывает onkeypress и не вызывает onkeydown *\) *)
(*             kdh <- get keydownHandled; *)
(* (\*             alert ("onkeypress " ^ show key ^ " kdh=" ^ show kdh); *\) *)
(*             if kdh then *)
(*                 set keydownHandled False; *)
(*                 preventDefault; *)
(*                 stopPropagation *)
(*             else *)
(*                 h <- keyDown key; return ()) *)
    end

fun displayIfG [a] (f : a -> bool) (s : source a) (xml : xbody) : xbody =
   <xml><dyn signal={c <- signal s;
                     return (if f c then xml else <xml/>)} /></xml>

val displayIf = displayIfG (fn x => x)
val displayIfNot = displayIfG (fn x => not x)
fun displayIfC c x = if c then x else <xml/>
fun displayIfNotC c x = if c then <xml/> else x

fun displayIfSigG [a] (f : a -> bool) (s : signal a) (xml : xbody) : xbody =
   <xml><dyn signal={c <- s;
                     return (if f c then xml else <xml/>)} /></xml>

val displayIfSig = displayIfSigG (fn x => x)
val displayIfNotSig = displayIfSigG (fn x => not x)

fun ifS [r] (s : source bool) (t : r) (e : r) : signal r =
    c <- signal s;
    return (if c then t else e)
fun ifSig [r] (s : signal bool) (t : r) (e : r) : signal r =
    c <- s;
    return (if c then t else e)
fun ifClass r cls c = if r then classes cls c else c

fun dyn_ (s : signal xbody) : xbody = <xml><dyn signal={s} /></xml>

fun intersperse s l =
    case l of
        [] => []
      | a::b::c => a::s::intersperse s (b::c)
      | a::[] => a::[]
fun intercalate s l = List.foldr strcat "" (intersperse s l)

fun pageNoBody' (prefix : string) (head_ : xhead) (title_ : string) x : transaction page =
    meta <- htmlHead ();
    v <- version ();
    return <xml>
  <head>
    {meta}
    {head_}
    <title>{[title_]}</title>
    <link href={bless (prefix ^ "/css/basic" ^ v ^ ".css")}
          rel="stylesheet" type="text/css" media="all" />
    <link rel="shortcut icon" href="/favicon.ico" />
  </head>
  {x}
</xml>
fun pageNoBody t x = pageNoBody' "" <xml/> t x

fun page (title : string) x : transaction page =
  pageNoBody title <xml><body class="noTouch">{x}</body></xml>

fun page1 (title : string) x : transaction page = page title <xml>
  <h1>{[title]}</h1>
  {x}
</xml>

con infoMessageAtTheTop
  = { Html : xbody
    , Show : css_class -> string -> transaction {}
    , Error : string -> transaction {}
    , Hide : transaction {}
    }

(* опциональное сообщение -- загрузка, и т.д. *)
val infoMessageAtTheTop : transaction infoMessageAtTheTop =
    text <- source "";
    st <- source null;
    visible <- source False;
    return
        { Html =
          <xml>
            <div dynClass={
                v <- signal visible;
                s <- signal st;
                return (classes s
                       (classes Css.infoMsgContainer
                        (if not v then Css.visibilityHidden else null)))
                (* с Css.displayNone сбрасывается scroll bar в firefox *)
                }>
            <div class={Css.infoMsgText}>
              <dyn signal={t <- signal text;
                           return <xml>{[t]}</xml>}/>
              </div>
            </div>
          </xml>
        , Show = fn s t =>
            set st s; set text t; set visible True
        , Error = fn t =>
            set st errorMsg; set text t; set visible True
(*             spawn ((\*sleep 5000; *\)alert "!!!"; Js.hideLayer (elt id)) *)
            (* ^ что-то не работает spawn *)
        , Hide =
            set visible False
        }

fun showInfo [a]
             st text
             (infoMessage : infoMessageAtTheTop)
             (act : transaction a) : transaction a =
    infoMessage.Show st text;
    r <- act;
    infoMessage.Hide;
    return r


fun textWithLink' (tail : string) (link : option url) (text : xbody) : xbody =
    case link of
        None => <xml>{text}{[tail]}</xml>
      | Some l => <xml><a href={l} target="_blank">{text}</a>{[tail]}</xml>

val textWithLink = textWithLink' ""

(* fun stopPropagationLink link text = *)
(*     <xml><a href={link} target={"_blank"} onclick={fn _ => stopPropagation}>{text}></a></xml> *)

fun textWithLinkNoPropagate cls title (link : option url) (text : xbody) : xbody =
    case link of
        None => <xml><span class={cls} title={title}>{text}</span></xml>
      | Some l => Js.stopPropagationLink cls title l text
(* изврат для iPad, чтобы открывал в Safari, а не в web app, как openLink *)
(* хотя для времени оно теперь и так работает, то ли из-за обновления до iOS 6,
   то ли из-за убирания лишних onclick *)
fun buttonSymbol cls =
    <xml>
      <span class={classes cls btnIcon}></span>
    </xml>

fun buttonT2' bcls cls name title click =
    <xml><a onclick={fn _ => click} class={classes Css.button bcls} title={title}>
      <span class={Css.buttonText}>{[name]}</span>
        {buttonSymbol cls}<span class={Css.buttonText}>{[name]}</span>
      </a></xml>
fun buttonT' bcls cls name title click =
    <xml><a onclick={fn _ => click} class={classes Css.button bcls} title={title}>
        {buttonSymbol cls}<span class={Css.buttonText}>{[name]}</span>
      </a></xml>
val buttonT = buttonT' null
(*     <xml> *)
(*       <a onclick={fn _ => click} class={Css.button}> *)
(*       <table (\* cellpadding=0 cellspacing=0 *\)><tr> *)
(*         <td (\* valign='top' *\)> *)
(*           {buttonSymbol cls} *)
(*         </td><td (\* valign='top' *\)> *)
(*           {[name]} *)
(*       </td></tr></table> *)
(*       </a> *)
(*     </xml> *)
fun button cls name click = buttonT cls name "" click
fun textButton name click =
    <xml>
      <div onclick={fn _ => click} class={Css.textButton}>{[name]}</div>
    </xml>
fun linkButton name click =
    <xml>
      <a onclick={fn _ => click}>{[name]}</a>
    </xml>
fun symbolButton t cls title click =
    <xml><a onclick={fn _ => click} class={Css.symbolButton} title={title}>{buttonSymbol cls}{[t]}</a></xml>

con popups =
    { Hide : transaction {},
      NewMenu : css_class -> xbody -> transaction (id * xbody),
      Toggle : source xbody -> (id * xbody) -> transaction {},
      NewIdPosMenu : id -> {Top : int, Left : int} -> css_class ->
                     xbody -> (id * xbody),
      LiI : css_class -> string -> transaction {} -> xbody,
      LLiI : transaction {} -> css_class -> string -> string -> xbody,
      New : css_class -> xbody -> transaction (id * xbody),
      Setup : transaction {},
      ToggleNoPrevent : source xbody -> (id * xbody) -> transaction {},
      LiSubI : css_class -> string -> xbody,
      NewInfoBox : string -> xbody -> transaction (string * (id * xbody)),
      Li : string -> transaction {} -> xbody,
      LLi : string -> string -> xbody,
      LiInfoBox : source xbody -> (string * (id * xbody)) -> xbody,
      LiSub : string -> xbody,
      IsVisible : (id * xbody) -> transaction bool,
      IsActive : transaction bool,
      NewClickHideBox : css_class -> string -> xbody ->
                        transaction (string * (id * xbody))
    }

val popups : transaction popups =
    popupVisible <- source (None : option (id * source xbody));
    skipClick <- source False;
    let val hide =
            pv <- get popupVisible;
            withSome (fn (_,p) =>
                         set popupVisible None;
                         set p <xml/>) pv
        fun show p (id, x) =
            set popupVisible (Some (id, p));
            set p <xml><span id={id}>{x}</span></xml>
        fun isVisible (id, x) =
            v <- get popupVisible;
            return (case v of
                        Some (i,_) => Js.eq_id i id
                      | None => False)
        val isActive =
            v <- get popupVisible;
            return (Option.isSome v)
        fun toggleNoPrevent p (id, x) =
            v <- isVisible (id,x);
            if v then hide else hide; show p (id,x)
        fun toggle p (id, x) =
            toggleNoPrevent p (id, x);
            stopPropagation
        fun newIdPos id position cls (inner : xbody) =
            (id, Js.msgOnClick "" (fn _ e => case e.Button of
                                                 Basis.Left => set skipClick True
                                               | _ => return ()) <xml><div
                         dynClass={return (ifClass (Option.isNone position)
                                                   Css.centerMenu cls)}
                         (* TODO: если class, а не dynClass, генерится кривой html *)
                         style={case position of
                                 | None => noStyle
                                 | Some p =>
                                   prop2 "left" (Basis.show p.Left ^ "px")
                                         "top" (Basis.show p.Top ^ "px")} >
(*                          onclick={fn _ => (\* stopPropagation *\)set stop}> *)
                         {inner}</div></xml>)
        fun new cls (inner : xbody) =
            id <- fresh;
            return (newIdPos id None cls inner)
        fun newClickHideBox cls title (inner : xbody) =
            id <- fresh;
            return (title, (id, <xml><div class="cls">
              <h1>{[title]}</h1>
              {inner}</div></xml>))
        val newInfoBox = newClickHideBox infoBox
        fun lii (icon : css_class) (t : string) (act : transaction {}) : xbody =
            <xml><li onclick={fn _ => hide; act}>{buttonSymbol icon}<span class={Css.buttonText}>{[t]}</span></li></xml>
        fun li_ (t : string) (act : transaction {}) : xbody =
            <xml><li onclick={fn _ => hide; act}>{[t]}</li></xml>
        fun llii act (icon : css_class) (t : string) (u : string) : xbody =
            Js.stopPropagationLink null "" (bless u) (Js.msgOnClick "" (fn _ _ => act; hide) <xml><li>{buttonSymbol icon}<span class={Css.buttonText}>{[t]}</span></li></xml>)
        fun lli (t : string) (u : string) : xbody =
            Js.stopPropagationLink null "" (bless u) (Js.msgOnClick "" (fn _ _ => hide) <xml><li>{[t]}</li></xml>)
        fun liSub (t : string) : xbody =
            <xml>
              <li onclick={fn _ => stopPropagation}><span class="menuRightArrow">{buttonSymbol Css.btnRight}</span>{[t]}</li></xml>
        fun liSubI (icon : css_class) (t : string) : xbody =
            <xml>
              <li onclick={fn _ => stopPropagation}><span class="menuRightArrow">{buttonSymbol Css.btnRight}</span>{buttonSymbol icon}<span class={Css.buttonText}>{[t]}</span></li></xml>
    in
    return
    { New = new
    , NewMenu = fn c => new (classes Css.menu c)
    , NewIdPosMenu = fn i p c => newIdPos i (Some p) (classes Css.menu c)
    , NewClickHideBox = newClickHideBox
    , NewInfoBox = newInfoBox
    , IsVisible = isVisible
    , IsActive = isActive
    , ToggleNoPrevent = toggleNoPrevent
    , Toggle = toggle
    , Li = li_
    , LiI = lii
    , LiSub = liSub
    , LiSubI = liSubI
    , LLi = lli
    , LLiI = llii
    , LiInfoBox = fn p (title, r) => (li_ title (toggle p r) : xbody)
    , Hide = hide
    , Setup =
        onClick (fn e => case e.Button of
                           | Basis.Left =>
                             s <- get skipClick;
                             set skipClick False;
                             when (not s) hide
                           | _ => return ())
        (* TODO: для ipad-а нужен ontouchstart,
         onmousedown ничем не отличается от onclick и также работает
         только на нодах, имеющих onclick
         http://stackoverflow.com/questions/8430568/jquery-onmouseover-onclick-for-touchscreen-users-ie-ipad
         *)
    }
    end

(* Очередь RPC, не позволяет запускать несколько RPC одновременно.
   Позволяет запускать rpc, которые могут отменить друг-друга
   (изпользуется для UI запросов, когда смена просматриваемой подписки
    отменяет запрос дерева сообщений предыдущей подписки, или же
    отменяет его отображение, если запрос уже отправлен серверу,
    для этого в queueRpc два параметра)
 *)
con rpcQueue
  = { RpcRunning : source bool
    , SeqNum     : source int
    , RpcQueue   : source (list (transaction {}))
    , RunRpcs    : transaction {}
    }

val rpcQueue : transaction rpcQueue =
    rpcRunning <- source False;
    seqNum <- source 0;
    rpcQueue <- source ([] : list (transaction {}));
    let fun loopQueue () =
            q <- get rpcQueue;
            set rpcQueue [];
            case q of
                [] => return ()
              | _ => sequence_ q; loopQueue ()
        val runRpcs =
            r <- get rpcRunning;
            if r then
                return ()
            else
               (set rpcRunning True;
                loopQueue ();
                set rpcRunning False)
    in
        return
            { RpcRunning = rpcRunning
            , SeqNum = seqNum
            , RpcQueue = rpcQueue
            , RunRpcs = runRpcs
            }
    end

fun queueRpc (q : rpcQueue) [a] (tr : transaction a) (f : a -> transaction {}) =
    modify q.RpcQueue (cons (x <- tr; f x));
    q.RunRpcs

(* Запуск отменяемого RPC *)
fun queueCRpc (q : rpcQueue) [a] (tr : transaction a) (f : a -> transaction {}) =
    modify q.SeqNum succ;
    num <- get q.SeqNum;
    modify q.RpcQueue (cons (n <- get q.SeqNum;
                             when (n = num) (
                             x <- tr;
                             n <- get q.SeqNum;
                             when (n = num) ( (* после выполнения rpc
                                                 также проверяем seqNum *)
                             f x))));
    q.RunRpcs

con backgroundRpc a
  = { AddAction : a -> transaction {}
    , OnError   : transaction {}
    , OnUnload  : transaction {}
(*     , Flush     : transaction {} *)
      (* get/endGetList безопасно вызывать только внутри queueRpc,
         где они не перемежаются
       *)
    , GetList   : transaction (list a)
    , EndGetList : transaction {}
(*     , WithList : x :: Type -> (list a -> transaction x) -> transaction x *)
    , RpcQueue : rpcQueue
    }

(* RPC, выполняемые в фоне, через определенное время (15сек) одним махом.
   queueRpcB, queueCRpcB -- запускают rpc как только освободится очередь,
   дополнительно передавая текущий накопленный список фоновый действий
   preprocess получает список в обратном порядке,
   handle -- результат preprocess *)
fun backgroundRpc [a] (preprocess : list a -> list a)
                  (handle : list a -> transaction {})
    : transaction (backgroundRpc a) =
    rpcQueue <- rpcQueue;
    list <- source ([] : list a);
    activeList <- source [];
    processing <- source False;
    let fun setTimeout process =
            p <- get processing;
            when (not p)
                 (set processing True;
                  Js.setTimeout "bgActions" (process False) 15000)
        val getList =
            l <- get list;
            al <- get activeList;
            let val r = List.append l al
            in
                set list [];
                set activeList r;
                return (preprocess r)
            end
        val endGetList =
            set activeList []
        val act =
            l <- getList; when (notNull l) (handle l); endGetList
        fun process sync =
            if sync then
                act
            else
                queueRpc rpcQueue act
                         (fn _ =>
                             set processing False;
                             l <- get list;
                             when (notNull l) (setTimeout process))
        val onError =
            set rpcQueue.RpcRunning False;
            set processing False;
            al <- get activeList;
            modify list (fn l => List.append l al);
            setTimeout process
        fun addAction (a : a) =
            modify list (cons a);
            setTimeout process
    in
        return
        { AddAction = addAction
        , OnError = onError
        , OnUnload = process True
(*         , Flush = process False *)
        , GetList = getList
        , EndGetList = endGetList
(*         , WithList = withList *)
        , RpcQueue = rpcQueue
        }
    end

fun withBGRpcList [a] [x] (b : backgroundRpc a) (f : list a -> transaction x) =
    l <- b.GetList; r <- f l; b.EndGetList; return r

fun queueRpcB [a] [l] (b : backgroundRpc l) (tr : list l -> transaction a) (f : a -> transaction {}) =
    queueRpc b.RpcQueue (withBGRpcList b tr) f

fun queueCRpcB [a] [l] (b : backgroundRpc l) (tr : list l -> transaction a) (f : a -> transaction {}) =
    queueCRpc b.RpcQueue (withBGRpcList b tr) f


fun logo t = <xml>
  <span class="logoBaz">Baz</span><span class="logoQux">Qux</span>
  <span class="logoReader">Reader{[t]}</span></xml>

val baseUrl = "https://bazqux.com/"
val base = bless baseUrl

fun infoPage title (t : xbody) =
    pageNoBody title <xml><body class="errorPage noTouch">
      <h1>{[title]}</h1>
      {t}
      <p><a href={base}>Home</a></p>
    </body></xml>

fun errorPage (e : xbody) =
    infoPage "Error happened" <xml>
      <p>There was an error in handling your request:
      <div class="errorText">{e}</div></p>
    </xml>

val redirectToMain [a] : transaction a = redirect (bless "/")

val privacyPolicyText : xbody =
    <xml>
      <p> (* This policy covers how we use your personal information. *)
      We take your privacy seriously at BazQux Reader and we protect your personal information.</p>
      <p>Any personal information received will only be used to fill your order.
      We will not sell or redistribute your information to anyone. In fact we do not even keep order information on our servers, only your order ID.</p>
      <p>The only information we know about you is your email address (when you sign in with Facebook or Google), your subscriptions list and a list of starred and tagged items.</p>
      <p>At any moment you can export your feeds in OPML-format or opt out from our service. Account deletion is not yet available on our website but you can request us by e-mail to delete your account and remove your subscription data, stars and tags from our servers.</p>
    </xml>

val refundPolicyText : xbody =
    <xml>
      <p>If you are not 100% satisfied with your purchase,
      within 30 days from the purchase date,
      we will fully refund the cost of your order.</p>
    </xml>
