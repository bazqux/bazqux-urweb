open Datatypes
open Css
open HtmlTags

(* fun [a] [b] (+) (f : a -> b) (x : a) : b = f x *)

fun fail [a] (e:string) : a = error (txt e)
fun void [a] (act : transaction a) : transaction {} =
    _ <- act;
    return ()

fun compose [a] [b] [c] (f : b -> c) (g : a -> b) (x : a) : c = f (g x)

fun isHttpOrFtp u =
    case strindex u #":" of
      | Some i =>
        let val p = substring u 0 i in
            p = "https" || p = "http" || p = "ftp"
        end
      | _ => False
(* браузер Vivaldi не открывает почтовый агент для mailto ссылок,
   если у ссылки есть rel=noopener,
   у Android Chrome и iOS Safari с этим тоже проблемы
   https://github.com/GoogleChrome/lighthouse/issues/3714#issuecomment-341234411
   Дублируем здесь логику из Preprocess -- noopener только для http(s)/ftp,
   magnet, mailto без target=_blank
   Chrome будет заменять ридер при вызове почтового агента ну и ладно
 *)
fun hrefLink' (t : xbody) u =
    <xml><a href={bless u} rel="noopener" target="_blank">{t}</a></xml>
fun hrefLink (t : xbody) u =
    if isHttpOrFtp u then
        hrefLink' t u
    else
        <xml><a href={bless u}>{t}</a></xml>
fun pageLink (t : xbody) (u : transaction page) =
    <xml><a link={u} rel="noopener" target="_blank">{t}</a></xml>
fun clsHrefLink cls (t : xbody) u =
    if isHttpOrFtp u then
        <xml><a class={cls} href={bless u} dir="auto" rel="noopener" target="_blank">{t}</a></xml>
    else
        <xml><a class={cls} href={bless u} dir="auto">{t}</a></xml>
fun actHrefLink' (act : transaction {}) (t : xbody) u =
    <xml><a href={bless u} rel="noopener" target="_blank" onclick={fn _ => act}>{t}</a></xml>
fun actHrefLink (act : transaction {}) (t : xbody) u =
    if isHttpOrFtp u then
        actHrefLink' act t u
    else
        <xml><a href={bless u} onclick={fn _ => act}>{t}</a></xml>
(* rel=noopener для борьбы с этой уязвимостью https://dev.to/phishing
   правда он поддерживается только в Chrome 49+ и Opera 36+.
   в Firefox и Safari нужен noreferrer, но он убивает Referrer,
   что нам совершенно не нужно.
   Для FF/Chrome еще можно делать window.open().opener = null,
   чтобы по клавиатурным сокращениям не было уязвимости.
   В Safari это не работает.

   При открытии в фоновой вкладке (Cmd+Click) уязвимость не работает в
   Safari/FF, даже без noopener. В Chrome работает.

   В общем, пока noopener и ждем, пока баги в FF/Safari поправят,
   багрепорты уже есть.
 *)
fun hrefLinkStopPropagation' (t : xbody) (u : string) =
    actHrefLink' stopPropagation t u
fun hrefLinkStopPropagation (t : xbody) (u : string) =
    actHrefLink stopPropagation t u

val acceptLanguage =
    Monad.mp (Option.get "") (getHeader (blessRequestHeader "Accept-Language"))

fun min [t] (_:ord t) (a:t) (b:t) : t = if a < b then a else b
fun max [t] (_:ord t) (a:t) (b:t) : t = if a > b then a else b

fun modifyF [nm :: Name] [t] [r] [[nm] ~ r] (f : t -> t) (x : $([nm = t] ++ r)) : $([nm = t] ++ r) = x -- nm ++ { nm = f x.nm }
fun setF [nm :: Name] [t] [r] [[nm] ~ r] (v : t) (x : $([nm = t] ++ r)) : $([nm = t] ++ r) = x -- nm ++ { nm = v }
fun setF2 [nm1 :: Name] [nm2 :: Name] [t1] [t2] [r] [[nm1] ~ [nm2]] [[nm1, nm2] ~ r] (v1 : t1) (v2 : t2) (x : $([nm1 = t1, nm2 = t2] ++ r)) : $([nm1 = t1, nm2 = t2] ++ r) = x -- nm1 -- nm2 ++ { nm1 = v1, nm2 = v2 }
fun getF [nm :: Name] [t] [r] [[nm] ~ r] (x : $([nm = t] ++ r)) : t = x.nm
fun signalF [nm :: Name] [t] [r] [[nm] ~ r] (x : source $([nm = t] ++ r)) : signal t = r <- signal x; return r.nm

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
val forM_ = flip List.app
fun any [a] (p : a -> bool) : list a -> bool = Option.isSome <<< List.find p
fun fst [a] [b] (x : a * b) = x.1
fun snd [a] [b] (x : a * b) = x.2
fun cons [a] (x : a) (xs : list a) = x :: xs
fun cat (a:xbody) (b:xbody) : xbody = <xml>{a}{b}</xml>
fun when cond act =
    if cond then act else return ()
fun maybe [a] [b] (x : b) (f : a -> b) (o : option a) : b =
    case o of
      | None => x
      | Some x => f x
fun mapMaybe [a] [b] (f : a -> option b) (l : list a) : list b =
    case l of
      | [] => []
      | x :: xs =>
        case f x of
          | Some r => r :: mapMaybe f xs
          | None => mapMaybe f xs
fun withSome [a] (f : a -> transaction {}) (o : option a) =
    case o of
      | None => return ()
      | Some x => f x
fun some [a] (x : a) : option a = Some x
fun modify [a] (s : source a) (f : a -> a) =
    x <- get s;
    set s (f x)
fun setNeq [a] (_ : eq a) (s : source a) (x : a) : transaction {} =
    (* перерисовка dyn-ов часто проходит быстрее,
       чем проверка неравенства данных *)
    x0 <- get s;
    when (x <> x0) (set s x)
fun toggle s = modify s not
fun elem [t] (_ : eq t) (e : t) = List.exists (fn x => e = x)
fun notElem [t] (_ : eq t) (e : t) l = not (elem e l)
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
fun lookupDefault [a] (d : a) (f : a -> bool) (ls : list a) : a =
    case ls of
      | [] => d
      | x :: xs => if f x then x else lookupDefault d f xs
fun notNull [a] (l : list a) = case l of [] => False | _ => True
fun isNull [a] (l : list a) = case l of [] => True | _ => False
fun difference [a] (_ : eq a) a b : list a =
    List.filter (fn x => not (List.exists (eq x) b)) a
fun zip [a] [b] (a : list a) (b : list b) : list (a * b) =
    case (a, b) of
    | (ah :: at, bh :: bt) => (ah, bh) :: zip at bt
    | _ => []
(* слияние двух отсортированных списков *)
fun mergeWith
        [k] [v] (_ : eq k) (_ : ord k)
        (f : v -> v -> v) (a : list (k * v)) (b : list (k * v)) : list (k * v) =
    case (a, b) of
    | (ah :: at, bh :: bt) =>
      if ah.1 < bh.1 then
          ah :: mergeWith f at (bh :: bt)
      else if ah.1 = bh.1 then
          (ah.1, f ah.2 bh.2) :: mergeWith f at bt
      else
          bh :: mergeWith f (ah :: at) bt
    | ([], b) => b
    | (a, []) => a
(* пересечение двух отсортированных списков *)
fun intersectWith
        [k] [v] (_ : eq k) (_ : ord k)
        (f : v -> v -> v) (a : list (k * v)) (b : list (k * v)) : list (k * v) =
    case (a, b) of
    | (ah :: at, bh :: bt) =>
      if ah.1 < bh.1 then
          intersectWith f at (bh :: bt)
      else if ah.1 = bh.1 then
          (ah.1, f ah.2 bh.2) :: intersectWith f at bt
      else
          intersectWith f (ah :: at) bt
    | _ => []
fun findF [a] [b] (f : a -> option b) (l : list a) : option b =
    case l of
      | [] => None
      | x :: xs =>
        case f x of
          | None => findF f xs
          | some => some
fun concat [a] (l : list (list a)) : list a =
    let fun go acc l =
            case l of
              | [] => List.rev acc
              | x :: xs => go (List.revAppend x acc) xs
    in
        go [] l
    end
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
fun isPrefixOf p s = strlenGe s (strlen p) && substring s 0 (strlen p) = p
fun isInfixOf p s = Option.isSome (strsindex s p)
fun stripPrefix p s =
    if isPrefixOf p s then Some (strsuffix s (strlen p)) else None
fun mplus [a] (a : option a) (b : option a) : option a =
    case (a, b) of
      | (Some _, _) => a
      | (_, Some _) => b
      | _ => None
fun stripHttpWww u =
    Option.get u
    <| mplus (stripPrefix "https://www." u)
    <| mplus (stripPrefix "http://www." u)
    <| mplus (stripPrefix "https://" u)
    <|        stripPrefix "http://" u
fun showT2 [a] [b] (_:show a) (_:show b) : show (a*b) =
    mkShow (fn (a,b) => "(" ^ show a ^ "," ^ show b ^ ")")
fun showT3 [a] [b] [c] (_:show a) (_:show b) (_:show c) : show (a*b*c) =
    mkShow (fn (a,b,c) => "(" ^ show a ^ "," ^ show b ^ "," ^ show c ^ ")")
fun showT4 [a] [b] [c] [d] (_:show a) (_:show b) (_:show c) (_:show d) : show (a*b*c*d) =
    mkShow (fn (a,b,c,d) => "(" ^ show a ^ "," ^ show b ^ "," ^ show c ^ "," ^ show d ^ ")")
fun withScrollSaved [a] (eltId : Basis.id) (f : transaction a) : transaction a =
    st <- Js.scrollTop eltId;
    r <- f;
    (* неважно, получаем мы значение scrollTop или устанавливаем его,
       все равно будет reflow/layout.
     *)
    st' <- Js.scrollTop eltId;
    when (st' <> st) (Js.setScrollTopNoOnScroll eltId st);
    return r
fun prop1 n v =
    oneProperty noStyle (value (property n) (atom v))
fun prop2 n1 v1 n2 v2 =
    oneProperty (prop1 n1 v1) (value (property n2) (atom v2))
fun intPart x = float (trunc x)
fun fraction x = x - intPart x

fun askBefore (q:string) (act : transaction {}) : transaction {} =
    c <- confirm q;
    when c act

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

fun onKeyCode kc act k =
    when (k.KeyCode = kc)
         (stopPropagation; (* а то list view раскрывает *)
          act)
val onEnter = onKeyCode 13
val onEscape = onKeyCode 27

fun dyn_ (s : signal xbody) : xbody = <xml><dyn signal={s} /></xml>

fun displayIfG [a] (f : a -> bool) (s : source a) (xml : xbody) : xbody =
   dyn_ (c <- signal s;
         return (if f c then xml else <xml/>))

val displayIf = displayIfG (fn x => x)
val displayIfNot = displayIfG (fn x => not x)
fun displayIfC c (x : xbody) : xbody = if c then x else <xml/>
fun displayIfNotC c (x : xbody) : xbody = if c then <xml/> else x

fun displayIfSigG [a] (f : a -> bool) (s : signal a) (xml : xbody) : xbody =
    dyn_ (c <- s;
          return (if f c then xml else <xml/>))

val displayIfSig = displayIfSigG (fn x => x)
val displayIfNotSig = displayIfSigG (fn x => not x)
fun displaySome (s : source (option xbody)) : xbody =
    dyn_ (Monad.mp (Option.get <xml/>) (signal s))

fun ifS [r] (s : source bool) (t : r) (e : r) : signal r =
    c <- signal s;
    return (if c then t else e)
fun ifSig [r] (s : signal bool) (t : r) (e : r) : signal r =
    c <- s;
    return (if c then t else e)
fun ifClass r cls c = if r then classes cls c else c
fun ifClassS s cls other =
    c <- signal s;
    o <- other;
    return (if c then classes cls o else o)
val classList = List.foldl classes null
fun mapS [a] [b] (f : a -> b) (s : source a) : signal b =
    x <- signal s;
    return (f x)
fun dynS [a] (f : a -> xbody) (s : source a) : xbody =
    dyn_ (x <- signal s;
          return (f x))
fun eqS [a] (_ : eq a) (s : source a) (x : a) : signal bool =
    c <- signal s;
    return (c = x)
fun ifDynClass s c x : xbody =
    <xml><span dynClass={ifSig s c null}>{x}</span></xml>
fun disabledIf s x = ifDynClass s Css.disabled x
fun selectedIf s x = ifDynClass s Css.selected x
fun spanClass c (x : xbody) : xbody =
    <xml><span class={c}>{x}</span></xml>
fun divClass c (x : xbody) : xbody =
    <xml><div class={c}>{x}</div></xml>
fun pClass c (x : xbody) : xbody =
    <xml><p class={c}>{x}</p></xml>
val noHyphens = spanClass Css.noHyphens
fun buttonName t =
    spanClass (classes Css.noHyphens Css.nowrap) (txt ("“" ^ t ^ "”"))
fun emailX e = <xml><b>{noHyphens (txt e)}</b></xml>

fun intersperse s l =
    case l of
        [] => []
      | a::b::c => a::s::intersperse s (b::c)
      | a::[] => a::[]
fun intercalate s l = List.foldr strcat "" (intersperse s l)

fun pageNoBody' (head_ : xhead) (title_ : string) x : transaction page =
    webpackStyles <- H.webpackStyles;
    return <xml>
  <head>
    <meta charset="utf-8" />
    (* в Ur/Web железно зашит <!DOCTYPE html><html>,
       но было бы хорошо сделать <html lang=en-US>,
       тогда VoiceOver/JAWS будут правильным голосом и будут правильно
       работать переносы (что нам не нужно) и <q>
       -- сделал установку атрибута через jsInit, т.к., теоретически,
       мы можем сделать локализацию
     *)
    <meta name="viewport" content="initial-scale=1, width=device-width, viewport-fit=cover" />
    (* без viewport на мобильных устройствах открывается с width ~ 900 *)
    {head_}
    <title>{[title_]}</title>
    {webpackStyles}
    <link rel="shortcut icon" href="/favicon.ico" />
  </head>
  {H.hyphenatePage x}
</xml>
fun pageNoBody t x = pageNoBody' <xml/> t x

fun page (title : string) x : transaction page =
  pageNoBody title <xml><body>{x}</body></xml>

fun page1 (title : string) x : transaction page = page title <xml>
  <h1>{[title]}</h1>
  {x}
</xml>

con infoMessageAtTheTop
  = { Html : xbody
    , Show : string -> transaction {}
    , Error : string -> string -> transaction {}
    , Hide : transaction {}
    }

(* Внутренности диалогов/меню/сообщений,
   при помощи CSS определяется, прокручивается dialogContent или нет,
   показывается заголовок или нет
 *)
fun dialog (title : string) (closeAction : transaction {}) (inner : xbody) =
  <xml>
    <div class="dialogHeader">
      <h1>{[title]}</h1>
      <div class="iconClose"
           onclick={fn _ => stopPropagation; closeAction}></div>
    </div>
    <div class="dialogContent">
      {inner}
    </div>
  </xml>

datatype infoMessage
    = IMNone
    | IMText of string
    | IMError of (string * string)

(* опциональное сообщение -- загрузка, и т.д. *)
val infoMessageAtTheTop : transaction infoMessageAtTheTop =
    im <- source IMNone;
    let fun msg c inner =
            <xml><div class={c Css.infoMsgContainer}>
              <div class={Css.infoMsgText}>{inner}</div>
            </div></xml>
    in
    return
        { Html = dyn_
            (i <- signal im;
             return (case i of
               | IMNone => <xml/>
               | IMText t => msg id (txt t)
               | IMError (c, t) => msg (classes Css.errorMsg)
                 (dialog c (set im IMNone) <xml><p>{[Js.trim t]}</p></xml>)))
        , Show = fn t => set im (IMText t)
        , Error = fn c t => set im (IMError (c, t))
        , Hide = set im IMNone
        }
    end

fun showInfo [a]
             text
             (infoMessage : infoMessageAtTheTop)
             (act : transaction a) : transaction a =
    infoMessage.Show text;
    r <- act;
    infoMessage.Hide;
    return r

fun textWithLink (link : option url) (text : xbody) : xbody =
    case link of
        None => text
      | Some l => hrefLink text (show l)

fun buttonSymbol cls = spanClass (classes cls Css.buttonIcon) <xml/>
fun iconButtonNoOnclick cls title =
    Js.withDummyOnclick
    <xml><a class={classes cls Css.buttonIcon} title={title}></a></xml>
fun iconCheckIfS s =
    <xml><span dynClass={
       x <- s;
       return (classes Css.buttonIcon
                       (if x then Css.iconCheck else Css.iconEmpty))}
    ></span></xml>

fun buttonNoT' bcls cls title click =
    <xml><a onclick={fn _ => click} class={classes Css.button bcls} title={title}>{buttonSymbol cls}</a></xml>
fun buttonText (t : string) : xbody =
    <xml><span class={Css.buttonText} dir="auto">{[t]}</span></xml>
fun buttonT' bcls cls (name:string) title click =
    <xml><a onclick={fn _ => click} class={classes Css.button bcls} title={title}>{buttonSymbol cls}{buttonText name}</a></xml>
val buttonT = buttonT' null
fun button cls name click = buttonT cls name "" click
fun textButton'' cls (name:string) (title:string) click : xbody =
    <xml><span onclick={fn _ => click} class={classes cls Css.textButton} title={title}>{buttonText name}</span></xml>
fun textButton' cls (name:string) click : xbody =
    <xml><span onclick={fn _ => click} class={classes cls Css.textButton}>{buttonText name}</span></xml>
fun textButton (name:string) click : xbody =
    <xml><span onclick={fn _ => click} class={Css.textButton}>{buttonText name}</span></xml>
fun linkButton (name:string) click : xbody =
    <xml><a onclick={fn _ => click} class={Css.linkButton}>{[name]}</a></xml>

fun oneLineInputAndOkButton tid text placeholder buttonTitle action =
    <xml><div class={Css.oneLineInputAndButton}>
      {divClass Css.oneLineInputAndButtonBorder <xml>
      <ctextbox id={tid} source={text} placeholder={placeholder}
                class="oneLineInputAndButtonInput" dir={Js.dirAuto}
                onkeydown={fn k =>
                              a <- Js.isAutocompleteActive tid;
                              when (not a) (onEnter action k)} />
      </xml>}
      {textButton' Css.oneLineInputAndButtonButton buttonTitle action}
    </div></xml>

val show_id : show id = mkShow Js.showId
val show_css_class : show css_class = mkShow Js.show_css_class
val id_eq : eq id = mkEq Js.eq_id
(* val xmlEq [ctx] [a] [b] : eq (xml ctx a b) = *)
(*     mkEq (fn a b => Unsafe.fromXml a = Unsafe.fromXml b) *)
val option_eq [a] (ea : eq a) : eq (option a) =
    mkEq (fn a b => case (a, b) of
      | (None, None) => True
      | (Some a, Some b) => a = b
      | _ => False)
val list_eq [a] (ea : eq a) : eq (list a) =
    let fun e a b = case (a, b) of
        | ([], []) => True
        | ([], _) => False
        | (_, []) => False
        | (a :: as, b :: bs) => a = b && e as bs
    in
        mkEq e
    end
val xbody_eq : eq xbody =
    mkEq (fn a b => Unsafe.fromXml a = Unsafe.fromXml b)
val tuple_eq [a] [b] (ea : eq a) (eb : eq b) : eq (a*b) =
    mkEq (fn (a1,a2) (b1,b2) => a1 = b1 && a2 = b2)
val option_ord [a] (_ : ord a) : ord (option a) =
    mkOrd
    { Lt = fn a b => case (a,b) of
        | (None, None) => False
        | (None, Some _) => True
        | (Some _, None) => False
        | (Some a, Some b) => a < b
    , Le = fn a b => case (a,b) of
        | (None, _) => True
        | (Some _, None) => False
        | (Some a, Some b) => a <= b
    }
val tuple_ord [a] [b] (_ : ord a) (_ : ord b) : ord (a * b) =
    mkOrd
    { Lt = fn (a1,a2) (b1,b2) =>
              a1 < b1 || (not (a1 > b1) && a2 < b2)
    , Le = fn (a1,a2) (b1,b2) =>
              a1 < b1 || (not (a1 > b1) && a2 <= b2)
    }

val login_eq : eq loginType = mkEq (fn a b => case (a, b) of
    | (LTGoogle a, LTGoogle b) => a.Email = b.Email
    | (LTFacebook a, LTFacebook b) => a.Email = b.Email
    | (LTTwitter a, LTTwitter b) => a.Id = b.Id
    | (LTOpenId a, LTOpenId b) => a.URL = b.URL
    | (LTEmail a, LTEmail b) => a.Email = b.Email
    | (LTUsername a, LTUsername b) => a.Username = b.Username
    | (LTFeverApiKey a, LTFeverApiKey b) => a.ApiKey = b.ApiKey
    | _ => False)

fun svgIcon (href : css_class) = Unsafe.toXml ("<svg><use xlink:href=#icon-" ^ show href ^ " /></svg>")
(* пока нет svg hinting, svg иконки бесполезны -- невозможно выровнять
   положение <svg>-элемента по пиксельной сетке.
 *)
val redirectToMain : transaction page = redirect (bless "/")

(* В локальном времени браузера *)
fun formatTimeOnClient' fmt t : xbody =
    <xml><active code={return (txt (Js.strReplace "  " " " (Js.strftime fmt t)))} /></xml>
    (* %e добавляет пробел в начале, если дата состоит из одного разряда *)
fun formatTimeOnClient t : xbody = formatTimeOnClient' "%k:%M, %A, %B %e, %Y" t
    (* "%a %d %b %Y %T"        ->  Tue 21 Jun 2016 03:06:34" *)
    (* "%k:%M, %A, %B %e, %Y"  ->  3:06, Tuesday, June 21, 2016 *)
fun activeXml (x : transaction xbody) : xbody = <xml><active code={x} /></xml>
fun activeCode (x : transaction {}) : xbody = activeXml (x; return <xml/>)
fun autoFocus i = activeCode (Js.select i; Js.focus i)

fun plural x t = if x <> 1 then t ^ "s" else t

fun showBrowserType bt = case bt of
  | BTUnknown => "Unknown"
  | BTAndroid => "Android"
  | BTIPhone => "iPhone"
  | BTIPad => "iPad"
  | BTIPod => "iPod"
  | BTChrome => "Chrome"
  | BTIE => "IE"
  | BTIEMobile => "IEMobile"
  | BTSafari => "Safari"
  | BTOpera => "Opera"
  | BTOperaMini => "OperaMini"
  | BTFirefox => "Firefox"
  | BTVivaldi => "Vivaldi"
  | BTEdge => "Edge"

fun showAppType at = case at of
  | ATUnknown => "Unknown"
  | ATFeeddler => "Feeddler"
  | ATMrReader => "Mr. Reader"
  | ATReeder => "Reeder"
  | ATSlowFeeds => "SlowFeeds"
  | ATJustReader => "JustReader"
  | ATNewsPlus => "News+"
  | ATPress => "Press"
  | ATVienna => "Vienna"
  | ATReadKit => "ReadKit"
  | ATNewsJet => "NewsJet"
  | ATAmber => "Amber"
  | ATgzip => "gzip"
  | ATUnread => "Unread"
  | ATFeedMe => "FeedMe"
  | ATFieryFeeds => "FieryFeeds"
  | ATLire => "Lire"
  | ATWebSubscriber => "Web Subscriber"
  | ATReadably => "Readably"
  | ATokhttp => "okhttp"
  | ATFluentReader => "Fluent Reader"
  | ATRavenReader => "Raven Reader"
  | ATNetNewsWire => "NetNewsWire"
  | ATFocusReader => "FocusReader"
fun showOperatingSystem os = case os of
  | OSUnknown => "Unknown"
  | OSWindows => "Win"
  | OSMac => "Mac"
  | OSLinux => "Linux"
  | OSAndroid => "Android"
  | OSIOS => "iOS"
  | OSChromeOS => "ChromeOS"

fun showShareAction sa = case sa of
  | SAEMail => "EMail"
  | SATwitter => "Twitter"
  | SAFacebook => "Facebook"
  | SAGooglePlus => "GooglePlus"
  | SATumblr => "Tumblr"
  | SAEvernote => "Evernote"
  | SADelicious_discontinued => "Delicious"
  | SAPinboard => "Pinboard"
  | SAPocket => "Pocket"
  | SAReadability_discontinued => "Readability"
  | SAInstapaper => "Instapaper"
  | SATranslate => "Translate"
  | SABlogger => "Blogger"
  | SAWordpress => "Wordpress"
  | SALinkedIn => "LinkedIn"
  | SAPinterest => "Pinterest"
  | SAVK => "VK"
  | SASkype => "Skype"
  | SAReddit => "Reddit"
  | SAStumbleUpon => "StumbleUpon"
  | SADigg => "Digg"
  | SAScoopIt => "ScoopIt"
  | SAFlipboard => "Flipboard"
  | SABuffer => "Buffer"
  | SANewsVine => "NewsVine"
  | SADiigo => "Diigo"
  | SARememberTheMilk => "RememberTheMilk"
  | SAGoogleBookmarks => "GoogleBookmarks"
  | SAWallabag => "Wallabag"
  | SAWakelet => "Wakelet"
  | SACustom
    { CustomShareAction = c
    } => "CustomShareAction"
  | SASystem => "System"

fun showUsageFlag uf = case uf of
  | UFWeb
    { BrowserType     = bt
    , OperatingSystem = os
    } => showBrowserType bt ^ "/" ^ showOperatingSystem os
  | UFApp
    { AppType         = ATReeder
    , OperatingSystem = OSMac
    } => "Reeder/Mac"
  | UFApp
    { AppType         = at
    , OperatingSystem = _
    } => showAppType at (* все, кроме Reeder, работают на одной платформе *)
  | UFShareAction
    { ShareAction     = sa
    } => showShareAction sa
  | UFOPML => "OPML"
  | UFAddSubscription => "AddSubscription"
  | UFSearchSubscriptions => "SearchSubscriptions"
  | UFDiscoverySubscription => "DiscoverySubscription"
  | UFAddDiscoverySubscription => "AddDiscoverySubscription"
  | UFUnsubscribe => "Unsubscribe"
  | UFRetrySubscription => "RetrySubscription"
  | UFRenameSubscription => "RenameSubscription"
  | UFRenameFolder => "RenameFolder"
  | UFEditSubscriptionFolders => "EditSubscriptionFolders"
  | UFDragAndDrop => "DragAndDrop"
  | UFSearch => "Search"
  | UFSearchTags => "SearchTags"
  | UFSkip => "Skip"
  | UFIgnore => "Ignore"
  | UFKeepUnread => "KeepUnread"
  | UFMarkAllAsRead => "MarkAllAsRead"
  | UFStar => "Star"
  | UFTag => "Tag"
  | UFReadability => "Readability"
  | UFSetUsername => "SetUsername"
  | UFEnablePublicFeed => "EnablePublicFeed"
  | UFDisablePublicFeed => "DisablePublicFeed"
  | UFGenerateNewPublicFeed => "GenerateNewPublicFeed"
  | UFDeleteAccount => "DeleteAccount"
  | UFExportOPML => "ExportOPML"
  | UFMarkAllAsReadD
    { OlderThan       = d
    } => "MarkAllAsReadD " ^ show d
  | UFMarkSearchAsReadD
    { OlderThan       = d
    } => "MarkSearchAsReadD" ^ show d
  | UFFilterApply => "FilterApply"
  | UFFilterHide => "FilterHide"
  | UFNewSmartStream => "NewSmartStream"
  | UFEditFilter => "EditFilter"
  | UFEditSmartStream => "EditSmartStream"
  | UFDeleteFilter => "DeleteFilter"
  | UFDeleteSmartStream => "DeleteSmartStream"
  | UFWhatsNewClick { Time = t } => "WhatsNewClick"
  | UFWhatsNewClose { Time = t } => "WhatsNewClose"
  | UFThemeChange { ThemeName = t } => "ThemeChange " ^ t
  | UFFontChange { FontName = f } => "FontChange " ^ f
  | UFFontSizeChange { Size = s } => "FontSizeChange " ^ show s
  | UFLineHeightChange { Pixels = p, FontSize = fs } =>
    "LineHeightChange " ^ show p ^ " " ^ show fs
  | UFSetPassword => "UFSetPassword"
  | UFSetEmail => "UFSetEmail"
  | UFMarkReadAbove => "UFMarkReadAbove"
  | UFMarkReadBelow => "UFMarkReadBelow"
  | UFUnstarAbove => "UFUnstarAbove"
  | UFUnstarBelow => "UFUnstarBelow"
  | UFUntagAbove => "UFUntagAbove"
  | UFUntagBelow => "UFUntagBelow"
