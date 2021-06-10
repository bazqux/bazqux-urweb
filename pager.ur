open Utils
(* структура, разбивающая html на страницы и умеющая прятать
   ставшие невидимыми в результате прокрутки страницы, чтобы убрать тормоза
   браузера
 *)

val pagerDisabled = False

datatype pageState
  = Visible
  | Hidden of float

con page
  = { Num : int
    , Xml : xbody (* для последующего восстановления *)
    , State : source pageState
    , DomId : Basis.id
    , Ids : source (list Basis.id)
    , Invalidated : source bool
    }
(* когда прячем page, задаем min-height, чтобы не уменьшался при отображении
 *)

con idInfo
  = { PageNum : int
    , Top : float (* относительно верхка страницы *)
    , Height : float
    }

con t
  = { Above : source (list page)   (* все невидимые страницы сверху *)
    , Visible : source (list page) (* все видимые страницы *)
    , Below : source (list page)   (* все невидимые страницы снизу *)
(*
  (но есть вопрос с открытым list view -- его страница должна быть видимой
   чтобы уметь его схлопывать, еще нужно страницу с курсором
   и следующим/предыдущим уметь держать открытой.
   --> проще иметь доступ из uim к странице и при пробегании курсора
   (или сворачивании страницы) показывать этот uim/а потом снова прятать
   а развернутый list view item прятать, как и все остальное

   еще groupby feed куда-то надо девать?
   вне страниц?
  )
 *)
    , Xml : source xbody
    , HeightAbove : source float
    , HeightBelow : source float
    , DivId : Basis.id
    , ParentDivId : Basis.id
    , ParentDivIdRect : transaction Js.rect
    , CurrentPageDomId : source (option Basis.id)
    , CurrentPageXml : source (list xbody)
    , CurrentPageDomIds : source (list Basis.id)
    , PageNum : source int
    , Pages : HashMap.t page
    , Ids : HashMap.t idInfo
    , SavedIdHeights : HashMap.t float
    , PrevWidth : source float
    , PrevScrollTop : source float
    , AppendingPages : source (list int) (* отсортированный *)
    , AppendingPagesIds : HashMap.t HashSet.t (* pageNum -> [ids] *)
    }

fun pid (p : page) = p.Num
fun pDomId (p : page) = p.DomId

fun showPid (p : page) : string = show (pid p) ^ ":" ^ Js.showId (pDomId p)

ffi appendToPage : Basis.id -> Basis.id -> xbody -> transaction {}
ffi hidePage : Basis.id -> transaction {}
ffi showPage : Basis.id -> xbody -> transaction {}

fun getCurrentPageDomId p =
    cp <- get p.CurrentPageDomId;
    case cp of
      | Some r => return r
      | None =>
        r <- fresh;
        appendToPage p.DivId r <xml/>; (* чтобы добавить новый div *)
        set p.CurrentPageDomId (Some r);
        return r

fun currentPageNum p =
    _ <- getCurrentPageDomId p;
    get p.PageNum

fun append (p : t) x =
    modify p.CurrentPageXml (cons x);
    cp <- getCurrentPageDomId p;
    appendToPage p.DivId cp x

fun appendId (p : t) i =
    pi <- currentPageNum p;
    HashMap.insert p.Ids (Js.showId i) { PageNum = pi, Top = 0.0, Height = 0.0 };
    modify p.CurrentPageDomIds (cons i)

fun reset (p : t) =
    set p.Above [];
    (* корректно убираем все urweb closures через hidePage *)
    v <- get p.Visible;
    List.app (fn p => hidePage (pDomId p)) v;
    cp <- get p.CurrentPageDomId;
    set p.CurrentPageDomId None;
    withSome (fn p => hidePage p) cp;
    set p.Visible [];
    set p.Below [];
    set p.HeightAbove 0.0;
    set p.HeightBelow 0.0;
    set p.CurrentPageXml [];
    set p.CurrentPageDomIds [];
    set p.PageNum 0;
    set p.PrevWidth 0.0;        (* не нужно, но пусть будет *)
    set p.PrevScrollTop (-1.0);
    set p.AppendingPages [];
    HashMap.clear p.Pages;
    HashMap.clear p.Ids;
    HashMap.clear p.SavedIdHeights;
    HashMap.clear p.AppendingPagesIds;
    set p.Xml <xml></xml>; (* иначе sv() считает, что значение не изменилось *)
    set p.Xml <xml><div id={p.DivId}></div></xml>

fun new parentDivId parentDivIdRect : transaction t =
    above <- source [];
    visible <- source [];
    below <- source [];
    ha <- source 0.0;
    hb <- source 0.0;
    pnum <- source 0;
    divId <- fresh;
    cp <- source None;
    cpx <- source [];
    cpi <- source [];
    x <- source <xml></xml>;
    pages <- HashMap.new ();
    ids <- HashMap.new ();
    idHeights <- HashMap.new ();
    prevWidth <- source 0.0;
    prevScrollTop <- source (-1.0);
    appendingPages <- source [];
    appendingPagesIds <- HashMap.new ();
    let val p =
            { Above = above
            , Visible = visible
            , Below = below
            , HeightAbove = ha
            , HeightBelow = hb
            , DivId = divId
            , ParentDivId = parentDivId
            , ParentDivIdRect = parentDivIdRect
            , Xml = x
            , CurrentPageDomId = cp
            , CurrentPageXml = cpx
            , CurrentPageDomIds = cpi
            , PageNum = pnum
            , Pages = pages
            , Ids = ids
            , SavedIdHeights = idHeights
            , PrevWidth = prevWidth
            , PrevScrollTop = prevScrollTop
            , AppendingPages = appendingPages
            , AppendingPagesIds = appendingPagesIds
            }
    in
        reset p;
        return p
    end

fun html (p : t) = dyn_ (signal p.Xml)

fun setPaddingTop x p = Js.css x "padding-top" (show p ^ "px")
fun setMarginTop x p = Js.css x "margin-top" (show p ^ "px")
fun resetMinHeight page = Js.css page.DomId "min-height" "auto"
fun setMinHeight page x = Js.css page.DomId "min-height" (show x ^ "px")

fun currentPageDivTop p =
    cp <- getCurrentPageDomId p;
    divTop <- Js.boundingClientRectTop p.DivId;
    r <- Js.boundingClientRectTop cp;
    return (r - divTop)

fun updateHeightBelow p =
    bh <- get p.HeightBelow;
    cp <- getCurrentPageDomId p;
    setMarginTop cp bh

fun updateHeightAbove p =
    ha <- get p.HeightAbove;
    setPaddingTop p.DivId ha

fun toRelative' divTop r =
    { Top = r.Top - divTop, Bottom = r.Bottom - divTop }

fun getIdsRects (p : page) =
    i <- get p.Ids;
    List.mapM (fn i =>
                  r <- Js.boundingClientRect i;
                  return (i,r)) i

fun hide r p pager idRects =
    set p.State (Hidden (r.Bottom - r.Top));
(*     debug ("Hide " ^ showPid p ^ "; top = " ^ show (toRelative' divTop r).Top ^ "; bottom = " ^ show (toRelative' divTop r).Bottom); *)
    List.app (fn (i,ir) =>
(*                  debug ("hide " ^ Js.showId i ^ " " ^ show ir.Top ^ " - " ^ show r.Top); *)
                 HashMap.insert pager.Ids (Js.showId i)
                     { PageNum = p.Num
                     , Top = ir.Top - r.Top
                     , Height = ir.Bottom - ir.Top }) idRects;
    hidePage (pDomId p)

val reserved = 2000.0 (*parent.Bottom - parent.Top*)

fun checkInvariants (p : t) =
    divTop <- Js.boundingClientRectTop p.DivId;
    let fun err [a] m : a =
            trace ("checkInvariants: " ^ m)
            (fail ("checkInvariants: " ^ m))
        fun go prevBottom ps = case ps of
              | [] => return prevBottom
              | (x :: xs) =>
                s <- get x.State;
                case s of
                  | Visible =>
                    r <- Monad.mp (toRelative' divTop)
                                  (Js.boundingClientRect (pDomId x));
                    if r.Top <> prevBottom then
                        err ("page #" ^ showPid x ^ ": top = " ^ show r.Top ^ " <> " ^ show prevBottom)
                    else
                        go r.Bottom xs
                  | Hidden h =>
                    go (prevBottom + h) xs
        fun checkSuccession n ps = case ps of
              | [] => return ()
              | (x :: xs) =>
                if pid x <> n then
                    err ("page #" ^ showPid x ^ ": num " ^ show (pid x) ^ " <> " ^ show n)
                else
                    checkSuccession (n+1) xs
    in
        a <- get p.Above;
        v <- get p.Visible;
        b <- get p.Below;
        Js.forceImpure (checkSuccession 0 (List.revAppend a (List.append v b)));
        ha <- go 0.0 (List.rev a);
        haSet <- get p.HeightAbove;
        when (ha <> haSet)
             (err ("ha = " ^ show ha ^ " <> haSet = " ^ show haSet));
        vb <- go ha v;
        hb <- go 0.0 b;
        hbSet <- get p.HeightBelow;
        when (hb <> hbSet)
             (err ("hb = " ^ show hb ^ " <> hbSet = " ^ show hbSet));
        cp <- getCurrentPageDomId p;
        cpr <- Monad.mp (toRelative' divTop) (Js.boundingClientRect cp);
        when (cpr.Top <> hb+vb)
             (err ("cp.Top = " ^ show cpr.Top ^ " <> hb+vb = " ^ show (hb+vb)))
    end

fun invalidate (p : t) =
    set p.PrevScrollTop (-1.0);
    bind (get p.Above) (List.app (fn p => set p.Invalidated True));
    bind (get p.Below) (List.app (fn p => set p.Invalidated True));
    bind (get p.Visible) (List.app resetMinHeight)

(* Восстановление инвариантов above/visible/below.
 *)
fun scroll (p : t) = (* measure "Pager.scroll" *) (
    fs <- Js.isFullScreen;
    if fs || pagerDisabled then return False else
    parent <- p.ParentDivIdRect;
    pw <- get p.PrevWidth;
    when (pw <> parent.Right - parent.Left)
         (set p.PrevWidth (parent.Right - parent.Left);
          invalidate p);
    divTop <- Js.boundingClientRectTop p.DivId;
    divHeight <- Js.boundingClientRectHeight p.DivId;
    cpTop <- bind (getCurrentPageDomId p) Js.boundingClientRectTop;
    above0 <- get p.Above;
    below0 <- get p.Below;
    visible0 <- get p.Visible;
    appendingPages <- get p.AppendingPages;
    let fun splitAt cmp acc offs up pages = case pages of
          | [] => return (acc, offs, pages)
          | p :: ps =>
            s <- get p.State;
            r <- (case s of
               | Visible =>
                 r <- Js.boundingClientRect (pDomId p);
                 return { Top = r.Top, Bottom = r.Bottom }
               | Hidden h =>
                 return (if up
                         then { Top = offs - h, Bottom = offs }
                         else { Top = offs, Bottom = offs + h }));
            let val offs' = if up then r.Top else r.Bottom
            in
                if cmp r p.Num then
                    return (acc, offs', pages)
                else
                    splitAt cmp (p :: acc) offs' up ps
            end
        (* прячем ставшие невидимыми страницы до первой уже спрятанной *)
        fun tryHide height = List.mapM (fn page =>
            s <- get page.State;
            (case s of
               | Visible =>
                 r <- Js.boundingClientRect (pDomId page);
                 idRects <- getIdsRects page;
                 modify height (plus (r.Bottom - r.Top));
                 (* для производительности важно накопить координаты,
                    а потом прятать *)
                 return (hide r page p idRects)
               | Hidden h =>
                 return (return ())))
        (* показываем ставшие видимыми страницы до первой уже видимой *)
        fun tryShow height = List.mapPartialM (fn p =>
            s <- get p.State;
            case s of
              | Visible =>
                return None
              | Hidden h =>
                (* debug ("Show " ^ showPid p ^ "; height = " ^ show h); *)
                i <- get p.Invalidated;
                set p.Invalidated False;
                (if i then resetMinHeight p else setMinHeight p h);
                showPage (pDomId p) p.Xml;
                modify height (fn h0 => h0 - h);
                set p.State Visible;
                if i then
                    return (Some (pDomId p, h))
                else
                    return None)
        fun heightDiff xs =
            List.foldlM (fn (domId, h) acc =>
                            r <- Js.boundingClientRect domId;
                            return (acc + (r.Bottom - r.Top) - h)) 0.0 xs
        fun moveHeight a b = List.app (fn p =>
            s <- get p.State;
            case s of
              | Visible =>
                return ()
              | Hidden h =>
                modify a (fn h0 => h0 - h);
                modify b (fn h0 => h0 + h))
        val belowTopCmp =
            case appendingPages of
              | []  =>     fn r _ => r.Bottom < parent.Top-reserved
              | tn :: _ => fn r n => r.Bottom < parent.Top-reserved && n < tn
        val belowTop    = splitAt belowTopCmp []
        val aboveTop    = splitAt (fn r n => not (belowTopCmp r n)) []
        val aboveBottom = splitAt (fn r _ => r.Top >= parent.Bottom) []
    in
(*         debug (case appendingPages of *)
(*               | []  => "appendingPages is empty" *)
(*               | tn :: _ => "top appendingPage is " ^ show tn); *)
        aboveOffs <- (case visible0 of
          | (v :: _) =>
            rv <- Js.boundingClientRect (pDomId v);
            return rv.Top
          | _ =>
            return cpTop);
        (a_belowTop, a_aoffs, a_aboveTop) <- belowTop aboveOffs True above0;
        (a_visible, _, a_belowBottom) <- aboveBottom a_aoffs False a_belowTop;
        belowOffs <- (case List.rev visible0 of
          | (v :: _) =>
            rv <- Js.boundingClientRect (pDomId v);
            return rv.Bottom
          | _ =>
            return cpTop);
        (b_aboveBottom, b_boffs, b_belowBottom) <- aboveBottom belowOffs False below0;
        (b_visible, _, b_aboveTop) <- belowTop b_boffs True b_aboveBottom;
        (* для visible смещения уже не нужны *)
        (v_aboveTop, _, v_belowTop) <- aboveTop 0.0 False visible0;
        (v_visible, _, v_belowBottom) <- aboveBottom 0.0 False v_belowTop;
        let val above =
                List.append b_aboveTop (List.append v_aboveTop a_aboveTop)
            val below0 =
                List.append a_belowBottom (List.append v_belowBottom b_belowBottom)
            val visible0 =
                List.revAppend a_visible (List.revAppend v_visible b_visible)
            fun ids x = intercalate "," (List.mp showPid x)
            fun collectHeight hLeft acc pages =
                case pages of
                  | x :: xs =>
                    s <- get x.State;
                    h <- (case s of
                      | Visible => Js.boundingClientRectHeight (pDomId x)
                      | Hidden h => return h);
                    if h >= hLeft then
                        return (Some (pid x, List.rev (x :: acc), xs))
                    else
                        collectHeight (hLeft - h) (x :: acc) xs
                  | _ =>
                    pn <- get p.PageNum;
                    return (Some (pn, List.rev acc, []))
        in
            (* набираем N страниц после последней видимой так,
               чтобы их общая высота была > reserved, таким образом,
               при прокрутке на следующий пост или сворачивании развернутого
               list view item внизу будет достаточно уже видимого места
             *)
            ch <- collectHeight reserved [] below0;
        let fun swapTillNum n acc b vis =
                case b of
                  | x :: xs =>
                    if pid x <= n then
                        swapTillNum n (x :: acc) xs vis
                    else
                        (b, List.append vis (List.rev acc))
                  | _ =>
                    (b, List.append vis (List.rev acc))
            val (below, visible, swap) =
                case ch of
                  | Some (lastId, vis', below') =>
                    (below', List.append visible0 vis',
                     swapTillNum lastId [])
                  | _ => (below0, visible0, fn a b => (a, b))
             val (a_belowBottom, a_visible) = swap a_belowBottom a_visible
             val (v_belowBottom, v_visible) = swap v_belowBottom v_visible
             val (b_belowBottom, b_visible) = swap b_belowBottom b_visible
        in
            h1 <- tryHide p.HeightAbove v_aboveTop;
            h2 <- tryHide p.HeightBelow v_belowBottom;
            moveHeight p.HeightBelow p.HeightAbove b_aboveTop;
            moveHeight p.HeightAbove p.HeightBelow a_belowBottom;
            ha <- (* measure "Pager.scroll/withScrollSaved" *) (
                (if notNull h1 || notNull h2 then
                 withScrollSaved p.ParentDivId
                 (* если проигрывается YouTube video, то Chrome прокручивает
                    наверх при его убирании из DOM в hidePage *)
             else id) ((* measure "Pager.scroll/perform" *) (
            List.app id h1;
            List.app id h2;
            ha <- tryShow p.HeightAbove a_visible;
            _ <- tryShow p.HeightBelow b_visible;
            updateHeightBelow p;
            updateHeightAbove p;
            return ha)));

            hd <- heightDiff ha;

            (* measure "Pager.scroll/hd<>0.0" *) (
            when (hd <> 0.0)
                 (st <- Js.scrollTop p.ParentDivId;
                  sh <- Js.scrollHeight p.ParentDivId;
                  r <- p.ParentDivIdRect;
                  if st + (r.Bottom - r.Top) >= sh then
                      return ()
(*                       debug "Fixing scrollTop cancelled -- at the bottom" *)
                  else
(*                       debug ("Fixing scrollTop by " ^ show hd ^ " (was " ^ show st ^ " -> " ^ show (st+hd) ^ ")"); *)
                      Js.setScrollTopNoOnScroll p.ParentDivId (st + hd));
(*             hb <- get p.HeightBelow; *)
(*             ha <- get p.HeightAbove; *)

(*             debug ("above = " ^ ids above ^ ";\nvisible = " ^ ids visible ^ ";\nbelow = " ^ ids below ^ "\nheightAbove = " ^ show ha  ^ "\nheightBelow = " ^ show hb); *)
            set p.Above above;
            set p.Visible visible;
            set p.Below below;
            st <- Js.scrollTop p.ParentDivId;
            set p.PrevScrollTop st;
            return (hd <> 0.0))
(*             checkInvariants p *)
    end end end)

fun tryScroll (p : t) =
    st <- Js.scrollTop p.ParentDivId;
    pst <- get p.PrevScrollTop;
    when (abs (st - pst) > reserved*0.75) (void (scroll p))

fun realNewPage (p : t) =
    cpx <- get p.CurrentPageXml;
    cpi <- get p.CurrentPageDomIds;
    cpd <- getCurrentPageDomId p;
    state <- source Visible;
    set p.CurrentPageDomId None;
    set p.CurrentPageXml [];
    set p.CurrentPageDomIds [];
    pn <- get p.PageNum;
    set p.PageNum (pn + 1);
    ids <- source (List.rev cpi);
    inv <- source False;
    let val np : page =
            { Num = pn
            , Xml = List.mapX id (List.rev cpx)
            , State = state
            , DomId = cpd
            , Ids = ids
            , Invalidated = inv
            }
        fun hideNewPage var upd =
            r <- Js.boundingClientRect cpd;
            idRects <- getIdsRects np;
            hide r np p idRects;
            modify var (plus (r.Bottom - r.Top));
            upd p
    in
        HashMap.insert p.Pages (show pn) np;
        if pagerDisabled then return () else
        bh0 <- get p.HeightBelow;
        below <- get p.Below;
        (if notNull below then
            hideNewPage p.HeightBelow updateHeightBelow;
            modify p.Below (fn b => List.append b (np::[]))
        else
            modify p.Visible (fn b => List.append b (np::[])));
        when (bh0 <> 0.0)
             (setMarginTop cpd 0.0) (* после подсчета idRects в hideNewPage *)
    end

fun newPage (p : t) =
    cp <- get p.CurrentPageDomId;
    xml <- get p.CurrentPageXml;
    case (cp, xml) of
      | (Some cpi, _ :: _) =>
(*         r <- Js.boundingClientRect cpi; *)
(*         when (r.Bottom <> r.Top) *)
             (realNewPage p)    (* без проверки, чтобы лишних reflow не было *)
      | _ => return ()

fun withPage [a] (default : a) (f : page -> pageState -> transaction a) (p : t) (pid : int) : transaction a =
    r <- HashMap.lookup p.Pages (show pid);
    case r of
      | Some page =>
        s <- get page.State;
        f page s
      | None =>
        pn <- get p.PageNum;
        if pn = pid then
            s <- source Visible;
            ids <- source [];
            inv <- source False;
            cp <- getCurrentPageDomId p;
            f ({ Num = pn
               , DomId = cp
               , Xml = <xml/>
               , State = s
               , Ids = ids
               , Invalidated = inv
               } : page) Visible
        else
            return default

type check = Js.rect -> float -> float -> transaction bool

fun pageClientRectTop (check : check) p i = (* measure "Pager.pageClientRectTop" *) (withPage 0.0 (fn page s =>
(*     debug ("Pager.pageClientRectTop " ^ showPid page); *)
    case s of
      | Visible =>
        Js.boundingClientRectTop (pDomId page)
      | Hidden h =>
        parentRect <- Js.boundingClientRect p.ParentDivId;
        let fun search up t b a =
                c <- check parentRect t b;
                if not c then return t
                else
                case a of
                | [] => return t
                | (p::ps) =>
(*                   debug ("search " ^ show up ^ " " ^ show t); *)
                  s <- get p.State;
                  case s of
                    | Hidden h =>
                      if pid p = pid page then
                          let val r = if up then t-h else b
                          in
(*                               debug ("pageClientRectTop " ^ showPid page *)
(*                                      ^ " = " ^ show r); *)
                              return r
                          end
                      else if up then
                          search up (t-h) t ps
                      else
                          search up b (b+h) ps
                    | Visible =>
                      return t  (* не должно происходить *)
            fun goUp domId =
                r <- Js.boundingClientRect domId;
                a <- get p.Above;
                search True r.Top r.Bottom a
            fun goDown domId =
                r <- Js.boundingClientRect domId;
                b <- get p.Below;
                search False r.Top r.Bottom b
        in
            vis <- get p.Visible;
            case vis of
              | [] =>
                cpd <- getCurrentPageDomId p;
                goUp cpd
              | top :: _ =>
                if pid page < pid top then
                    goUp (pDomId top)
                else
                    case List.rev vis of
                      | [] => fail "empty list?"
                      | bottom :: _ =>
(*                         debug ("goDown from " ^ showPid bottom); *)
                        goDown (pDomId bottom)
        end
    ) p i)

fun pagePositionTop (check : check) p i =
    rt <- pageClientRectTop check p i;
    drt <- Js.boundingClientRectTop p.DivId;
    dpt <- Js.positionTop p.DivId;
    let val r = dpt + rt - drt
    in
(*         withPage () (fn page _ => *)
(*             debug ("pagePositionTop " ^ showPid page ^ " = " ^ show r)) *)
(*             p i; *)
        return r
    end

fun idTop jsf f p i =
    mbi <- HashMap.lookup p.Ids (Js.showId i);
    case mbi of
      | None =>
(*         debug ("idTop " ^ Js.showId i ^ " not found!"); *)
        jsf i
      | Some info =>
        withPage 0.0
            (fn page s =>
                case s of
                  | Visible =>
                    r <- jsf i;
(*                     debug ("idTop " ^ Js.showId i ^ " visible = " ^ show r); *)
                    return r
                  | Hidden _ =>
                    pt <- f p info.PageNum;
(*                     debug ("idTop " ^ Js.showId i ^ " " ^ show pt *)
(*                            ^ " + " ^ show info.Top *)
(*                            ^ " = " ^ show (pt + info.Top)); *)
                    return (pt + info.Top))
            p info.PageNum

fun idPositionTop' check = idTop Js.positionTop (pagePositionTop check)
fun idClientRectTop' check =
    idTop Js.boundingClientRectTop (pageClientRectTop check)
val idPositionTop = idPositionTop' (fn _ _ _ => return True)
val idClientRectTop = idClientRectTop' (fn _ _ _ => return True)

fun idClientRectHeight p i =
    mbi <- HashMap.lookup p.Ids (Js.showId i);
    case mbi of
      | None =>
        Js.boundingClientRectHeight i
      | Some info =>
        withPage 0.0
            (fn page s =>
                case s of
                  | Visible => Js.boundingClientRectHeight i
                  | Hidden _ => return info.Height)
            p info.PageNum

fun saveIdHeight p i =
    let val save =
            h <- Js.boundingClientRectHeight i;
            HashMap.insert p.SavedIdHeights (Js.showId i) h
    in
    mbi <- HashMap.lookup p.Ids (Js.showId i);
    case mbi of
      | None => save
      | Some info =>
        withPage ()
            (fn page s =>
                case s of
                  | Visible => save
                  | Hidden h => return ())
            p info.PageNum
    end

fun restoreIdHeight p i =
    mbih <- HashMap.lookup p.SavedIdHeights (Js.showId i);
    HashMap.delete p.SavedIdHeights (Js.showId i);
    mbi <- HashMap.lookup p.Ids (Js.showId i);
    case mbi of
      | None => return ()
      | Some info =>
        withPage ()
            (fn page s =>
                case (s, mbih) of
                  | (Hidden h, Some savedIdHeight) =>
                    let val diff = info.Height - savedIdHeight
                        val fixAbove =
                            modify p.HeightAbove (fn ha => ha - diff);
                            updateHeightAbove p
                        val fixBelow =
                            modify p.HeightBelow (fn hb => hb - diff);
                            updateHeightBelow p
                        fun fixIdTop pi =
                            mbi <- HashMap.lookup p.Ids (Js.showId pi);
                            withSome (fn i' =>
                                when (i'.Top > info.Top)
                                     (HashMap.insert p.Ids (Js.showId pi)
                                      (modifyF [#Top] (fn t => t - diff) i')))
                                mbi
                    in
                        set page.State (Hidden (h - diff));
                        HashMap.insert p.Ids (Js.showId i)
                            (setF [#Height] savedIdHeight info);
                        pIds <- get page.Ids;
                        List.app fixIdTop pIds;
                        vis <- get p.Visible;
                        case vis of
                          | [] => fixAbove
                          | x :: _ =>
                            if pid page < pid x then
                                fixAbove
                            else
                                fixBelow
                    end
                  | _ => return ())
            p info.PageNum

fun isIdOnVisiblePage p i =
    mbi <- HashMap.lookup p.Ids (Js.showId i);
    case mbi of
      | None => return True
      | Some info =>
        withPage True
            (fn page s =>
                case s of
                  | Visible => return True
                  | Hidden h => return False)
            p info.PageNum
fun isIdOnValidPage p i =
    mbi <- HashMap.lookup p.Ids (Js.showId i);
    case mbi of
      | None => return True
      | Some info =>
        withPage True (fn page s => Monad.mp not (get page.Invalidated))
                 p info.PageNum

fun appendChildId (p : t) pi i =
    mbi <- HashMap.lookup p.Ids (Js.showId pi);
    case mbi of
      | None =>
(*         debug ("appendChildId: parent id " ^ Js.showId pi ^ " not found for " ^ *)
(*                Js.showId i); *)
        appendId p i
      | Some info =>
        withPage ()
            (fn page s =>
                modify page.Ids (cons i);
                case s of
                  | Visible =>
                    HashMap.insert p.Ids (Js.showId i)
                    { PageNum = info.PageNum, Top = 0.0, Height = 0.0 }
                    (* чтобы дальше parent-ов определять *)
                  | Hidden h =>
                    HashMap.insert p.Ids (Js.showId i)
                    { PageNum = info.PageNum, Top = h, Height = 0.0 }
                    (* предполагаем, что добавится в конец.
                       в основном, страница, куда добавляются сообщения,
                       должна быть видна, так что это не столь принципиально
                     *)
            )
            p info.PageNum

fun resetPageHeight p i =
    mbi <- HashMap.lookup p.Ids (Js.showId i);
    case mbi of
      | None => return ()
      | Some info =>
        withPage ()
            (fn page s =>
                case s of
                  | Visible => resetMinHeight page
                  | Hidden h => return ())
            p info.PageNum

fun sortedSetInsert a l =
    case l of
      | [] => a :: []
      | x :: xs =>
        if a < x then a :: l
        else if a > x then x :: sortedSetInsert a xs
        else l (* a == x *)

fun setAppendingId (p : t) i a =
    mbi <- HashMap.lookup p.Ids (Js.showId i);
(*     debug ("setAppendingId " ^ Js.showId i ^ " " ^ show a); *)
    case mbi of
      | None =>
(*         debug "not found"; *)
        return ()
      | Some info =>
        mbids <- HashMap.lookup p.AppendingPagesIds (show info.PageNum);
        ids <- (case mbids of
          | None =>
            r <- HashSet.new ();
            HashMap.insert p.AppendingPagesIds (show info.PageNum) r;
            return r
          | Some r => return r);
        let fun modWhenEmpty f =
                s <- HashSet.size ids;
                when (s = 0)
                     (modify p.AppendingPages f)
        in
(*             debug ("found page #" ^ show info.PageNum); *)
            if a then
                modWhenEmpty (sortedSetInsert info.PageNum);
                HashSet.insert ids (Js.showId i)
            else
                HashSet.delete ids (Js.showId i);
                modWhenEmpty (List.filter (fn n => n <> info.PageNum))
        end

val test =
    r <- fresh;
    p <- new r (Js.boundingClientRect r);
    let fun loop n =
            append p <xml><div>Hello,</div></xml>;
            append p <xml><div>World</div></xml>;
            append p <xml><div>!!!</div></xml>;
            append p <xml><div>Testing large chunks</div></xml>;
            append p <xml><div>of html code</div></xml>;
(*             sleep 1000; *)
            newPage p;
            if n > 0 then
                loop (n - 1)
            else
                modify p.Visible List.rev
    in
        pageNoBody "pager test" <xml>
          <body onload={spawn (loop 10000)}>
            <div (* class={Css.right} -- теперь другая верстка *) id={r} onscroll={Js.requestAnimationFrameOnce "scroll" (void (scroll p))}>
              <h1>Some header here</h1>
              {html p}
            </div>
          </body>
        </xml>
    end
