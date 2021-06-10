open Utils
open Either

(* Id используем в toggle для определения того, что этот popup уже открыт *)
con popup = { Id : id, Xbody : xbody }
con position = {Top : int, Left : int}

fun popupId (p : popup) = p.Id
fun popupXbody (p : popup) = p.Xbody

val popupSource = Unsafe.xbodySource "Popups.popup" <xml/>
fun embed ps =
    <xml><div class="popup"><dyn signal={signal ps}/></div></xml>

type activePopup = Basis.id * source xbody

datatype state
  = NoPopup
  | Popup of activePopup * option activePopup
  | ExternalPopup of source bool

ffi stateSource jsFunc "unsafeSource" : string -> state -> source state

val state = stateSource "Popups.state" NoPopup

val isSubPopupActive =
    ps <- get state;
    return (case ps of Popup (_, Some _) => True | _ => False)
val hideSubPopup =
    ps <- get state;
    case ps of
      | Popup (p, Some (_, s)) =>
        set state (Popup (p, None));
        set s <xml/>
      | _ => return ()

val hide =
    let val close =
            set state NoPopup
    in
      ps <- get state;
      case ps of
        | Popup ((_, s), None) =>
          Js.hideContextMenu (set s <xml/>); close
        | Popup ((_, s), Some (_, ss)) =>
          set s <xml/>; set ss <xml/>; close
        | ExternalPopup e =>
          Js.enableMenuTransitions;
          set e False; close
        | _ => return ()
    end
val preShow =
    hide
fun show ps p =
    preShow;
    set state (Popup ((p.Id, ps), None));
    set ps p.Xbody;
    Js.adjustMenuPosition
fun toggleExternalPopup e =
    ea <- get e;
    if ea then
        hide
    else
        preShow;
        set e True;
        set state (ExternalPopup e);
        stopPropagation
fun isVisible p =
    v <- get state;
    return (case v of Popup ((i,_), _) => i = p.Id | _ => False)
val isActive =
    v <- get state;
    return (case v of NoPopup => False | _ => True)
fun toggle' ps stop p =
    v <- isVisible p;
    if v then
        hide
    else
        hide;
        show ps p;
        when stop stopPropagation
fun toggle x = toggle' popupSource True x
fun positionedXbody i position cls (inner : xbody) =
    <xml>
      <div
       id={i}
       class={cls}
       style={case position of
                | None => noStyle
                | Some (Left (p : position)) =>
                  prop2 "left" (Basis.show p.Left ^ "px")
                        "top" (Basis.show p.Top ^ "px")
                | Some (Right (p : position)) =>
                  prop2 "--right" (Basis.show p.Left ^ "px")
                        "top" (Basis.show p.Top ^ "px")
             }
       >{inner}</div></xml>
fun positioned add position cls (inner : xbody) =
    i <- fresh;
    return { Id = i, Xbody = <xml>{add}{positionedXbody i position cls inner}</xml> }
val new = positioned <xml/> None
fun newBox' shadow cls title inner =
    positioned
      <xml><div class={shadow} onclick={fn _ => hide}></div></xml>
      None cls (dialog title hide inner)
fun newBoxC cls title (inner : xbody) =
    newBox' (classes cls Css.smallDialogBoxShadow) (classes cls Css.smallDialogBox) title inner
val newBox = newBoxC null
fun newBigBox title (inner : xbody) =
    newBox' Css.bigDialogBoxShadow Css.bigDialogBox title inner
fun newBigBoxWithSubPopup title inner =
    subBox <- source <xml/>;
    let fun toggle (sp : popup) =
            s <- get state;
            case s of
              | Popup (p, None) =>
                set subBox sp.Xbody;
                set state (Popup (p, Some (sp.Id, subBox)))
              | Popup (_, Some _) =>
                hideSubPopup
              | _ => return ()
    in
        b <- newBigBox title (inner toggle);
        return
        { Id = b.Id
        , Xbody = <xml>
            <span dynClass={s <- signal state;
                            return (case s of
                                      | Popup (_, Some _) => visibilityHidden
                                      | _ => null)}>
              {b.Xbody}
            </span>
            {dyn_ (signal subBox)}
          </xml>
        }
    end

fun newMenu c t x =
    m <- new (classes Css.menu c) (dialog t hide x);
    return { Id = m.Id, Xbody = m.Xbody }
fun newIdPosMenu i subId p c t x =
    let val id = Unsafe.id (Js.showId i ^ "-" ^ subId)
    in
        { Id = id
        , Xbody = positionedXbody id (Some p) (classes Css.menu c) (dialog t hide x) }
    end

val currentMousePosition =
    e <- Js.uw_mouseEvent;
    return { Left = e.ClientX - 1
             (* учитываем border, чтобы при перемещении мышки вниз сразу
                выделился элемент меню *)
           , Top = e.ClientY + 1
             (* чуть ниже, чтобы не терялось выделение текущей подписки *)
           }

fun toggleContextMenu id position content =
    p <- maybe (mp <- currentMousePosition; return (Left mp)) return position;
    toggle (newIdPosMenu id "contextMenu"
        p Css.contextMenu "" content)
fun showContextMenu position content =
    hide;
    toggleContextMenu (Unsafe.id "contextMenu") position content

fun hideAndAct act =
    Js.setSmoothHide False;
    hide;
    (* прячем меню тут, а не в глобальном onclick,
       т.к. меню по клику на него не закрывается, см. newMenu почему *)
    stopPropagation;
    (* убираем дальнейшие клики -- а то меню прячется и клик может прийти
       куда угодно, например, при назначении тега через меню, умудряется
       перейти на этот тег
     *)
    act
fun indentedLine (x : xbody) : xbody =
    <xml><div class={Css.indentedLine}>{buttonSymbol Css.iconEmpty}{x}</div></xml>
fun lii (icon : css_class) (t : string) (act : transaction {}) : xbody =
    <xml><a onclick={fn _ => hideAndAct act}><li>{buttonSymbol icon}{buttonText t}</li></a></xml>
(* iOS не подсвечивает нажатие на <li onclick>, но подсвечивает <a onclick> *)
fun lif (u : url) (t : string) (act : transaction {}) : xbody =
    <xml><a onclick={fn _ => hideAndAct act}><li>{Js.menuFavicon u} {buttonText t}</li></a></xml>
fun li_ (t : string) (act : transaction {}) : xbody =
    <xml><a onclick={fn _ => hideAndAct act}><li>{buttonText t}</li></a></xml>
fun llif act (t : string) (u : url) : xbody =
    actHrefLink (hideAndAct act) <xml><li>{Js.menuFavicon u} {buttonText t}</li></xml> (Basis.show u)
fun lli (t : string) (u : url) : xbody =
    actHrefLink (hideAndAct (return ())) <xml><li>{buttonText t}</li></xml> (Basis.show u)
fun llii (icon : css_class) (t : string) (u : url) : xbody =
    actHrefLink (hideAndAct (return ())) <xml><li>{buttonSymbol icon}{buttonText t}</li></xml> (Basis.show u)
fun menuLabel what =
    <xml>
      <hr/>
      <li class={Css.menuLabel}>
        <span class={Css.buttonText} dir="auto">{[what]}</span>
      </li>
    </xml>
fun liSub' what inner lix =
    <xml><span class="subMenu">
      <div class="subMenuLabel">{menuLabel what}</div>
      {inner}
      <div class="subMenuMenuItem">{lix}</div>
    </span></xml>
fun liSub (t : string) (i : xbody) : xbody =
    liSub' t i
    <xml><li>{[t]}{buttonSymbol Css.iconMenuRightArrow}</li></xml>
fun liSubI (icon : css_class) (t : string) (i : xbody) : xbody =
    liSub' t i
    <xml><li>{buttonSymbol icon}{buttonSymbol Css.iconMenuRightArrow}<span class={Css.buttonText}>{[t]}</span></li></xml>

val mouseMoved = Unsafe.boolSource "Popups.mouseMoved" False
val mouseDownX = Unsafe.intSource "Popups.mouseDownX" 0
val mouseDownY = Unsafe.intSource "Popups.mouseDownY" 0

val mouseMovedAfterMouseDown = get mouseMoved

val setup =
    onMousedown
        (fn e =>
            set mouseMoved False;
            set mouseDownX e.ScreenX;
            set mouseDownY e.ScreenY);
    onMousemove
        (fn e =>
            x <- get mouseDownX;
            y <- get mouseDownY;
            when (pow (x - e.ScreenX) 2 + pow (y - e.ScreenY) 2 > pow 5 2)
                 (set mouseMoved True));
    (*
    Edge выдает и mousedown и mousemove, а у некоторых
    пользователей происходит движение при нормальном клике, так что
    имеем запас. Не самое лучшее решение, если мышка сильно
    дергается -- все равно может не кликнуть. Но это меньшее зло.

    Пытался определять, выделился ли текст, но отказался от этого.
    выделение текста может сброситься при клике или появиться вновь
    по double click, т.е. при отсутсвии движения мышкой и желании
    закрыть/раскрыть сообщение.
    *)
    (* неважно, на что кликнем (кроме окошек), меню будет спрятано *)
    onClick
        (fn e =>
            p <- Js.clickedInPopup;
            case (p, e.Button) of
              | (False, Basis.Left) =>
                m <- mouseMovedAfterMouseDown;
                when (not m) hide
              | _ => return ())
    (* TODO: для ipad-а нужен ontouchstart,
     onmousedown ничем не отличается от onclick и также работает
     только на нодах, имеющих onclick
     http://stackoverflow.com/questions/8430568/jquery-onmouseover-onclick-for-touchscreen-users-ie-ipad
     *)

val li = li_
