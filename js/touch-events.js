// Обработка touch* событий:
// * Перемещение левой панели касанием
// * Отключение прокрутки под диалогами (имитация overscroll-behavior: contain)
//

import * as leftPanel from './left-panel'
import { $, scrollingElement, passiveListener,
         isMobile, supportsOverscrollContain, disableScrolling,
         blurActiveElement, safeAreaInset } from './vendor'
import { eventTargetClassesArray } from './history'
import { uimByElement } from './uim'

let popupsHide_ = null;
export function set_popupsHide(h) { popupsHide_ = h; }
export function popupsHide() { execF(popupsHide_); }

let showUimContextMenu_ = null;
export function set_showUimContextMenu(s) { showUimContextMenu_ = s }
function showUimContextMenu(uim, x, y) {
    execF(execF(execF(showUimContextMenu_, uim), { _Left : x, _Top : y }));
    setSmoothHide(true);
}

var touch = null;

const TouchState = {
    START : 1,
    CANCELLED : 2, // началась прокрутка, выделение текста или был long tap
    MOVING_LEFT_PANEL : 3,
    MOVING_CONTEXT_MENU : 4
}

let transitionsResetTimer = null;
function disableMenuTransitions() {
    $('.left').removeClass('menuLeftTransition');
    $('.leftPanelShadow').removeClass('menuShadowTransition');
    $(contextMenu()).removeClass('menuLeftTransition');
}
export function enableMenuTransitions() {
    const m = contextMenu();
    if ($(m).data('smoothHide')) {
        $(m).addClass('menuLeftTransition');
    }
    $('.left').addClass('menuLeftTransition');
    $('.leftPanelShadow').addClass('menuShadowTransition');
    if (transitionsResetTimer) {
        window.clearTimeout(transitionsResetTimer);
    }
    transitionsResetTimer = window.setTimeout(() => {
        disableMenuTransitions();
        transitionsResetTimer = null;
    }, 300);
}

function startMovingLeftPanel() {
    DEBUG('startMovingLeftPanel');
    addUnselectable();
    if (!leftPanel.isVisible()) {
        // popupsHide();
        // ^ нельзя, т.к. может исчезнуть элемент на котором начался
        // touchstart и дальнейшие touchmove/touchend не придут
        blurActiveElement();
    }
    touch.state = TouchState.MOVING_LEFT_PANEL;
    disableScrolling(".left", true);
    disableMenuTransitions();
    $('.leftPanelShadow').css('left', '0');
}
function startMovingContextMenu() {
    DEBUG('startMovingContextMenu');
    addUnselectable();
    disableMenuTransitions();
    blurActiveElement();
    touch.state = TouchState.MOVING_CONTEXT_MENU;
    disableScrolling(".menu", true);
}
function stopMoving() {
    if (touch.state == TouchState.MOVING_LEFT_PANEL) {
        enableMenuTransitions();
        $('.left, .leftPanelShadow').removeAttr('style');
        disableScrolling(".left", false);
        touch.state = TouchState.START;
    }
    else if (touch.state == TouchState.MOVING_CONTEXT_MENU) {
        enableMenuTransitions();
        disableScrolling(".menu", false);
        touch.state = TouchState.START;
    }
}
function moveLeftPanel(t) {
    DEBUG("moveLeftPanel()");
    DEBUG(t)
    const w = leftPanel.width();
    const newLeft = Math.min(0, Math.round(
        touch.moveLeftX
        ? t.clientX - touch.moveLeftX // двигаем влево
        : t.clientX - w));  // вправо
    DEBUG(`w = ${w}, newLeft = ${newLeft}`);
    $('.left').css('left', newLeft);
    $('.leftPanelShadow').css('opacity', (newLeft+w)/w*0.3);
}
function menuMargin() { return Math.max(safeAreaInset().right, 10) }
function moveContextMenu(t) {
    const m = contextMenu();
    const w = $(m).outerWidth() + menuMargin();
    const left0 = window.innerWidth - w;
    const newLeft = Math.round(
        touch.moveRightX
        ? Math.max(0, t.clientX - touch.moveRightX) + touch.moveRightMinLeft
          // двигаем вправо
        : Math.max(left0, t.clientX - contextMenuOffset()/3));  // влево
    DEBUG(`moveContextMenu: newLeft = ${newLeft}px`);
    $(m).css({ left: newLeft + 'px' });
}

function DEBUG() {}
// function DEBUG(x) { uw_debug(x) }
// function DEBUG(x) {
//     uw_debug(x);
//     const d = document.createElement('div');
//     d.innerHTML = x;
//     const s = $('.left .subscriptions')[0];
//     s.appendChild(d);
//     s.scrollTop = s.scrollHeight;
// }

function reportTouches(a, e) {
    const t = e.changedTouches;
    const of = t.length > 1 ? '/' + t.length : '';
    for (var i = 0; i < t.length; i++)
        DEBUG(`${a} [${t[i].identifier}${of}] x = ${t[i].clientX}, y = ${t[i].clientY}`);
    if (t.length == 0)
        DEBUG(`${a} with no changedTouches`);
}

function findTouchInTouchList(t) {
    for (var i = 0; i < t.length; i++) {
        if (touch && touch.identifier == t[i].identifier)
            return t[i];
    }
    return null
}
function findTouch(e) { return findTouchInTouchList(e.changedTouches) }

function checkScrolling(touch) {
    if (touch.state == TouchState.START) {
        // pageYOffset тоже не меняется при инерционной прокрутке
//         if (touch.pageYOffset != window.pageYOffset) {
//             DEBUG('scrolling page y offset detected ('+ touch.pageYOffset + ' -> ' + window.pageYOffset + ')');
//             stopMoving();
//             touch.state = TouchState.CANCELLED;
//             return;
//         }
        for (var i in touch.scrollables) {
            const s = touch.scrollables[i];
            const dy = s.node.scrollTop - s.scrollTop;
            const dx = s.node.scrollLeft - s.scrollLeft;

//             if (dy != 0) {
//                 DEBUG(`scrollTop ${s.scrollTop} ${s.node.nodeName}/"${s.node.className}" ${s.node.scrollTop}`);
//             }
            if ((s.node == scrollingElement() ? Math.abs(dy) > 100 : dy != 0)
                // для определения swipe игнорируем небольшую прокрутку
                // сообщений, т.к. на iPad они очень легко прокручиваются
                // из-за чего практически невозможно сделать swipe.
                || dx != 0) {
                DEBUG('scrolling detected');
                touch.state = TouchState.CANCELLED;
                return;
            }
        }
    }
}

function clearLongTapTimeout() {
    if (touch && touch.longTapTimeout) {
        DEBUG("clearLongTapTimeout");
        window.clearTimeout(touch.longTapTimeout);
        touch.longTapTimeout = null;
    }
}
var longTapHandlers = [];
var removeUnselectableTimeoutId = null;
function addUnselectable() {
    window.getSelection().removeAllRanges();
    if (removeUnselectableTimeoutId) {
        window.clearTimeout(removeUnselectableTimeoutId);
    } else {
        $("body").addClass("unselectable");
        DEBUG("add unselectable");
    }
    removeUnselectableTimeoutId = window.setTimeout(removeUnselectable_, 1000);
    // примерно 750мсек достаточно
}
function removeUnselectable() {
    if (removeUnselectableTimeoutId) {
        window.clearTimeout(removeUnselectableTimeoutId);
        removeUnselectable_();
    }
}
function removeUnselectable_() {
    $("body").removeClass("unselectable");
    DEBUG("remove unselectable");
    removeUnselectableTimeoutId = null;
}
function onLongTapTimeout() {
    if (!touch || touch.state != TouchState.START)
        return;
    DEBUG("longtap");
    touch.longTapTimeout = null;
    const o = contextMenuOffset();
    for (const h of longTapHandlers) {
        if (h({ type : "longtap",
                target : touch.target,
                preventDefault : () => {},
                clientX : touch.x - o,
                clientY : touch.y - o
              })) {
            DEBUG("longtap handled");
            touch.longTapHandled = true;
            touch.state = TouchState.CANCELLED;
        }
    }
}
export function onLongTap(f) { longTapHandlers.push(f) }

document.addEventListener('touchstart', (e) => {
    uw_event = e;
    reportTouches('touchstart', e);
    if (e.touches.length > 1) {
        // не обрабатываем multitouch
        if (touch) {
            DEBUG('multitouch detected, ignoring touch');
            stopMoving();
            clearLongTapTimeout();
            touch = null;
            return;
        }
    }
    if (touch) {
        // сбрасываем состояние левой панели,
        // если вдруг исчез элемент DOM на котором был touchstart
        // (меню/диалог закрылись) и touchend не пришел
        stopMoving();
    }
//     if (touch && !findTouchInTouchList(e.touches)) {
//         DEBUG('old touch no longer exists, missed touchend?');
//         touch = null;
//         // в принципе, этот код не нужен, т.к. touch и так перезаписывается
//     }

    const t = e.changedTouches;
    if (t.length > 0) {
        var target = e.target;
        var scrollables = [];
        var draggingDisabled = false;

        while (target) {
            const nn = target.nodeName.toLowerCase();
            if (($(target).hasClass('button')
                 || $(target).hasClass('buttonIcon')
                 || $(target).hasClass('textButton')
                 || nn == "input" || nn == "label" || nn == "a")
                && t[0].clientX > 1)
            {
                // Не мешаем работе кнопок (не начинаем показывать левую
                // панель поверх звездочек и showLeftPanelButton)
                // но позволяем двигать панель от самого края.
                // Обработка touch продолжается, чтобы
                // останавливать прокрутку body в меню
                // (а то галочки в меню -- это тоже buttonIcon),
                // swipe также отрабатываем
                // (хотя и непонятно, как ось отделяет swipe от клика),
                // т.к. ссылка может занимать весь экран (если она поверх
                // изображения), да и хочется чтобы поверх кнопок toolbar-а
                // swipe тоже работал
                DEBUG('button or input detected, left panel moving disabled');
                draggingDisabled = true;
            }
            if (target.scrollHeight != target.clientHeight
                || target.scrollWidth != target.clientWidth) {
                scrollables.push(
                    { node: target,
                      scrollTop : target.scrollTop,
                      scrollLeft : target.scrollLeft
                    });
            }
            target = target.parentNode;
        }

        touch =
            { x : t[0].clientX,
              y : t[0].clientY,
              prevX : t[0].clientX,
              prevY : t[0].clientY,
              time : Date.now(),
              state : TouchState.START,
              scrollables : scrollables,
              identifier : t[0].identifier,
              pageYOffset : window.pageYOffset,
              event : e,
              target : e.target,
              contextMenu : contextMenu()
            };
        if (!draggingDisabled
            && leftPanel.isMovable() && !leftPanel.isVisible()
            && touch.x < 20 + safeAreaInset().left)
        {
            startMovingLeftPanel();
            moveLeftPanel(t[0]);
            e.preventDefault();
        } else {
            touch.longTapTimeout = window.setTimeout(onLongTapTimeout, 500);
            const c = eventTargetClassesArray();
            if (c.includes("postHeader") || c.includes("postImage")) {
                addUnselectable();
                // чтобы не выделяло заголовок под плиткой в mosaic
            }
        }
    }
}, passiveListener(false));

document.addEventListener('touchcancel', (e) => {
    reportTouches('touchcancel', e);
    touchEnd(e);
}, passiveListener(false));

document.addEventListener('touchend', (e) => {
    reportTouches('touchend', e);
    touchEnd(e);
}, passiveListener(false));

function touchEnd(e) {
    const t = findTouch(e);
    if (!t)
        return;

    if (touch.longTapHandled) {
        e.preventDefault();
        // чтобы не было клика по только что открытому меню
        return;
    }
    clearLongTapTimeout();
    removeUnselectable();

    checkScrolling(touch);
    if (touch.state != TouchState.CANCELLED) {
        const x = t.clientX;
        const dy = Math.abs(t.clientY - touch.y);
        const dx = Math.abs(x - touch.x);
        const dt = Date.now() - touch.time;
        let m = contextMenu();
        const showMenu = () => {
            enableMenuTransitions();
            $(m).css({ left: (touch.moveRightMinLeft || (window.innerWidth - $(m).outerWidth() - menuMargin())) + 'px' });
        }
        // определяем swipe
        DEBUG(`check swipe dx=${dx} dy=${dy} dx/dy=${dx/dy} dx/dt=${dx/dt}`);
        if (dt < 1000 && (dx > 30 || dx/dt >= 0.1) && dx/dy > 2
            && !touch.selectionChanged) {
            DEBUG(`swipe detected (x > touch.x) == ${x > touch.x}`);
            window.getSelection().removeAllRanges();
            // выделение текста видно поверх левой панели или контекстного меню
            if (x < touch.x && (!leftPanel.isMovable0() || !leftPanel.isVisible())) {
                const u = uimByElement(touch.target);
                if (u && !touch.contextMenu) {
                    if (!m) {
                        const o = contextMenuOffset();
                        showUimContextMenu(u, window.innerWidth, touch.y - o);
                        m = contextMenu();
                        $(m).css({ left: window.innerWidth + 'px' });
                    }
                    showMenu();
                }
            } else if (x > touch.x && m) {
                setSmoothHide(true);
                popupsHide();
            } else if (leftPanel.isMovable()) {
                enableMenuTransitions();
                leftPanel.show(x > touch.x);
            }
            e.preventDefault();
            // а то может прийти onclick который сразу же закроет панель
        }
        else if (touch.state == TouchState.MOVING_LEFT_PANEL) {
            const w = leftPanel.width();
            enableMenuTransitions();
            leftPanel.show(x > w/3);
            e.preventDefault();
        }
        else if (touch.state == TouchState.MOVING_CONTEXT_MENU) {
            const mr = touch.moveRightX;
            const w = $(m).outerWidth();
            if (x < (mr ? mr + w/3 : window.innerWidth - w/3)) {
                showMenu()
            } else {
                setSmoothHide(true);
                popupsHide()
            }
            e.preventDefault();
        }
        stopMoving();
    }
}

document.addEventListener('touchmove', (e) => {
    reportTouches('touchmove', e);

    // чтобы не прокручивать сообщения под меню/диалоговыми окнами
    // на Android хватает overflow:hidden для body,
    // а для iOS требуется e.preventDefault(), когда мы пытаемся прокрутить
    // диалоговое окно за его пределы
    const t = findTouch(e);
    if (!t)
        return;

    clearLongTapTimeout();

    DEBUG(`ontouchmove x = ${t.clientX}, y = ${t.clientY}`);

    checkScrolling(touch);

    try {

        if (leftPanel.isMovable()) {
            if (touch.state == TouchState.START
                && leftPanel.isVisible()
                && t.clientX < leftPanel.width()
                && t.clientX < touch.x - 20)
            {
                startMovingLeftPanel();
                touch.moveLeftX = Math.min(touch.x - 20, leftPanel.width());
            }
            if (touch.state == TouchState.MOVING_LEFT_PANEL) {
                moveLeftPanel(t);
                e.preventDefault();
                return;
            }
        }

        const m = contextMenu();
        if (touch.state == TouchState.START
            && m
            && t.clientX > touch.x + 20)
        {
            startMovingContextMenu();
            touch.moveRightMinLeft = $(m).offset().left;
            touch.moveRightX = Math.max(touch.x + 20, touch.moveRightMinLeft);
        }

        const u = uimByElement(e.target);
        const dy = Math.abs(t.clientY - touch.y);
        if (touch.state == TouchState.START
            && !touch.rightDragHandled
            && !m && u
            && touch.x >= window.innerWidth - (5 + safeAreaInset().right)
            && (touch.x - t.clientX)*1.5 > dy)
        {
            // не начинаем drag в touchstart, поскольку он сильно мешает
            // вертикальной прокрутке (большой палец правой руки часто оказывается
            // у правого края)
            const o = contextMenuOffset();
            showUimContextMenu(u, window.innerWidth, touch.y - o);
            startMovingContextMenu();
        }
        if (dy > 10 || t.clientX > touch.prevX) {
            touch.rightDragHandled = true;
        }

        if (touch.state == TouchState.MOVING_CONTEXT_MENU) {
            moveContextMenu(t);
            e.preventDefault();
            return;
        }

        var up = touch.prevY < t.clientY;
        var down = touch.prevY > t.clientY;
        DEBUG(`ontouchmove ${e.target.className} up ${up} / down ${down}, ${t.length}, ${touch.prevY} -> ${t.clientY}`);

        tryPreventScroll(e, up, down, false);
    } finally {
        touch.prevX = t.clientX;
        touch.prevY = t.clientY;
    }
}, passiveListener(false));

document.addEventListener('wheel', (e) => {
    DEBUG(`wheel deltaY = ${e.deltaY}`);
    if (e.deltaY && isMobile)
        tryPreventScroll(e, e.deltaY < 0, e.deltaY > 0, true);
}, passiveListener(false));

document.addEventListener('selectionchange', () => {
    const rc = window.getSelection().rangeCount;
    DEBUG(`Selection change. Range count = ${rc}`);
    if (rc == 0) {
        // Отрабатываем появление выделения, а не исчезновение.
        // При focus/blur у <input> приходит selectionchange
        // с getSelection().selectionType == "Caret",
        // а потом, при клике по любому месту, снова приходит selectionchange,
        // сообщающий о том, что <input> (который уже давно спрятан вместе
        // с диалогом) теперь не выделен. Отсекаем такие случаи
        return;
    }
    if (touch) {
        touch.selectionChanged = true;
        if (touch.state == TouchState.START) {
            touch.state = TouchState.CANCELLED;
        }
    }
}, passiveListener(false));

function tryPreventScroll(e, up, down, fromWheel) {
    var target = e.target;
    var atBorder = false;
    var hasScroll = false;

    while (target && target != document) {
        const overflowY = $(target).css('overflow-y')
        if ((overflowY == 'scroll' || overflowY == 'auto')
            && target.clientHeight > 0
            // ^ в мобильном FF иногда появляется span с ненулевым scrollHeight,
            // но с нулевым clientHeight, из-за чего мы не можем прокручивать
            // вверх
            && target.scrollHeight > target.clientHeight
            && !hasScroll)
        {
            hasScroll = true;
            DEBUG('has scroll ' + target.className);
            if ((target.scrollTop <= 0 && up) ||
                (target.scrollTop >= target.scrollHeight - target.clientHeight && down)) {
                atBorder = true;
                DEBUG(`at border: up = ${up}, down = ${down}`);
            }
        }

        if ($(target).hasClass('popup') || $(target).hasClass('left')
            || $(target).hasClass('leftPanelShadow') || $(target).hasClass('ui-menu'))
        {
            if ((atBorder && !(supportsOverscrollContain && fromWheel))
                // В Firefox подписки из крайних положений начинают
                // прокручиваться рывками (если быстро менять направление
                // wheel), но он поддерживает overscroll-behavior.
                //
                // А mobile safari с включенным CSS Overscroll behavior
                // всё равно прокручивает body когда подписки находятся
                // в крайних положениях, так что оставляем preventDefault
                // для touch events (или только для Safari? нужно будет
                // в будущем проверить еще раз, когда overscroll behavior
                // уже не будет экспериментальной фичей iOS Safari 14.5)
                || !hasScroll) {
                DEBUG('preventing scroll')
                e.preventDefault();
            }
            break;
        }
        target = target.parentNode;
    }
}

function contextMenuOffset() {
    // чтобы меню было на уровне пункта Star
    const emr2_12 = getComputedStyle(document.querySelector('.bodyScaler')).getPropertyValue("--emr2_12");
    return 1.5*(parseInt(emr2_12) + 14);
}
export function contextMenu() {
    return $(".popup .contextMenu")[0];
}
export function setSmoothHide(x) {
    $(contextMenu()).data('smoothHide', x);
}
export function hideContextMenu(clearSource) {
    const m = contextMenu();
    if (m && $(m).data('smoothHide')) {
        enableMenuTransitions();
        $(m).css({ left: window.innerWidth + 'px' });
        window.setTimeout(() => {
            if (document.contains(m)) {
                execF(clearSource)
            }
        }, 300);
    } else {
        execF(clearSource);
    }
}
