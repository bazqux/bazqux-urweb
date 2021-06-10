"use strict";

import {
    $, iOS, iOSStandalone, getFromLocalStorage, saveToLocalStorage, elem,
    addClickHandler, arrayToList
} from './vendor'
import { uim, uimMsgKey, uimIdByMsgKey } from './uim'

if (location.hash == '#_=_') {
    try{
        history.replaceState(null, '', location.pathname.replace(/^\/\//, "/") + location.search);
        // remove # from the end
    } catch(e) {
        // Microsoft Edge browser rises "Unspecified error." on pushState
        // hence the try/catch
    }
}

export function getInterfacePath()
{
    const l = location.pathname.match(/^\/i\/(.+)/);
    const h = l ? l[1] : "";
    return iOSStandalone ? getFromLocalStorage('', 'interfacePath', h) : h;
    // iOS любит перезагружать страницу в home screen app и не запоминает
    // при этом её URL, так что храним interfacePath в localStorage
}

export function replaceInterfacePath(h)
{
    try {
        history.replaceState(null, '', '/i/' + h);
    } catch (_) {
    }
}

export function pushInterfacePath(h)
{
    if (iOSStandalone) {
        saveToLocalStorage('', 'interfacePath', h);
    }
    try {
        if (iOS) {
            history.replaceState(null, '', '/i/' + h);
            // не добавляем в историю, чтобы можно было левую панель показать
            // через смахивание от левого края
        } else
            history.pushState(null, '', '/i/' + h);
        // в отличии от "window.location.hash = h"
        // не вызывает отображения адресной строки в мобильном браузере
    } catch (e) {
    }
}

let onPopstate = null;

export function registerOnPopstate(f) { onPopstate = f }

window.addEventListener("popstate", (e) => {
    uw_event = e;
    uw_preventDefault();
    if (onPopstate) {
        execF(onPopstate);
        restoreScrollTop();
    }
});

history.scrollRestoration = 'manual';

function restoreScrollTop() {
    const st = uw_event.state;
    if (st && st.msgKey != null) {
        const uimId = uimIdByMsgKey(st.msgKey);
        if (!uimId)
            return;
        scrollToFragment(uimId, st.pos);
    }
}

let _scrollToFragment = null;
let _uimScrollOffset = null;

export function registerScrollToFragment(s) { _scrollToFragment = s }
export function registerUimScrollOffset(u) { _uimScrollOffset = u }

function scrollToFragment(uimId, id) {
    if (_scrollToFragment) {
        execF(execF(execF(_scrollToFragment, uim(uimId)), id));
    }
}
function uimScrollOffset(uimId) {
    return execF(execF(_uimScrollOffset, uim(uimId)));
}

export function eventTargetClassesArray()
{
    let n = uw_event.target;
    let cn = [];
    while (n && n.classList && !n.classList.contains("msgFrame")) {
        for (var i = 0; i < n.classList.length; i++) {
            // classList.keys не поддерживаются в iOS 9
            cn.push(n.classList.item(i));
        }
        n = n.parentNode;
    }
    //    uw_debug(cn);
    return cn;
}

export function eventTargetClasses() {
    return arrayToList(eventTargetClassesArray());
}

export function eventTargetHasLink() { return (eventTargetLink() != null) }

function eventTargetLink()
{
    let n = uw_event.target;
    while (n && (typeof n.className !== "string" || n.className.indexOf("msgFrame") == -1)) {
        if (n.nodeName == "A" && n.href && n.href != "")
            return n;
        n = n.parentNode;
    }
    return null;
}

addClickHandler(() => {
//     uw_debug('clickHandler e.button = ' + e.button);
    const l = eventTargetLink();
    if (!l)
        return;
    const href = l.getAttribute('href')
    if (href && href.indexOf('#') == 0) {
        uw_preventDefault();
        // В firefox нужно делать preventDefault в click, а не auxclick
        // иначе все равно открывает ссылку
        const id = decodeURIComponent(href.substr(1));
        if (!elem(id)) {
            // <a name> вместо <a id>,
            // устанавливаем id, чтобы в scrollToFragment найти top
            const e = document.getElementsByName(id)[0];
            if (e)
                e.setAttribute("id", id);
        }
        const uimId = $(l).parents('.msgFrame').attr('id');
        history.replaceState(
            { pos : { _Offset : uimScrollOffset(uimId) },
              msgKey : uimMsgKey(uim(uimId))
            },
            '', location.href);
        scrollToFragment(uimId, { _Id : id });
        history.pushState(
            { pos : { _Id : id },
              msgKey : uimMsgKey(uim(uimId))
            },
            '', location.pathname + href)
    }
});
