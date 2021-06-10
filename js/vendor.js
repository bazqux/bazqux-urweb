"use strict";

// import "core-js/modules/es.promise";
// import "core-js/modules/es.array.iterator";

import "regenerator-runtime/runtime";

export const $ = require("jquery")
// export { $ } from "jquery"
// ^ почему-то вызывает ошибку __webpack_require__.i(...) is not a function
// в webpack 3 так и не исправили __WEBPACK_IMPORTED_MODULE_0__vendor__.$

import "jquery-ui/ui/widgets/autocomplete"

export * from "./browser-info"
import { isMobile, iOS, useCapture } from "./browser-info"
export * from "./country-code"
export * from "./hash-map"
export * from "./local-storage"
import * as cssParser from "./css-parser"
export { cssParser }

export function elem(id)
{
    return document.getElementById(id);
}

// https://dev.opera.com/articles/fixing-the-scrolltop-bug/
export function scrollingElement() {
    if ('scrollingElement' in document) {
	return document.scrollingElement;
    }
    // Fallback for legacy browsers
    if (navigator.userAgent.indexOf('WebKit') != -1) {
	return document.body;
    }
    return document.documentElement;
}

export function disableScrolling(selector, d) {
    const b = $(selector);
    if (d) {
        b.addClass("noScroll");
    }
    else {
        b.removeClass("noScroll");
    }
}
export function blurActiveElement() {
    $(document.activeElement).blur();
}
export function isPinchZoomed() {
    return window.visualViewport
        ? window.visualViewport.scale != 1
          // Chrome 60+
        : document.documentElement.clientWidth != window.innerWidth;
          // iOS, macOS Safari
}

export function boundingClientRectTop(id) {
    return boundingClientRect(id)._Top;
}
export function boundingClientRectHeight(id) {
    const r = boundingClientRect(id);
    return r._Bottom - r._Top;
}
export function boundingClientRect(id) {
    const e = elem(id);
    if (!e)
        return { _Top : 0, _Left : 0, _Right : 0, _Bottom : 0 };
//        throw "getBoundingClientRect: " + id + " doesn't exists";
    const r = elem(id).getBoundingClientRect();

    // https://openradar.appspot.com/radar?id=6668472289329152
    // iPad при прокрутке сообщает неверные top/bottom координаты fixed элементов
    if (id == 'topId' && isMobile && iOS)
        return { _Top : 0.0, _Left : r.left,
                 _Right : r.right, _Bottom : r.height }
        // будет некорректно работать при zoom, но это меньшее из двух зол
    else
        return { _Top : r.top, _Left : r.left,
                 _Right : r.right, _Bottom : r.bottom }
}


// При щелчке средней кнопкой
// в FF приходит и click и auxclick
// в Chrome только auxclick
// в Safari только click
// в Edge ничего не приходит, пока его вообще не поддерживаем,
// т.к. возиться с mouseup неохота
export function addClickHandler(h) {
    let hadAuxclickInClick = false;
    document.addEventListener('click', (e) => {
        if (e.button > 0)
            hadAuxclickInClick = true;
        uw_event = e;
        h(e);
    }, useCapture(true));
    document.addEventListener('auxclick', (e) => {
        if (!hadAuxclickInClick) {
            uw_event = e;
            h(e);
        }
    }, useCapture(true));
}

export function arrayToList(a) {
    let acc = null;
    for (let i = a.length - 1; i >= 0; i--)
        acc = { _1 : a[i], _2 : acc };
    return acc;
}

export function partition(a, pred) {
    let t = [];
    let f = [];
    for (var e of a) {
        (pred(e) ? t : f).push(e);
    }
    return [t,f];
}
export function words(x) { return x.split(/\s+/).filter((w) => w != "") }
export function uniq(x) {
    const s = new Set();
    x.forEach((w) => s.add(w));
    return Array.from(s);
}
export function getVariable(n) {
    return getComputedStyle(document.querySelector(':root')).getPropertyValue(n);
}
