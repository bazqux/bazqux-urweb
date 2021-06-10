import 'css-browser-selector'
import './mq.genie'

if (!(history && history.pushState && history.replaceState)
    || !(window.CSS && CSS.supports('color', 'var(--fake-var)'))
   ) {
    alert('Your browser is too old to run BazQux Reader. \nTry latest version of Firefox, Vivaldi, Safari or Chrome.');
}

try{
    // http://www.opera.com/support/kb/view/827/
    opera.setOverrideHistoryNavigationMode('compatible');
    history.navigationMode = 'compatible';
    // для вызова onunload в Опере.
}catch(e){}

// Test via a getter in the options object to see if the passive property is accessed
let supportsPassive = false;
let supportsCapture = false;
try {
    const opts = Object.defineProperty({}, 'passive', {
        get: function() {
            supportsPassive = true;
            return true;
        }
    });
    Object.defineProperty(opts, 'capture', {
        get: function() {
            supportsCapture = true;
            return true;
        }
    });
    window.addEventListener("test", null, opts);
} catch (e) {}

export function passiveListener(p) {
    return supportsPassive ? { passive: p } : false;
}
export function useCapture(c) {
    return supportsCapture ? { capture: c } : c;
}

let iOSVersion = null;
if ((/iP(hone|od|ad)/).test(navigator.userAgent)) {
    var v = (navigator.userAgent).match(/OS (\d+)_(\d+)_?(\d+)?/);
    iOSVersion = [parseInt(v[1]), parseInt(v[2]), parseInt(v[3] || 0)];
}

let classList = document.documentElement.classList;

if (!iOSVersion && navigator.userAgent.includes("Mac") && "ontouchend" in document) {
    iOSVersion = [13];
    // iPad в iOS 13 работает в desktop mode
    classList.add("mobile", "ios");
}

export const visibleScrollbars = window.mqGenie.width > 0;

if (visibleScrollbars) {
    classList.remove('mobile');
    classList.add('visibleScrollbars');
    // не делаем мобильный вид при наличии scrollbar-ов, т.к. scrollbar
    // сбоку от toolbar, а не под ним, смотрится плохо (в left и в articles
    // получаются scollbar-ы разной высоты)
}

export const isMobile = classList.contains('mobile');
export const isMac = classList.contains('mac');
export const isWin = classList.contains('win');
export const isTouchDevice = 'ontouchstart' in document;

classList.add(isTouchDevice ? 'touch' : 'noTouch');

export const iOS = iOSVersion;
export const iOSStandalone = navigator.standalone == true;
// из css_browser_selector
export function isMobileF() { return isMobile; }
export function isWinF() { return isWin; }

export const supportsOverscrollContain = CSS.supports("(overscroll-behavior: contain) or (-ms-scroll-chaining: none)");

// let _safeAreaInsetWidthDiv = null;
// function safeAreaInsetWidthDiv() {
//     if (!_safeAreaInsetWidthDiv) {
//         _safeAreaInsetWidthDiv = document.createElement("div");
//         _safeAreaInsetWidthDiv.classList.add("safeAreaInsetWidthDiv");
//         document.documentElement.appendChild(_safeAreaInsetWidthDiv);
//     }
//     return _safeAreaInsetWidthDiv;
// }

export function pf(x) {
    return parseFloat(x.replace(/,/, "."));
    // 1,9em в русском Chrome?
}

let _safeAreaInset = null;
function updateSafeAreaInset() {
    const s = getComputedStyle(document.documentElement);
    const v = (x) => pf(s.getPropertyValue("--safe-area-inset-" + x));
    let r = {
        left : v("left"),
        right : v("right"),
        top : v("top"),
        bottom : v("bottom")
    };
    r.width = r.left + r.right;
    r.height = r.top + r.bottom;
    _safeAreaInset = r;
    classList.toggle("nonZeroSafeAreaInsetTop", r.top != 0);
//     uw_debug(_safeAreaInset);
}
export function safeAreaInset() {
    if (_safeAreaInset == null)
        updateSafeAreaInset();
    return _safeAreaInset;
}

let onOrientaionChange_ = []
let mql = window.matchMedia('(orientation: landscape)');

export function registerOnOrientationChange(f) { onOrientaionChange_.push(f) }

function onOrientaionChange() {
//     uw_debug(e.matches ? "landscape orientation" : "portrait orientation");
    updateSafeAreaInset();
    onOrientaionChange_.forEach((f) => f());
}

if (mql.addEventListener)
    mql.addEventListener("change", onOrientaionChange);
else if (mql.addListener)
    mql.addListener(onOrientaionChange);
