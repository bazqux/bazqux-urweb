import { $, cssParser, isPinchZoomed } from './vendor'
import { safeAreaInset } from './browser-info'

export function isVisible() {
    return !isMovable0() || $('.showLeftPanel').length > 0
//     const l = $('.left').css('left');
// //    uw_debug('isVisible ' + l);
//     // l == "0px"
//     return (l != '' && l[0] == '0') || $('.showLeftPanel').length > 0;
    // после removeAttr('style') left обновляется не сразу,
    // по этому дополнительно проверяем наличие showLeftPanel
}
export function width() {
    return window.innerWidth - parseInt(cssParser.getRuleValue('.js-movable-left-panel', 'margin-right')) - safeAreaInset().right;
}
let _show = null;
export function show(x) { execF(execF(_show, x)); }
export function set_showLeftPanel(t) { _show = t }
export function isMovable() {
    return isMovable0() && !isPinchZoomed() && !$('.bigDialogBox, .smallDialogBox, .blackout, .popup .menu').length;
    // не двигаем панель (в том числе swipe-ом) поверх полноэкранных диалогов,
    // странно выглядит, а прятать их сразу нельзя,
    // т.к. после этого перестает приходить touchmove
}
export function isMovable0() {
    return window.innerWidth <= parseInt(cssParser.getRuleValue('.js-left-panel-visibility', 'width'));
    //    return $('.showLeftPanelButton').css('display') != 'none';
    // ^ дико тормозит в Firefox
}
