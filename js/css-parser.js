export { getRuleValue, overrideRule,
         overrideMediaRule, getOverridenMediaRuleValue,
         getPreprocessedCss, bulkOverride, getMediaRules }

// { selectors  : [selector][property] -> value
// , properties : [property][selector] -> value
// }
let preprocessedCss = null;
function normalizeSelector(sel) {
    return sel.split(/\s+/).map(function (s) {
        const firstDot = s.indexOf('.');
        if (firstDot > 0) {
            return s.substr(0, firstDot) + s.substr(firstDot).split(/\./).sort().join('.')
        } else if (firstDot == 0) {
            return s.split(/\./).sort().join('.')
        } else
            return s
    }).join(' ');
}
function splitCssSelectors(x) {
    return x.split(/ *, */).map(normalizeSelector);
}
function endsWith(str, suffix) {
    return str.indexOf(suffix, str.length - suffix.length) !== -1;
}
function iterAllRules(f) {
    const overrideSheet = getOverrideSheet();

    for (let i = 0; i < document.styleSheets.length; i++) {
        const sheet = document.styleSheets[i];
        if (sheet.disabled || sheet == overrideSheet)
            continue;
        const rules = sheetCssRules(sheet);
        for (let j = 0; j < rules.length; j++) {
            f(rules[j])
        }
    }
}
function getPreprocessedCss() {
    if (preprocessedCss) return preprocessedCss;

    // не учитывает style.getPropertyPriority(), т.е. флаг !important
    const selectors = Object.create(null);
    const properties = Object.create(null);

    function set(x, k1, k2, v) {
        if (!x[k1]) {
            x[k1] = {}
        }
        x[k1][k2] = v;
    }

    iterAllRules((r) => {
        if (!r.selectorText || !r.style)
            return;
        const sel = splitCssSelectors(r.selectorText);
        for (const s in sel) {
            for (let p = 0; p < r.style.length; p++) {
                let pn = r.style.item(p);
                const vSuffix = "-value";
                if (endsWith(pn, vSuffix))
                    pn = pn.substr(0, pn.length - vSuffix.length);
                // старый FF зачем-то разбивал padding-right
                // на padding-right-value, padding-right-rtl-…,
                // а вариант без -value не оставлял
                const pv = r.style.getPropertyValue(pn);
                set(selectors, sel[s], pn, pv);
                set(properties, pn, sel[s], pv);
            }
        }
    });
    preprocessedCss = { properties : properties, selectors : selectors };
    return preprocessedCss;
}
function getRuleValue(sel, prop) {
    const s = getPreprocessedCss().selectors[normalizeSelector(sel)];
    return s ? s[prop] : null;
}
let overrideSheet = null;
let overrideRules = {};
function getOverrideSheet() {
    if (overrideSheet)
        return overrideSheet;
    else {
        const s = document.createElement('style');
        s.type = 'text/css';
        s.rel = 'stylesheet';
        s.setAttribute('id', 'overrideSheet')
        const h = document.querySelectorAll('head');
        // Evernote Clipper добавляет свой <head> перед нашим,
        // так что добавляем overrideSheet в последний head.
        h[h.length - 1].appendChild(s);
        overrideSheet = s.sheet;
        return overrideSheet;
    }
}

function bulkOverride(f) {
    let sheet = getOverrideSheet();
    sheet.disabled = true;
    // alignUiToPixelBoundary работает на 20-40% быстрее, но основное
    // время все равно занимает reflow, pager.invalidate и т.д.
    try {
        f();
    } finally {
        sheet.disabled = false;
    }
}
function sheetCssRules(sheet) {
    // похоже, если стоит custom CSS, то Firefox выдает SecurityError
    try {
        // In Chrome, if stylesheet originates from a different domain,
        // ss.cssRules simply won't exist. I believe the same is true for IE, but
        // I haven't tested it.
        //
        // In Firefox, if stylesheet originates from a different domain, trying
        // to access ss.cssRules will throw a SecurityError. Hence, we must use
        // try/catch to detect this condition in Firefox.
        const r = sheet.cssRules ? sheet.cssRules : sheet.rules;
        return (r && r.length) ? r : [];
    } catch (e) {
        // Rethrow exception if it's not a SecurityError. Note that SecurityError
        // exception is specific to Firefox.
//         if(e.name !== 'SecurityError')
//             throw e;
        return [];
    }
}
function overrideRule(selector, prop, value, important) {
    const sheet = getOverrideSheet();
    const rules = sheetCssRules(sheet);
    const index = overrideRules[selector];
    if (index) {
        rules[index].style.setProperty(prop, value, important ? "important" : "");
    } else {
        overrideRules[selector] =
            sheet.insertRule(selector + " { " + prop + ": " + value
                             + (important ? "!important }" : " }"), rules.length)
    }
    // important может поменять семантику
    // .iconKeepUnread {.. !important } в дополнительном стиле
    // также заменит
    // .read .iconKeepUnread {..} в основном
}
function mediaRuleOrSel(mediaText, selector, withRule, withSel) {
    const sheet = getOverrideSheet();
    const rules = sheetCssRules(sheet);
    const sel = `@media ${mediaText} ${selector}`;
    const index = overrideRules[sel];
    if (index) {
        const mr = rules[index];
        const mrules = mr.cssRules ? mr.cssRules : mr.rules;
        return withRule(mrules[0]);
    } else {
        return withSel(sel, sheet, rules)
    }
}
function overrideMediaRule(mediaText, selector, prop, value, important) {
    mediaRuleOrSel(mediaText, selector, (r) => {
        r.style.setProperty(prop, value, important ? "important" : "");
    }, (sel, sheet, rules) => {
        overrideRules[sel] =
            sheet.insertRule(`@media ${mediaText} { ${selector} {
                     ${prop}: ${value} ${important ? "!important" : ""} }
            }`, rules.length);
    })
}
function getOverridenMediaRuleValue(mediaText, selector, prop) {
    return mediaRuleOrSel(mediaText, selector,
                          (r) => r.style.getPropertyValue(prop), () => null);
}
function getMediaRules() {
    const rules = [];
    iterAllRules((r) => {
        if (r.constructor === CSSMediaRule)
            rules.push(r)
    });
    return rules;
}
