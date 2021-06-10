"use strict";

import {
    $,
    countriesList,
    getFromLocalStorage, saveToLocalStorage,
    passiveListener, cssParser,
    isMobile, iOS, isMac, isTouchDevice, scrollingElement, elem,
    boundingClientRectHeight, addClickHandler, useCapture,
    arrayToList, words, uniq, getVariable, safeAreaInset, pf,
    registerOnOrientationChange
} from './vendor'

import { getInterfacePath, eventTargetHasLink } from './history'
import * as leftPanel from './left-panel'
import { popupsHide, onLongTap, contextMenu } from './touch-events'
import { uim, uimByElement, uimMsgKey } from './uim'

// code from link from http://stackoverflow.com/questions/470832/getting-an-absolute-url-from-a-relative-one-ie6-issue
function parseURI(url) {
  const m = String(url).match(/^([^:/?#]+:)?(\/\/(?:[^:@]*(?::[^:@]*)?@)?(([^:/?#]*)(?::(\d*))?))?([^?#]*)(\?[^#]*)?(#[\s\S]*)?/);
  // authority = '//' + user + ':' + pass '@' + hostname + ':' port
  return (m ? {
    href     : m[0] || '',
    protocol : m[1] || '',
    authority: m[2] || '',
    host     : m[3] || '',
    hostname : m[4] || '',
    port     : m[5] || '',
    pathname : m[6] || '',
    search   : m[7] || '',
    hash     : m[8] || ''
  } : null);
}

export function getUrlUsernameAndPassword(url) {
    const m = url.match(/^([^:/]+:\/\/)?([^/#?]+)@(.*)/);
    if (m) {
        const a = m[2].split(':');
        return { _1 : decodeURIComponent(a[0]), _2 : a[1]?decodeURIComponent(a[1]):'', m : m }
    } else
        return { _1 : "", _2 : "" }
}
export function setUrlUsernameAndPassword(up, url) {
    const m = getUrlUsernameAndPassword(url).m;
    const auth =
          up._1 != ""
          ? (encodeURIComponent(up._1)
             + (up._2 != "" ? ':'+encodeURIComponent(up._2) : '') + '@')
          : "";
    if (m) {
        return (m[1] ? m[1] : "") + auth + m[3];
    } else {
        const m = url.match(/^([^:/]+:\/\/)?(.*)/);
        return m ? (m[1] ? m[1] : "") + auth + m[2] : url;
    }
}
export function titleFromUrl(u) {
    return setUrlUsernameAndPassword({_1:"", _2:""}, u.trim()).replace(/^https?:\/\//, "")
}

export function preloadImages(xml) {
    const div = document.createElement('div');
    div.innerHTML = flatten_(xml);
}

function fixUnprocessed(n, f) {
    $('.' + n + 'Unprocessed').each(function () {
        const e = $(this);
        e.removeClass(n + 'Unprocessed');
        e.addClass(n);
        f(e);
    })
}

function highlightPres(e, checkSpoiler) {
    import('./hljs').then(({hljs}) => {
    $(e).find('pre code[class]').each(function () {
        // просто <pre> не раскрашиваем, т.к. тормозит на больших <pre>
        // (пост с текстом лицензии на хабре)
        const e = $(this);
        if (e.hasClass('hljs')
            || (checkSpoiler && e.parents('.bqrHabr_spoiler').length > 0)
            || this.textContent.length > 10000)
            return;
//         uw_debug('len: '+e.innerHTML.length+' '+e.innerHTML.substr(0,40));
//         console.time('highlightBlock');
        const lang = this.className;
        hljs.highlightBlock(this, null, false);
        this.className = lang + " hljs";
        // hljs зачем-то добавляет alias-ы языка, не добавляя при этом к ним
        // префикс language-
//         console.timeEnd('highlightBlock');
    })});
}

export function highlightCode() {
//     fixUnprocessed("bqrHabr", function (e) { highlightPres(e, false); });
    highlightPres(msgTreeDiv(), false);
}

export function fixVKPostTime() {
    fixUnprocessed('bqrVKPostTime', function (e) {
        const t = e.attr('data-time');
        if (t) {
            e.html(showTimeAndAgo(t*1000*1000));
        }
    });
}

let instagramLoaded = false;
export function addInstagram() {
    restoreInstagramImgSizes();
    requestAnimationFrameOnce("addInstagram", function () {
        // будет грузится не через мсек, а на следующем кадре, ну и ладно
        if (window.instgrm && instgrm.Embeds && instgrm.Embeds.process) {
            instgrm.Embeds.process();
        } else if (!instagramLoaded) {
            instagramLoaded = true;

            const ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
            ga.src = '//platform.instagram.com/en_US/embeds.js';
            const s = document.getElementsByTagName('body')[0]; s.appendChild(ga);
        }
    });
}

let twitterLoaded = false;
export function addTwitter() {
    restoreTwitterImgSizes();
    requestAnimationFrameOnce("addTwitter", function () {
        // будет грузится не через мсек, а на следующем кадре, ну и ладно
        if (window.twttr && twttr.widgets && twttr.widgets.load) {
            twttr.widgets.load();
        } else if (!twitterLoaded) {
            twitterLoaded = true;

            const ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
            ga.src = '//platform.twitter.com/widgets.js';
            const s = document.getElementsByTagName('body')[0]; s.appendChild(ga);
        }
    });
}

export function addVideo() {
    if (!lazyLoadVideos) {
        $(".iframe_placeholder").each(function () {
            this.setAttribute('data-bqr-html', this.getAttribute('data-bqr-html').replace('autoplay=1', 'autoplay=0').replace('youtube-nocookie', 'youtube'));
            // в youtube-nocookie нет иконки add to watch later
            this.click();
        });
        return;
    }
    requestAnimationFrameOnce("addVideo", function () {
        collectVideoIdsAndPerformBatchRequests(
            'youtube-unprocessed', 50, requestYouTubeVideoDetails);
        collectVideoIdsAndPerformBatchRequests(
            'vimeo-unprocessed', 1, requestVimeoVideoDetails);
    });
}

function collectVideoIdsAndPerformBatchRequests(cls, batchSize, performRequest) {
    const vids = {};
    $("."+cls).each(function () {
        const e = this;
        $(e).removeClass(cls);
        const vid = e.getAttribute('data-video-id');
        const id = fresh();
        e.setAttribute('id', id);
        // на всякий случай, отвязываемся от DOM
        if (vids[vid])
            vids[vid].push(id);
        else
            vids[vid] = [id];
    });
    let ids = [];
    for (let id in vids) {
        ids.push(id);
        if (ids.length >= batchSize) {
            performRequest(ids, vids);
            ids = [];
        }
    }
    if (ids.length > 0)
        performRequest(ids, vids);
}

export function setProxiedAttrs() {
    requestAnimationFrameOnce("setProxiedAttrs", updateProxiedAttrs)
}

function apiProxy(url, success) {
    $.ajax({
        dataType: "json",
        url: addImageHost("proxy", '/api_proxy' + url),
        success: success,
        crossDomain: true,
        xhrFields: {
            withCredentials: true
        }
    });
}

function maximumBy(f, a) {
    let r = null;
    let m = null;
    for (const i in a) {
        if (!r) {
            r = a[i];
            m = f(r);
        } else {
            const mi = f(a[i]);
            if (mi > m) {
                r = a[i];
                m = mi;
            }
        }
    }
    return r;
}

function requestYouTubeVideoDetails(ids, vids) {
    apiProxy("/youtube_video_details/" + ids.join(","), function (data) {
        for (const idx in data.items) {
            const i = data.items[idx];
            const es = vids[i.id];
            for (const vi in es) {
                const e = elem(es[vi]);
                const maxT = maximumBy((t) => t.width, i.snippet.thumbnails);
                if (maxT) {
                    $(e).find('.ytp-thumbnail').css('background-image',
                                                    `url("${maxT.url}")`);
                }
                $(e).find('.ytp-title-link').text(i.snippet.title);
            }
        }
    })
}

function requestVimeoVideoDetails(ids, vids) {
    apiProxy('/vimeo_video_details/' + ids[0], function(data) {
            const es = vids[ids[0]];
            for (const vi in es) {
                const e = elem(es[vi]);
                $(e).find('.cover').css('background-image',
                                        `url("${data.thumbnail_url}")`);
                $(e).find('.h1 a')
                    .text(data.title)
                    .attr('href', 'https://vimeo.com/'+ids[0]);
                $(e).find('.h2 a')
                    .text(data.author_name)
                    .attr('href', data.author_url);
                $(e).removeClass('visibilityHidden');
            }
    });
}

let habrSpoilers = {}

export function resetHabrSpoilers() { habrSpoilers = {} }
function habrSpoilersState(s) {
    let parent = s;
    let id = "";
    while (!(id = parent.getAttribute('id')))
        parent = parent.parentNode;
    if (!habrSpoilers[id])
        habrSpoilers[id] = {};
    return habrSpoilers[id];
}
function habrSpoilerNum(s) { return s.getAttribute('data-spoiler-num'); }

// jQuery.fx.off = isMobile;
// ^ для отключения всех анимаций (автоматический duration = 0 для всех animate)

export function habrahabrToggleSpoiler(s, immediate)
{
    const n = s.parentNode.querySelector("div.bqrHabr_spoiler_text");
    if (n.style.display == "block") {
        habrSpoilersState(s)[habrSpoilerNum(s)] = false;
        s.parentNode.className = "bqrHabr_spoiler";
        if (immediate) {
            n.style.display = "none";
            return;
        }
        const h0 = n.clientHeight - 20; // - paddingTop - paddingBottom
        n.style.height = h0+'px';
        $(n).animate({
            height: '0px', marginTop : '0px',
            paddingTop : '0px', paddingBottom : '0px'
        }, 300, 'swing', function () {
            n.style.display = "none";
            n.style.height = "auto";
        });
    } else {
        habrSpoilersState(s)[habrSpoilerNum(s)] = true;
        s.parentNode.className = "bqrHabr_spoiler bqrHabr_spoiler_open";
        highlightPres(n, true);
        n.style.display = "block";
        if (immediate) {
            return;
        }
        n.style.marginTop = '0px';
        n.style.paddingTop = '0px';
        n.style.paddingBottom = '0px';
        const h = n.clientHeight;
        n.style.height = '0px';
        $(n).animate({
            height: h+'px', marginTop : '10px',
            paddingTop : '10px', paddingBottom : '10px'
        }, 300, 'swing', function () {
            n.style.height = "auto";
        });
    }
}

export function fixHabrSpoilers()
{
    fixUnprocessed('bqrHabr_spoiler_title', function (e) {
        const s = e[0];
        if (habrSpoilersState(s)[habrSpoilerNum(s)])
            habrahabrToggleSpoiler(s, true);
    })
}

function flattenAcc_(a, tr) {
    if (tr.cat1 != null) {
        flattenAcc_(a, tr.cat1);
        flattenAcc_(a, tr.cat2);
    } else if (tr.closure != null) {
        a.push("cr(_)");
    } else
        a.push(tr);
}

function flatten_(tr) {
    const a = [];
    flattenAcc_(a, tr);
    return a.join("");
}

function rootPath(p) {
    const l = window.location;
    return l.protocol + "//" + l.host + p;
}


function addImageHost(p, u) {
    if (u.substr(0, 1) == "/") {
        let host = window.location.host;
        if (host.match(/\d+\.\d+\.\d+\.\d+/))
            return u;
        // (favicons|proxy).bazqux.com вместо bazqux.com,
        // чтобы браузер качал иконки/картинки, делал запросы к api_proxy
        // с "другого" домена и не задерживал запросы дерева сообщений
        if (host.substr(0,4).toLowerCase() == "www.")
            host = host.substr(4);
        return window.location.protocol + "//" + p + "." + host + u;
    } else
        return u
}

export function backgroundImage(u) {
    return 'background-image: url(' + atr(`"${u}"`) + ')'
}

function hashCode32(s) {
    // return CryptoJS.SHA1(s).toString({stringify : function (b) { return b.words[0] }})
    // SHA1 не делает выборку authorStyles[hash % 10] ощутимо более равномерной
    // (в масштабах сотен авторов разброс те же +-20%)
    let hash = 0;
    for (let i = 0; i < s.length; i++) {
        hash  = hash*31 + s.charCodeAt(i);
        hash |= 0; // Convert to 32bit integer
    }
    return hash;
}
function authorStyle(author) {
    if (author == "") return "";
    const styles =
        ["author0", "author1", "author2", "author3", "author4",
         "author5", "author6", "author7", "author8", "author9"];
    // можно набрать большой список нейтральных web цветов и подставлять
    // их в svg, но есть ли смысл -- кто запомнит более 10 цветов?
    return styles[Math.abs(hashCode32(author) % styles.length)];
}
export function authorPicImg(src, author)
{
    const e = `<div class="emptyAuthorPic ${authorStyle(author)}"></div>`;
    if (!src)
        return e;
    const id = fresh();
    const eid = fresh();
    return `<img id="${id}" class=authorPicLoading src="${atr(src)}"
        onload="bq.apOnload('${id}')" onerror="bq.apOnerror('${id}','${eid}')">
      </img><span style="display:none" id="${eid}">${e}</span>
      <script>bq.apCheck('${id}')</script>`;
}
export function apCheck(id)
{
    setTimeout(function () { if (complete(id)) apOnload(id) }, 1);
}
export function apOnload(id)
{
    const e = elem(id); if (e) e.className = 'authorPic';
// TODO: стоит проверять размер картинки, если 1х1
// (как у blogspot-овского blank.gif), то это тот же error ?
}
export function apOnerror(id, eid)
{
    const i = elem(id); if (i) i.style.display = 'none';
    const e = elem(eid); if (e) e.style.display = 'inline';
}

let popupWindow = null;

export function enclosurePopup(div)
{
    uw_stopPropagation();
    uw_preventDefault();
    const kind = div.getAttribute('data-bqr-popup-type');
    const inner = div.getAttribute('data-bqr-popup-html');
    const link = div.getAttribute('data-bqr-popup-link');
    const width = 700;
    const ch = kind == "audio"
        ? 100
          : width * div.getAttribute('data-bqr-popup-aspect-ratio');
    try {
        // popupWindow.close();
        // ^ так хуже, окно мигает и переоткрывается в другом месте
        popupWindow.resizeBy(width - popupWindow.innerWidth, ch - popupWindow.innerHeight);
        // меняем размер окна, если оно уже открыто (чтобы сделать нормальную
        // высоту при открытии видео после аудио).
        // К сожалению, сразу после открытия окна в DOMContentLoaded и onload
        // clientHeight/innerHeight содержат некорректное значение
        // (1299 -- похоже, высота по-умолчанию)
        // Так что не получится этот код использовать сразу после открытия,
        // чтобы починить неправильный размер окна в Chrome/Safari.
        // Можно запускать его в requestAnimationFrame внутри окна
        // или через setTimeout в главном окне, но, в обоих случаях видно,
        // как меняется размер окна
    } catch (e) {
    }
    popupWindow = window.open('', "bqrPopup", `scrollbars=no,resizable=yes,toolbar=no,location=yes,status=no,width=${width},height=${ch}`);
    // Safari/Chrome задают height для всего окна, а не для внутренней части,
    // только Firefox работает правильно.
    // Chrome/FF добавляют адресную строку вне зависимости от значения
    // параметра location
    let cssPath = ""
    for (let i = 0; i < document.styleSheets.length; i++) {
        const href = document.styleSheets[i].href;
        if (href && href.indexOf('bazqux.com') != -1) {
            cssPath = href;
            break;
        }
    }

    const i = `
<html><head>
<title>▶</title>
<link href="${cssPath}" media="all" rel="stylesheet" type="text/css" />
</head>
<body class=bqrPopup>${inner + link}</body>
<script>
// window.requestAnimationFrame(function () {
//    var b = document.body;
//    window.resizeBy(${width} - window.innerWidth, ${ch} - window.innerHeight);
// });
</script>
</html>`
    popupWindow.document.write(i);
    popupWindow.document.close();
    popupWindow.focus();
}

export function strGt(a, b)
{
    return a.toLowerCase() > b.toLowerCase();
}

function elementIsEditable(el) {
    if (!el || el.nodeType != 1) return false;
    const wkum = $(el).css('-webkit-user-modify');
    return ((( el.tagName.match(/input|textarea|select|button/i) &&
        (el.getAttribute('type') || '').match(/^|text|search|password$/) )
      || el.getAttribute('contenteditable') == 'true'
      || (wkum != null && wkum != '' && wkum != 'read-only') ? el : false) ? 1 : 0);
}
/// Активный элемент, в котором возможно редактирование текста
export function activeElementIsEditable()
{
    return elementIsEditable(document.activeElement);
}

export function getEventKeyIdentifier()
{
    if (!window.event) return "";
    const ki = window.event.keyIdentifier;
    return ki == null ? "" : ki;
}

export function mkKeyIdentifier(code)
{
    const p = "U+0000";
    const s = code.toString(16).toUpperCase();
    return p.substr(0, p.length - s.length) + s;
}
function roundPixels(p) {
    return Math.round(p);
    // TODO: на устройствах большей плотности можно округлять до 0.5px
}
export function offsetBottomRight(child, cls)
{
    // для tag/share menu
    const r = $('#'+child+' .'+cls)[0].getBoundingClientRect();
    return {_Top  : roundPixels(r.bottom),
            _Left : roundPixels(window.innerWidth - r.right) }
}
function $fromCls(cls) {
    return $(cls.split(/ +/).map((x) => '.'+x).join(' '));
}
export function offsetBottomLeft(cls)
{
    // msgMenu/markAllAsReadMenu
    const r = $fromCls(cls)[0].getBoundingClientRect();
    return {_Top  : roundPixels(r.bottom - 1),
            _Left : roundPixels(r.left) }
}
export function isVisible(cls) {
    return $fromCls(cls).is(':visible');
}

const msgTreeDivId = 'msgDivId';
export function msgTreeDiv() { return elem(msgTreeDivId); }

const pixelsPerMsec = 3.2;

export function scrollTo(id, mode, offset, after)
{
    const msgTree = elem(id);
    const st0 = msgTree.scrollTop;
    const st = offset;

    const time =
          Math.abs(st - msgTree.scrollTop) > 2
          ? scrollTime(mode, 100 + Math.min(900, Math.abs((st-st0) / pixelsPerMsec)))
          : 0;

//    uw_debug(`scrollTo time = ${time}`);
    $(msgTree).stop().animate({
        'scrollTop': st
//     $({st:st0}).animate({
//         st: st
    }, {
        duration : time,
        easing: scrollEasing(mode),
//         step: function( now, fx ) {
//             fx.now = roundScrollTop(now);
//             window.scrollTo(0, fx.now);
//             uw_debug("left.top = " + $(".left")[0].getBoundingClientRect().top);
//             //uw_debug(fx.elem.id + " " + fx.prop + ": " + now);
//         },
        progress: function() {
            callOnscroll(msgTree);
            // иначе метки фидов не обновляются
        },
        always: function(){
            //window.scrollTo(0, st);
            msgTree.scrollTop = st;
            // может не докрутить, если дробное число (это scrollTo,
            // а animate?)
            setTimeout(function () {
                execF(after);
                callOnscroll(msgTree);
            }, 0); }
    });
}

function scrollTime(mode, time)
{
    return mode == 'immediate'
        ? 0 : (mode == 'quick' ? Math.min(200, time) : time);
}
function scrollEasing(mode)
{
    return mode == 'normal' ? 'linear' : 'swing';
}

function calcCollapseExpandHeights(minHeight, id) {
    const top = elem('topId').getBoundingClientRect().bottom;
    const bottom = window.innerHeight;
    const gRect = elem(id).getBoundingClientRect();
    const limit = (h) => Math.round(Math.max(h, minHeight));
    const h0 = limit(top - gRect.top);
    const h = limit(Math.min(bottom, gRect.bottom) - gRect.top);
//    uw_debug('h='+h+'; h0='+h0+'; h-h0='+(h-h0));
    return [h0, h];
}

export function collapseComments(minHeight, id, mode, after)
{
    const g = elem(id);
    if (!g) { execF(after); return; }
    const msgTree = msgTreeDiv();
    if (!msgTree) { execF(after); return; }

    const st = msgTree.scrollTop
    const [h0,h] = calcCollapseExpandHeights(minHeight, id);

    if (h-h0 <= 0) { execF(after); return; }
    if (!g || mode == 'immediate') {
        if (h0 != 0) msgTree.scrollTop = st - h0;
        execF(after); return;
    }

    g.style.height = h+'px';
    $(g).animate({
        height: h0+'px'
    }, scrollTime(mode, Math.max((h-h0)/pixelsPerMsec,150))
     , scrollEasing(mode), function () {
        g.style.height = "auto";
        if (h0 != 0) msgTree.scrollTop = st - h0;
        execF(after);
    });
}

export function expandComments(minHeight, id, mode, after)
{
    const g = elem(id);
    if (!g || mode == 'immediate') {
        execF(after); return;
    }
    const msgTree = msgTreeDiv();
    const st = msgTree.scrollTop;
    const [h0,h] = calcCollapseExpandHeights(minHeight, id);
    if (h <= 0) { execF(after); return; }

    g.style.height = h0+"px";
    $(g).animate({
        height: h+"px"
    }, scrollTime(mode, Math.max((h-h0)/pixelsPerMsec,150)), scrollEasing(mode), function () {
        g.style.height = "auto";
        setScrollTopNoOnScroll(msgTreeDivId, st);
        // ^ иначе FF 66 может сделать лишнюю прокрутку
        execF(after);
    });
}

export function moveUpDown(f, id, mode, after)
{
    const g = elem(id);
    const ch = g == scrollingElement() ? window.innerHeight : g.clientHeight;
    let l = execF(f, ch);
    const st = Math.min(g.scrollHeight - ch, Math.max(0, g.scrollTop + l));
    l = Math.abs(st - g.scrollTop);
    const t = Math.max(100, Math.min(900, l/pixelsPerMsec));

    $(g).stop().animate({
        'scrollTop': st
    }, {
        duration : scrollTime(mode, t),
        easing: scrollEasing(mode),
        always: function() {
            execF(after); callOnscroll(g); }
    });
}

export function showAgo(t) {
    const time = new Date(t/1000);
    const cd = new Date();
    let ms = cd.getTime() - time;
    if (ms<0) ms = 0;
    const s = ms/1000;
    const m = s/60;
    const h = m/60;
    const d = h/24;
//     const w = d/7;
//     const mo = w/4;
    const rnd = Math.round;
    if (d > 6) return showDayShortYear(cd, time)
    else if (h > 23) return rnd(d)+'d'
    else if (m > 59) return rnd(h)+'h'
    else if (s > 59) return rnd(m)+'m'
    else return rnd(s)+'s';
}

function showDayShortYear(cd, d)
{
    // в декабре надо показывать до Jan, потом уже Dec'YY
    // в январе надо до сентября без года, затем Aug'YY
    const cy = cd.getFullYear();
    const cm = cd.getMonth(); // zero based
    const tm = 5;
    const threshold =
        cm >= tm ? new Date(cy, 0, 1) : new Date(cy-1, cm-tm+12, 1);
    return d >= threshold
        ? ((cd.getMonth() != d.getMonth() || cd.getDate() != d.getDate())
           ? (months[d.getMonth()] + ' ' + d.getDate())
           : '')
        : (months[d.getMonth()] + ' ’' + d.getFullYear().toString().substr(2));
}


export function showTimeAndAgo(t) {
    return formatDate(new Date(t/1000), true, false);
}

// const months =
//     ["January","February","March","April","May","June","July","August",
//      "September","October","November","December"];
const months =
   ["Jan","Feb","Mar","Apr","May","Jun",
    "Jul","Aug","Sep","Oct","Nov","Dec"];

function showDay(cd, d)
{
    return ''
        + ((cd.getFullYear() != d.getFullYear()
            || cd.getMonth() != d.getMonth()
            || cd.getDate() != d.getDate())
           ? months[d.getMonth()] + ' ' + d.getDate()
          : '')
        + (cd.getFullYear() != d.getFullYear() ? ', ' + d.getFullYear() : '');
//        + '.' + d.getMilliseconds();
}

function formatDate(d,span,onlyspan) {
    if (onlyspan) return ago(d);
    const sp = span ? ' (' + ago(d) + ')' : '';
    const cd = new Date();
    const day = showDay(cd, d);
    function pad(n){
        if (('' + n).length == 1) return '0' + n;
        else return n;
    }

//     return (day != '' ? day + ' ' :
//             pad(d.getHours()) + ':' + pad(d.getMinutes())) + sp;
    return (day != '' ? day + ' at ' : '')
        + pad(d.getHours()) + ':' + pad(d.getMinutes()) + sp;
//        + ':' + pad(d.getSeconds()) + '.' + d.getMilliseconds() + sp;
    }

function ago(d) {
    function seconds(ms){return ms*1000;}
    function minutes(ms){return seconds(ms)*60;}
    function hours(ms){return minutes(ms)*60;}
    function days(ms){return hours(ms)*24;}
    function weeks(ms){return days(ms)*7;}

    let ms = Date.now() - d;
    if (ms<0) ms = 0;
    const rnd = Math.round;
    let out = '';
    function range(x,y){return ms >= x && ms <= y;}
    if (range(0,seconds(1))) out = "a second";
    else if (range(0,minutes(1) - seconds(10))) out = "just now";
    else if (ms >= minutes(1) - seconds(10)
             && ms <= minutes(1) + seconds(10)) out = "about a minute";
    else if (ms >= minutes(25) && ms <= minutes(35)) out = "half an hour";
    else if (ms > hours(1) - minutes(10) && ms < hours(1) + minutes(10)) out = "about an hour";
    else if (ms < hours(1)) out = rnd(ms / minutes(1)) + " minutes";
    else if (rnd(ms / hours(1)) < 2) out = "an hour";
    else if (ms < days(1)) out = rnd(ms / hours(1)) + " hours";
    else if (ms >= days(1) && ms <= days(2)) out = "a day";
    else if (ms < weeks(1)) out = rnd(ms / days(1)) + " days";
    else if (ms > weeks(1) && ms < weeks(2)) out = "a week";
    else if (ms > weeks(1) && ms < weeks(4)) out = rnd(ms / weeks(1)) + " weeks";
    else if (rnd(ms / weeks(4)) < 2) out = "a month";
    else if (ms < weeks(50)) out = rnd(ms / weeks(4)) + " months";
    else if (rnd(ms / weeks(52)) < 2) out = "a year";
    else out = rnd(ms / weeks(52)) + " years";
    return out == "just now" ? out : out + " ago";
}
export function locationHost() { return location.host }

export function setDocumentTitle(t)
{
    document.title = t;
}
export function setTimeoutUr(what, code, time)
{
    setTimeout(function () {
//        uw_debug("timeout: "+what);
        execF(code);
    }, time);
}
export function focus(i) { const e = elem(i); if (e) { e.focus();} }
export function setReadOnly(i, r) {
    const e = elem(i);
    if (!e) return;
    if (r)
        e.setAttribute("readonly", "readonly");
    else
        e.removeAttribute("readonly");
}
export function blur(i) { elem(i).blur(); }
export function select(i) {
    const e = elem(i);
    if (!e) return;
//     if (e.className && e.className.search("Url") != -1 && !navigator.userAgent.match(/msie\s(\d)/i))
//         e.type = "url";
    e.setAttribute("autocapitalize", "none");
    e.setAttribute("spellcheck", "false");
    if (!e.className.match(/login|password/i))
        e.setAttribute("autocomplete", "none");

    if (iOS) {
        // iOS не выделяет содержимое элемента по e.select()
        // для iOS 12 (или раньше?) достаточно setSelectionRange,
        // для предыдущих можно и помощнее
        if (iOS[0] >= 12)
            e.setSelectionRange(0, 999999);
        else {
            let editable = e.contentEditable;
            let readOnly = e.readOnly;

            e.contentEditable = true;
            e.readOnly = false;

            let range = document.createRange();
            range.selectNodeContents(e);

            let selection = window.getSelection();
            selection.removeAllRanges();
            selection.addRange(range);

            e.setSelectionRange(0, 999999);
            e.contentEditable = editable;
            e.readOnly = readOnly;
        }
    } else {
        e.select();
    }
}
function forcedBlur() {
    // activeElement.blur() не работает на iframe, body.focus() также
    // не перемещает фокус с iframe, по-этому делаем невидимую кнопку
    if (!_dummyButton) {
        _dummyButton = $('<button class=dummyButton />').appendTo('body')[0];
    }
    _dummyButton.focus();
    _dummyButton.blur();
}
let _dummyButton = null;
function tryResetFocus() {
    if (isMobile) {
        return;
        // forcedBlur() в Android Сhrome вызывает прокрутку в начало.
        // При этом на мобилах без клавиатуры forcedBlur() всё равно не нужен
    }
    const ae = document.activeElement;
    if (!$(ae).parents('.msgFrame').length)
        return;
    const b = ae.getBoundingClientRect();
    // сбрасываем фокус с iframe/video как только они уходят за пределы окна
    if (b.bottom < boundingClientRectHeight('topId')
        || b.top > window.innerHeight) {
        //uw_debug('clear focus from ' + ae.tagName);
        forcedBlur();
    }
}
export function openLink(l) {
    if (l.indexOf("mailto:") == 0) {
        const w=window.open(l, "_blank");
        if (navigator.userAgent.match(/chrome/i))
            setTimeout(function () {
                if (w.document.location == "about:blank") w.close() }, 1000); // даем время загрузиться gmail
        else
            w.close();
//        ;
    }
    else {
        const w = window.open(l, "_blank");
        w.opener = null;
        // для защиты от window.opener уязвимости, не работает в Safari
    }
    // к сожалению, в web-app в iOS открывает ссылку в том же окне
}
const bqrExtensionId = "bqr-extension-installed";
export function openLinkInBackgroud(l) {
    const e = elem(bqrExtensionId);
    if (e) {
        window.postMessage({ bqr_request: {openLinkInBackground: l }}, "*");
        e.onclick = function () {
            if (e.getAttribute("bqr_response") != "ok")
                openLinkInBackgroundNoExt(l);
        };
    } else
        openLinkInBackgroundNoExt(l)
}
function chromeVersion() {
    const cv = navigator.userAgent.match(/chrome\/([0-9]+)/i);
    return cv ? cv[1] : null;
}
function safariVersion() {
    let sv = navigator.userAgent.match(/safari\/([0-9]+)/i);
    if (sv)
        sv = navigator.userAgent.match(/applewebkit\/([0-9]+)/i);
    // у старых Safari были 4-хзначные версии, не совпадающие с версией webkit
    return sv ? sv[1] : null; // версия WebKit, а не Safari
}
function firefoxVersion() {
    const fv = navigator.userAgent.match(/firefox\/([0-9]+)/i);
    return fv ? fv[1] : null;
}

export function openLinkInBackgroundNoExt(l) {
    let extension = null;
    if (chromeVersion() >= 41)
    {
        extension = {
            message: 'Chrome no longer allows sites to open links in background tabs programmatically.\n\nYou need to install “BazQux Reader: open links in background tab” browser extension for this.\n\nDo you want to install the extension?\n\n(please, click “Add to Chrome” on the extension page)',
            link: 'https://chrome.google.com/webstore/detail/dfoegpibjpjpchgmjnmomelfnclbijnm' }
    } else if (firefoxVersion() >= 57) {
        extension = {
            message: 'Firefox does not allow sites to open links in background tabs programmatically.\n\nYou need to install “BazQux Reader: open links in background tab” browser extension for this.\n\nDo you want to install the extension?\n\n(please, click “Add to Firefox” on the extension page)',
            link: 'https://addons.mozilla.org/addon/bazqux-reader-bg/'
        }
    }

    if (extension) {
        if (confirm(extension.message))
            openLink(extension.link);
        else
            openLink(l);
    } else {
        openLink(l);
    }
}
export function windowClose() { window.close() }
export function noModifiers() {
    const e = window.event ? window.event : uw_event;
    return !(e && (e.ctrlKey || e.metaKey || e.altKey) && e.keyCode != 38 && e.keyCode != 40);
}
export function reloadPage() { window.location.reload(); }

function subItemNode(node)
{
    if (node == null) return null;
    if (node.id == null || node.id.substr(0,3) != "cls")
        return subItemNode(node.parentNode);
    else
        return node;
}

function nodeToSubItem(node)
{
    const si = subItemNode(node);
    return si != null ? subItems.get(domIdToSubItemIndex.get(parseInt(si.id.replace(/cls/,'')))) : null;

}

function dragMark() { return elem("Main.dragMarkId"); }
function siNodeParentSubItem(n)
{
    while (n) {
        if (n.id && n.id.indexOf("folder") == 0) {
            return subItems.get(parseInt(n.id.replace(/folder/,'')));
        }
        n = n.parentNode;
    }
    return null;
}

function findDragTarget(y, folderOK, nodeCheck)
{
//    uw_debug(`findDragTarget y = ${y}`);
    let minO = 1e9;
    const r = {};
    const maxY = $('.subscriptions')[0].scrollHeight;
    const last = {};
    let lastNode = null;
    let atTheEnd = false;
    for (const domId of sortedDomIds)
    {
        const si = subItems.get(domIdToSubItemIndex.get(domId));
        const node = elem('cls'+domId);
        if (node && Math.round(node.offsetTop) > 0) // видимая нода
            lastNode = node;
        if (si == null || node == null || !nodeCheck(node, si)) {
//            r._Before = true; // под следующей нельзя?? -- так тоже теги не работают
            continue;
        }
        const ch = node.clientHeight;
        const nodeTO = node.getBoundingClientRect().top;
        const o = y - nodeTO;
        if (folderOK && isFolder(si) && o > ch/4 && o < ch-(ch/4))
            return { _SubItem : si, _Node : node,
                     _ToFolder : true, _ParentSubItem : null, _Before : false }
        if (Math.abs(o) < minO) {
            last.o = o;
            last.ch = ch;
            minO = Math.abs(o);
            last.si = r._SubItem ? r._SubItem : subItems.get(0);
            last.node = r._Node ? r._Node : elem('cls0');
            r._SubItem = si;
            r._Node = node;
            r._ToFolder = false;
            r._ParentSubItem = siNodeParentSubItem(node);
            r._Before = true;
            if (o >= maxY - ch/2 - 2) // последний элемент в списке
                r._Before = false;
            if (o >= maxY - 5)
                atTheEnd = true;
        }
    }
    if (!r._SubItem) return null;
    const draggingTag = r._SubItem._Path.indexOf("tag/") == 0;
    if (draggingTag && last.o > last.ch/2)
        r._Before = false; // ниже последнего тега

    if (r._Before) {
        r._SubItem = last.si;
        // используем для рисования ноду, к которой ближе мышка, а для
        // вставки используем предыдущую ноду
    } else if (!draggingTag && lastNode) {
        r._Node = lastNode;
        // в случае последней развернутой папки используем не заголовок
        // папки, а последнюю подписку
    }


    if (!r._ToFolder && r._Before
        && r._ParentSubItem == null && siNodeParentSubItem(last.node) != null) {
        // ^ на границе последнего элемента в папке и следующего top-level
        if (last.o < 0) {
            r._ParentSubItem = siNodeParentSubItem(last.node);
            r._SubItem = last.si;
            r._NodeToInsertAfter = last.node;
            // вставляем в предыдущую папку, после последней подписки
        } else {
            r._SubItem = siNodeParentSubItem(last.node);
            // вставляем после предыдущей папки
        }
    }
    if (!draggingTag && lastNode && atTheEnd &&
        siNodeParentSubItem(lastNode) != null) {
        // если в конце развернутая папка
        r._SubItem = siNodeParentSubItem(lastNode);
        r._ParentSubItem = null;
    }

    return r;
}

let draggingSubItem = null;
let draggingSubItemNode = null;
let lastDragTarget = null;
let onDragAndDrop = null;

export function registerOnDragAndDrop(c) { onDragAndDrop = c; }
export function isDragAndDropActive() { return draggingSubItem != null; }

function onSubItemEvent(event)
{
//    uw_debug('onSubItemEvent ' + event.type);
    if (event.type == "dragstart") {
        if (!leftPanel.isMovable()) {
            // ^ сама панель является popup-ом
            popupsHide();
        }
        event.originalEvent.dataTransfer.dropEffect = "move";
        draggingSubItemNode = subItemNode(event.target);
        draggingSubItem = nodeToSubItem(event.target);
        $(draggingSubItemNode).addClass('dragging');
        const p = rootPath("/i/" + draggingSubItem._Path);
        event.originalEvent.dataTransfer.setData("text/uri-list", p);
        event.originalEvent.dataTransfer.setData("text/plain", p);
        // ^ setData нужен для firefox
//        event.originalEvent.dataTransfer.setDragImage($(draggingSubItemNode).children(".subscriptionItem").children(".buttonText")[0], 0, 0);
        event.originalEvent.dataTransfer.setDragImage(draggingSubItemNode, 0, 0);
    } else if (event.type == "dragover") {
        if (draggingSubItem == null) return;
        const dhash = draggingSubItem._Path;
        const y = event.originalEvent.clientY;
        let dt = null;
        if (dhash == "tags" || dhash == "starred" ||
            dhash.indexOf("folder/") == 0 ||
            dhash.indexOf("smartstream/") == 0)
            // top-level
            dt = findDragTarget(y, false,
                                function (n,si)
                                { return si._Index > 0 &&
                                         siNodeParentSubItem(n) == null });
        else if (dhash.indexOf("tag/") == 0)
            // тег
            dt = findDragTarget(y, false,
                                function (n,si)
                                { return si._Path.indexOf("tag/") == 0; });
        else if (dhash.indexOf("subscription/") == 0)
            // фид
            dt = findDragTarget(y, true,
                                function (node,si)
                                { return si._Index > 0 &&
                                         si._Path.indexOf("tag/") != 0; });
        if (dt == null)
            return;
        lastDragTarget = dt;
        const siO = Math.round(dt._Node.offsetTop) + 1
              - $('.left .flexFullHeight').offset().top;
        const ch = Math.round(dt._Node.clientHeight);
        const mark = dragMark();
        mark.style.display = 'block';
        $(mark).toggleClass('folderRectangle', dt._ToFolder);
        $(mark).toggleClass('inFolder', dt._ParentSubItem != null);
        if (dt._Before) {
            mark.style.top = siO + 'px';
        } else {
            mark.style.top = siO + ch + 'px';
        }
        if (dt._ToFolder) {
            mark.style.height = (ch - 2) + 'px';
            mark.style.marginTop = (-(ch+2)) + 'px';
        } else {
            mark.style.marginTop = '-2px';
            mark.style.height = '2px';
        }
        event.preventDefault(); // возможно сделать drop
        event.originalEvent.dataTransfer.dropEffect = "move";
    } else if (event.type == "drop") {
        $(".left").addClass("pointerEventsNone");
        // чтобы safari/chrome не добавляли лишний :hover к случайным
        // соседним элементам
        // https://bugs.chromium.org/p/chromium/issues/detail?id=410328
        event.preventDefault(); // чтобы не открывал как ссылку
        const dt = lastDragTarget;
        if (dt == null) return;
        const nsi = nodeToSubItem(dt._Node);
        const srcPSI = siNodeParentSubItem(draggingSubItemNode);
        if (dt._SubItem._Index == draggingSubItem._Index
            || (nsi._Index == draggingSubItem._Index &&
                !((srcPSI == null && dt._ParentSubItem != null)
                 || (srcPSI != null && dt._ParentSubItem == null)))
                // >>>>
                //   --
                // ---- ^ перемещение в папку из top-level
            || (dt._ToFolder && srcPSI && srcPSI._Index == dt._SubItem._Index)
           ) {
//             uw_debug("no move");
            return;
        }
//         uw_debug(dt._SubItem._Path+"\n"+
//                  (dt._ParentSubItem ? dt._ParentSubItem._Path : "")+"\n"+
//                  "to folder: " +dt._ToFolder+"; before: "+dt._Before);

        if (!dt._ToFolder) {
            if (dt._NodeToInsertAfter) {
                dt._Node = dt._NodeToInsertAfter;
                dt._Before = false;
            }
            if (!dt._Before && siNodeParentSubItem(dt._Node) != null &&
                dt._ParentSubItem == null) {
                // последний элемент в развернутой папке, а вставить надо
                // не в папку
                dt._Node.parentNode.parentNode.insertBefore(
                    draggingSubItemNode, null);
            }
            else
                dt._Node.parentNode.insertBefore(
                    draggingSubItemNode,
                    dt._Before ? dt._Node : dt._Node.nextSibling);
            if (isFolder(draggingSubItem) || draggingSubItem._Path == "tags")
                draggingSubItemNode.parentNode.insertBefore(
                    elem("folder" + draggingSubItem._Index),
                    draggingSubItemNode.nextSibling);
        }

        if (onDragAndDrop)
            execF(execF(onDragAndDrop,
                        { _What : draggingSubItem._SIType,
                          _SourceFolder : srcPSI ? siFolderName(srcPSI) : null,
                          _TargetFolder : dt._ParentSubItem ? siFolderName(dt._ParentSubItem) : (dt._ToFolder ? siFolderName(dt._SubItem) : null),
                          _InsertAfter : dt._ToFolder ? null : dt._SubItem._SIType
                        }));
    } else if (event.type == "dragend") {
        $(".left").addClass("pointerEventsNone");
        setTimeout(() => { $(".left").removeClass("pointerEventsNone") }, 0);
        dragMark().style.display = 'none';
        if (draggingSubItemNode != null)
            $(draggingSubItemNode).removeClass('dragging');
        lastDragTarget = null;
        draggingSubItem = null;
        draggingSubItemNode = null;
    } else if (event.type == "selectstart") {
        if (event.target.dragDrop)
            event.target.dragDrop(); // для IE9
        return false;
    } else if (event.type == "contextmenu") {
//        uw_debug(`contextmenu x = ${event.clientX}, y = ${event.clientY}`);
        const si = nodeToSubItem(event.target);
        if (si && !(event.metaKey || event.altKey)) {
            execF(execF(onSubscriptionRightClick, si._Index));
            return false;
        }
    }
}

let onSubscriptionRightClick = null;

export function setOnSubscriptionRightClick(on)
{
    onSubscriptionRightClick = on;
}

export function hasOnscreenKeyboard()
{
    return isTouchDevice;
}

let prevScrollTop = 0;

function patchUrWeb () {
    uw_event = {}; // чтобы uw_stopPropagation не падал при перезагрузке страницы
    if (window.orig_exec)
        return; // patched

    window.property = (s) => {
        if (s.length <= 0)
            er("Empty CSS property");

        if (!isLower(s[0]) && s[0] != '_' && s[0] != '-')
            er("Bad initial character in CSS property");

        for (var i = 0; i < s.length; ++i) {
            var c = s[i];
            if (!isLower(c) && !isDigit(c) && c != '_' && c != '-')
                er("Disallowed character in CSS property");
        }

        return s;
    }

    window.orig_exec = window.exec;
    window.exec = function(e) {
        if (e != null)
            // иногда popup/menu может уже прибиться, а обработчик вызывается
            // с null-аргументом (результатом cr(NNN) -- убитого замыкания)
            // например в onclick='uw_event=event;exec(cr(NNN))'
            return window.orig_exec(e);
    };
//     window.sv = function (s, v) {
//         if (!_.isEqual(s.data, v)) { // тормозит
//             s.data = v;

//             for (const ls = s.dyns; ls; ls = ls.next)
//                 populate(ls.data);
//         }
//     }

    window.whine = function(msg)
    {
        throw msg; // убираем urweb-овский alert
    };
    window.atr = function(s) { // фикс urweb-овского atr
        return s.replace(/"/g, "&quot;");
    };
    function errorText(msg) {
        const rx = /(.*)errorText.*>((.|\n|\r)*)<\/pre>(.*)/g;
        const arr = rx.exec(msg);
        if (arr && arr.length >= 3) {
            return arr[2]
        } else {
            const div = document.createElement('div');
            div.innerHTML = msg.replace(/<title>.*<\/title>/,"");
            return $(div).text(); // innerText не работает в FF
        }
    }
    const timeout = 180000;
    // наш таймаут на обработку 2 минуты, но с медленным интернетом
    // может 30 секунд только пинг идти.
    window.requestUri = function(xhr, uri, needsSig, isRpc) {
        let extraData = null;
        let maxLen = uri.substr(1).search("/")+2;
        let headers = {};

        if (uri.match(/^\/[A-Z]/))
            maxLen = uri.substr(maxLen).search("/")+maxLen+1;
            // добавляем имя модуля, если есть
        // необходимо полное имя ф-ии (иначе 404 Not found)
        // и нужен слеш в конце, т.к. может unurlify не сработать.
        // в discovery можно вбить "ad1." или "adops." и adblock порежет запрос,
        // таких шаблонов много, по-этому режу все, после названия ф-ии

        if (isRpc && uri.length > maxLen) {
            extraData = uri.substring(maxLen);
            uri = uri.substring(0, maxLen);
        }
//        uw_debug("extraData.length is " + extraData.length);

        headers["Content-Type"] = "text/plain";

        if (client_id != null) {
            headers["UrWeb-Client"] = client_id.toString();
            headers["UrWeb-Pass"] = client_pass.toString();
        }

        if (needsSig) {
            if (sig == null)
                whine("Missing cookie signature!");

            headers["UrWeb-Sig"] = sig;
        }

        try {
            xhr.open("POST", uri, !window.unloading);

            for (const n in headers) {
                xhr.setRequestHeader(n, headers[n]);
            }

            if (!window.unloading) {
                // не отменяем последний bgactions
                window.inFlight = cons(xhr, window.inFlight);

                xhr.timeout = timeout;
            }
            // в IE11 timeout можно ставить только после open
//             const err = null;
//             xhr.ontimeout = function(e) {
//                 err = "timeout";
//                 // приходит после onreadystatechange
//             };
//             xhr.onerror = function(e) {
//                 err = "some error";
//             };

            xhr.send(extraData);
        } catch (_) {
            // Chrome 80 не позволяет синхронный XHR, а асинхронный игнорирует.
            // Используем fetch c keepalive.
            // (Chrome 81 вернул deprecation вместо exception)
            //
            // А в iOS Safari не работает fetch в onunload, говорит
            //   "Fetch API cannot load _ due to access control checks."
            // хотя стоит same-origin.
            // Так что используем Fetch только если был exception от XHR.
            // Ха, синхронный XHR в iOS Safari тоже пока не работает
            // (а асинхронный так же пишет "due to access control checks",
            // подождём обновления iOS.
            // И пока оставим с try, вдруг где старый синхронный XHR работает
            //
            // Firefox вообще не вызвает onunload/onbeforeunload
            // когда закрывается последняя вкладка (или весь браузер)
            if (window.unloading && window.fetch) {
                // fetch не умеет пользовательский timeout,
                // так что используем его только для keepalive-запросов
                fetch(uri, {
                    method: "POST",
                    headers: headers,
                    body: extraData,
                    mode: "same-origin",
                    keepalive: true
                });
            }
        }
    };
    window.conn = function (msg) {
        servErr(errorText(msg));
    }
    window.rc = function (prefix, uri, parse, k, needsSig, isN) {
        function exn(what, e, updated) {
            doExn(what + ": "
                  + e.replace(/\(\/.*\/bazqux\//,'(')
                  // убираем путь из Pattern match failure.
                  + (updated ? updated : ""));
        }
        if (!maySuspend)
            er("May not 'rpc' in main thread of 'code' for <active>");

        uri = cat(prefix, uri);
        uri = flattenLocal(uri);
        const xhr = getXHR();
        const t1 = Date.now();

        xhr.onreadystatechange = function() {
            if (xhr.readyState == 4) {
                let isok = false;

                try {
                    if (xhr.status != 200) {
                        const e = errorText(xhr.responseText);
                        if (e == 'Wrong cookie signature' // куку удалили
                            || e == 'Not logged in') {
                            window.location.href = "/please_log_in_again";
                            return;
                        }
                    }
                    if (xhr.status == 200)
                        isok = true;
                } catch (e) { }

                const updated = "\n(probably reader was updated and you need to reload the page)";

                if (isok) {
                    const lines = xhr.responseText.split("\n");
                    if (lines.length != 2) {
                        if (isN == null)
                            whine("Bad RPC response lines");
                        else
                            k(null);
                    } else {
                        eval(lines[0]);
                        let v = null;

                        requestAnimationFrameF(function () {
                            // не обрабатываем rpc при прокрутке
                        try {
                            //uw_debug('parsing ' + lines[1].length + ' bytes');
                            //console.time('rpc parse');
                            v = parse(lines[1]);
                            //console.timeEnd('rpc parse');
                        } catch (e) {
                            exn("Can’t parse request result",
                                e.message ? e.message : e, updated);
                            //k(null);
                        }
                        try {
                            k(makeSome(isN, v));
                        } catch (e) {
                            let m = e.message ? e.message : e;
                            exn("Can’t process request result", m,
                                (m.indexOf("Pattern match failure") != -1
                                 || m.indexOf("Error unurlifying") != -1
                                 || m.indexOf("null") != -1
                                 || m.indexOf("undefined") != -1
                                )
                                ? updated : "");
                        }});
                    }
                } else {
                    if (isN == null) {
                        const t2 = Date.now();
                        if (xhr.status == 0) {
                            conn(t2 - t1 > timeout-1000
                                 ? "Timeout connecting to the server (try check your network or reload the page)"
                                 : "Can’t connect to the server (try check your network or reload the page)");
                            // к сожалению, более подробной информации об ошибке
                            // в XHR нет
                        } else if (xhr.status == 404)
                            conn("404 Not found" + updated);
                        else {
                            const m = errorText(xhr.responseText);
                            conn(m +
                                 ((m.indexOf("Error unurlifying") != -1)
                                  ? updated : ""));
                        }
                    } else
                        k(null);
                }

                xhrFinished(xhr);
            }
        };

        requestUri(xhr, uri, needsSig, true);
    };

//     for (const i in urfuncs) {
//         const e = urfuncs[i];
//         if (e.c == "t")
//             urfuncs[i] = eval("(" + e.f + ")");
//     }
    // если убрать eval из exec1, то Chrome все равно жалуется
    // что exec1 optimized too many times
}

$(document).ready(patchUrWeb);

const dummyEvent = {}; // чтобы uw_preventDefault/uw_stopPropagation работали

function callOnscroll() {
    (msgTreeDiv().onscroll || document.body.onscroll)(dummyEvent);
}

export function jsInit()
{
    $("html").attr("lang", "en-US");
    setVariable("--scrollbar-width", window.mqGenie.width + "px");
    if (isMobile) {
        scrollingElement().setAttribute("id", "msgDivId");
        document.addEventListener("scroll", function() {
            callOnscroll();
        }, passiveListener(true));
    }
    saveMediaRulesToUpdate();
    registerOnOrientationChange(updateMediaRules);

    patchUrWeb();

    window.setTimeout(function () {
    // <body> в onload еще нет, посему setTimeout
    if (!isTouchDevice) {
        $('.subscriptions').on('dragstart contextmenu selectstart', 'li', onSubItemEvent);
        $('.left').on('dragend dragover drop', onSubItemEvent);
        // на весь left, чтобы пустое место под subscriptions тоже работало
//         $('.discoveryContents').on('mousedown', 'li', function (e) {
//             if (e.button == 0)
//                 setTimeout(function () {$('.discoverySearchInput input').focus();},0);
//         })
    }

    (isMobile ? document : msgTreeDiv()).addEventListener('scroll', () => {
        if (!isFullScreen()) {
            const st = msgTreeDiv().scrollTop;
//             uw_debug('prevScrollTop = ' + st);
            prevScrollTop = st;
            tryResetFocus();
        }
//        uw_debug('onscroll ' + st);
    }, passiveListener(true));
    $(document).on('webkitfullscreenchange mozfullscreenchange fullscreenchange',  () => {
        fullScreen = isFullScreenReal();
        // на iPad iOS 14 после выхода из fullscreen приходит лишний scroll на 0
        // (isFullScreenReal() на этот момент уже возвращает false),
        // потом fullscreenchange и потом снова scroll на последнюю позицию.
        // При этом реальной прокрутки на 0 нет.
        // Чтобы обрабатывать такие случаи и не запоминать некорректный
        // prevScrollTop, запоминаем состояние fullScreen в обработчике
        // fullscreenchange.
        if (!fullScreen) {
            msgTreeDiv().scrollTop = prevScrollTop;
//             uw_debug('set scrollTop = ' + prevScrollTop);
        }
    });
    if (isMobile) {
        // Получается, что пока мы в edit-е, даже если onscreen keyboard уже
        // спрятан, мы не можем прокручивать страницу.
        // Но как отлавливать именно момент глюка с прокруткой
        // в mobile Edge все равно непонятно, так что лучше так.
        $('body').on('focus', 'input[type=text], input[type=password]', (e) => {
            if (inputFocusFixActive())
                return;

            if (iOS && iOS[0] >= 8 && (iOS[0] < 10 || (iOS[0] == 10 && iOS[1] < 3)))
            {
                // https://stackoverflow.com/questions/29001977/safari-in-ios8-is-scrolling-screen-when-fixed-elements-get-focus#35675094
                // в iOS 8-10.3 при переходе в input внутри fixed элемента
                // высота html/body становится неограниченной, сбрасывается
                // scrollTop и прокручивается всё окно целиком
                // вместе с fixed элементами.
                // - Попытка поставить scrollTop не работает,
                //   т.к. прокручивается все окно
                // - установка height/max-height для html/body игнорируется
                //   (работет только для .left, причем ставить по-умолчанию
                //    100vh вместо 100% не канает, т.к. 100vh иногда бывает
                //    выше реального viewport)
                // Помогает body { position:relative, top:… }, но body
                // по-прежнему можно прокрутить и ничего с этим не сделаешь
                // Реагировать на focus, т.к. в touchstart еще все в порядке
                // и появляется лишнее моргание.
                $('body').css('top' , `${-prevScrollTop}px`);
                $('body').addClass('positionRelativeImportant');
                $('.left').css('height', '100vh');
                // в iOS 11 есть небольшая прокрутка наверх при выборе input,
                // установка scrollTop также прокручивает все окно
                // вместе с fixed-элементами. Поскольку scrollTop у iOS 11
                // уже не сбрасывается, приходится его сбрасывать самому.
//                 msgTreeDiv().scrollTop = 0;

                // этот же код работает в Android Chrome/FF,
                // но уж очень он нестандартный
            }

            $('html').addClass('overflowHiddenImportant');
            // ^ необязательно для iOS, но чуть меньше перемещает
            //   окно вверх для input
            // ^ в mobile Edge вроде как помогает
            //   причем в нем глючит только при нажати на search input
            //   (add subscription работает нормально), дополнительная прокрутка
            //   также прокручивает search autocomplete
            // ^ android сам по себе работает нормально -- если address bar
            //   не скрыт, то вообще ничего не двигает, а если скрыт,
            //   то показывает его, при этом стараясь не двигать содержимое.
            //   получается, что как будто немного скролит наверх,
            //   если добавить html{ overflow: hidden }, то прокручивает
            //   содержимое вместе с address bar+top, но не факт, что это
            //   нужно делать

            // Резюме: html{overflow:hidden} стоит оставить везде,
            // а фикс для старых iOS только в них, прокрутка на несколько
            // строк не так страшна, как полная прокрутка наверх
            // (плюс, ее скорее всего со временем окончательно исправят).

            // blur не приходит при удалении input из DOM
            // отлавливаем удаление input через MutationObserver
            const observer = new MutationObserver((mutationList, observer) => {
                mutationList.forEach((mutation) => {
                    if (mutation.type == 'childList') {
                        mutation.removedNodes.forEach((node) => {
                            for (let t = e.target; t; t = t.parentNode)
                                if (t == node) {
                                    clearInputFocusFix();
                                    observer.disconnect();
                                }
                        });
                    }
                })});
            observer.observe(document.body, {
                childList: true,
                subtree: true
            });
            $(e.target).one("blur", function(){
                observer.disconnect();
                clearInputFocusFix();
            });
        });
    }

    $('body').on('touchstart focus', 'input[type=text]', function(e){
        e.target.setAttribute("autocapitalize", "none");
    });

    if (isMobile) {
        $('.articles').on('click', () => {});
        // По каким-то непонятным причинам, без этого на старом iPad iOS 9.3
        // перестаёт вызываться document.addEventListener('click') в
        // addClickHandler (а с ним и uimOnClickHandler) при клике по элементам
        // list view или карточкам в mosaic/magazine.
    } else {
        $('.feedMark').each(function () {
            this.addEventListener('wheel', function (e) {
                const r = msgTreeDiv();
                r.scrollTop = r.scrollTop + e.deltaY;
            }, passiveListener(true))
        });
    }
    }, 0);

    fixAudioPlaceholderHeight();
    checkOpacityCalc();
    // setupFontUsageTracking();
}
let fullScreen = false;
export function isFullScreen()
{
    return isFullScreenReal() || fullScreen;
}
function isFullScreenReal()
{
    const e = document.fullscreenElement || document.mozFullScreenElement || document.webkitFullscreenElement;
    const r = (e || document.fullScreen || document.mozFullScreen || document.webkitIsFullScreen) ? true : false;
//    uw_debug('isFullScreen ' + r);
    return r;
}
export function clearPrevScrollTop()
{
    clearInputFocusFix();
    // сбрасываем фикс для focused input в fixed-элементе,
    // т.к. мы и так прокручиваемся вверх и хочется иметь работающую
    // прокрутку в результатах поиска
    prevScrollTop = 0;
}
function inputFocusFixActive() {
    return $('html').hasClass('overflowHiddenImportant');
}
function clearInputFocusFix() {
    if (inputFocusFixActive()) {
        const top = pf($('body').css('top'));
        const st = msgTreeDiv().scrollTop;

        $('html').removeClass('overflowHiddenImportant');
        $('body').removeClass('positionRelativeImportant');
        $('body').css('top', '');
        $('.left').css('height', '');

        if (!isNaN(top) && top != 0)
            msgTreeDiv().scrollTop = st - top;
    }
}
export let opmlOkCode = null;
export function opmlForm(body)
{
    let b = body;
    if (navigator.userAgent.match(/msie\s(\d)/i)) {
        b = flatten_(body).replace(/<div/, '<label for=opmlFile').replace(/<a/, '<label for=opmlFile').replace(/<\/a/, '</label').replace(/<\/div/, '</label').replace(/onclick/, 'data-c');
    }
    /* с display:none не работает в Opera */
    return cat(b, `<form style='visibility:hidden;float:left;width:1px;height:0;' name='opmlForm' method="post" action="/importOPML" enctype="multipart/form-data"><input type='hidden' name='Sig' value='${sig}' /><input id='opmlFile' type="file" name="OPML" onchange='if(!this.value)return;if(bq.opmlOkCode)execF(bq.opmlOkCode);document.forms["opmlForm"].submit()' onclick='uw_event=event;uw_stopPropagation();' /><input type="submit" onclick='uw_event=event;uw_stopPropagation();' /></form>`);
}
export function opmlUpload(code)
{
    opmlOkCode = code;
    elem("opmlFile").click();
    opmlOkCode = null;
}

export function windowInnerHeight() { return window.innerHeight }
export function viewportWidth() { return $(window).width() }
export function viewportHeight() { return Math.max($(window).height(), window.innerHeight) }
export function windowDevicePixelRatio() { return window.devicePixelRatio }
export function complete(id) { return elem(id) ? elem(id).complete : false; }
export function scrollTop(id) { return elem(id).scrollTop; }
export function setScrollTop(id, x) { if (elem(id)) elem(id).scrollTop = x; }
export function setScrollTopNoOnScroll(id, x) {
    const e = elem(id);
    if (!e) return;
    const os = e.onscroll;
    e.onscroll = function () {
        e.onscroll = os;
    };
    e.scrollTop = x;
}
export function clientHeight(id) { return elem(id).clientHeight; }
export function scrollHeight(id) { return elem(id).scrollHeight; }
function position(e) {
    return { top : e[0].offsetTop, left : e[0].offsetLeft }
//     // новый jQuery игнориует scrollTop,
//     // т.е. теперь $(e).position().top != e.offsetTop
//     const op = e.offsetParent();
//     const p = e.position();
//     const p0 = $(op).find('.articles').position();
//     const st = p0 ? -p0.top : op.scrollTop();
//     // Safari/FF округляют scrollTop, ориентируемся по позиции первого элемента
//     const t = Math.round((p.top + st)*1024)/1024;
//     // ^ чуть округляем, чтобы не дрожали младшие разряды при прокрутке
//     // метки фида
// //    uw_debug(`st = ${st}, p.top = ${p.top}, p.top + st = ${p.top + st}, t = ${t}`);
//     return (op && st && !(op[0] === scrollingElement()))
//         ? { top : t, left : p.left }
//         : p;
}
export function positionTop(id) {
    const e = elem(id);
    return e ? position($(e)).top : 0;
}

let browserScale = 1; // только в Chrome работает
let browserScaleDiv = null;

// Desktop Chrome при zoom делает дробный scrollTop, по-этому мы округляем
// положение сообщений так же, как это делает Chrome, чтобы не было частично
// видимых border-top.
// Но, в некоторых случаях, Chrome все равно неверно устанавливает scrollTop
// (чтобы установить scrollTop=123.456 иногда может потребоваться присвоить
// scrollTop=123.567, т.е. чуть большую величину)
export function roundScrollTop(x) {
    return Math.round(x*browserScale) / browserScale;
}

export function updateBrowserScale() {
    if (isMobile || !chromeVersion())
        return;

    if (!browserScaleDiv) {
        const sd = document.createElement('div');
        sd.className = "browserScale";
        sd.innerHTML = "<div class=browserScaleInner></div>";
        document.getElementsByTagName('body')[0].appendChild(sd);
        browserScaleDiv = sd;
    }

    browserScale = 1;

//    console.time('bs');
    for (let i = 0.2; i <= 5; i+=(i>=1 ? 1 : 0.2)) {
        browserScaleDiv.scrollTop = i;
        // работает только в Chrome, остальные браузеры округляют scrollTop
        const st = browserScaleDiv.scrollTop;
        if (st > 0) {
            browserScale = Math.round(1000/st)/1000;
            //uw_debug('browserScale = ' + browserScale);
            break;
        }
    }
//    console.timeEnd('bs');
}
let _emDiv = null;
function emDiv() {
    if (!_emDiv) {
        _emDiv = $('<div class=emDiv>a</div>').appendTo('body')[0];
    }
    return _emDiv;
}

let _mediaRulesToUpdate = [];

function saveMediaRulesToUpdate() {
    for (const r of cssParser.getMediaRules()) {
        if (r.media.mediaText.match(/m(in|ax)-width:\s*([0-9.]+)px/i))
            _mediaRulesToUpdate.push({ mediaText : r.media.mediaText, rule : r });
    }
}
function updateMediaRules() {
    const leftPanelWidth = pf(getVariable("--left-panel-width"));
    const insetWidth = safeAreaInset().width;
//     uw_debug(`insetWidth = ${insetWidth}`);
    for (const r of _mediaRulesToUpdate) {
        r.rule.media.mediaText =
            r.mediaText.replace(/m(in|ax)-width:\s*([0-9.]+)px/gi, (s) =>
                s.replace(/[0-9.]+/, (d) => {
                    const w = pf(d);
                    return ((w >= 10000 ? w - 10000 + leftPanelWidth : w)
                            + insetWidth).toString()
                }));
//         uw_debug(`${r.mediaText} => ${r.rule.media.mediaText}`);
    }
}


// Учитываем, что у браузера может быть установлен minimum font size
// и ставим ближайший к нему .fontSize
// с большим или равным размером шрифта (иначе Safari/FF начинают криво
// масштабировать line-height и летит верстка).
export function fixFontSize(cls) {
    const m = cls.match(/^(.*)([0-9]+)$/);
    if (!m)
        return cls;
    const prefix = m[1];
    const minFS = emDiv().clientHeight;
//    uw_debug(`minFS = ${minFS}`);
    const fs = (c) => {
        const r = cssParser.getRuleValue('.'+c, 'font-size');
        return r ? pf(r) : null;
    }
    if (fs(cls) >= minFS) {
        return cls;
    } else {
        let r = cls;
        for (let n = parseInt(m[2])+1; ; n++) {
            const c = prefix + n;
            const cfs = fs(c);
            if (!cfs) {
//                uw_debug(`Can’t find large enough font size. Returning ${r} (font-size: ${fs(r)} < ${minFS}`)
                return r;
            }
            if (cfs >= minFS) {
//                uw_debug(`Found large enough font size. Returning ${r} (font-size: ${cfs} >= ${minFS}`)
                return c;
            }
            r = c;
        }
    }
}

let _baselineTestDivs = null;
function baselineTestDiv(font, substyle='', s='') {
    return $(`<div class=baselineTestDiv${substyle} style='font-family: ${font}; ${s}'>H<span class=baselineTestDiv${substyle}_aligner></span></div>`).appendTo('body')[0];
}
function baselineTestDivs() {
    if (!_baselineTestDivs) {
        _baselineTestDivs =
            { subscription : baselineTestDiv('var(--header-font-family)'),
              subscriptionBold : baselineTestDiv('var(--header-font-family)', '', 'font-weight: bold'),
              articleHeader : baselineTestDiv('var(--header-font-family)', 'Article'),
              articleHeaderBold : baselineTestDiv('var(--header-font-family)', 'Article', 'font-weight: bold'),
              articleHeaderMinus1px : baselineTestDiv('var(--header-font-family)', 'ArticleMinus1px'),
              articleHeaderBoldMinus1px : baselineTestDiv('var(--header-font-family)', 'ArticleMinus1px', 'font-weight: bold'),
              paragraph : baselineTestDiv('var(--paragraph-font-family)', 'Article')
            }
    }
    return _baselineTestDivs;
}

export function setMediaVariable(media, n, v) {
    setMediaVariableTo(media, ':root', n, v)
}
export function setMediaVariableTo(media, sel, n, v) {
    cssParser.overrideMediaRule(media, sel, n, v)
}
function getMediaVariable(media, n) {
    return cssParser.getOverridenMediaRuleValue(media, ':root', n)
}
export function setVariable(n, v) {
    setVariableTo(':root', n, v);
}
export function setVariableTo(sel, n, v) {
//     if (sel == ':root')
//         document.querySelector(sel).style.setProperty(n, v);
//     else
    cssParser.overrideRule(sel, n, v)
    if (n == "--left-panel-width")
        updateMediaRules();
}

let _fontsLoading = false;
let _onFontsLoaded = null;
export function fontsLoading() { return _fontsLoading; }
export function registerOnFontsLoaded(f) { _onFontsLoaded = f }

// Вычисление компенсации для понижения baseline, чтобы надписи были вровень
// с иконками. Браузеры устанавливают baseline в зависимости от размеров,
// прописанных в шрифте (обычно, чтобы строчные буквы были примерно по-середине
// line-height), нам же нужно, чтобы прописные буквы были примерно по-середине
// иконок, т.к. названия фидов обычно начинаются с большой буквы и несовпадение
// с иконкой очень бросается в глаза.
export function updateBaselineCompensation(rfsClass, afsClass) {
    const hff = getVariable("--header-font-family");
    const pff = getVariable("--paragraph-font-family");
    const f = document.fonts;
    if (f && f.load) {
        // Edge и iOS 9 не поддерживают, остальные браузеры поддерживают
        //         console.time('document.fonts.load');
        _fontsLoading = true;
        setTimeout(async () => {
            // почему-то load() не работает в onload при отладке с отключенным
            // кешированием, а через setTimeout(..., 0) работает.
            // document.fonts.ready работают всегда, но они необязательно
            // загружают все нужные для расчета шрифты
            await f.load(`${classFontSize(rfsClass)}px ${hff}`, 'H');
//             uw_debug('loaded normal font');
//             console.timeEnd('document.fonts.load');
            const c = updateBaselineCompensation_(rfsClass, hff);
            await f.load(`bold ${classFontSize(rfsClass)}px ${hff}`);
            await f.load(`bold ${classFontSize(afsClass)}px ${hff}`);
//             uw_debug('loaded bold font');
            await f.load(`${classFontSize(afsClass)}px ${pff}`);
//             uw_debug('loaded paragraph font');
            updateBaselineCompensation_2(rfsClass, afsClass, c, hff);
            _fontsLoading = false;
            if (_onFontsLoaded)
                execF(_onFontsLoaded);
        }, 0);
    }
    else {
        const c = updateBaselineCompensation_(rfsClass, hff);
        updateBaselineCompensation_2(rfsClass, afsClass, c, hff);
    }

    if (pff.indexOf("ITC Charter") == 0)
        trackFontUsage("5327262,5179514,5188095,5345314");
        // ITC Charter® W05 Regular, Italic, Bold, Bold Italic
}

const usedFonts = {}
function trackFontUsage(id) {
    if (usedFonts[id])
        return;
//     uw_debug('used ' + id + ' ' + name);
    usedFonts[id] = true;
    var path = "https://fast.fonts.net/lt/1.css?apiType=css&c=a7612f1f-cee0-4445-94d5-f45461417258&fontids=" + id;
    const s = document.createElement('link');
    s.type = 'text/css';
    s.rel = 'stylesheet';
    s.href = path;
    document.head.appendChild(s);
}

// export function setupFontUsageTracking() {
//     // loaded.then() работают только в Safari и то, похоже, только при начальной
//     // загрузке. В Chrome/FF они всегда pending.
//     let i = document.fonts.values();
//     // for (const f of document.fonts.values())
//     // ^ не работает в FF
//     for (let x = i.next(); !x.done; x = i.next()) {
//         const f = x.value;
//         if (f.family.replace(/["']/g, "") == 'ITC Charter'
//             && f.unicodeRange.toLowerCase() != "u+400-4ff") {
//             if (f.weight == "normal" && f.style == "normal")
//                 f.loaded.then(() => trackFontUsage("5327262", "ITC Charter® W05 Regular"));
//             if (f.weight == "normal" && f.style == "italic")
//                 f.loaded.then(() => trackFontUsage("5179514", "ITC Charter® W05 Regular Italic"))
//             if (f.weight == "bold" && f.style == "normal")
//                 f.loaded.then(() => trackFontUsage("5188095", "ITC Charter® W05 Bold"), () => uw_debug("bold normal"));
//             if (f.weight == "bold" && f.style == "italic")
//                 f.loaded.then(() => trackFontUsage("5345314", "ITC Charter® W05 Bold Italic"), () => uw_debug("bold italic"));
//         }
//     }
// }

function baseline(e, fs) {
    return Math.floor(e.getBoundingClientRect().height) - 3*fs;
}
function baselineCompensation(bl, fs, ff) {
    const capHeight = fontMetrics(ff, fs, "H").height;
    const compensation = Math.max(0, Math.floor((capHeight+ (capHeight < 15 ? 1 : 0))/2) + bl);
//    uw_debug(`${fs}px ${ff}, capHeight = ${capHeight}, baseline = ${bl}, compensation = ${compensation}`)
    return compensation;
}
function classFontSize(cls) {
    return parseInt(cssParser.getRuleValue('.'+cls, 'font-size'));
}
function updateBaselineCompensation_(fsClass, hff) {
    const d = baselineTestDivs();
    const fs = classFontSize(fsClass);
    const hBaseline = baseline(d.subscription, fs);
    const compensation = baselineCompensation(hBaseline, fs, hff);
    cssParser.overrideRule(`.${fsClass}`, '--baseline-compensation', compensation + 'px');
    return compensation;
}
function updateBaselineCompensation_2(rfsClass, afsClass, c0, hff) {
    const d = baselineTestDivs();

    const rfs = classFontSize(rfsClass);
    const sBl = baseline(d.subscription, rfs);
    const sbBl = baseline(d.subscriptionBold, rfs);
    const sbComp = sbBl - sBl;
    // если целиком bold, то компенсируем в обе стороны
//    uw_debug(`rfs = ${rfs}, sBl = ${sBl}, sbBl = ${sbBl}, bold subscription compensaion = ${sbComp}`);

    const afs = classFontSize(afsClass);
    const hBl = baseline(d.articleHeader, afs);
    const hbBl = baseline(d.articleHeaderBold, afs);
    const hBlMinus1px = baseline(d.articleHeaderMinus1px, afs-1);
    const hbBlMinus1px = baseline(d.articleHeaderBoldMinus1px, afs-1);
    const hbComp = hbBl - hBl;

    const pBl = baseline(d.paragraph, afs);
    const phtsComp = Math.min(0, Math.min(hbBl, pBl) - hBl);
    const phtComp = Math.min(0, pBl - hBl);
    const soComp = Math.min(0, hbBl - hBl);
    // по-умолчанию у postHeaderText --header-font-family,
    // если baseline используемого шрифта выше, то общая baseline не меняется
    // (не раздвигается и не сползает вниз), по-этому компенсируем только
    // отрицательные величины (сдвигаем вверх)

//     uw_debug(`hBl = ${hBl}, hbBl = ${hbBl}, hBlMinus1px = ${hBlMinus1px}, hbBlMinus1px = ${hbBlMinus1px}, pBl = ${pBl}, postHeaderText compensaion = ${phtComp}/${phtsComp}, subject only compensaion = ${soComp}`);

    const ml = '(max-width: ' + getVariable('--multiline-list-view-breakpoint-px') + ')';

    let c = c0;

    if (afs != rfs) {
        c = baselineCompensation(hBl, afs, hff);
        cssParser.overrideRule(`.${afsClass}`, '--baseline-compensation', c + 'px');
    }

    cssParser.overrideRule('.hasUnreadPosts:not(.tagLI) .buttonText', '--baseline-compensation', `${c0 + sbComp}px`);
    cssParser.overrideRule('.filterQuery, .smartStreamName', '--baseline-compensation', `${sbComp}px`);
    cssParser.overrideRule('.feedLabelFeed, .subscriptionTitle', '--baseline-compensation', `${c + hbComp}px`);

//     cssParser.overrideRule('.postHeaderTag', '--baseline-compensation',
//                            `${c}px`)
    cssParser.overrideRule('.postHeaderText.hasText', '--baseline-compensation',
                           `${c + phtComp}px`)
    cssParser.overrideRule('.postHeaderText.hasSubject', '--baseline-compensation',
                           `${c + soComp}px`)
    cssParser.overrideRule('.postHeaderText.hasSubject.hasText', '--baseline-compensation',
                           `${c + phtsComp}px`)
    cssParser.overrideRule('.mosaic .msubject', 'margin-top', `${hbBl - hBl}px`)


    cssParser.overrideMediaRule(ml, '.postHeaderText.hasText, .postHeaderTag, .postHeaderShortText', '--baseline-compensation',
                                `${c + pBl - hBl}px`)
    cssParser.overrideMediaRule(ml, '.postHeaderText.hasSubject, .postHeaderText.hasSubject.hasText', '--baseline-compensation',
                                `${c + hbComp}px`)
    cssParser.overrideMediaRule(ml, '.postHeaderText.hasSubject, .postHeaderText.hasSubject.hasText', '--baseline-compensation',
                                `${c + hbComp}px`)
    cssParser.overrideMediaRule(ml, '.mosaic .msubject', 'margin-top', `${hbBlMinus1px - hBlMinus1px}px`)
}

function fontMetrics(fontFamily, fontSize, text) {
    const canvas = document.createElement("canvas");
    canvas.width  = 3*fontSize;
    canvas.height = 3*fontSize;
    canvas.style.opacity = 1;
    canvas.style.fontFamily = fontFamily;
    canvas.style.fontSize = fontSize;
    const ctx = canvas.getContext("2d");
    ctx.font = fontSize + "px " + fontFamily;
//     ctx.textBaseline = "top";

    const w = canvas.width;
    const h = canvas.height;
    const baseline = 2*fontSize;

    // Set all canvas pixeldata values to 255, with all the content
    // data being 0. This lets us scan for data[i] != 255.
    ctx.fillStyle = "white";
    ctx.fillRect(0, 0, w, h);
    ctx.fillStyle = "black";
    ctx.fillText(text, fontSize, baseline);

    // canvas pixel data is w*4 by h*4, because R, G, B and A are separate,
    // consecutive values in the array, rather than stored as 32 bit ints.
    const p = ctx.getImageData(0, 0, w, h).data;
    const len = p.length;
    const w4 = w*4;

    // Finding the ascent uses a normal, forward scanline
    var i = 0;
    while (++i < len && p[i] > 192);
    const ascent = (i/w4)|0;

    // Finding the descent uses a reverse scanline
    i = len - 1;
    while (--i > 0 && p[i] > 192);
    const descent = (i/w4)|0;

//     canvas.style.cssText = 'position:absolute; top:0; left:0; z-index: 100';
//     document.body.appendChild(canvas);

    if ((ascent == 0 && descent == h - 1) // случайные данные в canvas
        || ascent >= descent // пустая canvas
       ) {
        blockedCanvasWarning();
//        uw_debug(`ascent = ${ascent}, descent = ${descent}, p.length = ${p.length}`);
    }

    return { ascent : baseline - ascent,
             descent : descent - baseline,
             height : 1 + descent - ascent
           }
}

let blockedCanvasWarning = () => {
    alert('Your browser seems to block access to HTML Canvas.\nCanvas is used for coloring button icons in different themes and for calculating font metrics to properly align text.\nPlease, whitelist bazqux.com to access canvas if you see issues with icons and text alignment.');
    blockedCanvasWarning = () => {}
}

function fixAudioPlaceholderHeight() {
    cssParser.bulkOverride(() => {
    const dummyAudio = $('<audio controls class=dummyAudio preload=none />').appendTo('body')[0];
        // почему-то, если устанавливать class через параметры:
        // { class='dummyButton' },
        // то он не добавляется, если установлен мобильный User-Agent
    let ah = dummyAudio.clientHeight;
    if (ah < 30)
        ah = 30;
    const o = cssParser.overrideRule;
    o('.bqrMsgAudio', 'min-height', ah+'px');
    o('.bqrMsgAudioInner', 'height', ah+'px');
    o('.iconPlayAudio', 'width', (ah*1.25)+'px');
    o('.iconPlayAudio', 'background-size', (ah/2)+'px');
    dummyAudio.parentNode.removeChild(dummyAudio);
    });
}
function checkOpacityCalc() {
    const checkDiv = $('<div class=opacityCalcCheck />').appendTo('body')[0];
    if (pf(getComputedStyle(checkDiv).getPropertyValue('opacity')) != 0.5) {
        $("html").addClass('opacityCalcNotSupported')
    }
    checkDiv.parentNode.removeChild(checkDiv);
}
function parseCssColor(media, c) {
    c = c.replace(/\s+/g, '');
    let m = c.match(/var\((--[^,)]+)/);
    if (m)
        return parseCssColor(media, getMediaVariable(media, m[1]));

    m = c.match(/rgb\((\d+),*(\d+),*(\d+)\)/);
    if (m)
        return { r : parseInt(m[1]), g : parseInt(m[2]), b : parseInt(m[3]) };

    m = c.match(/^#([0-9A-Fa-f])([0-9A-Fa-f])([0-9A-Fa-f])$/);
    if (m)
        return { r : parseInt(m[1]+m[1], 16), g : parseInt(m[2]+m[2], 16), b : parseInt(m[3]+m[3], 16) };

    m = c.match(/^#([0-9A-Fa-f][0-9A-Fa-f])([0-9A-Fa-f][0-9A-Fa-f])([0-9A-Fa-f][0-9A-Fa-f])$/);
    if (m)
        return { r : parseInt(m[1], 16), g : parseInt(m[2], 16), b : parseInt(m[3], 16) }

    throw ("parseCssColor: bad color " + c)
}
function getCssColor(media, sel, prop) {
    return parseCssColor(media, cssParser.getRuleValue(sel, prop));
}
let _iconsCache = {};
export function updateButtonsColor(media, color) {
    const bg = cssParser.getPreprocessedCss().properties['background-image'];
    $.each(bg, function (sel, s) {
        const favicon = sel.indexOf('.favicon') != -1;
        if (!sel.match(/icon/i) && !favicon || sel.match(/\.appearanceBox/))
            return;
        const m = s.match(/^url\("?(data:image\/(png|gif)[^")]*)"?\)/);
        if (!m)
            return;
        let newColor =
            parseCssColor(media, color);
            //getComputedStyle(document.documentElement).getPropertyValue('--text-color')
            //getCssColor('.buttonIcon', 'color');
        if (sel == '.bqrRetweetIcon') {
            newColor = getCssColor(media, '.mtext .bqrRetweetIcon', 'color');
        }
        if (sel.indexOf('.selected') != -1) {
            newColor = getCssColor(media, '.subscriptions li.selected', 'color');
        }
        if (sel.indexOf('.iconClose') > 0) {
            newColor = getCssColor(media, sel, 'color');
        }

        const repaint = (img) => {
            try {
                const canvas = document.createElement("canvas");
                const ctx = canvas.getContext("2d");

                canvas.width = img.width;
                canvas.height = img.height;
                ctx.drawImage(img, 0, 0);

                const imgData = ctx.getImageData(0, 0, canvas.width, canvas.height);
                const d = imgData.data;

                if (sel == ".iconStar" || sel == ".noTouch .iconStar:hover" || sel == ".menu .iconStar") {
                    const a = parseCssColor(media, "var(--text-color)");
                    const b = parseCssColor(media, "var(--interpolate-color)");
                    const minOp = pf(getMediaVariable(media, "--min-opacity"));
                    const alpha0 = sel == ".iconStar" ? 0.24 : 0.4;
                    let mul = ((1-alpha0)*minOp + alpha0) / alpha0;
                    if (sel == ".iconStar")
                        mul *= 0.24 / 0.2;

                    // интерполируем цвета
                    for (let i = 0; i < d.length; i += 4) {
                        if (d[i+3] != 0) {
                            const c = d[i] / 255;
                            d[i]   = Math.round((1-c) * a.r + c * b.r);
                            d[i+1] = Math.round((1-c) * a.g + c * b.g);
                            d[i+2] = Math.round((1-c) * a.b + c * b.b);
                            d[i+3] = Math.round(mul * d[i+3]);
                        }
                    }
                }
                else {
                    // определяем, черно-белая картинка или нет
                    for (let i = 0; i < d.length; i += 4) {
                        if (d[i+3] != 0
                            && (d[i] > 16 || d[i+1] > 16 || d[i+2] > 16)) {
                            // Brave для борьбы с fingerprinting случайным образом
                            // меняет некоторые младшие биты, превращая
                            // (0,0,0,0) в (0,0,1,3)
                            // по-этому, вместо !=0 используем >16.
                            // uw_debug(`${sel} is not a monochrome image: d[${i}]=${d[i]},  d[${i}+1]=${d[i+1]},  d[${i}+2]=${d[i+2]},  d[${i}+3]=${d[i]+3}`);
                            return; // неодноцветная картинка
                        }
                    }

                    // перекрашиваем
                    for (let i = 0; i < d.length; i += 4) {
                        if (d[i+3] != 0) {
                            d[i] = newColor.r;
                            d[i+1] = newColor.g;
                            d[i+2] = newColor.b;
                        }
                    }
                }
                ctx.putImageData(imgData, 0, 0);

                // uw_debug(`repaint ${media} ${sel}`);
                cssParser.overrideMediaRule(
                    media, sel, 'background-image',
                    'url("' + canvas.toDataURL() + '")',
                    favicon // important, чтобы заменить style
                );
                (new Image).src = canvas.toDataURL();
                // ^ чтобы Firefox закешировал, а то в режиме разработчика
                // мерцают иконки при первом выделении
            } catch (e) {
                uw_debug(`Can’t repaint ${media} ${sel}: ${e.message}`);
            }
        }

        let i = _iconsCache[m[1]];
        if (i) {
            if (i.repaints)
                i.repaints.push(repaint);
            else
                repaint(i.img);
        } else {
            i = { img : new Image(),
                  repaints : [repaint] };
            _iconsCache[m[1]] = i;
            if (safariVersion() >= 603) {
                i.img.crossOrigin = "Anonymous";
                // ^ Safari 10.1 требует этого для ctx.getImageData некоторых
                // (почему не всех?) data-url
                // Видимо, какие-то больше похожи на данные о банковской карточке?)
                // а Safari 10.0.3 наоборот, не позволяет загружать картинки
                // с установленным crossOrigin (img.src = m[1] не работает)
            }
            i.img.onload = () => {
                i.repaints.forEach((rp) => rp(i.img));
                i.repaints = null;
            };
            i.img.src = m[1];
        }
    })
}

export function scannedPercentGradientStyle(percent)
{
    return percent == 100 ? "" : "background: linear-gradient(to right, #f5f5f5 0%,#f5f5f5 50%,#fff 50%);".replace(/50/g, percent);
    //for(const i=0;i<=100;i+=5) uw_debug(".sp"+i+" { " + scannedPercentGradientStyle(i) + "}")
}

function subItemCls(si, s, cs, cnt, vm)
{
    const p = cnt._TotalPosts - cnt._ReadPosts;
    const cc = cnt._TotalComments - cnt._ReadComments;
    const c = isCommentCountsVisible(si) ? cc : 0;
    const r = [];
//     if (isFolder(si) && !vm._ExpandedComments && p==0 && cc != 0)
//         r.push("unreadCommentsOnlyInChildren")
    if (s) r.push("selected");
    if (cs) r.push("childSelected");
    if (p==0 && c==0 && (cnt._Feed != 0 || isTag(si))) r.push("allItemsRead");
    if (p==0 && c==0 && !(isFolder(si) && cc != 0) && cnt._Scanning == 0)
        r.push("noNewItems");
//(*                                 (\* && cnt.Error = 0 *\))
    if (p>0) r.push("hasUnreadPosts");
    if (c>0) r.push("hasUnreadComments");
    if (cnt._Scanning != 0) r.push("subItemScanning");
    if (cnt._Error != 0) r.push("subItemError");
    if (isFolder(si) || isTag(si))
    {
        r.push(vmFolderExpanded(vm) ? "folderExpanded" : "folderCollapsed");
    }
    if (isTag(si)) r.push("tagLI");
    return r.join(" ");
}

let mtvmFolderExpanded = -1;
export function registerMTVMFolderExpanded(x) { mtvmFolderExpanded = x }

function vmFolderExpanded(vm)
{
    return typeof(vm._Ex) == "object"
        ? vm._Ex.v._FolderExpanded : vm._Ex == mtvmFolderExpanded;
}

let exactUnreadCounts = false;
export function showUnread(cnt, expandedComments)
{
    const p = cnt._TotalPosts - cnt._ReadPosts;
    const c = expandedComments ? cnt._TotalComments - cnt._ReadComments : 0;
    if (p == 0 && c == 0) return "0"; else
        return ((!exactUnreadCounts && p > 500) ? "500+" : (p != 0 ? p.toString() : "")) +
        (c != 0 ? ("/" + ((!exactUnreadCounts && c > 500) ? "500+" : c.toString())) : "");
// (* 999+ смотрятся короче, но уж слишком сложно *)
}
export function setExactUnreadCounts(e)
{
    exactUnreadCounts = e;
    subItems.forEach(updateSubItem);
}

let onSetFeed = null;
export function setOnSetFeed(osf)
{
    onSetFeed = osf;
}
let onToggleFolder = null;
export function setOnToggleFolder(otf)
{
    onToggleFolder = otf;
}

let subItems = new Map();
let urlToSubItem = new Map();
let grIdToSubItem = new Map();
let pathToSubItem = new Map();
let domIdToSubItemIndex = new Map();
let sortedDomIds = [];
let domIdParentSubItem = new Map();
let toplevelFeedsAndFolders = [];

export function selectSubItem(idx)
{
//    uw_debug("selectSubItem " + idx);
    const upd = [];
    subItems.forEach(si => {
        if (si._Selected || si._ChildSelected) {
            si._Selected = false;
            si._ChildSelected = false;
            upd.push(si);
        }
    });
    if (subItems.get(idx) != undefined)
        setSelectedFlags(upd, subItems.get(idx));
    upd.forEach(updateSubItem);
}
export function toggleFolder(domIdx)
{
    uw_stopPropagation();
    execF(execF(onToggleFolder, domIdToSubItemIndex.get(domIdx)));
}
function setFeedI(idx)
{
    execF(execF(onSetFeed, idx));
}
let selectedFeedDomIdx = -1;
export function setFeed(domIdx)
{
    selectedFeedDomIdx = domIdx;
    setFeedI(domIdToSubItemIndex.get(domIdx));
}
export function setFeedL(idx)
{
    setFeedI(idx);
}
export function setFeedLink(cls, idx, inner)
{
    return `<a class="${cls}" dir=auto onclick="bq.setFeedL(${idx})">${inner}</a>`;
}

export function feedKeyboardAction(idx, action)
{
    if (idx < 0) return;
    function last(n){
        if (n == null) return -1;
        let r = n._1;
        while ((n = n._2)) r = n._1;
        return r
    }
    const domIdx = domIdToSubItemIndex.get(selectedFeedDomIdx) == idx ? selectedFeedDomIdx : last(subItems.get(idx)._DomIds);
    const si = subItems.get(idx);
    const node = elem("cls"+domIdx);
    if (!node) return;
    if (action == "toggleFolder") {
        const p =
            (si._Path == "tags" || si._Path.indexOf("folder/") == 0)
            ? si : siNodeParentSubItem(node);
        if (p)
            execF(execF(onToggleFolder, p._Index));
    } else if (action == "parentFolder") {
        const p = siNodeParentSubItem(node);
        setFeedI(p ? p._Index : 0);
    } else if (action == "nextFeed") {
        selectNextFeed(domIdx, function (i) { return i+1; }, false);
    } else if (action == "prevFeed") {
        selectNextFeed(domIdx, function (i) { return i-1; }, false);
    } else if (action == "nextUnreadFeed") {
        selectNextFeed(domIdx, function (i) { return i+1; }, true);
    } else if (action == "prevUnreadFeed") {
        selectNextFeed(domIdx, function (i) { return i-1; }, true);
    }
}

export function selectNextFeed(domIdx, f, unreadOnly)
{
    const maxIdx = domIdToSubItemIndex.size - 1;
    let   di = domIdx;
    const hasParent = domIdParentSubItem.has(domIdx);
    while ((di = f(di)) != domIdx) // на всякий случай проверяем на циклы
    {
        if (di > maxIdx) break;//di = 0;
        if (di < 0) break; //di = maxIdx;
        const node = elem('cls'+di);
        if (node && (node.offsetTop > 0 || di == 0)) { // видимая нода
            if (unreadOnly) {
                const si = nodeToSubItem(node);
                if (si._Path == "starred" || si._Path.indexOf("tag") == 0
                    || node.className.indexOf("hasUnread") == -1
                    || (!hasParent && siNodeParentSubItem(node) != null))
                    continue;
            }
            const left = $(".left .flexFullHeight")[0];
            const ot = node.offsetTop - left.offsetTop;
            if (ot < left.scrollTop)
                left.scrollTop = ot;
            if (ot + node.clientHeight > left.scrollTop + left.clientHeight)
                left.scrollTop = ot + node.clientHeight - left.clientHeight;
            setFeed(di);
            break;
        }
    }
}

let prevSubItemsCounters = new Map();

export function setSubItems(sameHash, ls)
{
    const h2si = new Map(pathToSubItem);
    let toUpdate = new Set();

    if (!sameHash) // изменился порядок subItem-ов
    {
        subItems = new Map();
        urlToSubItem = new Map();
        grIdToSubItem = new Map();
        pathToSubItem = new Map();
        domIdToSubItemIndex = new Map();
        sortedDomIds = [];
        domIdParentSubItem = new Map();
        toplevelFeedsAndFolders = [];
    } else {
        // восстанавливаем все счетчики, т.к. к нам придут только изменения
        for (const [i,c] of prevSubItemsCounters) {
            const d = subItems.get(i)._Counters.data;
            if (d._ReadPosts != c._ReadPosts
                || d._ReadComments != c._ReadComments
                || d._TotalPosts != c._TotalPosts
                || d._TotalComments != c._TotalComments
               ) {
                d._ReadPosts = c._ReadPosts;
                d._ReadComments = c._ReadComments;
                d._TotalPosts = c._TotalPosts;
                d._TotalComments = c._TotalComments;
                toUpdate.add(i);
            }
        }
    }

    for (; ls; ls = ls._2) {
        const si = ls._1;
        const url = siUrl(si);
        if (url) urlToSubItem.set(url, si);
        grIdToSubItem.set(si._GRId, si);
        pathToSubItem.set(si._Path, si);

        subItems.set(si._Index, si);
        toUpdate.add(si._Index);
        si._Selected = false;
        si._ChildSelected = false;

        for (let di = si._DomIds; di; di=di._2)
            domIdToSubItemIndex.set(di._1, si._Index);

        const si0 = h2si.get(si._Path);
        if (si0 != undefined)
        {
            const vm0 = si0._ViewMode.data;
            const c0 = si0._Counters.data;
            const c = si._Counters;
            const vm = si._ViewMode;
            // перемещаем старые source, чтобы обновлялись счетчики,
            // если этот фид используется в ur/web (currentFeed).
            si._Counters = si0._Counters;
            si0._Counters = sc(c0);
            si._ViewMode = si0._ViewMode;
            si0._ViewMode = sc(vm0);
            si._ViewMode.data = vm;
            si._Counters.data = c;
        }
        else
        {
            // для новой подписки создаем новые source
            si._Counters = sc(si._Counters);
            si._ViewMode = sc(si._ViewMode);
        }
    }

    const sel = pathToSubItem.get(getInterfacePath().replace(/^search\/[^/]+\//,''));
    const upd = []
    if (sel != undefined) {
        const c = sel._Counters.data;
        if (c._Feed + c._Error + c._Scanning > 0 || isTag(sel))
            setSelectedFlags(upd, sel);
    }

    for (const i of toUpdate) {
        updateSubItem(subItems.get(i));
    }

    prevSubItemsCounters = new Map();
    for (const [i,s] of subItems) {
        const d = s._Counters.data;
        prevSubItemsCounters.set(i,
            { _ReadPosts : d._ReadPosts, _ReadComments : d._ReadComments,
              _TotalPosts : d._TotalPosts, _TotalComments : d._TotalComments
            });
    }

    if (!sameHash) {
        // подготавливаем данные для фильтров, чтобы каждый раз не собирать
        // список фидов и папок
        sortedDomIds = Array.from(domIdToSubItemIndex.keys()).sort(function(a,b){return a - b});
        let lastFolder = null;
        for (const domId of sortedDomIds)
        {
            const si = subItems.get(domIdToSubItemIndex.get(domId));
            const f = siFolderName(si);
            const root = !(si._ParentFolders && si._ParentFolders._2);
            if (f) {
                lastFolder = { si: si, folderFeeds: [] };
                toplevelFeedsAndFolders.push(lastFolder);
            } else if (root) {
                lastFolder = null;
                if (siFeedUrl(si))
                    toplevelFeedsAndFolders.push({ si: si });
            } else {
                domIdParentSubItem.set(domId, lastFolder.si);
                if (siFeedUrl(si)) // только живые фиды
                    lastFolder.folderFeeds.push(si);
            }
        }
        // убираем папки без живых фидов
        toplevelFeedsAndFolders = toplevelFeedsAndFolders
            .filter(t => !t.folderFeeds || t.folderFeeds.length > 0)
    }
}

function setSelectedFlags(upd, si)
{
    upd.push(si);
    si._Selected = true;
    for (let p=si._ParentFolders; p; p=p._2) {
        subItems.get(p._1)._ChildSelected = true;
        upd.push(subItems.get(p._1));
    }
}

export function getSubItem(idx)
{
    return subItems.get(idx);
}
export function getSubItemByUrl(url)
{
    return urlToSubItem.get(url);
}
export function getSubItemByPath(hash)
{
//        uw_debug("getSubItemByPath " + hash);
    return pathToSubItem.get(hash);
}
export function getSubItemByGRId(i)
{
    return grIdToSubItem.get(i);
}
export function getSubItemByTag(t)
{
    for (const si of subItems.values())
    {
        if (siTagName(si) == t) {
            return si;
        }
    }
    return null;
}
export function getSubItems(idx)
{
    const si = subItems.get(idx);
    let r = null;
    if (si && si._Path && si._Path.indexOf("smartstream/") == 0) {
        let s = si._SIType.v._StreamFeedSirs;
        while (s) {
            const si = subItems.get(s._1);
            if (si)
                r = {_1:si, _2:r};
            s = s._2;
        }
    } else {
        subItems.forEach(si => {
            if (si._Index == idx && !isFolder(si)) r = {_1:si, _2:r};
            for (let p = si._ParentFolders; p; p=p._2)
                if (p._1 == idx) r = {_1:si, _2:r};
        });
    }
    return r;
}
export function getUrls(idx) {
    let s = getSubItems(idx);
    let r = null;
    while (s) {
        const u = siFeedUrl(s._1);
        if (u) {
            const c = s._1._Counters.data;
            r = { _1:{_1:s._1._GRId, _2:c._ReadPosts, _3:c._ReadComments, _4:c._TotalPosts, _5:c._TotalComments}, _2:r};
        }
        s = s._2;
    }
    return r;
}
export function getUrlsOnly(idx) {
    let s = getSubItems(idx);
    let r = null;
    while (s) {
        const u = siFeedUrl(s._1);
        if (u)
            r = { _1:u, _2:r};
        s = s._2;
    }
    return r;
}
export function getSubItemGRIds(idx) {
    let s = getSubItems(idx);
    let r = null;
    while (s) {
        const u = siFeedUrl(s._1);
        if (u)
            r = { _1:s._1._GRId, _2:r};
        s = s._2;
    }
    return r;
}
export function getFeedsCount() {
    let s = getSubItems(0);
    let n = 0;
    while (s) {
        if (siUrl(s._1))
            n++;
        s = s._2;
    }
    return n;
}
export function subItemHasQueryFeeds(idx, feeds, feeds2)
{
    const si = getSubItem(idx);
    if (!si || isTag(si)) {
        return true;
        // Теги всегда обновляем, т.к. тег может содержать статью,
        // которая при редактировании фильтра может стать readLocked
        // (невидима в фиде, но есть в теге) и наоборот.
        // Также тег может содержать статью из smart stream, которой надо
        // обновить smart stream unread counters
    }

    let l = getSubItemGRIds(idx);
    let f = {};
    while (l) { f[l._1] = true; l = l._2; }
    while (feeds) {
        if (f[feeds._1])
            return true;
        feeds = feeds._2;
    }
    while (feeds2) {
        if (f[feeds2._1])
            return true;
        feeds2 = feeds2._2;
    }
    return false;
}
let eqf = {};
function eqfFolderCheckMark(domId)
{
    const urls = eqf.folderFeeds[eqf.domIdToFolderIndex[domId]];
    let checked = 0;
    let unchecked = 0;
    for (const i in urls) {
        if (eqf.checkedFeeds[urls[i]]) checked++; else unchecked++;
    }
//    elem('eqfCheck'+domId).className =
    return (checked == 0 ? "iconCheckBox0" :
            unchecked == 0 ? "iconCheckBox1" : "iconCheckBoxU");
}
export function editQueryFeeds(feeds)
{
    // TODO: хорошо бы эту логику тоже перевести на toplevelFeedsAndFolders,
    // но много всего надо переделывать
    eqf = { checkedFeeds : {},
            domIdToUrl : {},
            domIdToFolderIndex : {},
            feedDomIds : {},
            folderIndexToDomId : {},
            folderFeeds : {}, // [folderIndex] -> [url1,…]
            feedFolders : {}, // [url] -> [folderIndex1,…]
            displayQueryFolders : displayQueryFeeds_(feeds, true).Folders
          };

    while (feeds) {
        const si = grIdToSubItem.get(feeds._1);
        if (!si) {
            uw_debug("Can’t find feed #" + feeds._1);
        }
        else
            eqf.checkedFeeds[siFeedUrl(si)] = true;
        feeds = feeds._2;
    }
    const r = [];
    r.push("<span class=subscriptions>");

    let lastFolder = null;
//     function folderClass(domId) { return eqfFolderCheckMark(domId) == "iconCheckBoxU" ? "folderExpanded" : "folderCollapsed"; }

    let prevDomId = null;
    for (const domId of sortedDomIds)
    {
        const si = subItems.get(domIdToSubItemIndex.get(domId));
        const u = siFeedUrl(si);
        const f = siFolderName(si);
        if (!u && !f) continue;
        const folder = domIdParentSubItem.get(domId);
        if (folder) {
            if (!lastFolder || lastFolder != folder._Index) {
                lastFolder = folder._Index;
                r.push('<div class="folder folderCollapsed" id="eqfFolder'+lastFolder+'">');
                eqf.domIdToFolderIndex[prevDomId] = folder._Index;
                eqf.folderIndexToDomId[folder._Index] = prevDomId;
                eqf.folderFeeds[folder._Index] = [];
            }
            eqf.folderFeeds[lastFolder].push(u);
        } else if (lastFolder) {
            r.push("</div>");
            lastFolder = null;
        }
        if (u) {
            eqf.domIdToUrl[domId] = u;
            const di = eqf.feedDomIds[u];
            if (di) di.push(domId); else eqf.feedDomIds[u] = [domId];
            if (!eqf.checkedFeeds[u])
                eqf.checkedFeeds[u] = false;
        }
        const l =
            `<li title="${atr(si._Title)}" id="eqfCls${domId}"
               onclick="bq.eqfToggleFeed(${domId})"
               class="${eqf.checkedFeeds[u] ? "eqfChecked" : ""}">
             <span id="eqfCheck${domId}"
               class="${
                 eqf.checkedFeeds[u] ? "iconCheckBox1" : "iconCheckBox0"
               }"></span><span class="${
                 si._FaviconStyle
                 ? `favicon" style='${si._FaviconStyle}'`
                 : `iconFolder" onclick="uw_event=event;bq.eqfToggleFolder(${domId});return false;"`
               }"></span><span class=buttonText dir=auto>${
                 eh(si._Title)
               }</span></li>`;
        r.push(l);
        prevDomId = domId;
    }
    if (lastFolder) r.push("</div>");
    r.push("</span>")
    return { _Xml : r.join(""),
             _SelectAll : function () { eqfSelect(true); },
             _SelectNone : function () { eqfSelect(false); },
             _GetFeedGRIds : function () {
                 let r = null;
                 for (const u in eqf.checkedFeeds) {
                     if (eqf.checkedFeeds[u])
                         r = { _1 : getSubItemByUrl(u)._GRId, _2 : r };
                 }
                 return r;
             },
             _UpdateFolders : function () {
                 eqfUpdateFolders();
                 for (const f of eqf.displayQueryFolders) {
                     if (f.usedFeeds.length > 0 && f.excludedFeeds.length > 0)
                         eqfToggleFolder(eqf.folderIndexToDomId[f.si._Index], true);
                 }
             }

           }
}
function eqfUpdate(domId, cls) {
    let e = elem('eqfCheck'+domId);
    if (e.className != cls) e.className = cls;
    e = elem('eqfCls'+domId);
    if (cls == "iconCheckBox1")
        $(e).addClass("eqfChecked");
    else
        $(e).removeClass("eqfChecked");
}
function eqfUpdateFolders() {
    for (const domId in eqf.domIdToFolderIndex) {
        const cls = eqfFolderCheckMark(domId);
        eqfUpdate(domId, cls);
    }
}
export function eqfToggleFolder(domId, noStop)
{
    $(elem('eqfCls'+domId)).toggleClass('folderExpanded');
    $(elem('eqfFolder'+eqf.domIdToFolderIndex[domId])).toggleClass('folderExpanded');
    $(elem('eqfCls'+domId)).toggleClass('folderCollapsed');
    $(elem('eqfFolder'+eqf.domIdToFolderIndex[domId])).toggleClass('folderCollapsed');
    if (!noStop) uw_stopPropagation();
}
function eqfUpdateFeed(url)
{
    const di = eqf.feedDomIds[url];
    const cls = eqf.checkedFeeds[url] ? "iconCheckBox1" : "iconCheckBox0";
    for (const i in di)
        eqfUpdate(di[i], cls);
}
export function eqfToggleFeed(domId)
{
    const u = eqf.domIdToUrl[domId];
    if (u) {
        eqf.checkedFeeds[u] = !eqf.checkedFeeds[u];
        eqfUpdateFeed(u);
    } else { // folder
        const c = eqfFolderCheckMark(domId);
        const e = !(c == "iconCheckBox1");
        const ff = eqf.folderFeeds[eqf.domIdToFolderIndex[domId]];
        for (const i in ff) {
            eqf.checkedFeeds[ff[i]] = e;
            eqfUpdateFeed(ff[i]);
        }
    }
    eqfUpdateFolders();
}
function eqfSelect(sel) {
    for (const u in eqf.checkedFeeds) {
        eqf.checkedFeeds[u] = sel;
        eqfUpdateFeed(u);
    }
    eqfUpdateFolders();
}
export function displayQueryFeeds(feedsList) {
    return displayQueryFeeds_(feedsList).Xml;
}
function displayQueryFeeds_(feedsList, forceIncl = false)
{
    if (feedsList == null)
        return "no feeds selected";
    const feeds = new Set();
    while (feedsList) {
        const si = grIdToSubItem.get(feedsList._1);
        if (!si) {
            const err =
                "Can’t find feed #" + feedsList._1 + " (try refresh reader page)";
            uw_debug(err);
            return err
        }

        feeds.add(feedsList._1);
        feedsList = feedsList._2;
    }

    // собираем список папок и toplevel фидов
    const toplevel = [];
    for (const t of toplevelFeedsAndFolders)
    {
        if (t.folderFeeds) {
            let u = [];
            let e = [];
            for (const f of t.folderFeeds) {
                if (feeds.has(f._GRId)) u.push(f); else e.push(f);
            }
            toplevel.push({ si : t.si, usedFeeds : u, excludedFeeds : e });
        } else {
            toplevel.push({ si : t.si, used : feeds.has(t.si._GRId) });
        }
    }

    // Анализируем, что короче -- показать список включенных папок/фидов или
    // список исключенных
    let icnt = 0;
    let ecnt = 0;
    for (const i in toplevel) {
        const t = toplevel[i];
        if (t.usedFeeds) {
            const u = t.usedFeeds.length;
            const e = t.excludedFeeds.length;
            if (e == 0) { icnt++; } // папка полностью включена
            if (u == 0) { ecnt++; } // полностью исключена
            if (u > e) {
                icnt += 1+e; // folder (exluding e feeds)
                ecnt += e;   // Latest exluding e feeds
            } else {
                icnt += u;   // u feeds
                if (u == e)
                    ecnt += e; // u >= e
                else
                    ecnt += 1+u; // Latest exluding folder (but including u feeds)
            }
        } else {
            if (t.used) { icnt++; } else { ecnt++; }
        }
    }

    const incl = icnt < ecnt || forceIncl;
    const usedFeeds = incl ? (t) => t.usedFeeds : (t) => t.excludedFeeds;
    const excludedFeeds = incl ? (t) => t.excludedFeeds : (t) => t.usedFeeds;

    // убираем дубликаты фидов, чтобы не показывать их несколько раз
    let shown = new Set();
    const folders = toplevel.filter((t) => t.usedFeeds)
    const foldersBySize = folders
          .sort((a,b) =>
                (excludedFeeds(a).length - excludedFeeds(b).length)
                // сначала папки, с минимумом невыбранных фидов, т.к. бывают
                // большие папки, включающие в себя много других папок,
                // и просто сортировка по числу выбранных фидов может
                // отображать эти папки (с большим списком excluding),
                // вместо исходных.
                ||
                (usedFeeds(b).length - usedFeeds(a).length)
                // сортируем по числу выбранных фидов, чтобы убрать мелкие
                // папки, полностью включенные в большие.
               )
    for (const f of foldersBySize) {
        const u = usedFeeds(f);
        for (const iu in u) {
            const fi = u[iu]._GRId;
            if (shown.has(fi))
                u[iu] = null;
            else
                shown.add(fi);
        }
        const fu = u.filter(x => x != null);
        if (incl) f.usedFeeds = fu; else f.excludedFeeds = fu;
    }

    // Собираем результат
    const r = [];
    function add(si, suffix = null) {
        const l =
            "<div class=ffeed title=\"" + atr(si._Title) +
            "\"><span class=" +
            (si._FaviconStyle
             ? "favicon style='" + si._FaviconStyle + "'></span>"
             : (si._Index == 0 ? "iconLatest></span>" : "iconFolder></span>")) +
            "<div class=ffeedName dir=auto>" + eh(si._Title) + "</div>" +
            (suffix ? suffix : "") + "</div>";
        r.push(l);
    }
    function parentheses(title, feeds) {
        r.push("<span class=ffExcluding><span class='ffExcludingText ffExcludingStart'>("+title+"</span>");
        for (let i = 0; i < feeds.length; i++)
            add(feeds[i],
                i == feeds.length - 1
                ? "<span class='ffExcludingText ffExcludingEnd'>)</span>" : "");
        r.push("</span>");
    }

    if (!incl) {
        add(subItems.get(0)); // Latest
        if (ecnt > 0)
            r.push("<span class='ffExcludingText ffExcludingStart'>excluding</span>");
    }

    for (const i in toplevel) {
        const t = toplevel[i];
        const si = t.si;
        if (t.usedFeeds) {
            const u = t.usedFeeds.length;
            const e = t.excludedFeeds.length;
            if (incl) {
                if (u == 0) continue; // полностью исключена
                if (e == 0) add(si); // полностью включена
                else if (u > e) {
                    add(si);
                    parentheses("excluding", t.excludedFeeds)
                } else {
                    for (const j in t.usedFeeds)
                        add(t.usedFeeds[j]);
                }
            } else {
                if (e == 0) continue; // полностью включена
                if (u == 0) add(si); // полностью исключена
                else if (u >= e) {
                    for (const j in t.excludedFeeds)
                        add(t.excludedFeeds[j]);
                } else {
                    add(si);
                    parentheses("but including", t.usedFeeds);
                }
            }
        } else {
            if ((incl && t.used) || (!incl && !t.used))
                add(si);
        }
    }
    return { Xml : r.join(""), Folders : folders };
}

function updateSource(s)
{
    for (let ls = s.dyns; ls; ls = ls.next)
        if (!ls.dead)
            populate(ls.data);
}
function isFolder(si)
{
    return si._Index == 0 || siFolderName(si) != null;
    // starred и all tags -- не папки (all tags просто подписка, которая
    // выводит все теги
}
function isSmartStream(si)
{
    return si._Path.indexOf("smartstream/") == 0;
}
function isSearch(si)
{
    return si._Path.indexOf("search/") == 0;
}
function isTag(si)
{
    return si._Path == "starred" || si._Path == "tags" || siTagName(si) != null;
}
function siUrl(si)
{
    return (typeof(si._SIType) == "object" && si._SIType.v._Subscription)
            ? si._SIType.v._Subscription._Url : null;
}
function siFeedUrl(si)
{
    return (typeof(si._SIType) == "object" && si._SIType.v._Subscription &&
            typeof(si._SIType.v._Subscription._State) == "object")
            ? si._SIType.v._Subscription._State.v._Url : null;
}
export function isCommentCountsVisible(si)
{
    if (isFolder(si) || isTag(si) || isSearch(si))
        return true;
    const vm = si._ViewMode.data;
    return isSmartStream(si)
        ? (vm._NoOverride || vm._ExpandedComments)
        : vm._ExpandedComments;
}
function updateSubItem(si)
{
    const cnt = si._Counters.data;
    const vm = si._ViewMode.data;
    const uc = showUnread(cnt, isCommentCountsVisible(si));
    const c  = subItemCls(si, si._Selected, si._ChildSelected, cnt, vm);
    const ucC = "unreadCount sp" + Math.round(cnt._ScannedPercent/5)*5;
    const fs = si._FaviconStyle;
    for (let d = si._DomIds; d; d=d._2) {
        const i = d._1;
        const clsE = elem("cls" + i);
        const ucE = elem("uc" + i);
        if (clsE.className != c) clsE.className = c;
        if (ucE.className != ucC) ucE.className = ucC;
        if (ucE.innerHTML != uc) ucE.innerHTML = uc;
        if (fs) {
            const fav = elem("fav" + i);
            if (fav.style && fav.style.cssText != fs) fav.style.cssText = fs;
        }
//         if (cnt._Error == 1 && !isFolder(si))
//         {
//             const ch = subItemCharSpan(clsE);
//             if (ch.innerHTML != "!") ch.innerHTML = "!";
//         }
    }
    if (si._Path.indexOf("folder/") == 0 || si._Path == "tags")
    {
        const fE = elem("folder"+si._Index);
        const cls = "folder " + (vmFolderExpanded(vm) ? "folderExpanded" : "folderCollapsed");
        if (fE.className != cls) fE.className = cls;
    }

    updateSource(si._Counters);
    updateSource(si._ViewMode);
}
export function updateCounters(si,up,uc) {
//    const si = getSubItem(idx);
    si._Counters.data._ReadPosts -= up;
    si._Counters.data._ReadComments -= uc;
    updateSubItem(si);

    const ec = si._ViewMode.data._ExpandedComments;
    for (let p = si._ParentFolders; p; p = p._2) {
        const si = getSubItem(p._1);
        si._Counters.data._ReadPosts -= up;
        if (ec)
            si._Counters.data._ReadComments -= uc;
        updateSubItem(si);
    }
    return false;
}
function updateReadCounters_(c)
{
    let si = getSubItemByGRId(c._1);
    if (si == null)
        si = getSubItemByPath(c._1);
    if (si == null) return; // а надо ли???
    const d = si._Counters.data;
    if (si._Scanning)
        return;
    const up = d._ReadPosts - c._2;
    const uc = d._ReadComments - c._3;
    const utp = d._TotalPosts - c._4;
    const utc = d._TotalComments - c._5;
    if (up == 0 && uc == 0 && utp == 0 && utc == 0)
        return;
    d._ReadPosts -= up;
    d._ReadComments -= uc;
    d._TotalPosts -= utp;
    d._TotalComments -= utc;
    updateSubItem(si);

    const ec = si._ViewMode.data._ExpandedComments;
    for (let p = si._ParentFolders; p; p = p._2) {
        const si = getSubItem(p._1);
        si._Counters.data._ReadPosts -= up;
        si._Counters.data._TotalPosts -= utp;
        if (ec) {
            si._Counters.data._ReadComments -= uc;
            si._Counters.data._TotalComments -= utc;
        }
        updateSubItem(si);
    }
}
export function updateExpandedComments(idx, ec)
{
    const si = getSubItem(idx);
    if (!si) return; // discovery
    const c = si._Counters.data;
    updateSubItem(si);

    for (let p = si._ParentFolders; p; p = p._2) {
        const si = getSubItem(p._1);
        const fc = si._Counters.data;
        if (ec) {
            fc._TotalComments += c._TotalComments;
            fc._ReadComments += c._ReadComments;
        } else {
            fc._TotalComments -= c._TotalComments;
            fc._ReadComments -= c._ReadComments;
        }
        updateSubItem(si);
    }
}
export function retryScanning(idx)
{
    const si = getSubItem(idx);
    const c = si._Counters.data;
    c._Scanning = 1;
    c._Error = 0;
    updateSubItem(si);
//     for (const d=si._DomIds; d; d=d._2)
//         subItemCharSpan(elem("cls"+d._1)).innerHTML = "R";
}
// function subItemCharSpan(node)
// {
//     return node.children[1].children[0].children[0];
// }
export function hideSubItems(sis)
{
    const fixes = [];
    const fixSi = {};
    for (let s=sis; s; s=s._2)
    {
        const si = s._1;
        for (let d=si._DomIds; d; d=d._2)
            elem("cls"+d._1).style.display = "none";
        const c = si._Counters.data;
        const ec = si._ViewMode.data._ExpandedComments;

        for (let p = si._ParentFolders; p; p = p._2) {
            const fsi = getSubItem(p._1);
            const fc = fsi._Counters.data;
            fc._TotalPosts -= c._TotalPosts;
            fc._ReadPosts -= c._ReadPosts;
            if (ec) {
                fc._TotalComments -= c._TotalComments;
                fc._ReadComments -= c._ReadComments;
            }
            fc._Scanning -= c._Scanning;
            fc._ScanningComments -= c._ScanningComments;
            fc._Feed -= c._Feed;
            fc._Error -= c._Error;
            // _ScanningPercent???
            if (fixSi[fsi._Index] == undefined)
            {
                fixSi[fsi._Index] = fsi;
                fixes.push(fsi);
            }
        }
    }

    for (const i in fixes) {
        updateSubItem(fixes[i]);
    }

    return true;
}

export function fromFeedIcon(s)
{
    return "<div class='fromFeedIcon' style='" + s + "'></div>";
}

export function menuFavicon(url)
{
    return url.indexOf("mailto:") == 0
        ? "<span class='iconEMail'></span>"
        : "<span class='menuFavicon' style='" + faviconStyle(url,true) + " !important'></span>";
}

export function adjustMenuPosition()
{
    const m = $(".popup .menu")[0];
    if (!m) return;

    const top = parseInt(m.style.top);
    const height = $(m).outerHeight();
    const width = $(m).outerWidth();
    const margin = 10;
    const wh = window.innerHeight;
    const right = getComputedStyle(m).getPropertyValue("--right");
    let left = right
        ? window.innerWidth - parseInt(right) - width
        : parseInt(m.style.left);

    // max-height, max-window задаются в css,
    // так что здесь просто пододвигаем меню вверх/вправо/влево
    if (top + height > wh - margin) {
        $(m).css({ top : Math.round(wh - margin - height) + 'px' });
        if (right) {
            // tag/share меню подвигаем налево, чтобы кнопку было видно
            const buttonWidth = $('.msgButtons .buttonIcon').outerWidth();
            if (buttonWidth > 0) {
                left -= buttonWidth;
            }
        }
    }

    const minTop = $(".top").outerHeight() + margin;

    if (contextMenu() && top < minTop) {
        $(m).css({ top: minTop + 'px' });
    }

    const maxLeft = window.innerWidth - margin - width;
    if (left > maxLeft) {
        left = maxLeft;
    } else if (left < margin) {
        left = margin;
    }

    $(m).css({ left: left + 'px' });
}

function positionAutocomplete(pos, params) {
    const b = parseInt(params.target.element.parent().css("border-left-width"));
    params.element.element.css(
        { left : roundPixels(pos.left - b),
          // ^ ранее специально убирал roundPixels для мобил (что-то было криво?),
          // но без этого в desktop Chrome
          // dd меню скачет влево/вправо по мере набора текста
          top  : roundPixels(pos.top-1),
          width : roundPixels(Math.min(params.element.width,
                                       $(window).width() - pos.left + b - 10))
          // $(window).width() возвращает ширину viewport
          // (т.е., document.documentElement.clientWidth),
          // а window.innerWidth может стать больше viewport,
          // как раз когда появляется слишком широкое меню.
        });
    // чтобы иконки в подписках по прежнему позиционировались по пикселям
    // и подписи были правильно выровнены
    // также
    // бывают скачки в положении менюшки,
    // 19px и 19.875px -- меняются с вводом каждой буквы
    // непонятно, как появилось
    if ($(params.target.element).parents('.popup').length > 0) {
        params.element.element.css(
            { width: params.target.element.outerWidth()
            });
        // в диалогах ширина всегда равна ширине input-а (у списка подписок
        // иногда она получается шире)
    }
}

let searches = [];
let searchId = '';

export function setupSearchAutocomplete(id, list, search)
{
    searchId = id;
    while (list != null)
    {
        searches.push(list._1); list = list._2;
    }

    const ac = $(elem(id)).autocomplete({
        appendTo : ".bodyScaler",
        minLength: 0,
        delay: 0,
        position: { using: positionAutocomplete },
        select: (event) => {
            if (!(event.which == 13 || event.keyCode == 13))
                setTimeout(() => { elem(id).onchange(); execF(search) }, 1);
        },
        source: function( request, response ) {
            response($.ui.autocomplete.filter(searches, request.term));
        }
    }).data("ui-autocomplete");

    ac._renderItem = autocompleteRenderItemDirAuto;

    $(elem(id)).focus(function () {
        $(elem(id)).autocomplete("search");
    });
}

function autocompleteRenderItemDirAuto(ul, item) {
    const li = document.createElement('div');
    li.setAttribute("dir", "auto")
    li.innerHTML = eh(item.value);
    const menuItem = document.createElement('div');
    menuItem.appendChild(li);
    return $(menuItem).appendTo(ul);
}

export function updateSearchAutocomplete(query)
{
    const prevIdx = searches.indexOf(query);
    if (prevIdx == 0) return;
    if (prevIdx != -1)
        searches = searches.slice(0, prevIdx).concat(searches.slice(prevIdx+1,searches.length));
    searches.unshift(query);
    $(elem(searchId)).autocomplete("close");
}

function splitTagsText(s)
{
    return s.split( /,/ ).map(trimString);
}

export function isAutocompleteActive(id)
{
    const ac = $(elem(id)).data( "ui-autocomplete" );
    return !!(ac && ac.menu.active);
}

export function setupTagAutocomplete(id)
{
    const availableTags = getUsedTagsArray();
    function extractLast( term ) {
      return splitTagsText( term ).pop();
    }
    const ac = $(elem(id))
      // don't navigate away from the field on tab when selecting an item
      .bind( "keydown", function( event ) {
          if ( (event.keyCode === $.ui.keyCode.TAB || event.keyCode === $.ui.keyCode.ENTER) &&
             isAutocompleteActive( id ) ) {
          event.preventDefault();
        }
      })
      .autocomplete({
        appendTo : ".bodyScaler",
        minLength: 0,
        delay: 0,
        autoFocus: true,
        position: { using: positionAutocomplete },
        source: function( request, response ) {
            // TODO: надо учитывать, что курсор может быть не на последнем
            // теге
            const t = extractLast( request.term );
            if (t == '')
                response( null );
            else
            {
                const matcher =
                    new RegExp("^" + $.ui.autocomplete.escapeRegex(t), "i");
                const r = $.grep(availableTags, (t) => matcher.test(t));
                response (r.length > 0 ? r :
                          $.ui.autocomplete.filter( availableTags, t ) );
            }
        },
        focus: function() {
            // prevent value inserted on focus
            return false;
        },
        select: function( event, ui ) {
            const terms = splitTagsText( this.value );
            terms.pop();
            terms.push( ui.item.value );
            terms.push( "" );
            // add placeholder to get the comma-and-space at the end
            this.value = terms.join( ", " );
            this.scrollLeft = this.scrollWidth;
            this.onchange();
            event.stopPropagation(); // чтобы не закрывать окно при клике
            return false;
        }
      }).data("ui-autocomplete");
    ac._renderItem = autocompleteRenderItemDirAuto;
}

export function setupSubscriptionAutocomplete(type, id)
{
    const subs = [];
    const usedSubs = {};
    for (const domId of sortedDomIds)
    {
        const si = subItems.get(domIdToSubItemIndex.get(domId));
        if (usedSubs[si._Index]) continue;
        usedSubs[si._Index] = true;
        if (type == "tag" && si._Path.indexOf("tag/") != 0)
            continue;
        if (type == "feed" && si._Path.indexOf("subscription/") != 0)
            continue;
        if (type == "folder" && si._Path.indexOf("folder/") != 0)
            continue;
        if (type == "smartstream" && si._Path.indexOf("smartstream/") != 0)
            continue;
        subs.push({ label : si._Title, value : parseInt(domId), url : siUrl(si) });
//         const node = elem('cls'+domId);
//         if (node && node.offsetTop > 0) // видимая нода
//             lastNode = node;
    }

    const maxResults = 100;

    const ac = $(elem(id))
      .bind( "keydown", function( event ) {
          const k = event.keyCode;
          // Ctrl+J == ENTER
          if ( event.ctrlKey &&
               (k == 74 || k == 106 || k == 1054 || k == 1086)) {
              $(".subscriptions.ui-menu .ui-state-focus").click();
              event.preventDefault();
          }
      })
      .autocomplete({
        appendTo : ".bodyScaler",
        minLength: 0,
        delay: 0,
        autoFocus: true,
        position: { using: positionAutocomplete },
        select: function( event, ui )  {
            setFeed(ui.item.value);
            return false;
        },
        source: function( request, response ) {
            const ts = words(request.term.trim());
            const rg = (prefix, x) =>
                new RegExp(prefix + $.ui.autocomplete.escapeRegex(x), "i");
            let prefixes = ts.map((t) => rg("^[\"'()‘“’”«»:\\-,?!.<>\\\\/]*", t));
            let anys = uniq(ts).map((t) => rg("", t));

            // возвращает массив:
            //  - отрицательное число совпадений (-N)
            //    регулярных выражений в словах
            //  - для каждого из 1..N-1 совпадений:
            //    - расстояние до предыдущего совпадения
            //      (+1e9, если оно отрицательное)
            //  - положение первого совпадения в массиве
            //
            // Такие массивы можно сравнивать, получая наиболее близкое
            // к началу заголовка совпадение с поисковой фразой
            const matchOrder = (regexes, ws) => {
                const order = [];
                let prevIndex = -1;

                regexes.forEach((p) => {
                    let pos = -1;
                    for (let i = prevIndex + 1; i < ws.length; i++) {
                        if (p.test(ws[i])) {
                            pos = i;
                            break;
                        }
                    }
                    if (pos < 0) {
                        for (let i = prevIndex; i >= 0; i--) {
                            if (p.test(ws[i])) {
                                pos = i;
                                break;
                            }
                        }
                    }

                    if (pos >= 0) {
                        if (prevIndex >= 0) {
                            const d = pos - prevIndex;
                            order.push(d > 0 ? d : 1e9 - d);
                        } else {
                            order.push(pos);
                        }
                        prevIndex = pos;
                    }
                });
                if (order.length > 0) {
                    const first = order.shift();
                    order.unshift(-order.length-1);
                    order.push(first);
                } else {
                    order.push(0);
                }
                return order;
            }

            response(
                subs
                    .map((s) => {
                        if (!anys.every((p) => p.test(s.label) || p.test(s.url)))
                            return null;
                        const ws = words(s.label);
                        return { order : matchOrder(prefixes, ws)
                                   .concat(matchOrder(anys, ws)),
                                 value : s }
                    })
                    .filter((a) => a != null)
                    .sort((a,b) => {
                        const ao = a.order;
                        const bo = b.order;
                        for (let i = 0; i < ao.length; i++) {
                            const d = ao[i] - bo[i];
                            if (d != 0)
                                return d
                        }
                        return 0
                    })
                    .slice(0, maxResults)
                    .map((a) => a.value)
//                     .map((a) => {
//                         uw_debug(`${a.order}\t${a.value.label}`);
//                         return a.value
//                     })
            );
        },
        focus: function() {
            // prevent value inserted on focus
            return false;
        }

    }).data("ui-autocomplete");

    ac._renderItem = function( ul, item ) {
        if (item.value == "Latest") item.value = 0; // почему портится первый элемент?
        const li = elem('cls'+item.value).cloneNode(true);
        li.removeAttribute('id');
        li.className = li.className.replace(/folderExpanded/, 'folderCollapsed');
        $(li).removeClass('selected').removeClass('mouseOver');

        const menuItem = document.createElement('div');
        menuItem.appendChild(li);

        return $(menuItem).appendTo(ul);
    };
    ac._renderMenu = function( ul, items ) {
        const that = this;
        $.each( items, function( index, item ) {
            that._renderItemData( ul, item );
        });
        $( ul ).addClass( "subscriptions" );
        if (items.length >= maxResults)
            $("<li>")
            .addClass('tooManySubscriptionSearchResults')
            .addClass('ui-state-disabled')
            .append("… too many results, please narrow your search")
            .on('click', function (e) { e.stopPropagation() })
            .appendTo(ul);
    }

    $(elem(id)).focus(function () {
        $(elem(id)).autocomplete("search");
    });
}

export function setupCountryAutocomplete(id, setCountry)
{
    const countries = [];
    for (let i = 0; i < countriesList.length; i++) {
        const c = countriesList[i];
        countries.push({ value : c.substr(0,2).trim(), label : c.substr(5) });
    }
    $(elem(id))
      .bind( "keydown", function( event ) {
          const k = event.keyCode;
          // Ctrl+J == ENTER
          if ( event.ctrlKey &&
               (k == 74 || k == 106 || k == 1054 || k == 1086)) {
              $(".subscriptions.ui-menu .ui-state-focus").click();
              event.preventDefault();
          }
      })
      .autocomplete({
        appendTo : ".bodyScaler",
        minLength: 0,
        delay: 0,
        autoFocus: true,
        position: { using: positionAutocomplete },
        select: function( event, ui )  {
            execF(execF(setCountry,ui.item.value));
            uw_event = event;
            uw_stopPropagation(); // чтобы enter текущее сообщение не сворачивал
            return false;
        },
        source: function( request, response ) {
            const t = request.term;
            const matcher =
                new RegExp("^" + $.ui.autocomplete.escapeRegex( t ), "i");
            let r = $.grep(countries, function (value) {
                return matcher.test(value.label);
            });
            if (r.length == 0)
                r = $.ui.autocomplete.filter( countries, t );
            response (r);
        },
        focus: function() {
            // prevent value inserted on focus
            return false;
        }

    }).data("uiAutocomplete");

    $(elem(id)).focus(function () {
        $(elem(id)).autocomplete("search");
    });
}

export function checkLogin(login)
{
    return (login.length >= 4 && login.match(/^[a-zA-Z0-9]+$/) != null);
}

export function toLowerCase(s)
{
    return s.toLowerCase();
}

export function faviconUrl(u,fav)
{
    const uri = parseURI(u);
    return addImageHost("favicons", (fav?"/favicon?u=":"/feedicon?u=") + (uri ? uri.hostname : ""));
}
export function faviconStyle(u,fav)
{
    return backgroundImage(faviconUrl(u,fav));
}

export function trimString(str) {
    return str.replace(/^\s\s*/, '').replace(/\s\s*$/, '');
}

export function getTagsList(id)
{
    if (!elem(id)) return {_1:false, _2:null};
    const ts = splitTagsText(elem(id).value);
    const uniqueTags = [];
    $.each(ts, function(i, el){
        if(el != '' && $.inArray(el, uniqueTags) === -1)
            uniqueTags.push(el);
    });
    for( const i in uniqueTags )
        if (!checkTagName(uniqueTags[i]))
            return {_1:false, _2:null};
    return {_1:true, _2:arrayToList(uniqueTags)};
}

function checkTagName(tn)
{
    return checkName('tag', tn);
}

function capitalizeFirstLetter(string)
{
    return string.charAt(0).toUpperCase() + string.slice(1);
}

export function checkName(what, tn)
{
    tn = trimString(tn);
    if (what == '')
        return tn;
    const What = capitalizeFirstLetter(what);
    if (tn == '') {
        alert(What+' name can’t be empty.');
        return null;
    }
    if (what == 'tag' && tn.replace(/[,"]/g,'') != tn) {
        alert('Invalid tag name “' + tn + '”. Tag names can’t contain commas or double quotes.');
        return null;
    }
    for (const si of subItems.values())
    {
        if (what != 'folder' && siFolderName(si) == tn) {
            alert('Invalid '+what+' name “' + tn + '”. '+What+' name can’t be the same as an existing folder name.');
            return null;
        }
        if (what != 'tag' && siTagName(si) == tn) {
            alert('Invalid '+what+' name “' + tn + '”. '+What+' name can’t be the same as an existing tag name.');
            return null;
        }
        if (// what != 'smart stream' &&
            siSmartStreamName(si) == tn) {
            alert('Invalid '+what+' name “' + tn + '”. '+What+' name can’t be the same as an existing smart stream name.');
            return null;
        }
    }
    return tn;
}

export function getUsedTags() { return arrayToList(getUsedTagsArray()); }
export function getUsedTagsArray() { return selectSubItemNames(siTagName) }
export function selectSubItemNames(f)
{
    const r = [];
    subItems.forEach(si => {
        const t = f(si);
        if (t)
            r.push(t);
    });
    return r;
}

function siTagName(si)
{
    return typeof(si._SIType) == "object" ? si._SIType.v._TagName : null;
}
function siSmartStreamName(si)
{
    return typeof(si._SIType) == "object" ? si._SIType.v._StreamName : null;
}
function siFolderName(si)
{
    return typeof(si._SIType) == "object" ? si._SIType.v._Folder : null;
}

let mw_ = null;
export function mw() {
    return mw_;
}
export function setmw(mw) { mw_ = mw; return true; }
export function getCharCode() { return uw_event.charCode; }

export function alwaysFalse() { return false; }

let readCounters = {};

export function updateReadCounters(uc)
{
    for (let c = uc; c != null; c = c._2) {
        updateReadCounters_(c._1);
    }
}

export function getReadCounters()
{
    let r = null;
    for (const i in readCounters) {
        r = { _1: readCounters[i], _2: r };
    }
    return r;
}

let isWindowActive_ = true;
let refreshNumber = 0;
let updateSubscriptionsFunc = null;
let lastUpdateTime = Date.now();
let backgroundRpcFlush = null;

function updateSubscriptions()
{
    if (updateSubscriptionsFunc)
        execF(updateSubscriptionsFunc);
    lastUpdateTime = Date.now();
}

export function isWindowActive() { return isWindowActive_ }

window.onfocus = () => {
    isWindowActive_ = true;
    const t = Date.now();
    if (t - lastUpdateTime > 120000)
        updateSubscriptions();
};
window.onblur = () => {
    if (backgroundRpcFlush)
        execF(backgroundRpcFlush);
    isWindowActive_ = false;
};

export function registerUpdateSubscriptions(upd)
{
    updateSubscriptionsFunc = upd;
    setInterval(function () {
        if (refreshNumber % 3 == 0 || isWindowActive())
            // неактивное окно обновляем реже (раз в 5.5 минут)
            updateSubscriptions();
        refreshNumber++;
    }, isMobile ? 600000 : 110000);
}
export function registerBackgroundRpcFlush(f) {
    backgroundRpcFlush = f;
}

// for (const ufi in urfuncs) {
//     const f = urfuncs[ufi].f;
//     if (f.length > 20000)
//         uw_debug('urfuncs['+ufi+'].f.length = ' + f.length);
// }

let lazyLoadVideos = getFromLocalStorage('', 'lazyLoadVideos', '1') != '0';
export function setLazyLoadVideos(l) {
    lazyLoadVideos = l ? true : false;
    saveToLocalStorage('','lazyLoadVideos', l ? '1' : '0');
}

let clickedInPopup_ = false;

document.addEventListener('click', (e) => {
    uw_event = e;
    const t = uw_event.target;
    clickedInPopup_ =
           $(t).parents('.popup, .menu').length
        || (leftPanel.isMovable0() && $(t).parents('.left').length);
}, useCapture(true));

// Т.к. после клика может удалиться элемент, по которому кликали
// (например, после клика по discoveryTopic, список топиков очищается
// перед загрузкой результатов поиска), мы определяем где был клик
// *до* вызова обработчиков конкретных элементов, используя EventListener
// с useCapture = true.
export function clickedInPopup() { return clickedInPopup_ }


let setDiscoveryFeed = null;
let subscribeDiscoveryFeed = null;
let discoveryHide = null;
export function set_setDiscoveryFeed(x) { setDiscoveryFeed = x; }
export function set_subscribeDiscoveryFeed(s) { subscribeDiscoveryFeed = s; }
export function set_discoveryHide(h) { discoveryHide = h; }

export function discoveryClearSelection()
{
    $('.discoveryContents .selected').removeClass('selected');
    $('.discoveryContents .dfBtnOuter').remove();
}
export function dfClick(event, li)
{
    discoveryClearSelection();

    const url = $(li).data('url');
    const si = getSubItemByUrl(url);

    if (si) {
        setFeedI(si._Index);
    } else {
        let l = $(li).data('link');
        if (l == "") l = null;
        const title = $(li).find('.dfTitle .buttonText').text();
        const f = $(li).find('.favicon').attr('style');
        execF(execF(execF(execF(execF(execF(setDiscoveryFeed, url), title), l), f), null));
    }

    // сначала ставим фид, а потом обновляем кнопку,
    // т.к. установка фида убирает кнопку в discovery
    $(li).addClass('selected');
    const btn = document.createElement('div');
    const i = $(li).find('.dfTitle')[0];
    const outer = document.createElement('div');
    outer.className = 'dfBtnOuter';
    i.appendChild(outer);
    if (si) {
        btn.innerHTML = 'subscribed';
        btn.className = 'discoveryAlreadySubscribed dfBtn';
    } else {
        btn.innerHTML = 'Subscribe';
        btn.className = 'textButton dfBtn';
        btn.onclick = function (event) { uw_event = event; uw_stopPropagation(); dfDblClick(null, li); }
    }
    outer.appendChild(btn);
}
function dfDblClick(event, li)
{
    const url = $(li).data('url');
    const si = getSubItemByUrl(url);
    if (si) {
        setFeedI(si._Index);
        execF(discoveryHide);
    } else {
        execF(execF(subscribeDiscoveryFeed, url));
    }
}
export function cleanDiscoveryQuery(f)
{
    f = $.trim(f).replace(/\s+/g, ' ');
    return ($.trim(f.replace(/#/g, '')) == "") ? "" : f;
}
export function discoveryQueryLooksLikeUrl(q)
{
    return q.match(/.\.../) != null;
}

export function logTime(name, action) {
    console.time(name);
    const r = execF(action);
    console.timeEnd(name);
    return r;
}

export function newMtvmMap(l) {
    const r = { _MtvmMap : 0 };
    while (l) { r[l._1._1] = l._1._2; l = l._2; }
    return r;
}
export function updateMtvmMap(k, x, m) { m[k] = x; }
export function clearMtvmMap(k, m) { m[k] = undefined; }
export function lookupMtvmMap(k, m) { return m[k]; }

export function up1() { msgTreeDiv().scrollTop -= 1/browserScale; }
export function down1() { msgTreeDiv().scrollTop += 1/browserScale; }

export function strReplace(from,to,s) { return s.split(from).join(to); }

const unsafeSources = {};
export function unsafeSource(n, v) {
    if (!(n in unsafeSources))
        unsafeSources[n] = sc(v);
//    uw_debug('unsafeSource ' + n);
    return unsafeSources[n];
}
export function unsafeGlobal(n, v) {
    if (!(n in unsafeSources))
        unsafeSources[n] = execF(v);
//    uw_debug('unsafeGlobal ' + n);
    return unsafeSources[n];
}
const unsafeStorageSources = {};
let userId = null;
export function unsafeStorageSource(read, show, n, v) {
    return unsafeStorageSource_((x) => execF(read, x), (x) => execF(show, x), n, v)
}
export function unsafeStorageEnumSource(list, n, v) {
    return unsafeStorageSource_((x) => {
        for (let l = list; l; l = l._2)
            if (l._1._2 == x) return l._1._1;
        return v
    }, (x) => {
        for (let l = list; l; l = l._2)
            if (l._1._1 == x) return l._1._2;
        return ""
    }, n, v)
}
function unsafeStorageSource_(read, show, n, v) {
    if (!(n in unsafeSources)) {
        const s = {
            name : n,
            read : read,
            show : show,
            source : sc(v)
        };
        const save = () => {
            saveToLocalStorage(userId, n, s.show(s.source.data));
            return { _sources : null }
        };
        s.source.dyns = cons({
            signal : { body : { c :"f", f : save } },
            recreate : function () {},
            sources : null
        }, null);
        unsafeStorageSources[n] = s;
        if (userId)
            initStorageSource(s);
        unsafeSources[n] = s.source;
    }
//    uw_debug('unsafeGlobal ' + n);
    return unsafeSources[n];
}
function initStorageSource(s) {
    const x = s.read(getFromLocalStorage(userId, s.name, s.show(s.source.data)));
    if (x != s.source.data)
        sv(s.source, x)
}
export function initStorageSources(uid) {
    userId = uid;
    for (const n in unsafeStorageSources)
        initStorageSource(unsafeStorageSources[n])
}

export function textareaReplaceAndSelectText(id, from, to) {
    const ta = elem(id);
    if (!ta)
        return;
    const t = ta.value
    const start = t.indexOf(from);
    if (start == -1)
        return;
    ta.value = t.substr(0,start) + to + t.substr(start+from.length);
    ta.setSelectionRange(start, start + to.length);
    ta.focus();
    if (ta.onchange)
        ta.onchange(); // чтобы Ur/Web обновил связанный source
}
export function trim(s) { return s.trim() }

const loadedImgSizes = Object.create(null);
function loadedImgKey(i) {
    const u = i.className !== 'authorPic'
        ? uim($(i).parents('.msgFrame').attr('id')) : null;
    const key = u ? JSON.stringify(uimMsgKey(u))+'\n' : '';
    return key + (i.src ? i.src : "") + '\n'
        + (i.srcset ? i.srcset : "") + '\n' + (i.sizes ? i.sizes : "");
}
function loadedTweetKey(i) {
    return "tweet-" + i;
}
function loadedInstagramKey(i) {
    const m = i.match(/^https:\/\/(www\.)?instagram\.com\/p\/([^/?#]+)/);
    return "instagram-" + (m ? m[2] : i);
}
export function restoreLoadedImgSizes() {
    fixUnprocessed("bqrUnknownImgSize", function (i) {
        const img = i[0];
        const wh = loadedImgSizes[loadedImgKey(img)];
        if (!wh
            || img.className != "bqrUnknownImgSize"  // avatar/thumbnail
            || img.getAttribute('width') > 0
            || img.getAttribute('height') > 0)
            return;

        // задаем сохраненную ранее ширину и высоту, чтобы
        // не было лишних изменений высоты страницы (и проблем
        // с неправильным расположением курсора) в процессе
        // загрузки картинок.
        const scaler = document.createElement('span');
        scaler.className = "imgScaler";
        scaler.setAttribute("style",
            (img.getAttribute('style') ? img.getAttribute('style') + "; " : "")
            + "width: " + wh.width + "px; height: auto");

        const inner = document.createElement('span');
        inner.setAttribute("class", "scalable");
        inner.setAttribute("style", "padding-bottom: " + (wh.width > 0 ? wh.height * 100 / wh.width : 0) + "%");
        scaler.appendChild(inner);

        img.parentNode.insertBefore(scaler, img);
        img.parentNode.removeChild(img);

        img.setAttribute("class", "inner bqrUnknownImgSize");
        img.removeAttribute("style");
        inner.appendChild(img);
    });
}
function restoreTwitterImgSizes() {
    fixUnprocessed('twitter-tweet', function (t) {
        t.find('a').each(function () {
            const h = this.getAttribute('href');
            if (!h) return;
            const m = h.match(/status\/([0-9]+)$/);
            if (!m) return;
            const s = loadedImgSizes[loadedTweetKey(m[1])];
            if (!s || !s.style) return;
            t.attr("style", s.style);
        })
    });
}
function restoreInstagramImgSizes() {
    fixUnprocessed('instagram-media', function (t) {
        const h = t.find('a').last().attr('href');
        if (!h) return;
        const s = loadedImgSizes[loadedInstagramKey(h)];
        if (!s || !s.style) return;
        t.attr("style", s.style);
    });
}
export function saveLoadedImgSizes(pid) {
    $(elem(pid)).find('img').each(function () {
        const ns = this.naturalWidth * this.naturalHeight;
        const key = loadedImgKey(this);
        if ((this.complete || ns > 0) && $(this).is(':visible')
            && this.style.width == "" && this.style.height == ""
            && !this.getAttribute("width") && !this.getAttribute("height")
            // ничего не делаем, если вдруг задан width или height
            // (случаи с заданными в стиле width и height в пикселах
            // обрабатываются в Preprocess и класс bqrUnknownImgSize таким <img>
            // не устанавливается)
            && !loadedImgSizes[key]) {
            let wh =
                { width : this.naturalWidth, height : this.naturalHeight };
            if (ns <= 0) {
                // в случае ошибки загрузки используем экранный размер
                const r = this.getBoundingClientRect();
                const i = $(this);
                const s = (p) => pf(i.css(p));
                wh =
                    { width : r.width
                      - s('border-left-width')
                      - s('border-right-width')
                      - s('padding-left')
                      - s('padding-right')
                    , height : r.height
                      - s('border-top-width')
                      - s('border-bottom-width')
                      - s('padding-top')
                      - s('padding-bottom')
                    };
            }

            loadedImgSizes[key] = wh;
        }
    });
    $(elem(pid)).find('iframe.twitter-tweet-rendered').each(function () {
        const id = this.getAttribute('data-tweet-id');
        let   s =  this.getAttribute('style');
        if (id && s && id != "" && s != "") {
            s += '; height: ' + this.getBoundingClientRect().height + 'px';
            loadedImgSizes[loadedTweetKey(id)] = { style : s };
            // к сожалению, при загрузке твита, происходит короткий скачок
            // всего что снизу на несколько пикселей вниз и обратно.
            // Наличие/отсутствие заданной высоты blockquote на это не влияет,
            // Но при навигации вверх нижележащие сообщения находятся на месте,
            // так что этот код нужен.
        }
    });
    $(elem(pid)).find('iframe.instagram-media-rendered').each(function () {
        const src = this.getAttribute('src');
        const h =  this.getAttribute('height');
        let   s =  this.getAttribute('style');
        if (src && s && src != "" && s != "" && h && h != "") {
            s += '; height: ' + h + 'px';
            loadedImgSizes[loadedInstagramKey(src)] = { style : s };
        }
    });
}

let imageProxy = "";
function fixProxyUrl(u) {
    return u.replace(/(.*\/image_proxy\/)(-\/)/,
                     imageProxy != "" ? "$1" + imageProxy + "/" : "")
}
export function setImageProxy(s) {
    if (imageProxy == s)
        return;
    imageProxy = s;
    $(".articles img").each(function () {
        const e = this;
        const es = e.getAttribute('data-orig-src');
        if (es) {
            e.setAttribute('src', fixProxyUrl(es));
            if (e.className == 'inner bqrUnknownImgSize') {
                e.classList.remove("inner");
                const scalable = e.parentNode;
                scalable.removeAttribute("class");
                scalable.removeAttribute("style");
                const imgScaler = scalable.parentNode;
                imgScaler.removeAttribute("class");
                imgScaler.removeAttribute("style");
            }
        }
    });
    $("audio, video").each(function () {
        this.pause();
        // удивительно, но при display:none audio/video всё равно играются
    });
    updateProxiedAttrs()
    // TODO: у background-image: url(...) тоже надо уметь ставить
    // правильный размер прокси и убирать/восстанавливать его
    // и poster у video тоже надо заменять
    // но для этого необходимо делать полноценный разбор style (в нем
    // теоретически может быть много url(), а простая текстовая замена
    // image_proxy/-/ может заменить лишнее), так что пускай background-image
    // всегда проксируется и всегда без изменения размера.
    // А за счет proxied/nonProxied placeholder-ов проблема есть только
    // у background-image из фидов, что бывает достаточно редко
//     $(".articles [style]").each(function () {
//         const e = this;
//         const s = e.getAttribute("style");

//         const b = e.style.backgroundImage;
//         if (b) {
//             e.style.backgroundImage = fixProxyUrl(
//         }
//     }
}
function updateProxiedAttr(n, a) {
    n.setAttribute(a, n.getAttribute((imageProxy != "" ? "data-proxied-" : "data-nonproxied-") + a));
}
function updateProxiedAttrs() {
    $("embed").each(function () { updateProxiedAttr(this, "src") });
    $("object").each(function () { updateProxiedAttr(this, "data") });
}
export function preprocessImg(html) {
    return html.replace(/(<img[^>]* src=")([^"]*\/image_proxy\/)(-\/)([^"]*")/g,
                        (imageProxy != ""
                         ? "$1$2" + imageProxy + "/$4"
                         : "$1$4")
                        + ' data-orig-src="$2$3$4');
}
function appendToPageInner(page, html) {
//     if (page.children.length == 0) {
//         const label = document.createElement("div");
//         label.innerHTML = "<b>Page #" + page.getAttribute('id') + "</b>";
//         page.appendChild(label);
//     }
    if (html != '') {
//         console.time('appendToPageInner');
        const dummy = document.createElement("span");
        page.appendChild(dummy);
        setInnerHTML(dummy, html);
//         console.timeEnd('appendToPageInner');
    }
}
export function appendToPage(parentId, pageId, html) {
    let page = elem(pageId);
    if (!page) {
        page = document.createElement("div");
        page.setAttribute('class', 'pagerPage');
        page.setAttribute('id', pageId);
        elem(parentId).appendChild(page);
    }
    appendToPageInner(page, html);
}
export function hidePage(pid) {
    saveLoadedImgSizes(pid)
    const p = elem(pid);
    $(p).hide();
    $(p).children('span').each(function () { setInnerHTML(this, '') });
    p.innerHTML = '';
}
export function showPage(pid, inner) {
//    console.time('showPage '+pid);
    const p = elem(pid);
    appendToPageInner(p, inner);
    $(p).show();
//    console.timeEnd('showPage '+pid);
}

const requestAnimationFrameOnceActive = {};
function requestAnimationFrameOnce(name, f) {
    if (requestAnimationFrameOnceActive[name])
        return;
    else {
        requestAnimationFrameOnceActive[name] = true;

        requestAnimationFrameF(function () {
            requestAnimationFrameOnceActive[name] = false;
            f();
        });
    }
}
export function requestAnimationFrameOnceUr(name, code) {
    requestAnimationFrameOnce(name, function () { execF(code); });
}

function requestAnimationFrameF(f) {
    if (window.requestAnimationFrame) {
        window.requestAnimationFrame(f);
    } else {
        setTimeout(f, 66);
    }
}
export function requestAnimationFrameUr(code) {
    requestAnimationFrameF(function () { execF(code); });
}
export function css(id, prop, val) {
    $(elem(id)).css(prop, val);
}
export function addClass(sel, cls) {
    $(sel).addClass(cls);
}
export function removeClass(sel, cls) {
    $(sel).removeClass(cls);
}
let uimOnClickHandlers = [];
export function uimOnClick(cls, fun) {
    uimOnClickHandlers.push({ cls: cls, fun: fun, button: 0, e: 'click' });
}
export function uimOnMiddleclick(cls, fun) {
    uimOnClickHandlers.push({ cls: cls, fun: fun, button: 1, e: 'click' });
}
export function uimOnMiddlemousedown(cls, fun) {
    uimOnClickHandlers.push({ cls: cls, fun: fun, button: 1, e: 'mousedown' });
}
export function uimOnContextmenu(cls, fun) {
    uimOnClickHandlers.push({ cls: cls, fun: fun, e: 'contextmenu' });
}
export function uimOnLongtap(cls, fun) {
    uimOnClickHandlers.push({ cls: cls, fun: fun, e: 'longtap' });
}
function uimClickHandler(e) {
    uw_event = e;
//     uw_debug(`uimClickHandler, button = ${e.button}`);
    const b = ((isMac ? e.metaKey : e.ctrlKey) && e.button <= 1) ? 1 : e.button;
    // cmd+click = middleclick
    const u = uimByElement(e.target);
    if (!u)
        return false;
    let n = e.target;
    for (;;) {
        if (n.getAttribute('onclick'))
            return false;
        for (var i in uimOnClickHandlers) {
            const h = uimOnClickHandlers[i];
            if (h.e == 'click' && (e.type != 'click' && e.type != 'auxclick'
                                   || h.button != b)
                || h.e == 'mousedown' && (e.type != 'mousedown'
                                          || h.button != e.button)
                   // mousedown используется только для отмены появления
                   // autoscroll, так что проверяем только e.button,
                   // без имитаций middleclick
                || h.e == 'contextmenu'
                   && (e.type != h.e || e.metaKey || e.altKey)
                || h.e == 'longtap' && e.type != h.e
               )
                continue;

            if ($(n).hasClass(h.cls)) {
//                 uw_debug('uimClickHandler ' + h.cls);

                execF(execF(h.fun, u));
                return true;
            }
        }
        if ($(n).hasClass("msgFrame"))
            break;
        n = n.parentNode;
    }
    return false;
}

addClickHandler(uimClickHandler);
document.addEventListener('mousedown', uimClickHandler, useCapture(true));
if (!isTouchDevice) {
    document.addEventListener('contextmenu', uimClickHandler, useCapture(true));
    // Android выдает 'contextmenu' по long tap
}
onLongTap(uimClickHandler);
// true, чтобы stopPropagation отключал Ur/Web-овский document.onclick
// Интересно, что если для обычных нод useCapture/stopPropagation не влияют
// на вызов и порядок вызовов .onclick/eventListener, то в document
// eventListener-ы c useCapture=true вызываетюся в первую очередь
// и stopPropagation в них может отменить последующие onclick и eventListener-ы
// с useCapture=false

// Из-за uimOnClick Safari не подсвечивает msgButtons и commentsButton
// при нажатии.
// Добавляем пустой onclick чтобы iOS Safari все-таки подсвечивала.
// Добавляем не через Ur/Web, чтобы не создавать кучу лишних замыканий
// (для чего и была придумана uimOnClick).
export function withDummyOnclick(b) {
    if (b.cat1) {
        return { cat1 : withDummyOnclick(b.cat1), cat2 : b.cat2 };
    } else if (b.substr(0,3) == '<a ') {
        return '<a onclick="" ' + b.substr(3);
    } else
        return b
}
// Замена iframe_placeholder на настоящий iframe
export function replaceWithHtml(n, t) {
    uw_stopPropagation();
    popupsHide(); // т.к. click не приходит
    if (eventTargetHasLink())
        // клик по ссылке-заголовку не должен запускать воспроизведение
        return;
    const span = document.createElement('span');
    span.innerHTML = t;
    //const ch = [...span.childNodes];
    // ^ не рабоает на iPad 2
    const ch = [];
    for (let i = 0; i < span.childNodes.length; i++)
        ch.push(span.childNodes[i]);
    // копируем, т.к. при добавлении в DOM ноды будут удаляться из childNodes
    n.parentNode.replaceChild(ch[0], n);
    for (let i = 1; i < ch.length; i++)
        ch[0].parentNode.insertBefore(ch[i], ch[i-1].nextSibling);
}

export function fromDatetimeUtc(year, month, day, h, m, s) {
    return Date.UTC(year, month, day, h, m, s) * 1000;
}

export function copyToClipboardEnabled() {
    try {
        return document.queryCommandSupported('copy'); // &&
            // document.queryCommandEnabled('copy')
    } catch (_) {
        return false;
    }
}
export function copyToClipboard(e) {
    select(e);
    try {
        document.execCommand('copy');
    } catch (_) {
    }
    blur(e); // Chrome не делает blur (видимо не на что)
    // forcedBlur также не работает, т.к. dummyButton.blur() возвращает фокус
    // на input
    // при этом, если использовать setSelectionRange вместо select
    // (чтобы не было фокуса на элементе) Chrome перестает копировать выделение
}
export function imgOnLoad(img) {
    if (img.style.backgroundColor == "")
        img.style.setProperty("background-color", "white", "important");
    // чтобы полупрозрачные изображения на темном фоне смотрелись нормально
}
export function toFixed(x, n) { return x.toFixed(n) }

// Копируем выделение в буфер обмена, убирая soft hyphen.
// Поля ввода игнорируются и копируются браузером как есть
// (заодно не занимаемся вырезанием текста при cut).
// Стиль шрифта текста не копируется и это хорошо.
function copySelectionToClipboard(e) {
    try {
        const selection = window.getSelection();
        if (selection.isCollapsed) {
            // на edit-ах почему-то collapsed
            return;
        }
        const html = [];
        for (let i = 0; i < selection.rangeCount; i++) {
            const r = selection.getRangeAt(i);
            if (elementIsEditable(r.startContainer)) {
                return;
                // Доходим ли мы досюда?
            }

            const c = r.cloneContents().childNodes;
            for (let ci = 0; ci < c.length; ci++) {
                html.push(c[ci].outerHTML || c[ci].data || "");
            }
        }
        e.clipboardData.setData('text/plain', selection.toString().replace(/\xAD/g, ""));
        e.clipboardData.setData('text/html', html.join("").replace(/\xAD/g, ""));
//         uw_debug(e.clipboardData.getData('text/html'));

        e.preventDefault();
        // ^ чтобы браузер не скопировал выделение в буфер обмена
    } catch (e) {
    }
}

document.addEventListener('copy', copySelectionToClipboard);
document.addEventListener('cut', copySelectionToClipboard);

function calcDirAuto() {
    const i = document.createElement("input");
    try {
        i.dir = "auto";
        return "auto";
    } catch (e) {
        return "ltr";
        // Edge не поддерживает auto и бросает исключение в Ur/Web-коде
        // создания input (где идет присвоение i.dir=...)
    }
}
const _dirAuto = calcDirAuto();
export function dirAuto() { return _dirAuto }
export function unsafeTransactionToSignal(t) {
    return sr(execF(t));
}
function fromUrShareData(d) {
    return { // title : d._Title,
             // text : d._Text,
             text : d._Title,
             url : d._Url }
    // iOS игнорирует title
    // Android использует его только в email и закладках firefox
}
export function canShare(d) {
    // canShare нужен только для файлов, в iOS его вообще нет
    return navigator.canShare ? navigator.canShare(fromUrShareData(d))
        : "share" in navigator;
}
export function share(d) {
    if (navigator.share) {
        navigator.share(fromUrShareData(d))
    }
}
