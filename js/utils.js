function whine(msg)
{
    throw msg; // убираем urweb-овский alert
}

try{
    // http://www.opera.com/support/kb/view/827/
    opera.setOverrideHistoryNavigationMode('compatible');
    history.navigationMode = 'compatible';
}catch(e){}

function nodeNameLo(e)
{
    return e.nodeName.toLowerCase();
}

function isNewlineElement(e, prevIsNewline)
{
    if (e.nodeType == 1)
    {
        var nn = nodeNameLo(e);
        if (nn.match(/(div|p|br)/))
            return true;
        else {
            var elements = e.childNodes;
            var newline = prevIsNewline;
            for( var i = 0; i < elements.length; i++) {
                newline = isNewlineElement(elements[i], newline);
            }
            return newline;
        }
    }
    else if (e.nodeType == 3)
        return prevIsNewline && e.data.trim() == "";
}

function replaceGtToCite(div)
{
    var elements = div.childNodes;
    var newline = true;
    for( var i = 0; i < elements.length; i++) {
        var e = elements[i];
        var nn = nodeNameLo(e);
        if (e.nodeType == 1 && nn.match(/(div$|p$|span$)/)) {
            replaceGtToCite(e);
        }
        if (e.nodeType == 1 && (nn == "i" || nn == "em") && newline) {
            // italic-ноды на новой строке превращаем в цитату
            // TODO: может em-ноды тоже?
            var cite = document.createElement('cite');
            div.replaceChild(cite, e);
            cite.innerHTML = e.innerHTML;
            continue;
        }
        var wasNewline = newline;
        newline = isNewlineElement(e, newline);
        if (e.nodeType != 3 || !wasNewline)
            continue;
        var s = e.data;
        var gtPos = s.indexOf(">");
        var count = 0;
        for ( var c = 0; c < gtPos; c++ )
        {
            if (s.charAt(c).match(/\s/) == null)
                count++;
            if (count > 3)
                break;
        }
        if (gtPos != -1 && count <= 3)
        {
            // перед '>' от одного до 3-х значащих символов,
            // делаем cite всем элементам до <br>
            for (; i < elements.length; i++)
            {
                var e = elements[i];
                if (e.nodeType == 1 && nodeNameLo(e) == "br") {
                    break;
                }
                if (e.nodeType != 1 && e.nodeType != 3)
                    continue;
                var cite = document.createElement('cite');
                div.replaceChild(cite, e);
                cite.appendChild(e);
            }
            newline = true;
        }
    }
}

function foreachNode(func)
{
    return function (div) {
        var elements = div.childNodes;
        for( var i = 0; i < elements.length; i++) {
            func(div, elements[i]);
        }
    };
}

// code from link from http://stackoverflow.com/questions/470832/getting-an-absolute-url-from-a-relative-one-ie6-issue
function parseURI(url) {
  var m = String(url).match(/^([^:\/?#]+:)?(\/\/(?:[^:@]*(?::[^:@]*)?@)?(([^:\/?#]*)(?::(\d*))?))?([^?#]*)(\?[^#]*)?(#[\s\S]*)?/);
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

// code from http://stackoverflow.com/questions/37684/how-to-replace-plain-urls-with-links
function linkify(inputText) {
    var replaceText, replacePattern1, replacePattern2, replacePattern3;

    //URLs starting with http://, https://, or ftp://
    replacePattern1 = /(\b(https?|ftp):\/\/[-A-Z0-9+&@#\/%?=~_|!:,.;]*[-A-Z0-9+&@#\/%=~_|])/gim;
    replacedText = inputText.replace(replacePattern1, '<a href="$1" target="_blank">$1</a>');

    //URLs starting with "www." (without // before it, or it'd re-link the ones done above).
    replacePattern2 = /(^|[^\/])(www\.[\S]+(\b|$))/gim;
    replacedText = replacedText.replace(replacePattern2, '$1<a href="http://$2" target="_blank">$2</a>');

    //Change email addresses to mailto:: links.
    replacePattern3 = /(\w+@[a-zA-Z_]+?(\.[a-zA-Z]{2,6})+)/gim;
    replacedText = replacedText.replace(replacePattern3, '<a href="mailto:$1">$1</a>');

    return replacedText
}

var plainUrlsToLinks = foreachNode(function (div, e) {
    if (e.nodeType == 1) {
        if (nodeNameLo(e) != "a")
            plainUrlsToLinks(e);
    }
    if ( e.nodeType == 3 )
    {
        var s = document.createElement("span");
        var t = document.createTextNode(e.data);
        s.appendChild(t);
        var s2 = linkify(s.innerHTML);
        if ( s.innerHTML != s2 )
        {
            s.innerHTML = s2;
            foreachNode(function (div, e) {
                    if (e.nodeType == 1 && nodeNameLo(e) == "a")
                        e.href = e.href.replace("&amp;", "&");
                })(s);
            div.replaceChild(s, e);
        }
    }
});

var highlightPres = foreachNode(function (div, e) {
    if (e.nodeType == 1) {
        var n = nodeNameLo(e);
        if (n == "pre")
        {
            if (e.childNodes.length > 0 && nodeNameLo(e.childNodes[0]) == "code")
                hljs.highlightBlock(e.childNodes[0], null, false);
            else
                hljs.highlightBlock(e, null, false);
        }
        else
            highlightPres(e);
    }
});

// В постах иногда попадаются одиночные <li>, которые почему-то вылезают
// отдельными пунктами _после_ сообщения, а не внутри него.
// Возможно, это какой-нить глюк Ur/Web-а (такой же как с вложенными div-ами)
var wrapLIs = foreachNode(function (div, e) {
    if (e.nodeType == 1) {
        if (nodeNameLo(e) == "li") {
            var ul = document.createElement('ul');
            ul.className = "insertedUL";
            div.replaceChild(ul, e);
            ul.appendChild(e);
        }
        if (nodeNameLo(e) != "ul" && nodeNameLo(e) != "ol")
            wrapLIs(e);
    }
});

var rmStylesheets = foreachNode(function (div, e) {
    if (e.nodeType == 1) {
        if (nodeNameLo(e) == "link" && e.getAttribute("rel") &&
            e.getAttribute("rel").toLowerCase() == "stylesheet") {
            var s = document.createElement('span');
            div.replaceChild(s, e);
        }
        else
            rmStylesheets(e);
    }
});
var clearPosition = foreachNode(function (div, e) {
    if (e.nodeType == 1) {
        if (e.style && e.style.position) {
            // бывает реклама с атрибутом position:fixed
            e.style.position = null;
        }
        if (e.style && e.style.zIndex) {
            e.style.zIndex = null;
        }
        clearPosition(e);
    }
});

function preprocessMessageText(x)
{
    var div = document.createElement('div');
    //createDocumentFragment(); <- не определяет "> ..."
    div.innerHTML = x.replace(/<div[^>]+\/>/g, '<div></div>');
    // TODO: не сливать div-ы в tagsoup, а то
    //   div.innerHTML = "<a href='asdf'><div/></a>"
    //   или "<div style='...' />"
    // выдает какую-то жесть
    var child0 = div.childNodes[0];
//     else
//         while (div.childNodes.length == 1 && div.childNodes[0] &&
//                nodeNameLo(div.childNodes[0]).match(/(div|p)/)) {
//             div = div.childNodes[0];
//         }
    // не стоит так делать, вдруг стиль какой у div-а есть
//    var child0 = div.childNodes[0];
    if (child0) {
        if (child0.className == "bqrHabr")
            highlightPres(div);
        for (var c in div.childNodes) {
            child0 = div.childNodes[c];
            if (child0.style && child0.style.textAlign == "justify") {
                child0.style.textAlign = "left";
            }
        }
        var lastChild = div.childNodes[div.childNodes.length-1];
        lastChild.className += " last";

        plainUrlsToLinks(div);
        wrapLIs(div);
        replaceGtToCite(div);
        clearPosition(div);
        if (x.indexOf("link") != -1)
            rmStylesheets(div);
    }
    return div.innerHTML;
//    return cat("<script>replace(", cat(cs(code), cat(")'>", "</script>")));
}

function bqrHabrahabrToggleSpoiler(s)
{
    var n = s.nextSibling;
    if (n.style.display == "block") {
        s.parentNode.className = "bqrHabr_spoiler";
        if (isMobile) {
            n.style.display = "none";
            return;
        }
        var h0 = n.clientHeight;
        n.style.height = h0+'px';
        $(n).animate({
            height: '0px', marginTop : '0px',
            paddingTop : '0px', paddingBottom : '0px'
        }, 300, 'swing', function () {
            n.style.display = "none";
            n.style.height = "auto";
        });
    } else {
        s.parentNode.className = "bqrHabr_spoiler bqrHabr_spoiler_open";
        n.style.display = "block";
        if (isMobile) {
            return;
        }
        var h = n.clientHeight;
        n.style.height = '0px';
        n.style.marginTop = '0px';
        n.style.paddingTop = '0px';
        n.style.paddingBottom = '0px';
        $(n).animate({
            height: h+'px', marginTop : '10px',
            paddingTop : '10px', paddingBottom : '10px'
        }, 300, 'swing', function () {
            n.style.height = "auto";
        });
    }
}

function fixMessageImageUrl(s, w)
{
    if (!s)
        return null;
    if (s.substr(0, 4).toLowerCase() == "data" || s.length > 1000)
        return s; // на длинных data:image/... виснет второй regexp
    if (s.match(/\.files\.wordpress\.com\//))
    {
        return s.replace(/\?w=.+$/, "?w="+w);
    }
    var m = s.match(/(.*\.(bp.blogspot|ggpht|googleusercontent).com\/[^\/]+\/[^\/]+\/[^\/]+\/[^\/]+\/)[^\/]+(\/.*)/);
    if (m)
        return m[1]+'s'+w+m[3];
    return s;
}

var vimeo_thumbnails = {}

function videoPreviewImg(s)
{
    // Возвращается не закрытый <img ..., чтобы можно было добавить к нему
    // запасные картинки и обработчики ошибок
    if (!s) return null;
    var m = s.match(/youtube.*com\/(watch\?v=|embed\/|v\/)([0-9a-zA-Z\-_]+)/);
    // бывают youtube-nocookie.com
    if (m && m.length >= 3)
    {
        return '<img src="https://img.youtube.com/vi/' + m[2] + '/0.jpg"';
    }

    m = s.match(/youtu.be\/([0-9a-zA-Z\-_]+)/);
    if (m && m.length >= 2)
    {
        return '<img src="https://img.youtube.com/vi/' + m[1] + '/0.jpg"';
    }

    m = s.match(/vimeo.com\/(video\/|moogaloop.swf\?clip_id=|)([0-9a-zA-Z\-_]+)/);
    if (m && m.length >= 3)
    {
        if (vimeo_thumbnails[m[2]] != undefined)
            return '<img src="'+vimeo_thumbnails[m[2]]+'"';
        else
        {
            var id = fresh();
            $.jsonp({
                type:'GET',
                url: 'http://vimeo.com/api/v2/video/' + m[2] + '.json?callback=?',
                cache: true,
                success: function(data, textStatus, jqXHR) {
                    var e = elt(id);
                    if (data[0] != undefined)
                    {
                        var thumb = data[0].thumbnail_large;
                        vimeo_thumbnails[m[2]] = thumb;
                        if (e) e.src = thumb;
                    }
                },
                error: function (jqXHR, textStatus) {
                    var e = elt(id);
                    if (e) nextMessageImage(e);
                }
            });

            return '<img id="'+id+'"';
        }
    }
    return null;
}

function checkVideoSrcs(div, elemType)
{
    var elems = div.getElementsByTagName(elemType);
    var len = elems.length;
    for (var i = 0; i < len; ++i) {
        var f = elems[i];
        var p = videoPreviewImg(f.src || f.href);
        if (p) return p;
    }
    return null;
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
    var a = [];
    flattenAcc_(a, tr);
    return a.join("");
}

function messageImage(x, attachments)
{
    attachments = flatten_(attachments);
    var div = document.createElement('div');
    div.innerHTML = x.replace(/<div[^>]+\/>/g, '<div></div>');//.replace(/<img/g, '<img_');
    if (attachments != "")
    {
        var divA = document.createElement('div');
        divA.innerHTML = attachments;
        if (div.firstChild)
            div.insertBefore(divA, div.firstChild);
        else
            div.appendChild(divA);
    }
    var videoImg = checkVideoSrcs(div, "iframe") || checkVideoSrcs(div, "embed");
    var imgs = div.getElementsByTagName("img");
    var len = imgs.length;
    for (var i = 0; i < len; ++i) {
        var img = imgs[i];
        var w = parseInt(img.getAttribute("width")) || img.width || 100;
        var h = parseInt(img.getAttribute("height")) || img.height || 100;
        if (img.src && checkMessageImageWH(w,h))
        {
            var j = videoImg ? -1 : 0;
            var srcs = ' data-n='+j;
            for (j=j+1; j <= 5 && i+j < len; ++j)
                srcs += ' data-src-'+j+'="'+atr(imgs[i+j].src)+'"';
            if (img.complete && !videoImg) srcs += " class=alreadyLoaded";
            return (videoImg || ('<img src="'+atr(fixMessageImageUrl(img.src, 240))+'"')) + srcs + ' style="opacity:1" onload="checkMessageImage(this)" onerror="nextMessageImage(this)"></img>';
            // загрузка без opacity transition все-таки лучше,
            // видно, что картинки грузятся, а не появляются в случайном порядке
        }
    }
    var rawVideoImg = videoPreviewImg(x + attachments);
    return rawVideoImg ? rawVideoImg+"></img>" : null;
}

function checkMessageImageWH(w, h)
{
    return (w > 30 && h > 30 && (w > 40 || h > 40))
}

function checkMessageImage(img)
{
    if (checkMessageImageWH(img.width, img.height))
        img.style.opacity = 1;
    else
        nextMessageImage(img);
}

function nextMessageImage(img)
{
    img.className = ""; // AdBlock в FF добавляет свой класс и прячет image
    var n = parseInt(img.getAttribute('data-n'))+1;
    img.setAttribute('data-n', n);
    var src = img.getAttribute('data-src-'+n);
    if (src) {
        img.src = src;
        if (img.complete)
            checkMessageImage(img); // onload не вызовется
    }
    else
        img.style.visibility = 'hidden';
}

function authorPicImg(src, err)
{
    var id = fresh();
    var eid = fresh();
    var e = flatten_(err);
    return '<img id="'+id+'" class=authorPicLoading src="'+atr(src)
        + '" onload="apOnload(\''+id+'\')" onerror="apOnerror(\''+eid+'\')">'
        + '</img><span style="display:none" id="'+eid+'">'+e+'</span>'
        + '<script>apCheck(\''+id+'\')</script>';
}
function apCheck(id)
{
    setTimeout(function () { if (complete(id)) apOnload(id) }, 1);
}
function apOnload(id)
{
    var e = elt(id); if (e) e.className = 'authorPic';
// TODO: стоит проверять размер картинки, если 1х1
// (как у blogspot-овского blank.gif), то это тот же error ?
}
function apOnerror(id)
{
    var e = elt(id); if (e) e.style.display = 'inline';
}

function downloadLink(x, kind, popupInner)
{
    var t = x._Url;
    var q = t.search(/[\?#]/);
    if (q != -1) t = t.substr(0, q);
    var slash = t.lastIndexOf('/');
    if (slash != -1) t = t.substr(slash+1);
    if (t == '')
        t = x._Url;
    t = decodeURIComponent(t);
    var details = '';
    if (x._FileSize > 1) {
        var fs = x._FileSize;
        var s = '';
        var k = 1024; var m = k*k; var g = k*m;
        if (fs > g) s = (fs / g).toFixed(1) + 'GB'
        else if (fs > 10*m) s = (fs / m).toFixed() + 'MB'
        else if (fs > m) s = (fs / m).toFixed(1) + 'MB'
        else if (fs > 1024) s = Math.round(fs / 1024) + 'kB'
        else s = fs + ' bytes';
        details = s;
    }
    if (x._Duration > 1) {
        var d = x._Duration;
        var h = Math.floor(d / 3600).toFixed();
        var m = Math.floor((d % 3600)/60).toFixed();
        var s = Math.floor(d % 60).toFixed();
        function pad(x) { return x.length > 1 ? x : '0'+x };
        var dt = '';
        if (h != '0') dt = h+':'+pad(m)+':'+pad(s)
        else if (m != '0') dt = m+':'+pad(s)
        else dt = s + 'sec';
        if (details != '')
            details = dt + ', ' + details;
        else
            details = dt;
    }
    if (details != '')
        details = ' (' + details + ')';
    var link = '<div class="downloadLink '+kind+'"><a target=_blank title="'+decodeURIComponent(atr(x._Url))+'"href="'+atr(x._Url)+'">'+atr(t)+'</a>'
    var popup = popupInner != null ? cat(", <a target=bqrPopup onclick='enclosurePopup(event,", cat(cs(kind == "video" ? '' : link + details), cat(",", cat(popupInner, ",\""+kind+"\")' href='"+atr(x._Url) +"'>popup</a>")))) : "";
    return cat(link + details, cat(popup, '</div>'));
}

var prevPopupKind = '';

function enclosurePopup(event,link, inner,kind)
{
    uw_event = event;
    uw_stopPropagation();
    uw_preventDefault();
    var ch = kind == "audio" ? 100 : 396;
    var w = window.open('', "bqrPopup", "scrollbars=no,resizable=yes,toolbar=no,location=no,status=0,width=700,height=" + ch);
    var i =
        '<html><head>' +
        '<title>▶</title>' +
        '<link href="https://bazqux.com/css/basic_v5.css" media="all" rel="stylesheet" type="text/css" />' +
        '<script type="text/javascript" src="https://bazqux.com/js/libs_v3.js"></script>'+
        '</head><body class=bqrPopup>' + inner + link + '</body></html>';
    w.document.write(i);
    w.document.close();
//     if (prevPopupKind != kind) {
//         w.resizeTo(700, ch);
//         // если было видео и осталось видео -- сохраняем размер
//         prevPopupKind = kind;
//     }
    // resizeTo лажа, т.к. меняет размер рамки, а не содержимого
}

function attachmentXml(x)
{
    x = x.v;
    if (x.hasOwnProperty("_Poster")) {
        var v = '<video controls autoplay '
            + (x._Poster != null ? 'poster="' + atr(x._Poster) + '"' : '')
            + '><source src="' + atr(x._Url)
            + '" type="' + x._Mime + '" /></video>';
        var a = v;
        var id = fresh();
        var act = cat('elt(\''+id+'\').innerHTML = ', cs(v));
        if ((navigator.userAgent.match(/(firefox|opera)/i)
             && (x._Mime == "video/mp4" || x._Mime == "video/x-m4v"))
            || x._Mime == 'video/x-flv' || x._Mime == 'video/x-f4v'
            || (x._Mime == 'video/quicktime' &&
                !navigator.userAgent.match(/safari/i)))
        {
            v = 'flowplayer(\''+id+'\', \'/images/flowplayer/flowplayer-3.2.16.swf\', { clip: { url : \'' + x._Url.replace(/'/g, "\\'") + '\', autoPlay: true, scaling: \'fit\' }, canvas: {background: \'#000000\'} })';
            a = '<div class=bqrMsgVideo id=' + id + '></div><script>'
                +v+ '</script>';
            act = cat('elt(\''+id+'\').innerHTML = \'\';', v);
        }
        var r =
        cat('<div class=bqrMsgVideo id=' + id + '>'
            + '<div class=bqrMsgVideo style="cursor:pointer;background-color:black;'
            + (x._Poster != null
               ? 'background-image:url(\'' + atr(x._Poster) + '\');background-size:contain;background-position:center;background-repeat:no-repeat;' : '')
            + '" onclick="',
            cat(act, cat('"><div class="btnPlayVideoOuter"><div class="btnPlayVideo"></div></div></div></div>', cat (downloadLink(x, 'video', cs(a)),
            (x._Poster != null ? '<img style="display:none" src="' + x._Poster + '"></img>' : '')
             // дублирую, чтобы thumbnail был
             ))));
        return r;
    }
    else if (x.hasOwnProperty("_Xml")) {
        return x._Xml.replace("http://player.vimeo.com", "https://player.vimeo.com").replace("http://www.youtube.com", "https://www.youtube.com");
    }
    else if (x.hasOwnProperty("_HtmlUrl")) { // AGrOrigin
        return "GrOrigin??"
    } else if (x.hasOwnProperty("_Mime")) {
        if (x._Mime.indexOf("audio") == 0) {
            var a = '';
            if ((x._Mime == "audio/mpeg" // || x._Mime == "audio/x-m4a"
                ) &&
                navigator.userAgent.match(/(firefox|opera)/i))
            {
                a = '<embed type="application/x-shockwave-flash" src="/images/audio-player.swf" quality="best" flashvars="autoPlay=True&audioUrl=' + encodeURIComponent(x._Url) + '" width="700" height="27"></embed>';
            }
            else
            {
                a = '<audio controls autoplay><source src="'
                    + atr(x._Url) + '" type="' + x._Mime + '" ></audio>';
            }
            var id = fresh();
            return cat(
                '<div class=bqrMsgAudio id=' + id + '>'
                + '<div class=bqrMsgAudioInner onclick="',
                cat(cat('elt(\''+id+'\').innerHTML = ', cs(a)),
                    cat('"><div class="btnPlayAudioOuter"><div class="btnPlayAudio"></div></div></div></div>', downloadLink(x, 'audio', cs(a)))));
        }
        else {
            return downloadLink(x); // other
        }
    }
    else
    {
        var u = fixMessageImageUrl(x._Url, 700)
        return '<img src="' + atr(u) + '"'
            + (x._Title  != null ? ' alt="' + atr(x._Title) + '"' : '')
            + (u == x._Url && x._Width  != null ? ' width=' + x._Width : '')
            + (u == x._Url && x._Height != null ? ' height=' + x._Height : '')
            + '></img>';
    }
}

function removeXMLTags(x)
{
    function textOnly(acc, div)
    {
        var elements = div.childNodes;
        for( var i = 0; i < elements.length; i++) {
            var e = elements[i];
            if (e.nodeType == 1) {
                acc = textOnly(acc, e);
            }
            if (e.nodeType != 3)
                continue;
            acc = acc + e.data;
        }
        return acc;
    }

    var div = document.createElement('div');
    div.innerHTML = x;
    var t = document.createTextNode(textOnly("", div));
    var div = document.createElement('div');
    div.appendChild(t);
    return div.innerHTML.trim();
}

var preprocessSubjectText = removeXMLTags;

function preprocessAuthorText(x)
{
//     var a = removeXMLTags(x);
//     a = x.search("<s>") == 0 ? "<s>" + a + "</s>" : a;
    // разрешаем только зачеркивание
    a = x.replace(/^http:\/\/juick\.com\/(.+)\/$/, '$1');

    return a;
//     var div = document.createElement('div');
//     div.innerHTML = a;
//     plainUrlsToLinks(div);
//     return div.innerHTML.trim();
}

function removeNonMeaningCharaters(s)
{
    return s.split(/\s/).join("").split(".").join("").split(/\n/).join("").split("&").join("");
}

function divMeaningfulText(acc, maxLen, div)
{
    var elements = div.childNodes;
    var r = acc;
    for( var i = 0; i < elements.length && r.length < maxLen; i++) {
        var e = elements[i];
        if (e.nodeType == 1) {
            r = divMeaningfulText(r, maxLen, e);
        }
        if (e.nodeType != 3)
            continue;
        r = r + removeNonMeaningCharaters(e.data);
    }
    return r;
}

// Получить видимый текст без пробелов/тегов/точек/амперсандов
function meaningfulText(text, maxLen)
{
    var div = document.createElement('div');
    div.innerHTML = text;
    return divMeaningfulText("", maxLen, div);
}

function subjectDuplicatesMessage(subject, message)
{
    var ms = meaningfulText(subject, subject.length);
    var md = meaningfulText(message, ms.length);
    return md.indexOf(ms) == 0;
}

function authorIsFoundInSubject(subject, author)
{
    return subject.replace(/(By: |Автор: )/,"") == removeXMLTags(author);
    // автор может быть зачеркнут, по-этому удаляем теги.
//    return author != "" && subject.indexOf(author) != -1;
    // ^ лажа в delicious
    // TODO: надо добавить strsindex в js
}

function strIndexOf(needle, haystack)
{
    return haystack.indexOf(needle);
}

function commentSubjectNotNeeded(authorPic, feedUrl)
{
    var u = parseURI(feedUrl);
    var a = parseURI(authorPic || "");
    return !((u && (u.host.indexOf("livejournal.com") != -1
                    || u.host.indexOf("lambda-the-ultimate.org") != -1))
              || (a && a.host.indexOf("livejournal.com") != -1)
             );
}

function strGt(a, b)
{
    return a.toLowerCase() > b.toLowerCase();
}

function showTags(t)
{
    return t.split(",").map(removeXMLTags).join(", ");
}

function toXml(x)
{
    return x;
}
var fromXml = toXml;

function elt(id)
{
    return document.getElementById(id);
}

function getProp(id, prop)
{
    return elt(id)[prop];
}

function setProp(id, prop, val)
{
    elt(id)[prop] = val;
}

function getStyle(el, styleProp)
{
    if (el.currentStyle)
	return el.currentStyle[styleProp];
    else if (window.getComputedStyle)
	return document.defaultView.getComputedStyle(el,null).getPropertyValue(styleProp);
    else
	return null;
}

/// Активный элемент, в котором возможно редактирование текста
function activeElementIsEditable()
{
    var el = document.activeElement;
    if (!el || el.nodeType != 1) return false;
    var wkum = getStyle(el, '-webkit-user-modify');
    return ((( el.tagName.match(/input|textarea|select|button/i) &&
        (el.getAttribute('type') || '').match(/^|text|search|password$/) )
      || el.getAttribute('contenteditable') == 'true'
      || (wkum != null && wkum != '' && wkum != 'read-only') ? el : false) ? 1 : 0);
}

function getEventKeyIdentifier()
{
    if (!window.event) return "";
    var ki = window.event.keyIdentifier;
    return ki == null ? "" : ki;
}

function mkKeyIdentifier(code)
{
    var p = "U+0000";
    var s = code.toString(16).toUpperCase();
    return p.substr(0, p.length - s.length) + s;
}

function offsetTopLeft(child)
{
    var e = elt(child);
    return e ? {_Top : e.offsetTop, _Left : e.offsetLeft} : null;
}
function offsetBottomRight(child)
{
    var e = $('#'+child+' .buttonLeft');
    if (e.length == 0) {
        e = elt(child).childNodes[0]; // используется только для меню
        return {_Top : e.offsetTop + e.clientHeight, _Left : e.offsetLeft + e.clientWidth}
    } else {
        e = e[0];
        p = e.offsetParent; // msgToolBar
        return { _Top : 0 // e.offsetTop + p.offsetTop + e.clientHeight
               , _Left : e.offsetLeft + p.offsetLeft//  + e.clientWidth
               }
    }
}

var scrollingNow = false;
document.ontouchmove = function(e){
    return !scrollingNow;
}

var pixelsPerMsec = 3.2;

function offsetParentScrollTop(id)
{
    var e = elt(id);
    return e ? e.offsetParent.scrollTop : 0;
}
function setOffsetParentScrollTop(id, st)
{
    var e = elt(id);
    if (e) e.offsetParent.scrollTop = st;
}

function scrollToElement(child, mode, after)
{
    var offset = offsetTopLeft(child);
    if (offset != null) {
        var span = elt(child);
//         var msgBody = span.parentNode;
//         var msgFrame = msgBody.parentNode;
        var t = span.offsetTop;
//         var l = msgFrame.offsetLeft
//             - elt("uw-1").children[0].children[0].offsetLeft;
//             // TODO: сделать id-шку заголовку
//         var w = msgBody.clientWidth+5;
//         var h = msgFrame.clientHeight - (span.offsetTop - msgFrame.offsetTop);
        var msgTree = span.offsetParent;
        var st0 = msgTree.scrollTop;
        var sl = msgTree.scrollLeft;
        var ch = msgTree.clientHeight;
        var st=t; // все-таки как курсор плохо, новые сообщения снизу
//         // выезжают на разную высоту глазу нужно постоянно перестраиваться
//         // и горизонтальную прокрутку при прокрутке мышой надо выполнять только
//         // после остановки
        var time = scrollTime(mode, 100 + Math.min(900, Math.abs((st-st0) / pixelsPerMsec)));
//         uw_debug("ch = " + ch);
//         if (t < st) st = t;
//         if (l < sl) sl = l;
// //         if (st < t-ch/2)  st = t-ch/2;
//         if (t - st + h > ch) {
//             st = Math.min(t, // Math.max(
//                 t//t+h - ch// , t-ch*2/3)
//                          );
// //            time = 500;
//         }
//         if (l - sl + w > msgTree.clientWidth+1)
//             sl = Math.min(l, l+w - msgTree.clientWidth-1);
// //        uw_debug("st = " + st + "; sl = " + sl + "; l = " + l + "; w = " + w + "; mfl = " + msgFrame.offsetLeft);
        if (Math.abs(st - msgTree.scrollTop) > 2 && mode != 'immediate' && !isMobile) { // || sl != msgTree.scrollLeft) {
            scrollingNow = true;
            $(msgTree).scrollTo( { top:st, left:sl }, time,
                                 { easing: scrollEasing(mode),
                                   onAfter: function(){
                                       setTimeout(function () {
                                           scrollingNow = false;
                                           execF(after);
                                           msgTree.onscroll(null)
                                       }, 0); }}
                               );
//             msgTree.scrollTop = st;//offset;//._Top;
//             setTimeout(function () { execF(after); msgTree.onscroll(null) },0);
        }
        else
        {
            if (msgTree.scrollTop != st) msgTree.scrollTop = st;
            execF(after);
            msgTree.onscroll(null);
//            setTimeoutUr("scroll", after, 0);
        }
        // , {easing:'easeOutElastic'});
        // http://gsgd.co.uk/sandbox/jquery/easing/
//        $("#" + parent).scrollTo( { top:offset._Top, left:0 }, 100);
//        elt(parent).scrollTop = offset;//._Top;
//         elt(parent).scrollLeft = offset._Left;
    }
    // child.parentNode -- msgBody -- правильный clientWidth
    // и offsetLeft можно смотреть
    // child.parentNode.parentNode -- msgFrame
}

var isMobile = /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent);

function expandMessage(h0, id, after)
{
    execF(after); return;
    // как-то не очень выглядит разворачивание сообщения как свитка
    // в обе стороны
//     var snap = elt(id);
//     if (!snap) { execF(after); return; }
//     var msgFrame = snap.parentNode.parentNode;
//     if (!msgFrame) { execF(after); return; }
//     var msgTree = msgFrame.offsetParent;
//     var st0 = msgTree.scrollTop;
//     var st = snap.offsetTop;
//     var h = Math.min(msgTree.clientHeight, msgFrame.clientHeight);
//     h0 = Math.min(h0, h);
//     var moves = 0;
//     function onafter () {
//         moves++;
//         if (moves == 2) {
//             execF(after);
//         }
//     };
//     $(msgTree).scrollTo( { top:st, left:0 },
//                          Math.abs(st-st0)/pixelsPerMsec,
//                          { easing: 'linear',
//                            onAfter: onafter } );
//     msgFrame.style.height = h0+'px';
//     $(msgFrame).animate({
//         height: h+'px'
//     }, (h-h0)/pixelsPerMsec/2, 'linear', function () {
//         msgFrame.style.height = "auto";
//         onafter()
//     });
}

function collapseMessage(minHeight, id, after)
{
    execF(after);
}

function scrollTime(mode, time)
{
    return mode == 'immediate' || isMobile ? 0 : (mode == 'quick' ? Math.min(200, time) : time);
}
function scrollEasing(mode)
{
    return mode == 'normal' ? 'linear' : 'swing';
}

function collapseComments(minHeight, id, mode, after)
{
    var g = elt(id);
    if (!g) { execF(after); return; }
    var msgTree = g.offsetParent;
    if (!msgTree) { execF(after); return; }
    var h = Math.min(msgTree.clientHeight - (g.offsetTop - msgTree.scrollTop),
                     g.clientHeight);
    var h0 = Math.max(msgTree.scrollTop - g.offsetTop,
                      minHeight);
//    alert('h='+h+'; h0='+h0+'; h-h0='+(h-h0));
    if (h-h0 <= 0) { execF(after); return; }
    if (!g || isMobile || mode == 'immediate') {
        if (h0 != 0) msgTree.scrollTop -= h0;
        execF(after); return;
    }

    g.style.height = h+'px';
    $("#"+id).animate({
        height: h0+'px'
    }, scrollTime(mode, Math.max((h-h0)/pixelsPerMsec,150))
     , scrollEasing(mode), function () {
        g.style.height = "auto";
        if (h0 != 0) msgTree.scrollTop -= h0;
        execF(after);
    });
}

function expandComments(minHeight, id, mode, after)
{
    var g = elt(id);
    if (!g || isMobile || mode == 'immediate') { execF(after); return; }
    var msgTree = g.offsetParent;
    var h0 = Math.max(msgTree.scrollTop - g.offsetTop,
                      minHeight);
    var h = Math.min(msgTree.clientHeight - (g.offsetTop - msgTree.scrollTop),
                     g.clientHeight);
//    alert('h='+h+'; h0='+h0+'; h-h0='+(h-h0));
    if (h <= 0) { execF(after); return; }

    g.style.height = h0+"px";
    $("#"+id).animate({
        height: h+"px"
    }, scrollTime(mode, Math.max((h-h0)/pixelsPerMsec,150)), scrollEasing(mode), function () {
        g.style.height = "auto";
        execF(after);
    });
}

function moveUpDown(f, id, mode, after)
{
    var g = elt(id);
    var l = execF(f, g.clientHeight);
    var st = Math.min(g.scrollHeight - g.clientHeight, Math.max(0, g.scrollTop + l));
    l = Math.abs(st - g.scrollTop);
    var t = Math.max(100, Math.min(900, l/pixelsPerMsec));
    $("#" + id).scrollTo( { top:st, left: 0 },
                          scrollTime(mode, t),
                          { easing: scrollEasing(mode),
                            onAfter: function() {
                                execF(after); g.onscroll(null); }} );
}

function showAgo(t) {
    var time = new Date(t/1000);
    var cd = new Date();
    var ms = cd.getTime() - time;
    if (ms<0) ms = 0;
    var s = ms/1000;
    var m = s/60;
    var h = m/60;
    var d = h/24;
//     var w = d/7;
//     var mo = w/4;
    var rnd = Math.round;
    if (d > 6) return showDay(cd, time)
    else if (h > 23) return rnd(d)+'d'
    else if (m > 59) return rnd(h)+'h'
    else if (s > 59) return rnd(m)+'m'
    else return rnd(s)+'s';
}

function showTime(t) {
    return formatDate(new Date(t/1000), false, true, false);
}

// var months =
//     ["January","February","March","April","May","June","July","August",
//      "September","October","November","December"];
var months =
    ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
     "Sep","Oct","Nov","Dec"];

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

function formatDate(d,z,span,onlyspan) {
    if (onlyspan) return ago(d);
    var tz = '';
    if (z) {
        var m = d.toString().match(/\(([^\)]+)\)$/);
        if (m && m[1]) tz = ' (' + m[1] + ')';
    }
    if (span) sp = ' (' + ago(d) + ')';
    var cd = new Date();
    var day = showDay(cd, d);

//     return (day != '' ? day + ' ' :
//             pad(d.getHours()) + ':' + pad(d.getMinutes())) + sp;
    return (day != '' ? day + ' at ' : '')
        + pad(d.getHours()) + ':' + pad(d.getMinutes()) + sp;
//        + ':' + pad(d.getSeconds()) + '.' + d.getMilliseconds() + sp;
     function pad(n){
         if (('' + n).length == 1) return '0' + n;
         else return n;
     }
    };

var ago = (function(){
  function seconds(ms){return ms*1000;};
  function minutes(ms){return seconds(ms)*60;};
  function hours(ms){return minutes(ms)*60;}
  function days(ms){return hours(ms)*24;}
  function weeks(ms){return days(ms)*7;}
  function ago(d) {
      var ms = ((new Date).getTime()) - d;
      if (ms<0) ms = 0;
    var rnd = Math.round;
    var out = '';
    function range(x,y){return ms >= x && ms <= y;};
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
  };

  return ago;
})();

//document.addEventListener('touchmove', function(e){ e.preventDefault(); }, false);

function getLocationHash()
{
    return (location.href.split("#")[1] || "");
}
function setLocationHash(h)
{
    window.location.hash = h;
}
if (getLocationHash() == '_=_' || getLocationHash() == '') {
    window.location.hash = ''; // for older browsers, leaves a # behind
    if (history && history.pushState)
        history.pushState('', document.title, window.location.pathname.replace(/^\/\//, "/")); // nice and clean
    if (uw_event) uw_preventDefault(); // no page reload
}
function setDocumentTitle(t)
{
    document.title = t;
}
function setTimeoutUr(what, code, time)
{
    setTimeout(function () {
//        uw_debug("timeout: "+what);
        execF(code);
    }, time);
}
function showId(i) { return i; }
function focus(i) { var e = elt(i); if (e) { e.focus();}  };
function setReadOnly(i, r) { var e = elt(i); if (e) e.setAttribute("readonly", r); };
function blur(i) { elt(i).blur(); };
function inputValue(i) { var e = elt(i); return e ? e.value : ""; };
function select(i) {
    var e = elt(i);
    if (!e) return;
//     if (e.className && e.className.search("Url") != -1 && !navigator.userAgent.match(/msie\s(\d)/i))
//         e.type = "url";
    e.setAttribute("autocapitalize", "none");
    elt(i).select();
}
function openLink(l) {
    if (l.indexOf("mailto:") == 0) {
        var w=window.open(l, "_blank");
        if (navigator.userAgent.match(/chrome/i))
            setTimeout(function () {
                if (w.document.location == "about:blank") w.close() }, 1000); // даем время загрузиться gmail
        else
            w.close();
//        ;
    }
    else
        window.open(l, "_blank");
    // к сожалению, в web-app в iOS открывает ссылку в том же окне
}
function openLinkInBackgroud(l) {
    if (!navigator.userAgent.match(/(chrome|safari)/i)) {
        openLink(l);
    } else {
        // http://stackoverflow.com/questions/10812628/open-a-new-tab-in-the-background
        var a = document.createElement("a");
        a.href = l;
        //    a.target = "_blank";
        var evt = document.createEvent("MouseEvents");
        evt.initMouseEvent("click", true, true, window, 0, 0, 0, 0, 0, true, false, false, true, 0, null);
        // FF работает только если _blank и все false и не открывает в фоне
        // в Opera открывает в том же окне, а если с _blank, то блокирует popup
        return a.dispatchEvent(evt);
        // openLink("http://www.google.com/");
        // window.focus();
    }
}
function windowClose() { window.close() }
function noModifiers() {
    var e = window.event ? window.event : uw_event;
    return !(e && (e.ctrlKey || e.metaKey || e.altKey) && e.keyCode != 38 && e.keyCode != 40);
}
function reloadPage() { window.location.reload(); }

/**
 * jQuery.ScrollTo - Easy element scrolling using jQuery.
 * Copyright (c) 2007-2009 Ariel Flesler - aflesler(at)gmail(dot)com | http://flesler.blogspot.com
 * Dual licensed under MIT and GPL.
 * Date: 5/25/2009
 * @author Ariel Flesler
 * @version 1.4.2
 *
 * http://flesler.blogspot.com/2007/10/jqueryscrollto.html
 */
// ;(function(d){var k=d.scrollTo=function(a,i,e){d(window).scrollTo(a,i,e)};k.defaults={axis:'xy',duration:parseFloat(d.fn.jquery)>=1.3?0:1};k.window=function(a){return d(window)._scrollable()};d.fn._scrollable=function(){return this.map(function(){var a=this,i=!a.nodeName||d.inArray(a.nodeName.toLowerCase(),['iframe','#document','html','body'])!=-1;if(!i)return a;var e=(a.contentWindow||a).document||a.ownerDocument||a;return d.browser.safari||e.compatMode=='BackCompat'?e.body:e.documentElement})};d.fn.scrollTo=function(n,j,b){if(typeof j=='object'){b=j;j=0}if(typeof b=='function')b={onAfter:b};if(n=='max')n=9e9;b=d.extend({},k.defaults,b);j=j||b.speed||b.duration;b.queue=b.queue&&b.axis.length>1;if(b.queue)j/=2;b.offset=p(b.offset);b.over=p(b.over);return this._scrollable().each(function(){var q=this,r=d(q),f=n,s,g={},u=r.is('html,body');switch(typeof f){case'number':case'string':if(/^([+-]=)?\d+(\.\d+)?(px|%)?$/.test(f)){f=p(f);break}f=d(f,this);case'object':if(f.is||f.style)s=(f=d(f)).offset()}d.each(b.axis.split(''),function(a,i){var e=i=='x'?'Left':'Top',h=e.toLowerCase(),c='scroll'+e,l=q[c],m=k.max(q,i);if(s){g[c]=s[h]+(u?0:l-r.offset()[h]);if(b.margin){g[c]-=parseInt(f.css('margin'+e))||0;g[c]-=parseInt(f.css('border'+e+'Width'))||0}g[c]+=b.offset[h]||0;if(b.over[h])g[c]+=f[i=='x'?'width':'height']()*b.over[h]}else{var o=f[h];g[c]=o.slice&&o.slice(-1)=='%'?parseFloat(o)/100*m:o}if(/^\d+$/.test(g[c]))g[c]=g[c]<=0?0:Math.min(g[c],m);if(!a&&b.queue){if(l!=g[c])t(b.onAfterFirst);delete g[c]}});t(b.onAfter);function t(a){r.animate(g,j,b.easing,a&&function(){a.call(this,n,b)})}}).end()};k.max=function(a,i){var e=i=='x'?'Width':'Height',h='scroll'+e;if(!d(a).is('html,body'))return a[h]-d(a)[e.toLowerCase()]();var c='client'+e,l=a.ownerDocument.documentElement,m=a.ownerDocument.body;return Math.max(l[h],m[h])-Math.min(l[c],m[c])};function p(a){return typeof a=='object'?a:{top:a,left:a}}})(jQuery);

function subItemNode(node)
{
    if (node == null) return null;
    if (node.id == null || node.id.substr(0,3) != "cls")
        return subItemNode(node.parentNode);
    else
        return node;
}

function totalOffsetTop(node) {
    var o = 0;
    do { o += node.offsetTop;
         if (node.offsetParent) o -= node.offsetParent.scrollTop;
       } while (node = node.offsetParent);
    return o;
}

function nodeToSubItem(node)
{
    var si = subItemNode(node);
    return si != null ? subItems[domIdToSubItemIndex[parseInt(si.id.replace(/cls/,''))]] : null;

}

function dragMark() { return elt("uw1"); }
function siNodeParentSubItem(n)
{
    while (n) {
        if (n.id && n.id.indexOf("folder") == 0) {
            return subItems[parseInt(n.id.replace(/folder/,''))];
        }
        n = n.parentNode;
    }
    return null;
}

function arrayKeys(obj)
{
    var keys = [];

    for(var key in obj)
    {
        if(obj.hasOwnProperty(key))
        {
            keys.push(key);
        }
    }

    return keys;
}

function findDragTarget(y, folderOK, nodeCheck)
{
    var minO = 1e9;
    var r = {};
    var maxY = $('.subscriptions')[0].clientHeight;
    var last = {};
    var lastNode = null;
    var atTheEnd = false;
    for (var domId in arrayKeys(domIdToSubItemIndex).sort(function(a,b){return a - b}))
    {
        var si = subItems[domIdToSubItemIndex[domId]];
        var node = elt('cls'+domId);
        if (node && node.offsetTop > 0) // видимая нода
            lastNode = node;
        if (si == null || node == null || !nodeCheck(node, si)) {
//            r._Before = true; // под следующей нельзя?? -- так тоже теги не работают
            continue;
        }
        var nodeY = node.offsetTop;
        var ch = node.clientHeight;
        var nodeTO = totalOffsetTop(node);
        var o = y - nodeTO;
        if (folderOK && isFolder(si) && o > ch/4 && o < ch-(ch/4))
            return { _SubItem : si, _Node : node,
                     _ToFolder : true, _ParentSubItem : null, _Before : false }
        if (Math.abs(o) < minO) {
            last.o = o;
            last.ch = ch;
            minO = Math.abs(o);
            last.si = r._SubItem ? r._SubItem : subItems[0];
            last.node = r._Node ? r._Node : elt('cls0');
            r._SubItem = si;
            r._Node = node;
            r._ToFolder = false;
            r._ParentSubItem = siNodeParentSubItem(node);
            r._Before = true;
            if (nodeY + o >= maxY - ch/2 - 2) // последний элемент в списке
                r._Before = false;
            if (nodeY + o >= maxY - 5)
                atTheEnd = true;
        }
    }
    if (!r._SubItem) return null;
    var draggingTag = r._SubItem._Hash.indexOf("tag/") == 0;
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


    if (r._ToFolder == false && r._Before == true
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

var draggingSubItem = null;
var draggingSubItemNode = null;
var lastDragTarget = null;
var onDragAndDrop = null;
var onDragAndDropStart = null;

function registerOnDragAndDrop(c) { return function() { onDragAndDrop = c; } }
function registerOnDragAndDropStart(c) { onDragAndDropStart = c; }
function isDragAndDropActive() { return draggingSubItem != null; }

function onSubItemDragAndDrop(event)
{
    if (event.type == "dragstart") {
        execF(onDragAndDropStart);
        event.originalEvent.dataTransfer.dropEffect = "move";
        draggingSubItemNode = subItemNode(event.target);
        draggingSubItem = nodeToSubItem(event.target);
        $(draggingSubItemNode).addClass('dragging').removeClass('mouseOver');
        event.originalEvent.dataTransfer.setData("text/uri-list", "https://bazqux.com/#" + draggingSubItem._Hash);
        // ^ setData нужен для firefox
//        event.originalEvent.dataTransfer.setDragImage($(draggingSubItemNode).children(".subscriptionItem").children(".buttonText")[0], 0, 0);
        event.originalEvent.dataTransfer.setDragImage(draggingSubItemNode, 0, 0);
    } else if (event.type == "dragover") {
        if (draggingSubItem == null) return;
        var dhash = draggingSubItem._Hash;
        var y = event.originalEvent.clientY;
        var dt = null;
        if (dhash == "tags" || dhash == "starred" ||
            dhash.indexOf("folder/") == 0)
            // top-level
            dt = findDragTarget(y, false,
                                function (n,si)
                                { return si._Index > 0 &&
                                         siNodeParentSubItem(n) == null });
        else if (dhash.indexOf("tag/") == 0)
            // тег
            dt = findDragTarget(y, false,
                                function (n,si)
                                { return si._Hash.indexOf("tag/") == 0; });
        else if (dhash.indexOf("subscription/") == 0)
            // фид
            dt = findDragTarget(y, true,
                                function (node,si)
                                { return si._Index > 0 &&
                                         si._Hash.indexOf("tag/") != 0; });
        if (dt == null)
            return;
        lastDragTarget = dt;
        var siO = dt._Node.offsetTop + 1;
        var ch = dt._Node.clientHeight;
        var mark = dragMark();
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
        event.preventDefault(); // чтобы не открывал как ссылку
        var dt = lastDragTarget;
        if (dt == null) return;
        var nsi = nodeToSubItem(dt._Node);
        var srcPSI = siNodeParentSubItem(draggingSubItemNode);
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
//         uw_debug(dt._SubItem._Hash+"\n"+
//                  (dt._ParentSubItem ? dt._ParentSubItem._Hash : "")+"\n"+
//                  "to folder: " +dt._ToFolder+"; before: "+dt._Before);

        if (!dt._ToFolder) {
            if (dt._NodeToInsertAfter) {
                dt._Node = dt._NodeToInsertAfter;
                dt._Before = false;
            }
            if (dt._Before == false && siNodeParentSubItem(dt._Node) != null &&
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
            if (isFolder(draggingSubItem) || draggingSubItem._Hash == "tags")
                draggingSubItemNode.parentNode.insertBefore(
                    elt("folder" + draggingSubItem._Index),
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
        dragMark().style.display = 'none';
        if (draggingSubItemNode != null)
            $(draggingSubItemNode).removeClass('dragging');
        lastDragTarget = null;
        draggingSubItem = null;
        draggingSubItemNode = null;
    } else if (event.type == "click" || event.type == "mouseover") {
        $(subItemNode(event.target)).addClass("mouseOver");
    } else if (event.type == "mouseout") {
        $(subItemNode(event.target)).removeClass("mouseOver");
    } else if (event.type == "selectstart") {
        event.target.dragDrop(); // для IE9
        return false;
    } else if (event.type == "contextmenu") {
        var si = nodeToSubItem(event.target);
        if (si) {
            var m = execF(execF(onSubscriptionRightClick, si._Index));
            if (m == null) return true;
//            $(e).addClass('menuActive');
            var e = $('#'+m+' .menu')[0]; //.show();
            e.style.display = 'block';
            e.style.left = (event.clientX-2)+'px';
            e.style.margin = '0';
            e.style.overflowY = 'auto';
            var th = $('body').height();
            var mh = e.clientHeight;
            var cy = event.clientY;
            if (cy+mh <= th)
                e.style.top = cy+'px';
            else if (cy-mh > 0)
                e.style.top = (cy-mh-2)+'px';
            else {
                if (cy < th/2) {
                    e.style.top = cy+'px';
                    e.style.height = (th - cy - 10)+'px';
                } else {
                    e.style.top = '3px';
                    e.style.height = (cy-6)+'px';
                }
            }
            return false;
        }
    }
}

var onSubscriptionRightClick = null;

function setOnSubscriptionRightClick(on)
{
    onSubscriptionRightClick = on;
}

function hasOnscreenKeyboard()
{
    return ('ontouchstart' in document || isMobile);
}

function jsInit()
{
    if ('ontouchstart' in document || isMobile) {
        var b = document.getElementsByTagName("body")[0];
        b.className = b.className.replace(/noTouch/g, "");
    }
    else {
        $('.subscriptions').on('dragstart click mouseover mouseout contextmenu selectstart', 'li', onSubItemDragAndDrop);
        $('.left').on('dragend dragover drop', onSubItemDragAndDrop);
        // на весь left, чтобы пустое место под subscriptions тоже работало
//         $('.discoveryContents').on('mousedown', 'li', function (e) {
//             if (e.button == 0)
//                 setTimeout(function () {$('.discoverySearchInput input').focus();},0);
//         })

    }
    $('#uw0').attr('autocomplete', 'on');
}

var opmlOkCode = null;
function opmlForm(body)
{
    var b = body;
    if (navigator.userAgent.match(/msie\s(\d)/i)) {
        b = flatten_(body).replace(/<div/, '<label for=opmlFile').replace(/<a/, '<label for=opmlFile').replace(/<\/a/, '</label').replace(/<\/div/, '</label').replace(/onclick/, 'data-c');
    }
    /* с display:none не работает в Opera */
    return cat(b, "<form style='visibility:hidden;float:left;width:1px;height:0;' name='opmlForm' method=\"post\" action=\"/importOPML\" enctype=\"multipart/form-data\"><input type='hidden' name='Sig' value='"+sig+"' /><input id='opmlFile' type=\"file\" name=\"OPML\" onchange='if(!this.value)return;if(opmlOkCode)execF(opmlOkCode);document.forms[\"opmlForm\"].submit()' onclick='uw_event=event;uw_stopPropagation();' /><input type=\"submit\" onclick='uw_event=event;uw_stopPropagation();' /></form>");
}
function opmlUpload(code)
{
    opmlOkCode = code;
    elt("opmlFile").click();
    opmlOkCode = null;
}

var _gaq = _gaq || [];
_gaq.push(['_setAccount', 'UA-32263037-1']);

function loadGoogleAnalytics(login)
{
    if (location.host != "bazqux.com") return;
    // были какие-то дикие тормоза у iPad с IP адресом
    // из-за них почему-то ajax тормозил

    _gaq.push(['_setCustomVar',
      1,             // This custom var is set to slot #1.  Required parameter.
      'Login',   // The name of the custom variable.  Required parameter.
      login ? 'True' : 'False',      // Sets the value of "User Type" to "Member" or "Visitor" depending on status.  Required parameter.
      2             // Sets the scope to session-level.  Optional parameter.
    ]);
    _gaq.push(['_trackPageview']);

    window.setTimeout(function () {
        var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
        ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
        var s = document.getElementsByTagName('body')[0]; s.appendChild(ga);

//         var div = document.createElement('div');
//         div.innerHTML = "<iframe style='display:none;' src='https://www.subtome.com/register-no-ui.html?name=BazQux%20Reader&url=https%3A%2F%2Fbazqux.com%2Fadd%3Furl%3D%7Burl%7D' />'";
//         document.body.appendChild(div);
    }, 1);
}

function trackEvent(category, action, user)
{
    _gaq.push(['_trackEvent', category, action, user])
}

function complete(id) { return elt(id) ? elt(id).complete : false; }
function offsetTop(id) { return elt(id) ? elt(id).offsetTop : 0; }
function scrollTop(id) { return elt(id).scrollTop; }
function scrollLeft(id) { return elt(id).scrollLeft; }
function setScrollTop(id, x) { elt(id).scrollTop = x; }
function setScrollLeft(id, x) { elt(id).scrollLeft = x; }
function clientHeight(id) { return elt(id).clientHeight; }
function scrollHeight(id) { return elt(id).scrollHeight; }

function execMsgOnClick(code)
{
    var n = uw_event.target;
    var cn = n ? n.className + (n.parentNode ? ' ' + n.parentNode.className : '') : '';
//     uw_debug(cn);
//     while (n && n.className != "mtext")
//     {
//         if (n.nodeName == "A") // не выделяем сообщение при щелчке по ссылке
//             return;
//         n = n.parentNode;
//     }
// //    if (n) // не выделяем сообщение при щелчке по кнопке
    // ^ нафиг не надо, гуглоридер выделяет сообщения при шелчке по ссылке
    if (code != null) // окошко может исчезнуть, и код убъется
        execF(execF(execF(code, cn), uw_mouseEvent()));
}

function msgOnClick(cls, code, inner)
{
    return cat("<span class='"+cls+"' onclick='uw_event=event;execMsgOnClick(", cat(cs(code), cat(")'>", cat(inner, "</span>"))));
}
function stopPL()
{
//     if (!((uw_event.ctrlKey || uw_event.metaKey) && navigator.userAgent.match(/firefox/i)))
        uw_stopPropagation();
    // тупой FF после Ctrl+Click не открывает фоновую вкладку
    // так что пока будем прокручиваться, но все-таки открывать
//    https://bugzilla.mozilla.org/show_bug.cgi?id=748740

}

function stopPropagationLink(cls, title, link, inner)
{
    return cat('<a class="'+cls+'" title="'+title+'" href="', cat(atr(link), cat('" target="_blank" onclick="uw_event=event;stopPL(); return true;">', cat(inner, '</a>'))));
}

function scannedPercentGradientStyle(percent)
{
    return percent == 100 ? "" : "background: -moz-linear-gradient(left, #f5f5f5 0%, #f5f5f5 50%, #fff 50%); background: -webkit-gradient(linear, left top, right top, color-stop(0%,#f5f5f5), color-stop(50%,#f5f5f5), color-stop(50%,#fff)); background: -webkit-linear-gradient(left, #f5f5f5 0%,#f5f5f5 50%,#fff 50%); background: -o-linear-gradient(left, #f5f5f5 0%,#f5f5f5 50%,#fff 50%); background: -ms-linear-gradient(left, #f5f5f5 0%,#f5f5f5 50%,#fff 50%); background: linear-gradient(to right, #f5f5f5 0%,#f5f5f5 50%,#fff 50%); ".replace(/50/g, percent);
    //     return percent == 100 ? "" : "background: -moz-linear-gradient(left, #f5f5f5 0%, #f5f5f5 50%, #fff 50%); /* FF3.6+ */ background: -webkit-gradient(linear, left top, right top, color-stop(0%,#f5f5f5), color-stop(50%,#f5f5f5), color-stop(50%,#fff)); /* Chrome,Safari4+ */ background: -webkit-linear-gradient(left, #f5f5f5 0%,#f5f5f5 50%,#fff 50%); /* Chrome10+,Safari5.1+ */ background: -o-linear-gradient(left, #f5f5f5 0%,#f5f5f5 50%,#fff 50%); /* Opera 11.10+ */ background: -ms-linear-gradient(left, #f5f5f5 0%,#f5f5f5 50%,#fff 50%); /* IE10+ */ background: linear-gradient(to right, #f5f5f5 0%,#f5f5f5 50%,#fff 50%); /* W3C */".replace(/50/g, percent);
    //for(var i=0;i<=100;i+=5) uw_debug(".sp"+i+" { " + scannedPercentGradientStyle(i) + " }")
}

function subItemCls(si, s, cs, cnt, vm)
{
    var p = cnt._TotalPosts - cnt._ReadPosts;
    var cc = cnt._TotalComments - cnt._ReadComments;
    var c = (vm._ExpandedComments || isFolder(si)) ? cc : 0;
    var r = [];
//     if (isFolder(si) && !vm._ExpandedComments && p==0 && cc != 0)
//         r.push("unreadCommentsOnlyInChildren")
    if (s) r.push("selected");
    if (cs) r.push("childSelected");
    if (p==0 && c==0 && cnt._Feed != 0) r.push("allItemsRead");
    if (p==0 && c==0 && !(isFolder(si) && cc != 0) && cnt._Scanning == 0)
        r.push("noNewItems");
//(*                                 (\* && cnt.Error = 0 *\))
    if (p>0) r.push("hasUnreadPosts");
    if (c>0) r.push("hasUnreadComments");
    if (cnt._Scanning != 0) r.push("subItemScanning");
    if (cnt._Error != 0) r.push("subItemError");
    if (isFolder(si) || isTag(si))
    {
        r.push(vm._FolderExpanded ? "folderExpanded" : "folderCollapsed");
    }
    if (isTag(si)) r.push("tagLI");
    return r.join(" ");
}
var exactUnreadCounts = false;
function showUnread(cnt, expandedComments)
{
    var p = cnt._TotalPosts - cnt._ReadPosts;
    var c = expandedComments ? cnt._TotalComments - cnt._ReadComments : 0;
    if (p == 0 && c == 0) return "0"; else
        return ((!exactUnreadCounts && p > 500) ? "500+" : (p != 0 ? p.toString() : "")) +
        (c != 0 ? ("/" + ((!exactUnreadCounts && c > 500) ? "500+" : c.toString())) : "");
// (* 999+ смотрятся короче, но уж слишком сложно *)
}
function setExactUnreadCounts(e)
{
    exactUnreadCounts = e;
    for (var i in subItems)
        updateSubItem(subItems[i]);
}

function setClass(id, cls)
{
    var e = elt(id);
    if (e) e.className = cls;
}
function setInnerHTML_(id, h)
{
    var e = elt(id);
    if (e) e.innerHTML = h;
}

function arrayToList(a) {
    var acc = null;
    for (var i = a.length - 1; i >= 0; i--)
        acc = { _1 : a[i], _2 : acc };
    return acc;
}

function reverse(ls) {
    var acc = null;
    for (; ls; ls = ls._2)
        acc = { _1 : ls._1, _2 : acc };
    return acc;
}
function revAppend(a) {
    var acc = a._2;
    var ls = a._1;
    for (; ls; ls = ls._2)
        acc = { _1 : ls._1, _2 : acc };
    return acc;
}
function lengthUW(ls) {
    var acc = 0;

    for (; ls; ls = ls._2)
        ++acc;

    return acc;
}
function lookupS(a) {
    var x = a._1;
    var ls = a._2;
//     uw_debug("lookupS " + lengthUW(ls));
//     return function(ls) {
        for (; ls; ls = ls._2)
            if (ls._1._1 == x)
                return ls._1._2;
        return null;
//     }
}

var onSetFeed = null;
function setOnSetFeed(osf)
{
    onSetFeed = osf;
}
var onToggleFolder = null;
function setOnToggleFolder(otf)
{
    onToggleFolder = otf;
}

var subItems = [];
var urlToSubItem = {};
var hashToSubItem = {};
var domIdToSubItemIndex = [];
function selectSubItem(idx)
{
//    uw_debug("selectSubItem " + idx);
    var upd = [];
    for (var i in subItems) {
        var si = subItems[i];
        if (si._Selected || si._ChildSelected) {
            si._Selected = false;
            si._ChildSelected = false;
            upd.push(si);
        }
    }
    if (subItems[idx] != undefined)
        setSelectedFlags(upd, subItems[idx]);
    for (var i in upd)
        updateSubItem(upd[i]);
}
function toggleFolder(domIdx)
{
    uw_stopPropagation();
    execF(execF(onToggleFolder, domIdToSubItemIndex[domIdx]));
}
function setFeedI(idx)
{
    execF(execF(onSetFeed, idx));
}
var selectedFeedDomIdx = -1;
function setFeed(domIdx)
{
    selectedFeedDomIdx = domIdx;
    setFeedI(domIdToSubItemIndex[domIdx]);
}
function setFeedL(idx)
{
    setFeedI(idx);
}
function setFeedLink(idx, inner)
{
    return '<a onclick="setFeedL('+idx+')">'+inner+'</a>';
}

function feedKeyboardAction(idx, action)
{
    if (idx < 0) return;
    function last(n){
        if (n == null) return -1;
        var r = n._1;
        while (n = n._2) r = n._1;
        return r
    }
    var domIdx = domIdToSubItemIndex[selectedFeedDomIdx] == idx ? selectedFeedDomIdx : last(subItems[idx]._DomIds);
    var si = subItems[idx];
    var node = elt("cls"+domIdx);
    if (!node) return;
    if (action == "toggleFolder") {
        var p =
            (si._Hash == "tags" || si._Hash.indexOf("folder/") == 0)
            ? si : siNodeParentSubItem(node);
        if (p)
            execF(execF(onToggleFolder, p._Index));
    } else if (action == "parentFolder") {
        var p = siNodeParentSubItem(node);
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

function selectNextFeed(domIdx, f, unreadOnly)
{
    var maxIdx = domIdToSubItemIndex.length - 1;
    var di = domIdx;
    var hasParent = siNodeParentSubItem(elt('cls'+domIdx)) != null;
    while ((di = f(di)) != domIdx) // на всякий случай проверяем на циклы
    {
        if (di > maxIdx) break;//di = 0;
        if (di < 0) break; //di = maxIdx;
        var node = elt('cls'+di);
        if (node && (node.offsetTop > 0 || di == 0)) { // видимая нода
            if (unreadOnly) {
                var si = nodeToSubItem(node);
                if (si._Hash == "starred" || si._Hash.indexOf("tag") == 0
                    || node.className.indexOf("hasUnread") == -1
                    || (!hasParent && siNodeParentSubItem(node) != null))
                    continue;
            }
            var left = $(".left")[0];
            if (node.offsetTop < left.scrollTop)
                left.scrollTop = node.offsetTop;
            if (node.offsetTop + node.clientHeight > left.scrollTop + left.clientHeight)
                left.scrollTop = node.offsetTop + node.clientHeight - left.clientHeight;
            setFeed(di);
            break;
        }
    }
}

var start = 0;
function setSubItems(tuple)
{
    var sameHash = tuple._1;
    var ls = tuple._2;
//    uw_debug("setSubItems");
//     if (ls) {
//         var si = ls._1;
//         subItems[0] = si;
//         si._Counters = sc(si._Counters);
//         si._ViewMode = sc(si._ViewMode);
//     }
//     return true;
    start = new Date().getTime();
//    console.time('setSubItems');
    // При установке списка подписок мы переносим read counts из имеющихся,
    // т.к. на клиенте они могут измениться за время запроса подписок.
    // При их несовпадении мы обновляем UI подписки
    // и накапливаем разницу для папок, которые обновляем в конце.
    // Т.к. titleHash может не измениться, нам необходимо обновлять
    // все UI, чтобы правильно отображать счетчики и стили.
    // Можно попробовать не обновлять total-ы для готовых подписок
    // (также копить их разницу для папок), но пока, думаю, это не нужно.
    var h2si = $.extend({},hashToSubItem); // shallow copy
    var rcFixes = {};
    var rpFixes = {};
    var fixes = [];
    if (!sameHash) // изменился порядок subItem-ов
    {
        subItems = [];
        urlToSubItem = {};
        hashToSubItem = {};
        domIdToSubItemIndex = [];
    }
    for (; ls; ls = ls._2) {
        var si = ls._1;

//         if (sameHash) { // вроде особо не влияет
//             var c0 = subItems[si._Index]._Counters.data;
//             var c = si._Counters;
//             if (c0._TotalPosts == c._TotalPosts &&
//                 c0._TotalComments == c._TotalComments &&
//                 c0._Scanning == c._Scanning &&
//                 c0._ScanningComments == c._ScanningComments &&
//                 c0._Error == c._Error &&
//                 c0._Feed == c._Feed &&
//                 c0._ScannedPercent == c._ScannedPercent
//                ) {
//                 continue;
//             }
//         }

        var url = siUrl(si);
        if (url) urlToSubItem[url] = si;
        hashToSubItem[si._Hash] = si;

        subItems[si._Index] = si;
        si._Selected = false;
        si._ChildSelected = false;

        for (var di = si._DomIds; di; di=di._2)
            domIdToSubItemIndex[di._1] = si._Index;

        var si0 = h2si[si._Hash];
        if (si0 != undefined)
        {
            var vm0 = si0._ViewMode.data;
            var c0 = si0._Counters.data;
            var c = si._Counters;
            var vm = si._ViewMode;
            si._Counters = si0._Counters; // старый source
            si0._Counters = sc(c0);
            si._ViewMode = si0._ViewMode; // не обновляем viewMode
            si0._ViewMode = sc(vm0);
            si._ViewMode.data = vm;
            si._Counters.data = c; // обновили старый source
            if ((url && changedReadCounters[url]) || isTag(si))
                // для подписок, у которых менялось число прочитанных
                // в процессе запроса новых подписок,
                // восстанавливаем readPosts/readComments
            {
                var rp0 = c0._ReadPosts;
                var rc0 = c0._ReadComments;
                var rp = c._ReadPosts;
                var rc = c._ReadComments;
                var folder = isFolder(si)
                if ((vm0._ExpandedComments != vm._ExpandedComments && !folder)
                    || rp0 != rp || rc0 != rc)
                {
                    c._ReadPosts = rp0;
                    c._ReadComments = rc0;
                    var fixRp = rp0 - rp;
                    var fixRc = ((vm0._ExpandedComments || folder) ? rc0 : 0)
                        - ((vm._ExpandedComments || folder) ? rc : 0);
                    for (var f = si._ParentFolders; f; f=f._2)
                    {
                        if (rpFixes[f._1] != undefined) {
                            rpFixes[f._1] += fixRp;
                            rcFixes[f._1] += fixRc;
                        } else {
                            rpFixes[f._1] = fixRp;
                            rcFixes[f._1] = fixRc;
                            fixes.push(f._1);
                        }
                    }
                }
            }
        }
        else
        {
            // для новой подписки создаем новые source
            si._Counters = sc(si._Counters);
            si._ViewMode = sc(si._ViewMode);
        }
    }

    for (var i in fixes) {
        var f = fixes[i];
        var si = subItems[f];
        var c = si._Counters.data;
        c._ReadPosts += rpFixes[f];
        c._ReadComments += rcFixes[f];
    }

    var sel = hashToSubItem[getLocationHash().replace(/^search\/[^\/]+\//,'')];
    var upd = []
    if (sel != undefined) {
        var c = sel._Counters.data;
        if (c._Feed + c._Error + c._Scanning > 0 || isTag(sel))
            setSelectedFlags(upd, sel);
    }

    ls = tuple._2;
    for (; ls; ls = ls._2) {
        var si = ls._1;
        updateSubItem(subItems[si._Index]);
    }
//     setTimeout(function () {
//         var end = new Date().getTime();
//         var time = end - start;
//         alert('Execution time: ' + time); }, 0);

    // Chrome: 8, iPad: 80
    // Parsing: Chrome: 20-80, iPad: 120-180
    // в целом parse занимает порядка 80% времени на iPad.
    // всего обновление 700 подписок где-то 200мсек на iPad
//    console.timeEnd('setSubItems');
    return true;
}

function setSelectedFlags(upd, si)
{
    upd.push(si);
    si._Selected = true;
    for (var p=si._ParentFolders; p; p=p._2) {
        subItems[p._1]._ChildSelected = true;
        upd.push(subItems[p._1]);
    }
}

function getSubItem(idx)
{
    return subItems[idx];
}
function getSubItemByUrl(url)
{
    return urlToSubItem[url];
}
function getSubItemByHash(hash)
{
//        uw_debug("getSubItemByHash " + hash);
    return hashToSubItem[hash];
}
function getSubItems(idx)
{
    var r = null;
    for (var i in subItems)
    {
        var si = subItems[i];
        if (si._Index == idx && !isFolder(si)) r = {_1:si, _2:r};
        for (var p = si._ParentFolders; p; p=p._2)
            if (p._1 == idx) r = {_1:si, _2:r};
    }
    return r;
}
function updateSource(s)
{
    for (var ls = s.dyns; ls; ls = ls.next)
        if (!ls.dead)
            populate(ls.data);
}
var expandedCommentsVm = { _ExpandedComments : true };
function isFolder(si)
{
    return si._Index == 0 || siFolderName(si) != null;
    // starred и all tags -- не папки (all tags просто подписка, которая
    // выводит все теги
}
function isTag(si)
{
    return si._Hash == "starred" || si._Hash == "tags" || siTagName(si) != null;
}
function siUrl(si)
{
    return (typeof(si._SIType) == "object" && si._SIType.v._Subscription)
            ? si._SIType.v._Subscription._Url : null;
}
function updateSubItem(si)
{
    var cnt = si._Counters.data;
    var vm = si._ViewMode.data;
    var uc = showUnread(cnt, vm._ExpandedComments || isFolder(si));
    var c  = subItemCls(si, si._Selected, si._ChildSelected, cnt, vm);
    var ucC = "unreadCount sp" + Math.round(cnt._ScannedPercent/5)*5;
    var fs = si._FaviconStyle;
    for (var d = si._DomIds; d; d=d._2) {
        var i = d._1;
        var clsE = elt("cls" + i);
        var ucE = elt("uc" + i);
        if (clsE.className != c) clsE.className = c;
        if (ucE.className != ucC) ucE.className = ucC;
        if (ucE.innerHTML != uc) ucE.innerHTML = uc;
        if (fs) {
            var fav = elt("fav" + i);
            if (fav.style && fav.style.cssText != fs) fav.style.cssText = fs;
        }
//         if (cnt._Error == 1 && !isFolder(si))
//         {
//             var ch = subItemCharSpan(clsE);
//             if (ch.innerHTML != "!") ch.innerHTML = "!";
//         }
    }
    if (si._Hash.indexOf("folder/") == 0 || si._Hash == "tags")
    {
        var fE = elt("folder"+si._Index);
        var cls = "folder " + (vm._FolderExpanded ? "folderExpanded" : "folderCollapsed");
        if (fE.className != cls) fE.className = cls;
    }

    updateSource(si._Counters);
}
function updateCounters(x) {
    var si = x._1;
    var up = x._2;
    var uc = x._3;
//    var si = getSubItem(idx);
    si._Counters.data._ReadPosts -= up;
    si._Counters.data._ReadComments -= uc;
    updateSubItem(si);

    var ec = si._ViewMode.data._ExpandedComments;
    for (var p = si._ParentFolders; p; p = p._2) {
        var si = getSubItem(p._1);
        si._Counters.data._ReadPosts -= up;
        if (ec)
            si._Counters.data._ReadComments -= uc;
        updateSubItem(si);
    }
    return function () { return false; }
}
function updateReadCounters(c)
{
    var si = getSubItemByUrl(c._1);
    if (si == null) return; // а надо ли???
    var d = si._Counters.data;
    var up = d._ReadPosts - c._2;
    var uc = d._ReadComments - c._3;
    var utp = d._TotalPosts - c._4;
    var utc = d._TotalComments - c._5;
    d._ReadPosts -= up;
    d._ReadComments -= uc;
    d._TotalPosts -= utp;
    d._TotalComments -= utc;
    updateSubItem(si);

    var ec = si._ViewMode.data._ExpandedComments;
    for (var p = si._ParentFolders; p; p = p._2) {
        var si = getSubItem(p._1);
        si._Counters.data._ReadPosts -= up;
        si._Counters.data._TotalPosts -= utp;
        if (ec) {
            si._Counters.data._ReadComments -= uc;
            si._Counters.data._TotalComments -= utc;
        }
        updateSubItem(si);
    }
}
function updateExpandedComments(idx, ec)
{
    var si = getSubItem(idx);
    if (!si) return; // discovery
    var c = si._Counters.data;
    updateSubItem(si);

    for (var p = si._ParentFolders; p; p = p._2) {
        var si = getSubItem(p._1);
        var fc = si._Counters.data;
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
function retryScanning(idx)
{
    var si = getSubItem(idx);
    var c = si._Counters.data;
    c._Scanning = 1;
    c._Error = 0;
    updateSubItem(si);
//     for (var d=si._DomIds; d; d=d._2)
//         subItemCharSpan(elt("cls"+d._1)).innerHTML = "R";
}
// function subItemCharSpan(node)
// {
//     return node.children[1].children[0].children[0];
// }
function hideSubItems(sis)
{
    var fixes = [];
    var fixSi = {};
    for (var s=sis; s; s=s._2)
    {
        var si = s._1;
        for (var d=si._DomIds; d; d=d._2)
            elt("cls"+d._1).style.display = "none";
        var c = si._Counters.data;

        for (var p = si._ParentFolders; p; p = p._2) {
            var fsi = getSubItem(p._1);
            var fc = fsi._Counters.data;
            fc._TotalPosts -= c._TotalPosts;
            fc._ReadPosts -= c._ReadPosts;
            fc._TotalComments -= c._TotalComments;
            fc._ReadComments -= c._ReadComments;
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

    for (var i in fixes) {
        updateSubItem(fixes[i]);
    }

    return true;
}

function fromFolderIcon(s)
{
    return "<div class='fromFolderIcon' style='opacity: 1; " + s + "'></div>";
}

function adjustMenuHeight(menu)
{
    var m = $("." + menu)[0];
    if (!m) return;
    m.style.display = "block";
    m.style.maxHeight =
        ((menu == "tagsMenu"
          ? (m.offsetParent.clientHeight -
             (m.offsetTop - m.offsetParent.scrollTop))
          : ($(".mainPage")[0].clientHeight -
             m.offsetTop - m.offsetParent.offsetTop))
         - 10) + 'px';
    m.style.display = "";
//     alert("adjustFoldersMenuHeight");
}

var searches = [];
var searchId = '';

function setupSearchAutocomplete(id, list, search)
{
    searchId = id;
    while (list != null)
    {
        searches.push(list._1); list = list._2;
    }
    $( "#"+id ).autocomplete({
        appendTo : ".bodyScaler",
        minLength: 0,
        delay: 0,
        select: function( event, ui )  {
            if (!(event.which == 13 || event.keyCode == 13))
                setTimeout(function () { elt(id).onchange(); execF(search); }, 1);
        },
        source: function( request, response ) {
            var matcher = new RegExp( $.ui.autocomplete.escapeRegex( request.term ), "i" );
            response( $.grep( searches, function( value ) {
                value = value.label || value.value || value;
                return matcher.test( value );
            }) );
        }
    });
    $( '#'+id).focus(function () {
        $('#'+id).autocomplete("search");
    });
}

function updateSearchAutocomplete(query)
{
    var prevIdx = searches.indexOf(query);
    if (prevIdx == 0) return;
    if (prevIdx != -1)
        searches = searches.slice(0, prevIdx).concat(searches.slice(prevIdx+1,searches.length));
    searches.unshift(query);
    $('#'+searchId).autocomplete("close");
}

function splitTagsText(s)
{
    return s.split( /,/ ).map(trimString);
}

function isAutocompleteActive(id)
{
    if (!elt(id)) return false;
    return $( "#" + id ).data( "ui-autocomplete" ).menu.active ? true : false;
}

function setupTagAutocomplete(id)
{
    var availableTags = getUsedTagsArray();
    function extractLast( term ) {
      return splitTagsText( term ).pop();
    }
    $( "#"+id )
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
        source: function( request, response ) {
            // TODO: надо учитывать, что курсор может быть не на последнем
            // теге
            var t = extractLast( request.term );
            if (t == '')
                response( null );
            else
            {
                var matcher =
                    new RegExp("^" + $.ui.autocomplete.escapeRegex(t), "i");
                var r = $.grep(availableTags, function (value) {
                    return matcher.test(value.label || value.value || value);
                });
                response (r.length > 0 ? r :
                          $.ui.autocomplete.filter( availableTags, t ) );
            }
        },
        focus: function() {
            // prevent value inserted on focus
            return false;
        },
        select: function( event, ui ) {
            var terms = splitTagsText( this.value );
            terms.pop();
            terms.push( ui.item.value );
            // add placeholder to get the comma-and-space at the end
            terms.push( "" );
            this.value = terms.join( ", " );
            this.onchange();
            $('#'+id).autocomplete("search");
            return false;
        }
      });
//     $( '#'+id).focus(function () {
//         $('#'+id).autocomplete("search");
//     });
}

function setupSubscriptionAutocomplete(type, id)
{
    var subs = [];
    var usedSubs = {};
    for (var domId in arrayKeys(domIdToSubItemIndex).sort(function(a,b){return a - b}))
    {
        var si = subItems[domIdToSubItemIndex[domId]];
        if (usedSubs[si._Index]) continue;
        usedSubs[si._Index] = true;
        if (type == "tag" && si._Hash.indexOf("tag/") != 0)
            continue;
        if (type == "feed" && si._Hash.indexOf("subscription/") != 0)
            continue;
        if (type == "folder" && si._Hash.indexOf("folder/") != 0)
            continue;
        subs.push({ label : si._Title, value : parseInt(domId), url : siUrl(si) });
//         var node = elt('cls'+domId);
//         if (node && node.offsetTop > 0) // видимая нода
//             lastNode = node;
    }

    var maxResults = 100;

    var ac = $( "#"+id )
      .bind( "keydown", function( event ) {
          var k = event.keyCode;
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
        select: function( event, ui )  {
            setFeed(ui.item.value);
            return false;
        },
        source: function( request, response ) {
            var t = request.term;
            var matcher =
                new RegExp("^" + $.ui.autocomplete.escapeRegex( t ), "i");
            var r = $.grep(subs, function (value) {
                return matcher.test(value.label);
            });
            if (r.length == 0)
                r = $.ui.autocomplete.filter( subs, t );
            if (r.length == 0) {
                matcher = new RegExp($.ui.autocomplete.escapeRegex( t ), "i");
                r = $.grep(subs, function (value) {
                    return matcher.test(value.url);
                });
            }

            if (r.length > maxResults) {
                r = r.slice(0, maxResults);
            }
            response (r);
        },
        focus: function() {
            // prevent value inserted on focus
            return false;
        }

    }).data("uiAutocomplete");

    ac._renderItem = function( ul, item ) {
        var li = elt('cls'+item.value).cloneNode(true);
        li.innerHTML = '<a>'+li.innerHTML+'</a>';
        li.className = li.className.replace(/folderExpanded/, 'folderCollapsed');
        return $(li).removeClass('selected').removeClass('mouseOver')
            .appendTo( ul );
    };
    ac._renderMenu = function( ul, items ) {
        var that = this;
        $.each( items, function( index, item ) {
            that._renderItemData( ul, item );
        });
        $( ul ).addClass( "subscriptions" );
        if (items.length >= maxResults)
            $("<li>")
            .addClass('tooManySubscriptionSearchResults')
            .append("...too many results, please narrow your search")
            .appendTo(ul);
    }

    $( '#'+id).focus(function () {
        $('#'+id).autocomplete("search");
    });
}

function setupCountryAutocomplete(id, setCountry)
{
    var countries = [];
    for (var i = 0; i < countriesList.length; i++) {
        var c = countriesList[i];
        countries.push({ value : c.substr(0,2).trim(), label : c.substr(5) });
    }
    var ac = $( "#"+id )
      .bind( "keydown", function( event ) {
          var k = event.keyCode;
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
        select: function( event, ui )  {
            execF(execF(setCountry,ui.item.value));
            uw_event = event;
            uw_stopPropagation(); // чтобы enter текущее сообщение не сворачивал
            return false;
        },
        source: function( request, response ) {
            var t = request.term;
            var matcher =
                new RegExp("^" + $.ui.autocomplete.escapeRegex( t ), "i");
            var r = $.grep(countries, function (value) {
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

    $( '#'+id).focus(function () {
        $('#'+id).autocomplete("search");
    });
}

function passwordHash(passwd)
{
    var salt = CryptoJS.lib.WordArray.random(24).toString(CryptoJS.enc.Base64);
    return salt + CryptoJS.SHA1(salt+passwd).toString(CryptoJS.enc.Base64);
}
function feverApiKey(login, passwd)
{
    return CryptoJS.MD5(login+":"+passwd) + "";
}

function checkLogin(login)
{
    return (login.length >= 4 && login.match(/^[a-zA-Z0-9]+$/) != null);
}

function toLowerCase(s)
{
    return s.toLowerCase();
}

function makePasswordInput(i)
{
    var e = elt(i);
    if (e) e.setAttribute("type","password");
}

function faviconStyle(u)
{
    var uri = parseURI(u);
    return "background-image: url(\"https://s2.googleusercontent.com/s2/favicons?domain=" + atr(uri ? uri.hostname : "") + "&alt=feed\")";
}

function trimString(str) {
    return str.replace(/^\s\s*/, '').replace(/\s\s*$/, '');
}

function getTagsList(id)
{
    if (!elt(id)) return {_1:false, _2:null};
    var ts = splitTagsText(elt(id).value);
    var uniqueTags = [];
    $.each(ts, function(i, el){
        if(el != '' && $.inArray(el, uniqueTags) === -1)
            uniqueTags.push(el);
    });
    for( var i in uniqueTags )
        if (!checkTagName(uniqueTags[i]))
            return {_1:false, _2:null};
    return {_1:true, _2:arrayToList(uniqueTags)};
}


function checkTagName(tn)
{
    tn = trimString(tn);
    if (tn == '') {
        alert('Tag name can\'t be empty.');
        return null;
    }
    if (tn.replace(/[,\"<>\?&\/\\\^]/g,'') != tn) {
        alert('Invalid tag name "' + tn + '". Tag names can\'t contain following symbols: ",<,>,?,&,/,\\,^');
        return null;
    }
    for (var i in subItems)
    {
        var si = subItems[i];
        if (isFolder(si) && si._Title.toLowerCase() == tn.toLowerCase()) {
            alert('Invalid tag name "' + tn + '". Tag name can\'t be the same as existing folder name.');
            return null;
        }
    }
    return tn;
}

function getUsedTags() { return arrayToList(getUsedTagsArray()); }
function getUsedTagsArray()
{
    var r = [];
    for (var i in subItems)
    {
        var si = subItems[i];
        var t = siTagName(si);
        if (t)
            r.push(t);
    }
    return r;
}
function selTag(t)
{
    if (uw_event) uw_stopPropagation();
    for (var i in subItems)
    {
        var si = subItems[i];
        if (siTagName(si) == t)
            setLocationHash(si._Hash)
    }
}

function siTagName(si)
{
    return typeof(si._SIType) == "object" ? si._SIType.v._TagName : null;
}
function siFolderName(si)
{
    return typeof(si._SIType) == "object" ? si._SIType.v._Folder : null;
}
function identity(x) { return x }
var mw_ = null;
function mw(_) {
    return mw_;
}
function setmw(mw) { mw_ = mw; return true; }
var uims_ = {};
function clearuims() { uims_ = {}; return true; }
function setuim(id) { return function(uim) {uims_[id] = uim; return true; }}
function uim(id) { return uims_[id]; }
function getCharCode() { return uw_event.charCode; }

function alwaysFalse() { return false; }

var readCounters = {};

function updateAndSaveReadCounters(feeds, uc)
{
    readCounters = {};

    for (var c = feeds; c != null; c = c._2)
        readCounters[c._1._1] = c._1;
    for (var c = uc; c != null; c = c._2) {
        readCounters[c._1._1] = c._1;
        updateReadCounters(c._1);
    }
}

function getReadCounters()
{
    var r = null;
    for (var i in readCounters) {
        r = { _1: readCounters[i], _2: r };
    }
    return r;
}

var isWindowActive = true;
var refreshNumber = 0;
var updateSubscriptionsFunc = null;
var lastUpdateTime = 0;

function updateSubscriptions()
{
    execF(execF(updateSubscriptionsFunc, !isWindowActive));
    lastUpdateTime = (new Date()).getTime();
}

window.onfocus = function () {
    isWindowActive = true;
    var t = (new Date()).getTime();
    if (t - lastUpdateTime > 120000)
        updateSubscriptions();
};
window.onblur = function () {  isWindowActive = false; };

function registerUpdateSubscriptions(upd)
{
    updateSubscriptionsFunc = upd;
    var i = setInterval(function () {
        if (refreshNumber % 3 == 0 || isWindowActive)
            // неактивное окно обновляем реже (раз в 5.5 минут)
            updateSubscriptions();
        refreshNumber++;
    }, 110000);
//    clearInterval(i)
}

var changedReadCounters = {};

function clearChangedReadCountersSet()
{
    changedReadCounters = {};
}
function markChangedReadCounters(url)
{
    changedReadCounters[url] = true;
}
function getChangedReadCounters()
{
    var r = null;
    for (var i in changedReadCounters) {
        r = { _1: i, _2: r };
    }
    return r;
}

// for (var ufi in urfuncs) {
//     var f = urfuncs[ufi].f;
//     if (f.length > 20000)
//         uw_debug('urfuncs['+ufi+'].f.length = ' + f.length);
// }

var dummyLocalStorage = {};

function getLocalStorage() {
    return (typeof(Storage)!=="undefined" && window.localStorage != null) ? localStorage : dummyLocalStorage;
}
function storageKey(user, key)
{
    return "_" + CryptoJS.MD5(user) + "_" + key;
}
function getFromLocalStorage(user, key, def)
{
    var ls = getLocalStorage();
    key = storageKey(user,key);
    return ls[key] != null ? parseInt(ls[key]) : def;
}
function saveToLocalStorage(user, key, val)
{
    key = storageKey(user,key);
    getLocalStorage()[key] = val;
}
var setDiscoveryFeed = null;
var subscribeDiscoveryFeed = null;
var discoveryHide = null;
function set_setDiscoveryFeed(x) { return function () { setDiscoveryFeed = x;} }
function set_subscribeDiscoveryFeed(s) { subscribeDiscoveryFeed = s; }
function set_discoveryHide(h) { discoveryHide = h; }

function discoveryClearSelection()
{
    $('.discoveryContents .selected').removeClass('selected');
    $('.discoveryContents .dfBtn').remove();
}
function dfClick(event, li)
{
    discoveryClearSelection();
    $(li).addClass('selected');
    var btn = document.createElement('div');
    var i = li; // $(li).find('.subscriptionItem')[0];

    var url = $(li).data('url');
    var si = getSubItemByUrl(url);
    if (si) {
        btn.innerHTML = 'subscribed';
        btn.className = 'discoveryAlreadySubscribed dfBtn';
        i.insertBefore(btn, i.firstChild);
        setFeedI(si._Index);
    } else {
        btn.innerHTML = 'Subscribe';
        btn.className = 'textButton dfBtn';
        btn.onclick = function (event) { uw_event = event; uw_stopPropagation(); dfDblClick(null, li); }
        i.insertBefore(btn, i.firstChild);
        var l = $(li).data('link');
        if (l == "") l = null;
        var title = $(li).find('.subscriptionItem').text();
        var f = $(li).find('.favicon').attr('style');
        execF(execF(execF(execF(execF(execF(setDiscoveryFeed, url), title), l), f), null));
    }
}
function dfDblClick(event, li)
{
    var url = $(li).data('url');
    var si = getSubItemByUrl(url);
    if (si) {
        setFeedI(si._Index);
        execF(discoveryHide);
    } else {
        execF(execF(subscribeDiscoveryFeed, url));
    }
}
function setOninput(id, f)
{
    $('#'+id).bind('input', function () { execF(f); })
}
function cleanDiscoveryQuery(f)
{
    f = $.trim(f).replace(/\s+/g, ' ');
    return ($.trim(f.replace(/#/g, '')) == "") ? "" : f;
}
function discoveryQueryLooksLikeUrl(q)
{
    return q.match(/.\.../) != null;
}

function countryNameFromCountryCode(c)
{
    for (var i in countriesList)
        if (countriesList[i].substr(0,2) == c)
            return countriesList[i].substr(5);
    return countriesList[0].substr(5);
}
function countryCodeFromCountryName(n)
{
    for (var i in countriesList)
        if (countriesList[i].substr(5) == n)
            return countriesList[i].substr(0,2);
    return "-";
}

var countriesList =
    [ "-  - International"
    , "US - United States"
    , "RU - Russian Federation"
    , "DE - Germany"
    , "FR - France"
    , "IT - Italy"
    , "ES - Spain"
    , "GB - United Kingdom"
    , "CA - Canada"
    , "AU - Australia"
    , "AF - Afghanistan"
    , "AX - Åland Islands"
    , "AL - Albania"
    , "DZ - Algeria"
    , "AS - American Samoa"
    , "AD - Andorra"
    , "AO - Angola"
    , "AI - Anguilla"
    , "AQ - Antarctica"
    , "AG - Antigua and Barbuda"
    , "AR - Argentina"
    , "AM - Armenia"
    , "AW - Aruba"
    , "AT - Austria"
    , "AZ - Azerbaijan"
    , "BS - Bahamas"
    , "BH - Bahrain"
    , "BD - Bangladesh"
    , "BB - Barbados"
    , "BY - Belarus"
    , "BE - Belgium"
    , "BZ - Belize"
    , "BJ - Benin"
    , "BM - Bermuda"
    , "BT - Bhutan"
    , "BO - Bolivia, Plurinational State of"
    , "BQ - Bonaire, Sint Eustatius and Saba"
    , "BA - Bosnia and Herzegovina"
    , "BW - Botswana"
    , "BV - Bouvet Island"
    , "BR - Brazil"
    , "IO - British Indian Ocean Territory"
    , "BN - Brunei Darussalam"
    , "BG - Bulgaria"
    , "BF - Burkina Faso"
    , "BI - Burundi"
    , "KH - Cambodia"
    , "CM - Cameroon"
    , "CV - Cape Verde"
    , "KY - Cayman Islands"
    , "CF - Central African Republic"
    , "TD - Chad"
    , "CL - Chile"
    , "CN - China"
    , "CX - Christmas Island"
    , "CC - Cocos (Keeling) Islands"
    , "CO - Colombia"
    , "KM - Comoros"
    , "CG - Congo"
    , "CD - Congo, the Democratic Republic of the"
    , "CK - Cook Islands"
    , "CR - Costa Rica"
    , "CI - Côte d'Ivoire"
    , "HR - Croatia"
    , "CU - Cuba"
    , "CW - Curaçao"
    , "CY - Cyprus"
    , "CZ - Czech Republic"
    , "DK - Denmark"
    , "DJ - Djibouti"
    , "DM - Dominica"
    , "DO - Dominican Republic"
    , "EC - Ecuador"
    , "EG - Egypt"
    , "SV - El Salvador"
    , "GQ - Equatorial Guinea"
    , "ER - Eritrea"
    , "EE - Estonia"
    , "ET - Ethiopia"
    , "FK - Falkland Islands (Malvinas)"
    , "FO - Faroe Islands"
    , "FJ - Fiji"
    , "FI - Finland"
    , "GF - French Guiana"
    , "PF - French Polynesia"
    , "TF - French Southern Territories"
    , "GA - Gabon"
    , "GM - Gambia"
    , "GE - Georgia"
    , "GH - Ghana"
    , "GI - Gibraltar"
    , "GR - Greece"
    , "GL - Greenland"
    , "GD - Grenada"
    , "GP - Guadeloupe"
    , "GU - Guam"
    , "GT - Guatemala"
    , "GG - Guernsey"
    , "GN - Guinea"
    , "GW - Guinea-Bissau"
    , "GY - Guyana"
    , "HT - Haiti"
    , "HM - Heard Island and McDonald Islands"
    , "VA - Holy See (Vatican City State)"
    , "HN - Honduras"
    , "HK - Hong Kong"
    , "HU - Hungary"
    , "IS - Iceland"
    , "IN - India"
    , "ID - Indonesia"
    , "IR - Iran, Islamic Republic of"
    , "IQ - Iraq"
    , "IE - Ireland"
    , "IM - Isle of Man"
    , "IL - Israel"
    , "JM - Jamaica"
    , "JP - Japan"
    , "JE - Jersey"
    , "JO - Jordan"
    , "KZ - Kazakhstan"
    , "KE - Kenya"
    , "KI - Kiribati"
    , "KP - Korea, Democratic People's Republic of"
    , "KR - Korea, Republic of"
    , "KW - Kuwait"
    , "KG - Kyrgyzstan"
    , "LA - Lao People's Democratic Republic"
    , "LV - Latvia"
    , "LB - Lebanon"
    , "LS - Lesotho"
    , "LR - Liberia"
    , "LY - Libya"
    , "LI - Liechtenstein"
    , "LT - Lithuania"
    , "LU - Luxembourg"
    , "MO - Macao"
    , "MK - Macedonia, the former Yugoslav Republic of"
    , "MG - Madagascar"
    , "MW - Malawi"
    , "MY - Malaysia"
    , "MV - Maldives"
    , "ML - Mali"
    , "MT - Malta"
    , "MH - Marshall Islands"
    , "MQ - Martinique"
    , "MR - Mauritania"
    , "MU - Mauritius"
    , "YT - Mayotte"
    , "MX - Mexico"
    , "FM - Micronesia, Federated States of"
    , "MD - Moldova, Republic of"
    , "MC - Monaco"
    , "MN - Mongolia"
    , "ME - Montenegro"
    , "MS - Montserrat"
    , "MA - Morocco"
    , "MZ - Mozambique"
    , "MM - Myanmar"
    , "NA - Namibia"
    , "NR - Nauru"
    , "NP - Nepal"
    , "NL - Netherlands"
    , "NC - New Caledonia"
    , "NZ - New Zealand"
    , "NI - Nicaragua"
    , "NE - Niger"
    , "NG - Nigeria"
    , "NU - Niue"
    , "NF - Norfolk Island"
    , "MP - Northern Mariana Islands"
    , "NO - Norway"
    , "OM - Oman"
    , "PK - Pakistan"
    , "PW - Palau"
    , "PS - Palestinian Territory, Occupied"
    , "PA - Panama"
    , "PG - Papua New Guinea"
    , "PY - Paraguay"
    , "PE - Peru"
    , "PH - Philippines"
    , "PN - Pitcairn"
    , "PL - Poland"
    , "PT - Portugal"
    , "PR - Puerto Rico"
    , "QA - Qatar"
    , "RE - Réunion"
    , "RO - Romania"
    , "RW - Rwanda"
    , "BL - Saint Barthélemy"
    , "SH - Saint Helena, Ascension and Tristan da Cunha"
    , "KN - Saint Kitts and Nevis"
    , "LC - Saint Lucia"
    , "MF - Saint Martin (French part)"
    , "PM - Saint Pierre and Miquelon"
    , "VC - Saint Vincent and the Grenadines"
    , "WS - Samoa"
    , "SM - San Marino"
    , "ST - Sao Tome and Principe"
    , "SA - Saudi Arabia"
    , "SN - Senegal"
    , "RS - Serbia"
    , "SC - Seychelles"
    , "SL - Sierra Leone"
    , "SG - Singapore"
    , "SX - Sint Maarten (Dutch part)"
    , "SK - Slovakia"
    , "SI - Slovenia"
    , "SB - Solomon Islands"
    , "SO - Somalia"
    , "ZA - South Africa"
    , "GS - South Georgia and the South Sandwich Islands"
    , "SS - South Sudan"
    , "LK - Sri Lanka"
    , "SD - Sudan"
    , "SR - Suriname"
    , "SJ - Svalbard and Jan Mayen"
    , "SZ - Swaziland"
    , "SE - Sweden"
    , "CH - Switzerland"
    , "SY - Syrian Arab Republic"
    , "TW - Taiwan, Province of China"
    , "TJ - Tajikistan"
    , "TZ - Tanzania, United Republic of"
    , "TH - Thailand"
    , "TL - Timor-Leste"
    , "TG - Togo"
    , "TK - Tokelau"
    , "TO - Tonga"
    , "TT - Trinidad and Tobago"
    , "TN - Tunisia"
    , "TR - Turkey"
    , "TM - Turkmenistan"
    , "TC - Turks and Caicos Islands"
    , "TV - Tuvalu"
    , "UG - Uganda"
    , "UA - Ukraine"
    , "AE - United Arab Emirates"
    , "UM - United States Minor Outlying Islands"
    , "UY - Uruguay"
    , "UZ - Uzbekistan"
    , "VU - Vanuatu"
    , "VE - Venezuela, Bolivarian Republic of"
    , "VN - Viet Nam"
    , "VG - Virgin Islands, British"
    , "VI - Virgin Islands, U.S."
    , "WF - Wallis and Futuna"
    , "EH - Western Sahara"
    , "YE - Yemen"
    , "ZM - Zambia"
    , "ZW - Zimbabwe"
    ]
