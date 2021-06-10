/*!
 * mqGenie v0.5.0
 *
 * Adjusts CSS media queries in browsers that include the scrollbar's width in the viewport width so they fire at the intended size
 *
 * Returns the mqGenie object containing .adjusted, .width & fontSize for use in re-calculating media queries in JavaScript with mqAdjust(string)
 *
 * Copyright (c) 2014 Matt Stow
 *
 * http://mattstow.com
 *
 * Licensed under the MIT license
 */
(function(window, document) {
    if (!document.addEventListener) {
        window.mqGenie = {
            adjustMediaQuery: function(mediaQuery) {
                return mediaQuery;
            }
        }

        return;
    }

    function processRules(stylesheet, processor) {
        var rules = stylesheet.cssRules ? stylesheet.cssRules : stylesheet.media;
        var rule;
        var processed = [];
        var length = rules.length;

        for (var i = 0; i < length; i++) {
            rule = rules[i];

            if (processor(rule))
                processed.push(rule);
        }

        return processed;
    }

    function getMediaQueries(stylesheet) {
        return processRules(stylesheet, function (rule) {
            return rule.constructor === CSSMediaRule;
        });
    }

    function sameOrigin(url) {
        var loc = window.location;
        var a = document.createElement('a');

        a.href = url;

        return a.hostname === loc.hostname && a.protocol === loc.protocol;
    }

    function isInline(stylesheet) {
        return stylesheet.ownerNode.constructor === HTMLStyleElement;
    }

    function isValidExternal(stylesheet) {
        return stylesheet.href && sameOrigin(stylesheet.href);
    }

    function getStylesheets() {
        var sheets = document.styleSheets;
        var sheet;
        var length = sheets.length;
        var valid = [];

        for (var i = 0; i < length; i++) {
            sheet = sheets[i];

            if (isValidExternal(sheet) || isInline(sheet))
                valid.push(sheet);
        }

        return valid;
    }

    window.mqGenie = (function() {
        var html = document.documentElement;

        html.style.overflowY = 'scroll';
        var width = window.innerWidth - html.clientWidth;
        html.style.overflowY = null;
        return { width : width }
    })();

    document.addEventListener('DOMContentLoaded', function() {
        window.mqGenie = (function() {
            var html = document.documentElement;

//             html.style.overflowY = 'scroll';
//             var width = window.innerWidth - html.clientWidth;
//             html.style.overflowY = null;
            var width = window.mqGenie.width;

            var props = {
                    adjusted: width > 0,
                    fontSize: parseFloat(window.getComputedStyle(html).getPropertyValue('font-size')),
                    width: width,
                    adjustMediaQuery: function(mediaQuery) {
                        if (!window.mqGenie.adjusted)
                            return mediaQuery;

                        var mq = mediaQuery.replace(/\d.+?px/gi, function(c) {
                            return parseFloat(c) + window.mqGenie.width + 'px';
                        });

                        mq = mq.replace(/\d.+?em/gi, function(c) {
                            return ((parseFloat(c) * window.mqGenie.fontSize) + window.mqGenie.width) / window.mqGenie.fontSize + 'em';
                        });

                        return mq;
                    }
                };

            if (props.adjusted) {
                if ('WebkitAppearance' in html.style) {
                    var chromeRX = /Chrome\/(\d*?\.\d*?\.\d*?\.\d*?)\s/g;
                    var chrome = navigator.userAgent.match(chromeRX);
                    var chromeVersion;

                    if (chrome) {
                        chrome = chrome[0].replace(chromeRX, '$1');
                        chromeVersion = chrome.split('.');
                        chromeVersion[0] = parseInt(chromeVersion[0]);
                        chromeVersion[2] = parseInt(chromeVersion[2]);
                        chromeVersion[3] = parseInt(chromeVersion[3]);

                        if (chromeVersion[0] <= 29) {
                            if (chromeVersion[0] === 29 && chromeVersion[2] < 1548 && chromeVersion[3] < 57) {
                                props.adjusted = false;
                            }
                            else if (chromeVersion[0] < 29) {
                                props.adjusted = false;
                            }
                        }
                    }
                    else {
                        props.adjusted = false;
                    }

                    if (!props.adjusted)
                        return props;
                }

                var stylesheets = getStylesheets();
                var stylesheetsLength = stylesheets.length;
                var mediaQueries;
                var mediaQueriesLength;

                for (var i = 0; i < stylesheetsLength; i++) {
                    mediaQueries = getMediaQueries(stylesheets[i]);
                    mediaQueriesLength = mediaQueries.length;

                    for (var j = 0; j < mediaQueriesLength; j++) {
//                         uw_debug('before: ' + mediaQueries[j].media.mediaText);
                        mediaQueries[j].media.mediaText = mediaQueries[j].media.mediaText.replace(/m(in|ax)-width:\s*(\d|\.)+(px|em)/gi, function(strA) {
                            if (strA.match('px')) {
                                return strA.replace(/\d.+?px/gi, function(strB) {
                                    return parseFloat(strB) + props.width + 'px';
                                });
                            }
                            else {
                                return strA.replace(/\d.+?em/gi, function(strB) {
                                    return ((parseFloat(strB) * props.fontSize) + props.width) / props.fontSize + 'em';
                                });
                            }
                        });
//                         uw_debug('after: ' + mediaQueries[j].media.mediaText);
                    }
                }
            }

            return props;
        })();
    });
})(window, document);
