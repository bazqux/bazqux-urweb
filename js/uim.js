"use strict";

import { $ } from './vendor'

let uims_ = {};
export function clearuims() { uims_ = {}; return true; }
export function setuim(id, uim) {uims_[id] = uim; return true; }
export function uim(id) { return uims_[id]; }
export function uimIdByMsgKey(k) {
    for (let i in uims_) {
        const uk = uimMsgKey(uims_[i]);
        if (uk._BlogFeedUrl == k._BlogFeedUrl
            && uk._PostGuid == k._PostGuid
            && uk._CommentGuid == k._CommentGuid) {
            return i;
        }
    }
    return null;
}
export function uimMsgKey(uim) { return uim.v._Mi._MsgId._MsgKey; }

export function uimByElement(e) {
    const frame = $(e).hasClass('msgFrame') ? e : $(e).parents('.msgFrame')[0];
    return frame ? uim(frame.id) : null;
}
