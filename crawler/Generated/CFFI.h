#include <urweb.h>
#include "HsFFI.h"

#include "Rts.h"

uw_unit uw_H_ffi_init(uw_context ctx) { 
static int argc = 7;
static char* arg0 = "/tmp/coreader.exe";
static char* argv[20]; argv[0]=arg0; int i; for(i=1; i<20;i++)argv[i]=NULL; 
argv[1] = "+RTS";
argv[2] = "-N4";
argv[3] = "-T";
argv[4] = "-A64m";
argv[5] = "-I0";
argv[6] = "-M15G";
static char** argv_ = argv;
RtsConfig conf = defaultRtsConfig;
/*conf.rts_opts = "-N4 -I0 -T -A2M";*/
conf.rts_opts_enabled = RtsOptsAll;
setvbuf(stdout,NULL,_IOLBF,0);
/*puts(getenv("GHCRTS"));*/
hs_init_ghc(&argc, &argv_, conf);
return 0; }

uw_Basis_bool uw_Js_alwaysFalse(uw_context ctx, uw_Basis_unit x) { 
return 0; }


extern HsPtr uw_HsFFI_readUserSettings(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_readUserSettings_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_readUserSettings(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_cachedReadUserSettings(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_cachedReadUserSettings_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_cachedReadUserSettings(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_cachedNothingReadUserSettings(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_cachedNothingReadUserSettings_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_cachedNothingReadUserSettings(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_mergeWriteUserSettings(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_mergeWriteUserSettings_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_mergeWriteUserSettings(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_deleteUserSettings(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_deleteUserSettings_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_deleteUserSettings(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_readManyUserSettingss(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_readManyUserSettingss_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_readManyUserSettingss(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_cachedReadManyUserSettingss(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_cachedReadManyUserSettingss_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_cachedReadManyUserSettingss(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_cachedNothingReadManyUserSettingss(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_cachedNothingReadManyUserSettingss_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_cachedNothingReadManyUserSettingss(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_writeManyUserSettingss(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_writeManyUserSettingss_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_writeManyUserSettingss(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_readSession(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_readSession_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_readSession(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_cachedReadSession(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_cachedReadSession_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_cachedReadSession(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_cachedNothingReadSession(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_cachedNothingReadSession_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_cachedNothingReadSession(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_mergeWriteSession(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_mergeWriteSession_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_mergeWriteSession(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_deleteSession(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_deleteSession_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_deleteSession(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_readManySessions(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_readManySessions_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_readManySessions(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_cachedReadManySessions(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_cachedReadManySessions_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_cachedReadManySessions(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_cachedNothingReadManySessions(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_cachedNothingReadManySessions_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_cachedNothingReadManySessions(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_writeManySessions(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_writeManySessions_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_writeManySessions(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_readMsg(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_readMsg_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_readMsg(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_cachedReadMsg(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_cachedReadMsg_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_cachedReadMsg(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_cachedNothingReadMsg(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_cachedNothingReadMsg_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_cachedNothingReadMsg(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_mergeWriteMsg(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_mergeWriteMsg_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_mergeWriteMsg(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_deleteMsg(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_deleteMsg_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_deleteMsg(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_readManyMsgs(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_readManyMsgs_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_readManyMsgs(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_cachedReadManyMsgs(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_cachedReadManyMsgs_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_cachedReadManyMsgs(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_cachedNothingReadManyMsgs(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_cachedNothingReadManyMsgs_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_cachedNothingReadManyMsgs(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_writeManyMsgs(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_writeManyMsgs_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_writeManyMsgs(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_loginGetForwardUrl(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_H_ffi_loginGetForwardUrl_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_loginGetForwardUrl(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_loginCallback(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_H_ffi_loginCallback_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_loginCallback(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userSubscribe(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_H_ffi_userSubscribe_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_userSubscribe(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userDiscoverySubscribe(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4, HsPtr a5, HsPtr a6);
uw_Basis_string uw_H_ffi_userDiscoverySubscribe_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4, uw_Basis_string x5, uw_Basis_string x6)
{
    long size;
    char* cr = uw_HsFFI_userDiscoverySubscribe(ctx, &size, x1, x2, x3, x4, x5, x6);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userRenameSubscription(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_H_ffi_userRenameSubscription_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_userRenameSubscription(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userRenameFolder(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_H_ffi_userRenameFolder_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_userRenameFolder(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userEditSubscriptionFolders(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_H_ffi_userEditSubscriptionFolders_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_userEditSubscriptionFolders(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userUnsubscribe(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_H_ffi_userUnsubscribe_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_userUnsubscribe(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userRetrySubscription(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_H_ffi_userRetrySubscription_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_userRetrySubscription(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_deleteFilter(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_H_ffi_deleteFilter_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_deleteFilter(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_deleteSmartStream(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_H_ffi_deleteSmartStream_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_deleteSmartStream(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_checkQuerySyntax(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_checkQuerySyntax_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_checkQuerySyntax(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_addFilter(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_H_ffi_addFilter_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_addFilter(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_editFilter(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4, HsPtr a5);
uw_Basis_string uw_H_ffi_editFilter_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4, uw_Basis_string x5)
{
    long size;
    char* cr = uw_HsFFI_editFilter(ctx, &size, x1, x2, x3, x4, x5);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_addSmartStream(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_H_ffi_addSmartStream_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_addSmartStream(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_editSmartStream(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_H_ffi_editSmartStream_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_editSmartStream(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userOPML(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_H_ffi_userOPML_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_userOPML(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_opmlSubscriptions(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_H_ffi_opmlSubscriptions_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_opmlSubscriptions(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_subscriptionsAndRenames(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4, HsPtr a5, HsPtr a6, HsPtr a7, HsPtr a8);
uw_Basis_string uw_H_ffi_subscriptionsAndRenames_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4, uw_Basis_string x5, uw_Basis_string x6, uw_Basis_string x7, uw_Basis_string x8)
{
    long size;
    char* cr = uw_HsFFI_subscriptionsAndRenames(ctx, &size, x1, x2, x3, x4, x5, x6, x7, x8);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_subscriptionsAndSettings(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_H_ffi_subscriptionsAndSettings_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_subscriptionsAndSettings(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_orderNotification(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_orderNotification_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_orderNotification(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_orderNotificationNew(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_H_ffi_orderNotificationNew_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_orderNotificationNew(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_checkOrder(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_checkOrder_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_checkOrder(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_getPaidTill(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_getPaidTill_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_getPaidTill(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_getUserAccountTypeAndRenewalUrl(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_getUserAccountTypeAndRenewalUrl_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_getUserAccountTypeAndRenewalUrl(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_getFeedDetails(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_H_ffi_getFeedDetails_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_getFeedDetails(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_performBgActions(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_H_ffi_performBgActions_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_performBgActions(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_tagsForest(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_H_ffi_tagsForest_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_tagsForest(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_folderForest(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4, HsPtr a5, HsPtr a6, HsPtr a7);
uw_Basis_string uw_H_ffi_folderForest_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4, uw_Basis_string x5, uw_Basis_string x6, uw_Basis_string x7)
{
    long size;
    char* cr = uw_HsFFI_folderForest(ctx, &size, x1, x2, x3, x4, x5, x6, x7);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_getTree(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_H_ffi_getTree_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_getTree(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_filterForest(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4, HsPtr a5, HsPtr a6, HsPtr a7, HsPtr a8);
uw_Basis_string uw_H_ffi_filterForest_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4, uw_Basis_string x5, uw_Basis_string x6, uw_Basis_string x7, uw_Basis_string x8)
{
    long size;
    char* cr = uw_HsFFI_filterForest(ctx, &size, x1, x2, x3, x4, x5, x6, x7, x8);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_filterTagsForest(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4, HsPtr a5, HsPtr a6);
uw_Basis_string uw_H_ffi_filterTagsForest_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4, uw_Basis_string x5, uw_Basis_string x6)
{
    long size;
    char* cr = uw_HsFFI_filterTagsForest(ctx, &size, x1, x2, x3, x4, x5, x6);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_smartStreamForest(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4, HsPtr a5);
uw_Basis_string uw_H_ffi_smartStreamForest_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4, uw_Basis_string x5)
{
    long size;
    char* cr = uw_HsFFI_smartStreamForest(ctx, &size, x1, x2, x3, x4, x5);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_markReqReadCounters(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_H_ffi_markReqReadCounters_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_markReqReadCounters(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_pageFromFile(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_pageFromFile_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_pageFromFile(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_addWebpackScripts(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_addWebpackScripts_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_addWebpackScripts(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_webpackStyles(HsPtr ctx, HsPtr pLen);
uw_Basis_string uw_H_ffi_webpackStyles_(uw_context ctx)
{
    long size;
    char* cr = uw_HsFFI_webpackStyles(ctx, &size);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_blessId(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_blessId_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_blessId(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_parseQueryStringUtf8Only(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_parseQueryStringUtf8Only_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_parseQueryStringUtf8Only(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userEmail(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_userEmail_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_userEmail(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_buyPage(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_H_ffi_buyPage_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_buyPage(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_invoiceLink(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_invoiceLink_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_invoiceLink(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_prettyUID(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_prettyUID_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_prettyUID(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_xbodyStringToString(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_xbodyStringToString_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_xbodyStringToString(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_xbodyStringToXbody(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_xbodyStringToXbody_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_xbodyStringToXbody(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_escapeXbody(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_escapeXbody_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_escapeXbody(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_hyphenatePage(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_hyphenatePage_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_hyphenatePage(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_hyphenateXbody(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_hyphenateXbody_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_hyphenateXbody(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_toLowerCase(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_toLowerCase_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_toLowerCase(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_addTwitterScreenName(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_H_ffi_addTwitterScreenName_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_addTwitterScreenName(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_newSessionJunk(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_H_ffi_newSessionJunk_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_newSessionJunk(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_getUserByLogin(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_H_ffi_getUserByLogin_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_getUserByLogin(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_clearSession(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_H_ffi_clearSession_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_clearSession(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userEvent(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_H_ffi_userEvent_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_userEvent(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_runTasks(HsPtr ctx, HsPtr pLen);
uw_Basis_string uw_H_ffi_runTasks_(uw_context ctx)
{
    long size;
    char* cr = uw_HsFFI_runTasks(ctx, &size);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_runApiServer(HsPtr ctx, HsPtr pLen);
uw_Basis_string uw_H_ffi_runApiServer_(uw_context ctx)
{
    long size;
    char* cr = uw_HsFFI_runApiServer(ctx, &size);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_reloadBrowserPage(HsPtr ctx, HsPtr pLen);
uw_Basis_string uw_H_ffi_reloadBrowserPage_(uw_context ctx)
{
    long size;
    char* cr = uw_HsFFI_reloadBrowserPage(ctx, &size);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_logOutAllSessions(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_H_ffi_logOutAllSessions_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_logOutAllSessions(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_getFullText(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4, HsPtr a5);
uw_Basis_string uw_H_ffi_getFullText_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4, uw_Basis_string x5)
{
    long size;
    char* cr = uw_HsFFI_getFullText(ctx, &size, x1, x2, x3, x4, x5);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_getUrTime(HsPtr ctx, HsPtr pLen);
uw_Basis_string uw_H_ffi_getUrTime_(uw_context ctx)
{
    long size;
    char* cr = uw_HsFFI_getUrTime(ctx, &size);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_setUsername(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_H_ffi_setUsername_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_setUsername(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_setPassword(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_H_ffi_setPassword_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_setPassword(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_tryRemoveAssociatedAccount(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_H_ffi_tryRemoveAssociatedAccount_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_tryRemoveAssociatedAccount(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_tryAddAssociatedAccount(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_H_ffi_tryAddAssociatedAccount_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_tryAddAssociatedAccount(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_tryGetFeverUser(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_tryGetFeverUser_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_tryGetFeverUser(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_enablePublicFeed(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_H_ffi_enablePublicFeed_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_enablePublicFeed(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_disablePublicFeed(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_H_ffi_disablePublicFeed_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_disablePublicFeed(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_generateNewPublicFeed(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_H_ffi_generateNewPublicFeed_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_generateNewPublicFeed(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_discover(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_H_ffi_discover_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_discover(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_restoreSubscriptionsFromBackup(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_restoreSubscriptionsFromBackup_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_restoreSubscriptionsFromBackup(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_isUserExists(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_isUserExists_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_isUserExists(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_deleteAccount(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_H_ffi_deleteAccount_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_deleteAccount(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_recordWebUsage(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_H_ffi_recordWebUsage_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_recordWebUsage(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_readMsgAndApplyFixes(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_H_ffi_readMsgAndApplyFixes_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_readMsgAndApplyFixes(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_parseRenewalUserId(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_parseRenewalUserId_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_parseRenewalUserId(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_passwordResetEmail(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_passwordResetEmail_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_passwordResetEmail(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_sendSignUpEmail(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_H_ffi_sendSignUpEmail_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_sendSignUpEmail(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_sendPasswordResetEmail(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_H_ffi_sendPasswordResetEmail_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_sendPasswordResetEmail(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_sendChangeEmailEmail(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_H_ffi_sendChangeEmailEmail_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_sendChangeEmailEmail(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_verifySignUpToken(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_verifySignUpToken_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_verifySignUpToken(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_verifyPasswordResetToken(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_H_ffi_verifyPasswordResetToken_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_verifyPasswordResetToken(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_verifyChangeEmailToken(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_H_ffi_verifyChangeEmailToken_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_verifyChangeEmailToken(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_verifyRestoreAccessToken(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_H_ffi_verifyRestoreAccessToken_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_verifyRestoreAccessToken(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_validateEmail(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_validateEmail_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_validateEmail(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_maskEmail(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_maskEmail_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_maskEmail(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userAddToPocket(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4, HsPtr a5);
uw_Basis_string uw_H_ffi_userAddToPocket_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4, uw_Basis_string x5)
{
    long size;
    char* cr = uw_HsFFI_userAddToPocket(ctx, &size, x1, x2, x3, x4, x5);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userAuthorizeAndAddToPocket(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_userAuthorizeAndAddToPocket_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_userAuthorizeAndAddToPocket(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_logT(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_logT_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_logT(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_findUsersLeft(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_findUsersLeft_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_findUsersLeft(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_updateFeedbackUserInfo(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_H_ffi_updateFeedbackUserInfo_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_updateFeedbackUserInfo(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

