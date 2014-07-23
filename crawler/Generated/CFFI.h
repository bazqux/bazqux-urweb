#include <urweb.h>
#include "HsFFI.h"

#include "Rts.h"

uw_unit uw_Ur_ffi_init(uw_context ctx) { 
static int argc = 7;
static char* arg0 = "/tmp/coreader.exe";
static char* argv[20]; argv[0]=arg0; int i; for(i=1; i<20;i++)argv[i]=NULL; 
argv[1] = "+RTS";
argv[2] = "-N6";
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
uw_Basis_string uw_Ur_ffi_readUserSettings_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_cachedReadUserSettings_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_cachedNothingReadUserSettings_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_mergeWriteUserSettings_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_deleteUserSettings_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_readManyUserSettingss_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_cachedReadManyUserSettingss_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_cachedNothingReadManyUserSettingss_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_writeManyUserSettingss_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_readSession_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_cachedReadSession_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_cachedNothingReadSession_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_mergeWriteSession_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_deleteSession_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_readManySessions_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_cachedReadManySessions_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_cachedNothingReadManySessions_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_writeManySessions_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_readMsg_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_cachedReadMsg_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_cachedNothingReadMsg_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_mergeWriteMsg_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_deleteMsg_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_readManyMsgs_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_cachedReadManyMsgs_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_cachedNothingReadManyMsgs_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_writeManyMsgs_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_loginGetForwardUrl_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
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
uw_Basis_string uw_Ur_ffi_loginCallback_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_loginCallback(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_fbTokenGetForwardUrl(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_Ur_ffi_fbTokenGetForwardUrl_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_fbTokenGetForwardUrl(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_fbTokenCallback(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_Ur_ffi_fbTokenCallback_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_fbTokenCallback(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_importFromGoogleReaderGetForwardUrl(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_Ur_ffi_importFromGoogleReaderGetForwardUrl_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_importFromGoogleReaderGetForwardUrl(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_importFromGoogleReaderCallback(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_Ur_ffi_importFromGoogleReaderCallback_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_importFromGoogleReaderCallback(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_importStarredAndTaggedItemsFromGoogleReaderCallback(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_Ur_ffi_importStarredAndTaggedItemsFromGoogleReaderCallback_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_importStarredAndTaggedItemsFromGoogleReaderCallback(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userSubscribe(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_Ur_ffi_userSubscribe_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
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
uw_Basis_string uw_Ur_ffi_userDiscoverySubscribe_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4, uw_Basis_string x5, uw_Basis_string x6)
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
uw_Basis_string uw_Ur_ffi_userRenameSubscription_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
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
uw_Basis_string uw_Ur_ffi_userRenameFolder_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
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
uw_Basis_string uw_Ur_ffi_userEditSubscriptionFolders_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
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
uw_Basis_string uw_Ur_ffi_userUnsubscribe_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
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
uw_Basis_string uw_Ur_ffi_userRetrySubscription_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_userRetrySubscription(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userDeleteFilter(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_Ur_ffi_userDeleteFilter_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_userDeleteFilter(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userDeleteSmartStream(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_Ur_ffi_userDeleteSmartStream_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_userDeleteSmartStream(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userAddFilter(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_Ur_ffi_userAddFilter_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_userAddFilter(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userEditFilter(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4, HsPtr a5, HsPtr a6);
uw_Basis_string uw_Ur_ffi_userEditFilter_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4, uw_Basis_string x5, uw_Basis_string x6)
{
    long size;
    char* cr = uw_HsFFI_userEditFilter(ctx, &size, x1, x2, x3, x4, x5, x6);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userAddSmartStream(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_Ur_ffi_userAddSmartStream_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_userAddSmartStream(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userEditSmartStream(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_Ur_ffi_userEditSmartStream_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_userEditSmartStream(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userOPML(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_Ur_ffi_userOPML_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
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
uw_Basis_string uw_Ur_ffi_opmlSubscriptions_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_opmlSubscriptions(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userSubscriptionsAndRenames(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4, HsPtr a5, HsPtr a6);
uw_Basis_string uw_Ur_ffi_userSubscriptionsAndRenames_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4, uw_Basis_string x5, uw_Basis_string x6)
{
    long size;
    char* cr = uw_HsFFI_userSubscriptionsAndRenames(ctx, &size, x1, x2, x3, x4, x5, x6);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userSubscriptionsAndSettings(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_Ur_ffi_userSubscriptionsAndSettings_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_userSubscriptionsAndSettings(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userGetFiltersAndSmartStreams(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_userGetFiltersAndSmartStreams_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_userGetFiltersAndSmartStreams(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_orderNotification(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_orderNotification_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_orderNotification(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_checkOrder(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_checkOrder_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_getPaidTill_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_getPaidTill(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_activeGRImportsCount(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_activeGRImportsCount_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_activeGRImportsCount(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_activeGRImportNames(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_activeGRImportNames_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_activeGRImportNames(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_getFeedDetails(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_Ur_ffi_getFeedDetails_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_getFeedDetails(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_tagsMsgForest(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_Ur_ffi_tagsMsgForest_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_tagsMsgForest(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_folderMsgForest(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4, HsPtr a5);
uw_Basis_string uw_Ur_ffi_folderMsgForest_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4, uw_Basis_string x5)
{
    long size;
    char* cr = uw_HsFFI_folderMsgForest(ctx, &size, x1, x2, x3, x4, x5);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userGetTree(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_Ur_ffi_userGetTree_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_userGetTree(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_performBgActions(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_Ur_ffi_performBgActions_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_performBgActions(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_filterMsgForest(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4, HsPtr a5);
uw_Basis_string uw_Ur_ffi_filterMsgForest_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4, uw_Basis_string x5)
{
    long size;
    char* cr = uw_HsFFI_filterMsgForest(ctx, &size, x1, x2, x3, x4, x5);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_filterTagsMsgForest(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_Ur_ffi_filterTagsMsgForest_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_filterTagsMsgForest(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_smartStreamMsgForest(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4, HsPtr a5);
uw_Basis_string uw_Ur_ffi_smartStreamMsgForest_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4, uw_Basis_string x5)
{
    long size;
    char* cr = uw_HsFFI_smartStreamMsgForest(ctx, &size, x1, x2, x3, x4, x5);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_htmlHead(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_htmlHead_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_htmlHead(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_htmlHeadMain(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_htmlHeadMain_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_htmlHeadMain(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_htmlHeadMainNoTranslate(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_htmlHeadMainNoTranslate_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_htmlHeadMainNoTranslate(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_htmlLikeButtons(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_htmlLikeButtons_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_htmlLikeButtons(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_htmlLandingScripts(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_htmlLandingScripts_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_htmlLandingScripts(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_htmlOpenIdSignInButton(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_htmlOpenIdSignInButton_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_htmlOpenIdSignInButton(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_htmlConversionLogin(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_htmlConversionLogin_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_htmlConversionLogin(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_version(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_version_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_version(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_blessId(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_blessId_(uw_context ctx, uw_Basis_string x1)
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
uw_Basis_string uw_Ur_ffi_parseQueryStringUtf8Only_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_parseQueryStringUtf8Only(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_buyLink(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_Ur_ffi_buyLink_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_buyLink(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_encodeURIComponent(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_encodeURIComponent_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_encodeURIComponent(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_prettyUID(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_prettyUID_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_prettyUID(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_textToXbody(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_textToXbody_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_textToXbody(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_newSession(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_Ur_ffi_newSession_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_newSession(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_getUserByMobileLogin(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_Ur_ffi_getUserByMobileLogin_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_getUserByMobileLogin(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_clearSession(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_clearSession_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_clearSession(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userEvent(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_Ur_ffi_userEvent_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_userEvent(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_initMailer(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_initMailer_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_initMailer(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_initApiServer(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_initApiServer_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_initApiServer(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_readFullTextCache(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_readFullTextCache_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_readFullTextCache(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_cachedReadFullTextCache(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_cachedReadFullTextCache_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_cachedReadFullTextCache(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_cachedNothingReadFullTextCache(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_cachedNothingReadFullTextCache_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_cachedNothingReadFullTextCache(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_mergeWriteFullTextCache(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_mergeWriteFullTextCache_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_mergeWriteFullTextCache(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_deleteFullTextCache(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_deleteFullTextCache_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_deleteFullTextCache(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_readManyFullTextCaches(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_readManyFullTextCaches_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_readManyFullTextCaches(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_cachedReadManyFullTextCaches(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_cachedReadManyFullTextCaches_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_cachedReadManyFullTextCaches(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_cachedNothingReadManyFullTextCaches(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_cachedNothingReadManyFullTextCaches_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_cachedNothingReadManyFullTextCaches(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_writeManyFullTextCaches(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_writeManyFullTextCaches_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_writeManyFullTextCaches(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userGetFullText(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_Ur_ffi_userGetFullText_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_userGetFullText(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_getUrTime_(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_getUrTime__(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_getUrTime_(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_setMobileLogin(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
uw_Basis_string uw_Ur_ffi_setMobileLogin_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4)
{
    long size;
    char* cr = uw_HsFFI_setMobileLogin(ctx, &size, x1, x2, x3, x4);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_tryGetFeverUser(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_tryGetFeverUser_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_tryGetFeverUser(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userEnablePublicFeed(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_Ur_ffi_userEnablePublicFeed_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_userEnablePublicFeed(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userDisablePublicFeed(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_Ur_ffi_userDisablePublicFeed_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_userDisablePublicFeed(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userGenerateNewPublicFeed(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_Ur_ffi_userGenerateNewPublicFeed_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_userGenerateNewPublicFeed(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userSearchSubscriptions(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3);
uw_Basis_string uw_Ur_ffi_userSearchSubscriptions_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3)
{
    long size;
    char* cr = uw_HsFFI_userSearchSubscriptions(ctx, &size, x1, x2, x3);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userRestoreSubscriptionsFromBackup(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_userRestoreSubscriptionsFromBackup_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_userRestoreSubscriptionsFromBackup(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_isUserExists(HsPtr ctx, HsPtr pLen, HsPtr a1);
uw_Basis_string uw_Ur_ffi_isUserExists_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_isUserExists(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userDeleteAccount(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_Ur_ffi_userDeleteAccount_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_userDeleteAccount(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_recordWebUsage(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2);
uw_Basis_string uw_Ur_ffi_recordWebUsage_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2)
{
    long size;
    char* cr = uw_HsFFI_recordWebUsage(ctx, &size, x1, x2);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

extern HsPtr uw_HsFFI_userAddToPocket(HsPtr ctx, HsPtr pLen, HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4, HsPtr a5);
uw_Basis_string uw_Ur_ffi_userAddToPocket_(uw_context ctx, uw_Basis_string x1, uw_Basis_string x2, uw_Basis_string x3, uw_Basis_string x4, uw_Basis_string x5)
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
uw_Basis_string uw_Ur_ffi_userAuthorizeAndAddToPocket_(uw_context ctx, uw_Basis_string x1)
{
    long size;
    char* cr = uw_HsFFI_userAuthorizeAndAddToPocket(ctx, &size, x1);
    long sz = size >= 0 ? size : -size;
    uw_Basis_string r = uw_malloc(ctx, sz + 1);
    memcpy(r, cr, sz);
    r[sz] = '\0';
    if (size >= 0) return r; else uw_error(ctx, FATAL, r);

}

