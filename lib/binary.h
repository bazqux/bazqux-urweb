#include <urweb.h>
#include <string.h>
#include <assert.h>

typedef struct uw_Binary_ffi_putBuf
{
        int totalSize;
        int chunkSize;
        void* chunk;
        struct uw_Binary_ffi_putBuf* prev;
} *uw_Binary_ffi_putBuf;

typedef char* uw_Binary_ffi_getBuf;

uw_Binary_ffi_putBuf uw_Binary_ffi_mkPutBuf(uw_context ctx, uw_unit _)
{
    uw_Binary_ffi_putBuf pb = uw_malloc(ctx, sizeof (struct uw_Binary_ffi_putBuf));
    pb->totalSize = pb->chunkSize = 0;
    pb->chunk = pb->prev = NULL;
    return pb;
}

uw_Binary_ffi_putBuf uw_Binary_ffi_mkPutBufP(uw_context ctx, uw_Binary_ffi_putBuf prev)
{
    uw_Binary_ffi_putBuf pb = uw_malloc(ctx, sizeof (struct uw_Binary_ffi_putBuf));
    pb->totalSize = prev->totalSize;
    pb->chunkSize = 0;
    pb->chunk = NULL;
    pb->prev = prev;
    return pb;
}

uw_Basis_string uw_Binary_ffi_putBufString (uw_context ctx, uw_Binary_ffi_putBuf pb)
{
    uw_Basis_int totalSize = pb->totalSize;
    uw_Basis_string result = uw_malloc(ctx, totalSize + 9);
    uw_Basis_string r = result + totalSize + 8;
    uw_Basis_int i;
    *r = 0;
    while (pb)
    {
        r -= pb->chunkSize;
        memcpy(r, pb->chunk, pb->chunkSize);
        pb = pb->prev;
    }
    assert(r - 8 == result);

/*     result = uw_malloc(ctx, totalSize*3+1); */
/*     uw_Basis_string dst = result; */

/*     for ( i = 0; i < totalSize; i++ ) */
/*     { */
/*         unsigned char c = *r++; */
/*         *dst++ = "0123456789ABCDEF"[c >> 4]; */
/*         *dst++ = "0123456789ABCDEF"[c & 0x0F]; */
/*         *dst++ = '.'; */
/*     } */
/*     *dst = 0; */

    *(uw_Basis_int*)(r-8) = totalSize;
    return r;
}
uw_Binary_ffi_putBuf uw_Binary_ffi_put_char   (uw_context ctx, uw_Binary_ffi_putBuf prev, uw_Basis_char c)
{
    uw_Binary_ffi_putBuf pb = uw_Binary_ffi_mkPutBufP(ctx, prev);
    pb->chunkSize = 1; pb->totalSize += pb->chunkSize;
    pb->chunk = uw_malloc(ctx,1);
    *((uw_Basis_char*)pb->chunk) = c;
    return pb;
}
#define ASSIGN_INT64_BE(_d, _i)                                         \
    ((unsigned char*)_d)[0] = (((unsigned long long)_i) >> 56) & 0xFF;  \
    ((unsigned char*)_d)[1] = (((unsigned long long)_i) >> 48) & 0xFF;  \
    ((unsigned char*)_d)[2] = (((unsigned long long)_i) >> 40) & 0xFF;  \
    ((unsigned char*)_d)[3] = (((unsigned long long)_i) >> 32) & 0xFF;  \
    ((unsigned char*)_d)[4] = (((unsigned long long)_i) >> 24) & 0xFF;  \
    ((unsigned char*)_d)[5] = (((unsigned long long)_i) >> 16) & 0xFF;  \
    ((unsigned char*)_d)[6] = (((unsigned long long)_i) >>  8) & 0xFF;  \
    ((unsigned char*)_d)[7] = (((unsigned long long)_i) >>  0) & 0xFF
uw_Binary_ffi_putBuf uw_Binary_ffi_put_int    (uw_context ctx, uw_Binary_ffi_putBuf prev, uw_Basis_int i)
{
    uw_Binary_ffi_putBuf pb = uw_Binary_ffi_mkPutBufP(ctx, prev);
    pb->chunkSize = 8; pb->totalSize += pb->chunkSize;
    pb->chunk = uw_malloc(ctx,8);
    ASSIGN_INT64_BE(pb->chunk, i);
    return pb;
}
uw_Binary_ffi_putBuf uw_Binary_ffi_put_time   (uw_context ctx, uw_Binary_ffi_putBuf prev, uw_Basis_time t)
{
    uw_Binary_ffi_putBuf pb = uw_Binary_ffi_mkPutBufP(ctx, prev);
    pb->chunkSize = 16; pb->totalSize += pb->chunkSize;
    pb->chunk = uw_malloc(ctx,16);
    ASSIGN_INT64_BE(pb->chunk, t.seconds);
    ASSIGN_INT64_BE(((char*)pb->chunk + 8), t.microseconds);
    return pb;
}
uw_Binary_ffi_putBuf uw_Binary_ffi_put_string (uw_context ctx, uw_Binary_ffi_putBuf prev, uw_Basis_string str)
{
    uw_Basis_int len = uw_Basis_strlen(ctx, str);
    uw_Binary_ffi_putBuf pb = uw_Binary_ffi_mkPutBufP(ctx, uw_Binary_ffi_put_int(ctx, prev, len));
    pb->chunkSize = len; pb->totalSize += pb->chunkSize;
    pb->chunk = str;
    return pb;
}
uw_Binary_ffi_putBuf uw_Binary_ffi_put_blob (uw_context ctx, uw_Binary_ffi_putBuf prev, uw_Basis_blob blob)
{
    uw_Basis_int len = (uw_Basis_int)blob.size;
    uw_Binary_ffi_putBuf pb = uw_Binary_ffi_mkPutBufP(ctx, uw_Binary_ffi_put_int(ctx, prev, len));
    pb->chunkSize = len; pb->totalSize += pb->chunkSize;
    pb->chunk = blob.data;
    return pb;
}
uw_Binary_ffi_putBuf uw_Binary_ffi_put_url (uw_context ctx, uw_Binary_ffi_putBuf prev, uw_Basis_string str)
{
    return uw_Binary_ffi_put_string(ctx, prev, str);
}

uw_Binary_ffi_getBuf uw_Binary_ffi_mkGetBuf(uw_context ctx, uw_Basis_string str)
{
    return str;
}
uw_Binary_ffi_getBuf uw_Binary_ffi_advanceGetBuf(uw_context ctx, uw_Binary_ffi_getBuf str, uw_Basis_int n)
{
    return str + n;
}
uw_Basis_char uw_Binary_ffi_get_char_   (uw_context ctx, uw_Binary_ffi_getBuf buf)
{
    return *((uw_Basis_char*)buf);
}

#define READ_INT64_BE(_s) (uw_Basis_int)(                       \
        ((unsigned long long)(((unsigned char*)_s)[0]) << 56) + \
        ((unsigned long long)(((unsigned char*)_s)[1]) << 48) + \
        ((unsigned long long)(((unsigned char*)_s)[2]) << 40) + \
        ((unsigned long long)(((unsigned char*)_s)[3]) << 32) + \
        ((unsigned long long)(((unsigned char*)_s)[4]) << 24) + \
        ((unsigned long long)(((unsigned char*)_s)[5]) << 16) + \
        ((unsigned long long)(((unsigned char*)_s)[6]) <<  8) + \
        ((unsigned long long)(((unsigned char*)_s)[7]) <<  0))

uw_Basis_int uw_Binary_ffi_get_int_    (uw_context ctx, uw_Binary_ffi_getBuf buf)
{
    return READ_INT64_BE(buf);
}

uw_Basis_time uw_Binary_ffi_get_time_   (uw_context ctx, uw_Binary_ffi_getBuf buf)
{
    uw_Basis_time r = { READ_INT64_BE(buf), READ_INT64_BE(((char*)buf+8)) };
    return r;
}
uw_Basis_string uw_Binary_ffi_get_string_ (uw_context ctx, uw_Binary_ffi_getBuf buf)
{
    uw_Basis_int len = uw_Binary_ffi_get_int_(ctx, buf);
    uw_Basis_string r = uw_malloc(ctx, len+1);
    memcpy(r, buf+8, len);
    r[len] = 0;
    return r;
}
uw_Basis_blob uw_Binary_ffi_get_blob_ (uw_context ctx, uw_Binary_ffi_getBuf buf)
{
    uw_Basis_int len = uw_Binary_ffi_get_int_(ctx, buf);
    uw_Basis_blob r;
    r.size = len;
    r.data = buf+8;
    return r;
}
#define uw_Binary_ffi_xhead uw_Basis_string
#define uw_Binary_ffi_get_xhead_ uw_Binary_ffi_get_string_
#define uw_Binary_ffi_put_xhead uw_Binary_ffi_put_string
#define uw_Binary_ffi_get_xbody_ uw_Binary_ffi_get_string_
#define uw_Binary_ffi_put_xbody uw_Binary_ffi_put_string
#define uw_Binary_ffi_get_page_ uw_Binary_ffi_get_string_
#define uw_Binary_ffi_put_page uw_Binary_ffi_put_string
#define uw_Binary_ffi_get_id_ uw_Binary_ffi_get_string_
#define uw_Binary_ffi_put_id uw_Binary_ffi_put_string

uw_Basis_string uw_Binary_ffi_get_url_ (uw_context ctx, uw_Binary_ffi_getBuf buf)
{
    return uw_Binary_ffi_get_string_(ctx, buf);
}
