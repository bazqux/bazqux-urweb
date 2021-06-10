void uw_Hacks_set_script_header(uw_context ctx, const char *s) {
    uw_Basis_int sz = strlen(s);
    if (sz)
    {
        uw_Basis_string ss = uw_malloc(ctx, sz + 16);
        (*(uw_Basis_int*)ss) = sz + 8;
        ASSIGN_INT64_BE(ss + 8, sz);
        memcpy(ss + 16, s, sz);
        uw_Basis_string r = uw_H_ffi_addWebpackScripts_(ctx, ss+8);
        uw_Basis_int rsz = READ_INT64_BE(r);
        uw_Basis_string rs = uw_malloc(ctx, rsz + 1);
        memcpy(rs, r + 8, rsz);
        rs[rsz] = '\0';
        uw_set_script_header(ctx, rs);
    }
    else
    {
        uw_set_script_header(ctx, "");
    }
}

uw_unit uw_Hacks_clear_script_header(uw_context ctx)
{
    uw_set_script_header(ctx, "");
    return 0;
}

const uw_Basis_postBody uw_Hacks_dummyPostBody = {0};

char *uw_Basis_urlifyPostBody(uw_context ctx, uw_Basis_postBody pb) {
    return "";
}

char *uw_Unsafe_toXml(uw_context ctx, uw_Basis_string s) {
    return s;
}

char *uw_Js_show_css_class(uw_context ctx, uw_Basis_string s) {
    return s;
}
