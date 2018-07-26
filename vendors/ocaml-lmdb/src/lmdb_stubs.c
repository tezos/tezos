/* --------------------------------------------------------------------------
   Copyright (c) 2018 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  --------------------------------------------------------------------------- */

#include <string.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/bigarray.h>

#include "lmdb.h"

CAMLprim value stub_mdb_version(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(result);

    int major, minor, patch;
    mdb_version(&major, &minor, &patch);
    result = caml_alloc_tuple(3);
    Store_field(result, 0, Val_int(major));
    Store_field(result, 1, Val_int(minor));
    Store_field(result, 2, Val_int(patch));

    CAMLreturn(result);
}

CAMLprim value stub_mdb_strerror(value errno) {
    CAMLparam1(errno);
    CAMLlocal1(result);

    char *errstr;
    errstr = mdb_strerror(Int_val(errno));
    result = caml_copy_string(errstr);

    CAMLreturn(result);
}

#define Env_val(v) (*((MDB_env **) Data_custom_val(v)))
#define Txn_val(v) (*((MDB_txn **) Data_custom_val(v)))
#define Cursor_val(v) (*((MDB_cursor **) Data_custom_val(v)))

#define Gen_custom_block(SNAME, CNAME, MNAME)                           \
    static int compare_##SNAME(value a, value b) {                      \
        CNAME *aa = MNAME(a), *bb = MNAME(b);                           \
        return (aa == bb ? 0 : (aa < bb ? -1 : 1));                     \
    }                                                                   \
                                                                        \
    static struct custom_operations lmdb_##SNAME##_ops = {              \
        .identifier = "lmdb_" #SNAME,                                   \
        .finalize = custom_finalize_default,                            \
        .compare = compare_##SNAME,                                     \
        .compare_ext = custom_compare_ext_default,                      \
        .hash = custom_hash_default,                                    \
        .serialize = custom_serialize_default,                          \
        .deserialize = custom_deserialize_default                       \
    };                                                                  \
                                                                        \
    static value alloc_##SNAME (CNAME *a) {                             \
        value custom = alloc_custom(&lmdb_##SNAME##_ops, sizeof(CNAME *), 0, 1); \
        MNAME(custom) = a;                                              \
        return custom;                                                  \
    }

Gen_custom_block(env, MDB_env, Env_val)
Gen_custom_block(txn, MDB_txn, Txn_val)
Gen_custom_block(cursor, MDB_cursor, Cursor_val)

CAMLprim value stub_mdb_env_create(value unit) {
    CAMLparam1(unit);
    CAMLlocal2(result, ml_env);

    int ret;
    MDB_env *env;

    ret = mdb_env_create(&env);
    if (ret) {
        result = caml_alloc(1, 1);
        Store_field(result, 0, Val_int(ret));
    }
    else {
        result = caml_alloc(1, 0);
        ml_env = alloc_env(env);
        Store_field(result, 0, ml_env);
    }

    CAMLreturn(result);
}

CAMLprim value stub_mdb_env_open(value env, value path, value flags, value mode) {
    return Val_int(mdb_env_open(Env_val(env), String_val(path), Int_val(flags), Int_val(mode)));
}

CAMLprim value stub_mdb_env_close(value env) {
    mdb_env_close(Env_val(env));
    return Val_unit;
}

CAMLprim value stub_mdb_env_copy2(value env, value path, value flags) {
    return Val_int(mdb_env_copy2(Env_val(env), String_val(path), Int_val(flags)));
}

CAMLprim value stub_mdb_env_copyfd2(value env, value fd, value flags) {
    return Val_int(mdb_env_copyfd2(Env_val(env), Int_val(fd), Int_val(flags)));
}

static void caml_mdb_stat(value result, const MDB_stat *stat) {
    Store_field(result, 0, Val_int(stat->ms_psize));
    Store_field(result, 1, Val_int(stat->ms_depth));
    Store_field(result, 2, Val_long(stat->ms_branch_pages));
    Store_field(result, 3, Val_long(stat->ms_leaf_pages));
    Store_field(result, 4, Val_long(stat->ms_overflow_pages));
    Store_field(result, 5, Val_long(stat->ms_entries));
}

CAMLprim value stub_mdb_env_stat(value env) {
    CAMLparam1(env);
    CAMLlocal1(result);

    MDB_stat stat;
    mdb_env_stat(Env_val(env), &stat);
    result = caml_alloc_tuple(6);
    caml_mdb_stat(result, &stat);
    CAMLreturn(result);
}

CAMLprim value stub_mdb_env_info(value env) {
    CAMLparam1(env);
    CAMLlocal1(result);

    MDB_envinfo info;
    mdb_env_info(Env_val(env), &info);
    result = caml_alloc_tuple(5);

    Store_field(result, 0, Val_long(info.me_mapsize));
    Store_field(result, 1, Val_long(info.me_last_pgno));
    Store_field(result, 2, Val_long(info.me_last_txnid));
    Store_field(result, 3, Val_int(info.me_maxreaders));
    Store_field(result, 4, Val_int(info.me_numreaders));

    CAMLreturn(result);
}

CAMLprim value stub_mdb_env_sync(value env, value force) {
    return Val_int(mdb_env_sync(Env_val(env), Bool_val(force)));
}

CAMLprim value stub_mdb_env_set_flags(value env, value flags, value onoff) {
    return Val_int(mdb_env_set_flags(Env_val(env), Int_val(flags), Bool_val(onoff)));
}

CAMLprim value stub_mdb_env_get_flags(value env) {
    unsigned int flags;
    mdb_env_get_flags(Env_val(env), &flags);
    return Val_int(flags);
}

CAMLprim value stub_mdb_env_get_path(value env) {
    CAMLparam1(env);
    CAMLlocal1(result);

    const char *path;
    mdb_env_get_path(Env_val(env), &path);
    result = caml_copy_string(path);

    CAMLreturn(result);
}

CAMLprim value stub_mdb_env_get_fd(value env) {
    mdb_filehandle_t fd;
    mdb_env_get_fd(Env_val(env), &fd);
    return Val_int(fd);
}

CAMLprim value stub_mdb_env_set_mapsize(value env, value size) {
    return Val_int(mdb_env_set_mapsize(Env_val(env), Int64_val(size)));
}

CAMLprim value stub_mdb_env_set_maxreaders(value env, value readers) {
    return Val_int(mdb_env_set_maxreaders(Env_val(env), Int_val(readers)));
}

CAMLprim value stub_mdb_env_get_maxreaders(value env) {
    unsigned int readers;
    mdb_env_get_maxreaders(Env_val(env), &readers);
    return Val_int(readers);
}

CAMLprim value stub_mdb_env_set_maxdbs(value env, value dbs) {
    return Val_int(mdb_env_set_maxdbs(Env_val(env), Int_val(dbs)));
}

CAMLprim value stub_mdb_env_get_maxkeysize(value env) {
    return Val_int(mdb_env_get_maxkeysize(Env_val(env)));
}

CAMLprim value stub_mdb_txn_begin(value env, value flags, value parent) {
    CAMLparam3(env, flags, parent);
    CAMLlocal2(result, ml_txn);

    int ret;
    MDB_txn *parent_txn = Is_block(parent) ? Txn_val(Field(parent, 0)) : NULL;
    MDB_txn *new_txn;

    ret = mdb_txn_begin(Env_val(env), parent_txn, Int_val(flags), &new_txn);

    if (ret) {
        result = caml_alloc(1, 1);
        Store_field(result, 0, Val_int(ret));
    }
    else {
        result = caml_alloc(1, 0);
        ml_txn = alloc_txn(new_txn);
        Store_field(result, 0, ml_txn);
    }

    CAMLreturn(result);
}

CAMLprim value stub_mdb_txn_env(value txn) {
    CAMLparam1(txn);
    CAMLlocal1(result);
    MDB_env *env = mdb_txn_env(Txn_val(txn));
    result = alloc_env(env);
    CAMLreturn(result);
}

CAMLprim value stub_mdb_txn_id(value txn) {
    return Val_long(mdb_txn_id(Txn_val(txn)));
}

CAMLprim value stub_mdb_txn_commit(value txn) {
    return Val_int(mdb_txn_commit(Txn_val(txn)));
}

CAMLprim value stub_mdb_txn_abort(value txn) {
    mdb_txn_abort(Txn_val(txn));
    return Val_unit;
}

CAMLprim value stub_mdb_txn_reset(value txn) {
    mdb_txn_reset(Txn_val(txn));
    return Val_unit;
}

CAMLprim value stub_mdb_txn_renew(value txn) {
    return Val_int(mdb_txn_renew(Txn_val(txn)));
}

CAMLprim value stub_mdb_dbi_open(value txn, value name, value flags) {
    CAMLparam3(txn, name, flags);
    CAMLlocal2(result, ml_dbi);

    MDB_dbi dbi;
    int ret;
    const char* db_name = NULL;

    if (Is_block(name)) db_name = String_val(Field(name, 0));

    ret = mdb_dbi_open(Txn_val(txn), db_name, Int_val(flags), &dbi);

    if (ret) {
        result = caml_alloc(1, 1);
        Store_field(result, 0, Val_int(ret));
    }
    else {
        result = caml_alloc(1, 0);
        ml_dbi = caml_copy_nativeint(dbi);
        Store_field(result, 0, ml_dbi);
    }

    CAMLreturn(result);
}

CAMLprim value stub_mdb_stat(value txn, value dbi) {
    CAMLparam2(txn, dbi);
    CAMLlocal2(result, tuple);

    MDB_stat stat;
    int ret;
    ret = mdb_stat(Txn_val(txn), Nativeint_val(dbi), &stat);

    if (ret) {
        result = caml_alloc(1, 1);
        Store_field(result, 0, Val_int(ret));
    }
    else {
        result = caml_alloc(1, 0);
        tuple = caml_alloc_tuple(6);
        caml_mdb_stat(tuple, &stat);
        Store_field(result, 0, tuple);
    }

    CAMLreturn(result);
}

CAMLprim value stub_mdb_dbi_flags(value txn, value dbi) {
    CAMLparam2(txn, dbi);
    CAMLlocal1(result);

    unsigned int flags;
    int ret;
    ret = mdb_dbi_flags(Txn_val(txn), Nativeint_val(dbi), &flags);

    if (ret) {
        result = caml_alloc(1, 1);
        Store_field(result, 0, Val_int(ret));
    }
    else {
        result = caml_alloc(1, 0);
        Store_field(result, 0, Val_int(flags));
    }

    CAMLreturn(result);
}

CAMLprim value stub_mdb_dbi_close(value env, value dbi) {
    mdb_dbi_close(Env_val(env), Nativeint_val(dbi));
    return Val_unit;
}

CAMLprim value stub_mdb_drop(value txn, value dbi, value del) {
    return Val_int(mdb_drop(Txn_val(txn), Nativeint_val(dbi), Bool_val(del)));
}

static inline value alloc_mdb_val_ba (MDB_val *v) {
    return
        (v ?
         caml_ba_alloc_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1, v->mv_data, v->mv_size) :
         caml_ba_alloc_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1, NULL, 0));
}

CAMLprim value stub_mdb_get(value txn, value dbi, value key) {
    CAMLparam3(txn, dbi, key);
    CAMLlocal1(result);

    MDB_val k, v;
    int ret;

    k.mv_size = caml_string_length(key);
    k.mv_data = String_val(key);

    ret = mdb_get(Txn_val(txn), Nativeint_val(dbi), &k, &v);
    if (ret) {
        result = caml_alloc(1, 1);
        Store_field(result, 0, Val_int(ret));
    }
    else {
        result = caml_alloc(1, 0);
        Store_field(result, 0, alloc_mdb_val_ba(&v));
    }

    CAMLreturn(result);
}

CAMLprim value stub_mdb_put(value txn, value dbi,
                            value key, value data, value flags) {
    MDB_val k, v;
    k.mv_size = caml_string_length(key);
    k.mv_data = String_val(key);
    v.mv_size = Caml_ba_array_val(data)->dim[0];
    v.mv_data = Caml_ba_data_val(data);
    return Val_int(mdb_put(Txn_val(txn), Nativeint_val(dbi), &k, &v, Int_val(flags)));
}

CAMLprim value stub_mdb_put_string(value txn, value dbi,
                                   value key, value data, value flags) {
    MDB_val k, v;
    k.mv_size = caml_string_length(key);
    k.mv_data = String_val(key);
    v.mv_size = caml_string_length(data);
    v.mv_data = String_val(data);
    return Val_int(mdb_put(Txn_val(txn), Nativeint_val(dbi), &k, &v, Int_val(flags)));
}

CAMLprim value stub_mdb_del(value txn, value dbi, value key, value data) {
    MDB_val k, v, *vp = NULL;
    k.mv_size = caml_string_length(key);
    k.mv_data = String_val(key);

    if (Is_block(data)) {
        v.mv_size = Caml_ba_array_val(Field(data, 0))->dim[0];
        v.mv_data = Caml_ba_data_val(Field(data, 0));
        vp = &v;
    }

    return Val_int(mdb_del(Txn_val(txn), Nativeint_val(dbi), &k, vp));
}

CAMLprim value stub_mdb_del_string(value txn, value dbi, value key, value data) {
    MDB_val k, v, *vp = NULL;
    k.mv_size = caml_string_length(key);
    k.mv_data = String_val(key);

    if (Is_block(data)) {
        v.mv_size = caml_string_length(Field(data, 0));
        v.mv_data = String_val(Field(data, 0));
        vp = &v;
    }

    return Val_int(mdb_del(Txn_val(txn), Nativeint_val(dbi), &k, vp));
}

CAMLprim value stub_mdb_cursor_open(value txn, value dbi) {
    CAMLparam2(txn, dbi);
    CAMLlocal2(result, ml_cursor);

    MDB_cursor *cursor;
    int ret;
    ret = mdb_cursor_open(Txn_val(txn), Nativeint_val(dbi), &cursor);

    if (ret) {
        result = caml_alloc(1, 1);
        Store_field(result, 0, Val_int(ret));
    }
    else {
        result = caml_alloc(1, 0);
        ml_cursor = alloc_cursor(cursor);
        Store_field(result, 0, ml_cursor);
    }

    CAMLreturn(result);
}

CAMLprim value stub_mdb_cursor_close(value cursor) {
    mdb_cursor_close(Cursor_val(cursor));
    return Val_unit;
}

CAMLprim value stub_mdb_cursor_renew(value txn, value cursor) {
    return Val_int(mdb_cursor_renew(Txn_val(txn), Cursor_val(cursor)));
}

CAMLprim value stub_mdb_cursor_txn(value cursor) {
    CAMLparam1(cursor);
    CAMLlocal1(txn);
    txn = alloc_txn(mdb_cursor_txn(Cursor_val(cursor)));
    CAMLreturn(txn);
}

CAMLprim value stub_mdb_cursor_dbi(value cursor) {
    return Val_int(mdb_cursor_dbi(Cursor_val(cursor)));
}

CAMLprim value stub_mdb_cursor_get(value cursor, value key, value data, value op) {
    CAMLparam4(cursor, key, data, op);
    CAMLlocal2(result, tuple);

    MDB_val k, v;
    int ret;

    if (Is_block(key)) {
        k.mv_size = caml_string_length(Field(key, 0));
        k.mv_data = String_val(Field(key, 0));
    }

    if (Is_block(data)) {
        v.mv_size = Caml_ba_array_val(Field(data, 0))->dim[0];
        v.mv_data = Caml_ba_data_val(Field(data, 0));
    }

    ret = mdb_cursor_get(Cursor_val(cursor), &k, &v, Int_val(op));
    if (ret) {
        result = caml_alloc(1, 1);
        Store_field(result, 0, Val_int(ret));
    }
    else {
        result = caml_alloc(1, 0);
        tuple = caml_alloc_tuple(2);
        Store_field(tuple, 0, alloc_mdb_val_ba(&k));
        Store_field(tuple, 1, alloc_mdb_val_ba(&v));
        Store_field(result, 0, tuple);
    }

    CAMLreturn(result);
}

CAMLprim value stub_mdb_cursor_get_string(value cursor, value key, value data, value op) {
    CAMLparam4(cursor, key, data, op);
    CAMLlocal2(result, tuple);

    MDB_val k, v;
    int ret;

    if (Is_block(key)) {
        k.mv_size = caml_string_length(Field(key, 0));
        k.mv_data = String_val(Field(key, 0));
    }

    if (Is_block(data)) {
        v.mv_size = caml_string_length(Field(data, 0));
        v.mv_data = String_val(Field(data, 0));
    }

    ret = mdb_cursor_get(Cursor_val(cursor), &k, &v, Int_val(op));
    if (ret) {
        result = caml_alloc(1, 1);
        Store_field(result, 0, Val_int(ret));
    }
    else {
        result = caml_alloc(1, 0);
        tuple = caml_alloc_tuple(2);
        Store_field(tuple, 0, alloc_mdb_val_ba(&k));
        Store_field(tuple, 1, alloc_mdb_val_ba(&v));
        Store_field(result, 0, tuple);
    }

    CAMLreturn(result);
}

CAMLprim value stub_mdb_cursor_put(value cursor, value key, value data, value flags) {
    MDB_val k, v;
    k.mv_size = caml_string_length(key);
    k.mv_data = String_val(key);
    v.mv_size = Caml_ba_array_val(data)->dim[0];
    v.mv_data = Caml_ba_data_val(data);
    return Val_int(mdb_cursor_put(Cursor_val(cursor), &k, &v, Int_val(flags)));
}

CAMLprim value stub_mdb_cursor_put_string(value cursor, value key, value data, value flags) {
    MDB_val k, v;
    k.mv_size = caml_string_length(key);
    k.mv_data = String_val(key);
    v.mv_size = caml_string_length(data);
    v.mv_data = String_val(data);
    return Val_int(mdb_cursor_put(Cursor_val(cursor), &k, &v, Int_val(flags)));
}

CAMLprim value stub_mdb_cursor_del(value cursor, value flags) {
    return Val_int(mdb_cursor_del(Cursor_val(cursor), Int_val(flags)));
}

CAMLprim value stub_mdb_cursor_count(value cursor) {
    CAMLparam1(cursor);
    CAMLlocal1(result);

    mdb_size_t count;
    int ret;

    ret = mdb_cursor_count(Cursor_val(cursor), &count);
    if (ret) {
        result = caml_alloc(1, 1);
        Store_field(result, 0, Val_int(ret));
    }
    else {
        result = caml_alloc(1, 0);
        Store_field(result, 0, Val_long(count));
    }

    CAMLreturn(result);
}

/* --------------------------------------------------------------------------
   Copyright (c) 2018 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  --------------------------------------------------------------------------- */
