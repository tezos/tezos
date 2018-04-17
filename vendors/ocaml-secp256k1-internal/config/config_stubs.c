#include <sys/utsname.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

CAMLprim value ml_get_hw_identifier(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(res);

    struct utsname buf;
    uname(&buf);
    res = caml_copy_string(buf.machine);
    CAMLreturn(res);
}
