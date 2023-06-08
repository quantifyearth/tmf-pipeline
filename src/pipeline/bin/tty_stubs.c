#include <sys/ioctl.h>

#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/memory.h>

CAMLprim value caml_tmf_output_buffer_count(value v_fd) {
    CAMLparam0();
    int count;
    ioctl(Int_val(v_fd), TIOCOUTQ, &count);
    CAMLreturn(Val_int(count));
}