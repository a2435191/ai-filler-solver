#include <caml/mlvalues.h>

CAMLprim value fast_popcount_native(value arg)
{
    unsigned long n = Unsigned_long_val(arg);
    int bits = __builtin_popcountl(n);
    return Val_long(bits);
}

CAMLprim value fast_popcount_bytecode(value *argv, int argn)
{
    return fast_popcount_native(argv[0]);
}