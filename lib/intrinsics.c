#include <caml/mlvalues.h>

CAMLprim intnat fast_popcount_native(value arg)
{
    unsigned long n = Unsigned_long_val(arg);
    int bits = __builtin_popcountl(n);
    return (intnat)bits;
}

CAMLprim intnat fast_popcount_bytecode(value *argv, int argn)
{
    return fast_popcount_native(argv[0]);
}