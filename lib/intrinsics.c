#include <caml/mlvalues.h>

CAMLprim intnat fast_popcount_native(value arg)
{
    unsigned long n = Unsigned_long_val(arg);
    int bits = __builtin_popcountl(n);
    return (intnat)bits;
}

CAMLprim value fast_popcount_bytecode(value arg)
{
    return Val_int(fast_popcount_native(arg));
}