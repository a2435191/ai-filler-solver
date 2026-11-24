#include <caml/mlvalues.h>
#include <stdint.h>

CAMLprim value test_identity(value n)
{
    return n;
}

CAMLprim value fast_popcount(value arg)
{
    uintnat n = Unsigned_long_val(arg);
    int count = __builtin_popcountll(n);
    return Val_int(count);
}