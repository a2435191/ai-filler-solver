#include <caml/mlvalues.h>
#include <stdint.h>

CAMLprim value test_identity(value n)
{
    return n;
}

CAMLprim value fast_popcount(value arg)
{
    uint64_t n = arg >> 1;
    int count = __builtin_popcountll(n);
    return (count << 1) | (arg & 1);
}