#include <caml/mlvalues.h>
#include <stdint.h>

CAMLprim intnat fast_popcount_native(intnat arg)
{
    return __builtin_popcountl(arg);
}

CAMLprim intnat fast_popcount_bytecode(value *argv, int argn)
{
    return fast_popcount_native(argv[0]);
}