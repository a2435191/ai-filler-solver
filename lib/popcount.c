#include <caml/mlvalues.h>

CAMLprim value test_identity(value n)
{
    return n;
}