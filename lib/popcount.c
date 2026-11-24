#include <caml/mlvalues.h>
#include <stdint.h>

CAMLprim value test_identity(value n)
{
    return n;
}

void print_bits(uint64_t n)
{
    static char digits[64];
    for (int i = 0; i < 64; i++)
    {
        digits[i] = (n % 2) ? '1' : '0';
        n /= 2;
    }
    for (int i = 0; i < 64; i++)
        printf("%c", digits[63 - i]);
}

CAMLprim value fast_popcount(value arg)
{
    print_bits(arg);
    printf("\n");

    uint64_t n = arg >> 1;

    int count = __builtin_popcountll(n);
    printf("%d\n", count);
    return (count << 1) | (n & 1);
}