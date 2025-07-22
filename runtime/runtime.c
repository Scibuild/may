#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void panic_index_out_of_bounds(int sl, int sc, int el, int ec)
{
  fprintf(stderr, "Panic! Index out of bounds at %d:%d-%d:%d\n", sl, sc, el, ec);
  abort();
}

void panic_unexpected_option_is_null(int sl, int sc, int el, int ec)
{
  fprintf(stderr, "Panic! Encountered unexpected null value at %d:%d-%d:%d\n", sl, sc, el, ec);
  abort();
}

struct may_string
{
  char *ptr;
  int off;
  int len;
};

void internal_may_print_string(struct may_string *s)
{
  fwrite(s->ptr + s->off, 1, s->len, stdout);
}

void internal_may_print_int(long i)
{
  printf("%ld", i);
}