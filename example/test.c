#include <stddef.h>
#include "test.h"


/*@
  requires \valid(a + (0 .. n-1));
  terminates \true;
  exits \false;
  assigns a[0 .. n-1];
  ensures l2: \forall integer i; 0 <= i < n ==> a[i] == 0;
*/
int set_to_0(int *a, size_t n)
{
  size_t i;
  
  /*@
    loop invariant 0 <= i <= n;
    loop invariant \forall integer j; 0 <= j < i ==> a[j] == 0;
    loop assigns i, a[0..n-1];
    loop variant n-i;
  */
  for (i = 0; i < n; ++i) a[i] = 0;
  int z = test();
  return z;
}