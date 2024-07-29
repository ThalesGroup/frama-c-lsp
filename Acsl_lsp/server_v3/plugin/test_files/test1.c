#include <stddef.h>
#include <stdio.h>
#include "math.h"
#include "test2.h"
#include "test1.h"

#define STRING_CONSTANT "ABCD"

/*@
  requires \valid(a+(0..n-1));
  assigns a[0..n-1];

  ensures L : l2: \forall integer i;
    0 <= i < n ==> a[i] == 0;
*/

void set_to_0(int *a, size_t n)
{
  size_t i;

  /*@
    loop invariant 0 <= i <= n;
    loop invariant
    \forall integer j;
      0 <= j < i ==> a[j] == 0;
    loop assigns i, a[0..n-1]; // acsl comment example
    loop variant n-i;

  */
  for (i = 0; i < n; ++i)
    a[i] = 0;
}

int test(double x) {
  float y = __finite(x);
  return x + 1231;
}

void print_string_const(){
  printf("string constant : %s\n", STRING_CONSTANT);
}

void print_ints(int *a, size_t n)
{
  for (int i = 0; i < n; i++)
  {
    printf("%d\t", a[i]);
  }
  printf("\n");
}

void _(int p){
  return;
}

/*@ 
  ghost 
  void func2(void){}
    void func1(int *a, size_t n){
    func2();
  }
*/

int main()
{
  size_t n;
  n = 8;
  int a[8] = {0, 1, 2, 5, 87, 41, -9, -1};

  print_ints(a, n);
  set_to_0(a, n);
  print_ints(a, n);
  _(3);

  int z = test(2);
  
  return 0;
}
