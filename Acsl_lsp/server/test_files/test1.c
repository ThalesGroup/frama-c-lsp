#include <stddef.h>
#include <stdio.h>
#include <ctype.h>
#include "test3.h"
#include "test1.h"
#include "test2.h"

#define STRING_CONSTANT "ABCD"
#define INTEGER_CONSTANT 2

// extern int test(double x);

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
  return x + 1231;
}

void print_string_const(){
  printf("strîng constant : %s\n", STRING_CONSTANT);
}

/*@
  requires n > 0;
*/
void print_ints(int *a, size_t n)
{
  for (int i = 0; i < n; i++)
  {
    printf("%d\t", a[i]);
  }
  printf("\n");
}

int az(int p, int b){
  size_t n;
  return b;
}

/*@ 
  ghost 
  void func2(void){}
  void func1(int *a, size_t n){
    func2();
  }
  void func3(int *a, size_t n){
    func2();
  }
*/

int main()
{
  const int c1 = 785;
  int i1 = (isalpha('a')) ? c1 + INTEGER_CONSTANT : 52;
  size_t n;
  n = 8;
  int a[8] = {0, 1, 2, 5, 87, 41, -9, -1};

  print_ints(a, n);
  set_to_0(a, n);
  print_ints(a, n);
  az(i1, n);
  print_string_const();ù

  int z = test(2);
  return 0;
}