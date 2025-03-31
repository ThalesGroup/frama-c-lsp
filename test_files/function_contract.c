#include <limits.h>
/*@
requires INT_MIN < val;
ensures \result >= 0;
ensures (val >= 0 ==> \result == val) &&
(val < 0 ==> \result == -val);
*/
int abs(int val){
 if(val < 0) return -val;
 return val;
}


 /*@
requires 0 <= a < 100;
*/
 void foo(int a){
 int b = abs(42);
 int c = abs(-42);
 int d = abs(a); // False : "a" can be INT_MIN
 int e = abs(INT_MIN); // False : the parameter must be strictly greater than INT_MIN
}


/*@
ensures \result >= a && \result >= b;
ensures \result == a || \result == b;
*/
int max(int a, int b){
return (a > b) ? a : b;
}


void foo_void(void){
 int a = 42;
 int b = 37;
 int c = max(a,b);

 //@assert c == 42;
}



void example_1(void){
 L: ;
 int x = 1 ;
 //@ assert \at(x, L) == 1 ;
}

void example_2(void){
int x ;
L:
 x = 1 ;
 //@ assert \at(x, L) == 1 ;
 }

 void example_3(void){
 L: ;
 int x = 1 ;
 int *ptr = &x ;
 //@ assert \at(*\at(ptr, Here), L) == 1 ;
}


/*@ requires x + 2 != p ; */
void example_4(int* x, int* p){
 *p = 2 ;
 //@ assert x[2] == \at(x[2], Pre) ;
 //@ assert x[*p] == \at(x[*p], Pre) ;
}


/*@ requires \valid(p); */
 int unref(int* p){
 return *p;
}

int const value = 42;

int main_0(){
 int i = unref(&value);
}


int h = 42;

/*@
 requires \valid(a) && \valid(b);
 assigns *a, *b;
 ensures *a == \old(*b) && *b == \old(*a);
*/
void swap(int* a, int* b){
 int tmp = *a;
 *a = *b;
 *b = tmp;
}

int main(){
 int a = 37;
 int b = 91;

 //@ assert h == 42;
 swap(&a, &b);
 //@ assert h == 42;
}


 /*@
 requires \valid_read(a);
 requires *a <= INT_MAX - 5 ;
 assigns \nothing ;
 ensures \result == *a + 5 ;
*/
 int plus_5(int* a){
 return *a + 5 ;
 }

#include <limits.h>

/*@
 requires \valid(a) && \valid_read(b);
 requires \separated(a, b);
 assigns *a;
 ensures *a == \old(*a) + *b;
 ensures *b == \old(*b);
*/
void incr_a_by_b(int* a, int const* b){
 *a += *b;
}


