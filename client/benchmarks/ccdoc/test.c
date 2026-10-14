/*
 * Benchmark ccdoc
 * Objectif : generer la documentation LaTeX via ccdoc
 *
 * Attendu : .frama-c/fc_ccdoc.tex genere
 */

/*@ requires x >= 0;
  @ assigns \nothing;
  @ ensures \result >= 0;
  @*/
int abs_val(int x) {
    return x >= 0 ? x : -x;
}

/*@ requires n > 0;
  @ assigns \nothing;
  @ ensures \result >= 1;
  @*/
int factorial(int n) {
    if (n == 1) return 1;
    return n * factorial(n - 1);
}

/*@ requires \valid(a) && \valid(b);
  @ assigns *a, *b;
  @ ensures *a == \old(*b) && *b == \old(*a);
  @*/
void swap(int *a, int *b) {
    int tmp = *a;
    *a = *b;
    *b = tmp;
}