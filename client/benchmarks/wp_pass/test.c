/*
 * Benchmark provePO (WP Pass)
 *
 * Objectif : tester que provePO retourne des goals couvrant tous les types
 * d'obligations de preuve WP prouvables.
 *
 * Cas couverts :
 *   - assigns simple et \nothing
 *   - requires / ensures simples
 *   - behaviors nommés (complete, disjoint)
 *   - loop invariant + loop variant (terminaison)
 *   - RTE guards : division, overflow, accès tableau
 *   - predicate logique réutilisé dans contrat
 *   - lemme utilisé dans preuve
 *   - fonctions avec plusieurs postconditions
 *   - séparation mémoire (\separated)
 *   - pointeurs valides (\valid, \valid_read)
 *
 * Commande Frama-C équivalente :
 *   frama-c -wp -wp-rte -wp-prover=alt-ergo -wp-timeout=30
 *           -wp-report-json=.frama-c/latest_results.json
 *           -machdep gcc_x86_32 ./test.c
 */

typedef unsigned int uint32_t;
typedef int          int32_t;

/* ─────────────────────────────────────────────────────────────────────
 * ANNOTATIONS GLOBALES
 * ──────────────────────────────────────────────────────────────────── */

/*@ predicate valid_range(int *t, integer lo, integer hi) =
  @   \valid(t + (lo .. hi));
  @*/

/*@ predicate sorted(int *t, integer n) =
  @   \forall integer i, j; 0 <= i < j < n ==> t[i] <= t[j];
  @*/

/*@ logic integer sum(int *t, integer n) =
  @   (n <= 0) ? 0 : sum(t, n-1) + t[n-1];
  @*/

/*@ lemma sum_pos:
  @   \forall int *t, integer n;
  @   n >= 0 ==>
  @   (\forall integer i; 0 <= i < n ==> t[i] >= 0) ==>
  @   sum(t, n) >= 0;
  @*/


/* ─────────────────────────────────────────────────────────────────────
 * CAS 1 : assigns \nothing + ensures simple
 * Goals attendus : assigns, ensures
 * Prover attendu : qed (trivial)
 * ──────────────────────────────────────────────────────────────────── */

/*@ requires x >= 0 && x < 100;
  @ assigns \nothing;
  @ ensures \result == x + 1;
  @ ensures \result > 0;
  @ ensures \result > x;
  @*/
int increment(int x) {
    return x + 1;
}


/* ─────────────────────────────────────────────────────────────────────
 * CAS 2 : assigns sur pointeurs + \separated
 * Goals attendus : assigns, ensures, séparation mémoire
 * Prover attendu : qed
 * ──────────────────────────────────────────────────────────────────── */

/*@ requires \valid(a) && \valid(b);
  @ requires \separated(a, b);
  @ assigns *a, *b;
  @ ensures *a == \old(*b);
  @ ensures *b == \old(*a);
  @*/
void swap(int *a, int *b) {
    int tmp = *a;
    *a = *b;
    *b = tmp;
}


/* ─────────────────────────────────────────────────────────────────────
 * CAS 3 : behaviors nommés (complete + disjoint)
 * Goals attendus : behaviors, disjointness, completeness, ensures par behavior
 * Prover attendu : qed
 * ──────────────────────────────────────────────────────────────────── */

/*@ requires x >= -1000 && x <= 1000;
  @ assigns \nothing;
  @ behavior positive:
  @   assumes x > 0;
  @   ensures \result == 1;
  @ behavior zero:
  @   assumes x == 0;
  @   ensures \result == 0;
  @ behavior negative:
  @   assumes x < 0;
  @   ensures \result == -1;
  @ complete behaviors positive, zero, negative;
  @ disjoint behaviors positive, zero, negative;
  @*/
int sign(int x) {
    if (x > 0) return 1;
    if (x == 0) return 0;
    return -1;
}


/* ─────────────────────────────────────────────────────────────────────
 * CAS 4 : loop invariant + loop variant (terminaison)
 * Goals attendus : loop invariant (établissement + maintien), loop variant
 * Prover attendu : qed + alt-ergo
 * ──────────────────────────────────────────────────────────────────── */

/*@ requires n >= 0 && n <= 1000;
  @ assigns \nothing;
  @ ensures \result == n * (n + 1) / 2;
  @*/
int gauss(int n) {
    int s = 0;
    /*@ loop invariant 0 <= i <= n + 1;
      @ loop invariant s == i * (i - 1) / 2;
      @ loop assigns i, s;
      @ loop variant n - i + 1;
      @*/
    for (int i = 1; i <= n; i++) {
        s += i;
    }
    return s;
}


/* ─────────────────────────────────────────────────────────────────────
 * CAS 5 : RTE guards — division par zéro
 * Goals attendus : rte:division_by_zero (valid si b != 0)
 * Prover attendu : qed
 * ──────────────────────────────────────────────────────────────────── */

/*@ requires b != 0;
  @ requires a >= -1000000 && a <= 1000000;
  @ assigns \nothing;
  @ ensures \result == a / b;
  @*/
int safe_div(int a, int b) {
    return a / b;
}


/* ─────────────────────────────────────────────────────────────────────
 * CAS 6 : RTE guards — accès tableau hors bornes
 * Goals attendus : rte:index_bound, assigns sur tableau
 * Prover attendu : qed
 * ──────────────────────────────────────────────────────────────────── */

/*@ requires valid_range(t, 0, n-1);
  @ requires n > 0;
  @ requires 0 <= idx < n;
  @ assigns \nothing;
  @ ensures \result == t[idx];
  @*/
int safe_get(int *t, int n, int idx) {
    return t[idx];
}


/* ─────────────────────────────────────────────────────────────────────
 * CAS 7 : loop sur tableau avec invariant et predicate logique
 * Goals attendus : loop invariant, loop variant, assigns tableau
 * Prover attendu : alt-ergo
 * ──────────────────────────────────────────────────────────────────── */

/*@ requires valid_range(t, 0, n-1);
  @ requires n > 0;
  @ assigns t[0 .. n-1];
  @ ensures \forall integer i; 0 <= i < n ==> t[i] == 0;
  @*/
void fill_zero(int *t, int n) {
    /*@ loop invariant 0 <= i <= n;
      @ loop invariant \forall integer k; 0 <= k < i ==> t[k] == 0;
      @ loop assigns i, t[0 .. n-1];
      @ loop variant n - i;
      @*/
    for (int i = 0; i < n; i++) {
        t[i] = 0;
    }
}


/* ─────────────────────────────────────────────────────────────────────
 * CAS 8 : predicate sorted + \valid_read + ensures sur résultat
 * Goals attendus : requires, ensures, assigns \nothing
 * Prover attendu : qed (trivial car t[0] est le min si sorted)
 * ──────────────────────────────────────────────────────────────────── */

/*@ requires \valid_read(t + (0 .. n-1));
  @ requires n > 0;
  @ requires sorted(t, n);
  @ assigns \nothing;
  @ ensures \forall integer i; 0 <= i < n ==> \result <= t[i];
  @ ensures \result == t[0];
  @*/
int find_min_sorted(int *t, int n) {
    return t[0];
}


/* ─────────────────────────────────────────────────────────────────────
 * CAS 9 : RTE overflow + requires borne
 * Goals attendus : rte:overflow, ensures
 * Prover attendu : qed
 * ──────────────────────────────────────────────────────────────────── */

/*@ requires x >= 0 && x <= 46340;
  @ assigns \nothing;
  @ ensures \result == x * x;
  @ ensures \result >= 0;
  @*/
int square(int x) {
    return x * x;
}


/* ─────────────────────────────────────────────────────────────────────
 * CAS 10 : \separated + assigns précis sur struct via pointeur
 * Goals attendus : assigns, ensures sur champs struct
 * Prover attendu : qed
 * ──────────────────────────────────────────────────────────────────── */

typedef struct { int x; int y; } point_t;

/*@ requires \valid(dst) && \valid_read(src);
  @ requires \separated(dst, src);
  @ assigns dst->x, dst->y;
  @ ensures dst->x == src->x;
  @ ensures dst->y == src->y;
  @*/
void copy_point(point_t *dst, const point_t *src) {
    dst->x = src->x;
    dst->y = src->y;
}
