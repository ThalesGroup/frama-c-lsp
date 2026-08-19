/*
 * Benchmark 01 - DisplayAST
 * Objectif : tester que getAST retourne correctement toutes les catégories
 *
 * Catégories testées :
 *   - functions  : fonctions simples, void, static, avec/sans paramètres
 *   - globals    : variables globales, constantes, tableaux globaux
 *   - types      : typedef, struct, enum, union
 *   - annotations: predicate, logic function, lemma, axiomatic, invariant
 *
 */

/* ─────────────────────────────────────────────────────────────────────────
 * SECTION 1 : TYPES (typedef, struct, enum, union)
 * Attendu dans AST : "types" -> [size_t, status_t, point_t, color_t, data_t]
 * ───────────────────────────────────────────────────────────────────────── */

typedef unsigned int  uint32_t;
typedef unsigned char uint8_t;
typedef int           int32_t;

/* struct simple */
typedef struct {
    int32_t  x;
    int32_t  y;
} point_t;

/* struct avec tableau membre */
typedef struct {
    uint8_t  data[16];
    uint32_t len;
} buffer_t;

/* enum */
typedef enum {
    STATUS_OK    = 0,
    STATUS_ERROR = 1,
    STATUS_BUSY  = 2
} status_t;

/* union */
typedef union {
    uint32_t  word;
    uint8_t   bytes[4];
} data_t;

/* struct imbriquée */
typedef struct {
    point_t   origin;
    point_t   end;
    status_t  state;
} segment_t;


/* ─────────────────────────────────────────────────────────────────────────
 * SECTION 2 : VARIABLES GLOBALES
 * Attendu dans AST : "globals" -> [g_counter, g_buffer, g_MAX, g_table]
 * ───────────────────────────────────────────────────────────────────────── */

/* variable globale simple */
int32_t g_counter = 0;

/* tableau global */
uint8_t g_buffer[16];

/* constante globale */
const uint32_t g_MAX = 255;

/* tableau global de structs */
point_t g_table[4];

/* pointeur global */
uint8_t *g_ptr;


/* ─────────────────────────────────────────────────────────────────────────
 * SECTION 3 : ANNOTATIONS ACSL
 * Attendu dans AST : "annotations" -> predicats, logic functions, lemmes, axiomatiques
 * ───────────────────────────────────────────────────────────────────────── */

/*@ predicate valid_buffer{L}(uint8_t *b, uint32_t n) =
  @   \valid(b + (0 .. n-1)) && n <= 16;
  @*/

/*@ predicate non_null(uint8_t *p) =
  @   p != \null;
  @*/

/*@ logic integer sum_range(int *t, integer lo, integer hi) =
  @   (lo > hi) ? 0 : t[lo] + sum_range(t, lo+1, hi);
  @*/

/*@ logic uint32_t max_val(uint32_t a, uint32_t b) =
  @   (a >= b) ? a : b;
  @*/

/*@ lemma max_commut:
  @   \forall uint32_t a, b; max_val(a, b) == max_val(b, a);
  @*/

/*@ axiomatic SumProps {
  @   logic integer sum{L}(int *t, integer n);
  @   axiom sum_empty{L}:
  @     \forall int *t; sum(t, 0) == 0;
  @   axiom sum_next{L}:
  @     \forall int *t, integer n; n > 0 ==>
  @       sum(t, n) == sum(t, n-1) + t[n-1];
  @ }
  @*/

/*@ type invariant buffer_inv(buffer_t b) =
  @   b.len <= 16;
  @*/


/* ─────────────────────────────────────────────────────────────────────────
 * SECTION 4 : FONCTIONS
 * Attendu dans AST : "functions" -> toutes les fonctions ci-dessous
 * ───────────────────────────────────────────────────────────────────────── */

/* fonction simple sans annotation */
int32_t add(int32_t a, int32_t b) {
    return a + b;
}

/* fonction void */
void reset_counter(void) {
    g_counter = 0;
}

/* fonction avec contrat WP complet */
/*@ requires \valid(dst) && \valid_read(src);
  @ requires \separated(dst, src);
  @ assigns *dst;
  @ ensures *dst == *src;
  @*/
void copy_int(int32_t *dst, const int32_t *src) {
    *dst = *src;
}

/* fonction avec boucle et invariant */
/*@ requires \valid(buf + (0 .. n-1));
  @ requires n > 0 && n <= 16;
  @ assigns buf[0 .. n-1];
  @*/
void fill_zero(uint8_t *buf, uint32_t n) {
    /*@ loop invariant 0 <= i <= n;
      @ loop assigns i, buf[0 .. n-1];
      @ loop variant n - i;
      @*/
    for (uint32_t i = 0; i < n; i++) {
        buf[i] = 0;
    }
}

/* fonction sur struct */
/*@ requires \valid_read(p);
  @ assigns \nothing;
  @ ensures \result.x == p->x && \result.y == p->y;
  @*/
point_t clone_point(const point_t *p) {
    point_t res;
    res.x = p->x;
    res.y = p->y;
    return res;
}

/* fonction sur enum */
/*@ requires \valid(s);
  @ assigns *s;
  @ ensures *s == STATUS_OK;
  @*/
void reset_status(status_t *s) {
    *s = STATUS_OK;
}

/* fonction sur union */
/*@ requires \valid_read(d);
  @ assigns \nothing;
  @*/
uint8_t get_byte(const data_t *d, uint32_t idx) {
    if (idx < 4) return d->bytes[idx];
    return 0;
}

/* fonction récursive */
/*@ requires n >= 0;
  @ decreases n;
  @ assigns \nothing;
  @*/
uint32_t factorial(uint32_t n) {
    if (n == 0) return 1;
    return n * factorial(n - 1);
}

/* fonction static */
/*@ requires a >= 0 && b >= 0;
  @ assigns \nothing;
  @ ensures \result >= a && \result >= b;
  @*/
static uint32_t max(uint32_t a, uint32_t b) {
    return (a >= b) ? a : b;
}

/* déclaration seule (pas de corps) — cas "forward declaration" */
int32_t external_func(int32_t x);
