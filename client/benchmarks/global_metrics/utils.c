/*
 * Benchmark showGlobalMetrics - fichier utilitaire
 */

/*@ requires \true;
  @ assigns \nothing;
  @ ensures \result == a + b;
  @*/
int add(int a, int b) {
    return a + b;
}

/*@ requires \true;
  @ assigns \nothing;
  @ ensures \result == a * b;
  @*/
int multiply(int a, int b) {
    return a * b;
}