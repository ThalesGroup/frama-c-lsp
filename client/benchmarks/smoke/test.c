/*
 * Benchmark smoke tests
 * WP -wp-smoke-tests detecte les preconditions contradictoires
 *
 * Attendu :
 *   - impossible : requires x > 0 && x < 0 → contradiction → "Doomed"
 *   - bounded_square : postcondition verifiable → smoke passe
 *   - safe_abs : fonction normale → smoke passe
 */

/* precondition contradictoire → WP detecte "Doomed" */
/*@ requires x > 0;
  @ requires x < 0;
  @ assigns \nothing;
  @ ensures \result == x;
  @*/
int impossible(int x) {
    return x;
}

/* postcondition trivialement vraie */
/*@ requires x >= 0 && x <= 10;
  @ assigns \nothing;
  @ ensures \result >= 0 && \result <= 100;
  @*/
int bounded_square(int x) {
    return x * x;
}

/* fonction normale — ne doit PAS generer de smoke */
/*@ requires n >= 0;
  @ assigns \nothing;
  @ ensures \result >= 0;
  @*/
int safe_abs(int n) {
    return n >= 0 ? n : -n;
}