/*
 * Benchmark smoke tests - detecte du code trivial ou inatteignable
 *
 * Attendu : WP -wp-smoke-tests emet des diagnostics pour :
 *   - trivial_post   : postcondition \true trivialement vraie
 *   - dead_branch    : branche inatteignable (x > 0 && x < 0)
 *   - trivial_assert : assertion 1 == 1 trivialement vraie
 */

/*@ ensures \true; */
void trivial_post(void) {
    return;
}

int dead_branch(int x) {
    if (x > 0) {
        if (x < 0) {            // contradiction : jamais atteinte
            return -1;
        }
    }
    return x;
}

int trivial_assert(int x) {
    /*@ assert 1 == 1; */
    return x + 1;
}