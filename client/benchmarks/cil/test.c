/*
 * Benchmark CIL - constructions que Frama-C normalise
 *
 * Attendu dans la sortie CIL :
 *   - for(...)  devient un  while(...)
 *   - switch(...) devient une chaine if/else
 *   - a ? b : c devient un if/else
 *   - && / || devient des if imbriques
 */

int abs_val(int x) {
    return x >= 0 ? x : -x;
}

int classify(int x) {
    switch (x) {
        case 0:  return 100;
        case 1:  return 200;
        default: return 300;
    }
}

int sum_to(int n) {
    int s = 0;
    for (int i = 0; i < n; i++) {
        s += i;
    }
    return s;
}

int in_range(int x, int lo, int hi) {
    return x >= lo && x <= hi;
}