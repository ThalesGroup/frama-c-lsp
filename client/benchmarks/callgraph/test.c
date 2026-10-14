/*
 * Benchmark call graph - structure de graphe representative
 *
 * Attendu dans le graphe :
 *   entry  ->  process_a  ->  helper
 *           ->  process_b  ->  helper   (feuille partagee)
 *   isolated : noeud sans arete entrante ni sortante utile
 */

int helper(int x) {
    return x * 2;
}

int process_a(int x) {
    return helper(x) + 1;
}

int process_b(int x) {
    return helper(x) - 1;
}

int isolated(void) {
    return 42;
}

int entry(int x) {
    return process_a(x) + process_b(x);
}