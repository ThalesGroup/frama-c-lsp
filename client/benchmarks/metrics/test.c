/*
 * Benchmark metrics - fonctions de complexite variee
 *
 * Attendu dans la sortie :
 *   - trivial       : cyclomatic = 1 (aucune branche)
 *   - branchy       : cyclomatic eleve (plusieurs if imbriques)
 *   - with_loop     : contient une boucle
 *   - ptr_ops       : manipulation de pointeurs (derefs)
 */

int trivial(int x) {
    return x + 1;
}

int branchy(int x, int y) {
    if (x > 0) {
        if (y > 0) return 1;
        return 2;
    }
    if (x == 0) return 3;
    return 4;
}

int with_loop(int n) {
    int s = 0;
    for (int i = 0; i < n; i++) {
        if (i % 2 == 0) s += i;
    }
    return s;
}

void ptr_ops(int *p, int n) {
    for (int i = 0; i < n; i++) {
        *(p + i) = 0;
    }
}