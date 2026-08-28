/*
 * Benchmark showGlobalMetrics - fichier principal
 * Configure kernel.sourceFiles = [main.c, utils.c]
 * Attendu : fc_metrics.txt contient les metriques des deux fichiers
 */

extern int add(int a, int b);
extern int multiply(int a, int b);

/*@ requires \valid(result);
  @ assigns *result;
  @ ensures *result == a + b * 2;
  @*/
void compute(int a, int b, int *result) {
    *result = add(a, multiply(b, 2));
}

int main(void) {
    int r;
    compute(3, 4, &r);
    return r;
}