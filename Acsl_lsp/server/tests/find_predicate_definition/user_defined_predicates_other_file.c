#include "user_defined_predicates_same_file.h"
/*@
    requires IsPositive(a);
    requires IsNonNegative(b);
    requires a + b < 100;

*/
int add(int a, int b) {
    return a + b;
}