/*@
    predicate IsPositive(int x) = x > 0;
    predicate IsNonNegative(int x) = x >= 0;
*/

/*@
    requires IsPositive(a);
    requires IsNonNegative(b);
    requires a + b < 100;

*/
int add(int a, int b);