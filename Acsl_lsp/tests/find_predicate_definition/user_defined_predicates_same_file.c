/*@
    predicate IsPositive(int x) = x > 0;
    predicate IsNonNegative(int x) = x >= 0;
*/

/*@
    requires IsPositive(a);
    requires IsNonNegative(b);
    requires a + b < 100;

    assigns \nothing;

    ensures \result == a + b;
*/
int add(int a, int b) {
    return a + b;
}