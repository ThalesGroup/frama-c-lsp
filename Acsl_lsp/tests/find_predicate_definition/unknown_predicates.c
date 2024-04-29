#include <stddef.h>

/*@
    requires abra(a);
    requires cada(b);
    requires bra(c);
    requires a + b <= c;

    assigns \nothing;

    ensures \result == a + b;
*/
int add(int a, int b) {
    return a + b;
}