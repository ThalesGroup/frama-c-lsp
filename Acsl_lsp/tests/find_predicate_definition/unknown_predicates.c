#include <stddef.h>

/*@
    requires myWorkIs(a);
    requires toDevelopAn(b);
    requires ACSL_language_server(c);
    requires a + b <= c;

    assigns \nothing;

    ensures \result == a + b;
*/
int add(int a, int b) {
    return a + b;
}