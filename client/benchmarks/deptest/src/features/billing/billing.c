#include "core.h"
int auth_login(void);

int billing_log(void) { return core_log(0); }

int billing_run(void) { return auth_login() + core_log(1); }  /* billing -> auth : VIOLATION */

int test_static(int a){
    return core_static(a);
}