#include "core.h"
int auth_login(void);

int core_log_wrap(void) { return core_log(0); }

int billing_run(void) { return auth_login() + core_log(1); }  /* billing -> auth : VIOLATION */

typedef int (*hook_t)(void);

/* cible non déterminée à la compilation : le callgraph syntaxique
   ne peut pas choisir entre les deux sans annotation `calls` */
int billing_hook(int which) {
    hook_t hook = (which ? auth_login : core_log_wrap);
    //@ calls auth_login, core_log_wrap;
    return hook();
}