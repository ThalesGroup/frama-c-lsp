#include "core.h"
typedef int (*hook_t)(void);

int auth_login(void);
int billing_log(void);

int billing_dispatch(hook_t hook) {
        //@ calls auth_login, billing_log;
    return hook();
}