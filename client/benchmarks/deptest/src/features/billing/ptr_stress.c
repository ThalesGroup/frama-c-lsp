#include "core.h"
typedef int (*hook_t)(void);
int auth_login(void);
int billing_log(void);
int via_double(hook_t *pp) {
    return (*pp)();         
}