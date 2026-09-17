#include "core.h"
#include "util.h"
int core_init(void) { return util_add(1, 2); }      /* core -> lib : OK */
int core_log(int v)  { return v; }
