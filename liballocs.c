#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>

#include "liballocs.h"

void *ocaml_printf(void *a[], ...) {
    va_list args;

    va_start(args, a);
    vprintf(a[1], args);
    va_end(args);

    return NULL;
}

ocaml_value_t *Printf;

void Printf__init() {
    Printf = malloc(2 * sizeof(ocaml_value_t));
    Printf[1].fp = (generic_funcp_t) &ocaml_printf;
}

void Test__init();

void ocaml_liballocs_show(void *obj) {
    fprintf(stderr, "%d\n", (intptr_t)obj);
}

int main() {
    Test__init();
    return 0;
}
