#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>

#include "liballocs.h"

/*****************************************************************************
  caml internal functions
*****************************************************************************/

static int64_t oo_last_id = 0;

ocaml_value_t caml_set_oo_id (ocaml_value_t obj) {
  obj.p[1].i = oo_last_id;
  oo_last_id++;
  return obj;
}

// used by [exception]
ocaml_value_t caml_fresh_oo_id (ocaml_value_t v) {
  return (ocaml_value_t){.i = oo_last_id++};
}

/*****************************************************************************
  Printf
*****************************************************************************/

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


/*****************************************************************************
  liballocs
*****************************************************************************/

void ocaml_liballocs_show(void *obj) {
    fprintf(stderr, "%d\n", (intptr_t)obj);
}

/*****************************************************************************
  main
*****************************************************************************/

int main() {
    Test__init();
    return 0;
}
