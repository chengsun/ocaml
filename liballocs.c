#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>
#include <setjmp.h>

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

// exception handling
struct ocaml_liballocs_exn_handler {
    jmp_buf env;
    struct ocaml_liballocs_exn_handler *next;
};
// the head of the exception handler linked list
static struct ocaml_liballocs_exn_handler
    *ocaml_liballocs_g_exn_handler_head = NULL;
// the exception currently being handled
static ocaml_value_t ocaml_liballocs_g_exn_handler_exn;

// returns true on first invocation, false if handling exception
bool ocaml_liballocs_push_exn_handler() {
    {
        // push us onto the head of the list
        struct ocaml_liballocs_exn_handler *prev_head = ocaml_liballocs_g_exn_handler_head;
        ocaml_liballocs_g_exn_handler_head = malloc(sizeof(struct ocaml_liballocs_exn_handler));
        ocaml_liballocs_g_exn_handler_head->next = prev_head;
    }

    if (setjmp(ocaml_liballocs_g_exn_handler_head->env)) {
        // pop us from the list again
        ocaml_liballocs_g_exn_handler_head =
            ocaml_liballocs_g_exn_handler_head->next;
        return false;
    } else {
        return true;
    }
}

ocaml_value_t ocaml_liballocs_get_exn() {
    return ocaml_liballocs_g_exn_handler_exn;
}

ocaml_value_t ocaml_liballocs_raise_exn(ocaml_value_t exn) {
    ocaml_liballocs_g_exn_handler_exn = exn;
    longjmp(ocaml_liballocs_g_exn_handler_head->env, 1);
}

void ocaml_liballocs_show(ocaml_value_t obj) {
    fprintf(stderr, "%d\n", obj.i);
}

/*****************************************************************************
  main
*****************************************************************************/

int main() {
    // set up root exception handler
    if (ocaml_liballocs_push_exn_handler()) {
        Test__init();
        return 0;
    } else { // catch
        fprintf(stderr, "Fatal uncaught OCaml exception: %s\n",
                (const char *)ocaml_liballocs_g_exn_handler_exn.p[0].p);
        return 1;
    }
}
