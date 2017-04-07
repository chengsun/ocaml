#define _DEFAULT_SOURCE     // for MAP_ANONYMOUS

#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>    // for memcpy
#include <setjmp.h>    // for setjmp
#include <sys/mman.h>  // for mmap

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
// TODO: currently this uses an explicit stored singly linked list. I figured this would be easier if I ever needed to debug. but it's really better for neatness/perf if all these next pointers were all implicitly on the stack
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



// closure creation

static void *__closure_buffer = NULL;
static int __closure_buffer_i = 0;
static const int PAGE_SIZE = 4096;

/* This function returns a pointer to a function stub allocated in a page with WX permissions.
 * n_args indicates the number of ordinary arguments fun expects to receive.
 * fun must also be prepared to receive an additional closure object as an argument, as follows:
 *
 * if n_args <= 5, fun must be declared as (<n_args>, ocaml_value_t *env)
 *   In this case the overhead is 3 instructions (one extra jump), 23 bytes
 * if n_args > 5,  fun must be declared as (<n_args>, void *IGNORED, ocaml_value_t *env)
 *   In this case the overhead is 6 instructions (two extra jumps), 27 bytes
 *
 * This function creates stubs assuming the calling convention follows the AMD64 System V standard.
 */
generic_funcp_t ocaml_liballocs_close(generic_funcp_t fun, int n_args, int64_t *env) {
    char buf[256];
    char *ptr = buf;

    bool stack_passing = false;
    switch (n_args) {
    case 0:  *ptr++ = 0x48; *ptr++ = 0xbf; break; // mov rdi (env), ...
    case 1:  *ptr++ = 0x48; *ptr++ = 0xbe; break; // mov rsi (env), ...
    case 2:  *ptr++ = 0x48; *ptr++ = 0xba; break; // mov rdx (env), ...
    case 3:  *ptr++ = 0x48; *ptr++ = 0xb9; break; // mov rcx (env), ...
    case 4:  *ptr++ = 0x49; *ptr++ = 0xb8; break; // mov r8 (env), ...
    case 5:  *ptr++ = 0x49; *ptr++ = 0xb9; break; // mov r9 (env), ...
    default: *ptr++ = 0x49; *ptr++ = 0xbb; stack_passing = true; // mov r11 (env), ...
    }

    assert(sizeof(void*) == 8);
    memcpy(ptr, &env, 8);
    ptr += 8;

    if (stack_passing) {
        // push r11 (env)
        *ptr++ = 0x41;
        *ptr++ = 0x53;
    }

    // mov r10 (fp), ...
    *ptr++ = 0x49;
    *ptr++ = 0xba;

    assert(sizeof(generic_funcp_t) == 8);
    memcpy(ptr, &fun, 8);
    ptr += 8;

    if (!stack_passing) {
        // jmp r10 (fp)
        *ptr++ = 0x41;
        *ptr++ = 0xff;
        *ptr++ = 0xe2;
    } else {
        // call r10 (fp)
        *ptr++ = 0x41;
        *ptr++ = 0xff;
        *ptr++ = 0xd2;

        // pop rcx (env)
        *ptr++ = 0x59;

        // ret
        *ptr++ = 0xc3;
    }

    int len = ptr - buf;

    if (__closure_buffer == NULL || PAGE_SIZE - __closure_buffer_i < len) {
        // allocate a new page
        __closure_buffer = mmap(NULL, PAGE_SIZE, PROT_WRITE | PROT_EXEC,
                              MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
        __closure_buffer_i = 0;
    }

    memcpy(__closure_buffer + __closure_buffer_i, buf, len);

    generic_funcp_t the_closure = (generic_funcp_t) (__closure_buffer + __closure_buffer_i);

    __closure_buffer_i += len;

    return the_closure;
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
        fprintf(stderr, "Uncaught OCaml exception: %s\n",
                (const char *)ocaml_liballocs_g_exn_handler_exn.p[0].p);
        return 1;
    }
}
