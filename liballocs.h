#ifndef OCAML_LIBALLOCS_H
#define OCAML_LIBALLOCS_H

#include <stdint.h>
#include <stdbool.h>
#include <setjmp.h>    // for setjmp

#define STATIC_ASSERT(COND,MSG) typedef char static_assertion_##MSG[(COND)?1:-1]

union _ocaml_value_t;

typedef union _ocaml_value_t *generic_datap_t;
typedef union _ocaml_value_t (*generic_funcp_t)();

typedef union _ocaml_value_t {
    intptr_t        i;
    double          d;
    generic_datap_t p;
    generic_funcp_t fp;
} ocaml_value_t;

STATIC_ASSERT(sizeof(intptr_t) == 8, sizeof_funcp);
STATIC_ASSERT(sizeof(double) == 8, sizeof_funcp);
STATIC_ASSERT(sizeof(generic_datap_t) == 8, sizeof_datap);
STATIC_ASSERT(sizeof(generic_funcp_t) == 8, sizeof_funcp);
STATIC_ASSERT(sizeof(ocaml_value_t) == 8, sizeof_ocaml_value);


// exception handling

struct ocaml_liballocs_exn_handler {
    jmp_buf env;
    struct ocaml_liballocs_exn_handler *next;
};
extern struct ocaml_liballocs_exn_handler
    *__ocaml_liballocs_g_exn_handler_head;

// push a new exception handler onto the head of the list
#define OCAML_LIBALLOCS_EXN_PUSH() \
    do { \
        struct ocaml_liballocs_exn_handler *prev_head = __ocaml_liballocs_g_exn_handler_head; \
        __ocaml_liballocs_g_exn_handler_head = malloc(sizeof(struct ocaml_liballocs_exn_handler)); \
        __ocaml_liballocs_g_exn_handler_head->next = prev_head; \
    } while (0)

#define OCAML_LIBALLOCS_EXN_SETJMP() (setjmp(__ocaml_liballocs_g_exn_handler_head->env))

#define OCAML_LIBALLOCS_EXN_POP() \
    do { \
        __ocaml_liballocs_g_exn_handler_head = \
            __ocaml_liballocs_g_exn_handler_head->next; \
    } while (0)

ocaml_value_t ocaml_liballocs_get_exn(); // get the currently-being-handled exception
ocaml_value_t ocaml_liballocs_raise_exn(ocaml_value_t exn);


// closures

generic_funcp_t ocaml_liballocs_close(generic_funcp_t fun, int n_args, int64_t *env);



ocaml_value_t caml_set_oo_id (ocaml_value_t obj);
ocaml_value_t caml_fresh_oo_id (ocaml_value_t v);


#endif
