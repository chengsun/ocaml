#ifndef OCAML_LIBALLOCS_H
#define OCAML_LIBALLOCS_H

typedef void *generic_datap_t;
typedef void (*generic_funcp_t)();

typedef union {
    generic_datap_t d;
    generic_funcp_t f;
} boxed_pointer_t;

#endif
