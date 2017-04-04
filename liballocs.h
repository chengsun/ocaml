#ifndef OCAML_LIBALLOCS_H
#define OCAML_LIBALLOCS_H

#include <stdint.h>

#define STATIC_ASSERT(COND,MSG) typedef char static_assertion_##MSG[(COND)?1:-1]

typedef void *generic_datap_t;
typedef void (*generic_funcp_t)();

typedef union {
    int64_t         i;
    double          d;
    generic_datap_t p;
    generic_funcp_t fp;
} ocaml_value_t;

STATIC_ASSERT(sizeof(int64_t) == 8, sizeof_funcp);
STATIC_ASSERT(sizeof(double) == 8, sizeof_funcp);
STATIC_ASSERT(sizeof(generic_datap_t) == 8, sizeof_datap);
STATIC_ASSERT(sizeof(generic_funcp_t) == 8, sizeof_funcp);
STATIC_ASSERT(sizeof(ocaml_value_t) == 8, sizeof_ocaml_value);

#endif
