#define _DEFAULT_SOURCE     // for MAP_ANONYMOUS

#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>    // for memcpy
#include <sys/mman.h>  // for mmap

#include "liballocs_runtime.h"

/*****************************************************************************
  caml internal functions
*****************************************************************************/

static int64_t oo_last_id = 0;

ocaml_value_t caml_set_oo_id (ocaml_value_t obj) {
  GET_P(obj)[1] = NEW_I(oo_last_id);
  oo_last_id++;
  return obj;
}

// used by [exception]
ocaml_value_t caml_fresh_oo_id (ocaml_value_t v) {
  return NEW_I(oo_last_id++);
}



/*****************************************************************************
  liballocs
*****************************************************************************/

// exception handling

// the head of the exception handler linked list
struct ocaml_liballocs_exn_handler
    *__ocaml_liballocs_g_exn_handler_head = NULL;
// the exception currently being handled
static ocaml_value_t __ocaml_liballocs_g_exn_handler_exn;

ocaml_value_t ocaml_liballocs_get_exn() {
    return __ocaml_liballocs_g_exn_handler_exn;
}

ocaml_value_t ocaml_liballocs_raise_exn(ocaml_value_t exn) {
    __ocaml_liballocs_g_exn_handler_exn = exn;
    longjmp(__ocaml_liballocs_g_exn_handler_head->env, 1);
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
generic_funcp_t ocaml_liballocs_close(generic_funcp_t fun, int64_t n_args, ocaml_value_t env) {
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

    assert(sizeof(env) == 8);
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





// runtime stuff

#define _QUOTE(x) #x
#define QUOTE(x) _QUOTE(x)
#define DEFINE_BUILTIN_EXCEPTION(name) \
    ocaml_value_t __##name[2] = { \
        NEW_P((generic_datap_t) QUOTE(name)), \
        NEW_I(0) \
    }; \
    ocaml_value_t *name = __##name;

DEFINE_BUILTIN_EXCEPTION(Match_failure)
DEFINE_BUILTIN_EXCEPTION(Assert_failure)
DEFINE_BUILTIN_EXCEPTION(Invalid_argument)
DEFINE_BUILTIN_EXCEPTION(Failure)
DEFINE_BUILTIN_EXCEPTION(Not_found)
DEFINE_BUILTIN_EXCEPTION(Out_of_memory)
DEFINE_BUILTIN_EXCEPTION(Stack_overflow)
DEFINE_BUILTIN_EXCEPTION(Sys_error)
DEFINE_BUILTIN_EXCEPTION(End_of_file)
DEFINE_BUILTIN_EXCEPTION(Division_by_zero)
DEFINE_BUILTIN_EXCEPTION(Sys_blocked_io)
DEFINE_BUILTIN_EXCEPTION(Undefined_recursive_module)

#undef _QUOTE
#undef QUOTE
#undef DEFINE_BUILTIN_EXCEPTION



void ocaml_show(ocaml_value_t v) {
    if (IS_P(v)) {
        fprintf(stderr, "pointer-like: %"PRIxPTR"\n", (uintptr_t)GET_P(v));
    } else if (IS_I(v)) {
        fprintf(stderr, "integer-like: %"PRIiPTR"\n", GET_I(v));
    } else if (IS_D(v)) {
        fprintf(stderr, "float-like: %f\n", GET_D(v));
    } else {
        assert(false);
    }
}


/*****************************************************************************
  main
*****************************************************************************/

void Test__init();


int main() {
    int ret;

    // set up root exception handler
    OCAML_LIBALLOCS_EXN_PUSH();
    if (0 == OCAML_LIBALLOCS_EXN_SETJMP()) {
        Test__init();
        ret = 0;
    } else { // catch
        OCAML_LIBALLOCS_EXN_POP();
        fprintf(stderr, "Uncaught OCaml exception: %s\n",
                (const char *) GET_P(GET_P(ocaml_liballocs_get_exn())[0]));
        ret = 1;
    }

    return ret;
}
