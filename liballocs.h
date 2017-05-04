#ifndef OCAML_LIBALLOCS_H
#define OCAML_LIBALLOCS_H

#include <assert.h>
#include <math.h>
#include <stdlib.h>    // for malloc
#include <stdint.h>
#include <stdbool.h>
#include <setjmp.h>    // for setjmp
#include <string.h>    // for strcmp

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

STATIC_ASSERT(sizeof(intptr_t) == 8, sizeof_intptr_t);
STATIC_ASSERT(sizeof(double) == 8, sizeof_double);
STATIC_ASSERT(sizeof(generic_datap_t) == 8, sizeof_datap);
STATIC_ASSERT(sizeof(generic_funcp_t) == 8, sizeof_funcp);
STATIC_ASSERT(sizeof(ocaml_value_t) == 8, sizeof_ocaml_value);

#define GET_I(v) ((v).i)
#define GET_D(v) ((v).d)
#define GET_P(v) ((v).p)
#define GET_FP(v) ((v).fp)

#define SET_I(v,x) do { (v).i = (x); } while (0)
#define SET_D(v,x) do { (v).d = (x); } while (0)
#define SET_P(v,x) do { (v).p = (x); } while (0)
#define SET_FP(v,x) do { (v).fp = (x); } while (0)

#define NEW_I(v) ((ocaml_value_t){.i = (v)})
#define NEW_D(v) ((ocaml_value_t){.d = (v)})
#define NEW_P(v) ((ocaml_value_t){.p = (v)})
#define NEW_FP(v) ((ocaml_value_t){.fp = (v)})


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

extern ocaml_value_t ocaml_liballocs_get_exn(); // get the currently-being-handled exception
extern ocaml_value_t ocaml_liballocs_raise_exn(ocaml_value_t exn);


// closures

extern generic_funcp_t ocaml_liballocs_close(generic_funcp_t fun, int64_t n_args, ocaml_value_t env);



extern ocaml_value_t caml_set_oo_id (ocaml_value_t obj);
extern ocaml_value_t caml_fresh_oo_id (ocaml_value_t v);



// runtime stuff

extern ocaml_value_t *Match_failure;
extern ocaml_value_t *Assert_failure;
extern ocaml_value_t *Invalid_argument;
extern ocaml_value_t *Failure;
extern ocaml_value_t *Not_found;
extern ocaml_value_t *Out_of_memory;
extern ocaml_value_t *Stack_overflow;
extern ocaml_value_t *Sys_error;
extern ocaml_value_t *End_of_file;
extern ocaml_value_t *Division_by_zero;
extern ocaml_value_t *Sys_blocked_io;
extern ocaml_value_t *Undefined_recursive_module;

static ocaml_value_t caml_equal(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_equal unimplemented"); }
static ocaml_value_t caml_notequal(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_notequal unimplemented"); }
static ocaml_value_t caml_lessthan(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_lessthan unimplemented"); }
static ocaml_value_t caml_greaterthan(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_greaterthan unimplemented"); }
static ocaml_value_t caml_lessequal(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_lessequal unimplemented"); }
static ocaml_value_t caml_greaterequal(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_greaterequal unimplemented"); }

static ocaml_value_t caml_string_equal(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_string_equal unimplemented"); }
static ocaml_value_t caml_string_notequal(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_string_notequal unimplemented"); }
static ocaml_value_t caml_string_lessthan(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_string_lessthan unimplemented"); }
static ocaml_value_t caml_string_greaterthan(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_string_greaterthan unimplemented"); }
static ocaml_value_t caml_string_lessequal(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_string_lessequal unimplemented"); }
static ocaml_value_t caml_string_greaterequal(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_string_greaterequal unimplemented"); }


static ocaml_value_t caml_compare(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_compare unimplemented"); }
static ocaml_value_t caml_int_compare(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_int_compare unimplemented"); }
static ocaml_value_t caml_string_compare(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_string_compare unimplemented"); }
static ocaml_value_t caml_nativeint_compare(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_nativeint_compare unimplemented"); }
static ocaml_value_t caml_int32_compare(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_int32_compare unimplemented"); }
static ocaml_value_t caml_int64_compare(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_int64_compare unimplemented"); }



/*
external register_named_value : string -> 'a -> unit
                              = "caml_register_named_value"
*/
static ocaml_value_t caml_register_named_value(ocaml_value_t v1, ocaml_value_t v2) {
    // TODO: doesn't need to do anything right now
    // assert(false && "caml_register_named_value unimplemented");
    return NEW_I(0);
}

/*
external ( ** ) : float -> float -> float = "caml_power_float" "pow"
  [@@unboxed] [@@noalloc]
external exp : float -> float = "caml_exp_float" "exp" [@@unboxed] [@@noalloc]
external expm1 : float -> float = "caml_expm1_float" "caml_expm1"
  [@@unboxed] [@@noalloc]
external acos : float -> float = "caml_acos_float" "acos"
  [@@unboxed] [@@noalloc]
external asin : float -> float = "caml_asin_float" "asin"
  [@@unboxed] [@@noalloc]
external atan : float -> float = "caml_atan_float" "atan"
  [@@unboxed] [@@noalloc]
external atan2 : float -> float -> float = "caml_atan2_float" "atan2"
  [@@unboxed] [@@noalloc]
external hypot : float -> float -> float
               = "caml_hypot_float" "caml_hypot" [@@unboxed] [@@noalloc]
external cos : float -> float = "caml_cos_float" "cos" [@@unboxed] [@@noalloc]
external cosh : float -> float = "caml_cosh_float" "cosh"
  [@@unboxed] [@@noalloc]
external log : float -> float = "caml_log_float" "log" [@@unboxed] [@@noalloc]
external log10 : float -> float = "caml_log10_float" "log10"
  [@@unboxed] [@@noalloc]
external log1p : float -> float = "caml_log1p_float" "caml_log1p"
  [@@unboxed] [@@noalloc]
external sin : float -> float = "caml_sin_float" "sin" [@@unboxed] [@@noalloc]
external sinh : float -> float = "caml_sinh_float" "sinh"
  [@@unboxed] [@@noalloc]
external sqrt : float -> float = "caml_sqrt_float" "sqrt"
  [@@unboxed] [@@noalloc]
external tan : float -> float = "caml_tan_float" "tan" [@@unboxed] [@@noalloc]
external tanh : float -> float = "caml_tanh_float" "tanh"
  [@@unboxed] [@@noalloc]
external ceil : float -> float = "caml_ceil_float" "ceil"
  [@@unboxed] [@@noalloc]
external floor : float -> float = "caml_floor_float" "floor"
  [@@unboxed] [@@noalloc]
external copysign : float -> float -> float
                  = "caml_copysign_float" "caml_copysign"
                  [@@unboxed] [@@noalloc]
external mod_float : float -> float -> float = "caml_fmod_float" "fmod"
  [@@unboxed] [@@noalloc]
external frexp : float -> float * int = "caml_frexp_float"
external ldexp : (float [@unboxed]) -> (int [@untagged]) -> (float [@unboxed]) =
  "caml_ldexp_float" "caml_ldexp_float_unboxed" [@@noalloc]
external modf : float -> float * float = "caml_modf_float"
*/
static ocaml_value_t caml_power_float(ocaml_value_t v1, ocaml_value_t v2) {
    return NEW_D(pow(GET_D(v1), GET_D(v2)));
}
static ocaml_value_t caml_exp_float(ocaml_value_t v1) {
    return NEW_D(exp(GET_D(v1)));
}
static ocaml_value_t caml_expm1_float(ocaml_value_t v1) {
    assert (false && "unimplemented");
    return NEW_D(0.0);
}
static ocaml_value_t caml_acos_float(ocaml_value_t v1) {
    return NEW_D(acos(GET_D(v1)));
}
static ocaml_value_t caml_asin_float(ocaml_value_t v1) {
    return NEW_D(asin(GET_D(v1)));
}
static ocaml_value_t caml_atan_float(ocaml_value_t v1) {
    return NEW_D(atan(GET_D(v1)));
}
static ocaml_value_t caml_atan2_float(ocaml_value_t v1, ocaml_value_t v2) {
    return NEW_D(atan2(GET_D(v1), GET_D(v2)));
}
static ocaml_value_t caml_hypot_float(ocaml_value_t v1, ocaml_value_t v2) {
    assert (false && "unimplemented");
    return NEW_D(0.0);
}
static ocaml_value_t caml_cos_float(ocaml_value_t v1) {
    return NEW_D(cos(GET_D(v1)));
}
static ocaml_value_t caml_cosh_float(ocaml_value_t v1) {
    return NEW_D(cosh(GET_D(v1)));
}
static ocaml_value_t caml_log_float(ocaml_value_t v1) {
    return NEW_D(log(GET_D(v1)));
}
static ocaml_value_t caml_log10_float(ocaml_value_t v1) {
    return NEW_D(log10(GET_D(v1)));
}
static ocaml_value_t caml_log1p_float(ocaml_value_t v1) {
    assert (false && "unimplemented");
    return NEW_D(0.0);
}
static ocaml_value_t caml_sin_float(ocaml_value_t v1) {
    return NEW_D(sin(GET_D(v1)));
}
static ocaml_value_t caml_sinh_float(ocaml_value_t v1) {
    return NEW_D(sinh(GET_D(v1)));
}
static ocaml_value_t caml_sqrt_float(ocaml_value_t v1) {
    return NEW_D(sqrt(GET_D(v1)));
}
static ocaml_value_t caml_tan_float(ocaml_value_t v1) {
    return NEW_D(tan(GET_D(v1)));
}
static ocaml_value_t caml_tanh_float(ocaml_value_t v1) {
    return NEW_D(tanh(GET_D(v1)));
}
static ocaml_value_t caml_ceil_float(ocaml_value_t v1) {
    return NEW_D(ceil(GET_D(v1)));
}
static ocaml_value_t caml_floor_float(ocaml_value_t v1) {
    return NEW_D(floor(GET_D(v1)));
}
static ocaml_value_t caml_copysign_float(ocaml_value_t v1, ocaml_value_t v2) {
    assert (false && "unimplemented");
    return NEW_D(0.0);
}
static ocaml_value_t caml_fmod_float(ocaml_value_t v1, ocaml_value_t v2) {
    return NEW_D(fmod(GET_D(v1), GET_D(v2)));
}
static ocaml_value_t caml_frexp_float(ocaml_value_t v1) {
    assert (false && "unimplemented");
    return NEW_D(0.0);
}
static ocaml_value_t caml_ldexp_float(ocaml_value_t v1, ocaml_value_t v2) {
    assert (false && "unimplemented");
    return NEW_D(0.0);
}
static ocaml_value_t caml_modf_float(ocaml_value_t v1) {
    assert (false && "unimplemented");
    return NEW_D(0.0);
}

/*
external float_of_bits : int64 -> float = "caml_int64_float_of_bits"
*/
static ocaml_value_t caml_int64_float_of_bits(ocaml_value_t v) {
    return NEW_D((double) GET_I(v));
}

/*
external classify_float : (float [@unboxed]) -> fpclass =
  "caml_classify_float" "caml_classify_float_unboxed" [@@noalloc]
external bytes_create : int -> bytes = "caml_create_string"
external string_blit : string -> int -> bytes -> int -> int -> unit
                     = "caml_blit_string" [@@noalloc]
external bytes_blit : bytes -> int -> bytes -> int -> int -> unit
                        = "caml_blit_string" [@@noalloc]
external format_int : string -> int -> string = "caml_format_int"
external format_float : string -> float -> string = "caml_format_float"
external int_of_string : string -> int = "caml_int_of_string"
external float_of_string : string -> float = "caml_float_of_string"
*/
static ocaml_value_t caml_create_string(ocaml_value_t v) { assert(false && "caml_create_string unimplemented"); }
static ocaml_value_t caml_blit_string(ocaml_value_t v1, ocaml_value_t v2, ocaml_value_t v3, ocaml_value_t v4, ocaml_value_t v5) { assert(false && "caml_blit_string unimplemented"); }
static ocaml_value_t caml_format_int(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_format_int unimplemented"); }
static ocaml_value_t caml_format_float(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_format_float unimplemented"); }
static ocaml_value_t caml_int_of_string(ocaml_value_t v) {
    return NEW_I(atol((const char *) GET_P(v)));
}
static ocaml_value_t caml_float_of_string(ocaml_value_t v) {
    return NEW_D((double)atof((const char *) GET_P(v)));
}


/*
external open_descriptor_out : int -> out_channel
                             = "caml_ml_open_descriptor_out"
external open_descriptor_in : int -> in_channel = "caml_ml_open_descriptor_in"
external open_desc : string -> open_flag list -> int -> int = "caml_sys_open"
*/
static ocaml_value_t caml_ml_open_descriptor_out(ocaml_value_t v) {
    // TODO: doesn't need to do anything right now
    // assert(false && "caml_ml_open_descriptor_out unimplemented");
    return NEW_I(0);
}
static ocaml_value_t caml_ml_open_descriptor_in (ocaml_value_t v) {
    // TODO: doesn't need to do anything right now
    // assert(false && "caml_ml_open_descriptor_in unimplemented");
    return NEW_I(0);
}
static ocaml_value_t caml_sys_open(ocaml_value_t v1, ocaml_value_t v2, ocaml_value_t v3) { assert(false && "caml_ml_open_desc unimplemented"); }


/*
external set_out_channel_name: out_channel -> string -> unit =
  "caml_ml_set_channel_name"
external flush : out_channel -> unit = "caml_ml_flush"
external out_channels_list : unit -> out_channel list
                           = "caml_ml_out_channels_list"
external unsafe_output : out_channel -> bytes -> int -> int -> unit
                       = "caml_ml_output"
external unsafe_output_string : out_channel -> string -> int -> int -> unit
                              = "caml_ml_output"
external output_char : out_channel -> char -> unit = "caml_ml_output_char"
external output_byte : out_channel -> int -> unit = "caml_ml_output_char"
external output_binary_int : out_channel -> int -> unit = "caml_ml_output_int"
external marshal_to_channel : out_channel -> 'a -> unit list -> unit
     = "caml_output_value"
external seek_out : out_channel -> int -> unit = "caml_ml_seek_out"
external pos_out : out_channel -> int = "caml_ml_pos_out"
external out_channel_length : out_channel -> int = "caml_ml_channel_size"
external close_out_channel : out_channel -> unit = "caml_ml_close_channel"
external set_binary_mode_out : out_channel -> bool -> unit
                             = "caml_ml_set_binary_mode"
external set_in_channel_name: in_channel -> string -> unit =
  "caml_ml_set_channel_name"
external input_char : in_channel -> char = "caml_ml_input_char"

external unsafe_input : in_channel -> bytes -> int -> int -> int
                      = "caml_ml_input"
external input_scan_line : in_channel -> int = "caml_ml_input_scan_line"
external input_byte : in_channel -> int = "caml_ml_input_char"
external input_binary_int : in_channel -> int = "caml_ml_input_int"
external input_value : in_channel -> 'a = "caml_input_value"
external seek_in : in_channel -> int -> unit = "caml_ml_seek_in"
external pos_in : in_channel -> int = "caml_ml_pos_in"
external in_channel_length : in_channel -> int = "caml_ml_channel_size"
external close_in : in_channel -> unit = "caml_ml_close_channel"
external set_binary_mode_in : in_channel -> bool -> unit
                            = "caml_ml_set_binary_mode"
external seek_out : out_channel -> int64 -> unit = "caml_ml_seek_out_64"
external pos_out : out_channel -> int64 = "caml_ml_pos_out_64"
external out_channel_length : out_channel -> int64
                            = "caml_ml_channel_size_64"
external seek_in : in_channel -> int64 -> unit = "caml_ml_seek_in_64"
external pos_in : in_channel -> int64 = "caml_ml_pos_in_64"
external in_channel_length : in_channel -> int64 = "caml_ml_channel_size_64"
external sys_exit : int -> 'a = "caml_sys_exit"
*/
static ocaml_value_t caml_ml_set_channel_name(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_ml_set_channel_name unimplemented"); }
static ocaml_value_t caml_ml_flush(ocaml_value_t v) { assert(false && "caml_ml_flush unimplemented"); }
static ocaml_value_t caml_ml_out_channels_list(ocaml_value_t v) { assert(false && "caml_ml_out_channels_list unimplemented"); }

static ocaml_value_t caml_ml_output(ocaml_value_t v1, ocaml_value_t v2, ocaml_value_t v3, ocaml_value_t v4) { assert(false && "caml_ml_output unimplemented"); }
static ocaml_value_t caml_ml_output_char(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_ml_output_char unimplemented"); }
static ocaml_value_t caml_ml_output_int(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_ml_output_int unimplemented"); }
static ocaml_value_t caml_output_value(ocaml_value_t v1, ocaml_value_t v2, ocaml_value_t v3) { assert(false && "caml_ml_output_value unimplemented"); }

static ocaml_value_t caml_ml_input(ocaml_value_t v1, ocaml_value_t v2, ocaml_value_t v3, ocaml_value_t v4) { assert(false && "caml_ml_input unimplemented"); }
static ocaml_value_t caml_ml_input_scan_line(ocaml_value_t v) { assert(false && "caml_ml_input_scan_line unimplemented"); }
static ocaml_value_t caml_ml_input_char(ocaml_value_t v) { assert(false && "caml_ml_input_char unimplemented"); }
static ocaml_value_t caml_ml_input_int(ocaml_value_t v) { assert(false && "caml_ml_input_int unimplemented"); }
static ocaml_value_t caml_input_value(ocaml_value_t v) { assert(false && "caml_input_value unimplemented"); }

static ocaml_value_t caml_ml_close_channel(ocaml_value_t v) { assert(false && "caml_ml_close_channel unimplemented"); }
static ocaml_value_t caml_ml_set_binary_mode(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_ml_set_binary_mode unimplemented"); }

static ocaml_value_t caml_ml_seek_in(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_ml_seek_in unimplemented"); }
static ocaml_value_t caml_ml_pos_in(ocaml_value_t v) { assert(false && "caml_ml_pos_in unimplemented"); }
static ocaml_value_t caml_ml_seek_out(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_ml_seek_out unimplemented"); }
static ocaml_value_t caml_ml_pos_out(ocaml_value_t v) { assert(false && "caml_ml_pos_out unimplemented"); }
static ocaml_value_t caml_ml_channel_size(ocaml_value_t v) { assert(false && "caml_ml_channel_size unimplemented"); }

static ocaml_value_t caml_ml_seek_in_64(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_ml_seek_in_64 unimplemented"); }
static ocaml_value_t caml_ml_pos_in_64(ocaml_value_t v) { assert(false && "caml_ml_pos_in_64 unimplemented"); }
static ocaml_value_t caml_ml_seek_out_64(ocaml_value_t v1, ocaml_value_t v2) { assert(false && "caml_ml_seek_out_64 unimplemented"); }
static ocaml_value_t caml_ml_pos_out_64(ocaml_value_t v) { assert(false && "caml_ml_pos_out_64 unimplemented"); }
static ocaml_value_t caml_ml_channel_size_64(ocaml_value_t v) { assert(false && "caml_ml_channel_size_64 unimplemented"); }

static ocaml_value_t caml_sys_exit(ocaml_value_t v) {
    exit(GET_I(v));
}

#endif
