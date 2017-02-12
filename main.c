#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>

void *ocaml_printf(void *a[], ...) {
    va_list args;

    va_start(args, a);
    vprintf(a[1], args);
    va_end(args);
}

void **Printf;

void Printf__init() {
    Printf = malloc(2 * sizeof(intptr_t));
    Printf[1] = &ocaml_printf;
}

void Test__init();

int main() {
    Test__init();
    return 0;
}
