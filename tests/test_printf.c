#include <stdlib.h>
#include <stdint.h>
#include "liballocs.h"

/*
global decls:
*/

void Printf__init();

extern boxed_pointer_t* Printf;

/*
deinlined functions:
*/

/*
fixed toplevels:
*/

boxed_pointer_t* Test_printf;

/*
the module constructor:
*/

void Test_printf__init(){
if (Test_printf) {
return;
} else {
Printf__init();
Printf;
boxed_pointer_t __structured_constant_1232 = malloc(sizeof(boxed_pointer_t)*2);
boxed_pointer_t __structured_constant_1233 = malloc(sizeof(boxed_pointer_t)*2);
__structured_constant_1233[0] = "static\n";
__structured_constant_1233[1] = ((void*)0);
__structured_constant_1232[0] = __structured_constant_1233;
__structured_constant_1232[1] = "static\n";
((boxed_pointer_t(*)(boxed_pointer_t,...))Printf[1])(__structured_constant_1232);
boxed_pointer_t __structured_constant_1234 = malloc(sizeof(boxed_pointer_t)*2);
boxed_pointer_t __structured_constant_1235 = malloc(sizeof(boxed_pointer_t)*2);
__structured_constant_1235[0] = "integer ";
boxed_pointer_t __structured_constant_1236 = malloc(sizeof(boxed_pointer_t)*4);
__structured_constant_1236[0] = ((void*)0);
__structured_constant_1236[1] = ((void*)0);
__structured_constant_1236[2] = ((void*)0);
boxed_pointer_t __structured_constant_1237 = malloc(sizeof(boxed_pointer_t)*2);
__structured_constant_1237[0] = '\n';
__structured_constant_1237[1] = ((void*)0);
__structured_constant_1236[3] = __structured_constant_1237;
__structured_constant_1235[1] = __structured_constant_1236;
__structured_constant_1234[0] = __structured_constant_1235;
__structured_constant_1234[1] = "integer %d\n";
((boxed_pointer_t(*)(boxed_pointer_t,...))Printf[1])(__structured_constant_1234, 1);
boxed_pointer_t __structured_constant_1238 = malloc(sizeof(boxed_pointer_t)*2);
boxed_pointer_t __structured_constant_1239 = malloc(sizeof(boxed_pointer_t)*2);
__structured_constant_1239[0] = "string ";
boxed_pointer_t __structured_constant_1240 = malloc(sizeof(boxed_pointer_t)*2);
__structured_constant_1240[0] = ((void*)0);
boxed_pointer_t __structured_constant_1241 = malloc(sizeof(boxed_pointer_t)*2);
__structured_constant_1241[0] = '\n';
__structured_constant_1241[1] = ((void*)0);
__structured_constant_1240[1] = __structured_constant_1241;
__structured_constant_1239[1] = __structured_constant_1240;
__structured_constant_1238[0] = __structured_constant_1239;
__structured_constant_1238[1] = "string %s\n";
((boxed_pointer_t(*)(boxed_pointer_t,...))Printf[1])(__structured_constant_1238, "hi");
boxed_pointer_t __makeblock_1242 = malloc(sizeof(boxed_pointer_t)*0);
Test_printf = __makeblock_1242;
}
}

