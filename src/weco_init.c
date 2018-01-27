#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void getrl(void *, void *, void *);
extern void rule1(void *, void *, void *, void *, void *, void *);
extern void rule2(void *, void *, void *, void *, void *);
extern void rule3(void *, void *, void *, void *, void *);
extern void rule4(void *, void *, void *, void *, void *);
extern void rule5(void *, void *, void *, void *, void *, void *, void *, void *);
extern void rule7(void *, void *, void *, void *, void *, void *, void *);
extern void rule8(void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"getrl", (DL_FUNC) &getrl, 3},
    {"rule1", (DL_FUNC) &rule1, 6},
    {"rule2", (DL_FUNC) &rule2, 5},
    {"rule3", (DL_FUNC) &rule3, 5},
    {"rule4", (DL_FUNC) &rule4, 5},
    {"rule5", (DL_FUNC) &rule5, 8},
    {"rule7", (DL_FUNC) &rule7, 7},
    {"rule8", (DL_FUNC) &rule8, 7},
    {NULL, NULL, 0}
};

void R_init_weco(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, TRUE);
}
