
// #define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

extern SEXP set_intensity_(SEXP fill_, SEXP frac_);

static const R_CallMethodDef CEntries[] = {

  {"set_intensity_", (DL_FUNC) &set_intensity_, 2},
  {NULL , NULL, 0}
};


void R_init_isocubes(DllInfo *info) {
  R_registerRoutines(
    info,      // DllInfo
    NULL,      // .C
    CEntries,  // .Call
    NULL,      // Fortran
    NULL       // External
  );
  R_useDynamicSymbols(info, FALSE);
}



