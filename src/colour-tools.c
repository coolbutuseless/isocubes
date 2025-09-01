
#define R_NO_REMAP

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#include <colorfast.h>

// set_intensity <- function(fill, frac) {
//   if (frac == 1) return(fill)
//     
//     mat <- col2rgb(fill, alpha = TRUE)
//     mat[1:3,] <- mat[1:3,] * frac
//   rgb(mat[1,], mat[2,], mat[3,], mat[4,], maxColorValue = 255)
//     
// }


SEXP set_intensity_(SEXP fill_, SEXP frac_) {
  
  double frac = Rf_asReal(frac_);
  if (frac < 0 || frac > 1) {
    Rf_error("'frac' must be in range [0, 1]. Got: %.2f", frac);
  }
  
  int N = Rf_length(fill_);
  SEXP out_fill_ = PROTECT(Rf_allocVector(STRSXP, N));
  
  char col[10] = "#000000FF"; // template
  for (int i = 0; i < N; i++) {
    
    const char *color = CHAR(STRING_ELT(fill_, i));
    uint32_t cint = col_to_int(color);
    uint8_t *rgba = (uint8_t *)&cint;
    
    rgba[0] = frac * (double)rgba[0];
    rgba[1] = frac * (double)rgba[1];
    rgba[2] = frac * (double)rgba[2];
    
    int_to_col(cint, col);
    SEXP col_ = PROTECT(Rf_mkChar(col));    
    SET_STRING_ELT(out_fill_, i, col_);
    UNPROTECT(1);
  } 
  
  UNPROTECT(1);
  return out_fill_;
}


