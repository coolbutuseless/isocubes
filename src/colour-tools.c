
#define R_NO_REMAP

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#include "hash-color.h"

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Hashed colour lookup from 'hash-color.c'
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
extern char *col_name[];
extern uint8_t col_int[][4];




#define hex2nibble(x) ( (((x) & 0xf) + ((x) >> 6) + ((x >> 6) << 3)) & 0xf )


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
    
    int red, green, blue;
    
    const char *color = CHAR(STRING_ELT(fill_, i));
    unsigned long nc = strlen(color);
    bool is_hex = false;
    if (color[0] == '#') {
      is_hex = true;
      if (nc != 7 && nc != 9) {
        Rf_error("hash colour must be #rrggbb or #rrggbbaa");  
      }
      
      red   = (uint8_t)( (hex2nibble(color[1]) << 4) + hex2nibble(color[2]) ); // R
      green = (uint8_t)( (hex2nibble(color[3]) << 4) + hex2nibble(color[4]) ); // G
      blue  = (uint8_t)( (hex2nibble(color[5]) << 4) + hex2nibble(color[6]) ); // B
    } else {
      int idx = hash_color((const unsigned char *)color);
      if (idx < 0 || idx > 658 || memcmp(color, col_name[idx], 2) != 0) {
        Rf_error("Not a valid colour name: %s", color);
      }
      uint8_t *vals = col_int[idx];
      red   = vals[0];
      green = vals[1];
      blue  = vals[2];
      col[7] = 'F';
      col[8] = 'F';
    }
    
    red   = (double)(frac * (double)red);
    green = (double)(frac * (double)green);
    blue  = (double)(frac * (double)blue);
    
    static const char hex_lookup[]= "0123456789ABCDEF"; // Lookup table
    col[1] = hex_lookup[(red    >>  4) & 0x0F];
    col[2] = hex_lookup[(red    >>  0) & 0x0F];
    col[3] = hex_lookup[(green  >>  4) & 0x0F];
    col[4] = hex_lookup[(green  >>  0) & 0x0F];
    col[5] = hex_lookup[(blue   >>  4) & 0x0F];
    col[6] = hex_lookup[(blue   >>  0) & 0x0F];
    
    if (is_hex) {
      if (nc == 9) {
        col[7] = color[7];
        col[8] = color[8];
      } else {
        col[7] = 'F';
        col[8] = 'F';
      }
    }
    
    SET_STRING_ELT(out_fill_, i, Rf_mkChar(col));
  } 
  

  
  UNPROTECT(1);
  return out_fill_;
}


