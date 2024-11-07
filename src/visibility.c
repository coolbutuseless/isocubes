
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


SEXP visibility_(SEXP x_, SEXP y_, SEXP z_) {
  
  int N = length(x_);
  int *x = INTEGER(x_);
  int *y = INTEGER(y_);
  int *z = INTEGER(z_);
  
  int xc_max = -INT_MAX;
  int yc_max = -INT_MAX;
  int xc_min =  INT_MAX;
  int yc_min =  INT_MAX;
  
  int *xc = malloc(N * sizeof(int));
  int *yc = malloc(N * sizeof(int));
  if (xc == NULL || yc == NULL) {
    error("xc/yc malloc");
  }
  for (int i = 0; i < N; i++) {
    xc[i] = x[i] - z[i];
    if (xc[i] > xc_max) { xc_max = xc[i]; }
    if (xc[i] < xc_min) { xc_min = xc[i]; }
    yc[i] = 2 * y[i] - x[i] + z[i];
    if (yc[i] > yc_max) { yc_max = yc[i]; }
    if (yc[i] < yc_min) { yc_min = yc[i]; }
  }
  
  int cwidth  = xc_max - xc_min + 1;
  int cheight = yc_max - yc_min + 1;
  
  int **mat = malloc(cheight * sizeof(int *));
  if (mat == NULL) {
    error("**mat malloc");
  }
  for (int i = 0; i < cheight; i++) {
    mat[i] = malloc(cwidth * sizeof(int));
    if (mat[i] == NULL) {
      error("*mat malloc");
    }
  }
  
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Tidy and return
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (int i = 0; i < cheight; i++) {
    free(mat[i]);
  }
  free(mat);
  
  free(xc);
  free(yc);
  
  return R_NilValue;
}