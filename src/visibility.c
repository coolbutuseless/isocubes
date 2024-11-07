
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
    yc[i] = 2 * y[i] + x[i] + z[i];
    if (yc[i] > yc_max) { yc_max = yc[i]; }
    if (yc[i] < yc_min) { yc_min = yc[i]; }
  }
  
  int cwidth  = xc_max - xc_min + 1;
  int cheight = yc_max - yc_min + 1;
  
  int **mat = malloc(cheight * sizeof(int *));
  if (mat == NULL) {
    error("**mat malloc");
  }
  
  for (int row = 0; row < cheight; row++) {
    mat[row] = malloc(cwidth * sizeof(int));
    if (mat[row] == NULL) {
      error("*mat malloc");
    }
    for (int col = 0; col < cwidth; col++) {
      mat[row][col] = -1;
    }
  }
  
  for (int i = 0; i < N; i++) {
    int row = yc[i] - yc_min;
    int col = xc[i] - xc_min;
    if (col >= cwidth || row >= cheight || col < 0 || row < 0) {
      error("[%i, %i] / [%i, %i]\n", col, row, cheight, cwidth);
    }
    int val = mat[row][col];
    if (val < 0) {
      mat[row][col] = i;
    } else {
      int cidx = mat[row][col];
      if (y[i] > y[cidx] && z[i] < z[cidx] && x[i] < x[cidx]) {
        mat[row][col] = i;
      }
    }
  }
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Extract visible indices
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  int nvisible = 0;
  for (int row = 0; row < cheight; row++) {
    for (int col = 0; col < cwidth; col++) {
      nvisible += (mat[row][col] >= 0);
    }
  }
  
  SEXP res_ = PROTECT(allocVector(INTSXP, nvisible));
  int *res = INTEGER(res_);
  int vidx = 0;
  for (int row = 0; row < cheight; row++) {
    for (int col = 0; col < cwidth; col++) {
      if (mat[row][col] >= 0) {
        res[vidx] = mat[row][col] + 1;
        vidx++;
      }
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
  UNPROTECT(1);
  return res_;
}