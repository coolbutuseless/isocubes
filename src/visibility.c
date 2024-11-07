
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#include "utils.h"


SEXP visibility_(SEXP x_, SEXP y_, SEXP z_) {
  
  int nprotect = 0;
  
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
  // Extract indices of visible cubes
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  int nvisible = 0;
  for (int row = 0; row < cheight; row++) {
    for (int col = 0; col < cwidth; col++) {
      nvisible += (mat[row][col] >= 0);
    }
  }
  
  // for (int row = 0; row < cheight; row++) {
  //   Rprintf("[%i] ", row);
  //   for (int col = 0; col < cwidth; col++) {
  //     if (mat[row][col] >= 0) {
  //       Rprintf("%4i",mat[row][col] + 1);
  //     } else {
  //       Rprintf("   .");
  //     }
  //   }
  //   Rprintf("\n");
  // }
  
  
  SEXP visible_idx_ = PROTECT(allocVector(INTSXP, nvisible)); nprotect++;
  SEXP vis_type_    = PROTECT(allocVector(INTSXP, nvisible)); nprotect++;
  int *visible_idx = INTEGER(visible_idx_);
  int *vis_type    = INTEGER(vis_type_);
  int vidx = 0;
  for (int row = 0; row < cheight; row++) {
    bool visible_top0 = (row >= (cheight - 2));
    bool visible_lr0  = (row == 0);
    for (int col = 0; col < cwidth; col++) {
      if (mat[row][col] >= 0) {
        visible_idx[vidx] = mat[row][col] + 1; // convert to R 1-indexing
        
        bool visible_right = visible_lr0 || (col == cheight - 1);
        bool visible_left  = visible_lr0 || (col == 0);
        
        bool visible_top  = visible_top0 || (mat[row + 2][col] < 0);
        visible_left  = visible_left  || (mat[row - 1][col - 1] < 0);
        visible_right = visible_right || (mat[row - 1][col + 1] < 0);
        
        vis_type[vidx] = 
          visible_top        |
          visible_left  << 1 |
          visible_right << 2;
        
        vidx++;
      }
    }
  }
  
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Create list to return
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SEXP res_ = PROTECT(create_named_list(2, "idx", visible_idx_, "type", vis_type_)); nprotect++;
  set_df_attributes(res_);
  
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Tidy and return
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (int i = 0; i < cheight; i++) {
    free(mat[i]);
  }
  free(mat);
  
  free(xc);
  free(yc);
  UNPROTECT(nprotect);
  return res_;
}