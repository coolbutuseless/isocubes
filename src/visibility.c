
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#include "utils.h"


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// REturn a data.frame of information about the per-face visibliity of
// voxels which are viaible at all
//
// @param x,y,z voxel coordinates
//
// Steps:
// 1. calculate the hashed coordinates of each voxel
//    This renders (x,y,z) to (xc,yc) which are basically the isometric
//    coordinates of the voxel.  It differes from the actual coordinates in 
//    that I've rigged the xc/yc calculation so that they're integers only
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SEXP visibility_(SEXP x_, SEXP y_, SEXP z_) {
  
  int nprotect = 0;
  
  int N = length(x_);
  if (length(y_) != N || length(z_) != N) {
    error("All of x,y,z must the same length");
  }
  
  int *x = INTEGER(x_);
  int *y = INTEGER(y_);
  int *z = INTEGER(z_);
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Keep track of the ranges of the hashed coordinates
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  int xc_max = -INT_MAX;
  int yc_max = -INT_MAX;
  int xc_min =  INT_MAX;
  int yc_min =  INT_MAX;
  
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Allocate and calculate the hashed coordiantes
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Determine the size of the hashed coordinate matrix
  // This matrix will be used to track if a voxel is being drawn at the same
  // position on screen.   Resolution of which voxel to draw is governed by 
  // the relative sizes of the original (x,y,z) coordianates.
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
      mat[row][col] = -1; // Sentiel value: -1 = empty
    }
  }
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // For each hashed coordiante
  //   - locate its slot in the matrix
  //   - if the slot is empty: insert this index
  //   - if the slot already contains an index, work out whether this new
  //       voxel is in front of it, and if so, replace the index with this
  //       new voxel's index
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  int nvisible = 0;
  for (int i = 0; i < N; i++) {
    int row = yc[i] - yc_min;
    int col = xc[i] - xc_min;
    if (col >= cwidth || row >= cheight || col < 0 || row < 0) {
      error("[%i, %i] / [%i, %i]\n", col, row, cheight, cwidth);
    }
    int val = mat[row][col];
    if (val < 0) {
      mat[row][col] = i;
      nvisible++;
    } else {
      int cidx = mat[row][col];
      if (y[i] > y[cidx] && z[i] < z[cidx] && x[i] < x[cidx]) {
        mat[row][col] = i;
      }
    }
  }
  
  // for (int row = cheight - 1; row >= 0; row--) {
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
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Create a data.frame of
  //  - idx: indices of the 'coords' data.frame which are visible
  //  - type: bitset of which faces are visible 001 = top, 010 = left, 100 = right
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SEXP visible_idx_ = PROTECT(allocVector(INTSXP, nvisible)); nprotect++;
  SEXP vis_type_    = PROTECT(allocVector(INTSXP, nvisible)); nprotect++;
  int *visible_idx = INTEGER(visible_idx_);
  int *vis_type    = INTEGER(vis_type_);
  int vidx = 0;
  for (int row = 0; row < cheight; row++) {
    for (int col = 0; col < cwidth; col++) {
      int this_idx = mat[row][col];
      if (this_idx >= 0) {
        visible_idx[vidx] = this_idx + 1; // convert to R 1-indexing
        
        bool visible_top   = true;
        bool visible_left  = true;
        bool visible_right = true;
        
        // Top visibliti
        if (row < (cheight - 2)) {
          int above_idx = mat[row + 2][col];
          if (above_idx >= 0) {
            if (y[above_idx] > y[this_idx]) {
              visible_top = false;
            }
          }
        }
        
        // Left visibility
        if (row > 0 && col > 0) {
          int left_idx = mat[row - 1][col - 1];
          if (left_idx >= 0) {
            if (x[left_idx] < x[this_idx]) {
              visible_left = false;
            }
          }
        }
        
        // right visibility
        if (row > 0 && col < (cwidth - 1)) {
          int right_idx = mat[row - 1][col + 1];
          if (right_idx >= 0) {
            if (z[right_idx] < z[this_idx]) {
              visible_right = false;
            }
          }
        }
        
        
        vis_type[vidx] =
          visible_top        |
          visible_left  << 1 |
          visible_right << 2;
        
        vidx++;
      }
    }
  }
  
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Create data.frame to return
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SEXP res_ = PROTECT(
    create_named_list(2, "idx", visible_idx_, "type", vis_type_)
  ); nprotect++;
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