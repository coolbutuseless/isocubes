
#define R_NO_REMAP

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
// @param df_ named list object which is to be promoted to data.frame
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void set_df_attributes(SEXP df_) {
  int nprotect = 0;
  
  if (!Rf_isNewList(df_)) {
    Rf_error("set_df_attributes(): only accepts 'lists' as input");
  }
  
  int len = Rf_length(VECTOR_ELT(df_, 0));
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Set row.names
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SEXP rownames = PROTECT(Rf_allocVector(INTSXP, 2)); nprotect++;
  SET_INTEGER_ELT(rownames, 0, NA_INTEGER);
  SET_INTEGER_ELT(rownames, 1, -len);
  Rf_setAttrib(df_, R_RowNamesSymbol, rownames);
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Set as tibble
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SEXP classnames = PROTECT(Rf_allocVector(STRSXP, 3)); nprotect++;
  SET_STRING_ELT(classnames, 0, Rf_mkChar("tbl_df"));
  SET_STRING_ELT(classnames, 1, Rf_mkChar("tbl"));
  SET_STRING_ELT(classnames, 2, Rf_mkChar("data.frame"));
  SET_CLASS(df_, classnames);
  
  UNPROTECT(nprotect);
} 



//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Create a named list
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SEXP create_named_list(int n, ...) {
  
  va_list args;
  va_start(args, n);
  
  int nprotect = 0;
  SEXP res_ = PROTECT(Rf_allocVector(VECSXP, n)); nprotect++;
  SEXP nms_ = PROTECT(Rf_allocVector(STRSXP, n)); nprotect++;
  Rf_setAttrib(res_, R_NamesSymbol, nms_);
  
  for (int i = 0; i < n; i++) {
    
    const char *nm = va_arg(args, const char *);
    SEXP val_ = va_arg(args, SEXP);
    
    SET_STRING_ELT(nms_, i, Rf_mkChar(nm));
    SET_VECTOR_ELT(res_, i, val_);
  }
  
  
  va_end(args);
  UNPROTECT(nprotect);
  return res_;
}

