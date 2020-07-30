!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper module to provide interfaces to and simplified versions of ODB
! functions and subroutines used by OPS.
!-------------------------------------------------------------------------------

MODULE ops_odb_wrappers

USE OpsMod_ODBKinds, ONLY: &
  odb_int

IMPLICIT NONE

INTEGER(kind=odb_int), PARAMETER :: odb_all_pools = -1_odb_int

INTERFACE ops_odb_varindex
  MODULE PROCEDURE ops_odb_varindex_scalar
END INTERFACE ops_odb_varindex

CONTAINS

#include "ops_odb_addpools.inc"
#include "ops_odb_cancel.inc"
#include "ops_odb_close.inc"
#include "ops_odb_get.inc"
#include "ops_odb_get_column_names.inc"
#include "ops_odb_get_num_columns.inc"
#include "ops_odb_get_query_size.inc"
#include "ops_odb_getsize.inc"
#include "ops_odb_getval.inc"
#include "ops_odb_init.inc"
#include "ops_odb_is_column_available.inc"
#include "ops_odb_is_query_valid.inc"
#include "ops_odb_open.inc"
#include "ops_odb_varindex_scalar.inc"
#include "ops_odb_put.inc"
#include "ops_odb_register_vector.inc"

#ifndef NO_ODB
#include "ops_odb_get_column_types.inc"
#include "ops_odb_get_num_tables.inc"
#include "ops_odb_get_table_names.inc"
#include "ops_odb_getval_hardwired.inc"
#endif

END MODULE ops_odb_wrappers
