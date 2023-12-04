!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Program to merge one ODB into another ODB.  Each table from the ODB is read
! and written to the ECMA with any special modifications required.
!-------------------------------------------------------------------------------

PROGRAM OpsProg_MergeODB

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  gen_trace_report,    &
  UseTrace

USE GenMod_Setup, ONLY: &
  Gen_SetupControl

USE ops_odb_wrappers, ONLY: &
  ops_odb_addpools,         &
  ops_odb_cancel,           &
  ops_odb_get_column_names, &
  ops_odb_get_column_types, &
  ops_odb_get_num_columns,  &
  ops_odb_get_num_tables,   &
  ops_odb_get_table_names,  &
  ops_odb_getval,           &
  ops_odb_init,             &
  ops_odb_varindex

USE OpsMod_Control, ONLY: &
  DefaultDocURL,          &
  Ops_InitMPI

USE OpsMod_Gcom, ONLY: &
  gc_exit

USE OpsMod_ODBKinds, ONLY: &
  odb_real

USE OpsMod_ODBTypes, ONLY:    &
  ColumnValuePair_type,       &
  max_len_col_and_table_name, &
  max_len_colname,            &
  max_len_tablename,          &
  max_len_varname,            &
  ODBSession_type

USE OpsMod_ODBUtils, ONLY: &
  Ops_CloseODB,            &
  Ops_OpenODB,             &
  Ops_ReadODBControlNL,    &
  Ops_ReadODBMergeNL,      &
  Ops_RunODBQuery,         &
  Ops_WriteODBTable,       &
  OpsFn_ODBExists

IMPLICIT NONE

! Local declarations:
CHARACTER(len=*), PARAMETER                            :: ProgName = "OpsProg_MergeODB"
INTEGER                                                :: num_tables_in
INTEGER                                                :: num_tables_out
INTEGER                                                :: i
REAL(kind=odb_real), POINTER                           :: data_in(:,:)
REAL(kind=odb_real), ALLOCATABLE                       :: data_out(:,:)
CHARACTER(len=100), ALLOCATABLE                        :: tables_in(:)
CHARACTER(len=100), ALLOCATABLE                        :: tables_out(:)
CHARACTER(len=100)                                     :: query
CHARACTER(len=100)                                     :: target_table
TYPE (ODBSession_type)                                 :: ecma_in
TYPE (ODBSession_type)                                 :: ecma_out
INTEGER                                                :: in_pool
INTEGER                                                :: num_new_pools
INTEGER                                                :: num_columns_in
INTEGER                                                :: num_columns_out
CHARACTER(len=max_len_col_and_table_name), ALLOCATABLE :: colnames_in(:)
CHARACTER(len=max_len_col_and_table_name), ALLOCATABLE :: colnames_out(:)
CHARACTER(len=max_len_col_and_table_name), ALLOCATABLE :: coltypes_in(:)
CHARACTER(len=max_len_col_and_table_name), ALLOCATABLE :: coltypes_out(:)
CHARACTER(len=max_len_col_and_table_name)              :: target_column
INTEGER                                                :: col_in
INTEGER                                                :: col_out
CHARACTER(len=max_len_tablename), ALLOCATABLE          :: table_mappings(:,:)
CHARACTER(len=max_len_colname), ALLOCATABLE            :: column_mappings(:,:)
CHARACTER(len=max_len_varname), ALLOCATABLE            :: varno_swaps(:,:)
INTEGER                                                :: j
INTEGER                                                :: k
TYPE (ColumnValuePair_type), ALLOCATABLE               :: set_columns(:)
CHARACTER(len=max_len_tablename), ALLOCATABLE          :: delete_tables(:)
INTEGER                                                :: varno_column
REAL                                                   :: varnos_for_swap(2)

CALL Gen_SetupControl (DefaultDocURL)

CALL Ops_InitMPI

IF (UseTrace) CALL gen_trace_entry (ProgName)

CALL Ops_ReadODBControlNL

CALL Ops_ReadODBMergeNL (table_mappings,  &
                         column_mappings, &
                         set_columns,     &
                         varno_swaps,     &
                         delete_tables)

CALL ops_odb_init

CALL Ops_OpenODB (ecma_in,    &
                  "READONLY", &
                  "ECMATMP")
num_new_pools = SIZE (ecma_in % pool_write_list)

IF (OpsFn_ODBExists ("ECMA")) THEN
  CALL Ops_OpenODB (ecma_out, &
                    "OLD",    &
                    "ECMA")
  CALL ops_odb_addpools (ecma_out,                         &
                         new_pools_to_add = num_new_pools)
ELSE
  CALL Ops_OpenODB (ecma_out,                        &
                    "NEW",                           &
                    "ECMA",                          &
                    npools = INT (ecma_in % npools))
END IF

num_tables_in = ops_odb_get_num_tables (ecma_in)
ALLOCATE (tables_in(num_tables_in))
CALL ops_odb_get_table_names (ecma_in,   &
                              tables_in)

num_tables_out = ops_odb_get_num_tables (ecma_out)
ALLOCATE (tables_out(num_tables_out))
CALL ops_odb_get_table_names (ecma_out,   &
                              tables_out)

! 2. For each table, do
DO in_pool = 1, SIZE (ecma_in % pool_write_list)
  DO i = 1, num_tables_in
    target_table = tables_in(i)
    IF (ALLOCATED (delete_tables)) THEN
      IF (ANY (delete_tables == target_table)) CYCLE
    END IF
    IF (ALLOCATED (table_mappings)) THEN
      DO j = 1, SIZE (table_mappings, DIM = 1)
        IF (table_mappings(j,1) == tables_in(i)) THEN
          target_table = table_mappings(j,2)
          EXIT
        END IF
      END DO
    END IF
    query = TRIM (tables_in(i))
    CALL Ops_RunODBQuery (ecma_in,                                     &
                          query,                                       &
                          data_in,                                     &
                          poolno = ecma_in % pool_write_list(in_pool))

    IF (ASSOCIATED (data_in)) THEN

      num_columns_in = SIZE (data_in, DIM = 2) - 1
      num_columns_out = ops_odb_get_num_columns (ecma_out, target_table)
      ALLOCATE (colnames_in(num_columns_in))
      ALLOCATE (colnames_out(num_columns_out))
      ALLOCATE (coltypes_in(num_columns_in))
      ALLOCATE (coltypes_out(num_columns_out))
      CALL ops_odb_get_column_names (ecma_in,     &
                                     query,       &
                                     colnames_in)
      CALL ops_odb_get_column_types (ecma_in,     &
                                     query,       &
                                     coltypes_in)
      CALL ops_odb_get_column_names (ecma_out,     &
                                     target_table, &
                                     colnames_out)
      CALL ops_odb_get_column_types (ecma_out,     &
                                     target_table, &
                                     coltypes_out)

      CALL ops_odb_cancel (ecma_in, &
                           query)

      ALLOCATE (data_out(SIZE (data_in, DIM = 1), 0:num_columns_out))
      data_out = ecma_out % odb_mdi

      DO col_in = 1, SIZE (colnames_in)
        target_column = colnames_in(col_in)
        IF (ALLOCATED (column_mappings)) THEN
          DO j = 1, SIZE (column_mappings, DIM = 1)
            IF (column_mappings(j,1) == target_column) THEN
              target_column = column_mappings(j,2)
              EXIT
            END IF
          END DO
        END IF
        DO col_out = 1, SIZE (colnames_out)
          IF (target_column == colnames_out(col_out)) THEN
            IF (ALLOCATED (set_columns)) THEN
              IF (ANY (set_columns % colname == target_column)) THEN
                DO k = 1, SIZE (set_columns)
                  IF (set_columns(k) % colname == target_column) THEN
                    data_out(:,col_out) = set_columns(k) % value
                    EXIT
                  END IF
                END DO
              ELSE
                data_out(:,col_out) = data_in(:,col_in)
              END IF
            ELSE
              data_out(:,col_out) = data_in(:,col_in)
            END IF
            EXIT
          END IF
        END DO
      END DO
      IF (target_table == "@body") THEN
        IF (ALLOCATED (varno_swaps)) THEN
          CALL ops_odb_varindex (ecma_out,     &
                                 target_table, &
                                 "varno",      &
                                 varno_column)
          DO j = 1, SIZE (varno_swaps, DIM = 1)
            varnos_for_swap(1) = ops_odb_getval (ecma_out, varno_swaps(j,1))
            varnos_for_swap(2) = ops_odb_getval (ecma_out, varno_swaps(j,2))
            DO k = 1, SIZE (data_out, DIM = 1)
              IF (NINT (data_out(k,varno_column)) == NINT (varnos_for_swap(1))) THEN
                data_out(k,varno_column) = varnos_for_swap(2)
              ELSE IF (NINT (data_out(k,varno_column)) == NINT (varnos_for_swap(2))) THEN
                data_out(k,varno_column) = varnos_for_swap(1)
              END IF
            END DO
          END DO
        END IF
      END IF
      CALL Ops_WriteODBTable (ecma_out,                                     &
                              target_table,                                 &
                              data_out,                                     &
                              poolno = ecma_out % pool_write_list(in_pool))
      DEALLOCATE (data_in)
      DEALLOCATE (data_out)
      DEALLOCATE (colnames_in)
      DEALLOCATE (colnames_out)
      DEALLOCATE (coltypes_in)
      DEALLOCATE (coltypes_out)
    END IF
  END DO
END DO

DEALLOCATE (tables_in)

CALL Ops_CloseODB (ecma_in, &
                   .FALSE.)
CALL Ops_CloseODB (ecma_out)

IF (UseTrace) THEN
  CALL gen_trace_exit (ProgName)
  CALL gen_trace_report
END IF

CALL gc_exit

END PROGRAM OpsProg_MergeODB
