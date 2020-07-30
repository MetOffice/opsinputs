!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Reads an ODB merge control namelist.  Name of the ODB control namelist is taken
! from OPS_ODBMERGE_NL.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ReadODBMergeNL (table_mapping,  &
                               column_mapping, &
                               set_columns,    &
                               varno_swap,     &
                               delete_tables)

USE GenMod_Control, ONLY: &
    mype,                 &
    GeneralMode,          &
    DiagnosticMode

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    MessageOut,        &
    StatusOK,          &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE OpsMod_IOUtils, ONLY: &
    Ops_CloseFile,        &
    Ops_OpenEnvRead,      &
    Ops_OpenFileRead

USE OpsMod_ODBTypes, ONLY: &
    ColumnValuePair_type,  &
    max_len_colname,       &
    max_len_tablename,     &
    max_len_varname

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), ALLOCATABLE, INTENT(OUT)            :: table_mapping(:,:)
CHARACTER(len=*), ALLOCATABLE, INTENT(OUT)            :: column_mapping(:,:)
TYPE (ColumnValuePair_type), ALLOCATABLE, INTENT(OUT) :: set_columns(:)
CHARACTER(len=*), ALLOCATABLE, INTENT(OUT)            :: varno_swap(:,:)
CHARACTER(len=*), ALLOCATABLE, INTENT(OUT)            :: delete_tables(:)

! Local declarations:
CHARACTER(len=*), PARAMETER                           :: RoutineName = "Ops_ReadODBMergeNL"
INTEGER                                               :: Unit
CHARACTER(len=max_len_tablename)                      :: mapping_in(20)
CHARACTER(len=max_len_tablename)                      :: mapping_out(20)
CHARACTER(len=max_len_colname)                        :: column_mapping_in(20)
CHARACTER(len=max_len_colname)                        :: column_mapping_out(20)
CHARACTER(len=max_len_colname)                        :: set_column_names(20)
REAL                                                  :: set_column_vals(20)
CHARACTER(len=max_len_varname)                        :: varno_swap_in(20)
CHARACTER(len=max_len_varname)                        :: varno_swap_out(20)
CHARACTER(len=max_len_tablename)                      :: delete_table(20)
INTEGER                                               :: num_maps
INTEGER                                               :: map

NAMELIST / ODBMergeNL / &
  column_mapping_in,    &
  column_mapping_out,   &
  delete_table,         &
  mapping_in,           &
  mapping_out,          &
  set_column_names,     &
  set_column_vals,      &
  varno_swap_in,        &
  varno_swap_out

!-----------------------------------------------------------------------------
! 0. Setup and tracing
!-----------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-----------------------------------------------------------------------------
! 1. Open the namelist
!-----------------------------------------------------------------------------

column_mapping_in = ""
column_mapping_out = ""
mapping_in = ""
mapping_out = ""
set_column_names = ""
set_column_vals = RMDI
varno_swap_in = ""
varno_swap_out = ""
delete_table = ""

CALL Ops_OpenEnvRead ("OPS_ODBMERGE_NL",                &
                      Unit,                             &
                      status_if_env_not_set = StatusOK)
IF (Unit <= 0) THEN
  CALL Ops_OpenFileRead ("ops_odb_merge.nl",               &
                         Unit,                             &
                         status_if_open_failed = StatusOK)
END IF

IF (Unit > 0) THEN
  !-----------------------------------------------------------------------------
  ! 2. Read the namelist to obtain extraction info.
  !-----------------------------------------------------------------------------

  READ (Unit, NML = ODBMergeNL)
  IF (GeneralMode >= DiagnosticMode .AND. mype == 0) THEN
    WRITE (MessageOut, NML = ODBMergeNL)
  END IF

  CALL Ops_CloseFile (Unit)

  IF (ANY (mapping_in /= "") .AND. ANY (mapping_out /= "")) THEN
    num_maps = MIN (COUNT (mapping_in /= ""), COUNT (mapping_out /= ""))
    ALLOCATE (table_mapping(num_maps,2))
    DO map = 1, num_maps
      IF (INDEX (mapping_in(map), "@") > 0) THEN
        table_mapping(map,1) = mapping_in(map)
      ELSE
        table_mapping(map,1) = "@" // mapping_in(map)
      END IF
      IF (INDEX (mapping_out(map), "@") > 0) THEN
        table_mapping(map,2) = mapping_out(map)
      ELSE
        table_mapping(map,2) = "@" // mapping_out(map)
      END IF
    END DO
  END IF

  IF (ANY (column_mapping_in /= "") .AND. ANY (column_mapping_out /= "")) THEN
    num_maps = MIN (COUNT (column_mapping_in /= ""), COUNT (column_mapping_out /= ""))
    ALLOCATE (column_mapping(num_maps,2))
    DO map = 1, num_maps
      column_mapping(map,1) = column_mapping_in(map)
      column_mapping(map,2) = column_mapping_out(map)
    END DO
  END IF

  IF (ANY (varno_swap_in /= "") .AND. ANY (varno_swap_out /= "")) THEN
    num_maps = MIN (COUNT (varno_swap_in /= ""), COUNT (varno_swap_out /= ""))
    ALLOCATE (varno_swap(num_maps,2))
    DO map = 1, num_maps
      varno_swap(map,1) = varno_swap_in(map)
      varno_swap(map,2) = varno_swap_out(map)
    END DO
  END IF

  IF (ANY (set_column_names /= "") .AND. ANY (set_column_vals /= RMDI)) THEN
    num_maps = MIN (COUNT (set_column_names /= ""), COUNT (set_column_vals /= RMDI))
    ALLOCATE (set_columns(num_maps))
    DO map = 1, num_maps
      set_columns(map) % colname = set_column_names(map)
      set_columns(map) % value = set_column_vals(map)
    END DO
  END IF

  IF (ANY (delete_table /= "")) THEN
    num_maps = COUNT (delete_table /= "")
    ALLOCATE (delete_tables(num_maps))
    DO map = 1, num_maps
      delete_tables(map) = "@" // delete_table(map)
    END DO
  END IF
END IF

!-------------------------------------------------------------------------------
! 3. Error reporting, tracing, and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ReadODBMergeNL
