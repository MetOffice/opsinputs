!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Derived types related to ODB interaction.
!-------------------------------------------------------------------------------

MODULE OpsMod_ODBTypes

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI,                                  &
  RMDI

USE OpsMod_ODBKinds, ONLY: &
  odb_int,                 &
  odb_real

IMPLICIT NONE

SAVE

! Public declarations:
INTEGER, PARAMETER                 :: max_len_dbname = 7
INTEGER, PARAMETER                 :: max_len_varname = 20
INTEGER, PARAMETER                 :: max_len_tablename = 50
INTEGER, PARAMETER                 :: max_len_odb_query = 50
INTEGER, PARAMETER                 :: max_len_colname = 50
INTEGER, PARAMETER                 :: max_len_col_and_table_name = max_len_tablename + max_len_colname
INTEGER, PARAMETER                 :: max_len_odbvars = 50
INTEGER, PARAMETER                 :: max_len_string_odbvars = 50

TYPE ODBSession_type
  INTEGER(kind=odb_int)            :: handle = IMDI
  CHARACTER(len=max_len_dbname)    :: dbname = ""
  REAL(kind=odb_real)              :: odb_mdi = RMDI
  INTEGER(kind=odb_int)            :: npools = IMDI
  INTEGER(kind=odb_int)            :: npools_old = IMDI
  INTEGER(kind=odb_int)            :: npools_local = IMDI
  INTEGER, POINTER                 :: pool_write_list(:) => NULL ()
  LOGICAL                          :: just_opened = .TRUE.
END TYPE ODBSession_type

TYPE QueryInfo_type
  REAL                             :: ops_subtype = RMDI
  REAL                             :: ops_obsgroup = RMDI
  INTEGER                          :: level_number = IMDI
  REAL                             :: cycle_date = RMDI
  REAL                             :: cycle_time = RMDI
  REAL                             :: time_window_before = RMDI
  REAL                             :: time_window_after = RMDI
  INTEGER, ALLOCATABLE             :: satellite_list(:)
  INTEGER                          :: start_index = IMDI
  INTEGER                          :: end_index = IMDI
END TYPE QueryInfo_type

TYPE ColumnValuePair_type
  CHARACTER(len=max_len_colname)   :: colname = ""
  REAL                             :: value = RMDI
END TYPE ColumnValuePair_type

TYPE ODBElemDesp_type
  CHARACTER(len=max_len_tablename) :: TableName = ""              ! table where this column is
  CHARACTER(len=max_len_colname)   :: ColName = ""                ! column name
  INTEGER                          :: colname_array_length = IMDI ! length of colname array (IMDI if scalar)
  INTEGER                          :: Varno = IMDI                ! varno for this obsvalue
  INTEGER                          :: num_repeats = IMDI
  INTEGER                          :: Pressno = IMDI
  INTEGER                          :: related_varnos(20) = IMDI
  INTEGER                          :: also_do(20) = IMDI
  INTEGER                          :: also_do_levels(20) = IMDI
  INTEGER                          :: num_extra_levs = IMDI
  REAL                             :: fixed_vertco_value = RMDI
  INTEGER                          :: fixed_vertco_type = IMDI
  INTEGER                          :: data_type = IMDI
  INTEGER                          :: has_missing = IMDI
END TYPE ODBElemDesp_type

END MODULE OpsMod_ODBTypes
