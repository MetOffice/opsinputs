!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Constants, namelist options and derived types for use in ODB code.
!-------------------------------------------------------------------------------

MODULE OpsMod_ODBInfo

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE OpsMod_ODBKinds, ONLY: &
  odb_int,                 &
  odb_real

IMPLICIT NONE

SAVE

! Public declarations:
INTEGER, PARAMETER               :: notvarno = -1
INTEGER, PARAMETER               :: notpress = -1
INTEGER(kind=odb_int), PARAMETER :: odb_all_pools = -1_odb_int

! ODB update query stuff
INTEGER, PARAMETER               :: num_update_params = 11
CHARACTER(len=*), PARAMETER      :: update_query_strings(num_update_params) =     &
                                                        (/"$cycle_date        ",  &
                                                          "$cycle_time        ",  &
                                                          "$time_window_before",  &
                                                          "$time_window_after ",  &
                                                          "$level_number      ",  &
                                                          "$ops_subtype       ",  &
                                                          "$ops_obsgroup      ",  &
                                                          "$varno             ",  &
                                                          "$model_level_flag  ",  &
                                                          "$superob_flag      ",  &
                                                          "$runid             "/)
INTEGER, PARAMETER               :: cycle_date = 1
INTEGER, PARAMETER               :: cycle_time = 2
INTEGER, PARAMETER               :: time_window_before = 3
INTEGER, PARAMETER               :: time_window_after = 4
INTEGER, PARAMETER               :: level_number = 5
INTEGER, PARAMETER               :: ops_subtype = 6
INTEGER, PARAMETER               :: ops_obsgroup = 7
INTEGER, PARAMETER               :: varno = 8
INTEGER, PARAMETER               :: model_level_flag = 9
INTEGER, PARAMETER               :: superob_flag = 10
INTEGER, PARAMETER               :: used_in_runid = 11
INTEGER, PARAMETER               :: satid_list = 6
INTEGER, PARAMETER               :: filter_on_satids = 7

! Private declarations:
PRIVATE                          :: odb_int
PRIVATE                          :: odb_real

! Namelist options

LOGICAL                          :: merge_to_existing_odb = .FALSE.
LOGICAL                          :: verbose_odb = .FALSE.
LOGICAL                          :: use_all_pools = .FALSE.
INTEGER                          :: num_batches_for_write = 1
INTEGER                          :: number_of_pools_per_batch = 1
LOGICAL                          :: fail_with_unrecognised_satids = .FALSE.
INTEGER                          :: num_cloud_vars = 70
LOGICAL                          :: store_seconds_of_60 = .FALSE.
LOGICAL                          :: force_conv_body_creation = .FALSE.
INTEGER                          :: update_table_number = IMDI

END MODULE OpsMod_ODBInfo
