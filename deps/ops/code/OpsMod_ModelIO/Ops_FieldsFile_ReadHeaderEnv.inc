!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Open a fields file and read its header.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_FieldsFile_ReadHeaderEnv (self,                  &
                                         env_var,               &
                                         filename,              &
                                         status_if_env_not_set)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_UMHeaderConstants, ONLY: &
  FH_HorizGrid,                     &
  FH_RowDepCStart,                  &
  FH_SubModel,                      &
  FH_SubModel_Atmos,                &
  IC_XLen,                          &
  IC_YLen,                          &
  RC_FirstLat,                      &
  RC_FirstLong,                     &
  RC_LatSpacing,                    &
  RC_LongSpacing,                   &
  RC_PoleLat,                       &
  RC_PoleLong

USE OpsMod_IOUtils, ONLY: &
  Ops_OpenCEnvRead

IMPLICIT NONE

! Subroutine arguments:
TYPE (DomainInfo_type), INTENT(INOUT)  :: self
CHARACTER(len=*), INTENT(IN)           :: env_var
CHARACTER(len=*), OPTIONAL, INTENT(IN) :: filename
INTEGER, OPTIONAL, INTENT(IN)          :: status_if_env_not_set

! Local declarations:
CHARACTER(len=*), PARAMETER            :: RoutineName = 'Ops_FieldsFile_ReadHeaderEnv'

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (ALLOCATED (self % header)) THEN
  CALL self % header(1) % dealloc
  DEALLOCATE (self % header)
ELSE
  ALLOCATE (self % header(1))
END IF

CALL Ops_OpenCEnvRead (env_var,                                       &
                       self % header(1) % UnitNum,                    &
                       filename = filename,                           &
                       status_if_env_not_set = status_if_env_not_set)

IF (self % header(1) % UnitNum > 0) THEN
  CALL self % header(1) % read ("foo")
  self % pole_lat = self % header(1) % RealC(RC_PoleLat)
  self % pole_lon = self % header(1) % RealC(RC_PoleLong)
  self % grid_type = self % header(1) % Fixhd(FH_HorizGrid)
  self % variable_resolution = self % header(1) % FixHd(FH_RowDepCStart) > 0 .AND. &
                               self % header(1) % FixHd(FH_SubModel) == FH_SubModel_Atmos
  IF (self % variable_resolution) THEN
    self % first_lat = MINVAL (self % header(1) % RowDepC)
    self % num_lats = self % header(1) % IntC(IC_YLen)
    self % lat_spacing = (MAXVAL (self % header(1) % RowDepC(1:self % num_lats)) - &
                               MINVAL (self % header(1) % RowDepC(1:self % num_lats))) / (self % num_lats - 1)
    self % first_lon = MINVAL (self % header(1) % ColDepC)
    self % num_lons = self % header(1) % IntC(IC_XLen)
    self % lon_spacing = (MAXVAL (self % header(1) % ColDepC(1:self % num_lons)) - &
                               MINVAL (self % header(1) % ColDepC(1:self % num_lons))) / (self % num_lons - 1)
  ELSE
    self % first_lat = self % header(1) % RealC(RC_FirstLat)
    self % num_lats = self % header(1) % IntC(IC_YLen)
    self % lat_spacing = self % header(1) % RealC(RC_LatSpacing)
    self % first_lon = self % header(1) % RealC(RC_FirstLong)
    self % num_lons = self % header(1) % IntC(IC_XLen)
    self % lon_spacing = self % header(1) % RealC(RC_LongSpacing)
  END IF
ELSE
  DEALLOCATE (self % header)
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_FieldsFile_ReadHeaderEnv
