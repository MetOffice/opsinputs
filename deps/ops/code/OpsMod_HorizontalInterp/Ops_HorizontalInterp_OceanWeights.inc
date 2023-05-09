!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Horizontal interpolation weights generation. 
!
! This routine is a modified version of the routine interpolation written
! for SCRIP, and a description is available in the SCRIP user guide.
!
! The four model columns surrounding the lat lon of interest are specified:
!                          x i-1,j              x i, j
!                            _corner(4)         _corner(3)
!                            _weight(4)         _weight(3)
!
!
!                                 * lon,lat
!
!                          x i-1,j-1            x i,j-1
!                            _corner(1)         _corner(2)
!                            _weight(1)         _weight(2)
!
! The weights and corner variables are listed in a counter-clockwise direction.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_HorizontalInterp_OceanWeights (Obs,           &
                                              is,            &
                                              js,            &
                                              netcdf_fields, &
                                              weights)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE OpsMod_ModelIO, ONLY:   &
    OceanNetCDFFields_type

USE OpsMod_ObsInfo, ONLY: &
    Ob_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (Ob_type), INTENT(IN)                :: Obs           ! Observations
INTEGER, INTENT(IN)                       :: is(:)
INTEGER, INTENT(IN)                       :: js(:)
TYPE (OceanNetCDFFields_type), INTENT(IN) :: netcdf_fields
REAL, ALLOCATABLE, INTENT(OUT)            :: weights(:,:)

! Local declarations:
CHARACTER(len=*), PARAMETER               :: RoutineName = "Ops_HorizontalInterp_OceanWeights"
INTEGER                                   :: I
REAL                                      :: lat_corners(4)
REAL                                      :: lon_corners(4)
LOGICAL                                   :: remappingInterpFail

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! ----------------------------------------------------------------------
!  Section 1.   Initialize
! ----------------------------------------------------------------------

ALLOCATE (weights(4, obs % header % numobslocal))

weights(:,:) = RMDI

DO i = 1, Obs % header % NumObsLocal

  !------------------------------------------------------------------------------
  !     Calculate horizontal weights
  !------------------------------------------------------------------------------

  remappingInterpFail = .FALSE.

  IF (is(i) == -1 .OR. js(i) == -1) CYCLE

  lat_corners(1) = netcdf_fields % nav_lat(is(i) - 1,js(i) - 1) ! lats (bottom left)
  lat_corners(2) = netcdf_fields % nav_lat(is(i),js(i) - 1)
  lat_corners(3) = netcdf_fields % nav_lat(is(i),js(i))
  lat_corners(4) = netcdf_fields % nav_lat(is(i) - 1,js(i))

  lon_corners(1) = netcdf_fields % nav_lon(is(i) - 1,js(i) - 1) ! lons (bottom left)
  lon_corners(2) = netcdf_fields % nav_lon(is(i),js(i) - 1)
  lon_corners(3) = netcdf_fields % nav_lon(is(i),js(i))
  lon_corners(4) = netcdf_fields % nav_lon(is(i) - 1,js(i))

  CALL Ops_BilinearRemappingWeights (lat_corners(:),      &
                                     lon_corners(:),      &
                                     weights(:,i),        &
                                     Obs % Latitude(i),   &
                                     Obs % Longitude(i),  &
                                     remappingInterpFail)

  IF (remappingInterpFail) THEN
    CALL Ops_GreatCircleDistWeights (lat_corners(:),     &
                                     lon_corners(:),     &
                                     weights(:,i),       &
                                     Obs % Latitude(i),  &
                                     Obs % Longitude(i))
  END IF
END DO

!------------------------------------------------------------------------------
! [5] Tidy up
!------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_HorizontalInterp_OceanWeights
