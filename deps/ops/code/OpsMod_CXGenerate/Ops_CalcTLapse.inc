!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Calculate the near surface temperature lapse rate, by performing a linear
! regression analysis of level k temperature against height.
!
! This is carried out over a NX by NY array centred on UM grid-point nearest to
! the downscaled location.  The calculation is only performed if the UM point is
! a land point, and a sufficient proportion of the array are also land points
! (determined by ndiameter2).  This is based on Sheridan P., S. Smith, A. Brown
! and S. Vosper, 2010:  A simple height-based correction for temperature
! downscaling in complex terrain.  Meteorological Applications, 17, pp 329-339.
!
! The original routine (Fss_CalcTLevGrad.f90) applied limits of [-3*DALR,
! DALR=dry adiabatic lapse rate] these have been replaced by the limits used for
! this variable in the MetDB.
!
! Inputs:
!     ZLev, TLev and LMask
!
! Outputs:
!     dTLevdz - variable lapse rate (RMDI where it can't be computed)
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CalcTLapse (ZLev,    &
                           TLev,    &
                           LMask,   &
                           dTLevdz)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    ZeroDegC,                            &
    RMDI

USE OpsMod_Kinds, ONLY: &
    logical_default

USE OpsMod_Kinds, ONLY : &
    real32

IMPLICIT NONE

! Subroutine arguments:
REAL(real32), INTENT(IN)    :: ZLev(:,:)       ! Height of model surface
REAL(real32), INTENT(IN)    :: TLev(:,:)       ! Level k temperature data
LOGICAL, INTENT(IN)         :: LMask(:,:)      ! Land mask
REAL(real32), INTENT(OUT)   :: dTLevdz(:,:)    ! Vertical temperature gradient

! Local declarations:
INTEGER                     :: NCols           ! x dimension
INTEGER                     :: NRows           ! y dimension
INTEGER                     :: i
INTEGER                     :: j               ! Loop counters
INTEGER                     :: n_points        ! number of points used to calculate T gradient
INTEGER                     :: i_min
INTEGER                     :: i_max
INTEGER                     :: j_min
INTEGER                     :: j_max           ! end stops for loop counters
INTEGER                     :: ndiameter2      ! minimum number of land points needed
REAL                        :: sum_elev        ! sum of elevations
REAL                        :: sum_elev2       ! sum of elevations squared
REAL                        :: sum_TLev        ! sum of temperatures
REAL                        :: sum_elev_TL     ! sum of elev * temperature
REAL                        :: av_elev         ! average elevation
REAL                        :: av_tl           ! average temperature
REAL(real32), PARAMETER     :: LapseRateMin = -0.02
REAL(real32), PARAMETER     :: LapseRateMax =  0.03
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_CalcTLapse"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!----------------------------------------------------------------------
! Calculate temperature gradient in lower boundary layer
!----------------------------------------------------------------------

ndiameter2 = 2 * xRadiusLapse * yRadiusLapse
NCols = SIZE (ZLev(:,1))
NRows = SIZE (ZLev(1,:))
DO j = 1, NRows
  DO i = 1, NCols

    i_min = MAX (1, i - xRadiusLapse)
    i_max = MIN (NCols, i + xRadiusLapse)
    j_min = MAX (1, j - yRadiusLapse)
    j_max = MIN (NRows, j + yRadiusLapse)
    n_points = COUNT (LMask(i_min:i_max, j_min:j_max))

    IF (LMask(I,J) .AND. n_points >= ndiameter2) THEN

      sum_elev  = SUM (ZLev(i_min:i_max, j_min:j_max), &
                       mask = LOGICAL (LMask(i_min:i_max, j_min:j_max), kind = logical_default))

      sum_TLev  = SUM (TLev(i_min:i_max, j_min:j_max) - ZeroDegC, &
                       mask = LOGICAL (LMask(i_min:i_max, j_min:j_max), kind = logical_default))

      sum_elev2 = SUM (ZLev(i_min:i_max, j_min:j_max) ** 2, &
                       mask = LOGICAL (LMask(i_min:i_max, j_min:j_max), kind = logical_default))

      sum_elev_TL = SUM ((TLev(i_min:i_max, j_min:j_max)-ZeroDegC) * &
                          ZLev(i_min:i_max, j_min:j_max), &
                       mask = LOGICAL (LMask(i_min:i_max, j_min:j_max), kind = logical_default))

      av_TL = sum_TLev / n_points
      av_elev = sum_elev / n_points

      ! level k gradient
      IF (sum_Elev2 - n_points * av_elev ** 2 > ZVarMinLapse * n_points) THEN
        dTLevdz(i,j) = (sum_Elev_TL - n_points * av_elev * av_TL) / &
            (sum_Elev2 - n_points * av_elev ** 2)
        dTLevdz(i,j) = MAX (MIN (dTLevdz(i,j),LapseRateMax),LapseRateMin)
      ELSE  ! This avoids Divide-by-Zero errors.
        dTLevdz(i,j) = RMDI
      END IF

    ELSE
      dTLevdz(i,j) = RMDI
    END IF

  END DO
END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CalcTLapse
