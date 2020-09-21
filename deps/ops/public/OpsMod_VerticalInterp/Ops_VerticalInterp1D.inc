!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Performs a cubic Lagrange interpolation in the vertical from DataIn at RIn
! altitudes to DataOut at ROut altitudes.
!
! Method:
!     This is a simplified version of Mark Mawson's cubiclagrange routine
!     described in F.R. Division working paper No 162.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_VerticalInterp1D (RIn,     &
                                 ROut,    &
                                 DataIn,  &
                                 DataOut)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI,                                &
    RMDI

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(IN)            :: RIn(:)      ! Vertical co-ordinate of input data
REAL, INTENT(IN)            :: ROut(:)     ! Vertical co-ordinate of output data
REAL, INTENT(IN)            :: DataIn(:)   ! Data on the original vertical levels.
REAL, INTENT(OUT)           :: DataOut(:)  ! Data interpolated to the new vertical levels.

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_VerticalInterp1D"
INTEGER                     :: i
INTEGER                     :: index
INTEGER                     :: j
REAL                        :: rhere          ! } Temporary data stores for
REAL                        :: rhereplus      ! } elements of the RIn array
INTEGER                     :: KOut(SIZE (DataOut))     ! level in RIn below ROut

IF (UseTrace) CALL gen_trace_entry (RoutineName)

Kout = IMDI
DataOut = RMDI

DO i = 1, SIZE (ROut)
  index = IMDI
  if (ROut(i) == RMDI) EXIT
  DO j = 1, SIZE (RIn)
    IF (RIn(j) == RMDI) EXIT
    IF (ROut(i) <= RIn(j)) THEN
      index = j
      EXIT
    END IF
  END DO
  IF (index /= IMDI) KOut(i) = index - 1
END DO

DO i = 1, SIZE (DataOut)
  IF (KOut(i) == IMDI) CYCLE
  IF (KOut(i) == 0) THEN
    DataOut(i) = DataIn(1)
  ELSE
    rhere = RIn(KOut(i))
    rhereplus = RIn(KOut(i) + 1)
    DataOut(i) = (((ROut(i) - rhere) * DataIn(KOut(i) + 1)) - &
                          ((ROut(i) - rhereplus) * DataIn(KOut(i)))) / &
                              (rhereplus - rhere)
  END IF
END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_VerticalInterp1D
