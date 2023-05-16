!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Performs a cubic Lagrange interpolation in the vertical from DataIn at RIn
! altitudes to DataOut at ROut altitudes.
!
! Method:
!     This is a simplified version of Mark Mawson's cubiclagrange routine
!     described in F.R. Division working paper No 162.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_VerticalInterpLinear (len1In,    &
                                     len2In,    &
                                     len3Out,   &
                                     KOut,      &
                                     RIn,       &
                                     ROut,      &
                                     DataIn,    &
                                     DataOut,   &
                                     RMDICheck)

USE GenMod_Constants, ONLY: &
    RMDItol

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)           :: len1In          ! Extent of DataIn in i direction.
INTEGER, INTENT(IN)           :: len2In          ! Extent of DataIn in j direction.
INTEGER, INTENT(IN)           :: len3Out         ! Extent of DataOut in k direction.
INTEGER, INTENT(IN)           :: KOut(:,:,:)     ! level in RIn below ROut
REAL, INTENT(IN)              :: RIn(:,:,:)      ! Vertical co-ordinate of input data
REAL, INTENT(IN)              :: ROut(:,:,:)     ! Vertical co-ordinate of output data
REAL, INTENT(IN)              :: DataIn(:,:,:)   ! Data on the original vertical levels.
REAL, INTENT(OUT)             :: DataOut(:,:,:)  ! Data interpolated to the new vertical levels.
LOGICAL, INTENT(IN), OPTIONAL :: RMDICheck       ! check for RMDI if present/true

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = "Ops_VerticalInterpLinear"
INTEGER                       :: i
INTEGER                       :: j
INTEGER                       :: k              ! Loop indices
REAL                          :: rhere          ! } Temporary data stores for
REAL                          :: rhereplus      ! } elements of the RIn array
LOGICAL                       :: RMDICheck2     ! copy RMDICheck

IF (UseTrace) CALL gen_trace_entry (RoutineName)

RMDICheck2 = .FALSE.
IF (PRESENT (RMDICheck)) THEN
  IF (RMDICheck) THEN
    RMDICheck2 = .TRUE.
  END IF
END IF

IF (RMDICheck2) THEN
  DO k = 1, len3Out
    DO j = 1, len2In
      DO i = 1, len1In
        IF (ROut(i,j,k) == RIn (i,j,KOut(i,j,k))) THEN
          DataOut(i,j,k) = DataIn (i,j,KOut(i,j,k))
        ELSE IF (ABS (DataIn(i,j,KOut(i,j,k) + 1) - RMDI) < RMDItol .OR. &
                 ABS (DataIn(i,j,KOut(i,j,k)) - RMDI) < RMDItol) THEN

          ! dont interpolate missing data
          DataOut(i,j,k) = RMDI
        ELSE
          rhere = RIn(i,j,KOut(i,j,k))
          rhereplus = RIn(i,j,KOut(i,j,k) + 1)

          DataOut(i,j,k) = (((ROut(i,j,k) - rhere) * DataIn (i,j,KOut(i,j,k) + 1)) - &
                            ((ROut(i,j,k) - rhereplus) * DataIn (i,j,KOut(i,j,k)))) / &
                                (rhereplus - rhere)
        END IF
      END DO
    END DO
  END DO
ELSE
  DO k = 1, len3Out
    DO j = 1, len2In
      DO i = 1, len1In
        rhere = RIn(i,j,KOut(i,j,k))
        rhereplus = RIn(i,j,KOut(i,j,k) + 1)

        DataOut(i,j,k) = (((ROut(i,j,k) - rhere) * DataIn (i,j,KOut(i,j,k) + 1)) - &
                          ((ROut(i,j,k) - rhereplus) * DataIn (i,j,KOut(i,j,k)))) / &
                              (rhereplus - rhere)
      END DO
    END DO
  END DO
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_VerticalInterpLinear
