!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Performs a cubic Lagrange interpolation in the vertical from DataIn at RIn
! altitudes to DataOut at ROut altitudes.
!
! This is a simplified version of Mark Mawson's cubiclagrange routine described
! in F.R. Division working paper No 162.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_VerticalInterpCubic (len1In,  &
                                    len2In,  &
                                    len3In,  &
                                    len3Out, &
                                    KOut,    &
                                    RIn,     &
                                    ROut,    &
                                    DataIn,  &
                                    DataOut)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)         :: len1In         ! Extent of DataIn in i direction.
INTEGER, INTENT(IN)         :: len2In         ! Extent of DataIn in j direction.
INTEGER, INTENT(IN)         :: len3In         ! Extent of DataIn in k direction.
INTEGER, INTENT(IN)         :: len3Out        ! Extent of DataOut in k direction.
INTEGER, INTENT(IN)         :: KOut(:,:,:)    ! level in RIn below ROut
REAL, INTENT(IN)            :: RIn(:,:,:)     ! Vertical co-ordinate of input data
REAL, INTENT(IN)            :: ROut(:,:,:)    ! Vertical co-ordinate of output data
REAL, INTENT(IN)            :: DataIn(:,:,:)  ! Data on the original vertical levels.
REAL, INTENT(OUT)           :: DataOut(:,:,:) ! Data interpolated to the new vertical levels.

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_VerticalInterpCubic"
INTEGER                     :: i
INTEGER                     :: j
INTEGER                     :: k                 ! Loop indices
REAL                        :: rhereminus        ! } Temporary data stores for
REAL                        :: rhere             ! } elements of the RIn array
REAL                        :: rhereplus         ! }
REAL                        :: rhereplus2        ! }
REAL                        :: Z1
REAL                        :: Z2
REAL                        :: Z3
REAL                        :: Z4                ! Stores for partial sums

IF (UseTrace) CALL gen_trace_entry (RoutineName)

DO k = 1, len3Out
  DO j = 1, len2In
    DO i = 1, len1In
      IF (KOut(i,j,k) == 1 .OR. KOut(i,j,k) == len3In - 1) THEN
        ! use the linear method to interpolate between bottom most levels
        ! or top most levels.
        rhere = RIn(i,j,KOut(i,j,k))
        rhereplus = RIn(i,j,KOut(i,j,k) + 1)

        DataOut(i,j,k) = (((ROut(i,j,k) - rhere) * DataIn (i,j,KOut(i,j,k) + 1)) - &
                          ((ROut(i,j,k) - rhereplus) * DataIn(i,j,KOut(i,j,k)))) / (rhereplus - rhere)

      ELSE ! use cubic interpolation.
        rhereminus = RIn(i,j,KOut(i,j,k) - 1)
        rhere = RIn(i,j,KOut(i,j,k))
        rhereplus = RIn(i,j,KOut(i,j,k) + 1)
        rhereplus2 = RIn(i,j,KOut(i,j,k) + 2)

        Z1 = ((ROut(i,j,k) - rhere) * (ROut(i,j,k) - rhereplus) * &
              (ROut(i,j,k) - rhereplus2) *  DataIn(i,j,KOut(i,j,k) - 1)) / &
             ((rhereminus - rhere) * (rhereminus - rhereplus) * (rhereminus - rhereplus2))

        Z2 = ((ROut(i,j,k) - rhereminus) * (ROut(i,j,k) - rhereplus) * &
              (ROut(i,j,k) - rhereplus2) * DataIn(i,j,KOut(i,j,k))) /  &
             ((rhere - rhereminus) * (rhere - rhereplus) * (rhere - rhereplus2))

        Z3 = ((ROut(i,j,k) - rhereminus) * (ROut(i,j,k) - rhere) * &
              (ROut(i,j,k) - rhereplus2) * DataIn(i,j,KOut(i,j,k) + 1)) / &
             ((rhereplus - rhereminus) * (rhereplus - rhere) * (rhereplus - rhereplus2))

        Z4 = ((ROut(i,j,k) - rhereminus) * (ROut(i,j,k) - rhere) * &
              (ROut(i,j,k) - rhereplus) * DataIn(i,j,KOut(i,j,k) + 2)) / &
             ((rhereplus2 - rhereminus) * (rhereplus2 - rhere) * (rhereplus2 - rhereplus))

        DataOut(i,j,k) = Z1 + Z2 + Z3 + Z4
      END IF
    END DO
  END DO
END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_VerticalInterpCubic
