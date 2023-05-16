!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Performs a quintic Lagrange interpolation in the vertical from DataIn at RIn
! altitudes to DataOut at ROut altitudes.
!
! This is a simplified version of Sue Coulter's quinticlagrange routine
! described in F.R. Division working paper No 162.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_VerticalInterpQuintic (len1In,  &
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
INTEGER, INTENT(IN)         :: len1In          ! Extent of DataIn in i direction.
INTEGER, INTENT(IN)         :: len2In          ! Extent of DataIn in j direction.
INTEGER, INTENT(IN)         :: len3In          ! Extent of DataIn in k direction.
INTEGER, INTENT(IN)         :: len3Out         ! Extent of DataOut in k direction.
INTEGER, INTENT(IN)         :: KOut(:,:,:)     ! level in RIn below ROut
REAL, INTENT(IN)            :: RIn(:,:,:)      ! Vertical co-ordinate of input data
REAL, INTENT(IN)            :: ROut(:,:,:)     ! Vertical co-ordinate of output data
REAL, INTENT(IN)            :: DataIn(:,:,:)   ! Data on the original vertical levels.
REAL, INTENT(OUT)           :: DataOut(:,:,:)  ! Data interpolated to the new vertical levels.

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_VerticalInterpQuintic"
INTEGER                     :: i
INTEGER                     :: j
INTEGER                     :: k               ! Loop indices
REAL                        :: rhereminus2     ! } Temporary data stores for
REAL                        :: rhereminus      ! } elements of the RIn array
REAL                        :: rhere           ! }
REAL                        :: rhereplus       ! }
REAL                        :: rhereplus2      ! }
REAL                        :: rhereplus3      ! }
REAL                        :: Z1
REAL                        :: Z2
REAL                        :: Z3
REAL                        :: Z4              ! } Stores for Partial sums
REAL                        :: Z1A
REAL                        :: Z2A
REAL                        :: Z3A
REAL                        :: Z4A
REAL                        :: Z5A
REAL                        :: Z6A             ! } Stores for Partial sums
REAL                        :: Z1B
REAL                        :: Z2B
REAL                        :: Z3B
REAL                        :: Z4B
REAL                        :: Z5B
REAL                        :: Z6B             ! } Stores for Partial sums

IF (UseTrace) CALL gen_trace_entry (RoutineName)

DO k = 1, len3Out
  DO j = 1, len2In
    DO i = 1, len1In
      IF (KOut(i,j,k) == 1 .OR. KOut(i,j,k) == len3In - 1) THEN

        ! use the linear method to interpolate between bottom most levels
        ! or top most levels.

        rhere = RIn(i,j,KOut(i,j,k))
        rhereplus = RIn(i,j,KOut(i,j,k) + 1)

        DataOut(i,j,k) = (((ROut(i,j,k) - rhere) * DataIn(i,j,KOut(i,j,k) + 1)) - &
                          ((ROut(i,j,k) - rhereplus) * DataIn(i,j,KOut(i,j,k)))) / (rhereplus - rhere)

      ELSE IF (KOut(i,j,k) == 2 .OR. KOut(i,j,k) == len3In - 2) THEN

        ! use cubic interpolation.

        rhereminus = RIn(i,j,KOut(i,j,k) - 1)
        rhere = RIn(i,j,KOut(i,j,k))
        rhereplus = RIn(i,j,KOut(i,j,k) + 1)
        rhereplus2 = RIn(i,j,KOut(i,j,k) + 2)

        Z1 = ((ROut(i,j,k) - rhere) * (ROut(i,j,k) - rhereplus) * &
              (ROut(i,j,k) - rhereplus2) *  DataIn(i,j,KOut(i,j,k) - 1)) / &
             ((rhereminus - rhere) * (rhereminus - rhereplus) * (rhereminus - rhereplus2))

        Z2 = ((ROut(i,j,k) - rhereminus) * (ROut(i,j,k) - rhereplus) * &
              (ROut(i,j,k) - rhereplus2) * DataIn(i,j,KOut(i,j,k))) / &
             ((rhere - rhereminus) * (rhere - rhereplus) * (rhere - rhereplus2))

        Z3 = ((ROut(i,j,k) - rhereminus) * (ROut(i,j,k) - rhere) * &
              (ROut(i,j,k) - rhereplus2) * DataIn(i,j,KOut(i,j,k) + 1)) / &
             ((rhereplus - rhereminus) * (rhereplus - rhere) * (rhereplus - rhereplus2))

        Z4 = ((ROut(i,j,k) - rhereminus) * (ROut(i,j,k) - rhere) * &
              (ROut(i,j,k) - rhereplus) * DataIn(i,j,KOut(i,j,k) + 2)) / &
             ((rhereplus2 - rhereminus) * (rhereplus2 - rhere) * (rhereplus2 - rhereplus))

        DataOut(i,j,k) = Z1 + Z2 + Z3 + Z4
      ELSE

        ! use quintic interpolation

        rhereminus2 = RIn(i,j,KOut(i,j,k) - 2)
        rhereminus = RIn(i,j,KOut(i,j,k) - 1)
        rhere = RIn(i,j,KOut(i,j,k))
        rhereplus = RIn(i,j,KOut(i,j,k) + 1)
        rhereplus2 = RIn(i,j,KOut(i,j,k) + 2)
        rhereplus3 = RIn(i,j,KOut(i,j,k) + 3)

        Z1A = (ROut(i,j,k) - rhereminus) * (ROut(i,j,k) - rhere) * &
              (ROut(i,j,k) - rhereplus) * (ROut(i,j,k) - rhereplus2) * &
              (ROut(i,j,k) - rhereplus3) * DataIn(i,j,KOut(i,j,k) - 2)

        Z1B = (rhereminus2 - rhereminus) * (rhereminus2 - rhere) * &
              (rhereminus2 - rhereplus) * (rhereminus2 - rhereplus2) * &
              (rhereminus2 - rhereplus3)

        Z2A = (ROut(i,j,k) - rhereminus2) * (ROut(i,j,k) - rhere) * &
              (ROut(i,j,k) - rhereplus) * (ROut(i,j,k) - rhereplus2) * &
              (ROut(i,j,k) - rhereplus3) * DataIn(i,j,KOut(i,j,k)-1)

        Z2B = (rhereminus - rhereminus2) * (rhereminus - rhere) * &
              (rhereminus - rhereplus) * (rhereminus - rhereplus2) * &
              (rhereminus - rhereplus3)

        Z3A = (ROut(i,j,k) - rhereminus2) * (ROut(i,j,k) - rhereminus) * &
              (ROut(i,j,k) - rhereplus) * (ROut(i,j,k) - rhereplus2) * &
              (ROut(i,j,k) - rhereplus3) * DataIn(i,j,KOut(i,j,k))

        Z3B = (rhere - rhereminus2) * (rhere - rhereminus) * &
              (rhere - rhereplus) * (rhere - rhereplus2) * &
              (rhere - rhereplus3)

        Z4A = (ROut(i,j,k) - rhereminus2) * (ROut(i,j,k) - rhereminus) * &
              (ROut(i,j,k) - rhere) * (ROut(i,j,k) - rhereplus2) * &
              (ROut(i,j,k) - rhereplus3) * DataIn(i,j,KOut(i,j,k) + 1)

        Z4B = (rhereplus - rhereminus2) * (rhereplus - rhereminus) * &
              (rhereplus - rhere) * (rhereplus - rhereplus2) * &
              (rhereplus - rhereplus3)

        Z5A = (ROut(i,j,k) - rhereminus2) * (ROut(i,j,k) - rhereminus) * &
              (ROut(i,j,k) - rhere) * (ROut(i,j,k) - rhereplus) * &
              (ROut(i,j,k) - rhereplus3) * DataIn(i,j,KOut(i,j,k) + 2)

        Z5B = (rhereplus2 - rhereminus2) * (rhereplus2 - rhereminus) * &
              (rhereplus2 - rhere) * (rhereplus2 - rhereplus) * &
              (rhereplus2 - rhereplus3)

        Z6A = (ROut(i,j,k) - rhereminus2) * (ROut(i,j,k) - rhereminus) * &
              (ROut(i,j,k) - rhere) * (ROut(i,j,k) - rhereplus) * &
              (ROut(i,j,k) - rhereplus2) * DataIn(i,j,KOut(i,j,k) + 3)

        Z6B = (rhereplus3 - rhereminus2) * (rhereplus3 - rhereminus) * &
              (rhereplus3 - rhere) * (rhereplus3 - rhereplus) * &
              (rhereplus3 - rhereplus2)

        DataOut(i,j,k) = Z1A / Z1B + Z2A / Z2B + Z3A / Z3B + Z4A / Z4B + Z5A / Z5B + Z6A / Z6B
      END IF
    END DO
  END DO
END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_VerticalInterpQuintic
