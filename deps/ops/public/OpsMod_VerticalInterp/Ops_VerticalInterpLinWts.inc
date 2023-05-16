!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Produces weights for a linear interpolation in the vertical from DataIn at RIn
! altitudes to DataOut at ROut altitudes.
!
! This is a simplified version of Mark Mawson's cubiclagrange routine described
! in F.R. Division working paper No 162.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_VerticalInterpLinWts (RIn,     &
                                     ROut,    &
                                     KOut,    &
                                     WOut,    &
                                     KInvOut)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(IN)            :: RIn(:)      ! Vertical co-ordinate of input data
REAL, INTENT(IN)            :: ROut(:)     ! Vertical co-ordinate of output data
INTEGER, INTENT(INOUT)      :: KOut(:)     ! level in RIn below ROut
REAL, INTENT(INOUT)         :: WOut(:)     ! weighting for level below in ROut
INTEGER, INTENT(INOUT)      :: KInvOut(:)  ! level in ROut above Rin

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_VerticalInterpLinWts"
INTEGER                     :: len3In     ! Extent of RIn in k direction.
INTEGER                     :: len3Out    ! Extent of ROut in k direction.
INTEGER                     :: k          ! Loop index
INTEGER                     :: index      ! index
INTEGER                     :: temp       ! Temporary index
INTEGER                     :: LastKInv
REAL                        :: LastROut

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! ----------------------------------------------------------------------
! Section 1. For each output point find k so that the point on the
! output grid lies between k and k+1
! ----------------------------------------------------------------------

len3In = SIZE (RIn, DIM = 1)
len3Out = SIZE (ROut, DIM = 1)
KInvOut(:) = -1
KOut(:) = 1
LastROut = -HUGE (LastROut)
LastKInv = -1
DO k = len3Out, 1, -1
  IF (rOut(k) > LastROut) temp = Len3In - 1
  DO index = temp, 2, -1
    IF (rIn(index) <= rOut(k)) THEN
      KOut(k) = index
      KInvOut(index) = k
      temp = index
      IF (LastKInv == -1) LastKInv = k
      EXIT
    END IF
  END DO
  LastROut = rOut(k)
END DO

IF (rIn(1) <= rOut(1)) THEN
  KInvOut(1) = 1
END IF

DO k = len3In, 1, -1
  IF (KInvOut(k) == -1) THEN
    KInvOut(k) = LastKInv
  ELSE
    LastKInv = KInvOut(k)
  END IF
END DO

! ----------------------------------------------------------------------
! Section 2. For each output point calculate interpolation weights for
! point k
! ----------------------------------------------------------------------

WOut(:) = 0.0

DO k = 1, len3Out
  WOut(k) = ((RIn(KOut(k) + 1) - ROut(k))) / &
             (RIn(KOut(k) + 1) - RIn(KOut(k)))
END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_VerticalInterpLinWts
