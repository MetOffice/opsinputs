!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Divides a total into num_batches of equal length (or as close as possible),
! returning the lengths in batch_lens.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_DivideIntoBatches (total,       &
                                  num_batches, &
                                  batch_lens)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)         :: total
INTEGER, INTENT(IN)         :: num_batches
INTEGER, ALLOCATABLE        :: batch_lens(:)

! Local declarations:
CHARACTER(len=*), PARAMETER :: routine_name = "Ops_DivideIntoBatches"
INTEGER                     :: i

IF (UseTrace) CALL gen_trace_entry (routine_name)

ALLOCATE (batch_lens(num_batches))

batch_lens(:) = total / num_batches

DO i = 1, MODULO (total, num_batches)
  batch_lens(i) = batch_lens(i) + 1
END DO

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE Ops_DivideIntoBatches
