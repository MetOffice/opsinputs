!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Initialises the index and grpinfo arrays
!
! Method:
!   The index is set to the identity,
!   and the grpinfo array is set to one all inclusive group
!
! Inputs:
!   n: the size of the array to be sorted
!
! Outputs:
!   index:   an integer array which will eventually
!               point to the sorted information.
!   grpinfo: an integer array which stores information
!               about the ambiguous groups within the index.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_InitSort (index,   &
                         grpinfo, &
                         n)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(OUT)        :: index(:)
INTEGER, INTENT(OUT)        :: grpinfo(:)
INTEGER, INTENT(IN)         :: n ! The size of the array to be sorted.

! Local declarations:
INTEGER                     :: i
CHARACTER(len=*), PARAMETER :: RoutineName = 'Ops_InitSort'

IF (UseTrace) CALL gen_trace_entry (RoutineName)

grpinfo(:) = 0
index(1:n) = (/ (i, i = 1, n) /)
IF (n > 1) THEN
  grpinfo(1) = 1
  grpinfo(2) = n
ELSE IF (n == 1) THEN
  grpinfo(1) = 2
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_InitSort
