!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Initialises the index and grpinfo arrays for subsequent sorting including
! items for which mask is true.
!
! The index is set to point to each of the elements in order, but skipping
! those that are false in mask.  The grpinfo array is set to one group to
! include the whole index.
!
! Inputs:
!     mask: a logical array dictating which items should be kept
!           for sorting (those which are true are kept).  If not present
!           it is assumed to be .true. for all elements.
!
! Inputs/Outputs:
!     n: the size of the array to be sorted. This is modified to
!        account for items excluded from the sort.
!
! Outputs:
!     index:   an integer array which will eventually
!                 point to the sorted information.
!     grpinfo: an integer array which stores information
!                 about the ambiguous groups within the index.
!
!-------------------------------------------------------------------------------

SUBROUTINE Ops_InitSortMask (index,   &
                             grpinfo, &
                             n,       &
                             mask)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT (OUT)          :: index(:)
INTEGER, INTENT (OUT)          :: grpinfo(:)
INTEGER, INTENT (INOUT)        :: n ! The size of the array to be sorted.
LOGICAL, OPTIONAL, INTENT (IN) :: mask(:)

! Local declarations:
INTEGER                        :: i
INTEGER                        :: nkept
CHARACTER(len=*), PARAMETER    :: RoutineName = 'Ops_InitSortMask'

IF (UseTrace) CALL gen_trace_entry(RoutineName)

index(:) = 0
nkept = 0
DO i = 1, n
  IF (PRESENT (mask)) THEN
    IF (mask(i)) THEN
      nkept = nkept + 1
      index(nkept) = i
    END IF
  ELSE
    nkept = nkept + 1
    index(nkept) = i
  END IF
END DO
grpinfo(:) = 0
IF (nkept > 1) THEN
  grpinfo(1) = 1
  grpinfo(2) = nkept
ELSE IF (nkept == 1) THEN
  grpinfo(1) = 2
END IF
n = nkept

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_InitSortMask
