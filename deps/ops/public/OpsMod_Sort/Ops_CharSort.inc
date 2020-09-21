!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Generates a index array pointing to the elements of the character
! string array 'key' in increasing order
!
! Method:
!   The sort is performed by sorting on each character in turn
!   using the routine Ops_OneCharSort
!
! Inputs:
!   key : An array of character strings, to be sorted
!   n : The number of items to sort
!
! Input/Output:
!   index : An integer array pointing to the sorted items.
!   grpinfo : An integer array describing the groups of items within the
!              index whose positions remain ambiguous.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CharSort (key,     &
                         n,       &
                         index,   &
                         grpinfo)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(len=*), INTENT(IN)   :: key(:)
INTEGER, INTENT(IN)            :: n      ! The number of items to be sorted.
INTEGER, TARGET, INTENT(INOUT) :: index(:)
INTEGER, INTENT(INOUT)         :: grpinfo(:)

! Local declarations:
CHARACTER, ALLOCATABLE         :: OneChar(:)
INTEGER                        :: ichar ! Used to scan through characters in string.
INTEGER                        :: j
CHARACTER(len=*), PARAMETER    :: RoutineName = 'Ops_CharSort'

IF (UseTrace) CALL gen_trace_entry (RoutineName)

ALLOCATE (OneChar(SIZE (key)))

DO ichar = 1, LEN (key)

  ! Copy ichar'th character in each string to a single character array:
  ! this overcomes an apparent bug in HPUX10.

  DO j = 1, SIZE (key)
    OneChar(j) = key(j)(ichar:ichar)
  END DO

  CALL Ops_OneCharSort (OneChar, &
                        n,       &
                        index,   &
                        grpinfo)

END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CharSort
