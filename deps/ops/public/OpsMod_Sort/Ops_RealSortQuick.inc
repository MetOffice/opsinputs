!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Generates a index array pointing to the elements of the array 'key'
! in increasing order
!
! Method:
!   The heap sort invented by J.W.J.Williams is used.
!   A description of the method can be found in 'Numerical Recipes'
!   The group information array is used to allow easy sorting on several
!   parameters of different types. For details see the Parent Module
!   OpsMod_Sort
!
! Inputs:
!   key : An array of character strings, to be sorted
!
! Input/Output:
!   index : An integer array pointing to the sorted items.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_RealSortQuick (key,   &
                              index)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(IN)                  :: key(:)
INTEGER, ALLOCATABLE, INTENT(OUT) :: index(:)

! Local declarations:
INTEGER                           :: n     ! The number of items
INTEGER                           :: head  ! heaps are tree structures: head and child refer
INTEGER                           :: child ! to related items within the tree
INTEGER                           :: j
INTEGER                           :: dum   ! used to swap index items
CHARACTER(len=*),  PARAMETER      :: RoutineName = 'Ops_RealSortQuick'

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! Could put in an optional mask
n = SIZE (key)
ALLOCATE (Index(n))
DO j = 1, n
  Index(j) = j
END DO

! Do heapsort: Create the heap...

makeheap : DO j = n / 2, 1, -1
  head = j
  sift1 : DO

    ! find the largest out of the head and its two children...

    child = head * 2
    IF (child > n) EXIT sift1
    IF (child < n) THEN
      IF (key(Index(child + 1)) > key(Index(child))) child = child + 1
    END IF

     ! if the head is the largest, then sift is done...

    IF (key(Index(head)) >= key(Index(child))) EXIT sift1

    ! otherwise swap to put the largest child at the head,
    ! and prepare to repeat the procedure for the head in its new
    ! subordinate position.

    dum = Index(child)
    Index(child) = Index(head)
    Index(head) = dum
    head = child
  END DO sift1
END DO makeheap

! Retire heads of the heap, which are the largest, and
! stack them at the end of the array.

retire : DO j = n, 2, -1
  dum = Index(1)
  Index(1) = Index(j)
  Index(j) = dum
  head = 1

  ! second sift is similar to first...

  sift2: DO
    child = head * 2
    IF (child > (j - 1)) EXIT sift2
    IF (child < (j - 1)) THEN
      IF (key(Index(child + 1)) > key(Index(child))) child = child + 1
    END IF
    IF (key(Index(head)) >= key(Index(child))) EXIT sift2
    dum = Index(child)
    Index(child) = Index(head)
    Index(head) = dum
    head = child
  END DO sift2
END DO retire

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_RealSortQuick
