!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Generates a index array pointing to the elements of the real
! array 'key' in increasing order
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
!   n : The number of items to sort
!
! Input/Output:
!   index : An integer array pointing to the sorted items.
!   grpinfo : An integer array describing the groups of items within the
!              index whose positions remain ambiguous.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_RealSort (key,     &
                         n,       &
                         index,   &
                         grpinfo)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(IN)               :: key(:)
INTEGER, INTENT(IN)            :: n      ! The number of items to be sorted.
INTEGER, TARGET, INTENT(INOUT) :: index(:)
INTEGER, INTENT(INOUT)         :: grpinfo(:)

! Local declarations:
INTEGER, POINTER               :: lclindx(:)
INTEGER                        :: StartGrp   ! The position in the index of the first item of the present group
INTEGER                        :: EndGrp     ! The position in the index of the last item of the present group
INTEGER                        :: ngrp       ! The number of items in the present group
INTEGER                        :: StartNew   ! The position in the index of the first item of a new group
INTEGER                        :: EndNew     ! The position in the index of the last item of a new group
INTEGER                        :: EndLast    ! The position in the index of the last item of the last new group
INTEGER                        :: head       ! heaps are tree structures: head and child refer
INTEGER                        :: child      ! to related items within the tree
INTEGER                        :: j
INTEGER                        :: dum        ! used to swap index items
CHARACTER(len=*),  PARAMETER   :: RoutineName = 'Ops_RealSort'

IF (UseTrace) CALL gen_trace_entry (RoutineName)

EndGrp = 0
EndLast = 0
sortgroups : DO

  ! Use the grpinfo array to find the beginning and end of each
  ! subgroup, and proceed to sort each of these.

  IF (EndGrp >= n) EXIT sortgroups
  StartGrp = grpinfo(EndGrp + 1)
  IF (StartGrp >= n) EXIT sortgroups
  EndGrp = grpinfo(StartGrp + 1)
  ngrp = EndGrp - StartGrp + 1
  lclindx => index(StartGrp:EndGrp)

  ! Do heapsort: Create the heap...

  makeheap : DO j = ngrp / 2, 1, -1
    head = j
    sift1 : DO

      ! find the largest out of the head and its two children...

      child = head * 2
      IF (child > ngrp) EXIT sift1
      IF (child < ngrp) THEN
        IF (key(lclindx(child + 1)) > key(lclindx(child))) child = child + 1
      END IF

      ! if the head is the largest, then sift is done...

      IF (key(lclindx(head)) >= key(lclindx(child))) EXIT sift1

      ! otherwise swap to put the largest child at the head,
      ! and prepare to repeat the procedure for the head in its new
      ! subordinate position.

      dum = lclindx(child)
      lclindx(child) = lclindx(head)
      lclindx(head) = dum
      head = child
    END DO sift1
  END DO makeheap

  ! Retire heads of the heap, which are the largest, and
  ! stack them at the end of the array.

  retire : DO j = ngrp, 2, -1
    dum = lclindx(1)
    lclindx(1) = lclindx(j)
    lclindx(j) = dum
    head = 1

    ! second sift is similar to first...

    sift2: DO
      child = head * 2
      IF (child > (j - 1)) EXIT sift2
      IF (child < (j - 1)) THEN
        IF (key(lclindx(child + 1)) > key(lclindx(child))) child = child + 1
      END IF
      IF (key(lclindx(head)) >= key(lclindx(child))) EXIT sift2
      dum = lclindx(child)
      lclindx(child) = lclindx(head)
      lclindx(head) = dum
      head = child
    END DO sift2
  END DO retire

  ! After sorting the subgroup, subgroups remaining within the
  ! sorted subgroup must be identified, and the grpinfo array
  ! modified accordingly.

  StartNew = StartGrp
  findgroups : DO EndNew = StartGrp, EndGrp
    IF (EndNew /= EndGrp) THEN
      IF (key(index(EndNew)) == key(index(EndNew + 1))) CYCLE FindGroups
    END IF
    IF (EndNew > StartNew) THEN
      grpinfo(StartNew + 1) = EndNew
      grpinfo(EndLast + 1) = StartNew
      EndLast = EndNew
    END IF
    StartNew = EndNew + 1
  END DO findgroups
END DO sortgroups

IF (EndLast < n) grpinfo(EndLast + 1) = n + 1

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_RealSort
