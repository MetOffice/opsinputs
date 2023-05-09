!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Converts a list of numbers specified in a command line to a simple array of
! numbers.  Handles comma separated values and ranges, e.g. a-b -> a,...,b.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ConvertCommandLineList (ob_list_char, &
                                       ob_list)

USE GenMod_Core, ONLY: &
  Gen_StringSplit,     &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)      :: ob_list_char
INTEGER, ALLOCATABLE, INTENT(OUT) :: ob_list(:)

! Local declarations:
CHARACTER(len=*), PARAMETER       :: RoutineName = "Ops_ConvertCommandLineList"
CHARACTER(len=6), POINTER         :: obs_pointer(:)
INTEGER                           :: j
INTEGER                           :: k
INTEGER                           :: index_dash
INTEGER                           :: start_list
INTEGER                           :: end_list
INTEGER, ALLOCATABLE              :: ob_list_tmp(:)
INTEGER                           :: m

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL Gen_StringSplit (ob_list_char, &
                      ',',          &
                      obs_pointer)
ALLOCATE (ob_list(SIZE (obs_pointer)))
ob_list = 0
k = 0
DO j = 1, SIZE (obs_pointer)
  index_dash = INDEX (obs_pointer(j), "-")
  IF (index_dash > 0) THEN
    READ (obs_pointer(j)(1:index_dash - 1),*) start_list
    READ (obs_pointer(j)(index_dash + 1:),*) end_list
    ALLOCATE (ob_list_tmp(SIZE (ob_list) + end_list - start_list))
    ob_list_tmp(1:k) = ob_list(1:k)
    ob_list_tmp(k + 1:) = 0
    CALL MOVE_ALLOC (ob_list_tmp, &
                     ob_list)
    DO m = start_list, end_list
      k = k + 1
      ob_list(k) = m
    END DO
  ELSE
    k = k + 1
    READ (obs_pointer(j), *) ob_list(k)
  END IF
END DO
DEALLOCATE (obs_pointer)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ConvertCommandLineList
