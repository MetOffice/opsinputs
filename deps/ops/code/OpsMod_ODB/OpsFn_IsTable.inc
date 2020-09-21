!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Checks whether a query is a table or not.
!-------------------------------------------------------------------------------

FUNCTION OpsFn_IsTable (query)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Function arguments:
CHARACTER(len=*), INTENT(IN) :: query

! Function result:
LOGICAL                      :: OpsFn_IsTable

! Local declarations:
CHARACTER(len=*), PARAMETER  :: RoutineName = "OpsFn_IsTable"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

OpsFn_IsTable = INDEX (query, "@") /= 0

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END FUNCTION OpsFn_IsTable
