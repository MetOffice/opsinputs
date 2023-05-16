!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Function to return the size of a file in bytes as portably as possible.
!-------------------------------------------------------------------------------

FUNCTION Ops_FileSize (filename,              &
                       status_if_open_failed) RESULT (file_size)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  gen_warn,            &
  StatusFatal,         &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE, INTRINSIC :: ISO_C_BINDING, ONLY: &
  C_NULL_CHAR

IMPLICIT NONE

! Function arguments:
CHARACTER(len=*), INTENT(IN)  :: filename
INTEGER, OPTIONAL, INTENT(IN) :: status_if_open_failed

! Function result:
INTEGER                       :: file_size

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = "Ops_FileSize"
LOGICAL                       :: exists
INTEGER                       :: local_status_if_open_failed

! Depends on: ops_file_size.o
INTERFACE
  FUNCTION ops_file_size(file_name) BIND (C,NAME = "ops_file_size")
    USE, INTRINSIC :: ISO_C_BINDING,  ONLY: &
      C_CHAR,                               &
      C_LONG
    CHARACTER(kind=C_CHAR) :: file_name
    INTEGER(kind=C_LONG)   :: ops_file_size
  END FUNCTION ops_file_size
END INTERFACE

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (PRESENT (status_if_open_failed)) THEN
  local_status_if_open_failed = status_if_open_failed
ELSE
  local_status_if_open_failed = StatusFatal
END IF

INQUIRE (FILE = filename, EXIST = exists)

IF (exists) THEN
  file_size = ops_file_size (TRIM (filename) // C_NULL_CHAR)
ELSE
  SELECT CASE (local_status_if_open_failed)
    CASE (StatusFatal)
      CALL gen_fail (RoutineName,                                       &
                     "File " // TRIM (filename) // " cannot be opened")
    CASE DEFAULT
      CALL gen_warn (RoutineName,                                       &
                     "File " // TRIM (filename) // " cannot be opened")
  END SELECT
  file_size = IMDI
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END FUNCTION Ops_FileSize
