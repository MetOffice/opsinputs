!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Assigns an array of character data to a specified ODB column.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SetODBVal_array_char (handle,      &
                                     table_name,  &
                                     var,         &
                                     data,        &
                                     values,      &
                                     mask,        &
                                     unpack_mask)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE ops_odb_wrappers, ONLY: &
  ops_odb_varindex

USE OpsMod_ODBKinds, ONLY: &
  odb_real

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN) :: handle
CHARACTER(len=*), INTENT(IN)       :: table_name
CHARACTER(len=*), INTENT(IN)       :: var
REAL(kind=odb_real), INTENT(INOUT) :: data(:,0:)
CHARACTER(len=*), INTENT(IN)       :: values(:)
LOGICAL, OPTIONAL, INTENT(IN)      :: mask(:)
LOGICAL, OPTIONAL, INTENT(IN)      :: unpack_mask(:)

! Local declarations:
INTEGER                            :: idx
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_SetODBVal_array_char"
INTEGER                            :: i
INTEGER                            :: j
CHARACTER(len=8)                   :: local_values(SIZE (values))

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL ops_odb_varindex (handle,     &
                       table_name, &
                       var,        &
                       idx)

local_values = values

IF (PRESENT (mask)) THEN
  DO i = 1, SIZE (values)
    IF (mask(i)) THEN
      data(i,idx) = TRANSFER (local_values(i), data(i,idx))
    END IF
  END DO
ELSE IF (PRESENT (unpack_mask)) THEN
  j = 1
  DO i = 1, SIZE (unpack_mask)
    IF (unpack_mask(i)) THEN
      data(i,idx) = TRANSFER (local_values(j), data(i,idx))
      j = j + 1
    END IF
  END DO
ELSE
  DO i = 1, SIZE (values)
    data(i,idx) = TRANSFER (local_values(i), data(i,idx))
  END DO
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SetODBVal_array_char
