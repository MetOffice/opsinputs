!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for fodb_register/unregister_vector.
!-------------------------------------------------------------------------------

#ifndef NO_ODB
SUBROUTINE ops_odb_register_vector (list, &
                                    ref)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE OpsMod_ODBKinds, ONLY: &
  odb_int,                 &
  odb_real

USE, INTRINSIC :: ISO_C_BINDING, ONLY: &
  C_DOUBLE,                            &
  C_INT

IMPLICIT NONE

! Subroutine arguments:
INTEGER(kind=odb_int), INTENT(IN) :: list(:)
REAL(kind=odb_real), INTENT(OUT)  :: ref

! Local declarations:
CHARACTER(len=*), PARAMETER       :: routine_name = "ops_odb_register_vector"
REAL(kind=C_DOUBLE)               :: local_ref

INTERFACE

SUBROUTINE fodb_unregister_in_vector (addr)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: &
    C_DOUBLE
  REAL(kind=C_DOUBLE) :: addr
END SUBROUTINE fodb_unregister_in_vector

SUBROUTINE fodb_register_in_vector (d,      &
                                    nd,     &
                                    is_int, &
                                    addr)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: &
    C_DOUBLE,                            &
    C_INT
  INTEGER(kind=C_INT)     :: d(*)
  INTEGER(kind=C_INT)     :: nd
  INTEGER(kind=C_INT)     :: is_int
  REAL(kind=C_DOUBLE)     :: addr
END SUBROUTINE fodb_register_in_vector

END INTERFACE

IF (UseTrace) CALL gen_trace_entry (routine_name)

local_ref = 0

CALL fodb_unregister_in_vector (local_ref)

CALL fodb_register_in_vector (list,                      &
                              SIZE (list, KIND = C_INT), &
                              1_C_INT,                   &
                              local_ref)

ref = local_ref

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_odb_register_vector

#else

SUBROUTINE ops_odb_register_vector (list, &
                                    ref)

USE GenMod_Core, ONLY: &
  gen_fail

USE OpsMod_ODBKinds, ONLY: &
  odb_int,                 &
  odb_real

IMPLICIT NONE

! Subroutine arguments:
INTEGER(kind=odb_int), INTENT(IN) :: list(:)
REAL(kind=odb_real), INTENT(OUT)  :: ref

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_register_vector"

CALL gen_fail (routine_name,                                   &
               "ODB support not compiled for this executable")

END SUBROUTINE ops_odb_register_vector

#endif
