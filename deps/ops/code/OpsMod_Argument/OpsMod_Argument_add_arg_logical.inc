!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Add a real argument.
!-------------------------------------------------------------------------------

SUBROUTINE OpsMod_Argument_add_arg_logical (args,          &
                                            short_opt,     &
                                            long_opt,      &
                                            type,          &
                                            mandatory,     &
                                            initial_value)

USE GenMod_Core, ONLY: &
  gen_fail

IMPLICIT NONE

! Subroutine arguments:
CLASS (arguments_type), INTENT(INOUT) :: args
CHARACTER(len=*), INTENT(IN)          :: short_opt
CHARACTER(len=*), INTENT(IN)          :: long_opt
INTEGER, INTENT(IN)                   :: type
LOGICAL, INTENT(IN)                   :: mandatory
LOGICAL, INTENT(IN)                   :: initial_value

! Local declarations:
CHARACTER(len=200)                    :: message
TYPE (argument_type), ALLOCATABLE     :: tmp(:)
TYPE (argument_type)                  :: new_arg

IF (type < 1 .OR. type > 5) THEN
  WRITE (message, '(A,I0)') "Invalid type: ", type
  CALL gen_fail ("add_arg", message)
END IF

IF (ALLOCATED (args % args)) THEN
  ALLOCATE (tmp(SIZE (args % args)))
  tmp(:) = args % args(:)
  DEALLOCATE (args % args)
  ALLOCATE (args % args(SIZE (tmp) + 1))
  args % args(1:SIZE (tmp)) = tmp(:)
  new_arg % short_opt = short_opt
  new_arg % long_opt = long_opt
  new_arg % type = type
  new_arg % mandatory = mandatory
  new_arg % logical_val = initial_value
  args % args(SIZE (args % args)) = new_arg
ELSE
  ALLOCATE (args % args(1))
  new_arg % short_opt = short_opt
  new_arg % long_opt = long_opt
  new_arg % type = type
  new_arg % mandatory = mandatory
  new_arg % logical_val = initial_value
  args % args(SIZE (args % args)) = new_arg
END IF

END SUBROUTINE OpsMod_Argument_add_arg_logical
