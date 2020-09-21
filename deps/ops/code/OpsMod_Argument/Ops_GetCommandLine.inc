!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Get the command line arguments as a single string.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_GetCommandLine (CommandLine)

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), POINTER :: CommandLine(:)

! Local declarations:
INTEGER                   :: NumArgs
INTEGER                   :: i

NumArgs = COMMAND_ARGUMENT_COUNT ()

ALLOCATE (CommandLine(NumArgs))

DO i = 1, NumArgs
  CALL GET_COMMAND_ARGUMENT (i,              &
                             CommandLine(i))
END DO

END SUBROUTINE Ops_GetCommandLine
