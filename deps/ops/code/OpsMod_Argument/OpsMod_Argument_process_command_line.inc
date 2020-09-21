!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Examine the command line and set up the args structure.
!-------------------------------------------------------------------------------

SUBROUTINE OpsMod_Argument_process_command_line (args, &
                                                 opts)

USE GenMod_Core, ONLY: &
  gen_fail

USE OpsMod_UtilsProgs, ONLY: &
  MaxLenCommandLine

IMPLICIT NONE

! Subroutine arguments:
CLASS (arguments_type), INTENT(INOUT)        :: args
CHARACTER(len=*), ALLOCATABLE, INTENT(INOUT) :: opts(:)

! Local declarations:
CHARACTER(len=MaxLenCommandLine), POINTER    :: CommandLine(:)
CHARACTER(len=MaxLenCommandLine)             :: Command
INTEGER                                      :: i
INTEGER                                      :: j
CHARACTER(len=500)                           :: list_char
CHARACTER(len=500)                           :: message
LOGICAL                                      :: found_command
CHARACTER(len=256), ALLOCATABLE              :: tmp_opts(:)

IF (.NOT. ALLOCATED (args % args)) THEN
  ALLOCATE (args % args(0))
END IF

CALL Ops_GetCommandLine (CommandLine)

IF (ASSOCIATED (CommandLine)) THEN
  i = 1
  DO
    IF (i > SIZE (CommandLine)) EXIT
    CommandLine(i) = ADJUSTL (CommandLine(i))
    IF (CommandLine(i)(1:2) == "--") THEN
      IF (SCAN (CommandLine(i), '=') > 1) THEN
        Command = CommandLine(i)(1:SCAN(CommandLine(i),'=') - 1)
        found_command = .FALSE.
        DO j = 1, SIZE (args % args)
          IF (Command == args % args(j) % long_opt) THEN
            found_command = .TRUE.
            Command = CommandLine(i)(SCAN(CommandLine(i),'=') + 1:)
            args % args(j) % was_set = .TRUE.
            SELECT CASE (args % args(j) % type)
              CASE (type_integer)
                READ (Command, *) args % args(j) % int_val
              CASE (type_real)
                READ (Command, *) args % args(j) % real_val
              CASE (type_logical)
                READ (Command, *) args % args(j) % logical_val
              CASE (type_character)
                READ (Command, '(A)') args % args(j) % char_val
              CASE (type_integer_list)
                READ (Command, '(A)') list_char
                CALL Ops_ConvertCommandLineList (list_char,                     &
                                                 args % args(j) % int_list_val)
              CASE DEFAULT
                WRITE (message, '(2(A,I0))') "Invalid argument type: ", args % args(j) % type, " for arg ", j
                CALL gen_fail ("process_command_line", &
                               message)
            END SELECT
            EXIT
          END IF
        END DO
        IF (.NOT. found_command) THEN
          message = "Invalid argument: " // Command
          CALL gen_fail ("process_command_line", &
                         message)
        END IF
      ELSE
        Command = CommandLine(i)
        found_command = .FALSE.
        DO j = 1, SIZE (args % args)
          IF (Command == args % args(j) % long_opt) THEN
            found_command = .TRUE.
            args % args(j) % logical_val = .TRUE.
            args % args(j) % was_set = .TRUE.
            EXIT
          END IF
        END DO
        IF (.NOT. found_command) THEN
          message = "Invalid argument: " // Command
          CALL gen_fail ("process_command_line", &
                         message)
        END IF
      END IF
    ELSE
      IF (ALLOCATED (opts)) THEN
        ALLOCATE (tmp_opts(SIZE (opts)))
        tmp_opts(:) = opts(:)
        DEALLOCATE (opts)
        ALLOCATE (opts(SIZE (tmp_opts) + 1))
        opts(1:SIZE (tmp_opts)) = tmp_opts(:)
        DEALLOCATE (tmp_opts)
        opts(SIZE (opts)) = CommandLine(i)
      ELSE
        ALLOCATE (opts(1))
        opts(1) = CommandLine(i)
      END IF
    END IF
    i = i + 1
  END DO
  DEALLOCATE (CommandLine)
ELSE
  CALL gen_fail ("process_command_line",  &
                 "Must state a filename")
END IF

END SUBROUTINE OpsMod_Argument_process_command_line
