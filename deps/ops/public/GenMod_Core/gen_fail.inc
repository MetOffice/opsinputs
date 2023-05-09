!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Report a fatal error. Terminate the execution.
!
! Implementation should follow the logic below:
!
!   Local = .true.
!         Print out on all PEs
!         Finalise output streams
!         Sleep ErrorSleep (to give all processors a chance to finish)
!         Abort
!   Local = .false.
!         PE 0:
!            Print out on PE 0
!            Finalise output streams
!            Sleep ErrorSleep (to give other processors a chance to finish)
!            Abort
!         Other PEs:
!            Finalise output streams
!            Sleep 2 * ErrorSleep (to give PE 0 a chance to finish and abort)
!            Print out on all PEs
!            Finalise output streams
!            Sleep ErrorSleep (to give all processors a chance to finish)
!            Abort
!   Local = not present
!      Default to Local = .false.
!-------------------------------------------------------------------------------

RECURSIVE SUBROUTINE gen_fail_array (NameOfRoutine, &
                                     Message,       &
                                     Local,         &
                                     ErrorSummary)

USE GenMod_Control, ONLY: &
  mype,                   &
  nproc,                  &
  ProduceHTML,            &
  ErrorSleep

USE GenMod_Platform, ONLY: &
  Gen_FlushUnit

USE GenMod_Sleep, ONLY: &
  Gen_Sleep

USE OpsMod_Gcom, ONLY: &
  gc_abort

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT (IN)   :: NameOfRoutine ! calling procedure
CHARACTER(len=*), INTENT (IN)   :: Message(:)    ! message to report
LOGICAL, OPTIONAL, INTENT (IN)  :: Local         ! is message local to the PE?
LOGICAL, OPTIONAL, INTENT (IN)  :: ErrorSummary  ! Call gen_summary?

! Local declarations:
CHARACTER(len=256), ALLOCATABLE :: mesg(:)  ! header + Message(:)
CHARACTER(len=32)               :: html_s   ! HTML start
CHARACTER(len=16)               :: label    ! fatal or warning
CHARACTER(len=8)                :: html_e   ! HTML end
CHARACTER(len=8)                :: colour   ! HTML colour
LOGICAL                         :: is_local ! is message local to the PE?
LOGICAL                         :: local_ErrorSummary

! Is the message local to this PE?
IF (PRESENT (Local)) THEN
  is_local = Local
ELSE
  is_local = .FALSE.
END IF

IF (PRESENT (ErrorSummary)) THEN
  local_ErrorSummary = ErrorSummary
ELSE
  local_ErrorSummary = .TRUE.
END IF

colour = ColourFatal
label = 'FATAL ERROR'

IF (ProduceHTML) THEN
  html_s = '<span style="color: ' // TRIM (colour) // '">'
  html_e = '</span>'
ELSE
  html_s = ''
  html_e = ''
END IF

! Set up the message to report
ALLOCATE (mesg (SIZE (Message) + 1))

mesg(2:) = Message(1:)
mesg(1) = TRIM (html_s) // TRIM (label) // TRIM (html_e)

IF (mype == 0 .OR. is_local) THEN
  ! Write modified message to both stdout and stderr.
  CALL gen_message (NameOfRoutine, &
                    mesg,          &
                    ErrorOut,      &
                    is_local)
  CALL gen_message (NameOfRoutine, &
                    mesg,          &
                    MessageOut,    &
                    is_local)
END IF

IF (local_ErrorSummary) CALL gen_summary

! Flush output streams
CALL Gen_FlushUnit (MessageOut)
CALL Gen_FlushUnit (ErrorOut)

! Finalise trace
IF (FlowTrace .AND. (mype == TracePEnum .OR. TraceAllPEs)) THEN
  WRITE (TraceUnit, '(A)') "</body></html>"
  CLOSE (TraceUnit)
END IF

IF (mype /= 0 .AND. .NOT. is_local) THEN
  ! Give PE 0 a chance to report the error and exit the program.
  CALL Gen_Sleep (2 * ErrorSleep)

  ! Write modified message to both stdout and stderr of all PEs
  CALL gen_message (NameOfRoutine,  &
                    mesg,           &
                    ErrorOut,       &
                    Local = .TRUE.)
  CALL gen_message (NameOfRoutine,  &
                    mesg,           &
                    MessageOut,     &
                    Local = .TRUE.)

  ! Flush output streams
  CALL Gen_FlushUnit (MessageOut)
  CALL Gen_FlushUnit (ErrorOut)
END IF

! Abort
IF (nproc > 1) THEN
  ! Give all PE's a chance to finalise their output streams
  CALL Gen_Sleep (ErrorSleep)
END IF
CALL gc_abort (mype,       &
               nproc,      &
               Message(1))

END SUBROUTINE gen_fail_array

SUBROUTINE gen_fail_scalar (NameOfRoutine, &
                            Message,       &
                            Local)

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT (IN)  :: NameOfRoutine ! name of calling procedure
CHARACTER(len=*), INTENT (IN)  :: Message       ! message to output
LOGICAL, OPTIONAL, INTENT (IN) :: Local         ! message local to this PE?

CALL gen_fail_array (NameOfRoutine, &
                     (/Message/),   &
                     Local = Local)

END SUBROUTINE gen_fail_scalar
