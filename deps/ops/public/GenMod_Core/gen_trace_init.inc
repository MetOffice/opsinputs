!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Basic setup of trace variables. See gen_trace_initialise for the main setup
! of the tracing code.
!-------------------------------------------------------------------------------

SUBROUTINE gen_trace_init (ReturnCode,       &
                           Trace_Use,        &
                           Trace_Names,      &
                           Trace_Maxdepth,   &
                           Trace_RepeatHead, &
                           Trace_RepeatBody, &
                           Trace_Memory)

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(OUT)                   :: ReturnCode     ! general return code
CHARACTER(len=*), INTENT(IN), OPTIONAL :: Trace_Use
CHARACTER(len=*), INTENT(IN), OPTIONAL :: Trace_Names(:) ! left in to avoid breaking user programs
CHARACTER(len=*), INTENT(IN), OPTIONAL :: Trace_Maxdepth
CHARACTER(len=*), INTENT(IN), OPTIONAL :: Trace_RepeatHead
CHARACTER(len=*), INTENT(IN), OPTIONAL :: Trace_RepeatBody
CHARACTER(len=*), INTENT(IN), OPTIONAL :: Trace_Memory

! Local declarations:
CHARACTER(len=*), PARAMETER            :: RoutineName = "gen_trace_init"
INTEGER                                :: IOStatus             ! I/O return code
INTEGER                                :: TraceMaxDepth_temp
INTEGER                                :: TraceRepeatHead_temp
INTEGER                                :: TraceRepeatBody_temp

!===========================================================================

! Start of code

ReturnCode = StatusOK

IOStatus = 0

!-----------------------------------------------------------------
! 1. Apply system specific overrides to certain trace variables.
!-----------------------------------------------------------------

IF (PRESENT (Trace_Maxdepth)) THEN
  READ (Trace_MaxDepth, *, IOSTAT = IOStatus) TraceMaxDepth_temp
  IF (IOStatus == 0) THEN
    TraceMaxDepth = TraceMaxDepth_temp
  END IF
END IF
IF (PRESENT (Trace_RepeatHead)) THEN
  READ (Trace_RepeatHead, *, IOSTAT = IOStatus) TraceRepeatHead_temp
  IF (IOStatus == 0) THEN
    TraceRepeatHead = TraceRepeatHead_temp
  END IF
END IF
IF (PRESENT (Trace_RepeatBody)) THEN
  READ (Trace_RepeatBody, *, IOSTAT = IOStatus) TraceRepeatBody_temp
  IF (IOStatus == 0) THEN
    TraceRepeatBody = TraceRepeatBody_temp
  END IF
END IF
IF (PRESENT (Trace_Memory)) THEN
  IF (SCAN (Trace_Memory, 'tTyY') > 0) THEN
    TraceMemory = .TRUE.
  ELSE
    TraceMemory = .FALSE.
  END IF
END IF
IF (PRESENT (Trace_Use)) THEN
  IF (SCAN (Trace_Use, 'tTyY') > 0) THEN
    FlowTrace = .TRUE.
    BackTrace = .TRUE.
  ELSE
    FlowTrace = .FALSE.
    BackTrace = .FALSE.
  END IF
END IF
IF (PRESENT (Trace_Names)) THEN
  WRITE (ErrorOut, '(A)') RoutineName // &
     " called with Trace_Names argument, this argument is not used anymore"
END IF

END SUBROUTINE gen_trace_init
