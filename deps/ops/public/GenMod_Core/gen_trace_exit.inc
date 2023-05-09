!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Trace exit point of subroutine.
!
! If called with sufficient priority, write a string of text to output
! stream (file or screen), normally including the function name. Store
! timing information.
!-------------------------------------------------------------------------------

SUBROUTINE gen_trace_exit (Name,       &
                           Message,    &
                           Messages,   &
                           MaxNoCalls)

USE GenMod_Control, ONLY: &
  DocumentationURL,       &
  mype

USE GenMod_Platform, ONLY: &
  Gen_FlushUnit,           &
  Gen_Memory,              &
  GenFn_ThreadCount,       &
  GenFn_ThreadNum

USE parkind1, ONLY: &
  jpim

USE yomhook, ONLY: &
  dr_hook,         &
  lhook

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)           :: Name         ! subroutine name
CHARACTER(len=*), OPTIONAL, INTENT(IN) :: Message      ! text to trace
CHARACTER(len=*), OPTIONAL, INTENT(IN) :: Messages(:)  ! text to trace
INTEGER, OPTIONAL, INTENT(IN)          :: MaxNoCalls   ! max no calls to show

! Local declarations:
CHARACTER(len=1000), PARAMETER         :: pad = ""
CHARACTER(len=*), PARAMETER            :: RoutineName = "gen_trace_exit"
INTEGER                                :: IOStatus        ! I-O return code
INTEGER                                :: Loop            ! General loop counter
INTEGER                                :: Count
INTEGER                                :: CountRate
INTEGER                                :: CountMax
INTEGER                                :: TotalSpace
INTEGER                                :: LocalMaxNoCalls
INTEGER                                :: Caller
INTEGER                                :: thread
REAL                                   :: temp_CPUTime
REAL                                   :: temp1
REAL                                   :: temp2
CHARACTER(len=33)                      :: Memory
CHARACTER(len=20)                      :: Timestamp = ""

!======================================================================
! Start of code
!======================================================================

! test for calls to trace not wrapped in a IF (UseTrace) block. This
! impacts the speed of runs that aren't being traced!

IF (.NOT. UseTrace) THEN
  CALL gen_warn (RoutineName,                                                          &
                 "Routine called by " // TRIM (Name) // " even though UseTrace FALSE")
  RETURN
END IF

IF (lhook) THEN
  thread = GenFn_ThreadNum ()
  dr_hook_pointer(thread) = dr_hook_pointer(thread) - 1
  CALL Dr_Hook (Name,                                                 &
                1_jpim,                                               &
                dr_hook_handle_stack(dr_hook_pointer(thread),thread))
END IF

! Problems with native tracing with threaded code. Simplest
! solution is to not trace routines like that.
IF (GenFn_ThreadCount () > 1) RETURN

IF (BackTrace) THEN
  TraceStackPointer = TraceStackPointer - 1
END IF

IF (FlowTrace) THEN

  IF (TraceTimes) THEN
    ! stop the clock
    temp_CPUTime = gen_cpu_time () ! in clock ticks

    CALL SYSTEM_CLOCK (COUNT = Count,          &
                       COUNT_RATE = CountRate, &
                       COUNT_MAX = CountMax)
    IF (Count < BaseElapsedTime) THEN
      ! wrapped
      Count = Count + CountMax
    END IF
  END IF

  !======================================================================
  ! check whether trace active and whether depth exceeded
  !======================================================================

  IF (.NOT. TraceActive) THEN
    RETURN
  END IF

  IF (TraceActive) THEN
    ! was tracing enabled by this routine? If it was, disable it, to
    ! take affect after the trace line has been written
    IF (Name == TraceStartedBy(1:LEN (Name))) THEN
      TraceActive = .FALSE.
    END IF
  END IF

  IF (TraceTimes) THEN
    !=======================================================================
    ! Check timing and maximum heap memory usage
    !=======================================================================

    temp1 = REAL (Count - BaseElapsedTime) - ElapsedTimeLocalStart
    temp2 = temp_CPUTime - CPUTimeLocalStart

    ElapsedTimeLocal(Pointer) = ElapsedTimeLocal(Pointer) + temp1
    ElapsedTimeThisCall(Pointer) = ElapsedTimeThisCall(Pointer) + temp1
    ElapsedTime(Pointer) = ElapsedTime(Pointer) + ElapsedTimeThisCall(Pointer)

    CPUTimeLocal(Pointer) = CPUTimeLocal(Pointer) + temp2
    CPUTimeThisCall(Pointer) = CPUTimeThisCall(Pointer) + temp2
    CPUTime(Pointer) = CPUTime(Pointer) + CPUTimeThisCall(Pointer)

    Caller = CalledBy(Pointer)
    IF (Caller /= 0) THEN
      ElapsedTimeThisCall(Caller) = ElapsedTimeThisCall(Caller) + &
                                                ElapsedTimeThisCall(Pointer)
      CPUTimeThisCall(Caller) = CPUTimeThisCall(Caller) + CPUTimeThisCall(Pointer)
    END IF
  END IF

  IF (TraceMemory) THEN
    CALL Gen_Memory (TotalSpace)
    IF (EntryHeap(Pointer) < TotalSpace) THEN
      WRITE (Memory, "(I0,A,I0)") TotalSpace - BaseMemory, ", BIGGER ", TotalSpace - EntryHeap(Pointer)
    ELSE IF (EntryHeap(Pointer) > TotalSpace) THEN
      WRITE (Memory, "(I0,A,I0)") TotalSpace - BaseMemory, ", SMALLER ", TotalSpace - EntryHeap(Pointer)
    ELSE
      WRITE (Memory, "(I0)") TotalSpace - BaseMemory
    END IF
    IF (MaxHeap(Pointer) < TotalSpace) THEN
      MaxHeap(Pointer) = TotalSpace
    END IF
  ELSE
    TotalSpace = 0
    Memory = ""
  END IF

  IF (TraceTimestamp .AND. TraceTimes) THEN
    IF (TraceRelTimeStamp) THEN
      WRITE (Timestamp, '(F10.2)', IOSTAT = IOStatus) &
        REAL (Count - Count0) / REAL (CountRate)
    ELSE
      CALL DATE_AND_TIME (DATE = Timestamp(1:8),   &
                          TIME = Timestamp(10:19))
    END IF
  ELSE
    Timestamp = ""
  END IF

  !-----------------------------------------------------------------------
  ! Print trace of this call :
  !   only on PE TracePEnum,  only if TraceDepth <= TraceMaxDepth,
  !   only if this routine has not been traced too often already.
  !-----------------------------------------------------------------------

  IF (PRESENT (MaxNoCalls)) THEN
    LocalMaxNoCalls = MaxNoCalls
  ELSE
    LocalMaxNoCalls = TraceRepeatHead
  END IF

  IF (.NOT. ActiveCallTree) THEN
    TraceDepth = TraceDepth - 1
  END IF

  IF (NoCalls(Pointer) <= LocalMaxNoCalls) THEN

    IF (TraceDepth < 0) THEN
      TraceDepth = 0
    END IF

    IF (ActiveCallTree) THEN

      OldOldTraceDepth = OldTraceDepth
      OldTraceDepth = TraceDepth
      TraceDepth = TraceDepth - 1

      IF ((mype == TracePEnum .OR. TraceAllPEs) .AND. &
           TraceDepth <= TraceMaxDepth) THEN

        IOStatus = 0

        ! Close off the list of children, if required

        IF (OldOldTraceDepth == TraceDepth + 2 .AND. TraceDepth < TraceMaxDepth) THEN
          WRITE (CallTreeUnit, '(A)') '],'
        END IF

        ! Write out the memory information

        WRITE (CallTreeUnit, '(A)') '"memory" : "' // TRIM (Memory) // '"'

        ! Let the user know if we have reached the limit of trace output

        IF (NoCalls(Pointer) == TraceRepeatHead) THEN
          WRITE (CallTreeUnit, '(A)') ','

          WRITE (CallTreeUnit, '(A)') '"exit"     : "Called enough, going quiet"'
        END IF

        ! Finish the output for this routine

        WRITE (CallTreeUnit, '(A)') '}'

        IF (TraceFlush) CALL Gen_FlushUnit (CallTreeUnit)

      END IF ! mype==TracePEnum .OR. TraceAllPEs

    ELSE
      IOStatus = 0

      IF ((mype == TracePEnum .OR. TraceAllPEs) .AND. &
           TraceDepth <= TraceMaxDepth) THEN
        WRITE (TraceUnit, '(A, "&lt; <a href=",A,"/",A,".html>",A,"</a> ",A," ",A)') &
          pad(1:TraceDepth * TraceIndentAmount), TRIM (DocumentationURL), TRIM (Name), &
          TRIM (Name), TRIM (Timestamp), TRIM (Memory)

        IF (PRESENT (Message)) THEN
          WRITE (TraceUnit, '(A," ",A)') &
            pad(1:TraceDepth * TraceIndentAmount), TRIM (Message)
        END IF

        IF (PRESENT (Messages)) THEN
          DO Loop = 1, SIZE (Messages)
            WRITE (TraceUnit, '(A," ",A)') &
              pad(1:TraceDepth * TraceIndentAmount), TRIM (Messages(Loop))
          END DO ! Loop
        END IF

        IF (NoCalls(Pointer) == TraceRepeatHead) THEN
          WRITE (TraceUnit, '(A,"  Called enough, going quiet")') &
            pad(1:TraceDepth * TraceIndentAmount)
        END IF

        IF (TraceFlush) CALL Gen_FlushUnit (TraceUnit)

      END IF ! mype == TracePEnum .OR. TraceAllPEs
    END IF ! ActiveCallTree
  END IF ! NoCalls(Pointer) <= LocalMaxNoCalls

  ! Restore pointer
  Pointer = CalledBy(Pointer)

  IF (TraceTimes) THEN
    ! note local time

    CALL SYSTEM_CLOCK (COUNT = Count,        &
                       COUNT_MAX = CountMax)
    IF (Count < BaseElapsedTime) THEN
      ! wrapped
      Count = Count + CountMax
    END IF

    ElapsedTimeLocalStart = REAL (Count - BaseElapsedTime)
    CPUTimeLocalStart = gen_cpu_time () ! in clock ticks
  END IF
END IF

END SUBROUTINE gen_trace_exit
