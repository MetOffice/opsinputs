!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Trace entry point to subroutine
!
! If called with sufficient priority, write a string of text to output
! stream (file or screen), noting the name of the traced routine. Store
! timing information. See GTDP 5 for more details.
!-------------------------------------------------------------------------------

SUBROUTINE gen_trace_entry (Name,       &
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
CHARACTER(len=*), INTENT(IN)           :: Name         ! Routine name
CHARACTER(len=*), OPTIONAL, INTENT(IN) :: Message      ! message
CHARACTER(len=*), OPTIONAL, INTENT(IN) :: Messages(:)  ! message array
INTEGER, OPTIONAL, INTENT(IN)          :: MaxNoCalls   ! max no calls to show

! Local declarations:
CHARACTER(len=1000), PARAMETER         :: pad = ""
CHARACTER(len=*), PARAMETER            :: RoutineName = "gen_trace_entry"
INTEGER                                :: Loop            ! General loop counter
INTEGER                                :: Count
INTEGER                                :: CountRate
INTEGER                                :: CountMax
INTEGER                                :: OldPointer
INTEGER                                :: TotalSpace
INTEGER                                :: LocalMaxNoCalls
INTEGER                                :: thread
REAL                                   :: CPUTime
REAL                                   :: temp1
REAL                                   :: temp2
LOGICAL                                :: NewRoutine
CHARACTER(len=11)                      :: Memory
CHARACTER(len=20)                      :: Timestamp = ""
LOGICAL, SAVE                          :: FirstTime = .TRUE.
LOGICAL, SAVE                          :: FirstWrite = .TRUE.

!-----------------------------------------------------------------------
! Start of code
!-----------------------------------------------------------------------

IF (FirstTime) THEN
  CALL gen_trace_initialise (Name = Name)
  FirstTime = .FALSE.
  IF (.NOT. UseTrace) RETURN
END IF

! test for calls to trace not wrapped in a IF (UseTrace) block. This
! impacts the speed of runs that aren't being traced!

IF (.NOT. UseTrace) THEN
  CALL gen_warn (RoutineName,                                                          &
                 "Routine called by " // TRIM (Name) // " even though UseTrace FALSE")
  RETURN
END IF

IF (lhook) THEN
  thread = GenFn_ThreadNum ()
  CALL Dr_Hook (Name,                                                 &
                0_jpim,                                               &
                dr_hook_handle_stack(dr_hook_pointer(thread),thread))
  dr_hook_pointer(thread) = dr_hook_pointer(thread) + 1
END IF

! Problems with native tracing with threaded code. Simplest
! solution is to not trace routines like that.
IF (GenFn_ThreadCount () > 1) RETURN

IF (BackTrace) THEN

  TraceStackPointer = TraceStackPointer + 1
  IF (TraceStackPointer <= TraceMaxStackPointer) THEN
    TraceStack(TraceStackPointer) = Name
  END IF

END IF

IF (FlowTrace) THEN

  ! Stop the clock

  IF (TraceTimes) THEN
    CPUTime = gen_cpu_time () ! in clock ticks

    CALL SYSTEM_CLOCK (COUNT = Count,          &
                       COUNT_RATE = CountRate, &
                       COUNT_MAX = CountMax)
    IF (Count < BaseElapsedTime) THEN
      ! wrapped
      Count = Count + CountMax
    END IF
  END IF

  !-----------------------------------------------------------------------
  ! check if tracing active. If not check whether to switch it on
  !-----------------------------------------------------------------------

  IF (.NOT. TraceActive) THEN
    IF (NoTraceNames == 0) THEN
      ! start with first call
      TraceActive = .TRUE.
    ELSE
      DO Loop = 1, NoTraceNames
        IF (Name == TraceNames(Loop)(1:LEN (Name))) THEN
          TraceActive = .TRUE.
          TraceDepth = 0
          TraceStartedBy = Name
          EXIT
        END IF
      END DO
    END IF
    IF (.NOT. TraceActive) THEN
      ! didn't want to start trace, so leave
      RETURN
    END IF
  END IF

  !-----------------------------------------------------------------------
  ! timing and maximum heap usage
  !-----------------------------------------------------------------------

  IF (TraceTimes) THEN
    ! Increment the local elapsed time and local CPU time since the
    ! last trace entry, if any

    IF (Pointer /= 0) THEN
      temp1 = REAL (Count - BaseElapsedTime) - ElapsedTimeLocalStart
      temp2 = CPUTime - CPUTimeLocalStart
      ElapsedTimeLocal(Pointer) = ElapsedTimeLocal(Pointer) + temp1
      ElapsedTimeThisCall(Pointer) = ElapsedTimeThisCall(Pointer) + temp1
      CPUTimeLocal(Pointer) = CPUTimeLocal(Pointer) + temp2
      CPUTimeThisCall(Pointer) = CPUTimeThisCall(Pointer) + temp2
    END IF
  END IF

  OldPointer = Pointer

  ! Check subroutine name

  NewRoutine = .TRUE.
  DO Pointer = 1, NoRoutines
    IF (TimerNames(Pointer) == Name) THEN
      NewRoutine = .FALSE.
      EXIT
    END IF
  END DO

  IF (NewRoutine) THEN
    ! New subroutine entered
    IF (NoRoutines >= MaxNoRoutines) THEN ! too many to trace
      CALL gen_warn (RoutineName,                                     &
                     "Too many routines. Not timing " // TRIM (Name))
      ! NB warning messages only appear from pe0.

      ! All the timings etc are put instead to the calling routine,
      ! which therefore may have incorrect summaries.
      ! The best solution is to increase MaxNoRoutines.
      Pointer = OldPointer
      ! Fix to get the correct NoCalls(OldPointer) despite the +1 later
      NoCalls(Pointer) = NoCalls(Pointer) - 1

    ELSE ! Pointer=NoRoutines+1 (from the end of earlier DO loop)
      NoRoutines = NoRoutines + 1
      TimerNames(NoRoutines) = Name
    END IF
  END IF

  NoCalls(Pointer) = NoCalls(Pointer) + 1

  IF (TraceTimes) THEN
    CPUTimeThisCall(Pointer) = 0.0
    ElapsedTimeThisCall(Pointer) = 0.0
  END IF

  CalledBy(Pointer) = OldPointer

  IF (TraceMemory) THEN
    CALL Gen_Memory (TotalSpace)
    EntryHeap(Pointer) = TotalSpace
    LastSpace = TotalSpace
    IF (MaxHeap(Pointer) < TotalSpace) THEN
      MaxHeap(Pointer) = TotalSpace
    END IF
    WRITE (Memory, '(I0)') TotalSpace - BaseMemory
  ELSE
    Memory = ""
    TotalSpace = 0
  END IF

  IF (TraceTimes .AND. TraceTimestamp) THEN
    IF (TraceRelTimeStamp) THEN
      WRITE (Timestamp, '(F10.2)') &
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

  IF (ActiveCallTree) THEN
    IF (PRESENT (MaxNoCalls)) THEN
      LocalMaxNoCalls = MaxNoCalls
    ELSE
      LocalMaxNoCalls = TraceRepeatHead
    END IF

    IF (NoCalls(Pointer) <= LocalMaxNoCalls) THEN

      IF ((mype == TracePEnum .OR. TraceAllPEs) .AND. &
          TraceDepth <= TraceMaxDepth) THEN

        IF (OldTraceDepth < TraceDepth) THEN
          WRITE (CallTreeUnit, '(A)') '"children" : ['
        ELSE
          IF (.NOT. FirstWrite) THEN
            WRITE (CallTreeUnit, '(A)') ','
          ELSE
            FirstWrite = .FALSE.
          END IF
        END IF
        WRITE (CallTreeUnit, '(A)') '{'
        WRITE (CallTreeUnit, '(A)') '"name"     : "' // TRIM (Name) // '",'
        WRITE (CallTreeUnit, '(A)') '"link"     : "' // TRIM (DocumentationURL) // '/' // TRIM (Name) // '",'

      END IF
      IF (TraceFlush) CALL Gen_FlushUnit (CallTreeUnit)

      OldOldTraceDepth = OldTraceDepth
      OldTraceDepth = TraceDepth
      TraceDepth = TraceDepth + 1

    END IF ! mype==TracePEnum .OR. TraceAllPEs

  ELSE
    IF ((mype == TracePEnum .OR. TraceAllPEs) .AND. &
        TraceDepth <= TraceMaxDepth) THEN

      IF (PRESENT (MaxNoCalls)) THEN
        LocalMaxNoCalls = MaxNoCalls
      ELSE
        LocalMaxNoCalls = TraceRepeatHead
      END IF

      IF (NoCalls(Pointer) <= LocalMaxNoCalls) THEN
        WRITE (TraceUnit, '(A,"&gt; <a href=",A,"/",A,".html>",A,"</a> ",A," ",A)') &
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
      END IF
      IF (TraceFlush) CALL Gen_FlushUnit (TraceUnit)

    END IF ! mype==TracePEnum .OR. TraceAllPEs  END IF

    TraceDepth = TraceDepth + 1
  END IF

  IF (TraceTimes) THEN
    CALL SYSTEM_CLOCK (COUNT = Count,        &
                       COUNT_MAX = CountMax)
    IF (Count < BaseElapsedTime) THEN
      ! wrapped
      Count = Count + CountMax
    END IF

    CPUTime = gen_cpu_time () ! in clock ticks

    ! set the start elapsed and CPU time both locally and generally

    ElapsedTimeStart(Pointer) = REAL (Count - BaseElapsedTime)
    ElapsedTimeLocalStart = REAL (Count - BaseElapsedTime)

    CPUTimeStart(Pointer) = CPUTime
    CPUTimeLocalStart = CPUTime
  END IF

END IF

END SUBROUTINE gen_trace_entry
