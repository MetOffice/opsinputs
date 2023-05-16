!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! General trace within a subroutine.
!
! If called with sufficient priority, write a string of text to output
! stream (file or screen), noting the name of the traced routine. See GTDP 5
! for more details.
!-------------------------------------------------------------------------------

SUBROUTINE gen_do_trace (Name,       &
                         Message,    &
                         Messages,   &
                         MaxNoCalls)

USE GenMod_Control, ONLY: &
  DocumentationURL,       &
  mype

USE GenMod_Platform, ONLY: &
  Gen_FlushUnit,           &
  Gen_Memory,              &
  GenFn_ThreadCount

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)           :: Name         ! Subroutine name
CHARACTER(len=*), OPTIONAL, INTENT(IN) :: Message      ! Text to trace
CHARACTER(len=*), OPTIONAL, INTENT(IN) :: Messages(:)  ! Text to trace
INTEGER, OPTIONAL, INTENT(IN)          :: MaxNoCalls   ! max no calls to show

! Local declarations:
CHARACTER(len=1000), PARAMETER         :: pad = ""
CHARACTER(len=*), PARAMETER            :: RoutineName="gen_do_trace"
INTEGER                                :: Loop         ! General loop counter
INTEGER                                :: TotalSpace
INTEGER                                :: LocalMaxNoCalls
INTEGER                                :: Count
INTEGER                                :: CountRate
INTEGER                                :: CountMax
CHARACTER(len=33)                      :: Memory
CHARACTER(len=20)                      :: Timestamp = ""

!-----------------------------------------------------------------------
! Start of code
!-----------------------------------------------------------------------

IF (TraceTimes) THEN
  CALL SYSTEM_CLOCK (COUNT = Count,          &
                     COUNT_RATE = CountRate, &
                     COUNT_MAX = CountMax)
  IF (Count < BaseElapsedTime) THEN
    ! wrapped
    Count = Count + CountMax
  END IF
END IF

! test for calls to trace not wrapped in a IF (UseTrace) block. This
! impacts the speed of runs that aren't being traced!

IF (.NOT. UseTrace) THEN
  CALL gen_warn (RoutineName,                                                          &
                 "Routine called by " // TRIM (Name) // " even though UseTrace FALSE")
  RETURN
END IF

IF (GenFn_ThreadCount () > 1) RETURN

!-----------------------------------------------------------------------
! Check whether trace active and depth of trace
!-----------------------------------------------------------------------

IF (.NOT. TraceActive) THEN
  RETURN
END IF

IF (TraceDepth >= TraceMaxDepth) THEN
  ! already at maximum depth, so return
  RETURN
END IF

IF (TraceTimestamp .AND. TraceTimes) THEN
  IF (TraceRelTimeStamp) THEN
    WRITE (Timestamp, '(F10.2)') REAL (Count - Count0) / REAL (CountRate)
  ELSE
    CALL DATE_AND_TIME (DATE = Timestamp(1:8),   &
                        TIME = Timestamp(10:19))
  END IF
ELSE
  Timestamp = ""
END IF

!-----------------------------------------------------------------------
! Note memory usage
!-----------------------------------------------------------------------

IF (TraceMemory) THEN
  CALL Gen_Memory (TotalSpace)
  IF (LastSpace < TotalSpace) THEN
    WRITE (Memory, "(I0,A,I0)") TotalSpace - BaseMemory, ", bigger ", TotalSpace - LastSpace
  ELSE IF (LastSpace > TotalSpace) THEN
    WRITE (Memory, "(I0,A,I0)") TotalSpace - BaseMemory, ", smaller ", TotalSpace - LastSpace
  ELSE
    WRITE (Memory, "(I0)") TotalSpace - BaseMemory
  END IF
  IF (MaxHeap(Pointer) < TotalSpace) THEN
    MaxHeap(Pointer) = TotalSpace
  END IF
  LastSpace = TotalSpace
ELSE
  Memory = ""
  TotalSpace = 0
END IF

!-----------------------------------------------------------------------
! Perform the trace if not done too many times before. Only on PE 0
!-----------------------------------------------------------------------

IF (.NOT. ActiveCallTree) THEN
  IF (mype == TracePEnum .OR. TraceAllPEs) THEN

    IF (PRESENT (MaxNoCalls)) THEN
      LocalMaxNoCalls = MaxNoCalls
    ELSE
      LocalMaxNoCalls = TraceRepeatBody
    END IF

    NoCallsBody(Pointer) = NoCallsBody(Pointer) + 1

    IF (NoCallsBody(Pointer) <= LocalMaxNoCalls) THEN
      WRITE (TraceUnit, '(A, "| <a href=",A,"/",A,".html>",A,"</a> ",A," ",A)') &
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

    IF (NoCallsBody(Pointer) == TraceRepeatBody) THEN
      WRITE (TraceUnit, '(A,"  Called enough, going quiet")') &
        pad(1:TraceDepth * TraceIndentAmount)
    END IF
    IF (TraceFlush) CALL Gen_FlushUnit (TraceUnit)
  END IF ! mype==TracePEnum .OR. TraceAllPEs
END IF ! .NOT. ActiveCallTree

END SUBROUTINE gen_do_trace
