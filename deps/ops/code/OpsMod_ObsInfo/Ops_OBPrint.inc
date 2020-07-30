!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Print all non absent data fields in OB state.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_OBPrint (OB)

USE GenMod_Control, ONLY: &
    mype,                 &
    nproc

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    MessageOut,        &
    UseTrace

USE OpsMod_MiscTypes, ONLY: &
    Ops_SetObPrintIndex

IMPLICIT NONE

! Subroutine arguments:
CLASS (OB_type), INTENT(IN) :: OB      ! OB column

! Local declarations:
INTEGER                     :: i
INTEGER                     :: j
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_ObPrint"

!  Section 0.   initialise
!-------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (mype == 0 .AND. Ob % Header % NumObsLocal > 0) THEN

  WRITE (MessageOut, '(A)') ' ************** START PRINT OF OB header ***************'

  WRITE (MessageOut, '(A,I0)') ' OB: Number of Observations on PE 0 = ', Ob % Header % NumObsLocal
  WRITE (MessageOut, '(A,I0)') ' OB: NumObsTotal = ', OB % header % NumObsTotal
  DO i = 0, nproc - 1
    WRITE (MessageOut, '(A,I0,A)',ADVANCE = "NO") " OB: Obs per batch on PE ", i, " "
    DO j = 1, SIZE (OB % header % ObsPerBatchPerPE, DIM = 1)
      WRITE (MessageOut, '(I0)', ADVANCE = "NO") OB % header % ObsPerBatchPerPE(j,i)
      WRITE (MessageOut, '(A)', ADVANCE = "NO") ","
    END DO
    WRITE (MessageOut, '(A)') ""
    WRITE (MessageOut, '(A,I0,A,I0)') " OB: Total num obs on PE ", i, " = ", OB % header % ObsPerPE(i)
  END DO
  WRITE (MessageOut, '(A,I0)') ' OB: NumCx Batches = ', OB % header % NumCxBatches
  WRITE (MessageOut, '(A,6I6)') ' OB: Validity Time = ', OB % header % ValidityTime % year, &
                                                         OB % header % ValidityTime % month, &
                                                         OB % header % ValidityTime % day, &
                                                         OB % header % ValidityTime % hour, &
                                                         OB % header % ValidityTime % minute, &
                                                         OB % header % ValidityTime % second
  WRITE (MessageOut, '(A,I0)') ' OB: ObsGroup = ', OB % header % ObsGroup
  WRITE (MessageOut, '(A,L7)') ' OB: Model level data = ', OB % header % ModelLevel
  WRITE (MessageOut, '(A,I0)') ' OB: ObsRelease = ', OB % header % ObsRelease

  CALL Ops_SetObPrintIndex (Ob % Header % NumObsLocal)

  WRITE (MessageOut, '(A24,16I20)') ' OB columns', ObPrintIndex

  CALL Ops_ObsGlobalAction (Ob,                  &
                            Ops_ObsActionPrint1)
  CALL Ops_ObsGlobalAction (Ob,                  &
                            Ops_ObsActionPrint2)

  DEALLOCATE (ObPrintIndex)

  WRITE (MessageOut, '(A)') ' ************** END PRINT OF OB header ***************'

END IF ! mype==0

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ObPrint
