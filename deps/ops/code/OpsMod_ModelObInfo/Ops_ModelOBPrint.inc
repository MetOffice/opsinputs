!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Print all non Absent data fields in Y (Model Ob equivalent) state.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ModelOBPrint (ModelOB)

USE GenMod_Control, ONLY: &
    mype

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    MessageOut,        &
    UseTrace

USE OpsMod_MiscTypes, ONLY: &
    Ops_SetObPrintIndex

USE OpsMod_ObsInfo, ONLY: &
    ObPrintIndex,         &
    Ops_ObsActionPrint1

IMPLICIT NONE

! Subroutine arguments:
CLASS (ModelOB_type), INTENT (IN) :: ModelOB ! ModelOB column

! Local declarations:
CHARACTER(len=*), PARAMETER       :: RoutineName = "Ops_ModelOBPrint"
INTEGER                           :: NumObsLocal    ! Number of ob columns on this PE

!---------------------------
!  Section 0.   initialise
!---------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! Only handle the selection of obs on PE 0 for the moment

NumObsLocal = ModelOB % header % NumObsLocal

IF (mype == 0 .AND. NumObsLocal > 0) THEN

  WRITE (MessageOut, '(A)') ' ************** START PRINT OF ModelOB header ***************'

  WRITE (MessageOut, '(A,10I6)') ' ModelOB: Validity Time =', ModelOB % header % ValidityTime
  WRITE (MessageOut, '(A,I0)') ' ModelOB: ObsGroup = ', ModelOB % header % ObsGroup

  CALL Ops_SetObPrintIndex (NumObsLocal)

  WRITE (MessageOut, '(A24,16I12)') ' ModelOB columns', ObPrintIndex

  CALL Ops_ModelObGlobalAction (ModelOb,             &
                                Ops_ObsActionPrint1)

  DEALLOCATE (ObPrintIndex)

END IF ! mype==0

IF (mype == 0) WRITE (MessageOut, '(A)')' ************** END PRINT OF ModelOB header ***************'

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ModelOBPrint
