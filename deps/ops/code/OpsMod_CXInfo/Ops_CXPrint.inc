!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
!-------------------------------------------------------------------------------
! Print selected columns from a CX structure for all non-absent data files.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CXPrint (CX)

USE GenMod_Control, ONLY: &
  mype

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  MessageOut,          &
  UseTrace

USE OpsMod_MiscTypes, ONLY: &
  ActionPrint1,             &
  ObPrintIndex,             &
  Ops_SetObPrintIndex

IMPLICIT NONE

! Subroutine arguments:
CLASS (Cx_type), INTENT (IN) :: CX      ! CX column

! Local declarations:
INTEGER                      :: NumLocal ! Number of cx columns on this PE
CHARACTER(len=*), PARAMETER  :: RoutineName = "Ops_CXPrint"

!  Section 0.   initialise
!---------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! Only worry about the selection of obs on PE 0 for the moment

NumLocal = CX % header % NumLocal
IF (mype == 0 .AND. NumLocal > 0) THEN

  WRITE (MessageOut, '(A)') ' ************** START PRINT OF CX header ***************'

  WRITE (MessageOut, '(A,I0)') ' CX: Number of Columns on PE 0 = ',NumLocal

  WRITE (MessageOut, '(A,L7)') ' CX: Rotated = ', CX % header % Rotated
  WRITE (MessageOut, '(A,F16.2)') ' CX: PoleLat = ', CX % header % PoleLat
  WRITE (MessageOut, '(A,F16.2)') ' CX: PoleLong = ', CX % header % PoleLong
  WRITE (MessageOut, '(A,5(I0,1x),I0)') ' CX: Validity Time = ', CX % header % ValidityTime
  WRITE (MessageOut, '(A,I0)') ' CX: ObsGroup = ', CX % header % ObsGroup

  CALL Ops_SetObPrintIndex (NumLocal)

  WRITE (MessageOut, '(A24,16I12)') ' CX columns ', ObPrintIndex
  CALL Ops_CXGlobalAction (CX,           &
                           ActionPrint1)

  DEALLOCATE (ObPrintIndex)

  WRITE (MessageOut,'(A)') ' ************** END PRINT OF CX header ***************'

END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CXPrint
