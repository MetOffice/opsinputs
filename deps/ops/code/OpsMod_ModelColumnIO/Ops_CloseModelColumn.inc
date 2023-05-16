!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Closes the Cx file(s) and frees the unit number(s).  The Cx file is closed by
! a call to Ops_CloseCFile
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CloseModelColumn (ObType,     & ! Observation type
                                 CxUMheader)   ! UM header for obs file

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_ModelIO, ONLY: &
    UM_header_type

USE OpsMod_IOUtils, ONLY: &
    Ops_CloseCFile

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)         :: ObType
TYPE (UM_header_type), INTENT(INOUT) :: CxUMheader

! Local declarations:
CHARACTER(len=*), PARAMETER          :: RoutineName = "Ops_CloseModelColumn"
CHARACTER(len=*), PARAMETER          :: TypeOfDump = "Cx "

!-------------------------------------------------------------------------------
! 0. Tracing and initialisation
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName,                                          &
                                    Message = 'Closing Cx File(s) for ' // TRIM (ObType))

!-------------------------------------------------------------------------------
! 1. Rewrite Cx UM header
!-------------------------------------------------------------------------------

CALL CxUMHeader % write (TypeOfDump,            &
                         CheckLookup = .FALSE.)

!-------------------------------------------------------------------------------
! 2. Close Cx file(s) and free unit number(s)
!-------------------------------------------------------------------------------

CALL Ops_CloseCFile (CxUMHeader % UnitNum)

!-------------------------------------------------------------------------------
! 2. Free space used by Cx UM header
!-------------------------------------------------------------------------------

CALL CxUMHeader % dealloc

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CloseModelColumn
