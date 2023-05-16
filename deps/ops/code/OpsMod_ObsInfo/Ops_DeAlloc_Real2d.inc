!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Deallocates a pointer array.  Check the assocation and deallocate if
! necessary, and set header status to absent.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_DeAlloc_Real2d (Hdr,   &
                               Name,  &
                               Real2)

USE GenMod_Control, ONLY: &
    DebugMode,            &
    GeneralMode,          &
    mype

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    MessageOut,        &
    UseTrace

USE OpsMod_MiscTypes, ONLY: &
    ElementHeader_Type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ElementHeader_Type), INTENT(INOUT) :: Hdr
CHARACTER(len=*), INTENT(IN)             :: Name
REAL, POINTER                            :: Real2(:,:)

! Local declarations:
LOGICAL                                  :: Done
CHARACTER(len=*), PARAMETER              :: RoutineName = "Ops_DeAlloc_Real2d"

!----------------------------------
! 0. Tracing and initialisations
!----------------------------------
IF (UseTrace) CALL gen_trace_entry (RoutineName)

Done = .FALSE.

Done = ASSOCIATED (Real2)
IF (Done) DEALLOCATE (Real2)

! Now set up the header
IF (Done) Hdr % Present = .FALSE.

! Output a diagnostic message if required
IF (GeneralMode >= DebugMode .AND. mype == 0 .AND. Done) THEN
  WRITE (MessageOut,'(A,A)') TRIM (Name),' DeAllocate OK'
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_DeAlloc_Real2d
