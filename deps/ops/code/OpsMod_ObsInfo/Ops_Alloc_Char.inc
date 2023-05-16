!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Allocates a pointer array, and sets up the header.  Check the assocation and
! allocate if necessary. Initialise contents to missing. Then initialise the
! header.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_Alloc_Char (Hdr,           &
                           Name,          &
                           NumObs,        &
                           Char,          &
                           HdrIn,         &
                           initial_value)

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
    ElementHeader_Type,     &
    ZCodeSurface

IMPLICIT NONE

! Subroutine arguments:
TYPE (ElementHeader_Type), INTENT(INOUT)        :: Hdr
CHARACTER(len=*), INTENT(IN)                    :: Name
INTEGER, INTENT(IN)                             :: NumObs
CHARACTER(len=*), POINTER                       :: Char(:)
TYPE (ElementHeader_Type), OPTIONAL, INTENT(IN) :: HdrIn
CHARACTER(len=*), OPTIONAL, INTENT(IN)          :: initial_value

! Local declarations:
LOGICAL                                         :: Done
LOGICAL                                         :: Needed
INTEGER                                         :: NumLevLoc
CHARACTER(len=*), PARAMETER                     :: RoutineName = "Ops_Alloc_Char"

!----------------------------------
! 0. Tracing and initialisations
!----------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

NumLevLoc = 1

IF (PRESENT (HdrIn)) NumLevLoc = HdrIn % NumLev

Needed = .TRUE.
IF (PRESENT (HdrIn)) Needed = HdrIn % Present

Done = .FALSE.
IF (Needed) THEN
  Done = .NOT. ASSOCIATED (Char)
  IF (Done) THEN
    ALLOCATE (Char(NumObs))
    IF (PRESENT (initial_value)) THEN
      Char(:) = initial_value
    ELSE
      Char(:) = ''
    END IF
  END IF
END IF

! If allocated, and header is present, set up the header

IF (Done) THEN
  Hdr % Present = .TRUE.
  Hdr % NumLev = NumLevLoc
  IF (PRESENT (HdrIn)) THEN
    Hdr % ZCode = HdrIn % ZCode
  ELSE
    Hdr % ZCode = ZCodeSurface
  END IF
END IF

! Output a diagnostic message if required
IF (GeneralMode >= DebugMode .AND. mype == 0 .AND. Done) THEN
  WRITE (MessageOut, '(2A)') TRIM (Name),' Allocate OK'
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_Alloc_Char
