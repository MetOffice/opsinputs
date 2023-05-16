!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Allocates a pointer array, and sets up the header.  Check the assocation and
! allocate if necessary. Initialise contents to missing. Then initialise the
! header.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_Alloc_Int (Hdr,           &
                          Name,          &
                          NumObs,        &
                          Int1,          &
                          HdrIn,         &
                          initial_value, &
                          zcode)

USE GenMod_Control, ONLY: &
    DebugMode,            &
    GeneralMode,          &
    mype

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    MessageOut,        &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI

USE OpsMod_MiscTypes, ONLY: &
    ElementHeader_Type,     &
    ZCodeSurface

IMPLICIT NONE

! Subroutine arguments:
TYPE (ElementHeader_Type), INTENT(INOUT)        :: Hdr
CHARACTER(len=*), INTENT(IN)                    :: Name
INTEGER, INTENT(IN)                             :: NumObs
INTEGER, POINTER                                :: Int1(:)
TYPE (ElementHeader_Type), OPTIONAL, INTENT(IN) :: HdrIn
INTEGER, OPTIONAL, INTENT(IN)                   :: initial_value
INTEGER, OPTIONAL, INTENT(IN)                   :: zcode

! Local declarations:
LOGICAL                                         :: Done
LOGICAL                                         :: Needed
INTEGER                                         :: NumLevLoc
INTEGER                                         :: ZCodeLoc
CHARACTER(len=*), PARAMETER                     :: RoutineName = "Ops_Alloc_Int"

!----------------------------------
! 0. Tracing and initialisations
!----------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

NumLevLoc = 1
ZCodeLoc = ZCodeSurface

IF (PRESENT (HdrIn)) THEN
  NumLevLoc = HdrIn % NumLev
  ZCodeLoc = HdrIn % ZCode
END IF
IF (PRESENT (zcode)) ZCodeLoc = zcode

Needed = .TRUE.
IF (PRESENT (HdrIn)) Needed = HdrIn % Present

Done = .FALSE.
IF (Needed) THEN
  Done = .NOT. ASSOCIATED (Int1)
  IF (Done) THEN
    ALLOCATE (Int1(NumObs))
    IF (PRESENT (initial_value)) THEN
      Int1(:) = initial_value
    ELSE
      Int1(:) = IMDI
    END IF
  END IF
END IF

! If allocated, and header is present, set up the header

IF (Done) THEN
  Hdr % Present = .TRUE.
  Hdr % NumLev = NumLevLoc
  Hdr % ZCode = ZCodeLoc
END IF

! Output a diagnostic message if required
IF (GeneralMode >= DebugMode .AND. mype == 0 .AND. Done) THEN
  WRITE (Messageout, '(2A)') TRIM (Name),' Allocate OK'
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_Alloc_Int
