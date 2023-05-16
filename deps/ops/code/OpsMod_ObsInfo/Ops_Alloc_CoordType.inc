!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Allocates a pointer array, and sets up the header.  Check the assocation and
! allocate if necessary. Initialise contents to missing. Then initialise the
! header.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_Alloc_CoordType (Hdr,           &
                                Name,          &
                                NumObs,        &
                                Coord1,        &
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

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE OpsMod_MiscTypes, ONLY: &
    ElementHeader_Type,     &
    Coord_Type,             &
    ZCodeSurface

IMPLICIT NONE

! Subroutine arguments:
TYPE (ElementHeader_Type), INTENT(INOUT)        :: Hdr
CHARACTER(len=*), INTENT(IN)                    :: Name
INTEGER, INTENT(IN)                             :: NumObs
TYPE (Coord_Type), POINTER                      :: Coord1(:)
TYPE (ElementHeader_Type), OPTIONAL, INTENT(IN) :: HdrIn
TYPE (Coord_Type), OPTIONAL, INTENT(IN)         :: initial_value

! Local declarations:
LOGICAL                                         :: Done
LOGICAL                                         :: Needed
INTEGER                                         :: NumLevLoc
CHARACTER(len=*), PARAMETER                     :: RoutineName = "Ops_Alloc_CoordType"

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
  Done = .NOT. ASSOCIATED (Coord1)
  IF (Done) THEN
    ALLOCATE (Coord1(NumObs))
    IF (PRESENT (initial_value)) THEN
      Coord1(:) = initial_value
    ELSE
      Coord1(:) = Coord_type(RMDI,0)
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
  WRITE (MessageOut,'(2A)') TRIM (Name),' Allocate OK'
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_Alloc_CoordType
