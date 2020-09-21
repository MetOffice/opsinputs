!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Allocates a pointer array, and sets up the header.  Check the assocation and
! allocate if necessary. Initialise contents to missing. Then initialise the
! header.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_Alloc_CoordType2d (Hdr,           &
                                  Name,          &
                                  NumObs,        &
                                  Coord2,        &
                                  HdrIn,         &
                                  num_levels,    &
                                  zcode,         &
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
TYPE (Coord_Type), POINTER                      :: Coord2(:,:)
TYPE (ElementHeader_Type), OPTIONAL, INTENT(IN) :: HdrIn
INTEGER, OPTIONAL, INTENT(IN)                   :: num_levels
INTEGER, OPTIONAL, INTENT(IN)                   :: zcode
TYPE (Coord_Type), OPTIONAL, INTENT(IN)         :: initial_value

! Local declarations:
LOGICAL                                         :: Done
LOGICAL                                         :: Needed
INTEGER                                         :: NumLevLoc
INTEGER                                         :: ZCodeLoc
CHARACTER(len=*), PARAMETER                     :: RoutineName = "Ops_Alloc_CoordType2d"

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
IF (PRESENT (num_levels)) NumLevLoc = num_levels
IF (PRESENT (zcode)) ZCodeLoc = zcode

Needed = .TRUE.
IF (PRESENT (HdrIn)) Needed = HdrIn % Present

Done = .FALSE.
IF (Needed) THEN
  Done = .NOT. ASSOCIATED (Coord2)
  IF (Done) THEN
    ALLOCATE (Coord2(NumObs,NumLevLoc))
    IF (PRESENT (initial_value)) THEN
      Coord2(:,:) = initial_value
    ELSE
      Coord2(:,:) = Coord_type(RMDI,0)
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
  WRITE (MessageOut,'(2A)') TRIM (Name),' Allocate OK'
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_Alloc_CoordType2d
