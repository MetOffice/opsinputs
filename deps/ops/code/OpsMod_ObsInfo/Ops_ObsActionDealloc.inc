!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Performs an action on an observation structure array depending on the value of
! module variable Action.  Possible actions are:
!
!   Initialise: set header to absent, and nullify array pointer
!   Print1: print sample of array: ob value and ob error for elements
!   Print2: print sample of element array: pgefinal and flags
!   Deallocate: set header to absent, and deallocate array
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ObsActionDealloc (Hdr,    &
                                 Name,   &
                                 Real2,  &
                                 Real1,  &
                                 El1,    &
                                 El2,    &
                                 Int1,   &
                                 Int2,   &
                                 Coord1, &
                                 Coord2, &
                                 Char)

USE GenMod_Control, ONLY: &
  GeneralMode,            &
  VerboseMode,            &
  mype

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  MessageOut,          &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE OpsMod_MiscTypes, ONLY: &
  ElementHeader_Type,       &
  Element_Type,             &
  Coord_Type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ElementHeader_Type)              :: Hdr
CHARACTER(len=*), INTENT(IN)           :: Name
REAL, POINTER, OPTIONAL                :: Real2(:,:)
REAL, POINTER, OPTIONAL                :: Real1(:)
TYPE (Element_Type), POINTER, OPTIONAL :: El1(:)
TYPE (Element_Type), POINTER, OPTIONAL :: El2(:,:)
INTEGER, POINTER, OPTIONAL             :: Int1(:)
INTEGER, POINTER, OPTIONAL             :: Int2(:,:)
TYPE (Coord_Type), POINTER, OPTIONAL   :: Coord1(:)
TYPE (Coord_Type), POINTER, OPTIONAL   :: Coord2(:,:)
CHARACTER(len=*), POINTER, OPTIONAL    :: Char(:)

! Local declarations:
LOGICAL                                :: Done
CHARACTER(len=*), PARAMETER            :: RoutineName = "Ops_ObsActionDealloc"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (Hdr % Present) THEN
  Done = .FALSE.

  IF (PRESENT (Real1)) THEN
    Done = ASSOCIATED (Real1)
    IF (Done) DEALLOCATE (Real1)
  ELSE IF (PRESENT (Real2)) THEN
    Done = ASSOCIATED (Real2)
    IF (Done) DEALLOCATE (Real2)
  ELSE IF (PRESENT (Int1)) THEN
    Done = ASSOCIATED (Int1)
    IF (Done) DEALLOCATE (Int1)
  ELSE IF (PRESENT (Int2)) THEN
    Done = ASSOCIATED (Int2)
    IF (Done) DEALLOCATE (Int2)
  ELSE IF (PRESENT (El1)) THEN
    Done = ASSOCIATED (El1)
    IF (Done) DEALLOCATE (El1)
  ELSE IF (PRESENT (El2)) THEN
    Done = ASSOCIATED (El2)
    IF (Done) DEALLOCATE (El2)
  ELSE IF (PRESENT (Coord1)) THEN
    Done = ASSOCIATED (Coord1)
    IF (Done) DEALLOCATE (Coord1)
  ELSE IF (PRESENT (Coord2)) THEN
    Done = ASSOCIATED (Coord2)
    IF (Done) DEALLOCATE (Coord2)
  ELSE IF (PRESENT (Char)) THEN
    Done = ASSOCIATED (Char)
    IF (Done) DEALLOCATE (Char)
  END IF
  Hdr % Present = .FALSE.
  Hdr % NumLev = IMDI
  Hdr % Zcode = IMDI

  IF (GeneralMode >= VerboseMode .AND. mype == 0 .AND. Done) THEN
    WRITE (MessageOut, '(2A)') TRIM (Name), ' Deallocate OK'
  END IF

END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ObsActionDealloc
