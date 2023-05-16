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

SUBROUTINE Ops_ObsAction (Hdr,    &
                          Name,   &
                          Action, &
                          Real2,  &
                          Real1,  &
                          El1,    &
                          El2,    &
                          Int1,   &
                          Int2,   &
                          Coord1, &
                          Coord2, &
                          Char,   &
                          sub)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE OpsMod_MiscTypes, ONLY: &
  ElementHeader_Type,       &
  Element_Type,             &
  Coord_Type,               &
  ActionDealloc,            &
  ActionInit,               &
  ActionPrint1,             &
  ActionPrint2

IMPLICIT NONE

! Subroutine arguments:
TYPE (ElementHeader_Type)              :: Hdr
CHARACTER(len=*), INTENT(IN)           :: Name
INTEGER, OPTIONAL, INTENT(IN)          :: Action
REAL, POINTER, OPTIONAL                :: Real2(:,:)
REAL, POINTER, OPTIONAL                :: Real1(:)
TYPE (Element_Type), POINTER, OPTIONAL :: El1(:)
TYPE (Element_Type), POINTER, OPTIONAL :: El2(:,:)
INTEGER, POINTER, OPTIONAL             :: Int1(:)
INTEGER, POINTER, OPTIONAL             :: Int2(:,:)
TYPE (Coord_Type), POINTER, OPTIONAL   :: Coord1(:)
TYPE (Coord_Type), POINTER, OPTIONAL   :: Coord2(:,:)
CHARACTER(len=*), POINTER, OPTIONAL    :: Char(:)
OPTIONAL                               :: sub

INTERFACE
SUBROUTINE sub (Hdr,    &
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
END SUBROUTINE sub
END INTERFACE

! Local declarations:
CHARACTER(len=*), PARAMETER            :: RoutineName = "Ops_ObsAction"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (PRESENT (sub)) THEN

  CALL sub (Hdr,             &
            Name,            &
            Real2 = Real2,   &
            Real1 = Real1,   &
            El1 = El1,       &
            El2 = El2,       &
            Int1 = Int1,     &
            Int2 = Int2,     &
            Coord1 = Coord1, &
            Coord2 = Coord2, &
            Char = Char)

ELSE

  IF (Action == ActionDealloc) THEN

    CALL Ops_ObsActionDealloc (Hdr,             &
                               Name,            &
                               Real2 = Real2,   &
                               Real1 = Real1,   &
                               El1 = El1,       &
                               El2 = El2,       &
                               Int1 = Int1,     &
                               Int2 = Int2,     &
                               Coord1 = Coord1, &
                               Coord2 = Coord2, &
                               Char = Char)

  ELSE IF (Action == ActionInit) THEN

    CALL Ops_ObsActionInit (Hdr,             &
                            Name,            &
                            Real2 = Real2,   &
                            Real1 = Real1,   &
                            El1 = El1,       &
                            El2 = El2,       &
                            Int1 = Int1,     &
                            Int2 = Int2,     &
                            Coord1 = Coord1, &
                            Coord2 = Coord2, &
                            Char = Char)

  ELSE IF (Action == ActionPrint1) THEN

    CALL Ops_ObsActionPrint1 (Hdr,             &
                              Name,            &
                              Real2 = Real2,   &
                              Real1 = Real1,   &
                              El1 = El1,       &
                              El2 = El2,       &
                              Int1 = Int1,     &
                              Int2 = Int2,     &
                              Coord1 = Coord1, &
                              Coord2 = Coord2, &
                              Char = Char)

  ELSE IF (Action == ActionPrint2) THEN

    CALL Ops_ObsActionPrint2 (Hdr,             &
                              Name,            &
                              Real2 = Real2,   &
                              Real1 = Real1,   &
                              El1 = El1,       &
                              El2 = El2,       &
                              Int1 = Int1,     &
                              Int2 = Int2,     &
                              Coord1 = Coord1, &
                              Coord2 = Coord2, &
                              Char = Char)

  END IF

END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ObsAction
