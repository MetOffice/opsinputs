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

SUBROUTINE Ops_ObsActionInit (Hdr,    &
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

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
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
CHARACTER(len=*), PARAMETER            :: RoutineName = "Ops_ObsActionInit"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

Hdr % Present = .FALSE.
Hdr % NumLev = IMDI
Hdr % Zcode = IMDI

IF (PRESENT (Real1)) THEN
  NULLIFY (Real1)
ELSE IF (PRESENT (Real2)) THEN
  NULLIFY (Real2)
ELSE IF (PRESENT (Int1)) THEN
  NULLIFY (Int1)
ELSE IF (PRESENT (Int2)) THEN
  NULLIFY (Int2)
ELSE IF (PRESENT (El1)) THEN
  NULLIFY (El1)
ELSE IF (PRESENT (El2)) THEN
  NULLIFY (El2)
ELSE IF (PRESENT (Coord1)) THEN
  NULLIFY (Coord1)
ELSE IF (PRESENT (Coord2)) THEN
  NULLIFY (Coord2)
ELSE IF (PRESENT (Char)) THEN
  NULLIFY (Char)
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ObsActionInit
