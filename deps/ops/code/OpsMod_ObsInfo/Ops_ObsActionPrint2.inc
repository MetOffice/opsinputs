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

SUBROUTINE Ops_ObsActionPrint2 (Hdr,    &
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
  VerboseMode

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  MessageOut,          &
  UseTrace

USE OpsMod_Control, ONLY: &
  print_absent_fields,    &
  print_level_max,        &
  print_level_min,        &
  print_level_stride

USE OpsMod_MiscTypes, ONLY: &
  ElementHeader_Type,       &
  Element_Type,             &
  Coord_Type,               &
  ObPrintIndex

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
INTEGER                                :: NumPrint
INTEGER                                :: Job
INTEGER                                :: JLev
CHARACTER(len=*), PARAMETER            :: RoutineName = "Ops_ObsActionPrint2"
CHARACTER(len=100)                     :: format
CHARACTER(len=100)                     :: format2

IF (UseTrace) CALL gen_trace_entry (RoutineName)

NumPrint = SIZE (ObPrintIndex(:))

IF (Hdr % Present) THEN
  IF (PRESENT (El1)) THEN
    WRITE (format, '(A,I0,A)') "(A24,", NumPrint, "(1X,F19.7))"
    WRITE (MessageOut, format) TRIM (Name) // " pge final", &
      (El1(ObPrintIndex(Job)) % PGEFinal, Job = 1, NumPrint)
    WRITE (format, '(A,I0,A)') "(A24,", NumPrint, "(1X,O19))"
    WRITE (MessageOut, format) TRIM (Name) // " flags", &
      (El1(ObPrintIndex(Job)) % Flags, Job = 1, NumPrint)
  ELSE IF (PRESENT (El2)) THEN
    IF (SIZE (El2, DIM = 1) >= MAXVAL (ObPrintIndex)) THEN
      WRITE (MessageOut, '(3A,I0)') 'Zcode for ', TRIM (Name), ' = ', Hdr % ZCode
      WRITE (format, '(A,I0,A)') "(A20,I4,", NumPrint, "(1X,F19.7))"
      WRITE (format2, '(A,I0,A)') "(A20,I4,", NumPrint, "(1X,O19))"
      DO JLev = MIN (Hdr % NumLev, print_level_max), MAX (1, print_level_min), -print_level_stride
        WRITE (MessageOut, format) TRIM (Name) // " pge final", JLev, &
          (El2(ObPrintIndex(Job),JLev) % PGEFinal, Job = 1, NumPrint)
        WRITE (MessageOut, format2) TRIM (Name) // " flags", JLev, &
          (El2(ObPrintIndex(Job),JLev) % Flags, Job = 1, NumPrint)
      END DO
    END IF
  END IF
ELSE
  IF (GeneralMode >= VerboseMode .AND. print_absent_fields) THEN
    WRITE (MessageOut, '(2A)') TRIM (Name), ' is absent'
  END IF
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ObsActionPrint2
