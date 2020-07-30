!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Performs an action on an observation structure array depending on the value of
! module variable Action.  Possible actions are:
!
!   Initialise: set header to absent, and nullify array pointer
!   Print1: print sample of array: ob value and ob error for elements
!   Print2: print sample of element array: pgefinal and flags
!   Deallocate: set header to absent, and deallocate array
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ObsActionPrint1 (Hdr,    &
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
CHARACTER(len=*), PARAMETER            :: RoutineName = "Ops_ObsActionPrint1"
CHARACTER(len=100)                     :: format

IF (UseTrace) CALL gen_trace_entry (RoutineName)

NumPrint = SIZE (ObPrintIndex(:))

IF (Hdr % Present) THEN
  IF (PRESENT (Real1)) THEN
    WRITE (format, '(A,I0,A)') "(A24,", NumPrint, "(1X,F19.7))"
    WRITE (MessageOut, format) TRIM (Name), Real1(ObPrintIndex(:))
  ELSE IF (PRESENT (Real2)) THEN
    IF (SIZE (Real2, DIM = 1) >= MAXVAL (ObPrintIndex)) THEN
      WRITE (format, '(A,I0,A)') "(A20,I4,", NumPrint, "(1X,F19.7))"
      DO JLev = MIN (Hdr % NumLev, print_level_max), MAX (1, print_level_min), -print_level_stride
        WRITE (MessageOut, format) TRIM (Name), JLev, Real2(ObPrintIndex(:),JLev)
      END DO
    END IF
  ELSE IF (PRESENT (Int1)) THEN
    WRITE (format, '(A,I0,A)') "(A24,", NumPrint, "(1X,I19))"
    WRITE (MessageOut, format) TRIM (Name), Int1(ObPrintIndex(:))
  ELSE IF (PRESENT (Int2)) THEN
    WRITE (format, '(A,I0,A)') "(A20,I4,", NumPrint, "(1X,I19))"
    DO JLev = MIN (Hdr % NumLev, print_level_max), MAX (1, print_level_min), -print_level_stride
      WRITE (MessageOut, format) TRIM (Name), JLev, Int2(ObPrintIndex(:),JLev)
    END DO
  ELSE IF (PRESENT (El1)) THEN
    WRITE (format, '(A,I0,A)') "(A24,", NumPrint, "(1X,F19.7))"
    WRITE (MessageOut, format) TRIM (Name) // " value", (El1(ObPrintIndex(Job)) % Value, Job = 1, NumPrint)
    WRITE (MessageOut, format) TRIM (Name) // " oberr", (El1(ObPrintIndex(Job)) % OBErr, Job = 1, NumPrint)
  ELSE IF (PRESENT (El2)) THEN
    IF (SIZE (El2, DIM = 1) >= MAXVAL (ObPrintIndex)) THEN
      WRITE (MessageOut, '(3A,I0)') 'Zcode for ', TRIM (Name), ' = ', Hdr % ZCode
      WRITE (format, '(A,I0,A)') "(A20,I4,", NumPrint, "(1X,F19.7))"
      DO JLev = MIN (Hdr % NumLev, print_level_max), MAX (1, print_level_min), -print_level_stride
        WRITE (MessageOut, format) TRIM (Name) // " value", jlev, &
          (El2(ObPrintIndex(Job),jlev) % Value, Job = 1, NumPrint)
        WRITE (MessageOut, format) TRIM (Name) // " oberr", jlev, &
          (El2(ObPrintIndex(Job),jlev) % OBErr, Job = 1, NumPrint)
      END DO
    END IF
  ELSE IF (PRESENT (Coord1)) THEN
    WRITE (format, '(A,I0,A)') "(A24,", NumPrint, "(1X,F19.7))"
    WRITE (MessageOut, format) TRIM (Name), Coord1(ObPrintIndex(:)) % Value
  ELSE IF (PRESENT (Coord2)) THEN
    WRITE (format, '(A,I0,A)') "(A20,I4,", NumPrint, "(1X,F19.7))"
    DO JLev = MIN (Hdr % NumLev, print_level_max), MAX (1, print_level_min), -print_level_stride
      WRITE (MessageOut, format) TRIM (Name), JLev, Coord2(ObPrintIndex(:),jlev) % Value
    END DO
  ELSE IF (PRESENT (Char)) THEN
    WRITE (format, '(A,I0,A)') "(A24,", NumPrint, "(1X,A19))"
    WRITE (MessageOut, format) TRIM (Name), Char(ObPrintIndex(:))
  END IF
ELSE
  IF (GeneralMode >= VerboseMode .AND. print_absent_fields) THEN
    WRITE (MessageOut, '(2A)') TRIM (Name),' is absent'
  END IF
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ObsActionPrint1
