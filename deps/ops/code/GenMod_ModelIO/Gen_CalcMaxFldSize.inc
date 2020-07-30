!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Find number of fields in dump and maximum unpacked field size.
!-------------------------------------------------------------------------------

SUBROUTINE Gen_CalcMaxFldSize (UMhdr)

USE GenMod_CLookAdd, ONLY: &
  LBEXT,                   &
  LBLREC,                  &
  LBNPT,                   &
  LBPACK,                  &
  LBROW

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_UMHeaderConstants, ONLY: &
  FH_Dataset,                       &
  FH_Dataset_FF,                    &
  FH_LookupSize2

IMPLICIT NONE

! Subroutine arguments:
CLASS (UM_header_type), INTENT (INOUT) :: UMhdr

! Local declarations:
CHARACTER(len=*), PARAMETER            :: RoutineName = "Gen_CalcMaxFldSize"
INTEGER                                :: ifld
INTEGER                                :: FldSize

IF (UseTrace) CALL gen_trace_entry (RoutineName)

UMhdr % NumFlds = 0
UMhdr % MaxFldSize = 0

Fields: DO Ifld = 1, UMhdr % FixHd(FH_LookupSize2) ! Loop over all Lookup entries (flds)
  IF (UMhdr % Lookup(LBLREC,Ifld) < 0) EXIT ! At end of fields

  ! Increment total number of fields:
  UMhdr % NumFlds = UMhdr % NumFlds + 1

  ! Find unpacked size of field:
  IF (UMhdr % FixHd(FH_Dataset) == FH_Dataset_FF .AND. &
      MODULO (UMhdr % Lookup(LBPACK,Ifld), 10) /= 0) THEN
    ! Packed field in a fieldsfiles, so LBLREC holds the number of words
    ! needed for the PACKED data rather than the length of the unpacked field.
    ! Use field dimensions instead.
    FldSize = UMhdr % Lookup(LBROW,Ifld) * UMhdr % Lookup(LBNPT,Ifld) + UMhdr % Lookup(LBEXT,Ifld)
  ELSE
    ! LBLREC is the number of data points.
    FldSize = UMhdr % Lookup(LBLREC,Ifld)
  END IF

  ! Update maximum field size:
  UMhdr % MaxFldSize = MAX (UMhdr % MaxFldSize, FldSize)

END DO Fields

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Gen_CalcMaxFldSize
