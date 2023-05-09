!-----------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-----------------------------------------------------------------------
! Writes out one field from file on unit hdr % UnitNum, starting at
! field number field.
!-----------------------------------------------------------------------

SUBROUTINE Gen_WriteField (hdr,   &
                           field, &
                           D1)

USE GenMod_CLookAdd, ONLY: &
  LBEGIN,                  &
  LBLREC,                  &
  LBNREC,                  &
  LBPACK

USE GenMod_Control, ONLY: &
  UM_SECTOR_SIZE

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI,                                  &
  RMDI

USE GenMod_UMHeaderConstants, ONLY: &
  FH_DataStart,                     &
  FH_ModelVersion

USE OpsMod_Kinds, ONLY: &
  real32

USE OpsMod_Portio_Wrappers, ONLY: &
  Ops_Buffout_Wrapper,            &
  Ops_Buffout32_Wrapper,          &
  Ops_Setpos_Wrapper

IMPLICIT NONE

! Subroutine arguments:
CLASS (UM_Header_Type), INTENT(IN) :: hdr              ! UM header from file
INTEGER, INTENT(IN)                :: field
REAL, INTENT(IN)                   :: D1(:)

! Local declarations:
INTEGER                            :: i
INTEGER                            :: ipts           ! No. of values to be written to disk.
INTEGER                            :: WordAddress    ! Address from which to begin I/O.
INTEGER                            :: l_ipts         ! Record length during index search.
INTEGER                            :: um_sector_ipts ! No. of words to write, rounded up.
REAL                               :: Buf(((SIZE (D1) + um_sector_size - 1) / um_sector_size) * um_sector_size)
REAL(kind=real32)                  :: Buf32(1 + ((SIZE (D1) + um_sector_size - 1) / um_sector_size) * um_sector_size)
CHARACTER(len=*), PARAMETER        :: routine_name = "Gen_WriteField"
REAL, PARAMETER                    :: tiny32 = TINY (buf32(1))

!----------------------------------------------------------------------
! [1]: Initialize.
!----------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (routine_name)

!----------------------------------------------------------------------
! [2]: Write fields.
!----------------------------------------------------------------------

IF (MOD (hdr % Lookup(LBPACK,field), 10) == 2) THEN
  ipts = (hdr % Lookup(LBLREC,field) + 1) / 2
ELSE
  ipts = hdr % Lookup(LBLREC,field) + 1
END IF

IF (hdr % Lookup(LBNREC,field) == 0 .OR. & ! Old format dumps.
    hdr % Lookup(LBNREC,field) == IMDI .OR.  & ! \ Ocean ACOBS
    hdr % Lookup(LBEGIN,field) == IMDI .OR. & ! / files?
    (hdr % Lookup(LBNREC,field) == IMDI .AND. hdr % FixHd(FH_ModelVersion) < 301)) THEN

  WordAddress = 1
  DO i = 2, field
    IF (MOD (hdr % Lookup(LBPACK,i - 1), 10) == 2) THEN
      l_ipts = (hdr % Lookup(LBLREC, i - 1) + 1) / 2
    ELSE
      l_ipts = hdr % Lookup(LBLREC, i - 1)
    END IF
    WordAddress = WordAddress + l_ipts
  END DO
  WordAddress = WordAddress + hdr % FixHd(FH_DataStart) - 2
  um_sector_ipts = ipts
ELSE ! PP type files and post vn4.3 dumps.

  WordAddress = hdr % Lookup(LBEGIN,field)

  ! Use the stored rounded-up value:
  um_sector_ipts = hdr % Lookup(LBNREC,field)

END IF

! Position file pointer:
CALL Ops_Setpos_Wrapper (hdr % UnitNum, &
                         WordAddress)

!----------------------------------------------------------------------
! [2.1]: Non-MPP write.
!----------------------------------------------------------------------

Buf = RMDI
IF (MOD (hdr % Lookup(LBPACK,field), 10) == 2) THEN
  DO i = 1, hdr % Lookup(LBLREC,field)
    if (ABS (d1(i)) < tiny32) then
      BUF32(i) = 0
    else
      BUF32(i) = d1(i)
    end if
  end do
  IF (MOD(hdr % Lookup(LBLREC,field), 2) == 0) THEN
    buf(1:hdr % Lookup(LBLREC,field) / 2) = TRANSFER (buf32(1:hdr % Lookup(LBLREC,field)), &
                                                        buf(1:hdr % Lookup(LBLREC,field) / 2))
  ELSE
    ! Pad it
    buf32(1 + hdr % Lookup(LBLREC,field)) = 0
    buf(1:1 + hdr % Lookup(LBLREC,field) / 2) = TRANSFER (buf32(1:1 + hdr % Lookup(LBLREC,field)), &
                                                            buf(1:1 + hdr % Lookup(LBLREC,field) / 2))
  END IF
  CALL Ops_Buffout32_Wrapper (hdr % UnitNum,  &
                              Buf,            &
                              um_sector_ipts)
ELSE
  DO i = 1, hdr % Lookup(LBLREC,field)
    Buf(i) = D1(i)
  END DO
  CALL Ops_Buffout_Wrapper (hdr % UnitNum,  &
                            Buf(:),         &
                            um_sector_ipts)
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE Gen_WriteField
