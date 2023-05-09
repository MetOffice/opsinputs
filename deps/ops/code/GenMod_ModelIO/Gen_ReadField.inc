!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Reads in one field from file on unit hdr % UnitNum,
! starting at field number field. The data is returned
! in the out_buf array.
!-------------------------------------------------------------------------------

SUBROUTINE Gen_ReadField (hdr,     &
                          field,   &
                          out_buf)

USE GenMod_CLookAdd, ONLY: &
  LBEGIN,                  &
  LBLREC,                  &
  LBPACK

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  RMDI

USE OpsMod_EnvUtils, ONLY: &
  ops_env_is_true

USE OpsMod_Kinds, ONLY: &
  integer64,            &
  real32,               &
  real64

USE OpsMod_Portio_Wrappers, ONLY: &
  Ops_Buffin32_Wrapper,           &
  Ops_Buffin_Wrapper,             &
  ops_coex,                       &
  ops_coex_old,                   &
  Ops_Setpos_Wrapper

IMPLICIT NONE

! Subroutine arguments:
CLASS (UM_Header_Type), INTENT(IN) :: hdr              ! UM header from file
INTEGER, INTENT(IN)                :: field            ! first field to read in
REAL(kind=real64), INTENT(INOUT)   :: out_buf(:)       ! array to return the data in

! Local declarations:
CHARACTER(len=300)                 :: cmessage(2)        ! Error message
INTEGER                            :: pack_code          ! packing code for field
INTEGER                            :: field_start        ! location of field on disk
INTEGER                            :: data_read_size     ! number of words to read from disk
INTEGER                            :: num_unpack_values
INTEGER                            :: dimx
INTEGER                            :: dimy
INTEGER                            :: idum
REAL                               :: BUF(hdr % MaxFldSize) ! I/O buffer
REAL(kind=real32)                  :: BUF32(hdr % MaxFldSize * 2) ! I/O buffer
INTEGER(kind=integer64)            :: IntBuf(hdr % MaxFldSize) ! I/O buffer
CHARACTER(len=*), PARAMETER        :: RoutineName = "Gen_ReadField"
INTEGER                            :: buf_size
LOGICAL                            :: use_shumlib_coex

IF (UseTrace) CALL gen_trace_entry (RoutineName)

use_shumlib_coex = ops_env_is_true ("OPS_USE_SHUMLIB_COEX")

pack_code = MODULO (hdr % lookup(LBPACK,field), 10)

! Set up the location of the field on disk
field_start = hdr % lookup(LBEGIN,field) ! position of field in file
IF (field_start <= 0) THEN
  WRITE (CMessage(1), '(A,I0)') 'Start address =', field_start
  CMessage(2) = 'Start address of field not given'
  CALL gen_fail (RoutineName,   &
                 CMessage(1:2))
END IF

! data_read_size contains the number of words of data used to store the
! field on disk (needs to be halved if 32 bit packing has been used)
IF (pack_code == 2) THEN
  data_read_size = (hdr % lookup(LBLREC,field) + 1) / 2
ELSE
  data_read_size = hdr % lookup(LBLREC,field)
END IF

IF (data_read_size < 0) THEN
  WRITE (CMessage(1), '(A,I0)') 'Number of words to read =', data_read_size
  CMessage(2) = 'Number of words to read not given'
  CALL gen_fail (RoutineName,   &
                 CMessage(1:2))
END IF

IF (data_read_size > 0) THEN

  ! Move file pointer to the start of the field
  CALL Ops_Setpos_Wrapper (hdr % UnitNum, &
                           field_start)

  ! ops_buffin the data

  SELECT CASE (pack_code)
    CASE (0)
      CALL Ops_Buffin_Wrapper (hdr % UnitNum,  &
                               out_buf(:),     &
                               data_read_size)
    CASE (1)
      IF (use_shumlib_coex) THEN
        CALL Ops_Buffin32_Wrapper (hdr % UnitNum,  &
                                   IntBuf(:),      &
                                   data_read_size)
      ELSE
        CALL Ops_Buffin_Wrapper (hdr % UnitNum,  &
                                 IntBuf(:),      &
                                 data_read_size)
      END IF
    CASE (2)
      CALL Ops_Buffin32_Wrapper (hdr % UnitNum,  &
                                 BUF(:),         &
                                 data_read_size)
    CASE DEFAULT
      CALL Ops_Buffin_Wrapper (hdr % UnitNum,  &
                               BUF(:),         &
                               data_read_size)
  END SELECT

  ! Check for I/O errors

  ! Deal with any packing

  SELECT CASE (pack_code)
    CASE (2) ! 32 Bit packing

      BUF32 = TRANSFER (BUF, BUF32)
      out_buf(1:hdr % lookup(lblrec,field)) = BUF32(1:hdr % lookup(lblrec,field))
      num_unpack_values = hdr % lookup(lblrec,field)

    CASE (1) ! WGDOS packing

      idum = 0
      buf_size = SIZE (out_buf)
      IF (use_shumlib_coex) THEN
        CALL ops_coex (out_buf(:),     &
                       buf_size,       &
                       IntBuf(:),      &
                       data_read_size, &
                       dimx,           &
                       dimy,           &
                       idum,           &
                       rmdi)
      ELSE
        CALL ops_coex_old (out_buf(:),     &
                           IntBuf(:),      &
                           data_read_size, &
                           dimx,           &
                           dimy,           &
                           idum,           &
                           rmdi)
      END IF
      num_unpack_values = dimx * dimy

    CASE (0)

      CONTINUE  ! no packing

    CASE DEFAULT
      cmessage(1) = ' Packing type not supported'
      CALL gen_fail (RoutineName, &
                     CMessage(1))
  END SELECT

  IF (pack_code /= 0) THEN

    IF (hdr % MaxFldSize < num_unpack_values) THEN
      WRITE (CMessage(1), '(A,I0,A,I0)') 'Max field size = ', hdr % MaxFldSize, &
                  ' but NUM_UNPACK_VALUES = ', num_unpack_values
      cmessage(2) = ' Max field size is too small for Unpacked Data'
      CALL gen_fail (RoutineName,   &
                     CMessage(1:2))
    END IF

  END IF

END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Gen_ReadField
