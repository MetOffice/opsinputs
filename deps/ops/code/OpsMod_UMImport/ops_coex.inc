!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for shumlib procedures to read WGDOS packed data.
!-------------------------------------------------------------------------------

SUBROUTINE ops_coex (field, &
                     m,     &
                     icomp, &
                     n,     &
                     ix,    &
                     iy,    &
                     isc,   &
                     rmdi)

USE f_shum_wgdos_packing_mod, ONLY: &
  f_shum_read_wgdos_header,         &
  f_shum_wgdos_unpack

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE OpsMod_Kinds, ONLY: &
  integer32,            &
  integer64,            &
  real64

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)                    :: m
REAL(kind=real64), INTENT(INOUT)       :: field(m)
INTEGER, INTENT(IN)                    :: n
INTEGER(kind=integer64), INTENT(INOUT) :: icomp(n)
INTEGER, INTENT(INOUT)                 :: ix
INTEGER, INTENT(INOUT)                 :: iy
INTEGER, INTENT(INOUT)                 :: isc
REAL, INTENT(IN)                       :: rmdi

! Local declarations:
INTEGER                                :: num_words
INTEGER(kind=integer32)                :: icomp32(2 * n)
REAL(kind=real64), ALLOCATABLE         :: field2d(:,:)
CHARACTER(len=*), PARAMETER            :: RoutineName = "ops_coex"
CHARACTER(len=200)                     :: messages(3)
REAL(kind=real64)                      :: rmdi_local
CHARACTER(len=200)                     :: cmessage
INTEGER                                :: icode

IF (UseTrace) CALL gen_trace_entry (RoutineName)

icomp32(:) = TRANSFER (icomp, icomp32)
rmdi_local = rmdi

! Extract scale factor, columns and rows from header

icode = f_shum_read_wgdos_header (icomp32, num_words, isc, ix, iy, cmessage)
IF (icode /= 0) THEN
  messages(1) = "Failure in f_shum_read_wgdos_header"
  messages(2) = cmessage
  WRITE (messages(3), '(A,I0)') "Return code = ", icode
  CALL gen_fail (RoutineName,   &
                 messages(1:3))
END IF

ALLOCATE (field2d(ix,iy))

icode = f_shum_wgdos_unpack (icomp32, rmdi_local, field2d, cmessage)
IF (icode /= 0) THEN
  messages(1) = "Failure in f_shum_wgdos_unpack"
  messages(2) = cmessage
  WRITE (messages(3), '(A,I0)') "Return code = ", icode
  CALL gen_fail (RoutineName,   &
                 messages(1:3))
END IF

field(1:ix * iy) = RESHAPE (field2d, (/ix * iy/))

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE ops_coex
