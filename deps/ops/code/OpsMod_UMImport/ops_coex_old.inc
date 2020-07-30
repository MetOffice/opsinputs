!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for old UM coex to read WGDOS packed data.
!-------------------------------------------------------------------------------

SUBROUTINE ops_coex_old (field, &
                         icomp, &
                         n,     &
                         ix,    &
                         iy,    &
                         isc,   &
                         rmdi)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE OpsMod_Kinds, ONLY: &
  integer16,            &
  integer32,            &
  integer64,            &
  real64

IMPLICIT NONE

! Subroutine arguments:
INTEGER                     :: n
REAL(kind=real64)           :: field(:)
INTEGER(kind=integer64)     :: icomp(n)
INTEGER                     :: ix
INTEGER                     :: iy
INTEGER                     :: isc
REAL                        :: rmdi

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "ops_coex_old"
INTEGER(kind=integer32)     :: isc_mvbits(2)
INTEGER(kind=integer16)     :: isc_mvbits16(4)

IF (UseTrace) CALL gen_trace_entry (RoutineName)

isc_mvbits = TRANSFER (icomp(1), isc_mvbits)
isc = isc_mvbits(1)

isc_mvbits16 = TRANSFER (icomp(2), isc_mvbits16)
ix = isc_mvbits16(4)
iy = isc_mvbits16(3)

CALL ops_xpnd_all (field, &
                   icomp, &
                   n,     &
                   ix,    &
                   iy,    &
                   isc,   &
                   rmdi)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE ops_coex_old
