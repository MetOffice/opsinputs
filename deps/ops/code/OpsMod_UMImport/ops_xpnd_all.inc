!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Unpack from WGDOS format.
!-------------------------------------------------------------------------------

SUBROUTINE ops_xpnd_all (field,   &
                         icomp64, &
                         n,       &
                         ix,      &
                         iy,      &
                         isc,     &
                         rmdi)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE OpsMod_Kinds, ONLY: &
  integer64,            &
  real64

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)                 :: n
INTEGER(kind=integer64), INTENT(IN) :: icomp64(n)
INTEGER, INTENT(IN)                 :: ix
INTEGER, INTENT(IN)                 :: iy
REAL(kind=real64), INTENT(OUT)      :: field(ix,iy)
INTEGER, INTENT(IN)                 :: isc
REAL, INTENT(IN)                    :: rmdi

! Local declarations:
INTEGER                             :: i
INTEGER                             :: j
INTEGER                             :: nshft
INTEGER                             :: num
INTEGER                             :: iword
INTEGER                             :: ioff
INTEGER                             :: mant
INTEGER                             :: iexp
INTEGER(kind=integer64)             :: ival
INTEGER                             :: i1
INTEGER                             :: i2
INTEGER                             :: nbits_bmap
INTEGER                             :: itmp(3 * ix)
INTEGER                             :: idx(ix)
INTEGER                             :: imap(ix)
INTEGER                             :: istart(iy)
INTEGER                             :: nop(iy)
INTEGER                             :: nbits(iy)
INTEGER(kind=integer64)             :: ibase(iy)
INTEGER(kind=integer64)             :: icomp(iy * (2 * ix + 2) + 4)
REAL                                :: aprec
REAL                                :: base(iy)
LOGICAL                             :: obtzer(iy)
LOGICAL                             :: obtmin(iy)
LOGICAL                             :: obtmis(iy)
LOGICAL                             :: obtmap(iy)
INTEGER(kind=integer64), PARAMETER  :: Mask16 = INT (z'FFFF')
INTEGER(kind=integer64), PARAMETER  :: Mask32 = 4294967295_integer64
INTEGER(kind=integer64), PARAMETER  :: Mask_Mant_IBM = INT (z'00FFFFFF')
INTEGER(kind=integer64), PARAMETER  :: Mask_Expt_IBM = INT (z'7F000000')
INTEGER(kind=integer64), PARAMETER  :: Mask_Sign_IBM = 2147483648_integer64
INTEGER(kind=integer64), PARAMETER  :: One64 = 1
INTEGER(kind=integer64), SAVE       :: mask_bits(0:63)
LOGICAL, SAVE                       :: first = .TRUE.
CHARACTER(len=*), PARAMETER         :: RoutineName = "ops_xpnd_all"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (first) THEN
  DO i = 0, 63
    mask_bits(i) = ISHFT (One64, 63 - i)
  END DO
  first = .FALSE.
END IF

! Scale factor

aprec = 2.0 ** isc

! All lengths and alignments in WGDOS packing are for 32-bit words,
! so life gets much easier when we treat the packed data as 32-bit
! words.
! We split therefore the 64-bit compressed data into two 32 bit words

num = ISHFT (icomp64(1), -32) ! Number of 32 bit words

IF (num > SIZE (icomp) - 2) THEN
  CALL gen_fail (RoutineName,                             &
                 'Compressed data has too many elements')
END IF

DO i = 1, (num + 1) / 2
  icomp(2 * i - 1) = IAND (ISHFT (icomp64(i), -32), mask32)
  icomp(2 * i) = IAND (icomp64(i), mask32)
END DO

! The following word MUST be 0, it is used during decomposition!

icomp(num + 1) = 0
icomp(num + 2) = 0

! Get start word and length of every row

istart(1) = 6
nop(1) = IAND (icomp(5), mask16)

DO j = 2, iy
  istart(j) = istart(j - 1) + nop(j - 1) + 2
  nop(j) = IAND (icomp(istart(j) - 1), mask16)
  IF (istart(j) + nop(j) - 1 > num) THEN
    CALL gen_fail (RoutineName,                    &
                   'Compressed data inconsistent')
  END IF
END DO

! Get base (as a 32-bit IBM floating point number) and number of bits
! for every row and convert IBM floats to native floats

!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE)                        &
!$OMP&         SHARED(iy, obtmis, obtmin, obtzer, obtmap, nbits, ibase, &
!$OMP&                base, ix, mask_bits, field, icomp, rmdi, istart,  &
!$OMP&                aprec)                                            &
!$OMP&         PRIVATE(j, nbits_bmap, mant, iexp, ival, iword, itmp,    &
!$OMP&                 nshft, i1, i2, i, num, imap, idx, ioff           &
!$OMP&                )
DO j = 1, iy
  ibase(j) = icomp(istart(j) - 2)
  nbits(j) = IAND (ISHFT (icomp(istart(j) - 1), -16), mask16)

  mant = IAND (ibase(j), mask_mant_ibm)
  iexp = ISHFT (IAND (ibase(j), mask_expt_ibm), -24) - 64 - 6
  base(j) = 16.0 ** iexp * mant
  IF (IAND (ibase(j), mask_sign_ibm) /= 0) base(j) = -base(j)

  ! Check if bitmaps are used

  obtzer(j) = IAND (nbits(j), 128) /= 0
  obtmin(j) = IAND (nbits(j), 64) /= 0
  obtmis(j) = IAND (nbits(j), 32) /= 0
  obtmap(j) = obtzer(j) .OR. obtmin(j) .OR. obtmis(j)
  nbits(j)  = IAND (nbits(j), 31)

  ! Decode data row by row

  ! Care about bitmaps

  imap(:) = 1 ! Data present indicator

  nbits_bmap = 0
  IF (obtmis(j)) nbits_bmap = nbits_bmap + ix
  IF (obtmin(j)) nbits_bmap = nbits_bmap + ix
  IF (obtzer(j)) nbits_bmap = nbits_bmap + ix

  IF (nbits_bmap > 0) THEN
    iword = istart(j)
    DO i1 = 1, nbits_bmap, 64
      ival = IOR (ISHFT (icomp(iword), 32), icomp(iword + 1))
      iword = iword + 2
      DO i2 = 0, MIN (nbits_bmap - i1, 63)
        itmp(i1 + i2) = MERGE (1, 0, IAND (ival, mask_bits(i2)) /= 0)
      END DO
    END DO
    istart(j) = istart(j) + (nbits_bmap + 31) / 32
  END IF

  nbits_bmap = 0

  ! Extract missing data bitmap

  IF (obtmis(j)) THEN
    WHERE (itmp(nbits_bmap + 1:nbits_bmap + ix) /= 0)
      field(:,j) = rmdi
      imap(:) = 0
    END WHERE
    nbits_bmap = nbits_bmap + ix
  END IF

  ! Extract minimum value bitmap

  IF (obtmin(j)) THEN
    WHERE (itmp(nbits_bmap + 1:nbits_bmap + ix) /= 0)
      field(:,j) = base(j)
      imap(:) = 0
    END WHERE
    nbits_bmap = nbits_bmap + ix
  END IF

  ! Extract zero value bitmap

  IF (obtzer(j)) THEN
    WHERE (itmp(nbits_bmap + 1:nbits_bmap + ix) == 0)
      field(:,j) = 0.0
      imap(:) = 0
    END WHERE
    nbits_bmap = nbits_bmap + ix
  END IF

  IF (nbits(j) == 0) THEN

    ! All points in row have same value

    IF (obtmap(j)) THEN
      WHERE (imap(:) /= 0) field(:,j) = base(j)
    ELSE
      field(:,j) = base(j)
    END IF

  ELSE

    ! Get number [and index] of values to decode

    IF (obtmap(j)) THEN
      num = 0
      DO i = 1, ix
        IF (imap(i) /= 0) THEN
          num = num + 1
          idx(num) = i
        END IF
      END DO
    ELSE
      num = ix
    END IF

    ! Decode data

    IF (obtmap(j)) THEN
      DO i = 1, num

        ! Bit offset to value:

        ioff = (i - 1) * nbits(j)

        ! Number of word in icomp which contains first bit:

        iword = ISHFT (ioff, -5) + istart(j)

        ! We load this word and the following into ival,
        ! this way we don't have to care if a word boundary
        ! is crossed. This requires that ival is a 64 bit word!

        ival = IOR (ISHFT (icomp(iword), 32), icomp(iword + 1))

        ! Number of bits we have to shift to the right:

        nshft = 64 - IAND (ioff, 31) - nbits(j)

        ! Mask ival and calculate decoded value:

        ival = IBITS (ival, nshft, nbits(j))
        field(idx(i),j) = ival * aprec + base(j)
      END DO
    ELSE
      DO i = 1, num

        ! Bit offset to value:

        ioff = (i - 1) * nbits(j)

        ! Number of word in icomp which contains first bit:

        iword = ISHFT (ioff, -5) + istart(j)

        ! We load this word and the following into ival,
        ! this way we don't have to care if a word boundary
        ! is crossed. This requires that ival is a 64 bit word!

        ival = IOR (ISHFT (icomp(iword), 32), icomp(iword + 1))

        ! Number of bits we have to shift to the right:

        nshft = 64 - IAND (ioff, 31) - nbits(j)

        ! Mask ival and calculate decoded value:

        ival = IBITS (ival, nshft, nbits(j))
        field(i,j) = ival * aprec + base(j)
      END DO
    END IF

  END IF

END DO
!$OMP END PARALLEL DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE ops_xpnd_all
