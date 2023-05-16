!-----------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-----------------------------------------------------------------------
! Prints out Kth 64-word PP header
!-----------------------------------------------------------------------

SUBROUTINE Ops_PrintLookup (UMhdr, &
                            K)

USE GenMod_CLookAdd, ONLY: &
  ITEM_CODE,               &
  LBDAT,                   &
  LBDATD,                  &
  LBDAY,                   &
  LBDAYD,                  &
  LBHR,                    &
  LBHRD,                   &
  LBMIN,                   &
  LBMIND,                  &
  LBMON,                   &
  LBMOND,                  &
  LBYR,                    &
  LBYRD,                   &
  MODEL_CODE

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  MessageOut,          &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:
CLASS (UM_header_type), INTENT(IN) :: UMhdr
INTEGER, INTENT(IN)                :: K                     ! Field number in Look Up Table

! Local declarations:
INTEGER                            :: ITEM                  ! Stash item number
INTEGER                            :: SECTION               ! Stash section number
INTEGER                            :: MODEL                 ! Internal model number
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_PrintLookup"
REAL                               :: RLOOKUP(SIZE (UMhdr % Lookup, DIM = 1),SIZE (UMhdr % Lookup, DIM = 2))

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! Write time and field type

ITEM = MOD (UMhdr % Lookup(ITEM_CODE,K), 1000)
SECTION = (UMhdr % Lookup(ITEM_CODE,K) - ITEM) / 1000
MODEL = UMhdr % Lookup(MODEL_CODE,K)

! All diagnostics under model code of 10 are in section 20 of Atmos StashMaster file.

IF (MODEL == 10) THEN
  MODEL = 1
END IF

WRITE (MessageOut, '(A,I0,4X,A)') 'FIELD NO. ', K, "Name not known"

WRITE (MessageOut, '(A,2I2.2,A,2(I2.2,A),I4.4,A,I0,A,2I2.2,A,2(I2.2,A),I4.4,A,I0)') &
       'VALID AT: ', UMhdr % Lookup(LBHR,K), UMhdr % Lookup(LBMIN,K), ' Z  ', &
       UMhdr % Lookup(LBDAT,K), '/', UMhdr % Lookup(LBMON,K), '/', UMhdr % Lookup(LBYR,K), &
       ' DAY ', UMhdr % Lookup(LBDAY,K), &
       'DATA TIME: ', UMhdr % Lookup(LBHRD,K), UMhdr % Lookup(LBMIND,K), ' Z  ', &
       UMhdr % Lookup(LBDATD,K), '/', UMhdr % Lookup(LBMOND,K), '/', UMhdr % Lookup(LBYRD,K), &
       ' DAY ', UMhdr % Lookup(LBDAYD,K)

! Rest of header

RLOOKUP = RESHAPE (TRANSFER (UMhdr % Lookup, RLOOKUP), [SIZE (UMhdr % Lookup, DIM = 1),SIZE (UMhdr % Lookup, DIM = 2)])

WRITE (MessageOut, '(A)') '   LBTIM   LBFT    LBLREC LBCODE  LBHEM  LBROW  LBNPT  LBEXT LBPACK'
WRITE (MessageOut, '(1X,2I7,I10,6I7)') UMhdr % Lookup(13:21,K)
WRITE (MessageOut, '(A)') '   LBREL   LBFC  LBCFC LBPROC   LBVC  LBRVC  LBEXP   LBBEGIN    LBNREC'
WRITE (MessageOut, '(1X,7I7,2I10)') UMhdr % Lookup(22:30,K)
WRITE (MessageOut, '(A)') '  LBPROJ  LBTYP  LBLEV LBRSVD LBRSVD LBRSVD LBRSVD   LBSRCE'
WRITE (MessageOut, '(1X,7I7,I9)') UMhdr % Lookup(31:38,K)
WRITE (MessageOut, '(A)') '  DATA_TYPE     NADDR    LBUSER ITEM_CODE    LBPLEV    LBUSER MODEL_CODE'
WRITE (MessageOut, '(1X,6I10,I11)') UMhdr % Lookup(39:45,K)
WRITE (MessageOut, '(A)') '         BULEV       BHULEV     BRSVD(3)     BRSVD(4)       BDATUM'
WRITE (MessageOut, '(1X,1P,5E13.4)') RLOOKUP(46:50,K)
WRITE (MessageOut, '(A)') '          BACC         BLEV        BRLEV        BHLEV       BHRLEV'
WRITE (MessageOut, '(1X,1P,5E13.4)') RLOOKUP(51:55,K)
WRITE (MessageOut, '(A)') '         BPLAT        BPLON         BGOR          BZY          BDY'
WRITE (MessageOut, '(1X,1P,5E13.4)') RLOOKUP(56:60,K)
WRITE (MessageOut, '(A)') '           BZX          BDX         BMDI         BMKS'
WRITE (MessageOut, '(1X,1P,4E13.4)') RLOOKUP(61:64,K)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_PrintLookup
