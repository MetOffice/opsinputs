!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Read in a model header from a UM dump.
!-------------------------------------------------------------------------------

SUBROUTINE Gen_ReadUMhdr (UMhdr,     &
                          ModelType)

USE GenMod_CLookAdd, ONLY: &
    ITEM_CODE,             &
    LBNREC,                &
    MODEL_CODE

USE GenMod_Control, ONLY: &
    GenMode,              &
    VerboseMode

USE GenMod_Core, ONLY: &
    gen_fail,          &
    gen_message,       &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI

USE GenMod_UMHeaderConstants, ONLY: &
    FH_ColDepCSize1,                &
    FH_ColDepCSize2,                &
    FH_ColDepCStart,                &
    FH_CompFldI1Size,               &
    FH_CompFldI1Start,              &
    FH_CompFldI2Size,               &
    FH_CompFldI2Start,              &
    FH_CompFldI3Size,               &
    FH_CompFldI3Start,              &
    FH_DataSize,                    &
    FH_ExtraCSize,                  &
    FH_ExtraCStart,                 &
    FH_FldsOfCSize1,                &
    FH_FldsOfCSize2,                &
    FH_FldsOfCStart,                &
    FH_IntCSize,                    &
    FH_IntCStart,                   &
    FH_LevDepCSize1,                &
    FH_LevDepCSize2,                &
    FH_LevDepCStart,                &
    FH_LookupSize1,                 &
    FH_LookupSize2,                 &
    FH_LookupStart,                 &
    FH_RealCSize,                   &
    FH_RealCStart,                  &
    FH_RowDepCSize1,                &
    FH_RowDepCSize2,                &
    FH_RowDepCStart

USE OpsMod_Portio_Wrappers, ONLY: &
    Ops_Buffin_Wrapper,           &
    Ops_Setpos_Wrapper

IMPLICIT NONE

! Subroutine arguments:
CLASS (UM_header_type), INTENT (INOUT) :: UMhdr       ! UM header from Model dump
CHARACTER(len=*), INTENT (IN)          :: ModelType   ! e.g. 'LS ', 'PF ', 'Cov'

! Local declarations:
CHARACTER(len=*), PARAMETER            :: RoutineName = "Gen_ReadUMhdr"
INTEGER                                :: WordAddress  ! Position on file, used in OPS_SETPOS
CHARACTER(len=80)                      :: ErrMess      ! Message for gen_fail
INTEGER                                :: K
INTEGER                                :: FIXHD_152    !  Original value of FIXHD(152)
INTEGER                                :: START_BLOCK
CHARACTER(len=50), ALLOCATABLE         :: messages(:)

IF (UseTrace) THEN
  CALL gen_trace_entry (RoutineName,                                                          &
                        Message = "Reading in " // ModelType // " UM header from model dump")
END IF

!-------------------------------------------------------------------------------
! Section 1.    Allocate & read in Fixed-Length Header from Model dump file
!-------------------------------------------------------------------------------

WordAddress = 0
CALL Ops_Setpos_Wrapper (UMhdr % UnitNum, &
                         WordAddress)

CALL Ops_Buffin_Wrapper (UMhdr % UnitNum, &   ! in
                         UMhdr % FixHd,   &   ! out
                         LenFixHd)            ! in

!-------------------------------------------------------------------------------
! Section 2.    Allocate space for the UM header from the Model dump file
!-------------------------------------------------------------------------------

CALL UMhdr % alloc        ! inout

!-------------------------------------------------------------------------------
! Section 3.    Read in complete UM header from the Model dump file
!-------------------------------------------------------------------------------

WordAddress = 0
CALL Ops_Setpos_Wrapper (UMhdr % UnitNum, &
                         WordAddress)

START_BLOCK = LenFixHd + 1

FIXHD_152 = UMhdr % FixHd(FH_LookupSize2)    !  Store original value

! Print out fixed header information on high verbosity

IF (GenMode >= VerboseMode) THEN
  ALLOCATE (messages(0:LenFixHd))
  WRITE (messages(0), '(A)') "Print of fixed header"
  DO K = 1, LenFixHd
    WRITE (messages(K), '(A,I0,A,I0)') "index = ", K, " fixed_header(K) = ", UMhdr % FixHd(K)
  END DO
  CALL gen_message (RoutineName, &
                    messages)
  DEALLOCATE (messages)
END IF

CALL Ops_Setpos_Wrapper (UMhdr % UnitNum, &
                         START_BLOCK - 1)

! integer constants

IF (UMhdr % FixHd(FH_IntCStart) > 0) THEN
  IF (UMhdr % FixHd(FH_IntCStart) /= START_BLOCK) THEN
    WRITE (ErrMess, '(A,I0,A,I0)') 'IntC start ',UMhdr % FixHd(FH_IntCStart), &
                                   ' /= end of FixHd ', START_BLOCK
    CALL gen_fail (RoutineName, &  ! in
                   ErrMess)        ! in
  END IF
  CALL Ops_Buffin_Wrapper (UMhdr % UnitNum,            &
                           UMhdr % IntC,               &
                           UMHdr % FixHd(FH_IntCSize))
  START_BLOCK = START_BLOCK + UMHdr % FixHd(FH_IntCSize)
END IF

! real constants

IF (UMhdr % FixHd(FH_RealCStart) > 0) THEN
  IF (UMhdr % FixHd(FH_RealCStart) /= START_BLOCK) THEN
    WRITE (ErrMess, '(A,I0,A,I0)') 'RealC start ',UMhdr % FixHd(FH_RealCStart), &
                                   ' /= end of IntC ', START_BLOCK
    CALL gen_fail (RoutineName, &  ! in
                   ErrMess)        ! in
  END IF
  CALL Ops_Buffin_Wrapper (UMhdr % UnitNum,             &
                           UMhdr % RealC,               &
                           UMHdr % FixHd(FH_RealCSize))
  START_BLOCK = START_BLOCK + UMHdr % FixHd(FH_RealCSize)
END IF

! level dependent constants

IF (UMhdr % FixHd(FH_LevDepCStart) > 0) THEN
  IF (UMhdr % FixHd(FH_LevDepCStart) /= START_BLOCK) THEN
    WRITE (ErrMess, '(A,I0,A,I0)') 'LevDepC start ',UMhdr % FixHd(FH_LevDepCStart), &
                                   ' /= end of RealC ', START_BLOCK
    CALL gen_fail (RoutineName, &  ! in
                   ErrMess)        ! in
  END IF
  CALL Ops_Buffin_Wrapper (UMhdr % UnitNum,                                                 &
                           UMhdr % LevDepC,                                                 &
                           UMHdr % FixHd(FH_LevDepCSize1) * UMHdr % FixHd(FH_LevDepCSize2))
  START_BLOCK = START_BLOCK + UMHdr % FixHd(FH_LevDepCSize1) * UMHdr % FixHd(FH_LevDepCSize2)
END IF

! row dependent constants

IF (UMhdr % FixHd(FH_RowDepCStart) > 0) THEN
  IF (UMhdr % FixHd(FH_RowDepCStart) /= START_BLOCK) THEN
    WRITE (ErrMess, '(A,I0,A,I0)') 'RowDepC start ',UMhdr % FixHd(FH_RowDepCStart), &
                                   ' /= end of LevDepC ', START_BLOCK
    CALL gen_fail (RoutineName, &  ! in
                   ErrMess)        ! in
  END IF
  CALL Ops_Buffin_Wrapper (UMhdr % UnitNum,                                                 &
                           UMhdr % RowDepC,                                                 &
                           UMHdr % FixHd(FH_RowDepCSize1) * UMHdr % FixHd(FH_RowDepCSize2))
  START_BLOCK = START_BLOCK + UMHdr % FixHd(FH_RowDepCSize1) * UMHdr % FixHd(FH_RowDepCSize2)
END IF

! column dependent constants

IF (UMhdr % FixHd(FH_ColDepCStart) > 0) THEN
  IF (UMhdr % FixHd(FH_ColDepCStart) /= START_BLOCK) THEN
    WRITE (ErrMess, '(A,I0,A,I0)') 'ColDepC start ',UMhdr % FixHd(FH_ColDepCStart), &
                                   ' /= end of RowDepC ', START_BLOCK
    CALL gen_fail (RoutineName, &  ! in
                   ErrMess)        ! in
  END IF
  CALL Ops_Buffin_Wrapper (UMhdr % UnitNum,                                                 &
                           UMhdr % ColDepC,                                                 &
                           UMHdr % FixHd(FH_ColDepCSize1) * UMHdr % FixHd(FH_ColDepCSize2))
  START_BLOCK = START_BLOCK + UMHdr % FixHd(FH_ColDepCSize1) * UMHdr % FixHd(FH_ColDepCSize2)
END IF

! constants stored as fields

IF (UMhdr % FixHd(FH_FldsOfCStart) > 0) THEN
  IF (UMhdr % FixHd(FH_FldsOfCStart) /= START_BLOCK) THEN
    WRITE (ErrMess, '(A,I0,A,I0)') 'FldsOfC start ',UMhdr % FixHd(FH_FldsOfCStart), &
                                   ' /= end of ColDepC ', START_BLOCK
    CALL gen_fail (RoutineName, &  ! in
                   ErrMess)        ! in
  END IF
  CALL Ops_Buffin_Wrapper (UMhdr % UnitNum,                                                 &
                           UMhdr % FldsOfC,                                                 &
                           UMHdr % FixHd(FH_FldsOfCSize1) * UMHdr % FixHd(FH_FldsOfCSize2))
  START_BLOCK = START_BLOCK + UMHdr % FixHd(FH_FldsOfCSize1) * UMHdr % FixHd(FH_FldsOfCSize2)
END IF

! extra constants

IF (UMhdr % FixHd(FH_ExtraCStart) > 0) THEN
  IF (UMhdr % FixHd(FH_ExtraCStart) /= START_BLOCK) THEN
    WRITE (ErrMess, '(A,I0,A,I0)') 'ExtraC start ',UMhdr % FixHd(FH_ExtraCStart), &
                                   ' /= end of FldsOfC ', START_BLOCK
    CALL gen_fail (RoutineName, &  ! in
                   ErrMess)        ! in
  END IF
  CALL Ops_Buffin_Wrapper (UMhdr % UnitNum,              &
                           UMhdr % ExtraC,               &
                           UMHdr % FixHd(FH_ExtraCSize))
  START_BLOCK = START_BLOCK + UMHdr % FixHd(FH_ExtraCSize)
END IF

! compressed field index1

IF (UMhdr % FixHd(FH_CompFldI1Start) > 0) THEN
  IF (UMhdr % FixHd(FH_CompFldI1Start) /= START_BLOCK) THEN
    WRITE (ErrMess, '(A,I0,A,I0)') 'CompFld1 start ',UMhdr % FixHd(FH_CompFldI1Start), &
                                   ' /= end of ExtraC ', START_BLOCK
    CALL gen_fail (RoutineName, &  ! in
                   ErrMess)        ! in
  END IF
  CALL Ops_Buffin_Wrapper (UMhdr % UnitNum,                 &
                           UMhdr % CompFldI1,               &
                           UMHdr % FixHd(FH_CompFldI1Size))
  START_BLOCK = START_BLOCK + UMHdr % FixHd(FH_CompFldI1Size)
END IF

! compressed field index2

IF (UMhdr % FixHd(FH_CompFldI2Start) > 0) THEN
  IF (UMhdr % FixHd(FH_CompFldI2Start) /= START_BLOCK) THEN
    WRITE (ErrMess, '(A,I0,A,I0)') 'CompFld2 start ',UMhdr % FixHd(FH_CompFldI2Start), &
                                   ' /= end of CompFld1 ', START_BLOCK
    CALL gen_fail (RoutineName, &  ! in
                   ErrMess)        ! in
  END IF
  CALL Ops_Buffin_Wrapper (UMhdr % UnitNum,                 &
                           UMhdr % CompFldI2,               &
                           UMHdr % FixHd(FH_CompFldI2Size))
  START_BLOCK = START_BLOCK + UMHdr % FixHd(FH_CompFldI2Size)
END IF

! compressed field index3

IF (UMhdr % FixHd(FH_CompFldI3Start) > 0) THEN
  IF (UMhdr % FixHd(FH_CompFldI3Start) /= START_BLOCK) THEN
    WRITE (ErrMess, '(A,I0,A,I0)') 'CompFld3 start ',UMhdr % FixHd(FH_CompFldI3Start), &
                                   ' /= end of CompFld2 ', START_BLOCK
    CALL gen_fail (RoutineName, &  ! in
                   ErrMess)        ! in
  END IF
  CALL Ops_Buffin_Wrapper (UMhdr % UnitNum,                 &
                           UMhdr % CompFldI3,               &
                           UMHdr % FixHd(FH_CompFldI3Size))
  START_BLOCK = START_BLOCK + UMHdr % FixHd(FH_CompFldI3Size)
END IF

! lookup table

IF (UMhdr % FixHd(FH_LookupStart) > 0) THEN
  ! Supress checking if not full dump
  IF (UMhdr % FixHd(FH_LookupStart) /= START_BLOCK) THEN
    WRITE (ErrMess, '(A,I0,A,I0)') 'Lookup start ',UMhdr % FixHd(FH_LookupStart), &
                                   ' /= end of CompFld3 ', START_BLOCK
    CALL gen_fail (RoutineName, &  ! in
                   ErrMess)        ! in
  END IF

  CALL Ops_Setpos_Wrapper (UMhdr % UnitNum,                   &
                           UMhdr % FixHd(FH_LookupStart) - 1)

  CALL Ops_Buffin_Wrapper (UMhdr % UnitNum,               &
                           UMhdr % Lookup,                &
                           UMHdr % FixHd(FH_LookupSize1), &
                           UMHdr % FixHd(FH_LookupSize2))

  ! Point to start of data section ( Use original FIXHD(152) )

  START_BLOCK = START_BLOCK + UMhdr % FixHd(FH_LookupSize1) * FIXHD_152

  !---------------------------------------------------------------
  ! Reset LOOKUP(MODEL_CODE) if not set
  !---------------------------------------------------------------

  DO K = 1,UMhdr % FixHd (FH_LookupSize2)
    IF (UMhdr % Lookup(MODEL_CODE,K) == 0 .OR. &
        UMhdr % Lookup(MODEL_CODE,K) == IMDI) THEN

      IF (UMhdr % Lookup(ITEM_CODE,K) <= 100 .OR. &
          (UMhdr % Lookup(ITEM_CODE,K) >= 200.AND. &
           UMhdr % Lookup(ITEM_CODE,K) <= 205)) THEN
        ! Section 0: Prognostic fields.
        UMhdr % Lookup(MODEL_CODE,K) = 1

      ELSE IF ((UMhdr % Lookup(ITEM_CODE,K) > 100 .AND. &
                UMhdr % Lookup(ITEM_CODE,K) <= 176) .OR. &
               (UMhdr % Lookup(ITEM_CODE,K) >= 180 .AND. &
                UMhdr % Lookup(ITEM_CODE,K) < 200))THEN
        UMhdr % Lookup(MODEL_CODE,K) = 2

      ELSE IF ((UMhdr % Lookup(ITEM_CODE,K) >= 177 .AND. &
                UMhdr % Lookup(ITEM_CODE,K) <= 179) .OR. &
               (UMhdr % Lookup(ITEM_CODE,K) >= 210 .AND. &
                UMhdr % Lookup(ITEM_CODE,K) <= 212)) THEN
        UMhdr % Lookup(MODEL_CODE,K) = 3

      ELSE IF(UMhdr % Lookup(ITEM_CODE,K) >= 1000 .AND. &
              UMhdr % Lookup(ITEM_CODE,K) <= 29999) THEN
        ! Sections 1 - 99: Diagnostic fields
        IF ((UMhdr % Lookup(ITEM_CODE,K) >= 21177 .AND. &
             UMhdr % Lookup(ITEM_CODE,K) <= 21179) .OR. &
            (UMhdr % Lookup(ITEM_CODE,K) >= 21225 .AND. &
             UMhdr % Lookup(ITEM_CODE,K) <= 21227) .OR. &
            (UMhdr % Lookup(ITEM_CODE,K) >= 22177 .AND. &
             UMhdr % Lookup(ITEM_CODE,K) <= 22179) .OR. &
            (UMhdr % Lookup(ITEM_CODE,K) >= 22225 .AND. &
             UMhdr % Lookup(ITEM_CODE,K) <= 22227) .OR. &
            (UMhdr % Lookup(ITEM_CODE,K) >= 23177 .AND. &
             UMhdr % Lookup(ITEM_CODE,K) <= 23179) .OR. &
            (UMhdr % Lookup(ITEM_CODE,K) >= 23225 .AND. &
             UMhdr % Lookup(ITEM_CODE,K) <= 23227) .OR. &
            (UMhdr % Lookup(ITEM_CODE,K) >= 24177 .AND. &
             UMhdr % Lookup(ITEM_CODE,K) <= 24179) .OR. &
            (UMhdr % Lookup(ITEM_CODE,K) >= 24225 .AND. &
             UMhdr % Lookup(ITEM_CODE,K) <= 24227)) THEN
          UMhdr % Lookup(MODEL_CODE,K) = 3        ! Slab diagnostic
        ELSE
          UMhdr % Lookup(MODEL_CODE,K) = 1        ! Atmosphere diagnostic
        END IF
      ELSE IF (UMhdr % Lookup(ITEM_CODE,K) >= 30000 .AND. &
               UMhdr % Lookup(ITEM_CODE,K) <= 99999) THEN
        IF (UMhdr % Lookup(ITEM_CODE,K) >= 40000 .AND. &
            UMhdr % Lookup(ITEM_CODE,K) <= 40999) THEN
          UMhdr % Lookup(MODEL_CODE,K) = 3        ! Slab diagnostic
        ELSE
          UMhdr % Lookup(MODEL_CODE,K) = 2        ! Ocean diagnostic
        END IF
      ELSE
        UMhdr % Lookup(MODEL_CODE,K) = 1
      END IF
    END IF
  END DO

  ! Check LOOKUP for consistency with PARAMETER statements

  IF (UMhdr % Lookup(LBNREC,1) == 0) THEN
    IF (UMhdr % FixHd(FH_DataSize) /= IMDI) THEN
      CALL UMhdr % check_lookup
    END IF
  END IF
END IF

WordAddress = 0
CALL Ops_Setpos_Wrapper (UMhdr % UnitNum, &
                         WordAddress)

CALL UMhdr % max_field_size

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Gen_ReadUMhdr
