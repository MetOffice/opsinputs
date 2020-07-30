!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Write out a model header to a UM dump.
!-------------------------------------------------------------------------------

SUBROUTINE Gen_WriteUMhdr (UMhdr,        &
                           ModelType,    &
                           TrustAddress, &
                           CheckLookup)

USE GenMod_CLookAdd, ONLY: &
    LBNREC

USE GenMod_Core, ONLY: &
    gen_do_trace,      &
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
    Ops_Buffout_Wrapper,          &
    Ops_Setpos_Wrapper

IMPLICIT NONE

! Subroutine arguments:
CLASS (UM_header_type), INTENT(INOUT) :: UMhdr        ! UM header from Model dump
CHARACTER(len=*), INTENT(IN)          :: ModelType    ! e.g. 'LS ', 'PF ', 'Adj'
LOGICAL, OPTIONAL, INTENT(IN)         :: TrustAddress ! is the UM header correct
LOGICAL, OPTIONAL, INTENT(IN)         :: CheckLookup

! Local declarations:
CHARACTER(len=*), PARAMETER           :: RoutineName = "Gen_WriteUMhdr"
INTEGER                               :: WordAddress        ! Position on file, used in ops_setpos
LOGICAL                               :: UpdateAddress
LOGICAL                               :: Local_CheckLookup

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-------------------------------------------------------------------------------
!  Section 0.1:  Initialisations
!-------------------------------------------------------------------------------

IF (UseTrace) THEN
  CALL gen_do_trace (RoutineName,                                                          &
                     Message = "Writing out a " // ModelType // " header to a model dump")
END IF

!-------------------------------------------------------------------------------
!  Section 1.   Write out a UM header to a Model dump file
!-------------------------------------------------------------------------------

WordAddress = 0
CALL Ops_Setpos_Wrapper (UMhdr % UnitNum, &
                         WordAddress)

UpdateAddress = .TRUE.
IF (PRESENT (TrustAddress)) THEN
  UpdateAddress = .NOT. TrustAddress
END IF

IF (UpdateAddress) THEN
  CALL Ops_SetDumpfileAddress (UMhdr % FixHd,                 &
                               LenFixHd,                      &
                               UMhdr % Lookup,                &
                               UMhdr % FixHd(FH_LookupSize1), &
                               UMhdr % FixHd(FH_LookupSize2))
END IF

IF (PRESENT (CheckLookup)) THEN
  Local_CheckLookup = CheckLookup
ELSE
  Local_CheckLookup = .TRUE.
END IF

! fixed length header

CALL Ops_Buffout_Wrapper (UMhdr % UnitNum, &
                          UMhdr % FixHd,   &
                          LenFixHd)

! integer constants

IF (UMhdr % FixHd(FH_IntCStart) > 0) THEN
  CALL Ops_Buffout_Wrapper (UMhdr % UnitNum,            &
                            UMhdr % IntC,               &
                            UMHdr % FixHd(FH_IntCSize))
END IF

! real constants

IF (UMhdr % FixHd(FH_RealCStart) > 0) THEN
  CALL Ops_Buffout_Wrapper (UMhdr % UnitNum,             &
                            UMhdr % RealC,               &
                            UMHdr % FixHd(FH_RealCSize))
END IF

! level dependent constants

IF (UMhdr % FixHd(FH_LevDepCStart) > 0) THEN
  CALL Ops_Buffout_Wrapper (UMhdr % UnitNum,                                                 &
                            UMhdr % LevDepC,                                                 &
                            UMHdr % FixHd(FH_LevDepCSize1) * UMHdr % FixHd(FH_LevDepCSize2))
END IF

! row dependent constants

IF (UMhdr % FixHd(FH_RowDepCStart) > 0) THEN
  CALL Ops_Buffout_Wrapper (UMhdr % UnitNum,                                                 &
                            UMhdr % RowDepC,                                                 &
                            UMHdr % FixHd(FH_RowDepCSize1) * UMHdr % FixHd(FH_RowDepCSize2))
END IF

! column dependent constants

IF (UMhdr % FixHd(FH_ColDepCStart) > 0) THEN
  CALL Ops_Buffout_Wrapper (UMhdr % UnitNum,                                                 &
                            UMhdr % ColDepC,                                                 &
                            UMHdr % FixHd(FH_ColDepCSize1) * UMHdr % FixHd(FH_ColDepCSize2))
END IF

! constants stored as fields

IF (UMhdr % FixHd(FH_FldsOfCStart) > 0) THEN
  CALL Ops_Buffout_Wrapper (UMhdr % UnitNum,                                                 &
                            UMhdr % FldsOfC,                                                 &
                            UMHdr % FixHd(FH_FldsOfCSize1) * UMHdr % FixHd(FH_FldsOfCSize2))
END IF

! extra constants

IF (UMhdr % FixHd(FH_ExtraCStart) > 0) THEN
  CALL Ops_Buffout_Wrapper (UMhdr % UnitNum,              &
                            UMhdr % ExtraC,               &
                            UMHdr % FixHd(FH_ExtraCSize))
END IF

! compressed field index1

IF (UMhdr % FixHd(FH_CompFldI1Start) > 0) THEN
  CALL Ops_Buffout_Wrapper (UMhdr % UnitNum,                 &
                            UMhdr % CompFldI1,               &
                            UMHdr % FixHd(FH_CompFldI1Size))
END IF

! compressed field index2

IF (UMhdr % FixHd(FH_CompFldI2Start) > 0) THEN
  CALL Ops_Buffout_Wrapper (UMhdr % UnitNum,                 &
                            UMhdr % CompFldI2,               &
                            UMHdr % FixHd(FH_CompFldI2Size))
END IF

! compressed field index3

IF (UMhdr % FixHd(FH_CompFldI3Start) > 0) THEN
  CALL Ops_Buffout_Wrapper (UMhdr % UnitNum,                 &
                            UMhdr % CompFldI3,               &
                            UMHdr % FixHd(FH_CompFldI3Size))
END IF

! lookup table

IF (UMhdr % FixHd(FH_LookupStart) > 0) THEN
  CALL Ops_Buffout_Wrapper (UMhdr % UnitNum,               &
                            UMhdr % Lookup,                &
                            UMHdr % FixHd(FH_LookupSize1), &
                            UMHdr % FixHd(FH_LookupSize2))

  ! Check LOOKUP for consistency with PARAMETER statements

  IF (Local_CheckLookup .AND. UMhdr % Lookup(LBNREC,1) == 0) THEN
    IF (UMhdr % FixHd(FH_DataSize) /= IMDI) THEN
      CALL UMhdr % check_lookup
    END IF
  END IF
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Gen_WriteUMhdr
