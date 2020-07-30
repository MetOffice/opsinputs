!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Spread a model header from a UM dump between processors.
! Broadcast all parts from a given source PE to all others.
!-------------------------------------------------------------------------------

SUBROUTINE Gen_SpreadUMhdr (UMhdr,    &
                            SourcePE, &
                            Comm)

USE GenMod_Control, ONLY: &
    GlobalGroup

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

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

USE OpsMod_MPLInterface, ONLY: &
    ops_mpl_bcast,             &
    ops_mpl_comm_rank

USE MPL, ONLY:   &
    MPL_INTEGER, &
    MPL_REAL 

IMPLICIT NONE

! Subroutine arguments:
CLASS (UM_header_type), INTENT(INOUT) :: UMhdr    ! UM header
INTEGER, INTENT(IN)                   :: SourcePE ! PE to broadcast from
INTEGER, OPTIONAL, INTENT(IN)         :: Comm 

! Local declarations:
CHARACTER(len=*), PARAMETER           :: RoutineName = "Gen_SpreadUMhdr"
INTEGER                               :: istat
INTEGER                               :: group
INTEGER                               :: id

IF (UseTrace) CALL gen_trace_entry (RoutineName)

#ifndef SERIAL_SRC

IF (PRESENT (Comm)) THEN
  group = Comm
ELSE
  group = GlobalGroup
END IF

CALL ops_mpl_comm_rank (group, &
                        id,    &
                        istat)

!-------------------------------------------------------------------------------
! Section 1.    Allocate & read in Fixed-Length Header from Model dump file
!-------------------------------------------------------------------------------

CALL ops_mpl_bcast (UMhdr % FixHd, &
                    LenFixHd,      &
                    MPL_INTEGER,   &
                    SourcePE,      &
                    group,         &
                    istat)

IF (id /= SourcePE) THEN
  CALL UMhdr % alloc ! inout
END IF

IF (UMhdr % FixHd(FH_IntCStart) > 0) THEN
  CALL ops_mpl_bcast (UMhdr % IntC,               &
                      UMhdr % FixHd(FH_IntCSize), &
                      MPL_INTEGER,                &
                      SourcePE,                   &
                      group,                      &
                      istat)

END IF

IF (UMhdr % FixHd(FH_RealCStart) > 0) THEN
  CALL ops_mpl_bcast (UMhdr % RealC,               &
                      UMhdr % FixHd(FH_RealCSize), &
                      MPL_REAL,                    &
                      SourcePE,                    &
                      group,                       &
                      istat)
END IF

IF (UMhdr % FixHd(FH_LevDepCStart) > 0) THEN
  CALL ops_mpl_bcast (UMhdr % LevDepC,                                                 &
                      UMhdr % FixHd(FH_LevDepCSize1) * UMhdr % FixHd(FH_LevDepCSize2), &
                      MPL_REAL,                                                        &
                      SourcePE,                                                        &
                      group,                                                           &
                      istat)
END IF

IF (UMhdr % FixHd(FH_RowDepCStart) > 0) THEN
  CALL ops_mpl_bcast (UMhdr % RowDepC,                                                 &
                      UMhdr % FixHd(FH_RowDepCSize1) * UMhdr % FixHd(FH_RowDepCSize2), &
                      MPL_REAL,                                                        &
                      SourcePE,                                                        &
                      group,                                                           &
                      istat)
END IF

IF (UMhdr % FixHd(FH_ColDepCStart) > 0) THEN
  CALL ops_mpl_bcast (UMhdr % ColDepC,                                                 &
                      UMhdr % FixHd(FH_ColDepCSize1) * UMhdr % FixHd(FH_ColDepCSize2), &
                      MPL_REAL,                                                        &
                      SourcePE,                                                        &
                      group,                                                           &
                      istat)
END IF

IF (UMhdr % FixHd(FH_FldsOfCStart) > 0) THEN
  CALL ops_mpl_bcast (UMhdr % FldsOfC,                                                 &
                      UMhdr % FixHd(FH_FldsOfCSize1) * UMhdr % FixHd(FH_FldsOfCSize2), &
                      MPL_REAL,                                                        &
                      SourcePE,                                                        &
                      group,                                                           &
                      istat)
END IF

IF (UMhdr % FixHd(FH_ExtraCStart) > 0) THEN
  CALL ops_mpl_bcast (UMhdr % ExtraC,               &
                      UMhdr % FixHd(FH_ExtraCSize), &
                      MPL_REAL,                     &
                      SourcePE,                     &
                      group,                        &
                      istat)
END IF

IF (UMhdr % FixHd(FH_LookupStart) > 0) THEN
  CALL ops_mpl_bcast (UMhdr % Lookup,                                                &
                      UMhdr % FixHd(FH_LookupSize1) * UMhdr % FixHd(FH_LookupSize2), &
                      MPL_INTEGER,                                                   &
                      SourcePE,                                                      &
                      group,                                                         &
                      istat)
END IF

! The following 3 fields are never used in anger, and could be
! skipped if only PE 0 is ever required to write out headers again

IF (UMhdr % FixHd(FH_CompFldI1Start) > 0) THEN
  CALL ops_mpl_bcast (UMhdr % CompFldI1,               &
                      UMhdr % FixHd(FH_CompFldI1Size), &
                      MPL_INTEGER,                     &
                      SourcePE,                        &
                      group,                           &
                      istat)
END IF

IF (UMhdr % FixHd(FH_CompFldI2Start) > 0) THEN
  CALL ops_mpl_bcast (UMhdr % CompFldI2,               &
                      UMhdr % FixHd(FH_CompFldI2Size), &
                      MPL_INTEGER,                     &
                      SourcePE,                        &
                      group,                           &
                      istat)
END IF

IF (UMhdr % FixHd(FH_CompFldI3Start) > 0) THEN
  CALL ops_mpl_bcast (UMhdr % CompFldI3,               &
                      UMhdr % FixHd(FH_CompFldI3Size), &
                      MPL_INTEGER,                     &
                      SourcePE,                        &
                      group,                           &
                      istat)
END IF

#endif

CALL UMhdr % max_field_size

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Gen_SpreadUMhdr
