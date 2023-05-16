!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Read in a varobs header.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ReadVarobsHdr (varobs)

USE GenMod_Core, ONLY: &
    gen_fail,          &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_ModelIO, ONLY: &
    LenFixHd

USE GenMod_UMHeaderConstants, ONLY: &
    FH_ColDepCSize1,                &
    FH_ColDepCSize2,                &
    FH_ColDepCStart,                &
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
    IC_NumItem,                     &
    IC_NumMeta

USE OpsMod_Portio_Wrappers, ONLY: &
    Ops_Buffin_Wrapper,           &
    Ops_Setpos_Wrapper

IMPLICIT NONE

! Subroutine arguments:
CLASS (varobs_type), INTENT (INOUT) :: varobs

! Local declarations:
CHARACTER(len=*), PARAMETER         :: RoutineName = "Ops_ReadVarobsHdr"
INTEGER                             :: WordAddress  ! Position on file, used in OPS_SETPOS
CHARACTER(len=200)                  :: ErrMess      ! Message for gen_fail
INTEGER                             :: start_block

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-------------------------------------------------------------------------------
! Section 1.    Allocate & read in Fixed-Length Header from Model dump file
!-------------------------------------------------------------------------------

WordAddress = 0
CALL Ops_Setpos_Wrapper (varobs % unit_num, &
                         WordAddress)

CALL Ops_Buffin_Wrapper (varobs % unit_num, &
                         varobs % Fixhd,    &
                         LenFixHd)

!-------------------------------------------------------------------------------
! Section 2.    Allocate space for the UM header from the Model dump file
!-------------------------------------------------------------------------------

CALL varobs % alloc

!-------------------------------------------------------------------------------
! Section 3.    Read in complete UM header from the Model dump file
!-------------------------------------------------------------------------------

WordAddress = 0
CALL Ops_Setpos_Wrapper (varobs % unit_num, &
                         WordAddress)

start_block = LenFixHd + 1

CALL Ops_Setpos_Wrapper (varobs % unit_num, &
                         start_block - 1)

! integer constants

IF (varobs % FixHd(FH_IntCStart) > 0) THEN
  IF (varobs % FixHd(FH_IntCStart) /= start_block) THEN
    WRITE (ErrMess, '(A,I0,A,I0)') 'IntC start ', varobs % FixHd(FH_IntCStart), &
                                   ' /= end of FixHd ', start_block
    CALL gen_fail (RoutineName, &  ! in
                   ErrMess)        ! in
  END IF
  CALL Ops_Buffin_Wrapper (varobs % unit_num,           &
                           varobs % IntC,               &
                           varobs % FixHd(FH_IntCSize))
  start_block = start_block + varobs % FixHd(FH_IntCSize)
END IF

! real constants

IF (varobs % FixHd(FH_RealCStart) > 0) THEN
  IF (varobs % FixHd(FH_RealCStart) /= start_block) THEN
    WRITE (ErrMess, '(A,I0,A,I0)') 'RealC start ', varobs % FixHd(FH_RealCStart), &
                                   ' /= end of IntC ', start_block
    CALL gen_fail (RoutineName, &  ! in
                   ErrMess)        ! in
  END IF
  CALL Ops_Buffin_Wrapper (varobs % unit_num,            &
                           varobs % RealC,               &
                           varobs % FixHd(FH_RealCSize))
  start_block = start_block + varobs % FixHd(FH_RealCSize)
END IF

! level dependent constants

IF (varobs % FixHd(FH_LevDepCStart) > 0) THEN
  IF (varobs % FixHd(FH_LevDepCStart) /= start_block) THEN
    WRITE (ErrMess, '(A,I0,A,I0)') 'LevDepC start ', varobs % FixHd(FH_LevDepCStart), &
                                   ' /= end of RealC ', start_block
    CALL gen_fail (RoutineName, &  ! in
                   ErrMess)        ! in
  END IF
  CALL Ops_Buffin_Wrapper (varobs % unit_num,                                                 &
                           varobs % LevDepC,                                                  &
                           varobs % FixHd(FH_LevDepCSize1) * varobs % FixHd(FH_LevDepCSize2))
  start_block = start_block + varobs % FixHd(FH_LevDepCSize1) * varobs % FixHd(FH_LevDepCSize2)
END IF

! column dependent constants

IF (varobs % FixHd(FH_ColDepCStart) > 0) THEN
  IF (varobs % FixHd(FH_ColDepCStart) /= start_block) THEN
    WRITE (ErrMess, '(A,I0,A,I0)') 'ColDepC start ', varobs % FixHd(FH_ColDepCStart), &
                                   ' /= end of RowDepC ', start_block
    CALL gen_fail (RoutineName, &  ! in
                   ErrMess)        ! in
  END IF
  CALL Ops_Buffin_Wrapper (varobs % unit_num,                                                 &
                           varobs % ColDepC,                                                  &
                           varobs % FixHd(FH_ColDepCSize1) * varobs % FixHd(FH_ColDepCSize2))
  start_block = start_block + varobs % FixHd(FH_ColDepCSize1) * varobs % FixHd(FH_ColDepCSize2)
END IF

! lookup table

CALL Ops_Setpos_Wrapper (varobs % unit_num,                  &
                         varobs % FixHd(FH_LookupStart) - 1)

CALL Ops_Buffin_Wrapper (varobs % unit_num,              &
                         varobs % Lookup,                &
                         varobs % FixHd(FH_LookupSize1), &
                         varobs % FixHd(FH_LookupSize2))

varobs % num_data = varobs % intc(IC_NumItem)
varobs % num_meta = varobs % intc(IC_NumMeta)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ReadVarobsHdr
