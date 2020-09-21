!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Set up the fixed length header part of the varobs header.
!
! Inputs: Fortran90 structure containing varobs header information; the
!         identifier for the type of file (varobs = 7); and the length of the
!         data array previously created.
!
! Outputs: Fortran90 structure containing varobs header information, with
!          modified fixed length header values.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SetupVarobsFixhd (varobs,          & ! Structure containing Varobs header info.
                                 TotalObsElement)   ! Total number of elements in obs data arrays

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_ModelIO, ONLY: &
    LenFixHd

USE GenMod_UMHeaderConstants, ONLY: &
    FH_ColDepCSize1,                &
    FH_ColDepCSize2,                &
    FH_ColDepCStart,                &
    FH_Dataset,                     &
    FH_Dataset_VAROBS,              &
    FH_DataSize,                    &
    FH_DataStart,                   &
    FH_IntCSize,                    &
    FH_IntCStart,                   &
    FH_LevDepCSize1,                &
    FH_LevDepCSize2,                &
    FH_LevDepCStart,                &
    FH_LookupSize1,                 &
    FH_LookupSize2,                 &
    FH_LookupStart,                 &
    FH_MaxDataSize,                 &
    FH_RealCSize,                   &
    FH_RealCStart,                  &
    FH_Version

USE OpsMod_VarobsLib, ONLY: &
    varobs_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (varobs_type), INTENT(INOUT) :: varobs
INTEGER, INTENT(IN)               :: TotalObsElement

! Local declarations:
CHARACTER(len=*), PARAMETER       :: RoutineName = "Ops_SetupVarobsFixhd"

!-------------------------------------------------------------------------------
! 0. Tracing
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-------------------------------------------------------------------------------
! 1. Assign fixed length header values.
!-------------------------------------------------------------------------------

varobs % FixHd(FH_Version) = VarobsVersionNumber
varobs % FixHd(FH_Dataset) = FH_Dataset_VAROBS
varobs % FixHd(FH_IntCStart) = LenFixHd + 1
varobs % FixHd(FH_RealCStart) = varobs % FixHd(FH_IntCStart) + varobs % FixHd(FH_IntCSize)
varobs % FixHd(FH_LevDepCStart) = varobs % FixHd(FH_RealCStart) + varobs % FixHd(FH_RealCSize)
varobs % FixHd(FH_ColDepCStart) = varobs % FixHd(FH_LevDepCStart) + &
                                  varobs % FixHd(FH_LevDepCSize1) * varobs % FixHd(FH_LevDepCSize2)
varobs % FixHd(FH_LookupStart) = varobs % FixHd(FH_ColDepCStart) + &
                                 varobs % FixHd(FH_ColDepCSize1) * varobs % FixHd(FH_ColDepCSize2)
varobs % FixHd(FH_DataStart) = varobs % FixHd(FH_LookupStart) + &
                               varobs % FixHd(FH_LookupSize1) * varobs % FixHd(FH_LookupSize2)
varobs % FixHd(FH_DataSize) = TotalObsElement
varobs % FixHd(FH_MaxDataSize) = varobs % FixHd(FH_DataSize)

!-------------------------------------------------------------------------------
! 2. Tracing and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SetupVarobsFixhd
