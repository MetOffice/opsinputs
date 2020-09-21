!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Set up the REAL HEADER part of the varobs header.
!
! Inputs: Fortran90 structures containing (i) Cx header information, and (ii)
!         varobs header information.
!
! Outputs: Fortran90 structure containing varobs header information, with
!          modified real constant values.
!
! The values required are copied from the Cx header structure. The rest are set
! to missing data (RMDI).
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SetupVarobsRealC (CxHeader, & ! Structure containing Cx header info
                                 varobs)     ! Structure containing Varobs header info

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE GenMod_ModelIO, ONLY: &
    UM_header_type

USE GenMod_UMHeaderConstants, ONLY: &
    RC_LongSpacing,                 &
    RC_LatSpacing,                  &
    RC_FirstLat,                    &
    RC_FirstLong,                   &
    RC_PoleLat,                     &
    RC_PoleLong

USE OpsMod_VarobsLib, ONLY: &
    varobs_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (UM_header_type), INTENT(IN) :: CxHeader
TYPE (varobs_type), INTENT(INOUT) :: varobs

! Local declarations:
CHARACTER(len=*), PARAMETER       :: RoutineName = "Ops_SetupVarobsRealC"

!-------------------------------------------------------------------------------
! 0. Tracing
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-------------------------------------------------------------------------------
! 1. Assign real header values.
!-------------------------------------------------------------------------------

varobs % RealC(RC_LongSpacing) = CxHeader % RealC(RC_LongSpacing)
varobs % RealC(RC_LatSpacing) = CxHeader % RealC(RC_LatSpacing)
varobs % RealC(RC_FirstLat) = CxHeader % RealC(RC_FirstLat)
varobs % RealC(RC_FirstLong) = CxHeader % RealC(RC_FirstLong)
varobs % RealC(RC_PoleLat) = CxHeader % RealC(RC_PoleLat)
varobs % RealC(RC_PoleLong) = CxHeader % RealC(RC_PoleLong)
varobs % RealC(RC_PoleLong + 1:) = RMDI

!-------------------------------------------------------------------------------
! 2. Tracing and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SetupVarobsRealC
