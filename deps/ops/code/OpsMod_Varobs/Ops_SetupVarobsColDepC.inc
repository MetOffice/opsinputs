!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Set up the COLUMN DEPENDENT CONSTANT part of the varobs header. These
! constants are pointers for decoding the varobs information.
!
! Inputs: Fortran90 structure describing the varobs header.
!
! Outputs: The same Fortran90 structure is output, with the column dependent
!          contants initialised.
!
! Simple initialisation of the column dependent constants. Further documentation
! can be found in VTDP 8 (External Data Files - VAROBS & Cx).
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SetupVarobsColDepC (ObsGroup,        &
                                   Varobs,          &
                                   ElementsPresent, &
                                   ElementsLevels)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI

USE OpsMod_ObsInfo, ONLY: &
    LenCallSign

USE GenMod_UMHeaderConstants, ONLY: &
    FH_ColDepCSize1,                &
    CC_Meta_Latitude,               &
    CC_Meta_Longitude,              &
    CC_Meta_Time,                   &
    CC_Meta_Type,                   &
    CC_Meta_Call,                   &
    CC_Meta_Level,                  &
    CC_Meta_RepPGE,                 &
    CC_Item_Value,                  &
    CC_Item_Error,                  &
    CC_Item_PGE

USE OpsMod_ObsGroupInfo, ONLY: &
    ObsGroupSurface

USE OpsMod_VarobsLib, ONLY: &
    varobs_type

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)               :: ObsGroup
TYPE (varobs_type), INTENT(INOUT) :: Varobs
REAL, INTENT(INOUT)               :: ElementsPresent(:)
REAL, INTENT(INOUT)               :: ElementsLevels(:)

! Local declarations:
CHARACTER(len=*), PARAMETER       :: RoutineName = "Ops_SetupVarobsColDepC"
INTEGER, PARAMETER                :: CC_CxSf_Orog = 1
INTEGER, PARAMETER                :: CC_CxSf_Press = 2
INTEGER, PARAMETER                :: CC_CxSf_Temp = 3
INTEGER, PARAMETER                :: CC_CxSf_Humidity = 4
INTEGER, PARAMETER                :: CC_CxSf_WindU = 5
INTEGER, PARAMETER                :: CC_CxSf_WindV = 6
INTEGER, PARAMETER                :: CC_CxSf_qt = 7
INTEGER, PARAMETER                :: CC_CxSf_Aerosol = 8
INTEGER, PARAMETER                :: CC_CxUa_Temp = 1
INTEGER, PARAMETER                :: CC_CxUa_Humidity = 2
INTEGER, PARAMETER                :: CC_CxUa_WindU = 3
INTEGER, PARAMETER                :: CC_CxUa_WindV = 4
INTEGER                           :: Pointer
INTEGER                           :: Length

!-------------------------------------------------------------------------------
! 0. Tracing
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-------------------------------------------------------------------------------
! 1. Assign column dependent constant header values.
!-------------------------------------------------------------------------------
!
! Constants are "batched" into 8 groups of 8. Where a constant is NOT set to
! missing data (IMDI), then the data is present. If data is absent, the
! corresponding pointer is set to IMDI.
!
! ColDepC(:,1)   points to "Metadata"
! ColDepC(:,2)   points to the items for each variable
! ColDepC(:,3)   points to the items defining the pressure for upper levels
!                in the Ob column
! ColDepC(:,4)   points to the observation variables
! ColDepC(:,5)   points to the items defining the pressure for upper levels
!                in the Cx column
! ColDepC(:,6) points to the Surface variables in Cx column
! ColDepC(:,7) points to the Upper air variables in Cx column

Length = varobs % FixHd(FH_ColDepCSize1)

! Metadata
Pointer = 0
varobs % ColDepC(Pointer + CC_Meta_Latitude) = IMDI
varobs % ColDepC(Pointer + CC_Meta_Longitude) = IMDI
varobs % ColDepC(Pointer + CC_Meta_Time) = 1.0
varobs % ColDepC(Pointer + CC_Meta_Type) = 2.0
varobs % ColDepC(Pointer + CC_Meta_Call) = 3.0
varobs % ColDepC(Pointer + CC_Meta_Level) = 3.0 + LenCallSign
varobs % ColDepC(Pointer + CC_Meta_RepPGE) = 4.0 + LenCallSign
varobs % ColDepC(Pointer + CC_Meta_RepPGE + 1:Pointer + Length) = IMDI

! Items for each variable
Pointer = Pointer + Length
varobs % ColDepC(Pointer + CC_Item_Value) = 1.0
IF (NumItem == 3) THEN
  varobs % ColDepC(Pointer + CC_Item_Error) = 2.0
  varobs % ColDepC(Pointer + CC_Item_PGE) = 3.0
ELSE
  varobs % ColDepC(Pointer + CC_Item_Error) = IMDI
  varobs % ColDepC(Pointer + CC_Item_PGE) = IMDI
END IF
varobs % ColDepC(Pointer + CC_Item_PGE + 1:Pointer + Length) = IMDI

! Items defining pressure for upper levels in ob. column
Pointer = Pointer + Length
varobs % ColDepC(Pointer + 1:Pointer + Length) = IMDI !(absent)

! Observation variables
Pointer = Pointer + Length
varobs % ColDepC(Pointer + 1:Pointer + Length) = ElementsPresent(:)

! Items defining pressure for upper levels in Cx column
Pointer = Pointer + Length
varobs % ColDepC(Pointer + 1:Pointer + Length) = IMDI !     (absent)

! Surface variables in Cx column
Pointer = Pointer + Length
varobs % ColDepC(Pointer + 1:Pointer + Length) = IMDI !     (absent)
varobs % ColDepC(Pointer + CC_CxSf_Orog) = 1.0
varobs % ColDepC(Pointer + CC_CxSf_Press) = 2.0
varobs % ColDepC(Pointer + CC_CxSf_Temp) = 3.0
varobs % ColDepC(Pointer + CC_CxSf_Humidity) = 4.0
varobs % ColDepC(Pointer + CC_CxSf_WindU) = 5.0
varobs % ColDepC(Pointer + CC_CxSf_Windv) = 6.0

! Only require qt and aerosol Cx fields with surface obs.
IF (ObsGroup == ObsGroupSurface) THEN
  varobs % ColDepC(Pointer + CC_CxSf_qt) = 7.0
  varobs % ColDepC(Pointer + CC_CxSf_Aerosol) = 8.0
END IF

! Upper air variables in Cx column
Pointer = Pointer + Length
varobs % ColDepC(Pointer + CC_CxUa_Temp) = 1.0
varobs % ColDepC(Pointer + CC_CxUa_Humidity) = 2.0
varobs % ColDepC(Pointer + CC_CxUa_WindU) = 3.0
varobs % ColDepC(Pointer + CC_CxUa_WindV) = 4.0
varobs % ColDepC(Pointer + CC_CxUa_WindV + 1:Pointer + Length) = IMDI

! Number of observation levels (currently only used for ATOVS radiances files).
Pointer = Pointer + Length
varobs % ColDepC(Pointer + 1:Pointer + Length) = ElementsLevels(:)

!-------------------------------------------------------------------------------
! 2. Tracing and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SetupVarobsColDepC
