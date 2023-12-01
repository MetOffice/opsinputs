!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Set up the lookup part of the varobs header.
!
! Inputs: Fortran90 structure describing the varobs header.
!
! Outputs: The same Fortran90 structure is output, with the look-up table
!          contants initialised.
!
! Method: Simple initialisation of the look-up table values. Further
!         documentation can be found in OTDP 16.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SetupVarobsLookup (Observations,    &
                                  NumVarObsPE,     &
                                  LocalObsElement, &
                                  NumVarObstotal,  &
                                  Varobs)

USE GenMod_CLookAdd, ONLY: &
    LBLREC,                &
    LBPACK,                &
    DATA_TYPE,             &
    NADDR,                 &
    VarobsLookupNumGroups, &
    VarobsLookupNumObs,    &
    VarobsLookupNumData

USE GenMod_Control, ONLY: &
    nproc

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE OpsMod_GPSRO, ONLY: &
    GPSRO_TPD

USE OpsMod_ObsGroupInfo, ONLY: &
    ObsGroupGPSRO

USE OpsMod_ObsInfo, ONLY: &
    Ob_type

USE OpsMod_Radar, ONLY: &
    RadFamily

USE OpsMod_VarobsLib, ONLY: &
    varobs_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (Ob_type), INTENT(IN)        :: Observations
INTEGER, INTENT(IN)               :: NumVarObsPE(0:nproc - 1)
INTEGER, INTENT(IN)               :: LocalObsElement(0:nproc - 1)
INTEGER, INTENT(IN)               :: NumVarObsTotal
TYPE (varobs_type), INTENT(INOUT) :: Varobs

! Local declarations:
CHARACTER(len=*), PARAMETER       :: RoutineName = "Ops_SetupVarobsLookup"
INTEGER, PARAMETER                :: PackingIndicator64bit = 0
INTEGER, PARAMETER                :: RealDataIndicator = 1
INTEGER, PARAMETER                :: NumObsGroups = 1
INTEGER                           :: NumDataValues
INTEGER                           :: j
INTEGER                           :: Start

!-------------------------------------------------------------------------------
! 0. Tracing
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-------------------------------------------------------------------------------
! 1. Assign lookup header values.
!-------------------------------------------------------------------------------

! Initialise to zero
varobs % Lookup(:,:) = 0

NumDataValues = NumMeta + NumItem * NumObVariables

! Number of elements in data array
varobs % Lookup(LBLREC,1:nproc) = LocalObsElement(0:nproc - 1)
varobs % Lookup(LBLREC,nproc + 1) = 2 * NumVarObsTotal
varobs % Lookup(LBLREC,nproc + 2) = NumVarObsTotal
IF (RadFamily) THEN
  varobs % Lookup(LBLREC,nproc + 3) = NumVarObsTotal
ELSE IF (Observations % header % ObsGroup == ObsGroupGPSRO .AND. GPSRO_TPD) THEN
  varobs % Lookup(LBLREC,nproc + 3) = 2 * NumVarObsTotal
END IF

! Pack as 64 bit words
varobs % Lookup(LBPACK,:) = PackingIndicator64bit

! Real data indicator
varobs % Lookup(DATA_TYPE,:) = RealDataIndicator

Start = 1
DO j = 0, nproc - 1
  varobs % Lookup(NADDR,j + 1) = Start
  Start = Start + LocalObsElement(j)
END DO
varobs % Lookup(NADDR,nproc + 1) = Start

! Number of observation groups
varobs % Lookup(VarobsLookupNumGroups,:) = NumObsGroups

! Number of var observations
varobs % Lookup(VarobsLookupNumObs,1:nproc) = NumVarObsPE(0:nproc - 1)
varobs % Lookup(VarobsLookupNumObs,nproc + 1) = NumVarObsTotal

! Number of data values - this is ob. type specific. It is the number
! of data values that describes each observation, and is equal to
!                       7 + (number of weather variables * 3)
! The 7 comes from [lat, long, time, model ob type, callsign, level, flag]
! The 3 comes from [data value, error variance, PGE]
! Weather variable are t, rh, u, v, p etc.
! The required information has already been set up in the integer constants.

varobs % Lookup(VarobsLookupNumData,1:nproc) = NumDataValues
varobs % Lookup(VarobsLookupNumData,nproc + 1) = 2 ! lat and long of every ob

!-------------------------------------------------------------------------------
! 2. Tracing and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SetupVarobsLookup
