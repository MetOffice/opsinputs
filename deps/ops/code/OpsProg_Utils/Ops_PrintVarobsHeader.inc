!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Prints the contents of a varob header.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_PrintVarobsHeader (varobs,   &
                                  out_unit)

USE GenMod_CLookAdd, ONLY: &
  DATA_TYPE,               &
  LBLREC,                  &
  LBPACK,                  &
  NADDR,                   &
  VarobsLookupNumData,     &
  VarobsLookupNumGroups,   &
  VarobsLookupNumObs

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE GenMod_UMHeaderConstants, ONLY: &
  FH_ColDepCSize1,                  &
  FH_LookupSize2,                   &
  IC_LenObCol,                      &
  IC_NumCxPItem,                    &
  IC_NumCxPItemMax,                 &
  IC_NumCxSFVar,                    &
  IC_NumCxSFVarMax,                 &
  IC_NumCxUaVar,                    &
  IC_NumCxUaVarMax,                 &
  IC_NumItem,                       &
  IC_NumItemMax,                    &
  IC_NumMeta,                       &
  IC_NumMetaMax,                    &
  IC_NumObLev,                      &
  IC_NumObPItem,                    &
  IC_NumObPItemMax,                 &
  IC_NumObsTotal,                   &
  IC_NumObVar,                      &
  IC_NumObVarMax,                   &
  IC_NumVarBatches,                 &
  IC_ObsGroup,                      &
  IC_ObsRelease,                    &
  IC_PLevels,                       &
  IC_TorTheta,                      &
  IC_TorTheta_T,                    &
  IC_XLen,                          &
  IC_WetLevels,                     &
  IC_YLen,                          &
  RC_FirstLat,                      &
  RC_FirstLong,                     &
  RC_LatSpacing,                    &
  RC_LongSpacing,                   &
  RC_PoleLat,                       &
  RC_PoleLong

USE OpsMod_UtilsProgs, ONLY: &
  Ops_PrintFixedHeader

USE OpsMod_Varfields, ONLY: &
  OpsFn_VarFieldNumToName

USE OpsMod_VarobsLib, ONLY: &
  varobs_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (varobs_type), INTENT(IN) :: varobs
INTEGER, INTENT(IN)            :: out_unit

! Local declarations:
INTEGER                        :: i
INTEGER                        :: j
INTEGER                        :: num_varfields
CHARACTER(len=56)              :: tmp_string

!--------------------------------------------------
! 1. Fixed length header
!--------------------------------------------------

CALL Ops_PrintFixedHeader (varobs % FixHd, &
                           out_unit)

!--------------------------------------------------
! 2. Integer Constants
!--------------------------------------------------

WRITE (out_unit, '(A)')  "Integer Constants:"
WRITE (out_unit, '(A)')

IF (varobs % IntC(IC_TorTheta) == IC_TorTheta_T) THEN
  WRITE (out_unit, '(2A)') "Theta or Temp? = ", "Temperature"
ELSE
  WRITE (out_unit, '(2A)') "Theta or Temp? = ", "Theta"
END IF

WRITE (out_unit, '(A,I0)') "Number EW points = ", varobs % IntC(IC_XLen)
WRITE (out_unit, '(A,I0)') "Number NS points = ", varobs % IntC(IC_YLen)
WRITE (out_unit, '(A,I0)') "Number pressure levels = ", varobs % IntC(IC_PLevels)
WRITE (out_unit, '(A,I0)') "Number of wet levels = ", varobs % IntC(IC_WetLevels)
WRITE (out_unit, '(A,I0)') "Total number of obs = ", varobs % IntC(IC_NumObsTotal)
WRITE (out_unit, '(A,I0)') "Length of observation column = ", varobs % IntC(IC_LenObCol)
WRITE (out_unit, '(A,I0)') "Observation group = ", varobs % IntC(IC_ObsGroup)
WRITE (out_unit, '(A,I0)') "Version Number = ", varobs % IntC(IC_ObsRelease)
WRITE (out_unit, '(A,I0)') "Maximum number of meta data items = ", varobs % IntC(IC_NumMetaMax)
WRITE (out_unit, '(A,I0)') "Maximum number of data items = ", varobs % IntC(IC_NumItemMax)
WRITE (out_unit, '(A,I0)') "Maximum number of variables in ob column = ", varobs % IntC(IC_NumObVarMax)
WRITE (out_unit, '(A,I0)') "Maximum number of pressure items in ob column = ", varobs % IntC(IC_NumObPItemMax)
WRITE (out_unit, '(A,I0)') "Maximum number of pressure items in cx column = ", varobs % IntC(IC_NumCxPItemMax)
WRITE (out_unit, '(A,I0)') "Maximum number of surface variables = ", varobs % IntC(IC_NumCxSFVarMax)
WRITE (out_unit, '(A,I0)') "Maximum number of upper air variables = ", varobs % IntC(IC_NumCxUaVarMax)
WRITE (out_unit, '(A,I0)') "Number of meta data items = ", varobs % IntC(IC_NumMeta)
WRITE (out_unit, '(A,I0)') "Number of data items = ", varobs % IntC(IC_NumItem)
WRITE (out_unit, '(A,I0)') "Number of variables in ob column = ", varobs % IntC(IC_NumObVar)
WRITE (out_unit, '(A,I0)') "Number of pressure items in ob column = ", varobs % IntC(IC_NumObPItem)
WRITE (out_unit, '(A,I0)') "Number of pressure items in cx column = ", varobs % IntC(IC_NumCxPItem)
WRITE (out_unit, '(A,I0)') "Number of surface variables = ", varobs % IntC(IC_NumCxSfVar)
WRITE (out_unit, '(A,I0)') "Number of upper air variables = ", varobs % IntC(IC_NumCxUaVar)
WRITE (out_unit, '(A,I0)') "Number of levels = ", varobs % IntC(IC_NumObLev)
WRITE (out_unit, '(A,I0)') "Number of batches of data = ", varobs % IntC(IC_NumVarBatches)

!--------------------------------------------------
! 3. Real Constants
!--------------------------------------------------

WRITE (out_unit, '(A)')
WRITE (out_unit, '(A)') "Real Constants:"
WRITE (out_unit, '(A)')

WRITE (out_unit, '(A21,F15.2)') "Longitude spacing = ", varobs % RealC(RC_LongSpacing)
WRITE (out_unit, '(A21,F15.2)') "Latitude spacing = ", varobs % RealC(RC_LatSpacing)
WRITE (out_unit, '(A21,F15.2)') "First latitude = ", varobs % RealC(RC_FirstLat)
WRITE (out_unit, '(A21,F15.2)') "First longitude = ", varobs % RealC(RC_FirstLong)
WRITE (out_unit, '(A21,F15.2)') "Pole latitude = ", varobs % RealC(RC_PoleLat)
WRITE (out_unit, '(A21,F15.2)') "Pole longitude = ", varobs % RealC(RC_PoleLong)

!--------------------------------------------------
! 4. Column Dependent Constants
!--------------------------------------------------

WRITE (out_unit, '(A)')
WRITE (out_unit, '(A)') "Column Dependent Constants:"
WRITE (out_unit, '(A)')

WRITE (out_unit, '(A20,A7)') "Varfield", " Levels"
num_varfields = COUNT (varobs % ColDepC(3 * varobs % FixHd(FH_ColDepCSize1) + 1: &
                                            4 * varobs % FixHd(FH_ColDepCSize1)) /= IMDI)
DO j = 1, num_varfields
  DO i = 1, varobs % FixHD(FH_ColDepCSize1)
    IF (varobs % ColDepC(3 * varobs % FixHd(FH_ColDepCSize1) + i) == j) THEN
      WRITE (tmp_string, '(A,I0,A)') OpsFn_VarFieldNumToName(i) // "(", i, ")"
      WRITE (out_unit, '(A55)', ADVANCE = "NO") tmp_string
      WRITE (out_unit, '(I6)') NINT (varobs % ColDepC(7 * varobs % FixHd(FH_ColDepCSize1) + i))
    END IF
  END DO
END DO
WRITE (out_unit, '(A)')

!--------------------------------------------------
! 5. Level Dependent Constants
!--------------------------------------------------

WRITE (out_unit, '(A)')
WRITE (out_unit, '(A)') "Level Dependent Constants:"
WRITE (out_unit, '(A)')

WRITE (out_unit, '(A,I0)') "Observation level type = ", NINT (varobs % LevDepC(1))
WRITE (out_unit, '(A,I0)') "Number of levels = ", NINT (varobs % LevDepC(2))
IF (NINT (varobs % LevDepC(2)) > 1) THEN
  DO i = 1, NINT (varobs % LevDepC(2))
    IF (NINT (varobs % LevDepC(1)) /= 2) THEN
      WRITE (out_unit, '(A,I0,A,F15.2)') "Pressure on level ", i, " is ", varobs % LevDepC(i + 2)
    END IF
  END DO
END IF

!--------------------------------------------------
! 6. Lookup
!--------------------------------------------------

WRITE (out_unit, '(A)')
WRITE (out_unit, '(A)') "Lookups:"
WRITE (out_unit, '(A)')

DO i = 1, varobs % FixHd(FH_LookupSize2)
  WRITE (out_unit, '(A,I0)') "Lookup for field ", i
  WRITE (out_unit, '(A,I0)') "Number of elements = ", varobs % Lookup(LBLREC,i)
  WRITE (out_unit, '(A,I0)') "Pack code = ", varobs % Lookup(LBPACK,i)
  WRITE (out_unit, '(A,I0)') "Data type = ", varobs % Lookup(DATA_TYPE,i)
  WRITE (out_unit, '(A,I0)') "Start of batch = ", varobs % Lookup(NADDR,i)
  WRITE (out_unit, '(A,I0)') "Number of observation groups = ", varobs % Lookup(VarobsLookupNumGroups,i)
  WRITE (out_unit, '(A,I0)') "Number of observations = ", varobs % Lookup(VarobsLookupNumObs,i)
  WRITE (out_unit, '(A,I0)') "Number of data values = ", varobs % Lookup(VarobsLookupNumData,i)
END DO

END SUBROUTINE Ops_PrintVarobsHeader
