!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Prints the contents of a CX header.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_PrintCXHeader (Header,   &
                              out_unit)

USE GenMod_CLookAdd, ONLY: &
  DATA_TYPE,               &
  ITEM_CODE,               &
  LBDAY,                   &
  LBDAYD,                  &
  LBEXT,                   &
  LBFT,                    &
  LBLREC,                  &
  LBNPT,                   &
  LBNREC,                  &
  LBPACK,                  &
  LBROW,                   &
  LBTIM,                   &
  LBYR,                    &
  LBYRD

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE GenMod_ModelIO, ONLY: &
  UM_Header_Type

USE GenMod_UMHeaderConstants, ONLY: &
  FH_ColDepCSize1,                  &
  FH_LevDepCSize1,                  &
  IC_LenCXCol,                      &
  IC_NumCxLev,                      &
  IC_NumCxSFVar,                    &
  IC_NumCxSFVarMax,                 &
  IC_NumCxUaVar,                    &
  IC_NumCxUaVarMax,                 &
  IC_NumObsTotal,                   &
  IC_NumVarBatches,                 &
  IC_ObsRelease,                    &
  RC_FirstLat,                      &
  RC_FirstLong,                     &
  RC_LatSpacing,                    &
  RC_LongSpacing,                   &
  RC_PoleLat,                       &
  RC_PoleLong,                      &
  RC_z_ModelTop

USE OpsMod_UtilsProgs, ONLY: &
  Ops_PrintFixedHeader

IMPLICIT NONE

! Subroutine arguments:
TYPE(UM_Header_Type), INTENT(IN) :: Header
INTEGER, INTENT(IN)              :: out_unit

! Local declarations:
INTEGER                          :: i

!--------------------------------------------------
! 1. Fixed length header
!--------------------------------------------------

CALL Ops_PrintFixedHeader (Header % FixHd, &
                           out_unit)

!--------------------------------------------------
! 2. Integer Constants
!--------------------------------------------------

WRITE (out_unit, '(A)')  "Integer Constants:"
WRITE (out_unit, '(A)')

WRITE (out_unit, '(A,I0)') "Total number of obs = ", Header % IntC(IC_NumObsTotal)
WRITE (out_unit, '(A,I0)') "Length of CX column = ", Header % IntC(IC_LenCXCol)
WRITE (out_unit, '(A,I0)') "Version Number = ", Header % IntC(IC_ObsRelease)
WRITE (out_unit, '(A,I0)') "Maximum number of surface variables = ", Header % IntC(IC_NumCxSFVarMax)
WRITE (out_unit, '(A,I0)') "Maximum number of upper air variables = ", Header % IntC(IC_NumCxUaVarMax)
WRITE (out_unit, '(A,I0)') "Number of surface variables = ", Header % IntC(IC_NumCxSfVar)
WRITE (out_unit, '(A,I0)') "Number of upper air variables = ", Header % IntC(IC_NumCxUaVar)
WRITE (out_unit, '(A,I0)') "Number of levels = ", Header % IntC(IC_NumCxLev)
WRITE (out_unit, '(A,I0)') "Number of batches of data = ", Header % IntC(IC_NumVarBatches)
WRITE (out_unit, '(A)')

!--------------------------------------------------
! 3. Real Constants
!--------------------------------------------------

WRITE (out_unit, '(A)')  "Real Constants:"
WRITE (out_unit, '(A)')

WRITE (out_unit, '(A,F18.4)') "EW grid spacing (degrees) = ", Header % RealC(RC_LongSpacing)
WRITE (out_unit, '(A,F18.4)') "NS grid spacing (degrees) = ", Header % RealC(RC_LatSpacing)
WRITE (out_unit, '(A,F18.4)') "First latitude = ", Header % RealC(RC_FirstLat)
WRITE (out_unit, '(A,F18.4)') "First longitude = ", Header % RealC(RC_FirstLong)
WRITE (out_unit, '(A,F18.4)') "North pole latitude (degrees) = ", Header % RealC(RC_PoleLat)
WRITE (out_unit, '(A,F18.4)') "North pole longitude (degrees) = ", Header % RealC(RC_PoleLong)
WRITE (out_unit, '(A,F18.4)') "Height of model top = ", Header % RealC(RC_z_ModelTop)
WRITE (out_unit, '(A,F18.4)') "Pressure of model top = ", Header % RealC(17)
WRITE (out_unit, '(A)')

!--------------------------------------------------
! 4. Level Dependent Constants
!--------------------------------------------------

WRITE (out_unit, '(A)')  "Level Dependent Constants:"
WRITE (out_unit, '(A)')

IF (NINT (Header % LevDepC(Header % FixHd(FH_LevDepCSize1) + 1)) == 14) THEN !Ocean
  WRITE (out_unit, '(A,I0,A)')      "Ocean levels (", NINT (Header % LevDepC(Header % FixHd(FH_LevDepCSize1) + 2)), "): "
  WRITE (out_unit, '(500(F6.2))')   Header % LevDepC(Header % FixHd(FH_LevDepCSize1) + 3:2 * Header % FixHd(FH_LevDepCSize1))
ELSE
  WRITE (out_unit, '(A,I0,A)')      "Eta-Theta levels (", NINT (Header % LevDepC(Header % FixHd(FH_LevDepCSize1) + 2)), "): "
  WRITE (out_unit, '(500(F6.2))')   Header % LevDepC(Header % FixHd(FH_LevDepCSize1) + 3:2 * Header % FixHd(FH_LevDepCSize1))
  WRITE (out_unit, '(A,I0,A)')      "Eta-Rho levels (", NINT (Header % LevDepC(2 * Header % FixHd(FH_LevDepCSize1) + 2)), "): "
  WRITE (out_unit, '(500(F6.2))')   Header % LevDepC(2 * Header % FixHd(FH_LevDepCSize1) + 3:3 * Header % FixHd(FH_LevDepCSize1))
  WRITE (out_unit, '(A)')
END IF

!--------------------------------------------------
! 5. Column Dependent Constants
!--------------------------------------------------

WRITE (out_unit, '(A)')  "Column Dependent Constants:"
WRITE (out_unit, '(A)')

WRITE (out_unit, '(A,2I8)') "Pressure levels indexes: ", NINT (Header % ColDepC(4 * Header % FixHd(FH_ColDepCSize1) + 1)), &
                                                           NINT (Header % ColDepC(4 * Header % FixHd(FH_ColDepCSize1) + 2))
WRITE (out_unit, '(A)')  "Surface variables present:"
DO i = 5 * Header % FixHd(FH_ColDepCSize1) + 1,6 * Header % FixHd(FH_ColDepCSize1)
  IF (NINT (Header % ColDepC(i)) /= IMDI) THEN
    WRITE (out_unit, '(A,I5,A,I5)') " Variable ", MOD (i - 1,Header % FixHd(FH_ColDepCSize1)) + 1, &
                                            " present at batch position ", NINT (Header % ColDepC(i))
  END IF
END DO
WRITE (out_unit, '(A)')  "Upper-air variables present:"
DO i = 6 * Header % FixHd(FH_ColDepCSize1) + 1,7 * Header % FixHd(FH_ColDepCSize1)
  IF (NINT (Header % ColDepC(i)) /= IMDI) THEN
    WRITE (out_unit, '(4(A,I5))') " Variable ", MOD (i - 1,Header % FixHd(FH_ColDepCSize1)) + 1, &
                         " present at batch position ", Header % IntC(IC_NumCxSfVar) + NINT (Header % ColDepC(i)), " to ", &
                         Header % IntC(IC_NumCxSfVar) + NINT (Header % ColDepC(i)) + &
                         NINT (Header % ColDepC(i + Header % FixHd(FH_ColDepCSize1))) - 1, &
                         " Number of levels = ",  NINT (Header % ColDepC(i + Header % FixHd(FH_ColDepCSize1)))
  END IF
END DO
WRITE (out_unit, '(A)')

!--------------------------------------------------
! 6. Lookup
!--------------------------------------------------

WRITE (out_unit, '(A)')  "Lookup:"
WRITE (out_unit, '(A)')

DO i = 1,Header % IntC(IC_NumVarBatches)
  WRITE (out_unit,'(A,I0,A)') "For batch number ", i, ":"
  WRITE (out_unit,'(A,I0)') "Data type indicator = ", Header % Lookup(DATA_TYPE,i)
  WRITE (out_unit,'(A,I0)') "Packing Code = ", Header % Lookup(LBPACK,i)
  WRITE (out_unit,'(A,6I5)') "Validity Time = ", Header % Lookup(LBYR:LBDAY,i)
  WRITE (out_unit,'(A,6I5)') "Data Time = ", Header % Lookup(LBYRD:LBDAYD,i)
  WRITE (out_unit,'(A,I0)') "Time Indicator = ", Header % Lookup(LBTIM,i)
  WRITE (out_unit,'(A,I0)') "Forecast Period = ", Header % Lookup(LBFT,i)
  WRITE (out_unit,'(A,I0)') "Length of Batch = ", Header % Lookup(LBLREC,i)
  WRITE (out_unit,'(A,I0)') "Length of Row = ", Header % Lookup(LBROW,i)
  WRITE (out_unit,'(A,I0)') "Number of Points per Row = ", Header % Lookup(LBNPT,i)
  WRITE (out_unit,'(A,I0)') "Length of Extra Data = ", Header % Lookup(LBEXT,i)
  WRITE (out_unit,'(A,I0)') "Number of Records = ", Header % Lookup(LBNREC,i)
  WRITE (out_unit,'(A,I0)') "Stash Code = ", Header % Lookup(ITEM_CODE,i)
  WRITE (out_unit,'(A)')
END DO

END SUBROUTINE Ops_PrintCXHeader
