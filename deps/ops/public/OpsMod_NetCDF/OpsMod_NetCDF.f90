!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Module to contain code specific to NetCDF interfacing
!-------------------------------------------------------------------------------

MODULE OpsMod_NetCDF

USE OpsMod_Kinds, ONLY: &
  integer16,            &
  integer32,            &
  real64

IMPLICIT NONE

SAVE

! Miscellaneous missing data values.
INTEGER(kind=integer16), PARAMETER :: short_fill = 32767
INTEGER(kind=integer32), PARAMETER :: int_fill = 2147483647
REAL(kind=real64), PARAMETER       :: double_fill = 1.84467440737096d+19
REAL, PARAMETER                    :: NetCDF_MDI = 99999.0

END MODULE OpsMod_NetCDF
