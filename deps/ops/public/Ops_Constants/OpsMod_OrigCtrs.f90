!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Module to store originating centres.
!-------------------------------------------------------------------------------

MODULE OpsMod_OrigCtrs

IMPLICIT NONE

SAVE

! Public declarations:

!-------------------------------------------------------------------------------
! 1. Define constants
!-------------------------------------------------------------------------------

INTEGER, PARAMETER  :: OrigCtr_BOM = 1
INTEGER, PARAMETER  :: OrigCtr_CIMSS_legacy = 2 ! for backwards compatibility only
INTEGER, PARAMETER  :: OrigCtr_UK_legacy = 7    ! for backwards compatibility only
INTEGER, PARAMETER  :: OrigCtr_UK = 74
INTEGER, PARAMETER  :: OrigCtr_KMA = 40
INTEGER, PARAMETER  :: OrigCtr_IMD = 28
INTEGER, PARAMETER  :: OrigCtr_CMA = 39
INTEGER, PARAMETER  :: OrigCtr_JMA = 34
INTEGER, PARAMETER  :: OrigCtr_NESDIS = 160
INTEGER, PARAMETER  :: OrigCtr_NASA = 173
INTEGER, PARAMETER  :: OrigCtr_CIMSS = 176
INTEGER, PARAMETER  :: OrigCtr_EUMETSAT = 254

END MODULE OpsMod_OrigCtrs
