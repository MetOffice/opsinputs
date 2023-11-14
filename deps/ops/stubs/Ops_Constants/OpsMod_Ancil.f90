!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Ancillary file codes used in OPS.
!-------------------------------------------------------------------------------

MODULE OpsMod_Ancil

IMPLICIT NONE

SAVE

! Public declarations:

INTEGER, PARAMETER :: AncilItem_SST = 24
INTEGER, PARAMETER :: AncilItem_SeaIce = 31
INTEGER, PARAMETER :: AncilCode_SeaHeight = 34
INTEGER, PARAMETER :: AncilCode_WaveHeight = 44
INTEGER, PARAMETER :: AncilCode_theta_ocean = 101
INTEGER, PARAMETER :: AncilCode_salt = 102
INTEGER, PARAMETER :: AncilCode_MeanSeaHeight = 184
INTEGER, PARAMETER :: AncilCode_SeaSrfcHeight = 285
INTEGER, PARAMETER :: AncilCode_SeaIce = 146

INTEGER, PARAMETER :: AncilCode_LCHLOb_Err = 99986
INTEGER, PARAMETER :: AncilCode_LKD490Ob_Err = 99987
INTEGER, PARAMETER :: AncilCode_CHLOb_Err = 99988
INTEGER, PARAMETER :: AncilCode_KD490Ob_Err = 99989

INTEGER, PARAMETER :: AncilCode_LCHL = 99996
INTEGER, PARAMETER :: AncilCode_LKD490 = 99997
INTEGER, PARAMETER :: AncilCode_CHL = 99998
INTEGER, PARAMETER :: AncilCode_KD490 = 99999

INTEGER, PARAMETER :: AncilCode_SSTMes_Var = 35411
INTEGER, PARAMETER :: AncilCode_SSTMes_HCor = 35412
INTEGER, PARAMETER :: AncilCode_SSTSyn_Var = 35413
INTEGER, PARAMETER :: AncilCode_SSTSyn_HCor = 35414
INTEGER, PARAMETER :: AncilCode_SSTOb_Err = 35415
INTEGER, PARAMETER :: AncilCode_SSHMes_Var = 35421
INTEGER, PARAMETER :: AncilCode_SSHMes_HCor = 35422
INTEGER, PARAMETER :: AncilCode_SSHSyn_Var = 35423
INTEGER, PARAMETER :: AncilCode_SSHSyn_HCor = 35424
INTEGER, PARAMETER :: AncilCode_SSHOb_Err = 35425
INTEGER, PARAMETER :: AncilCode_TempMes_Var = 35431
INTEGER, PARAMETER :: AncilCode_TempMes_HCor = 35432
INTEGER, PARAMETER :: AncilCode_TempSyn_Var = 35433
INTEGER, PARAMETER :: AncilCode_TempSyn_HCor = 35434
INTEGER, PARAMETER :: AncilCode_TempOb_Err = 35435
INTEGER, PARAMETER :: AncilCode_SalMes_Var = 35441
INTEGER, PARAMETER :: AncilCode_SalMes_HCor = 35442
INTEGER, PARAMETER :: AncilCode_SalSyn_Var = 35443
INTEGER, PARAMETER :: AncilCode_SalSyn_HCor = 35444
INTEGER, PARAMETER :: AncilCode_SalOb_Err = 35445
INTEGER, PARAMETER :: AncilCode_SeaIceMes_Var = 35511
INTEGER, PARAMETER :: AncilCode_SeaIceMes_HCor = 35512
INTEGER, PARAMETER :: AncilCode_SeaIceSyn_Var = 35513
INTEGER, PARAMETER :: AncilCode_SeaIceSyn_HCor = 35514
INTEGER, PARAMETER :: AncilCode_SeaIceOb_Err = 35515

END MODULE OpsMod_Ancil
