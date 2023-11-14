!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Set default BgerrFields for an obsgroup.
!
! Inputs:
!     Obsgroup
!
! Outputs:
!     Default BgerrFields
!-------------------------------------------------------------------------------

SUBROUTINE Ops_GetDefaultBgerrFields (ObsGroup,    &
                                      BgerrFields)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE OpsMod_Ancil

USE OpsMod_Control, ONLY: &
  ModelType,              &
  ModelType_SST

USE OpsMod_ObsGroupInfo

USE OpsMod_Stash

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)         :: ObsGroup
INTEGER, INTENT(OUT)        :: BgerrFields(:)

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_GetDefaultBgerrFields"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

BgerrFields = IMDI

SELECT CASE (ObsGroup)
  CASE (ObsGroupAircraft)
    BgerrFields(1:4) = (/StashCode_pmsl,StashCode_u_p_B_grid,StashCode_t_p, &
                         StashCode_rh_p/)
  CASE (ObsGroupAltim)
    BgerrFields(1:7) = (/AncilCode_MeanSeaHeight,AncilCode_SeaSrfcHeight, &
                         AncilCode_SSHMes_Var,AncilCode_SSHMes_HCor,      &
                         AncilCode_SSHSyn_Var,AncilCode_SSHSyn_HCor,      &
                         AncilCode_SSHOb_Err/)
  CASE (ObsGroupHLOSWIND)
    BgerrFields(1:1) = (/StashCode_u_p_B_grid/)
  CASE (ObsGroupSatSST)
    BgerrFields(1:1) = (/AncilItem_SST/)
  CASE (ObsGroupOcean)
    BgerrFields(1:12) = (/AncilCode_theta_ocean,AncilCode_salt,         &
                          AncilCode_TempMes_Var,AncilCode_TempMes_HCor, &
                          AncilCode_TempSyn_Var,AncilCode_TempSyn_HCor, &
                          AncilCode_TempOb_Err, AncilCode_SalMes_Var,   &
                          AncilCode_SalMes_HCor,AncilCode_SalSyn_Var,   &
                          AncilCode_SalSyn_HCor,AncilCode_SalOb_Err/)
  CASE (ObsGroupOceanColour)
    BgerrFields(1:4) = (/AncilCode_CHL,AncilCode_KD490,              &
                         AncilCode_CHLOb_Err,AncilCode_KD490Ob_Err/)
  CASE (ObsGroupOceanWinds)
    BgerrFields(1:2) = (/StashCode_pmsl,StashCode_u10_B_grid/)
  CASE (ObsGroupSatwind)
    BgerrFields(1:3) = (/StashCode_pmsl,StashCode_u_p_B_grid,StashCode_t_p/)
  CASE (ObsGroupScatwind)
    BgerrFields(1:2) = (/StashCode_pmsl,StashCode_u10_B_grid/)
  CASE (ObsGroupSeaIce)
    BgerrFields(1:6) = (/AncilCode_SeaIce,AncilCode_SeaIceMes_Var,         &
                         AncilCode_SeaIceMes_HCor,AncilCode_SeaIceSyn_Var, &
                         AncilCode_SeaIceSyn_HCor,AncilCode_SeaIceOb_Err/)
  CASE (ObsGroupSonde)
    BgerrFields(1:7) = (/StashCode_pmsl,StashCode_u10_B_grid,StashCode_t2, &
                         StashCode_rh2,StashCode_u_p_B_grid,StashCode_t_p, &
                         StashCode_rh_p/)
  CASE (ObsGroupSurface)
    IF (ModelType /= ModelType_SST) THEN
      BgerrFields(1:8) = (/StashCode_pmsl,StashCode_u10_B_grid,StashCode_t2, &
                           StashCode_rh2,StashCode_vis,StashCode_u_p_B_grid, &
                           StashCode_t_p,StashCode_rh_p/)
    END IF
END SELECT

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_GetDefaultBgerrFields
