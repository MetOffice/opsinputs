!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Iterates over obs groups and writes a .cx file as required.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_WriteOutVarCx (Obs,          &  ! ObsFile
                              CXArray,      &  ! Array of Cx Structures
                              NumObsExt,    &  ! array of numobsextracted from Varobsfile
                              CxLevels,     &
                              DumpUMHeader, &
                              mask)

USE GenMod_Control, ONLY: &
    mype

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    MaxFileNameLen,    &
    UseTrace

USE GenMod_ModelIO, ONLY: &
    UM_header_type

USE OpsMod_Control, ONLY: &
    read_cx_twice

USE OpsMod_CXInfo, ONLY: &
    CX_type

USE OpsMod_EnvUtils, ONLY: &
    ops_env_is_true,       &
    ops_get_env

USE OpsMod_IOUtils, ONLY: &
    Ops_TouchFile

USE OpsMod_ObsGroupInfo, ONLY: &
    ObsGroupSonde

USE OpsMod_ObsInfo, ONLY: &
    OB_type

USE OpsMod_ObTypeUtils, ONLY: &
    Ops_UnRotateWinds

USE OpsMod_SondeDrift, ONLY: &
    SondeDrift

IMPLICIT NONE

! Subroutine arguments:
TYPE (Ob_type), INTENT(IN)           :: Obs
TYPE (CX_type), INTENT(INOUT)        :: CXArray
INTEGER, INTENT(IN)                  :: NumObsExt(:)
INTEGER, INTENT(IN)                  :: CxLevels
TYPE (UM_header_type), INTENT(INOUT) :: DumpUMheader
LOGICAL, INTENT(IN)                  :: mask(:)

! Local declarations:
TYPE (UM_header_type)                :: Cx_UMhdr  ! Cx UM header
CHARACTER(len=*), PARAMETER          :: RoutineName = "Ops_WriteOutVarCx"
INTEGER                              :: MaxSurfaceIndex
INTEGER                              :: MaxUairIndex
CHARACTER(len=MaxFileNameLen)        :: Filename

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF ((CXArray % header % u10 % present .OR. CXArray % header % u % present) .AND. &
    .NOT. read_cx_twice .AND. CXArray % header % Rotated) THEN
  CALL Ops_UnRotateWinds (CXArray)
END IF

IF (.NOT. read_cx_twice .AND. Obs % header % ObsGroup == ObsGroupSonde .AND. SondeDrift > 0) THEN
  CXArray % theta(:,:) = CXArray % theta_unslanted(:,:)
  CXArray % u(:,:) = CXArray % u_unslanted(:,:)
  CXArray % v(:,:) = CXArray % v_unslanted(:,:)
  CXArray % q(:,:) = CXArray % q_unslanted(:,:)
  CXArray % PlevelsA(:,:) = CXArray % PlevelsA_unslanted(:,:)
END IF

IF (Obs % Header % NumObsTotal > 0) THEN
  CALL Ops_MaxCXIndexes (CxArray,         &
                         MaxSurfaceIndex, &
                         MaxUairIndex)

  CALL Ops_OpenModelColumn (Obs,             & ! in
                            DumpUMheader,    & ! in
                            Cx_UMhdr,        & ! out
                            MaxSurfaceIndex, &
                            MaxUairIndex)

  !  Set up the Cx UM header on all pe's
  CALL Ops_SetupCxHeader (CxArray,         &
                          Cx_UMhdr,        &
                          CxLevels,        &
                          MaxSurfaceIndex, &
                          MaxUairIndex)

  !  Write model column to Cx file and deallocate Cx structure(s)
  CALL Ops_WriteModelColumn (NumObsExt(:), & ! in Number of Cx columns
                             DumpUMheader, & ! in Model UM header
                             CxArray,      & ! inout Cx structure
                             Cx_UMhdr,     &
                             mask = mask)    ! inout Cx UM header

  IF (mype == 0) THEN
    CALL Ops_CloseModelColumn (Obs % Header % ObsGroupName, &
                               Cx_UMhdr)
  END IF
END IF

IF (mype == 0 .AND. ops_env_is_true ("OPS_CREATE_CX_DONE_FILE")) THEN
  CALL ops_get_env ("OPS_CX_DIR_LIST", &
                    Filename)
  Filename = TRIM (Filename) // "/" // TRIM (Obs % Header % ObsGroupName) // ".cx.done"
  CALL Ops_TouchFile (Filename)
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_WriteOutVarCx
