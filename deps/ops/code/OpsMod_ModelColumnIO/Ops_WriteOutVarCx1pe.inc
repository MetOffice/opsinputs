!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Iterates over obs groups and writes a .cx file as required.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_WriteOutVarCx1pe (Obs,          &  ! ObsFile
                                 CXArray,      &  ! Array of Cx Structures
                                 CxLevels,     &
                                 DumpUMHeader)

USE GenMod_Control, ONLY: &
    mype

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_ModelIO, ONLY: &
    UM_header_type

USE OpsMod_CXInfo, ONLY: &
    CX_type

USE OpsMod_ObsInfo, ONLY: &
    OB_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (Ob_type), INTENT(IN)           :: Obs(:)
TYPE (CX_type), INTENT(INOUT)        :: CXArray(:)
INTEGER, INTENT(IN)                  :: CxLevels(:)
TYPE (UM_header_type), INTENT(INOUT) :: DumpUMheader

! Local declarations:
TYPE (UM_header_type),  ALLOCATABLE  :: Cx_UMhdr(:)  ! Cx UM header
INTEGER                              :: ObType
CHARACTER(len=*), PARAMETER          :: RoutineName = "Ops_WriteOutVarCx1pe"
INTEGER                              :: MaxSurfaceIndex
INTEGER                              :: MaxUairIndex

IF (UseTrace) CALL gen_trace_entry (RoutineName)

ALLOCATE (Cx_UMhdr(SIZE (Obs)))

ObsTypes: DO  ObType = 1, SIZE (Obs)

  IF (Obs(ObType) % Header % NumObsTotal > 0) THEN
    CALL Ops_MaxCXIndexes (CxArray(ObType), &
                           MaxSurfaceIndex, &
                           MaxUairIndex)

    CALL Ops_OpenModelColumn (Obs(ObType),                                                       & ! in
                              DumpUMheader,                                                      & ! in
                              Cx_UMhdr(ObType),                                                  & ! out
                              MaxSurfaceIndex,                                                   &
                              MaxUairIndex,                                                      &
                              batches = SIZE (Obs(ObType) % header % ObsPerBatchPerPe, DIM = 1))

    !  Set up the Cx UM header on all pe's
    CALL Ops_SetupCxHeader (CxArray(ObType),  &
                            Cx_UMhdr(ObType), &
                            CxLevels(ObType), &
                            MaxSurfaceIndex,  &
                            MaxUairIndex)

    !  Write model column to Cx file and deallocate Cx structure(s)
    CALL Ops_WriteModelColumn1pe (Obs(ObType) % header % ObsPerBatchPerPe(:,0), & ! in Number of Cx columns
                                  DumpUMheader,                                 & ! in Model UM header
                                  CxArray(ObType),                              & ! inout Cx structure
                                  Cx_UMhdr(ObType))                               ! inout Cx UM header

    IF (mype == 0) THEN
      CALL Ops_CloseModelColumn (Obs(ObType) % Header % ObsGroupName, &
                                 Cx_UMhdr(ObType))
    END IF
  END IF

END DO ObsTypes

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_WriteOutVarCx1pe
