!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Performs linear vertical interpolation on Cx columns. Taking values from model
! heights to observation height.  Sets extrapolated data to zero and sets flag
! to 1.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_VertInterpCxUV2ObHeight (Cx,           &
                                        Zob,          &
                                        DataCxHeader, &
                                        DataCx,       &
                                        DataOb)
USE GenMod_Core, ONLY: &
    gen_fail,          &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace
    
USE OpsMod_MiscTypes, ONLY: &
    ElementHeader_type,     &
    ZCodeLevelsA,           &
    ZCodeLevelsB
    
USE OpsMod_CXInfo, ONLY: &
    CX_type
    
IMPLICIT NONE
! Subroutine arguments:
TYPE (Cx_type), INTENT(IN)            :: Cx           ! Background column structure
REAL, INTENT(IN)                      :: Zob(:)       ! Heights of obs
TYPE (ElementHeader_type), INTENT(IN) :: DataCxHeader
REAL, INTENT(IN)                      :: DataCx(:,:)  ! Cx variable to be interpolated
REAL, INTENT(INOUT)                   :: DataOb(:)    ! Cx Data interpolated to ob heights.

! Local declarations:
CHARACTER(len=*), PARAMETER       :: RoutineName = "Ops_VertInterpCxUV2ObHeight"
INTEGER                           :: i               ! Loop indices
INTEGER                           :: j
INTEGER                           :: NCxCol          ! No. of Cx cols (= no. of obs)
INTEGER                           :: NCxLev          ! No. of Cx levels
INTEGER                           :: KOb             ! ZCx level below Zob level
REAL, POINTER                     :: ZCx(:,:)        ! Heights of model levels (for input field)
REAL                              :: rh
REAL                              :: rhp

! ----------------------------------------------------------------------
! Initialize
! ----------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

NCxCol = SIZE (DataCx, DIM = 1)
NCxLev = SIZE (DataCx, DIM = 2)

! choose the correct vertical coordinate
IF (DataCxHeader % Zcode ==  ZcodeLevelsA) THEN
  IF (Cx % Header % ZA % Present) THEN
    ZCx => CX % ZA(:,:)
  ELSE
    CALL gen_fail (RoutineName,           &
                   "CX % ZA not present")
  END IF
ELSE IF (DataCxHeader % Zcode ==  ZcodeLevelsB) THEN
  IF (Cx % Header % ZB % Present) THEN
    ZCX => CX % ZB(:,:)
  ELSE
    CALL gen_fail (RoutineName,           &
                   "CX % ZB not present")
  END IF
END IF

! -------------------------------------------------------------------------
! For each ob find k so that the ob height lies between Cx levels k and k+1
!  then perform linear interpolation
! -------------------------------------------------------------------------

DO i = 1, NCxCol ! Loop through obs

  ! Find ZCx levels above & below Zob level
  
  KOb = 0
  
  DO j = 1, NCxLev - 1
    IF (ZCx(i,j + 1) >= Zob(i) .AND. Zob(i) >= ZCx(i,j)) KOb = j
  END DO
  
  ! Perform linear interpolation
  
  IF (KOb /= 0) THEN
  
    rh = ZCx(i,KOb)
    rhp = ZCx(i,KOb + 1)
    DataOb(i) = ((ZOb(i) - rh) * DataCx(i,KOb + 1) + (rhp - ZOb(i)) * DataCx(i,KOb)) &
                              / (rhp - rh)

  ELSE
    DataOb(i) = 0.0
  END IF

END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_VertInterpCxUV2ObHeight
