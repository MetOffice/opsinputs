!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Performs linear, cubic Lagrange or quintic lagrange vertical interpolations on
! 3D fields. The horizontal location of points must be the same for both the
! input and the output data arrays.
!
! N.B. This routine should NOT be used for vertically interpolating density
!      (or pressure?).
!
! Method:
!     This version is based on the standard VarMod_VerticalInterp which in
!     turn was based on the simplifed version of the full 3D interpolation
!     routine written by Mark Mawson and described in F.R. Division working
!     paper No 162.
!
!     The standard VarMod_VerticalInterp has no sophisticated selection of the
!     bracketing output levels, so attempts were made to take advantage of the
!     monotonically increasing level structure to pre-select levels. This
!     provides big performance benefits on the scalar HP, but is inherently hard
!     to vectorise, so actually runs slower on the Cray. This version
!     should only be used on scalar systems.
!
!     A further improvement could be achieved by writing a routine just for
!     the cases where the level structure is common for all data points.
!     This has yet to be done.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_VerticalInterp (Scheme,        &
                               rIn,           &
                               rOut,          &
                               DataIn,        &
                               DataOut,       &
                               NoExtrapolate, &
                               RMDICheck)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    gen_warn,          &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)           :: Scheme         ! a code saying which order of scheme to use
REAL, INTENT(IN)              :: rIn(:,:,:)     ! Vertical co-ordinate on input
REAL, INTENT(IN)              :: rOut(:,:,:)    ! Vertical co-ordinate on output
REAL, INTENT(IN)              :: DataIn(:,:,:)  ! Data to be interpolated.
REAL, INTENT(OUT)             :: DataOut(:,:,:) ! data interpolated to new vertical levels.
LOGICAL, INTENT(IN), OPTIONAL :: NoExtrapolate  ! no extrapolation if present/true
LOGICAL, INTENT(IN), OPTIONAL :: RMDICheck      ! check for RMDI if present/true

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = "Ops_VerticalInterp"
INTEGER                       :: i
INTEGER                       :: j
INTEGER                       :: k
INTEGER                       :: index  ! Loop indices
INTEGER                       :: VIorder         ! order of interpolation
INTEGER                       :: len1In          ! Size of DataIn /out in i direction
INTEGER                       :: len2In          ! Size of DataIn /out in j direction
INTEGER                       :: len3In          ! Size of DataIn  in k direction.
INTEGER                       :: len3Out         ! Size of DataOut in k direction.
INTEGER                       :: temp
INTEGER, ALLOCATABLE          :: top3In(:,:)
REAL                          :: LastROut
INTEGER, ALLOCATABLE          :: KOut(:,:,:)     ! level in rIn below rOut
CHARACTER(len=80)             :: ErrorMessage(2) ! Error message.

! ----------------------------------------------------------------------
!  Section 1.   Initialize
! ----------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! Check that a valid interpolation scheme has been specified.

IF (Scheme /= 1 .AND. Scheme /= 3 .AND. Scheme /= 5) THEN
  VIorder = 1

  ErrorMessage(1) = 'Invalid value of the variable Scheme:'
  ErrorMessage(2) = 'linear interpolation performed.'

  CALL gen_warn (RoutineName,       &
                 ErrorMessage(1:2))
ELSE
  VIorder = Scheme
END IF

IF (PRESENT (RMDICheck)) THEN
  IF (RMDICheck .AND. Scheme > 1) THEN
    VIorder = 1
    ErrorMessage(1) = 'RMDI check not available other than for linear.'
    ErrorMessage(2) = 'linear interpolation performed.'
    CALL gen_warn (RoutineName,       &
                   ErrorMessage(1:2))
  END IF
END IF

len1In = SIZE (DataIn, DIM = 1)
len2In = SIZE (DataIn, DIM = 2)
len3In = SIZE (DataIn, DIM = 3)
len3Out = SIZE (DataOut, DIM = 3)

ALLOCATE (top3In(len1In,len2In))
ALLOCATE (KOut(len1In,len2In,len3Out))

! ----------------------------------------------------------------------
! Section 2.   For each output point find k so that the point on the
!              output grid lies between k and k+1
! ----------------------------------------------------------------------

Kout(:,:,:) = 1

DO i = 1, len1In
  DO j = 1, len2In
    LastROut = -HUGE (LastROut)
    temp = 0
    DO index = Len3In, 1, -1  ! Upper Cx levels can be missing for slant sondes
      IF (rIn(i,j,index) /= RMDI) THEN
        temp = index
        EXIT
      END IF
    END DO
    top3In(i,j) = temp
    DO k = len3Out,1,-1
      IF (rOut(i,j,k) > LastROut) temp = top3In(i,j) - 1
      DO index = temp,2,-1
        IF (rIn(i,j,index) <= rOut(i,j,k)) THEN
          KOut(i,j,k) = index
          temp = index
          EXIT
        END IF
      END DO
      LastROut = rOut(i,j,k)
    END DO
  END DO
END DO

! ----------------------------------------------------------------------
! Section 3.   Perform required Interpolations with or without RMDI check.
! ----------------------------------------------------------------------

! Call the specified interpolation Scheme:

SELECT CASE (VIorder)
  CASE (1)
    IF (PRESENT (RMDICheck)) THEN
      CALL Ops_VerticalInterpLinear (len1In,    & ! in
                                     len2In,    & ! in
                                     len3Out,   & ! in
                                     KOut,      & ! in
                                     rIn,       & ! in
                                     rOut,      & ! in
                                     DataIn,    & ! in
                                     DataOut,   & ! out
                                     RMDICheck)   ! in optional
    ELSE
      CALL Ops_VerticalInterpLinear (len1In,  & ! in
                                     len2In,  & ! in
                                     len3Out, & ! in
                                     KOut,    & ! in
                                     rIn,     & ! in
                                     rOut,    & ! in
                                     DataIn,  & ! in
                                     DataOut)   ! out
    END IF

  CASE (3)
    CALL Ops_VerticalInterpCubic (len1In,  & ! in
                                  len2In,  & ! in
                                  len3In,  & ! in
                                  len3Out, & ! in
                                  KOut,    & ! in
                                  rIn,     & ! in
                                  rOut,    & ! in
                                  DataIn,  & ! in
                                  DataOut)   ! out

  CASE (5)
    CALL Ops_VerticalInterpQuintic (len1In,  & ! in
                                    len2In,  & ! in
                                    len3In,  & ! in
                                    len3Out, & ! in
                                    KOut,    & ! in
                                    rIn,     & ! in
                                    rOut,    & ! in
                                    DataIn,  & ! in
                                    DataOut)   ! out
END SELECT

! ----------------------------------------------------------------------
! Section 4.   Check for extrapolation
! ----------------------------------------------------------------------

IF (PRESENT (NoExtrapolate)) THEN
  IF (NoExtrapolate) THEN
    top3In(:,:) = MAX (1, top3In(:,:))  ! Avoid 0 index below (DataOut already RMDI in this case)
    DO k = 1, len3Out
      DO j = 1, len2In
        DO i = 1, len1In
          IF (rOut(i,j,k) > rIn(i,j,top3In(i,j)) .OR. rOut(i,j,k) < rIn(i,j,1)) DataOut(i,j,k) = RMDI
        END DO
      END DO
    END DO
  END IF
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_VerticalInterp
