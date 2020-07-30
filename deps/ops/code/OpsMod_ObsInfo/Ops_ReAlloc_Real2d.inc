!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Allocates a pointer array, and sets up the header.  Check the assocation and
! allocate if necessary. Initialise contents to missing. Then initialise the
! header.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ReAlloc_Real2d (Hdr,        &
                               Name,       &
                               El2,        &
                               num_levels)

USE GenMod_Control, ONLY: &
    DebugMode,            &
    GeneralMode,          &
    mype

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    MessageOut,        &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE OpsMod_MiscTypes, ONLY: &
    ElementHeader_Type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ElementHeader_Type), INTENT(INOUT) :: Hdr
CHARACTER(len=*), INTENT(IN)             :: Name
REAL, POINTER                            :: El2(:,:)
INTEGER, INTENT(IN)                      :: num_levels

! Local declarations:
CHARACTER(len=*), PARAMETER              :: RoutineName = "Ops_ReAlloc_Real2d"
REAL, ALLOCATABLE                        :: local_el2(:,:)
INTEGER                                  :: num_obs

!----------------------------------
! 0. Tracing and initialisations
!----------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (Hdr % Present) THEN

  IF (num_levels /= SIZE (El2, DIM = 2)) THEN

    num_obs = SIZE (El2, DIM = 1)

    ALLOCATE (local_el2(num_obs,SIZE (El2, DIM = 2)))
    local_el2 = El2

    DEALLOCATE (El2)
    ALLOCATE (El2(num_obs,num_levels))

    IF (num_levels > SIZE (local_el2, DIM = 2)) THEN
      El2(:,SIZE (local_el2, DIM = 2) + 1:) = RMDI
      El2(:,1:SIZE (local_el2, DIM = 2)) = local_el2(:,:)
    ELSE
      El2(:,:) = local_el2(:,1:num_levels)
    END IF

    Hdr % NumLev = num_levels

    ! Output a diagnostic message if required
    IF (GeneralMode >= DebugMode .AND. mype == 0) THEN
      WRITE (MessageOut, '(2A)') TRIM (Name), ' Reallocate OK'
    END IF

  ELSE

    ! Output a diagnostic message if required
    IF (GeneralMode >= DebugMode .AND. mype == 0) THEN
      WRITE (MessageOut, '(2A)') TRIM (Name), ' Reallocate unnecesary'
    END IF

  END IF

END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ReAlloc_Real2d
