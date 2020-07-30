!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Setup random seed, choosing the method specified by random_number_method in
! the module
!-------------------------------------------------------------------------------

SUBROUTINE Ops_RandomSeed(rankId)

USE GenMod_Control, ONLY: &
  mype

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_message,         &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE OpsMod_Kinds, ONLY: &
  integer64

IMPLICIT NONE

INTEGER, OPTIONAL, INTENT(IN) :: rankId ! Optional control over sequence

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = "Ops_RandomSeed"
INTEGER                       :: nseed
INTEGER(kind=integer64)       :: clock
INTEGER, ALLOCATABLE          :: seed(:)
INTEGER                       :: iseed
INTEGER                       :: id ! rank id to be used in seed 
CHARACTER(len=80)             :: Message

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (PRESENT(rankId)) THEN
  id = rankId
ELSE
  id = mype
END IF

SELECT CASE (random_number_method)
  CASE (meto_method)
    IF (initialise_with_clock) THEN
      CALL SYSTEM_CLOCK (COUNT = clock)
      WRITE (Message, '(A,I0)') "Initializer from system_clock is ", clock
      CALL gen_message (RoutineName, &
                        Message)
      IntRand = MOD (clock, meto_m)
    ELSE
      IntRand = MOD (initializer + id, meto_m)
    END IF
  CASE (intrinsic_method)
    CALL RANDOM_SEED (size = nseed)
    ALLOCATE (seed(nseed))
    IF (initialise_with_clock) THEN
      CALL SYSTEM_CLOCK (COUNT = clock)
      seed = clock + 37 * (/ (iseed - 1, iseed = 1, nseed) /)
      WRITE (Message, '(A,I0)') "Initializer from system_clock is ", clock
      CALL gen_message (RoutineName, &
                        Message)
    ELSE
      seed = initializer + id + 37 * (/(iseed - 1, iseed = 1, nseed)/)
    END IF
    CALL RANDOM_SEED (PUT = seed)
    DEALLOCATE (seed)
  CASE DEFAULT
    WRITE (Message, '(A,I0)') "Invalid random number method: ", random_number_method
    CALL gen_fail (RoutineName, &
                   Message)
END SELECT

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_RandomSeed
