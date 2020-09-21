!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Initialise or adjust the random number variables
!-------------------------------------------------------------------------------

SUBROUTINE Ops_InitRandom (method_arg,      &
                           with_clock_arg,  &
                           m_arg,           &
                           a_arg,           &
                           c_arg,           &
                           initializer_arg, &
                           junk_size_arg,   &
                           rankId)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:
INTEGER, OPTIONAL, INTENT(IN) :: method_arg
LOGICAL, OPTIONAL, INTENT(IN) :: with_clock_arg
INTEGER, OPTIONAL, INTENT(IN) :: m_arg
INTEGER, OPTIONAL, INTENT(IN) :: a_arg
INTEGER, OPTIONAL, INTENT(IN) :: c_arg
INTEGER, OPTIONAL, INTENT(IN) :: initializer_arg
INTEGER, OPTIONAL, INTENT(IN) :: junk_size_arg
INTEGER, OPTIONAL, INTENT(IN) :: rankId

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName   = "Ops_InitRandom"
INTEGER, PARAMETER            :: junk_size_max = 100
REAL                          :: junk(1:junk_size_max) ! Array to receive spin-up numbers

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (PRESENT (method_arg)) random_number_method = method_arg
IF (PRESENT (with_clock_arg)) initialise_with_clock = with_clock_arg
IF (PRESENT (m_arg)) meto_m = m_arg
IF (PRESENT (a_arg)) meto_a = a_arg
IF (PRESENT (c_arg)) meto_c = c_arg
IF (PRESENT (initializer_arg)) initializer = initializer_arg
IF (PRESENT (junk_size_arg)) junk_size = min(junk_size_arg, junk_size_max)

CALL Ops_ReadRandomControlNL

CALL Ops_RandomSeed(rankId)

initialised = .TRUE.

IF (junk_size > 0) THEN
  ! Generate a sequence of random numbers to discard
  CALL Ops_RandomNumber (junk(1:junk_size))
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_InitRandom
