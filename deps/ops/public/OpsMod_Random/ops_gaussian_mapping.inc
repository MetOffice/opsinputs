!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Generate Gaussian distributed random numbers.
!-------------------------------------------------------------------------------

FUNCTION ops_gaussian_mapping (out_size) RESULT (out)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    Pi

IMPLICIT NONE

! Function arguments:
INTEGER, INTENT(IN)         :: out_size

! Function result:
REAL                        :: out(out_size)

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "ops_gaussian_mapping"
REAL, PARAMETER             :: Threshold = TINY(out(1))
REAL, ALLOCATABLE           :: temp(:)            ! temporary array to hold uniform random numbers
INTEGER                     :: temp_size          ! Size of the temporary array
INTEGER                     :: i                  ! Loop variable

IF (UseTrace) CALL gen_trace_entry (RoutineName)
!
! Allocate a temporary array with an even number of elements
!
temp_size = out_size + MOD(out_size,2)
ALLOCATE( temp(1:temp_size) )
!
! Populate the temporary array with uniform random numbers, using
! the chosen method for generating random numbers
!
SELECT CASE (random_number_method)
  CASE (meto_method)
    CALL Ops_RandomNumberMeto_array1d( temp )
  CASE (intrinsic_method)
    CALL Ops_RandomNumberInt_array1d( temp )
  CASE DEFAULT
    WRITE(*, '(A,I0)') "Invalid random number method: ", random_number_method
    STOP
END SELECT
!
! Convert the random numbers of a Gaussian distribution N(0,1)
!
DO i = 1, out_size - MOD(out_size,2), 2
  out(i)   = SQRT(-2 * LOG(MAX(temp(i), Threshold))) * COS (2 * Pi * temp(i+1))
  out(i+1) = SQRT(-2 * LOG(MAX(temp(i), Threshold))) * SIN (2 * Pi * temp(i+1))
END DO
!
! If the output array is of odd length, then deal with the final element
! separately
!
IF ( MOD(out_size,2) == 1 ) THEN
  out(out_size) = SQRT(-2 * LOG(MAX(temp(out_size), Threshold))) * COS (2 * Pi * temp(out_size+1))
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END FUNCTION ops_gaussian_mapping
