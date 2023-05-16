!-----------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-----------------------------------------------------------------------
! Cross checks pointers in PP LOOKUP records with model parameters
!-----------------------------------------------------------------------

SUBROUTINE Ops_CheckLookup (UMHdr)

USE GenMod_CLookAdd, ONLY: &
  DATA_TYPE,               &
  LBLREC

USE GenMod_Core, ONLY: &
  gen_message,         &
  gen_trace_entry,     &
  gen_trace_exit,      &
  gen_warn,            &
  UseTrace

USE GenMod_UMHeaderConstants, ONLY: &
  FH_Dataset,                       &
  FH_Dataset_Boundary,              &
  FH_DataSize,                      &
  FH_LookupSize2,                   &
  FH_ModelVersion

IMPLICIT NONE

! Subroutine arguments:
CLASS (UM_header_type), INTENT(IN) :: UMhdr

! Local declarations:
INTEGER                            :: K                     ! Loop counter
INTEGER                            :: LEN_D                 ! Cumulative length of data
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_CheckLookup"
CHARACTER(len=300)                 :: message(2)

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! Ops_CheckLookup falls over with Boundary Datasets if pre-3.4
IF (UMhdr % FixHd(FH_Dataset) == FH_Dataset_Boundary .AND. UMhdr % FixHd(FH_ModelVersion) < 304) THEN
  WRITE (message(1), '(A)') 'Skipped for Boundary Dataset (Pre 3.4)'
  CALL gen_message (RoutineName, &
                    message(1))
ELSE
  LEN_D = 0
  DO K = 1, UMhdr % FixHd(FH_LookupSize2)
    ! Check that data_type is valid no: 1 to 3 or -1 to -3
    IF ((UMhdr % Lookup(DATA_TYPE,K) >= 1 .AND. UMhdr % Lookup(DATA_TYPE,K) <= 3) .OR. &
        (UMhdr % Lookup(DATA_TYPE,K) <= -1 .AND. UMhdr % Lookup(DATA_TYPE,K) >= -3)) THEN
      LEN_D = LEN_D + UMhdr % Lookup(LBLREC,K)
    ELSE
      WRITE (message(1), '(A,I0,A,I0,A)') "*ERROR* Wrong value of ", UMhdr % Lookup(DATA_TYPE,K), &
                                          " in UMhdr % Lookup(DATA_TYPE,", K, ")"
      CALL UMhdr % print_lookup (K)
      CALL gen_warn (RoutineName, &
                     message(1))
      GOTO 9999
    END IF
  END DO

  IF (UMhdr % FixHd(FH_DataSize) /= LEN_D) THEN

    WRITE (message(1), '(A,I0,A,I0)') "*ERROR* Length of model data specified wrongly: FILE = ", LEN_D, &
                                      " UMhdr % FixHd(FH_DataSize) = ", UMhdr % FixHd(FH_DataSize)
    WRITE (message(2), '(A)')' Your initial dump may need reconfiguring.'
    CALL gen_warn (RoutineName,  &
                   message(1:2))
    GOTO 9999
  END IF

END IF  !  Check on pre-3.4 Boundary Datasets

9999 CONTINUE

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CheckLookup
