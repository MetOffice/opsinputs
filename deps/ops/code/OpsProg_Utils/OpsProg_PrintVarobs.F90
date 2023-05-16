!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Dumps a varob file.
!-------------------------------------------------------------------------------

PROGRAM OpsProg_PrintVarobs

USE GenMod_CLookAdd, ONLY: &
  VarobsLookupNumObs

USE GenMod_Control, ONLY: &
  mype,                   &
  ProduceHtml

USE GenMod_Core, ONLY: &
  gen_fail,            &
  MaxFileNameLen,      &
  MessageOut,          &
  UseTrace

USE GenMod_Setup, ONLY: &
  Gen_SetupControl

USE GenMod_UMHeaderConstants, ONLY: &
  FH_LookupSize2

USE OpsMod_Argument, ONLY: &
  arguments_type,          &
  type_character,          &
  type_integer,            &
  type_integer_list,       &
  type_logical

USE OpsMod_Control, ONLY: &
  DefaultDocURL,          &
  Ops_InitMPI

USE OpsMod_Gcom, ONLY: &
  gc_exit

USE OpsMod_IOUtils, ONLY: &
  Ops_CloseCFile,         &
  Ops_OpenCFileRead,      &
  Ops_OpenFileNew

USE OpsMod_PrintVarobs, ONLY: &
  Ops_PrintVarobsHeader,      &
  Ops_PrintVarobsObByOb,      &
  Ops_PrintVarobsSummary

USE OpsMod_VarobsLib, ONLY: &
  varobs_type

IMPLICIT NONE

! Declarations:
CHARACTER(len=MaxFileNameLen)              :: VarobsFilename
TYPE (varobs_Type)                         :: varobs
INTEGER                                    :: i
INTEGER                                    :: ob_stride
INTEGER                                    :: ob_start
INTEGER                                    :: ob_end
INTEGER                                    :: batch_stride
INTEGER                                    :: batch_start
INTEGER                                    :: batch_end
INTEGER                                    :: decimal_places
INTEGER, ALLOCATABLE                       :: batch_list(:)
INTEGER, ALLOCATABLE                       :: ob_list(:)
INTEGER, ALLOCATABLE                       :: varfields_list(:)
INTEGER, ALLOCATABLE                       :: ignore_varfields_list(:)
LOGICAL                                    :: print_header
LOGICAL                                    :: print_body
LOGICAL                                    :: header_only
LOGICAL                                    :: body_only
LOGICAL                                    :: all_obs
LOGICAL                                    :: summary_only
TYPE (arguments_type)                      :: args
CHARACTER(len=MaxFileNameLen), ALLOCATABLE :: opts(:)
CHARACTER(len=MaxFileNameLen)              :: outfile
INTEGER                                    :: out_unit
CHARACTER(len=*), PARAMETER                :: ProgName = "OpsProg_PrintVarobs"
#ifdef BROKEN_F2003_ARRAY_CONSTRUCTOR
INTEGER                                    :: empty_array(0)
#endif

!-------------------------------------------------------------------------------
! 0.  Initialisations
!-------------------------------------------------------------------------------

CALL Gen_SetupControl (DefaultDocURL)

CALL Ops_InitMPI

IF (mype == 0) THEN
  Producehtml = .FALSE.
  UseTrace = .FALSE.
  print_header = .TRUE.
  print_body = .TRUE.
  all_obs = .FALSE.
  summary_only = .FALSE.

  CALL args % add_arg ("", "--ob-stride", type_integer, .FALSE., 1)
  CALL args % add_arg ("", "--ob-start", type_integer, .FALSE., 1)
  CALL args % add_arg ("", "--ob-end", type_integer, .FALSE., 1)
  CALL args % add_arg ("", "--batch-stride", type_integer, .FALSE., 1)
  CALL args % add_arg ("", "--batch-start", type_integer, .FALSE., 1)
  CALL args % add_arg ("", "--batch-end", type_integer, .FALSE., 1)
#ifdef BROKEN_F2003_ARRAY_CONSTRUCTOR
  CALL args % add_arg ("", "--batch-list", type_integer_list, .FALSE., empty_array)
  CALL args % add_arg ("", "--ob-list", type_integer_list, .FALSE., empty_array)
  CALL args % add_arg ("", "--varfield-list", type_integer_list, .FALSE., empty_array)
  CALL args % add_arg ("", "--ignore-varfield-list", type_integer_list, .FALSE., empty_array)
#else
  CALL args % add_arg ("", "--batch-list", type_integer_list, .FALSE., [INTEGER :: ])
  CALL args % add_arg ("", "--ob-list", type_integer_list, .FALSE., [INTEGER :: ])
  CALL args % add_arg ("", "--varfield-list", type_integer_list, .FALSE., [INTEGER :: ])
  CALL args % add_arg ("", "--ignore-varfield-list", type_integer_list, .FALSE., [INTEGER :: ])
#endif
  CALL args % add_arg ("", "--header-only", type_logical, .FALSE., .FALSE.)
  CALL args % add_arg ("", "--body-only", type_logical, .FALSE., .FALSE.)
  CALL args % add_arg ("", "--all", type_logical, .FALSE., .FALSE.)
  CALL args % add_arg ("", "--summary", type_logical, .FALSE., .FALSE.)
  CALL args % add_arg ("", "--decimal-places", type_integer, .FALSE., 5)
  CALL args % add_arg ("", "--outfile", type_character, .FALSE., "")
  CALL args % process (opts)

  IF (.NOT. ALLOCATED (opts)) THEN
    CALL gen_fail (ProgName,                   &
                   "No input varobs provided")
  END IF

  !-------------------------------------------------------------------------------
  ! 1.  Analyse command line
  !-------------------------------------------------------------------------------

  VarobsFilename = opts(1)
  CALL args % get_arg ("--ob-stride", ob_stride)
  CALL args % get_arg ("--ob-start", ob_start)
  CALL args % get_arg ("--ob-end", ob_end)
  CALL args % get_arg ("--batch-stride", batch_stride)
  CALL args % get_arg ("--batch-start", batch_start)
  CALL args % get_arg ("--batch-end", batch_end)
  IF (args % was_set ("--batch-list")) CALL args % get_arg ("--batch-list", batch_list)
  IF (args % was_set ("--ob-list")) CALL args % get_arg ("--ob-list", ob_list)
  IF (args % was_set ("--varfield-list")) CALL args % get_arg ("--varfield-list", varfields_list)
  IF (args % was_set ("--ignore-varfield-list")) CALL args % get_arg ("--ignore-varfield-list", ignore_varfields_list)
  CALL args % get_arg ("--header-only", header_only)
  CALL args % get_arg ("--body-only", body_only)
  CALL args % get_arg ("--all", all_obs)
  CALL args % get_arg ("--decimal-places", decimal_places)
  CALL args % get_arg ("--summary", summary_only)
  CALL args % get_arg ("--outfile", outfile)

  IF (header_only) print_body = .FALSE.
  IF (body_only) print_header = .FALSE.
  IF (summary_only) THEN
    print_header = .FALSE.
    print_body = .FALSE.
  END IF

  !-------------------------------------------------------------------------------
  ! 2. Open varobs file
  !-------------------------------------------------------------------------------

  CALL Ops_OpenCFileRead (VarobsFilename,    &
                          varobs % unit_num)

  !-------------------------------------------------------------------------------
  ! 3. Read the varobs header
  !-------------------------------------------------------------------------------

  CALL varobs % read_head

  IF (outfile /= "") THEN
    CALL Ops_OpenFileNew (outfile,  &
                          out_unit)
  ELSE
    out_unit = MessageOut
  END IF

  !-------------------------------------------------------------------------------
  ! 4. Print the varobs header
  !-------------------------------------------------------------------------------

  IF (print_header) THEN
    CALL Ops_PrintVarobsHeader (varobs,   &
                                out_unit)
  END IF

  !-------------------------------------------------------------------------------
  ! 5. Print ob list
  !-------------------------------------------------------------------------------

  IF (print_body .OR. summary_only) THEN
    IF (all_obs) THEN
      IF (ALLOCATED (ob_list)) DEALLOCATE (ob_list)
      ALLOCATE (ob_list (MAXVAL (varobs % Lookup(VarobsLookupNumObs,:))))
      ob_list = (/(i, i = 1, MAXVAL (varobs % Lookup(VarobsLookupNumObs,:)))/)
    ELSE
      IF (.NOT. ALLOCATED (ob_list)) THEN
        ALLOCATE (ob_list(1 + (ob_end - ob_start) / ob_stride))
        ob_list = (/(i, i = ob_start, ob_end, ob_stride)/)
      END IF
    END IF
    IF (all_obs) THEN
      IF (ALLOCATED (batch_list)) DEALLOCATE (batch_list)
      ALLOCATE (batch_list (varobs % Fixhd(FH_LookupSize2)))
      batch_list = (/(i, i = 1, varobs % Fixhd(FH_LookupSize2))/)
    ELSE
      IF (.NOT. ALLOCATED (batch_list)) THEN
        ALLOCATE (batch_list(1 + (batch_end - batch_start) / batch_stride))
        batch_list = (/(i, i = batch_start,batch_end,batch_stride)/)
      END IF
    END IF
    IF (.NOT. ALLOCATED (varfields_list)) THEN
      ALLOCATE (varfields_list(1))
      varfields_list(1) = -1
    END IF
    IF (.NOT. ALLOCATED (ignore_varfields_list)) THEN
      ALLOCATE (ignore_varfields_list(1))
      ignore_varfields_list(1) = -1
    END IF

    IF (print_body) THEN
      CALL Ops_PrintVarobsObByOb (varobs,                &
                                  batch_list,            &
                                  ob_list,               &
                                  varfields_list,        &
                                  decimal_places,        &
                                  ignore_varfields_list, &
                                  out_unit)
    ELSE
      CALL Ops_PrintVarobsSummary (varobs,     &
                                   batch_list, &
                                   ob_list,    &
                                   out_unit)
    END IF
  END IF

  !-------------------------------------------------------------------------------
  ! 6. Close varobs file and exit
  !-------------------------------------------------------------------------------

  CALL Ops_CloseCFile (varobs % unit_num, &
                       VarobsFilename)
END IF

CALL gc_exit

END PROGRAM OpsProg_PrintVarobs
