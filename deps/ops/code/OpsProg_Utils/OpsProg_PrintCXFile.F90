!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Dumps a CX file.
!-------------------------------------------------------------------------------

PROGRAM OpsProg_PrintCXFile

USE GenMod_CLookAdd, ONLY: &
  LBNPT

USE GenMod_Control, ONLY: &
  mype,                   &
  ProduceHtml

USE GenMod_Core, ONLY: &
  gen_fail,            &
  MaxFileNameLen,      &
  MessageOut,          &
  UseTrace

USE GenMod_ModelIO, ONLY: &
  UM_Header_Type

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

USE OpsMod_PrintCX, ONLY: &
  Ops_PrintCXBatches,     &
  Ops_PrintCXHeader

IMPLICIT NONE

! Declarations:
CHARACTER(len=MaxFileNameLen)              :: CXFileName
TYPE (UM_Header_Type)                      :: Header
INTEGER                                    :: i
INTEGER                                    :: column_stride
INTEGER                                    :: column_start
INTEGER                                    :: column_end
INTEGER                                    :: batch_stride
INTEGER                                    :: batch_start
INTEGER                                    :: batch_end
INTEGER, ALLOCATABLE                       :: batch_list(:)
INTEGER, ALLOCATABLE                       :: column_list(:)
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
CHARACTER(len=*), PARAMETER                :: ProgName = "OpsProg_PrintCXFile"
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

  CALL args % add_arg ("", "--column-stride", type_integer, .FALSE., 1)
  CALL args % add_arg ("", "--column-start", type_integer, .FALSE., 1)
  CALL args % add_arg ("", "--column-end", type_integer, .FALSE., 1)
  CALL args % add_arg ("", "--batch-stride", type_integer, .FALSE., 1)
  CALL args % add_arg ("", "--batch-start", type_integer, .FALSE., 1)
  CALL args % add_arg ("", "--batch-end", type_integer, .FALSE., 1)
#ifdef BROKEN_F2003_ARRAY_CONSTRUCTOR
  CALL args % add_arg ("", "--batch-list", type_integer_list, .FALSE., empty_array)
  CALL args % add_arg ("", "--column-list", type_integer_list, .FALSE., empty_array)
#else
  CALL args % add_arg ("", "--batch-list", type_integer_list, .FALSE., [INTEGER :: ])
  CALL args % add_arg ("", "--column-list", type_integer_list, .FALSE., [INTEGER :: ])
#endif
  CALL args % add_arg ("", "--header-only", type_logical, .FALSE., .FALSE.)
  CALL args % add_arg ("", "--body-only", type_logical, .FALSE., .FALSE.)
  CALL args % add_arg ("", "--all", type_logical, .FALSE., .FALSE.)
  CALL args % add_arg ("", "--summary", type_logical, .FALSE., .FALSE.)
  CALL args % add_arg ("", "--outfile", type_character, .FALSE., "")
  CALL args % process (opts)

  IF (.NOT. ALLOCATED (opts)) THEN
    CALL gen_fail (ProgName,               &
                   "No input cx provided")
  END IF

  !-------------------------------------------------------------------------------
  ! 1.  Analyse command line
  !-------------------------------------------------------------------------------

  CXFilename = opts(1)
  CALL args % get_arg ("--column-stride", column_stride)
  CALL args % get_arg ("--column-start", column_start)
  CALL args % get_arg ("--column-end", column_end)
  CALL args % get_arg ("--batch-stride", batch_stride)
  CALL args % get_arg ("--batch-start", batch_start)
  CALL args % get_arg ("--batch-end", batch_end)
  IF (args % was_set ("--batch-list")) CALL args % get_arg ("--batch-list", batch_list)
  IF (args % was_set ("--column-list")) CALL args % get_arg ("--column-list", column_list)
  CALL args % get_arg ("--header-only", header_only)
  CALL args % get_arg ("--body-only", body_only)
  CALL args % get_arg ("--all", all_obs)
  CALL args % get_arg ("--summary", summary_only)
  CALL args % get_arg ("--outfile", outfile)

  IF (header_only) print_body = .FALSE.
  IF (body_only) print_header = .FALSE.
  IF (summary_only) THEN
    print_header = .FALSE.
    print_body = .FALSE.
  END IF

  !-------------------------------------------------------------------------------
  ! 2. Open CX file
  !-------------------------------------------------------------------------------

  CALL Ops_OpenCFileRead (CXFilename,       &
                          Header % UnitNum)

  !-------------------------------------------------------------------------------
  ! 3. Read the CX header
  !-------------------------------------------------------------------------------

  CALL Header % read ("CX")

  IF (outfile /= "") THEN
    CALL Ops_OpenFileNew (outfile,  &
                          out_unit)
  ELSE
    out_unit = MessageOut
  END IF

  !-------------------------------------------------------------------------------
  ! 4. Print the CX header
  !-------------------------------------------------------------------------------

  IF (print_header) THEN
    CALL Ops_PrintCXHeader (Header,   &
                            out_unit)
  END IF

  !-------------------------------------------------------------------------------
  ! 5. Print the CX file contents
  !-------------------------------------------------------------------------------

  IF (print_body) THEN
    IF (all_obs) THEN
      IF (ALLOCATED (column_list)) DEALLOCATE (column_list)
      ALLOCATE (column_list (MAXVAL (Header % Lookup (LBNPT,:))))
      column_list = (/(i,i = 1,MAXVAL (Header % Lookup (LBNPT,:)))/)
    ELSE
      IF (.NOT. ALLOCATED (column_list)) THEN
        ALLOCATE (column_list(1 + (column_end - column_start) / column_stride))
        column_list = (/(i,i = column_start,column_end,column_stride)/)
      END IF
    END IF

    IF (all_obs) THEN
      IF (ALLOCATED (batch_list)) DEALLOCATE (batch_list)
      ALLOCATE (batch_list (Header % Fixhd(FH_LookupSize2)))
      batch_list = (/(i,i = 1,Header % Fixhd(FH_LookupSize2))/)
    ELSE
      IF (.NOT. ALLOCATED (batch_list)) THEN
        ALLOCATE (batch_list(1 + (batch_end - batch_start) / batch_stride))
        batch_list = (/(i,i = batch_start,batch_end,batch_stride)/)
      END IF
    END IF

    CALL Ops_PrintCXBatches (Header,      &
                             batch_list,  &
                             column_list, &
                             out_unit)
  END IF

  !-------------------------------------------------------------------------------
  ! 6. Close CX file and exit
  !-------------------------------------------------------------------------------

  CALL Ops_CloseCFile (Header % UnitNum, &
                       CXFilename)
END IF

CALL gc_exit

END PROGRAM OpsProg_PrintCXFile
