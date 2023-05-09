!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Sets up control variables.
!-------------------------------------------------------------------------------

SUBROUTINE Gen_SetupControl (URL)

USE gcom_mod, ONLY:  &
    GC_FORCE_BITREP, &
    GC_ON,           &
    GC_OK,           &
    GC_INT_KIND

USE GenMod_Control, ONLY: &
    DocumentationURL,     &
    GCOMBitRep,           &
    GeneralMode,          &
    GlobalGroup,          &
    mype,                 &
    nproc,                &
    ProduceHTML,          &
    QuietMode,            &
    UM_SECTOR_SIZE

USE GenMod_Core, ONLY: &
    ErrorOut,          &
    gen_get_env,       &
    Gen_ReadConfigNL,  &
    MessageOut

USE OpsMod_EnvUtils, ONLY: &
    ops_get_env

USE OpsMod_Gcom, ONLY: &
    gc_abort,          &
    gc_init,           &
    gc_setopt

USE OpsMod_Kinds, ONLY: &
    integer_default

#ifndef SERIAL_SRC
USE mpl, ONLY: &
    mpl_comm_world
#endif

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN), OPTIONAL :: URL

! Local declarations:
CHARACTER(len=*), PARAMETER            :: verbosity_env = "GEN_MODE"
CHARACTER(len=*), PARAMETER            :: ProduceHTML_env = "GEN_PRODUCE_HTML"
CHARACTER(len=*), PARAMETER            :: DocumentationURL_env = "GEN_DOCUMENTATION_URL"
INTEGER                                :: EnvLength
INTEGER(kind=integer_default)          :: ErrorStatus
INTEGER                                :: verbosity
INTEGER(kind=GC_INT_KIND)              :: GC_ISTAT
CHARACTER(len=20)                      :: verbosity_string
CHARACTER(len=20)                      :: ProduceHTML_value ! contents of Env var of similar name

!-------------------------------------------------------------------------------
! Get GEN_MODE (verbosity)
!-------------------------------------------------------------------------------

CALL gen_get_env (verbosity_env,    &
                  EnvLength,        &
                  verbosity_string)
IF (EnvLength > 0) THEN
  READ (verbosity_string, *, IOSTAT = ErrorStatus) verbosity
  IF (ErrorStatus == 0) THEN
    GeneralMode = verbosity
  ELSE
    WRITE (ErrorOut, '(A)') verbosity_env // '=' // TRIM (verbosity_string) // ': bad value'
  END IF
END IF

!-------------------------------------------------------------------------------
! Setup MPP
!-------------------------------------------------------------------------------

CALL gc_init (" ",   &
              mype,  &
              nproc)
IF (nproc < 0) THEN
  ! Don't use ErrorReport, as you get a circular dependancy
  WRITE (ErrorOut, '(A)') "Gen_SetupControl: gc_init failed with error ", nproc
  STOP
END IF

IF (GeneralMode > QuietMode .AND. mype == 0 .AND. nproc /= 1) THEN
  WRITE (MessageOut, '(I0," Processors initialised.")') nproc
END IF

#ifndef SERIAL_SRC
GlobalGroup = mpl_comm_world
#endif

!-------------------------------------------------------------------------------
! Read configuration namelist. After mype and GeneralMode has been set up.
!-------------------------------------------------------------------------------

CALL Gen_ReadConfigNL

!-------------------------------------------------------------------------------
! get PRODUCE_HTML
!-------------------------------------------------------------------------------

CALL gen_get_env (ProduceHTML_env,   &
                  EnvLength,         &
                  ProduceHTML_value)
IF (EnvLength > 0) THEN
  ProduceHTML = SCAN (ProduceHTML_value,"YyTt") /= 0
END IF

!-----------------------------------------------------------------------------
! get DOCUMENTATION_URL
!-----------------------------------------------------------------------------

CALL gen_get_env (DocumentationURL_env, &
                  EnvLength,            &
                  DocumentationURL)
IF (EnvLength <= 0 .AND. PRESENT (URL)) THEN
  DocumentationURL = URL
END IF

!-------------------------------------------------------------------------------
! Force bit rep calls in GCOM
!-------------------------------------------------------------------------------

IF (GCOMBitRep) THEN
  CALL gc_setopt (GC_FORCE_BITREP, &
                  GC_ON,           &
                  GC_ISTAT)
  IF (GC_ISTAT /= GC_OK) THEN
    ! Don't use ErrorReport, as you get a circular dependency. (Apparently)
    CALL gc_abort (mype,                                              &
                   nproc,                                             &
                   "Gen_SetupControl: Failed to set GC_FORCE_BITREP")
  END IF
  IF (GeneralMode > QuietMode .AND. mype == 0) THEN
    WRITE (MessageOut, '(A)') "Gen_SetupControl: GC_FORCE_BITREP set"
  END IF
END IF

!-------------------------------------------------------------------------------
! Read UM_SECTOR_SIZE
!-------------------------------------------------------------------------------

CALL ops_get_env ("UM_SECTOR_SIZE",                         &
                  UM_SECTOR_SIZE,                           &
                  result_if_env_not_set = (UM_SECTOR_SIZE))

END SUBROUTINE Gen_SetupControl
