!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Procedures and constants for file opening and closing.
!-------------------------------------------------------------------------------

MODULE OpsMod_IOUtils

IMPLICIT NONE

SAVE

! Public declarations:

! Default record length for OPEN statements
INTEGER, PARAMETER :: default_recl = 512

! Constants for access argument to ops_file_open
INTEGER, PARAMETER :: read_only = 0
INTEGER, PARAMETER :: read_write = 1

! Constants for environment variable argument to ops_file_open and ops_file_close
INTEGER, PARAMETER :: file_name_from_env = 0
INTEGER, PARAMETER :: explicit_file_name = 1

! Constants for deletion (or not) of file upon close
INTEGER, PARAMETER :: delete_file_no = 0
INTEGER, PARAMETER :: delete_file_yes = 1

! Private declarations:
PRIVATE            :: Ops_OpenCEnv_inner
PRIVATE            :: Ops_OpenCFile_inner
PRIVATE            :: Ops_OpenEnv_inner
PRIVATE            :: Ops_OpenFile_inner

CONTAINS

INCLUDE 'Ops_FileSize.inc'

INCLUDE 'Ops_TouchFile.inc'

INCLUDE 'Ops_OpenCEnv_inner.inc'
INCLUDE 'Ops_OpenCFile_inner.inc'
INCLUDE 'Ops_OpenEnv_inner.inc'
INCLUDE 'Ops_OpenFile_inner.inc'
INCLUDE 'Ops_CloseCFile.inc'
INCLUDE 'Ops_CloseFile.inc'
INCLUDE 'Ops_OpenCEnvNew.inc'
INCLUDE 'Ops_OpenCEnvRead.inc'
INCLUDE 'Ops_OpenCFileNew.inc'
INCLUDE 'Ops_OpenCFileRead.inc'
INCLUDE 'Ops_OpenEnvAppend.inc'
INCLUDE 'Ops_OpenEnvNew.inc'
INCLUDE 'Ops_OpenEnvRead.inc'
INCLUDE 'Ops_OpenFileAppend.inc'
INCLUDE 'Ops_OpenFileNew.inc'
INCLUDE 'Ops_OpenFilePathRead.inc'
INCLUDE 'Ops_OpenFileRead.inc'
INCLUDE 'Ops_OpenFileReadWrite.inc'
INCLUDE 'Ops_OpenPathRead.inc'

END MODULE OpsMod_IOUtils
