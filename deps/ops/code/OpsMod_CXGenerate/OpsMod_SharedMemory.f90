!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! SharedMemory_type and type bound procedures.
!-------------------------------------------------------------------------------

MODULE OpsMod_SharedMemory

USE GenMod_Core, ONLY: &
    gen_fail,          &
    MessageOut

USE mpl, ONLY:            &
    mpl_comm_world,       &
    mpl_comm_type_shared, &
    mpl_info_null,        &
    mpl_success

USE OpsMod_MPLInterface, ONLY:   &
    ops_mpl_comm_split_type,     &
    ops_mpl_comm_rank,           &
    ops_mpl_win_allocate_shared, &
    ops_mpl_win_free,            &
    ops_mpl_win_shared_query,    &
    ops_mpl_comm_size

USE, INTRINSIC :: ISO_C_BINDING, ONLY: &
    C_F_POINTER,                       &
    C_PTR

USE OpsMod_Kinds, ONLY : &
    real32

USE GenMod_Control, ONLY: &
    mype

IMPLICIT NONE
PRIVATE

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
TYPE, PUBLIC :: SharedMemory_type
  PRIVATE

  ! Data and pointer to data
  TYPE (C_PTR)          :: BasePtr
  REAL(real32), POINTER :: SharedData(:,:,:,:)

  ! MPI info
  INTEGER               :: SharedRank
  INTEGER               :: ShareComm
  INTEGER               :: Win
  INTEGER               :: NProcNode

  ! Data info
  INTEGER, ALLOCATABLE  :: FieldDims(:,:,:)
  INTEGER, ALLOCATABLE  :: PEReadTable(:,:)

  ! Object info
  LOGICAL               :: is_instantiated = .FALSE.

  ! Read info
  LOGICAL               :: SharedMemoryTimeSlice = .FALSE.

CONTAINS

  ! Get pointer to the array
  PROCEDURE, PUBLIC     :: GetData

  ! Get status of is_instantiated
  PROCEDURE, PUBLIC     :: GetIsInstantiated

  ! Get Read start and end
  PROCEDURE, PUBLIC     :: GetReadStartEnd

  ! Get the node communicator
  PROCEDURE, PUBLIC     :: GetShareComm

  ! Routine to destroy SharedData - Final doesnt work currently
  PROCEDURE, PUBLIC     :: DestroySharedData

END TYPE

!-------------------------------------------------------------------------------
! Module parameters
!-------------------------------------------------------------------------------

INTEGER, PARAMETER      :: ARRAY_RANK = 4

! ValueSize is the size of a value in bytes 8/4 for real double/single
! precision. Set to 4 to use real32 (32 bit - 4 bytes)

INTEGER                 :: ValueSize = 4
INTEGER                 :: Key = 0
INTEGER                 :: istat

INTERFACE SharedMemory_type
  MODULE PROCEDURE InitSharedMemory
END INTERFACE

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
CONTAINS

! Construct the object
FUNCTION InitSharedMemory (FieldDims,    &
                           ReadOnAllPes, &
                           SharedMemoryTimeSlice) RESULT (self)

IMPLICIT NONE

! Function arguments:
INTEGER, INTENT(IN)      :: FieldDims(:,:,:)
LOGICAL, INTENT(IN)      :: ReadOnAllPes
LOGICAL, INTENT(IN)      :: SharedMemoryTimeSlice

! Function result:
TYPE (SharedMemory_type) :: self

! Local declarations:
INTEGER                      :: i
INTEGER                      :: WindowSize
INTEGER                      :: ArraySize(ARRAY_RANK)
CHARACTER(len=80)            :: ErrMess
CHARACTER (len=*), PARAMETER :: RoutineName = 'InitSharedMemory'

ALLOCATE (self % FieldDims(SIZE (FieldDims, DIM = 1), SIZE (FieldDims, DIM = 2), SIZE (FieldDims, DIM = 3)))
self % FieldDims = FieldDims

! Get the size of the array - put this array in the shared object too
ArraySize(1) = MAXVAL (FieldDims(1,:,:)) ! Max size of points in Row
ArraySize(2) = MAXVAL (FieldDims(2,:,:)) ! Max size of points in Col
ArraySize(3) = SUM (FieldDims(3,:,1))    ! Sum of all the levels
! Number of time slices to store
IF(SharedMemoryTimeSlice) THEN
  ArraySize(4) = 1                       ! Read one time-slice per time
ELSE
  ArraySize(4) = SIZE (FieldDims,3)      ! Number of time-slices for all fields
ENDIF

! Setting up the shared memory
CALL ops_mpl_comm_split_type (mpl_comm_world,       &
                              mpl_comm_type_shared, &
                              Key,                  &
                              mpl_info_null,        &
                              self % ShareComm,     &
                              istat)

IF (istat /= mpl_success) THEN
  WRITE (ErrMess, '(A,I0,A,I0)') &
         "Error in ops_mpl_comm_split_type, mype = ", mype, " istat = ", istat
  CALL gen_fail (RoutineName, &
                 ErrMess)
END IF

! Getting the rank of the shared memory
CALL ops_mpl_comm_rank (self % ShareComm,  &
                        self % SharedRank, &
                        istat)

IF (istat /= mpl_success) THEN
  WRITE (ErrMess, '(A,I0,A,I0)') &
         "Error in ops_mpl_comm_rank, mype = ", mype, " istat = ", istat
  CALL gen_fail (RoutineName, &
                 ErrMess)
END IF


! Getting the number of ranks on this node
CALL ops_mpl_comm_size (self % ShareComm, &
                        self % NProcNode, &
                        istat)

IF (istat /= mpl_success) THEN
  WRITE (ErrMess, '(A,I0,A,I0)') &
         "Error in ops_mpl_comm_size, mype = ", mype, " istat = ", istat
  CALL gen_fail (RoutineName, &
                 ErrMess)
END IF

! Get the size of the required memory
IF (self % SharedRank == 0) THEN
  ! Total size of the array to allocate * size of a value (total bytes)
  WindowSize = ValueSize
  DO i = 1, ARRAY_RANK
    WindowSize = WindowSize * ArraySize(i) ! size of the window in bytes
  END DO

  ! Output information about the number of PE's per node and the allocated size
  WRITE (MessageOut, '(A,I0,A,F7.3,A)') "InitSharedMemory: this node has ", &
         self % NProcNode ," PE's and is allocating ",                      &
         real(WindowSize) / real(2**30) ," GiB of shared memory"

ELSE
  WindowSize = 0
END IF

! Allocate the space for the shared memory
CALL ops_mpl_win_allocate_shared (WindowSize,       &
                                  ValueSize,        &
                                  mpl_info_null,    &
                                  self % ShareComm, &
                                  self % BasePtr,   &
                                  self % Win,       &
                                  istat)

IF (istat /= mpl_success) THEN
  WRITE (ErrMess, '(A,I0,A,I0)') &
         "Error in ops_mpl_win_allocate_shared, mype = ", mype, " istat = ", istat
  CALL gen_fail (RoutineName, &
                 ErrMess)
END IF

! Obtain the location of the memory segment
IF (self % SharedRank /= 0) THEN
  CALL ops_mpl_win_shared_query (self % Win,     &
                                 0,              &
                                 WindowSize,     &
                                 ValueSize,      &
                                 self % BasePtr, &
                                 istat)
  IF (istat /= mpl_success) THEN
    WRITE (ErrMess, '(A,I0,A,I0)') &
           "Error in ops_mpl_win_shared_query, mype = ", mype, " istat = ", istat
    CALL gen_fail (RoutineName, &
                   ErrMess)
  END IF

END IF

! self % BasePtr can now be associated with a Fortran pointer
! and thus used to access the shared data
CALL C_F_POINTER (self % BasePtr,                                        &
                  self % SharedData,                                     &
                  [ArraySize(1),ArraySize(2),ArraySize(3),ArraySize(4)])

! Get the table that defines which PE reads the model fields
CALL SetPEReadTable (self,         &
                     ArraySize(3), &
                     ReadOnAllPes)

self % is_instantiated = .TRUE.
self % SharedMemoryTimeSlice = SharedMemoryTimeSlice


END FUNCTION InitSharedMemory

! Setup the read table
SUBROUTINE SetPEReadTable (self,         &
                           NFields,      &
                           ReadOnAllPes)

IMPLICIT NONE

! Subroutine arguments:
CLASS (SharedMemory_type) :: self
INTEGER, INTENT(IN)       :: NFields
LOGICAL, INTENT(IN)       :: ReadOnAllPes

! Local declarations:
INTEGER                   :: NFields_PE
INTEGER                   :: PE
INTEGER                   :: NProcRemain
INTEGER                   :: PEPlusOne
INTEGER                   :: NProcNode

NProcNode = self % NProcNode

ALLOCATE (self % PEReadTable(0:NProcNode - 1,3))

! Get the number of reads per PE (column 1)

IF (ReadOnAllPEs) THEN

  ! Even number of reads (some PE's have '+1' field to read)

  NFields_PE = NFields / NProcNode
  NProcRemain = NFields - (NFields / NProcNode) *  NProcNode
  PEPlusOne = NProcNode - NProcRemain
  DO PE = 0, PEPlusOne - 1
    self % PEReadTable(PE,1) = NFields_PE
  END DO

  ! Do all the plus ones seperately

  DO PE = PEPlusOne, NProcNode - 1
    self % PEReadTable(PE,1) = NFields_PE + 1
  END DO

ELSE

  PE = 0
  self % PEReadTable = 0
  self % PEReadTable(PE,:) = [NFields,1,NFields]

END IF

! Get the start (column 2) and end (column 3) index

PE = 0

! Set up the first index

self % PEReadTable(PE,2) = 1
self % PEReadTable(PE,3) = self % PEReadTable(0,1)

! The remaining index values
DO PE = 1, NProcNode - 1
  self % PEReadTable(PE,2) = self % PEReadTable(PE - 1,3) + 1
  self % PEReadTable(PE,3) = self % PEReadTable(PE - 1,3) + self % PEReadTable(PE,1)
END DO

END SUBROUTINE SetPEReadTable

! Get a slice of the memory defined by the iField, iLevelCounter/iLevel and iTime
FUNCTION GetData (self,          &
                  iField,        &
                  iLevelCounter, &
                  iTime) RESULT (FieldData)

IMPLICIT NONE

! Function arguments:
CLASS (SharedMemory_type)    :: self
INTEGER, INTENT(IN)          :: iLevelCounter
INTEGER, INTENT(IN)          :: iTime
INTEGER, INTENT(IN)          :: iField

! Function result:
REAL(real32), POINTER        :: FieldData(:,:)

! Local declarations:
CHARACTER (len=*), PARAMETER :: RoutineName = 'GetData(SharedMemory_type)'
INTEGER :: iTime_local

IF (.NOT. self % is_instantiated) THEN
  CALL gen_fail (RoutineName,                               &
                 'SharedMemory object is NOT instantiated')
END IF

! If not reading all fields then reuse the first time slice
iTime_local=1
if (.NOT. self % SharedMemoryTimeSlice) iTime_local = iTime

FieldData => self % SharedData(1:self % FieldDims(1,iField,iTime), &
                               1:self % FieldDims(2,iField,iTime), &
                               iLevelCounter,                      &
                               iTime_local)

END FUNCTION GetData

! Get is_instantiated
FUNCTION GetIsInstantiated (self) RESULT (is_instantiated)

IMPLICIT NONE

! Function arguments:
CLASS (SharedMemory_type) :: self

! Function result:
LOGICAL                  :: is_instantiated

is_instantiated = self % is_instantiated

END FUNCTION GetIsInstantiated

! Get the start and end read locations
FUNCTION GetReadStartEnd (self) RESULT (iStartEnd)

IMPLICIT NONE

! Function arguments:
CLASS (SharedMemory_type) :: self

! Function result:
INTEGER                   :: iStartEnd(2)

iStartEnd(1) = self % PEReadTable(self % SharedRank,2)
iStartEnd(2) = self % PEReadTable(self % SharedRank,3)

END FUNCTION GetReadStartEnd

! Get the start and end read locations
FUNCTION GetShareComm (self) RESULT (ShareComm)

IMPLICIT NONE

! Function arguments:
CLASS (SharedMemory_type) :: self

! Function result:
INTEGER                   :: ShareComm

ShareComm = self % ShareComm

END FUNCTION GetShareComm

SUBROUTINE DestroySharedData (self)

IMPLICIT NONE

! Subroutine arguments:
CLASS (SharedMemory_type) :: self

CHARACTER(len=80)            :: ErrMess
CHARACTER (len=*), PARAMETER :: RoutineName = 'DestroySharedData'

IF (self % is_instantiated) THEN

  ! Destroy the shared memory window

  CALL ops_mpl_win_free (self % Win, &
                         istat)

  IF (istat /= mpl_success) THEN
    WRITE (ErrMess, '(A,I0,A,I0)') &
           "Error in ops_mpl_win_free, mype = ", mype, " istat = ", istat
    CALL gen_fail (RoutineName, &
                   ErrMess)
  END IF

  DEALLOCATE (self % FieldDims)
  DEALLOCATE (self % PEReadTable)
  self % is_instantiated = .FALSE.

END IF

END SUBROUTINE DestroySharedData

END MODULE OpsMod_SharedMemory
