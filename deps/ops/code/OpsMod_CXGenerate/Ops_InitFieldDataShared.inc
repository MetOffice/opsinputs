!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Initialise and read the model state into memory
! Inputs: 
!   domain: contains the infomation required to get the header data and read the
!       fields.
!   FieldsInfo: information about fields in UM dump, prepared in 
!       Ops_CXSetupArrays
!   CxArrays: used to get the number of levels in each required field
!   ForecastTimes: define the time slices to be read
!   ModelTimeUTC: Validity time UTC in the UM dump.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_InitFieldDataShared (domain,                &
                                    FieldsInfo,            &
                                    CXArrays,              &
                                    ForecastTimes,         &
                                    ModelTimeUTC,          &
                                    ReadOnAllPes,          &
                                    SharedMemoryTimeSlice, &
                                    ncfields)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE OpsMod_FieldDataShared, ONLY: &
    FieldDataShared

USE OpsMod_ModelIO, ONLY: &
    DomainInfo_type,      &
    NetCDFFields_type

USE OpsMod_MPLInterface, ONLY: &
    ops_mpl_barrier

USE OpsMod_SharedMemory, ONLY: &
    SharedMemory_type

USE OpsMod_Timer, ONLY: &
    Ops_Timer

IMPLICIT NONE

! Subroutine arguments:
TYPE (DomainInfo_type), INTENT(IN)      :: domain
TYPE (FieldInfo_type), INTENT(INOUT)    :: FieldsInfo(:)
TYPE (CXArray_type), INTENT(IN)         :: CXArrays(:,:)
INTEGER, INTENT(IN)                     :: ForecastTimes(:)
INTEGER, INTENT(IN)                     :: ModelTimeUTC(:)
LOGICAL, INTENT(IN)                     :: ReadOnAllPes
LOGICAL, INTENT(IN)                     :: SharedMemoryTimeSlice
TYPE (NetCDFFields_type), INTENT(INOUT) :: ncfields

! Local arguments:
INTEGER                                 :: istat
INTEGER, ALLOCATABLE                    :: FieldDims(:,:,:)
INTEGER                                 :: NumForecastTimes
INTEGER                                 :: iTime
CHARACTER(len=*), PARAMETER             :: RoutineName = 'Ops_InitFieldDataShared'
INTEGER                                 :: ShareComm

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL Ops_Timer (RoutineName)

IF (.NOT. FieldDataShared % GetIsInstantiated()) THEN

  CALL Ops_GetReqDims (FieldDims,     &
                       domain,        &
                       FieldsInfo,    &
                       CXArrays,      &
                       ForecastTimes, &
                       ncfields,      &
                       ModelTimeUTC)

  FieldDataShared = SharedMemory_type (FieldDims, ReadOnAllPes, SharedMemoryTimeSlice)

  ShareComm = FieldDataShared % GetShareComm()
  IF ( .NOT. SharedMemoryTimeSlice ) THEN
    NumForecastTimes = SIZE (FieldDims,3)        ! Number of time-slices
    DO iTime = 1, NumForecastTimes
      CALL Ops_ReadModelShared (FieldDataShared, &
                                domain,          &
                                FieldsInfo,      &
                                CXArrays,        &
                                ForecastTimes,   &
                                ModelTimeUTC,    &
                                ncfields,        &
                                iTime)
    ENDDO
  ENDIF

  CALL ops_mpl_barrier (ShareComm, &
                        istat)

ENDIF

CALL Ops_Timer (RoutineName)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_InitFieldDataShared
