!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Prints a fixed header.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_PrintFixedHeader (Fixed_Header, &
                                 out_unit)

USE GenMod_UMHeaderConstants, ONLY: &
  FH_ColDepCSize1,                  &
  FH_ColDepCSize2,                  &
  FH_ColDepCStart,                  &
  FH_DataSize,                      &
  FH_DataStart,                     &
  FH_DTDay,                         &
  FH_DTDayNo,                       &
  FH_DTHour,                        &
  FH_DTMinute,                      &
  FH_DTMonth,                       &
  FH_DTSecond,                      &
  FH_DTYear,                        &
  FH_GridStagger

USE GenMod_UMHeaderConstants, ONLY: &
  FH_HorizGrid,                     &
  FH_IntCSize,                      &
  FH_LevDepCSize1,                  &
  FH_LevDepCSize2,                  &
  FH_LevDepCStart,                  &
  FH_LookupSize1,                   &
  FH_LookupSize2,                   &
  FH_LookupStart,                   &
  FH_MaxDataSize,                   &
  FH_ObsFileType,                   &
  FH_RealCSize,                     &
  FH_RealCStart,                    &
  FH_SubModel,                      &
  FH_VertCoord,                     &
  FH_VTDay,                         &
  FH_VTDayNo,                       &
  FH_VTHour,                        &
  FH_VTMinute,                      &
  FH_VTMonth,                       &
  FH_VTSecond,                      &
  FH_VTYear

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN) :: Fixed_Header(:)
INTEGER, INTENT(IN) :: out_unit

WRITE (out_unit, '(A)')  "Fixed Length Header:"
WRITE (out_unit, '(A)')

WRITE (out_unit, '(A,I10)')      "Submodel type =                        ", Fixed_Header(FH_SubModel)
WRITE (out_unit, '(A,I10)')      "Vertical coordinate type =             ", Fixed_Header(FH_VertCoord)
WRITE (out_unit, '(A,I10)')      "Horizontal grid type =                 ", Fixed_Header(FH_HorizGrid)
WRITE (out_unit, '(A,I10)')      "Grid staggering type =                 ", Fixed_Header(FH_GridStagger)
WRITE (out_unit, '(A,I10)')      "Observation file type =                ", Fixed_Header(FH_ObsFileType)
WRITE (out_unit, '(A,I10)')      "Size of data =                         ", Fixed_Header(FH_DataSize)
WRITE (out_unit, '(A,I10)')      "Start of data =                        ", Fixed_Header(FH_DataStart)
WRITE (out_unit, '(A,I10)')      "Maximum size of data =                 ", Fixed_Header(FH_MaxDataSize)
WRITE (out_unit, '(A,I10,6I7)')  "Data Time =                            ", Fixed_Header(FH_DTYear), &
                                                                            Fixed_Header(FH_DTMonth), &
                                                                            Fixed_Header(FH_DTDay), &
                                                                            Fixed_Header(FH_DTHour), &
                                                                            Fixed_Header(FH_DTMinute), &
                                                                            Fixed_Header(FH_DTSecond), &
                                                                            Fixed_Header(FH_DTDayNo)
WRITE (out_unit, '(A,I10,6I7)')  "Validity Time =                        ", Fixed_Header(FH_VTYear), &
                                                                            Fixed_Header(FH_VTMonth), &
                                                                            Fixed_Header(FH_VTDay), &
                                                                            Fixed_Header(FH_VTHour), &
                                                                            Fixed_Header(FH_VTMinute), &
                                                                            Fixed_Header(FH_VTSecond), &
                                                                            Fixed_Header(FH_VTDayNo)

WRITE (out_unit, '(A,I10)')      "Dimension Int constants =              ", Fixed_Header(FH_IntCSize)
WRITE (out_unit, '(A,I10,I4)')   "Dimension Real constants =             ", Fixed_Header(FH_RealCStart), &
                                                                            Fixed_Header(FH_RealCSize)
WRITE (out_unit, '(A,I10,I4,I4)') "Dimension Level Dependent constants =  ", Fixed_Header(FH_LevDepCStart), &
                                                                             Fixed_Header(FH_LevDepCSize1), &
                                                                             Fixed_Header(FH_LevDepCSize2)
WRITE (out_unit, '(A,I10,I4,I4)') "Dimension Column Dependent constants = ", Fixed_Header(FH_ColDepCStart), &
                                                                             Fixed_Header(FH_ColDepCSize1), &
                                                                             Fixed_Header(FH_ColDepCSize2)
WRITE (out_unit, '(A,I10,I4,I4)') "Dimension Lookup =                     ", Fixed_Header(FH_LookupStart), &
                                                                             Fixed_Header(FH_LookupSize1), &
                                                                             Fixed_Header(FH_LookupSize2)
WRITE (out_unit, '(A)')

END SUBROUTINE Ops_PrintFixedHeader
