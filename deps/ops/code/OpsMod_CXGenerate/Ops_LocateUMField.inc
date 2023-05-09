!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Locates a UM field in a UM dump.
!
! Inputs:
!   UMHdr: the UM header
!   StashCode: The stashcode of the required field.
!   Time: The forecast time of the required field.
!   ModelTimeUTC: Validity time UTC in the UM dump.
!
! Outputs:
!   Pos: The position of the first level in the UM dump.
!   NLev: The number of levels of the field available.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_LocateUMField (UMHdr,        &
                              StashCode,    &
                              Time,         &
                              Pos,          &
                              NLev,         &
                              ModelTimeUTC)

USE GenMod_CLookAdd, ONLY: &
  LBFT,                    &
  ITEM_CODE

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE GenMod_ModelIO, ONLY: &
  UM_Header_Type

IMPLICIT NONE

! Subroutine arguments:
TYPE (UM_Header_Type), INTENT(IN) :: UMHdr
INTEGER, INTENT(IN)               :: StashCode
INTEGER, INTENT(IN)               :: Time
INTEGER, INTENT(OUT)              :: Pos
INTEGER, INTENT(OUT)              :: NLev
INTEGER, INTENT(IN)               :: ModelTimeUTC(:)

! Local declarations:
INTEGER                           :: LocNLev
INTEGER                           :: iField
CHARACTER(len=*), PARAMETER       :: RoutineName = "Ops_LocateUMField"

!----------------------------------
! 0. Tracing and initialisations
!----------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

LocNLev = 0
Pos = IMDI

DO iField = 1, UMHdr % NumFlds
  IF (UMHdr % LookUp(ITEM_CODE,iField) == StashCode .AND. &
      (ModelTimeUTC(iField) == Time .OR. UMHdr % Lookup(LBFT,iField) == Time)) THEN
    IF (LocNLev == 0) Pos = iField
    LocNLev = LocNLev + 1
  END IF
END DO

NLev = LocNLev

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_LocateUMField
