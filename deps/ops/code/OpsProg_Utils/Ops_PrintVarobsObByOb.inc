!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Prints the contents of a varob file ob by ob.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_PrintVarobsObByOb (varobs,               &
                                  batch_list,           &
                                  ob_list,              &
                                  varfield_list,        &
                                  num_decimal_places,   &
                                  ignore_varfield_list, &
                                  out_unit)

USE GenMod_CLookAdd, ONLY: &
  VarobsLookupNumObs

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI,                                  &
  RMDI

USE GenMod_UMHeaderConstants, ONLY: &
  FH_ColDepCSize1,                  &
  FH_LookupSize2,                   &
  IC_LenObCol,                      &
  IC_NumItem,                       &
  IC_NumMeta,                       &
  IC_NumVarBatches,                 &
  IC_ObsGroup

USE OpsMod_ObsGroupInfo, ONLY: &
  ObsGroupRadar

USE OpsMod_VarobsLib, ONLY: &
  varobs_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (varobs_Type), INTENT(IN) :: varobs
INTEGER, INTENT(IN)            :: batch_list(:)
INTEGER, INTENT(IN)            :: ob_list(:)
INTEGER, INTENT(IN)            :: varfield_list(:)
INTEGER, INTENT(IN)            :: num_decimal_places
INTEGER, INTENT(IN)            :: ignore_varfield_list(:)
INTEGER, INTENT(IN)            :: out_unit

! Local declarations:
INTEGER                        :: ob_num
INTEGER                        :: batch_num
INTEGER                        :: i
INTEGER                        :: j
INTEGER                        :: k
INTEGER                        :: l
INTEGER                        :: batchOffset
INTEGER                        :: len_ob_col
INTEGER                        :: num_meta
INTEGER                        :: num_levs
INTEGER                        :: num_varfields
REAL, ALLOCATABLE              :: BatchOfObs(:)
INTEGER, ALLOCATABLE           :: varfields(:)
INTEGER, ALLOCATABLE           :: varlevels(:)
INTEGER, PARAMETER             :: len_callsign = 16
CHARACTER(len=len_callsign)    :: callsign
REAL, ALLOCATABLE              :: lat_lons(:)
REAL, ALLOCATABLE              :: times(:)
REAL, ALLOCATABLE              :: family(:)
REAL, ALLOCATABLE              :: SiteLatLon(:)
INTEGER                        :: coord_ob_start
LOGICAL                        :: FamilyFlag
LOGICAL                        :: GpsroTPDFlag
CHARACTER(len=50)              :: header_format
CHARACTER(len=50)              :: data_format
CHARACTER(len=9)               :: pressure_string

FamilyFlag = .FALSE.
GpsroTPDFlag = .FALSE.
batchOffset = 2
IF (varobs % Fixhd(FH_LookupSize2) ==  varobs % IntC(IC_NumVarBatches) + 3) THEN
  batchOffset = 3
  IF (varobs % IntC(IC_ObsGroup) == ObsGroupRadar) THEN
    FamilyFlag = .TRUE.
  ELSE
    GpsroTPDFlag = .TRUE.
  END IF
END IF

IF (varobs % IntC(IC_NumItem) == 3) THEN
  IF (FamilyFlag) THEN
    WRITE (header_format, '(A,I0,A)') '(6A7,A9,A17,3A', 15 + num_decimal_places, ',4A20)' 
    WRITE (data_format, '(A,I0,A,I0,A)') '(6I7,A9,A17,3F', 15 + num_decimal_places, '.', num_decimal_places, ',3F20.5,I7)'
    WRITE (out_unit, header_format) "  batch", " ob num", "  level", "  field", "   Time", " obtype", "   Plevel", &
                                    "        Callsign", &
                                    "            ob value", &
                                    "            ob error", &
                                    "                 pge", &
                                    "                 lat", &
                                    "                 lon", &
                                    "                time", &
                                    "              family"
  ELSE IF (GpsroTPDFlag) THEN
    WRITE (header_format, '(A,I0,A)') '(6A7,A9,A17,3A', 15 + num_decimal_places, ',5A20)' 
    WRITE (data_format, '(A,I0,A,I0,A)') '(6I7,A9,A17,3F', 15 + num_decimal_places, '.', num_decimal_places, ',5F20.5)'
    WRITE (out_unit, header_format) "  batch", " ob num", "  level", "  field", "   Time", " obtype", "   Plevel", &
                                    "        Callsign", &
                                    "            ob value", &
                                    "            ob error", &
                                    "                 pge", &
                                    "                 lat", &
                                    "                 lon", &
                                    "                time", &
                                    "            site lat", &
                                    "            site lon"
  ELSE
    WRITE (header_format, '(A,I0,A)') '(6A7,A9,A17,3A', 15 + num_decimal_places, ',3A20)' 
    WRITE (data_format, '(A,I0,A,I0,A)') '(6I7,A9,A17,3F', 15 + num_decimal_places, '.', num_decimal_places, ',3F20.5)'
    WRITE (out_unit, header_format) "  batch", " ob num", "  level", "  field", "   Time", " obtype", "   Plevel", &
                                    "        Callsign", &
                                    "            ob value", &
                                    "            ob error", &
                                    "                 pge", &
                                    "                 lat", &
                                    "                 lon", &
                                    "                time"
  END IF
ELSE
  IF (FamilyFlag) THEN
    WRITE (header_format, '(A,I0,A)') '(6A7,A9,A17,A', 15 + num_decimal_places, ',4A20)' 
    WRITE (data_format, '(A,I0,A,I0,A)') '(6I7,A9,A17,F', 15 + num_decimal_places, '.', num_decimal_places, ',3F20.5,I7)'
    WRITE (out_unit, header_format) "  batch", " ob num", "  level", "  field", "   Time", " obtype", "   Plevel", &
                                    "        Callsign", &
                                    "            ob value", &
                                    "                 lat", &
                                    "                 lon", &
                                    "                time", &
                                    "              family"
  ELSE
    WRITE (header_format, '(A,I0,A)') '(6A7,A9,A17,A', 15 + num_decimal_places, ',3A20)' 
    WRITE (data_format, '(A,I0,A,I0,A)') '(6I7,A9,A17,F', 15 + num_decimal_places, '.', num_decimal_places, ',3F20.5)'
    WRITE (out_unit, header_format) "  batch", " ob num", "  level", "  field", "   Time", " obtype", "   Plevel", &
                                    "        Callsign", &
                                    "            ob value", &
                                    "                 lat", &
                                    "                 lon", &
                                    "                time"
  END IF
END IF

num_varfields = COUNT (varobs % ColDepC(3 * varobs % FixHd(FH_ColDepCSize1) + 1: &
                                                    4 * varobs % FixHd(FH_ColDepCSize1)) /= IMDI)
ALLOCATE (varfields(num_varfields))
ALLOCATE (varlevels(num_varfields))
DO i = 1, varobs % FixHd(FH_ColDepCSize1)
  IF (NINT (varobs % ColDepC(3 * varobs % FixHd(FH_ColDepCSize1) + i)) /= IMDI) THEN
    varfields(NINT (varobs % ColDepC(3 * varobs % FixHd(FH_ColDepCSize1) + i))) = i
    varlevels(NINT (varobs % ColDepC(3 * varobs % FixHd(FH_ColDepCSize1) + i))) = &
                NINT (varobs % ColDepC(7 * varobs % FixHd(FH_ColDepCSize1) + i))
  END IF
END DO
num_meta = varobs % IntC(IC_NumMeta)
len_ob_col = varobs % IntC(IC_LenObCol)

IF (FamilyFlag) THEN
  CALL varobs % read_batch (varobs % Fixhd(FH_LookupSize2) - 2, &
                            lat_lons)
  CALL varobs % read_batch (varobs % Fixhd(FH_LookupSize2) - 1, &
                            times)
  CALL varobs % read_batch (varobs % Fixhd(FH_LookupSize2), &
                            family)
ELSE IF (GpsroTPDFlag) THEN
  CALL varobs % read_batch (varobs % Fixhd(FH_LookupSize2) - 2, &
                            lat_lons)
  CALL varobs % read_batch (varobs % Fixhd(FH_LookupSize2) - 1, &
                            times)
  CALL varobs % read_batch (varobs % Fixhd(FH_LookupSize2), &
                            SiteLatLon)
ELSE
  CALL varobs % read_batch (varobs % Fixhd(FH_LookupSize2) - 1, &
                            lat_lons)
  CALL varobs % read_batch (varobs % Fixhd(FH_LookupSize2), &
                            times)
END IF

DO i = 1,SIZE (batch_list)
  batch_num = batch_list(i)
  IF (batch_num <= varobs % Fixhd(FH_LookupSize2) - batchOffset) THEN
    IF (batch_num == 1) THEN
      coord_ob_start = 0
    ELSE
      coord_ob_start = SUM (varobs % Lookup(VarobsLookupNumObs,1:batch_num - 1))
    END IF
    CALL varobs % read_batch (batch_num,  &
                              BatchOfObs)
    DO j = 1,SIZE (ob_list)
      ob_num = ob_list(j)
      IF (ob_num <= varobs % Lookup(VarobsLookupNumObs,batch_num)) THEN
        callsign = ""
        DO k = 1, MIN (num_meta - 4, len_callsign)
          callsign(k:k) = CHAR (NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 2 + k)))
        END DO
        IF (varobs % IntC(IC_NumItem) == 3) THEN
          DO k = 1, num_varfields
            IF ((ALL (varfield_list(:) /= varfields(k)) .AND. ALL (varfield_list(:) /= -1)) .OR. &
                ANY (ignore_varfield_list(:) == varfields(k))) CYCLE
            num_levs = SUM (varlevels(1:k - 1))
            DO l = 1, varlevels(k)
              IF (BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta - 1) == RMDI) THEN
                IF (FamilyFlag) THEN
                  WRITE (out_unit, data_format) batch_num, ob_num, l, varfields(k), &
                      NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 1)), &
                      NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 2)), &
                      "   n/a   ", &
                      " " // callsign(1:16), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + 3 * num_levs + l), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + 3 * num_levs + l + varlevels(k)), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + 3 * num_levs + l + 2 * varlevels(k)), &
                      lat_lons(2 * (ob_num + coord_ob_start) - 1), &
                      lat_lons(2 * (ob_num + coord_ob_start)), times(ob_num + coord_ob_start),&
                      INT (family(ob_num + coord_ob_start))
                ELSE IF (GpsroTPDFlag) THEN
                  WRITE (out_unit, data_format) batch_num, ob_num, l, varfields(k), &
                      NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 1)), &
                      NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 2)), &
                      "   n/a   ", &
                      " " // callsign(1:16), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + 3 * num_levs + l), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + 3 * num_levs + l + varlevels(k)), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + 3 * num_levs + l + 2 * varlevels(k)), &
                      lat_lons(2 * (ob_num + coord_ob_start) - 1), &
                      lat_lons(2 * (ob_num + coord_ob_start)), times(ob_num + coord_ob_start),&
                      SiteLatLon(2 * (ob_num + coord_ob_start) - 1), SiteLatLon(2 * (ob_num + coord_ob_start))
                ELSE
                  WRITE (out_unit, data_format) batch_num, ob_num, l, varfields(k), &
                      NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 1)), &
                      NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 2)), &
                      "   n/a   ", &
                      " " // callsign(1:16), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + 3 * num_levs + l), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + 3 * num_levs + l + varlevels(k)), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + 3 * num_levs + l + 2 * varlevels(k)), &
                      lat_lons(2 * (ob_num + coord_ob_start) - 1), &
                      lat_lons(2 * (ob_num + coord_ob_start)), times(ob_num + coord_ob_start)
                END IF
              ELSE
                WRITE (pressure_string, '(F9.1)') BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta - 1)
                IF (FamilyFlag) THEN
                  WRITE (out_unit, data_format) batch_num, ob_num, l, varfields(k), &
                      NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 1)), &
                      NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 2)), &
                      pressure_string, &
                      " " // callsign(1:16), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + 3 * num_levs + l), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + 3 * num_levs + l + varlevels(k)), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + 3 * num_levs + l + 2 * varlevels(k)), &
                      lat_lons(2 * (ob_num + coord_ob_start) - 1), &
                      lat_lons(2 * (ob_num + coord_ob_start)), times(ob_num + coord_ob_start),&
                      INT (family(ob_num + coord_ob_start))
                ELSE IF (GpsroTPDFlag) THEN
                  WRITE (out_unit, data_format) batch_num, ob_num, l, varfields(k), &
                      NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 1)), &
                      NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 2)), &
                      pressure_string, &
                      " " // callsign(1:16), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + 3 * num_levs + l), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + 3 * num_levs + l + varlevels(k)), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + 3 * num_levs + l + 2 * varlevels(k)), &
                      lat_lons(2 * (ob_num + coord_ob_start) - 1), &
                      lat_lons(2 * (ob_num + coord_ob_start)), times(ob_num + coord_ob_start),&
                      SiteLatLon(2 * (ob_num + coord_ob_start) - 1), SiteLatLon(2 * (ob_num + coord_ob_start))
                ELSE
                  WRITE (out_unit, data_format) batch_num, ob_num, l, varfields(k), &
                      NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 1)), &
                      NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 2)), &
                      pressure_string, &
                      " " // callsign(1:16), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + 3 * num_levs + l), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + 3 * num_levs + l + varlevels(k)), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + 3 * num_levs + l + 2 * varlevels(k)), &
                      lat_lons(2 * (ob_num + coord_ob_start) - 1), &
                      lat_lons(2 * (ob_num + coord_ob_start)), times(ob_num + coord_ob_start)
                END IF
              END IF
            END DO
          END DO
        ELSE
          DO k = 1, num_varfields
            IF ((ALL (varfield_list(:) /= varfields(k)) .AND. ALL (varfield_list(:) /= -1)) .OR. &
                ANY (ignore_varfield_list(:) == varfields(k))) CYCLE
            num_levs = SUM (varlevels(1:k - 1))
            DO l = 1, varlevels(k)
              IF (BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta - 1) == RMDI) THEN
                IF (FamilyFlag) THEN
                  WRITE (out_unit, data_format) batch_num, ob_num, l, varfields(k), &
                      NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 1)), &
                      NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 2)), &
                      "   n/a   ", &
                      " " // callsign(1:16), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + num_levs + l), &
                      lat_lons(2 * (ob_num + coord_ob_start) - 1), &
                      lat_lons(2 * (ob_num + coord_ob_start)), times(ob_num + coord_ob_start), &
                      INT(family(ob_num + coord_ob_start))
                ELSE
                  WRITE (out_unit, data_format) batch_num, ob_num, l, varfields(k), &
                      NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 1)), &
                      NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 2)), &
                      "   n/a   ", &
                      " " // callsign(1:16), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + num_levs + l), &
                      lat_lons(2 * (ob_num + coord_ob_start) - 1), &
                      lat_lons(2 * (ob_num + coord_ob_start)), times(ob_num + coord_ob_start)
                END IF
              ELSE
                WRITE (pressure_string, '(F9.1)') BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta - 1)
                IF (FamilyFlag) THEN
                  WRITE (out_unit, data_format) batch_num, ob_num, l, varfields(k), &
                      NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 1)), &
                      NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 2)), &
                      pressure_string, &
                      " " // callsign(1:16), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + num_levs + l), &
                      lat_lons(2 * (ob_num + coord_ob_start) - 1), &
                      lat_lons(2 * (ob_num + coord_ob_start)), times(ob_num + coord_ob_start), &
                      INT(family(ob_num + coord_ob_start))
                ELSE
                  WRITE (out_unit, data_format) batch_num, ob_num, l, varfields(k), &
                      NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 1)), &
                      NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 2)), &
                      pressure_string, &
                      " " // callsign(1:16), &
                      BatchOfObs((ob_list(j) - 1) * len_ob_col + num_meta + num_levs + l), &
                      lat_lons(2 * (ob_num + coord_ob_start) - 1), &
                      lat_lons(2 * (ob_num + coord_ob_start)), times(ob_num + coord_ob_start)
                END IF
              END IF
            END DO
          END DO
        END IF
      END IF
    END DO
    DEALLOCATE (BatchOfObs)
  END IF
END DO

END SUBROUTINE Ops_PrintVarobsObByOb
