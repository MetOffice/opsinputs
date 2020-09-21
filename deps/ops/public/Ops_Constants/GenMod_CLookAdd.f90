!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Lookup header stuff from the UM.
!-------------------------------------------------------------------------------
MODULE GenMod_CLookAdd

Implicit None

! Validity time
Integer, Parameter :: LBYR   =1   ! Year
Integer, Parameter :: LBMON  =2   ! Month
Integer, Parameter :: LBDAT  =3   ! Day of month
Integer, Parameter :: LBHR   =4   ! Hour
Integer, Parameter :: LBMIN  =5   ! Minute
Integer, Parameter :: LBDAY  =6   ! Day number (if LBREL <=2)
Integer, Parameter :: LBSEC  =6   ! Second     (if LBREL >=3)

! Data time
Integer, Parameter :: LBYRD  =7   ! Year
Integer, Parameter :: LBMOND =8   ! Month
Integer, Parameter :: LBDATD =9   ! Day of month
Integer, Parameter :: LBHRD  =10  ! Hour
Integer, Parameter :: LBMIND =11  ! Minute
Integer, Parameter :: LBDAYD =12  ! Day number (if LBREL <=2)
Integer, Parameter :: LBSECD =12  ! Second     (if LBREL >=3)

Integer, Parameter :: LBTIM  =13  ! Time indicator
Integer, Parameter :: LBFT   =14  ! Forcast period (hours)
Integer, Parameter :: LBLREC =15  ! Length of data record
Integer, Parameter :: LBCODE =16  ! Grid type code
Integer, Parameter :: LBHEM  =17  ! Hemisphere indicator
Integer, Parameter :: LBROW  =18  ! Number of rows in grid
Integer, Parameter :: LBNPT  =19  ! Number of points per row
Integer, Parameter :: LBEXT  =20  ! Length of extra data
Integer, Parameter :: LBPACK =21  ! Packing method indicator
Integer, Parameter :: LBREL  =22  ! Header release number
Integer, Parameter :: LBFC   =23  ! Field code
Integer, Parameter :: LBCFC  =24  ! Second field code
Integer, Parameter :: LBPROC =25  ! Processing code
Integer, Parameter :: LBVC   =26  ! Vertical coordinate type
Integer, Parameter :: LBRVC  =27  ! Coordinate type for reference
    ! level

Integer, Parameter :: LBEXP  =28  ! Experiment number
Integer, Parameter :: LBEGIN =29  ! Start record
Integer, Parameter :: LBNREC =30  ! No of records-Direct access
    ! only
Integer, Parameter :: LBPROJ =31  ! Met-O-8 projection number
Integer, Parameter :: LBTYP  =32  ! Met-O-8 field type
Integer, Parameter :: LBLEV  =33  ! Met-O-8 level code
Integer, Parameter :: LBRSVD1=34  ! Reserved for future PP-package
    !  use
Integer, Parameter :: LBRSVD2=35  ! Reserved for future PP-package
    !  use
Integer, Parameter :: LBRSVD3=36  ! Reserved for future PP-package
    !  use
Integer, Parameter :: LBRSVD4=37  ! Reserved for future PP-package
    !  use
Integer, Parameter :: LBSRCE =38  ! =1111 to indicate following
    ! apply to UM
Integer, Parameter :: DATA_TYPE =39  ! Indicator for real/int or
 ! timeseries
Integer, Parameter :: NADDR  =40  ! Start address in DATA_REAL or
    ! DATA_INT
Integer, Parameter :: LBUSER3=41  ! Free for user-defined function
Integer, Parameter :: ITEM_CODE =42  !Stash code
Integer, Parameter :: LBPLEV =43  ! Pseudo-level indicator (if
    ! defined)
Integer, Parameter :: LBUSER6=44  ! Free for user-defined function
Integer, Parameter :: MODEL_CODE =45 ! internal model identifier

Integer, Parameter :: BULEV  =46  ! Upper level boundary
Integer, Parameter :: BHULEV =47  ! Upper level boundary
Integer, Parameter :: BRSVD3 =48  ! Reserved for future PP-package
    ! use
Integer, Parameter :: BRSVD4 =49  ! Reserved for future PP-package
    ! use
Integer, Parameter :: BDATUM =50  ! Datum value
Integer, Parameter :: BACC   =51  ! (Packed fields) Packing
    ! accuracy
Integer, Parameter :: BLEV   =52  ! Level
Integer, Parameter :: BRLEV  =53  ! Lower level boundary
Integer, Parameter :: BHLEV  =54  ! (Hybrid levels) A-level of
    ! value
Integer, Parameter :: BHRLEV =55  ! Lower level boundary
Integer, Parameter :: BPLAT  =56  ! Real latitude of 'pseudo'
    ! N Pole
Integer, Parameter :: BPLON  =57  ! Real longitude of 'pseudo'
    ! N Pole
Integer, Parameter :: BGOR   =58  ! Grid orientation
Integer, Parameter :: BZY    =59  ! Zeroth latitude
Integer, Parameter :: BDY    =60  ! Latitude interval
Integer, Parameter :: BZX    =61  ! Zeroth longitude
Integer, Parameter :: BDX    =62  ! Longitude interval
Integer, Parameter :: BMDI   =63  ! Missing data indicator
Integer, Parameter :: BMKS   =64  ! M,K,S scaling factor

! Relevant to obstore, varob and acobs files only:
Integer, Parameter :: VarobsLookupNumGroups = 65
Integer, Parameter :: VarobsLookupNumObs    = 66
Integer, Parameter :: VarobsLookupNumData   = 67

! Relevant to obstore files only:
Integer, Parameter  :: MOT = 68 ! Model observation type
Integer, Parameter  :: time_window_before = 69
Integer, Parameter  :: time_window_after = 70

! Relevant to acobs files only:
Integer, Parameter  :: AcobsLookupOceanDSS   = 76
Integer, Parameter  :: AcobsLookupOceanStart = 77

Integer, Parameter :: LBCC_LBYR   = 1  ! Year
Integer, Parameter :: LBCC_LBMON  = 2  ! Month
Integer, Parameter :: LBCC_LBDAT  = 3  ! Day of the month
Integer, Parameter :: LBCC_LBHR   = 4  ! Hour
Integer, Parameter :: LBCC_LBMIN  = 5  ! Minute
Integer, Parameter :: LBCC_LBDAY  = 6  ! Day number
Integer, Parameter :: LBCC_LBEGIN = 7  ! Start record
Integer, Parameter :: LBCC_NADDR  = 8  ! Start address of DATA
! Mapping of MPP_LOOKUP; analogous to mapping in PP header

Integer, Parameter :: P_NADDR=1    ! Address on local PE
Integer, Parameter :: P_LBLREC=2   ! Local length of record

!*----------------------------------------------------------------------
! NADDR IS LOCATION IN PP-HEADER (LOOKUP) FOR START POSN OF VARIABLE
! ITEM_CODE is the location in PP header for a code defined as
!           (section number)*1000+item number
! DATA_TYPE is the location in the PP header defining data as REAL or
!           INTEGER.
! LBNPT is the location defining the number of points per row
!

END MODULE GenMod_CLookAdd
