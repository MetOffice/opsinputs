! GC.mtags
!   Message tags reserved by GCOM
!   All MPI tags must be below 2^15
!   Tags 99999960-80 are reserved for GCG (groups)
#if defined (MPI_SRC)
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__task = 32767
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__sync0 = 32766
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__sync1 = 32765
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__bcast = 32764
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__rsum0 = 32763
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__rsum1 = 32762
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__rmin0 = 32761
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__rmin1 = 32760
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__rmax0 = 32759
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__rmax1 = 32758
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__isum0 = 32757
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__isum1 = 32756
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__imin0 = 32755
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__imin1 = 32754
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__imax0 = 32753
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__imax1 = 32752
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__arch  = 32751
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__nxamb = 32750
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__first = 32701
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__last  = gcid__task
#else
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__task  = 999999999
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__sync0 = 999999998
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__sync1 = 999999997
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__bcast = 999999996
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__rsum0 = 999999995
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__rsum1 = 999999994
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__rmin0 = 999999993
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__rmin1 = 999999992
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__rmax0 = 999999991
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__rmax1 = 999999990
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__isum0 = 999999989
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__isum1 = 999999988
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__imin0 = 999999987
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__imin1 = 999999986
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__imax0 = 999999985
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__imax1 = 999999984
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__arch  = 999999983
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__nxamb = 999999982
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__first = 999999901
INTEGER (KIND=gc_int_kind), PARAMETER :: gcid__last  = gcid__task
#endif
!============================================================
