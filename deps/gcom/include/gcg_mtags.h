! GCG.mtags
!   Message tags reserved by GCOM GCG
!   All MPI tags must be below 2^15
!   Tags 99999960-80 are reserved for GCG (groups)
#if defined (MPI_SRC)
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__vec0 = 32720
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__vec1 = 32721
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__rot0 = 32722
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__rot1 = 32723
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__rot2 = 32724
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__rot3 = 32725
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__a2a  = 32726
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__rsum = 32727
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__rmin = 32728
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__rmax = 32729
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__isum = 32727
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__imin = 32728
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__imax = 32729
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__new  = 32730
#else
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__vec0 = 999999970
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__vec1 = 999999971
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__rot0 = 999999972
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__rot1 = 999999973
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__rot2 = 999999974
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__rot3 = 999999975
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__a2a  = 999999976
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__rsum = 999999977
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__rmin = 999999978
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__rmax = 999999979
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__isum = 999999977
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__imin = 999999978
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__imax = 999999979
INTEGER (KIND=gc_int_kind), PARAMETER :: gcgid__new  = 999999980
#endif

!============================================================
