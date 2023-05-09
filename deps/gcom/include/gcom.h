! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

! Fortran header file. PLEASE use the parameter variables in user
! routines calling GC and NOT the numeric values. The latter are
! potentially subject to change without further notice.

!     GC general options
INTEGER (KIND=gc_int_kind), PARAMETER :: gc_ok         =     0
INTEGER (KIND=gc_int_kind), PARAMETER :: gc_fail       =    -1
INTEGER (KIND=gc_int_kind), PARAMETER :: gc_none       =     0
INTEGER (KIND=gc_int_kind), PARAMETER :: gc_any        =    -1
INTEGER (KIND=gc_int_kind), PARAMETER :: gc_dontcare   =    -1

!     GC reserved message tags
INTEGER (KIND=gc_int_kind), PARAMETER :: gc_mtag_low   = 999999901
INTEGER (KIND=gc_int_kind), PARAMETER :: gc_mtag_high  = 999999999

!     GCG_RALLETOALLE index parameters
INTEGER (KIND=gc_int_kind), PARAMETER :: s_destination_pe = 1
INTEGER (KIND=gc_int_kind), PARAMETER :: s_base_address_in_send_array = 2
INTEGER (KIND=gc_int_kind), PARAMETER :: s_number_of_elements_in_item = 3
INTEGER (KIND=gc_int_kind), PARAMETER :: s_stride_in_send_array = 4
INTEGER (KIND=gc_int_kind), PARAMETER :: s_element_length = 5
INTEGER (KIND=gc_int_kind), PARAMETER :: s_base_address_in_recv_array = 6
INTEGER (KIND=gc_int_kind), PARAMETER :: s_stride_in_recv_array = 7

INTEGER (KIND=gc_int_kind), PARAMETER :: r_source_pe = 1
INTEGER (KIND=gc_int_kind), PARAMETER :: r_base_address_in_recv_array = 2
INTEGER (KIND=gc_int_kind), PARAMETER :: r_number_of_elements_in_item = 3
INTEGER (KIND=gc_int_kind), PARAMETER :: r_stride_in_recv_array = 4
INTEGER (KIND=gc_int_kind), PARAMETER :: r_element_length = 5
INTEGER (KIND=gc_int_kind), PARAMETER :: r_base_address_in_send_array = 6
INTEGER (KIND=gc_int_kind), PARAMETER :: r_stride_in_send_array = 7

!     Options
INTEGER (KIND=gc_int_kind), PARAMETER :: gc_force_bitrep     = 1
INTEGER (KIND=gc_int_kind), PARAMETER :: gc_alltoall_version = 2
INTEGER (KIND=gc_int_kind), PARAMETER :: gc_is_parallel      = 3

!     Option Status
INTEGER (KIND=gc_int_kind), PARAMETER :: gc_on               = 1
INTEGER (KIND=gc_int_kind), PARAMETER :: gc_off              = 0
INTEGER (KIND=gc_int_kind), PARAMETER :: gc_parallel         = 1
INTEGER (KIND=gc_int_kind), PARAMETER :: gc_serial           = 2
INTEGER (KIND=gc_int_kind), PARAMETER :: gc_alltoall_orig    = 1
INTEGER (KIND=gc_int_kind), PARAMETER :: gc_alltoall_multi   = 2
