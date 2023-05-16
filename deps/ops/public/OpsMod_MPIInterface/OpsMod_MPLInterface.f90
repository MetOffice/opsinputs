!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Contains wrappers and interfaces to some MPL/MPI routines.
!-------------------------------------------------------------------------------

MODULE OpsMod_MPLInterface

IMPLICIT NONE

INTERFACE ops_mpl_alltoallv
MODULE PROCEDURE ops_mpl_alltoallv_char
MODULE PROCEDURE ops_mpl_alltoallv_integer
MODULE PROCEDURE ops_mpl_alltoallv_real
MODULE PROCEDURE ops_mpl_alltoallv_real2d
END INTERFACE ops_mpl_alltoallv

INTERFACE ops_mpl_bcast
MODULE PROCEDURE ops_mpl_bcast_integer
MODULE PROCEDURE ops_mpl_bcast_integer2d
MODULE PROCEDURE ops_mpl_bcast_real
END INTERFACE

INTERFACE ops_mpl_gather
MODULE PROCEDURE ops_mpl_gather_integer
END INTERFACE ops_mpl_gather

INTERFACE ops_mpl_gatherv
MODULE PROCEDURE ops_mpl_gatherv_char
MODULE PROCEDURE ops_mpl_gatherv_integer
MODULE PROCEDURE ops_mpl_gatherv_real
END INTERFACE ops_mpl_gatherv

INTERFACE ops_mpl_irecv
MODULE PROCEDURE ops_mpl_irecv_char2d
MODULE PROCEDURE ops_mpl_irecv_integer
MODULE PROCEDURE ops_mpl_irecv_real
END INTERFACE ops_mpl_irecv

INTERFACE ops_mpl_isend
MODULE PROCEDURE ops_mpl_isend_char2d
MODULE PROCEDURE ops_mpl_isend_integer
MODULE PROCEDURE ops_mpl_isend_real
END INTERFACE ops_mpl_isend

INTERFACE ops_mpl_recv
MODULE PROCEDURE ops_mpl_recv_integer
MODULE PROCEDURE ops_mpl_recv_real
END INTERFACE ops_mpl_recv

INTERFACE ops_mpl_scatterv
MODULE PROCEDURE ops_mpl_scatterv_char2d
MODULE PROCEDURE ops_mpl_scatterv_integer
MODULE PROCEDURE ops_mpl_scatterv_real
MODULE PROCEDURE ops_mpl_scatterv_real2d
END INTERFACE ops_mpl_scatterv

INTERFACE ops_mpl_send
MODULE PROCEDURE ops_mpl_send_integer
MODULE PROCEDURE ops_mpl_send_real
MODULE PROCEDURE ops_mpl_send_real2d
END INTERFACE ops_mpl_send

CONTAINS

INCLUDE 'ops_mpl_alltoallv_char.inc'
INCLUDE 'ops_mpl_alltoallv_integer.inc'
INCLUDE 'ops_mpl_alltoallv_real.inc'
INCLUDE 'ops_mpl_alltoallv_real2d.inc'
INCLUDE 'ops_mpl_barrier.inc'
INCLUDE 'ops_mpl_bcast_integer.inc'
INCLUDE 'ops_mpl_bcast_integer2d.inc'
INCLUDE 'ops_mpl_bcast_real.inc'
INCLUDE 'ops_mpl_comm_rank.inc'
INCLUDE 'ops_mpl_comm_size.inc'
INCLUDE 'ops_mpl_gatherv_char.inc'
INCLUDE 'ops_mpl_gather_integer.inc'
INCLUDE 'ops_mpl_gatherv_integer.inc'
INCLUDE 'ops_mpl_gatherv_real.inc'
INCLUDE 'ops_mpl_irecv_char2d.inc'
INCLUDE 'ops_mpl_irecv_integer.inc'
INCLUDE 'ops_mpl_irecv_real.inc'
INCLUDE 'ops_mpl_isend_char2d.inc'
INCLUDE 'ops_mpl_isend_integer.inc'
INCLUDE 'ops_mpl_isend_real.inc'
INCLUDE 'ops_mpl_recv_integer.inc'
INCLUDE 'ops_mpl_recv_real.inc'
INCLUDE 'ops_mpl_scatterv_char2d.inc'
INCLUDE 'ops_mpl_scatterv_integer.inc'
INCLUDE 'ops_mpl_scatterv_real.inc'
INCLUDE 'ops_mpl_scatterv_real2d.inc'
INCLUDE 'ops_mpl_send_integer.inc'
INCLUDE 'ops_mpl_send_real.inc'
INCLUDE 'ops_mpl_send_real2d.inc'
INCLUDE 'ops_mpl_waitall.inc'
INCLUDE 'ops_mpl_comm_split_type.inc'
INCLUDE 'ops_mpl_win_allocate_shared.inc'
INCLUDE 'ops_mpl_win_free.inc'
INCLUDE 'ops_mpl_win_shared_query.inc'

END MODULE OpsMod_MPLInterface
