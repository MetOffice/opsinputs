! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

#include "gcg_prolog.h"

SUBROUTINE gcg_ralltoalle(                                                     &
   send_array, send_map, n_items_to_send, sarr_len,                            &
   recv_array, recv_map, n_items_to_recv, rarr_len,                            &
   gid, flag, istat)
!     ******************************************************************
!     * Purpose:
!     *
!     *  An extended all-to-all permutation of real data between the
!     *  processors in a group. One processor may send several items
!     *  of data to another processor. Similarly, a processor may
!     *  receive several items from another processor. This routine
!     *  may also be used for 1-to-all, all-to-1 and some-to-some
!     *  permutations.
!     *
!     * Input:
!     *  SEND_ARRAY       - array containing all data to be sent
!     *  SEND_MAP         - a map containing the following information
!     *                     for each of the items to be sent:
!     *                        1 - destination processor
!     *                        2 - base address in SEND_ARRAY
!     *                        3 - number of elements in this item
!     *                        4 - stride between the elements in
!     *                            SEND_ARRAY
!     *                        5 - element length
!     *                        6 - base address in the receiving
!     *                            processor's RECV_ARRAY
!     *                        7 - stride between the elements in the
!     *                            receiving processor's RECV_ARRAY
!     *  N_ITEMS_TO_SEND  - total number of items to be sent from this
!     *                     processor
!     *  SARR_LEN         - length of SEND_ARRAY
!     *  RECV_MAP         - a map containing the following information
!     *                     for each of the items to be received:
!     *                        1 - source processor
!     *                        2 - base address in RECV_ARRAY
!     *                        3 - number of elements in this item
!     *                        4 - stride between the elements in
!     *                            RECV_ARRAY
!     *                        5 - element length
!     *                        6 - base address in the sending
!     *                            processor's SEND_ARRAY
!     *                        7 - stride between the elements in the
!     *                            sending processor's SEND_ARRAY
!     *  N_ITEMS_TO_RECV  - total number of items to be received at this
!     *                     processor
!     *  RARR_LEN         - length of RECV_ARRAY
!     *  GID              - processor group ID
!     *  FLAG             - Not currently used. Expected to be used
!     *                     to characterize the permutation
!     *
!     * Output:
!     *  RECV_ARRAY       - array containing the received data, in
!     *                     the structure defined by RECV_MAP.
!     *  ISTAT            - Status variable. 0 is OK, refer to the
!     *                     header files for nonzero status codes
!     *
!     ******************************************************************

#if defined(MPI_SRC)
USE gcom_mod, ONLY:                                                            &
    gc_alltoall_version,                                                       &
    gc_alltoall_orig
#endif

USE gc_kinds_mod, ONLY: gc_int_kind, gc_real_kind, gc_log_kind

IMPLICIT NONE

INTEGER (KIND=gc_int_kind) :: n_items_to_send, sarr_len,                       &
                              send_map(7,n_items_to_send)
INTEGER (KIND=gc_int_kind) :: n_items_to_recv, rarr_len,                       &
                              recv_map(7,n_items_to_recv)
INTEGER (KIND=gc_int_kind) :: gid, flag, istat
INTEGER (KIND=gc_int_kind) :: opt
REAL (KIND=gc_real_kind)   :: send_array(sarr_len),                            &
                              recv_array(rarr_len)

#if defined(SERIAL_SRC)
INTEGER (KIND=gc_int_kind) :: i,j,k
#endif

#if defined(SERIAL_SRC)
DO k=1,n_items_to_send
  DO j=1,send_map(3,k)
    DO i=1,send_map(5,k)
      recv_array(i-1+send_map(6,k)+                                            &
                 (j-1)*send_map(7,k))=                                         &
      send_array(i-1+send_map(2,k)+                                            &
                 (j-1)*send_map(4,k))
    END DO
  END DO
END DO
#else
CALL gc_getopt(gc_alltoall_version, opt, istat)
IF (opt == gc_alltoall_orig) THEN
  CALL gcg__ralltoalle(                                                        &
     send_array, send_map, n_items_to_send, sarr_len,                          &
     recv_array, recv_map, n_items_to_recv, rarr_len,                          &
     gid, flag, istat)
ELSE
  CALL gcg__ralltoalle_multi(                                                  &
     send_array, send_map, n_items_to_send, sarr_len,                          &
     recv_array, recv_map, n_items_to_recv, rarr_len,                          &
     gid, flag, istat)
END IF
#endif

RETURN
END SUBROUTINE gcg_ralltoalle


SUBROUTINE gcg__ralltoalle(                                                    &
   send_array, send_map, n_items_to_send, sarr_len,                            &
   recv_array, recv_map, n_items_to_recv, rarr_len,                            &
   gid, flag, istat)

USE mpl, ONLY:                                                                 &
    mpl_status_size

#if defined(MPI_SRC)
USE gc_globals_mod, ONLY:                                                      &
    gc__mpi_maxtag
#endif

USE gc_kinds_mod, ONLY:                                                        &
#if defined(MPI_SRC)
    gc_log_kind,                                                               &
#endif
    gc_int_kind,                                                               &
    gc_real_kind

IMPLICIT NONE

#include "gc_constants.h"

INTEGER (KIND=gc_int_kind) :: n_items_to_send, sarr_len,                       &
                              send_map(7,n_items_to_send)
INTEGER (KIND=gc_int_kind) :: n_items_to_recv, rarr_len,                       &
                              recv_map(7,n_items_to_recv)
INTEGER (KIND=gc_int_kind) :: gid, max_buf_size, flag, istat
REAL (KIND=gc_real_kind)   :: send_array(sarr_len),                            &
                              recv_array(rarr_len)

INTEGER (KIND=gc_int_kind) :: i, j, k, l, length, lbase, lstride,              &
                              rbase, rstride, tag

#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind) :: me

#include "gc_functions.h"

INTEGER (KIND=gc_int_kind) :: this_type  ! Derived MPI data type for a send/recv
INTEGER (KIND=gc_int_kind) :: llength    ! Size of a single block of data
INTEGER (KIND=gc_int_kind) :: lblocks    ! Number of blocks of data
INTEGER (KIND=gc_int_kind) :: send_handle(n_items_to_send)  ! Handles for sent data
INTEGER (KIND=gc_int_kind) :: recv_handle(n_items_to_recv)  ! Handles for received data
INTEGER (KIND=gc_int_kind) :: send_status(mpl_status_size,                     &
                                          n_items_to_send)  ! Status (sends)
INTEGER (KIND=gc_int_kind) :: recv_status(mpl_status_size,                     &
                                          n_items_to_recv)  ! Status (recvs)
INTEGER (KIND=gc_int_kind) :: n_send_handles  ! Number of messages sent
INTEGER (KIND=gc_int_kind) :: n_recv_handles  ! Number of messages received

LOGICAL (KIND=gc_log_kind) :: send_completed  ! All the outstanding sends have completed
LOGICAL (KIND=gc_log_kind) :: recv_completed  ! All the outstanding receives have completed

EXTERNAL mpl_comm_rank
#endif

istat = gc__ok

#if defined(MPI_SRC)
CALL mpl_comm_rank(gid, me, istat)

! Loop over all the items I have to receive
n_recv_handles=0
DO j=1,n_items_to_recv

  IF (recv_map(1,j)  /=  me) THEN  ! Only receive if the message
                                   ! is not from myself
    lbase=recv_map(2,j)
    lblocks=recv_map(3,j)
    lstride=recv_map(4,j)
    llength=recv_map(5,j)

    ! Get an MPI derived type which describes this data structure
    CALL gc__get_mpi_type(llength,lblocks,lstride,this_type)

    tag=MOD(lbase,INT(gc__mpi_maxtag,gc_int_kind))
    n_recv_handles=n_recv_handles+1
    CALL mpl_irecv(recv_array(lbase),1_gc_int_kind,                            &
                   this_type,recv_map(1,j),                                    &
                   tag,gid,                                                    &
                   recv_handle(n_recv_handles),istat)
    IF (istat  /=  0) RETURN

  END IF
END DO ! DO J=1,N_ITEMS_TO_RECV

! Loop over all the items I have to send
n_send_handles=0
DO j=1,n_items_to_send

  lbase=send_map(2,j)
  lblocks=send_map(3,j)
  lstride=send_map(4,j)
  llength=send_map(5,j)

  rbase=send_map(6,j)
  rstride=send_map(7,j)

  IF (send_map(1,j)  ==  me) THEN ! Sending to myself
    DO i=1,lblocks
      DO k=1,llength
        recv_array(rbase + (i-1)*rstride + k-1) =                              &
        send_array(lbase + (i-1)*lstride + k-1)
      END DO
    END DO
  ELSE ! Sending to another processor

    CALL gc__get_mpi_type(llength,lblocks,lstride,this_type)

    tag=MOD(rbase,INT(gc__mpi_maxtag,gc_int_kind))
    n_send_handles=n_send_handles+1
    CALL mpl_isend(send_array(lbase),1_gc_int_kind,                            &
                  this_type,send_map(1,j),                                     &
                   tag,gid,                                                    &
                   send_handle(n_send_handles),istat)
    IF (istat  /=  0) RETURN
  END IF
END DO
! Now all the sends and receives have been initiated, we
! can just sit back and wait for them to complete

send_completed=.FALSE.
recv_completed=.FALSE.
DO  ! Spin loop waiting for completion
  IF ((send_completed) .AND. (recv_completed)) EXIT

  IF (.NOT. send_completed) THEN
    CALL mpl_testall(n_send_handles,send_handle,                               &
                     send_completed,send_status,istat)
  END IF

  IF (.NOT. recv_completed) THEN
    CALL mpl_testall(n_recv_handles,recv_handle,                               &
                     recv_completed,recv_status,istat)
  END IF
END DO

#endif

RETURN
END SUBROUTINE gcg__ralltoalle
