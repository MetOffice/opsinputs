! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

#include "gcg_prolog.h"

SUBROUTINE gcg_ralltoalle_multi(                                               &
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

USE gc_kinds_mod, ONLY: gc_int_kind, gc_real_kind

IMPLICIT NONE

INTEGER (KIND=gc_int_kind) :: n_items_to_send, sarr_len,                       &
                              send_map(7,n_items_to_send)
INTEGER (KIND=gc_int_kind) :: n_items_to_recv, rarr_len,                       &
                              recv_map(7,n_items_to_recv)
INTEGER (KIND=gc_int_kind) :: gid, flag, istat
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
CALL gcg__ralltoalle_multi(                                                    &
   send_array, send_map, n_items_to_send, sarr_len,                            &
   recv_array, recv_map, n_items_to_recv, rarr_len,                            &
   gid, flag, istat)
#endif

RETURN
END SUBROUTINE gcg_ralltoalle_multi


SUBROUTINE gcg__ralltoalle_multi(                                              &
   send_array, send_map, n_items_to_send, sarr_len,                            &
   recv_array, recv_map, n_items_to_recv, rarr_len,                            &
   gid, flag, istat)

USE mpl, ONLY:                                                                 &
    mpl_status_size,                                                           &
    mpl_real

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

INTEGER (KIND=gc_int_kind) :: i, j, k, b, l, sbase,                            &
                              sstride, rbase, rstride, tag

#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind) :: me
INTEGER (KIND=gc_int_kind) :: nproc
INTEGER (KIND=gc_int_kind) :: n_handles
INTEGER (KIND=gc_int_kind) :: ierror
INTEGER (KIND=gc_int_kind) :: SOURCE
INTEGER (KIND=gc_int_kind) :: dest
INTEGER (KIND=gc_int_kind) :: pos

REAL (KIND=gc_real_kind), ALLOCATABLE   :: send_data(:)
REAL (KIND=gc_real_kind), ALLOCATABLE   :: recv_data(:)

#include "gc_functions.h"

INTEGER (KIND=gc_int_kind) :: this_type  ! Derived MPI data type for a send/recv
INTEGER (KIND=gc_int_kind) :: length    ! Size of a single block of data
INTEGER (KIND=gc_int_kind) :: blocks    ! Number of blocks of data

INTEGER (KIND=gc_int_kind), ALLOCATABLE :: recv_len(:)
INTEGER (KIND=gc_int_kind), ALLOCATABLE :: send_len(:)
INTEGER (KIND=gc_int_kind), ALLOCATABLE :: handles(:)
INTEGER (KIND=gc_int_kind), ALLOCATABLE :: pos1(:)
INTEGER (KIND=gc_int_kind), ALLOCATABLE :: pos2(:)
INTEGER (KIND=gc_int_kind), ALLOCATABLE :: STAT(:,:)

LOGICAL (KIND=gc_log_kind) :: send_completed  ! All the outstanding sends have completed
LOGICAL (KIND=gc_log_kind) :: recv_completed  ! All the outstanding receives have completed

EXTERNAL  mpl_comm_rank
EXTERNAL  mpl_comm_size
#endif

istat = gc__ok

#if defined(MPI_SRC)
CALL mpl_comm_rank(gid, me, istat)
CALL mpl_comm_size(gid, nproc, istat)
ALLOCATE(recv_len(0:nproc-1))
ALLOCATE(send_len(0:nproc-1))
ALLOCATE(handles(2*(nproc-1)))
ALLOCATE(pos1(0:nproc-1))
ALLOCATE(pos2(0:nproc-1))

! Loop over all the items I have to receive

recv_len(:) = 0
DO j=1,n_items_to_recv
  SOURCE=recv_map(1,j)
  IF (SOURCE /= me) THEN
    blocks=recv_map(3,j)
    length=recv_map(5,j)
    IF (blocks * length > 0) THEN
      recv_len(SOURCE)=recv_len(SOURCE)+1+blocks*length
    END IF
  END IF
END DO
ALLOCATE (recv_data(SUM(recv_len(:))))

pos=1
n_handles=0
DO i=0, nproc-1
  IF (recv_len(i) > 0) THEN
    tag=me*nproc+i
    tag=MOD(tag,INT(gc__mpi_maxtag,gc_int_kind))
    n_handles=n_handles+1
    CALL mpl_irecv(recv_data(pos),recv_len(i),                                 &
      mpl_real,i,                                                              &
      tag,gid,                                                                 &
      handles(n_handles),istat)
    IF (istat  /=  0) RETURN
    pos=pos+recv_len(i)
  END IF
END DO

! Loop over all the items I have to send

send_len(:) = 0
DO j=1,n_items_to_send
  dest=send_map(1,j)
  IF (dest /= me) THEN
    blocks=send_map(3,j)
    length=send_map(5,j)
    IF (blocks * length > 0) THEN
      send_len(dest)=send_len(dest)+1+blocks*length
    END IF
  END IF
END DO

ALLOCATE (send_data(SUM(send_len(:))))

pos1(0) = 1
pos2(0) = 1
DO i=1, nproc-1
  pos1(i) = pos1(i-1) + send_len(i-1)
  pos2(i) = pos1(i)
END DO

DO j=1,n_items_to_send
  dest=send_map(1,j)
  i       = dest
  pos     = pos2(i)
  sbase   = send_map(2,j)
  blocks  = send_map(3,j)
  sstride = send_map(4,j)
  length  = send_map(5,j)
  rbase   = send_map(6,j)
  rstride = send_map(7,j)
  IF (blocks * length > 0) THEN
    IF (dest == me) THEN ! Sending to myself
      DO b=1,blocks
        DO k=1,length
          recv_array(rbase + (b-1)*rstride + k-1) =                            &
            send_array(sbase + (b-1)*sstride + k-1)
        END DO
      END DO
    ELSE
      ! Store RBASE in send data in case maps have different
      ! orders on sender and recipient, this makes it
      ! unambiguous
      send_data(pos)=REAL(rbase)
      pos=pos+1
      DO b=1,blocks
        DO k=1,length
          send_data(pos)=                                                      &
            send_array(sbase + (b-1)*sstride + k-1)
          pos=pos+1
        END DO
      END DO
    END IF
  END IF

  pos2(i) = pos
END DO

DO i=0, nproc-1
  IF (send_len(i) > 0) THEN

    tag=i*nproc+me
    tag=MOD(tag,INT(gc__mpi_maxtag,gc_int_kind))
    n_handles=n_handles+1
    CALL mpl_isend(send_data(pos1(i)),send_len(i),                             &
      mpl_real,i,                                                              &
      tag,gid,                                                                 &
      handles(n_handles),istat)
    IF (istat  /=  0) RETURN
  END IF
END DO
! Now all the sends and receives have been initiated, we
! can just sit back and wait for them to complete

ALLOCATE(STAT(mpl_status_size,n_handles))
CALL mpl_waitall(n_handles,handles,STAT,ierror)
DEALLOCATE(STAT)

pos=1
DO i=0,nproc-1
  DO j=1,n_items_to_recv
    SOURCE=recv_map(1,j)
    IF (SOURCE == i .AND. SOURCE /= me) THEN
      blocks=recv_map(3,j)
      rstride=recv_map(4,j)
      length=recv_map(5,j)
      rbase=INT(recv_data(pos))
      IF (blocks * length > 0) THEN
        pos=pos+1
        DO b=1,blocks
          DO k=1,length
            recv_array(rbase+(b-1)*rstride+k-1) = recv_data(pos)
            pos=pos+1
          END DO
        END DO
      END IF
    END IF
  END DO
END DO

DEALLOCATE(recv_len)
DEALLOCATE(send_len)
DEALLOCATE(recv_data)
DEALLOCATE(send_data)
DEALLOCATE(handles)
DEALLOCATE(pos1)
DEALLOCATE(pos2)

#endif

RETURN
END SUBROUTINE gcg__ralltoalle_multi
