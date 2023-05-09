! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

!=======================================================================
!  This is an INTERNAL routine to be used within the GC interface ONLY.
!=======================================================================

#include "gc_prolog.h"

SUBROUTINE gc__get_mpi_type(length,blocks,stride,this_type)

!     Creates a new MPI derived datatype for the structure described
!     by LENGTH,BLOCKS,STRIDE or returns an existing one

USE mpl, ONLY:                                                                 &
    mpl_byte

#if defined(MPI_SRC)
USE gc_globals_mod, ONLY:                                                      &
    mpi_types_array,                                                           &
    max_mpi_types,                                                             &
    n_mpi_types_defined,                                                       &
    mpi_type_seq

USE gc__buildconst, ONLY: gc__rsize
#endif

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

INTEGER (KIND=gc_int_kind) :: length      ! IN : Number of words in each data block
INTEGER (KIND=gc_int_kind) :: blocks      ! IN : Number of blocks, each of length LENGTH
INTEGER (KIND=gc_int_kind) :: stride      ! IN : Stride between the start of each block
INTEGER (KIND=gc_int_kind) :: this_type   ! OUT : MPI reference ID for the new derived type

INTEGER (KIND=gc_int_kind) :: istat       ! MPI return code
INTEGER (KIND=gc_int_kind) :: type_count  ! Counter through existing types
INTEGER (KIND=gc_int_kind) :: type_index  ! Index into MPI_TYPES_ARRAY
INTEGER (KIND=gc_int_kind) :: best_seq_val
INTEGER (KIND=gc_int_kind) :: indx
INTEGER (KIND=gc_int_kind) :: i

type_index=-1
#if defined(MPI_SRC)
mpi_type_seq=mpi_type_seq+1
type_count=0
DO
  type_count=type_count+1
  IF (type_count > n_mpi_types_defined) EXIT

  IF ( ( mpi_types_array(2,type_count) == length) .AND.                        &
       ( mpi_types_array(3,type_count) == blocks) .AND.                        &
       ( mpi_types_array(4,type_count) == stride) ) THEN
    ! Found an existing type which matches
    type_index=type_count
    EXIT
  END IF
END DO

IF (type_index  ==  -1) THEN
  ! Need to add a new type
  IF (n_mpi_types_defined >= max_mpi_types) THEN
    ! The table is filled up, so we must search
    ! for least recently used entry
    best_seq_val=mpi_type_seq+1
    indx=1
    DO i=1,n_mpi_types_defined
      IF (mpi_types_array(5,i)  <   best_seq_val) THEN
        best_seq_val=mpi_types_array(5,i)
        indx=i
      END IF
    END DO
  ELSE
    n_mpi_types_defined=n_mpi_types_defined+1
    indx=n_mpi_types_defined
  END IF
  type_index=indx

  ! And put the information into the table
  mpi_types_array(2,type_index)=length
  mpi_types_array(3,type_index)=blocks
  mpi_types_array(4,type_index)=stride

  CALL mpl_type_vector(blocks,gc__rsize*length,                                &
                       gc__rsize*stride,                                       &
                       mpl_byte,mpi_types_array(1,type_index),                 &
                       istat)
  CALL mpl_type_commit(mpi_types_array(1,type_index),istat)

END IF

! Finally update the sequence number for this entry - this
! enables us to spot which entries are rarely used and can
! be used for new entries if the table is full

mpi_types_array(5,type_index)=mpi_type_seq
this_type=mpi_types_array(1,type_index)
#endif

RETURN
END SUBROUTINE gc__get_mpi_type

SUBROUTINE gc__free_mpi_types()

!     Frees MPI internals of the derived data types
#if defined(MPI_SRC)
USE gc_globals_mod, ONLY:                                                      &
    n_mpi_types_defined,                                                       &
    mpi_types_array,                                                           &
    mpi_type_seq
#endif

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

INTEGER (KIND=gc_int_kind) :: i,istat

#if defined(MPI_SRC)
DO i=1,n_mpi_types_defined
  CALL mpl_type_free(mpi_types_array(1,i),istat)
END DO
n_mpi_types_defined=0
mpi_type_seq=0
#endif

RETURN
END SUBROUTINE gc__free_mpi_types
