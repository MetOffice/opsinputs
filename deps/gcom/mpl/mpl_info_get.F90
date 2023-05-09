! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Info_get()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Retrieves a value associated with a key from an info object
!     *
!     *  Output:  val, len1, flag, error
!     *
!     ******************************************************************

SUBROUTINE mpl_info_get( info, key, len1, val, flag, ERROR )

USE gc_kinds_mod, ONLY: gc_int_kind, gc_log_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Log_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  info,                                                                        &
  len1,                                                                        &
  ERROR

LOGICAL (KIND=gc_log_kind) :: flag

CHARACTER(LEN=*) :: key, val

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_info,                                                                      &
  l_len,                                                                       &
  l_error

LOGICAL (KIND=mpl_log_kind) :: l_flag

!=======================================================================

#if defined(MPI_SRC)
l_info  = info

CALL MPI_Info_get(l_info, key, l_len, val, l_flag, l_error)

len1  = l_len
flag  = l_flag
ERROR = l_error
#endif

RETURN

END SUBROUTINE mpl_info_get
