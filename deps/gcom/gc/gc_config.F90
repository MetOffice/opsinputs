! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_config (mxproc, mxcoll, mxpt2pt, intf)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Return information about the GC configuration.
!     *
!     * Output:
!     *  MXPROC    - maximum numbers of processors compiled into the
!     *              interface - deprecated
!     *  MXCOLL    - maximum number of elements for collective
!     *              operations  - deprecated
!     *  MXPT2PT   - maximum number of elements for point to point
!     *              operations - deprecated
!     *  INTF      - name of interface selected at compile time
!     *
!     * NOTES:
!     *
!     ******************************************************************

USE gcom_mod, ONLY:                                                            &
    gc_none

USE gc__buildconst, ONLY:                                                      &
    gc_version, gc_build_date, gc_descrip

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

INTEGER (KIND=gc_int_kind) :: mxproc, mxcoll, mxpt2pt
CHARACTER(LEN=*)           :: intf


mxcoll = 0            ! Deprecated
mxproc = 0            ! Deprecated
mxpt2pt = gc_none     ! Deprecated
intf = 'GCOM Version ' //                                                      &
  gc_version //                                                                &
  ' built at ' //                                                              &
  gc_build_date //                                                             &
  ' Interface: ' //                                                            &
#if defined(PREC_32B)
     gc_descrip //                                                             &
  ' 32'
#else
gc_descrip //                                                                  &
' 64'
#endif
END SUBROUTINE gc_config
