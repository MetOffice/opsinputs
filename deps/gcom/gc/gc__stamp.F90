! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

SUBROUTINE gc__stamp()

USE gc__buildconst, ONLY:                                                      &
gc_version, gc_build_date, gc_descrip, gc_int_type, gc_real_type

IMPLICIT NONE

WRITE(6,*)
WRITE(6,*) '====================================================='
WRITE(6,*) 'GCOM Version ',                                                    &
 gc_version
WRITE(6,*)                                                                     &
 gc_descrip
WRITE(6,*) 'Using precision : ',                                               &
 gc_int_type , ' and ', gc_real_type
WRITE(6,*) 'Built at ',                                                        &
 gc_build_date
WRITE(6,*) '====================================================='
WRITE(6,*)

RETURN
END SUBROUTINE gc__stamp
