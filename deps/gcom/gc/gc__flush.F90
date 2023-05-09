! *****************************COPYRIGHT*******************************
! (C) CROWN COPYRIGHT , MET OFFICE, ALL RIGHTS RESERVED.
! *****************************COPYRIGHT*******************************

!=======================================================================
!  THIS IS AN INTERNAL ROUTINE TO BE USED WITHIN THE GC INTERFACE ONLY.
!  IT IS A WRAPPER FOR THE FLUSH CALL FOUND IN MANY COMPILERS.
!=======================================================================

SUBROUTINE gc__flush(lunit)

! If not set we won't use any of the subroutine
#if ! defined(GC__FLUSHUNIT6)

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE
!     The subroutine's arguments, whatever the compiler
INTEGER (KIND=gc_int_kind), INTENT(IN)  :: lunit

! else GC__FLUSHUNIT6 defined
#else

!     Required if using the NAG compiler
#if defined(LINUX_NAG_COMPILER)
USE f90_unix_io,ONLY:FLUSH
#endif

USE gc_kinds_mod, ONLY:                                                        &
#if defined(LINUX_NAG_COMPILER) || defined(_X1) || defined(XD1) \
|| defined(XT3)
    gc_integer32,                                                              &
#endif
    gc_int_kind

IMPLICIT NONE

!     The subroutine's arguments, whatever the compiler
INTEGER (KIND=gc_int_kind), INTENT(IN)  :: lunit

INTEGER (KIND=gc_int_kind)              :: icode

!     If on NAG, X1, XD1 or XT3 require two 32 bit arguments to flush.
#if defined(LINUX_NAG_COMPILER) || defined(_X1) || defined(XD1) \
|| defined(XT3)
INTEGER (KIND=gc_integer32) :: icode1
INTEGER (KIND=gc_integer32) :: lunit1
lunit1 = lunit
CALL FLUSH(lunit1,icode1)
icode = icode1

!     If on the T3E we require two 64 bit arguments
#elif defined(T3E)
CALL FLUSH(lunit,icode)

!     All others use one 64 bit argument
#elif defined(IBM) || defined(GNU)
FLUSH(lunit)
#else
FLUSH(lunit)
#endif

#endif
END SUBROUTINE gc__flush
