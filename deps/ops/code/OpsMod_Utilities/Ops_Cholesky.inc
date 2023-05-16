!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Solves the Linear equation UQ=V for Q where U is a symmetric positive definite
! matrix and U and Q are vectors of length N.  The method follows that in Golub
! and Van Loan although this is pretty standard.
!
! If U is not positive definite this will be detected by the program and flagged
! as an error.  U is assumed to be symmetric as only the upper triangle is in
! fact used.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_Cholesky (U,         &
                         V,         &
                         N,         &
                         Q,         &
                         ErrorCode)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    gen_warn,          &
    StatusOK,          &
    UseTrace

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)         :: n
REAL, INTENT(IN)            :: U(n,n)
REAL, INTENT(IN)            :: V(n)
REAL, INTENT(OUT)           :: Q(n)
INTEGER, INTENT(OUT)        :: ErrorCode

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_Cholesky"
REAL, PARAMETER             :: Tolerance = TINY (0.0) * 100.0
CHARACTER(len=80)           :: ErrorMessage  ! Message for gen_warn
INTEGER                     :: j
INTEGER                     :: k
REAL                        :: G(n,n)   ! The Cholesky Triangle Matrix
REAL                        :: X(n)     ! Temporary array used in calculating G

IF (UseTrace) CALL gen_trace_entry (RoutineName)

ErrorCode = StatusOK

! Determine the Cholesky triangle matrix.

DO j = 1, n
  X(j:n) = U(j:n,j)
  IF (j /= 1) THEN
    DO k = 1, j - 1
      X(j:n) = X(j:n) - G(j,k) * G(j:n,k)
    END DO
  END IF
  IF (X(j) <= Tolerance) THEN
    ErrorCode = 1
    Errormessage = 'U matrix is not positive definite'
    CALL gen_warn (RoutineName,  &
                   ErrorMessage)
    GOTO 9999
  END IF
  G(J:N,J) = X(J:N) / SQRT (X(J))
END DO

! Solve Gx=v for x by forward substitution

X = V
X(1) = X(1) / G(1,1)
DO j = 2, n
  X(j) = (X(j) - DOT_PRODUCT (G(j,1:j - 1), X(1:j - 1))) / G(j,j)
END DO

! Solve G^T.q=x for q by backward substitution

Q = x
Q(n) = Q(n) / G(n,n)
DO j = n - 1, 1, -1
  Q(j) = (Q(j) - DOT_PRODUCT (G(j + 1:n,j), Q(j + 1:n))) / G(j,j)
END DO

9999 CONTINUE

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_Cholesky
