!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Replace intrinsic MATMUL with call to ESSL routine DGEMUL.
!-------------------------------------------------------------------------------
MODULE GenMod_Matmul

INTERFACE Gen_MATMUL
  MODULE PROCEDURE Gen_MATMUL_MM
  MODULE PROCEDURE Gen_MATMUL_MV
  MODULE PROCEDURE Gen_MATMUL_VM
  MODULE PROCEDURE Gen_MATMUL_VV
END INTERFACE Gen_MATMUL

CONTAINS

FUNCTION Gen_MATMUL_MM (A, &
                        B) RESULT (C)

IMPLICIT NONE

! Function arguments:
REAL, INTENT (IN) :: A(:,:)
REAL, INTENT (IN) :: B(:,:)

! Function result:
REAL              :: C(SIZE(A,1),SIZE(B,2))

! Local declarations:
#if defined(MATMUL_ESSL)
INTEGER*4         :: ARG2
INTEGER*4         :: ARG5
INTEGER*4         :: ARG8
INTEGER*4         :: ARG9
INTEGER*4         :: ARG10
INTEGER*4         :: ARG11

ARG2 = SIZE (A, DIM = 1)
ARG5 = SIZE (B, DIM = 1)
ARG8 = SIZE (A, DIM = 1)
ARG9 = SIZE (A, DIM = 1)
ARG10 = SIZE (A, DIM = 2)
ARG11 = SIZE (B, DIM = 2)

CALL DGEMUL (A,     &
             ARG2,  &
             'N',   &
             B,     &
             ARG5,  &
             'N',   &
             C,     &
             ARG8,  &
             ARG9,  &
             ARG10, &
             ARG11)

#elif defined(MATMUL_BYHAND)
INTEGER           :: I
INTEGER           :: J
INTEGER           :: K

C(:,:) = 0.0
DO i = 1, SIZE (A, DIM = 1)
  DO j = 1, SIZE (B, DIM = 2)
    DO k = 1, SIZE (A, DIM = 2)
      C(i,j) = C(i,j) + A(i,k) * B(k,j)
    END DO
  END DO
END DO

#else
C = MATMUL (A, B)
#endif

END FUNCTION Gen_MATMUL_MM

FUNCTION Gen_MATMUL_MV (A, &
                        B) RESULT (C)

IMPLICIT NONE

! Function arguments:
REAL, INTENT (IN) :: A(:,:)
REAL, INTENT (IN) :: B(:)

! Function result:
REAL              :: C(SIZE (A, DIM = 1))

! Local declarations:
#if defined(MATMUL_BYHAND)
INTEGER           :: I
INTEGER           :: J

C(:) = 0.0
DO i = 1, SIZE (A, DIM = 1)
  DO j = 1, SIZE (A, DIM = 2)
    C(i) = C(i) + A(i,j) * B(j)
  END DO
END DO

#else
C = MATMUL (A, B)
#endif

END FUNCTION Gen_MATMUL_MV

FUNCTION Gen_MATMUL_VM (A,B) RESULT (C)

  IMPLICIT NONE

  REAL, INTENT (IN), DIMENSION (:) :: A
  REAL, INTENT (IN), DIMENSION (:,:) :: B
  REAL,              DIMENSION (SIZE(B,2)) :: C

#if defined(MATMUL_BYHAND)
  INTEGER :: I,J
  C(:) = 0.0
  DO i=1,SIZE(B,2)
    DO j=1,SIZE(A)
      C(i)=C(i)+A(j)*B(j,i)
    END DO
  END DO

#else
  C=MATMUL(A,B)
#endif

END FUNCTION Gen_MATMUL_VM

FUNCTION Gen_MATMUL_VV (A,B) RESULT (C)

  IMPLICIT NONE

  REAL, INTENT (IN), DIMENSION (:) :: A
  REAL, INTENT (IN), DIMENSION (:) :: B
  REAL,              DIMENSION (SIZE(A),SIZE(B)) :: C

  INTEGER :: I,J

  ! Tensor product

  DO J = 1,SIZE(B)
    DO I = 1,SIZE(A)
      C(I,J) = A(I) * B(J)
    END DO
  END DO

END FUNCTION Gen_MATMUL_VV

END MODULE GenMod_Matmul
