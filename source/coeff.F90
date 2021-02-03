!P. Olivucci 2015
!Smith-Hess Panel Method
!Algorithm to build the main algebraic system
!--------------------------------------------------------------------------
MODULE coeff
USE input
USE airfoil
USE const
IMPLICIT NONE
!INTEGER :: i,j
REAL, ALLOCATABLE :: theta(:), b(:)
REAL, ALLOCATABLE :: a(:,:), a1(:,:), as(:,:)

CONTAINS
!--------------------------------------------------------------------------
SUBROUTINE init()
!allocates and initializes matrix
INTEGER :: k
INTEGER :: i,j

ALLOCATE (a(n+1, n+1))
ALLOCATE (a1(n,n))
ALLOCATE (as(n,n))
ALLOCATE (theta(n))

!angles
DO k=1,n
theta(k) = ATAN2((y(k+1)-y(k)),(x(k+1)-x(k)))
ENDDO

DO i = 1, n+1
    DO j = 1, n+1
    a(i,j) = 0.0d0
    ENDDO
ENDDO

DO i = 1, n
    DO j = 1, n
    a1(i,j) = 0.0d0
    as(i,j) = 0.0d0
    ENDDO
ENDDO

END SUBROUTINE
!--------------------------------------------------------------------------
SUBROUTINE coeffs()
!populates the main matrix (influence coefficients)
INTEGER :: i,j
INTEGER :: k
REAL :: lo, beta, sintheta, costheta

!SOURCES AND VORTEX CONTRIBUTION
DO i = 1, n
DO j = 1, n
    IF (i==j) THEN
    !auto-contrib
    a(i,j) = 0.5d0
    as(i,j) = a(i,j)
    ELSE
    lo = LOG(r(i,j+1)/r(i,j))
    beta = bet(i,j)
    sintheta=SIN(theta(i))*COS(theta(j))-COS(theta(i))*SIN(theta(j))
    costheta=COS(theta(i))*COS(theta(j))+SIN(theta(i))*SIN(theta(j))
    !normal source contribution:
    a(i,j) = pi2inv()*(sintheta*lo+costheta*beta)
    !normal vortex contribution:
    a1(i,j) = pi2inv()*(costheta*lo-sintheta*beta)
    as(i,j) = a(i,j)
    ENDIF
!normal vortex contr (last column)
a(i,n+1) = a(i,n+1) + a1(i,j)
ENDDO
ENDDO

!KUTTA
DO k=1,n
!tangential source contr (last row)
a(n+1, k) = -a1(1, k) - a1(n, k)
!tangential vortex contr (last element)
a(n+1, n+1) = a(n+1, n+1) + a(1,k) + a(n,k)
ENDDO

END SUBROUTINE
!--------------------------------------------------------------------------
SUBROUTINE vec()
!populates the right-side vector
INTEGER :: i,j

ALLOCATE (b(n+1))

DO i = 1, n
!b(i) = -v*SIN(alpha-theta(i))
b(i) = -v*(SIN(alpha)*COS(theta(i))-COS(alpha)*SIN(theta(i)))
!WRITE(*,*) b(i)
ENDDO

!b(n+1) = -v*(COS(alpha)*COS(theta(n))+SIN(alpha)*SIN(theta(n)) + COS(alpha)*COS(theta(1))+SIN(alpha)*SIN(theta(1)))
b(n+1) = -v*(COS(theta(1))+COS(theta(n)))*COS(alpha)-(SIN(theta(1))+SIN(theta(n)))*SIN(alpha);
!WRITE(*,*) b(n+1)

END SUBROUTINE
!--------------------------------------------------------------------------
!Auxiliary definitions:
!--------------------------------------------------------------------------
REAL FUNCTION r(k,l)
!distance vector
INTEGER :: k, l

  r = SQRT((xcp(k) - x(l))**2 + (ycp(k) - y(l))**2)

END FUNCTION r
!--------------------------------------------------------------------------
REAL FUNCTION bet(k,l)
INTEGER :: k, l
REAL :: x1,x2,y1,y2, arg1, arg2

    x1=(xcp(k) - x(l+1))
    x2=(xcp(k) - x(l))
    y1=(ycp(k) - y(l+1))
    y2=(ycp(k) - y(l))
    arg1 = (y1*x2 - y2*x1)
    arg2 = (x1*x2 + y1*y2)
    !WRITE(*,*) arg
    bet = ATAN2(arg1, arg2)
    !bet = ATAN(arg1/arg2)

END FUNCTION bet
!--------------------------------------------------------------------------
END MODULE
