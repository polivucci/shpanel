!P. Olivucci 2015
!Smith-Hess Panel Method
!Code to draw a discretized profile.
!--------------------------------------------------------------------------
MODULE airfoil
USE input
IMPLICIT NONE
REAL, ALLOCATABLE :: x(:), y(:)
REAL, ALLOCATABLE :: ycp(:), xcp(:)
DOUBLE PRECISION, ALLOCATABLE :: ja(:)
REAL, ALLOCATABLE :: xs(:), ys(:), ts(:)
CONTAINS
!--------------------------------------------------------------------------
SUBROUTINE disc_foil()
!builds the discretized profile
INTEGER :: ind, ind1, ind2, m, slen, k
REAL :: delta2

slen = 2*lin-1

ALLOCATE (x(n+1))
ALLOCATE (y(n+1))
ALLOCATE (ycp(n))
ALLOCATE (xcp(n))
ALLOCATE (ja(lin-2))
ALLOCATE (xs(slen))
ALLOCATE (ys(slen))
ALLOCATE (ts(slen))


!builds an oriented camber line
DO ind=1,lin
xs(ind)=xc(lin-ind+1)
xs(ind+lin)=xc(ind+1)
ys(ind)=yc(lin-ind+1)
ys(ind+lin)=yc(ind+1)
ts(ind)=t(lin-ind+1)
ts(ind+lin)=t(ind+1)
ENDDO

!first point
x(1) = xs(1)
y(1) = ys(1)
!last point
x(n+1) = x(1)
y(n+1) = y(1)

!initializations

!derivative
DO ind1 = 2, (lin-1)
delta2 = ABS(xc(ind1+1) - xc(ind1-1))
ja(ind1) = ATAN2((-yc(ind1+1) + yc(ind1-1)),delta2)
ENDDO

!THICKNESS NORMAL TO CL
!unten
DO k = 2, lin
x(k) = xs(k) + ts(k)*SIN(ja(k))
y(k) = ys(k) - ts(k)*COS(ja(k))
ENDDO

!oben
DO k=lin+1, n
x(k) = xs(k) - ts(k)*SIN(-ja(k))
y(k) = ys(k) + ts(k)*COS(-ja(k))
ENDDO

!control points are computed
DO m = 1, n
xcp(m) = (x(m+1)+x(m))/2
ycp(m) = (y(m+1)+y(m))/2
ENDDO


END SUBROUTINE
!--------------------------------------------------------------------------
END MODULE
