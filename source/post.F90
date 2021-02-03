!P. Olivucci 2016
!Smith-Hess Panel Method
!Postprocessing routines
!--------------------------------------------------------------------------
MODULE post
USE input
USE airfoil
USE coeff
USE solver
IMPLICIT NONE
REAL, ALLOCATABLE :: vel(:), p(:), Cp(:)
REAL :: CL,CD, xcpmin, cpmin
CONTAINS
!--------------------------------------------------------------------------
SUBROUTINE draw()
!text output w profile discretization
INTEGER :: cou, cou1

OPEN(31, FILE='output/draw_profile.dat', form='formatted', status='replace')
DO cou=1,n+1
WRITE(31,'(4(2X,F24.12))') x(cou), y(cou)
ENDDO
 CLOSE(31)

OPEN(34, FILE='output/draw_contrp.dat', form='formatted', status='replace')
DO cou=1,n
WRITE(34,'(4(2X,F24.12))') xcp(cou), ycp(cou)
ENDDO
 CLOSE(34)

!text output w chord
OPEN(32, FILE='output/draw_chord.dat', form='formatted', status='replace')
DO cou=1,lin
WRITE(32,'(2(2X,F24.12))') xc(cou), yc(cou)
ENDDO
 CLOSE(32)

!text output w field quantities
!OPEN(36, FILE='output/draw_field.dat', form='formatted', status='replace')
OPEN(33, FILE='output/draw_field_l.dat', form='formatted', status='replace')
OPEN(35, FILE='output/draw_field_u.dat', form='formatted', status='replace')

DO cou=1,n
    !WRITE(36,'(4(2X,F12.5))') xcp(cou)/c, (vel(cou)/v), p(cou), Cp(cou)
    IF (cou <= n/2) THEN
    !WRITE(*,*) 'unten'
    WRITE(33,'(4(2X,F24.12))') xcp(cou)/c, (vel(cou)/v), p(cou), Cp(cou)
    ELSE
    !WRITE(*,*) 'oben'
    WRITE(35,'(4(2X,F24.12))') xcp(cou)/c, (vel(cou)/v), p(cou), Cp(cou)
    ENDIF
ENDDO

 !CLOSE(36)
 CLOSE(33)
 CLOSE(35)

END SUBROUTINE
!--------------------------------------------------------------------------
SUBROUTINE bernoulli()
!tan. velocity and pressure and coeffs
INTEGER :: cou, cou1
REAL :: co, co1, L, sum_a,Fx, Fy, D
REAL, ALLOCATABLE :: dl(:)

ALLOCATE(vel(n))
ALLOCATE(p(n))
ALLOCATE(Cp(n))
ALLOCATE(dl(n))

co1= 0.5d0*rho
co=(co1*v*v)
 cpmin=0

DO cou=1, n

    !tangential velocity
    sum_a=0
    vel(cou)=0
    DO cou1=1, n
    sum_a=sum_a+as(cou,cou1)
    vel(cou)=vel(cou) - a1(cou,cou1)*b(cou1)
    ENDDO
    vel(cou)=vel(cou) + v*(COS(alpha)*COS(theta(cou))+SIN(alpha)*SIN(theta(cou))) + b(n+1)*sum_a
    !WRITE(*,*) cou, vel(cou)

    !pressure by bernoulli
    Cp(cou) = 1 - vel(cou)*vel(cou)/(v*v)

    !look for minimal Cp
    IF (Cp(cou)<cpmin)THEN
    cpmin = Cp(cou)
    xcpmin = xcp(cou)/c
    ENDIF

    !lift/drag by pressure integration
    p(cou)=Cp(cou)*co
    dl(cou)=SQRT((x(cou+1)-x(cou))**2+(y(cou+1)-y(cou))**2)
    CL = CL - Cp(cou)*dl(cou)*COS(theta(cou))
    CD = CD - Cp(cou)*dl(cou)*SIN(theta(cou))

ENDDO

!SHOW
WRITE(*, *) "RESULTS:"
WRITE(*, '(A, E10.3)') " Lift Coefficient:    ",    CL
WRITE(*, '(A, E10.3)') " Drag Coefficient:    ",    CD
WRITE(*, '(A, E10.3)') " Lift/Drag ratio:     ",    CL/CD
WRITE(*, '(A, E10.3)') " Minimum of Cp:       ",    cpmin
WRITE(*, '(A, F5.3)')  " Position of min. Cp: ",    xcpmin
WRITE(*, *) "--------------------------------------------------"

END SUBROUTINE
!--------------------------------------------------------------------------
SUBROUTINE fieldprint()
OPEN(39, FILE='shpanel_output.dat', form='formatted', status='replace')
WRITE(39, '(A)') "Results:"
WRITE(39, '(A, E10.3)') "Lift Coefficient:    ",    CL
WRITE(39, '(A, E10.3)') "Drag Coefficient:    ",    CD
WRITE(39, '(A, E10.3)') "Lift/Drag ratio:     ",    CL/CD
WRITE(39, '(A, E10.3)') "Minimum of Cp:       ",    cpmin
WRITE(39, '(A, F5.3)')  "Position of min. Cp: ",    xcpmin
 CLOSE(39)
END SUBROUTINE
!--------------------------------------------------------------------------
END MODULE
