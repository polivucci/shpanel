!P. Olivucci 2016
!Smith-Hess Panel Method
!Input manager
!--------------------------------------------------------------------------
MODULE input
USE const
IMPLICIT NONE
INTEGER :: n, lin
REAL :: c, v, alpha_deg, alpha, rho
REAL :: fmax, tmax
REAL, ALLOCATABLE :: xc(:), yc(:), t(:)
CONTAINS
!--------------------------------------------------------------------------
INTEGER FUNCTION lines(funit)
!Determines the number of lines in the input file
INTEGER :: funit, nlines
OPEN(funit)

nlines = 0
DO
    READ (funit,*, END=10)
    nlines = nlines + 1
END DO
10 CLOSE (funit)
lines = nlines
END FUNCTION lines
!--------------------------------------------------------------------------
SUBROUTINE readinput()
!Reads input from controls.dat (no. of panels, incoming vel...)

CHARACTER :: a*80

1000 format(a,80x) 
OPEN(24,FILE='shpanel_parameters.dat',STATUS='unknown',FORM='formatted')
READ(24,1000) a
READ(24,*) c
READ(24,*) v
READ(24,*) fmax
READ(24,*) tmax
READ(24,*) alpha_deg
READ(24,*) n
READ(24,*) rho
CLOSE(24)

alpha = alpha_deg*(pi()/180)

WRITE(*, *) "--------------------------------------------------"
WRITE(*, *) "shpanel - 2D hydrofoil solver"
WRITE(*, *) "--------------------------------------------------"
WRITE(*, *) "INPUT DATA:"
WRITE(*, '(A, F6.3)') " Max. camber:     ", fmax
WRITE(*, '(A, F6.3)') " Max. thickness:  ", tmax
WRITE(*, '(A, F6.3)') " Angle of Attack: ", alpha_deg
WRITE(*, '(A, F6.2)') " Inflow Velocity: ", v
WRITE(*, '(A, F7.1)') " Fluid density:   ", rho
WRITE(*, *) "--------------------------------------------------"


END SUBROUTINE
!--------------------------------------------------------------------------
SUBROUTINE input_dat()
!Reads input from matlab splines output
INTEGER :: counter

OPEN(54,FILE='geometry/spline_line.dat', action='read')
lin = lines(54)


ALLOCATE (xc(lin))
ALLOCATE (yc(lin))
ALLOCATE (t(lin))

!reading routine

OPEN(52,FILE='geometry/spline_thick.dat', action='read')
OPEN(54,FILE='geometry/spline_line.dat', action='read')
DO counter=1, lin
READ(54,fmt=*) xc(counter), yc(counter)
READ(52,fmt=*) xc(counter), t(counter)
ENDDO

!routine to dimensionalize
DO counter=1, lin
xc(counter)=xc(counter)*c
yc(counter)=yc(counter)*fmax
t(counter)=t(counter)*tmax
ENDDO

 CLOSE(54)
  CLOSE(52)

END SUBROUTINE
!--------------------------------------------------------------------------
END MODULE
