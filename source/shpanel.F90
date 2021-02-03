!P. Olivucci 2015
!shpanel
!Main Routine
!------------------------------------------------------------
 PROGRAM shpanel

!includes modules:
 USE input
 USE airfoil
 USE coeff
 USE solver
 USE post

 IMPLICIT NONE
 INTEGER, ALLOCATABLE :: IPVT(:)
 INTEGER :: info

!reads input controls:
 CALL readinput()
!reads camber line and thickness distribution:
 CALL input_dat()
!builds hydrofoil discretization:
 CALL disc_foil()
!initializes the algebraic system:
 CALL init()
!populates the coefficient matrix (w/Kutta):
 CALL coeffs()
!populates the vector (w/Kutta):
 CALL vec()
!solves system:
 ALLOCATE (IPVT(n+1))
!factors the matrix by gaussian elimination:
 CALL SGEFA(a, (n+1), (n+1), IPVT,info)
!computes the solution:
 CALL SGESL(a, (n+1), (n+1), IPVT, b, 0)
!deallocates solved system variables:
 DEALLOCATE(a)
 DEALLOCATE (IPVT)
!flow field postprocessing:
 CALL bernoulli()    !computes velocity and pressure field
 CALL draw()        !geometry to text output
 CALL fieldprint()    !solution to text output

 END PROGRAM
