!P. Olivucci 2015
!Smith-Hess Panel Method
!Mathematical definition of const and functs
!------------------------------------------------------------------------------
MODULE const

IMPLICIT NONE

CONTAINS
!==============================================================================
DOUBLE PRECISION FUNCTION pi()

  pi =  ACOS(0.)*2.d0

END FUNCTION pi
!==============================================================================
DOUBLE PRECISION FUNCTION pi2inv()

  pi2inv = 0.5d0/pi()

END FUNCTION pi2inv
!==============================================================================
END MODULE const
