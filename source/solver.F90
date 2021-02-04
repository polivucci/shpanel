!Smith-Hess Panel Method
!Solvers for the algebraic system
!
!The subroutines are adapted from the package LINPACK.
!Please reference:
!J. J. Dongarra, C. B. Moler, 
!J. R. Bunch, G.W. Stewart,
!LINPACK Users' Guide, SIAM, 1979
!--------------------------------------------------------------------------
MODULE solver
!USE coeff
!IMPLICIT NONE
!REAL, ALLOCATABLE :: bres(:)
CONTAINS
!--------------------------------------------------------------------------
!     **************************************************************
!     **************************************************************
      SUBROUTINE SGEFA (A,LDA,N,IPVT,INFO)
      INTEGER LDA,N,IPVT(1),INFO
      DIMENSION A(LDA,1)
!
!     SGEFA FACTORS A REAL MATRIX BY GAUSSIAN ELIMINATION.
!
!     SGEFA IS USUALLY CALLED BY DGECO, BUT IT CAN BE CALLED
!     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.
!     (TIME FOR DGECO) = (1 + 9/N)*(TIME FOR DGEFA) .
!
!     ON ENTRY
!
!        A       REAL(LDA, N)
!                THE MATRIX TO BE FACTORED.
!
!        LDA     INTEGER
!                THE LEADING DIMENSION OF THE ARRAY  A .
!
!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .
!
!     ON RETURN
!
!        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS
!                WHICH WERE USED TO OBTAIN IT.
!                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE
!                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
!                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
!
!        IPVT    INTEGER(N)
!                AN INTEGER VECTOR OF PIVOT INDICES.
!
!        INFO    INTEGER
!                = 0  NORMAL VALUE.
!                = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR
!                     CONDITION FOR THIS SUBROUTINE, BUT IT DOES
!                     INDICATE THAT SGESL OR DGEDI WILL DIVIDE BY ZERO
!                     IF CALLED.  USE  RCOND  IN DGECO FOR A RELIABLE
!                     INDICATION OF SINGULARITY.
!
!     SUBROUTINES AND FUNCTIONS
!
!     BLAS DAXPY,DSCAL,IDAMAX
!
!     INTERNAL VARIABLES
!
      REAL T
      !INTEGER IDAMAX,J,K,KP1,L,NM1
	INTEGER J,K,KP1,L,NM1
!
!
!
!     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
!

      INFO = 0
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 70
      DO 60 K = 1, NM1
         KP1 = K + 1
!
!        FIND L = PIVOT INDEX
!
         L = IDAMAX(N-K+1,A(K,K),1) + K - 1
         IPVT(K) = L
!
!        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
!
         IF (A(L,K) .EQ. 0.0D0) GO TO 40
!
!           INTERCHANGE IF NECESSARY
!
            IF (L .EQ. K) GO TO 10
               T = A(L,K)
               A(L,K) = A(K,K)
               A(K,K) = T
   10       CONTINUE
!
!           COMPUTE MULTIPLIERS
!
            T = -1.0D0/A(K,K)
            CALL DSCAL(N-K,T,A(K+1,K),1)
!
!           ROW ELIMINATION WITH COLUMN INDEXING
!
            DO 30 J = KP1, N
               T = A(L,J)
               IF (L .EQ. K) GO TO 20
                  A(L,J) = A(K,J)
                  A(K,J) = T
   20          CONTINUE
               CALL DAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)
   30       CONTINUE
         GO TO 50
   40    CONTINUE
            INFO = K
   50    CONTINUE
   60 CONTINUE
   70 CONTINUE
      IPVT(N) = N
      IF (A(N,N) .EQ. 0.0D0) INFO = N
      RETURN
      END
      
      SUBROUTINE SGESL (A,LDA,N,IPVT,B,JOB)
      INTEGER LDA,N,IPVT(1),JOB
      DIMENSION A(LDA,1),B(1)
!
!     SGESL SOLVES THE REAL SYSTEM
!     A * X = B  OR  TRANS(A) * X = B
!     USING THE FACTORS COMPUTED BY DGECO OR DGEFA.
!
!     ON ENTRY
!
!        A       REAL(LDA, N)
!                THE OUTPUT FROM DGECO OR DGEFA.
!
!        LDA     INTEGER
!                THE LEADING DIMENSION OF THE ARRAY  A .
!
!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .
!
!        IPVT    INTEGER(N)
!                THE PIVOT VECTOR FROM DGECO OR DGEFA.
!
!        B       REAL(N)
!                THE RIGHT HAND SIDE VECTOR.
!
!        JOB     INTEGER
!                = 0         TO SOLVE  A*X = B ,
!                = NONZERO   TO SOLVE  TRANS(A)*X = B  WHERE
!                            TRANS(A)  IS THE TRANSPOSE.
!
!     ON RETURN
!
!        B       THE SOLUTION VECTOR  X .
!
!     ERROR CONDITION
!
!        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A
!        ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES SINGULARITY
!        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER
!        SETTING OF LDA .  IT WILL NOT OCCUR IF THE SUBROUTINES ARE
!        CALLED CORRECTLY AND IF DGECO HAS SET RCOND .GT. 0.0
!        OR DGEFA HAS SET INFO .EQ. 0 .
!
!     TO COMPUTE  INVERSE(A) * !  WHERE  !  IS A MATRIX
!     WITH  P  COLUMNS
!           CALL DGECO(A,LDA,N,IPVT,RCOND,Z)
!           IF (RCOND IS TOO SMALL) GO TO ...
!           DO 10 J = 1, P
!              CALL SGESL(A,LDA,N,IPVT,!(1,J),0)
!        10 CONTINUE
!
!     SUBROUTINES AND FUNCTIONS
!
!     BLAS DAXPY,DDOT
!
!     INTERNAL VARIABLES
!
      !REAL DDOT,T
      REAL T
      INTEGER K,KB,L,NM1
!
!
	!ALLOCATE(bres(n+1))

      NM1 = N - 1
      IF (JOB .NE. 0) GO TO 50
!
!        JOB = 0 , SOLVE  A * X = B
!        FIRST SOLVE  L*Y = B
!
         IF (NM1 .LT. 1) GO TO 30
         DO 20 K = 1, NM1
            L = IPVT(K)
            T = B(L)
            IF (L .EQ. K) GO TO 10
               B(L) = B(K)
               B(K) = T
   10       CONTINUE
            CALL DAXPY(N-K,T,A(K+1,K),1,B(K+1),1)
   20    CONTINUE
   30    CONTINUE
!
!        NOW SOLVE  U*X = Y
!
         DO 40 KB = 1, N
            K = N + 1 - KB
            B(K) = B(K)/A(K,K)
            T = -B(K)
            CALL DAXPY(K-1,T,A(1,K),1,B(1),1)
!WRITE(*,*) b(k)
   40    CONTINUE
      GO TO 100
   50 CONTINUE
!
!        JOB = NONZERO, SOLVE  TRANS(A) * X = B
!        FIRST SOLVE  TRANS(U)*Y = B
!
         DO 60 K = 1, N
            T = DDOT(K-1,A(1,K),1,B(1),1)
            B(K) = (B(K) - T)/A(K,K)
   60    CONTINUE
!
!        NOW SOLVE TRANS(L)*X = Y
!
         IF (NM1 .LT. 1) GO TO 90
         DO 80 KB = 1, NM1
            K = N - KB
            B(K) = B(K) + DDOT(N-K,A(K+1,K),1,B(K+1),1)
            L = IPVT(K)
            IF (L .EQ. K) GO TO 70
               T = B(L)
               B(L) = B(K)
               B(K) = T
   70       CONTINUE
   80    CONTINUE
   90    CONTINUE
  100 CONTINUE
      RETURN
	!do k=1, n+1
	!bres(k) = B(k)
	!enddo
      END

      REAL FUNCTION DDOT(N,DX,INCX,DY,INCY)
!
!     FORMS THE DOT PRODUCT OF TWO VECTORS.
!     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
!
      REAL DX(1),DY(1),DTEMP
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
!
      DDOT = 0.0D0
      DTEMP = 0.0D0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
!
!        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
!          NOT EQUAL TO 1
!
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DTEMP = DTEMP + DX(IX)*DY(IY)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      DDOT = DTEMP
      RETURN
!
!        CODE FOR BOTH INCREMENTS EQUAL TO 1
!
!
!        CLEAN-UP LOOP
!
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DTEMP = DTEMP + DX(I)*DY(I)
   30 CONTINUE
      IF( N .LT. 5 ) GO TO 60
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        DTEMP = DTEMP + DX(I)*DY(I) + DX(I + 1)*DY(I + 1) + DX(I + 2)*DY(I + 2) + DX(I + 3)*DY(I + 3) + DX(I + 4)*DY(I + 4)
   50 CONTINUE
   60 DDOT = DTEMP
      RETURN
      END
      SUBROUTINE  DSCAL(N,DA,DX,INCX)
!
!     SCALES A VECTOR BY A CONSTANT.
!     USES UNROLLED LOOPS FOR INCREMENT EQUAL TO ONE.
!
      REAL DA,DX(1)
      INTEGER I,INCX,M,MP1,N,NINCX
!
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GO TO 20
!
!        CODE FOR INCREMENT NOT EQUAL TO 1
!
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
        DX(I) = DA*DX(I)
   10 CONTINUE
      RETURN
!
!        CODE FOR INCREMENT EQUAL TO 1
!
!
!        CLEAN-UP LOOP
!
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DX(I) = DA*DX(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        DX(I) = DA*DX(I)
        DX(I + 1) = DA*DX(I + 1)
        DX(I + 2) = DA*DX(I + 2)
        DX(I + 3) = DA*DX(I + 3)
        DX(I + 4) = DA*DX(I + 4)
   50 CONTINUE
      RETURN
      END
      INTEGER FUNCTION IDAMAX(N,DX,INCX)
!
!     FINDS THE INDEX OF ELEMENT HAVING MAX. ABSOLUTE VALUE.
!
      REAL DX(1),DMAX
      INTEGER I,INCX,IX,N
!
      IDAMAX = 0
      IF( N .LT. 1 ) RETURN
      IDAMAX = 1
      IF(N.EQ.1)RETURN
      IF(INCX.EQ.1)GO TO 20
!
!        CODE FOR INCREMENT NOT EQUAL TO 1
!
      IX = 1
      DMAX = ABS(DX(1))
      IX = IX + INCX
      DO 10 I = 2,N
         IF(ABS(DX(IX)).LE.DMAX) GO TO 5
         IDAMAX = I
         DMAX = ABS(DX(IX))
    5    IX = IX + INCX
   10 CONTINUE
      RETURN
!
!        CODE FOR INCREMENT EQUAL TO 1
!
   20 DMAX = ABS(DX(1))
      DO 30 I = 2,N
         IF(ABS(DX(I)).LE.DMAX) GO TO 30
         IDAMAX = I
         DMAX = ABS(DX(I))
   30 CONTINUE
      RETURN
      END
      SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
!
!     CONSTANT TIMES A VECTOR PLUS A VECTOR.
!     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
!
      REAL DX(1),DY(1),DA
      INTEGER I,INCX,INCY,IXIY,M,MP1,N
!
      IF(N.LE.0)RETURN
      IF (DA .EQ. 0.0D0) RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
!
!        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
!          NOT EQUAL TO 1
!
      IXIY = 1*IXIY
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DY(IY) = DY(IY) + DA*DX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
!
!        CODE FOR BOTH INCREMENTS EQUAL TO 1
!
!
!        CLEAN-UP LOOP
!
   20 M = MOD(N,4)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DY(I) = DY(I) + DA*DX(I)
   30 CONTINUE
      IF( N .LT. 4 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
        DY(I) = DY(I) + DA*DX(I)
        DY(I + 1) = DY(I + 1) + DA*DX(I + 1)
        DY(I + 2) = DY(I + 2) + DA*DX(I + 2)
        DY(I + 3) = DY(I + 3) + DA*DX(I + 3)
   50 CONTINUE
      RETURN
      END
!--------------------------------------------------------------------------
END MODULE
