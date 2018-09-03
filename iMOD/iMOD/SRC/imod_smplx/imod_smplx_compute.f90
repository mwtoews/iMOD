!!  Copyright (C) Stichting Deltares, 2005-2018.
!!
!!  This file is part of iMOD.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License as published by
!!  the Free Software Foundation, either version 3 of the License, or
!!  (at your option) any later version.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!!
!!  Contact: imod.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands.
!!
MODULE SMPLX_COMPUTE

USE SMPLX_CONSTANTS_NSWC

CONTAINS

 !###======================================================================
 SUBROUTINE SMPLX (A, B0, C, KA, XDIM, N0, IND, IBASIS, X, Z, ITER, MXITER,   &
                   NUMLE, NUMGE, BI, RERR)
 !###======================================================================

 !-----------------------------------------------------------------------
 !     SIMPLEX PROCEDURE FOR SOLVING LINEAR PROGRAMMING PROBLEMS
 !-----------------------------------------------------------------------
 ! FINDS NON-NEGATIVE X'S TO MAXIMIZE:
 !     C1.X1 + ... + CN.XN
 !
 ! SUBJECT TO THE CONSTRAINTS:
 !     A11.X1 + ... + A1N.XN {<=,=,>=} B1
 !              ...
 !     AM1.X1 + ... + AMN.XN {<=,=,>=} BM
 !
 ! THE CONSTRAINTS MUST BE ORDERED SO THAT THE <= CONSTRAINTS, IF ANY, COME
 ! FIRST, THEN ANY >= CONSTRAINTS, WITH THE EQUALITY CONSTRAINTS LAST.
 !
 ! ARGUMENTS:
 ! A(KA,N0)   INPUT   COEFFICIENTS IN THE CONSTRAINTS.
 ! B0(M)      INPUT   RIGHT-HAND SIDE OF THE CONSTRAINTS.
 ! C(N0)      INPUT   VECTOR OF `COSTS' IN THE OBJECTIVE FUNCTION.
 ! KA         INPUT   FIRST DIMENSION OF ARRAY A.
 ! M          INPUT   DIMENSION OF ARRAY B0.
 ! N0         INPUT   2ND DIMENSION OF ARRAY A.
 ! IND        IN/OUT  IF IND = 0 ON INPUT, THE ROUTINE SELECTS ITS OWN INITIAL
 !                    BASIS, ELSE IF IND = 1 THEN THE INDICES OF THE INITIAL
 !                    BASIS SHOULD BE IN IBASIS.
 !                    ON OUTPUT:
 !                    IND = 0 THE PROBLEM WAS SOLVED
 !                    IND = 1 THE PROBLEM HAS NO SOLUTION
 !                    IND = 2 MXITER ITERATIONS WERE PERFORMED; MORE NEEDED
 !                    IND = 3 SUFFICIENT ACCURACY COULD NOT BE MAINTAINED TO
 !                            SOLVE THE PROBLEM
 !                    IND = 4 THE PROBLEM HAS AN UNBOUNDED SOLUTION
 !                    IND = 5 INPUT ERROR DETECTED
 !                    IND = 6 THE SOLUTION MAY HAVE BEEN OBTAINED
 ! IBASIS(M)  IN/OUT  INDICES OF THE VARIABLES IN THE BASIS.
 ! X()        OUTPUT  DIMENSION MUST BE >= N + NUMLE + NUMGE.
 !                    IF IND = 0 OR 6, IT CONTAINS THE VALUES OF THE ORIGINAL,
 !                    SLACK AND SURPLUS VARIABLES.
 ! Z          OUTPUT  IF IND = 0 OR 6, CONTAINS THE VALUE OF THE OBJECTIVE.
 ! ITER       OUTPUT  NUMBER OF ITERATIONS USED.
 ! MXITER     INPUT   MAXIMUM NUMBER OF ITERATIONS.
 ! NUMLE      INPUT   NUMBER OF <= CONSTRAINTS.
 ! NUMGE      INPUT   NUMBER OF >= CONSTRAINTS.
 ! BI(M,M)    OUTPUT  THE INVERSE OF THE BASIS MATRIX.
 ! RERR       OUTPUT  THE ESTIMATED RELATIVE ERROR ACHIEVED.
 ! N.B. THE LAST TWO ARGUMENTS IN THE NSWC ROUTINE HAVE BEEN ELIMINATED.
 !      THESE WERE WORK SPACES.   THEY ARE NOW DECLARED AS AUTOMATIC ARRAYS.
 !-----------------------------------------------------------------------
 !     WRITTEN BY ALFRED H. MORRIS JR.
 !        NAVAL SURFACE WEAPONS CENTER
 !        DAHLGREN, VIRGINIA
 !------------------------
 !     INITIAL VERSION  DEC  1977
 !     LAST UPDATE      OCT  1990
 !-----------------------------------------------------------------------
 !     CONVERTED USING F90 INTRINSICS BY
 !        ALAN MILLER
 !        CSIRO MATHEMATICAL & INFORMATION SCIENCES
 !        CLAYTON, VICTORIA, AUSTRALIA 3169
 !     LATEST REVISION - 5 FEBRUARY 1997
 !-----------------------------------------------------------------------

 IMPLICIT NONE

 INTEGER, INTENT(IN) :: KA, XDIM, N0, MXITER, NUMLE, NUMGE
 INTEGER, INTENT(IN OUT) :: IND
 INTEGER, INTENT(OUT) :: ITER
 REAL (DP), INTENT(OUT) :: Z, RERR
 INTEGER, DIMENSION(KA), INTENT(IN OUT)  :: IBASIS
 REAL (DP), DIMENSION(KA,N0), INTENT(IN)  :: A
 REAL (DP), DIMENSION(KA), INTENT(IN) :: B0
 REAL (DP), DIMENSION(N0), INTENT(IN) :: C
 REAL (DP), DIMENSION(XDIM), INTENT(OUT)   :: X
 REAL (DP), DIMENSION(KA,KA), INTENT(OUT) :: BI

 !------------------------
 !     DIMENSION X(N0+NUMLE+NUMGE)
 !------------------------

 !     ********** EPS0 IS A MACHINE DEPENDENT PARAMETER. ASSIGN EPS0
 !                THE VALUE U WHERE U IS THE SMALLEST POSITIVE FLOATING
 !                POINT NUMBER SUCH THAT 1.0D0 + U .GT. 1.0D0.

 !     LOCAL VARIABLES
 REAL (DP) :: EPS0, RERRMN, RERRMX

 EPS0 = DPMPAR(1)

 !------------------------
 RERRMN = 10.0_DP*EPS0
 RERRMX = 1.0D-4
 IF (EPS0 < 1.0D-13) RERRMX = 1.0D-5

!WRITE(*,*) A
!WRITE(*,*) B0
!WRITE(*,*) KA,M,N0

 CALL SMPLX1(A, B0, C, KA, XDIM, N0, IND, IBASIS, X, Z, ITER, MXITER, EPS0,   &
         RERRMN, RERRMX, RERR, NUMLE, NUMGE, BI)

 RETURN
 END SUBROUTINE SMPLX

 !###======================================================================
 SUBROUTINE SMPLX1 (A, B0, C, KA, XDIM, N0, IND, IBASIS, R, Z, ITER, MXITER,  &
                    EPS0, RERRMN, RERRMX, RERR, NUMLE, NUMGE, BI)
 !###======================================================================

 !----------------------
 !     NSTEP = 1   ELIMINATE THE NEGATIVE VARIABLES
 !     NSTEP = 2   PHASE 1 OF THE SIMPLEX ALGORITHM
 !     NSTEP = 3   PHASE 2 OF THE SIMPLEX ALGORITHM
 !----------------------
 !     MXITER = THE MAXIMUM NUMBER OF ITERATIONS PERMITTED
 !     ITER = THE NUMBER OF THE CURRENT ITERATION
 !     ICOUNT = THE NUMBER OF ITERATIONS SINCE THE LAST INVERSION
 !----------------------
 !     NUMLE = THE NUMBER OF .LE. CONSTRAINTS
 !     NUMGE = THE NUMBER OF .GE. CONSTRAINTS
 !----------------------
 !     THE ROUTINE ASSUMES THAT THE .LE. CONSTRAINTS PRECEDE THE .GE.
 !     CONSTRAINTS AND THAT THE .EQ. CONSTRAINTS COME LAST. THERE ARE
 !     M CONSTRAINTS. X(N0+I) IS THE SLACK, SURPLUS, OR ARTIFICIAL
 !     VARIABLE FOR THE I-TH CONSTRAINT (I=1, ..., M).
 !----------------------
 !     N0 = THE NUMBER OF ORGINAL VARIABLES
 !     NS = THE NUMBER OF ORGINAL AND SLACK VARIABLES
 !     N  = THE NUMBER OF ORGINAL, SLACK, AND SURPLUS VARIABLES
 !     NUM = THE TOTAL NUMBER OF VARIABLES
 !----------------------
 !     RERRMN = THE SMALLEST RELATIVE ERROR TOLERANCE USED
 !     RERRMX = THE LARGEST RELATIVE ERROR TOLERACE USED
 !     RERR   = THE ESTIMATED CURRENT RELATIVE ERROR
 !----------------------
 !     ASSUME THAT
 !         B0 = (B0(1), ..., B0(M))
 !         C  = (C(1), ..., C(N0))
 !         Z  = C(1)*X(1)+...+C(N0)*X(N0)
 !     THE PROBLEM IS TO MAXIMIZE Z SUBJECT TO
 !         AX(LE,EQ,GE)B0
 !         X.GE.0
 !----------------------
 !     ON INPUT IND CAN HAVE THE VALUES
 !         IND = 0   NO BEGINNING BASIS IS PROVIDED BY THE USER
 !         IND = 1   THE ARRAY IBASIS HAS BEEN SET BY THE USER
 !     ON OUTPUT IND IS ASSIGNED ONE OF THE VALUES
 !         IND = 0   Z WAS SuccessfullY MAXIMIZED
 !         IND = 1   THE PROBLEM HAS NO FEASIBLE SOLUTION
 !         IND = 2   MXITER ITERATIONS WERE PERFORMED
 !         IND = 3   SUFFICIENT ACCURACY CANNOT BE MAINTAINED
 !         IND = 4   THE PROBLEM HAS AN UNBOUNDED SOLUTION
 !         IND = 5   THERE IS AN INPUT ERROR
 !         IND = 6   Z WAS POSSIBLY MAXIMIZED
 !----------------------
 !     BASIS IS AN INTEGER ARRAY OF DIMENSION N0+M. FOR J.LE.N
 !         BASIS(J) = 1  IF X(J) IS A BASIC VARIABLE
 !         BASIS(J) = 0  IF X(J) IS NOT A BASIC VARIABLE
 !     IF THE BASIC VARIABLES ARE X(I1), ..., X(IM) THEN
 !         IBASIS = (I1, ..., IM)
 !     ALSO XB(1), ..., XB(M) ARE THE CORRESPONDING VALUES OF THE
 !     BASIC VARIABLES.
 !----------------------
 !     BI IS AN MXM ARRAY CONTAINING THE INVERSE OF THE BASIS MATRIX.
 !----------------------
 !     R IS AN ARRAY OF DIMENSION N. ON OUTPUT R CONTAINS THE CURRENT
 !     VALUE OF X. DURING COMPUTATION R NORMALLY CONTAINS THE REDUCED
 !     COSTS USED FOR THE SELECTION OF THE VARIABLE TO BE MADE BASIC.
 !----------------------
! USE CONSTANTS_NSWC
 IMPLICIT NONE
 INTEGER, INTENT(IN) :: KA, XDIM, N0, MXITER, NUMLE, NUMGE
 INTEGER, INTENT(OUT) :: IND, ITER
 REAL (DP), INTENT(OUT) :: Z, RERR
 REAL (DP), INTENT(IN) :: EPS0, RERRMN, RERRMX
 REAL (DP), DIMENSION(KA,N0), INTENT(IN)  :: A !
 REAL (DP), DIMENSION(KA), INTENT(IN) :: B0 !
 REAL (DP), DIMENSION(N0), INTENT(IN) :: C !
 REAL (DP), DIMENSION(KA,KA), INTENT(OUT) :: BI !
 REAL (DP), DIMENSION(XDIM), INTENT(OUT) :: R
 INTEGER, DIMENSION(KA), INTENT(IN OUT) :: IBASIS

 !----------------------

 !     LOCAL VARIABLES
 INTEGER   :: I, IBEG, ICOUNT, IEND, IERR, II, IL, IMIN, IOUT, IP, J, JJ,  &
              JMIN, JP, K, KI, KJ, L, LL, LROW, M0, MCHECK, MS, N, NPOS,   &
              NROW, NS, NSTEP, NUM, BFLAG, M
 REAL (DP) :: XB(KA), Y(KA)!XB(M), Y(M)
 INTEGER   :: BASIS(KA+N0), INDX(KA)!BASIS(M+N0), INDX(M)
 REAL (DP) :: AMAX, BINORM, BMAX, BMIN, BNORM, CMIN, CONST, EPS, EPSI, RATIO, &
              RERR1, RMIN, RTOL, S, SGN, T, TOL, TOTAL, W, XMAX, ZERO = 0._DP, &
              DSUM, DSUMP, DSUMN, DT

 !     ****** XMAX IS A MACHINE DEPENDENT CONSTANT. XMAX IS THE
 !            LARGEST POSITIVE FLOATING POINT NUMBER.

!WRITE(*,*) 'A'
!WRITE(*,*) A
!WRITE(*,*) 'B0'
!WRITE(*,*) B0
!WRITE(*,*) 'C'
!WRITE(*,*) C
!WRITE(*,*) 'BI'
!WRITE(*,*) BI
!WRITE(*,*) 'R'
!WRITE(*,*) R
!WRITE(*,*) 'IBASIS'
!WRITE(*,*) IBASIS
!WRITE(*,*) 'KA=',KA,'M=',M,'N0=',N0

 M=KA

 XMAX = DPMPAR(3)

 !----------------------
 ITER = 0
 ICOUNT = 0
 MCHECK = MIN(5, 1 + M/15)
 Z = ZERO

 !                CHECK FOR INPUT ERRORS

 MS = NUMLE + NUMGE
 IF (M < 2 .OR. N0 < 2 .OR. MS > M .OR. KA < M)  &
 GO TO 12
 DO I = 1, M
   IF (B0(I) < ZERO) GO TO 12
   XB(I) = ZERO
 END DO
 RTOL = XMAX
 DO I = 1, N0
   IF (C(I) /= ZERO) RTOL = MIN(ABS(C(I)), RTOL)
 END DO
 RTOL = RERRMX*RTOL
 GO TO 20

 12 IND = 5
 RETURN

 !     FORMATION OF THE IBASIS AND BASIS ARRAYS.
 !     (IF IND = 1  THEN THE IBASIS ARRAY IS DEFINED BY THE USER.)

 20 NS = N0 + NUMLE
 N = NS + NUMGE
 IF (IND == 0) GO TO 30
 NUM = N
 DO I = 1, M
   IF (IBASIS(I) > N) NUM = NUM + 1
 END DO
 GO TO 32
 22 IF (IND == 0) GO TO 590
 IND = 0

 30 NUM = N0 + M
 DO I = 1, M
   IBASIS(I) = N0 + I
 END DO
 32 BFLAG = 0
 BASIS(1:N) = 0
 DO I = 1, M
   KI = IBASIS(I)
   BASIS(KI) = 1
 END DO
 IF (IND == 1) GO TO 100

 !          CALCULATION OF XB AND BI WHEN IND = 0

 RERR = RERRMN
 DO J = 1, M
   XB(J) = B0(J)
   BI(1:M,J) = ZERO
   BI(J,J) = 1.0_DP
 END DO
 IF (NUMGE == 0) GO TO 630
 JMIN = NUMLE + 1
 DO J = JMIN, MS
   XB(J) = -XB(J)
   BI(J,J) = -1.0_DP
 END DO
 GO TO 601

 !                  REORDER THE BASIS

 100 IBEG = 1
 IEND = M
 DO I = 1, M
   IF (IBASIS(I) > N0) THEN
     INDX(IBEG) = IBASIS(I)
     IBEG = IBEG + 1
   ELSE
     INDX(IEND) = IBASIS(I)
     IEND = IEND - 1
   END IF
 END DO
 IF (IEND == M) GO TO 22
 IBASIS(1:M) = INDX(1:M)

 !            REINVERSION OF THE BASIS MATRIX

 DO J = 1, M
   KJ = IBASIS(J)
   IF (KJ <= N0) GO TO 110
   IF (KJ <= NS) GO TO 120
   IF (KJ <= N) GO TO 130
   GO TO 120

   110 BI(1:M,J) = A(1:M,KJ)
   CYCLE

   120 L = KJ - N0
   BI(1:M,J) = ZERO
   BI(L,J) = 1.0_DP
   CYCLE

   130 L = KJ - N0
   BI(1:M,J) = ZERO
   BI(L,J) = -1.0_DP
 END DO

 ICOUNT = 0
 CALL CROUT1 (BI(1,1), M, M, IEND, INDX, Y, IERR)
! CALL CROUT1 (BI(1:,1), M, M, IEND, INDX, Y, IERR)
 IF (IERR /= 0) GO TO 580

 !         CHECK THE ACCURACY OF BI AND RESET RERR

 BNORM = ZERO
 DO J = 1, M
   KJ = IBASIS(J)
   IF (KJ <= N0) GO TO 140
   TOTAL = 1.0_DP
   GO TO 142
   140 TOTAL = SUM( ABS(A(1:M,KJ)) )
   142 BNORM = MAX(BNORM, TOTAL)
 END DO

 BINORM = ZERO
 DO J = 1, M
   TOTAL = SUM( ABS(BI(1:M,J)) )
   BINORM = MAX(BINORM, TOTAL)
 END DO
 RERR = MAX(RERRMN, EPS0*BNORM*BINORM)
 IF (RERR > 1.D-2) GO TO 580
 BFLAG = 0

 !                 RECALCULATION OF XB

 DO I = 1, M
   DSUMP = ZERO
   DSUMN = ZERO
   DO L = 1, M
     DT = BI(I,L)*B0(L)
     IF (DT > ZERO) THEN
       DSUMP = DSUMP + DT
     ELSE
       DSUMN = DSUMN + DT
     END IF
   END DO
   XB(I) = DSUMP + DSUMN
   S = DSUMP
   T = DSUMN
   TOL = RERRMX*MAX(S, -T)
   IF (ABS(XB(I)) < TOL) XB(I) = ZERO
 END DO
 GO TO 601

 !     FIND THE NEXT VECTOR A(--, JP) TO BE INSERTED INTO THE BASIS

 200 JP = 0
 RMIN = ZERO
 IF (NSTEP == 3) RMIN = -RTOL
 DO J = 1, N0
   IF (BASIS(J) /= 0) CYCLE
   IF (R(J) >= RMIN) CYCLE
   JP = J
   RMIN = R(J)
 END DO
 IF (N0 == N) GO TO 203
 JMIN = N0 + 1
 RMIN = RMIN*1.1_DP
 DO J = JMIN, N
   IF (BASIS(J) /= 0) CYCLE
   IF (R(J) >= RMIN) CYCLE
   JP = J
   RMIN = R(J)
 END DO
 203 IF (JP /= 0) GO TO 300
 IF (NSTEP < 2) THEN
    GO TO 800
 ELSE IF (NSTEP == 2) THEN
   GO TO 230
 ELSE
   GO TO 250
 END IF

 !     INSERT THE VALUES OF THE ORGINAL, SLACK, AND SURPLUS
 !             VARIABLES INTO R, THEN TERMINATE.

 220 R(1:N) = ZERO
 DO I = 1, M
   KI = IBASIS(I)
   IF (KI <= N) R(KI) = XB(I)
 END DO
 RETURN

 !             COMPLETION OF THE NSTEP = 2 CASE

 230 DO I = 1, M
   IF (IBASIS(I) <= N) CYCLE
   IF (XB(I) > ZERO) GO TO 800
 END DO
 GO TO 680

 240 IF (ICOUNT >= 5) GO TO 100
 IND = 1
 GO TO 220

 !             COMPLETION OF THE NSTEP = 3 CASE

 250 IF (RERR > 1.D-2) GO TO 251
 IND = 0
 GO TO 800
 251 IF (ICOUNT >= 5) GO TO 100
 IND = 6
 GO TO 800

 !     IF MXITER ITERATIONS HAVE NOT BEEN PERFORMED THEN BEGIN THE
 !     NEXT ITERATION. COMPUTE THE JP-TH COLUMN OF BI*A AND STORE IT IN Y.

 300 IF (ITER < MXITER) GO TO 301
 IND = 2
 GO TO 220
 301 ITER = ITER + 1
 ICOUNT = ICOUNT + 1
 IF (JP > NS) GO TO 330
 IF (JP > N0) GO TO 320

 NROW = 0
 AMAX = ZERO
 DO I = 1, M
   IF (A(I,JP) == ZERO) CYCLE
   NROW = NROW + 1
   INDX(NROW) = I
   AMAX = MAX(ABS(A(I,JP)), AMAX)
 END DO
 IF (NROW /= 0) GO TO 310
 IND = 4
 GO TO 220

 310 RERR1 = RERRMX*AMAX
 DO I = 1, M
   DSUM = ZERO
   DO LL = 1, NROW
     L = INDX(LL)
     DSUM = DSUM + BI(I,L)*A(L,JP)
   END DO
   Y(I) = DSUM
   IF (ABS(Y(I)) >= 5.D-3) CYCLE
   BMAX = ZERO
   DO L = 1, M
     BMAX = MAX(ABS(BI(I,L)), BMAX)
   END DO
   TOL = RERR1*BMAX
   IF (ABS(Y(I)) < TOL) Y(I) = ZERO
 END DO
 GO TO 350

 320 L = JP - N0
 Y(1:M) = BI(1:M,L)
 GO TO 350

 330 L = JP - N0
 Y(1:M) = -BI(1:M,L)

 350 DO I = 1, M
   IF (Y(I) /= ZERO) GO TO 360
 END DO
 R(JP) = ZERO
 ITER = ITER - 1
 ICOUNT = ICOUNT - 1
 GO TO 200

 360 IF (NSTEP == 2) GO TO 430
 IF (NSTEP > 2) GO TO 440

 !     FINDING THE VARIABLE XB(IP) TO BE MADE NONBASIC FOR THE NSTEP = 1 CASE

 NPOS = 0
 IP = 0
 EPS = ZERO
 EPSI = XMAX
 DO I = 1, M
   IF (XB(I) < ZERO .OR. Y(I) <= ZERO) CYCLE
   RATIO = XB(I)/Y(I)
   IF (RATIO < EPSI) THEN
     EPSI = RATIO
     NPOS = 1
     INDX(1) = I
     CYCLE
   ELSE IF (RATIO > EPSI) THEN
     CYCLE
   END IF
   NPOS = NPOS + 1
   INDX(NPOS) = I
 END DO
 IF (NPOS == 0) GO TO 420
 IF (EPSI == ZERO) GO TO 460

 DO I = 1, M
   IF (XB(I) >= ZERO .OR. Y(I) >= ZERO) CYCLE
   RATIO = XB(I)/Y(I)
   IF (RATIO > EPSI) CYCLE
   IF (RATIO < EPS) CYCLE
   EPS = RATIO
   IP = I
 END DO
 IF (IP /= 0) GO TO 500
 GO TO 460

 420 DO I = 1, M
   IF (XB(I) >= ZERO .OR. Y(I) >= ZERO) CYCLE
   RATIO = XB(I)/Y(I)
   IF (RATIO < EPS) CYCLE
   EPS = RATIO
   IP = I
 END DO
 GO TO 500

 !     FINDING THE VARIABLE XB(IP) TO BE MADE NONBASIC FOR THE NSTEP = 2 CASE

 430 NPOS = 0
 EPSI = XMAX
 DO I = 1, M
   IF (Y(I) <= ZERO) CYCLE
   RATIO = XB(I)/Y(I)
   IF (RATIO < EPSI) THEN
     EPSI = RATIO
     NPOS = 1
     INDX(1) = I
   ELSE IF (RATIO > EPSI) THEN
     CYCLE
   END IF
   NPOS = NPOS + 1
   INDX(NPOS) = I
 END DO
 GO TO 450

 !     FINDING THE VARIABLE XB(IP) TO BE MADE NONBASIC FOR THE NSTEP = 3 CASE

 440 NPOS = 0
 EPSI = XMAX
 DO I = 1, M
   IF (Y(I) < ZERO) THEN
     IF (IBASIS(I) <= N) CYCLE
     IP = I
     GO TO 500
   ELSE IF (Y(I) > ZERO) THEN
     RATIO = XB(I)/Y(I)
     IF (RATIO < EPSI) THEN
       EPSI = RATIO
       NPOS = 1
       INDX(1) = I
     ELSE IF (RATIO > EPSI) THEN
       CYCLE
     END IF
     NPOS = NPOS + 1
     INDX(NPOS) = I
   END IF
 END DO

 450 IF (NPOS /= 0) GO TO 460
 IF (ICOUNT >= 5) GO TO 100
 IND = 4
 GO TO 220

 !              TIE BREAKING PROCEDURE

 460 IP = INDX(1)
 IF (NPOS == 1) GO TO 500
 IP = 0
 BMIN = XMAX
 CMIN = XMAX
 DO II = 1, NPOS
   I = INDX(II)
   L = IBASIS(I)
   IF (L > N0) GO TO 461
   IF (C(L) <= ZERO) CMIN = MIN(ZERO, CMIN)
   IF (C(L) > CMIN) CYCLE
   IMIN = I
   CMIN = C(L)
   CYCLE
   461 IF (L <= N) GO TO 462
   IP = I
   GO TO 500
   462 LROW = L - N0
   S = B0(LROW)
   IF (LROW <= NUMLE) THEN
     IF (S > BMIN) CYCLE
     IP = I
     BMIN = S
   ELSE
     S = -S
     BMIN = MIN(ZERO, BMIN)
     IF (S > BMIN) CYCLE
     IP = I
     BMIN = S
   END IF
 END DO
 IF (CMIN <= ZERO .OR. IP == 0) IP = IMIN

 !               TRANSFORMATION OF XB

 500 IF (XB(IP) == ZERO) GO TO 510
 CONST = XB(IP)/Y(IP)
 DO I = 1, M
   S = XB(I)
   XB(I) = XB(I) - CONST*Y(I)
   IF (XB(I) >= ZERO) CYCLE
   IF (S >= ZERO .OR. XB(I) >= RERRMX*S) XB(I) = ZERO
 END DO
 XB(IP) = CONST

 !               TRANSFORMATION OF BI

 510 DO J = 1, M
   IF (BI(IP,J) == ZERO) CYCLE
   CONST = BI(IP,J)/Y(IP)
   BI(1:M,J) = BI(1:M,J) - CONST*Y(1:M)
   BI(IP,J) = CONST
 END DO

 !             UPDATING IBASIS AND BASIS

 IOUT = IBASIS(IP)
 IBASIS(IP) = JP
 BASIS(IOUT) = 0
 BASIS(JP) = 1
 IF (IOUT > N) NUM = NUM - 1

 !        CHECK THE ACCURACY OF BI AND RESET RERR

 IF (RERR > 1.D-2) GO TO 530
 K = 0
 DO J = 1, M
   KJ = IBASIS(J)
   IF (KJ > N0) CYCLE
   TOTAL = DOT_PRODUCT( BI(J,1:M), A(1:M, KJ) )
   RERR = MAX(RERR, ABS(1.0_DP - TOTAL))
   K = K + 1
   IF (K >= MCHECK) GO TO 522
 END DO
 522 IF (RERR <= 1.D-2) GO TO 600

 !        THE ACCURACY CRITERIA ARE NOT SATISFIED

 530 IF (ICOUNT < 5) GO TO 600
 BFLAG = 1
 GO TO 100

 580 IF (ITER == 0) GO TO 12
 IF (BFLAG == 0) GO TO 590
 BFLAG = 0
 DO IP = 1, M
   IF (JP == IBASIS(IP)) GO TO 582
 END DO
 582 IBASIS(IP) = IOUT
 BASIS(JP) = 0
 BASIS(IOUT) = 1
 IF (IOUT > N) NUM = NUM + 1
 GO TO 100

 590 IND = 3
 GO TO 220

 !       SET UP THE R ARRAY FOR THE NSTEP = 1 CASE

 600 IF (NSTEP == 2) GO TO 630
 IF (NSTEP > 2) GO TO 700
 601 DO J = 1, M
   IF (XB(J) < ZERO) GO TO 610
 END DO
 GO TO 630

 610 NSTEP = 1
 M0 = 0
 DO L = 1, M
   IF (XB(L) >= ZERO) CYCLE
   M0 = M0 + 1
   INDX(M0) = L
 END DO

 DO J = 1, M
   DSUMP = ZERO
   DSUMN = ZERO
   DO LL = 1, M0
     L = INDX(LL)
     IF (BI(L,J) < ZERO) THEN
       DSUMN = DSUMN + BI(L,J)
     ELSE IF (BI(L,J) > ZERO) THEN
       DSUMP = DSUMP + BI(L,J)
     END IF
   END DO
   Y(J) = DSUMP + DSUMN
   S = DSUMP
   T = DSUMN
   TOL = RERRMX*MAX(S, -T)
   IF (ABS(Y(J)) < TOL) Y(J) = ZERO
 END DO
 GO TO 650

 !       SET UP THE R ARRAY FOR THE NSTEP = 2 CASE

 630 IF (N == NUM) GO TO 680
 NSTEP = 2
 M0 = 0
 DO L = 1, M
   IF (IBASIS(L) <= N) CYCLE
   M0 = M0 + 1
   INDX(M0) = L
 END DO

 DO J = 1, M
   DSUMP = ZERO
   DSUMN = ZERO
   DO LL = 1, M0
     L = INDX(LL)
     IF (BI(L,J) < ZERO) THEN
       DSUMN = DSUMN + BI(L,J)
     ELSE IF (BI(L,J) > ZERO) THEN
       DSUMP = DSUMP + BI(L,J)
     END IF
   END DO
   Y(J) = -(DSUMP + DSUMN)
   S = DSUMP
   T = DSUMN
   TOL = RERRMX*MAX(S, -T)
   IF (ABS(Y(J)) < TOL) Y(J) = ZERO
 END DO

 650 DO J = 1, N0
   IF (BASIS(J) == 0) THEN
     R(J) = DOT_PRODUCT( Y(1:M), A(1:M,J) )
   ELSE
     R(J) = ZERO
   END IF
 END DO

 660 IF (N0 == NS) GO TO 670
 JMIN = N0 + 1
 DO J = JMIN, NS
   R(J) = ZERO
   IF (BASIS(J) /= 0) CYCLE
   JJ = J - N0
   R(J) = Y(JJ)
 END DO

 670 IF (NS == N) GO TO 200
 JMIN = NS + 1

 DO J = JMIN, N
   R(J) = ZERO
   IF (BASIS(J) /= 0) CYCLE
   JJ = J - N0
   R(J) = -Y(JJ)
 END DO
 GO TO 200

 !      SET UP A NEW R ARRAY FOR THE NSTEP = 3 CASE

 680 NSTEP = 3
 DO J = 1, M
   DSUM = ZERO
   DO L = 1, M
     IL = IBASIS(L)
     IF (IL <= N0) DSUM = DSUM + C(IL)*BI(L,J)
   END DO
   Y(J) = DSUM
 END DO

 DO J = 1, N0
   R(J) = ZERO
   IF (BASIS(J) /= 0) CYCLE
   DSUM = -C(J) + DOT_PRODUCT( Y(1:M), A(1:M,J) )
   R(J) = DSUM
   IF (R(J) >= ZERO) CYCLE
   TOL = RERRMX*ABS(C(J))
   IF (ABS(R(J)) < TOL) R(J) = ZERO
 END DO
 GO TO 660

 !       UPDATE THE R ARRAY FOR THE NSTEP = 3 CASE

 700 CONST = R(JP)
 DO J = 1, N0
   IF (BASIS(J) /= 0) THEN
     R(J) = ZERO
   ELSE
     TOTAL = DOT_PRODUCT( BI(IP,1:M), A(1:M,J) )
     R(J) = R(J) - CONST*TOTAL
     IF (R(J) >= ZERO) CYCLE
     TOL = RERRMX*ABS(C(J))
     IF (ABS(R(J)) < TOL) R(J) = ZERO
   END IF
 END DO

 IF (N0 == NS) GO TO 720
 JMIN = N0 + 1
 DO J = JMIN, NS
   IF (BASIS(J) /= 0) THEN
     R(J) = ZERO
   ELSE
     JJ = J - N0
     R(J) = R(J) - CONST*BI(IP,JJ)
   END IF
 END DO

 720 IF (NS == N) GO TO 200
 JMIN = NS + 1
 DO J = JMIN, N
   IF (BASIS(J) /= 0) THEN
     R(J) = ZERO
   ELSE
     JJ = J - N0
     R(J) = R(J) + CONST*BI(IP,JJ)
   END IF
 END DO
 GO TO 200
 !-----------------------------------------------------------------------
 !               REFINE XB AND STORE THE RESULT IN Y
 !-----------------------------------------------------------------------
 800 Y(1:M) = ZERO

 M0 = 0
 DO J = 1, M
   KJ = IBASIS(J)
   IF (KJ <= N0) GO TO 810
   IF (KJ <= NS) GO TO 820
   IF (KJ <= N) GO TO 830
   GO TO 820

   810 M0 = M0 + 1
   INDX(M0) = J
   CYCLE

   820 L = KJ - N0
   Y(L) = XB(J)
   CYCLE

   830 L = KJ - N0
   Y(L) = -XB(J)
 END DO

 IF (M0 == 0) THEN
   R(1:M) = B0(1:M) - Y(1:M)
 ELSE
   DO I = 1, M
     DSUM = Y(I)
     DO JJ = 1, M0
       J = INDX(JJ)
       KJ = IBASIS(J)
       DSUM = DSUM + A(I,KJ)*XB(J)
     END DO
     R(I) = B0(I) - DSUM
   END DO
 END IF

 RERR1 = MIN(RERRMX, RERR)
 DO I = 1, M
   Y(I) = ZERO
   IF (XB(I) < ZERO) THEN
     SGN = -1.0_DP
     DSUMP = ZERO
     DSUMN = XB(I)
   ELSE IF (XB(I) > ZERO) THEN
     SGN = 1.0_DP
     DSUMP = XB(I)
     DSUMN = ZERO
   ELSE
     CYCLE
   END IF
   DO L = 1, M
     DT = BI(I,L)*R(L)
     IF (DT > ZERO) THEN
       DSUMP = DSUMP + DT
     ELSE
       DSUMN = DSUMN + DT
     END IF
   END DO
   W = DSUMP + DSUMN
   IF (W == ZERO) CYCLE
   IF (SGN /= SIGN(1.0_DP, W)) CYCLE
   S = DSUMP
   T = DSUMN
   TOL = RERR1*MAX(S, -T)
   IF (ABS(W) > TOL) Y(I) = W
 END DO
 IF (NSTEP == 2) GO TO 870
 IF (NSTEP > 2) GO TO 880

 !         CHECK THE REFINEMENT (NSTEP = 1)

 DO I = 1, M
   IF (Y(I) >= ZERO) GO TO 861
   IF (Y(I) < -RERRMX) GO TO 240
   Y(I) = ZERO
   861 XB(I) = Y(I)
 END DO
 GO TO 630

 !         CHECK THE REFINEMENT (NSTEP = 2)

 870 DO I = 1, M
   IF (IBASIS(I) <= N) GO TO 871
   IF (Y(I) > RERRMX) GO TO 240
   Y(I) = ZERO
   871 XB(I) = Y(I)
 END DO
 GO TO 680

 !              COMPUTE Z  (NSTEP = 3)

 880 DSUM = ZERO
 DO I = 1, M
   KI = IBASIS(I)
   IF (KI > N0) GO TO 881
   DSUM = DSUM + C(KI)*Y(I)
   881 XB(I) = Y(I)
 END DO
 Z = DSUM
 GO TO 220
 END SUBROUTINE SMPLX1

 !###======================================================================
 SUBROUTINE CROUT1(A, KA, N, IEND, INDX, TEMP, IERR)
 !###======================================================================
 !     ******************************************************************
 !     CROUT PROCEDURE FOR INVERTING MATRICES
 !     ******************************************************************
 !     A IS A MATRIX OF ORDER N WHERE N IS GREATER THAN OR EQUAL TO 1.
 !     THE INVERSE OF A IS COMPUTED AND STORED IN A.

 !     KA = LENGTH OF THE COLUMNS OF THE ARRAY A

 !     IEND MAY BE 0, 1, ..., N-1.  IT IS ASSUMED THAT EACH OF THE FIRST
 !     IEND COLUMNS OF THE MATRIX A CONTAINS ONLY ONE NONZERO ELEMENT
 !     AND THAT THE NONZERO ELEMENT IS 1 OR -1.

 !     INDX IS AN ARRAY OF DIMENSION N-1 OR LARGER THAT IS USED BY THE
 !     ROUTINE FOR KEEPING TRACK OF THE ROW INTERCHANGES THAT ARE MADE.

 !     TEMP IS A TEMPORARY STORAGE ARRAY.

 !     IERR REPORTS THE STATUS OF THE RESULTS. IF A IS NONSINGULAR THEN
 !     THE INVERSE OF A IS COMPUTED AND IERR=0. OTHERWISE IF A IS FOUND
 !     TO BE SINGULAR THEN IERR=1 AND THE ROUTINE TERMINATES.
 !     --------------------
! USE CONSTANTS_NSWC
 IMPLICIT NONE
 INTEGER, INTENT(IN)                     :: KA, N, IEND
 INTEGER, INTENT(OUT)                    :: IERR
 REAL (DP), DIMENSION(KA*N), INTENT(IN OUT) :: A!, TEMP
 REAL (DP), DIMENSION(:), INTENT(IN OUT) :: TEMP
 INTEGER, DIMENSION(:), INTENT(IN OUT)   :: INDX

 !     LOCAL VARIABLES
 INTEGER   :: I, IBEG, IJ, IK, IL, J, JCOL, JJ, K, KCOL, KCOUNT, KJ,  &
              KJ0, KK, KL, KM1, KP1, L, LJ, LJ0, LK, LMIN, MAXDIM, MCOL,  &
              NCOL, NK, NM1, NMJ, NMK, NN
 REAL (DP) :: ZERO = 0._DP
 REAL (DP) :: DSUM, C, PMIN, S

 MAXDIM = KA*N
 MCOL = IEND*KA
 IF (IEND == 0) GO TO 100

 !           PROCESS THE FIRST IEND COLUMNS OF A

 KCOL = 0
 DO K = 1, IEND
   KK = KCOL + K
   NK = KCOL + N
   DO LK = KK, NK
     IF (A(LK) < ZERO) GO TO 20
     IF (A(LK) > ZERO) GO TO 30
   END DO
   GO TO 300

   20 L = LK - KCOL
   LJ0 = MCOL + L
   DO LJ = LJ0, MAXDIM, KA
     A(LJ) = -A(LJ)
   END DO

   30 L = LK - KCOL
   INDX(K) = L
   IF (K == L) GO TO 32
   LJ = LK
   DO KJ = KK, MAXDIM, KA
!WRITE(*,*) SIZE(A),KJ,KA,MAXDIM
     C = A(KJ)
     A(KJ) = A(LJ)
     A(LJ) = C
     LJ = LJ + KA
   END DO
   32 KCOL = KCOL + KA
 END DO

 !           PROCESS THE REMAINING COLUMNS OF A

 100 NM1 = N - 1
 IERR = 0
 PMIN = ZERO
 IBEG = IEND + 1
 IF (IBEG == N) GO TO 190

 K = IBEG
 KM1 = IEND
 KP1 = K + 1
 KCOL = MCOL
 KK = KCOL + K
 DO KCOUNT = IBEG, NM1

 !     SEARCH FOR THE K-TH PIVOT ELEMENT (K=IBEG, ..., N-1)

   L = K
   S = ABS(A(KK))
   DO I = KP1, N
     IK = KCOL + I
     C = ABS(A(IK))
     IF (S >= C) CYCLE
     L = I
     S = C
   END DO

   IF (K > IBEG .AND. S >= PMIN) GO TO 120
   PMIN = S
   IF (S == ZERO) GO TO 300

 !              INTERCHANGING ROWS K AND L

   120 INDX(K) = L
   IF (K == L) GO TO 130
   KJ0 = MCOL + K
   LJ  = MCOL + L
   DO KJ = KJ0, MAXDIM, KA
     C = A(KJ)
     A(KJ) = A(LJ)
     A(LJ) = C
     LJ = LJ + KA
   END DO

 !       COMPUTE THE K-TH ROW OF U (K=IBEG, ..., N-1)

   130 C = A(KK)
   IF (K > IBEG) GO TO 140
   KJ0 = KK + KA
   DO KJ = KJ0, MAXDIM, KA
     A(KJ) = A(KJ)/C
   END DO
   GO TO 160

   140 KL = MCOL + K
   DO L = IBEG, KM1
     TEMP(L) = A(KL)
     KL = KL + KA
   END DO

   KJ0 = KK + KA
   DO KJ = KJ0, MAXDIM, KA
     JCOL = KJ - K
     DSUM = -A(KJ)
     DO L = IBEG, KM1
       LJ = JCOL + L
       DSUM = DSUM + TEMP(L)*A(LJ)
     END DO
     A(KJ) = -DSUM/C
   END DO

 !      COMPUTE THE K-TH COLUMN OF L (K=IBEG+1, ..., N)

   160 KM1 = K
   K = KP1
   KP1 = K + 1
   KCOL = KCOL + KA
   KK = KCOL + K
   DO L = IBEG, KM1
     LK = KCOL + L
     TEMP(L) = A(LK)
   END DO

   DO I = K, N
     IL = MCOL + I
     DSUM = ZERO
     DO L = IBEG, KM1
       DSUM = DSUM + A(IL)*TEMP(L)
       IL = IL + KA
     END DO
     A(IL) = A(IL) - DSUM
   END DO
 END DO

 !           CHECK THE N-TH PIVOT ELEMENT

 190 NCOL = MAXDIM - KA
 NN = NCOL + N
 C = ABS(A(NN))
 IF (C > PMIN) GO TO 200
 IF (C == ZERO) GO TO 300

 !          REPLACE L WITH THE INVERSE OF L

 200 IF (IBEG == N) GO TO 213
 JJ = MCOL + IBEG
 I = KA + 1
 DO J = IBEG, NM1
   A(JJ) = 1.0_DP/A(JJ)
   TEMP(J) = A(JJ)
   KJ = JJ
   DO KM1 = J, NM1
     K = KM1 + 1
     KJ = KJ + 1
     DSUM = ZERO
     KL = KJ
     DO L = J, KM1
       DSUM = DSUM + A(KL)*TEMP(L)
       KL = KL + KA
     END DO
     A(KJ) = -DSUM/A(KL)
     TEMP(K) = A(KJ)
   END DO
   JJ = JJ + I
 END DO
 213 A(NN) = 1.0_DP/A(NN)
 IF (N == 1) RETURN

 !       SOLVE UX = Y WHERE Y IS THE INVERSE OF L

 DO NMK = 1, NM1
   K = N - NMK
   LMIN = MAX(IBEG, K+1)
   KL = (LMIN-1)*KA + K
   DO L = LMIN, N
     TEMP(L) = A(KL)
     A(KL) = ZERO
     KL = KL + KA
   END DO

   KJ0 = MCOL + K
   DO KJ = KJ0, MAXDIM, KA
     DSUM = -A(KJ)
     LJ = (KJ - K) + LMIN
     DO L = LMIN, N
       DSUM = DSUM + TEMP(L)*A(LJ)
       LJ = LJ + 1
     END DO
     A(KJ) = -DSUM
   END DO
 END DO

 !                 COLUMN INTERCHANGES

 JCOL = NCOL - KA
 DO NMJ = 1, NM1
   J = N - NMJ
   K = INDX(J)
   IF (J == K) GO TO 251
   IJ = JCOL
   IK = (K-1)*KA
   DO I = 1, N
     IJ = IJ + 1
     IK = IK + 1
     C = A(IJ)
     A(IJ) = A(IK)
     A(IK) = C
   END DO
   251 JCOL = JCOL - KA
 END DO
 RETURN

 !                    ERROR RETURN

 300 IERR = 1
 RETURN

END SUBROUTINE CROUT1

END MODULE SMPLX_COMPUTE
