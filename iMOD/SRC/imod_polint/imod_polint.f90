!!  Copyright (C) Stichting Deltares, 2005-2016.
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
MODULE MOD_POLINT

USE WINTERACTER

CONTAINS

 !###==================================================================
 SUBROUTINE POL1INTMAIN(NCOL,NROW,NPC,NPR,XCRD,YCRD,ZCRD,DELR,DELC,X, &
                        IINT,NODATA)
 !###==================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NCOL,NROW,NPC,NPR,IINT
 REAL,INTENT(IN) :: NODATA
 REAL,DIMENSION(NPC),INTENT(IN) :: XCRD       !fixed point x-coordinates
 REAL,DIMENSION(NPR),INTENT(IN) :: YCRD       !fixed point y-coordinates
 REAL,DIMENSION(NPC,NPR),INTENT(IN) :: ZCRD   !fixed point values
 REAL,DIMENSION(0:NCOL),INTENT(IN) :: DELR
 REAL,DIMENSION(0:NROW),INTENT(IN) :: DELC
 REAL,DIMENSION(NCOL,NROW),INTENT(INOUT) :: X
 REAL :: Y,DY,X1,X2
 INTEGER :: NMAX,IROW,ICOL 
 REAL,ALLOCATABLE,DIMENSION(:) :: C,D,YMTMP,YNTMP

 NMAX=MAX(NPR,NPC)
 IF(ALLOCATED(C))DEALLOCATE(C)
 IF(ALLOCATED(D))DEALLOCATE(D)
 IF(ALLOCATED(YMTMP))DEALLOCATE(YMTMP)
 IF(ALLOCATED(YNTMP))DEALLOCATE(YNTMP)
 ALLOCATE(C(NMAX),D(NMAX),YNTMP(NPR),YMTMP(NPC))

 !## loop over all points!
 DO IROW=1,NROW
  DO ICOL=1,NCOL
   X1=(DELR(ICOL-1)+DELR(ICOL))/2.0
   X2=(DELC(IROW-1)+DELC(IROW))/2.0
   CALL POL2DINT(XCRD,YCRD,ZCRD,C,D,NMAX,YMTMP,YNTMP,NPC,NPR,X1,X2,Y,DY,IINT,NODATA)
   X(ICOL,IROW)=Y
  ENDDO
 ENDDO

 END SUBROUTINE POL1INTMAIN

 !###==================================================================
 SUBROUTINE POL2DINT(XCRD,YCRD,ZCRD,C,D,NMAX,YMTMP,YNTMP,NPC,NPR,XINT,&
                     YINT,Y,DY,IINT,NODATA)
 !###==================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NPC,NPR,NMAX,IINT
 REAL,INTENT(IN) :: NODATA
 REAL,DIMENSION(NPC),INTENT(IN) :: XCRD
 REAL,DIMENSION(NPR),INTENT(IN) :: YCRD
 REAL,DIMENSION(NPC,NPR) :: ZCRD
 REAL,INTENT(IN) :: XINT,YINT
 REAL,INTENT(OUT) :: DY,Y
 REAL,DIMENSION(NPR),INTENT(INOUT) :: YNTMP
 REAL,DIMENSION(NPC),INTENT(INOUT) :: YMTMP
 REAL,DIMENSION(NMAX),INTENT(INOUT) :: C,D
 INTEGER :: J,K,XPOS,XPOS1,XPOS2,DXPOS,YPOS,YPOS1,YPOS2,DYPOS,DPOS,INODATA
 
 CALL POL1LOCATE(XCRD,NPC,REAL(XINT,8),XPOS)
 CALL POL1LOCATE(YCRD,NPR,REAL(YINT,8),YPOS)

 DPOS =INT(SQRT(REAL(IINT)))   !iint=4,dpos=2; iint=16,dpos=4
 DPOS =DPOS/2

 XPOS1=MAX(1,XPOS-(DPOS-1))
 XPOS2=MIN(XPOS+DPOS,NPC)
 DXPOS=(XPOS2-XPOS1)+1

 YPOS1=MAX(1,YPOS-(DPOS-1))
 YPOS2=MIN(YPOS+DPOS,NPR)
 DYPOS=(YPOS2-YPOS1)+1

 INODATA=0
 JLOOP: DO J=XPOS1,XPOS2
  DO K=YPOS1,YPOS2
   YNTMP(K)=ZCRD(J,K)
   IF(ZCRD(J,K).EQ.NODATA)THEN
    INODATA=1
    EXIT JLOOP
   ENDIF
  END DO
  CALL POL1DINT(YCRD(YPOS1),YNTMP(YPOS1),C,D,DYPOS,NMAX,YINT,YMTMP(J),DY)
 END DO JLOOP

 IF(INODATA.EQ.1)THEN
  Y =NODATA
  DY=0.0
 ELSE
  CALL POL1DINT(XCRD(XPOS1),YMTMP(XPOS1),C,D,DXPOS,NMAX,XINT,Y,DY)
 ENDIF

 END SUBROUTINE POL2DINT

 !###==================================================================
 SUBROUTINE POL1DINT(XA,YA,C,D,NPR,NMAX,X,Y,DY)
 !###==================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NPR,NMAX
 REAL,DIMENSION(NPR),INTENT(IN) :: XA,YA
 REAL,DIMENSION(NMAX),INTENT(INOUT) :: C,D
 REAL,INTENT(IN) :: X
 REAL,INTENT(OUT) :: DY,Y
 INTEGER :: I,M,NS
 REAL :: DEN,DIF,DIFT,HO,HP,W

 NS =1
 DIF=ABS(X-XA(1))

 DO I=1,NPR
  DIFT=ABS(X-XA(I))
  IF(DIFT.LT.DIF)THEN
   NS =I
   DIF=DIFT
  ENDIF
  C(I)=YA(I)
  D(I)=YA(I)
 END DO

 Y =YA(NS)
 NS=NS-1

 DO M=1,NPR-1
  DO I=1,NPR-M
   HO =XA(I)-X
   HP =XA(I+M)-X
   W  =C(I+1)-D(I)
   DEN=HO-HP
   IF(DEN.EQ.0.0)PAUSE 'FAILURE IN POLINT' !## occurs whenever two xa(i) are almost the same
   DEN=W/DEN
   D(I)=HP*DEN
   C(I)=HO*DEN
  END DO
  IF(2*NS.LT.NPR-M)THEN
   DY=C(NS+1)
  ELSE
   DY=D(NS)
   NS=NS-1
  ENDIF
  Y=Y+DY
 END DO

 END SUBROUTINE POL1DINT

 !###==================================================================
 SUBROUTINE POL1LOCATE(XX,N,X,J)
 !###==================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: J
 INTEGER,INTENT(IN) :: N
 DOUBLE PRECISION,INTENT(IN) :: X
 REAL,INTENT(IN),DIMENSION(N) :: XX
 INTEGER :: JL,JM,JU

 JL=0
 JU=N+1
 DO
  IF(JU-JL.GT.1)THEN
   JM=(JU+JL)/2
   IF((XX(N).GT.XX(1)).EQV.(X.GT.XX(JM)))THEN
    JL=JM
   ELSE
    JU=JM
   ENDIF
  ELSE
   EXIT
  ENDIF
 ENDDO

 J=JL

 END SUBROUTINE POL1LOCATE

 !###==================================================================
 SUBROUTINE SPLINE_MAIN(XB,YB,N,XI,ZI,M)
 !###==================================================================
 ! SPLINE INTERPOLATION
 ! COMMENTS: VALUES OF FUNCTION F(X) ARE CALCULATED IN N BASE POINTS
 ! THEN: SPLINE COEFFICIENTS ARE COMPUTED
 !       SPLINE INTERPOLATION IS COMPUTED IN 2N-1 POINTS, 
 !       A DIFFERENCE SUM|F(U)-ISPLINE(U)| 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,M       
 REAL,DIMENSION(N),INTENT(IN) :: XB,YB
 REAL,DIMENSION(:),POINTER,INTENT(OUT) :: XI,ZI
 REAL,DIMENSION(:),ALLOCATABLE :: B,C,D
 REAL :: X
 INTEGER :: I

 ALLOCATE(B(N),C(N),D(N))
 
 !## call spline to calculate spline coeficients
 CALL SPLINE(XB,YB,B,C,D,N) 

 DO I=1,M
  X=XI(I)
  ZI(I)=ISPLINE(X,XB,YB,B,C,D,N)
 ENDDO
 
 DEALLOCATE(B,C,D)
 
 END SUBROUTINE SPLINE_MAIN

 !###==================================================================
 SUBROUTINE SPLINE(X,Y,B,C,D,N)
 !###==================================================================
 ! CALCULATE THE COEFFICIENTS B(I), C(I), AND D(I), I=1,2,...,N
 ! FOR CUBIC SPLINE INTERPOLATION
 ! S(X) = Y(I) + B(I)*(X-X(I)) + C(I)*(X-X(I))**2 + D(I)*(X-X(I))**3
 ! FOR  X(I) <= X <= X(I+1)
 ! ALEX G: JANUARY 2010
 !----------------------------------------------------------------------
 !  X = THE ARRAYS OF DATA ABSCISSAS (IN STRICTLY INCREASING ORDER)
 !  Y = THE ARRAYS OF DATA ORDINATES
 !  N = SIZE OF THE ARRAYS XI() AND YI() (N>=2)
 !  OUTPUT..
 !  B, C, D  = ARRAYS OF SPLINE COEFFICIENTS
 !  COMMENTS ...
 !  SPLINE.F90 PROGRAM IS BASED ON FORTRAN VERSION OF PROGRAM SPLINE.F
 !  THE ACCOMPANYING FUNCTION FSPLINE CAN BE USED FOR INTERPOLATION
 !###========================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL,INTENT(IN),DIMENSION(N) :: X,Y
 REAL,INTENT(OUT),DIMENSION(N) :: B,C,D
 INTEGER :: I,J,GAP
 REAL :: H

 GAP = N-1
! CHECK INPUT
 IF ( N .LT. 2 ) RETURN
 IF ( N .LT. 3 ) THEN
  B(1) = (Y(2)-Y(1))/(X(2)-X(1))   ! LINEAR INTERPOLATION
  C(1) = 0.
  D(1) = 0.
  B(2) = B(1)
  C(2) = 0.
  D(2) = 0.
  RETURN
 END IF
!
! STEP 1: PREPARATION
!
 D(1) = X(2) - X(1)
 C(2) = (Y(2) - Y(1))/D(1)
 DO I = 2, GAP
  D(I) = X(I+1) - X(I)
  B(I) = 2.0*(D(I-1) + D(I))
  C(I+1) = (Y(I+1) - Y(I))/D(I)
  C(I) = C(I+1) - C(I)
 END DO
!
! STEP 2: END CONDITIONS 
!
 B(1) = -D(1)
 B(N) = -D(N-1)
 C(1) = 0.0
 C(N) = 0.0
 IF(N .NE. 3) THEN
  C(1) = C(3)/(X(4)-X(2)) - C(2)/(X(3)-X(1))
  C(N) = C(N-1)/(X(N)-X(N-2)) - C(N-2)/(X(N-1)-X(N-3))
  C(1) = C(1)*D(1)**2/(X(4)-X(1))
  C(N) = -C(N)*D(N-1)**2/(X(N)-X(N-3))
 END IF
!
! STEP 3: FORWARD ELIMINATION 
!
 DO I = 2, N
  H = D(I-1)/B(I-1)
  B(I) = B(I) - H*D(I-1)
  C(I) = C(I) - H*C(I-1)
 END DO
!
! STEP 4: BACK SUBSTITUTION
!
 C(N) = C(N)/B(N)
 DO J = 1, GAP
   I = N-J
   C(I) = (C(I) - D(I)*C(I+1))/B(I)
 END DO

 !## STEP 5: COMPUTE SPLINE COEFFICIENTS
 B(N) = (Y(N) - Y(GAP))/D(GAP) + D(GAP)*(C(GAP) + 2.0*C(N))
 DO I = 1, GAP
  B(I) = (Y(I+1) - Y(I))/D(I) - D(I)*(C(I+1) + 2.0*C(I))
  D(I) = (C(I+1) - C(I))/D(I)
  C(I) = 3.*C(I)
 END DO
 C(N) = 3.0*C(N)
 D(N) = D(N-1)

 END SUBROUTINE SPLINE

 !###==================================================================
 REAL FUNCTION ISPLINE(U,X,Y,B,C,D,N)
 !###==================================================================
 ! FUNCTION ISPLINE EVALUATES THE CUBIC SPLINE INTERPOLATION AT POINT Z
 ! ISPLINE = Y(I)+B(I)*(U-X(I))+C(I)*(U-X(I))**2+D(I)*(U-X(I))**3
 ! WHERE  X(I) <= U <= X(I+1)
 ! INPUT
 ! U       = THE ABSCISSA AT WHICH THE SPLINE IS TO BE EVALUATED
 ! X, Y    = THE ARRAYS OF GIVEN DATA POINTS
 ! B, C, D = ARRAYS OF SPLINE COEFFICIENTS COMPUTED BY SPLINE
 ! N       = THE NUMBER OF DATA POINTS
 ! OUTPUT:
 ! ISPLINE = INTERPOLATED VALUE AT POINT U
 !###=========================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: U
 INTEGER,INTENT(IN) :: N
 REAL,DIMENSION(N),INTENT(IN) :: X(N),Y(N),B(N),C(N),D(N)
 INTEGER :: I,J,K
 REAL :: DX

 !## if u is ouside the x() interval take a boundary value (left or right)
 IF(U.LE.X(1)) THEN
  ISPLINE=Y(1)
  RETURN
 END IF
 IF(U.GE.X(N)) THEN
  ISPLINE=Y(N)
  RETURN
 END IF

 !## binary search for for i, such that x(i) <= u <= x(i+1)
 I=1
 J=N+1
 DO WHILE (J.GT.I+1)
  K=(I+J)/2
  IF(U.LT.X(K))THEN
   J=K
  ELSE
   I=K
  ENDIF
 END DO

 !# evaluate spline interpolation
 DX=U-X(I)
 ISPLINE=Y(I)+DX*(B(I)+DX*(C(I)+DX*D(I)))

 END FUNCTION ISPLINE

 !###==================================================================
 SUBROUTINE SPLINE_AKIMA_MAIN(XB,YB,N,XI,ZI,M)  
 !###==================================================================
 !********************************************************
 !*          Akima spline fitting subroutine             *
 !* ---------------------------------------------------- *
 !* The input table is X(i), Y(i), where Y(i) is the     *
 !* dependant variable. The interpolation point is xx,   *
 !* which is assumed to be in the interval of the table  *
 !* with at least one table value to the left, and three *
 !* to the right. The interpolated returned value is yy. *
 !* n is returned as an error check (n=0 implies error). *
 !* It is also assumed that the X(i) are in ascending    *
 !* order.                                               *
 !********************************************************
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,M
 REAL,DIMENSION(N),INTENT(IN) :: XB,YB
 REAL,DIMENSION(:),POINTER,INTENT(OUT) :: XI,ZI
 REAL,DIMENSION(:),ALLOCATABLE :: XM,Z,X,Y
 INTEGER :: NN,I,IERROR
 REAL :: DX,XX,SX1,SX2

 DX=10.0

 NN=N
      
 !## add one fake points to the begin
 NN=NN+1
 !## add three fake points to the end
 NN=NN+3

 ALLOCATE(XM(0:NN+3),Z(0:NN),X(0:NN),Y(0:NN)); X=0.0; Y=0.0

 !## add mid-section
 DO I=2,N+1; X(I)=XB(I-1); Y(I)=YB(I-1); ENDDO

 !## add to the begin
 DO I=1,0,-1; X(I)=X(I+1)-DX; Y(I)=Y(I+1); ENDDO

 !## add to the end
 DO I=N+2,N+4; X(I)=X(I-1)+DX; Y(I)=Y(I-1); ENDDO
 
 !## shift fix locations to avoid negative values in x-coordinates
 DO I=0,N+4
  X(I)=X(I)+DX*2.0
 ENDDO
 
 !## for spline it is not possible to have identical x(i) and x(i+1) coordinates
 DO I=1,N+4
  IF(X(I).LE.X(I-1))THEN
   SX1=0.0; IF(I.GT.1)SX1=MIN(DX/2.0,X(I-1)-X(I-2))
   SX2=0.0; IF(I.LT.N+4)SX2=MIN(DX/2.0,X(I+1)-X(I))
   X(I-1)=X(I-1)-SX1
   X(I)  =X(I)  +SX2
  ENDIF
 ENDDO
 
 DO I=1,M
  XM=0.0; Z=0.0
  XX=XI(I)+DX
  CALL INTERPOL_AKIMA(NN,X,Y,XX,ZI(I),XM,Z,IERROR)
  XI(I)=XX-DX
  !## could not spline current location
  IF(IERROR.EQ.1)THEN
write(*,*)
  ENDIF
 ENDDO
 
 DEALLOCATE(XM,Z,X,Y)
 
 END SUBROUTINE SPLINE_AKIMA_MAIN

 !###==================================================================
 SUBROUTINE INTERPOL_AKIMA(IV,X,Y,XX,YY,XM,Z,IERROR)
 !###==================================================================
 INTEGER,INTENT(IN) :: IV 
 REAL,INTENT(IN) :: XX
 REAL,INTENT(OUT) :: YY
 REAL,DIMENSION(0:IV),INTENT(INOUT) :: X,Y 
 REAL,DIMENSION(0:IV+3),INTENT(OUT) :: XM ! 
 REAL,DIMENSION(0:IV),INTENT(OUT) :: Z 
 INTEGER,INTENT(OUT) :: IERROR
 INTEGER :: I
 REAL :: A,B

 IERROR=0
 !SPECIAL CASE XX=0
 IF (XX.EQ.0.0) THEN
   YY=0.0; IERROR=1; RETURN
 END IF
 !CHECK TO SEE IF INTERPOLATION POINT IS CORRECT
 IF (XX.LT.X(1).OR.XX.GE.X(IV-3)) THEN
   IERROR=2 ; RETURN
 END IF
 X(0)=2.0*X(1)-X(2)
 !CALCULATE AKIMA COEFFICIENTS, A AND B
 DO I = 1, IV-1
   !SHIFT I TO I+2
   XM(I+2)=(Y(I+1)-Y(I))/(X(I+1)-X(I))
 END DO
 XM(IV+2)=2.0*XM(IV+1)-XM(IV)
 XM(IV+3)=2.0*XM(IV+2)-XM(IV+1)
 XM(2)=2.0*XM(3)-XM(4)
 XM(1)=2.0*XM(2)-XM(3)
 DO I = 1, IV
  A=ABS(XM(I+3)-XM(I+2))
  B=ABS(XM(I+1)-XM(I))
  IF (A+B.NE.0.0) GOTO 100
  Z(I)=(XM(I+2)+XM(I+1))/2.D0
  GOTO 200
100 Z(I)=(A*XM(I+1)+B*XM(I+2))/(A+B)
200 END DO
  !FIND RELEVANT TABLE INTERVAL
 I=0
300 I=I+1
 IF (XX.GT.X(I)) GOTO 300
 I=I-1
 
 !BEGIN INTERPOLATION
 B=X(I+1)-X(I)
 A=XX-X(I)
 YY=Y(I)+Z(I)*A+(3.0*XM(I+2)-2.0*Z(I)-Z(I+1))*A*A/B
 YY=YY+(Z(I)+Z(I+1)-2.0*XM(I+2))*A*A*A/(B*B)
 
 END SUBROUTINE INTERPOL_AKIMA

END MODULE MOD_POLINT
