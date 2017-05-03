!!  Copyright (C) Stichting Deltares, 2005-2017.
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
MODULE MOD_INTERSECT

USE MOD_POLINT, ONLY : POL1LOCATE
USE MOD_INTERSECT_PAR
USE MOD_QKSORT

PRIVATE :: INTERSECT_SORT

DOUBLE PRECISION,PRIVATE :: XBEGIN,YBEGIN,A,B
INTEGER,PRIVATE :: IHOR,IVER

CONTAINS

 !###======================================================================
 SUBROUTINE INTERSECT_EQUI(XMIN,XMAX,YMIN,YMAX,CSX_IN,CSY_IN,XIN1,XIN2,YIN1,YIN2,N,LHFB)  
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: XMIN,XMAX,YMIN,YMAX,CSX_IN,CSY_IN
 REAL,INTENT(IN) :: XIN1,XIN2,YIN1,YIN2
 INTEGER,INTENT(OUT) :: N
 LOGICAL,INTENT(IN) :: LHFB
 REAL :: X,Y,XMN,XMX,YMN,YMX,DX,DY,LENG,TD,X1,X2,Y1,Y2,CSX,CSY
 INTEGER :: I,ICOL,IROW,ID,N_IN
 
 X1=XIN1; Y1=YIN1; X2=XIN2; Y2=YIN2
 
 IF(.NOT.ASSOCIATED(XA))ALLOCATE(XA(1000)); IF(.NOT.ASSOCIATED(YA))ALLOCATE(YA(1000))
 IF(.NOT.ASSOCIATED(FA))ALLOCATE(FA(1000)); IF(.NOT.ASSOCIATED(LN))ALLOCATE(LN(1000))
 IF(.NOT.ASSOCIATED(CA))ALLOCATE(CA(1000)); IF(.NOT.ASSOCIATED(RA))ALLOCATE(RA(1000))

 CSX=CSX_IN
 CSY=CSY_IN
 IF(LHFB)THEN
  CSX=CSX/2.0
  CSY=CSY/2.0
 ENDIF
 
 N_IN=N

 IF(.NOT.INTERSECT_EQUATION(XMIN,XMAX,YMIN,YMAX,X1,X2,Y1,Y2,N))RETURN
 
 !## find search box - result can be negative, does not matter!
 IF(X1.LT.XMIN)THEN
  XMN=XMIN
 ELSE 
  I=1
  XMN=XMIN+CSX*(INT((X1-XMIN)/CSX)+I)
 ENDIF
 IF(X2.GT.XMAX)THEN
  XMX=XMAX
 ELSE 
  I=0
  DX=X2-XMIN
  IF(MOD(DX,CSX).EQ.0.0)I=-1
  XMX=XMIN+CSX*(INT((X2-XMIN)/CSX)+I) 
 ENDIF
 
 Y=MIN(Y1,Y2)
 IF(Y.LT.YMIN)THEN
  YMN=YMIN
 ELSE
  I =0
  DY=YMAX-Y
  IF(MOD(DY,CSY).EQ.0.0)I=-1
  YMN=YMAX-CSY*(INT((YMAX-Y)/CSY)+I)
 ENDIF
 Y=MAX(Y1,Y2)
 IF(Y.GT.YMAX)THEN
  YMX=Y
 ELSE
  I =1
  YMX=YMAX-CSY*(INT((YMAX-Y)/CSY)+I)
 ENDIF
 
 !## not for horizontal lines
 IF(IHOR.EQ.0)THEN
 
  !## continue seach rest of intersections
  !## try intersections with x-axes firstly
  Y=YMN-CSY
  DO
   Y=Y+CSY
   IF(Y.GT.YMX)EXIT

   !## array overwritten
   N=N+1; CALL INTERSECT_RESIZEVECTORS(N) 

   IF(IVER.EQ.1)THEN
    XA(N)=X1 !## same as xmx
   ELSE
    XA(N)=(Y-B)/A
   ENDIF
   YA(N)=Y
 
  ENDDO

 ENDIF

 !## not for vertical lines
 IF(IVER.EQ.0)THEN 

  !## try intersections with y-axes secondly
  X=XMN-CSX
  DO
   X=X+CSX
   IF(X.GT.XMX)EXIT

   !## array overwritten
   N=N+1; CALL INTERSECT_RESIZEVECTORS(N) 
   XA(N)=X
   IF(IHOR.EQ.1)THEN
    YA(N)=Y1  !## same as ymx
   ELSE
    YA(N)=A*X+B
   ENDIF
 
  ENDDO
 
 ENDIF
 
 CSX=CSX_IN
 CSY=CSY_IN

 DX=X1-X2; DY=Y2-Y1
 CALL INTERSECT_SORT(DX,DY,N_IN+1,N)

 !## sample each of the point to determine irow/icol, overwrite point with this
 !## skip first and last, they represented already by the second and one-last point
 DO I=N_IN+2,N 

  !## mid point
  X   =(XA(I-1)+XA(I))/2.0
  Y   =(YA(I-1)+YA(I))/2.0
  IF(X.GE.XMIN)THEN
   ICOL=INT((X-XMIN)/CSX)+1
  ELSE
   ICOL=INT((X-XMIN)/CSX)-1
  ENDIF
  IF(Y.LE.YMAX)THEN  
   IROW=INT((YMAX-Y)/CSY)+1
  ELSE
   IROW=INT((YMAX-Y)/CSY)-1
  ENDIF
  
  TD=CSX*CSY; ID=0 !## fraction
  CALL INTERSECT_NCORNER(ID,TD,XMIN+(ICOL-1)*CSX,YMAX-(IROW-1)*CSY,X,Y,1)
  CALL INTERSECT_NCORNER(ID,TD,XMIN+ ICOL   *CSX,YMAX-(IROW-1)*CSY,X,Y,2)
  CALL INTERSECT_NCORNER(ID,TD,XMIN+ ICOL   *CSX,YMAX- IROW   *CSY,X,Y,3)
  CALL INTERSECT_NCORNER(ID,TD,XMIN+(ICOL-1)*CSX,YMAX- IROW   *CSY,X,Y,4)
  FA(I-1)=REAL(ID) 
  
  DX  =XA(I)-XA(I-1)
  DY  =YA(I)-YA(I-1)
  LENG=DX**2.0+DY**2.0; IF(LENG.GT.0.0)LENG=SQRT(LENG)

  !## store results in row/column indices
  CA(I-1)=REAL(ICOL)
  RA(I-1)=REAL(IROW)

  LN(I-1)=LENG
 END DO
 N=N-1

 END SUBROUTINE INTERSECT_EQUI
 
 !###======================================================================
 SUBROUTINE INTERSECT_NCORNER(ID,TD,X1,Y1,XC,YC,JD)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: ID
 REAL,INTENT(INOUT) :: TD
 INTEGER,INTENT(IN) :: JD
 REAL,INTENT(IN) :: X1,Y1,XC,YC
 REAL :: D
  
 !## get nearest corner
 D=(XC-X1)**2.0+(YC-Y1)**2.0
 IF(D.GT.0.0)THEN
  D=SQRT(D)
  IF(D.LT.TD)THEN
   ID=JD
   TD=D
  ENDIF
 ELSE
  ID=JD
  TD=0.0
 ENDIF

 END SUBROUTINE INTERSECT_NCORNER
 
 !###======================================================================
 SUBROUTINE INTERSECT_NONEQUI(DELR,DELC,NROW,NCOL,XIN1,XIN2,YIN1,YIN2,N,LHFB)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NROW,NCOL
 REAL,INTENT(IN),DIMENSION(0:NCOL) :: DELR
 REAL,INTENT(IN),DIMENSION(0:NROW) :: DELC
 REAL,INTENT(IN) :: XIN1,XIN2,YIN1,YIN2
 INTEGER,INTENT(OUT) :: N
 LOGICAL,INTENT(IN) :: LHFB
 REAL :: X,Y,XMN,XMX,YMN,YMX,DX,DY,LENG,XMIN,YMIN,XMAX,YMAX,X1,X2,Y1,Y2,CS,TD
 INTEGER :: I,ICOL,IROW,IMN,JMN,ID,N_IN
 
 X1=XIN1; Y1=YIN1; X2=XIN2; Y2=YIN2
 
 IF(.NOT.ASSOCIATED(XA))ALLOCATE(XA(1000)); IF(.NOT.ASSOCIATED(YA))ALLOCATE(YA(1000))
 IF(.NOT.ASSOCIATED(FA))ALLOCATE(FA(1000)); IF(.NOT.ASSOCIATED(LN))ALLOCATE(LN(1000))
 IF(.NOT.ASSOCIATED(CA))ALLOCATE(CA(1000)); IF(.NOT.ASSOCIATED(RA))ALLOCATE(RA(1000))

 XMIN=DELR(0)
 XMAX=DELR(NCOL)
 YMIN=DELC(NROW)
 YMAX=DELC(0)

 N_IN=N
 
 IF(.NOT.INTERSECT_EQUATION(XMIN,XMAX,YMIN,YMAX,X1,X2,Y1,Y2,N))RETURN
 
 !## continue search rest of intersections
 !## try intersections with y-axes firstly
 IMN=0; DO I=1,NROW 
  IF(DELC(I).GT.MIN(Y1,Y2))THEN; YMN=DELC(I); IMN=I; ENDIF
 END DO
 JMN=0; DO I=NROW,1,-1 
  IF(DELC(I).LT.MAX(Y1,Y2))THEN; YMX=DELC(I); JMN=I; ENDIF
 END DO

 IF(IMN.GT.0.AND.JMN.GT.0)THEN

  !## not for horizontal lines
  IF(IHOR.EQ.0)THEN

   CS=DELC(IMN-1)-DELC(IMN)
   IF(LHFB)CS=CS/2.0
   Y =YMN-CS
   DO

    Y=Y+CS
    IF(Y.GT.YMX)EXIT
    N=N+1; CALL INTERSECT_RESIZEVECTORS(N) 
    IF(IVER.EQ.1)THEN
     XA(N)=X1 !## same as xmx
    ELSE
     XA(N)=(Y-B)/A
    ENDIF
    YA(N)=Y

    !## double intersections, for better estimate for hfb
    IF(LHFB)THEN
     Y=Y+CS
     !## array overwritten
     N=N+1; CALL INTERSECT_RESIZEVECTORS(N)
     IF(IVER.EQ.1)THEN
      XA(N)=X1 !## same as xmx
     ELSE
      XA(N)=(Y-B)/A
     ENDIF
     YA(N)=Y
    ENDIF
  
    IMN  =IMN-1
    !## model is not bigger than line-segment
    IF(IMN.LE.0)EXIT
    CS=DELC(IMN-1)-DELC(IMN)
    IF(LHFB)CS=CS/2.0
   ENDDO

  ENDIF
 ENDIF

 !## try intersections with x-axes secondly
 IMN=0; DO I=NCOL,1,-1 
  IF(DELR(I).GT.X1)THEN; XMN=DELR(I); IMN=I; ENDIF
 END DO
 JMN=0; DO I=0,NCOL
  IF(DELR(I).LT.X2)THEN; XMX=DELR(I); JMN=I; ENDIF
 END DO

 IF(IMN.GT.0.AND.JMN.GT.0)THEN

  !## not for vertical lines
  IF(IVER.EQ.0)THEN

   CS=DELR(IMN-1)-DELR(IMN)
   IF(LHFB)CS=CS/2.0
   X =XMN-CS
   DO
   
    X=X+CS
    IF(X.GT.XMX)EXIT
    N=N+1; CALL INTERSECT_RESIZEVECTORS(N) 
    XA(N)=X
    IF(IHOR.EQ.1)THEN
     YA(N)=Y1  !## same as ymx
    ELSE
     YA(N)=A*X+B
    ENDIF

    !## double intersections, for better estimate for hfb
    IF(LHFB)THEN
     X=X+CS
     !## array overwritten
     N=N+1; CALL INTERSECT_RESIZEVECTORS(N) 
     XA(N)=X
     IF(IHOR.EQ.1)THEN
      YA(N)=Y1  !## same as ymx
     ELSE
      YA(N)=A*X+B
     ENDIF
    ENDIF
  
    IMN  =IMN+1
    !## model is not bigger than line-segment
    IF(IMN.GT.NCOL)EXIT
    CS=DELR(IMN)-DELR(IMN-1)
    IF(LHFB)CS=CS/2.0
   ENDDO

  ENDIF
 ENDIF

 !## sort intersections, determined by the one with the largest difference
 DX=X1-X2; DY=Y2-Y1
 CALL INTERSECT_SORT(DX,DY,N_IN+1,N)

 !## sample each of the point to determine irow/icol, overwrite point with this
 !## skip first and last, they represented already by the second and one-last point
 DO I=N_IN+2,N
  !## mid point
  X   =(XA(I-1)+XA(I))/2.0
  Y   =(YA(I-1)+YA(I))/2.0

  CALL POL1LOCATE(DELR,NCOL+1,REAL(X,8),ICOL)
  CALL POL1LOCATE(DELC,NROW+1,REAL(Y,8),IROW)
  
  TD=CS*2.0; ID=0 !## fraction
  IF(ICOL.GE.1.AND.ICOL.LE.NCOL.AND. &
     IROW.GE.1.AND.IROW.LE.NROW)THEN
   CALL INTERSECT_NCORNER(ID,TD,DELR(ICOL-1),DELC(IROW-1),X,Y,1)
   CALL INTERSECT_NCORNER(ID,TD,DELR(ICOL)  ,DELC(IROW-1),X,Y,2)
   CALL INTERSECT_NCORNER(ID,TD,DELR(ICOL)  ,DELC(IROW)  ,X,Y,3)
   CALL INTERSECT_NCORNER(ID,TD,DELR(ICOL-1),DELC(IROW)  ,X,Y,4)
  ELSE
   ICOL=0; IROW=0
  ENDIF
  
  FA(I-1)=REAL(ID) 

  DX  =XA(I)-XA(I-1)
  DY  =YA(I)-YA(I-1)
  LENG=DX**2.0+DY**2.0; IF(LENG.GT.0.0)LENG=SQRT(LENG)

  !## store results in row/column indices
  CA(I-1)=REAL(ICOL)
  RA(I-1)=REAL(IROW)

  LN(I-1)=LENG
 END DO
 N=N-1

 END SUBROUTINE INTERSECT_NONEQUI

 !###======================================================================
 SUBROUTINE INTERSECT_NULLIFY()
 !###======================================================================
 IMPLICIT NONE
 
 NULLIFY(XA,YA,FA,LN,CA,RA)
  
 END SUBROUTINE INTERSECT_NULLIFY

 !###======================================================================
 SUBROUTINE INTERSECT_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 
 IF(ASSOCIATED(XA))DEALLOCATE(XA)
 IF(ASSOCIATED(YA))DEALLOCATE(YA) 
 IF(ASSOCIATED(FA))DEALLOCATE(FA) 
 IF(ASSOCIATED(LN))DEALLOCATE(LN)
 IF(ASSOCIATED(CA))DEALLOCATE(CA)
 IF(ASSOCIATED(RA))DEALLOCATE(RA)
  
 END SUBROUTINE INTERSECT_DEALLOCATE
 
 !###======================================================================
 SUBROUTINE INTERSECT_RESIZEVECTORS(N)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: DN=100
 INTEGER,INTENT(IN) :: N
 INTEGER :: MM,NN,I
 
 IF(N.LE.SIZE(XA))RETURN
 
 NN=SIZE(XA)
 MM=NN+DN
 
 ALLOCATE(XA_DUMMY(MM),YA_DUMMY(MM),FA_DUMMY(MM),LN_DUMMY(MM),CA_DUMMY(MM),RA_DUMMY(MM))

 DO I=1,NN
  XA_DUMMY(I)=XA(I)
  YA_DUMMY(I)=YA(I)
  FA_DUMMY(I)=FA(I)
  LN_DUMMY(I)=LN(I)
  CA_DUMMY(I)=CA(I)
  RA_DUMMY(I)=RA(I)
 ENDDO
 
 DEALLOCATE(XA,YA,FA,LN,CA,RA)
 
 XA=>XA_DUMMY
 YA=>YA_DUMMY
 FA=>FA_DUMMY
 LN=>LN_DUMMY
 CA=>CA_DUMMY
 RA=>RA_DUMMY
 
 END SUBROUTINE INTERSECT_RESIZEVECTORS
 
 !###======================================================================
 LOGICAL FUNCTION INTERSECT_EQUATION(XMIN,XMAX,YMIN,YMAX,X1,X2,Y1,Y2,N)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: XMIN,XMAX,YMIN,YMAX
 REAL,INTENT(INOUT) :: X1,X2,Y1,Y2
 INTEGER,INTENT(INOUT) :: N
 DOUBLE PRECISION :: X,Y,DX,DY

 INTERSECT_EQUATION=.FALSE.

 XBEGIN=X1; YBEGIN=Y1

 !## arrange x1,x2,y1,y2 such that x1<x2
 IF(X1.GT.X2)THEN               
  X =X1; Y =Y1
  X1=X2; Y1=Y2
  X2=X;  Y2=Y
 ENDIF

 !## adjust perfect 45/135/215,305 aanpassen
! DX=X2-X1; DY=Y2-Y1
! IF(ABS(DX).EQ.ABS(DY))X1=X1+1.0

 !## use always mid between point x1,y1 and x2,y2 as first position
 N=N+1; CALL INTERSECT_RESIZEVECTORS(N) 
 XA(N)= X1; YA(N)= Y1
 N=N+1; CALL INTERSECT_RESIZEVECTORS(N) 
 XA(N)= X1; YA(N)= Y1

 N=N+1; CALL INTERSECT_RESIZEVECTORS(N) 
 XA(N)= (X1+X2)/2.0; YA(N)= (Y1+Y2)/2.0

 N=N+1; CALL INTERSECT_RESIZEVECTORS(N) 
 XA(N)= X2; YA(N)= Y2
 N=N+1; CALL INTERSECT_RESIZEVECTORS(N) 
 XA(N)= X2; YA(N)= Y2

 !## find mathematical expression for line: y=ax+b
 DX=X2-X1; DY=Y2-Y1
 IVER=0; IHOR=0
 IF(DX.EQ.0.0)IVER=1
 IF(DY.EQ.0.0)IHOR=1
 IF(DX.NE.0.0)THEN 
  A=DY/DX
  B=Y1-A*X1
 ENDIF

 INTERSECT_EQUATION=.TRUE.

 END FUNCTION INTERSECT_EQUATION

 !###======================================================================
 SUBROUTINE INTERSECT_SORT(DX,DY,N_IN,N)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: DX,DY
 INTEGER,INTENT(IN) :: N,N_IN
 DOUBLE PRECISION :: X,Y
 REAL :: LENG
 INTEGER :: I

 !## sort intersections, determined by the one with the largest difference
 IF(ABS(DX).GE.ABS(DY))THEN
  CALL QKSORTDOUBLE2(XA(N_IN:),YA(N_IN:),(N-N_IN)+1)
 ELSE
  CALL QKSORTDOUBLE2(YA(N_IN:),XA(N_IN:),(N-N_IN)+1)
 ENDIF

 !## resort - if neccessary
 IF(XA(N_IN).NE.XBEGIN.OR.YA(N_IN).NE.YBEGIN)THEN
  DO I=N_IN,N_IN+((N-N_IN)/2)
   X           =XA(I)
   XA(I)       =XA(N-I+N_IN) 
   XA(N-I+N_IN)=X
   Y           =YA(I)
   YA(I)       =YA(N-I+N_IN) 
   YA(N-I+N_IN)=Y
   LENG        =LN(I)
   LN(I)       =LN(N-I+N_IN)
   LN(N-I+N_IN)=LENG
  END DO
 ENDIF

 END SUBROUTINE INTERSECT_SORT

END MODULE
