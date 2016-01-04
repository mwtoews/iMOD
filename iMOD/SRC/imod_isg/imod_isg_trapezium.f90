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
MODULE MOD_ISG_TRAPEZIUM

USE MOD_UTL, ONLY : UTL_POLYGON1AREA
USE MOD_QKSORT

CONTAINS

 !     Last change:  PTM  26 Nov 2010    1:35 pm
 !###====================================================================
 SUBROUTINE ISGCOMPUTETRAPEZIUM(X,Y,XSYM,YSYM,XTRAP,YTRAP,NTRAP,NDIM,N,AORG,ATRAP)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NDIM
 INTEGER,INTENT(OUT) :: NTRAP
 INTEGER,INTENT(INOUT) :: N
 REAL,INTENT(OUT) :: AORG,ATRAP
 REAL,INTENT(INOUT),DIMENSION(NDIM) :: X,Y
 REAL,INTENT(OUT),DIMENSION(4,NDIM) :: XTRAP,YTRAP
 REAL,INTENT(OUT),DIMENSION(NDIM) :: XSYM,YSYM
 INTEGER :: I,J,K,I1,I2,NN,N1,N2,N3,IMID,ITRAP
 REAL :: YMIN,XMIN,RC,RC1,DX,DY,XC,A1,A2,A3,YV,XT
 REAL,ALLOCATABLE,DIMENSION(:,:) :: XN,YN
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IPOS
 REAL :: AREA
 !INTEGER :: IU

 !IU=UTL_GETUNIT()
 !OPEN(IU,FILE='test.csv',STATUS='unknown')

 AORG =0.0
 ATRAP=0.0

 !DO I=1,N
 ! WRITE(*,*) I,X(I),Y(I)
 !END DO
 !WRITE(*,*)

 !determine number of points, CLIP outside point if they point downwards
 DO I=1,N-1
  IF(Y(I).GT.Y(I+1))EXIT
 END DO
 I1=I
 IF(I1.GE.N)RETURN
 DO I=N,2,-1
  IF(Y(I).GT.Y(I-1))EXIT
 END DO
 I2=I

 NN=I2-I1+1

 !##something wrong in profile, probably up-side-down
 IF(NN.LE.1)THEN
  N=0
  RETURN
 ENDIF

 X(1:NN)=X(I1:I2)
 Y(1:NN)=Y(I1:I2)

 !DO I=1,NN
 ! WRITE(*,*) I,X(I),Y(I),ndim
 !END DO
 !WRITE(*,*)

 AREA=UTL_POLYGON1AREA(X,Y,NN)
 !WRITE(*,*) 'AREA=',AREA
 !WRITE(*,*)

 !find lowest point
 YMIN=MAXVAL(Y(1:NN))
 DO I=1,NN
  IF(Y(I).LT.YMIN)THEN
   YMIN=Y(I)
   I1  =I
  ENDIF
 END DO

 !shift to mid-point
 X(1:NN)=X(1:NN)-X(I1)

 !#sort symmetric profile down- and upwards
 DO
  J=0
  DO I=2,I1
   IF(Y(I).GT.Y(I-1))THEN
    YV    =Y(I-1)
    Y(I-1)=Y(I)
    Y(I)  =YV
    J     =J+1
   ENDIF
  END DO
  IF(J.EQ.0)EXIT
 ENDDO
 DO
  J=0
  DO I=I1+1,N
   IF(Y(I).LT.Y(I-1))THEN
    YV    =Y(I-1)
    Y(I-1)=Y(I)
    Y(I)  =YV
    J     =J+1
   ENDIF
  END DO
  IF(J.EQ.0)EXIT
 ENDDO

 !DO I=1,NN
 ! WRITE(IU,*) I,X(I),Y(I)
 !END DO
 !PAUSE
 !WRITE(*,*) I1

 ALLOCATE(XN(NDIM,3),YN(NDIM,3),IPOS(NDIM))

 !construct two symmetric cross-sections
 N1=0
 DO I=1,I1
  N1      =N1+1
  XN(N1,1)=X(I)
  YN(N1,1)=Y(I)
 END DO

 !DO I=1,N1
 ! WRITE(*,*) I,XN(I,1),YN(I,1)
 !END DO
 !WRITE(*,*)

 N2=0
 DO I=NN,I1,-1
  N2      =N2+1
  XN(N2,2)=-1.0*X(I)
  YN(N2,2)=Y(I)
 END DO

 !DO I=1,N2
 ! WRITE(*,*) I,XN(I,2),YN(I,2)
 !END DO
 !WRITE(*,*)

 !#intersect first symmetric-cross-section
 N3  =0
 IPOS=0
 DO I=1,N1
  DO J=1,N2
   IF(YN(J,2).EQ.YN(I,1))THEN
    IF(IPOS(J).EQ.0)THEN
     N3      = N3+1
     XN(N3,3)=(XN(I,1)+XN(J,2))/2.0
     YN(N3,3)= YN(I,1)
     IPOS(J) =1
     EXIT
    ENDIF
   ENDIF
 !#intersect
   IF(YN(J,2).GT.YN(I,1).AND.YN(J+1,2).LT.YN(I,1))THEN
 !##get line-formulae
    XC=ISGGETX(XN(J,2),YN(J,2),XN(J+1,2),YN(J+1,2),YN(I,1))
    N3      = N3+1
    XN(N3,3)=(XN(I,1)+XC)/2.0
    YN(N3,3)= YN(I,1)
    EXIT
   ENDIF
  END DO
 END DO

 !WRITE(*,*) 'INTERSECT FIRST'
 !DO I=1,N3
 ! WRITE(*,*) I,XN(I,3),YN(I,3)
 !END DO
 !WRITE(*,*)

 !#intersect second symmetric-cross-section
 IPOS=0
 DO J=1,N2
  DO I=1,N1
   IF(YN(J,2).EQ.YN(I,1))THEN
    IF(IPOS(I).EQ.0)THEN
     N3      = N3+1
     XN(N3,3)=(XN(I,1)+XN(J,2))/2.0
     YN(N3,3)= YN(I,1)
     IPOS(I) =1
     EXIT
    ENDIF
   ENDIF
 !#intersect
   IF(YN(I,1).GT.YN(J,2).AND.YN(I+1,1).LT.YN(J,2))THEN
 !#get line-formulae
    XC=ISGGETX(XN(I,1),YN(I,1),XN(I+1,1),YN(I+1,1),YN(J,2))
    N3      = N3+1
    XN(N3,3)=(XN(J,2)+XC)/2.0
    YN(N3,3)= YN(J,2)
    EXIT
   ENDIF
  END DO
 END DO

 !WRITE(*,*) 'INTERSECT SECOND'
 !DO I=1,N3
 ! WRITE(*,*) I,XN(3,I),YN(3,I)
 !END DO
 !WRITE(*,*)

 !#sort sequence
 CALL UTL_QKSORT2(XN(:,3),YN(:,3),NDIM,N3)

 CALL ISGDOUBLES(N3,NDIM,XN,YN)

 J=N3
 DO I=1,N3
  J=J+1
  XN(J,3)=-1.0*XN(N3-I+1,3)
  YN(J,3)= YN(N3-I+1,3)
 END DO
 N3=J

 CALL ISGDOUBLES(N3,NDIM,XN,YN)

 !DO I=1,N3
 ! WRITE(IU,*) I,XN(I,3),YN(I,3)
 !END DO
 !WRITE(IU,*)

 AREA=UTL_POLYGON1AREA(XN(:,3),YN(:,3),N3)
 !WRITE(*,*) 'AREA=',AREA
 !WRITE(*,*)

 !#create surfaces for each interval
 IMID   =(N3-1)/2+1
 K      = IMID
 YN(K,2)= 0.0
 XN(K,2)= MINVAL(YN(1:N3,3))
 N1     = N3/2
 DO I=N1,1,-1
  K=K+1
  XN(1,1)=XN(I,3)
  XN(2,1)=XN(I+1,3)
  XN(3,1)=XN(N3-I,3)
  XN(4,1)=XN(N3-I+1,3)
  YN(1,1)=YN(I,3)
  YN(2,1)=YN(I+1,3)
  YN(3,1)=YN(N3-I,3)
  YN(4,1)=YN(N3-I+1,3)
 !#total area for current z-level
  YN(K,2)=UTL_POLYGON1AREA(XN(:,1),YN(:,1),4)
 ! WRITE(*,*) 'AREA=',K,YN(1,1),yn(K,2)
  XN(K,2)=YN(1,1)
 ENDDO

 !DO I=IMID,N3
 ! WRITE(*,*) I,XN(I,2),YN(I,2)
 !END DO

 ITRAP=0
 NN   =0
 J    =IMID
 DO I=IMID,N3-1
  NN=NN+1
  DX=XN(I,3)-XN(I+1,3)
  DY=YN(I,3)-YN(I+1,3)
  IF(DX.EQ.0.0)DX=10.0E-10
  RC=DY/DX
  IF(I.GT.IMID)THEN
   IF(RC.LT.RC1)THEN
    ITRAP=ITRAP+1
    CALL ISGFIT(I,J,NN,XTRAP(:,ITRAP),YTRAP(:,ITRAP),XN(:,3),YN(:,3),YN(:,2),NDIM)
    NN=1
    J =I
   ENDIF
  ENDIF
  RC1=RC
 ENDDO
 NN=NN+1

 ITRAP=ITRAP+1
 CALL ISGFIT(I,J,NN,XTRAP(:,ITRAP),YTRAP(:,ITRAP),XN(:,3),YN(:,3),YN(:,2),NDIM)

 NTRAP=ITRAP

 !DO I=1,NTRAP
 ! DO J=1,4
 !  WRITE(*,*) I,J,XTRAP(J,I),YTRAP(J,I)
 ! END DO
 !END DO
 !PAUSE

 !#correction of trapeziums, except first trapezium (of course)
 DO I=2,NTRAP
 !#extend
  A3  =0.0
  XT  =0.0
  XMIN=XTRAP(1,I)
 ! WRITE(*,*) 'minx=',XMIN

  DO J=1,I-1

 !  DO K=1,4
 !   WRITE(*,*) j,K,XTRAP(K,j),YTRAP(K,j)
 !  ENDDO

   XC=ISGGETX(XTRAP(1,J),YTRAP(1,J),XTRAP(4,J),YTRAP(4,J),YTRAP(1,I))

 !#surface trapezium previous (original, before extension)
   A1=ABS(UTL_POLYGON1AREA(XTRAP(:,J),YTRAP(:,J),4))

 !  WRITE(*,*) 'a1(original)=',a1,J

   IF(A1.GT.0.0)THEN

    XT=XT+XC
 !   WRITE(*,*) 'xt,xc,xmin=',xt,XC,XMIN

 !#it is not allowed to oversize maximum width of current profile(s)
    IF(XT.LT.XMIN)THEN
     XT        = XT-XC
     XC        = SUM(XTRAP(1,1:J))-XT !<--- KIEZEN VOOR RECHTHOEKIG PROFIEL!!!???
 !    XC        = XTRAP(1,J)-XT !<--- KIEZEN VOOR RECHTHOEKIG PROFIEL!!!???
 !WRITE(*,*) xt,xc,xtrap(1,j)
     XT        = XT+XC
     DX        = XC
     XTRAP(1,J)= DX
     XTRAP(2,J)=-1.0*XTRAP(1,J)
     XTRAP(3,J)= XTRAP(2,J)
     XTRAP(4,J)= XTRAP(1,J)
    ELSE
     XTRAP(1,J)= XC
     XTRAP(2,J)=-1.0*XTRAP(1,J)
    ENDIF

 !#extended cross-section
    YTRAP(1,J)= YTRAP(1,I)
    YTRAP(2,J)= YTRAP(1,I)

 !   DO K=1,4
 !    WRITE(*,*) j,K,XTRAP(K,j),YTRAP(K,j)
 !   ENDDO

 !#area of extended trapezium
    A2=ABS(UTL_POLYGON1AREA(XTRAP(:,J),YTRAP(:,J),4))
 !   WRITE(*,*) 'a1(extended)=',a2

    A3=A3+(A2-A1)
   ENDIF
  ENDDO

 !#area current trapezium i
  A1=ABS(UTL_POLYGON1AREA(XTRAP(:,I),YTRAP(:,I),4))

 ! WRITE(*,*) 'a1(second original)=',a1

 !#net area
  A2=A1-A3

 ! WRITE(*,*) a2,a1,a3

  DX=XT-XTRAP(1,I)

 !#if net area > 0.0 than add trapezium, otherwise skip it!
  IF(A2.GT.0.0.AND.DX.GT.0.0)THEN

 !  WRITE(*,*) 'XT=',XT,XTRAP(1,I),DX,A2/2.0

 !#new crosssection
   XTRAP(1,I)= 0.0
   XTRAP(2,I)= DX
   XTRAP(3,I)= 0.0 !to be fitted
   XTRAP(4,I)= 0.0

 !  DO K=1,4
 !   WRITE(*,*) i,K,XTRAP(K,I),YTRAP(K,I)
 !  ENDDO

   CALL ISGFITPOLYGON(XTRAP(:,I),YTRAP(:,I),4,A2/2.0)

 !  DO K=1,4
 !   WRITE(*,*) i,K,XTRAP(K,I),YTRAP(K,I)
 !  ENDDO

 !#make sure profile is always less wide on the bottom than on the top!
   IF(XTRAP(2,I).LT.XTRAP(3,I))THEN
    DX        =(XTRAP(3,I)-XTRAP(2,I))/2.0
    XTRAP(2,I)= DX
    XTRAP(3,I)= DX
    XTRAP(1,I)=-DX
    XTRAP(4,I)=-DX
   ENDIF

  ELSE
   XTRAP(:,I)=0.0
   YTRAP(:,I)=0.0
  ENDIF

 !#total area for current z-level
 !  WRITE(*,*) 'itrap',i,ABS(POLYGONAREA(XTRAP(:,I),YTRAP(:,I),4))
 !  YN(K,2)=POLYGONAREA(XN(:,1),YN(:,1),4)

 ! END DO
 ENDDO

 A1=0.0
 K =0
 DO I=1,NTRAP
  A2=ABS(UTL_POLYGON1AREA(XTRAP(:,I),YTRAP(:,I),4))
  IF(A2.GT.0.0)THEN
   K         =K+1
   XTRAP(:,K)=XTRAP(:,I)
   YTRAP(:,K)=YTRAP(:,I)
   A1=A1+ABS(UTL_POLYGON1AREA(XTRAP(:,I),YTRAP(:,I),4))
  ENDIF
 END DO
 NTRAP=K

 !WRITE(*,*) AREA,A1,NTRAP

 AORG =AREA
 ATRAP=A1

 !A1=0.0
 !DO I=1,ITRAP
 ! A1=A1+ABS(POLYGONAREA(XTRAP(:,I),YTRAP(:,I),4))
 ! DO J=1,4
 !  WRITE(IU,*) XTRAP(J,I),YTRAP(J,I)
 ! END DO
 !END DO

 !WRITE(*,*) AREA,A1

 N        =N3
 XSYM(1:N)=XN(1:N,3)
 YSYM(1:N)=YN(1:N,3)

 DEALLOCATE(XN,YN,IPOS)

 !CLOSE(IU)

 RETURN
 END SUBROUTINE

 !###====================================================================
 REAL FUNCTION ISGGETX(X1,Y1,X2,Y2,Y3)
 !###====================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: X1,Y1,X2,Y2,Y3
 REAL :: DX,DY,RC,B

 !#get line-formulae
 DX=X1-X2
 DY=Y1-Y2
 IF(DX.EQ.0.0)DX=10.0E-10
 RC     = DY/DX
 B      = Y1-RC*X1
 IF(RC.EQ.0.0)THEN
  ISGGETX=X1
 ELSE
  ISGGETX=(-Y3+B)/(-1.0*RC)
 ENDIF
 !ISGGETX=(-Y3+B)/(-1.0*RC)

 RETURN
 END FUNCTION

 !###====================================================================
 SUBROUTINE ISGDOUBLES(N3,NDIM,XN,YN)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NDIM
 INTEGER,INTENT(INOUT) :: N3
 REAL,INTENT(INOUT),DIMENSION(NDIM,3) :: XN,YN
 INTEGER :: J,I,N

 !DO I=1,N3
 ! WRITE(*,*) I,XN(I,3),YN(I,3)
 !END DO
 !WRITE(*,*)

 N =N3
 N3=0
 J =2
 DO I=2,N
  IF(XN(J-1,3).EQ.XN(J,3))THEN
   XN(J-1:N-1,3)=XN(J:N,3)
   YN(J-1:N-1,3)=YN(J:N,3)
  ELSE
   N3=N3+1
   J =J+1
  ENDIF
 END DO
 N3=N3+1

 !DO I=1,N3
 ! WRITE(*,*) I,XN(I,3),YN(I,3)
 !END DO
 !WRITE(*,*)

 RETURN
 END SUBROUTINE

 !###====================================================================
 SUBROUTINE ISGFIT(I,J,NN,XTRAP,YTRAP,XN,YN,XA,NDIM)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I,J,NN,NDIM
 REAL,INTENT(IN),DIMENSION(NDIM) :: XN,YN,XA
 REAL,INTENT(OUT),DIMENSION(NDIM) :: XTRAP,YTRAP
 REAL :: AREA
 INTEGER :: K

 AREA=0.0
 DO K=J+1,J+NN-1
  AREA=AREA+XA(K)
 ENDDO
 AREA=AREA/2.0

 !##compute x1
 XTRAP(1)=0.0
 XTRAP(2)=XN(I)
 XTRAP(3)=0.0
 XTRAP(4)=0.0
 YTRAP(1)=YN(I)
 YTRAP(2)=YN(I)
 YTRAP(3)=YN(J)
 YTRAP(4)=YN(J)

 CALL ISGFITPOLYGON(XTRAP,YTRAP,NDIM,AREA)

 RETURN
 END SUBROUTINE

 !###====================================================================
 SUBROUTINE ISGFITPOLYGON(X,Y,N,AREA)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL,INTENT(IN) :: AREA
 REAL,DIMENSION(N),INTENT(INOUT) :: X,Y
 REAL :: A,B

 A= 0.5*(X(1)*Y(2))- &
    0.5*(X(2)*Y(1))+ &
    0.5*(X(4)*Y(1))- &
    0.5*(X(1)*Y(4))+ &
    0.5*(X(2)*Y(3))- &
    0.5*(X(4)*Y(3))
 B= 0.5*Y(4)- &
    0.5*Y(2)
 !WRITE(*,*) A,B
 X(3)=ABS((AREA-ABS(A))/B)
 !WRITE(*,*) X(3)

 !X(5)=X(1)
 !Y(5)=Y(1)

 !WRITE(*,*) 'HALVE AREA ',ABS(POLYGONAREA(X,Y,4)),AREA

 X(1)=-1.0*X(2)
 X(4)=-1.0*X(3)
 !X(5)= X(1)

 !WRITE(*,*) 'FULL AREA  ',ABS(POLYGONAREA(X,Y,4)),AREA*2.0

 RETURN
 END SUBROUTINE

END MODULE MOD_ISG_TRAPEZIUM
