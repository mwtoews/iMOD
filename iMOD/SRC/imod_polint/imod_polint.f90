!!  Copyright (C) Stichting Deltares, 2005-2014.
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

 !loop over all points!
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

END MODULE MOD_POLINT
