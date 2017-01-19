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
MODULE MOD_ASC2IDF_HFB

USE MOD_ASC2IDF_PAR
USE MOD_INTERSECT_PAR
USE MOD_INTERSECT, ONLY : INTERSECT_EQUI,INTERSECT_DEALLOCATE
USE MOD_IDF_PAR
USE MOD_UTL, ONLY : UTL_GETUNIT
USE MOD_OSD

CONTAINS

 !###====================================================================
 SUBROUTINE ASC2IDF_HFB(IDF,NROW,NCOL,IPC,FNAME,ITB,TOP,BOT)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NCOL,NROW,ITB
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 TYPE(IDFOBJ),INTENT(INOUT),OPTIONAL :: TOP,BOT
 INTEGER(KIND=1),DIMENSION(NCOL,NROW,2),INTENT(OUT) :: IPC
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 REAL :: X1,Y1,X2,Y2,Z1,Z2,TL,DZ,NODATA,ZL,ZF
 INTEGER :: IU,I,J,IOS,N,IROW,ICOL,ILINE,NP,IC1,IC2,IR1,IR2,IP1,IP2,IL1,IL2
 CHARACTER(LEN=52) :: CID
 
 NODATA=HUGE(1.0)

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,ACTION='READ',STATUS='OLD',FORM='FORMATTED')
 IF(IU.EQ.0)THEN; WRITE(*,'(A)') 'Error opening '//TRIM(FNAME); RETURN; ENDIF
  
 IF(ITB.EQ.0)THEN; TOP%X=NODATA; BOT%X=NODATA; ENDIF

 !## use blnfile
 NP=0; ILINE=0
 DO
  READ(IU,*,IOSTAT=IOS) CID; IF(IOS.NE.0)EXIT
  I=0; ILINE=ILINE+1
  DO
   IF(ITB.EQ.0)THEN
    READ(IU,*,IOSTAT=IOS) X2,Y2,Z2; IF(IOS.NE.0)EXIT
   ELSE
    READ(IU,*,IOSTAT=IOS) X2,Y2; IF(IOS.NE.0)EXIT
   ENDIF

   I=I+1
   IF(I.GT.1)THEN

    !## intersect line
    N=0; CALL INTERSECT_EQUI(IDF%XMIN,IDF%XMAX,IDF%YMIN,IDF%YMAX,IDF%DX,IDF%DY,X1,X2,Y1,Y2,N,.TRUE.)

    IF(ITB.EQ.0)THEN
     !## skip, probably a perfect-vertical segment
     IF(SUM(LN(1:N)).LE.0.0)N=0
     TL=0.0; DZ=(Z2-Z1)/SUM(LN(1:N))
    ENDIF
      
    DO J=1,N
     ICOL=CA(J); IROW=RA(J) 
     IF(ICOL.GE.1.AND.IROW.GE.1.AND. &
        ICOL.LE.IDF%NCOL.AND.IROW.LE.IDF%NROW)THEN
      NP=NP+1; CALL ASC2IDF_INT_RESIZEVECTORS(NP,100)
      XP(NP)=REAL(ICOL); YP(NP)=REAL(IROW); FP(NP)=FA(J); WP(NP)=REAL(ILINE)

      IF(ITB.EQ.0)THEN
       IF(J.EQ.1)THEN; TL=0.5*LN(J)
       ELSE; TL=TL+0.5*LN(J-1)+0.5*LN(J); ENDIF
       ZL=Z1+(TL*DZ); ZP(NP)=ZL*100.0
      ENDIF

     ENDIF
    ENDDO
   ENDIF     
   X1=X2; Y1=Y2; IF(ITB.EQ.0)Z1=Z2
  ENDDO
 ENDDO
 CALL INTERSECT_DEALLOCATE()

 CLOSE(IU)

 CALL ASC2IDF_INT_RESIZEVECTORS(NP,0)

 DO I=2,SIZE(XP)
  IL1=INT(WP(I-1))
  IL2=INT(WP(I))
  !## similar line
  IF(IL1.NE.IL2)CYCLE

  IC1=INT(XP(I-1))
  IC2=INT(XP(I  ))
  IR1=INT(YP(I-1))
  IR2=INT(YP(I  ))
  IP1=INT(FP(I-1))
  IP2=INT(FP(I  ))
 
  CALL ASC2IDF_HFB_GETFACES(IC1,IC2,IR1,IR2,IP1,IP2,IPC,NROW,NCOL)

  IF(ITB.EQ.0)THEN
   !## fill in z-coordinates
   ZF=(ZP(I-1)+ZP(I))/2.0
   DO IROW=IR1,IR2; DO ICOL=IC1,IC2
    IF(TOP%X(ICOL,IROW).EQ.NODATA)THEN
     TOP%X(ICOL,IROW)=ZF/100.0
     BOT%X(ICOL,IROW)=ZF/100.0
    ELSE
     TOP%X(ICOL,IROW)=MAX(TOP%X(ICOL,IROW),ZF/100.0)
     BOT%X(ICOL,IROW)=MIN(BOT%X(ICOL,IROW),ZF/100.0)
    ENDIF
   ENDDO; ENDDO
  ENDIF
  
 ENDDO

 END SUBROUTINE ASC2IDF_HFB

 !###====================================================================
 SUBROUTINE ASC2IDF_HFB_GETFACES(IC1,IC2,IR1,IR2,IP1,IP2,IPC,NROW,NCOL)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IC1,IC2,IR1,IR2,IP1,IP2
 INTEGER,INTENT(IN) :: NROW,NCOL
 INTEGER(KIND=1),INTENT(INOUT),DIMENSION(NCOL,NROW,2) :: IPC
 INTEGER,DIMENSION(2) :: JPC,JPR,JC,JR,JP
 INTEGER :: I,IC,IR

 JC(1)=IC1; JC(2)=IC2
 JR(1)=IR1; JR(2)=IR2
 JP(1)=IP1; JP(2)=IP2

 DO I=1,2
  IF(JP(I).EQ.2.OR.JP(I).EQ.3)JPC(I)=JC(I)
  IF(JP(I).EQ.1.OR.JP(I).EQ.4)JPC(I)=JC(I)-1
  IF(JP(I).EQ.1.OR.JP(I).EQ.2)JPR(I)=JR(I)-1
  IF(JP(I).EQ.3.OR.JP(I).EQ.4)JPR(I)=JR(I)
 ENDDO

 !## do nothing, is similar point
 IF(JPR(1).EQ.JPR(2).AND.JPC(1).EQ.JPC(2))RETURN

 !## do nothing whenever jpc.eq.0 or jpr.eq.0
! IF(JPC(1).EQ.0.AND.JPC(2).EQ.0)RETURN
! IF(JPR(1).EQ.0.AND.JPR(2).EQ.0)RETURN
 IF(JPC(1).EQ.0.OR.JPC(2).EQ.0)RETURN
 IF(JPR(1).EQ.0.OR.JPR(2).EQ.0)RETURN

 !## horizontal fault ipc(,,1)=1
 IF(JPR(1).EQ.JPR(2).AND.JPC(1).NE.JPC(2))THEN
  IC=MAX(JPC(1),JPC(2)); IR=JPR(1); IPC(IC,IR,2)=INT(1,1)
 ENDIF
 !## vertical fault ipc(,,2)=1
 IF(JPC(1).EQ.JPC(2).AND.JPR(1).NE.JPR(2))THEN
  IC=JPC(1); IR=MAX(JPR(1),JPR(2)); IPC(IC,IR,1)=INT(1,1)
 ENDIF
 !## diagonal, add two faults
 IF(JPR(1).NE.JPR(2).AND.JPC(1).NE.JPC(2))THEN
  !## goto to the west
  IF(JPC(1).GT.JPC(2))THEN
   !## goto to the north-west
   IF(JPR(1).GT.JPR(2))THEN
    IC=MIN(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2)); IPC(IC,IR,1)=INT(1,1) !## vertical
    IC=MAX(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2)); IPC(IC,IR,2)=INT(1,1) !## horizontal
   !## goto to the south-west
   ELSE
    IC=MIN(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2)); IPC(IC,IR,1)=INT(1,1) !## vertical
    IC=MAX(JPC(1),JPC(2)); IR=MIN(JPR(1),JPR(2)); IPC(IC,IR,2)=INT(1,1) !## horizontal
   ENDIF
  !## goto to the east
  ELSE
   !## goto to the north-east
   IF(JPR(1).GT.JPR(2))THEN
    IC=MIN(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2)); IPC(IC,IR,1)=INT(1,1) !## vertical
    IC=MAX(JPC(1),JPC(2)); IR=MIN(JPR(1),JPR(2)); IPC(IC,IR,2)=INT(1,1) !## horizontal   
   !## goto to the south-east
   ELSE
    IC=MIN(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2)); IPC(IC,IR,1)=INT(1,1) !## vertical
    IC=MAX(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2)); IPC(IC,IR,2)=INT(1,1) !## horizontal  
   ENDIF
  ENDIF
 ENDIF

 if(ipc(6,25,1).eq.int(1,1))then
  write(*,*)
 endif
 
 END SUBROUTINE ASC2IDF_HFB_GETFACES
  
END MODULE MOD_ASC2IDF_HFB
