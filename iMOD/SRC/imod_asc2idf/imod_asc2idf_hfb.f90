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
 SUBROUTINE ASC2IDF_HFB(IDF,NROW,NCOL,IPC,FNAME)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NCOL,NROW
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER(KIND=1),DIMENSION(NCOL,NROW,2),INTENT(OUT) :: IPC
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 REAL :: X1,Y1,X2,Y2
 INTEGER :: IU,I,J,IOS,ID,N,IROW,ICOL,ILINE,NP,IC1,IC2,IR1,IR2,IP1,IP2,IL1,IL2

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,ACTION='READ',STATUS='OLD',FORM='FORMATTED')
 IF(IU.EQ.0)THEN
  WRITE(*,'(A)') 'Error opening '//TRIM(FNAME); RETURN
 ENDIF
  
 !## use blnfile
 NP=0; ILINE=0
 DO
  READ(IU,*,IOSTAT=IOS) ID; IF(IOS.NE.0)EXIT
  I=0; ILINE=ILINE+1
  DO
   READ(IU,*,IOSTAT=IOS) X2,Y2; IF(IOS.NE.0)EXIT
   I=I+1
   IF(I.GT.1)THEN
    !## intersect line
    N=0; CALL INTERSECT_EQUI(IDF%XMIN,IDF%XMAX,IDF%YMIN,IDF%YMAX,IDF%DX,IDF%DY,X1,X2,Y1,Y2,N,.TRUE.,.TRUE.)
    DO J=1,N
     ICOL=INT(XA(J)); IROW=INT(YA(J))
     IF(ICOL.GE.1.AND.IROW.GE.1.AND. &
        ICOL.LE.IDF%NCOL.AND.IROW.LE.IDF%NROW)THEN
      NP=NP+1; CALL ASC2IDF_INT_RESIZEVECTORS(NP,100)
      XP(NP)=REAL(ICOL); YP(NP)=REAL(IROW); ZP(NP)=FA(J); WP(NP)=REAL(ILINE)
     ENDIF
    ENDDO
   ENDIF     
   X1=X2; Y1=Y2
  ENDDO
 ENDDO
 CALL INTERSECT_DEALLOCATE()

 CLOSE(IU)

 CALL ASC2IDF_INT_RESIZEVECTORS(NP,0)

 DO I=2,SIZE(XP)
  IL1=INT(WP(I))
  IL2=INT(WP(I))
  !## similar line
  IF(IL1.NE.IL2)CYCLE

  IC1=INT(XP(I-1))
  IC2=INT(XP(I  ))
  IR1=INT(YP(I-1))
  IR2=INT(YP(I  ))
  IP1=INT(ZP(I-1))
  IP2=INT(ZP(I  ))
   
  CALL ASC2IDF_HFB_GETFACES(IC1,IC2,IR1,IR2,IP1,IP2,IPC,NROW,NCOL)

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

 END SUBROUTINE ASC2IDF_HFB_GETFACES
  
END MODULE MOD_ASC2IDF_HFB
