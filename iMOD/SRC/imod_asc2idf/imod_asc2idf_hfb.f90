!!  Copyright (C) Stichting Deltares, 2005-2020.
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

USE RESOURCE
USE MOD_ASC2IDF_PAR
USE MOD_ASC2IDF_UTL
USE MOD_INTERSECT_PAR
USE MOD_POLYGON_UTL, ONLY : POLYGON_UTL_OPENGEN
USE MOD_INTERSECT, ONLY : INTERSECT_EQUI,INTERSECT_NONEQUI,INTERSECT_DEALLOCATE
USE MOD_IDF_PAR
USE MOD_UTL, ONLY : UTL_GETUNIT
USE MOD_OSD

CONTAINS

 !###====================================================================
 SUBROUTINE ASC2IDF_HFB(IDF,NROW,NCOL,IPC,FNAME,ITB,TOP,BOT,IPOL)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITB,NCOL,NROW
 INTEGER,INTENT(IN),OPTIONAL :: IPOL
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 TYPE(IDFOBJ),INTENT(INOUT),OPTIONAL :: TOP,BOT
 INTEGER(KIND=1),DIMENSION(NCOL,NROW,2),INTENT(OUT) :: IPC
 CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: FNAME
 REAL(KIND=DP_KIND) :: X1,Y1,X2,Y2,Z1,Z2,TL,DZ,NODATA,ZL,ZF,XMIN,XMAX,YMIN,YMAX
 INTEGER :: IU,II,I,J,K,IOS,N,IROW,ICOL,ILINE,NP,NPNT,MPNT,IC1,IC2,IR1,IR2,IP1,IP2,IL1,IL2,IFORMAT,MAXCOL,MAXPOL,ITYPE
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: XC,YC,ZC
 CHARACTER(LEN=52) :: CID
 INTEGER,DIMENSION(4) :: ICOUT,IROUT
 
 NODATA=HUGE(1.0D0)

 NP=0
 DO II=1,SIZE(FNAME)
 
  IF(.NOT.POLYGON_UTL_OPENGEN(FNAME(II),IFORMAT,IU))THEN
   WRITE(*,'(A)') 'Error opening '//TRIM(FNAME(II)); RETURN
  ENDIF
  
  !## automatic layer assignment
  IF(ITB.EQ.0)THEN; TOP%X=NODATA; BOT%X=NODATA; ENDIF

  !## use genfile
  ILINE=0
  !## binair
  IF(IFORMAT.EQ.0)THEN

   READ(IU)
   READ(IU) MAXPOL,MAXCOL
   IF(MAXCOL.GT.0)THEN; READ(IU); READ(IU); ENDIF
   DO I=1,MAXPOL
    ILINE=ILINE+1
    READ(IU) NPNT,ITYPE
    IF(MAXCOL.GT.0)READ(IU)
    READ(IU)
    
    SELECT CASE (ITYPE)
     CASE (ID_RECTANGLE); MPNT=5
     CASE (ID_POLYGON);   MPNT=NPNT+1
     CASE DEFAULT;        MPNT=NPNT
    END SELECT

    IF(ITB.EQ.0)THEN
     ALLOCATE(XC(MPNT),YC(MPNT),ZC(MPNT))
     READ(IU) (XC(J),YC(J),ZC(J),J=1,NPNT)
    ELSE
     ALLOCATE(XC(MPNT),YC(MPNT))
     READ(IU) (XC(J),YC(J),J=1,NPNT)
    ENDIF

    SELECT CASE (ITYPE)
     CASE (ID_RECTANGLE)
      XMIN=XC(1); YMIN=YC(1); XMAX=XC(2); YMAX=YC(2)
      XC(1)=XMIN; YC(1)=YMIN; XC(2)=XMIN; YC(2)=YMAX
      XC(3)=XMAX; YC(3)=YMAX; XC(4)=XMAX; YC(4)=YMIN
      XC(5)=XMIN; YC(5)=YMIN
     CASE (ID_POLYGON)
      XC(MPNT)=XC(1); YC(MPNT)=YC(1)     
     CASE (ID_LINE)
     CASE DEFAULT
      MPNT=0
    END SELECT

    !## process selected line only
    IF(PRESENT(IPOL))THEN
     IF(IPOL.NE.I)MPNT=0
    ENDIF

    DO J=1,MPNT-1
     X1=XC(J); X2=XC(J+1)
     Y1=YC(J); Y2=YC(J+1)
     !## intersect line
     N=0
     !## equidistantial
     IF(IDF%IEQ.EQ.0)THEN
      CALL INTERSECT_EQUI(IDF%XMIN,IDF%XMAX,IDF%YMIN,IDF%YMAX,IDF%DX,IDF%DY,X1,X2,Y1,Y2,N,.TRUE.)
     !## non-equidistantial
     ELSE
      CALL INTERSECT_NONEQUI(IDF%SX,IDF%SY,IDF%NROW,IDF%NCOL,X1,X2,Y1,Y2,N,.TRUE.)
     ENDIF
     
     IF(ITB.EQ.0)THEN
      !## skip, probably a perfect-vertical segment
      TL=0.0D0; DZ=0.0D0
      IF(SUM(LN(1:N)).LE.0.0D0)THEN
       N=0
      ELSE
       Z1=ZC(J)
       Z2=ZC(J+1)
       DZ=(Z2-Z1)/SUM(LN(1:N))
      ENDIF
     ENDIF
        
     DO K=1,N
      ICOL=CA(K); IROW=RA(K) 
      IF(ICOL.GE.1.AND.IROW.GE.1.AND. &
         ICOL.LE.IDF%NCOL.AND.IROW.LE.IDF%NROW)THEN
       NP=NP+1; CALL ASC2IDF_INT_RESIZEVECTORS(NP,100)
       XP(NP)=REAL(ICOL); YP(NP)=REAL(IROW); FP(NP)=INT(FA(K)); WP(NP)=REAL(ILINE) 

       IF(ITB.EQ.0)THEN
        IF(K.EQ.1)THEN
         TL=   0.5D0*LN(K)
        ELSE
         TL=TL+0.5D0*LN(K-1)+0.5D0*LN(K)
        ENDIF
        ZL=Z1+(TL*DZ); ZP(NP)=ZL*100.0D0
       ENDIF
  
      ENDIF
     ENDDO
    ENDDO
    DEALLOCATE(XC,YC)
    IF(ITB.EQ.0)DEALLOCATE(ZC)
   ENDDO

  !## ascii
  ELSE

   DO
    READ(IU,*,IOSTAT=IOS) CID; IF(IOS.NE.0)EXIT
    I=0; ILINE=ILINE+1
    
    !## process selected line only
    IF(PRESENT(IPOL))THEN
     IF(IPOL.NE.ILINE)CYCLE
    ENDIF
    
    DO
     IF(ITB.EQ.0)THEN
      READ(IU,*,IOSTAT=IOS) X2,Y2,Z2; IF(IOS.NE.0)EXIT
     ELSE
      READ(IU,*,IOSTAT=IOS) X2,Y2; IF(IOS.NE.0)EXIT
     ENDIF
 
     I=I+1
     IF(I.GT.1)THEN
 
      !## intersect line
      N=0
      !## equidistantial
      IF(IDF%IEQ.EQ.0)THEN
       CALL INTERSECT_EQUI(IDF%XMIN,IDF%XMAX,IDF%YMIN,IDF%YMAX,IDF%DX,IDF%DY,X1,X2,Y1,Y2,N,.TRUE.)
       !## non-equidistantial
      ELSE
       CALL INTERSECT_NONEQUI(IDF%SX,IDF%SY,IDF%NROW,IDF%NCOL,X1,X2,Y1,Y2,N,.TRUE.)
      ENDIF

      IF(ITB.EQ.0)THEN
       !## skip, probably a perfect-vertical segment
       TL=0.0D0; DZ=0.0D0
       IF(SUM(LN(1:N)).LE.0.0D0)THEN
        N=0
       ELSE
        DZ=(Z2-Z1)/SUM(LN(1:N))
       ENDIF
      ENDIF
        
      DO J=1,N
       ICOL=CA(J); IROW=RA(J) 
       IF(ICOL.GE.1.AND.IROW.GE.1.AND. &
          ICOL.LE.IDF%NCOL.AND.IROW.LE.IDF%NROW)THEN
        NP=NP+1; CALL ASC2IDF_INT_RESIZEVECTORS(NP,100)
        XP(NP)=REAL(ICOL); YP(NP)=REAL(IROW); FP(NP)=INT(FA(J)); WP(NP)=REAL(ILINE) 
        
        IF(ITB.EQ.0)THEN
         IF(J.EQ.1)THEN
          TL=   0.5D0*LN(J)
         ELSE
          TL=TL+0.5D0*LN(J-1)+0.5D0*LN(J)
         ENDIF
         ZL=Z1+(TL*DZ); ZP(NP)=ZL*100.0D0
        ENDIF
  
       ENDIF
      ENDDO
     ENDIF     
     X1=X2; Y1=Y2; IF(ITB.EQ.0)Z1=Z2
    ENDDO
   ENDDO
  ENDIF
  
  CALL INTERSECT_DEALLOCATE()
  CLOSE(IU)
 
 ENDDO
 
 CALL ASC2IDF_INT_RESIZEVECTORS(NP,0)

 IF(ASSOCIATED(XP))THEN
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
  
   CALL ASC2IDF_HFB_GETFACES(IC1,IC2,IR1,IR2,IP1,IP2,IPC,NROW,NCOL,ICOUT,IROUT)

   IF(ITB.EQ.0)THEN
    !## fill in z-coordinates
    ZF=(ZP(I-1)+ZP(I))/2.0D0
   
    DO II=1,4
     ICOL=ICOUT(II); IROW=IROUT(II)
     IF(ICOL.LE.0.OR.IROW.LE.0)CYCLE
     IF(TOP%X(ICOL,IROW).EQ.NODATA)THEN
      TOP%X(ICOL,IROW)=ZF/100.0D0; BOT%X(ICOL,IROW)=ZF/100.0D0
     ELSE
      TOP%X(ICOL,IROW)=MAX(TOP%X(ICOL,IROW),ZF/100.0D0)
      BOT%X(ICOL,IROW)=MIN(BOT%X(ICOL,IROW),ZF/100.0D0)
     ENDIF
    ENDDO

   ENDIF 
  ENDDO
 ENDIF
 
 END SUBROUTINE ASC2IDF_HFB

 !###====================================================================
 SUBROUTINE ASC2IDF_HFB_GETFACES(IC1,IC2,IR1,IR2,IP1,IP2,IPC,NROW,NCOL,ICOUT,IROUT)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IC1,IC2,IR1,IR2,IP1,IP2
 INTEGER,INTENT(IN) :: NROW,NCOL
 INTEGER,INTENT(OUT),DIMENSION(4) :: ICOUT,IROUT
 INTEGER(KIND=1),INTENT(INOUT),DIMENSION(NCOL,NROW,2) :: IPC
 INTEGER,DIMENSION(2) :: JPC,JPR,JC,JR,JP
 INTEGER :: I,IC,IR 

 !## cells capture faults
 ICOUT=0; IROUT=0
      
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
  ICOUT(1)=IC; IROUT(1)=IR; ICOUT(2)=IC; IROUT(2)=IR+1
 ENDIF
 !## vertical fault ipc(,,2)=1
 IF(JPC(1).EQ.JPC(2).AND.JPR(1).NE.JPR(2))THEN
  IC=JPC(1); IR=MAX(JPR(1),JPR(2)); IPC(IC,IR,1)=INT(1,1)
  ICOUT(3)=IC; IROUT(3)=IR; ICOUT(4)=IC+1; IROUT(4)=IR
 ENDIF
 !## diagonal, add two faults
 IF(JPR(1).NE.JPR(2).AND.JPC(1).NE.JPC(2))THEN
  !## goto to the west
  IF(JPC(1).GT.JPC(2))THEN
   !## goto to the north-west
   IF(JPR(1).GT.JPR(2))THEN
    IC=MIN(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2))
    IPC(IC,IR,1)=INT(1,1) !## vertical
    ICOUT(3)=IC; IROUT(3)=IR; ICOUT(4)=IC+1; IROUT(4)=IR
    IC=MAX(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2))
    IPC(IC,IR,2)=INT(1,1) !## horizontal
    ICOUT(1)=IC; IROUT(1)=IR; ICOUT(2)=IC; IROUT(2)=IR+1
   !## goto to the south-west
   ELSE
    IC=MIN(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2))
    IPC(IC,IR,1)=INT(1,1) !## vertical
    ICOUT(3)=IC; IROUT(3)=IR; ICOUT(4)=IC+1; IROUT(4)=IR
    IC=MAX(JPC(1),JPC(2)); IR=MIN(JPR(1),JPR(2))
    IPC(IC,IR,2)=INT(1,1) !## horizontal
    ICOUT(1)=IC; IROUT(1)=IR; ICOUT(2)=IC; IROUT(2)=IR+1
   ENDIF
  !## goto to the east
  ELSE
   !## goto to the north-east
   IF(JPR(1).GT.JPR(2))THEN
    IC=MIN(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2))
    IPC(IC,IR,1)=INT(1,1) !## vertical
    ICOUT(3)=IC; IROUT(3)=IR; ICOUT(4)=IC+1; IROUT(4)=IR
    IC=MAX(JPC(1),JPC(2)); IR=MIN(JPR(1),JPR(2))
    IPC(IC,IR,2)=INT(1,1) !## horizontal   
    ICOUT(1)=IC; IROUT(1)=IR; ICOUT(2)=IC; IROUT(2)=IR+1
   !## goto to the south-east
   ELSE
    IC=MIN(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2))
    IPC(IC,IR,1)=INT(1,1) !## vertical
    ICOUT(3)=IC; IROUT(3)=IR; ICOUT(4)=IC+1; IROUT(4)=IR
    IC=MAX(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2))
    IPC(IC,IR,2)=INT(1,1) !## horizontal  
    ICOUT(1)=IC; IROUT(1)=IR; ICOUT(2)=IC; IROUT(2)=IR+1
   ENDIF
  ENDIF
 ENDIF

 END SUBROUTINE ASC2IDF_HFB_GETFACES
  
END MODULE MOD_ASC2IDF_HFB
