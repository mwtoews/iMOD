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

MODULE MOD_HFB

USE MOD_HFB_PAR
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_INTERSECT, ONLY : INTERSECT_EQUI,INTERSECT_NONEQUI,INTERSECT_DEALLOCATE
USE MOD_INTERSECT_PAR, ONLY : XA,YA,LN
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_CAP

CONTAINS

 !###====================================================================
 SUBROUTINE HFB_AL()
 !###====================================================================
 IMPLICIT NONE
 
 CALL HFB_DAL()
 IF(NHFBFILES.LE.0)RETURN
 ALLOCATE(HFBFILES(NHFBFILES),HFBC(NHFBFILES),HFBILAY(NHFBFILES))
 HFBC=1.0; HFBILAY=0; HFBFILES=''
 
 END SUBROUTINE HFB_AL

 !###====================================================================
 SUBROUTINE HFB_DAL()
 !###====================================================================
 IMPLICIT NONE
 
 IF(ALLOCATED(HFBFILES))DEALLOCATE(HFBFILES)
 IF(ALLOCATED(HFBC))DEALLOCATE(HFBC)
 IF(ALLOCATED(HFBILAY))DEALLOCATE(HFBILAY)
 IF(ALLOCATED(HFBIP))DEALLOCATE(HFBIP)
 IF(ASSOCIATED(HFBPOS))DEALLOCATE(HFBPOS) 
 IF(ALLOCATED(IPC))DEALLOCATE(IPC)
 
 END SUBROUTINE HFB_DAL

 !###====================================================================
 LOGICAL FUNCTION HFB_RP(IDF,ERRORSTRING)
 !###====================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 CHARACTER(LEN=*),INTENT(OUT),OPTIONAL :: ERRORSTRING
 REAL :: X1,Y1,X2,Y2,XX1,XX2,YY1,YY2
 INTEGER :: ID,IU,IROW,ICOL,I,II,L,N,M,ILINE,IOS
 CHARACTER(LEN=256) :: LINE
 
 HFB_RP=.TRUE.; IF(NHFBFILES.EQ.0)RETURN
 
 HFB_RP=.FALSE.

 ALLOCATE(HFBIP(0:NHFBFILES),HFBPOS(500,3)); HFBIP=0; HFBPOS=INT(0,2)
 ALLOCATE(IPC(0:IDF%NCOL+2,0:IDF%NROW+2,0:3)) 
 
 ILINE=0
 DO I=1,NHFBFILES

   HFBIP(I)=HFBIP(I-1)
   
  !## process genfile
  IU=UTL_GETUNIT(); OPEN(IU,FILE=HFBFILES(I),STATUS='OLD',ACTION='READ',IOSTAT=IOS)
  IF(IOS.NE.0)EXIT
  DO
   ILINE=ILINE+1; II=0
   READ(IU,'(A256)') LINE; LINE=UTL_CAP(LINE,'U')
   IOS=0; IF(INDEX(LINE,'END').GT.0)EXIT
   READ(LINE,*,IOSTAT=IOS) ID; IF(IOS.NE.0)EXIT
   !## read segment
   DO
    READ(IU,'(A256)') LINE; LINE=UTL_CAP(LINE,'U')
    IOS=0; IF(INDEX(LINE,'END').GT.0)EXIT
    READ(LINE,*,IOSTAT=IOS) X2,Y2; IF(IOS.NE.0)EXIT

    !## intersect line
    IF(II.GT.0)THEN
     XX1=X1; YY1=Y1; XX2=X2; YY2=Y2
     IF(IDF%IEQ.EQ.0)THEN
      N=0; CALL INTERSECT_EQUI(IDF%XMIN,IDF%XMAX,IDF%YMIN,IDF%YMAX,IDF%DX,XX1,XX2,YY1,YY2,N)
     ELSE
      N=0; CALL INTERSECT_NONEQUI(IDF%SX,IDF%SY,IDF%NROW,IDF%NCOL,XX1,XX2,YY1,YY2,N)
     ENDIF
    
     M=HFBIP(I)+N
     IF(M.GT.SIZE(HFBPOS,1))THEN
      ALLOCATE(HFBPOS_BU(M+N*2,3))
      HFBPOS_BU(1:HFBIP(I),:)=HFBPOS(1:HFBIP(I),:)
      DEALLOCATE(HFBPOS); HFBPOS=>HFBPOS_BU
     ENDIF
    
     !## fill result array
     DO L=1,N
      ICOL=INT(XA(L)); IROW=INT(YA(L)) 
      IF(ICOL.GT.0.AND.IROW.GT.0.AND.ICOL.LE.IDF%NCOL.AND.IROW.LE.IDF%NROW)THEN
       HFBIP(I)=HFBIP(I)+1
       HFBPOS(HFBIP(I),1)=ICOL
       HFBPOS(HFBIP(I),2)=IROW
       HFBPOS(HFBIP(I),3)=ILINE
      ENDIF
     ENDDO
    ENDIF
    II=II+1; X1=X2; Y1=Y2
   ENDDO
   IF(IOS.NE.0.AND.PRESENT(ERRORSTRING))ERRORSTRING='Error while reading SEGMENT in HFB-file: '//TRIM(HFBFILES(I))
  END DO
  IF(IOS.NE.0.AND.PRESENT(ERRORSTRING))ERRORSTRING='Error while reading HEADER/IDENTIFICATION in HFB-file: '//TRIM(HFBFILES(I))
  CLOSE(IU)
 END DO

 CALL INTERSECT_DEALLOCATE()
  
 HFB_RP=.TRUE.
 
 END FUNCTION HFB_RP

 !###====================================================================
 SUBROUTINE HFB_CALC(IDF,CC,CR,ILAY)
 !###====================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER,INTENT(IN) :: ILAY
 REAL,DIMENSION(:,:),INTENT(INOUT) :: CC,CR  
 INTEGER,PARAMETER :: INEIGHBOURS=2
 INTEGER :: IROW,ICOL,J,IHFB,ILINE,JLINE,JCOL,JROW

 DO IHFB=1,NHFBFILES

  IF(HFBILAY(IHFB).NE.ILAY)CYCLE
  
  !## fill in ipc
  IPC=INT(0,1); JCOL=HFBPOS(1,1); JROW=HFBPOS(1,2); ILINE=HFBPOS(1,3)

  DO J=HFBIP(IHFB-1)+1,HFBIP(IHFB)
   ICOL =HFBPOS(J,1);IROW =HFBPOS(J,2); ILINE=HFBPOS(J,3)
   IF(ILINE.EQ.JLINE)THEN
    IF(ICOL.GT.JCOL)THEN
     IPC(JCOL,JROW,2)=INT(1,1);     JCOL=JCOL+1
    ELSEIF(ICOL.LT.JCOL)THEN
     IPC(JCOL-1,JROW,2)=INT(1,1);   JCOL=JCOL-1   
    ELSEIF(IROW.GT.JROW)THEN
     IPC(JCOL-1,JROW+1,1)=INT(1,1); JROW=JROW+1
    ELSEIF(IROW.LT.JROW)THEN
     IPC(JCOL-1,JROW,1)=INT(1,1);   JROW=JROW-1
    ENDIF
   ENDIF
   !## write fault-faces
   IF(ILINE.NE.JLINE.OR.J.EQ.HFBIP(IHFB))THEN

    DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
     IF(IPC(ICOL,IROW,1).EQ.INT(1,1))THEN
      CR(ICOL,IROW)=HFB_GETCOND(CR(ICOL,IROW),HFBC(IHFB),IDF,IROW,ICOL,1)
     ENDIF
     IF(IPC(ICOL,IROW,2).EQ.INT(1,1))THEN
      CC(ICOL,IROW)=HFB_GETCOND(CC(ICOL,IROW),HFBC(IHFB),IDF,IROW,ICOL,2)
     ENDIF
    ENDDO; ENDDO

    !## reset for the next line   
    IF(J.NE.HFBIP(IHFB))THEN
     IPC=INT(0,1); JCOL=ICOL; JROW=IROW; JLINE=ILINE
    ENDIF
   ENDIF
  ENDDO
   
 ENDDO

 END SUBROUTINE HFB_CALC

 !###====================================================================
 REAL FUNCTION HFB_GETCOND(COND,C,IDF,IROW,ICOL,IDIR)
 !###====================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 REAL,INTENT(IN) :: COND,C
 INTEGER,INTENT(IN) :: IROW,ICOL,IDIR
 REAL :: CC,DX,DY
 
 IF(IDF%IEQ.EQ.0)THEN
  DX=IDF%DX; DY=IDF%DY
 ELSE
  IF(IDIR.EQ.1)THEN
   DX=IDF%SX(ICOL)-IDF%SX(ICOL-1)
   DY=IDF%SY(IROW-1)-IDF%SY(IROW)
  ELSEIF(IDIR.EQ.2)THEN
   DX=IDF%SY(IROW-1)-IDF%SY(IROW)
   DY=IDF%SX(ICOL)-IDF%SX(ICOL-1)
  ENDIF
 ENDIF

 IF(COND.GT.0.0)CC=1.0/(COND/(DX*DY))
 
 HFB_GETCOND=0.0 
  
 END FUNCTION

END MODULE MOD_HFB