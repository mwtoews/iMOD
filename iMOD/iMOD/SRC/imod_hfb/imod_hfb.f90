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