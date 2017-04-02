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
MODULE MOD_IR_SELECTEDCELLS

USE WINTERACTER
USE RESOURCE
USE MOD_POLYGON_PAR
USE MOD_IR_PAR
USE MOD_IDF, ONLY : IDFREAD,IDFDEALLOCATEX,IDFALLOCATEX,IDFCOPY,IDFGETLOC,IDFIROWICOL,IDFNULLIFY
USE MOD_POLYGON_UTL, ONLY : POLYGON1INIT,POLYGON1CLOSE,POLYGON1DEALLOCATE_SELIDF
USE MOD_POLYGON_DRAW, ONLY : POLYGON1DRAWYSEL
USE MOD_IR_CLC, ONLY : IR2GETEXTENSION
USE MOD_IR_UTL, ONLY : IR1SHAPE2POL,IR1POL2SHAPE,IR1GETTREEVIEWID
USE MOD_UTL, ONLY : UTL_INSIDEPOLYGON

CONTAINS

 !###======================================================================
 SUBROUTINE IR_SELECTEDCELLS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 !## remove selected cells
 CALL IR_PLOTSELECTEDCELLS()

 CALL WDIALOGSELECT(ID_DIR_PMTAB2)
 CALL WDIALOGGETCHECKBOX(IDF_CHECK2,I)
 CALL WDIALOGFIELDSTATE(IDF_MENU2,I)

 !## whenever none is selected
 IF(SUM(SHPIACT(1:SHPNO)).EQ.0)THEN
  CALL WDIALOGPUTCHECKBOX(IDF_CHECK2,0)
  LPLOTYSEL=.FALSE.
 ELSE
  IF(I.EQ.0)LPLOTYSEL=.FALSE.
  IF(I.EQ.1)LPLOTYSEL=.TRUE.
  !## get selected cells
  CALL IR_GETSELECTEDCELLS()
  !## draw selected cells again
  CALL IR_PLOTSELECTEDCELLS()
 ENDIF

 END SUBROUTINE IR_SELECTEDCELLS

 !###======================================================================
 SUBROUTINE IR_PLOTSELECTEDCELLS()
 !###======================================================================
 IMPLICIT NONE

! CALL IDFPLOT1BITMAP()
! CALL IGRPLOTMODE(MODEXOR)

 !## plot ysel if available
 CALL POLYGON1DRAWYSEL()

! CALL IDFPLOT2BITMAP()
! CALL IGRPLOTMODE(MODECOPY)

 END SUBROUTINE IR_PLOTSELECTEDCELLS

 !###======================================================================
 SUBROUTINE IR_GETSELECTEDCELLS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,MXCRD,IROW,ICOL,ITREE,IFIELD,IR1,IR2,IC1,IC2
 REAL :: XMIN,YMIN,XMAX,YMAX,XC,YC

 IF(ALLOCATED(SELIDF))THEN
  SELIDF(1)%NTHREAD=0
 ENDIF
 IF(.NOT.LPLOTYSEL)RETURN

 !## get selected itree/ifield
 CALL IR1GETTREEVIEWID(ITREE,IFIELD)

 !## copy data of current measures
 CALL IR1SHAPE2POL(ITREE,IFIELD)
 !## restore them again ...
 CALL IR1POL2SHAPE(ITREE,IFIELD)

 IF(ITREE.NE.2)RETURN

 !## get window of given polygons
 IF(.NOT.IR2GETEXTENSION(2,IFIELD,XMIN,YMIN,XMAX,YMAX,MXCRD))RETURN

 CALL WDIALOGSELECT(ID_DIR_PMTAB2)
 CALL WDIALOGGETMENU(IDF_MENU2,I)

 ALLOCATE(IDFP(1))

 IF(.NOT.IDFREAD(IDFP(1)%IDF,IR(I)%IDFIR,0))THEN
  DEALLOCATE(IDFP)
  RETURN
 ENDIF
 
 IF(ALLOCATED(SELIDF))THEN
  IF(IDFP(1)%IDF%NCOL.NE.SELIDF(1)%NCOL.OR. &
     IDFP(1)%IDF%NROW.NE.SELIDF(1)%NROW.OR. &
     IDFP(1)%IDF%XMIN.NE.SELIDF(1)%XMIN.OR. &
     IDFP(1)%IDF%XMAX.NE.SELIDF(1)%XMAX.OR. &
     IDFP(1)%IDF%YMIN.NE.SELIDF(1)%YMIN.OR. &
     IDFP(1)%IDF%YMAX.NE.SELIDF(1)%YMAX.OR. &
     IDFP(1)%IDF%IEQ .NE.SELIDF(1)%IEQ)THEN
   CALL POLYGON1DEALLOCATE_SELIDF()
  ENDIF
 ENDIF

 IF(.NOT.ALLOCATED(SELIDF))THEN
  ALLOCATE(SELIDF(1)); CALL IDFNULLIFY(SELIDF(1))
  CALL IDFCOPY(IDFP(1)%IDF,SELIDF(1))
  SELIDF(1)%IXV =2  !##usages of nthreads, ysel -> selidf(1)%ysel()
  IF(.NOT.IDFALLOCATEX(SELIDF(1)))RETURN
 ENDIF

 CALL IDFIROWICOL(SELIDF(1),IR2,IC1,XMIN,YMIN)
 CALL IDFIROWICOL(SELIDF(1),IR1,IC2,XMAX,YMAX)

 !## check whether polygon is selecting gridcells in current idfp()
 DO I=1,MTREE(IFIELD)%NPOL
  IF(MTREE(IFIELD)%POL(I)%IACT.EQ.1)THEN

   DO IROW=IR1,IR2
    DO ICOL=IC1,IC2

     !## get x/y coordinates
     CALL IDFGETLOC(SELIDF(1),IROW,ICOL,XC,YC)

     IF(UTL_INSIDEPOLYGON(XC,YC,MTREE(IFIELD)%POL(I)%X,MTREE(IFIELD)%POL(I)%Y,MTREE(IFIELD)%POL(I)%NCRD).EQ.1)THEN

      DO J=1,SELIDF(1)%NTHREAD
       IF(SELIDF(1)%YSEL(1,J).EQ.INT(ICOL,2).AND.SELIDF(1)%YSEL(2,J).EQ.INT(IROW,2))EXIT
      END DO
      !## add location, if not yet existing!
      IF(J.GT.SELIDF(1)%NTHREAD)THEN
       SELIDF(1)%NTHREAD        =SELIDF(1)%NTHREAD+1
       SELIDF(1)%YSEL(1,SELIDF(1)%NTHREAD)=INT(ICOL,2)   !column number
       SELIDF(1)%YSEL(2,SELIDF(1)%NTHREAD)=INT(IROW,2)   !column number
      ENDIF
     ENDIF
    ENDDO
   ENDDO
  ENDIF
 ENDDO

 DO I=1,SIZE(IDFP)
  CALL IDFDEALLOCATEX(IDFP(I)%IDF)
  IF(IDFP(I)%IDF%IU.GT.0)CLOSE(IDFP(I)%IDF%IU)
  IDFP(I)%IDF%IU=0
 END DO
 DEALLOCATE(IDFP)

 END SUBROUTINE IR_GETSELECTEDCELLS

END MODULE MOD_IR_SELECTEDCELLS
