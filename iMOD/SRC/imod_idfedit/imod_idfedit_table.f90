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

MODULE MOD_IDFEDIT_TABLE

USE WINTERACTER
USE RESOURCE
USE MODPLOT
USE MOD_UTL, ONLY : ITOS,UTL_GETUNIT,UTL_IDFGETCLASS,UTL_DEBUGLEVEL
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_OSD, ONLY : OSD_OPEN,OSD_GETENV
USE MOD_IDF, ONLY : IDFREAD,IDFGETVAL,IDFALLOCATEX,IDFNULLIFY,IDFCOPY,IDFIROWICOL,IDFGETLOC
USE MOD_POLYGON_UTL, ONLY : SELIDF,POLYGON1DEALLOCATE_SELIDF

CONTAINS

 !###======================================================================
 SUBROUTINE UTL_EDITTABLE_INIT(IPLOT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPLOT
 INTEGER :: IR1,IR2,IR,IC1,IC2,IC,N
 
 CALL POLYGON1DEALLOCATE_SELIDF()
 ALLOCATE(SELIDF(1)); CALL IDFNULLIFY(SELIDF(1))
 CALL IDFCOPY(MP(IPLOT)%IDF,SELIDF(1))
 SELIDF(1)%IXV =2  !##usages of nthreads, ysel -> selidf(1)%ysel()
 IF(.NOT.IDFALLOCATEX(SELIDF(1)))THEN
  CALL POLYGON1DEALLOCATE_SELIDF()
  RETURN
 ENDIF
 
 !## fill in ysel()
 CALL IDFIROWICOL(MP(IPLOT)%IDF,IR2,IC1,MPW%XMIN,MPW%YMIN)
 CALL IDFIROWICOL(MP(IPLOT)%IDF,IR1,IC2,MPW%XMAX,MPW%YMAX)
 !## adjust ic1,ic2
 IF(MP(IPLOT)%IDF%XMIN.GT.MPW%XMIN)IC1=1
 IF(MP(IPLOT)%IDF%XMAX.LT.MPW%XMAX)IC2=MP(IPLOT)%IDF%NCOL
 !## adjust ir1,ir2
 IF(MP(IPLOT)%IDF%YMIN.GT.MPW%YMIN)IR2=MP(IPLOT)%IDF%NROW
 IF(MP(IPLOT)%IDF%YMAX.LT.MPW%YMAX)IR1=1

 N=0; DO IR=IR1,IR2; DO IC=IC1,IC2
  N=N+1; SELIDF(1)%YSEL(1,N)=INT(IC,2); SELIDF(1)%YSEL(2,N)=INT(IR,2)
 ENDDO; ENDDO
 SELIDF(1)%NTHREAD=N

 CALL UTL_EDITTABLE(JPLOT=IPLOT)
 
 CALL POLYGON1DEALLOCATE_SELIDF()

 END SUBROUTINE UTL_EDITTABLE_INIT

 !###======================================================================
 SUBROUTINE UTL_EDITTABLE(JPLOT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN),OPTIONAL :: JPLOT
 INTEGER :: I,ITYPE,ICOL,IC1,IC2,IROW,IR1,IR2,NC,NR,JCOL,JROW,IPLOT,IU,IOS,ICLR,IPLTCLR
 TYPE(WIN_MESSAGE) :: MESSAGE
 REAL :: IDFVAL
 REAL,ALLOCATABLE,DIMENSION(:,:) :: X
 
 IF(PRESENT(JPLOT))IPLOT=JPLOT
 
 CALL WDIALOGLOAD(ID_DIDFEDITTABLE,ID_DIDFEDITTABLE)
 IF(PRESENT(JPLOT))THEN
  CALL WDIALOGFIELDSTATE(IDOK,2)
  CALL WDIALOGPUTSTRING(IDCANCEL,'Close')
 ENDIF
 
 IC1=INT(MINVAL(SELIDF(1)%YSEL(1,1:SELIDF(1)%NTHREAD)))
 IC2=INT(MAXVAL(SELIDF(1)%YSEL(1,1:SELIDF(1)%NTHREAD)))
 IR1=INT(MINVAL(SELIDF(1)%YSEL(2,1:SELIDF(1)%NTHREAD)))
 IR2=INT(MAXVAL(SELIDF(1)%YSEL(2,1:SELIDF(1)%NTHREAD)))
 NC=(IC2-IC1)+1; NR=(IR2-IR1)+1
 
 IF(NR.GT.WINFOGRID(IDF_GRID1,GRIDROWSMAX))THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot present results in table'//CHAR(13)// &
    'with more than '//TRIM(ITOS(WINFOGRID(IDF_GRID1,GRIDROWSMAX)))//' rows','Warning')
 ELSE
 
  IPLTCLR=1

  IF(.NOT.PRESENT(JPLOT))THEN
   CALL WDIALOGSELECT(ID_DIDFEDITCALC)
   CALL WDIALOGGETMENU(IDF_MENU4,IPLOT)  !idf presented in table
  ENDIF

  IF(IDFREAD(MP(IPLOT)%IDF,MP(IPLOT)%IDFNAME,0))THEN; ENDIF
  ALLOCATE(X(NC,NR)); X=MP(IPLOT)%IDF%NODATA
  DO I=1,SELIDF(1)%NTHREAD
   !## cell-indices from selection
   ICOL=INT(SELIDF(1)%YSEL(1,I)); IROW=INT(SELIDF(1)%YSEL(2,I))
   !## get x/y coordinates
   CALL UTL_EDITGETCURRENTNODE(ICOL,IROW,IPLOT,JCOL,JROW)
   IF(JCOL.NE.0.AND.JROW.NE.0)THEN
    IDFVAL=IDFGETVAL(MP(IPLOT)%IDF,JROW,JCOL) 
    X((ICOL-IC1)+1,(IROW-IR1)+1)=IDFVAL
   ENDIF
  ENDDO
  CLOSE(MP(IPLOT)%IDF%IU)
  
  CALL WDIALOGSELECT(ID_DIDFEDITTABLE); CALL WDIALOGPUTINTEGER(IDF_INTEGER1,30)
  CALL UTL_EDITTABLE_VALUES(NC,NR,X,MP(IPLOT)%IDF%NODATA,IC1,IC2,IR1,IR2,IPLOT)
  CALL WDIALOGSELECT(ID_DIDFEDITTABLE); CALL WDIALOGSHOW(0,0,0,3)
  CALL WDIALOGFIELDOPTIONS(IDF_GRID1,EDITFIELDCHANGED,1)

  DO 
   CALL WMESSAGE(ITYPE,MESSAGE)
   SELECT CASE (ITYPE)
    CASE (FIELDCHANGED)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (IDF_CHECK1)
       CALL WDIALOGGETCHECKBOX(IDF_CHECK1,IPLTCLR)
       CALL UTL_DEBUGLEVEL(0); CALL WDIALOGUNDEFINED(RVALUE=-999999.0)
       DO I=1,SELIDF(1)%NTHREAD
        ICOL=INT(SELIDF(1)%YSEL(1,I)); IROW=INT(SELIDF(1)%YSEL(2,I))
        CALL WGRIDGETCELLREAL(IDF_GRID1,(ICOL-IC1)+1,(IROW-IR1)+1,IDFVAL)
        IF(IDFVAL.EQ.-999999.0)CYCLE
        IF(IPLTCLR.EQ.1)THEN
         ICLR=UTL_IDFGETCLASS(MP(IPLOT)%LEG,IDFVAL)
         CALL WGRIDCOLOURCELL(IDF_GRID1,(ICOL-IC1)+1,(IROW-IR1)+1,-1,ICLR)
        ELSE
         CALL WGRIDCOLOURCELL(IDF_GRID1,(ICOL-IC1)+1,(IROW-IR1)+1,-1,-1)
        ENDIF
       ENDDO
       CALL UTL_DEBUGLEVEL(0)

      CASE (IDF_GRID1)
       CALL WGRIDPOS(MESSAGE%X,ICOL,IROW)
       CALL UTL_DEBUGLEVEL(0)
       CALL WGRIDGETCELLREAL(IDF_GRID1,ICOL,IROW,IDFVAL)
       CALL UTL_DEBUGLEVEL(1)
       IF(IPLTCLR.EQ.1)THEN
        ICLR=UTL_IDFGETCLASS(MP(IPLOT)%LEG,IDFVAL)
        CALL WGRIDCOLOURCELL(IDF_GRID1,ICOL,IROW,-1,ICLR)
       ELSE
        CALL WGRIDCOLOURCELL(IDF_GRID1,ICOL,IROW,-1,-1)
       ENDIF
     END SELECT
    CASE (PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (ID_WIDTH)
       CALL UTL_EDITTABLE_VALUES(NC,NR,X,MP(IPLOT)%IDF%NODATA,IC1,IC2,IR1,IR2,IPLOT)
      CASE (IDOK)
       IF(.NOT.PRESENT(JPLOT))THEN
        IU=UTL_GETUNIT()
        CALL OSD_OPEN(IU,FILE=TRIM(PREFVAL(1))//'\TMP\IDFEDIT_'//TRIM(OSD_GETENV('USERNAME'))//'.CSV', &
         STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
        IF(IOS.NE.0)THEN
         CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot save the spreadsheet into the temporary file:'//CHAR(13)// &
           TRIM(PREFVAL(1))//'\TMP\IDFEDIT_'//TRIM(OSD_GETENV('USERNAME'))//'.CSV','Error')
        ELSE
         DO I=1,SELIDF(1)%NTHREAD
          ICOL=INT(SELIDF(1)%YSEL(1,I)); IROW=INT(SELIDF(1)%YSEL(2,I))
          CALL WGRIDGETCELLREAL(IDF_GRID1,(ICOL-IC1)+1,(IROW-IR1)+1,IDFVAL)
          WRITE(IU,*) ICOL,IROW,IDFVAL
         ENDDO
         CLOSE(IU); EXIT
        ENDIF
       ENDIF
      CASE (IDCANCEL)
       EXIT
      CASE (IDHELP)
       CALL IMODGETHELP('4.1.4','MMO.IDO.IDFEdit')
     END SELECT
   END SELECT
   
  ENDDO
 ENDIF
 
 CALL WDIALOGUNLOAD() !; CALL WDIALOGSELECT(ID_DIDFEDITCALC)
 
 END SUBROUTINE UTL_EDITTABLE

 !###======================================================================
 SUBROUTINE UTL_EDITGETCURRENTNODE(ICOL,IROW,IPLOT,JCOL,JROW)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPLOT,IROW,ICOL
 INTEGER,INTENT(OUT) :: JCOL,JROW
 REAL :: XC,YC

 !## get x/y coordinates
 CALL IDFGETLOC(SELIDF(1),IROW,ICOL,XC,YC)
 !## get irow/icol for current idf
 CALL IDFIROWICOL(MP(IPLOT)%IDF,JROW,JCOL,XC,YC)

 END SUBROUTINE UTL_EDITGETCURRENTNODE
 
 !###======================================================================
 SUBROUTINE UTL_EDITTABLE_VALUES(NC,NR,X,NODATA,IC1,IC2,IR1,IR2,IPLOT)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: NODATA
 INTEGER,INTENT(IN) :: NR,NC,IC1,IC2,IR1,IR2,IPLOT
 REAL,INTENT(IN),DIMENSION(NC,NR) :: X
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IC,WC
 INTEGER :: ICOL,IROW,ICLR,I,IPLTCLR

 CALL WDIALOGSELECT(ID_DIDFEDITTABLE); CALL WDIALOGGETINTEGER(IDF_INTEGER1,I); CALL WGRIDROWS(IDF_GRID1,NR)
 ALLOCATE(IC(NC),WC(NC)); IC=1; WC=I; 
#if (defined(WINTERACTER9))
 CALL WGRIDCOLUMNS(IDF_GRID1,NC,IC,WC)
#endif
#if (defined(WINTERACTER8))
 CALL WGRIDCOLUMNS(IDF_GRID1,NC,IC)
#endif
 DEALLOCATE(IC,WC)
  
 DO ICOL=IC1,IC2; CALL WGRIDLABELCOLUMN(IDF_GRID1,(ICOL-IC1)+1,TRIM(ITOS(ICOL))); ENDDO
 DO IROW=IR1,IR2; CALL WGRIDLABELROW   (IDF_GRID1,(IROW-IR1)+1,TRIM(ITOS(IROW))); ENDDO
  
 CALL WDIALOGSELECT(ID_DIDFEDITTABLE)
 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,IPLTCLR)
 DO ICOL=1,NC; CALL WGRIDSTATE(IDF_GRID1,ICOL,2); ENDDO
 DO IROW=1,NR; DO ICOL=1,NC
  IF(X(ICOL,IROW).NE.NODATA)THEN
   CALL WGRIDPUTCELLREAL(IDF_GRID1,ICOL,IROW,X(ICOL,IROW))
   CALL WGRIDSTATECELL(IDF_GRID1,ICOL,IROW,1)
   IF(IPLTCLR.EQ.1)THEN
    ICLR=UTL_IDFGETCLASS(MP(IPLOT)%LEG,X(ICOL,IROW))
    CALL WGRIDCOLOURCELL(IDF_GRID1,ICOL,IROW,-1,ICLR)
   ELSE
    CALL WGRIDCOLOURCELL(IDF_GRID1,ICOL,IROW,-1,-1)
   ENDIF
  ENDIF
 ENDDO; ENDDO
 
 END SUBROUTINE UTL_EDITTABLE_VALUES
 
END MODULE MOD_IDFEDIT_TABLE
