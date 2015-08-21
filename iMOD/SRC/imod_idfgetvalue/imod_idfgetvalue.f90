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
!!
MODULE MOD_IDFGETVALUE

USE WINTERACTER
USE RESOURCE
USE MOD_POLYGON_PAR
USE MODPLOT
USE MOD_IDF, ONLY : IDFREAD,IDFGETVAL,IDFGETLOC,IDFIROWICOL,IDFDEALLOCATE,IDFNULLIFY
USE MOD_UTL, ONLY : ITOS,UTL_PLOTLOCATIONIDF,UTL_GETRELEVANTDIR,INVERSECOLOUR,IDFPLOT1BITMAP,IDFPLOT2BITMAP,UTL_INSIDEPOLYGON
USE MOD_LEGPLOT, ONLY : LEGPLOTUPDATE
USE MOD_IDF_PAR, ONLY : IDFTRANSFORM,IDFOBJ
USE MOD_MDF, ONLY : MDF,READMDF,MDFDEALLOCATE,MDF
USE MOD_COLOURS

INTEGER,PARAMETER,PRIVATE :: MAXPOL=50
INTEGER,PRIVATE :: NIDFS
TYPE(IDFOBJ),DIMENSION(:),ALLOCATABLE,PRIVATE :: IDF
CHARACTER(LEN=256),DIMENSION(:),ALLOCATABLE,PRIVATE :: SNAMES
REAL,DIMENSION(:),ALLOCATABLE,PRIVATE :: XPOL,YPOL
INTEGER,ALLOCATABLE,DIMENSION(:),PRIVATE :: ICOLORIDF,IUNITS
INTEGER,ALLOCATABLE,DIMENSION(:,:),PRIVATE :: IPOSIDF
INTEGER,PRIVATE :: ISHOW

INTEGER,PARAMETER,PRIVATE :: MXID=18
INTEGER,DIMENSION(MXID),PRIVATE :: ID,IACT
DATA (ID(I),I=1,MXID) /ID_NEW,ID_OPEN,ID_SAVE,ID_SAVEAS,ID_COPY,ID_MANAGER,ID_OPENIDF, &
                       ID_IRDATABASE,ID_PROFILE,ID_IMODINFO,ID_TIMESERIES, &
                       ID_FILE,ID_EDIT,ID_VIEW,ID_TOOLBOX,ID_HELPMAIN,ID_3DTOOL,ID_MAP/


CONTAINS

 !###======================================================================
 SUBROUTINE IDFGETVALUE_MAIN()
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,I,J,IPLOT,IROW,ICOL,KSHAPE,IDOWN, &
            IR1,IR2,IC1,IC2,NPOL,N,MAXNIDF,IOPT,INODATA 
 REAL :: XC1,YC1,XC2,XC3,YC2,YC3,IDFVAL,IDFSUM,XCHECK,YCHECK
 LOGICAL :: LEX

 CALL IDFGETVALUE_FIELDS(0)

 CALL WDIALOGLOAD(ID_DIDFINFO,ID_DIDFINFO)
 MAXNIDF=WINFOGRID(IDF_GRID1,GRIDROWSMAX)

 NIDFS=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.(MP(IPLOT)%IPLOT.EQ.1.OR.MP(IPLOT)%IPLOT.EQ.5))THEN
   IF(MP(IPLOT)%IPLOT.EQ.5)THEN
    IF(READMDF(MP(IPLOT)%IDFNAME,N))THEN
     NIDFS=NIDFS+N
    ENDIF
   ELSE
    NIDFS=NIDFS+1
   ENDIF
  ENDIF
 ENDDO

 IF(NIDFS.GT.MAXNIDF.OR.NIDFS.LE.0)THEN
  CALL IDFGETVALUE_CLOSE()
  IF(NIDFS.GT.MAXNIDF)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'There is a maximum of '//TRIM(ITOS(MAXNIDF))// &
   ' you can select simultaneously'//CHAR(13)//'Currenty '//TRIM(ITOS(NIDFS))//' IDF files are selected','Error')
  IF(NIDFS.LE.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No IDFs selected','Error')
  RETURN
 ENDIF

 CALL IDFGETVALUE_OPENFILES()

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_IDFSHAPEVALUEPOINTS,2).EQ.1)   KSHAPE=ID_POINT
 IF(WMENUGETSTATE(ID_IDFSHAPEVALUERECTANGLE,2).EQ.1)KSHAPE=ID_RECTANGLE
 IF(WMENUGETSTATE(ID_IDFSHAPEVALUEPOLYGON,2).EQ.1)  KSHAPE=ID_POLYGON
 IF(WMENUGETSTATE(ID_IDFSHAPEVALUECIRCLE,2).EQ.1)   KSHAPE=ID_CIRCLE

 IF(WMENUGETSTATE(ID_IDFGETVALUE_NONE,2).EQ.1)      ISHOW=0
 IF(WMENUGETSTATE(ID_IDFGETVALUE_FIRST,2).EQ.1)     ISHOW=1
 IF(WMENUGETSTATE(ID_IDFGETVALUE_ALL,2).EQ.1)       ISHOW=2
 IF(ISHOW.EQ.0)CALL WDIALOGPUTSTRING(IDF_LABEL2,'No cells will be plotted.')
 IF(ISHOW.EQ.1)CALL WDIALOGPUTSTRING(IDF_LABEL2,'Cells of first mentioned IDF will be plotted.')
 IF(ISHOW.EQ.2)CALL WDIALOGPUTSTRING(IDF_LABEL2,'Cells of all mentioned IDFs will be plotted.')

 SELECT CASE (KSHAPE)
  CASE (ID_POINT)
   CALL WCURSORSHAPE(ID_CURSORIDFVALUE)
  CASE (ID_CIRCLE)
   CALL WCURSORSHAPE(ID_CURSORCIRCLE)
  CASE (ID_RECTANGLE)
  CALL WCURSORSHAPE(ID_CURSORRECTANGLE)
  CASE (ID_POLYGON)
   ALLOCATE(XPOL(MAXPOL),YPOL(MAXPOL))
   NPOL=0
   CALL WCURSORSHAPE(ID_CURSORPOLYGON)
 END SELECT

 CALL WDIALOGSELECT(ID_DIDFINFO)
 !## show no box
 IF(ISHOW.EQ.0)THEN
  CALL WGRIDCOLOURCOLUMN(IDF_GRID1,1,-1,WRGB(255,255,255))
 !## show first only
 ELSEIF(ISHOW.EQ.1)THEN
  CALL WGRIDCOLOURCOLUMN(IDF_GRID1,1,-1,ICOLORIDF(1))
 !## show all
 ELSEIF(ISHOW.EQ.2)THEN
  DO I=1,NIDFS
   CALL WGRIDCOLOURCELL(IDF_GRID1,1,I,-1,ICOLORIDF(I))
  END DO
 ENDIF
 CALL WGRIDCOLOURCOLUMN(IDF_GRID1,2,-1,WRGB(255,255,255))
 CALL WGRIDCOLOURCOLUMN(IDF_GRID1,3,-1,WRGB(255,255,255))
 CALL WGRIDCOLOURCOLUMN(IDF_GRID1,4,-1,WRGB(255,255,255))
 CALL WGRIDROWS(IDF_GRID1,NIDFS)

 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,INODATA)

 I=(NIDFS-2)*(268-80)/16
 CALL WDIALOGSIZE(IHEIGHT=90+I)

 !## construct filenames
 CALL WDIALOGSELECT(ID_DMANAGERPROPERTIES)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,IOPT)

 ALLOCATE(SNAMES(NIDFS))

 DO I=1,NIDFS
  J=INDEXNOCASE(IDF(I)%FNAME,'\',.TRUE.)+1
  SELECT CASE (IOPT)
   CASE (1)
    SNAMES(I)=IDF(I)%FNAME(J:)
   CASE (2)
    SNAMES(I)=TRIM(IDF(I)%FNAME(J:))//'     ('//IDF(I)%FNAME(:J-2)//')'
   CASE (3,4)
    SNAMES(I)=IDF(I)%FNAME
  END SELECT
 ENDDO
 IF(IOPT.EQ.4)CALL UTL_GETRELEVANTDIR(SNAMES,NIDFS)

 !## fill dialog with information
 CALL WDIALOGSELECT(ID_DIDFINFO)
 DO I=1,NIDFS
  CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,I,SNAMES(I))
 END DO

 CALL WDIALOGSETFIELD(IDF_LABEL11)
 CALL WDIALOGSHOW(-0,65,0,2)

 IF(KSHAPE.NE.ID_POINT)THEN
  CALL IGRPLOTMODE(MODEXOR)
  CALL IGRCOLOURN(WRGB(255,255,255))
  CALL IGRFILLPATTERN(HATCHED,MEDIUM,DIAGUP)
  CALL IGRLINETYPE(SOLIDLINE)
  IDOWN=0
  LEX  =.FALSE.
  XC1  =0.0
  YC1  =0.0
 ELSE
  IDOWN=1
 ENDIF
 XC2=0.0
 YC2=0.0

 INODATA=0

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)

   CASE (MENUSELECT)
    CALL IDFGETVALUE_FIELDS(1)
    CALL IMOD1MENUSELECT(MESSAGE)
    !## reopen files since they were closed after drawing!
    CALL IDFGETVALUE_OPENFILES()
    CALL IDFGETVALUE_FIELDS(0)
    !## reset cursor
    CALL WCURSORSHAPE(ID_CURSORIDFVALUE)
    XC2=0.0
    YC2=0.0
    
   !## mouse-move
   CASE (MOUSEMOVE)

    CALL WINDOWSELECT(0)
    CALL WINDOWOUTSTATUSBAR(1,'X:'//TRIM(ITOS(INT(MESSAGE%GX)))//' m, Y:'//TRIM(ITOS(INT(MESSAGE%GY)))//' m')
    CALL WDIALOGSELECT(ID_DIDFINFO)
    CALL WDIALOGPUTSTRING(IDF_LABEL11,'Current Location X='//TRIM(ITOS(INT(MESSAGE%GX)))// &
                                      ' m Y='//TRIM(ITOS(INT(MESSAGE%GY)))//' m')

    XC2=MESSAGE%GX
    YC2=MESSAGE%GY

    !## plot new one (and remove previous one(s))
    IF(KSHAPE.EQ.ID_POINT)THEN
     IF(IDOWN.EQ.1)CALL IDFGETVALUE_PLOTLOC(XC2,YC2,1)
    ELSE
     CALL IDFGETVALUE_PLOTLOC(XC2,YC2,1)
    ENDIF

    !## first point set!
    SELECT CASE (KSHAPE)

     CASE (ID_RECTANGLE)

      IF(IDOWN.EQ.1)THEN
       CALL IDFPLOT1BITMAP()
       IF(LEX)CALL IGRRECTANGLE(XC1,YC1,XC3,YC3)
       LEX=.FALSE.
       IF(XC1.NE.XC2.AND.YC1.NE.YC2)LEX=.TRUE.
       IF(LEX)CALL IGRRECTANGLE(XC1,YC1,XC2,YC2)
       CALL IDFPLOT2BITMAP()

       CALL WDIALOGSELECT(ID_DIDFINFO)
       DO I=1,NIDFS
        !## get min/max irow/icol current idf
        CALL IDFIROWICOL(IDF(I),IR2,IC1,MIN(XC1,XC2),MIN(YC1,YC2))
        CALL IDFIROWICOL(IDF(I),IR1,IC2,MAX(XC1,XC2),MAX(YC1,YC2))
        CALL WGRIDPUTCELLSTRING(IDF_GRID1,3,NIDFS,'C'//TRIM(ITOS(IC1))//'-'//TRIM(ITOS(IC2))// &
                                                 ';R'//TRIM(ITOS(IR1))//'-'//TRIM(ITOS(IR2)))
        IDFSUM=0.0
        N     =0
        DO IROW=MAX(1,IR1),MIN(IR2,IDF(I)%NROW)
         DO ICOL=MAX(1,IC1),MIN(IC2,IDF(I)%NCOL)
          IDFVAL=IDFGETVAL(IDF(I),IROW,ICOL,IUNITS(I))
          IF(IDFVAL.NE.IDF(I)%NODATA)THEN
           N     =N+1
           IDFSUM=IDFSUM+IDFVAL
          ENDIF
         ENDDO
        END DO
        IF(N.GT.0)IDFSUM=IDFSUM/REAL(N)
        CALL IDFGETVALUE_COLOURCELL(IDF_GRID1,2,I,IDFSUM)
        CALL WGRIDPUTCELLREAL(IDF_GRID1,2,I,IDFSUM,'(G15.7)')
       END DO
      ENDIF

      XC3=XC2
      YC3=YC2

     CASE (ID_POLYGON)

      IF(NPOL.GT.0)THEN
       CALL IDFPLOT1BITMAP()
       IF(LEX)THEN
        XPOL(NPOL+1)=XC3
        YPOL(NPOL+1)=YC3
        CALL IGRPOLYGONCOMPLEX(XPOL,YPOL,NPOL+1)
       ENDIF
       LEX=.TRUE.
       XPOL(NPOL+1)=XC2
       YPOL(NPOL+1)=YC2
       CALL IGRPOLYGONCOMPLEX(XPOL,YPOL,NPOL+1)
       CALL IDFPLOT2BITMAP()

       CALL WDIALOGSELECT(ID_DIDFINFO)
       DO I=1,NIDFS
        !## get min/max irow/icol current idf
        CALL IDFIROWICOL(IDF(I),IR2,IC1,MINVAL(XPOL(1:NPOL+1)),MINVAL(YPOL(1:NPOL+1)))
        CALL IDFIROWICOL(IDF(I),IR1,IC2,MAXVAL(XPOL(1:NPOL+1)),MAXVAL(YPOL(1:NPOL+1)))
        CALL WGRIDPUTCELLSTRING(IDF_GRID1,3,NIDFS,'C'//TRIM(ITOS(IC1))//'-'//TRIM(ITOS(IC2))// &
                                                   ';R'//TRIM(ITOS(IR1))//'-'//TRIM(ITOS(IR2)))
        IDFSUM=0.0
        N     =0
        DO IROW=MAX(1,IR1),MIN(IR2,IDF(I)%NROW)
         DO ICOL=MAX(1,IC1),MIN(IC2,IDF(I)%NCOL)
          CALL IDFGETLOC(IDF(I),IROW,ICOL,XCHECK,YCHECK)
          IF(UTL_INSIDEPOLYGON(XCHECK,YCHECK,XPOL,YPOL,NPOL+1).EQ.1)THEN
           IDFVAL=IDFGETVAL(IDF(I),IROW,ICOL,IUNITS(I))
           IF(IDFVAL.NE.IDF(I)%NODATA)THEN
            N     =N+1
            IDFSUM=IDFSUM+IDFVAL
           ENDIF
          ENDIF
         ENDDO
        END DO
        IF(N.GT.0)IDFSUM=IDFSUM/REAL(N)
        CALL IDFGETVALUE_COLOURCELL(IDF_GRID1,2,I,IDFSUM)
        CALL WGRIDPUTCELLREAL(IDF_GRID1,2,I,IDFSUM,'(G15.7)')
       END DO
       XC3=XC2
       YC3=YC2
      ENDIF

     CASE (ID_POINT)
      
      IF(IDOWN.EQ.1)THEN
       CALL WDIALOGSELECT(ID_DIDFINFO)
       DO I=1,NIDFS
        !## get irow/icol current idf
        CALL IDFIROWICOL(IDF(I),IROW,ICOL,MESSAGE%GX,MESSAGE%GY)
        !## print location
        IF(IROW.GT.IDF(I)%NROW.OR.IROW.LE.0.OR.ICOL.GT.IDF(I)%NCOL.OR.ICOL.LE.0)THEN
         CALL WGRIDPUTCELLSTRING(IDF_GRID1,3,I,'Outside')
         CALL WGRIDCLEARCELL(IDF_GRID1,2,I)
        ELSE
         CALL WGRIDPUTCELLSTRING(IDF_GRID1,3,I,'C'//TRIM(ITOS(ICOL))//';R'//TRIM(ITOS(IROW)))
         !## get grid parameter ieq (equidim vs. var)
         IDFVAL=IDFGETVAL(IDF(I),IROW,ICOL,IUNITS(I))
         CALL IDFGETVALUE_COLOURCELL(IDF_GRID1,2,I,IDFVAL)
         CALL WGRIDPUTCELLREAL(IDF_GRID1,2,I,IDFVAL,'(G15.7)')
        ENDIF
       END DO
       CALL WDIALOGSETFIELD(IDF_LABEL11)
      ENDIF
    END SELECT
    
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDHELP)
      CALL IMODGETHELP('2','GettingStarted') 
     CASE (IDCANCEL)
      EXIT
    END SELECT

   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_CHECK1); CALL WDIALOGGETCHECKBOX(IDF_CHECK1,INODATA)
    END SELECT

   CASE (MOUSEBUTDOWN)
    SELECT CASE (KSHAPE)

     CASE (ID_POINT)
      SELECT CASE (MESSAGE%VALUE1)
       CASE (RIGHTBUTTON)
        EXIT
       CASE (LEFTBUTTON)
        IDOWN=ABS(IDOWN-1)
      END SELECT
     CASE (ID_RECTANGLE)
      IF(IDOWN.EQ.0)THEN
       XC1=XC2
       YC1=YC2
       IDOWN=1
      ELSE
       CALL IDFPLOT1BITMAP()
       IF(LEX)CALL IGRRECTANGLE(XC1,YC1,XC3,YC3)
       CALL IDFPLOT2BITMAP()
       EXIT
      ENDIF

     CASE (ID_POLYGON)
      SELECT CASE (MESSAGE%VALUE1)
       CASE (1)  !## left button
        NPOL=NPOL+1
        XPOL(NPOL)=XC2
        YPOL(NPOL)=YC2

       CASE (3)  !## right button
        IF(NPOL.GT.0)THEN
         CALL IDFPLOT1BITMAP()
         IF(LEX)CALL IGRPOLYGONCOMPLEX(XPOL,YPOL,NPOL+1)
         CALL IDFPLOT2BITMAP()
        ENDIF
        EXIT
      END SELECT

    END SELECT

   !## bitmap scrolled, renew top-left pixel coordinates
   CASE (BITMAPSCROLLED)
    MPW%IX=MESSAGE%VALUE1
    MPW%IY=MESSAGE%VALUE2

   CASE (EXPOSE)
     IF(WMENUGETSTATE(ID_PLOTLEGEND,2).EQ.1)CALL LEGPLOTUPDATE(.FALSE.)
     CALL IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX)
     CALL WDIALOGSELECT(ID_DIDFINFO)

  END SELECT
 END DO

 !## remove everything
 CALL IDFGETVALUE_PLOTLOC(XC2,YC2,0)

 CALL WCURSORSHAPE(CURARROW)
 IF(KSHAPE.NE.ID_POINT)THEN
  CALL IGRPLOTMODE(MODECOPY)
  CALL IGRFILLPATTERN(OUTLINE)
  CALL IGRLINETYPE(SOLIDLINE)
 ENDIF

 !##close files
 CALL IDFGETVALUE_CLOSE()

 IF(ALLOCATED(XPOL))DEALLOCATE(XPOL)
 IF(ALLOCATED(YPOL))DEALLOCATE(YPOL)

 END SUBROUTINE IDFGETVALUE_MAIN

 !###======================================================================
 SUBROUTINE IDFGETVALUE_OPENFILES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IPLOT,I,J,N

 CALL IDFGETVALUE_DEALLOCATE()
 ALLOCATE(IDF(NIDFS),ICOLORIDF(NIDFS),IPOSIDF(2,NIDFS),IUNITS(NIDFS))
 DO I=1,SIZE(IDF); CALL IDFNULLIFY(IDF(I)); ENDDO
 
 !## open idf files (*.idf,*.mdf)
 NIDFS=0
 J=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.(MP(IPLOT)%IPLOT.EQ.1.OR.MP(IPLOT)%IPLOT.EQ.5))THEN
   !## get idf for mdf file
   IF(MP(IPLOT)%IPLOT.EQ.5)THEN
    IF(READMDF(MP(IPLOT)%IDFNAME,N))THEN
     DO I=1,N
      NIDFS=NIDFS+1
      IF(.NOT.IDFREAD(IDF(NIDFS),MDF(I)%FNAME,0))EXIT
      IDF(NIDFS)%FNAME=TRIM(IDF(NIDFS)%FNAME)//' ['//TRIM(MP(IPLOT)%ALIAS)//']'
      J=J+1
      IF(J.GT.MAXCOLOUR)J=1
!      ICOLORIDF(NIDFS)=K
!      IF(ICOLORIDF(NIDFS).LE.0)
      ICOLORIDF(NIDFS)=ICOLOR(J)
      IUNITS(NIDFS)=MDF(I)%UNITS
     ENDDO
     CALL MDFDEALLOCATE()
    ENDIF
   ELSE
    NIDFS=NIDFS+1
    IF(.NOT.IDFREAD(IDF(NIDFS),MP(IPLOT)%IDFNAME,0))EXIT
    J=J+1
    IF(J.GT.MAXCOLOUR)J=1
    ICOLORIDF(NIDFS)=ICOLOR(J)
    IUNITS(NIDFS)=MP(IPLOT)%UNITS
   ENDIF
  ENDIF
 ENDDO

 !## initialize drawing position
 IPOSIDF=0

! CALL IDFGETVALUE_FIELDS(0)

 END SUBROUTINE IDFGETVALUE_OPENFILES

 !###======================================================================
 SUBROUTINE IDFGETVALUE_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 LOGICAL :: LEX

 IF(ALLOCATED(XPOL))DEALLOCATE(XPOL)
 IF(ALLOCATED(YPOL))DEALLOCATE(YPOL)
 IF(ALLOCATED(SNAMES))DEALLOCATE(SNAMES)
 IF(ALLOCATED(IUNITS))DEALLOCATE(IUNITS)
 IF(ALLOCATED(ICOLORIDF))DEALLOCATE(ICOLORIDF)
 IF(ALLOCATED(IPOSIDF))DEALLOCATE(IPOSIDF)

 IF(.NOT.ALLOCATED(IDF))RETURN
 DO I=1,SIZE(IDF)
  IF(IDF(I)%IU.GT.0)THEN
   INQUIRE(UNIT=IDF(I)%IU,OPENED=LEX)
   IF(LEX)CLOSE(IDF(I)%IU)
  ENDIF
 END DO
 CALL IDFDEALLOCATE(IDF,SIZE(IDF))
 DEALLOCATE(IDF)

 END SUBROUTINE IDFGETVALUE_DEALLOCATE

 !###======================================================================
 SUBROUTINE IDFGETVALUE_PLOTLOC(X,Y,ICODE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ICODE
 REAL,INTENT(IN) :: X,Y
 INTEGER :: I,IROW,ICOL,N

 !## nothing to show
 IF(ISHOW.EQ.0)RETURN
 IF(ISHOW.EQ.1)N=1
 IF(ISHOW.EQ.2)N=NIDFS

 !## remove all rectangles
 IF(ICODE.EQ.0)THEN
  DO I=1,N; CALL UTL_PLOTLOCATIONIDF(IDF(I),IPOSIDF(1,I),IPOSIDF(2,I)); ENDDO
 ELSE
  DO I=1,N !IDFS
   IF(IDF(I)%XMIN.LE.X.AND.IDF(I)%XMAX.GE.X.AND. &
      IDF(I)%YMIN.LE.Y.AND.IDF(I)%YMAX.GE.Y)THEN
    CALL IGRCOLOURN(INVERSECOLOUR(ICOLORIDF(I))) !WRGB(255,255,255))
    CALL IDFIROWICOL(IDF(I),IROW,ICOL,X,Y)
    IF(IROW.NE.IPOSIDF(1,I).OR.ICOL.NE.IPOSIDF(2,I))THEN
     !## remove it, previous one
     CALL UTL_PLOTLOCATIONIDF(IDF(I),IPOSIDF(1,I),IPOSIDF(2,I))
     !## plot new one
     IF(ICODE.EQ.1)CALL UTL_PLOTLOCATIONIDF(IDF(I),IROW,ICOL)
     IPOSIDF(1,I)=IROW
     IPOSIDF(2,I)=ICOL
    ENDIF
   ENDIF
  ENDDO
 ENDIF

 END SUBROUTINE IDFGETVALUE_PLOTLOC

 !###======================================================================
 SUBROUTINE IDFGETVALUE_FIELDS(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I
 INTEGER :: J

 CALL WINDOWSELECT(0)
 IF(I.EQ.0)THEN
  DO J=1,MXID
   IACT(J)=WMENUGETSTATE(ID(J),1)
   CALL WMENUSETSTATE(ID(J),1,I)
  END DO
 ELSEIF(I.EQ.1)THEN
  DO J=1,MXID
   CALL WMENUSETSTATE(ID(J),1,IACT(J))
  END DO
 ENDIF

 END SUBROUTINE IDFGETVALUE_FIELDS

 !###======================================================================
 SUBROUTINE IDFGETVALUE_COLOURCELL(ID,IC,IR,IDFVAL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID,IC,IR
 REAL,INTENT(IN) :: IDFVAL

 IF(IDFVAL.GT.0.0)CALL WGRIDCOLOURCELL(ID,IC,IR,WRGB(255,0,0),WRGB(255,255,255))
 IF(IDFVAL.LT.0.0)CALL WGRIDCOLOURCELL(ID,IC,IR,WRGB(0,0,255),WRGB(255,255,255))
 IF(IDFVAL.EQ.0.0)CALL WGRIDCOLOURCELL(ID,IC,IR,-1,WRGB(255,255,255))

 END SUBROUTINE IDFGETVALUE_COLOURCELL

 !###======================================================================
 SUBROUTINE IDFGETVALUE_CLOSE()
 !###======================================================================
 IMPLICIT NONE

 CALL IDFGETVALUE_DEALLOCATE()
 CALL WDIALOGSELECT(ID_DIDFINFO)
 CALL WDIALOGUNLOAD()

 CALL IDFGETVALUE_FIELDS(1)

 END SUBROUTINE IDFGETVALUE_CLOSE

END MODULE MOD_IDFGETVALUE
