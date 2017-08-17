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
MODULE MOD_POLYGON

USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : IDOWN
USE MOD_POLYGON_PAR
USE MOD_UTL, ONLY : ITOS,RTOS,INVERSECOLOUR,UTL_CAP,IDFPLOT1BITMAP,IDFPLOT2BITMAP,UTL_INSIDEPOLYGON, &
                    UTL_POLYGON1AREA,VAR,VAR_TMP,NL,NV,UTL_MEASUREMAIN
USE MOD_TOPO, ONLY : TOPOINIT
USE MODPLOT
USE MOD_LEGPLOT, ONLY : LEGPLOT_PLOTUPDATE
USE MOD_POLYGON_UTL, ONLY : POLYGON1SAVELOADSHAPE,POLYGON1FIELDS
USE MOD_POLYGON_DRAW, ONLY : POLYGON1PLOTSHAPE,POLYGON1DRAWSHAPE
USE MOD_IR_SELECTEDCELLS, ONLY : IR_GETSELECTEDCELLS,IR_SELECTEDCELLS
USE MOD_IR_PAR, ONLY : IRWIN
USE MOD_IR_FIELDS, ONLY : IR1FIELDS_TAB1,IR1FIELDS_TAB2,IR1FIELDS_WRITETAB3TAB2
USE MOD_SCEN_FIELDS, ONLY : SCEN1FIELDS1

 CONTAINS

 !###======================================================================
 SUBROUTINE POLYGON1MAIN(ITYPE,MESSAGE,IDAT,GENFNAME)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE
 INTEGER,INTENT(IN),OPTIONAL :: IDAT
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: GENFNAME
! CHARACTER(LEN=52),POINTER,DIMENSION(:,:) :: VAR

 CALL WDIALOGSELECT(MESSAGE%WIN)

 SELECT CASE(ITYPE)

  CASE(FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE2)
    CASE (IDF_MENU1)
     CALL POLYGON1DRAWSHAPE(1,SHPNO)
     CALL POLYGON1FIELDS(MESSAGE%WIN)
     CALL POLYGON1DRAWSHAPE(1,SHPNO)
   END SELECT

  CASE(PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)

    CASE (ID_SAVESHAPE,ID_LOADSHAPE,ID_SAVE)
     IF(PRESENT(IDAT))THEN
      IF(PRESENT(GENFNAME))THEN
       CALL POLYGON1SAVELOADSHAPE(MESSAGE%VALUE1,MESSAGE%WIN,TRIM(GENFNAME),VAR,IDAT=IDAT)
      ELSE
       CALL POLYGON1SAVELOADSHAPE(MESSAGE%VALUE1,MESSAGE%WIN,'',VAR,IDAT=IDAT)
      ENDIF
     ELSE
      IF(PRESENT(GENFNAME))THEN
       CALL POLYGON1SAVELOADSHAPE(MESSAGE%VALUE1,MESSAGE%WIN,TRIM(GENFNAME),VAR)
      ELSE
       CALL POLYGON1SAVELOADSHAPE(MESSAGE%VALUE1,MESSAGE%WIN,'',VAR)
      ENDIF
     ENDIF
     IF(MESSAGE%VALUE1.EQ.ID_LOADSHAPE)THEN
      CALL IDFPLOTFAST(1)
      CALL POLYGON1FIELDS(MESSAGE%WIN)
     ENDIF
    CASE(ID_DRAW)
     !## remove selected polygons
     CALL POLYGON1DRAWSHAPE(1,SHPNO)
     !## turn all polygons off
     SHPIACT=0 
     CALL POLYGON1DRAWSHAPE(1,SHPNO)
     CALL POLYGON1CREATESHAPE(MESSAGE%WIN)
!     IF(ASSOCIATED(VAR))THEN
!      ALLOCATE(VAR_TMP(NV,0:NL+1))
!      VAR_TMP(1:NV,0:NL)=VAR(1:NV,0:NL)
!      DEALLOCATE(VAR); VAR=>VAR_TMP
!      NL=NL+1; VAR(:,NL)=''; VAR(1,NL)=TRIM(ITOS(SHPI)); VAR(3,NL)=TRIM(SHPNAME(SHPI))
!      SELECT CASE (SHPTYPE(SHPI))
!       CASE (ID_POLYGON); VAR(2,SHPI)='Polygon'
!       CASE (ID_POINT);   VAR(2,SHPI)='Points'
!       CASE (ID_LINE);    VAR(2,SHPI)='Lines'
!      END SELECT
!     ENDIF
    !## delete polygon
    CASE (ID_DELETE)
     CALL POLYGON1DELETE(MESSAGE%WIN)
     CALL POLYGON1FIELDS(MESSAGE%WIN)
    !## rename polygon
    CASE (ID_RENAME)
     CALL POLYGON1RENAME(MESSAGE%WIN)
    !## zoom for polygons
    CASE (ID_ZOOMSELECT)
     CALL POLYGON1ZOOMSELECT()

 !##    CASE (ID_POLYGONCOPYFROM)
 !##     CALL POLYGON1COPYFROM()

   END SELECT
  END SELECT

 END SUBROUTINE POLYGON1MAIN

 !###======================================================================
 SUBROUTINE POLYGON1ZOOMSELECT()
 !###======================================================================
 IMPLICIT NONE
 REAL :: XMIN,YMIN,XMAX,YMAX
 INTEGER :: N,I,J
 
 XMIN= 10.0E10; YMIN= XMIN
 XMAX=-10.0E10; YMAX= XMAX
 
 DO I=1,SHPNO
  IF(SHPIACT(I).EQ.1)THEN
   N=SHPNCRD(I)
   DO J=1,N
    XMIN=MIN(XMIN,SHPXC(J,I))
    XMAX=MAX(XMAX,SHPXC(J,I))
    YMIN=MIN(YMIN,SHPYC(J,I))
    YMAX=MAX(YMAX,SHPYC(J,I))
   ENDDO
  ENDIF
 ENDDO
 
 MPW%XMIN=XMIN-((MPW%XMAX-MPW%XMIN)/25.0)
 MPW%XMAX=XMAX+((MPW%XMAX-MPW%XMIN)/25.0)
 MPW%YMIN=YMIN-((MPW%YMAX-MPW%YMIN)/25.0)
 MPW%YMAX=YMAX+((MPW%YMAX-MPW%YMIN)/25.0)
 
 CALL IDFPLOT(1)
  
 END SUBROUTINE POLYGON1ZOOMSELECT

 !###======================================================================
 SUBROUTINE POLYGON1CREATEPOLYGON(XCRD,YCRD,MAXCRD,NCRD)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER,INTENT(IN) :: MAXCRD
 INTEGER,INTENT(OUT) :: NCRD
 REAL,INTENT(OUT),DIMENSION(MAXCRD) :: XCRD,YCRD
 REAL :: XC1,YC1
 INTEGER :: ITYPE

 CALL IGRPLOTMODE(MODEXOR)
 CALL IGRCOLOURN(WRGB(0,0,255))
 CALL IGRLINETYPE(SOLIDLINE)

 NCRD=0

 CALL WCURSORSHAPE(ID_CURSORPOLYGON)

 DO WHILE(.TRUE.)
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)

   CASE (MENUSELECT)
    !## zooming maps
    SELECT CASE (MESSAGE%VALUE1)
     CASE(ID_MOVEMAP)
      CALL IDFMOVE(0)
     CASE(ID_ZOOMINMAP,ID_ZOOMOUTMAP,ID_ZOOMRECTANGLEMAP,ID_ZOOMFULLMAP)
      CALL IDFZOOM(MESSAGE%VALUE1,(MPW%XMAX+MPW%XMIN)/2.0,(MPW%YMAX+MPW%YMIN)/2.0,0)
      CALL IDFPLOTFAST(1)
     CASE (ID_REDRAW)
      CALL IDFPLOTFAST(1)
     CASE (ID_DISTANCE)
      CALL UTL_MEASUREMAIN()
     CASE (ID_TOPOGRAPHY)
      CALL TOPOINIT()
    END SELECT
    !## reset cursor
    CALL WCURSORSHAPE(ID_CURSORPOLYGON)

   !## mouse-move
   CASE (MOUSEMOVE)

    CALL WINDOWSELECT(0)
    CALL WINDOWOUTSTATUSBAR(1,'X:'//TRIM(ITOS(INT(MESSAGE%GX)))//' m, Y:'//TRIM(ITOS(INT(MESSAGE%GY)))//' m')

    IF(NCRD.GE.1)THEN
     CALL IDFPLOT1BITMAP()
     XCRD(NCRD+1)=XC1
     YCRD(NCRD+1)=YC1
     IF(NCRD+1.EQ.2)CALL IGRPOLYLINE(XCRD,YCRD,NCRD+1)
     IF(NCRD+1.GT.2)CALL IGRPOLYGONCOMPLEX(XCRD,YCRD,NCRD+1)
     XCRD(NCRD+1)=MESSAGE%GX
     YCRD(NCRD+1)=MESSAGE%GY
     IF(NCRD+1.EQ.2)CALL IGRPOLYLINE(XCRD,YCRD,NCRD+1)
     IF(NCRD+1.GT.2)CALL IGRPOLYGONCOMPLEX(XCRD,YCRD,NCRD+1)
     CALL IDFPLOT2BITMAP()
    ENDIF
    XC1=MESSAGE%GX
    YC1=MESSAGE%GY

   CASE (MOUSEBUTDOWN)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (1)  !## left button
      NCRD      =NCRD+1
      XCRD(NCRD)=MESSAGE%GX
      YCRD(NCRD)=MESSAGE%GY
      CALL IDFPLOT1BITMAP()
      IF(NCRD.EQ.2)CALL IGRPOLYLINE(XCRD,YCRD,NCRD)
      CALL IDFPLOT2BITMAP()

     CASE (3)  !###right button
      IF(NCRD.GT.0)THEN
       CALL IDFPLOT1BITMAP()
       CALL IGRPOLYGONCOMPLEX(XCRD,YCRD,NCRD+1)
       CALL IDFPLOT2BITMAP()
      ENDIF
      EXIT
    END SELECT

   !## bitmap scrolled, renew top-left pixel coordinates
   CASE (BITMAPSCROLLED)
    MPW%IX=MESSAGE%VALUE1
    MPW%IY=MESSAGE%VALUE2

  END SELECT
 END DO

 CALL WCURSORSHAPE(CURARROW)
 CALL IGRPLOTMODE(MODECOPY)
 CALL IGRFILLPATTERN(OUTLINE)
 CALL IGRLINETYPE(SOLIDLINE)

 END SUBROUTINE POLYGON1CREATEPOLYGON

 !###======================================================================
 SUBROUTINE POLYGON1CREATESHAPE(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 INTEGER :: I,J,I1,I2,N
 CHARACTER(LEN=52),POINTER,DIMENSION(:,:) :: VAR

 SHPI=SHPNO+1; IF(SHPI.GT.MAXSHAPES)RETURN

 CALL POLYGONGETSHAPE(SHPTYPE(SHPI),IACTSHAPES)
 IF(SHPTYPE(SHPI).LE.0)RETURN
 SHPCOLOR(SHPI)=ICLRPOLG 
 SHPWIDTH(SHPI)=2
 SHPNAME(SHPI) =''
 SHPID(SHPI)   =SHPI
 SHPIACT(SHPI) =1

 CALL POLYGON1DRAW(); IF(SHPNCRD(SHPI).EQ.0)RETURN

 I1=SHPI; I2=I1
 IF(SHPTYPE(SHPI).EQ.ID_POINT)THEN
  !## split points into shapes
  DO I=1,SHPNCRD(SHPI)
   SHPNO=SHPNO+1
   SHPTYPE(SHPNO) =SHPTYPE(SHPI)
   SHPCOLOR(SHPNO)=ICLRPOLG 
   SHPWIDTH(SHPNO)=2
   SHPID(SHPNO)   =SHPNO+I
   SHPNCRD(SHPNO) =1
   SHPXC(1,SHPNO) =SHPXC(I,SHPI)
   SHPYC(1,SHPNO) =SHPYC(I,SHPI)
  ENDDO
  I1=SHPI; I2=SHPNO
 ELSE
  SHPNO=SHPNO+1
 ENDIF

 SHPIACT=0

 DO SHPI=I1,I2
  SHPNAME(SHPI) ='SHAPE'//TRIM(ITOS(SHPI))
  SHPIACT(SHPI) =1
  IF(ASSOCIATED(VAR))THEN
   ALLOCATE(VAR_TMP(NV,0:NL+1))
   DO I=1,NV
    DO J=0,NL
     VAR_TMP(I,J)=VAR(I,J)
    ENDDO
   ENDDO
!   VAR_TMP(1:NV,0:NL)=VAR(1:NV,0:NL)
   DEALLOCATE(VAR); VAR=>VAR_TMP
   NL=NL+1; VAR(:,NL)=''
   SELECT CASE (SHPTYPE(SHPI))
    CASE (ID_RECTANGLE); VAR(2,SHPI)='Rectangle'; VAR(1,SHPI)=TRIM(ITOS(SHPI)); VAR(3,SHPI)=TRIM(SHPNAME(SHPI))
    CASE (ID_POLYGON);   VAR(2,SHPI)='Polygon';   VAR(1,SHPI)=TRIM(ITOS(SHPI)); VAR(3,SHPI)=TRIM(SHPNAME(SHPI))
    CASE (ID_POINT);     VAR(3,SHPI)='Points';    VAR(1,SHPI)=RTOS(SHPXC(1,SHPI),'F',2); VAR(2,SHPI)=RTOS(SHPYC(1,SHPI),'F',2)
    CASE (ID_LINE);      VAR(2,SHPI)='Lines';     VAR(1,SHPI)=TRIM(ITOS(SHPI)); VAR(3,SHPI)=TRIM(SHPNAME(SHPI))
   END SELECT
  ENDIF

 ENDDO
 
 CALL WDIALOGSELECT(ID)
 CALL WDIALOGPUTMENU(IDF_MENU1,SHPNAME,SHPNO,SHPIACT)
 CALL POLYGON1FIELDS(ID)

 END SUBROUTINE POLYGON1CREATESHAPE

 !###======================================================================
 SUBROUTINE POLYGON1DRAW()
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE
 REAL :: X1,Y1,X2,Y2
 LOGICAL :: LEX

 CALL IGRPLOTMODE(MODEXOR)
 SHPCOLOR(SHPI)=INVERSECOLOUR(SHPCOLOR(SHPI))
 CALL IGRCOLOURN(SHPCOLOR(SHPI))

 SHPNCRD(SHPI)=1

 LEX=.FALSE.

 SELECT CASE (SHPTYPE(SHPI))
  CASE (ID_POLYGON)
   CALL WCURSORSHAPE(ID_CURSORPOLYGON)
  CASE (ID_RECTANGLE)
   CALL WCURSORSHAPE(ID_CURSORRECTANGLE)
  CASE (ID_POINT)
   CALL WCURSORSHAPE(ID_CURSORPOINT)
  CASE (ID_LINE)
   CALL WCURSORSHAPE(ID_CURSORLINE)
 END SELECT

 CALL IGRFILLPATTERN(OUTLINE)
 CALL IGRLINETYPE(SOLIDLINE)
 CALL IGRLINEWIDTH(SHPWIDTH(SHPI))

 DO WHILE(.TRUE.)
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)

   !## mouse-move
   CASE (MOUSEMOVE)

    CALL WINDOWSELECT(0)
    CALL WINDOWOUTSTATUSBAR(1,'X:'//TRIM(ITOS(INT(MESSAGE%GX)))//' m, Y:'//TRIM(ITOS(INT(MESSAGE%GY)))//' m')

    !## first point set 
    IF(SHPNCRD(SHPI).GT.1.OR.SHPTYPE(SHPI).EQ.ID_POINT)THEN
     CALL IDFPLOT1BITMAP()
     IF(LEX)CALL POLYGON1PLOTSHAPE(); LEX=.TRUE.
     SHPXC(SHPNCRD(SHPI),SHPI)=MESSAGE%GX
     SHPYC(SHPNCRD(SHPI),SHPI)=MESSAGE%GY
     CALL POLYGON1PLOTSHAPE()
     CALL IDFPLOT2BITMAP()
    ENDIF

   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDCANCEL)
      EXIT
    END SELECT

   CASE (MOUSEBUTDOWN)
    CALL IDFPLOT1BITMAP()
    IF(LEX)CALL POLYGON1PLOTSHAPE()
    SELECT CASE (MESSAGE%VALUE1)
     CASE (1)  !## left button
      IF(SHPTYPE(SHPI).EQ.ID_RECTANGLE.AND.SHPNCRD(SHPI).EQ.2)THEN
       CALL POLYGON1PLOTSHAPE()
       CALL IDFPLOT2BITMAP()
       EXIT
      ENDIF
      SHPXC(SHPNCRD(SHPI):SHPNCRD(SHPI)+1,SHPI)=MESSAGE%GX
      SHPYC(SHPNCRD(SHPI):SHPNCRD(SHPI)+1,SHPI)=MESSAGE%GY
      SHPNCRD(SHPI)=SHPNCRD(SHPI)+1
      CALL POLYGON1PLOTSHAPE()
      CALL IDFPLOT2BITMAP()
     CASE (3)  !## right button
      SHPNCRD(SHPI)=SHPNCRD(SHPI)-1
      CALL POLYGON1PLOTSHAPE()
      CALL IDFPLOT2BITMAP()
      EXIT
    END SELECT

   !## bitmap scrolled, renew top-left pixel coordinates
   CASE (BITMAPSCROLLED)
    MPW%IX=MESSAGE%VALUE1
    MPW%IY=MESSAGE%VALUE2

   CASE (EXPOSE)
     IF(WMENUGETSTATE(ID_PLOTLEGEND,2).EQ.1)CALL LEGPLOT_PLOTUPDATE(.FALSE.)
     CALL IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX)

  END SELECT
 END DO

 SELECT CASE (SHPTYPE(SHPI))
  CASE (ID_RECTANGLE)

  CASE (ID_POLYGON)
   IF(SHPNCRD(SHPI).LT.3)THEN
    CALL IDFPLOT1BITMAP()
    CALL POLYGON1PLOTSHAPE()
    CALL IDFPLOT2BITMAP()
    SHPNCRD(SHPI)=0
   ENDIF
  CASE (ID_POINT)

  CASE (ID_LINE)
   IF(SHPNCRD(SHPI).LT.2)THEN
    CALL IDFPLOT1BITMAP()
    CALL POLYGON1PLOTSHAPE()
    CALL IDFPLOT2BITMAP()
    SHPNCRD(SHPI)=0
   ENDIF
 END SELECT

 SHPCOLOR(SHPI)=INVERSECOLOUR(SHPCOLOR(SHPI))

 CALL WCURSORSHAPE(CURARROW)
 CALL IGRPLOTMODE(MODECOPY)
 CALL IGRFILLPATTERN(OUTLINE)
 CALL IGRLINETYPE(SOLIDLINE)
 CALL IGRLINEWIDTH(1)

 END SUBROUTINE POLYGON1DRAW

 !###====================================================================
 SUBROUTINE POLYGON1SELECT()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: ID,SHPJ

 IF(SHPNO.LE.0)RETURN
 IF(CRDITYPE.GE.1.AND.CRDITYPE.LE.3)RETURN

 !## redraw current polygons
 SHPJ=SHPI
 CALL POLYGON1DRAWSHAPE(1,SHPNO)

 SHPIACT=0
 SHPI   =SHPJ

 !## nothing selected, deselect all
 IF(CRDITYPE.EQ.4)SHPIACT(SHPI)=1

 ID=0
 IF(WMENUGETSTATE(ID_IDFEDIT,2).EQ.1)   ID=ID_DIDFEDIT
 IF(WMENUGETSTATE(ID_ISGEDIT,2).EQ.1)   ID=ID_DISGEDITTAB2
 IF(WMENUGETSTATE(ID_IRDATABASE,2).EQ.1)THEN
  ID=ID_DIR_PM
  CALL WDIALOGSELECT(ID)
  CALL WDIALOGGETTAB(IDF_TAB,ID)

  !## program the dialog number "hard" since other can be selected
  SELECT CASE (ID)

   !## target polygons
   CASE (ID_DIR_PMTAB1)
    ID=ID_DIR_PMTAB1TAB2
    !## change fields on tab
    CALL IR1FIELDS_TAB1()
   !## measure polygons
   CASE (ID_DIR_PMTAB2)
    ID=ID_DIR_PMTAB2TAB2
    !## change fields on tab
    CALL IR1FIELDS_TAB2()
   CASE (ID_DIR_PMTAB3)
    ID=ID_DIR_PMTAB1TAB2
    !## change fields on tab
    CALL IR1FIELDS_TAB1()
    !## write effects on result-tab
    CALL IR1FIELDS_WRITETAB3TAB2()

  END SELECT

 ENDIF
 
 IF(WMENUGETSTATE(ID_SCENARIO,2).EQ.1)     ID=ID_DSCEN1
 IF(WMENUGETSTATE(ID_SPOINTS,2).EQ.1)      ID=ID_DSPTAB1
 IF(WMENUGETSTATE(ID_WBAL_GENERATE,2).EQ.1)ID=ID_TOOLSTAB3
 IF(WMENUGETSTATE(ID_MEAN,2).EQ.1)         ID=ID_TOOLSTAB3
 IF(WMENUGETSTATE(ID_CREATEGEN,2).EQ.1)    ID=ID_DCREATEGEN
 IF(WMENUGETSTATE(ID_CREATEIPF,2).EQ.1)    ID=ID_DCREATEIPF
 IF(WMENUGETSTATE(ID_EXTRACTIPF,2).EQ.1)   ID=ID_DEXTRACT
 IF(WMENUGETSTATE(ID_CREATEIDF_GEN,2).EQ.1)ID=ID_DCREATEIDFTAB2
 IF(WMENUGETSTATE(ID_SOLIDS,2).EQ.1)       ID=ID_DSOLIDTAB2
 IF(ID.EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'id eq 0; IMOD1MOUSEBUTDOWN','Error')
  RETURN
 ENDIF

 CALL WDIALOGSELECT(ID)
 CALL WDIALOGPUTMENU(IDF_MENU1,SHPNAME,SHPNO,SHPIACT)
 CALL POLYGON1FIELDS(ID)

 !## draw new polygons
 CALL POLYGON1DRAWSHAPE(1,SHPNO)

 IF(WMENUGETSTATE(ID_IRDATABASE,2).EQ.1)THEN
  !## fields on main measure tab
  CALL IR_SELECTEDCELLS()
 ENDIF
 IF(WMENUGETSTATE(ID_SCENARIO,2).EQ.1)CALL SCEN1FIELDS1()

 END SUBROUTINE POLYGON1SELECT

 !###======================================================================
 SUBROUTINE POLYGON1MOUSEMOVE(XC,YC,ICURSOR)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ICURSOR
 REAL,INTENT(IN) :: XC,YC
 REAL :: POLAREA,TOTAREA
 INTEGER :: IAREA,ICRDITYPE
 
 CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(2,'')

 CRDITYPE=0; ICRD=0

 TOTAREA=10.0E20; IAREA=0; ICRDITYPE=0
 DO SHPI=1,SHPNO
  !## only polygons with more than 2 points
  IF(SHPNCRD(SHPI).GT.0)THEN
   CRDITYPE=0
   CALL POLYGON2MOUSEMOVE(XC,YC,ICURSOR,SHPIACT(SHPI))
   IF(CRDITYPE.EQ.3.OR.CRDITYPE.EQ.4)THEN
    IF(SHPTYPE(SHPI).EQ.ID_RECTANGLE)THEN
     POLAREA=ABS(SHPXC(1,SHPI)-SHPXC(2,SHPI))*ABS(SHPYC(1,SHPI)-SHPYC(2,SHPI))
    ELSE
     POLAREA=ABS(UTL_POLYGON1AREA(SHPXC(:,SHPI),SHPYC(:,SHPI),SHPNCRD(SHPI)))
    ENDIF
    IF(POLAREA.LT.TOTAREA)THEN; TOTAREA=POLAREA; IAREA=SHPI; ICRDITYPE=CRDITYPE; ENDIF
   ELSE
    IF(CRDITYPE.NE.0)THEN
     ICRDITYPE=CRDITYPE; IAREA=SHPI
     EXIT
    ENDIF
   ENDIF
  ENDIF
 END DO
 
 !## take smallest polygon to be selected
 IF(ICRDITYPE.NE.0)THEN
  SHPI=IAREA
  CRDITYPE=ICRDITYPE
 ENDIF
 
 SELECT CASE (CRDITYPE)
  CASE (0)
   IF(ICURSOR.EQ.1)CALL WCURSORSHAPE(CURARROW)
  CASE (1)
   IF(ICURSOR.EQ.1)CALL WCURSORSHAPE(ID_CURSORADJUSTPOINT)
  CASE (2)
   IF(ICURSOR.EQ.1)CALL WCURSORSHAPE(ID_CURSORADDPOINT) 
  CASE (3)
   IF(ICURSOR.EQ.1)CALL WCURSORSHAPE(ID_CURSORMOVE)
  CASE (4)
   IF(ICURSOR.EQ.1)CALL WCURSORSHAPE(ID_CURSORPIPET)
 END SELECT
 
 IF(CRDITYPE.EQ.0)CALL WCURSORSHAPE(CURARROW)

 END SUBROUTINE POLYGON1MOUSEMOVE

 !###======================================================================
 SUBROUTINE POLYGON2MOUSEMOVE(XC,YC,ICURSOR,IACT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ICURSOR,IACT
 REAL,INTENT(IN) :: XC,YC
 REAL :: DX,DDX,X1,Y1,X2,Y2

 DX=SQRT((MPW%XMAX-MPW%XMIN)**2.0+(MPW%YMAX-MPW%YMIN)**2.0)/500.0

 !## check active polygons only
 IF(IACT.EQ.1.AND.ISHPEDIT.EQ.1)THEN

  !## node
  DO ICRD=1,SHPNCRD(SHPI)
   IF(XC.GE.SHPXC(ICRD,SHPI)-DX.AND.XC.LE.SHPXC(ICRD,SHPI)+DX.AND. &
      YC.GE.SHPYC(ICRD,SHPI)-DX.AND.YC.LE.SHPYC(ICRD,SHPI)+DX)EXIT
  END DO
  IF(ICRD.LE.SHPNCRD(SHPI))THEN
   CRDITYPE=1
   CALL WINDOWOUTSTATUSBAR(2,'Move Node '//TRIM(ITOS(ICRD))//' '//TRIM(SHPNAME(SHPI)))
   RETURN
  ENDIF

  IF(SHPTYPE(SHPI).EQ.ID_POLYGON.OR.SHPTYPE(SHPI).EQ.ID_LINE)THEN

   !## line segment
   DO ICRD=1,SHPNCRD(SHPI)
    IF(SHPTYPE(SHPI).EQ.ID_POLYGON)THEN
     IF(ICRD.LT.SHPNCRD(SHPI))THEN
      DDX=IGRDISTANCELINE(SHPXC(ICRD,SHPI),SHPYC(ICRD,SHPI),SHPXC(ICRD+1,SHPI),SHPYC(ICRD+1,SHPI),XC,YC,0)
      IF(DDX.GE.0.0.AND.DDX.LE.DX)EXIT
     ELSE
      DDX=IGRDISTANCELINE(SHPXC(1,SHPI),SHPYC(1,SHPI),SHPXC(SHPNCRD(SHPI),SHPI),SHPYC(SHPNCRD(SHPI),SHPI),XC,YC,0)
      IF(DDX.GE.0.0.AND.DDX.LE.DX)EXIT
     ENDIF
    ELSEIF(SHPTYPE(SHPI).EQ.ID_LINE)THEN
     IF(ICRD.LT.SHPNCRD(SHPI))THEN
      DDX=IGRDISTANCELINE(SHPXC(ICRD,SHPI),SHPYC(ICRD,SHPI),SHPXC(ICRD+1,SHPI),SHPYC(ICRD+1,SHPI),XC,YC,0)
      IF(DDX.GE.0.0.AND.DDX.LE.DX)EXIT
     ENDIF
    ENDIF
   END DO

   IF(ICRD.LE.SHPNCRD(SHPI))THEN
    CRDITYPE=2
    CALL WINDOWOUTSTATUSBAR(2,'Add point on Line '//TRIM(ITOS(ICRD))//'-'//TRIM(ITOS(ICRD+1))//'('//TRIM(RTOS(DDX,'F',2))//'m) '//&
     TRIM(SHPNAME(SHPI)))
    RETURN
   ENDIF
  ENDIF

 ENDIF

 IF(SHPTYPE(SHPI).EQ.ID_POLYGON)THEN
  IF(UTL_INSIDEPOLYGON(XC,YC,SHPXC(:,SHPI),SHPYC(:,SHPI),SHPNCRD(SHPI)).EQ.1)THEN
   IF(IACT.EQ.1)THEN
    CRDITYPE=3
    CALL WINDOWOUTSTATUSBAR(2,'Move '//TRIM(SHPNAME(SHPI)))
   ELSE
    CRDITYPE=4
    CALL WINDOWOUTSTATUSBAR(2,'Select '//TRIM(SHPNAME(SHPI)))
   ENDIF
  ENDIF
  IF(CRDITYPE.EQ.4.OR.CRDITYPE.EQ.3)RETURN
 ELSEIF(SHPTYPE(SHPI).EQ.ID_RECTANGLE)THEN
  X1=MIN(SHPXC(1,SHPI),SHPXC(2,SHPI)); X2=MAX(SHPXC(1,SHPI),SHPXC(2,SHPI))
  Y1=MIN(SHPYC(1,SHPI),SHPYC(2,SHPI)); Y2=MAX(SHPYC(1,SHPI),SHPYC(2,SHPI))
  IF(XC.GT.X1.AND.XC.LT.X2.AND.YC.GT.Y1.AND.YC.LT.Y2)THEN
   IF(IACT.EQ.1)THEN
    CRDITYPE=3
    CALL WINDOWOUTSTATUSBAR(2,'Move '//TRIM(SHPNAME(SHPI)))
   ELSE
    CRDITYPE=4
    CALL WINDOWOUTSTATUSBAR(2,'Select '//TRIM(SHPNAME(SHPI)))
   ENDIF
  ENDIF
  IF(CRDITYPE.EQ.4.OR.CRDITYPE.EQ.3)RETURN 
 ELSEIF(SHPTYPE(SHPI).EQ.ID_POINT.OR.SHPTYPE(SHPI).EQ.ID_LINE)THEN
  DO ICRD=1,SHPNCRD(SHPI)
   IF(XC.GE.SHPXC(ICRD,SHPI)-DX.AND.XC.LE.SHPXC(ICRD,SHPI)+DX.AND. &
      YC.GE.SHPYC(ICRD,SHPI)-DX.AND.YC.LE.SHPYC(ICRD,SHPI)+DX)EXIT
  END DO
  IF(ICRD.LE.SHPNCRD(SHPI))THEN
   CRDITYPE=4
   CALL WINDOWOUTSTATUSBAR(2,'Select '//TRIM(SHPNAME(SHPI)))
   RETURN
  ENDIF
 ENDIF

 ICRD    =0
 CRDITYPE=0

 END SUBROUTINE POLYGON2MOUSEMOVE

 !###======================================================================
 SUBROUTINE POLYGON1DELNODE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER               :: I,SHPJ

 SHPJ=SHPI
 CALL POLYGON1DRAWSHAPE(SHPJ,SHPJ)
 SHPNCRD(SHPJ)=SHPNCRD(SHPJ)-1
 DO I=ICRD,SHPNCRD(SHPJ)
  SHPXC(I,SHPJ)=SHPXC(I+1,SHPJ)
  SHPYC(I,SHPJ)=SHPYC(I+1,SHPJ)
 END DO
 CRDITYPE=0
 CALL POLYGON1DRAWSHAPE(SHPJ,SHPJ)
 IDOWN=0

 END SUBROUTINE POLYGON1DELNODE

 !###======================================================================
 SUBROUTINE POLYGON1LINECOLOR()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IRGB,SHPJ

 IDOWN=0

 IRGB=SHPCOLOR(SHPI)
 CALL WSELECTCOLOUR(IRGB)
 IF(WINFODIALOG(4).NE.1)RETURN
 SHPJ=SHPI
 CALL POLYGON1DRAWSHAPE(SHPJ,SHPJ)

 SHPCOLOR(SHPJ)=IRGB

 CALL POLYGON1DRAWSHAPE(SHPJ,SHPJ)

 END SUBROUTINE POLYGON1LINECOLOR

 !###======================================================================
 SUBROUTINE POLYGON1LINETHICKNESS(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 INTEGER :: SHPJ

 SHPJ=SHPI
 CALL POLYGON1DRAWSHAPE(SHPJ,SHPJ)

 SELECT CASE(ID)
  CASE (ID_LTHICKNESS1)
   SHPWIDTH(SHPJ)=1
  CASE (ID_LTHICKNESS2)
   SHPWIDTH(SHPJ)=2
  CASE (ID_LTHICKNESS3)
   SHPWIDTH(SHPJ)=3
 END SELECT

 CALL POLYGON1DRAWSHAPE(SHPJ,SHPJ)
 IDOWN=0

 END SUBROUTINE POLYGON1LINETHICKNESS

 !###======================================================================
 SUBROUTINE POLYGON1ADJUSTSHAPE(XC,YC,DOWNX,DOWNY)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(INOUT) :: DOWNX,DOWNY
 REAL,INTENT(IN) :: XC,YC
 REAL :: DX,DY
 INTEGER :: I,SHPJ

 !## remove current line ...
 SHPJ=SHPI
 CALL POLYGON1DRAWSHAPE(SHPJ,SHPJ)
 SHPI=SHPJ
 DX=XC-DOWNX
 DY=YC-DOWNY

 SELECT CASE (CRDITYPE)
  !## adjust current
  CASE (1)
   SHPXC(ICRD,SHPI)=SHPXC(ICRD,SHPI)+DX
   SHPYC(ICRD,SHPI)=SHPYC(ICRD,SHPI)+DY
  !## add point
  CASE (2)
   SHPNCRD(SHPI)=SHPNCRD(SHPI)+1
   DO I=SHPNCRD(SHPI),ICRD+2,-1
    SHPXC(I,SHPI)=SHPXC(I-1,SHPI)
    SHPYC(I,SHPI)=SHPYC(I-1,SHPI)
   END DO
   SHPXC(ICRD+1,SHPI)=XC
   SHPYC(ICRD+1,SHPI)=YC
   CRDITYPE=1
   ICRD    =ICRD+1
  !## move
  CASE (3)
   DO I=1,SHPNCRD(SHPI)
    SHPXC(I,SHPI)=SHPXC(I,SHPI)+DX
    SHPYC(I,SHPI)=SHPYC(I,SHPI)+DY
   ENDDO
 END SELECT

 !## construct new ysel ... in irmode only
 IF(IRWIN.GT.0)CALL IR_GETSELECTEDCELLS()

 !## draw and plot current line
 CALL POLYGON1DRAWSHAPE(SHPJ,SHPJ)
 SHPI=SHPJ

 DOWNX=DOWNX+DX
 DOWNY=DOWNY+DY

 END SUBROUTINE POLYGON1ADJUSTSHAPE

 !###======================================================================
 SUBROUTINE POLYGON1DELETE(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 INTEGER :: I,N
 CHARACTER(LEN=1000) :: STRING

 CALL WDIALOGSELECT(ID)
 CALL WDIALOGGETMENU(IDF_MENU1,SHPIACT)

 STRING='Are you sure to delete:'
 DO I=1,SHPNO
  IF(SHPIACT(I).EQ.1)STRING=TRIM(STRING)//CHAR(13)//TRIM(SHPNAME(I))
 END DO
 CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,TRIM(STRING),'Question')
 IF(WINFODIALOG(4).NE.1)RETURN

 N=0
 DO I=1,SHPNO
  IF(SHPIACT(I).EQ.1)THEN
   !## remove selected
   CALL POLYGON1DRAWSHAPE(I,I)
  ELSE
   N=N+1
   IF(N.NE.I)THEN
    SHPXC(:,N) =SHPXC(:,I)
    SHPYC(:,N) =SHPYC(:,I)
    SHPNCRD(N) =SHPNCRD(I)
    SHPCOLOR(N)=SHPCOLOR(I)
    SHPNAME(N) =SHPNAME(I)
    SHPTYPE(N) =SHPTYPE(I)
   ENDIF
  ENDIF
 END DO
 SHPNO  =N
 SHPIACT=0
 IF(SHPNO.GT.0)THEN
  CALL WDIALOGPUTMENU(IDF_MENU1,SHPNAME,SHPNO,SHPIACT)
 ELSE
  CALL WDIALOGCLEARFIELD(IDF_MENU1)
 ENDIF

 CALL IDFPLOTFAST(0)

 END SUBROUTINE POLYGON1DELETE

 !###======================================================================
 SUBROUTINE POLYGON1RENAME(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: I,ITYPE

 CALL WDIALOGSELECT(ID)
 CALL WDIALOGGETMENU(IDF_MENU1,SHPIACT)

 CALL WDIALOGLOAD(ID_POLYGONSHAPENAME,ID_POLYGONSHAPENAME)
 CALL WDIALOGSHOW(-1,-1,0,3)
 DO I=1,SHPNO
  IF(SHPIACT(I).EQ.1)EXIT
 ENDDO
 CALL WDIALOGPUTSTRING(IDF_STRING1,SHPNAME(I))

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDOK)
      CALL WDIALOGGETSTRING(IDF_STRING1,SHPNAME(I))
      EXIT
     CASE (IDCANCEL)
      EXIT
     CASE (IDHELP)
       CALL IMODGETHELP('3.2.2','EMO.CreateGEN')     
     END SELECT
  END SELECT
 ENDDO

 CALL WDIALOGUNLOAD()
 CALL WDIALOGSELECT(ID)
 IF(MESSAGE%VALUE1.EQ.IDOK)CALL WDIALOGPUTMENU(IDF_MENU1,SHPNAME,SHPNO,SHPIACT)

 END SUBROUTINE POLYGON1RENAME

 !###======================================================================
 SUBROUTINE POLYGONGETSHAPE(ISHAPE,IACT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: ISHAPE
 INTEGER,INTENT(IN),DIMENSION(6) :: IACT
 INTEGER             :: ITYPE,I,N
 TYPE(WIN_MESSAGE)   :: MESSAGE
 INTEGER,DIMENSION(6) :: ID,JD
 DATA ID/ID_POINT,ID_RECTANGLE,ID_POLYGON,ID_CIRCLE,ID_LINE,ID_GRID/
 DATA JD/IDF_PICTURE1,IDF_PICTURE2,IDF_PICTURE3,IDF_PICTURE4,IDF_PICTURE5,IDF_PICTURE9/

 N=0; DO I=1,SIZE(IACT); IF(IACT(I).EQ.1)N=N+1; ENDDO
 
 IF(N.EQ.1)THEN !SUM(IACT).EQ.1)THEN
  DO I=1,SIZE(ID); IF(IACT(I).EQ.1)ISHAPE=ID(I); END DO
 ELSE
 !## get tool first
  CALL WDIALOGLOAD(ID_DTOOLS)
  CALL WDIALOGPUTIMAGE(IDF_PICTURE1,ID_ICONPOINT)
  CALL WDIALOGPUTIMAGE(IDF_PICTURE2,ID_ICONRECTANGLE)
  CALL WDIALOGPUTIMAGE(IDF_PICTURE3,ID_ICONPOLYGON)
  CALL WDIALOGPUTIMAGE(IDF_PICTURE4,ID_ICONCIRCLE)
  CALL WDIALOGPUTIMAGE(IDF_PICTURE5,ID_ICONLINE)
  CALL WDIALOGPUTIMAGE(IDF_PICTURE9,ID_ICONGRID)
  DO I=1,SIZE(ID); CALL WDIALOGFIELDSTATE(ID(I),IACT(I)); END DO
  DO I=1,SIZE(JD); CALL WDIALOGFIELDSTATE(JD(I),IACT(I)); END DO
  DO I=1,SIZE(ID)
   IF(IACT(I).EQ.1)THEN
    CALL WDIALOGPUTRADIOBUTTON(ID(I))
    ISHAPE=ID(I); EXIT
   ENDIF
  END DO
  CALL WDIALOGSHOW(ITYPE=SEMIMODELESS)
  DO WHILE(.TRUE.)
   CALL WMESSAGE(ITYPE, MESSAGE)
   SELECT CASE (ITYPE)
    CASE (FIELDCHANGED)
     ISHAPE=MESSAGE%VALUE1
    CASE(PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (IDOK,IDCANCEL)
       EXIT
     END SELECT
   END SELECT
  ENDDO
  CALL WDIALOGUNLOAD()
  IF(MESSAGE%VALUE1.EQ.IDCANCEL)THEN
   ISHAPE=0; RETURN
  ENDIF
 ENDIF

 SELECT CASE (ISHAPE)
  CASE (ID_POINT)
   CALL WCURSORSHAPE(ID_CURSORIDFVALUE)
  CASE (ID_CIRCLE)
   CALL WCURSORSHAPE(ID_CURSORCIRCLE)
  CASE (ID_RECTANGLE)
  CALL WCURSORSHAPE(ID_CURSORRECTANGLE)
  CASE (ID_POLYGON)
   CALL WCURSORSHAPE(ID_CURSORPOLYGON)
  CASE (ID_LINE)
   CALL WCURSORSHAPE(ID_CURSORLINE)
  CASE (ID_GRID)
!   CALL WCURSORSHAPE(ID_CURSORLINE)
 END SELECT

 END SUBROUTINE POLYGONGETSHAPE

END MODULE MOD_POLYGON

