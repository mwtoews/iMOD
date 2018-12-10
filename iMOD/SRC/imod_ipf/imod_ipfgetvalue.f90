!!  Copyright (C) Stichting Deltares, 2005-2018.
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
MODULE MOD_IPFGETVALUE

USE WINTERACTER
USE RESOURCE
USE MODPLOT
USE MOD_IDFPLOT
USE MOD_DBL
USE IMODVAR 
USE MOD_UTL 
USE MOD_SETTINGS
USE MOD_IPFASSFILE
USE MOD_IPFANALYSE
USE MOD_IPFGETVALUE_COLOURS 
USE MOD_IPF_PAR
USE MOD_IPF 
USE MOD_INFO
USE MOD_PROFILE_PAR 
USE MOD_PROFILE_UTL 
USE MOD_IDFTIMESERIE_UTL
USE MOD_TOPO 
USE MOD_CREATEIPF
USE DATEVAR
USE MOD_IPFGETVALUE_UTL

TYPE(WIN_MESSAGE),PRIVATE :: IPFMESSAGE
INTEGER,PRIVATE :: IPFITYPE

INTEGER,PRIVATE :: SSEL  !## selection mode
INTEGER :: JIPF  !## selected ipf (ook gebruikt in idtimeseries)
INTEGER :: ISEL  !## selected record of jipf (ook gebruikt in idtimeseries)
INTEGER, PRIVATE :: IQUICK
REAL(KIND=DP_KIND) :: GXMIN,GXMAX,GYMIN,GYMAX

CONTAINS

 !###======================================================================
 SUBROUTINE IPFGETVALUE_MAIN()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,IEXIT
 INTEGER :: IIPF,IPLOT,ILEG
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IC
 REAL(KIND=DP_KIND) :: MOUSEX,MOUSEY
 
 CALL MAIN_UTL_INACTMODULE(ID_ANALYSEIPF)
 IF(IDIAGERROR.EQ.1)RETURN

 CALL WMENUSETSTATE(ID_ANALYSEIPF,2,1)

 !## allocate memory for ipf-plotting, they will be read in memory and drawn from that
 CALL IPFINIT()

 CALL WDIALOGLOAD(ID_DIPFINFOFIND,ID_DIPFINFOFIND)
 CALL WDIALOGLOAD(ID_DIPFINFO,ID_DIPFINFO)

 IF(NIPF.GT.MXTAB)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'The maximal of IPFs files to be analysed simultaneously is '// &
   TRIM(ITOS(MXTAB))//CHAR(13)//'Currently you selected '//TRIM(ITOS(NIPF))//' files','Warning')
  CALL IPFGETVALUE_CLOSE()
  RETURN
 ENDIF

 CALL UTL_MESSAGEHANDLE(0)

 !## get proper iplot
 IIPF=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.2)THEN
   IIPF=IIPF+1
   IF(.NOT.IPFREAD(IPLOT,IIPF))THEN
    CALL IPFGETVALUE_CLOSE()
    CALL UTL_MESSAGEHANDLE(1)
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Make sure that the X,Y,Z,and Z2 columns in the IPF are'//CHAR(13)// &
     'assigned to real values instead of characters'//CHAR(13)// &
     'before entering the IPF Analyse functionality!','Error')
    RETURN
   ENDIF
   IPF(IIPF)%SYMBOL   =MP(IPLOT)%SYMBOL
   IPF(IIPF)%THICKNESS=MP(IPLOT)%THICKNESS
   IPF(IIPF)%PCOL     =IPF(IIPF)%ACOL
   IPF(IIPF)%IPLOT    =IPLOT
!   MP(IPLOT)%PCOL     =IPF(IIPF)%PCOL
   IPF(IIPF)%ILEGDLF  =MIN(MAX(1,MP(IPLOT)%ILEGDLF),10)
  ENDIF
 ENDDO

 CALL UTL_MESSAGEHANDLE(1)

 CALL WDIALOGSELECT(ID_DIPFINFOFIND)
 CALL WDIALOGPUTMENU(IDF_MENU1,IPF%ALIAS,NIPF,1)
 CALL WDIALOGPUTMENU(IDF_MENU2,IPF(1)%ATTRIB,IPF(1)%NCOL,1)
 
 CALL IPFGETVALUE_SELECTLOGICALFIELDS()

 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(ID_SFIGURE,1,1)

 CALL WDIALOGSELECT(ID_DIPFINFO_TAB1)
 CALL WDIALOGPUTIMAGE(ID_SERIES,ID_ICONSERIES)
 CALL WDIALOGPUTIMAGE(ID_PROP,ID_ICONPROPERTIES)
 CALL WDIALOGPUTIMAGE(ID_DELETE,ID_ICONDELETE)

 DO IIPF=1,NIPF
  CALL WDIALOGSELECT(ID_DIPFINFO_TAB1)
  CALL WDIALOGTABSTATE(IDF_TAB1,IDTAB(IIPF),1)
  CALL WDIALOGSELECT(IDTAB(IIPF))
  CALL WDIALOGTITLE(TRIM(IPF(IIPF)%ALIAS))
  IPF(IIPF)%PCOL=MIN(IPF(IIPF)%NCOL,MAX(1,IPF(IIPF)%ACOL))
  MP(IPF(IIPF)%IPLOT)%PCOL=IPF(IIPF)%PCOL
  CALL WDIALOGPUTMENU(IDF_MENU1,IPF(IIPF)%ATTRIB,IPF(IIPF)%NCOL,IPF(IIPF)%PCOL)
  CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,1)
  CALL WDIALOGPUTIMAGE(ID_REDRAW,ID_ICONREDRAW)
 END DO
 DO IIPF=NIPF+1,MXTAB
  CALL WDIALOGSELECT(ID_DIPFINFO_TAB1)
  CALL WDIALOGTABSTATE(IDF_TAB1,IDTAB(IIPF),0)
  CALL WDIALOGSELECT(IDTAB(IIPF))
  CALL WDIALOGTITLE('empty')
 END DO

 CALL WDIALOGSELECT(ID_DIPFINFO_TAB1)
 CALL WDIALOGFIELDSTATE(ID_SERIES,I)
 CALL WDIALOGFIELDSTATE(ID_DELETE,I)

 CALL IPFGETVALUE_FILLGRID()

 CALL WDIALOGSELECT(ID_DIPFINFO_TAB2)
 CALL WDIALOGPUTMENU(IDF_MENU1,CDATE,12,4)
 CALL WDIALOGPUTMENU(IDF_MENU2,CDATE,12,3)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,14)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER3,28)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER2,1996)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER4,2004)
 CALL IDFTIMESERIE_FIELDS(0)

 DO IIPF=1,NIPF
  CALL WDIALOGSELECT(IDTAB(IIPF))
  CALL WDIALOGCLEARFIELD(IDF_GRID1)
  CALL WGRIDSETCELL(IDF_GRID1,1,1)
  ALLOCATE(IC(IPF(IIPF)%NCOL))
  IC=1
  CALL WGRIDCOLUMNS(IDF_GRID1,IPF(IIPF)%NCOL,IC)
  DEALLOCATE(IC)
  DO I=1,IPF(IIPF)%NCOL
   CALL WGRIDLABELCOLUMN(IDF_GRID1,I,IPF(IIPF)%ATTRIB(I))
  ENDDO
 ENDDO

 CALL WDIALOGSELECT(ID_DIPFINFO_TAB3)
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPEN)
 CALL WDIALOGPUTIMAGE(ID_SAVEAS,ID_ICONSAVEAS)

 !## fill with first selected legend
 CALL WDIALOGPUTMENU(IDF_MENU1,IPF%ALIAS,NIPF,1)
 ILEG=IPF(1)%ILEGDLF; CALL WDIALOGPUTOPTION(IDF_MENU2,ILEG)
 CALL IPFGETVALUE_PLOTCOLOURS(ID_DIPFINFO_TAB3,ILEG)

 !## turn off plotting of figures
 CALL IPFGETVALUE_ADJUSTMENU(0)

 CALL WDIALOGSELECT(ID_DIPFINFO)
 CALL WDIALOGSHOW(-0,65,0,2)

 ISEL=0
 SSEL=0
 DO
  CALL WMESSAGE(IPFITYPE,IPFMESSAGE)

  MOUSEX=DBLE(IPFMESSAGE%GX)+OFFSETX
  MOUSEY=DBLE(IPFMESSAGE%GY)+OFFSETY

  SELECT CASE (IPFITYPE)

   CASE (MENUSELECT)
    CALL IPFGETVALUE_MENUSELECT()
   CASE (TABCHANGED)
     CALL IPFGETVALUE_GETSELECTEDIROW()
   CASE (FIELDCHANGED)
    CALL IPFGETVALUE_FIELDCHANGED()
   CASE (PUSHBUTTON)
    CALL IPFGETVALUE_PUSHBUTTON(IEXIT)
    IF(IEXIT.EQ.1)EXIT
   CASE (MOUSEMOVE)
    CALL IPFGETVALUE_MOUSEMOVE(MOUSEX,MOUSEY,0,ID_DIPFINFO)
   CASE (MOUSEBUTDOWN)
    CALL IPFGETVALUE_MOUSEBUTDOWN()
   CASE (BITMAPSCROLLED)
    MPW%IX=IPFMESSAGE%VALUE1
    MPW%IY=IPFMESSAGE%VALUE2
  END SELECT

 END DO

 CALL IPFGETVALUE_CLOSE()
 CALL IDFPLOTFAST(0)

 END SUBROUTINE IPFGETVALUE_MAIN

 !###======================================================================
 SUBROUTINE IPFGETVALUE_ACOL(IIPF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IIPF
 INTEGER :: I

 I=0
 IF(IPF(IIPF)%ACOL.GT.0)I=1

 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(ID_SERIESIMPLE,1,I)
 CALL WMENUSETSTATE(ID_SERIEADVANCED,1,I)
 CALL WMENUSETSTATE(ID_SERIEQUICKVIEW,1,I)
 CALL WMENUSETSTATE(ID_FCUREXT,1,I)
 CALL WMENUSETSTATE(ID_FSELECTION,1,I)
 CALL WMENUSETSTATE(ID_FZOOMBOX,1,I)
 CALL WMENUSETSTATE(ID_FENTDOM,1,I)
 CALL WMENUSETSTATE(ID_DESELECTALL,1,I)

 END SUBROUTINE IPFGETVALUE_ACOL

 !###====================================================================
 SUBROUTINE IPFGETVALUE_MOUSEBUTUP()
 !###====================================================================
 IMPLICIT NONE

 SELECT CASE (IPFMESSAGE%VALUE1)
 END SELECT

 END SUBROUTINE IPFGETVALUE_MOUSEBUTUP

 !###====================================================================
 SUBROUTINE IPFGETVALUE_MOUSEBUTDOWN()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I

 SELECT CASE (IPFMESSAGE%VALUE1)
  !## left button, add/delete depends on existence of current point
  CASE (1)
   I=3
   IF(WMENUGETSTATE(ID_SERIESIMPLE,2).EQ.1)  I=1
   IF(WMENUGETSTATE(ID_SERIEADVANCED,2).EQ.1)I=2
   CALL IPFGETVALUE_ADJIP(IPFMESSAGE%VALUE1,I)
   CALL IPFGETVALUE_FILLGRID()
   CALL IDFPLOT(1)
   ISEL=0
  !## right button
  CASE (3)
   CALL WMENUFLOATING(ID_MENU2,IPFMESSAGE%X,IPFMESSAGE%Y)
 END SELECT

 END SUBROUTINE IPFGETVALUE_MOUSEBUTDOWN

 !###====================================================================
 SUBROUTINE IPFGETVALUE_MOUSEMOVE(X,Y,IPROF,ID)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Y
 INTEGER,INTENT(IN) :: IPROF,ID
 INTEGER :: I
 CHARACTER(LEN=50),DIMENSION(:),ALLOCATABLE :: TXTCOLUMN
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IP

 IF(IPROF.EQ.0)THEN
  CALL WINDOWSELECT(0)
  CALL WINDOWOUTSTATUSBAR(1,'X:'//TRIM(RTOS(X/1000.0D0,'F',2))//' km, Y:'// &
                                  TRIM(RTOS(Y/1000.0D0,'F',2))//' km')
 !## profile
 ELSEIF(IPROF.EQ.1)THEN
  CALL WINDOWOUTSTATUSBAR(1,'Distance: '//TRIM(RTOS(X,'F',2))//' m, Map Value:'// &
                                  TRIM(RTOS(Y,'F',2)))
 ENDIF

 CALL IPFGETVALUE_GETCURRENTPOS(X,Y,IPROF,ID)

 IF(JIPF.GT.0)THEN
  IF(IPF(JIPF)%ACOL.GT.0)CALL WINDOWOUTSTATUSBAR(4,'Current Point: '//TRIM(IPF(JIPF)%INFO(IPF(JIPF)%ACOL,ISEL)))
  IF(IPF(JIPF)%ACOL.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Current Point: '//TRIM(IPF(JIPF)%INFO(1,ISEL)))
 ELSE
  CALL WINDOWOUTSTATUSBAR(4,'')
 ENDIF

 IF(ID.EQ.ID_DIPFINFO)THEN !_TAB1
!  IF(IPROF.EQ.0)THEN
!   CALL WDIALOGSELECT(ID_DIPFINFO_TAB1)
!   IF(ISEL.GT.0)THEN
!    CALL WDIALOGSETTAB(IDF_TAB1,IDTAB(JIPF))
!    CALL WDIALOGSELECT(IDTAB(JIPF))
!    DO I=1,IPF(JIPF)%NCOL
!     CALL WGRIDPUTCELLSTRING(IDF_GRID1,I,1,IPF(JIPF)%INFO(I,ISEL))
!    END DO
!   ENDIF
!  ENDIF

 ELSEIF(ID.EQ.ID_DTIMESERIES)THEN
  ALLOCATE(TXTCOLUMN(IPF(JIPF)%NCOL),IP(IPF(JIPF)%NCOL))
  DO I=1,IPF(JIPF)%NCOL
   TXTCOLUMN(I)=TRIM(IPF(JIPF)%ATTRIB(I))//'='//TRIM(IPF(JIPF)%INFO(I,ISEL))
  END DO
  !## plot label information
  CALL WDIALOGSELECT(ID_DTIMESERIESTAB3)
  !## get selected attributes
  IP   =0
  CALL WDIALOGGETMENU(IDF_MENU1,IP) 
  CALL WDIALOGPUTMENU(IDF_MENU1,TXTCOLUMN,IPF(JIPF)%NCOL,IP)
  DEALLOCATE(TXTCOLUMN,IP)
 ENDIF

 IF(ISEL.EQ.0)THEN
  IF(WINFOMOUSE(MOUSECURSOR).NE.CURARROW)CALL WCURSORSHAPE(CURARROW)
 ELSE
  !## see whether it has been selected already... (simple/advanced/selected)
  IF(IPF(JIPF)%IP(ISEL).GT.0.AND.IPF(JIPF)%IP(ISEL).LE.3)THEN
   IF(WINFOMOUSE(MOUSECURSOR).NE.ID_CURSORPOINTMIN)CALL WCURSORSHAPE(ID_CURSORPOINTMIN)
  ELSE
   IF(WINFOMOUSE(MOUSECURSOR).NE.ID_CURSORPOINTPLUS)CALL WCURSORSHAPE(ID_CURSORPOINTPLUS)
  ENDIF
 ENDIF

 END SUBROUTINE IPFGETVALUE_MOUSEMOVE

 !###====================================================================
 SUBROUTINE IPFGETVALUE_GETCURRENTPOS(X,Y,IPROF,ID)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Y
 INTEGER,INTENT(IN) :: IPROF,ID
 REAL(KIND=DP_KIND) :: D,MIND,DX,DY
 INTEGER :: IIPF,I
 
 !## initial minimal distance
 IF(ID.EQ.ID_DTIMESERIES)THEN
  MIND=10.0D10
 ELSE
  IF(IPROF.EQ.0)MIND=(MPW%XMAX-MPW%XMIN)/100.0D0
  IF(IPROF.EQ.1)MIND=10.0D10
 ENDIF
 
 ISEL=0
 JIPF=0

 IF(IPROF.EQ.0)THEN
  DO IIPF=1,NIPF
   DO I=1,IPF(IIPF)%NROW
    DX=(IPF(IIPF)%XYZ(1,I)-X)**2.0D0  !## x-coordinate
    DY=(IPF(IIPF)%XYZ(2,I)-Y)**2.0D0  !## y-coordinate
    D = DX+DY
    IF(D.GT.0.0D0)D=SQRT(D)
    IF(D.LT.MIND)THEN
     MIND=D
     ISEL=I
     JIPF=IIPF
    ENDIF
   END DO
  END DO
 ELSEIF(IPROF.EQ.1)THEN
  DO IIPF=1,NIPF
  !## number points drawn
   DO I=1,IPF(IIPF)%NROW
    !## displayed in cross-section
    IF(IPF(IIPF)%IPOS(I).EQ.INT(1,1))THEN
     DX=(IPF(IIPF)%XYPOS(1,I)-X)**2.0D0  !## x-coordinate
     DY=(IPF(IIPF)%XYPOS(2,I)-Y)**2.0D0  !## z-coordinate
     D = DX+DY
     IF(D.GT.0.0D0)D=SQRT(D)
     IF(D.LT.MIND)THEN
      MIND=D
      ISEL=I
      JIPF=IIPF
     ENDIF
    ENDIF
   END DO
  END DO
 ENDIF
 
 END SUBROUTINE IPFGETVALUE_GETCURRENTPOS

 !###====================================================================
 SUBROUTINE IPFGETVALUE_PLOTCURRENTPOS()
 !###====================================================================
 IMPLICIT NONE

!PAUSE
! IF(IPROF.EQ.0)THEN
!  CALL UTL_PLOT1BITMAP()
!  CALL IPFPLOTSELECTED(JIPF,ISEL,IPROF)
!  CALL UTL_PLOT2BITMAP()
! ELSEIF(IPROF.EQ.1)THEN
!  CALL IPFGETVALUE_PLOTMODE1()
!  CALL IPFPLOTSELECTED(JIPF,ISEL,IPROF)
!  CALL IPFGETVALUE_PLOTMODE2()
! ENDIF

 END SUBROUTINE IPFGETVALUE_PLOTCURRENTPOS

 !###====================================================================
 SUBROUTINE IPFGETVALUE_PLOTMODE1()
 !###====================================================================
 IMPLICIT NONE

! CALL IGRSELECT(DRAWBITMAP,IBITMAP)
! CALL IGRPLOTMODE(MODEXOR)

 END SUBROUTINE IPFGETVALUE_PLOTMODE1

 !###====================================================================
 SUBROUTINE IPFGETVALUE_PLOTMODE2()
 !###====================================================================
 IMPLICIT NONE

! CALL IGRSELECT(DRAWWIN,IWINPROFILE(1))
! CALL WBITMAPPUT(IBITMAP,0,1)
! CALL IGRPLOTMODE(MODECOPY)

 END SUBROUTINE IPFGETVALUE_PLOTMODE2

 !###====================================================================
 SUBROUTINE IPFGETVALUE_PUSHBUTTON(IEXIT)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IEXIT
 INTEGER :: I1,I2,ITAB,IR,I,ILEG,IIPF
 CHARACTER(LEN=10) :: STRING

 IEXIT=0

 SELECT CASE (IPFMESSAGE%WIN)
  !## main dialog
  CASE (ID_DIPFINFO)
   SELECT CASE (IPFMESSAGE%VALUE1)
    CASE (IDHELP)
      CALL UTL_GETHELP('4.2.3','MMO.IPO.IPFAna')
    CASE (IDCANCEL)
     IEXIT=1
   END SELECT
  CASE (ID_DIPFINFO_TAB1)
   SELECT CASE (IPFMESSAGE%VALUE1)
    !## plot series
    CASE (ID_SERIES)
     CALL UTL_HIDESHOWDIALOG(ID_DIPFINFO,0)
     CALL IPFANALYSE_MAIN()
     CALL UTL_HIDESHOWDIALOG(ID_DIPFINFO,2)
    !## show properties
    CASE (ID_PROP)
     CALL UTL_HIDESHOWDIALOG(ID_DIPFINFO,0)
     !## get tab
     CALL WDIALOGSELECT(ID_DIPFINFO_TAB1)
     CALL WDIALOGGETTAB(IDF_TAB1,ITAB)
     DO IIPF=1,NIPF; IF(ITAB.EQ.IDTAB(IIPF))EXIT; ENDDO
     CALL SETTINGS_MAIN(TRIM(IPF(IIPF)%ALIAS))
     !## reread ipf-file = neccessary because settings can be changed
     CALL IDFPLOTFAST(0)
     CALL IPFGETVALUE_RESTORESELECTION()
     !## replot ipf to show selection again
     CALL IDFPLOTFAST(0)
     CALL UTL_HIDESHOWDIALOG(ID_DIPFINFO,2)
    !## delete selection
    CASE (ID_DELETE)
     CALL WDIALOGSELECT(ID_DIPFINFO_TAB1)
     CALL WDIALOGGETTAB(IDF_TAB1,ITAB)
     CALL WDIALOGSELECT(ITAB)
     I1=WINFOGRID(IDF_GRID1,GRIDSELROW1)
     I2=WINFOGRID(IDF_GRID1,GRIDSELROW2)
     IF(I1.EQ.0.OR.I2.EQ.0)THEN
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to select one or more rows in the tabel','Warning')
     ELSE
      CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to delete row: ' &
        //TRIM(ITOS(I1))//' until row: '//TRIM(ITOS(I2))//' ?','Question')
      IF(WINFODIALOG(4).EQ.1)THEN
       DO IIPF=1,MXTAB
        IF(IDTAB(IIPF).EQ.ITAB)EXIT
       ENDDO
       DO IR=I1,I2
        CALL WGRIDGETROWLABEL(IDF_GRID1,IR,STRING)
        READ(STRING,*) I
        IPF(IIPF)%IP(I)=INT(0,1)
       ENDDO
       CALL IPFGETVALUE_FILLGRID()
       CALL IDFPLOT(1)
      ENDIF
     ENDIF
   END SELECT

 !## ipf files
 CASE (ID_DIPFINFO_TAB1TAB1,ID_DIPFINFO_TAB1TAB2,ID_DIPFINFO_TAB1TAB3,ID_DIPFINFO_TAB1TAB4,ID_DIPFINFO_TAB1TAB5)
  SELECT CASE (IPFMESSAGE%VALUE1)
   CASE (ID_REDRAW)
    CALL IDFPLOT(1)
  END SELECT

 !## settings
 CASE (ID_DIPFINFO_TAB2)
  SELECT CASE (IPFMESSAGE%VALUE1)
   !## replot figure
   CASE (ID_APPLY)
    CALL IDFPLOT(1)
  END SELECT

 !## colours
 CASE (ID_DIPFINFO_TAB3)
  SELECT CASE (IPFMESSAGE%VALUE1)
   CASE (ID_OPEN,ID_SAVEAS)
    CALL WDIALOGGETMENU(IDF_MENU2,ILEG)
    CALL IPFGETVALUE_OPENSAVECOLOURS('',IPFMESSAGE%VALUE1,ID_DIPFINFO_TAB3,ILEG)
  END SELECT

 END SELECT

 END SUBROUTINE IPFGETVALUE_PUSHBUTTON

 !###====================================================================
 SUBROUTINE IPFGETVALUE_FIELDCHANGED()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: IIPF,I,J,I1,ICOL,IROW,IRGB,ILEG

 CALL WDIALOGSELECT(IPFMESSAGE%WIN)

 SELECT CASE (IPFMESSAGE%WIN)

  !## start info dialog
  CASE (ID_DIPFINFO_TAB1)
!   SELECT CASE (IPFMESSAGE%VALUE1)
!    CASE (ID_INFO)
!     CALL WDIALOGGETCHECKBOX(ID_INFO,I)
!     CALL WDIALOGSELECT(ID_DIPFINFO_PREF)
!     IF(I.EQ.0)CALL WDIALOGHIDE()
!     IF(I.EQ.1)CALL WDIALOGSHOW(0,0,0,2)
!   END SELECT

  !## select ipf-point
  CASE (ID_DIPFINFO_TAB1TAB1,ID_DIPFINFO_TAB1TAB2,ID_DIPFINFO_TAB1TAB3, &
        ID_DIPFINFO_TAB1TAB4,ID_DIPFINFO_TAB1TAB5)
   SELECT CASE (IPFMESSAGE%VALUE1)
    CASE (IDF_CHECK1,IDF_MENU1)
     CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I)
     CALL WDIALOGFIELDSTATE(IDF_MENU1,I)
     IF(I.EQ.1)THEN
      SELECT CASE (IPFMESSAGE%WIN)
       CASE(ID_DIPFINFO_TAB1TAB1); CALL WDIALOGGETMENU(IDF_MENU1,IPF(1)%PCOL)
       CASE(ID_DIPFINFO_TAB1TAB2); CALL WDIALOGGETMENU(IDF_MENU1,IPF(2)%PCOL)
       CASE(ID_DIPFINFO_TAB1TAB3); CALL WDIALOGGETMENU(IDF_MENU1,IPF(3)%PCOL)
       CASE(ID_DIPFINFO_TAB1TAB4); CALL WDIALOGGETMENU(IDF_MENU1,IPF(4)%PCOL)
       CASE(ID_DIPFINFO_TAB1TAB5); CALL WDIALOGGETMENU(IDF_MENU1,IPF(5)%PCOL)
      END SELECT
     ELSE
      SELECT CASE (IPFMESSAGE%WIN)
       CASE(ID_DIPFINFO_TAB1TAB1); IPF(1)%PCOL=0
       CASE(ID_DIPFINFO_TAB1TAB2); IPF(2)%PCOL=0
       CASE(ID_DIPFINFO_TAB1TAB3); IPF(3)%PCOL=0
       CASE(ID_DIPFINFO_TAB1TAB4); IPF(4)%PCOL=0
       CASE(ID_DIPFINFO_TAB1TAB5); IPF(5)%PCOL=0
      END SELECT
     ENDIF
    CASE (IDF_GRID1)
     CALL IPFGETVALUE_GETSELECTEDIROW()
   END SELECT

  !## select fixed y-axes
  CASE (ID_DIPFINFO_TAB2)
   SELECT CASE (IPFMESSAGE%VALUE1)
    CASE (IDF_CHECK1,IDF_CHECK2)
     CALL IDFTIMESERIE_FIELDS(0)
    !## type of simple/extended plottype
    CASE (IDF_MENU3)
     CALL WDIALOGGETMENU(IDF_MENU3,I)
     IF(I.EQ.1)THEN
      CALL IPFGETVALUE_ADJUSTMENU(0)
      J=0
     ELSE
      IF(I.EQ.2)CALL IPFGETVALUE_ADJUSTMENU(ID_SERIESIMPLE)
      IF(I.EQ.3)CALL IPFGETVALUE_ADJUSTMENU(ID_SERIEADVANCED)
      J=1
     ENDIF
     CALL WDIALOGFIELDSTATE(IDF_MENU4,J)
     CALL WDIALOGFIELDSTATE(IDF_CHECK3,J)
    !## type of continuous lines/block lines
    CASE (IDF_MENU4)
     CALL WDIALOGGETMENU(IDF_MENU4,I)
     IF(I.EQ.1)I1=ID_CONTLINES
     IF(I.EQ.2)I1=ID_BLOCKLINES
     CALL WINDOWSELECT(0)
     CALL WMENUSETSTATE(ID_CONTLINES,2,0)
     CALL WMENUSETSTATE(ID_BLOCKLINES,2,0)
     CALL WMENUSETSTATE(I1,2,1)
    !## mark points
    CASE (IDF_CHECK3)
     CALL WDIALOGGETCHECKBOX(IDF_CHECK3,I)
     CALL WINDOWSELECT(0)
     CALL WMENUSETSTATE(ID_MARKDATA,2,I)
   END SELECT

  !## colouring
  CASE (ID_DIPFINFO_TAB3)
   SELECT CASE (IPFMESSAGE%VALUE2)
    CASE (IDF_MENU1)
     CALL WDIALOGGETMENU(IDF_MENU1,IIPF)
     ILEG=IPF(IIPF)%ILEGDLF
     CALL WDIALOGPUTOPTION(IDF_MENU2,ILEG)
     CALL IPFGETVALUE_PLOTCOLOURS(ID_DIPFINFO_TAB3,ILEG)
    CASE (IDF_MENU2)
     CALL WDIALOGGETMENU(IDF_MENU1,IIPF)
     CALL WDIALOGGETMENU(IDF_MENU2,ILEG)
     IPF(IIPF)%ILEGDLF=ILEG
     CALL IPFGETVALUE_PLOTCOLOURS(ID_DIPFINFO_TAB3,ILEG)
    CASE (IDF_GRID1)
     CALL WDIALOGGETMENU(IDF_MENU1,IIPF)
     ILEG=IPF(IIPF)%ILEGDLF
     CALL WGRIDPOS(IPFMESSAGE%Y,ICOL,IROW)
     IF(ICOL.EQ.2)THEN
      IRGB=BH(ILEG,IROW)%LITHOCLR
      CALL WSELECTCOLOUR(IRGB)
      IF(WINFODIALOG(4).EQ.1)THEN
       BH(ILEG,IROW)%LITHOCLR=IRGB
       CALL IPFGETVALUE_GETCOLOURS(ID_DIPFINFO_TAB3,ILEG)
       CALL IPFGETVALUE_PLOTCOLOURS(ID_DIPFINFO_TAB3,ILEG)
      ENDIF
     ENDIF

   END SELECT

 END SELECT

 END SUBROUTINE IPFGETVALUE_FIELDCHANGED

 !###====================================================================
 SUBROUTINE IPFGETVALUE_GETSELECTEDIROW()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I1,I2,ITAB,I

 CALL WDIALOGSELECT(ID_DIPFINFO_TAB1)
 CALL WDIALOGGETTAB(IDF_TAB1,ITAB)
 CALL WDIALOGSELECT(ITAB)
 I1=WINFOGRID(IDF_GRID1,GRIDSELROW1)
 I2=WINFOGRID(IDF_GRID1,GRIDSELROW2)
 I=0
 IF(I1.NE.0.AND.I2.NE.0)I=1
 CALL WDIALOGSELECT(ID_DIPFINFO_TAB1)
 CALL WDIALOGFIELDSTATE(ID_DELETE,I)

 END SUBROUTINE IPFGETVALUE_GETSELECTEDIROW

 !###====================================================================
 SUBROUTINE IPFGETVALUE_MENUSELECT()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,IIPF,IEXIT

 SELECT CASE (IPFMESSAGE%VALUE1)
  CASE (ID_DISTANCE)
   CALL UTL_MEASUREMAIN()
  CASE (ID_COPY)
   CALL WCLIPBOARDPUTBITMAP(MPW%IBITMAP)
  CASE(ID_ZOOMINMAP,ID_ZOOMOUTMAP,ID_ZOOMRECTANGLEMAP,ID_ZOOMFULLMAP,ID_ZOOMPREVIOUS,ID_ZOOMNEXT)
   I=IPFMESSAGE%VALUE1
   CALL MANAGER_UTL_MENUFIELDS(I,1,0)
   CALL IDFZOOM(I,(MPW%XMAX+MPW%XMIN)/2.0D0,(MPW%YMAX+MPW%YMIN)/2.0D0,0)
   CALL IDFPLOTFAST(1)
   CALL MANAGER_UTL_MENUFIELDS(I,0,1)
   CALL IPFGETVALUE_MENUFIELDS(0)
  CASE(ID_MOVEMAP)
   I=IPFMESSAGE%VALUE1
   CALL MANAGER_UTL_MENUFIELDS(I,1,0)
   CALL IDFMOVE(0)
   CALL MANAGER_UTL_MENUFIELDS(I,0,1)
   CALL IPFGETVALUE_MENUFIELDS(0)
  CASE (ID_TOPOGRAPHY)
   CALL TOPOINIT(); CALL IDFPLOTFAST(0)
  CASE(ID_TOPTRANSPARACY)
   CALL WINDOWSELECT(0)
   IF(WMENUGETSTATE(ID_TOPTRANSPARACY,2).EQ.1)THEN
    CALL WMENUSETSTATE(ID_TOPTRANSPARACY,2,0)
   ELSE
    CALL WMENUSETSTATE(ID_TOPTRANSPARACY,2,1)
   ENDIF
   CALL IDFPLOT(1)
!  CASE (ID_SERIESIMPLE,ID_SERIEADVANCED)
!   CALL IPFGETVALUE_ADJUSTMENU(IPFMESSAGE%VALUE1)
!  CASE (ID_CONTLINES,ID_BLOCKLINES)
!   CALL WMENUSETSTATE(ID_CONTLINES,2,0)
!   CALL WMENUSETSTATE(ID_BLOCKLINES,2,0)
!   CALL WMENUSETSTATE(IPFMESSAGE%VALUE1,2,1)
!   CALL IDFPLOT(1)
!  CASE (ID_MARKDATA)
!   I=WMENUGETSTATE(IPFMESSAGE%VALUE1,2)
!   I=ABS(I-1)
!   CALL WMENUSETSTATE(IPFMESSAGE%VALUE1,2,I)
!   CALL IDFPLOT(1)
  CASE (ID_SERIEQUICKVIEW)
   CALL UTL_HIDESHOWDIALOG(ID_DIPFINFO,0)
   CALL IPFGETVALUE_QUICKVIEW_INIT(0)
   DO
    CALL WMESSAGE(IPFITYPE,IPFMESSAGE)
    CALL IPFGETVALUE_QUICKVIEW(IPFITYPE,IPFMESSAGE,MPW%IWIN,0,IEXIT)
    IF(IEXIT.EQ.1)EXIT
   ENDDO
   CALL UTL_HIDESHOWDIALOG(ID_DIPFINFO,2)
   CALL IPFGETVALUE_RESTORESELECTION()
  CASE (ID_FCUREXT,ID_FENTDOM,ID_FZOOMBOX,ID_FSELECTION)
   I=3
   IF(WMENUGETSTATE(ID_SERIESIMPLE,2).EQ.1)  I=1
   IF(WMENUGETSTATE(ID_SERIEADVANCED,2).EQ.1)I=2
   CALL IPFGETVALUE_ADJIP(IPFMESSAGE%VALUE1,I)
   CALL IPFGETVALUE_FILLGRID()
   ISEL=0
   CALL IDFPLOT(1)
  CASE (ID_DESELECTALL)
   CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to delete entire selection ?','Question')
   IF(WINFODIALOG(4).EQ.1)THEN
    DO IIPF=1,NIPF; IPF(IIPF)%IP=INT(0,1); ENDDO
    CALL IPFGETVALUE_FILLGRID()
    CALL IDFPLOT(1)
   ENDIF
 END SELECT

 END SUBROUTINE IPFGETVALUE_MENUSELECT

 !###======================================================================
 SUBROUTINE IPFGETVALUE_RESTORESELECTION()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,MAXNROW,IOS
 CHARACTER(LEN=20) :: STRING
 
 IF(.NOT.ALLOCATED(IPF))RETURN

 DO I=1,SIZE(IPF) 
  CALL WDIALOGSELECT(IDTAB(I))
  MAXNROW=WINFOGRID(IDF_GRID1,GRIDROWSCUR)
  DO J=1,MAXNROW
   CALL WGRIDGETROWLABEL(IDF_GRID1,J,STRING)
   IF(LEN_TRIM(STRING).NE.0)THEN
    READ(STRING,*,IOSTAT=IOS) K
    IF(IOS.EQ.0)IPF(I)%IP(K)=INT(3,1)
   ENDIF
  ENDDO
 ENDDO
 
 END SUBROUTINE IPFGETVALUE_RESTORESELECTION

 !###======================================================================
 SUBROUTINE IPFGETVALUE_QUICKVIEW_INIT(IPROF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPROF
 INTEGER :: N,I
 
 !## select all "opened associated files" to be analysed
 N=0
 DO I=1,NIPF
  IF(IPF(I)%ACOL.NE.0)N=N+1
 ENDDO
 IF(N.EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No associated files found','Error')
  RETURN
 ENDIF

 !## copy GRAPHUNITS(1,1) since that will be overwritten in IPFPLOTASSFILE()
 IF(IPROF.EQ.1)THEN
  GXMIN=GRAPHUNITS(1,1)
  GXMAX=GRAPHUNITS(3,1)
  GYMIN=GRAPHUNITS(2,1)
  GYMAX=GRAPHUNITS(4,1)
 ENDIF

 CALL IPFANALYSE_INIT()
 CALL IPFANALYSE_GETLIST()
 IQUICK=1; JIPF=0; ISEL=0  

 END SUBROUTINE IPFGETVALUE_QUICKVIEW_INIT

 !###======================================================================
 SUBROUTINE IPFGETVALUE_QUICKVIEW(IPFITYPE,IPFMESSAGE,IWINXY,IPROF,IEXIT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IEXIT
 INTEGER,INTENT(IN) :: IPFITYPE
 TYPE(WIN_MESSAGE),INTENT(INOUT) :: IPFMESSAGE
 INTEGER,INTENT(IN) :: IWINXY,IPROF
 INTEGER :: I,J,ICLOSE 
 REAL(KIND=DP_KIND) :: MOUSEX,MOUSEY
 
 IEXIT=0  

 !## dialog and window connected to
 IF(IPFMESSAGE%WIN.EQ.ID_DIPFINFOSERIE.OR. &
    IPFMESSAGE%WIN.EQ.ID_DIPFINFOSERIEGRID.OR. &
    IPFMESSAGE%WIN.EQ.IWIN)THEN
   
  IEXIT=-1
   
  CALL WDIALOGSELECT(ID_DIPFINFOSERIE)
  CALL IGRSELECT(DRAWFIELD,IDF_PICTURE1)
  CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0)
  CALL DBL_IGRUNITS(0.0D0,0.0D0,1.0D0,1.0D0)

  CALL IPFANALYSE_DIALOG(IPFITYPE,IPFMESSAGE,ICLOSE,IPROF)
  IF(ICLOSE.EQ.1)THEN  
   CALL IPFGETVALUE_QUICKVIEW_CLOSE(IPROF); IEXIT=1; IQUICK=0
  ENDIF
   
 !## graphical area
 ELSEIF(IPFMESSAGE%WIN.EQ.IWINXY)THEN

  SELECT CASE (IPFITYPE)

   CASE (MOUSEMOVE)

    CALL WCURSORSHAPE(CURARROW)

    IF(IQUICK.EQ.1)THEN

     IEXIT=-1

     I=JIPF
     J=ISEL
     IF(IPROF.EQ.0)THEN
      MOUSEX=DBLE(IPFMESSAGE%GX)+OFFSETX
      MOUSEY=DBLE(IPFMESSAGE%GY)+OFFSETY
     ELSE
      MOUSEX=DBLE(IPFMESSAGE%GX)
      MOUSEY=DBLE(IPFMESSAGE%GY)
     ENDIF
     CALL IPFGETVALUE_MOUSEMOVE(MOUSEX,MOUSEY,IPROF,ID_DIPFINFO)

     !## at least one selected
     IF(JIPF.NE.0.AND.ISEL.NE.0)THEN
      
      IF(JIPF.NE.I.OR.ISEL.NE.J)THEN
       IPF(JIPF)%IP(ISEL)=INT(3,1)
       !## get list of plots
       CALL IPFANALYSE_GETLIST()
       !## initialize plot
       CALL IPFANALYSE_PLOTINIT()
       !## plot figures
       CALL IPFANALYSE_PLOT(0,0,IPROF)
       !## fill grid if one selected
       CALL IPFANALYSE_FILLGRID() 
       IPF(JIPF)%IP(ISEL)=INT(0,1)
      ELSE
       !## look for correct location in grid, to highlight
       IF(ALLOCATED(ASSF))THEN
        SELECT CASE (ASSF(1)%ITOPIC)
         !## time-series
         CASE (1)
          I=1
         !## boreholes
         CASE (2)
          DO I=1,ASSF(1)%NRASS-1
           IF(ASSF(1)%Z(I).GE.IPFMESSAGE%GY.AND.ASSF(1)%Z(I+1).LE.IPFMESSAGE%GY)EXIT         
          ENDDO
         !## logs
         CASE (3)
          DO I=1,ASSF(1)%NRASS-1
           IF(ASSF(1)%MEASURE(1,I).GE.IPFMESSAGE%GY.AND.ASSF(1)%MEASURE(1,I+1).LE.IPFMESSAGE%GY)EXIT         
          ENDDO        
        END SELECT
        CALL WDIALOGSELECT(ID_DIPFINFOSERIEGRID)
        IF(I.GT.0.AND.I.LT.ASSF(1)%NRASS)CALL WGRIDSETCELL(IDF_GRID1,1,I)
       ENDIF
      ENDIF

     ENDIF 

     IF(IPROF.EQ.1)THEN
      GRAPHUNITS(1,1)=GXMIN
      GRAPHUNITS(3,1)=GXMAX
      GRAPHUNITS(2,1)=GYMIN
      GRAPHUNITS(4,1)=GYMAX
      GRAPHAREA(1,1)=0.0D0 !## xmin
      GRAPHAREA(2,1)=0.0D0 !## ymin
      GRAPHAREA(3,1)=1.0D0 !## xmax
      GRAPHAREA(4,1)=1.0D0 !## ymax
      CALL PROFILE_EXTENT_GRAPH(1)    
     ENDIF

    ENDIF
   CASE (MOUSEBUTDOWN)
    SELECT CASE (IPFMESSAGE%VALUE1)
     CASE (1)
      IQUICK=ABS(IQUICK-1)
      IF(IQUICK.EQ.0)THEN
       CALL WDIALOGSELECT(ID_DIPFINFOSERIE)
       CALL IGRSELECT(DRAWFIELD,IDF_PICTURE1)
       CALL DBL_IGRAREA( 0.0D0,0.0D0,1.0D0,1.0D0)
       CALL DBL_IGRUNITS(0.0D0,0.0D0,1.0D0,1.0D0)
      ELSE
       IF(IPROF.EQ.0)THEN
        CALL IGRSELECT(DRAWBITMAP,MPW%IBITMAP)
        CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0)
        CALL DBL_IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX,IOFFSET=1)
       ENDIF
      ENDIF

    END SELECT
    
  END SELECT

 ENDIF

 END SUBROUTINE IPFGETVALUE_QUICKVIEW

 !###====================================================================
 SUBROUTINE IPFGETVALUE_QUICKVIEW_CLOSE(IPROF)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPROF
 
 IF(IPROF.EQ.1)CALL PROFILE_EXTENT_GRAPH(1)

 !## deallocate memory associated files
 CALL IPFANALYSE_CLOSE()

 END SUBROUTINE IPFGETVALUE_QUICKVIEW_CLOSE
 
 !###====================================================================
 SUBROUTINE IPFGETVALUE_ADJUSTMENU(ID)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 INTEGER :: IIPF,I,IP

 !## turn them all off
 CALL WMENUSETSTATE(ID_SERIESIMPLE,2,0)
 CALL WMENUSETSTATE(ID_SERIEADVANCED,2,0)

 !## turn selected menu-item off
 IF(ID.EQ.0)THEN 
  IP=3
  I =0
 ELSE
  !## set selected menu-item
  CALL WMENUSETSTATE(ID,2,1)

  !## adjust ip-values
  SELECT CASE (ID)
   CASE (ID_SERIESIMPLE)
    IP=1
   CASE (ID_SERIEADVANCED)
    IP=2
  END SELECT
  I=1

 ENDIF

 CALL WMENUSETSTATE(ID_CONTLINES,1,I)
 CALL WMENUSETSTATE(ID_BLOCKLINES,1,I)
 CALL WMENUSETSTATE(ID_MARKDATA,1,I)

 DO IIPF=1,NIPF
  DO I=1,IPF(IIPF)%NROW
   IF(IPF(IIPF)%IP(I).NE.INT(0,1))IPF(IIPF)%IP(I)=INT(IP,1)
  END DO
 ENDDO

 END SUBROUTINE IPFGETVALUE_ADJUSTMENU

 !###======================================================================
 SUBROUTINE IPFGETVALUE_ADJIP(ID,IDMENU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID,IDMENU
 INTEGER :: I,IIPF

 SELECT CASE (ID)

  !## select
  CASE (ID_FSELECTION)
   CALL IPFGETVALUE_SELECTLOGICAL(IDMENU)
  !## add/delete current position
  CASE (1)!## left-mouse-butdown
   IF(ISEL.GT.0)THEN
    IF(IPF(JIPF)%IP(ISEL).EQ.IDMENU)THEN
     IPF(JIPF)%IP(ISEL)=INT(0,1)
    ELSE
     IPF(JIPF)%IP(ISEL)=INT(IDMENU,1)
    ENDIF
   ENDIF
  !## current extent
  CASE (ID_FCUREXT)
   DO IIPF=1,NIPF
    DO I=1,IPF(IIPF)%NROW
     IF(MPW%XMIN.LE.IPF(IIPF)%XYZ(1,I).AND.MPW%XMAX.GE.IPF(IIPF)%XYZ(1,I).AND. &
        MPW%YMIN.LE.IPF(IIPF)%XYZ(2,I).AND.MPW%YMAX.GE.IPF(IIPF)%XYZ(2,I))IPF(IIPF)%IP(I)=INT(IDMENU,1)
    END DO
   END DO
  !## entire domain
  CASE (ID_FENTDOM)
   DO IIPF=1,NIPF
    DO I=1,IPF(IIPF)%NROW
     IPF(IIPF)%IP(I)=INT(IDMENU,1)
    END DO
   ENDDO
  !## zoombox
  CASE (ID_FZOOMBOX)
   CALL IPFGETVALUE_SELECTBOX(IDMENU)

 END SELECT

 END SUBROUTINE IPFGETVALUE_ADJIP

 !###======================================================================
 SUBROUTINE IPFGETVALUE_SELECTLOGICAL(IDMENU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDMENU
 INTEGER :: I,IIPF,IATTRIB,IOPTION,ICASE,IOS,IOP,IRAT,IRAT1,IEXT
 REAL(KIND=DP_KIND) :: X,Y
 CHARACTER(LEN=50) :: STRING
 LOGICAL :: LEX

 CALL WDIALOGSELECT(ID_DIPFINFOFIND)
 CALL WDIALOGSHOW(0,0,0,2)

! CALL WDIALOGPUTMENU(IDF_MENU1,IPF%ALIAS,NIPF,1)
! CALL IPFGETVALUE_SELECTLOGICALFIELDS()

 DO
  CALL WMESSAGE(IPFITYPE,IPFMESSAGE)
  SELECT CASE (IPFITYPE)

   CASE (FIELDCHANGED)
    SELECT CASE (IPFMESSAGE%VALUE2)
     CASE (IDF_MENU1)
      CALL WDIALOGGETMENU(IDF_MENU1,I)
      CALL WDIALOGPUTMENU(IDF_MENU2,IPF(I)%ATTRIB,IPF(I)%NCOL,1)
     CASE (IDF_RADIO1,IDF_RADIO2,IDF_CHECK2,IDF_CHECK3)
      CALL IPFGETVALUE_SELECTLOGICALFIELDS()
    END SELECT
   CASE (PUSHBUTTON)
    SELECT CASE (IPFMESSAGE%VALUE1)
     CASE (IDHELP)
       CALL UTL_GETHELP('4.2.5','MMO.IPO.IPFFin') !ID 1040
     CASE (IDCANCEL)
      EXIT
     CASE (IDOK)
      CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,IOPTION)
      CALL WDIALOGGETCHECKBOX(IDF_CHECK1,ICASE)
      CALL WDIALOGGETCHECKBOX(IDF_CHECK2,IEXT)
      CALL WDIALOGGETMENU(IDF_MENU1,IIPF)
      CALL WDIALOGGETMENU(IDF_MENU2,IATTRIB)
      CALL WDIALOGGETMENU(IDF_MENU3,IOP)
      CALL WDIALOGGETDOUBLE(IDF_REAL1,Y)
      CALL WDIALOGGETSTRING(IDF_STRING1,STRING)
      EXIT
    END SELECT
  END SELECT

 END DO

 CALL WDIALOGHIDE()!UNLOAD()
 IF(IPFMESSAGE%VALUE1.EQ.IDCANCEL)RETURN

 CALL UTL_MESSAGEHANDLE(0)

 !## select logical expression
 IRAT1=0

 IF(IOPTION.EQ.1)THEN

  DO I=1,IPF(IIPF)%NROW
   !## read as real ...
   READ(IPF(IIPF)%INFO(IATTRIB,I),*,IOSTAT=IOS) X
   IF(IOS.EQ.0)THEN
    LEX=.FALSE.
    !## current window extent
    IF(IEXT.EQ.1)THEN
     IF(MPW%XMIN.LE.IPF(IIPF)%XYZ(1,I).AND.MPW%XMAX.GE.IPF(IIPF)%XYZ(1,I).AND. &
        MPW%YMIN.LE.IPF(IIPF)%XYZ(2,I).AND.MPW%YMAX.GE.IPF(IIPF)%XYZ(2,I))LEX=.TRUE.
    ELSE
     LEX=.TRUE.
    ENDIF
!    WRITE(*,*) 1,LEX,X,Y,IOP
    IF(LEX)THEN
     SELECT CASE (IOP)
      CASE (1)  !## <
       LEX=X.LT.Y
      CASE (2)  !## <=
       LEX=X.LE.Y
      CASE (3)  !## =
       LEX=X.EQ.Y
      CASE (4)  !## >=
       LEX=X.GE.Y
      CASE (5)  !## >
       LEX=X.GT.Y
      CASE (6)  !## /=
       LEX=X.NE.Y
      CASE DEFAULT
       WRITE(*,*) 'may not come here!!!'
     END SELECT
!     WRITE(*,*) 2,LEX
     IF(LEX)IPF(IIPF)%IP(I)=INT(IDMENU,1)
    ENDIF
   ENDIF
   CALL UTL_WAITMESSAGE(IRAT,IRAT1,I,IPF(IIPF)%NROW,'Getting selection ... ')
  END DO

 !## select character string expression
 ELSE
  DO I=1,IPF(IIPF)%NROW
   LEX=.FALSE.
   IF(IEXT.EQ.1)THEN
    IF(MPW%XMIN.LE.IPF(IIPF)%XYZ(1,I).AND.MPW%XMAX.GE.IPF(IIPF)%XYZ(1,I).AND. &
       MPW%YMIN.LE.IPF(IIPF)%XYZ(2,I).AND.MPW%YMAX.GE.IPF(IIPF)%XYZ(2,I))LEX=.TRUE.
   ELSE
    LEX=.TRUE.
   ENDIF
   IF(LEX)THEN
    LEX=UTL_EQUALNAMES(TRIM(STRING),TRIM(IPF(IIPF)%INFO(IATTRIB,I)),ICAP=ICASE)
!    IF(ICASE.EQ.0)THEN
!     LEX=UTL_EQUALNAMES(TRIM(UTL_CAP(STRING,'U')),TRIM(UTL_CAP(IPF(IIPF)%INFO(IATTRIB,I),'U')))
!    ELSEIF(ICASE.EQ.1)THEN
!     LEX=UTL_EQUALNAMES(TRIM(STRING),TRIM(IPF(IIPF)%INFO(IATTRIB,I)))
!    ENDIF
    IF(LEX)IPF(IIPF)%IP(I)=INT(IDMENU,1)
   ENDIF
   CALL UTL_WAITMESSAGE(IRAT,IRAT1,I,IPF(IIPF)%NROW,'Getting selection ... ')
  END DO

 ENDIF

 CALL UTL_MESSAGEHANDLE(1)

 END SUBROUTINE IPFGETVALUE_SELECTLOGICAL

 !###======================================================================
 SUBROUTINE IPFGETVALUE_SELECTLOGICALFIELDS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,I)
 IF(I.EQ.1)THEN
  CALL WDIALOGFIELDSTATE(IDF_MENU3,1)
  CALL WDIALOGFIELDSTATE(IDF_REAL1,1)
  CALL WDIALOGFIELDSTATE(IDF_LABEL1,0)
  CALL WDIALOGFIELDSTATE(IDF_STRING1,0)
  CALL WDIALOGFIELDSTATE(IDF_LABEL2,0)
  CALL WDIALOGFIELDSTATE(IDF_CHECK1,0)
 ELSE
  CALL WDIALOGFIELDSTATE(IDF_MENU3,0)
  CALL WDIALOGFIELDSTATE(IDF_REAL1,0)
  CALL WDIALOGFIELDSTATE(IDF_LABEL1,1)
  CALL WDIALOGFIELDSTATE(IDF_STRING1,1)
  CALL WDIALOGFIELDSTATE(IDF_LABEL2,1)
  CALL WDIALOGFIELDSTATE(IDF_CHECK1,1)
 ENDIF

 END SUBROUTINE IPFGETVALUE_SELECTLOGICALFIELDS

 !###======================================================================
 SUBROUTINE IPFGETVALUE_SELECTBOX(IDMENU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDMENU
 INTEGER,PARAMETER :: MAXPOL=50
 INTEGER :: I,NPOL,IIPF
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: XPOL,YPOL

 ALLOCATE(XPOL(MAXPOL),YPOL(MAXPOL))
 IF(UTL_DRAWPOLYGON(XPOL,YPOL,MAXPOL,NPOL))THEN
  DO IIPF=1,NIPF
   DO I=1,IPF(IIPF)%NROW
    IF(MPW%XMIN.LE.IPF(IIPF)%XYZ(1,I).AND.MPW%XMAX.GE.IPF(IIPF)%XYZ(1,I).AND. &
       MPW%YMIN.LE.IPF(IIPF)%XYZ(2,I).AND.MPW%YMAX.GE.IPF(IIPF)%XYZ(2,I))THEN
     IF(DBL_IGRINSIDEPOLYGON(IPF(IIPF)%XYZ(1,I),IPF(IIPF)%XYZ(2,I),XPOL,YPOL,NPOL).EQ.1)IPF(IIPF)%IP(I)=INT(IDMENU,1)
    ENDIF
   END DO
  ENDDO
 ENDIF
 DEALLOCATE(XPOL,YPOL)

 END SUBROUTINE IPFGETVALUE_SELECTBOX

 !###======================================================================
 SUBROUTINE IPFGETVALUE_FILLGRID()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,ICOL,MAXNROW,IIPF

 K=0
 DO IIPF=1,NIPF

  CALL WDIALOGSELECT(IDTAB(IIPF))

  !## fill dialog with information
  MAXNROW=WINFOGRID(IDF_GRID1,GRIDROWSMAX)
  CALL WGRIDROWS(IDF_GRID1,MAXNROW)

  J=SSEL
  DO I=1,IPF(IIPF)%NROW
   IF(IPF(IIPF)%IP(I).GT.ABS(INT(0,1)))THEN
    J=J+1
    IF(J.LE.MAXNROW)THEN
     CALL WGRIDLABELROW(IDF_GRID1,J,TRIM(ITOS(I)))
     DO ICOL=1,IPF(IIPF)%NCOL
      CALL WGRIDPUTCELLSTRING(IDF_GRID1,ICOL,J,IPF(IIPF)%INFO(ICOL,I))
     END DO
    ELSE
     !## turn part of selection off, too many!
     IPF(IIPF)%IP(I)=INT(0,1)
    ENDIF
   ENDIF
  END DO
  IF(J.EQ.0)THEN
   CALL WGRIDROWS(IDF_GRID1,1)
   CALL WGRIDLABELROW(IDF_GRID1,1,'')
   IF(WINFODIALOGFIELD(IDF_GRID1,FIELDSTATE).NE.2)CALL WDIALOGFIELDSTATE(IDF_GRID1,2)
  ELSE
   IF(WINFODIALOGFIELD(IDF_GRID1,FIELDSTATE).NE.1)CALL WDIALOGFIELDSTATE(IDF_GRID1,1)
   CALL WGRIDROWS(IDF_GRID1,MIN(MAXNROW,J))
  ENDIF
  K=K+J

  IF(J.LE.MAXNROW)THEN
   CALL WDIALOGPUTSTRING(IDF_LABEL1,'Selected: '//TRIM(ITOS(J-SSEL))//' records out of '//TRIM(ITOS(IPF(IIPF)%NROW)))
  ELSE
   CALL WDIALOGPUTSTRING(IDF_LABEL1,'Warning! Selected: '//TRIM(ITOS(J-SSEL))//' records out of '//TRIM(ITOS(IPF(IIPF)%NROW))// &
             ', showed only '//TRIM(ITOS(MAXNROW))//' records')
  ENDIF
 END DO

 I=0
 IF(K.GT.0)I=1
 CALL WDIALOGSELECT(ID_DIPFINFO_TAB1)
 IF(WINFODIALOGFIELD(ID_SERIES,FIELDSTATE).NE.I)CALL WDIALOGFIELDSTATE(ID_SERIES,I)
 IF(WINFODIALOGFIELD(ID_DELETE,FIELDSTATE).NE.I)CALL WDIALOGFIELDSTATE(ID_DELETE,I)

 END SUBROUTINE IPFGETVALUE_FILLGRID

END MODULE MOD_IPFGETVALUE
