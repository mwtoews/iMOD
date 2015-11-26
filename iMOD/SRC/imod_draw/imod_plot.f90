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

!     Last change:  PTM  21 Mar 2011   11:34 am
!====================================================================
SUBROUTINE IDFMOVE(IWIN_ID)
!====================================================================
USE WINTERACTER
USE RESOURCE
USE IMODVAR
USE MODPLOT
USE IMOD
IMPLICIT NONE
INTEGER,INTENT(IN) :: IWIN_ID
TYPE(WIN_MESSAGE) :: MESSAGE
INTEGER :: ITYPE,IDCURSOR

!## hide dmanager
IF(WMENUGETSTATE(ID_MANAGER,2).EQ.1) THEN
 CALL WDIALOGSELECT(ID_DMANAGER)
 CALL WDIALOGHIDE()
END IF

CALL IDFMOVEINIT(0,IWIN_ID)

IDCURSOR=ID_CURSORHAND
CALL WCURSORSHAPE(IDCURSOR)

IDOWN=0
DO WHILE(.TRUE.)

 CALL WMESSAGE(ITYPE, MESSAGE)

 IF(IWIN_ID.NE.0.AND.MESSAGE%WIN.NE.IWIN_ID)THEN
   
  IF(WINFOMOUSE(MOUSECURSOR).NE.CURHOURGLASS)CALL WCURSORSHAPE(CURHOURGLASS)
  
 ELSE
  
  IF(WINFOMOUSE(MOUSECURSOR).NE.IDCURSOR)CALL WCURSORSHAPE(IDCURSOR)

  SELECT CASE(ITYPE)

   CASE(MOUSEMOVE)
    IF(IDOWN.EQ.1)CALL IDFMOVEIT(MESSAGE%GX,MESSAGE%GY,IWIN_ID)

   CASE (MOUSEBUTUP)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (1)
      !## update moved plot
      CALL IDFMOVEPLOT(IWIN_ID)
      IDCURSOR=ID_CURSORHAND
      CALL WCURSORSHAPE(IDCURSOR)
      CALL WINDOWSELECT(0)
      CALL WINDOWOUTSTATUSBAR(2,'')
      CALL WINDOWOUTSTATUSBAR(4,'Click your right-mouse button to leave this move-mode')
      IDOWN=0
    END SELECT

   !## mouse button pressed
   CASE (MOUSEBUTDOWN)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (1)
      IDCURSOR=ID_CURSORHANDGREP
      CALL WCURSORSHAPE(IDCURSOR)
      PX=INT(MESSAGE%GX)
      PY=INT(MESSAGE%GY)
      IDOWN=1
     CASE (3)
      EXIT
    END SELECT

   CASE (BITMAPSCROLLED)
    MPW%IX=MESSAGE%VALUE1
    MPW%IY=MESSAGE%VALUE2

  END SELECT
 
 ENDIF
ENDDO

CALL IDFMOVECLOSE(IWIN_ID)

!## show dmanager
IF(WMENUGETSTATE(ID_MANAGER,2).EQ.1) THEN
 CALL WDIALOGSELECT(ID_DMANAGER)
 CALL WDIALOGSHOW(-0,65,0,2)
ENDIF

RETURN
END SUBROUTINE

!====================================================================
SUBROUTINE IDFMOVEINIT(ITYPE,IWIN_ID)
!====================================================================
USE WINTERACTER
USE RESOURCE
USE MODPLOT
USE IMODVAR
IMPLICIT NONE
INTEGER,INTENT(IN) :: ITYPE,IWIN_ID

IW=WINFOBITMAP(MPW%IBITMAP,BITMAPWIDTH)
IH=WINFOBITMAP(MPW%IBITMAP,BITMAPHEIGHT)

IF(IWIN_ID.EQ.0)THEN
 CALL IGRSELECT(DRAWWIN)
ELSE
 !## moving in profile-dialog
 CALL WDIALOGSELECT(ID_DSERIESTAB1)
 CALL IGRSELECT(DRAWFIELD,IDF_PICTURE1)
ENDIF 

CALL IGRUNITS(0.0,0.0,REAL(IW),REAL(IH))
CALL WBITMAPCREATE(IBITMAP,IW,IH)

CALL WINDOWSELECT(0)
IF(ITYPE.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Click your right-mouse button to leave this move-mode')
IF(ITYPE.EQ.1)CALL WINDOWOUTSTATUSBAR(4,'Release Ctrl-Left Mouse Button to leave this move-mode')

RETURN
END SUBROUTINE

!====================================================================
SUBROUTINE IDFMOVECLOSE(IWIN_ID)
!====================================================================
USE WINTERACTER
USE RESOURCE
USE MODPLOT
USE IMODVAR, ONLY : IBITMAP
USE MOD_PROF_PAR, ONLY : AREA
IMPLICIT NONE
INTEGER,INTENT(IN) :: IWIN_ID

CALL WCURSORSHAPE(CURARROW)
CALL WINDOWOUTSTATUSBAR(1,'')
CALL WINDOWOUTSTATUSBAR(4,'')
IF(IWIN_ID.EQ.0)THEN
 CALL IGRSELECT(DRAWWIN)
! CALL IGRAREA(0.0,0.0,1.0,1.0)
 CALL IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX)
ELSE
 !## moving in profile-dialog
 CALL WDIALOGSELECT(ID_DSERIESTAB1)
 CALL IGRSELECT(DRAWFIELD,IDF_PICTURE1)
 CALL IGRAREA(AREA(1),AREA(2),AREA(3),AREA(4))
! CALL WBITMAPPUT(MPW%IBITMAP,2,1)
 CALL IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX)
ENDIF
CALL WBITMAPDESTROY(IBITMAP)

RETURN
END SUBROUTINE

!====================================================================
SUBROUTINE IDFMOVEIT(GX,GY,IWIN_ID)
!====================================================================
USE WINTERACTER
USE RESOURCE
USE MODPLOT
USE IMODVAR
USE MOD_UTL, ONLY : ITOS
USE MOD_PROFILE_UTL, ONLY : PROFILE_PUTBITMAP
IMPLICIT NONE
INTEGER,INTENT(IN) :: IWIN_ID
REAL,INTENT(IN) :: GX,GY
INTEGER :: IXSOUR1,IYSOUR1,IXSOUR2,IYSOUR2,IXDEST,IYDEST

CALL WINDOWSELECT(0)
CALL WINDOWOUTSTATUSBAR(1,'Pixel X:'//TRIM(ITOS(INT(GX)))//' Pixel Y:'//TRIM(ITOS(INT(GY))))

!##moving bitmap
DX=INT(GX-PX)
DY=INT(PY-GY)
CALL WINDOWOUTSTATUSBAR(2,'DP X:'//TRIM(ITOS(DX))//' DP Y:'//TRIM(ITOS(DY)))
!###shifted to the right
IF(DX.GT.0)THEN
 IXSOUR1=    1    !left
 IXSOUR2=  IW-DX  !right
 IXDEST =   DX    !left
ELSE
 IXSOUR1=-1*DX    !left
 IXSOUR2=   IW    !right
 IXDEST =    1    !left
ENDIF
!###shifted to the top
IF(DY.GT.0)THEN
 IYSOUR1=  IH-DY  !top
 IYSOUR2=    1    !bottom
 IYDEST =    IH   !top
ELSE
 IYSOUR1=   IH    !top
 IYSOUR2=  -1*DY  !bottom
 IYDEST =  IH+DY  !top
ENDIF

CALL IGRSELECT(DRAWBITMAP,IBITMAP)
CALL IGRPLOTMODE(MODECOPY)
CALL IGRAREA(0.0,0.0,1.0,1.0)
CALL IGRUNITS(0.0,0.0,REAL(IW),REAL(IH))
CALL IGRFILLPATTERN(SOLID)
CALL IGRCOLOURN(WRGB(255,255,255))
CALL IGRRECTANGLE(0.0,0.0,REAL(IW),REAL(IH))
CALL WBITMAPPUTPART(MPW%IBITMAP,1,IXSOUR1,IYSOUR1,IXSOUR2,IYSOUR2,IXDEST,IYDEST)

IF(IWIN_ID.EQ.0)THEN
 CALL IGRSELECT(DRAWWIN)
 CALL WINDOWSELECT(MPW%IWIN)
 CALL WBITMAPVIEW(IBITMAP,MPW%IX,MPW%IY,MODELESS) !,KEYSCROLL)
ELSE
 CALL PROFILE_PUTBITMAP(IBITMAP)
ENDIF

CALL IGRAREA(0.0,0.0,1.0,1.0)
CALL IGRUNITS(0.0,0.0,REAL(IW),REAL(IH))

RETURN
END SUBROUTINE

!====================================================================
SUBROUTINE IDFMOVEPLOT(IWIN_ID)
!====================================================================
USE WINTERACTER
USE RESOURCE
USE MODPLOT
USE IMODVAR
USE MOD_PROFILE_UTL, ONLY : PROFILE_PUTBITMAP
USE MOD_3D_SETTINGS, ONLY : IMOD3D_DISPLAY_UPDATE
USE IMOD
IMPLICIT NONE
INTEGER,INTENT(IN) :: IWIN_ID
REAL :: DXC,DYC

!##compute transformation of pixels onto coordinates
DXC=REAL(DX)*(MPW%XMAX-MPW%XMIN)/REAL(IW)
DYC=REAL(-1*DY)*(MPW%YMAX-MPW%YMIN)/REAL(IH)

DXC=-1.0*DXC
DYC=-1.0*DYC

MPW%XMIN=MPW%XMIN+DXC
MPW%XMAX=MPW%XMAX+DXC
MPW%YMIN=MPW%YMIN+DYC
MPW%YMAX=MPW%YMAX+DYC

CALL WBITMAPDESTROY(MPW%IBITMAP)

CALL IDFPLOTFAST(0)

IF(IWIN_ID.EQ.0)THEN
 CALL IGRSELECT(DRAWWIN)
ELSE
 !## moving in profile-dialog
 CALL WDIALOGSELECT(ID_DSERIESTAB1)
 CALL IGRSELECT(DRAWFIELD,IDF_PICTURE1)
 !## put bitmap to field ...
 CALL PROFILE_PUTBITMAP(MPW%IBITMAP)
ENDIF 

CALL IGRUNITS(0.0,0.0,REAL(IW),REAL(IH))
CALL WBITMAPCREATE(IBITMAP,IW,IH)

RETURN
END SUBROUTINE

!====================================================================
SUBROUTINE IDFZOOM(IDZ,GX,GY,IWIN_ID)
!====================================================================
USE WINTERACTER
USE RESOURCE
USE MOD_IDF, ONLY : IDFREAD
USE MOD_MDF, ONLY : READMDF,UTL_GETUNITMDF,MDF,MDFDEALLOCATE
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MODPLOT
USE MOD_GENPLOT_PAR, ONLY : GEN,MXGEN,ACTLISTGEN
USE MOD_UTL, ONLY : UTL_GETUNIT,ITOS,RTOS,IDFPLOT1BITMAP,IDFPLOT2BITMAP
USE MOD_PROFILE_UTL, ONLY : PROFILE_PUTBITMAP
USE MOD_ISG_PLOT, ONLY : ISGPLOTMINMAX
USE MOD_TAGS, ONLY : TAGZOOM
IMPLICIT NONE
REAL,PARAMETER :: FZIN =0.75
REAL,PARAMETER :: FZOUT=1.5
INTEGER,INTENT(IN) :: IDZ,IWIN_ID
TYPE(WIN_MESSAGE) :: MESSAGE
REAL,INTENT(IN) :: GX,GY
INTEGER :: ITYPE,I,IPLOT,IDOWN,IDCURSOR,ITAB,N
REAL :: FZ,XC1,YC1,XC2,YC2,XC3,YC3,XMIN,XMAX,YMIN,YMAX,Y
LOGICAL :: LEX
CHARACTER(LEN=256) :: FNAME

IF(IDZ.EQ.ID_ZOOMINMAP)THEN
 FZ=FZIN
ELSEIF(IDZ.EQ.ID_ZOOMOUTMAP)THEN
 FZ=FZOUT
ELSEIF(IDZ.EQ.ID_ZOOMRECTANGLEMAP)THEN
 IDCURSOR=ID_CURSORZOOMRECTANGLE
 FZ=FZIN
ENDIF

!## full map-view - selected idf's
IF(IDZ.EQ.ID_ZOOMFULLMAP)THEN
 CALL WDIALOGSELECT(ID_DMANAGER)
 CALL WDIALOGGETTAB(ID_DMTAB,ITAB)
 I=0
 IF(ITAB.EQ.ID_DMANAGERTAB1)THEN
  !## initialize variables
  XMIN=0.0
  XMAX=0.0
  YMIN=0.0
  YMAX=0.0
  DO IPLOT=1,MXMPLOT
   IF(ACTLIST(IPLOT).EQ.1)THEN
    SELECT CASE (MP(IPLOT)%IPLOT)
     !## idf,mdf
     CASE (1,5) !## IF(MP(IPLOT)%IPLOT.EQ.1.OR.5)THEN
      !## get idf or mdf file
      LEX=.TRUE.
      IF(MP(IPLOT)%IPLOT.EQ.5)THEN
       FNAME=MP(IPLOT)%IDFNAME
       !## read *.mdf file, only to get selected idf to be plotted
       LEX=READMDF(MP(IPLOT)%IDFNAME,N)
       MP(IPLOT)%IDFNAME=MDF(MP(IPLOT)%NLIDF)%FNAME
       CALL MDFDEALLOCATE()
      ENDIF
      IF(LEX)THEN
       IF(IDFREAD(MP(IPLOT)%IDF,MP(IPLOT)%IDFNAME,0))THEN
        CLOSE(MP(IPLOT)%IDF%IU)
        IF(I.EQ.0)THEN
         XMIN=MP(IPLOT)%IDF%XMIN
         XMAX=MP(IPLOT)%IDF%XMAX
         YMIN=MP(IPLOT)%IDF%YMIN
         YMAX=MP(IPLOT)%IDF%YMAX
        ELSE
         XMIN=MIN(XMIN,MP(IPLOT)%IDF%XMIN)
         YMIN=MIN(YMIN,MP(IPLOT)%IDF%YMIN)
         XMAX=MAX(XMAX,MP(IPLOT)%IDF%XMAX)
         YMAX=MAX(YMAX,MP(IPLOT)%IDF%YMAX)
        ENDIF
        I=I+1
       ENDIF
      ENDIF
      IF(MP(IPLOT)%IPLOT.EQ.5)MP(IPLOT)%IDFNAME=FNAME
     !## ipf/iff
     CASE (2,3,6) !## ELSEIF(MP(IPLOT)%IPLOT.EQ.2.OR.MP(IPLOT)%IPLOT.EQ.3)THEN
      IF(I.EQ.0)THEN
       XMIN=MP(IPLOT)%XMIN
       XMAX=MP(IPLOT)%XMAX
       YMIN=MP(IPLOT)%YMIN
       YMAX=MP(IPLOT)%YMAX
      ELSE
       IF(MP(IPLOT)%XMIN.NE.0.0.OR.MP(IPLOT)%XMAX.NE.0.0.OR. &
          MP(IPLOT)%YMIN.NE.0.0.OR.MP(IPLOT)%YMAX.NE.0.0)THEN
        XMIN=MIN(XMIN,MP(IPLOT)%XMIN)
        YMIN=MIN(YMIN,MP(IPLOT)%YMIN)
        XMAX=MAX(XMAX,MP(IPLOT)%XMAX)
        YMAX=MAX(YMAX,MP(IPLOT)%YMAX)
       ENDIF
      ENDIF
      I=I+1
     !## isg
     CASE (4) !## ELSEIF(MP(IPLOT)%IPLOT.EQ.4)THEN
      I=I+1
      CALL ISGPLOTMINMAX(MP(IPLOT)%IDFNAME,XMIN,XMAX,YMIN,YMAX)
    END SELECT
   ENDIF
  ENDDO
  IF(I.GT.0)THEN
   MPW%XMIN=XMIN
   MPW%YMIN=YMIN
   MPW%XMAX=XMAX
   MPW%YMAX=YMAX
  ENDIF
 ELSEIF(ITAB.EQ.ID_DMANAGERTAB2)THEN
  DO IPLOT=1,MXGEN
   IF(ACTLISTGEN(IPLOT).EQ.1)THEN
    IF(I.EQ.0)THEN
     I=I+1
     MPW%XMIN=GEN(IPLOT)%XMIN
     MPW%YMIN=GEN(IPLOT)%YMIN
     MPW%XMAX=GEN(IPLOT)%XMAX
     MPW%YMAX=GEN(IPLOT)%YMAX
    ELSE
     MPW%XMIN=MIN(MPW%XMIN,GEN(IPLOT)%XMIN)
     MPW%YMIN=MIN(MPW%YMIN,GEN(IPLOT)%YMIN)
     MPW%XMAX=MAX(MPW%XMAX,GEN(IPLOT)%XMAX)
     MPW%YMAX=MAX(MPW%YMAX,GEN(IPLOT)%YMAX)
    ENDIF
   ENDIF
  ENDDO
 !## tag zoom
 ELSEIF(ITAB.EQ.ID_DMANAGERTAB3)THEN
  CALL TAGZOOM()
 ENDIF

 IF(MPW%XMAX-MPW%XMIN.LE.0.0)THEN
  MPW%XMAX=MPW%XMAX+1.0
  MPW%XMIN=MPW%XMIN-1.0
 ENDIF
 IF(MPW%YMAX-MPW%YMIN.LE.0.0)THEN
  MPW%YMAX=MPW%YMAX+1.0
  MPW%YMIN=MPW%YMIN-1.0
 ENDIF
 !## increase window to count for y-size!
 Y       =(MPW%YMAX-MPW%YMIN)/2.0
 Y       = Y/2.0
 MPW%YMAX= MPW%YMAX+Y
 MPW%YMIN= MPW%YMIN-Y

!## zoom tags
ELSEIF(IDZ.EQ.ID_ZOOMTAG)THEN

 CALL TAGZOOM()

!## interactive zooming
ELSEIF(IDZ.EQ.ID_ZOOMINMAP.OR.IDZ.EQ.ID_ZOOMOUTMAP)THEN
 XC2=GX
 YC2=GY
 CALL IDFADJUSTMAP(IDZ,XC1,YC1,XC2,YC2,FZ)

!## interactive zooming
ELSEIF(IDZ.EQ.ID_ZOOMRECTANGLEMAP)THEN

!## rectangle zoom
 CALL IGRPLOTMODE(MODEXOR)
 CALL IGRCOLOURN(WRGB(255,255,255))
 CALL IGRFILLPATTERN(OUTLINE)
 CALL IGRLINETYPE(DASHED)
 CALL IGRLINEWIDTH(1)
 CALL WCURSORSHAPE(IDCURSOR)

 IDOWN=0
 LEX  =.FALSE.
 XC1  =0.0
 YC1  =0.0
 DO WHILE(.TRUE.)

  CALL WMESSAGE(ITYPE, MESSAGE)

  IF(IWIN_ID.NE.0.AND.MESSAGE%WIN.NE.IWIN_ID)THEN
   
   IF(WINFOMOUSE(MOUSECURSOR).NE.CURHOURGLASS)CALL WCURSORSHAPE(CURHOURGLASS)
  
  ELSE
  
   IF(WINFOMOUSE(MOUSECURSOR).NE.IDCURSOR)CALL WCURSORSHAPE(IDCURSOR)
   
   SELECT CASE(ITYPE)
 
    CASE(MOUSEMOVE)

     CALL WINDOWSELECT(0)
     CALL WINDOWOUTSTATUSBAR(1,'X:'//TRIM(ITOS(INT(MESSAGE%GX)))//' m, Y:'//TRIM(ITOS(INT(MESSAGE%GY)))//' m')
     XC2=MESSAGE%GX
     YC2=MESSAGE%GY

     IF(IDOWN.EQ.1)CALL WINDOWOUTSTATUSBAR(2,'Dx:'//TRIM(ITOS(INT(XC2-XC1)))//' m, Dy:'//TRIM(ITOS(INT(YC2-YC1)))//' m')

     !##first point set!
     IF(IDOWN.EQ.1)THEN
      CALL IDFPLOT1BITMAP()
      IF(LEX)CALL IGRRECTANGLE(XC1,YC1,XC3,YC3)
      LEX=.FALSE.
      IF(XC1.NE.XC2.AND.YC1.NE.YC2)LEX=.TRUE.
      IF(LEX)CALL IGRRECTANGLE(XC1,YC1,XC2,YC2)
      CALL IDFPLOT2BITMAP()
      !## if profiel zoomrectangle, put bitmap to that dialog too.
      IF(IWIN_ID.NE.0)CALL PROFILE_PUTBITMAP(MPW%IBITMAP)
     ENDIF

     XC3=XC2
     YC3=YC2

    !## mouse button pressed
    CASE (MOUSEBUTDOWN)
     SELECT CASE (MESSAGE%VALUE1)

      !## left button
      CASE (1)
       IF(IDOWN.EQ.0)THEN
        XC1=XC2
        YC1=YC2
        IDOWN=1
       ELSE
        CALL IDFADJUSTMAP(IDZ,XC1,YC1,XC2,YC2,FZ)
        CALL IGRLINETYPE(SOLIDLINE)
        EXIT
       ENDIF

      !## right button
      CASE (3)
       IF(IDOWN.EQ.1)THEN
        CALL IDFPLOT1BITMAP()
        IF(LEX)CALL IGRRECTANGLE(XC1,YC1,XC3,YC3)
        CALL IDFPLOT2BITMAP()
        !## if profiel zoomrectangle, put bitmap to that dialog too.
        IF(IWIN_ID.NE.0)CALL PROFILE_PUTBITMAP(MPW%IBITMAP)
       ENDIF
       EXIT
     END SELECT

    CASE (BITMAPSCROLLED)
     MPW%IX=MESSAGE%VALUE1
     MPW%IY=MESSAGE%VALUE2

   END SELECT
  
  ENDIF
 ENDDO

 CALL WCURSORSHAPE(CURARROW)
 CALL IGRPLOTMODE(MODECOPY)
 CALL IGRLINETYPE(SOLIDLINE)

!## zoom to previous extent
ELSEIF(IDZ.EQ.ID_ZOOMPREVIOUS)THEN
 ZM%IZOOM=ZM%IZOOM-1
 MPW%XMIN=ZM%ZOOMXY(ZM%IZOOM,1)
 MPW%YMIN=ZM%ZOOMXY(ZM%IZOOM,2)
 MPW%XMAX=ZM%ZOOMXY(ZM%IZOOM,3)
 MPW%YMAX=ZM%ZOOMXY(ZM%IZOOM,4)
!## zoom to next extent
ELSEIF(IDZ.EQ.ID_ZOOMNEXT)THEN
 ZM%IZOOM=ZM%IZOOM+1
 MPW%XMIN=ZM%ZOOMXY(ZM%IZOOM,1)
 MPW%YMIN=ZM%ZOOMXY(ZM%IZOOM,2)
 MPW%XMAX=ZM%ZOOMXY(ZM%IZOOM,3)
 MPW%YMAX=ZM%ZOOMXY(ZM%IZOOM,4)
ENDIF

CALL WINDOWSELECT(0)
CALL WINDOWOUTSTATUSBAR(2,'')

!## store current zoom-extent
IF(IDZ.NE.ID_ZOOMPREVIOUS.AND.IDZ.NE.ID_ZOOMNEXT)CALL IDFSTOREZOOMEXTENT()

RETURN
END SUBROUTINE

!==============================================================================
SUBROUTINE IDFSTOREZOOMEXTENT()
!==============================================================================
USE MODPLOT
IMPLICIT NONE
INTEGER :: I,J,N

ZM%IZOOM=ZM%IZOOM+1
ZM%NZOOM=ZM%IZOOM
N=SIZE(ZM%ZOOMXY,1)
IF(ZM%NZOOM.GT.N)THEN
 ALLOCATE(ZM%ZOOMXY_BU(N+10,4))
 DO I=1,N
  DO J=1,4; ZM%ZOOMXY_BU(I,J)=ZM%ZOOMXY(I,J); ENDDO
 ENDDO
 IF(ASSOCIATED(ZM%ZOOMXY))DEALLOCATE(ZM%ZOOMXY)
 ZM%ZOOMXY=>ZM%ZOOMXY_BU
ENDIF
ZM%ZOOMXY(ZM%NZOOM,1)=MPW%XMIN
ZM%ZOOMXY(ZM%NZOOM,2)=MPW%YMIN
ZM%ZOOMXY(ZM%NZOOM,3)=MPW%XMAX
ZM%ZOOMXY(ZM%NZOOM,4)=MPW%YMAX

END SUBROUTINE IDFSTOREZOOMEXTENT

!==============================================================================
SUBROUTINE IDFADJUSTMAP(IDZ,XC1,YC1,XC2,YC2,FZ)
!==============================================================================
USE RESOURCE
USE MODPLOT
USE IMOD
IMPLICIT NONE
INTEGER,INTENT(IN) :: IDZ
REAL,INTENT(IN) :: XC1,XC2,YC1,YC2,FZ
REAL :: DX,DY

SELECT CASE (IDZ)
 CASE (ID_ZOOMINMAP,ID_ZOOMOUTMAP)
  DX=(MPW%XMAX-MPW%XMIN)*FZ
  DY=(MPW%YMAX-MPW%YMIN)*FZ
  MPW%XMIN=XC2-0.5*DX
  MPW%XMAX=XC2+0.5*DX
  MPW%YMIN=YC2-0.5*DY
  MPW%YMAX=YC2+0.5*DY
 CASE (ID_ZOOMRECTANGLEMAP)
  MPW%XMIN=MIN(XC1,XC2)
  MPW%XMAX=MAX(XC1,XC2)
  MPW%YMIN=MIN(YC1,YC2)
  MPW%YMAX=MAX(YC1,YC2)
END SELECT

RETURN
END SUBROUTINE

!====================================================================
SUBROUTINE IDFMENUFIELDS(ID,K,J)
!====================================================================
USE WINTERACTER
USE RESOURCE
USE MOD_MANAGER, ONLY : MANAGERUPDATE
USE MOD_TOPO, ONLY : TOPO1UPDATEMANAGER 
IMPLICIT NONE
INTEGER,PARAMETER :: MAXID=22
INTEGER,INTENT(IN) :: ID,J,K
INTEGER :: I
INTEGER,DIMENSION(MAXID) :: ID1
DATA (ID1(I),I=1,MAXID) /ID_NEW,ID_OPEN,ID_SAVE,ID_SAVEAS,ID_COPY,         &
                      ID_MANAGER,ID_OPENIDF,ID_ZOOMINMAP,ID_ZOOMOUTMAP, &
                      ID_ZOOMRECTANGLEMAP,ID_ZOOMFULLMAP,ID_MOVEMAP,    &
                      ID_IRDATABASE,ID_IMODINFO,ID_DISTANCE,ID_PROFILE, &
                      ID_TIMESERIES,ID_3DTOOL,ID_TOPOGRAPHY, &
                      ID_MOVIE,ID_ZOOMPREVIOUS,ID_ZOOMNEXT/

CALL WINDOWSELECT(0)

!## (de)activate buttons
DO I=1,MAXID
 IF(ID1(I).NE.ID)CALL WMENUSETSTATE(ID1(I),1,J)
 IF(ID1(I).EQ.ID)CALL WMENUSETSTATE(ID1(I),2,K)
END DO
IF(J.EQ.1)THEN
 CALL MANAGERUPDATE()
 CALL TOPO1UPDATEMANAGER()
ENDIF

RETURN
END SUBROUTINE
