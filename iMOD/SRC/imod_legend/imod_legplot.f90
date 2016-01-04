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
MODULE MOD_LEGPLOT

USE WINTERACTER
USE RESOURCE
USE MODPLOT
USE MOD_LEGPLOT_PAR
USE MOD_UTL, ONLY : RTOS,UTL_SETTEXTSIZE,IDFPLOT1BITMAP,IDFPLOT2BITMAP,UTL_DIST
USE MOD_PROFILE_UTL, ONLY : PROFILE_GETFORMAT  

INTEGER,PARAMETER :: TFONT=FFHELVETICA

CONTAINS

 !###====================================================================
 SUBROUTINE LEGPLOT_MAIN(IDD,IDP,NC)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDD,IDP,NC
 INTEGER :: IPLOT

 DO IPLOT=1,MXMPLOT
  IF(DRWLIST(IPLOT).EQ.1)EXIT
 END DO
 !## nothing to draw
 IF(IPLOT.GT.MXMPLOT)RETURN

 CALL WDIALOGSELECT(IDD)
 CALL IGRSELECT(DRAWFIELD,IDP)
 CALL IGRAREA (0.0,0.0,1.0,1.0)
 CALL IGRUNITS(0.0,0.0,1.0,1.0)
 CALL LEGPLOT_PLOT(MP(IPLOT)%LEG,NC)

 CALL IGRSELECT(DRAWWIN,MPW%IWIN)
 CALL IGRAREA(0.0,0.0,1.0,1.0)
 CALL IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX)

 END SUBROUTINE LEGPLOT_MAIN

 !###====================================================================
 SUBROUTINE LEGPLOT_PLOT_BITMAP(IBLEG,NC,X1,Y1,X2,Y2)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NC
 INTEGER,INTENT(OUT) :: IBLEG
 REAL,INTENT(IN) :: X1,Y1,X2,Y2
 INTEGER,PARAMETER :: IX=500,IY=500
 INTEGER :: IPLOT,DIX,DIY,IWINDOW

 IBLEG=0
 
 !## create legend per idf type
 
 DO IPLOT=1,MXMPLOT
  IF(DRWLIST(IPLOT).EQ.0)CYCLE

  CALL WBITMAPCREATE(IBLEG,IX,IY)
  CALL IGRSELECT(DRAWBITMAP,IBLEG)
  CALL IGRPLOTMODE(MODECOPY)
  CALL IGRAREA (X1,Y1,X2,Y2)
  CALL IGRUNITS(X1,Y1,X2,Y2)
  CALL LEGPLOT_PLOT(MP(IPLOT)%LEG,NC)
 
 ENDDO 
 
 END SUBROUTINE LEGPLOT_PLOT_BITMAP

 !###====================================================================
 SUBROUTINE LEGPLOT_PLOT_SHOW()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: IPLOT,I
 CHARACTER(LEN=50) :: TXT1,TXT2
 
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL)EXIT
 END DO
 IF(IPLOT.GT.MXMPLOT)RETURN

 CALL WDIALOGSELECT(ID_DMANAGERTAB4)

 CALL WDIALOGCLEARFIELD(IDF_GRID1)
 CALL WGRIDROWS(IDF_GRID1,MP(IPLOT)%LEG%NCLR)
 DO I=1,MP(IPLOT)%LEG%NCLR
  CALL WGRIDCOLOURCELL(IDF_GRID1,1,I,MP(IPLOT)%LEG%RGB(I),MP(IPLOT)%LEG%RGB(I))
  IF(MP(IPLOT)%LEG%NCLR.GT.MXCLASS)THEN
   WRITE(TXT1,'('//PROFILE_GETFORMAT(MP(IPLOT)%LEG%CLASS(I))//')')   MP(IPLOT)%LEG%CLASS(I)
   WRITE(TXT2,'('//PROFILE_GETFORMAT(MP(IPLOT)%LEG%CLASS(I-1))//')') MP(IPLOT)%LEG%CLASS(I-1)
   MP(IPLOT)%LEG%LEGTXT(I)='>='//TRIM(ADJUSTL(TXT1))//' - < '//TRIM(ADJUSTL(TXT2))
  ENDIF
  CALL WGRIDPUTCELLSTRING(IDF_GRID1,2,I,MP(IPLOT)%LEG%LEGTXT(I))
 END DO

 END SUBROUTINE LEGPLOT_PLOT_SHOW

 !###======================================================================
 SUBROUTINE LEGPLOT_PLOT_INIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IPLOT

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_PLOTLEGEND,2).EQ.1)THEN
  CALL LEGPLOT_PLOT_CLOSE()
  RETURN
 ENDIF
 CALL WMENUSETSTATE(ID_PLOTLEGEND,2,1)

 DO IPLOT=1,MXMPLOT
  IF(ACTLIST(IPLOT).EQ.1)EXIT
 END DO

 XRELCRD(1)=0.4
 YRELCRD(1)=0.4
 XRELCRD(2)=0.5
 YRELCRD(2)=0.6
 ISHAPECOLOR=WRGB(255,0,0)

 CALL WMENUSETSTATE(ID_LEGENDCOLUMNS,1,1)

 CALL LEGPLOT_PLOTUPDATE(.FALSE.)

 END SUBROUTINE LEGPLOT_PLOT_INIT

 !###======================================================================
 SUBROUTINE LEGPLOT_PLOT_CLOSE()
 !###======================================================================
 IMPLICIT NONE

 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(ID_PLOTLEGEND,2,0)
 CALL WMENUSETSTATE(ID_LEGENDCOLUMNS,1,0)
 CALL IDFPLOT(1)

 END SUBROUTINE LEGPLOT_PLOT_CLOSE

 !###====================================================================
 SUBROUTINE LEGPLOT_PLOTUPDATE(LBITMAP)
 !###====================================================================
 IMPLICIT NONE
 LOGICAL,INTENT(IN) :: LBITMAP
 INTEGER :: IPLOT,NC,IBLEG
 
 CALL WINDOWSELECT(0); IF(WMENUGETSTATE(ID_PLOTLEGEND,2).NE.1)RETURN

 DO IPLOT=1,MXMPLOT
  IF(DRWLIST(IPLOT).EQ.1)EXIT
 END DO
 IF(IPLOT.GT.MXMPLOT)THEN
  CALL LEGPLOT_PLOT_CLOSE()
  RETURN
 ENDIF

 !## refresh labeling
 CALL LEGPLOT_PLOT_SHOW()

 IF(WMENUGETSTATE(ID_LEGENDCOLUMNS1,2).EQ.1)NC=1
 IF(WMENUGETSTATE(ID_LEGENDCOLUMNS2,2).EQ.1)NC=2
 IF(WMENUGETSTATE(ID_LEGENDCOLUMNS3,2).EQ.1)NC=3
 IF(WMENUGETSTATE(ID_LEGENDCOLUMNS4,2).EQ.1)NC=4
 IF(WMENUGETSTATE(ID_LEGENDCOLUMNS5,2).EQ.1)NC=5

 !## create legend in bitmap
 IF(LBITMAP)THEN
  CALL LEGPLOT_PLOT_BITMAP(IBLEG,NC,XRELCRD(1),YRELCRD(1),XRELCRD(2),YRELCRD(2))
  IF(IBLEG.NE.0)THEN
   CALL IGRSELECT(DRAWBITMAP,MPW%IBITMAP)
   CALL IGRPLOTMODE(MODECOPY)
   !## put legend bitmap on mother bitmap
   CALL WBITMAPPUT(IBLEG,2,1)
   !## remove legend bitmap
   CALL WBITMAPDESTROY(IBLEG)
  ENDIF
 ELSE
  CALL IGRSELECT(DRAWBITMAP,MPW%IBITMAP)
  CALL IGRPLOTMODE(MODECOPY)
  CALL IGRAREA( XRELCRD(1),YRELCRD(1),XRELCRD(2),YRELCRD(2))
  CALL IGRUNITS(XRELCRD(1),YRELCRD(1),XRELCRD(2),YRELCRD(2))
  CALL LEGPLOT_PLOT(MP(IPLOT)%LEG,NC)
 ENDIF
 
 CALL IGRSELECT(DRAWWIN,MPW%IWIN)
 CALL WINDOWSELECT(MPW%IWIN)
 CALL WBITMAPVIEW(MPW%IBITMAP,MPW%IX,MPW%IY,MODELESS) !,KEYSCROLL)

 CALL IGRAREA(0.0,0.0,1.0,1.0)
 CALL IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX)

 END SUBROUTINE LEGPLOT_PLOTUPDATE

 !###====================================================================
 SUBROUTINE LEGPLOT_PLOT(LEG,NC,TSIZE)
 !###====================================================================
 IMPLICIT NONE
 REAL,INTENT(IN),OPTIONAL :: TSIZE
 INTEGER,INTENT(IN) :: NC
 TYPE(LEGENDOBJ),INTENT(IN) :: LEG
 INTEGER,PARAMETER :: TFONT=FFHELVETICA
 INTEGER :: IGRY,NY,I,J
 REAL :: CHW,CHH,BOXX,BOXY,DX,DY,X,Y,TDY,TDX
 REAL :: X1,X2,Y1,Y2,AX1,AX2,AY1,AY2,OFFX,RAT

 !## current graphical units
 X1 =INFOGRAPHICS(GRAPHICSUNITMINX)
 X2 =INFOGRAPHICS(GRAPHICSUNITMAXX)
 Y1 =INFOGRAPHICS(GRAPHICSUNITMINY)
 Y2 =INFOGRAPHICS(GRAPHICSUNITMAXY)
 AX1=INFOGRAPHICS(GRAPHICSAREAMINX)
 AX2=INFOGRAPHICS(GRAPHICSAREAMAXX)
 AY1=INFOGRAPHICS(GRAPHICSAREAMINY)
 AY2=INFOGRAPHICS(GRAPHICSAREAMAXY)

 !## initial box-size
 BOXX=0.025
 OFFX=0.005
 
 CALL IGRPLOTMODE(MODECOPY)
 CALL IGRFILLPATTERN(SOLID)
 CALL IGRCOLOURN(WRGB(255,255,255))
 CALL IGRRECTANGLE(X1,Y1,X2,Y2)
 CALL IGRLINEWIDTH(1)
 CALL IGRFILLPATTERN(OUTLINE)
 CALL IGRCOLOURN(WRGB(0,0,0))
 CALL IGRRECTANGLE(X1,Y1,X2,Y2)

 DX=(X2-X1)/50.0
 DY=(Y2-Y1)/50.0
 X1=X1+DX; X2=X2-DX
 Y1=Y1+DY; Y2=Y2-DY
 RAT=REAL(MPW%DIX)/REAL(MPW%DIY)
 
 IGRY=0; IF(LEG%NCLR.LE.MXCLASS)IGRY=1

 !## textsize defined, compute number of columns
 IF(PRESENT(TSIZE))THEN
  !## set textsize
  CALL UTL_SETTEXTSIZE(CHW,CHH,TSIZE)
  !## number of boxes in vertical
  NY=(Y2-Y1)/TSIZE  
 !## define textsize as number of columns are defined
 ELSE
  !## number of boxes in vertical
  NY=LEG%NCLR/NC 
  !## set textsize
  IF(IGRY.EQ.0)THEN
   TDY=(Y2-Y1)/REAL(10)
   CALL UTL_SETTEXTSIZE(CHW,CHH,TDY)
  ELSE
   TDY=(Y2-Y1)/REAL(NY) 
   CALL UTL_SETTEXTSIZE(CHW,CHH,TDY)
  ENDIF
 ENDIF

 !## add extra line for legend-header
 IF(LEN_TRIM(LEG%HEDTXT).GT.0)NY=NY+1

 CALL WGRTEXTORIENTATION(ALIGNLEFT)
 CALL WGRTEXTFONT(TFONT,WIDTH=CHW,HEIGHT=CHH,ISTYLE=0)

 TDY=(Y2-Y1)/REAL(NY); BOXY=TDY; BOXX=BOXY/RAT

 !## write legend-header
 IF(LEN_TRIM(LEG%HEDTXT).GT.0)THEN
  CALL WGRTEXTSTRING(X1+0.25*BOXX,Y2-(TDY/2.25),TRIM(LEG%HEDTXT)) !'Legend Information (m3/day)')
  Y2=Y2-TDY; TDY=(Y2-Y1)/REAL(NY)
 ENDIF 
 
 !## set proper textsizes
 CALL WGRTEXTFONT(TFONT,WIDTH=CHW,HEIGHT=CHH,ISTYLE=0)
 CALL IGRFILLPATTERN(SOLID)

 CALL IGRCOLOURN(WRGB(0,0,0))
 X=X1+0.25*BOXX; Y=Y2+TDY; TDX=0.0; J=0
 DO I=1,LEG%NCLR
  
  J=J+1; Y=Y-TDY
  IF(J.GT.NY)THEN
   !## 256-colours - plot only last in column
   IF(IGRY.EQ.0)CALL WGRTEXTSTRING(X+BOXX+OFFX,Y1+(TDY/2.0),TRIM(LEG%LEGTXT(I)))
   Y=Y2
   X=X+BOXX+(TDX*RAT)
   TDX=0.0
   J=1
  ENDIF

  CALL IGRFILLPATTERN(SOLID)
  CALL IGRCOLOURN(LEG%RGB(I))
  CALL IGRRECTANGLE(X,Y,X+BOXX,Y-BOXY)
  IF(IGRY.EQ.1)THEN
   CALL IGRFILLPATTERN(OUTLINE)
   CALL IGRLINEWIDTH(2)
   CALL IGRCOLOURN(WRGB(255,255,255)) !200,200,200))
   CALL IGRRECTANGLE(X,Y,X+BOXX,Y-BOXY)
   CALL IGRLINEWIDTH(1) !2)
  ENDIF

  !## output text
  CALL IGRCOLOURN(WRGB(0,0,0))
  CALL IGRFILLPATTERN(OUTLINE)

  !## 256-colours - plot only first and last
  IF(IGRY.EQ.0)THEN
   IF(J.EQ.1)CALL WGRTEXTSTRING(X+BOXX+OFFX,Y2-(TDY/2.0),TRIM(LEG%LEGTXT(I)))
  !## max. 50 classes
  ELSE
   IF(LEN_TRIM(LEG%LEGTXT(I)).NE.0)THEN
    CALL WGRTEXTSTRING(X+BOXX+OFFX,Y-(TDY/2.0),TRIM(LEG%LEGTXT(I)))
   ENDIF
  ENDIF

  IF(LEN_TRIM(LEG%LEGTXT(I)).NE.0)TDX=WGRTEXTLENGTH('  '//TRIM(LEG%LEGTXT(I))//'  ')*WINFOGRREAL(GRAPHICSCHWIDTH)
  
 ENDDO

 !## 256-colours - plot only last in column
 IF(IGRY.EQ.0)CALL WGRTEXTSTRING(X+BOXX+OFFX,Y1+(TDY/2.0),TRIM(LEG%LEGTXT(LEG%NCLR)))

 END SUBROUTINE LEGPLOT_PLOT

 !###======================================================================
 SUBROUTINE LEGPLOT_PLOTMOUSEMOVE(XC,YC,ICRD,CRDITYPE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: ICRD,CRDITYPE
 REAL,INTENT(IN)     :: XC,YC
 REAL                :: DX

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_PLOTLEGEND,2).NE.1)RETURN

 !## refresh coordinates
 XCRD(1)=MPW%XMIN+XRELCRD(1)*(MPW%XMAX-MPW%XMIN)
 YCRD(1)=MPW%YMIN+YRELCRD(1)*(MPW%YMAX-MPW%YMIN)
 XCRD(2)=MPW%XMIN+XRELCRD(2)*(MPW%XMAX-MPW%XMIN)
 YCRD(2)=MPW%YMIN+YRELCRD(2)*(MPW%YMAX-MPW%YMIN)

 DX=SQRT((MPW%XMAX-MPW%XMIN)**2.0+(MPW%YMAX-MPW%YMIN)**2.0)/500.0

 !## linestuk
 ICRD=0
 IF(IGRDISTANCELINE(XCRD(1),YCRD(1),XCRD(2),YCRD(1),XC,YC,0).LE.DX)ICRD=1 !## bottom
 IF(IGRDISTANCELINE(XCRD(2),YCRD(1),XCRD(2),YCRD(2),XC,YC,0).LE.DX)ICRD=2 !## right
 IF(IGRDISTANCELINE(XCRD(2),YCRD(2),XCRD(1),YCRD(2),XC,YC,0).LE.DX)ICRD=3 !## top
 IF(IGRDISTANCELINE(XCRD(1),YCRD(2),XCRD(1),YCRD(1),XC,YC,0).LE.DX)ICRD=4 !## left
 
 IF(UTL_DIST(XCRD(2),YCRD(1),XC,YC).LE.DX)ICRD=6 !## lrc
 IF(UTL_DIST(XCRD(2),YCRD(2),XC,YC).LE.DX)ICRD=7 !## urc
 IF(UTL_DIST(XCRD(1),YCRD(2),XC,YC).LE.DX)ICRD=8 !## ulc
 IF(UTL_DIST(XCRD(1),YCRD(1),XC,YC).LE.DX)ICRD=9 !## llc 
 
 SELECT CASE(ICRD)
  CASE (1,3)
   CALL WCURSORSHAPE(ID_CURSORMOVEUPDOWN)
  CASE (2,4)
   CALL WCURSORSHAPE(ID_CURSORMOVELEFTRIGHT)
  CASE (6,8)
   CALL WCURSORSHAPE(ID_CURSORMOVENWSE)
  CASE (7,9)
   CALL WCURSORSHAPE(ID_CURSORMOVENESW)
 END SELECT
 CRDITYPE=-ICRD
 IF(CRDITYPE.NE.0)RETURN

 IF(XC.GT.XCRD(1).AND.XC.LT.XCRD(2).AND. &
    YC.GT.YCRD(1).AND.YC.LT.YCRD(2))THEN
  CALL WCURSORSHAPE(ID_CURSORMOVE)
  CRDITYPE=-5
  RETURN
 ENDIF

 ICRD    =0
 CRDITYPE=0
 CALL WCURSORSHAPE(CURARROW)

 END SUBROUTINE LEGPLOT_PLOTMOUSEMOVE

 !###======================================================================
 SUBROUTINE LEGPLOT_ADJUSTSHAPE(CRDITYPE,ICRD,XC,YC,DOWNX,DOWNY)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: ICRD,CRDITYPE
 REAL,INTENT(INOUT) :: DOWNX,DOWNY
 REAL,INTENT(IN) :: XC,YC
 REAL :: DX,DY,DXX

 DXX=SQRT((MPW%XMAX-MPW%XMIN)**2.0+(MPW%YMAX-MPW%YMIN)**2.0)/500.0
 
 CALL LEGPLOT_DRAWSHAPE()
 DX=XC-DOWNX
 DY=YC-DOWNY

 SELECT CASE (CRDITYPE)
  !## adjust bottom
  CASE (-1)
   YCRD(1)=YCRD(1)+DY
   YCRD(1)=MIN(YCRD(1),YCRD(2)-DXX)
  !## adjust right
  CASE (-2)
   XCRD(2)=XCRD(2)+DX
   XCRD(2)=MAX(XCRD(1)+DXX,XCRD(2))
  !## adjust top
  CASE (-3)
   YCRD(2)=YCRD(2)+DY
   YCRD(2)=MAX(YCRD(1)+DXX,YCRD(2))
  !## adjust left
  CASE (-4)
   XCRD(1)=XCRD(1)+DX
   XCRD(1)=MIN(XCRD(1),XCRD(2)-DXX)
  !## move
  CASE (-5)
   XCRD=XCRD+DX
   YCRD=YCRD+DY
  !## drag lower-right-corner
  CASE (-6)
   XCRD(2)=XCRD(2)+DX
   YCRD(1)=YCRD(1)+DY
   XCRD(2)=MAX(XCRD(1)+DXX,XCRD(2))
   YCRD(1)=MIN(YCRD(1),YCRD(2)-DXX)
  !## drag upper-right-corner
  CASE (-7)
   XCRD(2)=XCRD(2)+DX
   YCRD(2)=YCRD(2)+DY
   XCRD(2)=MAX(XCRD(1)+DXX,XCRD(2))
   YCRD(2)=MAX(YCRD(1)+DXX,YCRD(2))
  !## drag upper-left-corner
  CASE (-8)
   XCRD(1)=XCRD(1)+DX
   YCRD(2)=YCRD(2)+DY
   XCRD(1)=MIN(XCRD(1),XCRD(2)-DXX)
   YCRD(2)=MAX(YCRD(1)+DXX,YCRD(2))
  !## drag lower-left-corner
  CASE (-9)
   XCRD(1)=XCRD(1)+DX
   YCRD(1)=YCRD(1)+DY
   XCRD(1)=MIN(XCRD(1),XCRD(2)-DXX)
   YCRD(1)=MIN(YCRD(1),YCRD(2)-DXX)
 END SELECT

 CALL LEGPLOT_DRAWSHAPE()
 DOWNX=DOWNX+DX
 DOWNY=DOWNY+DY

 !## refresh coordinates to local ones
 DX=MPW%XMAX-MPW%XMIN
 DY=MPW%YMAX-MPW%YMIN
 XRELCRD(1)=MAX(0.0,(XCRD(1)-MPW%XMIN)/DX)
 YRELCRD(1)=MAX(0.0,(YCRD(1)-MPW%YMIN)/DY)
 XRELCRD(2)=MIN(1.0,(XCRD(2)-MPW%XMIN)/DX)
 YRELCRD(2)=MIN(1.0,(YCRD(2)-MPW%YMIN)/DY)

 END SUBROUTINE LEGPLOT_ADJUSTSHAPE

 !###======================================================================
 SUBROUTINE LEGPLOT_PLOTSHAPE()
 !###======================================================================
 IMPLICIT NONE

 CALL IGRPLOTMODE(MODEXOR)
 CALL IGRCOLOURN(ISHAPECOLOR)
 CALL IGRFILLPATTERN(OUTLINE)
 CALL IGRRECTANGLE(XCRD(1),YCRD(1),XCRD(2),YCRD(2))

 END SUBROUTINE LEGPLOT_PLOTSHAPE

 !###======================================================================
 SUBROUTINE LEGPLOT_DRAWSHAPE()
 !###======================================================================
 IMPLICIT NONE

 CALL IDFPLOT1BITMAP()
 CALL LEGPLOT_PLOTSHAPE()
 CALL IDFPLOT2BITMAP()

 CALL IGRPLOTMODE(MODECOPY)
 CALL IGRFILLPATTERN(OUTLINE)
 CALL IGRLINETYPE(SOLIDLINE)

 END SUBROUTINE LEGPLOT_DRAWSHAPE

END MODULE MOD_LEGPLOT
