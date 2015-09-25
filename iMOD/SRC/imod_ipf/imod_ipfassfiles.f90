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
MODULE MOD_IPFASSFILE

USE WINTERACTER
USE RESOURCE
USE MOD_COLOURS
USE MODPLOT, ONLY : MPW
USE MOD_UTL, ONLY : UTL_GETUNIT,ITOS,UTL_IDATETOJDATE,UTL_DRAWLEGENDBOX,ITIMETOFTIME,UTL_SETTEXTSIZE,UTL_CAP
USE MOD_PROFILE_UTL, ONLY : GRAPHUNITS,AXESOBJ,PROFILE_PLOTAXES,GRAPHAREA
USE MOD_IPFGETVALUE_COLOURS, ONLY : IPFGETVALUE_OPENSAVECOLOURS
USE MOD_IDFTIMESERIE_UTL, ONLY : IDFTIMESERIE_GETMINMAXX,IDFTIMESERIE_GETMINMAXY,IDFTIMESERIE_PUTMINMAXX,IDFTIMESERIE_PUTMINMAXY
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_IPF_PAR
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_IFF, ONLY : IFFFADEOUT

TYPE(AXESOBJ) :: AXES

!## IACT=1 SIMPLE
!## IACT=2 EXTENDED
!## IACT=3 QUICKVIEW
!## IACT=4
!## IACT=5 SELECTED IN IPFEXTRACTED
!## IACT=6 SELECTED IN IPFEXTRACTED

CONTAINS

 !###===============================================================================
 SUBROUTINE IPFDIMENSIONASSFILE(IASSF,FNAME,IAXES)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IASSF
 INTEGER,INTENT(IN),DIMENSION(:) :: IAXES
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: IU,I,J
 REAL :: D
 LOGICAL :: LEX

 IF(IPFOPENASSFILE(IU,IASSF,FNAME).AND. &
    IPFREADASSFILELABEL(IU,IASSF,FNAME).AND.  &
    IPFREADASSFILE(IU,IASSF,FNAME))THEN
  
  ASSF(IASSF)%FNAME=TRIM(FNAME(INDEX(FNAME,'\',.TRUE.)+1:))
  
  !## first axes
  ASSF(IASSF)%XMIN = 10.0E10
  ASSF(IASSF)%XMAX =-10.0E10
  ASSF(IASSF)%YMIN = 10.0E10
  ASSF(IASSF)%YMAX =-10.0E10

  !## second axes
  ASSF(IASSF)%Y2MIN= 10.0E10
  ASSF(IASSF)%Y2MAX=-10.0E10

  ASSF(IASSF)%IAXES=IAXES !1 !## using one axes

  SELECT CASE (ASSF(IASSF)%ITOPIC)

   !## measures
   CASE (1)
    DO I=1,ASSF(IASSF)%NCASS-1
     IF(GRAPHLINESONOFF(I+1).EQ.0)CYCLE
     DO J=1,ASSF(IASSF)%NRASS
      IF(ASSF(IASSF)%MEASURE(I,J).NE.ASSF(IASSF)%NODATA(I+1))THEN

       !## first axes
       IF(IAXES(I+1).EQ.1)THEN
        ASSF(IASSF)%YMIN=MIN(ASSF(IASSF)%MEASURE(I,J),ASSF(IASSF)%YMIN)
        ASSF(IASSF)%YMAX=MAX(ASSF(IASSF)%MEASURE(I,J),ASSF(IASSF)%YMAX)
       !## second axes
       ELSEIF(IAXES(I+1).EQ.2)THEN
        ASSF(IASSF)%Y2MIN=MIN(ASSF(IASSF)%MEASURE(I,J),ASSF(IASSF)%Y2MIN)
        ASSF(IASSF)%Y2MAX=MAX(ASSF(IASSF)%MEASURE(I,J),ASSF(IASSF)%Y2MAX)
       ENDIF

      ENDIF
     END DO
    END DO
    !## adjust ymax/ymin to fit nicely
    D                =(ASSF(IASSF)%YMAX-ASSF(IASSF)%YMIN)/50.0
    ASSF(IASSF)%YMAX = ASSF(IASSF)%YMAX+D
    ASSF(IASSF)%YMIN = ASSF(IASSF)%YMIN-D
    D                =(ASSF(IASSF)%Y2MAX-ASSF(IASSF)%Y2MIN)/50.0
    ASSF(IASSF)%Y2MAX= ASSF(IASSF)%Y2MAX+D
    ASSF(IASSF)%Y2MIN= ASSF(IASSF)%Y2MIN-D

    ASSF(IASSF)%XMIN=MINVAL(ASSF(IASSF)%IDATE(1:ASSF(IASSF)%NRASS))
    ASSF(IASSF)%XMAX=MAXVAL(ASSF(IASSF)%IDATE(1:ASSF(IASSF)%NRASS))

   !## draw drills
   CASE (2)
    ASSF(IASSF)%YMIN =MINVAL(ASSF(IASSF)%Z(1:ASSF(IASSF)%NRASS))
    ASSF(IASSF)%YMAX =MAXVAL(ASSF(IASSF)%Z(1:ASSF(IASSF)%NRASS))
    ASSF(IASSF)%XMIN =0.0
    ASSF(IASSF)%XMAX =1.0

   !## sonderingen
   CASE (3)
    DO I=2,ASSF(IASSF)%NCASS
     IF(GRAPHLINESONOFF(I).EQ.0)CYCLE
     DO J=1,ASSF(IASSF)%NRASS
      IF(ASSF(IASSF)%MEASURE(I,J).NE.ASSF(IASSF)%NODATA(I))THEN
       ASSF(IASSF)%XMIN=MIN(ASSF(IASSF)%MEASURE(I,J),ASSF(IASSF)%XMIN)
       ASSF(IASSF)%XMAX=MAX(ASSF(IASSF)%MEASURE(I,J),ASSF(IASSF)%XMAX)
      ENDIF
     END DO
    END DO
    ASSF(IASSF)%YMIN=MINVAL(ASSF(IASSF)%MEASURE(1,1:ASSF(IASSF)%NRASS))
    ASSF(IASSF)%YMAX=MAXVAL(ASSF(IASSF)%MEASURE(1,1:ASSF(IASSF)%NRASS))
  END SELECT
  CLOSE(IU)

  !## no values found at all!
  IF(ASSF(IASSF)%YMAX.LT.ASSF(IASSF)%YMIN)THEN
   ASSF(IASSF)%YMIN=-1;ASSF(IASSF)%YMAX=1
  ENDIF

  !## flat line
  IF(ABS(ASSF(IASSF)%YMAX-ASSF(IASSF)%YMIN).LE.0.01)THEN 
   ASSF(IASSF)%YMIN=ASSF(IASSF)%YMIN-0.01 !1.0
   ASSF(IASSF)%YMAX=ASSF(IASSF)%YMAX+0.01 !1.0
  ENDIF

 ELSE
  INQUIRE(UNIT=IU,OPENED=LEX)
  IF(LEX)CLOSE(IU)
  ASSF(IASSF)%NRASS=0
  ASSF(IASSF)%NCASS=0
 ENDIF

 END SUBROUTINE IPFDIMENSIONASSFILE

 !###===============================================================================
 SUBROUTINE IPFPLOTASSFILE(XLOC,YLOC,DFRAC,IASSF,IACT,PLOTSTYLE,GXMIN,GYMIN,GXMAX,GYMAX, &
                           IMARKDATA,LPROF,AXMIN,AXMAX,AYMIN,AYMAX,IZOOM,ISKIP,DWIDTH,IANALYSE,ILEG)
 !###===============================================================================
 IMPLICIT NONE
 LOGICAL,INTENT(IN) :: LPROF
 INTEGER,INTENT(IN) :: IZOOM !## =0,=1 use fixed axes yes/no
 INTEGER,INTENT(IN) :: ISKIP !## legend skip 
 INTEGER,INTENT(IN) :: PLOTSTYLE,IMARKDATA,IASSF,IANALYSE,ILEG
 INTEGER(KIND=1),INTENT(IN) :: IACT
 REAL,INTENT(IN) :: XLOC,YLOC,GXMIN,GYMIN,GXMAX,GYMAX,AXMIN,AXMAX,AYMIN,AYMAX,DFRAC,DWIDTH
 REAL :: DX,DY,AX1,AX2,AY1,AY2,XP,YP,MINX,MAXX,MINY,MAXY,XFRAC,YFRAC,XINT,YINT
 INTEGER :: IFIXX,IFIXY,I
 REAL :: COPYXMIN,COPYXMAX,COPYYMIN,COPYYMAX,MIN2Y,MAX2Y

 IF(LPROF.AND.ASSF(IASSF)%ITOPIC.GT.1)THEN
  DX=(GXMAX-GXMIN)/150.0  !## gxmin=graphical dimensions
  IF(DWIDTH.GT.0.0)DX=DWIDTH 
  !## compute area for the figure to be plotted
!  XP=(XLOC/(GXMAX-GXMIN))*(AXMAX-AXMIN)+AXMIN
!  YP=(YLOC/(GYMAX-GYMIN))*(AYMAX-AYMIN)+AYMIN
!  CALL IGRAREA(XP-DX,
  CALL IPFDRAW_DRILLPROF(ILEG,IASSF,XLOC,DX,PLOTSTYLE,DFRAC)
  RETURN
 ENDIF

 MINX=0.0; MINY=0.0; MAXX=1.0; MAXY=1.0

 !## nothing to do - don't know what to draw, draw rectangle
 IF(ASSF(IASSF)%NRASS.LE.0)THEN
  SELECT CASE (IACT)
   !## simple/extended
   CASE (1,2)
    XFRAC=50.0
    YFRAC=50.0
    DX=((GXMAX-GXMIN)*(MAXX-MINX))/XFRAC  
    DY=((GYMAX-GYMIN)*(MAXY-MINY))/YFRAC
    XP=XLOC
    YP=YLOC-DY
    AX1=(XP-GXMIN)/(GXMAX-GXMIN)
    AX2=(XP+DX-GXMIN)/(GXMAX-GXMIN)
    AY1=(YP-GYMIN)/(GYMAX-GYMIN)
    AY2=(YP+DY-GYMIN)/(GYMAX-GYMIN)

    !## correct units
    IF(AX1.LT.AXMIN)MINX=MINX+(MAXX-MINX)*((AXMIN-AX1)/(AX2-AX1))
    IF(AX2.GT.AXMAX)MAXX=MAXX+(MAXX-MINX)*((AXMAX-AX2)/(AX2-AX1))
    IF(AY1.LT.AYMIN)MINY=MINY+(MAXY-MINY)*((AYMIN-AY1)/(AY2-AY1))
    IF(AY2.GT.AYMAX)MAXY=MAXY+(MAXY-MINY)*((AYMAX-AY2)/(AY2-AY1))
    AX1=MIN(AXMAX,MAX(AXMIN,AX1))
    AX2=MIN(AXMAX,MAX(AXMIN,AX2))
    AY1=MIN(AYMAX,MAX(AYMIN,AY1))
    AY2=MIN(AYMAX,MAX(AYMIN,AY2))

   !## analyse
   CASE (3)
    AX1=AXMIN
    AX2=AXMAX
    AY1=AYMIN
    AY2=AYMAX
  END SELECT
  IF(AX2.GT.AX1.AND.AY2.GT.AY1)THEN
   CALL IGRAREA(AX1,AY1,AX2,AY2)
   CALL IGRAREACLEAR()
   CALL IGRUNITS(0.0,0.0,1.0,1.0)
   CALL IGRCOLOURN(WRGB(0,0,0))
   CALL IGRFILLPATTERN(OUTLINE)
   CALL IGRRECTANGLE(0.0,0.0,1.0,1.0)
   CALL IGRJOIN(0.0,0.0,1.0,1.0)
   CALL IGRJOIN(0.0,1.0,1.0,0.0)
   RETURN
  ENDIF
 ENDIF

 SELECT CASE (ASSF(IASSF)%ITOPIC)

  !## measures
  CASE (1)

   SELECT CASE (IACT)
    !## simple
    CASE (1)
     XFRAC=10.0   !## to be fit on total graphics-screen
     YFRAC=2.0*XFRAC/WINFOGRREAL(GRAPHICSRATIO)
    !## advanced
    CASE (2)
     XFRAC=2.5    !## to be fit on total graphics-screen
     YFRAC=2.0*XFRAC/WINFOGRREAL(GRAPHICSRATIO)
    !## analyse
    CASE (3)
     XFRAC=1.0    !## to be fit on total graphics-screen
     YFRAC=1.0
   END SELECT

  !## drills
  CASE (2)
   SELECT CASE (IACT)
    CASE (1)
     XFRAC=250.0
    CASE (2)
     XFRAC=100.0
    CASE (3)
     XFRAC=1.0
   END SELECT
   YFRAC=50.0  !meter
   MINY =ASSF(IASSF)%YMIN
   MAXY =ASSF(IASSF)%YMAX

  !## sonderingen
  CASE (3)
   SELECT CASE (IACT)
    CASE (1)
     XFRAC=50.0
    CASE (2)
     XFRAC=10.0
    CASE (3)
     XFRAC=1.0
   END SELECT
   YFRAC=50.0  !meter
   MINY =ASSF(IASSF)%YMIN
   MAXY =ASSF(IASSF)%YMAX

 END SELECT

 DX=((GXMAX-GXMIN)*(MAXX-MINX))/XFRAC  !gxmin=graphical dimensions
 DY=((GYMAX-GYMIN)*(MAXY-MINY))/YFRAC

 !## simple(1)/extended(2)
 IF(IACT.EQ.1.OR.IACT.EQ.2)THEN

  SELECT CASE (ASSF(IASSF)%ITOPIC)
   !## measures
   CASE (1)
    XP=XLOC
    YP=YLOC-DY
   !## drills/sonderingen
   CASE (2,3)
    XP=XLOC-(0.5*DX)
    YP=YLOC-DY
  END SELECT

 ENDIF

 !## extent for graph
 MINX =ASSF(IASSF)%XMIN
 MAXX =ASSF(IASSF)%XMAX
 MINY =ASSF(IASSF)%YMIN
 MAXY =ASSF(IASSF)%YMAX
 MIN2Y=ASSF(IASSF)%Y2MIN
 MAX2Y=ASSF(IASSF)%Y2MAX

 SELECT CASE (IACT)

  !## simple/extended
  CASE (1,2)
   AX1=(XP-GXMIN)/(GXMAX-GXMIN)
   AX2=(XP+DX-GXMIN)/(GXMAX-GXMIN)
   AY1=(YP-GYMIN)/(GYMAX-GYMIN)
   AY2=(YP+DY-GYMIN)/(GYMAX-GYMIN)

   !## correct units
   IF(AX1.LT.AXMIN)MINX =MINX +(MAXX-MINX)  *((AXMIN-AX1)/(AX2-AX1))
   IF(AX2.GT.AXMAX)MAXX =MAXX +(MAXX-MINX)  *((AXMAX-AX2)/(AX2-AX1))
   IF(AY1.LT.AYMIN)MINY =MINY +(MAXY-MINY)  *((AYMIN-AY1)/(AY2-AY1))
   IF(AY2.GT.AYMAX)MAXY =MAXY +(MAXY-MINY)  *((AYMAX-AY2)/(AY2-AY1))
   IF(AY1.LT.AYMIN)MIN2Y=MIN2Y+(MAX2Y-MIN2Y)*((AYMIN-AY1)/(AY2-AY1))
   IF(AY2.GT.AYMAX)MAX2Y=MAX2Y+(MAX2Y-MIN2Y)*((AYMAX-AY2)/(AY2-AY1))
   AX1=MIN(AXMAX,MAX(AXMIN,AX1))
   AX2=MIN(AXMAX,MAX(AXMIN,AX2))
   AY1=MIN(AYMAX,MAX(AYMIN,AY1))
   AY2=MIN(AYMAX,MAX(AYMIN,AY2))

  !## analyse
  CASE (3)
   AX1=AXMIN
   AX2=AXMAX
   AY1=AYMIN
   AY2=AYMAX

 END SELECT

 IF(AX2.GT.AX1.AND.AY2.GT.AY1)THEN

  CALL IGRAREA(AX1,AY1,AX2,AY2)

  !## get xmin/xmax/ymin/ymax as ifixed.eq.1
  CALL WDIALOGSELECT(ID_DIPFINFO_TAB2)
  IFIXX=0
  IFIXY=0
  XINT=1.0
  YINT=1.0
  
  SELECT CASE (ASSF(IASSF)%ITOPIC)
   CASE (1) !## timeseries
    CALL IDFTIMESERIE_GETMINMAXX(MINX,MAXX,XINT,IFIXX,0)
    CALL IDFTIMESERIE_GETMINMAXY(MINY,MAXY,YINT,IFIXY)
   CASE (2) !## boreholes
    CALL IDFTIMESERIE_GETMINMAXY(MINY,MAXY,YINT,IFIXY)
   CASE (3) !## sonderingen
    !## entered from ipfplot
    IF(IANALYSE.EQ.0)THEN
     IF(GRAPHLINESXAXES.EQ.1)THEN
      MINX=GRAPHLINESXMIN; MAXX=GRAPHLINESXMAX
     ENDIF
     IF(GRAPHLINESYAXES.EQ.1)THEN
      MINY=GRAPHLINESYMIN; MAXY=GRAPHLINESYMAX
     ENDIF
    !## entered from ipfanalyse
    ELSEIF(IANALYSE.EQ.1)THEN
     CALL WDIALOGSELECT(ID_DIPFINFOSERIE)
     IF(GRAPHLINESXAXES.EQ.1)THEN
      CALL WDIALOGGETREAL(IDF_REAL1,MINX) 
      CALL WDIALOGGETREAL(IDF_REAL2,MAXX) 
     ENDIF
     IF(GRAPHLINESYAXES.EQ.1)THEN
      CALL WDIALOGGETREAL(IDF_REAL3,MINY) 
      CALL WDIALOGGETREAL(IDF_REAL4,MAXY) 
     ENDIF  
     GRAPHLINESXMIN=MINX; GRAPHLINESXMAX=MAXX
     GRAPHLINESYMIN=MINY; GRAPHLINESYMAX=MAXY
    ENDIF
  END SELECT

  IF(IANALYSE.EQ.1)CALL WDIALOGSELECT(ID_DIPFINFOSERIE)
  CALL IGRAREACLEAR()

!## weg?
  CALL IGRUNITS(MINX,MINY,MAXX,MAXY)

  IF(IACT.NE.INT(1,1))THEN
  
   IF(MAXX-MINX.LE.0.0)THEN
    MINX=MINX-1.0
    MAXX=MAXX+1.0
   ENDIF

   AXES%XMIN  =MINX
   AXES%XMAX  =MAXX
   AXES%YMIN  =MINY
   AXES%YMAX  =MAXY
   AXES%IFIXX =IFIXX
   AXES%IFIXY =IFIXY
   AXES%IFIXY2=0
   AXES%XINT  =XINT
   AXES%YINT  =YINT
   AXES%ICLRRASTER=WRGB(191,191,191)
   AXES%XFACTOR=1.0
   AXES%YFACTOR=1.0
   !## mapview
   IF(IACT.EQ.INT(2,1))THEN
    AXES%DXAXESL=10
    AXES%DYAXESB=5
    AXES%DYAXEST=18.75
    AXES%DXAXESR=37.5
   !## quickview
   ELSE
    AXES%DXAXESL=40.0
    AXES%DYAXESB=20.0
    AXES%DYAXEST=75.0
    AXES%DXAXESR=150.0
   ENDIF
   AXES%TFONT=FFHELVETICA   !## text-font
   IF(SUM(ASSF(IASSF)%IAXES(1:ASSF(IASSF)%NCASS)).EQ.ASSF(IASSF)%NCASS)THEN
    AXES%IAXES=(/1,0/)   !## left/bottom axes only
   ELSE 
    AXES%IAXES=(/1,1/)   !## left/bottom and right axes added
    AXES%DXAXESR=AXES%DXAXESL
    AXES%Y2MIN=ASSF(IASSF)%Y2MIN
    AXES%Y2MAX=ASSF(IASSF)%Y2MAX
    AXES%Y2INT=YINT
    DO I=1,ASSF(IASSF)%NCASS
     IF(ASSF(IASSF)%IAXES(I).EQ.2)THEN
      AXES%Y2TITLE=ASSF(IASSF)%ATTRIB(I)
      EXIT
     ENDIF
    ENDDO
   ENDIF

   AXES%ICLRBACKGROUND=WRGB(123,152,168)
   !## timeseries
   IF(ASSF(IASSF)%ITOPIC.EQ.1)THEN
    AXES%XTITLE=ASSF(IASSF)%ATTRIB(1)
    AXES%YTITLE=ASSF(IASSF)%ATTRIB(2)
    AXES%LDATE=.TRUE.
    CALL PROFILE_PLOTAXES(AXES,1)
   !## drills
   ELSEIF(ASSF(IASSF)%ITOPIC.EQ.2)THEN
    AXES%XTITLE=''
    AXES%YTITLE=ASSF(IASSF)%ATTRIB(1)
    AXES%LDATE=.FALSE.
    CALL PROFILE_PLOTAXES(AXES,1)
   !## sonderingen
   ELSEIF(ASSF(IASSF)%ITOPIC.EQ.3)THEN
    AXES%XTITLE=ASSF(IASSF)%ATTRIB(2)
    AXES%YTITLE=ASSF(IASSF)%ATTRIB(1)
    AXES%LDATE=.FALSE.
    CALL PROFILE_PLOTAXES(AXES,1)
   ENDIF

  ENDIF

  !## fill in min./max. values
  CALL WDIALOGSELECT(ID_DIPFINFO_TAB2)
  SELECT CASE (ASSF(IASSF)%ITOPIC)
   CASE (1) !## timeseries
    CALL IDFTIMESERIE_PUTMINMAXX(MINX,MAXX,AXES%XINT,0)
    CALL IDFTIMESERIE_PUTMINMAXY(MINY,MAXY,AXES%YINT)
   CASE (2) !## drills
    CALL IDFTIMESERIE_PUTMINMAXY(MINY,MAXY,AXES%YINT)
   CASE (3) !## sonderingen
    CALL IDFTIMESERIE_PUTMINMAXY(MINY,MAXY,AXES%YINT)
    IF(IANALYSE.EQ.1)THEN
     CALL WDIALOGSELECT(ID_DIPFINFOSERIE)
     IF(GRAPHLINESXAXES.EQ.0)THEN
      CALL WDIALOGPUTREAL(IDF_REAL1,MINX) 
      CALL WDIALOGPUTREAL(IDF_REAL2,MAXX) 
     ENDIF
     IF(GRAPHLINESYAXES.EQ.0)THEN
      CALL WDIALOGPUTREAL(IDF_REAL3,MINY) 
      CALL WDIALOGPUTREAL(IDF_REAL4,MAXY) 
     ENDIF  
    ENDIF
  END SELECT
  
  IF(IANALYSE.EQ.1)CALL WDIALOGSELECT(ID_DIPFINFOSERIE)

  IF(IACT.EQ.INT(3,1))THEN

   IF(IZOOM.EQ.0)THEN
    ASSF(IASSF)%XMIN =GRAPHUNITS(1,1)
    ASSF(IASSF)%YMIN =GRAPHUNITS(2,1)
    ASSF(IASSF)%XMAX =GRAPHUNITS(3,1)
    ASSF(IASSF)%YMAX =GRAPHUNITS(4,1)
    ASSF(IASSF)%Y2MIN=GRAPHUNITS(5,1)
    ASSF(IASSF)%Y2MAX=GRAPHUNITS(6,1)
   ENDIF

  ENDIF

  SELECT CASE (ASSF(IASSF)%ITOPIC)

   !## measures
   CASE (1)
    CALL IPFDRAWITOPIC1(IASSF,PLOTSTYLE,IMARKDATA,MINX,MINY,MAXX,MAXY,MIN2Y,MAX2Y)
    IF(IACT.NE.INT(1,1))CALL IPFDRAWLEGEND(IASSF,MINX,MAXY,ISKIP)

   !## draw drills
   CASE (2)
    CALL IGRUNITS(MINX,MINY,MAXX,MAXY)
    CALL IPFDRAWITOPIC2(ILEG,IASSF,XLOC) 
   !## sonderingen
   CASE (3)
    CALL IGRUNITS(MINX,MINY,MAXX,MAXY)
    CALL IPFDRAWITOPIC3(IASSF,PLOTSTYLE,0.0,0.0)
    IF(IACT.NE.INT(1,1))CALL IPFDRAWLEGEND(IASSF,MINX,MAXY,ISKIP)

  END SELECT

 ELSE
!  DX=(WINFOGRREAL(GRAPHICSUNITMAXX)-WINFOGRREAL(GRAPHICSUNITMINX))/250.0
!  CALL IGRCOLOURN(WRGB(0,0,0))
!  CALL IGRRECTANGLE(XLOC,YLOC,XLOC+DX,YLOC-DX)
!  CALL WGRTEXTBLOCK(XLOC,YLOC,XLOC+DX,YLOC-DY,'Can not plot associated file!',1.0,TBJUSTIFY+TBFONTSIZE)
 ENDIF

 END SUBROUTINE IPFPLOTASSFILE

 !###===============================================================================
 SUBROUTINE IPFDRAWLEGEND(IASSF,XMIN,YMAX,ISKIP)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IASSF,ISKIP
 REAL,INTENT(IN) :: XMIN,YMAX
 REAL :: X1,X2,Y1,Y2,CHH,DY,SX_RATIO,DX,OFFX,BOXX,Y
 INTEGER :: IWD,IWS,I

 !## get current graph-dimensions
 X1 =INFOGRAPHICS(GRAPHICSUNITMINX)
 X2 =INFOGRAPHICS(GRAPHICSUNITMAXX)

 !## get current textsizes
 CHH=WINFOGRREAL(GRAPHICSCHHEIGHT)
 DY =CHH*MIN(MAXCOLOUR,ASSF(IASSF)%NCASS-1)

 !## drawable settings
 IWD=WINFODRAWABLE(DRAWABLEWIDTH)
 !## screen setting
 IWS=WINFOSCREEN(SCREENWIDTH)
 !## ratio's
 SX_RATIO=REAL(IWS)/REAL(IWD)

 DX=X2-X1
 OFFX=(DX/250.0)*SX_RATIO
 BOXX=OFFX*4.0

 CALL IGRFILLPATTERN(SOLID)

 DY=DY/REAL(MIN(MAXCOLOUR,ASSF(IASSF)%NCASS-1))
 Y =YMAX+0.5*DY

 CALL WGRTEXTORIENTATION(ALIGNLEFT)
 
 DO I=1,ISKIP; IF(GRAPHLINESONOFF(I).EQ.1)CYCLE; Y=Y-DY; ENDDO
 
 DO I=1,MIN(MAXCOLOUR,ASSF(IASSF)%NCASS-1)

  IF(GRAPHLINESONOFF(I+1).EQ.0)CYCLE
  
  Y=Y-DY

  !## plot axes-text
  CALL IGRCOLOURN(WRGB(0,0,0))
!  CALL WGRTEXTSTRING(XMIN+(2.0*OFFX)+BOXX,Y-0.5*DY,ASSF(IASSF)%ATTRIB(I+1))
  CALL WGRTEXTSTRING(XMIN+(2.0*OFFX)+BOXX,Y-0.5*DY,TRIM(ASSF(IASSF)%ATTRIB(I+1))) !//'('//TRIM(ASSF(IASSF)%FNAME)//')')

  X1=XMIN+OFFX
  Y1=Y-DY+0.1*DY
  X2=XMIN+OFFX+BOXX
  Y2=Y-0.1*DY
  
  CALL UTL_DRAWLEGENDBOX(X1,Y1,X2,Y2,GRAPHLINESCOLOUR(I+1),GRAPHLINESTHICKNESS(I+1),0,1)!OUTLINE)!,TRIM(MP(JPLOT(I))%ALIAS))  

 ENDDO

 END SUBROUTINE IPFDRAWLEGEND

 !###===============================================================================
 SUBROUTINE IPFDRAWITOPIC1(IASSF,PLOTSTYLE,IMARKDATA,MINX,MINY,MAXX,MAXY,MIN2Y,MAX2Y)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: IMARKER=1
 REAL,INTENT(IN) :: MINX,MINY,MAXX,MAXY,MIN2Y,MAX2Y
 INTEGER,INTENT(IN) :: PLOTSTYLE,IMARKDATA,IASSF
 INTEGER :: I,J,K
 REAL :: X1,Y1,X2,Y2

 CALL IGRFILLPATTERN(OUTLINE)

 DO J=1,MIN(MAXCOLOUR,ASSF(IASSF)%NCASS-1)

  IF(GRAPHLINESONOFF(J+1).EQ.0)CYCLE

  !## depends on axes
  IF(ASSF(IASSF)%IAXES(J+1).EQ.1)CALL IGRUNITS(MINX,MINY,MAXX,MAXY)
  IF(ASSF(IASSF)%IAXES(J+1).EQ.2)CALL IGRUNITS(MINX,MIN2Y,MAXX,MAX2Y)

  CALL IGRCOLOURN(GRAPHLINESCOLOUR(J+1))
  CALL IGRLINEWIDTH(GRAPHLINESTHICKNESS(J+1))
  
  IF(PLOTSTYLE.EQ.1)THEN
   K=0
   DO I=1,ASSF(IASSF)%NRASS
    IF(ASSF(IASSF)%MEASURE(J,I).NE.ASSF(IASSF)%NODATA(J+1))THEN
     K =K+1
     X1=ASSF(IASSF)%IDATE(I)
     Y1=ASSF(IASSF)%MEASURE(J,I)
    ENDIF
    IF(K.EQ.1)THEN
     CALL IGRMOVETO(X1,Y1)
    ELSE
     CALL IGRLINETO(X1,Y1)
    ENDIF
   END DO
  ELSEIF(PLOTSTYLE.EQ.2)THEN
   K=0
   DO I=1,ASSF(IASSF)%NRASS
    IF(ASSF(IASSF)%MEASURE(J,I).NE.ASSF(IASSF)%NODATA(J+1))THEN
     K =K+1
     X2=ASSF(IASSF)%IDATE(I)
     Y2=ASSF(IASSF)%MEASURE(J,I)
    ENDIF
    IF(K.EQ.1)THEN
     CALL IGRMOVETO(X2,Y2)
    ELSE
     CALL IGRLINETO(X1,Y1)
     CALL IGRLINETO(X2,Y1)
    ENDIF
    X1=X2; Y1=Y2
   END DO
  ENDIF

  IF(IMARKDATA.EQ.1)THEN
   DO I=1,ASSF(IASSF)%NRASS
    CALL IGRMARKER(REAL(ASSF(IASSF)%IDATE(I)),ASSF(IASSF)%MEASURE(J,I),IMARKER)
   END DO
  ENDIF

!## plot label
!   IF(ABS(IACT).EQ.3)THEN
!    CALL WGRTEXTORIENTATION(ALIGNRIGHT)
!    YL=YL-DYL
!    CALL WGRTEXTSTRING(XL,YL,ATTRIB(J+1))
!   ENDIF
 END DO

 CALL IGRCOLOURN(WRGB(0,0,0))
 CALL IGRBORDER()

 END SUBROUTINE IPFDRAWITOPIC1

 !###===============================================================================
 SUBROUTINE IPFDRAW_DRILLPROF(ILEG,IASSF,XLOC,DX,PLOTSTYLE,DFRAC)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IASSF,PLOTSTYLE,ILEG
 REAL,INTENT(IN) :: XLOC,DX,DFRAC
 INTEGER :: I,ICLR
 REAL :: IWIDTH,MXW,XW 

 IF(ASSF(IASSF)%NRASS.LE.1)RETURN

 SELECT CASE (ASSF(IASSF)%ITOPIC)

  !## measures
  CASE (1)
!   CALL IPFDRAWITOPIC1(IASSF,PLOTSTYLE,IMARKDATA,MINX,MINY,MAXX,MAXY,MIN2Y,MAX2Y)
!   IF(IACT.NE.INT(1,1))CALL IPFDRAWLEGEND(IASSF,MINX,MAXY,ISKIP)
  
   !## draw boreholes
  CASE (2)

   MXW=DX*0.5 
   DO I=1,ASSF(IASSF)%NRASS-1
 
    CALL IPFDRAWITOPIC2_ICLR(I,IASSF,ICLR,IWIDTH); CALL IFFFADEOUT(ICLR,DFRAC)
    XW=MXW*IWIDTH
    CALL IGRFILLPATTERN(SOLID)
    CALL IGRCOLOURN(ICLR)
    CALL IGRRECTANGLE(XLOC-XW,ASSF(IASSF)%Z(I),XLOC+XW,ASSF(IASSF)%Z(I+1))
 
    CALL IGRFILLPATTERN(OUTLINE)
    CALL IGRCOLOURN(WRGB(0,0,0))
    CALL IGRRECTANGLE(XLOC-XW,ASSF(IASSF)%Z(I),XLOC+XW,ASSF(IASSF)%Z(I+1))
   END DO

!   CALL IGRFILLPATTERN(OUTLINE)
!   CALL IGRCOLOURN(WRGB(0,0,0))
!   CALL IGRRECTANGLE(XLOC-XW,ASSF(IASSF)%Z(1),XLOC+XW,ASSF(IASSF)%Z(ASSF(IASSF)%NRASS))

   !## sonderingen
   CASE (3)
    XW=DX*2.0
    CALL IPFDRAWITOPIC3(IASSF,PLOTSTYLE,XLOC,XW)
 END SELECT
 
 END SUBROUTINE IPFDRAW_DRILLPROF
 
 !###===============================================================================
 SUBROUTINE IPFDRAWITOPIC2(ILEG,IASSF,XLOC) 
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IASSF,ILEG
 REAL,INTENT(IN) :: XLOC
 INTEGER :: I,ICLR
 REAL :: XW,MXW,IWIDTH

 MXW=0.5
 DO I=1,ASSF(IASSF)%NRASS-1
  CALL IGRFILLPATTERN(SOLID)

  CALL IPFDRAWITOPIC2_ICLR(I,IASSF,ICLR,IWIDTH)
  CALL IGRCOLOURN(ICLR)
  XW=MXW*IWIDTH
  CALL IGRRECTANGLE(0.5-XW,ASSF(IASSF)%Z(I),0.5+XW,ASSF(IASSF)%Z(I+1))

  CALL IGRFILLPATTERN(OUTLINE)
  CALL IGRCOLOURN(WRGB(0,0,0))
  CALL IGRRECTANGLE(0.5-XW,ASSF(IASSF)%Z(I),0.5+XW,ASSF(IASSF)%Z(I+1))

 END DO
 
! CALL IGRFILLPATTERN(OUTLINE)
! CALL IGRCOLOURN(WRGB(0,0,0))
! CALL IGRRECTANGLE(0.0,ASSF(IASSF)%Z(1),1.0,ASSF(IASSF)%Z(ASSF(IASSF)%NRASS))

 END SUBROUTINE IPFDRAWITOPIC2

 !###===============================================================================
 SUBROUTINE IPFDRAWITOPIC2_ICLR(IROW,IASSF,ICLR,IWIDTH)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IROW,IASSF
 INTEGER,INTENT(OUT) :: ICLR
 REAL,INTENT(OUT) :: IWIDTH
 INTEGER :: I,ICOLUMN,ILEG

 !## unknown/initial colour ... white/grey
 ICLR  =WRGB(250,250,250)
 IWIDTH=1
 ICOLUMN=ASSF(IASSF)%ASSCOL1-1 !## minus one since the first column is read by other parameter
 IF(ICOLUMN.LE.0.OR.ICOLUMN.GT.SIZE(ASSF(IASSF)%L,1))RETURN
 
 ILEG=ASSF(IASSF)%ILEGDLF
 DO I=1,NLITHO(ILEG); IF(TRIM(UTL_CAP(ASSF(IASSF)%L(ICOLUMN,IROW),'U')).EQ.TRIM(UTL_CAP(BH(ILEG,I)%LITHO,'U')))EXIT; END DO
 IF(I.GT.NLITHO(ILEG))RETURN
 
 ICLR  =BH(ILEG,I)%LITHOCLR
 IWIDTH=BH(ILEG,I)%LITHOWIDTH

 END SUBROUTINE IPFDRAWITOPIC2_ICLR

 !###===============================================================================
 SUBROUTINE IPFDRAWITOPIC3(IASSF,PLOTSTYLE,XOFFSET,XSCALE)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IASSF,PLOTSTYLE
 REAL,INTENT(IN) :: XOFFSET,XSCALE
 INTEGER :: I,J
 REAL :: X1,Y1,X2,Y2,MINX,MAXX,MINY,MAXY,XS
 
 CALL IGRFILLPATTERN(OUTLINE)

 !## get scaling whenever xscale.ne.1.0
 IF(XSCALE.NE.0.0)THEN
  MAXX=-10.0E10; MINX=10.0E10
  DO J=2,MIN(MAXCOLOUR,ASSF(IASSF)%NCASS)
   DO I=1,ASSF(IASSF)%NRASS
    IF(ASSF(IASSF)%MEASURE(J,I).NE.ASSF(IASSF)%NODATA(J))THEN
     MINX=MIN(MINX,ASSF(IASSF)%MEASURE(J,I))
     MAXX=MAX(MAXX,ASSF(IASSF)%MEASURE(J,I))
    ENDIF
   ENDDO
  ENDDO
  IF(GRAPHLINESXAXES.EQ.1)THEN
   MINX=GRAPHLINESXMIN; MAXX=GRAPHLINESXMAX
  ENDIF
  IF(GRAPHLINESYAXES.EQ.1)THEN
   MINY=GRAPHLINESYMIN; MAXY=GRAPHLINESYMAX
  ENDIF
  XS=XSCALE/(MAXX-MINX)
 ELSE
  XS=1.0
 ENDIF
 
 DO J=2,MIN(MAXCOLOUR,ASSF(IASSF)%NCASS)

  IF(GRAPHLINESONOFF(J).EQ.0)CYCLE
  CALL IGRCOLOURN(GRAPHLINESCOLOUR(J))
  CALL IGRLINEWIDTH(GRAPHLINESTHICKNESS(J))

  IF(PLOTSTYLE.EQ.1)THEN
   DO I=2,ASSF(IASSF)%NRASS
    IF(ASSF(IASSF)%MEASURE(J,I-1).NE.ASSF(IASSF)%NODATA(J).AND.ASSF(IASSF)%MEASURE(J,I).NE.ASSF(IASSF)%NODATA(J))THEN
     X1=ASSF(IASSF)%MEASURE(J,I-1); Y1=ASSF(IASSF)%MEASURE(1,I-1)
     X2=ASSF(IASSF)%MEASURE(J,I);   Y2=ASSF(IASSF)%MEASURE(1,I)
     X1=X1*XS; X2=X2*XS
     X1=X1+XOFFSET; X2=X2+XOFFSET
     CALL IGRJOIN(X1,Y1,X2,Y2)
    ENDIF
   END DO
  ELSEIF(PLOTSTYLE.EQ.2)THEN
   DO I=2,ASSF(IASSF)%NRASS
    IF(ASSF(IASSF)%MEASURE(J,I-1).NE.ASSF(IASSF)%NODATA(J).AND.ASSF(IASSF)%MEASURE(J,I).NE.ASSF(IASSF)%NODATA(J))THEN
     X1=ASSF(IASSF)%MEASURE(J,I-1); Y1=ASSF(IASSF)%MEASURE(1,I-1)
     X2=ASSF(IASSF)%MEASURE(J,I-1); Y2=ASSF(IASSF)%MEASURE(1,I)     
     X1=X1+XOFFSET; X2=X2+XOFFSET
     X2=X1+((X2-X1)*XS)
     CALL IGRJOIN(X1,Y1,X2,Y2) 
     X1=ASSF(IASSF)%MEASURE(J,I-1); Y1=ASSF(IASSF)%MEASURE(1,I)
     X2=ASSF(IASSF)%MEASURE(J,I);   Y2=ASSF(IASSF)%MEASURE(1,I)     
     X1=X1+XOFFSET; X2=X2+XOFFSET
     X2=X1+((X2-X1)*XSCALE)
     CALL IGRJOIN(X1,Y1,X2,Y2) 
    ENDIF
   END DO
  ENDIF

 END DO

 IF(XSCALE.EQ.0.0)THEN
  CALL IGRCOLOURN(WRGB(25,25,25))
  CALL IGRBORDER()
 ENDIF
 
 END SUBROUTINE IPFDRAWITOPIC3

 !###===============================================================================
 FUNCTION IPFOPENASSFILE(IU,IASSF,FNAME)
 !###===============================================================================
 IMPLICIT NONE
 LOGICAL :: IPFOPENASSFILE
 INTEGER,INTENT(IN) :: IASSF
 INTEGER,INTENT(OUT) :: IU
 CHARACTER(LEN=*),INTENT(IN)  :: FNAME
 LOGICAL :: LEX
 INTEGER :: IOS
 CHARACTER(LEN=50) :: LINE

 IPFOPENASSFILE=.FALSE.

 IU=0
 INQUIRE(FILE=FNAME,EXIST=LEX)
 IF(.NOT.LEX)RETURN
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ,DENYWRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  IU=0
  RETURN
 ENDIF
 ASSF(IASSF)%NRASS=0
 ASSF(IASSF)%NCASS=0
 ASSF(IASSF)%ITOPIC=1
 READ(IU,*,IOSTAT=IOS) ASSF(IASSF)%NRASS
 IF(IOS.NE.0)RETURN
 READ(IU,'(A50)',IOSTAT=IOS) LINE
 READ(LINE,*,IOSTAT=IOS) ASSF(IASSF)%NCASS,ASSF(IASSF)%ITOPIC
 IF(IOS.NE.0)THEN
  READ(LINE,*,IOSTAT=IOS) ASSF(IASSF)%NCASS
  IF(IOS.NE.0)THEN; IU=0; RETURN; ENDIF
  !## default timeseries
  ASSF(IASSF)%ITOPIC=1
 ENDIF
 ASSF(IASSF)%ITOPIC=MAX(1,ASSF(IASSF)%ITOPIC)
 
! ASSF(IASSF)%ASSCOL1=2 !## column used with dlf
! ASSF(IASSF)%ASSCOL2=0 !## on default not used --- border rings
 
 IPFOPENASSFILE=.TRUE.

 END FUNCTION IPFOPENASSFILE

 !###===============================================================================
 LOGICAL FUNCTION IPFREADASSFILELABEL(IU,IASSF,FNAME)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,IASSF
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: I,IOS

 IPFREADASSFILELABEL=.FALSE.

 IF(IU.EQ.0)RETURN

 ALLOCATE(ASSF(IASSF)%ATTRIB(ASSF(IASSF)%NCASS),ASSF(IASSF)%NODATA(ASSF(IASSF)%NCASS))

 IOS=0
 DO I=1,ASSF(IASSF)%NCASS
  READ(IU,*,IOSTAT=IOS) ASSF(IASSF)%ATTRIB(I),ASSF(IASSF)%NODATA(I)
  IF(IOS.NE.0)EXIT
 ENDDO

 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading ATTRIBUTE number: '//TRIM(ITOS(I))//' in:'//CHAR(13)// &
    'file: '//TRIM(FNAME),'Error')
  RETURN
 ENDIF

 ASSF(IASSF)%ATTRIB=ADJUSTL(ASSF(IASSF)%ATTRIB)

 IPFREADASSFILELABEL=.TRUE.

 END FUNCTION IPFREADASSFILELABEL

 !###===============================================================================
 LOGICAL FUNCTION IPFREADASSFILE(IU,IASSF,FNAME)
 !###===============================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER,INTENT(IN) :: IU,IASSF
 INTEGER :: I,J,IDATE,IOS,ITIME
 CHARACTER(LEN=16) :: CDATE
 INTEGER(KIND=8) :: DIDATE
 
 IPFREADASSFILE=.FALSE.

 IF(IU.EQ.0)RETURN
 IF(ASSF(IASSF)%NRASS.LE.0)RETURN

 SELECT CASE (ASSF(IASSF)%ITOPIC)

  !## measures
  CASE(1)

   ALLOCATE(ASSF(IASSF)%MEASURE(ASSF(IASSF)%NCASS-1,ASSF(IASSF)%NRASS))
   ALLOCATE(ASSF(IASSF)%IDATE(ASSF(IASSF)%NRASS))

   DO I=1,ASSF(IASSF)%NRASS
    READ(IU,*,IOSTAT=IOS) CDATE,(ASSF(IASSF)%MEASURE(J,I),J=1,ASSF(IASSF)%NCASS-1); IF(IOS.NE.0)EXIT
    READ(CDATE,'(I8)',IOSTAT=IOS)    IDATE; IF(IOS.NE.0)EXIT
    ASSF(IASSF)%IDATE(I)=REAL(UTL_IDATETOJDATE(IDATE))
    READ(CDATE,'(8X,I8)',IOSTAT=IOS) ITIME
    IF(IOS.EQ.0)ASSF(IASSF)%IDATE(I)=ASSF(IASSF)%IDATE(I)+ITIMETOFTIME(ITIME)
   ENDDO

  !## drills
  CASE (2)

   IF(ASSOCIATED(ASSF(IASSF)%Z))DEALLOCATE(ASSF(IASSF)%Z)
   IF(ASSOCIATED(ASSF(IASSF)%L))DEALLOCATE(ASSF(IASSF)%L)
   ALLOCATE(ASSF(IASSF)%Z(ASSF(IASSF)%NRASS),ASSF(IASSF)%L(ASSF(IASSF)%NCASS-1,ASSF(IASSF)%NRASS))

   DO I=1,ASSF(IASSF)%NRASS
    READ(IU,*,IOSTAT=IOS) ASSF(IASSF)%Z(I),(ASSF(IASSF)%L(J,I),J=1,ASSF(IASSF)%NCASS-1)
    IF(IOS.NE.0)EXIT
   ENDDO

  !## sondering
  CASE (3)

   ALLOCATE(ASSF(IASSF)%MEASURE(ASSF(IASSF)%NCASS,ASSF(IASSF)%NRASS))

   DO I=1,ASSF(IASSF)%NRASS
    READ(IU,*,IOSTAT=IOS) (ASSF(IASSF)%MEASURE(J,I),J=1,ASSF(IASSF)%NCASS)
    IF(IOS.NE.0)EXIT
    IF(ASSF(IASSF)%MEASURE(4,I).NE.ASSF(IASSF)%NODATA(4))ASSF(IASSF)%MEASURE(4,I)=-1.0*ASSF(IASSF)%MEASURE(4,I)
   ENDDO

 END SELECT

 !## error occured let calling routine handle error
 IF(IOS.NE.0)THEN
!  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading DATA record number: '//TRIM(ITOS(I))//' in:'//CHAR(13)// &
!    'file: '//TRIM(FNAME),'Error')
!  RETURN
  ASSF(IASSF)%NRASS=I-1
 ENDIF

 IPFREADASSFILE=.TRUE.

 END FUNCTION IPFREADASSFILE

 !###===============================================================================
 SUBROUTINE IPFINITASSFILE()
 !###===============================================================================
 IMPLICIT NONE
 INTEGER :: I,J,ILEG
 
 CALL IPFCLOSEASSFILE()

 DO I=1,SIZE(BH,1)

  ILEG=I

  IF(NLITHO(ILEG).GT.0)CYCLE
 
  IF(I.EQ.1)THEN
   !## try default legend ... prefval(1)\legend\drill.dlf
   CALL IPFGETVALUE_OPENSAVECOLOURS(TRIM(PREFVAL(1))//'\SETTINGS\DRILL.DLF',ID_OPEN,0,ILEG) !## no dialog necessary for opening
   IF(NLITHO(ILEG).GT.0)CYCLE
   !## try default legend ... prefval(1)\legend\drill1.dlf
   CALL IPFGETVALUE_OPENSAVECOLOURS(TRIM(PREFVAL(1))//'\SETTINGS\DRILL'//TRIM(ITOS(I))//'.DLF',ID_OPEN,0,ILEG) !## no dialog necessary for opening
   IF(NLITHO(ILEG).GT.0)CYCLE
  ELSE
   !## try default legend ... prefval(1)\legend\drill1...10.dlf
   CALL IPFGETVALUE_OPENSAVECOLOURS(TRIM(PREFVAL(1))//'\SETTINGS\DRILL'//TRIM(ITOS(I))//'.DLF',ID_OPEN,0,ILEG) !## no dialog necessary for opening
  ENDIF
  
  IF(NLITHO(ILEG).GT.0)CYCLE

  DO J=1,SIZE(BH,2)
   BH(ILEG,J)%LITHO=''
   BH(ILEG,J)%LITHOCLR =WRGB(255,255,255)
   BH(ILEG,J)%LITHOWIDTH =1.0 
  ENDDO
   
  BH(ILEG,1)%LITHO ='S'
  BH(ILEG,2)%LITHO ='SS'
  BH(ILEG,3)%LITHO ='G'
  BH(ILEG,4)%LITHO ='C'
  BH(ILEG,5)%LITHO ='P'
  BH(ILEG,6)%LITHO ='L'
  BH(ILEG,7)%LITHO ='SST'
  BH(ILEG,8)%LITHO ='LST'

  BH(ILEG,1)%LITHOCLR =WRGB(255,255,0)
  BH(ILEG,2)%LITHOCLR =WRGB(255,255,128)
  BH(ILEG,3)%LITHOCLR =WRGB(218,165,32)
  BH(ILEG,4)%LITHOCLR =WRGB(0,128,0)
  BH(ILEG,5)%LITHOCLR =WRGB(255,0,255)
  BH(ILEG,6)%LITHOCLR =WRGB(238,130,238)
  BH(ILEG,7)%LITHOCLR =WRGB(0,255,255)
  BH(ILEG,8)%LITHOCLR =WRGB(192,192,192)
 
  BH(ILEG,1)%LITHOTXT ='Sand'
  BH(ILEG,2)%LITHOTXT ='Silty Sand'
  BH(ILEG,3)%LITHOTXT ='Gravel'
  BH(ILEG,4)%LITHOTXT ='Clay'
  BH(ILEG,5)%LITHOTXT ='Peat'
  BH(ILEG,6)%LITHOTXT ='Loam'
  BH(ILEG,7)%LITHOTXT='Sandstone'
  BH(ILEG,8)%LITHOTXT='Limestone'

  BH(ILEG,1)%LITHOWIDTH =0.75 
  BH(ILEG,2)%LITHOWIDTH =0.6
  BH(ILEG,3)%LITHOWIDTH =1.0 
  BH(ILEG,4)%LITHOWIDTH =0.3
  BH(ILEG,5)%LITHOWIDTH =0.5
  BH(ILEG,6)%LITHOWIDTH =0.4
  BH(ILEG,7)%LITHOWIDTH =0.2
  BH(ILEG,8)%LITHOWIDTH =0.1
 
  NLITHO(ILEG)=8

 ENDDO
 
 END SUBROUTINE IPFINITASSFILE

 !###===============================================================================
 SUBROUTINE IPFCLOSEASSFILE()
 !###===============================================================================
 IMPLICIT NONE
 INTEGER :: I

 IF(.NOT.ALLOCATED(ASSF))RETURN

 DO I=1,SIZE(ASSF)

  IF(ASSOCIATED(ASSF(I)%ATTRIB))DEALLOCATE(ASSF(I)%ATTRIB);   NULLIFY(ASSF(I)%ATTRIB)
  IF(ASSOCIATED(ASSF(I)%NODATA))DEALLOCATE(ASSF(I)%NODATA);   NULLIFY(ASSF(I)%NODATA)
  IF(ASSOCIATED(ASSF(I)%MEASURE))DEALLOCATE(ASSF(I)%MEASURE); NULLIFY(ASSF(I)%MEASURE)
  IF(ASSOCIATED(ASSF(I)%IDATE))DEALLOCATE(ASSF(I)%IDATE);     NULLIFY(ASSF(I)%IDATE)
!  IF(ASSOCIATED(ASSF(I)%ITIME))DEALLOCATE(ASSF(I)%ITIME);     NULLIFY(ASSF(I)%ITIME)
  IF(ASSOCIATED(ASSF(I)%L))DEALLOCATE(ASSF(I)%L);             NULLIFY(ASSF(I)%L)
  IF(ASSOCIATED(ASSF(I)%Z))DEALLOCATE(ASSF(I)%Z);             NULLIFY(ASSF(I)%Z)

 END DO
 DEALLOCATE(ASSF)

 END SUBROUTINE IPFCLOSEASSFILE

 !###===============================================================================
 SUBROUTINE IPFASSFILEALLOCATE(N)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER :: I
 
 IF(ALLOCATED(ASSF))RETURN
 ALLOCATE(ASSF(N))
 
 DO I=1,SIZE(ASSF)

  NULLIFY(ASSF(I)%ATTRIB)
  NULLIFY(ASSF(I)%NODATA)
  NULLIFY(ASSF(I)%MEASURE)
  NULLIFY(ASSF(I)%IDATE)
!  NULLIFY(ASSF(I)%ITIME)
  NULLIFY(ASSF(I)%L)
  NULLIFY(ASSF(I)%Z)

 END DO

 END SUBROUTINE IPFASSFILEALLOCATE

END MODULE MOD_IPFASSFILE

