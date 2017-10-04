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
MODULE MOD_ISG_PLOT

USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : PI
USE MODPLOT
USE MOD_ISG_PAR, ONLY : ISGIU,NISG,ISG,ISP,IST,ISC,ISD,ISQ,DATISD,ICLRSP,ICLRND,ICLRSF,ICLRSD,ICLRSC, &
                        ICLRST,ICLRQH,ICLRCO,ISFR,NISP,ISGFNAME,ISGLEG !,SDATE,EDATE
USE MOD_OSD, ONLY : ICF
USE MOD_UTL, ONLY : UTL_CAP,INVERSECOLOUR,UTL_WAITMESSAGE,UTL_MESSAGEHANDLE,UTL_DIST, &
                    UTL_IDFGETCLASS,UTL_IDATETOJDATE,IDFPLOT1BITMAP,IDFPLOT2BITMAP
USE MOD_ISG_UTL, ONLY : ISGCLOSEFILES,ISGOPENFILES
USE MOD_ISG_GRID, ONLY : ISGGRIDGETSTREAMDATA,ISGGRIDINTSTREAMDATA

CONTAINS

 !###======================================================================
 SUBROUTINE ISGPLOTMAIN(IPLOT,XMIN,YMIN,XMAX,YMAX)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
 INTEGER,INTENT(IN) :: IPLOT
 REAL :: X1,Y1,X2,Y2
 LOGICAL :: LEX,LOPEN

 CALL ISGPLOTOPENFILES(LEX,LOPEN,MP(IPLOT)%IDFNAME,'OLD')
 IF(.NOT.LOPEN)RETURN

 CALL IGRFILLPATTERN(SOLID)
 CALL WINDOWSELECT(0)
 CALL WINDOWOUTSTATUSBAR(4,'Drawing ISG ...')
 CALL WCURSORSHAPE(CURHOURGLASS)

 X1=XMIN; Y1=YMIN; X2=XMAX; Y2=YMAX

 CALL ISGPLOT(X1,Y1,X2,Y2,IPLOT)

 IF(.NOT.LEX)CALL ISGCLOSEFILES()

 CALL IGRCOLOURN(WRGB(255,255,255)) 
 CALL IGRFILLPATTERN(OUTLINE)
 CALL WINDOWOUTSTATUSBAR(2,'')
 CALL WINDOWOUTSTATUSBAR(4,'')
 CALL WCURSORSHAPE(CURARROW)

 DRWLIST(IPLOT)=1

 END SUBROUTINE ISGPLOTMAIN

 !###===============================================================================
 SUBROUTINE ISGPLOTMINMAX(FNAME,XMIN,XMAX,YMIN,YMAX)
 !###===============================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 REAL,INTENT(INOUT) :: XMIN,YMIN,XMAX,YMAX
 CHARACTER(LEN=50) :: CID
 INTEGER :: I,J,ISEG,NSEG
 LOGICAL :: LEX,LOPEN
 REAL :: X,Y

 CALL ISGPLOTOPENFILES(LEX,LOPEN,FNAME,'OLD'); IF(.NOT.LOPEN)RETURN

 IF(XMIN.EQ.XMAX.AND.YMIN.EQ.YMAX)THEN
  XMIN=10.0E10; YMIN=10.0E10; XMAX=-10.0E10; YMAX=-10.0E10
 ENDIF

 !## if not opened by isgedit
 IF(.NOT.LEX)THEN
  READ(ISGIU(1,1),*) NISG
  DO I=1,NISG
   READ(ISGIU(1,1),*) CID,ISEG,NSEG
   !## read segments
   ISEG=ISEG-1
   DO J=1,NSEG
    READ(ISGIU(2,1),REC=ISEG+J+ICF) X,Y
    XMIN=MIN(X,XMIN); XMAX=MAX(X,XMAX)
    YMIN=MIN(Y,YMIN); YMAX=MAX(Y,YMAX)
   END DO
  END DO
  CALL ISGCLOSEFILES()
 ELSE
  XMIN=MINVAL(ISP(1:NISP)%X); XMAX=MAXVAL(ISP(1:NISP)%X)
  YMIN=MINVAL(ISP(1:NISP)%Y); YMAX=MAXVAL(ISP(1:NISP)%Y)
 ENDIF

 !## nothing yet filled in in ISG
 IF(XMIN.GT.XMAX.OR.YMIN.GT.YMAX)THEN
  XMIN=-1.0; YMIN=-1.0; XMAX=1.0; YMAX=1.0
 ENDIF

 END SUBROUTINE ISGPLOTMINMAX

 !###======================================================================
 SUBROUTINE ISGPLOT(XMIN,YMIN,XMAX,YMAX,IPLOT)
 !###======================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: SCALEXY=1.0/500.0
 INTEGER,PARAMETER :: TFONT=FFHELVETICA
 REAL,PARAMETER :: TSIZE=0.5
 INTEGER,INTENT(IN) :: IPLOT
 REAL,INTENT(INOUT) :: XMIN,YMIN,XMAX,YMAX
 INTEGER :: I,J,NSEG,NCRS,IREC,IQHR,IFDR,IFCO,ISTW,NSTW,IISGLABEL,DWNSEG,  &
            DIMCRS,INDS,ISND,ICRS,DIMCLC,NCLC,ICLC,IISG,DIMSTW,NQHR,DIMQHR,IUPSEG, &
            IREF,JREC,MSEG,IP,N
 INTEGER,ALLOCATABLE,DIMENSION(:) :: ID
 REAL :: DX,X1,Y1,X,Y,TWIDTH,THEIGHT,XPOS,YPOS,DIST
 REAL,DIMENSION(:),ALLOCATABLE :: XSEG,YSEG
 REAL,DIMENSION(4) :: TDIST
 CHARACTER(LEN=50) :: CID
 LOGICAL :: LEX,LOPEN
 TYPE OBJ
  CHARACTER(LEN=32) :: CNAME
  REAL :: DIST
  INTEGER :: N
 END TYPE OBJ
 TYPE(OBJ),ALLOCATABLE,DIMENSION(:) :: CRS,CLC,STW,QHR
 IF(ALLOCATED(ID))DEALLOCATE(ID)

 !## whether isg is opened or will be drawn from memory
 LOPEN=.TRUE.; IF(.NOT.ALLOCATED(ISGIU))LOPEN=.FALSE.

 DX=(XMAX-XMIN)*SCALEXY

 IF(MP(IPLOT)%SYMBOL.LT.0.OR.MP(IPLOT)%SYMBOL.GT.7)MP(IPLOT)%SYMBOL=0

 IISG=WMENUGETSTATE(ID_ISGEDIT,2); IISGLABEL=0
 IF(IISG.EQ.1)THEN
  CALL WDIALOGSELECT(ID_DISGEDIT)
  CALL WDIALOGGETCHECKBOX(IDF_CHECK7,IISG)
  CALL WDIALOGGETCHECKBOX(IDF_CHECK8,IISGLABEL)
  CALL WDIALOGSELECT(ID_DISGEDITTAB1)
  IF(IISG.EQ.1)CALL WDIALOGGETMENU(IDF_MENU1,ISG(1:NISG)%ILIST)
 ENDIF

 TWIDTH =0.013*TSIZE
 THEIGHT=0.04*TSIZE
 CALL WGRTEXTFONT(TFONT,WIDTH=TWIDTH,HEIGHT=THEIGHT,ISTYLE=0)
 CALL WGRTEXTORIENTATION(ALIGNLEFT)

 CALL WINDOWSELECT(0)
 INDS=WMENUGETSTATE(ID_ISGNODES,2)
 ISND=WMENUGETSTATE(ID_ISGSEGNODES,2)
 ICRS=WMENUGETSTATE(ID_ISGCRSSCTNS,2)
 ICLC=WMENUGETSTATE(ID_ISGCLCNODES,2)
 ISTW=WMENUGETSTATE(ID_ISGSTUWEN,2)
 IQHR=WMENUGETSTATE(ID_ISGQHR,2)
 IFDR=WMENUGETSTATE(ID_ISGSFR,2)
 IFCO=WMENUGETSTATE(ID_ISGSFC,2)

 ALLOCATE(ID(5))     !## id numbers

 IF(LOPEN)THEN

  DIMCRS=0; DIMCLC=0; DIMSTW=0; DIMQHR=0

  READ(ISGIU(1,1),*) N 

  DO I=1,N

   !## read from isg-files
   READ(ISGIU(1,1),*) CID,ID(1),NSEG,ID(2),NCLC,ID(3),NCRS,ID(4),NSTW,ID(5),NQHR

   !## allocate memory for plotting atrributes
   IF(NCLC.GT.DIMCLC)THEN
    IF(ALLOCATED(CLC))DEALLOCATE(CLC)
    DIMCLC=NCLC; ALLOCATE(CLC(DIMCLC))
   ENDIF
   IF(NCRS.GT.DIMCRS)THEN
    IF(ALLOCATED(CRS))DEALLOCATE(CRS)
    DIMCRS=NCRS; ALLOCATE(CRS(DIMCRS))
   ENDIF
   IF(NSTW.GT.DIMSTW)THEN
    IF(ALLOCATED(STW))DEALLOCATE(STW)
    DIMSTW=NSTW; ALLOCATE(STW(DIMSTW))
   ENDIF
   IF(NQHR.GT.DIMQHR)THEN
    IF(ALLOCATED(QHR))DEALLOCATE(QHR)
    DIMQHR=NQHR; ALLOCATE(QHR(DIMQHR))
   ENDIF

   !## read calculation point information from *.isd1,*.isd2
   IREC=ID(2)-1
   DO J=1,NCLC
    READ(ISGIU(3,1),REC=IREC+J+ICF) CLC(J)%N,IP,CLC(J)%DIST,CLC(J)%CNAME
   END DO
   !## read cross-section information from *.isc1,*.isc2
   IREC=ID(3)-1
   DO J=1,NCRS
    READ(ISGIU(5,1),REC=IREC+J+ICF) CRS(J)%N,IP,CRS(J)%DIST,CRS(J)%CNAME
   END DO
   !## read structure information from *.ist1,*.ist2
   IREC=ID(4)-1
   DO J=1,NSTW
    READ(ISGIU(7,1),REC=IREC+J+ICF) STW(J)%N,IP,STW(J)%DIST,STW(J)%CNAME
   END DO
   !## read qh information from *.isq1,*.isq2
   IREC=ID(5)-1
   DO J=1,NQHR
    READ(ISGIU(9,1),REC=IREC+J+ICF) QHR(J)%N,IP,QHR(J)%DIST,QHR(J)%CNAME
   END DO

   !## initialize tdist for all!
   TDIST=0.0

   !## read segments
   IREC=ID(1)-1
   DO J=1,NSEG
    READ(ISGIU(2,1),REC=IREC+J+ICF) X,Y

    IF(J.EQ.1)THEN
     X1=X
     Y1=Y
    ENDIF
    LEX=.TRUE.
    IF(IISG.EQ.1)THEN
     LEX=.FALSE.
     IF(ISG(I)%ILIST.EQ.1)LEX=.TRUE.
    ENDIF

    IF(LEX)THEN
     !## draw squares - begin node
     IF(X.LT.XMAX.AND.X.GT.XMIN.AND.Y.GT.YMIN.AND.Y.LT.YMAX)THEN

      !## draw circles - nodes
      IF(ISND.EQ.1)THEN
       IF(X.LT.XMAX.AND.X.GT.XMIN.AND.Y.GT.YMIN.AND.Y.LT.YMAX)THEN
        CALL IGRCOLOURN(ICLRSP)
        CALL IGRCIRCLE(X,Y,DX)
       ENDIF
      ENDIF

      IF(INDS.EQ.1.AND.(J.EQ.1.OR.J.EQ.NSEG))THEN
       CALL IGRCOLOURN(ICLRND)
       CALL IGRRECTANGLE(X-DX,Y-DX,X+DX,Y+DX)
       CALL ISGTEXTANGLE(X-X1,Y-Y1,-90.0,-3.0*DX,ALIGNRIGHT,XPOS,YPOS)
       IF(J.EQ.NSEG.AND.IISGLABEL.EQ.1)CALL WGRTEXTSTRING(X+XPOS,Y+YPOS,'[TN] '//TRIM(CID))
      ENDIF
      IF(INDS.EQ.1.AND.J.EQ.2.AND.IISGLABEL.EQ.1)THEN
       CALL ISGTEXTANGLE(X-X1,Y-Y1,-90.0,-3.0*DX,ALIGNRIGHT,XPOS,YPOS)
       CALL WGRTEXTSTRING(X1+XPOS,Y1+YPOS,'[FN] '//TRIM(CID))
      ENDIF
     ENDIF

     IF(J.GT.1)THEN
      IF(MIN(X1,X).LT.XMAX.AND.MAX(X1,X).GT.XMIN.AND. &
         MIN(Y1,Y).LT.YMAX.AND.MAX(Y1,Y).GT.YMIN)THEN
       !## draw calculation-nodes on segment
       IF(NCLC.GT.0)THEN
        IF(ICLC.EQ.1)CALL ISGPLOTLOCCRS(X1,X,Y1,Y,DX,TDIST(1),DIMCLC,CLC%DIST,CLC%CNAME,CLC%N,NCLC,1,IISGLABEL)
       ENDIF
       !## plot cross-sections on segment
       IF(NCRS.GT.0)THEN
        IF(ICRS.EQ.1)CALL ISGPLOTLOCCRS(X1,X,Y1,Y,DX,TDIST(2),DIMCRS,CRS%DIST,CRS%CNAME,CRS%N,NCRS,2,IISGLABEL)
       ENDIF
       !## plot weirs on segment
       IF(NSTW.GT.0)THEN
        IF(ISTW.EQ.1)CALL ISGPLOTLOCCRS(X1,X,Y1,Y,DX,TDIST(3),DIMSTW,STW%DIST,STW%CNAME,STW%N,NSTW,3,IISGLABEL)
       ENDIF
       !## plot weirs on segment
       IF(NQHR.GT.0)THEN
        IF(IQHR.EQ.1)CALL ISGPLOTLOCCRS(X1,X,Y1,Y,DX,TDIST(4),DIMQHR,QHR%DIST,QHR%CNAME,QHR%N,NQHR,4,IISGLABEL)
       ENDIF
      ELSE
       DIST =SQRT((X1-X)**2.0+(Y1-Y)**2.0)
       TDIST=TDIST+DIST
      ENDIF
     ENDIF
    ENDIF

    !## draw vertex
    IF(J.GT.1)THEN
     IF(MIN(X1,X).LT.XMAX.AND.MAX(X1,X).GT.XMIN.AND. &
        MIN(Y1,Y).LT.YMAX.AND.MAX(Y1,Y).GT.YMIN)THEN
      CALL IGRLINETYPE(MP(IPLOT)%SYMBOL)
      CALL IGRLINEWIDTH(MP(IPLOT)%THICKNESS)
      CALL IGRCOLOURN(MP(IPLOT)%SCOLOR)
      CALL IGRJOIN(X1,Y1,X,Y)
      CALL IGRLINEWIDTH(1)
     ENDIF
    ENDIF

    X1=X; Y1=Y

   END DO

   !## plot flow directions
   IF(IFDR.EQ.1)THEN
    CALL IGRCOLOURN(ICLRSF); CALL IGRFILLPATTERN(SOLID)
    !## get correct record-number for coordinates
    IREC=ID(1)-1
    !## read segments
    IF(ALLOCATED(XSEG))THEN; IF(SIZE(XSEG).LT.NSEG)DEALLOCATE(XSEG,YSEG); ENDIF
    IF(.NOT.ALLOCATED(XSEG))ALLOCATE(XSEG(NSEG),YSEG(NSEG))
    DO J=1,NSEG; READ(ISGIU(2,1),REC=IREC+J+ICF) XSEG(J),YSEG(J); ENDDO
    CALL ISGPLOT_FDIRECTION(XSEG,YSEG,NSEG,XMIN,XMAX,YMIN,YMAX,0)
   ENDIF

  END DO

  IF(ALLOCATED(XSEG))DEALLOCATE(XSEG); IF(ALLOCATED(YSEG))DEALLOCATE(YSEG)
  IF(ALLOCATED(CRS))DEALLOCATE(CRS);   IF(ALLOCATED(CLC))DEALLOCATE(CLC)
  IF(ALLOCATED(STW))DEALLOCATE(STW);   IF(ALLOCATED(QHR))DEALLOCATE(QHR)

 ELSE

  DO I=1,NISG

   CID  =ISG(I)%SNAME
   ID(1)=ISG(I)%ISEG
   ID(2)=ISG(I)%ICLC
   ID(3)=ISG(I)%ICRS
   ID(4)=ISG(I)%ISTW
   ID(5)=ISG(I)%IQHR
   NSEG =ISG(I)%NSEG
   NCLC =ISG(I)%NCLC
   NCRS =ISG(I)%NCRS
   NSTW =ISG(I)%NSTW
   NQHR =ISG(I)%NQHR

   !## initialize tdist for all!
   TDIST=0.0

   LEX=.TRUE.; IF(IISG.EQ.1)THEN
    IF(ISG(I)%ILIST.NE.1)LEX=.FALSE.
   ENDIF
   IF(LEX)THEN

    !## plot flow directions
    IF(IFDR.EQ.1)THEN
     CALL IGRCOLOURN(ICLRSF); CALL IGRFILLPATTERN(SOLID)
     !## get correct record-number for coordinates
     IREC=ISG(I)%ISEG; CALL ISGPLOT_FDIRECTION(ISP(IREC:IREC+NSEG-1)%X,ISP(IREC:IREC+NSEG-1)%Y,NSEG,XMIN,XMAX,YMIN,YMAX,0)
    ENDIF
   
    !## get connection-number
    IF(ISFR.EQ.1.AND.IFCO.EQ.1)THEN
        
     !## get upstream connection (first entry)
     IREC=ISG(I)%ICLC; IREF=ISD(IREC)%IREF
     !## use first defined entry for connection
     IUPSEG=DATISD(IREF)%UPSG
     IF(IUPSEG.GT.0.AND.IUPSEG.LE.SIZE(ISG))THEN
      IREC=ISG(I)%ISEG 
      JREC=ISG(IUPSEG)%ISEG; MSEG=ISG(IUPSEG)%NSEG
      CALL ISGPLOT_FCONNECTION(ISP(IREC:IREC+NSEG-1)%X,ISP(IREC:IREC+NSEG-1)%Y,NSEG, &
                               ISP(JREC:JREC+MSEG-1)%X,ISP(JREC:JREC+MSEG-1)%Y,MSEG, &
                               XMIN,XMAX,YMIN,YMAX,-1,0)
     ENDIF

     !## get downstream connection (second entry)
     IREC=ISG(I)%ICLC; IREF=ISD(IREC)%IREF+1
     !## use first defined entry for connection
     DWNSEG=DATISD(IREF)%DWNS
     IF(DWNSEG.GT.0.AND.DWNSEG.LE.SIZE(ISG))THEN
      IREC=ISG(I)%ISEG
      JREC=ISG(DWNSEG)%ISEG; MSEG=ISG(DWNSEG)%NSEG
      CALL ISGPLOT_FCONNECTION(ISP(IREC:IREC+NSEG-1)%X,ISP(IREC:IREC+NSEG-1)%Y,NSEG, &
                               ISP(JREC:JREC+MSEG-1)%X,ISP(JREC:JREC+MSEG-1)%Y,MSEG, &
                               XMIN,XMAX,YMIN,YMAX,1,0)
     ENDIF
    ENDIF
          
   ENDIF

   !## read segments
   DO J=1,NSEG

    !## get correct record-number for coordinates
    IREC=ID(1)-1

    X=ISP(IREC+J)%X
    Y=ISP(IREC+J)%Y

    !## initialize variable
    IF(J.EQ.1)THEN
     X1=X
     Y1=Y
    ENDIF

    LEX=.TRUE.
    IF(IISG.EQ.1)THEN
     IF(ISG(I)%ILIST.NE.1)LEX=.FALSE. 
    ENDIF

    IF(LEX)THEN
     !## draw squares - begin node
     IF(X.LT.XMAX.AND.X.GT.XMIN.AND.Y.GT.YMIN.AND.Y.LT.YMAX)THEN

      !## draw circles - nodes
      IF(ISND.EQ.1)THEN
       CALL IGRCOLOURN(ICLRSP)
       CALL IGRCIRCLE(X,Y,DX)
      ENDIF

      IF(INDS.EQ.1.AND.(J.EQ.1.OR.J.EQ.NSEG))THEN
       CALL IGRCOLOURN(ICLRND)
       CALL IGRRECTANGLE(X-DX,Y-DX,X+DX,Y+DX)
       CALL ISGTEXTANGLE(X-X1,Y-Y1,-90.0,-3.0*DX,ALIGNRIGHT,XPOS,YPOS)
       IF(J.EQ.NSEG.AND.IISGLABEL.EQ.1)CALL WGRTEXTSTRING(X+XPOS,Y+YPOS,'[TN] '//TRIM(CID))
      ENDIF
      IF(INDS.EQ.1.AND.J.EQ.2.AND.IISGLABEL.EQ.1)THEN
       CALL ISGTEXTANGLE(X-X1,Y-Y1,-90.0,-3.0*DX,ALIGNRIGHT,XPOS,YPOS)
       CALL WGRTEXTSTRING(X1+XPOS,Y1+YPOS,'[FN] '//TRIM(CID))
      ENDIF
     ENDIF

     IF(J.GT.1)THEN
      IF(MIN(X1,X).LT.XMAX.AND.MAX(X1,X).GT.XMIN.AND. &
         MIN(Y1,Y).LT.YMAX.AND.MAX(Y1,Y).GT.YMIN)THEN
       !## draw calculation-nodes on segment
       IF(NCLC.GT.0)THEN
        IF(ICLC.EQ.1)CALL ISGPLOTLOCCRS(X1,X,Y1,Y,DX,TDIST(1),NCLC,ISD(ID(2):ID(2)+NCLC-1)%DIST, &
                                        ISD(ID(2):ID(2)+NCLC-1)%CNAME,ISD(ID(2):ID(2)+NCLC-1)%N,NCLC,1,IISGLABEL)
       ENDIF
       !## plot cross-sections on segment
       IF(NCRS.GT.0)THEN
        IF(ICRS.EQ.1)CALL ISGPLOTLOCCRS(X1,X,Y1,Y,DX,TDIST(2),NCRS,ISC(ID(3):ID(3)+NCRS-1)%DIST, &
                                        ISC(ID(3):ID(3)+NCRS-1)%CNAME,ISC(ID(3):ID(3)+NCRS-1)%N,NCRS,2,IISGLABEL)
       ENDIF
       !## plot weirs on segment
       IF(NSTW.GT.0)THEN
        IF(ISTW.EQ.1)CALL ISGPLOTLOCCRS(X1,X,Y1,Y,DX,TDIST(3),NSTW,IST(ID(4):ID(4)+NSTW-1)%DIST, &
                                        IST(ID(4):ID(4)+NSTW-1)%CNAME,IST(ID(4):ID(4)+NSTW-1)%N,NSTW,3,IISGLABEL)
       ENDIF
       !## plot qh-relationships on segment
       IF(NQHR.GT.0)THEN
        IF(IQHR.EQ.1)CALL ISGPLOTLOCCRS(X1,X,Y1,Y,DX,TDIST(4),NQHR,ISQ(ID(5):ID(5)+NQHR-1)%DIST, &
                                        ISQ(ID(5):ID(5)+NQHR-1)%CNAME,ISQ(ID(5):ID(5)+NQHR-1)%N,NQHR,4,IISGLABEL)
       ENDIF
      ELSE
       DIST =SQRT((X1-X)**2.0+(Y1-Y)**2.0)
       TDIST=TDIST+DIST
      ENDIF
     ENDIF
    ENDIF

    !## draw vertex
    IF(J.GT.1)THEN
     IF(MIN(X1,X).LT.XMAX.AND.MAX(X1,X).GT.XMIN.AND. &
        MIN(Y1,Y).LT.YMAX.AND.MAX(Y1,Y).GT.YMIN)THEN
      CALL IGRLINETYPE(MP(IPLOT)%SYMBOL)
      CALL IGRCOLOURN(MP(IPLOT)%SCOLOR)
      CALL IGRLINEWIDTH(MP(IPLOT)%THICKNESS)
      CALL IGRJOIN(X1,Y1,X,Y)
      CALL IGRLINEWIDTH(1)
     ENDIF
    ENDIF

    X1=X; Y1=Y

   END DO

  END DO
 
  CALL ISGPLOT_LEGENDLINES(XMIN,YMIN,XMAX,YMAX)

  !## clear previous selected list
  ISG%JLIST=0

  !## plot selected segments
  CALL ISGPLOT_DRAWSELECTEDSEGMENTS()
 
 ENDIF

 IF(ALLOCATED(ID))DEALLOCATE(ID)
 CALL WGRTEXTORIENTATION(ALIGNLEFT,0.0,DIRHORIZ,ALIGNLEFT)
 
 END SUBROUTINE ISGPLOT

 !###======================================================================
 SUBROUTINE ISGPLOT_DRAWSELECTEDSEGMENTS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: II,I
 
 CALL IDFPLOT1BITMAP(); CALL IGRPLOTMODE(MODEXOR)

 CALL IGRLINEWIDTH(5); CALL IGRCOLOURN(WRGB(255,0,0))

 !## remove previous selected
 DO II=1,2
  DO I=1,NISG
   !## skip non-selected segments
   IF(II.EQ.1)THEN
    IF(ISG(I)%JLIST.EQ.0)CYCLE
   ELSE
    IF(ISG(I)%ILIST.EQ.0)CYCLE
   ENDIF
   CALL ISGPLOT_DRAWREACHES(I)
  ENDDO
 ENDDO
 ISG%JLIST=ISG%ILIST
  
 CALL IGRLINEWIDTH(1)
 CALL IDFPLOT2BITMAP(); CALL IGRPLOTMODE(MODECOPY)

 END SUBROUTINE ISGPLOT_DRAWSELECTEDSEGMENTS
 
 !###====================================================================
 SUBROUTINE ISGPLOT_DRAWREACHES(IISG)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IISG
 INTEGER :: J,I
  
 J=ISG(IISG)%ISEG; CALL IGRMOVETO(ISP(J)%X,ISP(J)%Y)

 DO I=2,ISG(IISG)%NSEG
  J=J+1; CALL IGRLINETO(ISP(J)%X,ISP(J)%Y)
 ENDDO

 END SUBROUTINE ISGPLOT_DRAWREACHES

 !###====================================================================
 SUBROUTINE ISGPLOT_LEGENDLINES(XMIN,YMIN,XMAX,YMAX)
 !###====================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
 INTEGER :: I,J,K,IREC,JREC,MAXNSEG,TTIME,NSEG,NDIM,ICLR, &
            IVAR,ILINEW,ILINET,ISTEADY,IISG,IH,IM,IS,IOS, &
            SDATE,EDATE,IAVERAGE
 LOGICAL :: LEX
 REAL :: X1,Y1,X2,Y2
 REAL,DIMENSION(:),ALLOCATABLE :: X,Y,DIST,XNR,NDATA
 REAL,DIMENSION(:,:),ALLOCATABLE :: RVAL,QSORT
 INTEGER,DIMENSION(:),ALLOCATABLE :: IPOS
 CHARACTER(LEN=20) :: STRING
  
 !## legend plotting not active
 CALL WDIALOGSELECT(ID_DISGEDITTAB1)
 IF(WINFODIALOGFIELD(ID_LEGEND,FIELDSTATE).EQ.1)RETURN

 CALL WDIALOGSELECT(ID_DISGEDITLEGEND)

 !## get date ...
 CALL WDIALOGGETMENU(IDF_MENU2,I,STRING)
 READ(STRING,'(I8)',IOSTAT=IOS) SDATE
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot read a proper DATE out of the string: '//TRIM(STRING),'Warning')
  RETURN
 ENDIF
 !## try to get time ...
 IF(LEN_TRIM(STRING).GT.8)THEN
  READ(STRING,'(9X,3(I2,1X))',IOSTAT=IOS) IH,IM,IS
  IF(IOS.NE.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot read a proper TIME out of the string: '//TRIM(STRING),'Warning')
   RETURN
  ENDIF
 ENDIF
 
 !## translate cdate in to julian date - for transient simulations only!
 SDATE=UTL_IDATETOJDATE(SDATE)
 EDATE=SDATE+1
 TTIME=EDATE-SDATE
 
 !## get window/selection
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO3,IISG)

 CALL WDIALOGGETMENU(IDF_MENU4,IVAR)
 CALL WDIALOGGETMENU(IDF_MENU5,ILINEW)
 CALL WDIALOGGETMENU(IDF_MENU6,ILINET); ILINET=ILINET-1

 IF(ALLOCATED(X))DEALLOCATE(X);       IF(ALLOCATED(Y))DEALLOCATE(Y)
 IF(ALLOCATED(RVAL))DEALLOCATE(RVAL); IF(ALLOCATED(DIST))DEALLOCATE(DIST)
 IF(ALLOCATED(IPOS))DEALLOCATE(IPOS); IF(ALLOCATED(QSORT))DEALLOCATE(QSORT)
 IF(ALLOCATED(XNR))DEALLOCATE(XNR);   IF(ALLOCATED(NDATA))DEALLOCATE(NDATA)

 NDIM=4; IF(ISFR.EQ.1)NDIM=13

 !## max. numbers of coordinates AND number of calculation points AND number of structures
 MAXNSEG=MAXVAL(ISG(1:NISG)%NSEG)+MAXVAL(ISG(1:NISG)%NCLC)+2*MAXVAL(ISG(1:NISG)%NSTW)
 ALLOCATE(DIST(MAXNSEG),IPOS(MAXNSEG),RVAL(NDIM,0:MAXNSEG),X(MAXNSEG),Y(MAXNSEG))
 ALLOCATE(QSORT(TTIME,NDIM),XNR(NDIM),NDATA(NDIM))

 NDATA=-9999.0
 !## arithmetic mean
 IAVERAGE=1
 ISTEADY=2
 
 CALL IGRLINETYPE(ILINET); CALL IGRLINEWIDTH(ILINEW) 

 !## display line
 DO I=1,NISG

  LEX=.TRUE.; IF(IISG.EQ.1)THEN; IF(ISG(I)%ILIST.NE.1)LEX=.FALSE.; ENDIF
  IF(.NOT.LEX)CYCLE
  
  !## get correct record-number for coordinates
  IREC=ISG(I)%ISEG; JREC=IREC+ISG(I)%NSEG-1

  !## check whether in current window
  X1=MINVAL(ISP(IREC:JREC)%X); X2=MAXVAL(ISP(IREC:JREC)%X)
  Y1=MINVAL(ISP(IREC:JREC)%Y); Y2=MAXVAL(ISP(IREC:JREC)%Y)

  !## entire line outside current graphical window
  IF(X1.GT.XMAX.OR.X2.LT.XMIN.OR.Y1.GT.YMAX.OR.Y2.LT.YMIN)CYCLE

  !## copy coordinates
  K=0; DO J=IREC,JREC; K=K+1; X(K)=ISP(J)%X; Y(K)=ISP(J)%Y; ENDDO

  !## get stream data
  NSEG=ISG(I)%NSEG
  CALL ISGGRIDGETSTREAMDATA(X,Y,DIST,IPOS,RVAL,ISG(I)%ICLC,ISG(I)%NCLC,ISG(I)%ISTW, &
                            ISG(I)%NSTW,NSEG,MAXNSEG,QSORT,XNR,NDATA,TTIME,NDIM,    &
                            ISTEADY,SDATE,EDATE,IAVERAGE)
  !## interpolate all segments in between and give proper values!
  CALL ISGGRIDINTSTREAMDATA(DIST,IPOS,RVAL,NSEG,MAXNSEG,NDIM,NDATA(1))
  
  CALL IGRMOVETO(X(1),Y(1))

  DO J=2,NSEG

   ICLR=UTL_IDFGETCLASS(ISGLEG(IVAR),RVAL(IVAR,J))

   CALL IGRCOLOURN(ICLR)
   CALL IGRLINETO(X(J),Y(J))

  ENDDO
 
 ENDDO
 
 CALL IGRLINETYPE(SOLIDLINE); CALL IGRLINEWIDTH(1) 
 
 IF(ALLOCATED(X))DEALLOCATE(X);       IF(ALLOCATED(Y))DEALLOCATE(Y)
 IF(ALLOCATED(RVAL))DEALLOCATE(RVAL); IF(ALLOCATED(DIST))DEALLOCATE(DIST)
 IF(ALLOCATED(IPOS))DEALLOCATE(IPOS); IF(ALLOCATED(QSORT))DEALLOCATE(QSORT)
 IF(ALLOCATED(XNR))DEALLOCATE(XNR);   IF(ALLOCATED(NDATA))DEALLOCATE(NDATA)

 END SUBROUTINE ISGPLOT_LEGENDLINES

 !###======================================================================
 SUBROUTINE ISGPLOT_FDIRECTION(X,Y,N,XMIN,XMAX,YMIN,YMAX,ISIZE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,ISIZE
 REAL,INTENT(IN) :: XMIN,XMAX,YMIN,YMAX
 REAL,DIMENSION(N) :: X,Y
 INTEGER :: I,II,J,M
 REAL :: DIST,TDIST,XC,YC,DX,DY,YFRAC,D
 LOGICAL :: LEX
 
 !## skip if not in current window
 IF(MINVAL(X).GT.XMAX.OR.MAXVAL(X).LT.XMIN.OR. &
    MINVAL(Y).GT.YMAX.OR.MAXVAL(Y).LT.YMIN)RETURN
    
 !## 2d plot - size of arrow never larger than dimensions of line
 IF(ISIZE.EQ.0)THEN
  YFRAC=SQRT((XMAX-XMIN)**2.0+(YMAX-YMIN)**2.0)/250.0
  DX=(MAXVAL(X)-MINVAL(X))**2.0+(MAXVAL(Y)-MINVAL(Y))**2.0; IF(DX.NE.0.0)DX=SQRT(DX)
  IF(YFRAC.GT.DX/5.0)YFRAC=DX/5.0
 ELSE
  YFRAC=SQRT((XMAX-XMIN)**2.0+(YMAX-YMIN)**2.0)/50.0
 ENDIF
  
 TDIST=0.0; J=0
 DO I=1,N-1
  !## to be drawn
  LEX=X(I)  .LT.XMAX.AND.X(I)  .GT.XMIN.AND. &
      Y(I)  .LT.YMAX.AND.Y(I)  .GT.YMIN.AND. &
      X(I+1).LT.XMAX.AND.X(I+1).GT.XMIN.AND. &
      Y(I+1).LT.YMAX.AND.Y(I+1).GT.YMIN
  IF(LEX)THEN
   !## store first of segment inside graphical view
   IF(J.EQ.0)J=I
   TDIST=TDIST+UTL_DIST(X(I),Y(I),X(I+1),Y(I+1))
  ENDIF
  IF(.NOT.LEX.OR.I.EQ.N-1)THEN
   !## plot arrow for current line-segment
   IF(TDIST.GT.0.0)THEN
    !## plot at half of the distance
    TDIST=TDIST/2.0
    !## find segment in which mid exists
    M=1; IF(.NOT.LEX)M=2
    DIST=0.0; DO II=J,I-M
     D   =UTL_DIST(X(II),Y(II),X(II+1),Y(II+1))
     DIST=DIST+D
     !## found segment of mid
     IF(DIST.GT.TDIST)EXIT
    ENDDO

    DX=X(II+1)-X(II); DY=Y(II+1)-Y(II)
    XC=(X(II)+X(II+1))/2.0; YC=(Y(II)+Y(II+1))/2.0
    CALL ISGPLOTARROW(DX,DY,XC,YC,YFRAC)
    
    !## reset j
    J=0; TDIST=0.0
   ENDIF
  ENDIF
 
 ENDDO
  
 END SUBROUTINE ISGPLOT_FDIRECTION
 
 !###======================================================================
 SUBROUTINE ISGPLOT_FCONNECTION(X1,Y1,N1,X2,Y2,N2,XMIN,XMAX,YMIN,YMAX,IDIR,ISIZE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N1,N2,IDIR,ISIZE
 REAL,INTENT(IN) :: XMIN,XMAX,YMIN,YMAX
 REAL,DIMENSION(N1) :: X1,Y1
 REAL,DIMENSION(N2) :: X2,Y2
 REAL,DIMENSION(4) :: XC,YC
 REAL :: YFRAC,DX,DY
 
 !## skip if not in current window
 IF(MINVAL(X1).GT.XMAX.OR.MAXVAL(X1).LT.XMIN.OR. &
    MINVAL(Y1).GT.YMAX.OR.MAXVAL(Y1).LT.YMIN)RETURN
 !## skip if not in current window
 IF(MINVAL(X2).GT.XMAX.OR.MAXVAL(X2).LT.XMIN.OR. &
    MINVAL(Y2).GT.YMAX.OR.MAXVAL(Y2).LT.YMIN)RETURN
    
 !## determine curve-coordinates
 IF(IDIR.EQ.1)THEN
  !## downstream
  XC(1)=X1(N1-1); XC(2)=X1(N1)
  YC(1)=Y1(N1-1); YC(2)=Y1(N1)
  XC(3)=X2(1)   ; XC(4)=X2(2)
  YC(3)=Y2(1)   ; YC(4)=Y2(2)
 ELSEIF(IDIR.EQ.-1)THEN
  !## upstream
  XC(1)=X2(N2-1); XC(2)=X2(N2)
  YC(1)=Y2(N2-1); YC(2)=Y2(N2)
  XC(3)=X1(1)   ; XC(4)=X1(2)
  YC(3)=Y1(1)   ; YC(4)=Y1(2)
 ENDIF
 
 IF(ISIZE.EQ.0)THEN
  YFRAC=SQRT((XMAX-XMIN)**2.0+(YMAX-YMIN)**2.0)/250.0
  DX=(MAXVAL(XC)-MINVAL(XC))**2.0+(MAXVAL(YC)-MINVAL(YC))**2.0; IF(DX.NE.0.0)DX=SQRT(DX)
  IF(YFRAC.GT.DX/5.0)YFRAC=DX/5.0
 ELSE
  YFRAC=SQRT((XMAX-XMIN)**2.0+(YMAX-YMIN)**2.0)/50.0
 ENDIF
 
 CALL IGRLINEWIDTH(3)
 CALL IGRCOLOURN(ICLRCO)
 CALL IGRFILLPATTERN(OUTLINE)
 
! CALL WGRBEZIERPOINTS(XC,YC,SIZE(XC),XPOS,YPOS,NVERT)
! CALL IGRPOLYLINE(XPOS,YPOS,NVERT)

! call igrcolourn(wrgb(0,255,0))
! CALL WGRBSPLINEPOINTS(XC,YC,SIZE(XC),XPOS,YPOS,NVERT)
! CALL IGRPOLYLINE(XPOS,YPOS,NVERT)

 CALL IGRPOLYLINE(XC,YC,4)

!REAL X(*) Array of control point X co-ordinates 
!REAL Y(*) Array of control point Y co-ordinates 
!INTEGER NCONTROL Number of control points in the X/Y arrays (>=3) 
!REAL XPOS(*) Returned array of curve X co-ordinates 
!REAL YPOS(*) Returned array of curve Y co-ordinates 
!INTEGER, OPTIONAL NVERT Number of vertices to return in XPOS/YPOS (default = NCONTROL*10) 

! CALL WGRCURVE(XC,YC,SIZE(XC)) !,NVERT)

 DX=XC(4)-XC(3); DY=YC(4)-YC(3)
 CALL ISGPLOTARROW(DX,DY,XC(4),YC(4),YFRAC)

! DX=XPOS(NVERT)-XPOS(NVERT-1); DY=YPOS(NVERT)-YPOS(NVERT-1)
! CALL ISGPLOTARROW(DX,DY,XPOS(NVERT),YPOS(NVERT),YFRAC)

!REAL X(*) Array of control point X co-ordinates 
!REAL Y(*) Array of control point Y co-ordinates 
!INTEGER NCONTROL Number of control points in the X/Y arrays (>=3) 
!INTEGER, OPTIONAL NVERT Number of vertices in drawn curve (default = NCONTROL*10) 
!Draws a curve which passes through all of the supplied control points

 END SUBROUTINE ISGPLOT_FCONNECTION

 !###======================================================================
 SUBROUTINE ISGPLOTOPENFILES(LEX,LOPEN,FNAME,CSTATUS)
 !###======================================================================
 IMPLICIT NONE
 LOGICAL,INTENT(OUT) :: LEX,LOPEN
 CHARACTER(LEN=*),INTENT(IN) :: CSTATUS
 CHARACTER(LEN=*),INTENT(IN) :: FNAME

 LEX  =.FALSE.
 LOPEN=.TRUE.

 !## if allocated then check name resemblance
 IF(ALLOCATED(ISG))THEN
  IF(TRIM(UTL_CAP(ISGFNAME,'U')).EQ.TRIM(UTL_CAP(FNAME,'U')))LEX=.TRUE.
 ENDIF

 IF(LEX)RETURN

 LOPEN=ISGOPENFILES(FNAME,CSTATUS)

 END SUBROUTINE ISGPLOTOPENFILES

 !###======================================================================
 SUBROUTINE ISGPLOTLOCCRS(X1,X2,Y1,Y2,DX,TDIST,DIMLOC,DISTLOC,LOCNAME,N,NLOC, &
                          ISHAPE,IISGLABEL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NLOC,DIMLOC,ISHAPE,IISGLABEL
 REAL,INTENT(IN) :: X1,X2,Y1,Y2,DX
 REAL,INTENT(INOUT) :: TDIST
 REAL,INTENT(INOUT),DIMENSION(DIMLOC) :: DISTLOC
 CHARACTER(LEN=*),DIMENSION(DIMLOC) :: LOCNAME
 INTEGER,INTENT(IN),DIMENSION(DIMLOC) :: N
 INTEGER :: ILOC,JLOC
 REAL :: DIST,XC,YC,FACTOR,D,XPOS,YPOS
 LOGICAL :: LEX

 !## find closest cross-section to from-point
 D   =DISTLOC(1)
 JLOC=1
 DO ILOC=2,NLOC
  IF(DISTLOC(ILOC).LT.D)THEN
   D   =DISTLOC(ILOC)
   JLOC=ILOC
  ENDIF
 ENDDO

 D   =DX*2.0
 DIST=SQRT((X2-X1)**2.0+(Y2-Y1)**2.0)

 DO ILOC=1,NLOC
  
  LEX=.FALSE.
  IF(TDIST.EQ.0.0)THEN
   IF(DISTLOC(ILOC).LE.TDIST+DIST)LEX=.TRUE.
  ELSE
   IF(TDIST.LT.DISTLOC(ILOC).AND. &
      TDIST+DIST.GE.DISTLOC(ILOC))LEX=.TRUE.
  ENDIF
  
  !## overrule in case difference is less than 0.01 meter;
  !## might overshoot segment length and then the feature will not be plotted
  IF(ABS(DISTLOC(ILOC)-(TDIST+DIST)).LE.0.01)LEX=.TRUE.
  
  IF(LEX)THEN
   FACTOR=0.0
   IF(DIST.NE.0.0)FACTOR=((TDIST+DIST)-DISTLOC(ILOC))/DIST
   XC    = X2-((X2-X1)*FACTOR)
   YC    = Y2-((Y2-Y1)*FACTOR)

   CALL ISGPLOTSHAPE(ISHAPE,XC,YC,D)
   
   IF(IISGLABEL.EQ.1)THEN
    CALL ISGTEXTANGLE(X2-X1,Y2-Y1,-90.0,2.0*D,ALIGNLEFT,XPOS,YPOS)
    !## draw arrow of application-direction
    IF(ISHAPE.EQ.1)THEN
     CALL WGRTEXTSTRING(XC+XPOS,YC+YPOS,TRIM(LOCNAME(ILOC))) !//' >')
    !## cross-sections
    ELSEIF(ISHAPE.EQ.2)THEN
     IF(N(ILOC).GT.0)THEN
      CALL WGRTEXTSTRING(XC+XPOS,YC+YPOS,TRIM(LOCNAME(ILOC))//'[1D]') !//' < >')
     ELSE
      CALL WGRTEXTSTRING(XC+XPOS,YC+YPOS,TRIM(LOCNAME(ILOC))//'[2D]') !//' < >')
     ENDIF
    !## structures/qh-relationships
    ELSEIF(ISHAPE.EQ.3.OR.ISHAPE.EQ.4)THEN
     CALL WGRTEXTSTRING(XC+XPOS,YC+YPOS,TRIM(LOCNAME(ILOC)))
    ENDIF
   ENDIF
  ENDIF
 ENDDO
 
 TDIST=TDIST+DIST

 END SUBROUTINE ISGPLOTLOCCRS

 !###======================================================================
 SUBROUTINE ISGPLOTSHAPE(ISHAPE,XC,YC,D)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: XC,YC,D
 INTEGER,INTENT(IN) :: ISHAPE

 !## crossed-rectangle
 IF(ISHAPE.EQ.1)THEN
  CALL IGRCOLOURN(ICLRSD)
  CALL IGRFILLPATTERN(OUTLINE)
  CALL IGRRECTANGLE(XC-D,YC-D,XC+D,YC+D)    !  |--|--|
  CALL IGRJOIN(XC-D,YC-D,XC+D,YC+D)         !  | /|\ |
  CALL IGRJOIN(XC-D,YC+D,XC+D,YC-D)         !  |--|--|
  CALL IGRFILLPATTERN(SOLID)
 !## triangle
 ELSEIF(ISHAPE.EQ.2)THEN
  CALL IGRCOLOURN(ICLRSC)
  CALL IGRJOIN(XC-2.0*D,YC+D,XC+2.0*D,YC+D) !\--------/
  CALL IGRJOIN(XC-2.0*D,YC+D,XC-D,YC-D)     ! \      /
  CALL IGRJOIN(XC-D,YC-D,XC+D,YC-D)         !  \ *  /
  CALL IGRJOIN(XC+D,YC-D,XC+2.0*D,YC+D)     !   \--/
 !## structure
 ELSEIF(ISHAPE.EQ.3)THEN
  CALL IGRCOLOURN(ICLRST)
  CALL IGRJOIN(XC-D,YC-D,XC,YC+D)       !    /\
  CALL IGRJOIN(XC,YC+D,XC+D,YC-D)       !   /* \
  CALL IGRJOIN(XC+D,YC-D,XC-D,YC-D)     !  /----\
 !## qh-relationship
 ELSEIF(ISHAPE.EQ.4)THEN
  CALL IGRCOLOURN(ICLRQH)
  CALL IGRPOLYGONCOMPLEX((/XC-D,XC-D,XC+D/),(/YC-D,YC+D,YC+D/),3)
  CALL IGRCOLOURN(INVERSECOLOUR(ICLRQH))
  CALL IGRPOLYGONCOMPLEX((/XC-D,XC+D,XC+D/),(/YC-D,YC-D,YC+D/),3)
 ENDIF

 END SUBROUTINE ISGPLOTSHAPE

 !###======================================================================
 SUBROUTINE ISGPLOTARROW(DX,DY,XC,YC,XSIZE)
 !###======================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: PIT=(2.0*PI)/3.0 !## radian of triangle
 REAL,INTENT(IN) :: DX,DY,XSIZE,XC,YC
 REAL,DIMENSION(4) :: XPOL,YPOL
 REAL :: A
 
 !## radians  
 A=ATAN2(DY,DX)

 XPOL(1)=XC+COS(A)*XSIZE; YPOL(1)=YC+SIN(A)*XSIZE
 A=A-PIT; XPOL(2)=XC+COS(A)*XSIZE; YPOL(2)=YC+SIN(A)*XSIZE
 A=A-PIT; XPOL(3)=XC+COS(A)*XSIZE; YPOL(3)=YC+SIN(A)*XSIZE
 XPOL(4)=XPOL(1); YPOL(4)=YPOL(1)
 
 CALL IGRPOLYGONCOMPLEX(XPOL,YPOL,4)

 END SUBROUTINE ISGPLOTARROW
 
 !###======================================================================
 SUBROUTINE ISGTEXTANGLE(DX,DY,ROFFSET,DOFFSET,IALIGN,XPOS,YPOS)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: DX,DY,ROFFSET,DOFFSET
 REAL,INTENT(OUT) :: XPOS,YPOS
 INTEGER,INTENT(IN) :: IALIGN
 REAL :: RAD

 RAD=0.0
 IF(DY.NE.0.0)RAD=ATAN2(DY,DX)
 !WRITE(*,*) RAD,DY,DX
 RAD =RAD*360.0/(2.0*PI)
 RAD =RAD+ROFFSET

 CALL WGRTEXTORIENTATION(IALIGN,RAD,DIRHORIZ,ALIGNLEFT)

 !##compute offset for text
 RAD=RAD*(2.0*PI)/360.0

 XPOS=COS(RAD)*DOFFSET
 YPOS=SIN(RAD)*DOFFSET

 END SUBROUTINE ISGTEXTANGLE

END MODULE MOD_ISG_PLOT