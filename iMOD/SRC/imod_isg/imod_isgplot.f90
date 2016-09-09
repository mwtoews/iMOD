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
!!
MODULE MOD_ISG_PLOT

USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : PI
USE MODPLOT
USE MOD_ISG_PAR
USE MOD_OSD, ONLY : ICF
USE MOD_UTL, ONLY : UTL_CAP,INVERSECOLOUR,UTL_WAITMESSAGE,UTL_MESSAGEHANDLE
USE MOD_ISG_UTL, ONLY : ISGCLOSEFILES,ISGOPENFILES

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

 X1=XMIN
 Y1=YMIN
 X2=XMAX
 Y2=YMAX

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

 CALL ISGPLOTOPENFILES(LEX,LOPEN,FNAME,'OLD')
 IF(.NOT.LOPEN)RETURN

 IF(XMIN.EQ.XMAX.AND.YMIN.EQ.YMAX)THEN
  XMIN=10.0E10
  YMIN=10.0E10
  XMAX= 0.0
  YMAX= 0.0
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
    XMIN=MIN(X,XMIN)
    XMAX=MAX(X,XMAX)
    YMIN=MIN(Y,YMIN)
    YMAX=MAX(Y,YMAX)
   END DO
  END DO
  CALL ISGCLOSEFILES()
 ELSE
  XMIN=MINVAL(ISP(1:NISP)%X)
  XMAX=MAXVAL(ISP(1:NISP)%X)
  YMIN=MINVAL(ISP(1:NISP)%Y)
  YMAX=MAXVAL(ISP(1:NISP)%Y)
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
 INTEGER :: IRAT,IRAT1,I,J,NSEG,NCRS,IREC,IQHR,IFDR,ISTW,NSTW,IISGLABEL,  &
            DIMCRS,INDS,ISND,ICRS,DIMCLC,NCLC,ICLC,IP,IISG,DIMSTW,NQHR,DIMQHR  
 INTEGER,ALLOCATABLE,DIMENSION(:) :: ID
 REAL :: DX,X1,Y1,X,Y,TWIDTH,THEIGHT,XPOS,YPOS,DIST
 REAL,DIMENSION(4) :: TDIST
 CHARACTER(LEN=50) :: CID
 TYPE OBJ
  CHARACTER(LEN=32) :: CNAME
  REAL :: DIST
  INTEGER :: N
 END TYPE OBJ
 TYPE(OBJ),ALLOCATABLE,DIMENSION(:) :: CRS,CLC,STW,QHR
 LOGICAL :: LEX,LOPEN

 IF(ALLOCATED(ID))DEALLOCATE(ID)

 !## whether isg is opened or will be drawn from memory
 LOPEN=.TRUE.
 IF(.NOT.ALLOCATED(ISGIU))LOPEN=.FALSE.

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

 IRAT  =0

 ALLOCATE(ID(5))     !## id numbers

 IF(LOPEN)THEN

  DIMCRS=0; DIMCLC=0; DIMSTW=0; DIMQHR=0

  READ(ISGIU(1,1),*) NISG

  DO I=1,NISG

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

    !## draw circles - nodes
    IF(ISND.EQ.1)THEN
     IF(X.LT.XMAX.AND.X.GT.XMIN.AND.Y.GT.YMIN.AND.Y.LT.YMAX)THEN
      CALL IGRCOLOURN(ICLRSP)
      CALL IGRCIRCLE(X,Y,DX)
     ENDIF
    ENDIF

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

    X1   =X
    Y1   =Y

   END DO

   CALL UTL_WAITMESSAGE(IRAT,IRAT1,I,NISG,'Drawing ISG ...')

  END DO

  IF(ALLOCATED(CRS))DEALLOCATE(CRS); IF(ALLOCATED(CLC))DEALLOCATE(CLC)
  IF(ALLOCATED(STW))DEALLOCATE(STW); IF(ALLOCATED(QHR))DEALLOCATE(QHR)

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

!   IF(IFDR.EQ.1)THEN
!    !## read segments
!    IREC=ID(1)-1
!    DO J=1,NSEG
!     X=ISP(IREC+J)%X
!     Y=ISP(IREC+J)%Y
!    ENDDO
!   ENDIF
   
   !## read segments
   IREC=ID(1)-1
   DO J=1,NSEG
    X=ISP(IREC+J)%X
    Y=ISP(IREC+J)%Y

    !## initialize variable
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
      IF(INDS.EQ.1.AND.(J.EQ.1.OR.J.EQ.NSEG))THEN
       CALL IGRCOLOURN(ICLRND)
       CALL IGRRECTANGLE(X-DX,Y-DX,X+DX,Y+DX)
       CALL ISGTEXTANGLE(X-X1,Y-Y1,-90.0,-3.0*DX,ALIGNRIGHT,XPOS,YPOS)
       IF(J.EQ.NSEG)CALL WGRTEXTSTRING(X+XPOS,Y+YPOS,'[TN] '//TRIM(CID))
      ENDIF
      IF(INDS.EQ.1.AND.J.EQ.2)THEN
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
       !## draw circles - nodes
       IF(ISND.EQ.1.AND.J.NE.NSEG)THEN
        CALL IGRCOLOURN(ICLRSP)
        CALL IGRCIRCLE(X,Y,DX)
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

    X1   =X
    Y1   =Y

   END DO

   CALL UTL_WAITMESSAGE(IRAT,IRAT1,I,NISG,'Drawing ISG ...')

  END DO
 ENDIF

 IF(ALLOCATED(ID))DEALLOCATE(ID)
 CALL WGRTEXTORIENTATION(ALIGNLEFT,0.0,DIRHORIZ,ALIGNLEFT)
 
 END SUBROUTINE ISGPLOT

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

 CALL ISGOPENFILES(FNAME,LOPEN,CSTATUS)

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