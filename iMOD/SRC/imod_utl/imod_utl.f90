!!  Copyright (C) Stichting Deltares, 2005-2019.
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
MODULE MOD_UTL

USE MOD_DBL
USE WINTERACTER
USE RESOURCE
USE MOD_CONFIG
USE MOD_PREF_PAR
USE MODPLOT
USE MOD_IDF_PAR
USE MOD_POLINT
USE IMODVAR
USE MOD_OSD
USE MOD_QKSORT

!## max. number of messages
INTEGER,PARAMETER :: MXMESSAGE=16
INTEGER,DIMENSION(MXMESSAGE) :: IMESSAGE
INTEGER,PARAMETER :: MAXUNITS=10000

CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: LISTNAME
INTEGER,ALLOCATABLE,DIMENSION(:) :: ILIST
CHARACTER(LEN=2),PARAMETER :: NEWLINE=CHAR(13)//CHAR(10)

INTEGER,PARAMETER :: MAXLEN=52
CHARACTER(LEN=MAXLEN),POINTER,DIMENSION(:,:) :: VAR=>NULL(),VAR_TMP=>NULL(),DVAR=>NULL()
CHARACTER(LEN=MAXLEN),POINTER,DIMENSION(:) :: CCNST=>NULL()
INTEGER,ALLOCATABLE,DIMENSION(:) :: ICOL_VAR,IACT_VAR
!## max. variables/max. lines
INTEGER :: NV,NL,IV

TYPE PROCOBJ
 INTEGER,DIMENSION(2) :: IFLAGS
 INTEGER :: ID
 CHARACTER(LEN=52) :: CID
END TYPE PROCOBJ

REAL(KIND=DP_KIND),PARAMETER,PRIVATE :: SDAY=86400.0D0

REAL(KIND=DP_KIND),DIMENSION(20) :: SXVALUE,SYVALUE
INTEGER :: NSX,NSY

CONTAINS

 !#####=================================================================
 SUBROUTINE UTL_MATMUL(A,B,C)
 !#####=================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),DIMENSION(:,:) :: A
 REAL(KIND=DP_KIND),DIMENSION(:,:) :: B
 REAL(KIND=DP_KIND),DIMENSION(:,:) :: C
 INTEGER :: NCA,NRA,NCB,NRB,NCC,NRC,I,J,K
 
 NCA=SIZE(A,1); NRA=SIZE(A,2)
 NCB=SIZE(B,1); NRB=SIZE(B,2)
 NCC=SIZE(C,1); NRC=SIZE(C,2)
 
 !## check
 IF(NCA.NE.NRB)THEN; WRITE(*,*) 'COLUMN DIMENSIONS ARRAY A NEED TO BE EQUAL TO ROWS   ARRAY B'; PAUSE; STOP; ENDIF
 IF(NRA.NE.NRC)THEN; WRITE(*,*) 'ROWS   DIMENSIONS ARRAY A NEED TO BE EQUAL TO ROWS   ARRAY C'; PAUSE; STOP; ENDIF
 IF(NCB.NE.NCC)THEN; WRITE(*,*) 'COLUMN DIMENSIONS ARRAY B NEED TO BE EQUAL TO COLUMN ARRAY C'; PAUSE; STOP; ENDIF

 C=0.0D0
 DO I=1,NCB; DO J=1,NRC
  DO K=1,NCA
   C(I,J)=C(I,J)+A(K,J)*B(I,K)
  ENDDO
 ENDDO; ENDDO
 
 END SUBROUTINE UTL_MATMUL
 
 !###======================================================================
 SUBROUTINE UTL_MF2005_MAXNO(FNAME,NP)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN),DIMENSION(:) :: NP
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: I,IU,JU,IOS
 CHARACTER(LEN=12) :: NAN
 CHARACTER(LEN=256) :: LINE

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME                    ,STATUS='OLD'    ,ACTION='READ' ,FORM='FORMATTED'); IF(IU.EQ.0)RETURN
 JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=FNAME(:LEN_TRIM(FNAME)-1),STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED'); IF(JU.EQ.0)RETURN
 DO
  READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
  IF(INDEX(LINE,'NaN').GT.0)THEN
   DO I=1,SIZE(NP)
    NAN='NaN'//TRIM(ITOS(I))//'#'
    IF(INDEX(LINE,TRIM(NAN)).GT.0)LINE=UTL_SUBST(LINE,TRIM(NAN),ITOS(NP(I)))
   ENDDO
  ENDIF
  WRITE(JU,'(A)') TRIM(ADJUSTL(LINE))
 ENDDO

 CLOSE(IU,STATUS='DELETE'); CLOSE(JU)

 END SUBROUTINE UTL_MF2005_MAXNO

 !###======================================================================
 REAL(KIND=SP_KIND) FUNCTION UTL_D2R(X,PLACES)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X
 INTEGER,INTENT(IN) :: PLACES

 UTL_D2R = ANINT(X*(10.D0**PLACES))/(10.D0**PLACES)

 END FUNCTION UTL_D2R

 !###======================================================================
 LOGICAL FUNCTION UTL_DRAWPOLYGON(XPOL,YPOL,MAXPOL,NPOL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: MAXPOL
 INTEGER,INTENT(OUT) :: NPOL
 REAL(KIND=DP_KIND),DIMENSION(MAXPOL),INTENT(INOUT) :: XPOL,YPOL
 INTEGER :: ITYPE
 TYPE(WIN_MESSAGE) :: MESSAGE
 LOGICAL :: LEX
 REAL(KIND=DP_KIND) :: XC1,YC1,MOUSEX,MOUSEY

 UTL_DRAWPOLYGON=.FALSE.

 CALL WCURSORSHAPE(ID_CURSORPOLYGON)

 NPOL=1

 CALL IGRPLOTMODE(MODEXOR)
 CALL IGRCOLOURN(WRGB(255,255,255))
 CALL IGRFILLPATTERN(OUTLINE)
 LEX=.FALSE.

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)

  MOUSEX=DBLE(MESSAGE%GX)+OFFSETX
  MOUSEY=DBLE(MESSAGE%GY)+OFFSETY

  SELECT CASE (ITYPE)
   !## mouse-move
   CASE (MOUSEMOVE)
    CALL WINDOWSELECT(0)
    CALL WINDOWOUTSTATUSBAR(1,'X:'//TRIM(RTOS(MOUSEX,'F',3))//' m, Y:'//TRIM(RTOS(MOUSEY,'F',3))//' m')

    XC1=MOUSEX; YC1=MOUSEY

    IF(NPOL.GT.1)THEN
     CALL UTL_PLOT1BITMAP()
     IF(LEX)CALL UTL_PLOTPOLYGON(SIZE(XPOL),NPOL,XPOL,YPOL)
     LEX=.TRUE.
     XPOL(NPOL)=XC1; YPOL(NPOL)=YC1
     CALL UTL_PLOTPOLYGON(SIZE(XPOL),NPOL,XPOL,YPOL)
     CALL UTL_PLOT2BITMAP()
    ENDIF

   CASE (MOUSEBUTDOWN)
    CALL UTL_PLOT1BITMAP()
    IF(LEX)CALL UTL_PLOTPOLYGON(SIZE(XPOL),NPOL,XPOL,YPOL)
    SELECT CASE (MESSAGE%VALUE1)
     !## left button
     CASE (1)
      XPOL(NPOL:NPOL+1)=XC1; YPOL(NPOL:NPOL+1)=YC1; NPOL=NPOL+1
      CALL UTL_PLOTPOLYGON(SIZE(XPOL),NPOL,XPOL,YPOL)
      CALL UTL_PLOT2BITMAP()
     !## right button
     CASE (3)
      NPOL=NPOL-1
      CALL UTL_PLOT2BITMAP()
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

 UTL_DRAWPOLYGON=.TRUE.

 END FUNCTION UTL_DRAWPOLYGON

 !###======================================================================
 SUBROUTINE UTL_PLOTPOLYGON(MAXPOL,NPOL,XPOL,YPOL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NPOL,MAXPOL
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(MAXPOL) :: XPOL,YPOL

 IF(NPOL.EQ.2)THEN
  CALL DBL_IGRJOIN(XPOL(1),YPOL(1),XPOL(2),YPOL(2),IOFFSET=1)
 ELSE
  CALL DBL_IGRPOLYGONCOMPLEX(XPOL,YPOL,NPOL,IOFFSET=1)
 ENDIF

 END SUBROUTINE UTL_PLOTPOLYGON

 !###======================================================================
 SUBROUTINE UTL_GETMIDPOINT(X,Y,N,XMID,YMID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(N) :: X,Y
 REAL(KIND=DP_KIND),INTENT(OUT) :: XMID,YMID
 REAL(KIND=DP_KIND) :: X1,Y1,X2,Y2,DX,DY
 INTEGER :: I,J,ISTEP

 XMID=(MAXVAL(X)+MINVAL(X))/2.0D0
 YMID=(MAXVAL(Y)+MINVAL(Y))/2.0D0

 !## check whether point is inside polygon, if not try others - but limited, could be a line as polygon!
 IF(DBL_IGRINSIDEPOLYGON(XMID,YMID,X,Y,N).NE.1)THEN
  ISTEP=10
DOLOOP: DO
   X1=MINVAL(X); Y1=MINVAL(Y)
   X2=MAXVAL(X); Y2=MAXVAL(Y)
   DX=(X2-X1)/REAL(ISTEP,8)
   DY=(Y2-Y1)/REAL(ISTEP,8)
   YMID=Y1
   DO I=1,ISTEP
    YMID=YMID+DY
    XMID=X1
    DO J=1,ISTEP
     XMID=XMID+DX
     IF(DBL_IGRINSIDEPOLYGON(XMID,YMID,X,Y,N).EQ.1)EXIT DOLOOP
    ENDDO
   ENDDO
   ISTEP=ISTEP+1
   IF(ISTEP.GT.10)EXIT
  ENDDO DOLOOP
 ENDIF

 END SUBROUTINE UTL_GETMIDPOINT

 !###======================================================================
 SUBROUTINE UTL_CLEANLINE(LINE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 CHARACTER(LEN=*),INTENT(INOUT) :: LINE

 DO; I=INDEX(TRIM(LINE),'>'); IF(I.EQ.0)EXIT; LINE(I:I)=' '; ENDDO
 !## probably :-delimited
 IF(INDEX(TRIM(LINE),';').GT.0)THEN
  !## replace "," for "."
  DO; I=INDEX(TRIM(LINE),','); IF(I.EQ.0)EXIT; LINE(I:I)='.'; ENDDO
  DO; I=INDEX(TRIM(LINE),';'); IF(I.EQ.0)EXIT; LINE(I:I)=','; ENDDO
 ENDIF
 DO; I=INDEX(TRIM(LINE),' '); IF(I.EQ.0)EXIT; LINE(I:I)='_'; ENDDO
 DO; I=INDEX(TRIM(LINE),'/'); IF(I.EQ.0)EXIT; LINE(I:I)='|'; ENDDO

 END SUBROUTINE UTL_CLEANLINE

 !###======================================================================
 INTEGER FUNCTION UTL_DETERMINEIDFTYPE(XMIN,YMIN,XMAX,YMAX,CSIZE,NCOL,NROW)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: XMIN,YMIN,XMAX,YMAX,CSIZE
 INTEGER,INTENT(IN) :: NCOL,NROW
 REAL(KIND=SP_KIND) :: XSMAX,YSMAX
 INTEGER :: I

 !## determine whether it need to be a double precision IDF file
 XSMAX=REAL(XMIN,4); DO I=1,NCOL; XSMAX=XSMAX+REAL(CSIZE,4); ENDDO
 YSMAX=REAL(YMIN,4); DO I=1,NROW; YSMAX=YSMAX+REAL(CSIZE,4); ENDDO

 UTL_DETERMINEIDFTYPE=4
 IF(.NOT.UTL_EQUALS_REAL(XMAX,REAL(XSMAX,8)).OR. &
    .NOT.UTL_EQUALS_REAL(YMAX,REAL(YSMAX,8)))UTL_DETERMINEIDFTYPE=8

 END FUNCTION UTL_DETERMINEIDFTYPE

 !###======================================================================
 INTEGER FUNCTION UTL_SELECTIEDGE(X,Y,XA1,YA1,XA2,YA2)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) ::  X,Y,XA1,YA1,XA2,YA2
 REAL(KIND=DP_KIND) :: MIND,WX1,WX2,WY1,WY2
 INTEGER :: IEDGE
 INTEGER,DIMENSION(0:9) :: IDCURSOR
 DATA IDCURSOR/CURARROW,         ID_CURSORMOVELEFTRIGHT,ID_CURSORMOVELEFTRIGHT,ID_CURSORMOVEUPDOWN,ID_CURSORMOVEUPDOWN, &
               ID_CURSORMOVENESW,ID_CURSORMOVENWSE,     ID_CURSORMOVENESW,     ID_CURSORMOVENWSE,  ID_CURSORMOVE/

 WX1=REAL(WINFOGRREAL(GRAPHICSUNITMINX),8) ! (7)  left  limit of main graphics area
 WY1=REAL(WINFOGRREAL(GRAPHICSUNITMINY),8) ! (8)  lower limit of main graphics area
 WX2=REAL(WINFOGRREAL(GRAPHICSUNITMAXX),8) ! (9)  right limit of main graphics area
 WY2=REAL(WINFOGRREAL(GRAPHICSUNITMAXY),8) ! (10) upper limit of main graphics area

 MIND=MIN(WX2-WX1,WY2-WY1)/250.0D0

 IEDGE=0
 IF(UTL_DIST(X,Y,XA1,YA1).LE.MIND)IEDGE=5
 IF(UTL_DIST(X,Y,XA2,YA1).LE.MIND)IEDGE=6  !## lrc
 IF(UTL_DIST(X,Y,XA2,YA2).LE.MIND)IEDGE=7  !## urc
 IF(UTL_DIST(X,Y,XA1,YA2).LE.MIND)IEDGE=8  !## ulc
 IF(IEDGE.EQ.0)THEN
  IF(ABS(X-XA1).LE.MIND)THEN
   IF(Y.GE.YA1.AND.Y.LE.YA2)IEDGE=1 !## west
  ENDIF
  IF(ABS(X-XA2).LE.MIND)THEN
   IF(Y.GE.YA1.AND.Y.LE.YA2)IEDGE=2 !## east
  ENDIF
  IF(ABS(Y-YA1).LE.MIND)THEN
   IF(X.GE.XA1.AND.X.LE.XA2)IEDGE=3 !## south
  ENDIF
  IF(ABS(Y-YA2).LE.MIND)THEN
   IF(X.GE.XA1.AND.X.LE.XA2)IEDGE=4 !## north
  ENDIF
 ENDIF
 IF(IEDGE.EQ.0)THEN
  MIND=MIND*5.0D0
  IF(UTL_DIST(X,Y,XA1,YA1).LE.MIND)IEDGE=9
  IF(UTL_DIST(X,Y,XA2,YA1).LE.MIND)IEDGE=9
  IF(UTL_DIST(X,Y,XA2,YA2).LE.MIND)IEDGE=9
  IF(UTL_DIST(X,Y,XA1,YA2).LE.MIND)IEDGE=9
 ENDIF

 !## select something else
 IF(WINFOMOUSE(MOUSECURSOR).NE.IDCURSOR(IEDGE))CALL WCURSORSHAPE(IDCURSOR(IEDGE))

 UTL_SELECTIEDGE=IEDGE

 END FUNCTION UTL_SELECTIEDGE

 !###===============================================================================
 INTEGER FUNCTION UTL_PUTRECORDLENGTH(NBYTES)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NBYTES

 UTL_PUTRECORDLENGTH=(NBYTES*256)+247

 END FUNCTION UTL_PUTRECORDLENGTH

 !###===============================================================================
 INTEGER FUNCTION UTL_GETRECORDLENGTH(FNAME)
 !###===============================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: IU,IBYTE,IOS

 UTL_GETRECORDLENGTH=0

 !## open file
 IU=UTL_GETUNIT()
 OPEN(IU,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED',ACTION='READ',ACCESS='STREAM',IOSTAT=IOS)
 IF(IOS.NE.0)RETURN

 READ(IU,IOSTAT=IOS) IBYTE
 !## record length
 IF(IOS.EQ.0)UTL_GETRECORDLENGTH=(IBYTE-247)/256  !## in bytes
 CLOSE(IU)
 IF(UTL_GETRECORDLENGTH.LE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD read recordlength of '//TRIM(ITOS(UTL_GETRECORDLENGTH)),'Error')
 ENDIF

 END FUNCTION UTL_GETRECORDLENGTH

 !###===============================================================================
 SUBROUTINE UTL_PLOTLINE(X,Z1,Z2,THICKNESS)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: THICKNESS
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Z1,Z2

 CALL IGRLINEWIDTH(THICKNESS)
 CALL DBL_IGRJOIN(X,Z1,X,Z2)

 END SUBROUTINE UTL_PLOTLINE

 !###===============================================================================
 SUBROUTINE UTL_PLOTPOINT(X,Y,SYMBOL,FCT,LPROF)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: SYMBOL
 LOGICAL,INTENT(IN) :: LPROF
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Y,FCT
 REAL(KIND=DP_KIND) :: XWID,XHGH,CHW,CHH
 INTEGER :: IFAM,ISTL,IOFFSET

 IFAM=WINFOGRINTEGER(GRTEXTFAMILY)
 ISTL=WINFOGRINTEGER(GRTEXTSTYLE)
 XWID=REAL(WINFOGRREAL(GRAPHICSCHWIDTH),8)
 XHGH=REAL(WINFOGRREAL(GRAPHICSCHHEIGHT),8)

 IOFFSET=1; IF(LPROF)IOFFSET=0
 CALL UTL_SETTEXTSIZE(CHW,CHH,FCT=FCT*5.0D0,IMARKER=1)
 CALL DBL_WGRTEXTFONT(IFAMILY=0,TWIDTH=CHW,THEIGHT=CHH,ISTYLE=0)
 CALL DBL_IGRMARKER(X,Y,SYMBOL,IOFFSET=IOFFSET)

 END SUBROUTINE UTL_PLOTPOINT

 !###===============================================================================
 SUBROUTINE UTL_PLOTLABEL(X,Y,STRING,IATTRIB,NATTRIB,TWIDTH,THEIGHT,ATTRIB,LPROF,IEQ,IALIGN,GANGLE,CFORMAT)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NATTRIB,IEQ,IALIGN
 REAL(KIND=DP_KIND),INTENT(IN) :: TWIDTH,THEIGHT
 CHARACTER(LEN=*),INTENT(IN),DIMENSION(NATTRIB) :: STRING,ATTRIB
 INTEGER,DIMENSION(NATTRIB),INTENT(IN) :: IATTRIB
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CFORMAT
 LOGICAL,INTENT(IN) :: LPROF
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Y
 REAL(KIND=DP_KIND),INTENT(IN),OPTIONAL :: GANGLE
 REAL(KIND=DP_KIND) :: DX,DY,XC,YC,DXS,DYS
 INTEGER :: I,J
 CHARACTER(LEN=256) :: LINE,CLABEL

 !## nothing to draw
 IF(SUM(IATTRIB).EQ.0)RETURN

 CALL IGRLINEWIDTH(1)

 IF(LPROF)THEN

  CALL DBL_WGRTEXTFONT(IFAMILY=TFONT,TWIDTH=TWIDTH*1.5D0,THEIGHT=THEIGHT*1.5D0,ISTYLE=0)

  LINE=''
  DO I=SIZE(IATTRIB),1,-1
   IF(IATTRIB(I).EQ.1)THEN
    J=0
    IF(IBACKSLASH.EQ.1)THEN
     J=INDEX(STRING(I),'\',.TRUE.)
     IF(J.GT.0)J=J+1
    ENDIF
    J=MAX(1,J)
    CLABEL=STRING(I)(J:)
    !## try to make a nice real
    CALL UTL_PLOTLABEL_REAL(CLABEL,CFORMAT)
    IF(ILABELNAME.EQ.1)THEN
     IF(LEN_TRIM(ATTRIB(I)).GT.0)THEN
      LINE=TRIM(LINE)//TRIM(ATTRIB(I))//'= '//TRIM(CLABEL)//';'
     ELSE
      LINE=TRIM(LINE)//TRIM(CLABEL)//';'
     ENDIF
    ELSE
     LINE=TRIM(LINE)//TRIM(CLABEL)//';'
    ENDIF
   ENDIF
  ENDDO
  I=INDEX(LINE,';',.TRUE.)
  IF(I.GT.0)LINE(I:I)=''

  !## place connection line
  IF(PRESENT(GANGLE))THEN
   CALL DBL_WGRTEXTORIENTATION(IALIGN=IALIGN,ANGLE=GANGLE)
  ELSE
   CALL DBL_WGRTEXTORIENTATION(IALIGN=IALIGN,ANGLE=90.0D0)
  ENDIF
  CALL DBL_WGRTEXTSTRING(X,Y,'   '//TRIM(LINE),IOFFSET=1)

 ELSE

  CALL DBL_WGRTEXTFONT(IFAMILY=TFONT,TWIDTH=TWIDTH,THEIGHT=THEIGHT,ISTYLE=0)
  !## size of text character width/height in user units
  DXS=WINFOGRREAL(GRAPHICSCHWIDTH)
  DYS=WINFOGRREAL(GRAPHICSCHHEIGHT)
  IF(PRESENT(GANGLE))THEN
   CALL DBL_WGRTEXTORIENTATION(IALIGN=IALIGN,ANGLE=GANGLE)
  ELSE
   CALL DBL_WGRTEXTORIENTATION(IALIGN=IALIGN,ANGLE=0.0D0)
  ENDIF

  !## get size of box over labels
  DX=0.0D0
  DO I=1,SIZE(IATTRIB)
   IF(IATTRIB(I).EQ.1)THEN
    J=0
    IF(IBACKSLASH.EQ.1)THEN
     J=INDEX(STRING(I),'\',.TRUE.)
     IF(J.GT.0)J=J+1
    ENDIF
    J=MAX(1,J)

    CLABEL=STRING(I)(J:)
    !## try to make a nice real
    CALL UTL_PLOTLABEL_REAL(CLABEL,CFORMAT)

    IF(ILABELNAME.EQ.1)THEN
     IF(LEN_TRIM(ATTRIB(I)).GT.0)THEN
      DX=MAX(DX,WGRTEXTLENGTH('0'//TRIM(ATTRIB(I))//'= '//TRIM(CLABEL)//'0',0)*DXS)
     ELSE
      DX=MAX(DX,WGRTEXTLENGTH('0'//TRIM(CLABEL)//'0',0)*DXS)
     ENDIF
    ELSE
     DX=MAX(DX,WGRTEXTLENGTH('0'//TRIM(CLABEL)//'0',0)*DXS)
    ENDIF
   ENDIF
  END DO

  !## size of box for text
  DXS=DX

  DX=(WINFOGRREAL(GRAPHICSUNITMAXX)-WINFOGRREAL(GRAPHICSUNITMINX))/250.0D0
  DY= DYS*SUM(IATTRIB)
  XC= X+DX
  YC= Y-(DY/2.0D0)
  DX= WGRTEXTLENGTH('0',0)*WINFOGRREAL(GRAPHICSCHWIDTH)

  !## position labels in centre
  IF(.TRUE.)THEN
   XC=X
   YC=Y-1.0D0*DYS
  ENDIF

!  CALL IGRFILLPATTERN(OUTLINE)
!  CALL IGRCOLOURN(WRGB(0,0,0)) !## black
!  CALL DBL_IGRRECTANGLE(XC-DX,YC+0.5D0*DYS,XC+DX+DXS,YC-DY-0.5D0*DYS,IOFFSET=1)

  DO I=1,SIZE(IATTRIB)
   IF(IATTRIB(I).EQ.1)THEN
    IF(IEQ.GT.0)THEN
    ELSE
     CALL IGRFILLPATTERN(SOLID)
     CALL IGRCOLOURN(WRGB(0,0,0))
    ENDIF
    J=0
    IF(IBACKSLASH.EQ.1)THEN
     J=INDEX(STRING(I),'\',.TRUE.)
     IF(J.GT.0)J=J+1
    ENDIF
    J=MAX(1,J)
    CLABEL=STRING(I)(J:)
    !## try to make a nice real
    CALL UTL_PLOTLABEL_REAL(CLABEL,CFORMAT)
    IF(ILABELNAME.EQ.1)THEN
     IF(LEN_TRIM(ATTRIB(I)).GT.0)THEN
      CALL DBL_WGRTEXTSTRING(XC,YC,TRIM(ADJUSTL(ATTRIB(I)))//'= '//TRIM(ADJUSTL(CLABEL)),IOFFSET=1)
     ELSE
      CALL DBL_WGRTEXTSTRING(XC,YC,TRIM(ADJUSTL(CLABEL)),IOFFSET=1)
     ENDIF
    ELSE
     CALL DBL_WGRTEXTSTRING(XC,YC,TRIM(ADJUSTL(CLABEL)),IOFFSET=1)
    ENDIF

    YC=YC-DYS
   ENDIF
  END DO

 ENDIF

 END SUBROUTINE UTL_PLOTLABEL

 !###===============================================================================
 SUBROUTINE UTL_PLOTLABEL_REAL(CLABEL,CFORMAT)
 !###===============================================================================
 CHARACTER(LEN=*),INTENT(INOUT) :: CLABEL
 CHARACTER(LEN=*),INTENT(IN) :: CFORMAT
 INTEGER :: IOS
 REAL(DP_KIND) :: X

 IF(TRIM(CFORMAT).EQ.'')RETURN
 
 READ(CLABEL,*,IOSTAT=IOS) X
 IF(IOS.NE.0)RETURN
 WRITE(CLABEL,CFORMAT) X  
 
 END SUBROUTINE UTL_PLOTLABEL_REAL

 
      subroutine UTL_MODELLHS1(PDELR,ORGNCOL,newncol,&
                          IC1,IC2,OC1,OC2,INC,fincr,powr,&
                          NOMINCELL,NOMaxCELL,lclip)

! description:
! ------------------------------------------------------------------------------
!
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      INTEGER    ORGNCOL,&        ! (I) original number of columns
                 newncol,&        ! (O) new number of columns
                 NOMINCELL,&      ! (I) minimum number of cells (rows/columns)
                                  !     in an upscaled cell
                 NOMaxCELL,&      ! (I) maximum number of cells (rows/columns)
                                  !     in an upscaled cell
                 IC1,&            ! (I) column number 1 of unscaled area
                 IC2,&            ! (I) column number 2 of unscaled area
                 OC1,&            ! (O) column number 1 of scaled area
                 OC2              ! (O) column number 2 of scaled area

      REAL(KIND=DP_KIND)  INC,&            ! (I) start increment factor
                 powr             ! (I) power
      REAL(KIND=DP_KIND) fincr      ! (I) factor increment factor
                                  !     Used scale factor:
                                  !        f=fincr*(x-1)^powr+inc
                                  !        step=f*step
                                  !    x: cell position offset from AOI

      INTEGER    PDELR(ORGNCOL)   ! (O) pointer from unscaled to scaled
                                  !     column numbers

      logical   lclip             ! .true.  use clip-edges
                                  ! .false. don't use clip edges
                                  ! clip edge: first and last upscaled cell
                                  !            exist of one unscaled cell

! local variables
      INTEGER    ISC,L,I,J,ICOL,CCOL,ncel,idir

      integer    maxscc   ! maximum number of rows/columns to put together

      real(KIND=DP_KIND) x
      REAL(KIND=DP_KIND)  STEP,f


! functions


! include files


! program section
! ------------------------------------------------------------------------------


! get Area Of Interest
      isc = min(ic1,ic2)
      ccol= max(ic1,ic2)

      maxscc=nomaxcell


!##ccol fits minimal cell value
      I=((INT((CCOL-ISC)/NOMINCELL)+1)*NOMINCELL)-1
      CCOL=ISC+I


!##correct if ccol.gt.orgncol:i<0
      I=ORGNCOL-CCOL
      IF(I.LT.0)THEN
         ISC=MAX(1,ISC+I)
         CCOL=MAX(1,CCOL+I)
      ENDIF

!##correct if ccol.lt.1
      I=CCOL-1
      IF(I.LT.0)THEN
!         ISC =MIN(NCOL,ISC-I)
!         CCOL=MIN(NCOL,CCOL-I)
         ISC =MIN(ORGNCOL,ISC-I)
         CCOL=MIN(ORGNCOL,CCOL-I)
      ENDIF


!##computation of column-definition
! Area Of Interest
      L=0
      I=1
      DO ICOL=ISC,CCOL
         PDELR(ICOL)=L
!         write(*,*) ' AOI icol,L: ',icol,L
         I=I+1
         IF(I.GT.NOMINCELL)THEN
            I=1
            L=L+1
         ENDIF
      END DO
      IF(I.NE.1)L=L+1

! Area 'higher' and 'lower' than AOI

! 'lower' part
      idir=-1
      icol=isc
      !--------
      j   =pdelr(icol)
      icol=icol+idir
      step=inc
      x=1.

      do while(icol.ge.1)

         ncel=nint(step)*nomincell
         ncel=min(ncel,maxscc)
         j=j+idir

         do i=icol,min(orgncol,max(icol+idir*(ncel-1),1)),idir
!      write(*,*) ' i,j,ncel,step: ',i,j,ncel,step
            pdelr(i)=j
!         write(*,*) ' LOW icol,L: ',icol,L
         enddo
         icol=icol+idir*ncel

         if (ncel.lt.maxscc) then
            x=x+ncel
            f=fincr*(x-1.)**powr+inc
            step=f*step
         endif

      enddo

! 'upper' part
      idir=+1
      icol=ccol
      !--------
      j   =pdelr(icol)
      icol=icol+idir
      step=inc
      x=1.

      do while(icol.le.orgncol)

         ncel=nint(step)*nomincell
         ncel=min(ncel,maxscc)
         j=j+idir

         do i=icol,min(orgncol,max(icol+idir*(ncel-1),1)),idir
!      write(*,*) ' i,j,ncel,step: ',i,j,ncel,step
            pdelr(i)=j
!         write(*,*) ' UPP icol,L: ',icol,L
         enddo
         icol=icol+idir*ncel

         if (ncel.lt.maxscc) then
            x=x+ncel
            f=fincr*(x-1.)**powr+inc
            step=f*step
         endif

      enddo


! when clip option is active (lclip=.true.)
! The outermost active upscaled cell must consist at only 1 unscaled cell!
      if (lclip) then

         ! set edge cells to a width of 1
         if (pdelr(1).eq.pdelr(2)) then
            pdelr(1)=pdelr(1)-1
         endif

         if (pdelr(orgncol).eq.pdelr(orgncol-1)) then
            pdelr(orgncol)=pdelr(orgncol)+1
         endif

      endif


! renumber pdelr to let it undefined with value 1 in pdelr(1)
      J=1-PDELR(1)
      DO ICOL=1,orgncol
         PDELR(ICOL)=PDELR(ICOL)+J
      END DO


! assign number of scaled are cells
      newncol=PDELR(orgncol)


! Area Of Interest in scaled area
      oc1=PDELR(ic1)
      oc2=PDELR(ic2)

!      write(*,*) ' UTL_MODELLHS: nnew nold AOI ',newncol,orgncol,ic1,ic2
!      write(*,*) PDELR

! end of program
      end subroutine UTL_MODELLHS1

 !###======================================================================
 INTEGER FUNCTION UTL_GETUNITIPF(IPFNAME,TSTAT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IPFNAME
 CHARACTER(LEN=*),INTENT(IN) :: TSTAT
 LOGICAL :: LEX,LOPEN
 CHARACTER(LEN=10) :: TSTATUS

 TSTATUS=UTL_CAP(TSTAT,'U')
 UTL_GETUNITIPF=0

 INQUIRE(FILE=IPFNAME,OPENED=LOPEN)
 IF(LOPEN)THEN
  INQUIRE(FILE=IPFNAME,NUMBER=UTL_GETUNITIPF)
  CLOSE(UTL_GETUNITIPF)
 ENDIF

 IF(TRIM(TSTATUS).EQ.'OLD')THEN
  INQUIRE(FILE=IPFNAME,EXIST=LEX)
  IF(.NOT.LEX)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot find'//CHAR(13)//TRIM(IPFNAME),'Warning')
   RETURN
  ENDIF
  UTL_GETUNITIPF=UTL_GETUNIT()
  CALL OSD_OPEN(UTL_GETUNITIPF,FILE=IPFNAME,STATUS=TSTATUS,FORM='FORMATTED',ACTION='READ,DENYWRITE')
 ELSEIF(TRIM(TSTATUS).EQ.'UNKNOWN')THEN
  INQUIRE(FILE=IPFNAME,EXIST=LEX)
  IF(LEX)THEN
  ENDIF
  UTL_GETUNITIPF=UTL_GETUNIT()
  CALL OSD_OPEN(UTL_GETUNITIPF,FILE=IPFNAME,STATUS=TSTATUS,FORM='FORMATTED',ACTION='WRITE,DENYREAD')
 ENDIF

 END FUNCTION UTL_GETUNITIPF

 !###=========================================================================
 SUBROUTINE UTL_GETHELP(TOPIC,CTOPIC)
 !###=========================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: TOPIC,CTOPIC
 LOGICAL :: LEX,LACROBAT
 INTEGER :: ISTATUS,IEXCOD
 CHARACTER(LEN=256) :: LINE

 !## error/warning checking
 IF(PREFVAL(4).EQ.'')THEN
  CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'You should specify the keyword HELPFILE in the Preference file of iMOD.'// &
               'E.g. HELPFILE=D:\IMOD\HELP.PDF','Warning')
  RETURN
 ENDIF
 INQUIRE(FILE=PREFVAL(4),EXIST=LEX)
 IF(.NOT.LEX)THEN
  CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'Cannot find the specified HELPFILE='//TRIM(PREFVAL(4)),'Warning')
  RETURN
 ENDIF

 LACROBAT=.TRUE.

 !## acrobat reader
 IF(PREFVAL(13).EQ.'')THEN
  CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'You should specify the keyword ACROBATREADER in the Preference file of iMOD.'// &
               'E.g. ACROBATREADER=c:\Program Files (x86)\Adobe\Reader 11.0D0\Reader\AcroRd32.exe','Warning')
  RETURN
 ENDIF
 INQUIRE(FILE=PREFVAL(13),EXIST=LEX)
 IF(.NOT.LEX)THEN
  CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'Cannot find the specified ACROBATREADER='//TRIM(PREFVAL(13)),'Warning')
  RETURN
 ENDIF

 !## acrobat reader
 IF(LACROBAT)THEN
  LINE='"'//TRIM(PREFVAL(13))//'" /A "nameddest='//TRIM(CTOPIC)//'" "'//TRIM(PREFVAL(4))//'"'
 !## sumatra pdf
 ELSE
  LINE='"'//TRIM(PREFVAL(13))//'" -reuse-instance -named-dest sec:'//TRIM(CTOPIC)//' "'//TRIM(PREFVAL(4))//'"'
 ENDIF

 IF(IDPROC(1).NE.0)THEN
  CALL IOSCOMMANDCHECK(IDPROC,ISTATUS,IEXCOD=IEXCOD)
  !## killed
  IF(ISTATUS.EQ.0)THEN

  !## still running, kill it
  ELSEIF(ISTATUS.EQ.1)THEN
   CALL IOSCOMMANDKILL(IDPROC,0)
  ENDIF
 ENDIF

 CALL IOSCOMMAND(LINE,PROCSILENT,IDPROC=IDPROC)

 END SUBROUTINE UTL_GETHELP

 !###================================================================================
 SUBROUTINE UTL_STOREZOOMEXTENT()
 !###================================================================================
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

 END SUBROUTINE UTL_STOREZOOMEXTENT

 !###======================================================================
 SUBROUTINE UTL_TIMING(IBDT,IEDT,ELSEC)
 !###======================================================================
 IMPLICIT NONE
 INTEGER, DIMENSION(8), INTENT(IN) :: IBDT,IEDT
 REAL(KIND=DP_KIND), INTENT(OUT) :: ELSEC
 INTEGER, PARAMETER :: NSPD = 86400
 INTEGER, DIMENSION(12) :: IDPM(12)
 DATA IDPM/31,28,31,30,31,30,31,31,30,31,30,31/ ! DAYS PER MONTH
 INTEGER :: NDAYS, LEAP, IBD, IED, MB, ME, MC, M, NM

 !## calculate elapsed time in days and seconds
 NDAYS=0
 LEAP=0
 IF (MOD(IEDT(1),4).EQ.0) LEAP = 1
 IBD = IBDT(3)            ! begin day
 IED = IEDT(3)            ! end day
!   find days
 IF (IBDT(2).NE.IEDT(2)) THEN
!     months differ
  MB = IBDT(2)             ! begin month
  ME = IEDT(2)             ! end month
  NM = ME-MB+1             ! number of months to look at
  IF (MB.GT.ME) NM = NM+12
   MC=MB-1
   DO 10 M=1,NM
    MC=MC+1                ! MC IS CURRENT MONTH
    IF (MC.EQ.13) MC = 1
    IF (MC.EQ.MB) THEN
     NDAYS = NDAYS+IDPM(MC)-IBD
     IF (MC.EQ.2) NDAYS = NDAYS + LEAP
    ELSEIF (MC.EQ.ME) THEN
     NDAYS = NDAYS+IED
    ELSE
     NDAYS = NDAYS+IDPM(MC)
     IF (MC.EQ.2) NDAYS = NDAYS + LEAP
    ENDIF
10   CONTINUE
  ELSEIF (IBD.LT.IED) THEN
!     start and end in same month, only account for days
   NDAYS = IED-IBD
  ENDIF
   ELSEC=NDAYS*NSPD
!
  !## add or subtract seconds
  ELSEC = ELSEC+(IEDT(5)-IBDT(5))*3600.0D0
  ELSEC = ELSEC+(IEDT(6)-IBDT(6))*60.0D0
  ELSEC = ELSEC+(IEDT(7)-IBDT(7))
  ELSEC = ELSEC+(IEDT(8)-IBDT(8))*0.01D0

  END SUBROUTINE UTL_TIMING

 !###======================================================================
 SUBROUTINE UTL_MINTHICKNESS(TOP,BOT,HK,VK,VA, &
           TOP_BU,BOT_BU,HK_BU,VK_BU,VA_BU,BND,TH,MINTHICKNESS,NLAY,ICOL,IROW)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NLAY,ICOL,IROW
 INTEGER, PARAMETER :: DP_KIND=8
 INTEGER,INTENT(INOUT),DIMENSION(NLAY) :: BND
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(NLAY) :: TOP,BOT,HK,VK,VA,TOP_BU,BOT_BU,HK_BU,VK_BU,VA_BU
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(NLAY,2) :: TH
 REAL(KIND=DP_KIND),INTENT(IN) :: MINTHICKNESS
 INTEGER :: ILAY,JLAY
 REAL(KIND=DP_KIND) :: T,B,T1,T2,KD,MT,F,TT1,TT2,DT,TT,TC,KV
 INTEGER :: IU,I
 CHARACTER(LEN=24) :: FRM
 
 !## make sure no negative-thicknesses in original set
 DO ILAY=1,NLAY
  IF(BND(ILAY).EQ.0)CYCLE
  IF(ILAY.GT.1)TOP(ILAY)=MIN(TOP(ILAY),BOT(ILAY-1))
  BOT(ILAY)=MIN(TOP(ILAY),BOT(ILAY))
 ENDDO
 
 TH=0.0D0
 !## get thickness of aquifers
 DO ILAY=1,NLAY;   IF(BND(ILAY).EQ.0)CYCLE; TH(ILAY,1)=TOP(ILAY)-BOT(ILAY); ENDDO
 !## get thickness of aquitards
 DO ILAY=1,NLAY-1; IF(BND(ILAY).EQ.0.OR.BND(ILAY+1).EQ.0)CYCLE; TH(ILAY,2)=BOT(ILAY)-TOP(ILAY+1); ENDDO
 
 !## need to have data 
 DO ILAY=1,NLAY
  IF(BND(ILAY).NE.0)THEN
   IF(TH(ILAY,1).GT.0.0D0)THEN
    IF(HK(ILAY).LE.0.0D0)THEN
     WRITE(*,'(A,4I5,F15.7)') 'HK',ICOL,IROW,ILAY,BND(ILAY),HK(ILAY)
     BND(ILAY)=0; TH(ILAY,1)=0.0D0
    ENDIF
    IF(VA(ILAY).LE.0.0D0)THEN
     WRITE(*,'(A,4I5,F15.7)') 'VA',ICOL,IROW,ILAY,BND(ILAY),VA(ILAY)
     BND(ILAY)=0; TH(ILAY,1)=0.0D0
    ENDIF
   ENDIF
   IF(TH(ILAY,2).GT.0.0D0)THEN
    IF(VK(ILAY).LE.0.0D0)THEN
     WRITE(*,'(A,4I5,F15.7)') 'VK',ICOL,IROW,ILAY,BND(ILAY),VK(ILAY)
     TH(ILAY,2)=0.0D0
    ENDIF
   ENDIF
  ENDIF
 ENDDO

 !## nothing to do
 IF(SUM(TH(:,1))+SUM(TH(:,2)).EQ.0.0D0)RETURN

 !## make backup
 DO ILAY=1,NLAY
  TOP_BU(ILAY)=TOP(ILAY); BOT_BU(ILAY)=BOT(ILAY); HK_BU(ILAY)=HK(ILAY); VA_BU(ILAY)=VA(ILAY)
  IF(ILAY.LT.NLAY)VK_BU(ILAY) =VK(ILAY)
 ENDDO

 !## total thickness
 TT1=0.0D0; TT2=0.0D0; DO ILAY=1,NLAY
  IF(BND(ILAY).NE.0)THEN
   TT1=TT1+TH(ILAY,1)
   IF(ILAY.LE.NLAY)TT2=TT2+TH(ILAY,2)
  ENDIF
 ENDDO
 !## minimal appropriate layer thickness
 MT=(TT1+TT2)/DBLE(NLAY); MT=MIN(MINTHICKNESS,MT)
 
 !## remove everything - model here is too thin
 IF(MT.LT.0.25D0*MINTHICKNESS)THEN
  DO ILAY=1,NLAY
   BND(ILAY)=0
   TOP(ILAY)=TOP(1)
   BOT(ILAY)=TOP(1)
   HK(ILAY)=0.0D0
   VA(ILAY)=0.0D0
   VK(ILAY)=0.0D0
  ENDDO
  RETURN
 ENDIF
 
 !## adjust thicknesses
 DO ILAY=1,NLAY
  !## skip inactive 
  IF(BND(ILAY).EQ.0)CYCLE
  IF(TH(ILAY,1).LT.MT)THEN
   DT=MT-TH(ILAY,1); TH(ILAY,1)=MT
   !## reduce for this correction
   DO JLAY=ILAY,NLAY
    
    !## possible to correct from aquitard
    TT=0.0D0
    IF(JLAY.LT.NLAY)THEN
     TT=TH(JLAY,2)-DT
     IF(TT.GT.0.0D0)THEN
      TT=DT
     !## limited space
     ELSE
      TT=DT+TT
     ENDIF
     TH(JLAY,2)=TH(JLAY,2)-TT   
    ENDIF

    !## reduced to be corrected quantity, stop if all corrected
    DT=DT-TT; IF(DT.LE.0.0D0)EXIT
    
    TT=0.0D0
    !## possible to correct from aquifer
    IF(JLAY.GT.ILAY)THEN
     TT=TH(JLAY,1)-DT
     !## enough space
     IF(TT.GT.0.0D0)THEN
      TT=DT
     !## limited space
     ELSE
      TT=DT+TT
     ENDIF
     TH(JLAY,1)=TH(JLAY,1)-TT
    ENDIF
    
    !## reduced to be corrected quantity, stop if all corrected
    DT=DT-TT; IF(DT.LE.0.0D0)EXIT

   ENDDO   
  ENDIF
 ENDDO

 !## recompute new top/bottoms
 DO ILAY=1,NLAY
  IF(BND(ILAY).EQ.0)CYCLE
  IF(ILAY.GT.1)TOP(ILAY)=BOT(ILAY-1)-TH(ILAY-1,2)
  BOT(ILAY)=TOP(ILAY)-TH(ILAY,1) 
 ENDDO

 !## may never exceed bottom
 DO JLAY=NLAY,1,-1; IF(BND(JLAY).NE.0)EXIT; ENDDO
 !## include aquitard
 IF(JLAY.EQ.NLAY)THEN
  TT=BOT_BU(JLAY)
 ELSE
  TT=TOP_BU(JLAY+1)
 ENDIF
 !## set all below to this "new" base
 DO ILAY=JLAY+1,NLAY
  BOT(ILAY)=TT; TOP(ILAY)=TT
 ENDDO
 
 DO ILAY=JLAY,1,-1
  BOT(ILAY)=MAX(BOT(ILAY),TT)
  TT=TT+MT
  TOP(ILAY)=MAX(TOP(ILAY),TT)
 ENDDO

 TH=0.0D0
 !## get thickness of aquifers
 DO ILAY=1,NLAY;   IF(BND(ILAY).EQ.0)CYCLE; TH(ILAY,1)=TOP(ILAY)-BOT(ILAY); ENDDO
 !## get thickness of aquitards
 DO ILAY=1,NLAY-1; IF(BND(ILAY).EQ.0)CYCLE; TH(ILAY,2)=BOT(ILAY)-TOP(ILAY+1); ENDDO

 !## correct permeabilities for aquifers - leave k-aquitard intact
 DO ILAY=1,NLAY

  !## skip inactive cells
  IF(BND(ILAY).EQ.0)CYCLE
  
  KD=0.0D0
  TC=0.0D0

  !## add existing aquifers
  DO JLAY=1,NLAY
   T =MIN(TOP(ILAY),TOP_BU(JLAY)) 
   B =MAX(BOT(ILAY),BOT_BU(JLAY)) 
   TT=T-B
   IF(TT.GT.0.0D0)THEN
    KD=KD+TT*HK_BU(JLAY)
    IF(VA_BU(JLAY).NE.0.0D0.AND.HK_BU(JLAY).NE.0.0D0)THEN
     TC=TC+TT/(HK_BU(JLAY)/VA_BU(JLAY))
    ENDIF
   ENDIF
  ENDDO

  !## add existing aquitards
  DO JLAY=1,NLAY-1
   IF(BND(JLAY+1).EQ.0)CYCLE
   T =MIN(TOP(ILAY),BOT_BU(JLAY)) 
   B =MAX(BOT(ILAY),TOP_BU(JLAY+1)) 
   TT=T-B
   IF(TT.GT.0.0D0)THEN
    IF(VK_BU(JLAY).NE.0.0D0)THEN
     KD=KD+TT*VK_BU(JLAY)
     TC=TC+TT/VK_BU(JLAY)
    ENDIF
   ENDIF
  ENDDO

  TT=TOP(ILAY)-BOT(ILAY)
  HK(ILAY)=KD/TT
  KV=HK(ILAY); IF(TC.NE.0.0D0)KV=TT/TC
  
  VA(ILAY)=HK(ILAY)/KV
  
 ENDDO
 
 !## get thickness of aquifers
 TH=0.0D0
 DO ILAY=1,NLAY;   IF(BND(ILAY).EQ.0)CYCLE; TH(ILAY,1)=TOP(ILAY)-BOT(ILAY); ENDDO
 !## get thickness of aquitards
 DO ILAY=1,NLAY-1; IF(BND(ILAY).EQ.0)CYCLE; TH(ILAY,2)=BOT(ILAY)-TOP(ILAY+1); ENDDO

 !## get total sum of transmissivity
 TT=0.0D0; DO ILAY=1,NLAY
  IF(BND(ILAY).EQ.0)CYCLE
  TT=TT+TH(ILAY,1)*HK(ILAY)
  IF(ILAY.LT.NLAY)THEN
   IF(BND(ILAY+1).NE.0)TT=TT+TH(ILAY,2)*VK(ILAY)
  ENDIF
 ENDDO
 !## get total vertical resistance
 TC=0.0D0; DO ILAY=1,NLAY
  IF(BND(ILAY).EQ.0)CYCLE
  TC=TC+TH(ILAY,1)/(HK(ILAY)/VA(ILAY))
  IF(ILAY.LT.NLAY)THEN
   IF(VK(ILAY).NE.0.0D0.AND.BND(ILAY+1).NE.0)TC=TC+TH(ILAY,2)/VK(ILAY)
  ENDIF
 ENDDO
 TT1=TT; TT2=TC
 
 !## get thickness of aquifers
 TH=0.0D0
 DO ILAY=1,NLAY;   IF(BND(ILAY).EQ.0)CYCLE; TH(ILAY,1)=TOP_BU(ILAY)-BOT_BU(ILAY); ENDDO
 !## get thickness of aquitards
 DO ILAY=1,NLAY-1; IF(BND(ILAY).EQ.0)CYCLE; TH(ILAY,2)=BOT_BU(ILAY)-TOP_BU(ILAY+1); ENDDO

 !## get total sum of transmissivity
 TT=0.0D0; DO ILAY=1,NLAY
  IF(BND(ILAY).EQ.0)CYCLE
  TT=TT+TH(ILAY,1)*HK_BU(ILAY)
  IF(ILAY.LT.NLAY)THEN
   IF(BND(ILAY+1).NE.0)TT=TT+TH(ILAY,2)*VK_BU(ILAY)
  ENDIF
 ENDDO
 !## get total vertical resistance
 TC=0.0D0; DO ILAY=1,NLAY
  IF(BND(ILAY).EQ.0)CYCLE
  IF((HK_BU(ILAY)/VA_BU(ILAY)).GT.0.0D0)TC=TC+TH(ILAY,1)/(HK_BU(ILAY)/VA_BU(ILAY))
  IF(ILAY.LT.NLAY)THEN
   IF(BND(ILAY+1).NE.0)TC=TC+TH(ILAY,2)/VK_BU(ILAY)
  ENDIF
 ENDDO
 T1=TT; T2=TC

 IF(ABS(TT1-T1).GT.0.01D0*T1.OR.ABS(TT2-T2).GT.0.01D0*T2)THEN
  IU=UTL_GETUNIT(); OPEN(IU,FILE='VAR.F90',STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED')
  WRITE(IU,*) 'ICOL,IROW=',ICOL,IROW
  WRITE(FRM,'(A3,I3.3,A10)') '(A,',NLAY,'(I5,A1),A)'
  WRITE(IU,FRM) 'DATA BND/',(BND(I),',',I=1,NLAY-1),BND(NLAY),'/'
  WRITE(FRM,'(A3,I3.3,A13)') '(A,',NLAY,'(F10.3,A1),A)'
  WRITE(IU,FRM) 'DATA TOP/',(TOP(I),',',I=1,NLAY-1),TOP(NLAY),'/'
  WRITE(IU,FRM) 'DATA BOT/',(BOT(I),',',I=1,NLAY-1),BOT(NLAY),'/'
  WRITE(IU,FRM) 'DATA HK/',(HK(I),',',I=1,NLAY-1),HK(NLAY),'/'
  WRITE(IU,FRM) 'DATA VA/',(VA(I),',',I=1,NLAY-1),VA(NLAY),'/'
  WRITE(IU,FRM) 'DATA VK/',(VK(I),',',I=1,NLAY-1),VK(NLAY),'/'
  WRITE(IU,FRM) 'DATA TOP_BU/',(TOP_BU(I),',',I=1,NLAY-1),TOP_BU(NLAY),'/'
  WRITE(IU,FRM) 'DATA BOT_BU/',(BOT_BU(I),',',I=1,NLAY-1),BOT_BU(NLAY),'/'
  WRITE(IU,FRM) 'DATA HK_BU/',(HK_BU(I),',',I=1,NLAY-1),HK_BU(NLAY),'/'
  WRITE(IU,FRM) 'DATA VA_BU/',(VA_BU(I),',',I=1,NLAY-1),VA_BU(NLAY),'/'
  WRITE(IU,FRM) 'DATA VK_BU/',(VK_BU(I),',',I=1,NLAY-1),VK_BU(NLAY),'/'
  CLOSE(IU)
  IF(ABS(TT1-T1).GT.0.01D0*T1)WRITE(*,'(/1X,A,2F15.7/)') 'Error in consistency check KD ',TT1,T1
  IF(ABS(TT2-T2).GT.0.01D0*T2)WRITE(*,'(/1X,A,2F15.7/)') 'Error in consistency check VC ',TT2,T2
 ENDIF
 
 !## minimal horizontal k first layer is 0.5m2/d
 ILAY=1; KD=(TOP(ILAY)-BOT(ILAY))*HK(ILAY)
 F=KD/0.5D0; IF(F.GT.0.0D0.AND.F.LT.1.0D0)HK(ILAY)=HK(ILAY)/F

 END SUBROUTINE UTL_MINTHICKNESS

 !###======================================================================
 SUBROUTINE UTL_MINTHICKNESS_COMPLEX(TOP,BOT,HK,VK,VA, &
           TOP_BU,BOT_BU,HK_BU,VK_BU,VA_BU,BND,TH,MINTHICKNESS)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT),DIMENSION(:) :: BND
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(:) :: TOP,BOT,HK,VK,VA,TOP_BU,BOT_BU,HK_BU,VK_BU,VA_BU
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(:,:) :: TH
 REAL(KIND=DP_KIND),INTENT(IN) :: MINTHICKNESS
 INTEGER :: NLAY,ILAY,IL !N,IL1,IL2
 REAL(KIND=DP_KIND) :: K,T,B,T1,T2,B1,D,KD,VC,MT,F,TT1,TT2  !,K1,K2

 NLAY=SIZE(BND)

 !## make sure no negative-thicknesses in original set
 DO ILAY=1,NLAY
  IF(ILAY.GT.1)TOP(ILAY)=MIN(TOP(ILAY),BOT(ILAY-1))
  BOT(ILAY)=MIN(TOP(ILAY),BOT(ILAY))
 ENDDO
 !## clean boundary for zero-thickness layers from the bottom
 DO ILAY=NLAY,1,-1
  IF(TOP(ILAY)-BOT(ILAY).EQ.0.0D0)THEN
   BND(ILAY)=0
  ELSE
   EXIT
  ENDIF
 ENDDO
 
 TH=0.0D0
 !## get thickness of aquifers
 DO ILAY=1,NLAY;   TH(ILAY,1)=TOP(ILAY)-BOT(ILAY); ENDDO
 !## get thickness of aquitards
 DO ILAY=1,NLAY-1; TH(ILAY,2)=BOT(ILAY)-TOP(ILAY+1); ENDDO

 !## need to have data 
 DO ILAY=1,NLAY
  IF(TH(ILAY,1).GT.0)THEN
   IF(HK(ILAY).LE.0.0)WRITE(*,*) 'HK',HK(ILAY),BND(ILAY)
   IF(VA(ILAY).LE.0.0)WRITE(*,*) 'VA',VA(ILAY),BND(ILAY)
  ENDIF
  IF(TH(ILAY,2).GT.0)THEN
   IF(VK(ILAY).LE.0.0)WRITE(*,*) 'VK',VK(ILAY),BND(ILAY)
  ENDIF
 ENDDO

 !## nothing to do
 IF(SUM(TH(:,1)).EQ.0.0D0)RETURN

 !## make backup
 DO ILAY=1,NLAY
  TOP_BU(ILAY)=TOP(ILAY); BOT_BU(ILAY)=BOT(ILAY); HK_BU(ILAY)=HK(ILAY); VA_BU(ILAY)=VA(ILAY)
  IF(ILAY.LT.NLAY)VK_BU(ILAY) =VK(ILAY)
 ENDDO

 !## total thickness
 TT1=0.0D0; DO ILAY=1,NLAY; TT1=TT1+TH(ILAY,1); IF(ILAY.LE.NLAY)TT1=TT1+TH(ILAY,2); ENDDO
 MT=MIN(MINTHICKNESS,TT1/DBLE(NLAY))
 
 !## adjust thicknesses
 D=0.0D0
 DO ILAY=1,NLAY
  !## aquifer to thin
  IF(TH(ILAY,1).LT.MT)THEN
   !## corrected by
   D=D+(MT-TH(ILAY,1)); TH(ILAY,1)=MT
  ENDIF
  !## so go grab that from an aquitard - might become zero
  IF(TH(ILAY,2).GT.0.0)THEN
   !## what can be corrected in this aquitard
   F=TH(ILAY,2)-D; D=0.0D0; IF(F.LT.0.0D0)D=ABS(F)
   TH(ILAY,2)=MAX(0.0D0,F)
  ENDIF
 ENDDO
 
 !## correct in case new thickness is more than original thickness - use fraction
 TT2=0.0D0; DO ILAY=1,NLAY; TT2=TT2+TH(ILAY,1); IF(ILAY.LE.NLAY)TT2=TT2+TH(ILAY,2); ENDDO
 IF(TT2.GT.TT1)THEN
  F=TT1/TT2
  DO ILAY=1,NLAY
   TH(ILAY,1)=TH(ILAY,1)*F
   TH(ILAY,2)=TH(ILAY,2)*F
  ENDDO
 ENDIF
 
 !## check total thickness
 TT2=0.0D0; DO ILAY=1,NLAY
  TT2=TT2+TH(ILAY,1); IF(ILAY.LE.NLAY)TT2=TT2+TH(ILAY,2)
 ENDDO

 !## recompute all levels from the corrected thicknesses
 DO ILAY=1,NLAY
  IF(ILAY.GT.1)TOP(ILAY)=BOT(ILAY-1)-TH(ILAY-1,2)
  BOT(ILAY)=TOP(ILAY)-TH(ILAY,1)
 ENDDO
 
 !## correct permeabilities for aquifers
 DO ILAY=1,NLAY

  !## skip inactive cells
  IF(BND(ILAY).EQ.0)CYCLE

  !## current corrected layer
  T=TOP(ILAY); B=BOT(ILAY)

  !## if layer thickness, leave it - could be possible most lower layer(s)
  IF(T-B.LE.0.0D0)THEN

   HK(ILAY)=0.0D0; VA(ILAY)=0.0D0

  ELSE

   KD=0.0D0; VC=0.0D0
   DO IL=1,NLAY

    !## skip inactive cells
    IF(BND(IL).EQ.0)CYCLE

    T1=TOP_BU(IL); B1=BOT_BU(IL)

    D=MIN(T,T1)-MAX(B,B1)
    !## part of aquifer
    IF(D.GT.0.0D0)THEN
     KD=KD+HK_BU(IL)*D
     VC=VC+D/(HK_BU(IL)*VA_BU(IL))
    ENDIF

    IF(IL.LT.NLAY)THEN

     T1=BOT_BU(IL); B1=TOP_BU(IL+1)
     D=MIN(T,T1)-MAX(B,B1)
     !## part of aquitard
     IF(D.GT.0.0D0)THEN
      KD=KD+VK_BU(IL)*D
      VC=VC+D/(VK_BU(IL))
     ENDIF

    ENDIF

   ENDDO
   !## new parameters
   HK(ILAY)=KD/(T-B)
   K=HK(ILAY); IF(VC.GT.0.0)K=(T-B)/VC
   VA(ILAY)=K/HK(ILAY)

  ENDIF

 ENDDO

 !## correct permeabilities for aquitards
 DO ILAY=1,NLAY-1

  !## skip inactive cells
  IF(BND(ILAY).EQ.0.OR.BND(ILAY+1).EQ.0)CYCLE

  !## original layer
  T=BOT_BU(ILAY); B=TOP_BU(ILAY+1)

  !## if layer thickness, leave it - could be possible most lower layer(s)
  IF(T-B.GT.0.0D0)THEN

   !## previous c
   VC=(T-B)/VK_BU(ILAY)

   !## current corrected layer
   T=BOT(ILAY); B=TOP(ILAY+1)
   VK(ILAY)=(T-B)/VC

  ENDIF

 ENDDO

 !## check before and after

 !## get thickness of aquifers/aquitards
 TH=0.0D0

 !## make sure vka is not zero
 DO ILAY=1,NLAY; IF(VA(ILAY).LE.0.0D0)VA(ILAY)=1.0D0; ENDDO
 
 !## get total conductance
 DO ILAY=1,NLAY;   TH(ILAY,1)=TH(ILAY,1)+(TOP(ILAY) -BOT(ILAY)  )*HK(ILAY); ENDDO
 DO ILAY=1,NLAY-1; TH(ILAY,1)=TH(ILAY,1)+((BOT(ILAY)-TOP(ILAY+1))*VK(ILAY)); ENDDO

 !## get total conductance
 DO ILAY=1,NLAY;   TH(ILAY,2)=TH(ILAY,2)+(TOP_BU(ILAY) -BOT_BU(ILAY)  )*HK_BU(ILAY); ENDDO
 DO ILAY=1,NLAY-1; TH(ILAY,2)=TH(ILAY,2)+((BOT_BU(ILAY)-TOP_BU(ILAY+1))*VK_BU(ILAY)); ENDDO

 !## get total conductances
 T1=SUM(TH(:,1)); T2=SUM(TH(:,2)) 
 !## procent
 F=100.0D0*(T1/T2); F=ABS(100.0D0-F)
 IF(F.GT.0.5D0)THEN 
  !## get thickness of aquifers
  WRITE(*,'(A3,4F9.2)') 'TKD',T1,T2,T2-T1,F
  DO ILAY=1,NLAY
   WRITE(*,'(I3,8F9.2)') ILAY,TOP(ILAY)   ,BOT(ILAY)   ,HK(ILAY)    ,(TOP(ILAY)   -BOT(ILAY))   *HK(ILAY), &
                               TOP_BU(ILAY),BOT_BU(ILAY),HK_BU(ILAY),(TOP_BU(ILAY)-BOT_BU(ILAY))*HK_BU(ILAY)
  ENDDO
 ENDIF

 END SUBROUTINE UTL_MINTHICKNESS_COMPLEX
 
 !###======================================================================
 SUBROUTINE UTL_DRAWELLIPSE(X,Y,DX,DY,A)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Y,DX,DY,A
 REAL(KIND=DP_KIND) :: THETA,DTHETA
 REAL(KIND=DP_KIND) :: XP,YP,AR,FY

 AR=A/(360.0D0/(2.0D0*PI))
 FY=DY/DX

 THETA=0.0D0; DTHETA=PI/50.0D0
 DO
  CALL UTL_POINTELLIPSE(X,Y,THETA,FY,DX,AR,XP,YP)
  IF(THETA.EQ.0.0D0)THEN
   CALL DBL_IGRMOVETO(XP,YP)
  ELSE
   CALL DBL_IGRLINETO(XP,YP)
  ENDIF
  THETA=THETA+DTHETA
  IF(THETA.GT.2.0D0*PI)EXIT
 ENDDO

 END SUBROUTINE UTL_DRAWELLIPSE

 !###======================================================================
 SUBROUTINE UTL_POINTELLIPSE(X,Y,THETA,FY,DX,AR,XP,YP)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: FY,THETA,DX,AR,X,Y
 REAL(KIND=DP_KIND),INTENT(OUT) :: XP,YP
 REAL(KIND=DP_KIND) :: XR,YR

 XP=    DX*COS(THETA); XR=XP
 YP=-FY*DX*SIN(THETA); YR=YP
 IF(AR.NE.0.0D0)THEN
  XR= COS(AR)*XP+SIN(AR)*YP
  YR=-SIN(AR)*XP+COS(AR)*YP
 ENDIF
 XP=X+XR
 YP=Y+YR

 END SUBROUTINE UTL_POINTELLIPSE

 !###======================================================================
 LOGICAL FUNCTION UTL_INSIDEELLIPSE(X0,Y0,DX,DY,A,XP,YP)
 !###======================================================================
 !https://stackoverflow.com/questions/7946187/point-and-ellipse-rotated-position-test-algorithm
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: XP,YP,X0,Y0,DX,DY,A
 REAL(KIND=DP_KIND) :: X1,X2,X3,A2,B2,AR

 UTL_INSIDEELLIPSE=.FALSE.

 AR=A/(360.0D0/(2.0D0*PI))
 AR=-1.0D0*AR

 X1=( (XP-X0)*COS(AR)+(YP-Y0)*SIN(AR) ) **2.0D0
 X2=( (XP-X0)*SIN(AR)-(YP-Y0)*COS(AR) ) **2.0D0
 A2= DX**2.0D0
 B2= DY**2.0D0

 X3=X1/A2+X2/B2

 UTL_INSIDEELLIPSE=X3.LE.1.0D0

 END FUNCTION UTL_INSIDEELLIPSE

 !###======================================================================
 INTEGER FUNCTION UTL_COUNT_COLUMNS(LINE,SEP,BPV,EPV)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: LINE,SEP
 INTEGER,DIMENSION(:),OPTIONAL,INTENT(OUT) :: BPV,EPV
 INTEGER :: I,J,K,IOUT,NCSEP
 CHARACTER(LEN=1),ALLOCATABLE,DIMENSION(:) :: CSEP

 NCSEP=LEN_TRIM(SEP); ALLOCATE(CSEP(NCSEP))
 DO I=1,NCSEP; CSEP(I)=SEP(I:I); ENDDO

 IF(PRESENT(EPV))EPV=0
 IF(PRESENT(BPV))THEN; BPV=0; BPV(1)=1; ENDIF

! !## remove duplicate spaces in Line
! DO
!  IF(INDEX(LINE,'  ').EQ.0)EXIT
!  LINE=UTL_SUBST(LINE,'  ',' ')
! ENDDO

 !## look for first non-blank character
 DO I=1,LEN_TRIM(LINE)
  IF(LINE(I:I).NE.' ')EXIT
 ENDDO
 I=I-1

 J=1; IOUT=1
 DO
  I=I+1

  DO K=1,NCSEP
   IF(LINE(I:I).EQ.CSEP(K).AND.IOUT.EQ.1)THEN
    IF(PRESENT(EPV))EPV(J)=I-1
    J=J+1
    IF(PRESENT(BPV))BPV(J)=I+1
    EXIT
   ENDIF
  ENDDO

  !## inside quoted string
  IF(LINE(I:I).EQ.CHAR(34).OR. &             !'
     LINE(I:I).EQ.CHAR(39).OR. &             !"
     LINE(I:I).EQ.CHAR(96))IOUT=ABS(IOUT-1)  !`

  !## stop, whole line examined
  IF(I.EQ.LEN_TRIM(LINE))EXIT
  
!  !## stop if last character is a comma
!  IF(LINE(I:I).EQ.',')EXIT
  
 ENDDO
 IF(PRESENT(EPV))EPV(J)=LEN_TRIM(LINE)

 !## number of columns is number of comma-delimiters + 1
 IF(LINE(I:I).EQ.',')J=J-1
 UTL_COUNT_COLUMNS=J

 END FUNCTION UTL_COUNT_COLUMNS

 !###======================================================================
 LOGICAL FUNCTION UTL_READPOINTER(IU,NPOINTER,IPOINTER,TXT,IOPT,EXCLVALUE,IECHO)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOPT,IU !## ioptional
 INTEGER,INTENT(IN),OPTIONAL :: EXCLVALUE,IECHO !## wrong value
 INTEGER,INTENT(OUT) :: NPOINTER
 INTEGER,POINTER,DIMENSION(:) :: IPOINTER
 CHARACTER(LEN=*),INTENT(IN) :: TXT
 INTEGER :: IOS,I,N,JECHO,I1,I2
 CHARACTER(LEN=100*256) :: LINE

 JECHO=1; IF(PRESENT(IECHO))THEN
  JECHO=IECHO
 ENDIF

 NPOINTER=0
 IF(IOPT.EQ.0)THEN
  UTL_READPOINTER=.FALSE.
  IF(.NOT.UTL_READINITFILE(TRIM(TXT),LINE,IU,IOPT))RETURN
 ELSEIF(IOPT.EQ.1)THEN
  UTL_READPOINTER=.TRUE.
  IF(.NOT.UTL_READINITFILE(TRIM(TXT),LINE,IU,IOPT))RETURN
 ENDIF

 I=INDEX(LINE,':')
 IF(I.GT.0)THEN
  READ(LINE(1:I-1),*) I1
  READ(LINE(I+1:),*)  I2
  N=(I2-I1)+1
 ELSE
  N=UTL_COUNT_COLUMNS(LINE,',')
 ENDIF

 IF(ASSOCIATED(IPOINTER))THEN
  NPOINTER=SIZE(IPOINTER)
  IF(NPOINTER.LT.N)DEALLOCATE(IPOINTER)
 ENDIF
 NPOINTER=N
 IF(.NOT.ASSOCIATED(IPOINTER))ALLOCATE(IPOINTER(NPOINTER))

 IF(I.GT.0)THEN
  DO I=1,NPOINTER; IPOINTER(I)=I1+I-1; ENDDO; IOS=0
 ELSE
  READ(LINE,*,IOSTAT=IOS) (IPOINTER(I),I=1,NPOINTER)
 ENDIF
 IF(IOS.NE.0)THEN
  IF(JECHO.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'ERROR READING '//TRIM(TXT),'Error')
  IF(JECHO.EQ.1)WRITE(*,'(/1A/)') 'ERROR READING '//TRIM(TXT)
  STOP
 ENDIF

 IF(JECHO.EQ.1)WRITE(*,'(99A)') TRIM(TXT)//'=',(TRIM(ITOS(IPOINTER(I)))//',',I=1,NPOINTER-1),TRIM(ITOS(IPOINTER(NPOINTER)))

 IF(PRESENT(EXCLVALUE))THEN
  DO,I=1,NPOINTER
   IF(IPOINTER(I).EQ.EXCLVALUE)THEN
    DEALLOCATE(IPOINTER); RETURN
   ENDIF
  ENDDO
 ENDIF

 UTL_READPOINTER=.TRUE.

 END FUNCTION UTL_READPOINTER

 !###======================================================================
 LOGICAL FUNCTION UTL_READPOINTER_REAL(IU,NPOINTER,XPOINTER,TXT,IOPT,EXCLVALUE,IECHO)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOPT,IU !## ioptional
 REAL(KIND=DP_KIND),INTENT(IN),OPTIONAL :: EXCLVALUE,IECHO !## wrong value
 INTEGER,INTENT(OUT) :: NPOINTER
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: XPOINTER
 CHARACTER(LEN=*),INTENT(IN) :: TXT
 INTEGER :: IOS,I,N,JECHO
 CHARACTER(LEN=3*256) :: LINE

 JECHO=1; IF(PRESENT(IECHO))THEN
  JECHO=IECHO
 ENDIF

 NPOINTER=0
 IF(IOPT.EQ.0)THEN
  UTL_READPOINTER_REAL=.FALSE.
  IF(.NOT.UTL_READINITFILE(TRIM(TXT),LINE,IU,0))RETURN
 ELSEIF(IOPT.EQ.1)THEN
  UTL_READPOINTER_REAL=.TRUE.
  IF(.NOT.UTL_READINITFILE(TRIM(TXT),LINE,IU,1))RETURN
 ENDIF

 N=UTL_COUNT_COLUMNS(LINE,',')

 IF(ASSOCIATED(XPOINTER))THEN
  NPOINTER=SIZE(XPOINTER)
  IF(NPOINTER.LT.N)DEALLOCATE(XPOINTER)
 ENDIF
 NPOINTER=N
 IF(.NOT.ASSOCIATED(XPOINTER))ALLOCATE(XPOINTER(NPOINTER))

 READ(LINE,*,IOSTAT=IOS) (XPOINTER(I),I=1,NPOINTER)
 IF(IOS.NE.0)THEN
  IF(JECHO.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'ERROR READING '//TRIM(TXT),'Error')
  IF(JECHO.EQ.1)WRITE(*,'(/1A/)') 'ERROR READING '//TRIM(TXT); STOP
 ENDIF

 WRITE(*,'(A,99(F10.2,A1))') TRIM(TXT)//'=',(XPOINTER(I),',',I=1,NPOINTER)

 IF(PRESENT(EXCLVALUE))THEN
  DO,I=1,NPOINTER
   IF(XPOINTER(I).EQ.EXCLVALUE)THEN
    DEALLOCATE(XPOINTER); RETURN
   ENDIF
  ENDDO
 ENDIF

 UTL_READPOINTER_REAL=.TRUE.

 END FUNCTION UTL_READPOINTER_REAL

 !###======================================================================
 LOGICAL FUNCTION UTL_READPOINTER_CHARACTER(IU,NPOINTER,CPOINTER,TXT,IOPT,IECHO)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOPT,IU !## ioptional
 REAL(KIND=DP_KIND),INTENT(IN),OPTIONAL :: IECHO !## wrong value
 INTEGER,INTENT(OUT) :: NPOINTER
 CHARACTER(LEN=*),POINTER,DIMENSION(:) :: CPOINTER
 CHARACTER(LEN=*),INTENT(IN) :: TXT
 INTEGER :: IOS,I,N,JECHO
 CHARACTER(LEN=3*256) :: LINE

 JECHO=1; IF(PRESENT(IECHO))THEN
  JECHO=IECHO
 ENDIF

 NPOINTER=0
 IF(IOPT.EQ.0)THEN
  UTL_READPOINTER_CHARACTER=.FALSE.
  IF(.NOT.UTL_READINITFILE(TRIM(TXT),LINE,IU,0))RETURN
 ELSEIF(IOPT.EQ.1)THEN
  UTL_READPOINTER_CHARACTER=.TRUE.
  IF(.NOT.UTL_READINITFILE(TRIM(TXT),LINE,IU,1))RETURN
 ENDIF

 N=UTL_COUNT_COLUMNS(LINE,',')

 IF(ASSOCIATED(CPOINTER))THEN
  NPOINTER=SIZE(CPOINTER)
  IF(NPOINTER.LT.N)DEALLOCATE(CPOINTER)
 ENDIF
 NPOINTER=N
 IF(.NOT.ASSOCIATED(CPOINTER))ALLOCATE(CPOINTER(NPOINTER))

 READ(LINE,*,IOSTAT=IOS) (CPOINTER(I),I=1,NPOINTER)
 IF(IOS.NE.0)THEN
  IF(JECHO.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'ERROR READING '//TRIM(TXT),'Error')
  IF(JECHO.EQ.1)WRITE(*,'(/1A/)') 'ERROR READING '//TRIM(TXT); STOP
 ENDIF

 WRITE(*,'(A,99(A,A1))') TRIM(TXT)//'=',(TRIM(CPOINTER(I)),',',I=1,NPOINTER)

 UTL_READPOINTER_CHARACTER=.TRUE.

 END FUNCTION UTL_READPOINTER_CHARACTER

 !###======================================================================
 SUBROUTINE UTL_MEASUREMAIN()
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),DIMENSION(:),POINTER :: XCRD,YCRD=>NULL()
 INTEGER :: NCRD

 IF(ASSOCIATED(XCRD))DEALLOCATE(XCRD)
 IF(ASSOCIATED(YCRD))DEALLOCATE(YCRD)
 CALL UTL_MEASURE(XCRD,YCRD,NCRD,ID_CURSORDISTANCE)
 DEALLOCATE(XCRD,YCRD)

 END SUBROUTINE UTL_MEASUREMAIN

 !###======================================================================
 SUBROUTINE UTL_MEASURE(XCRD,YCRD,NCRD,IDCURSOR)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),DIMENSION(:),POINTER,INTENT(INOUT) :: XCRD,YCRD
 REAL(KIND=DP_KIND),DIMENSION(:),POINTER :: XCRD_BU,YCRD_BU
 REAL(KIND=DP_KIND) :: MOUSEX,MOUSEY
 INTEGER,INTENT(IN) :: IDCURSOR
 INTEGER,INTENT(OUT) :: NCRD
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: I,ITYPE,MAXCRD
 LOGICAL :: LEX

 CALL IGRPLOTMODE(MODEXOR); CALL IGRCOLOURN(WRGB(255,255,255))
 CALL WCURSORSHAPE(IDCURSOR) !ID_CURSORDISTANCE)

 CALL IGRFILLPATTERN(OUTLINE); CALL IGRLINETYPE(SOLIDLINE)
 CALL WINDOWOUTSTATUSBAR(2,'Press right mouse button to stop')

 MAXCRD=50; ALLOCATE(XCRD(MAXCRD),YCRD(MAXCRD))
 LEX =.FALSE.; NCRD=0

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)

  MOUSEX=DBLE(MESSAGE%GX)+OFFSETX
  MOUSEY=DBLE(MESSAGE%GY)+OFFSETY

  SELECT CASE (ITYPE)

   !## mouse-move
   CASE (MOUSEMOVE)

    CALL WINDOWSELECT(0)
    CALL WINDOWOUTSTATUSBAR(1,'X:'//TRIM(RTOS(MOUSEX,'F',3))//' m, Y:'//TRIM(RTOS(MOUSEY,'F',3))//' m')

    !## first point set
    IF(NCRD.GE.2)THEN
     CALL UTL_PLOT1BITMAP()
     IF(LEX)CALL UTL_MEASUREPLOTSHAPE(XCRD,YCRD,NCRD)
     LEX=.TRUE.; XCRD(NCRD)=MOUSEX; YCRD(NCRD)=MOUSEY
     CALL UTL_MEASUREPLOTSHAPE(XCRD,YCRD,NCRD)
     CALL UTL_PLOT2BITMAP()
    ENDIF

   CASE (MOUSEBUTDOWN)
    CALL UTL_PLOT1BITMAP()
    IF(LEX)CALL UTL_MEASUREPLOTSHAPE(XCRD,YCRD,NCRD)

    SELECT CASE (MESSAGE%VALUE1)
     !## left button
     CASE (1)
      !## increase memory
      IF(NCRD+2.GT.MAXCRD)THEN
       MAXCRD=MAXCRD+50; ALLOCATE(XCRD_BU(MAXCRD),YCRD_BU(MAXCRD))
       DO I=1,NCRD; XCRD_BU(I)=XCRD(I); YCRD_BU(I)=YCRD(I); ENDDO
       DEALLOCATE(XCRD,YCRD); XCRD=>XCRD_BU; YCRD=>YCRD_BU
      ENDIF
      NCRD=NCRD+1; IF(NCRD.EQ.1)NCRD=NCRD+1
      DO I=NCRD-1,NCRD; XCRD(I)=MOUSEX; YCRD(I)=MOUSEY; ENDDO
      CALL UTL_MEASUREPLOTSHAPE(XCRD,YCRD,NCRD)
      CALL UTL_PLOT2BITMAP()

     !## right button
     CASE (3)
      EXIT
    END SELECT

   !## bitmap scrolled, renew top-left pixel coordinates
   CASE (BITMAPSCROLLED)
    MPW%IX=MESSAGE%VALUE1; MPW%IY=MESSAGE%VALUE2

!   CASE (EXPOSE)
!    IF(WMENUGETSTATE(ID_PLOTLEGEND,2).EQ.1)CALL LEGPLOT_PLOTUPDATE(.FALSE.)
!    CALL DBL_IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX)

  END SELECT
 END DO

 NCRD=NCRD-1

 CALL UTL_PLOT1BITMAP()
 CALL UTL_MEASUREPLOTSHAPE(XCRD,YCRD,NCRD)
 CALL UTL_MEASUREPLOTSHAPE(XCRD,YCRD,NCRD)
 CALL UTL_PLOT2BITMAP()

 CALL WCURSORSHAPE(CURARROW); CALL IGRPLOTMODE(MODECOPY)
 CALL IGRFILLPATTERN(OUTLINE); CALL IGRLINETYPE(SOLIDLINE)
 CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(2,''); CALL WINDOWOUTSTATUSBAR(3,''); CALL WINDOWOUTSTATUSBAR(4,'')

 END SUBROUTINE UTL_MEASURE

 !###======================================================================
 SUBROUTINE UTL_MEASUREPLOTSHAPE(XCRD,YCRD,NCRD)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NCRD
 REAL(KIND=DP_KIND),DIMENSION(:),POINTER,INTENT(IN) :: XCRD,YCRD
 INTEGER :: I
 REAL(KIND=DP_KIND) :: TDIST,DIST
 CHARACTER(LEN=256) :: STRING
 CHARACTER(LEN=256) :: CDIST,CTDIST

 CALL IGRFILLPATTERN(OUTLINE)
 CALL DBL_IGRPOLYLINE(XCRD,YCRD,NCRD,IOFFSET=1)
 TDIST=0.0D0; DIST =0.0D0
 DO I=2,NCRD
  DIST =SQRT((XCRD(I)-XCRD(I-1))**2.0D0+(YCRD(I)-YCRD(I-1))**2.0D0)
  TDIST=DIST+TDIST
 END DO

 IF(TDIST.LT.1.0D0)THEN
  CTDIST=TRIM(RTOS(TDIST*100.0D0 ,'F',3))//' cm'
 ELSEIF(TDIST.LT.1000.0D0)THEN
  CTDIST=TRIM(RTOS(TDIST       ,'F',3))//' m'
 ELSE
  CTDIST=TRIM(RTOS(TDIST/1000.0D0,'F',3))//' km'
 ENDIF
 IF(DIST.LT.1.0D0)THEN
  CDIST=TRIM(RTOS(DIST*100.0D0 ,'F',3))//' cm'
 ELSEIF(DIST.LT.1000.0D0)THEN
  CDIST=TRIM(RTOS(DIST       ,'F',3))//' m'
 ELSE
  CDIST=TRIM(RTOS(DIST/1000.0D0,'F',3))//' km'
 ENDIF

 STRING='Total distance= '//TRIM(CTDIST)//'; Distance last segment= '//TRIM(CDIST)

 CALL WINDOWOUTSTATUSBAR(4,STRING)

 END SUBROUTINE UTL_MEASUREPLOTSHAPE

 !###======================================================================
 SUBROUTINE UTL_GETAXESCALES(XMIN,YMIN,XMAX,YMAX)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
 REAL(KIND=DP_KIND) :: DX,DY,X1,X2,Y1,Y2
 INTEGER :: I

 !## initially try 20 ticks
 NSX=15; DX=UTL_GETNICEAXES(XMAX-XMIN,DBLE(NSX))
 NSY=15; DY=UTL_GETNICEAXES(YMAX-YMIN,DBLE(NSY))

 !## set first tick-marK
 X1=  DX*CEILING(XMIN/DX);  Y1=  DY*CEILING(YMIN/DY)
 X2=  DX*FLOOR(XMAX/DX);    Y2=  DY*FLOOR(YMAX/DY)
 NSX=(X2-X1)/DX; NSX=NSX+1; NSY=(Y2-Y1)/DY; NSY=NSY+1
 SXVALUE(1)=X1; DO I=2,NSX; SXVALUE(I)=SXVALUE(I-1)+DX; ENDDO
 SYVALUE(1)=Y1; DO I=2,NSY; SYVALUE(I)=SYVALUE(I-1)+DY; ENDDO

 END SUBROUTINE UTL_GETAXESCALES

 !###======================================================================
 REAL(KIND=DP_KIND) FUNCTION UTL_GETNICEAXES(R,NTIC)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: R,NTIC
 REAL(KIND=DP_KIND) :: S,M,TIC,RES

 S=R/NTIC
 M=10.0D0**FLOOR(LOG10(S))
 RES=S/M
 IF(RES.GT.5.0D0)THEN
  TIC=10.0D0*M
 ELSEIF(RES.GT.2.0D0)THEN
  TIC=5.0D0*M
 ELSEIF(RES.GT.1.0D0)THEN
  TIC=2.0D0*M
 ELSE
  TIC=M
 ENDIF
 UTL_GETNICEAXES=TIC

 END FUNCTION UTL_GETNICEAXES

 !###======================================================================
 CHARACTER(LEN=24) FUNCTION UTL_WRITENUMBER(X)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X

 WRITE(UTL_WRITENUMBER,UTL_GETFORMAT(X)) X

 END FUNCTION UTL_WRITENUMBER

 !###======================================================================
 CHARACTER(LEN=32) FUNCTION UTL_GETFORMAT(X)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X
 CHARACTER(LEN=32) :: XC
 INTEGER :: I,J,K,II,NDEC,NNUM,DIGITS
 CHARACTER(LEN=1) :: F

 !## number equal to a NaN
 IF(X.NE.X.OR.X.GT.HUGE(1.0D0).OR.X.LT.-HUGE(1.0D0))THEN
  UTL_GETFORMAT='*'
  RETURN
 ENDIF

 WRITE(XC,'(G18.11)') X
 XC=ADJUSTL(XC)

 XC=UTL_CAP(XC,'U')

 !## determine length of value
 I=LEN_TRIM(XC); K=MAX(INDEX(XC,'D'),INDEX(XC,'E')); IF(K.GT.0)I=K-1

 !## get numbers before digit
 J=INDEX(XC,'.')
 IF(J.GT.0)THEN
  !## get first non-zero character
  DO II=I,J+1,-1
   IF(XC(II:II).NE.'0')EXIT
  ENDDO
  NDEC=MAX(0,II-J)
  IF(K.GT.0)NDEC=MAX(NDEC,1)
  NNUM=J-1+NDEC+1
 ELSE
  NDEC=0
  NNUM=J-1
 ENDIF

 F='F'
 IF(K.GT.0)THEN
  READ(XC(K+2:),*) DIGITS
  IF(DIGITS.NE.0)THEN
   F='E'
   NNUM=NNUM+4
  ENDIF
 ENDIF

 IF(X.LT.0.0D0)NNUM=NNUM+1

 WRITE(UTL_GETFORMAT,'(2A1,2(I2.2,A1))') '(',F,NNUM,'.',NDEC,')'

 END FUNCTION UTL_GETFORMAT

 !###======================================================================
 SUBROUTINE UTL_DEBUGLEVEL(IONOFF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IONOFF

 IF(IONOFF.EQ.0)THEN
  CALL IDEBUGLEVEL(DBGSILENT)
 ELSE
  CALL IDEBUGLEVEL(ICDEBUGLEVEL)
 ENDIF

 END SUBROUTINE UTL_DEBUGLEVEL

 !###====================================================================
 FUNCTION UTL_REALTOSTRING(X)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=20) :: UTL_REALTOSTRING,FSTRING
 REAL(KIND=DP_KIND),INTENT(IN) :: X
 INTEGER :: I,J,NSIG
 REAL(KIND=DP_KIND) :: F
 CHARACTER(LEN=12) :: FRM

 I=INT(X); F=X-I
 UTL_REALTOSTRING=TRIM(ITOS(I))
 IF(F.NE.0.0D0)THEN
  NSIG=MAX(0,7-LEN_TRIM(UTL_REALTOSTRING))
  WRITE(FRM,'(A5,I3.3,A1)') '(F15.',NSIG,')'
  WRITE(FSTRING,FRM) F; FSTRING=ADJUSTL(FSTRING)
  !## search backwards for first non-zero
  DO J=LEN_TRIM(FSTRING),1,-1
   IF(FSTRING(J:J).NE.'0')EXIT
  ENDDO
  IF(F.GT.0.0D0)UTL_REALTOSTRING=TRIM(UTL_REALTOSTRING)//'.'//FSTRING(3:J)
  IF(F.LT.0.0D0)UTL_REALTOSTRING=TRIM(UTL_REALTOSTRING)//'.'//FSTRING(4:J)
 ENDIF
 !## add minus
 IF(I.EQ.0.AND.X.LT.0.0D0)UTL_REALTOSTRING='-'//TRIM(UTL_REALTOSTRING)

 END FUNCTION UTL_REALTOSTRING

 !###====================================================================
 SUBROUTINE UTL_RELPATHNAME(PATH,RFNAME,GFNAME)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: PATH
 CHARACTER(LEN=*),INTENT(INOUT) :: RFNAME
 CHARACTER(LEN=*),INTENT(OUT) :: GFNAME
 CHARACTER(LEN=256) :: ROOTNAME

 !## check relative-pathnames
 IF(INDEX(RFNAME,':').EQ.0)THEN
  !## if file is given
  IF(INDEX(PATH,'.').GT.0)THEN
   ROOTNAME=PATH(:INDEX(PATH,'\',.TRUE.)-1)
  ELSE
   ROOTNAME=PATH
  ENDIF
  !## clip number of "..\" from the rootname
  DO
   IF(INDEX(RFNAME,'..\',.FALSE.).EQ.0)THEN
    IF(INDEX(RFNAME,'.\',.FALSE.).EQ.0)EXIT
    !## one point means same folder
    RFNAME=RFNAME(INDEX(RFNAME,'.\',.FALSE.)+2:); EXIT
   ELSE
    RFNAME=RFNAME(INDEX(RFNAME,'..\',.FALSE.)+3:)
   ENDIF
   ROOTNAME=ROOTNAME(:INDEX(ROOTNAME,'\',.TRUE.)-1)
  ENDDO
  !## construct global filename
  GFNAME=TRIM(ROOTNAME)//'\'//TRIM(RFNAME)
 ELSE
  !## drive letter found
  GFNAME=RFNAME
 ENDIF

 !## remove double "\\" if exist
 DO
  IF(INDEX(GFNAME,'\\').EQ.0)EXIT
  GFNAME=UTL_SUBST(GFNAME,'\\','\')
 ENDDO

 END SUBROUTINE UTL_RELPATHNAME

!###======================================================================
 SUBROUTINE UTL_REL_TO_ABS(ROOT,FNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*), INTENT(INOUT) :: ROOT
 CHARACTER(LEN=*), INTENT(INOUT) :: FNAME
 INTEGER :: M,N,IL
 CHARACTER(LEN=1) :: SLASH
 LOGICAL :: LREL

 N = LEN_TRIM(FNAME)
 IF (N==0) RETURN
 CALL UTL_GETSLASH(SLASH)
 FNAME = ADJUSTL(FNAME)
 N = LEN_TRIM(FNAME)

 M = LEN_TRIM(ROOT)
 IF(ROOT(M:M).EQ.SLASH) THEN
    ROOT = ROOT(1:M-1)
    M = M - 1
 END IF
 IL = M + 1

 LREL = .FALSE.
 DO WHILE(.TRUE.)
    IF(FNAME(1:1).NE.'.')EXIT
    IF(FNAME(1:2).EQ.'.'//SLASH)THEN
       LREL = .TRUE.
       FNAME = FNAME(3:N)
       N = LEN_TRIM(FNAME)
    END IF
    IF(FNAME(1:3).EQ.'..'//SLASH)THEN
       LREL = .TRUE.
       FNAME = FNAME(4:N)
       N = LEN_TRIM(FNAME)
       IL = INDEX(ROOT(1:IL-1),SLASH,BACK=.TRUE.)
    END IF
 END DO
 IF(LREL) THEN
    FNAME = ROOT(1:IL-1)//SLASH//TRIM(FNAME)
 END IF

 END SUBROUTINE UTL_REL_TO_ABS

 !###===================================================================
 SUBROUTINE UTL_GETSLASH(SLASH)
 !###===================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(OUT) :: SLASH

 IF(OS.EQ.1)THEN ! DOS
   SLASH='\'
 ELSEIF(OS.EQ.2)THEN ! UNIX
   SLASH='/'
 ENDIF

 END SUBROUTINE UTL_GETSLASH

 !###====================================================================
 FUNCTION UTL_IMODVERSION(S1,S2)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: S1,S2
 CHARACTER(LEN=156) :: UTL_IMODVERSION

 IF(LBETA)THEN
  UTL_IMODVERSION=TRIM(BVERSION)//'-iMOD'
 ELSE
  UTL_IMODVERSION='iMOD'
 ENDIF

 IF(PRESENT(S1).AND.PRESENT(S2))THEN
  UTL_IMODVERSION=TRIM(UTL_IMODVERSION)//' ['//TRIM(UTL_SUBST(RVERSION_EXE,S1,S2))//' '//TRIM(CCONFIG)//']'
 ELSE
  UTL_IMODVERSION=TRIM(UTL_IMODVERSION)//' ['//TRIM(RVERSION_EXE)//' '//TRIM(CCONFIG)//']'
 ENDIF
 IF(LEXPDATE)THEN
  UTL_IMODVERSION=TRIM(UTL_IMODVERSION)//' !!! Expiring date: '//TRIM(JDATETOGDATE(UTL_IDATETOJDATE(EXPDATE)))//' !!!'
 ENDIF

 END FUNCTION UTL_IMODVERSION

 !###====================================================================
 LOGICAL FUNCTION UTL_PCK_READTXT(ICOL,STIME,ETIME,QT,FNAME,INDICATOR,THRESHOLD,ISS,NCOUNT)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ICOL,INDICATOR,ISS
 INTEGER(KIND=8),INTENT(IN) :: STIME,ETIME
 CHARACTER(LEN=*),INTENT(IN) :: FNAME,THRESHOLD
 REAL(KIND=DP_KIND),INTENT(OUT) :: QT
 REAL(KIND=DP_KIND),INTENT(OUT) :: NCOUNT
 INTEGER :: IR,I,IU,NR,NC,IOS,ITYPE,IZMAX !,SDATE
 REAL(KIND=DP_KIND) :: Q1,QQ,Z,TZ,BZ,DZ,F,Z1,NQ,RTIME,TTIME
 INTEGER(KIND=8) :: DBL_SDATE,DBL_EDATE
 CHARACTER(LEN=8) :: ATTRIB
 CHARACTER(LEN=256) :: LINE
 REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE :: NODATA
 CHARACTER(LEN=52),DIMENSION(:),ALLOCATABLE :: QD

 UTL_PCK_READTXT=.FALSE.

 !## transient(2)/steady-state(1)
 QT=0.0D0

 !## open textfiles with pump information
 IU=UTL_GETUNIT()
 OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ',FORM='FORMATTED',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot find the associated txt file:'//CHAR(13)// &
    TRIM(FNAME),'Error')
  RETURN
 ENDIF

 READ(IU,*) NR; IF(NR.LE.0)THEN; CLOSE(IU); UTL_PCK_READTXT=.TRUE.; RETURN; ENDIF

 READ(IU,'(A256)') LINE
 READ(LINE,*,IOSTAT=IOS) NC,ITYPE
 IF(IOS.NE.0)ITYPE=1
 ITYPE=MAX(ITYPE,1)

 !## what type of file?
 SELECT CASE (ITYPE)
  !## timeseries
  CASE (1)
   !## duration in days - transient
   IF(ISS.EQ.2)THEN
    TTIME=DIFFTIME(STIME,ETIME)
    IF(TTIME.LE.0.0D0)THEN
     CLOSE(IU); CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Timestep size to extract data is '//TRIM(RTOS(TTIME,'E',7)),'Error')
     RETURN
    ENDIF
   ENDIF
  !## boreholes/seismic
  CASE (2,3)

 END SELECT

 ALLOCATE(NODATA(NC),QD(NC)); QD=''

 DO I=1,NC; READ(IU,*) ATTRIB,NODATA(I); ENDDO

 !## timeseries
 IF(ITYPE.EQ.1)THEN

  DBL_SDATE=STIME
  QQ=NODATA(ICOL)
  NCOUNT=0.0D0

  DO IR=1,NR
   READ(IU,*) DBL_EDATE,(QD(I),I=2,NC)
   !## steady-state
   IF(ISS.EQ.1)THEN
    !## get volume
    READ(QD(2),*) QQ
    IF(QQ.NE.NODATA(2))THEN
     NCOUNT=NCOUNT+1
     QT=QT+QQ
    ENDIF
   ELSE
    !## make double if needed
    IF(DBL_EDATE.LT.100000000)DBL_EDATE=DBL_EDATE*1000000
    IF(DBL_EDATE.GT.STIME)THEN
     DBL_SDATE=MAX(DBL_SDATE,STIME)
     DBL_EDATE=MIN(DBL_EDATE,ETIME)
     RTIME=DIFFTIME(DBL_SDATE,DBL_EDATE)
     !## add only if q ne nodata
     IF(QQ.NE.NODATA(ICOL))THEN
      QT=QT+RTIME*QQ
      NCOUNT=NCOUNT+RTIME
     ENDIF
    ENDIF
    IF(DBL_EDATE.LE.ETIME)THEN
     !## get volume
     READ(QD(ICOL),*) QQ
    ENDIF
    DBL_SDATE=DBL_EDATE
    !## stop
    IF(DBL_EDATE.GE.ETIME)EXIT
   ENDIF
  ENDDO

  !## last record probably read, extent extraction up to end of stress-period
  IF(QQ.NE.NODATA(ICOL).AND.IR.GT.NR)THEN
   RTIME=DIFFTIME(DBL_SDATE,ETIME)
   RTIME=MIN(TTIME,RTIME)
   QT=QT+RTIME*QQ
   NCOUNT=NCOUNT+RTIME
  ENDIF

  !## steady-state
  IF(NCOUNT.GT.0.0D0)QT=QT/NCOUNT

  UTL_PCK_READTXT=.TRUE.

 !## itype=2 borehole; itype=3 seismic
 ELSEIF(ITYPE.EQ.2.OR.ITYPE.EQ.3)THEN

  QQ=0.0D0

  !## get elevation in chronologic order
  IF(ICOL.EQ.1)THEN
   IZMAX=STIME !SDATE
   DO IR=1,MIN(IZMAX,NR)
    READ(IU,*) Z
   ENDDO
   QT=Z
   IF(Z.NE.NODATA(1).AND.IR.EQ.IZMAX+1)THEN
    NCOUNT=1.0D0; UTL_PCK_READTXT=.TRUE.
   ENDIF
   
  !## get the average value for the choosen interval
  ELSE

   NQ=0.0D0; TZ=STIME/100.0D0; BZ=ETIME/100.0D0; DZ=TZ-BZ
   DO IR=1,NR

    READ(IU,*) Z,(QD(I),I=2,NC)

    !## get first
    IF(IR.GT.1)THEN
     !## skip if equal to nodata
     IF(Q1.NE.NODATA(ICOL))THEN
      !## get fraction
      IF(Z1.GE.BZ.AND.Z.LT.TZ)THEN
       F=(MIN(TZ,Z1)-MAX(BZ,Z))/DZ
       QT=QT+F*Q1
       NQ=NQ+F
      ENDIF
     ENDIF
    ENDIF

    Z1=Z
    !## apply indicator
    IF(INDICATOR.GT.0)THEN
     Q1=0.0D0; IF(TRIM(UTL_CAP(QD(ICOL),'U')).EQ.TRIM(UTL_CAP(THRESHOLD,'U')))Q1=1.0D0
    ELSE
     READ(QD(ICOL),*,IOSTAT=IOS) Q1
     IF(IOS.NE.0)THEN
      WRITE(*,'(/A/)') 'iMOD cannot read '//QD(ICOL)//' into a number'; STOP
     ENDIF
    ENDIF

    IF(Z.LT.BZ)EXIT

   ENDDO

   IF(NQ.GT.0.0D0)THEN
    NCOUNT=NQ
    QT=QT/NQ
    UTL_PCK_READTXT=.TRUE.
   ENDIF

  ENDIF
 ENDIF

 CLOSE(IU); DEALLOCATE(QD)

 END FUNCTION UTL_PCK_READTXT

 !###======================================================================
 SUBROUTINE UTL_PCK_GETTLP(N,TLP,KH,TOP,BOT,Z1,Z2,MINKHT)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),PARAMETER :: MAXPNT=0.0D0
 INTEGER,INTENT(IN) :: N
 REAL(KIND=DP_KIND),INTENT(IN) :: MINKHT
 REAL(KIND=DP_KIND),INTENT(INOUT) :: Z1,Z2
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(N) :: KH,TOP,BOT
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(N) :: TLP
 INTEGER :: ILAY
 REAL(KIND=DP_KIND) :: ZM,ZT,ZB,ZC,FC,DZ
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: L,TL
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IL

 ALLOCATE(L(N),TL(N),IL(N))

 !## not thickness between z1 and z2 - look for correct modellayer
 IF(Z1.EQ.Z2)THEN

  TLP=0.0D0; ZM=Z1
  DO ILAY=1,N
   IF(ZM.GE.BOT(ILAY).AND.ZM.LE.TOP(ILAY))THEN
    TLP(ILAY)=1.0D0; EXIT
   ENDIF
  ENDDO

 ELSE

  !## filterlength for each modellayer
  L=0.0D0
  DO ILAY=1,N
   ZT=MIN(TOP(ILAY),Z1); ZB=MAX(BOT(ILAY),Z2); L(ILAY)=MAX(0.0D0,ZT-ZB)
  ENDDO

  TLP=0.0D0
  !## well within any aquifer(s)
  IF(SUM(L).GT.0.0D0)THEN
   !## compute percentage and include sumkd, only if itype.eq.2
   L=L*KH
   !## percentage (0-1) L*KH
   DO ILAY=1,N; IF(L(ILAY).NE.0.0D0)TLP=(1.0D0/SUM(L))*L; ENDDO
  ENDIF

  !## correct for dismatch with centre of modelcell
  DO ILAY=1,N
   IF(TLP(ILAY).GT.0.0D0)THEN
    DZ= TOP(ILAY)-BOT(ILAY)
    ZC=(TOP(ILAY)+BOT(ILAY))/2.0D0
    ZT= MIN(TOP(ILAY),Z1)
    ZB= MAX(BOT(ILAY),Z2)
    FC=(ZT+ZB)/2.0D0
    TLP(ILAY)=TLP(ILAY)*(1.0D0-(ABS(ZC-FC)/(0.5D0*DZ)))
   ENDIF
  ENDDO

  !## normalize tlp() again
  IF(SUM(TLP).GT.0.0D0)TLP=(1.0D0/SUM(TLP))*TLP

  !## remove small transmissivities
  IF(MINKHT.GT.0.0D0)THEN
   ZT=SUM(TLP)
   DO ILAY=1,N
    DZ= TOP(ILAY)-BOT(ILAY)
    IF(KH(ILAY)*DZ.LT.MINKHT)TLP(ILAY)=0.0D0
   ENDDO
   IF(SUM(TLP).GT.0.0D0)THEN
    ZT=ZT/SUM(TLP); TLP=ZT*TLP
   ENDIF

   !## normalize tlp() again
   IF(SUM(TLP).GT.0.0D0)TLP=(1.0D0/SUM(TLP))*TLP

  ENDIF

 ENDIF

 !## nothing in model, whenever system on top of model, put them in first modellayer with thickness
 IF(SUM(TLP).EQ.0.0D0)THEN
  IF(Z1.GE.TOP(1))TLP(1)=1.0D0
 ENDIF

 !## if no layers has been used for the assignment, try to allocate it to aquifer of this interbed
 IF(SUM(TLP).LE.0)THEN
  TLP=0
  DO ILAY=1,N-1
   IF(BOT(ILAY).GE.Z1.AND.TOP(ILAY+1).LE.Z2)THEN; TLP(ILAY)=1.0D0; EXIT; ENDIF
  ENDDO
 ENDIF

 DEALLOCATE(L,TL,IL)

 END SUBROUTINE UTL_PCK_GETTLP

 !###======================================================================
 SUBROUTINE UTL_LISTOFFILES(FNAME_IN,STRING,BACTION,TEXT,HELP)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: BACTION
 INTEGER,PARAMETER :: STRLEN=256
 INTEGER :: ITYPE
 TYPE(WIN_MESSAGE) :: MESSAGE
 CHARACTER(LEN=STRLEN),POINTER,DIMENSION(:),INTENT(INOUT) :: FNAME_IN
 CHARACTER(LEN=*),INTENT(IN),DIMENSION(6) :: STRING
 CHARACTER(LEN=STRLEN),POINTER,DIMENSION(:) :: FNAME=>NULL()
 CHARACTER(LEN=STRLEN) :: EFNAME
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: TEXT
 CHARACTER(LEN=256),INTENT(IN),OPTIONAL :: HELP
 INTEGER,DIMENSION(:),ALLOCATABLE :: LRLIST,ILIST,JLIST
 INTEGER :: N,DID,I,J,NL,NR

 DID=WINFODIALOG(CURRENTDIALOG)

 !## store copy of filenames
 IF(ASSOCIATED(FNAME_IN))THEN
  ALLOCATE(FNAME(SIZE(FNAME_IN)))
  IF(STRING(1).EQ.'IMODMANAGER')THEN
   DO I=1,SIZE(FNAME); FNAME(I)=FNAME_IN(I)(2:); ENDDO
  ELSE
   DO I=1,SIZE(FNAME); FNAME(I)=FNAME_IN(I); ENDDO
  ENDIF
 ENDIF

 !## define "String" for changing names on push buttons and window title if "String" is available.
 IF(STRING(1).EQ.'IMODMANAGER')THEN
  CALL WDIALOGLOAD(ID_DLISTOFFILES2,ID_DLISTOFFILES2)
  CALL WDIALOGPUTIMAGE(ID_RIGHT,ID_ICONRIGHT,1)
  CALL WDIALOGPUTIMAGE(ID_LEFT,ID_ICONLEFT,1)
  NR=0; NL=SIZE(FNAME_IN); ALLOCATE(LRLIST(NL),ILIST(NL),JLIST(NL))
  !## all on left menu
  DO I=1,SIZE(FNAME)
   IF(FNAME_IN(I)(1:1).EQ.'-')LRLIST(I)= I
   IF(FNAME_IN(I)(1:1).EQ.'+')LRLIST(I)=-I
  ENDDO
 ELSE
  CALL WDIALOGLOAD(ID_DLISTOFFILES1,ID_DLISTOFFILES1)
  CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPEN,1)
  CALL WDIALOGPUTIMAGE(ID_DELETE,ID_ICONDELETE,1)
 ENDIF

 CALL WDIALOGTITLE('Extra files')
 IF(LEN_TRIM(STRING(2)).NE.0)CALL WDIALOGTITLE(TRIM(STRING(2)))                !## changes title of dialog window
 IF(LEN_TRIM(STRING(3)).NE.0)CALL WDIALOGPUTSTRING(IDCANCEL,TRIM(STRING(3)))   !## changes text on close-button
 IF(LEN_TRIM(STRING(4)).NE.0)CALL WDIALOGPUTSTRING(IDHELP,TRIM(STRING(4)))     !## changes text on help-button
 IF(LEN_TRIM(STRING(5)).NE.0)CALL WDIALOGPUTSTRING(IDOK,TRIM(STRING(5)))       !## changes text on apply-button
 IF(LEN_TRIM(STRING(6)).NE.0)CALL WDIALOGPUTSTRING(IDF_STRING1,TRIM(STRING(6))//': '//TRIM(TEXT))!## changes text on text field

 IF(.NOT.PRESENT(HELP))CALL WDIALOGFIELDSTATE(IDHELP,3)

 IF(STRING(1).EQ.'IMODMANAGER')THEN
  CALL UTL_LISTOFFILES_FILLMENU(LRLIST,FNAME,FNAME_IN,ILIST,JLIST)
  CALL WDIALOGFIELDSTATE(ID_RIGHT,0)
  CALL WDIALOGFIELDSTATE(ID_LEFT,0)
 ELSE
  CALL UTL_LISTOFFILES_MANIPULATE(FNAME,STRLEN,0,EFNAME)
 ENDIF
 CALL WDIALOGSHOW(-1,-1,0,3)

 BACTION=0
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE(FIELDCHANGED)
    !## previous field
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDF_MENU1)
    END SELECT
    !## next field
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_MENU1)
      IF(STRING(1).EQ.'IMODMANAGER')THEN
       CALL WDIALOGGETMENU(IDF_MENU1,ILIST)
       CALL WDIALOGFIELDSTATE(ID_RIGHT,MIN(1,SUM(ILIST)))
      ENDIF
     CASE (IDF_MENU2)
      IF(STRING(1).EQ.'IMODMANAGER')THEN
       CALL WDIALOGGETMENU(IDF_MENU2,JLIST)
       CALL WDIALOGFIELDSTATE(ID_LEFT,MIN(1,SUM(JLIST)))
      ENDIF
    END SELECT

   CASE(PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (ID_OPEN)
      IF(UTL_WSELECTFILE('Files ('//TRIM(STRING(1))//')|'//TRIM(STRING(1))//'|',LOADDIALOG+MUSTEXIST+PROMPTON+ &
         DIRCHANGE+APPENDEXT+MULTIFILE,EFNAME,'Load Files ('//TRIM(STRING(1))//')'))CALL UTL_LISTOFFILES_MANIPULATE(FNAME,STRLEN,1,EFNAME)
     CASE (ID_DELETE)
      CALL WDIALOGGETMENU(IDF_MENU1,N,EFNAME)
      CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to remove the file:'//CHAR(13)// &
       TRIM(EFNAME),'Question'); IF(WINFODIALOG(4).EQ.1)CALL UTL_LISTOFFILES_MANIPULATE(FNAME,STRLEN,-1,EFNAME)

     CASE (ID_RIGHT,ID_LEFT)

      IF(MESSAGE%VALUE1.EQ.ID_RIGHT)THEN
       !## get selected files in left menu field
       CALL WDIALOGGETMENU(IDF_MENU1,ILIST)
       J=0; DO I=1,SIZE(LRLIST)
        IF(LRLIST(I).GT.0)THEN
         J=J+1; IF(ILIST(J).EQ.1)LRLIST(I)=-1*LRLIST(I)
        ENDIF
       ENDDO
      ELSE
       !## get selected files in right menu field
       CALL WDIALOGGETMENU(IDF_MENU2,JLIST)
       J=0; DO I=1,SIZE(LRLIST)
        IF(LRLIST(I).LT.0)THEN
         J=J+1; IF(JLIST(J).EQ.1)LRLIST(I)=-1*LRLIST(I)
        ENDIF
       ENDDO
      ENDIF

      CALL UTL_LISTOFFILES_FILLMENU(LRLIST,FNAME,FNAME_IN,ILIST,JLIST)

      CALL WDIALOGFIELDSTATE(ID_RIGHT,0); CALL WDIALOGFIELDSTATE(ID_LEFT,0)

     CASE (IDOK)
      BACTION=1
      !## copy adjusted filename
      IF(STRING(1).EQ.'IMODMANAGER')THEN
       NR=0; DO I=1,SIZE(LRLIST)
        IF(LRLIST(I).LT.0)THEN; NR=NR+1; FNAME(NR)=FNAME_IN(I); ENDIF
       ENDDO
       IF(ASSOCIATED(FNAME_IN))DEALLOCATE(FNAME_IN)
       ALLOCATE(FNAME_IN(NR)); DO I=1,NR; FNAME_IN(I)=FNAME(I); ENDDO
      ELSE
       IF(ASSOCIATED(FNAME))THEN
        ALLOCATE(FNAME_IN(SIZE(FNAME))); DO I=1,SIZE(FNAME); FNAME_IN(I)=FNAME(I); ENDDO
       ENDIF
      ENDIF
      EXIT
     CASE (IDHELP)
      CALL UTL_LISTOFFILES_GETHELP(HELP)
     CASE (IDCANCEL)
      EXIT
    END SELECT
  END SELECT

 ENDDO

 IF(ASSOCIATED(FNAME))DEALLOCATE(FNAME)
 IF(ALLOCATED(LRLIST))DEALLOCATE(LRLIST)
 IF(ALLOCATED(ILIST))DEALLOCATE(ILIST); IF(ALLOCATED(JLIST))DEALLOCATE(JLIST)

 CALL WDIALOGUNLOAD(); IF(DID.NE.0)CALL WDIALOGSELECT(DID)

 END SUBROUTINE UTL_LISTOFFILES

 !###======================================================================
 SUBROUTINE UTL_LISTOFFILES_FILLMENU(LRLIST,FNAME,FNAME_IN,ILIST,JLIST)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: FNAME_IN
 CHARACTER(LEN=*),DIMENSION(:),INTENT(OUT) :: FNAME
 INTEGER,DIMENSION(:),INTENT(INOUT) :: LRLIST,ILIST,JLIST
 INTEGER :: NR,NL,I

 !## fill left menu
 NL=0; DO I=1,SIZE(LRLIST); IF(LRLIST(I).GT.0)THEN; NL=NL+1; FNAME(NL)=FNAME_IN(I)(INDEX(FNAME_IN(I),'\',.TRUE.)+1:); ENDIF; ENDDO
 IF(NL.GT.0)THEN
  ILIST=0; CALL WDIALOGPUTMENU(IDF_MENU1,FNAME,NL,ILIST)
  CALL WDIALOGFIELDSTATE(IDF_MENU1,1)
 ELSE
  CALL WDIALOGCLEARFIELD(IDF_MENU1); CALL WDIALOGFIELDSTATE(IDF_MENU1,0)
 ENDIF

 !## fill right menu
 NR=0; DO I=1,SIZE(LRLIST); IF(LRLIST(I).LT.0)THEN; NR=NR+1; FNAME(NR)=FNAME_IN(I)(INDEX(FNAME_IN(I),'\',.TRUE.)+1:); ENDIF; ENDDO
 IF(NR.GT.0)THEN
  JLIST=0; CALL WDIALOGPUTMENU(IDF_MENU2,FNAME,NR,JLIST)
  CALL WDIALOGFIELDSTATE(IDF_MENU2,1)
 ELSE
  CALL WDIALOGCLEARFIELD(IDF_MENU2); CALL WDIALOGFIELDSTATE(IDF_MENU2,0)
 ENDIF

 END SUBROUTINE UTL_LISTOFFILES_FILLMENU

 !###======================================================================
 SUBROUTINE UTL_LISTOFFILES_MANIPULATE(FNAME,STRLEN,IADD,EFNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IADD,STRLEN
 CHARACTER(LEN=STRLEN),INTENT(IN) :: EFNAME
 CHARACTER(LEN=STRLEN),POINTER,DIMENSION(:),INTENT(INOUT) :: FNAME
 CHARACTER(LEN=STRLEN),POINTER,DIMENSION(:) :: FNAME_BU
 INTEGER :: I,J,K,II,ISEL,NFILE
 CHARACTER(LEN=256) :: FLIST
 CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: FNAMES

 NULLIFY(FNAME_BU)

 !## get number of files selected
 K=INDEX(EFNAME,CHAR(0))
 IF(K.GT.0)THEN
  FLIST=EFNAME
  NFILE=0
  I=K+1
  DO WHILE(.TRUE.)
   J=INDEX(FLIST(I:),CHAR(0))
   NFILE=NFILE+1
   IF(J.EQ.0)EXIT
   I=I+J
  END DO
 ELSE
  NFILE=1
 ENDIF

 !## collect filenames
 ALLOCATE(FNAMES(NFILE))
 DO II=1,NFILE
 !## construct new name in multi-file selection mode
  IF(NFILE.GT.1)THEN
   I=INDEX(FLIST,CHAR(0))+1
   DO K=1,II-1
    J=INDEX(FLIST(I:),CHAR(0))
    I=I+J
   END DO
   J=INDEX(FLIST(I:),CHAR(0))
   K=INDEX(FLIST,CHAR(0))-1
   IF(J.EQ.0)THEN
    FNAMES(II)=FLIST(:K)//'\'//FLIST(I:)
   ELSE
    J=J+I
    FNAMES(II)=FLIST(:K)//'\'//FLIST(I:J-1)
   ENDIF
   J=INDEXNOCASE(FNAMES(II),CHAR(0),.TRUE.)
   IF(J.GT.0)FNAMES(II)=FNAMES(II)(:J-1)
  ELSE
   FNAMES(II)=EFNAME
  ENDIF
 ENDDO

 DO II=1,NFILE

  !## add file
  IF(IADD.EQ.1)THEN

   IF(ASSOCIATED(FNAME))THEN
    !## check double files
!    DO I=1,SIZE(FNAME); IF(TRIM(UTL_CAP(FNAME(I),'U')).EQ.TRIM(UTL_CAP(EFNAME,'U')))EXIT; ENDDO
    DO I=1,SIZE(FNAME); IF(TRIM(UTL_CAP(FNAME(I),'U')).EQ.TRIM(UTL_CAP(FNAMES(II),'U')))EXIT; ENDDO
    IF(I.LE.SIZE(FNAME))THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Current file already exists'//CHAR(13)//TRIM(FNAMES(II)),'Error')
!     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Current file already exists'//CHAR(13)//TRIM(EFNAME),'Error')
     RETURN
    ELSE
     ALLOCATE(FNAME_BU(SIZE(FNAME)+1))
     DO I=1,SIZE(FNAME); FNAME_BU(I)=FNAME(I); ENDDO; FNAME_BU(I)=FNAMES(II); ISEL=I
!     DO I=1,SIZE(FNAME); FNAME_BU(I)=FNAME(I); ENDDO; FNAME_BU(I)=EFNAME; ISEL=I
     DEALLOCATE(FNAME)
    ENDIF
   ELSE
    ALLOCATE(FNAME_BU(1)); FNAME_BU(1)=FNAMES(II); ISEL=1
!    ALLOCATE(FNAME_BU(1)); FNAME_BU(1)=EFNAME; ISEL=1
   ENDIF
   FNAME=>FNAME_BU

  !## remove file
  ELSEIF(IADD.EQ.-1)THEN

   IF(SIZE(FNAME)-1.GT.0)THEN
    ALLOCATE(FNAME_BU(SIZE(FNAME)-1))
    J=0; DO I=1,SIZE(FNAME)
     IF(TRIM(UTL_CAP(FNAME(I),'U')).NE.TRIM(UTL_CAP(FNAMES(II),'U')))THEN
!     IF(TRIM(UTL_CAP(FNAME(I),'U')).NE.TRIM(UTL_CAP(EFNAME,'U')))THEN
      J=J+1; FNAME_BU(J)=FNAME(I)
     ELSE
      ISEL=J
     ENDIF
    ENDDO
    DEALLOCATE(FNAME); FNAME=>FNAME_BU
   ELSE
    DEALLOCATE(FNAME)
   ENDIF

  ELSE

   ISEL=1

  ENDIF

 ENDDO

 IF(ASSOCIATED(FNAME))THEN
  CALL WDIALOGPUTMENU(IDF_MENU1,FNAME,SIZE(FNAME),MAX(1,ISEL))
  CALL WDIALOGFIELDSTATE(IDF_MENU1,1)
  CALL WDIALOGFIELDSTATE(ID_DELETE,1)
 ELSE
  CALL WDIALOGPUTMENU(IDF_MENU1,(/'Add files ...'/),1,1)
  CALL WDIALOGFIELDSTATE(IDF_MENU1,2)
  CALL WDIALOGFIELDSTATE(ID_DELETE,2)
 ENDIF

 END SUBROUTINE UTL_LISTOFFILES_MANIPULATE

!###=========================================================================
 SUBROUTINE UTL_LISTOFFILES_GETHELP(HELP)
!###=========================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: HELP
 LOGICAL :: LEX
 INTEGER :: I
 CHARACTER(LEN=256) :: LINE
 CHARACTER(LEN=10) :: EXT

 !## error/warning checking
 IF(TRIM(HELP).EQ.'')THEN
  CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'You should specify the keyword HELP in the *.INI file of the plugin.'// &
              'E.g. HELP=D:\Plugin1\WaterbalanceTool\HELP.PDF','Warning')
  RETURN
 ENDIF
 INQUIRE(FILE=TRIM(HELP),EXIST=LEX)
 IF(.NOT.LEX)THEN
  CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'Cannot find the specified HELP= '//TRIM(HELP),'Warning')
  RETURN
 ENDIF

 !#find file extension
 I=INDEXNOCASE(TRIM(HELP),'.',.TRUE.)
 EXT=HELP(I+1:)

 !## open help file
 IF(UTL_CAP(TRIM(EXT),'U').EQ.'PDF')THEN
  !## acrobat reader
  IF(PREFVAL(13).EQ.'')THEN
   CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'You should specify the keyword ACROBATREADER in the Preference file of iMOD.'// &
              'E.g. ACROBATREADER=c:\Program Files (x86)\Adobe\Reader 11.0D0\Reader\AcroRd32.exe','Warning')
   RETURN
  ENDIF
  INQUIRE(FILE=PREFVAL(13),EXIST=LEX)
  IF(.NOT.LEX)THEN
   CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'Cannot find the specified ACROBATREADER='//TRIM(PREFVAL(13)),'Warning')
   RETURN
  ENDIF
  LINE='"'//TRIM(PREFVAL(13))//' '//TRIM(HELP)//'"'
  CALL IOSCOMMAND(LINE,PROCSILENT)

 ELSEIF(UTL_CAP(TRIM(EXT),'U').EQ.'HTM')THEN
  !## webpage
  CALL WHELPFILE(TRIM(HELP))

 ENDIF

 END SUBROUTINE UTL_LISTOFFILES_GETHELP

 !###======================================================================
 SUBROUTINE UTL_READTXTFILE(FNAME,TEXT)
 !###======================================================================
 !## Subroutine to read text containing multiple lines
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: TEXT
 CHARACTER(LEN=:),ALLOCATABLE :: LINE
 CHARACTER(LEN=*), INTENT(IN) :: FNAME
 INTEGER :: IU,IOS,LENTXT
 LOGICAL :: LEX

 TEXT=''

 INQUIRE(FILE=FNAME,EXIST=LEX)
 IF(.NOT.LEX)THEN; TEXT='No textfile with additional information found.'; RETURN; ENDIF
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ')
 IF(IU.EQ.0)RETURN

 LENTXT = LEN(TEXT)
 ALLOCATE(CHARACTER(LEN=LENTXT) :: LINE)

 DO
   READ(IU,'(A)',IOSTAT=IOS) LINE
   IF(IOS.NE.0)EXIT
   IF(LEN_TRIM(TEXT).EQ.0)THEN
    TEXT=TRIM(LINE)
   ELSE
    TEXT=TRIM(TEXT)//CHAR(13)//CHAR(10)//TRIM(LINE)
   ENDIF
 ENDDO
 CLOSE(IU)

 DEALLOCATE(LINE)

 END SUBROUTINE UTL_READTXTFILE

 !###===================================================================
 SUBROUTINE UTL_MODEL1CHECKFNAME(FNAME,LU)
 !###===================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: LU
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 REAL(KIND=DP_KIND) :: X
 INTEGER :: IOS,I,J
 LOGICAL :: LEX

 IF(LEN_TRIM(FNAME).EQ.0)THEN
  IF(LU.EQ.0)CALL UTL_PRINTTEXT('No file given',2)
  IF(LU.GT.0)THEN
   WRITE(LU,*) 'Error:'
   WRITE(LU,*) '  No file given'
  ENDIF
 ENDIF

 !get first non character
 I=0
 DO
  I=I+1
  J=ICHAR(FNAME(I:I))
  IF(J.GT.32)EXIT
 ENDDO

 X=UTL_GETREAL(FNAME(I:),IOS)
 IF(IOS.NE.0)THEN

  INQUIRE(FILE=FNAME(I:),OPENED=LEX)
  IF(LEX)RETURN

  INQUIRE(FILE=FNAME(I:),EXIST=LEX)
  IF(.NOT.LEX)THEN
   IF(LU.EQ.0)CALL UTL_PRINTTEXT('File '//TRIM(FNAME(I:))//' does not exist !',2)
   IF(LU.GT.0)THEN
    WRITE(LU,*) 'Error:'
    WRITE(LU,*)    TRIM(FNAME(I:))//' does not exist!'
   ENDIF
  ENDIF

 ENDIF

 END SUBROUTINE UTL_MODEL1CHECKFNAME

 !###====================================================================
 SUBROUTINE UTL_APPLYFCT_R(A,NODATA,NROW,NCOL,FCT,IMP)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NROW,NCOL
 REAL(KIND=DP_KIND),INTENT(IN) :: FCT,IMP,NODATA
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(NCOL,NROW) :: A
 INTEGER :: IROW,ICOL

 DO IROW=1,NROW
  DO ICOL=1,NCOL
   IF(A(ICOL,IROW).NE.NODATA)THEN
    A(ICOL,IROW)=A(ICOL,IROW)*FCT
    A(ICOL,IROW)=A(ICOL,IROW)+IMP
   ENDIF
  END DO
 END DO

 END SUBROUTINE UTL_APPLYFCT_R

 !###====================================================================
 SUBROUTINE UTL_APPLYFCT_I(A,NODATA,NROW,NCOL,FCT,IMP)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NROW,NCOL
 REAL(KIND=DP_KIND),INTENT(IN) :: FCT,IMP,NODATA
 INTEGER,INTENT(INOUT),DIMENSION(NCOL,NROW) :: A
 INTEGER :: IROW,ICOL

 DO IROW=1,NROW
  DO ICOL=1,NCOL
   IF(A(ICOL,IROW).NE.NODATA)THEN
    A(ICOL,IROW)=A(ICOL,IROW)*FCT
    A(ICOL,IROW)=A(ICOL,IROW)+IMP
   ENDIF
  END DO
 END DO

 END SUBROUTINE UTL_APPLYFCT_I

 !###===================================================================
 SUBROUTINE UTL_STRING(LINE)
 !###===================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: LINE

 CALL UTL_DELNULCHAR(LINE)
 CALL UTL_DELCONTROLM(LINE)

 END SUBROUTINE UTL_STRING

 !###===================================================================
 SUBROUTINE UTL_FILENAME(LINE)
 !###===================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: LINE

 CALL UTL_SWAPSLASH(LINE)
 LINE=ADJUSTL(LINE)

 END SUBROUTINE UTL_FILENAME

 !###===================================================================
 SUBROUTINE UTL_DELNULCHAR(LINE)
 !###===================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: LINE
 INTEGER :: I

 !## find ^M (null character)
 I=INDEX(LINE,CHAR(0))
 IF(I.EQ.0)RETURN
 !## replace by space
 LINE(I:I)=CHAR(32)

 END SUBROUTINE UTL_DELNULCHAR

 !###===================================================================
 SUBROUTINE UTL_DELCONTROLM(LINE)
 !###===================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: LINE
 INTEGER :: I

 !#find ^M (carriage return)
 I=INDEX(LINE,CHAR(13))
 IF(I.LE.0)RETURN
 !#replace by space
 LINE(I:I)=CHAR(32)

 END SUBROUTINE UTL_DELCONTROLM

 !###===================================================================
 REAL(KIND=DP_KIND) FUNCTION UTL_GETREAL(LINE,IOS)
 !###===================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IOS
 CHARACTER(LEN=*),INTENT(IN) :: LINE

 READ(LINE,*,IOSTAT=IOS) UTL_GETREAL
 IF(IOS.NE.0)UTL_GETREAL=0.0D0

 END FUNCTION UTL_GETREAL

 !###===================================================================
 CHARACTER(LEN=256) FUNCTION UTL_GETFNAME(LINE)
 !###===================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: LINE
 INTEGER :: I,J,K

 K=39
 I=INDEX(LINE,CHAR(K),.FALSE.)  !## '-tje
 IF(I.EQ.0)THEN
  K=34
  I=INDEX(LINE,CHAR(K),.FALSE.) !## "-tje
 ENDIF
 !## quotes found, find other, to be sure it is consistent
 IF(I.GT.0)THEN
  J=INDEX(LINE,CHAR(K),.TRUE.)
  IF(I.EQ.J)THEN
   CALL UTL_PRINTTEXT('',0)
   CALL UTL_PRINTTEXT('Missing second quote '//CHAR(K)//' in line:',0)
   CALL UTL_PRINTTEXT(TRIM(LINE),0)
   CALL UTL_PRINTTEXT('',2)
  ENDIF
  UTL_GETFNAME=LINE(I+1:J-1)
 ELSE
  !## search for comma's, backward
  I=INDEX(TRIM(LINE),',',.TRUE.)
  J=INDEX(TRIM(LINE),' ',.TRUE.)
  UTL_GETFNAME=LINE(MAX(I+1,J+1):)
 ENDIF

 END FUNCTION UTL_GETFNAME

 !###===================================================================
 SUBROUTINE UTL_SWAPSLASH(LINE)
 !###===================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: LINE
 INTEGER :: I,IFR,ITO

 IF(OS.EQ.1)THEN
  IFR=47
  ITO=92
 ELSEIF(OS.EQ.2)THEN
  IFR=92
  ITO=47
 ENDIF

 DO
  I=INDEX(LINE,CHAR(IFR))
  IF(I.EQ.0)EXIT
  LINE(I:I)=CHAR(ITO)
 ENDDO

 END SUBROUTINE UTL_SWAPSLASH

 !###======================================================================
 SUBROUTINE UTL_DIR_LEVEL_UP(FNAME)
 !###======================================================================

 IMPLICIT NONE

 CHARACTER(LEN=*), INTENT(INOUT) :: FNAME
 INTEGER :: N

 N = LEN_TRIM(FNAME)

 IF (N==0) RETURN

 IF (FNAME(1:1)=='.')THEN
  WRITE(FNAME,'(2A)') '..\',TRIM(FNAME)
 END IF

 CALL UTL_SWAPSLASH(FNAME)

 END SUBROUTINE UTL_DIR_LEVEL_UP

 !###===================================================================
 SUBROUTINE UTL_PRINTTEXT(TXT,TXTTYPE)
 !###===================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: TXT
 INTEGER,INTENT(IN) :: TXTTYPE

 SELECT CASE (TXTTYPE)
  !## file
  CASE (0)
   WRITE(*,'(A)') TRIM(TXT)
  !## information
  CASE (-1,1)
   WRITE(*,'(A)') TRIM(TXT)
   !IF(IFLAG(1).EQ.1)PAUSE
  !## error
  CASE (2)
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,TRIM(TXT),'Error')
  CASE DEFAULT
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,TRIM(TXT),'Error')
 END SELECT

 END SUBROUTINE UTL_PRINTTEXT

 !###======================================================================
 SUBROUTINE UTL_GET_ANGLES(X,Y,Z,RX,RY,RZ)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Y,Z
 REAL(KIND=DP_KIND),INTENT(OUT) :: RX,RY,RZ
 REAL(KIND=DP_KIND) :: P,DXY,DXYZ

 DXY =X**2.0D0+Y**2.0D0; IF(DXY.GT.0.0D0)DXY=SQRT(DXY)
 DXYZ=X**2.0D0+Y**2.0D0+Z**2.0D0; IF(DXYZ.GT.0.0D0)DXYZ=SQRT(DXYZ)

 !## get length of vector
 P=0.0D0; IF(DXYZ.GT.0.0D0)P=DXYZ

 RX=0.0D0
 RY=0.0D0
 RZ=0.0D0

 IF(P.GT.0.0D0)THEN
  !## get angle with x-axes
  RX=ACOS(X/P)
  !## get angle with x-axes
  RY=ACOS(Y/P)
 ENDIF

 !## get angle with x-axes
 IF(DXY.GT.0.0D0)RZ=ACOS(X/DXY)

! write(*,*) RX,RY,RZ
! write(*,*) RX*(360.0D0/(2.0*pi)),RY*(360.0D0/(2.0*pi)),RZ*(360.0D0/(2.0*pi))

 END SUBROUTINE UTL_GET_ANGLES

 !###======================================================================
 SUBROUTINE UTL_ROTATE_XYZ(X,Y,Z,AX,AY,AZ)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(INOUT) :: X,Y,Z
 REAL(KIND=DP_KIND),INTENT(IN) :: AX,AY,AZ
 REAL(KIND=DP_KIND) :: X1,Y1,Z1

 !## perform rotation around z-axes
 IF(AZ.NE.0.0D0)THEN
  X1= COS(AZ)*X+SIN(AZ)*Y
  Y1=-SIN(AZ)*X+COS(AZ)*Y
  X=X1
  Y=Y1
 ENDIF

 !## perform rotation around x-axes
 IF(AX.NE.0.0D0)THEN
  Y1=COS(AX)*Y-SIN(AX)*Z
  Z1=SIN(AX)*Y+COS(AX)*Z
  Y=Y1
  Z=Z1
 ENDIF

 !## perform rotation around y-axes
 IF(AY.NE.0.0D0)THEN
  X1= COS(AY)*X-SIN(AY)*Z
  Z1= SIN(AY)*X+COS(AY)*Z
  X=X1
  Z=Z1
 ENDIF

 END SUBROUTINE UTL_ROTATE_XYZ

 !###======================================================================
 SUBROUTINE UTL_PROFILE_GETVIEWBOX(X1,Y1,X2,Y2,XSIGHT,XYPOL,XMN,YMN,XMX,YMX)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X1,X2,Y1,Y2,XSIGHT
 REAL(KIND=DP_KIND),INTENT(OUT) :: XMN,YMN,XMX,YMX
 REAL(KIND=DP_KIND),DIMENSION(4,2),INTENT(OUT) :: XYPOL

 CALL UTL_PROFILE_COMPVIEWBOX(X1,X2,Y1,Y2,XYPOL,XSIGHT)
 XMN=MINVAL(XYPOL(:,1))
 XMX=MAXVAL(XYPOL(:,1))
 YMN=MINVAL(XYPOL(:,2))
 YMX=MAXVAL(XYPOL(:,2))

 END SUBROUTINE UTL_PROFILE_GETVIEWBOX

 !###======================================================================
 SUBROUTINE UTL_PROFILE_COMPVIEWBOX(X1,X2,Y1,Y2,XYPOL,XSIGHT)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),PARAMETER :: RAD=360.0D0/(2.0*3.1415)
 REAL(KIND=DP_KIND),INTENT(IN) :: X1,X2,Y1,Y2,XSIGHT
 REAL(KIND=DP_KIND),INTENT(OUT),DIMENSION(4,2) :: XYPOL
 REAL(KIND=DP_KIND) :: DX,DY,TNG

 DX =X2-X1
 DY =Y2-Y1
 IF(DY.EQ.0.0D0)TNG=0.0D0
 IF(ABS(DY).GT.0.0D0)TNG=ATAN2(DY,DX)
 TNG=TNG+90.0D0/RAD

 XYPOL(1,1)=X1+COS(TNG)*XSIGHT
 XYPOL(1,2)=Y1+SIN(TNG)*XSIGHT
 XYPOL(2,1)=X2+COS(TNG)*XSIGHT
 XYPOL(2,2)=Y2+SIN(TNG)*XSIGHT
 XYPOL(3,1)=X2-COS(TNG)*XSIGHT
 XYPOL(3,2)=Y2-SIN(TNG)*XSIGHT
 XYPOL(4,1)=X1-COS(TNG)*XSIGHT
 XYPOL(4,2)=Y1-SIN(TNG)*XSIGHT

 END SUBROUTINE UTL_PROFILE_COMPVIEWBOX

 !###======================================================================
 LOGICAL FUNCTION UTL_LOADIMAGE(BMPFNAME,N,IBMPDATA,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: BMPFNAME
 INTEGER,INTENT(IN) :: N
 INTEGER,INTENT(OUT),DIMENSION(N) :: IBMPDATA
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=256) :: LINE
 INTEGER :: I
 LOGICAL :: LEX

 UTL_LOADIMAGE=.TRUE.

 INQUIRE(FILE=BMPFNAME,EXIST=LEX)
 IF(.NOT.LEX)THEN
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'File: '//TRIM(BMPFNAME)//CHAR(13)//'does not exists','Error')
  IF(IBATCH.EQ.1)WRITE(*,'(A)') 'File: '//TRIM(BMPFNAME)//' does not exists'
  RETURN
 ENDIF

 !## clear existing error
 I=WINFOERROR(1)
 CALL IGRLOADIMAGEDATA(BMPFNAME,IBMPDATA)
 I=WINFOERROR(1)

 IF(I.EQ.0)RETURN

 CALL WINFOERRORMESSAGE(I,LINE)
 IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading file:'//CHAR(13)// &
  TRIM(BMPFNAME)//CHAR(13)//'Error message:'//CHAR(13)//TRIM(LINE),'Error')
 IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Error reading file:'//TRIM(BMPFNAME)//' Error message:'//TRIM(LINE)

 UTL_LOADIMAGE=.FALSE.

 END FUNCTION UTL_LOADIMAGE

 !###======================================================================
 INTEGER FUNCTION UTL_GETIDPROC(PROC,ICLEAN)
 !###======================================================================
 IMPLICIT NONE
 TYPE(PROCOBJ),INTENT(INOUT),POINTER,DIMENSION(:) :: PROC
 TYPE(PROCOBJ),POINTER,DIMENSION(:) :: PROC_BU
 INTEGER,INTENT(IN) :: ICLEAN
 INTEGER :: I,J,N,ISTATUS,IEXCOD
 CHARACTER(LEN=256) :: STRING
 INTEGER,DIMENSION(2) :: PID

 IF(ASSOCIATED(PROC))THEN
  !## evaluate current status
  DO I=1,SIZE(PROC)
   PID=PROC(I)%ID

   !## not running free, process
   IF(ISTATUS.EQ.0)THEN
    !## non-blocked process not stopped correctly
    IF(PROC(I)%IFLAGS(2).EQ.0.AND.IEXCOD.NE.0)THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,TRIM(STRING)//CHAR(13)//'in program:'//TRIM(PROC(I)%CID),'Program Terminated')
    ENDIF
    PROC(I)%ID=0; PROC(I)%CID=''; PROC(I)%IFLAGS=0
   !## process is still running
   ELSEIF(ISTATUS.EQ.1)THEN

   ENDIF
  ENDDO
 ELSE
  ALLOCATE(PROC(1)); PROC(1)%ID=0; PROC(1)%CID=''; PROC(1)%IFLAGS=0
 ENDIF

 N=SIZE(PROC)

 !## clean
 J=0; DO I=1,N
  IF(PROC(I)%ID.NE.0)THEN
   J=J+1; IF(I.NE.J)THEN; PROC(J)=PROC(I); ENDIF
  ENDIF
 ENDDO
 DO I=J+1,N; PROC(I)%ID=0; PROC(I)%CID=''; PROC(I)%IFLAGS=0; ENDDO

 !## find empty spot
 DO I=1,SIZE(PROC); IF(PROC(I)%ID.EQ.0)EXIT; ENDDO; N=I
 IF(ICLEAN.EQ.1)N=I-1

 IF(N.EQ.0)THEN
  IF(ASSOCIATED(PROC))DEALLOCATE(PROC)
 ELSE
  IF(N.NE.SIZE(PROC))THEN
   ALLOCATE(PROC_BU(N)); DO I=1,N;   PROC_BU(I)=PROC(I); ENDDO; DEALLOCATE(PROC)
   ALLOCATE(PROC(N+1));  DO I=1,N+1; PROC(I)%ID=0; PROC(I)%CID=''; PROC(I)%IFLAGS=0; ENDDO
   DO I=1,N; PROC(I)=PROC_BU(I); ENDDO; DEALLOCATE(PROC_BU)
  ENDIF
 ENDIF

 UTL_GETIDPROC=N

 END FUNCTION UTL_GETIDPROC

 !###======================================================================
 SUBROUTINE UTL_DELSPACE(LINE1,LINE2)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: LINE1
 CHARACTER(LEN=*),INTENT(OUT) :: LINE2
 INTEGER :: I,J,K

 LINE2=''; K=0
 J=0; DO I=1,LEN_TRIM(LINE1)
  IF(LINE1(I:I).EQ.CHAR(34).OR.LINE1(I:I).EQ.CHAR(39))THEN
   K=ABS(K-1)
  ENDIF
  !## copy non-spaces or inside quotes
  IF(LINE1(I:I).NE.CHAR(32).OR.K.EQ.1)THEN
   J=J+1; LINE2(J:J)=LINE1(I:I)
  ENDIF
 ENDDO

 END SUBROUTINE UTL_DELSPACE

 !###======================================================================
 LOGICAL FUNCTION UTL_DATA_CSV(TXT,VAR,ICOL_VAR,IACT_VAR,CCNST)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: TXT
 INTEGER :: I,J,ITYPE,NP
 CHARACTER(LEN=256) :: FNAME
 TYPE(WIN_MESSAGE) :: MESSAGE
 CHARACTER(LEN=*),POINTER,DIMENSION(:,:),INTENT(INOUT) :: VAR
 CHARACTER(LEN=*),POINTER,DIMENSION(:),INTENT(INOUT) :: CCNST
 INTEGER,ALLOCATABLE,DIMENSION(:),INTENT(INOUT) :: ICOL_VAR,IACT_VAR

 UTL_DATA_CSV=.FALSE.
 NP=SIZE(TXT)

 IF(.NOT.UTL_WSELECTFILE('Load Comma Separated File (*.csv)|*.csv|',&
                  LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,&
                  'Load Comma Separated File (*.csv)'))RETURN

 CALL UTL_MESSAGEHANDLE(0)
 CALL UTL_GENLABELSREAD(FNAME,VAR,NL,NV)
 CALL UTL_MESSAGEHANDLE(1)
 IF(NV.LE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot read column info (header) from file!','Error'); RETURN
 ENDIF

 CALL WDIALOGLOAD(ID_READCSV,ID_READCSV)
 IF(SIZE(TXT).GT.WINFOGRID(IDF_GRID1,GRIDROWSMAX))THEN
  CALL WDIALOGUNLOAD()
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot read more than '//TRIM(ITOS(WINFOGRID(IDF_GRID1,GRIDROWSMAX)))// &
  ' columns in this iMOD version','Error'); RETURN
 ENDIF

 CALL WGRIDROWS(IDF_GRID1,NP)

 !## put parameters
 CALL WGRIDPUTSTRING(IDF_GRID1,2,TXT,NP)

 !## assign variable to parameter
 IF(ALLOCATED(IACT_VAR))DEALLOCATE(IACT_VAR); ALLOCATE(IACT_VAR(NP))
 IACT_VAR=1
 CALL WGRIDPUTCHECKBOX(IDF_GRID1,1,IACT_VAR,NP)
 IF(ALLOCATED(ICOL_VAR))DEALLOCATE(ICOL_VAR); ALLOCATE(ICOL_VAR(NP))
 J=0; DO I=1,NP; J=J+1; IF(J.GT.NV)J=1; ICOL_VAR(I)=J; ENDDO
 CALL WGRIDPUTMENU(IDF_GRID1,3,VAR(:,0),NV,ICOL_VAR,NP)
 IF(ASSOCIATED(CCNST))DEALLOCATE(CCNST); ALLOCATE(CCNST(NP))
 CCNST=''
 CALL WGRIDPUTSTRING(IDF_GRID1,4,CCNST,NP)

 CALL WDIALOGSHOW(-1,-1,0,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDF_GRID1)
      CALL WGRIDGETCHECKBOX(IDF_GRID1,1,IACT_VAR,NP)
      DO I=1,SIZE(IACT_VAR)
       CALL WGRIDSTATECELL(IDF_GRID1,3,I,IACT_VAR(I))
       CALL WGRIDSTATECELL(IDF_GRID1,4,I,IACT_VAR(I))
      ENDDO
    END SELECT
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDOK)
      CALL WGRIDGETCHECKBOX(IDF_GRID1,1,IACT_VAR,NP)
      CALL WGRIDGETMENU(IDF_GRID1,3,ICOL_VAR,NP)
      CALL WGRIDGETSTRING(IDF_GRID1,4,CCNST,NP)
      UTL_DATA_CSV=.TRUE.
      EXIT
     CASE (IDHELP)
      CALL UTL_GETHELP('2.5.10','iF.CSV')
     CASE (IDCANCEL)
      EXIT
    END SELECT

  END SELECT
 ENDDO

 CALL WDIALOGUNLOAD()

 END FUNCTION UTL_DATA_CSV

 !###======================================================================
 SUBROUTINE UTL_GENLABELSGET(CID,JL,VARIABLE)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN),DIMENSION(:,:),POINTER :: VARIABLE
 CHARACTER(LEN=*),INTENT(IN) :: CID
 INTEGER,INTENT(OUT) :: JL
 INTEGER :: SC,N,M,J
 CHARACTER(LEN=52) :: STRING,GENSTR

 IF(.NOT.ASSOCIATED(VARIABLE))RETURN
 N=SIZE(VARIABLE,1); M=SIZE(VARIABLE,2)
 JL=0; IF(N.LE.0.OR.M.LE.0)RETURN

 SC=1  !## search column

 !## evaluate the first
 DO JL=1,M-1
  STRING=VARIABLE(SC,JL)
  !## math found
  J=INDEX(TRIM(UTL_CAP(CID,'U')),',')
  IF(J.GT.0)THEN
   GENSTR=CID(:J-1)
  ELSE
   GENSTR=CID
  ENDIF
  IF(TRIM(UTL_CAP(GENSTR,'U')).EQ.TRIM(UTL_CAP(STRING,'U')))RETURN
 END DO
 IF(JL.GE.NL)JL=0

 END SUBROUTINE UTL_GENLABELSGET

 !###======================================================================
 SUBROUTINE UTL_GENLABELSREAD(FNAME,VARIABLE,NVL,NVV,SKIPLINES,ILABELS)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 CHARACTER(LEN=*),DIMENSION(:,:),POINTER,INTENT(INOUT) :: VARIABLE
 INTEGER,INTENT(OUT) :: NVV,NVL
 INTEGER,INTENT(IN),OPTIONAL :: SKIPLINES,ILABELS
 INTEGER,ALLOCATABLE,DIMENSION(:) :: BPV,EPV
 INTEGER :: ML,I,J,INCL,IOS,IU,INL
 CHARACTER(LEN=1256) :: STRING
 CHARACTER(LEN=MAXLEN),DIMENSION(:,:),POINTER :: DVARIABLE

 !## initialize table of data for gen polygons
 NVV  =0
 NVL  =0
 INCL=50

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot OPEN/READ file called:'//CHAR(13)//&
    TRIM(FNAME),'Error')
  RETURN
 ENDIF

 !## estimate number of records
 NVL=0; DO
  READ(IU,'(A1256)',IOSTAT=IOS) STRING
  IF(IOS.NE.0)EXIT
  NVL=NVL+1
 ENDDO
 REWIND(IU)

 IF(PRESENT(SKIPLINES))THEN
  DO I=1,SKIPLINES; READ(IU,*); ENDDO
 ENDIF

 !## get number of variables
 READ(IU,'(A1256)',IOSTAT=IOS) STRING
 IF(IOS.NE.0)RETURN
 IF(LEN_TRIM(STRING).EQ.0)RETURN

 !## read the rest of the table in order to fill var(), 0=label
 INL=-1
 IF(PRESENT(ILABELS))INL=-ILABELS

 NVV=UTL_COUNT_COLUMNS(STRING,',;')
 ALLOCATE(BPV(NVV)); ALLOCATE(EPV(NVV))
 ALLOCATE(VARIABLE(NVV,INL+1:NVL))
 ML=NVL
 NVL=INL
 DO
  NVL=NVL+1
  IF(NVL.GT.ML)THEN
   ALLOCATE(DVARIABLE(NVV,INL+1:ML+INCL))
   !## copy current part
   DO I=1,SIZE(VARIABLE,1); DO J=INL+1,ML; DVARIABLE(I,J)=VARIABLE(I,J); ENDDO; ENDDO
   DEALLOCATE(VARIABLE)
   VARIABLE=>DVARIABLE
   ML=ML+INCL
   NULLIFY(DVARIABLE)
  ENDIF
  !## get variables
  NVV=UTL_COUNT_COLUMNS(STRING,',;',BPV=BPV,EPV=EPV)
  DO I=1,NVV
   VARIABLE(I,NVL)=''
   IF(BPV(I).LE.LEN(STRING).AND. &
      BPV(I).LE.EPV(I))THEN
    !## maximize length of variable
    J=(EPV(I)-BPV(I))+1; EPV(I)=BPV(I)+MIN(MAXLEN,J)-1
    VARIABLE(I,NVL)=STRING(BPV(I):EPV(I))
   ENDIF
  END DO
  READ(IU,'(A1256)',IOSTAT=IOS) STRING
  IF(IOS.NE.0)EXIT
  IF(LEN_TRIM(STRING).EQ.0)EXIT
 ENDDO
 CLOSE(IU)
 IF(ALLOCATED(BPV))DEALLOCATE(BPV); IF(ALLOCATED(EPV))DEALLOCATE(EPV)

 IF(NVL.NE.ML)THEN
  NVV=MAX(NVV,SIZE(VARIABLE,1))
  ALLOCATE(DVARIABLE(NVV,INL+1:NVL))
  !## copy current part
  DO I=1,SIZE(VARIABLE,1); DO J=INL+1,NVL; DVARIABLE(I,J)=VARIABLE(I,J); ENDDO; ENDDO
  DEALLOCATE(VARIABLE)
  VARIABLE=>DVARIABLE
  NULLIFY(DVARIABLE)
 ELSE
  NVV=0; NVL=0
 ENDIF

 END SUBROUTINE UTL_GENLABELSREAD

 !###======================================================================
 SUBROUTINE UTL_GENLABELSWRITE(FNAME,VAR)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 CHARACTER(LEN=*),POINTER,DIMENSION(:,:),INTENT(IN) :: VAR
 INTEGER :: IU,IOS,I,J
 CHARACTER(LEN=512) :: LINE

 !## nothing to write
 IF(NL.LE.0)RETURN
 IF(.NOT.ASSOCIATED(VAR))RETURN

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot OPEN/WRITE associated data file called:'//CHAR(13)//&
    TRIM(FNAME),'Error')
  RETURN
 ENDIF

 DO I=0,NL
  LINE=TRIM(VAR(1,I))
  DO J=2,NV; LINE=TRIM(LINE)//','//TRIM(VAR(J,I)); END DO
  WRITE(IU,'(A)') TRIM(LINE)
 ENDDO
 CLOSE(IU)

 END SUBROUTINE UTL_GENLABELSWRITE

 !###======================================================================
 SUBROUTINE UTL_GENLABELSDEALLOCATE()
 !###======================================================================
 IMPLICIT NONE

 IF(ASSOCIATED(VAR))    DEALLOCATE(VAR)
 IF(ALLOCATED(ICOL_VAR))DEALLOCATE(ICOL_VAR)
 IF(ALLOCATED(IACT_VAR))DEALLOCATE(IACT_VAR)
 IF(ASSOCIATED(CCNST))  DEALLOCATE(CCNST)

 END SUBROUTINE UTL_GENLABELSDEALLOCATE

 !###======================================================================
 REAL(KIND=DP_KIND) FUNCTION UTL_POLYGON1AREA(X,Y,N)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(N) :: X,Y
 INTEGER :: I

 UTL_POLYGON1AREA=0.0D0
 DO I=1,N-1
  UTL_POLYGON1AREA=UTL_POLYGON1AREA+0.5D0*((X(I)*Y(I+1))-(X(I+1)*Y(I)))
 END DO
 UTL_POLYGON1AREA=UTL_POLYGON1AREA+0.5D0*((X(N)*Y(1))-(X(1)*Y(N)))

 END FUNCTION UTL_POLYGON1AREA

! !###======================================================================
! REAL(KIND=DP_KIND) FUNCTION UTL_POLYGON1AREA(X,Y,N)
! !###======================================================================
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: N
! REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(N) :: X,Y
! INTEGER :: I
! REAL(KIND=DP_KIND) :: XM,DY
!
! UTL_POLYGON1AREA=0.0D0
! DO I=1,N-1
!  XM=(X(I)+X(I+1))/2.0D0
!  DY=Y(I+1)-Y(I)
!  UTL_POLYGON1AREA=UTL_POLYGON1AREA+(XM-XMIN)*DY
! ENDDO
!
! END FUNCTION UTL_POLYGON1AREA

 !###======================================================================
 REAL(KIND=DP_KIND) FUNCTION UTL_GETGAMMA(X1,Y1,X2,Y2,RANGE,C1,C0,KTYPE)  !c1=sill-nugget c0=nugget
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: KTYPE
 REAL(KIND=DP_KIND),INTENT(IN) :: X1,Y1,X2,Y2,RANGE,C1,C0
 REAL(KIND=DP_KIND) :: DXY,H
 
 DXY=UTL_DIST(X1,Y1,X2,Y2)
 
 IF(DXY.GE.RANGE)THEN
  UTL_GETGAMMA=C1; RETURN
!  H=0.999D0 
 ELSE

  !## no part of kriging, beyond given range, equal to sill
  SELECT CASE (ABS(KTYPE))
   CASE (1) !## linear
    H=DXY/RANGE
   CASE (2) !## spherical = OKAY
    H=(3.0D0*DXY)/(2.0D0*RANGE)-((DXY**3.0D0)/(2.0D0*RANGE**3.0D0))
!    H=(3.0D0*DXY)/(2.0D0*RANGE)-(0.5D0*(DXY/RANGE)**3.0D0)
   CASE (3) !## exponential
    H=1.0D0-10.0D0**(-3.0D0*DXY/RANGE)
!    H=1.0D0-EXP((-3.0D0*DXY)/RANGE)
!    H=1.0D0-EXP(-3.0D0*(DXY/RANGE))
   CASE (4) !## gaussian
    H=1.0D0-10.0D0**(-3.0D0*(DXY**2.0D0)/(RANGE**2.0D0))
!    H=1.0D0-EXP(-3.0D0*(DXY**2.0D0)/(RANGE**2.0D0))
   CASE (5) !## power
    H=DXY**0.5D0
   CASE DEFAULT
    WRITE(*,*) 'UNKNOWN KTYPE',KTYPE; PAUSE; STOP
  END SELECT
 ENDIF
 
 UTL_GETGAMMA=(C1-C0)*H+C0
! KRIGING_GETGAMMA=C0+C1*H
 
 END FUNCTION UTL_GETGAMMA
 
 !###======================================================================
 SUBROUTINE UTL_STDEF(X,N,NODATA,STDEV,XT,NPOP)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER,INTENT(OUT) :: NPOP
 REAL(KIND=DP_KIND),DIMENSION(N),INTENT(IN) :: X
 REAL(KIND=DP_KIND),INTENT(IN) :: NODATA
 REAL(KIND=DP_KIND),INTENT(OUT) :: XT,STDEV
 INTEGER :: I
 REAL(KIND=DP_KIND) :: XV

 STDEV=0.0D0

 NPOP=0
 XT=0.0D0
 DO I=1,N
  IF(X(I).NE.NODATA)THEN
   NPOP=NPOP+1
   XT=XT+X(I)
  ENDIF
 ENDDO

 IF(NPOP.LT.2)RETURN

 XT=XT/DBLE(NPOP)

 NPOP=0
 XV=0.0D0
 DO I=1,N
  IF(X(I).NE.NODATA)THEN
   NPOP=NPOP+1
   XV=XV+(X(I)-XT)**2.0D0
  ENDIF
 END DO

 IF(XV.LE.0.0D0)RETURN
 !## sample standard deviation
 STDEV=SQRT(XV/DBLE(NPOP-1))
 !## population standard deviation
 ! VAR=SQRT(XV/REAL(NPOP))

 END SUBROUTINE UTL_STDEF

 !###======================================================================
 REAL(KIND=DP_KIND) FUNCTION UTL_DIST(X1,Y1,X2,Y2)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X1,Y1,X2,Y2
 REAL(KIND=DP_KIND) :: DX,DY

 UTL_DIST=0.0D0

 DX=(X1-X2)**2.0D0; DY=(Y1-Y2)**2.0D0
 IF(DX+DY.NE.0.0D0)UTL_DIST=SQRT(DX+DY)

 END FUNCTION UTL_DIST

 !###======================================================================
 REAL(KIND=DP_KIND) FUNCTION UTL_DIST_3D(X1,Y1,Z1,X2,Y2,Z2)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: X1,Y1,Z1,X2,Y2,Z2
 REAL(KIND=DP_KIND) :: DX,DY,DZ

 UTL_DIST_3D=0.0D0

 DX=(X2-X1)**2.0D0; DY=(Y2-Y1)**2.0D0; DZ=(Z2-Z1)**2.0D0
 IF(DX+DY+DZ.NE.0.0D0)UTL_DIST_3D=SQRT(DX+DY+DZ)

 END FUNCTION UTL_DIST_3D

 !###======================================================================
 LOGICAL FUNCTION UTL_WSELECTFILE(FILTERSTR,IFLAGS,FILEDIR,TITLE,FTYPE)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FILTERSTR
 CHARACTER(LEN=*),INTENT(INOUT) :: FILEDIR
 INTEGER,INTENT(IN) :: IFLAGS
 INTEGER,INTENT(OUT),OPTIONAL :: FTYPE
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: TITLE
 INTEGER :: I,J,K,ISAVE
 CHARACTER(LEN=10) :: EXT
 INTEGER :: IFTYPE

 UTL_WSELECTFILE=.FALSE.

 !## store original filedir
 ISAVE=1; IF(INDEX(FILEDIR,'*').GT.0)ISAVE=0
 IF(ISAVE.EQ.1)FILEDIR=SAVEDIR
 IFTYPE=1; IF(PRESENT(FTYPE))IFTYPE=FTYPE

 DO

  IF(PRESENT(TITLE))THEN
   CALL WSELECTFILE(FILTERSTR,IFLAGS,FILEDIR,TITLE,IFTYPE=IFTYPE)
  ELSE
   CALL WSELECTFILE(FILTERSTR,IFLAGS,FILEDIR,IFTYPE=IFTYPE)
  ENDIF
  IF(WINFODIALOG(4).NE.1)THEN
   FILEDIR=''
   RETURN
  ENDIF
  !## check extent ...
  I=INDEX(FILEDIR,'.',.TRUE.)
  IF(I.EQ.0)EXIT

  IF(INDEX(FILTERSTR,'*.*').LE.0)THEN
   EXT=FILEDIR(I+1:)
   J=INDEX(UTL_CAP_BIG(FILTERSTR(1:MIN(1024,LEN(FILTERSTR))),'U'),'*.'//TRIM(UTL_CAP(EXT,'U')))
   IF(J.NE.0)EXIT

   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should select a file that agrees the supplied filterstring:'//CHAR(13)// &
    TRIM(FILTERSTR),'Error')
  ELSE
   EXIT
  ENDIF

 ENDDO

 !## removes filename from directory name before saving
 K=INDEX(FILEDIR,'\',.TRUE.)
 !## save directory name into SAVEDIR
 IF(ISAVE.EQ.1)SAVEDIR=FILEDIR(:K)
 IF(PRESENT(FTYPE))FTYPE=IFTYPE

 UTL_WSELECTFILE=.TRUE.

 END FUNCTION UTL_WSELECTFILE

 !###======================================================================
 SUBROUTINE UTL_PLOT1BITMAP()
 !###======================================================================
 IMPLICIT NONE

 CALL IGRSELECT(DRAWBITMAP,MPW%IBITMAP)
 CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0)
 CALL DBL_IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX,IOFFSET=1)

 END SUBROUTINE UTL_PLOT1BITMAP

 !###======================================================================
 SUBROUTINE UTL_PLOT2BITMAP()
 !###======================================================================
 IMPLICIT NONE

 CALL IGRSELECT(DRAWWIN)
 CALL WINDOWSELECT(MPW%IWIN)
 CALL WBITMAPVIEW(MPW%IBITMAP,MPW%IX,MPW%IY,MODELESS)
 CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0)
 CALL DBL_IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX,IOFFSET=1)

 END SUBROUTINE UTL_PLOT2BITMAP

 !###======================================================================
 FUNCTION REALTOSTRING(X)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=20) :: REALTOSTRING
 REAL(KIND=DP_KIND),INTENT(IN) :: X
 INTEGER :: I

 WRITE(REALTOSTRING,*) X
 !eliminate all zero at the end!
 DO I=LEN_TRIM(REALTOSTRING),1,-1
  IF(REALTOSTRING(I:I).NE.'0')EXIT
 END DO
 IF(REALTOSTRING(I:I).EQ.'.'.OR.REALTOSTRING(I:I).EQ.',')I=I-1
 REALTOSTRING=REALTOSTRING(1:I)

 END FUNCTION REALTOSTRING

 !###======================================================================
 INTEGER FUNCTION UTL_IDFGETCLASS(LEG,GRD)
 !###======================================================================
 IMPLICIT NONE
 TYPE(LEGENDOBJ),INTENT(IN) :: LEG
 REAL(KIND=DP_KIND),INTENT(IN) :: GRD
 INTEGER :: I

 !## default=wit!
 UTL_IDFGETCLASS=WRGB(255,255,255)
 !## NaN
 IF(GRD.NE.GRD)RETURN

 IF(GRD.GT.LEG%CLASS(0).OR.GRD.LT.LEG%CLASS(LEG%NCLR))RETURN

 CALL POL1LOCATE(LEG%CLASS,LEG%NCLR,REAL(GRD,8),I)
 !## correct if equal to top-class boundary
 IF(I.GT.0.AND.I.LE.LEG%NCLR)THEN !MXCLR)THEN
  UTL_IDFGETCLASS=LEG%RGB(I)
 ELSE
  IF(UTL_EQUALS_REAL(GRD,LEG%CLASS(0)))UTL_IDFGETCLASS=LEG%RGB(1)
 ENDIF

 END FUNCTION UTL_IDFGETCLASS

 !###======================================================================
 SUBROUTINE UTL_IDFCURDIM(XMIN,YMIN,XMAX,YMAX,IDF,NC1,NC2,NR1,NR2)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 REAL(KIND=DP_KIND),INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
 REAL(KIND=DP_KIND) :: D
 INTEGER,INTENT(OUT) :: NC1,NC2,NR1,NR2

 IF(IDF%IEQ.EQ.0)THEN

  !## min. column
  D  =XMIN-IDF%XMIN
  NC1=INT(D/IDF%DX)+1
  IF(MOD(D,IDF%DX).NE.0.0D0)NC1=NC1+1
  !## max. column
  D  =XMAX-IDF%XMIN
  NC2=INT(D/IDF%DX)
  !## min. row
  D  =IDF%YMAX-YMAX
  NR1=INT(D/IDF%DY)+1
  IF(MOD(D,IDF%DY).NE.0.0D0)NR1=NR1+1
  !## max. row
  D  =IDF%YMAX-YMIN
  NR2=INT(D/IDF%DY)

 ELSE

  !## min. column
  CALL POL1LOCATE(IDF%SX,IDF%NCOL+1,REAL(XMIN,8),NC1)
  !## max. column
  CALL POL1LOCATE(IDF%SX,IDF%NCOL+1,REAL(XMAX,8),NC2)
  !## min. row
  CALL POL1LOCATE(IDF%SY,IDF%NROW+1,REAL(YMAX,8),NR1)
  !## max. row
  CALL POL1LOCATE(IDF%SY,IDF%NROW+1,REAL(YMIN,8),NR2)

 ENDIF

 NC1=MAX(1,NC1); NC1=MIN(NC1,IDF%NCOL)
 NC2=MAX(1,NC2); NC2=MIN(NC2,IDF%NCOL)
 NR1=MAX(1,NR1); NR1=MIN(NR1,IDF%NROW)
 NR2=MAX(1,NR2); NR2=MIN(NR2,IDF%NROW)

 IF(IDF%IEQ.EQ.1)THEN
  IF(MPW%XMIN.GT.IDF%SX(NC1-1))NC1=NC1+1
  IF(MPW%XMAX.LT.IDF%SX(NC2))  NC2=NC2-1
  IF(MPW%YMAX.LT.IDF%SY(NR1-1))NR1=NR1+1
  IF(MPW%YMIN.GT.IDF%SY(NR2))  NR2=NR2-1
 ENDIF

 END SUBROUTINE UTL_IDFCURDIM

 !###======================================================================
 SUBROUTINE UTL_IDFCRDCOR(X1,X2,Y1,Y2,WIDTH,HEIGTH)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: WIDTH,HEIGTH
 REAL(KIND=DP_KIND),INTENT(INOUT) :: X1,X2,Y1,Y2
 REAL(KIND=DP_KIND) :: RAT1,RAT2,DX,DY,XC,YC

 RAT1=WIDTH/HEIGTH; DX=X2-X1; DY=Y2-Y1; RAT2=DX/DY
 XC=(X1+X2)/2.0D0; YC=(Y1+Y2)/2.0D0

 !## area up
 IF(RAT1.LT.1.0D0)THEN

  !## box smaller than image
  IF(RAT1.LT.RAT2)THEN
   DY=0.5D0*DX/RAT1; Y1=YC-DY; Y2=YC+DY
  !## image smaller than box
  ELSE
   DX=0.5D0*DY*RAT1; X1=XC-DX; X2=XC+DX
  ENDIF

 !## area flat
 ELSE

  !##
  IF(RAT1.GT.RAT2)THEN
   DX=0.5D0*DY*RAT1; X1=XC-DX; X2=XC+DX
  !## figure flat too
  ELSE
   DY=0.5D0*DX/RAT1; Y1=YC-DY; Y2=YC+DY
  ENDIF

 ENDIF

 END SUBROUTINE UTL_IDFCRDCOR

 !###======================================================================
 SUBROUTINE UTL_FILLARRAY(IP,NP,B)
 !###======================================================================
 !## read binair number (e.g. 256) and returns array (/1,0,0,1,0,0,1/)
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NP,B
 INTEGER,INTENT(OUT),DIMENSION(NP) :: IP
 INTEGER :: I,BB

 IP=0
 BB=B
 DO I=1,NP
  IP(I)=MOD(BB,2)
  BB=BB/2
 END DO

 !## make sure results are only 0/1 values
 DO I=1,NP
  IF(IP(I).LT.0.OR.IP(I).GT.1)IP(I)=0
 END DO

 END SUBROUTINE UTL_FILLARRAY

 !###======================================================================
 SUBROUTINE UTL_READARRAY(IP,NP,B)
 !###======================================================================
 !## write a binair-number given an array (/1,0,0,4,0,0,7/)
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NP
 INTEGER,INTENT(OUT) :: B
 INTEGER,INTENT(IN),DIMENSION(NP) :: IP
 INTEGER :: I,J

 B=0
 DO I=1,NP
  J=MAX(0,MIN(IP(I),1))
  B=B+(J*(2**(I-1)))
 END DO

 END SUBROUTINE UTL_READARRAY

 !###======================================================================
 LOGICAL FUNCTION UTL_READINITFILE(CKEY,LINE,IU,IOPT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,IOPT
 CHARACTER(LEN=*),INTENT(IN) :: CKEY
 CHARACTER(LEN=*),INTENT(OUT) :: LINE
 INTEGER :: IOS,I,J,N,M,II,ITRY
 CHARACTER(LEN=10) :: FRMT
 CHARACTER(LEN=256) :: FNAME
 CHARACTER(LEN=:),ALLOCATABLE :: STR

 UTL_READINITFILE=.FALSE.

 N=LEN(LINE); WRITE(FRMT,'(A2,I7.7,A1)') '(A',N,')'

 !## backup line
 ALLOCATE(CHARACTER(LEN=N) :: STR)

 !## read from current position, if not found try from beginning
 ITRY=1
 DO
  READ(IU,FRMT,IOSTAT=IOS) LINE
  IF(IOS.NE.0)THEN
   IF(ITRY.EQ.2)THEN
    IF(IOPT.EQ.0)THEN
     INQUIRE(UNIT=IU,NAME=FNAME); CLOSE(IU)
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD DID NOT find keyword: ['//TRIM(CKEY)//'] within Settings file:'// &
           CHAR(13)//'['//TRIM(FNAME)//']','Error')
    ENDIF
    RETURN
   ELSE
    REWIND(IU)
    ITRY=ITRY+1
    CYCLE
   ENDIF
  ENDIF

  STR=LINE

  N=LEN(LINE)
  M=256  !## length of utl_cap() function
  !## split cap function in case
  IF(N.GT.M)THEN
   I=1; J=I+M-1
   DO
    LINE(I:J)=UTL_CAP(LINE(I:J),'U')
    I=I+M
    IF(I.GT.N)EXIT
    J=MIN(N,I+M-1)
   ENDDO
  ELSE
   LINE=UTL_CAP(LINE,'U')
  ENDIF

  II=INDEX(TRIM(LINE),'!') !## skip comment lines for keyword
  I =INDEX(TRIM(LINE),TRIM(CKEY))
  IF(II.EQ.0)II=I
  !## okay, proper line found
  IF(I.NE.0.AND.II.GE.I)THEN
   !## make sure previous to i or j no character is available
   IF(I.GE.2)THEN
    IF(LINE(I-1:I-1).NE.' ')I=0 !## not correct
   ENDIF
   !## make sure next to i or j no character or "=" sign
   IF(LINE(I+LEN_TRIM(CKEY):I+LEN_TRIM(CKEY)).NE.' '.AND. &
      LINE(I+LEN_TRIM(CKEY):I+LEN_TRIM(CKEY)).NE.'=')I=0 !## not correct
   J=INDEX(TRIM(LINE),'=')
   IF(I.NE.0.AND.J.GT.I)EXIT
  ENDIF
 ENDDO

 I=INDEX(LINE,'=')
 IF(I.LE.0)THEN
  INQUIRE(UNIT=IU,NAME=FNAME); CLOSE(IU)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD misses "=" after keyword: ['//TRIM(CKEY)//'] within Settings file:'// &
             CHAR(13)//'[ '//TRIM(FNAME)//' ]','Error')
  RETURN
 ENDIF

 I=I+1
 LINE(1:N-I+1)=STR(I:N)

 !## remove leading space, if there is one
 LINE=ADJUSTL(LINE)

 DEALLOCATE(STR)

 !## check whether there is an argment given ...
 IF(TRIM(LINE).EQ.'')THEN
  INQUIRE(UNIT=IU,NAME=FNAME); CLOSE(IU)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD misses an argument after the "=" sign for keyword: ['//TRIM(CKEY)//'] within Settings file:'// &
             CHAR(13)//'[ '//TRIM(FNAME)//' ]','Error')
  RETURN
 ENDIF

 UTL_READINITFILE=.TRUE.

 END FUNCTION UTL_READINITFILE

 !###====================================================================
 SUBROUTINE UTL_DRAWLEGENDBOX(XMIN,YMIN,XMAX,YMAX,ICLR,IWIDTH,ITYPE,IPATTERN,LEG,XT)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPATTERN !## 0=solid,1=line,2=dots
 INTEGER,INTENT(IN) :: ICLR,IWIDTH,ITYPE
 REAL(KIND=DP_KIND),INTENT(IN) :: XMIN,XMAX,YMAX
 REAL(KIND=DP_KIND),INTENT(INOUT) :: YMIN
 REAL(KIND=DP_KIND),INTENT(IN),OPTIONAL :: XT
 TYPE(LEGENDOBJ),INTENT(INOUT),OPTIONAL :: LEG
 REAL(KIND=DP_KIND) :: DX,DY,Y
 INTEGER :: I

 !## solid
 IF(IPATTERN.EQ.0)THEN

  CALL IGRCOLOURN(ICLR)
  CALL IGRFILLPATTERN(SOLID)
  CALL DBL_IGRRECTANGLE(XMIN,YMIN,XMAX,YMAX)

 !## lines
 ELSEIF(IPATTERN.EQ.1)THEN

  !## clear it (white)
  CALL IGRFILLPATTERN(SOLID)
  CALL IGRCOLOURN(WRGB(255,255,255))
  CALL DBL_IGRRECTANGLE(XMIN,YMIN,XMAX,YMAX)

  CALL IGRCOLOURN(ICLR)
  CALL IGRFILLPATTERN(OUTLINE)

  CALL IGRLINETYPE(ITYPE)
  CALL IGRLINEWIDTH(IWIDTH)
  CALL DBL_IGRMOVETO(XMIN,YMIN)
  DX=(XMAX-XMIN)/3.0
  DY=(YMAX-YMIN)

  CALL DBL_IGRLINETOREL(DX, DY)
  CALL DBL_IGRLINETOREL(DX,-DY)
  CALL DBL_IGRLINETOREL(DX, DY)

 !## dots
 ELSEIF(IPATTERN.EQ.2)THEN

  !## clear it (white)
  CALL IGRFILLPATTERN(SOLID)
  CALL IGRCOLOURN(WRGB(255,255,255))
  CALL DBL_IGRRECTANGLE(XMIN,YMIN,XMAX,YMAX)

  DY=(XMAX-XMIN)/10.0D0
  CALL IGRCOLOURN(ICLR)
  CALL IGRFILLPATTERN(SOLID)
  DX=(XMAX-XMIN)/4.0
  DO I=1,3
   CALL DBL_IGRCIRCLE(XMIN+(DX*REAL(I)),(YMAX+YMIN)/2.0,DY)
  END DO

 !## filled in (a) if present with legend (b) stripes
 ELSEIF(IPATTERN.EQ.3)THEN

  CALL IGRFILLPATTERN(SOLID)

  !## use a legend if present
  IF(PRESENT(LEG))THEN
   IF(LEG%NCLR.GT.MXCLASS)THEN; DY=(3.0*(YMAX-YMIN))/REAL(LEG%NCLR); ELSE; DY=YMAX-YMIN; ENDIF; Y=YMAX
   DO I=1,LEG%NCLR
    CALL IGRCOLOURN(LEG%RGB(I)); CALL DBL_IGRRECTANGLE(XMIN,Y-DY,XMAX,Y)
    IF(LEG%NCLR.LE.MXCLASS)THEN
     CALL IGRCOLOURN(WRGB(0,0,0)); CALL DBL_WGRTEXTSTRING(XT,Y-(DY/2.0),TRIM(LEG%LEGTXT(I)))
    ELSE
     CALL IGRCOLOURN(WRGB(0,0,0))
     IF(I.EQ.1)       CALL DBL_WGRTEXTSTRING(XT,YMAX-(0.5*(YMAX-YMIN)),TRIM(LEG%LEGTXT(I)))
     IF(I.EQ.LEG%NCLR)CALL DBL_WGRTEXTSTRING(XT,YMAX-(2.5*(YMAX-YMIN)),TRIM(LEG%LEGTXT(I)))
    ENDIF
    Y=Y-DY
   END DO
   YMIN=Y
  ELSE
   DX=(XMAX-XMIN)/10.0D0
   DO I=1,9
    IF(MOD(I,2).EQ.0)CALL IGRCOLOURN(ICLR)
    IF(MOD(I,2).NE.0)CALL IGRCOLOURN(WRGB(255,255,255))
    CALL DBL_IGRRECTANGLE(XMIN+(DX*REAL(I-1)),YMIN,XMIN+(DX*REAL(I)),YMAX)
   END DO
  ENDIF

 ENDIF

 CALL IGRFILLPATTERN(OUTLINE)
 CALL IGRLINETYPE(SOLIDLINE)
 CALL IGRLINEWIDTH(1)
 CALL IGRCOLOURN(WRGB(225,225,225))
 CALL DBL_IGRRECTANGLE(XMIN,YMIN,XMAX,YMAX)

 CALL IGRCOLOURN(ICLR)

 END SUBROUTINE

 !###====================================================================
 SUBROUTINE UTL_GETRELEVANTDIR(DIRNAMES,NDIR)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NDIR
 CHARACTER(LEN=*),INTENT(INOUT),DIMENSION(NDIR) :: DIRNAMES
 INTEGER :: I,II,JJ,J

 !## nothing to do
 IF(NDIR.LE.0)RETURN

 IF(NDIR.EQ.1)THEN
  I=INDEX(DIRNAMES(1),'\',.TRUE.)
  IF(I.NE.0)DIRNAMES(1)='..\'//DIRNAMES(1)(I+1:)
  RETURN
 ENDIF

 DO I=1,NDIR
  DIRNAMES(I)=UTL_CAP(DIRNAMES(I),'U')
 END DO
 II=0
 JJ=0
 DO WHILE(JJ.EQ.0)
  II=II+1
  DO I=1,NDIR
   DO J=1,NDIR
    IF(DIRNAMES(I)(II:II).NE.DIRNAMES(J)(II:II).AND.JJ.EQ.0)JJ=II!EXIT! LOOPII
   END DO
  END DO
 ENDDO
 DO I=1,NDIR
  J=INDEX(DIRNAMES(I)(:II),'\',.TRUE.)
  IF(J.NE.0)DIRNAMES(I)='..\'//DIRNAMES(I)(J+1:)
 ENDDO

 END SUBROUTINE UTL_GETRELEVANTDIR

 !###====================================================================
 SUBROUTINE UTL_GETDIRPART(IPART,DIR,DIRPART)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPART
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 CHARACTER(LEN=*),INTENT(OUT) :: DIRPART
 INTEGER :: I,J,K

 DIRPART=''

 IF(IPART.EQ.0)THEN
  J=INDEX(DIR,':')
  IF(J.EQ.0)RETURN
  DIRPART=DIR(:J-1)
 ELSE
  K=1
  DO I=1,IPART
   J=INDEX(DIR(K:),'\')
   !## nothing found for current ipart
   IF(J.EQ.0)RETURN
   K=J+1
  ENDDO
  J=INDEX(DIR(K:),'\')
  IF(J.NE.0)DIRPART=DIR(K:J-1)
 ENDIF

 END SUBROUTINE UTL_GETDIRPART

 !###====================================================================
 SUBROUTINE UTL_SETTEXTSIZE(CHW,CHH,FCT,IMARKER)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),PARAMETER :: INI_CHH=0.00333D0
 REAL(KIND=DP_KIND),PARAMETER :: INI_CHW=0.00133D0
 REAL(KIND=DP_KIND),OPTIONAL :: FCT
 INTEGER,INTENT(IN),OPTIONAL :: IMARKER
 REAL(KIND=DP_KIND),INTENT(OUT) :: CHW,CHH
 REAL(KIND=DP_KIND) :: IWD,IHD,X1,X2,Y1,Y2,RAT,DY

 IWD=REAL(WINFODRAWABLE(DRAWABLEWIDTH),8)
 IHD=REAL(WINFODRAWABLE(DRAWABLEHEIGHT),8)
 X1 =REAL(INFOGRAPHICS(GRAPHICSAREAMINX),8)
 X2 =REAL(INFOGRAPHICS(GRAPHICSAREAMAXX),8)
 Y1 =REAL(INFOGRAPHICS(GRAPHICSAREAMINY),8)
 Y2 =REAL(INFOGRAPHICS(GRAPHICSAREAMAXY),8)

 DY=1.0D0; IF(PRESENT(FCT))DY=FCT

 CHH=INI_CHH
 IF(PRESENT(IMARKER))THEN
  IF(IMARKER.EQ.1)CHH=0.00133D0
 ENDIF

 CHH=CHH*FCT
 CHW=INI_CHW*FCT

 RAT=IWD/IHD
 CHW=CHW/RAT

 RAT=(X2-X1)/(Y2-Y1)
 CHW=CHW/RAT

 END SUBROUTINE UTL_SETTEXTSIZE

 !###======================================================================
 SUBROUTINE UTL_IMODFILLMENU(ID,DIRNAME,WC,F,N,IMENUTYPE,ISTORE,SETNAME,CORDER)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: N
 INTEGER,INTENT(IN) :: ID,IMENUTYPE,ISTORE                ! Dialog ID,*, *
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: SETNAME,CORDER   !*, display ordered (1) or not (0)
 CHARACTER(LEN=*),INTENT(IN) :: DIRNAME,WC,F              ! Directoryname, Selection string, display type (file 'F' or directory 'D')
 INTEGER :: I

 CALL UTL_IMODFILLMENU_DEAL()

 N=0

 IF(LEN_TRIM(DIRNAME).EQ.0)THEN
  IF(ID.NE.0)CALL WDIALOGCLEARFIELD(ID)
  RETURN
 ENDIF

 IF(.NOT.IOSDIREXISTS(DIRNAME))THEN
  IF(ID.NE.0)CALL WDIALOGCLEARFIELD(ID)
  RETURN
 ENDIF

 CALL IOSDIRENTRYTYPE(F)
 CALL IOSDIRCOUNT(DIRNAME,WC,N)

 IF(N.EQ.0)THEN
  IF(ID.NE.0)THEN
   CALL WDIALOGCLEARFIELD(ID)
   CALL WDIALOGFIELDSTATE(ID,2)
  ENDIF
 ELSE
  ALLOCATE(LISTNAME(N))
  IF(PRESENT(CORDER))THEN
   CALL UTL_DIRINFO(DIRNAME,WC,LISTNAME,N,F,CORDER)
  ELSE
   CALL UTL_DIRINFO(DIRNAME,WC,LISTNAME,N,F)
  ENDIF
  IF(N.GT.0.AND.ID.NE.0)THEN
   DO I=1,N; LISTNAME(I)=UTL_CAP(LISTNAME(I),'U'); END DO
   CALL WDIALOGFIELDSTATE(ID,1)
   IF(IMENUTYPE.EQ.0)THEN
    IF(PRESENT(SETNAME))THEN
     DO I=1,N; IF(UTL_CAP(LISTNAME(I),'U').EQ.UTL_CAP(SETNAME,'U'))EXIT; ENDDO
     IF(I.LE.N)THEN
      CALL WDIALOGPUTMENU(ID,LISTNAME,N,I)
     ELSE
      CALL WDIALOGPUTMENU(ID,LISTNAME,N,1)
     ENDIF
    ELSE
     CALL WDIALOGPUTMENU(ID,LISTNAME,N,1)
    ENDIF
   ELSEIF(IMENUTYPE.EQ.1)THEN
    ALLOCATE(ILIST(N)); ILIST=0
    CALL WDIALOGPUTMENU(ID,LISTNAME(1:N),N,ILIST)
   ENDIF
  ELSE
   IF(ID.NE.0)THEN
    CALL WDIALOGCLEARFIELD(ID)
    IF(IMENUTYPE.EQ.0)CALL WDIALOGFIELDSTATE(ID,2)
   ENDIF
  ENDIF
 ENDIF

 IF(ISTORE.EQ.0)CALL UTL_IMODFILLMENU_DEAL()

 END SUBROUTINE UTL_IMODFILLMENU

 !###====================================================================
 SUBROUTINE UTL_IMODFILLMENU_DEAL()
 !###====================================================================
 IMPLICIT NONE

 IF(ALLOCATED(ILIST))DEALLOCATE(ILIST)
 IF(ALLOCATED(LISTNAME))DEALLOCATE(LISTNAME)

 END SUBROUTINE UTL_IMODFILLMENU_DEAL

 !###====================================================================
 INTEGER FUNCTION GETITOPIC(CKEYWORD)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: CKEYWORD
 INTEGER :: I

 GETITOPIC=0
 DO I=1,MXTP
  IF(TRIM(TP(I)%ACRNM).EQ.TRIM(CKEYWORD))GETITOPIC=I
 ENDDO

 END FUNCTION GETITOPIC

 !###======================================================================
 SUBROUTINE UTL_PLOTLOCATIONIDF(IDF,IROW,ICOL)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: IROW,ICOL
 REAL(KIND=DP_KIND) :: X1,X2,Y1,Y2

 IF(IROW.EQ.0.OR.ICOL.EQ.0)RETURN

 CALL IGRPLOTMODE(MODEXOR); CALL IGRFILLPATTERN(OUTLINE); CALL IGRLINEWIDTH(2)
 CALL UTL_PLOT1BITMAP()

 IF(IDF%IEQ.EQ.0)THEN
  X1=IDF%XMIN+(ICOL-1)*IDF%DX; X2=IDF%XMIN+ ICOL   *IDF%DX
  Y1=IDF%YMAX-(IROW-1)*IDF%DY; Y2=IDF%YMAX- IROW   *IDF%DY
 ELSEIF(IDF%IEQ.EQ.1)THEN
  X1=IDF%SX(ICOL-1); Y1=IDF%SY(IROW-1)
  X2=IDF%SX(ICOL);   Y2=IDF%SY(IROW)
 ENDIF

 !## selected cell
 CALL DBL_IGRRECTANGLE(X1,Y1,X2,Y2,IOFFSET=1)

 CALL IGRLINEWIDTH(1); CALL IGRLINETYPE(SOLIDLINE); CALL UTL_PLOT2BITMAP()

 END SUBROUTINE UTL_PLOTLOCATIONIDF

 !###======================================================================
 SUBROUTINE UTL_HIDESHOWDIALOG(ID,ISHOW)
 !###======================================================================
 INTEGER,INTENT(IN) :: ID,ISHOW
 INTEGER :: IX,IY,I

 I=WINFOERROR(1)
 CALL WDIALOGSELECT(ID)
 I=WINFOERROR(1)
 IF(ISHOW.EQ.0)THEN
  CALL WDIALOGHIDE()
 ELSE
  IX=WINFODIALOG(DIALOGXPOS)
  IY=WINFODIALOG(DIALOGYPOS)
  CALL WDIALOGSHOW(IX,IY,0,ISHOW)
 ENDIF

 END SUBROUTINE UTL_HIDESHOWDIALOG

 !###======================================================================
 SUBROUTINE UTL_IDFGETLAYERS(IDFNAME,N,ILAY)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 CHARACTER(LEN=*),DIMENSION(N) :: IDFNAME
 INTEGER :: I,J,K,IL,IOS
 INTEGER,DIMENSION(:) :: ILAY

 ILAY=0
 DO I=1,N
  J=INDEXNOCASE(IDFNAME(I),'_L',.TRUE.)
  IF(J.NE.0)THEN
   K=INDEXNOCASE(IDFNAME(I),'.IDF',.TRUE.)
   IF(K.NE.0)THEN
    J=J+2
    K=K-1
    READ(IDFNAME(I)(J:K),*,IOSTAT=IOS) IL
    IF(IOS.EQ.0.AND.IL.GT.0)ILAY(IL)=1
   ENDIF
  ENDIF
 END DO

 END SUBROUTINE UTL_IDFGETLAYERS

 !###======================================================================
 SUBROUTINE UTL_IDFGETDATES(IDFNAME,N,M,O,MINDATE,MAXDATE,ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,ID
 INTEGER(KIND=8),INTENT(OUT) :: MINDATE,MAXDATE
 INTEGER,INTENT(OUT) :: M,O
 CHARACTER(LEN=*),DIMENSION(N) :: IDFNAME
 INTEGER :: I,IDATE,IYR,IMH,IDY,IHR,IMT,ISC
 INTEGER(KIND=8) :: DIDATE
 REAL(KIND=DP_KIND) :: DAYFRACTION

 MINDATE=21000101000000
 MAXDATE=19000101000000
 M      =0
 O      =0
 DO I=1,N
  IDATE=UTL_IDFGETDATE(IDFNAME(I),DAYFRACTION,IYR,IMH,IDY,IHR,IMT,ISC)
  IF(IDATE.NE.0)THEN
   O=O+1
   DIDATE=YMDHMSTOITIME(IYR,IMH,IDY,IHR,IMT,ISC)
   MINDATE=MIN(MINDATE,DIDATE)
   MAXDATE=MAX(MAXDATE,DIDATE)
  ELSE
   IF(INDEX(UTL_CAP(IDFNAME(I),'U'),'_STEADY-STATE_').NE.0)M=M+1
  ENDIF

  IF(ID.NE.0)CALL WDIALOGPUTPROGRESSBAR(ID,1,1)

 END DO

 END SUBROUTINE UTL_IDFGETDATES

 !###======================================================================
 INTEGER FUNCTION UTL_IDFGETDATE(IDFNAME,DAYFRACTION,IYR,IMH,IDY,IHR,IMT,ISC,IDATEFULL)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 REAL(KIND=DP_KIND),INTENT(OUT),OPTIONAL :: DAYFRACTION
 INTEGER,INTENT(OUT),OPTIONAL :: IDY,IMH,IYR,IHR,IMT,ISC
 INTEGER(KIND=DP_KIND),INTENT(OUT),OPTIONAL :: IDATEFULL
 INTEGER :: IOS
 INTEGER :: I,II,J,N,YR,MT,DY,HR,MN,SC
 INTEGER,DIMENSION(2) :: NI
 DATA NI/14,8/

 !## initially no data
 UTL_IDFGETDATE=0

 !## try to find 16 numbers after eachother ...
 !## try to find 8 numbers after eachother ...
 DO II=1,2
  N=0
  !## start after last "\"-symbol
  DO I=INDEX(IDFNAME,'\',.TRUE.)+1,LEN_TRIM(IDFNAME)
   !## part of a number
   SELECT CASE (ICHAR(IDFNAME(I:I)))
    CASE (48:57)
     !## count numbers
     N=N+1
     !## stop if total number is 8 or 14
     IF(N.EQ.NI(II))EXIT
     !## mark first position
     IF(N.EQ.1)J=I
    CASE DEFAULT
     N=0
   END SELECT
  END DO
  IF(N.EQ.NI(II))EXIT
  !## nothing found
  IF(II.EQ.2.AND.N.LT.NI(II))RETURN
 ENDDO

 !## default
 IF(PRESENT(DAYFRACTION))DAYFRACTION=-1.0D0

 IF(II.EQ.1)THEN
  IF(PRESENT(IDATEFULL))READ(IDFNAME(J:J+13),*) IDATEFULL
  READ(IDFNAME(J:)  ,'(I8) ',IOSTAT=IOS) UTL_IDFGETDATE
  IF(PRESENT(DAYFRACTION))THEN
   READ(IDFNAME(J+8:),'(3I2)',IOSTAT=IOS) HR,MN,SC
   DAYFRACTION=REAL(HR*3600+MN*60+SC)/86400.0D0
   DAYFRACTION=MAX(0.0D0,MIN(DAYFRACTION,1.0D0))
  ENDIF
  READ(IDFNAME(J:)  ,'(I4,5I2)',IOSTAT=IOS) YR,MT,DY,HR,MN,SC
 ELSE
  IF(PRESENT(IDATEFULL))THEN
   READ(IDFNAME(J:J+7),*) IDATEFULL; IDATEFULL=IDATEFULL*1000000
  ENDIF
  READ(IDFNAME(J:)  ,'(I8)',IOSTAT=IOS) UTL_IDFGETDATE
  READ(IDFNAME(J:)  ,'(I4,2I2)',IOSTAT=IOS) YR,MT,DY
  HR=0; MN=0; SC=0
 ENDIF

 IF(PRESENT(IYR))IYR=YR
 IF(PRESENT(IMH))IMH=MT
 IF(PRESENT(IDY))IDY=DY
 IF(PRESENT(IHR))IHR=HR
 IF(PRESENT(IMT))IMT=MN
 IF(PRESENT(ISC))ISC=SC

 IF(IOS.NE.0)UTL_IDFGETDATE=0

 END FUNCTION UTL_IDFGETDATE

 !###======================================================================
 SUBROUTINE UTL_FILLDATES(IDY,IDM,IDD,JULD)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT),OPTIONAL :: JULD
 INTEGER,INTENT(IN) :: IDM,IDY,IDD
 INTEGER :: M,Y,D,NDAY

 CALL WDIALOGGETMENU(IDM,M)
 CALL WDIALOGGETINTEGER(IDY,Y)
 NDAY=WDATEDAYSINMONTH(Y,M)
 CALL WDIALOGGETINTEGER(IDD,D)
 CALL WDIALOGRANGEINTEGER(IDD,1,NDAY)
 IF(D.GT.NDAY)CALL WDIALOGPUTINTEGER(IDD,NDAY)
 D=MIN(D,NDAY)

 IF(.NOT.PRESENT(JULD))RETURN
 JULD=JD(Y,M,D)

 END SUBROUTINE UTL_FILLDATES

 !###======================================================================
 SUBROUTINE UTL_FILLDATESDIALOG(ID,IDD,IDM,IDY,JD)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID,IDD,IDM,IDY,JD
 INTEGER :: I,J,K

 CALL WDIALOGSELECT(ID)
 !## put begin date
 CALL IDATETOGDATE(JD,I,J,K)  !## id,iy,im,id
 CALL WDIALOGPUTINTEGER(IDD,K)
 CALL WDIALOGPUTINTEGER(IDY,I)
 CALL WDIALOGPUTOPTION(IDM,J)

 END SUBROUTINE UTL_FILLDATESDIALOG

 !###======================================================================
 FUNCTION ITOS(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I
 CHARACTER(LEN=10)  :: TXT,ITOS

 WRITE(TXT,'(I10)') I
 ITOS=ADJUSTL(TXT)

 END FUNCTION ITOS
 
 !###======================================================================
 FUNCTION LTOS(LEX,OC)
 !###======================================================================
 IMPLICIT NONE
 LOGICAL,INTENT(IN) :: LEX
 INTEGER,INTENT(IN) :: OC ! output control: 1=single, 2= full
 CHARACTER(LEN=10)  :: TXT,LTOS

 ! Function: Logical 2 string
 
 IF(LEX) THEN ; WRITE(TXT,'(A)') 'TRUE' ; 
         ELSE ; WRITE(TXT,'(A)') 'FALSE' ; ENDIF         
 IF(OC.EQ.1) TXT=TXT(:1)
 LTOS=ADJUSTL(TXT)

 END FUNCTION LTOS

 !###======================================================================
 FUNCTION ITOS_DBL(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER(KIND=DP_KIND),INTENT(IN) :: I
 CHARACTER(LEN=16) :: TXT,ITOS_DBL

 WRITE(TXT,'(I16)') I
 ITOS_DBL=ADJUSTL(TXT)

 END FUNCTION ITOS_DBL

 !###======================================================================
 CHARACTER(LEN=24) FUNCTION RTOS(X,F,NDEC)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NDEC
 REAL(KIND=DP_KIND),INTENT(IN) :: X
 CHARACTER(LEN=1),INTENT(IN) :: F
 CHARACTER(LEN=24) :: TXT,FRM
 INTEGER :: IOS

 IF(F.EQ.'*')THEN
  WRITE(TXT,*,IOSTAT=IOS) X
 ELSE
  WRITE(FRM,'(2A1,I2.2,A1,I2.2,A1)') '(',F,LEN(RTOS),'.',NDEC,')'
  WRITE(TXT,FRM,IOSTAT=IOS) X
 ENDIF

 IF(IOS.NE.0)TXT='error'

 RTOS=ADJUSTL(TXT)

 END FUNCTION RTOS

 !###======================================================================
 INTEGER FUNCTION UTL_GETUNIT()
 !###======================================================================
 IMPLICIT NONE
 LOGICAL :: LEX

 UTL_GETUNIT=19
 DO
  UTL_GETUNIT=UTL_GETUNIT+1
  INQUIRE(UNIT=UTL_GETUNIT,OPENED=LEX)
  IF(.NOT.LEX)EXIT
  IF(UTL_GETUNIT.GT.MAXUNITS)EXIT
 END DO

 IF(LEX)THEN
  CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'iMOD cannot open more than '//TRIM(ITOS(MAXUNITS))//' files simultaneously!','ERROR')
  UTL_GETUNIT=0
 ENDIF

 END FUNCTION UTL_GETUNIT

 !###======================================================================
 SUBROUTINE UTL_CLOSEUNITS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J
 LOGICAL :: LEX

 DO I=20,MAXUNITS
  J=I
  INQUIRE(UNIT=J,OPENED=LEX)
  IF(LEX.AND.J.GT.0)CLOSE(J)
 END DO

 END SUBROUTINE UTL_CLOSEUNITS

 !###======================================================================
 SUBROUTINE UTL_LISTOPENFILES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER,ALLOCATABLE,DIMENSION(:) :: UNITS,ICHECK
 CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: FNAME
 INTEGER :: NOPEN,ITYPE,I,MXNROW
 TYPE(WIN_MESSAGE) :: MESSAGE

 !## get list of open unit numbers
 ALLOCATE(FNAME(100),UNITS(100),ICHECK(100))
 CALL INFOUNITS(FNAME,UNITS,SIZE(FNAME),NOPEN)
 IF(NOPEN.EQ.0)THEN
  DEALLOCATE(FNAME,UNITS,ICHECK)
  CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'No files currently opened.','Information')
  RETURN
 ENDIF
 ICHECK=1

 CALL WDIALOGLOAD(ID_DFILESOPEN,ID_DFILESOPEN)
 MXNROW=WINFOGRID(IDF_GRID1,GRIDROWSMAX); NOPEN=MIN(NOPEN,MXNROW)
 
 CALL WGRIDROWS(IDF_GRID1,NOPEN)
 CALL WGRIDPUTCHECKBOX(IDF_GRID1,1,ICHECK,NOPEN)
 CALL WGRIDPUTINTEGER(IDF_GRID1,2,UNITS,NOPEN)
 CALL WGRIDPUTSTRING(IDF_GRID1,3,FNAME,NOPEN)

 CALL WDIALOGSHOW(-1,-1,0,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (ID_SELECTALL)
     CASE (ID_DESELECTALL)
     CASE (IDOK)
      CALL WGRIDGETINTEGER(IDF_GRID1,1,ICHECK,NOPEN)
      DO I=1,SIZE(ICHECK); IF(ICHECK(I).EQ.1)CLOSE(UNITS(I)); ENDDO
      !## check again
      CALL INFOUNITS(FNAME,UNITS,SIZE(FNAME),NOPEN)
      IF(NOPEN.EQ.0)EXIT
      NOPEN=MIN(NOPEN,MXNROW)
      ICHECK=1
      CALL WGRIDROWS(IDF_GRID1,NOPEN)
      CALL WGRIDPUTCHECKBOX(IDF_GRID1,1,ICHECK,NOPEN)
      CALL WGRIDPUTINTEGER(IDF_GRID1,2,UNITS,NOPEN)
      CALL WGRIDPUTSTRING(IDF_GRID1,3,FNAME,NOPEN)
     CASE (IDHELP)
     CASE (IDCANCEL)
      EXIT
    END SELECT
   CASE (FIELDCHANGED)
  END SELECT
 ENDDO

 DEALLOCATE(FNAME,UNITS,ICHECK)
 CALL WDIALOGUNLOAD()

 END SUBROUTINE UTL_LISTOPENFILES

!###======================================================================
 SUBROUTINE INFOUNITS(FNAME,UNITS,N,NOPEN)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER,INTENT(OUT) :: NOPEN
 INTEGER,DIMENSION(N),INTENT(INOUT) :: UNITS
 CHARACTER(LEN=*),DIMENSION(N),INTENT(INOUT) :: FNAME
 INTEGER :: I,J
 LOGICAL :: LEX
 CHARACTER(LEN=256) :: FN

 NOPEN=0; DO I=20,MAXUNITS
  J=I
  INQUIRE(UNIT=J,OPENED=LEX)
  IF(LEX.AND.J.GT.0)THEN
   INQUIRE(UNIT=J,NAME=FN)
   NOPEN=NOPEN+1
   !## stop getting open files
   IF(NOPEN.GT.SIZE(FNAME))EXIT
   FNAME(NOPEN)=FN
   UNITS(NOPEN)=J
  ENDIF
 END DO

 END SUBROUTINE INFOUNITS

 !###======================================================================
 SUBROUTINE UTL_CREATEDIR(DIRNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIRNAME
 INTEGER :: I,J

 !## create/check entire directory-structure
 I=INDEX(DIRNAME,'\')+1
 DO
  J=INDEX(DIRNAME(I:),'\')
  IF(J.EQ.0)EXIT
  J=J+I
  IF(.NOT.IOSDIREXISTS(DIRNAME(:J-2)))CALL IOSDIRMAKE(DIRNAME(:J-2))
  I=J
 END DO

 !## only create folder, is there is a subfolder left
 IF(INDEX(DIRNAME,'\').NE.0)THEN
  !## last remaining of string
  IF(.NOT.IOSDIREXISTS(TRIM(DIRNAME)))CALL IOSDIRMAKE(TRIM(DIRNAME))
 ENDIF

 END SUBROUTINE UTL_CREATEDIR

 !###======================================================================
 LOGICAL FUNCTION UTL_DEL1TREE(DIR)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER :: I,IERROR
 CHARACTER(LEN=256) :: CURDIR,DELDIR

 UTL_DEL1TREE=.FALSE.

 CALL IOSDIRNAME(CURDIR)
 I=INDEXNOCASE(DIR,'\',.TRUE.)
 !## clear existing error?
 IERROR=INFOERROR(1)
 CALL IOSDIRCHANGE(DIR(:I-1))
 IERROR=INFOERROR(1)
 !## dirchange error?
 IF(IERROR.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Could not find directory:'//CHAR(13)// &
               TRIM(DIR(:I-1)),'iMOD: Error')
  RETURN
 ENDIF
 !## make sure to delete directory
 CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to delete'//CHAR(13)//TRIM(DIR),'Question?')
 IF(WINFODIALOG(4).NE.1)RETURN
 !## delete entire directory
 DELDIR=DIR(I+1:)
 CALL UTL_DEL2TREE(DELDIR)
 CALL IOSDIRCHANGE(CURDIR)

 UTL_DEL1TREE=.TRUE.

 END FUNCTION UTL_DEL1TREE

 !###======================================================================
 RECURSIVE SUBROUTINE UTL_DEL2TREE(DIR)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=256),INTENT(IN)       :: DIR
 INTEGER,PARAMETER                   :: MXDIR=50
 INTEGER                             :: I,NDIR,IERROR
 CHARACTER(LEN=256),DIMENSION(MXDIR) :: RESDIR

 CALL WINDOWOUTSTATUSBAR(4,'Delete directory '//TRIM(DIR)//'...')

 !## clear existing error?
 IERROR=INFOERROR(1)
 !## go one level down
 CALL IOSDIRCHANGE(DIR)
 IERROR=INFOERROR(1)
 !## dirchange error?
 IF(IERROR.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Could not change towards directory:'//CHAR(13)// &
               TRIM(DIR),'iMOD: Error')
  RETURN
 ENDIF

 !## how many subdirectories exist?
 NDIR=MXDIR
 CALL IOSDIRENTRYTYPE('D')
 CALL IOSDIRINFO(' ',' ',RESDIR,NDIR)
 IF(NDIR.GT.MXDIR)THEN
  CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'MXDIR overwritten in del2tree()','ERROR')
  RETURN
 ENDIF
 DO I=3,NDIR
  CALL UTL_DEL2TREE(RESDIR(I))
 END DO
 !## delete all files in directory
 CALL IOSDELETEFILE('*.*')
 !## return one level up
 CALL IOSDIRCHANGE('..')
 CALL IOSDIRDELETE(DIR)

 END SUBROUTINE UTL_DEL2TREE

 !###======================================================================
 SUBROUTINE UTL_WAITMESSAGE(IRAT,IRAT1,I1,I2,WAITTXT,IBOX)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I1,I2
 INTEGER,INTENT(INOUT) :: IRAT,IRAT1
 CHARACTER(LEN=*),INTENT(IN) :: WAITTXT
 INTEGER,OPTIONAL,INTENT(IN) :: IBOX
 INTEGER :: JBOX

 JBOX=4; IF(PRESENT(IBOX))JBOX=IBOX

 IRAT=(I1*100)/I2
 IF(IRAT.NE.IRAT1)THEN
  CALL WINDOWOUTSTATUSBAR(JBOX,TRIM(WAITTXT)//' '//TRIM(ITOS(IRAT))//' %')
  IRAT1=IRAT
 ENDIF

 END SUBROUTINE UTL_WAITMESSAGE

 !###======================================================================
 SUBROUTINE UTL_MESSAGEHANDLE(ONOFF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ONOFF
 INTEGER :: I

 IF(ONOFF.EQ.0)CALL WCURSORSHAPE(CURHOURGLASS)
 IF(ONOFF.EQ.1)CALL WCURSORSHAPE(CURARROW)
 DO I=1,MXMESSAGE
  IF(IMESSAGE(I).EQ.1)THEN
   IF(WINFOMESSAGE(I).NE.ONOFF)CALL WMESSAGEENABLE(I,ONOFF)
  ENDIF
 END DO
 IF(ONOFF.EQ.0)RETURN

 END SUBROUTINE UTL_MESSAGEHANDLE

 !###======================================================================
 SUBROUTINE UTL_MESSAGEHANDLE3D(ONOFF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ONOFF

 !## default mousemove off
 IMESSAGE(MOUSEMOVE)=ONOFF

 END SUBROUTINE UTL_MESSAGEHANDLE3D

 !###======================================================================
 SUBROUTINE UTL_GETDAYANDMONTHFROMDAYNUMBER(DN,IY,ID,IM)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DN,IY
 INTEGER,INTENT(OUT) :: IM,ID
 INTEGER :: TD
 
 TD=0; DO IM=1,12
  TD=TD+WDATEDAYSINMONTH(IY,IM)
  IF(DN.LE.TD)EXIT 
 ENDDO
 ID=WDATEDAYSINMONTH(IY,IM)-(TD-DN)

 END SUBROUTINE UTL_GETDAYANDMONTHFROMDAYNUMBER

 !###======================================================================
 INTEGER FUNCTION UTL_GETDAYNUMBERFROMJD(JDATE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: JDATE
 INTEGER :: ID,IM,IY,JD1
 
 CALL UTL_GDATE(JDATE,IY,IM,ID); JD1=JD(IY,1,1)
 UTL_GETDAYNUMBERFROMJD=(JDATE-JD1)+1

 END FUNCTION UTL_GETDAYNUMBERFROMJD

 !###======================================================================
 INTEGER FUNCTION UTL_GETCURRENTDATE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IY,IM,ID

 CALL IOSDATE(IY,IM,ID)
 UTL_GETCURRENTDATE=IY*10000+IM*100+ID

 END FUNCTION UTL_GETCURRENTDATE

 !###======================================================================
 CHARACTER(LEN=8) FUNCTION UTL_GETCURRENTTIME()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IH,IM,IS
 CHARACTER(LEN=8) :: CTIME

 CALL IOSTIME(IH,IM,IS)
 WRITE(CTIME,'(3(I2.2,A1))') IH,':',IM,':',IS

 UTL_GETCURRENTTIME=TRIM(CTIME)

 END FUNCTION UTL_GETCURRENTTIME

 !###======================================================================
 FUNCTION JDATETOGDATE(I,DTYPE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I
 INTEGER,INTENT(IN),OPTIONAL :: DTYPE
 CHARACTER(LEN=10)  :: JDATETOGDATE
 INTEGER :: IY,IM,ID

 CALL UTL_GDATE(I,IY,IM,ID)
 IF(PRESENT(DTYPE))THEN
  SELECT CASE (DTYPE)
   CASE (0)
    JDATETOGDATE=TRIM(ITOS(ID))//'-'//TRIM(ITOS(IM))//'-'//TRIM(ITOS(IY))
   CASE (1)
    JDATETOGDATE=TRIM(ITOS(ID))//'/'//TRIM(ITOS(IM))//'/'//TRIM(ITOS(IY))
   CASE (2)
    WRITE(JDATETOGDATE,'(I4.4,2I2.2)') IY,IM,ID
  END SELECT
 ELSE
  JDATETOGDATE=TRIM(ITOS(ID))//'/'//TRIM(ITOS(IM))//'/'//TRIM(ITOS(IY))
 ENDIF

 END FUNCTION JDATETOGDATE

 !###======================================================================
 CHARACTER(LEN=20) FUNCTION JDATETOFDATE(X,JOFFSET,DTYPE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: JOFFSET
 INTEGER,INTENT(IN),OPTIONAL :: DTYPE
 REAL(KIND=DP_KIND),INTENT(IN) :: X
 CHARACTER(LEN=8) :: CTIME
 REAL(KIND=DP_KIND) :: FTIME
 INTEGER :: DDTYPE

 IF(PRESENT(DTYPE))THEN
  DDTYPE=DTYPE
 ELSE
  DDTYPE=0
 ENDIF

 JDATETOFDATE=JDATETOGDATE(INT(X)+JOFFSET,DTYPE)
 FTIME=X-FLOOR(X)
 CALL FTIMETOCTIME(FTIME,CTIME,DDTYPE)
 IF(CTIME.NE.'00:00:00')THEN
  IF(DDTYPE.EQ.2)THEN
   JDATETOFDATE=TRIM(JDATETOFDATE)//TRIM(CTIME)
  ELSE
   JDATETOFDATE=TRIM(JDATETOFDATE)//' '//TRIM(CTIME)
  ENDIF
 ENDIF

 END FUNCTION JDATETOFDATE

 !###======================================================================
 INTEGER FUNCTION GDATETOJDATE(CDATE)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: CDATE
 INTEGER :: IY,IM,ID,I,J,MD
 INTEGER,DIMENSION(3) :: IOS

 IOS=1

 I=INDEX(CDATE,'/',.FALSE.)
 IF(I.GT.0)THEN
  READ(CDATE(1:I-1),*,IOSTAT=IOS(1)) ID
  J=INDEX(CDATE,'/',.TRUE.)
  IF(J.GT.0)THEN
   READ(CDATE(J+1:),*,IOSTAT=IOS(3)) IY
   IF(J-I.GT.0)READ(CDATE(I+1:J-1),*,IOSTAT=IOS(2)) IM
  ENDIF
 ENDIF

 !## initialize default value
 GDATETOJDATE=0

 IM=MAX(1,MIN(12,IM))
 MD=WDATEDAYSINMONTH(IY,IM)
 ID=MAX(1,MIN(MD,ID))

 !## error reading dates
 IF(SUM(IOS).NE.0)RETURN

 J           =JD(IY,IM,ID)
 CDATE       =JDATETOGDATE(J)
 GDATETOJDATE=J

 END FUNCTION GDATETOJDATE

 !###====================================================================
 INTEGER FUNCTION UTL_IDATETOJDATE(IDATE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDATE
 INTEGER :: IY,IM,ID

 CALL IDATETOGDATE(IDATE,IY,IM,ID)
 UTL_IDATETOJDATE=JD(IY,IM,ID)

 END FUNCTION UTL_IDATETOJDATE

 !###====================================================================
 INTEGER FUNCTION UTL_JDATETOIDATE(JDATE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: JDATE
 INTEGER :: IY,IM,ID

 CALL UTL_GDATE(JDATE,IY,IM,ID)
 UTL_JDATETOIDATE=IY*10000+IM*100+ID

 END FUNCTION UTL_JDATETOIDATE

 !###====================================================================
 SUBROUTINE IDATETOGDATE(IDATE,IY,IM,ID)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDATE
 INTEGER,INTENT(OUT) :: IY,IM,ID

 IY =      IDATE          / 10000
 IM = MOD( IDATE, 10000 ) / 100
 ID = MOD( IDATE, 100 )

 END SUBROUTINE IDATETOGDATE

 !###====================================================================
 SUBROUTINE FTIMETOITIME(FTIME,IH,IM,IS)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: FTIME
 INTEGER,INTENT(OUT) :: IH,IM,IS
 INTEGER :: ITIME

 ITIME=FTIME*SDAY
 CALL ITIMETOGTIME(ITIME,IH,IM,IS)

 END SUBROUTINE FTIMETOITIME

 !###====================================================================
 SUBROUTINE FTIMETOCTIME(FTIME,CTIME,DTYPE)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: FTIME
 CHARACTER(LEN=*),INTENT(OUT) :: CTIME
 INTEGER,INTENT(IN),OPTIONAL :: DTYPE
 INTEGER :: IH,IM,IS
 INTEGER :: ITIME

 ITIME=FTIME*SDAY
 CALL ITIMETOGTIME(ITIME,IH,IM,IS)
 IF(PRESENT(DTYPE))THEN
  SELECT CASE (DTYPE)
   CASE (0)
    WRITE(CTIME,'(3(I2.2,A1))') IH,':',IM,':',IS
   CASE (1)
    WRITE(CTIME,'(3(I2.2,A1))') IH,'-',IM,'-',IS
   CASE (2)
    WRITE(CTIME,'(3I2.2)') IH,IM,IS
  END SELECT
 ELSE
  WRITE(CTIME,'(3(I2.2,A1))') IH,':',IM,':',IS
 ENDIF

 END SUBROUTINE FTIMETOCTIME

 !###====================================================================
 SUBROUTINE ITIMETOCDATE(IDATE,CDATE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER(KIND=8),INTENT(IN) :: IDATE
 CHARACTER(LEN=52) :: CDATE
 INTEGER :: IYR,IMH,IDY,IHR,IMT,ISC

 CALL ITIMETOGDATE(IDATE,IYR,IMH,IDY,IHR,IMT,ISC)
 IF(IHR.EQ.0.AND.IMT.EQ.0.AND.ISC.EQ.0)THEN
  WRITE(CDATE,'(I4.4,2(A1,I2.2))') IYR,'/',IMH,'/',IDY
 ELSE
  WRITE(CDATE,'(I4.4,5(A1,I2.2))') IYR,'/',IMH,'/',IDY,' ',IHR,':',IMT,':',ISC
 ENDIF

 END SUBROUTINE ITIMETOCDATE

 !###====================================================================
 REAL(KIND=DP_KIND) FUNCTION ITIMETOFTIME(ITIME)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITIME !## hhmmss notation
 INTEGER :: IH,IM,IS

 CALL ITIMETOHMS(ITIME,IH,IM,IS)
 ITIMETOFTIME=(REAL(IH)*3600.0D0+REAL(IM)*60.0D0+REAL(IS))/SDAY

 END FUNCTION ITIMETOFTIME

 !###====================================================================
 SUBROUTINE ITIMETOHMS(ITIME,IH,IM,IS)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITIME !## hhmmss notation
 INTEGER,INTENT(OUT) :: IH,IM,IS

 IH =      ITIME          / 10000
 IM = MOD( ITIME, 10000 ) / 100
 IS = MOD( ITIME, 100 )

 END SUBROUTINE ITIMETOHMS

 !###====================================================================
 INTEGER FUNCTION HMSTOITIME(IH,IM,IS)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IH,IM,IS

 HMSTOITIME=IH*10000+IM*100+IS

 END FUNCTION HMSTOITIME

 !###====================================================================
 INTEGER(KIND=8) FUNCTION YMDHMSTOITIME(IY,IM,ID,IH,IT,IS)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IY,IM,ID,IH,IT,IS
 INTEGER(KIND=8) :: IYD,IMD,IDD,IHD,ITD,ISD

 IYD=IY
 IMD=IM
 IDD=ID
 IHD=IH
 ITD=IT
 ISD=IS
 YMDHMSTOITIME=IYD*10000000000+ &
               IMD*100000000+   &
               IDD*1000000+     &
               IHD*10000+       &
               ITD*100+         &
               ISD

 END FUNCTION YMDHMSTOITIME

 !###====================================================================
 SUBROUTINE ITIMETOGDATE(IDATE,IYR,IMH,IDY,IHR,IMT,ISC)
 !###====================================================================
 IMPLICIT NONE
 INTEGER(KIND=8),INTENT(IN) :: IDATE
 INTEGER,INTENT(OUT) :: IYR,IMH,IDY,IHR,IMT,ISC

 IYR =      IDATE                / 10000000000
 IMH = MOD( IDATE, 10000000000 ) / 100000000
 IDY = MOD( IDATE, 100000000 )   / 1000000
 IHR = MOD( IDATE, 1000000 )     / 10000
 IMT = MOD( IDATE, 10000 )       / 100
 ISC = MOD( IDATE, 100 )

 END SUBROUTINE ITIMETOGDATE

 !###====================================================================
 INTEGER FUNCTION UTL_DIFFDATE(SDATE,EDATE,DDATE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: SDATE,EDATE,DDATE
 INTEGER,DIMENSION(2) :: IY,IM,ID
 INTEGER :: SD,ED,N
 
 SD=UTL_IDATETOJDATE(SDATE); ED=UTL_IDATETOJDATE(EDATE)
 CALL IDATETOGDATE(SDATE,IY(1),IM(1),ID(1))
 CALL IDATETOGDATE(EDATE,IY(2),IM(2),ID(2))

 SELECT CASE (DDATE)
  !## daily
  CASE (1)
   UTL_DIFFDATE=(ED-SD)+1
  !## weekly
  CASE (2)
   N=((ED-SD)+1)
   UTL_DIFFDATE=N/7
   IF(MOD(N,7).NE.0)UTL_DIFFDATE=UTL_DIFFDATE+1
  !## month
  CASE (3)
   IF(IY(2).GT.IY(1))THEN
    N=(12-IM(1))+1
    N=N+(((IY(2)-IY(1))-1)*12)
    N=N+IM(2)
    UTL_DIFFDATE=N
   ELSE
    UTL_DIFFDATE=(IM(2)-IM(1))+1
   ENDIF
  CASE (4)
   UTL_DIFFDATE=(IY(2)-IY(1))+1
 END SELECT
 
 END FUNCTION UTL_DIFFDATE

  !###====================================================================
 REAL(KIND=DP_KIND) FUNCTION DIFFTIME(SDATE,EDATE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER(KIND=8),INTENT(IN) :: SDATE,EDATE
 INTEGER :: IYR1,IMH1,IDY1,IHR1,IMT1,ISC1, &
            IYR2,IMH2,IDY2,IHR2,IMT2,ISC2
 INTEGER :: SD,ED,DD
 REAL(KIND=DP_KIND) :: F1,F2,F

 SD=SDATE/1000000; ED=EDATE/1000000
 SD=UTL_IDATETOJDATE(SD); ED=UTL_IDATETOJDATE(ED)
 DD=ED-SD

 !## start time
 CALL ITIMETOGDATE(SDATE,IYR1,IMH1,IDY1,IHR1,IMT1,ISC1)
 F1=(REAL(IHR1)*3600.0D0+REAL(IMT1)*60.0D0+REAL(ISC1))/SDAY

 !## end   time
 CALL ITIMETOGDATE(EDATE,IYR2,IMH2,IDY2,IHR2,IMT2,ISC2)
 F2=(REAL(IHR2)*3600.0D0+REAL(IMT2)*60.0D0+REAL(ISC2))/SDAY

 !## same day
 IF(SD.EQ.ED)THEN
  F=F2-F1
 ELSE
  F=1.0D0-F1+F2+(DD-1)
 ENDIF

! IF(ED.GT.SD)THEN; F1=1.0D0-F1; DD=DD-1; ENDIF
! IF(ED.EQ.SD)THEN; F1=F2-F1; F2=0.0D0; ENDIF

 DIFFTIME=F !DD+(F1+F2)

 END FUNCTION DIFFTIME
 
 !###====================================================================
 REAL(KIND=DP_KIND) FUNCTION CTIMETOFTIME(CTIME)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*) :: CTIME
 INTEGER :: IH,IM,IS

 READ(CTIME,'(3(I2.0,1X))') IH,IM,IS
 CTIMETOFTIME=(REAL(IH)*3600.0D0+REAL(IM)*60.0D0+REAL(IS))/SDAY

 END FUNCTION CTIMETOFTIME

 !###====================================================================
 SUBROUTINE DECDEGREES_TO_DMS(DEGREES,D,M,S)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: DEGREES
 REAL(KIND=DP_KIND),INTENT(OUT) :: D,M,S
 REAL(KIND=DP_KIND) :: F

 D = INT(DEGREES)
 F = 60.0D0 * (DEGREES - D)
 M = INT(F)
 S = F - M

 END SUBROUTINE DECDEGREES_TO_DMS

 !###====================================================================
 SUBROUTINE ITIMETOGTIME(ITIME,IH,IM,IS)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITIME !## time seconds
 INTEGER,INTENT(OUT) :: IH,IM,IS

 IH =      ITIME         / 3600
 IM = MOD( ITIME, 3600 ) / 60
 IS = MOD( ITIME, 60 )

 END SUBROUTINE ITIMETOGTIME

 !###====================================================================
 INTEGER FUNCTION JD(YEAR,MONTH,DAY)
 !###====================================================================
 !Reference: Fliegel, H. F. & van Flandern, T. C. 1968, Communications of the ACM, 11, 657.
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: YEAR,MONTH,DAY
 INTEGER            :: I,J,K

 I =YEAR
 J =MONTH
 K =DAY
 JD=K-32075+1461*(I+4800+(J-14)/12)/4+367*(J-2-(J-14)/12*12) &
    /12-3*((I+4900+(J-14)/12)/100)/4

 END FUNCTION JD

 !###====================================================================
 SUBROUTINE UTL_GDATE(JD,YEAR,MONTH,DAY)
 !###====================================================================
 !Reference: Fliegel, H. F. & van Flandern, T. C. 1968, Communications of the ACM, 11, 657.
 IMPLICIT NONE
 INTEGER,INTENT(IN)  :: JD
 INTEGER,INTENT(OUT) :: YEAR,MONTH,DAY
 INTEGER :: I,J,K,L,N

 L=JD+68569
 N=4*L/146097
 L=L-(146097*N+3)/4
 I=4000*(L+1)/1461001
 L=L-1461*I/4+31
 J=80*L/2447
 K=L-2447*J/80
 L=J/11
 J=J+2-12*L
 I=100*(N-49)+I+L

 YEAR =I
 MONTH=J
 DAY  =K

 END SUBROUTINE UTL_GDATE

 !###====================================================================
 FUNCTION UTL_SUBST(FNAME,SUB1,SUB2,IERROR)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: SUB1,SUB2
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER,INTENT(OUT),OPTIONAL :: IERROR
 INTEGER :: I,J
 CHARACTER(LEN=256) :: UTL_SUBST

 UTL_SUBST=FNAME

 IF(PRESENT(IERROR))THEN
  IF(INDEX(TRIM(UTL_CAP(FNAME,'U')),TRIM(REPLACESTRING)).GT.0.AND.LEN_TRIM(SUB2).EQ.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'File: '//TRIM(FNAME)//CHAR(13)//'Cannot replace $DBASE$ as DBASE is undefined in the PRF-file','Error')
   IERROR=1; RETURN
  ENDIF
 ENDIF
 
 I=INDEX(UTL_CAP(FNAME,'U'),TRIM(UTL_CAP(SUB1,'U')))
 IF(I.EQ.0)RETURN
 I=I-1
 J=I+LEN(SUB1)+1

 UTL_SUBST=FNAME(:I)//TRIM(SUB2)//FNAME(J:)

 END FUNCTION UTL_SUBST

 !###====================================================
 SUBROUTINE UTL_CHECKNAME(FNAME,EXT)
 !checks for existence of an extension EXT (for instance 'idf')
 !and replaces fname including the extension (EXT) if not found.
 !###====================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: FNAME
 CHARACTER(LEN=*),INTENT(IN) :: EXT
 INTEGER :: I

 I=INDEXNOCASE(FNAME,'.',.TRUE.)
 IF(I.EQ.0)THEN
  FNAME=TRIM(FNAME)//'.'//TRIM(EXT)
 ELSE
  FNAME=FNAME(:I)//TRIM(EXT)
 ENDIF

 END SUBROUTINE UTL_CHECKNAME

 !###======================================================================
 INTEGER FUNCTION UTL_INVERSECOLOUR(IRGB)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IRGB
 INTEGER :: IR,IG,IB

 CALL WRGBSPLIT(IRGB,IR,IG,IB)
 UTL_INVERSECOLOUR=WRGB(255-IR,255-IG,255-IB)

 END FUNCTION UTL_INVERSECOLOUR

 !###======================================================================
 SUBROUTINE UTL_FADEOUTCOLOUR(ICLR,FCT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: ICLR
 REAL(KIND=DP_KIND),INTENT(IN) :: FCT
 INTEGER :: IR,IG,IB

 IF(FCT.GT.1.0D0.OR.FCT.LE.0.0D0)RETURN

 CALL WRGBSPLIT(ICLR,IR,IG,IB)
 IR =IR+((255-IR)*FCT)
 IG =IG+((255-IG)*FCT)
 IB =IB+((255-IB)*FCT)
 IR=MIN(255,MAX(0,IR))
 IG=MIN(255,MAX(0,IG))
 IB=MIN(255,MAX(0,IB))
 !## faded colour becomes
 ICLR=WRGB(IR,IG,IB)

 END SUBROUTINE UTL_FADEOUTCOLOUR

 !###======================================================================
 SUBROUTINE UTL_EQUALNAMES_UNITTEST()
 !###======================================================================
 IMPLICIT NONE

 write(*,*) '*'   ,' TEST',0,UTL_EQUALNAMES('*','TEST',0)
 write(*,*) 'T*'  ,' TEST',0,UTL_EQUALNAMES('T*','TEST',0)
 write(*,*) 't*'  ,' TEST',1,UTL_EQUALNAMES('t*','TEST',1)
 write(*,*) 'T*T' ,' TEST',1,UTL_EQUALNAMES('T*T','TEST',1)
 write(*,*) 'T??T',' TEST',1,UTL_EQUALNAMES('T??T','TEST',1)
 write(*,*) '*T'  ,' TEST',1,UTL_EQUALNAMES('*T','TEST',1)
 write(*,*) '*TS' ,' TEST',1,UTL_EQUALNAMES('*TS','TEST',1)
 write(*,*) 'T?T' ,' TEST',1,UTL_EQUALNAMES('T?T','TEST',1)
 write(*,*) 'T?S?',' TEST',1,UTL_EQUALNAMES('T?S?','TEST',1)
 write(*,*) '*S?' ,' TEST',1,UTL_EQUALNAMES('*S?','TEST',1)
 write(*,*) '*B31D011*' ,' cfB31D011gt',1,UTL_EQUALNAMES('*B31D011*','cfB31D011gt',1)
 write(*,*) '*B31D011*' ,' cfB31D01gt',1,UTL_EQUALNAMES('*B31D011*','cfB31D01gt',1)
 PAUSE

 END SUBROUTINE UTL_EQUALNAMES_UNITTEST

 !###======================================================================
 LOGICAL FUNCTION UTL_EQUALNAMES(SEARCH,STRING,ICAP)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: SEARCH,STRING
 INTEGER,OPTIONAL,INTENT(IN) :: ICAP
 INTEGER :: I,J,N,M,ICASE
 LOGICAL :: LEX

 ICASE=0; IF(PRESENT(ICAP))ICASE=ICAP

 N=LEN_TRIM(STRING)
 M=LEN_TRIM(SEARCH)

 !## test string on search
 J=1; I=0; DO

  LEX=.FALSE.; I=I+1
  !## string finished before ending
  IF(I.GT.N)EXIT

  !## check wildcard
  IF(SEARCH(J:J).EQ.'*')THEN
   !## proceed to next character in search
   J=J+1
   IF(J.LE.M)THEN
    LEX=.FALSE.
    DO
     IF(ICASE.EQ.0)THEN
      IF(SEARCH(J:J).EQ.STRING(I:I))EXIT
     ELSE
      IF(UTL_CAP(SEARCH(J:J),'U').EQ.UTL_CAP(STRING(I:I),'U'))EXIT
     ENDIF
     I=I+1; IF(I.GT.N)EXIT
    ENDDO
    IF(I.LE.N)THEN; LEX=.TRUE.; J=J+1; ENDIF
   ELSE
    LEX=.TRUE.
   ENDIF
  !## check wildcard
  ELSEIF(SEARCH(J:J).EQ.'?')THEN
   !## proceed to next character in search
   LEX=.TRUE.; J=J+1
  ELSE
   IF(ICASE.EQ.0)THEN
    LEX=SEARCH(J:J).EQ.STRING(I:I)
   ELSE
    LEX=UTL_CAP(SEARCH(J:J),'U').EQ.UTL_CAP(STRING(I:I),'U')
   ENDIF
   IF(LEX)J=J+1
  ENDIF
  !## incorectness found
  IF(.NOT.LEX)EXIT
  IF(J.GT.M)EXIT
 ENDDO

 J=MIN(J,M); IF(SEARCH(J:J).EQ.'*')LEX=.TRUE.

 UTL_EQUALNAMES=LEX

 END FUNCTION UTL_EQUALNAMES

 !###======================================================================
 FUNCTION UTL_CAP(STR,TXT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: TXT,STR
 INTEGER :: I,J,K,B1,B2,N
 CHARACTER(LEN=256) :: UTL_CAP

 IF(TXT.EQ.'l'.OR.TXT.EQ.'L')THEN
  B1= 65
  B2= 90
  K = 32
 ELSEIF(TXT.EQ.'u'.OR.TXT.EQ.'U')THEN
  B1= 97
  B2= 122
  K =-32
 ENDIF

 UTL_CAP=''
 N=MIN(LEN(STR),LEN(UTL_CAP))

 DO I=1,N !LEN_TRIM(STR)
  J=IACHAR(STR(I:I))
  IF(J.GE.B1.AND.J.LE.B2)J=J+K
  UTL_CAP(I:I)=ACHAR(J)
 END DO

 END FUNCTION UTL_CAP

 !###======================================================================
 FUNCTION UTL_CAP_BIG(STR,TXT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: TXT,STR
 INTEGER :: I,J,K,B1,B2
 CHARACTER(LEN=1052) :: UTL_CAP_BIG

 IF(TXT.EQ.'l'.OR.TXT.EQ.'L')THEN
  B1= 65
  B2= 90
  K = 32
 ELSEIF(TXT.EQ.'u'.OR.TXT.EQ.'U')THEN
  B1= 97
  B2= 122
  K =-32
 ENDIF

 UTL_CAP_BIG=''
 DO I=1,LEN_TRIM(STR)
  J=IACHAR(STR(I:I))
  IF(J.GE.B1.AND.J.LE.B2)J=J+K
  UTL_CAP_BIG(I:I)=ACHAR(J)
 END DO

 END FUNCTION UTL_CAP_BIG

 !###======================================================================
 SUBROUTINE UTL_DIRINFO(DIR,WC,LISTNAME,N,FT,CORDER)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: N
 CHARACTER(LEN=*),INTENT(IN) :: DIR,WC,FT
 CHARACTER(LEN=*),INTENT(OUT),DIMENSION(:) :: LISTNAME
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CORDER
 CHARACTER(LEN=512) :: LINE,BATFILE,TXTFILE
 INTEGER :: IU,I,J,IOS
 LOGICAL :: LEX

 BATFILE=TRIM(PREFVAL(1))//'\tmp\'//TRIM(OSD_GETENV('USERNAME'))//'_dir_imod.bat'
 TXTFILE=TRIM(PREFVAL(1))//'\tmp\'//TRIM(OSD_GETENV('USERNAME'))//'_dir_imod.txt'

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=BATFILE,ACTION='WRITE',FORM='FORMATTED',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD does not have priveleges to write CREATE: '//CHAR(13)//TRIM(BATFILE),'Error')
  IU=0; N=0; RETURN
 ENDIF

 INQUIRE(FILE=TXTFILE,EXIST=LEX)
 !## Successfully deleted
 IF(LEX)THEN
  I=WINFOERROR(1)
  CALL IOSDELETEFILE(TXTFILE)
  I=WINFOERROR(1)
  IF(I.EQ.ERROSCOMMAND)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD does not have priveleges to DELETE: '//CHAR(13)//TRIM(TXTFILE),'Error')
   CLOSE(IU); IU=0; N=0; RETURN
  ENDIF
 ENDIF

 IF(FT.EQ.'F'.OR.FT.EQ.'f')THEN
  !## corder
  IF(PRESENT(CORDER))THEN
   LINE='dir /b /o'//TRIM(CORDER)//' "'//TRIM(DIR)//'\'//TRIM(WC)//'" > "'//TRIM(TXTFILE)//'"'
  ELSE
   LINE='dir /b /o "'                  //TRIM(DIR)//'\'//TRIM(WC)//'" > "'//TRIM(TXTFILE)//'"'
  ENDIF
 ELSEIF(FT.EQ.'D'.OR.FT.EQ.'d')THEN
  IF(PRESENT(CORDER))THEN
   LINE='dir /ad /b /o'//TRIM(CORDER)//' "'//TRIM(DIR)//'\'//TRIM(WC)//'" > "'//TRIM(TXTFILE)//'"'
  ELSE
   LINE='dir /ad /b /o "'                  //TRIM(DIR)//'\'//TRIM(WC)//'" > "'//TRIM(TXTFILE)//'"'
  ENDIF
 ENDIF

 !## remove \\
 DO
  I=INDEX(LINE,'\\')
  IF(I.EQ.0)EXIT
  LINE(I+1:256-1)=LINE(I+2:)
 ENDDO

 WRITE(IU,'(A)') TRIM(LINE)
 CLOSE(IU)

 CALL IOSCOMMAND('"'//TRIM(BATFILE)//'"',PROCSILENT+PROCBLOCKED+PROCCMDPROC)

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TXTFILE,ACTION='READ',FORM='FORMATTED')
 IF(IU.LE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot OPEN: '//CHAR(13)//TRIM(TXTFILE),'Error')
  IU=0; N=0; RETURN
 ENDIF

 I=0
 DO
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  J=LEN_TRIM(LINE)
  IF(J.EQ.0)EXIT
  LINE(2:J+1)=LINE(1:J)
  LINE(1:1)='"'
  LINE(J+2:J+2)='"'
  I=I+1
  READ(LINE,*,IOSTAT=IOS) LISTNAME(I)
  IF(IOS.NE.0)EXIT
  !## no more space in allocated array
  IF(I.EQ.SIZE(LISTNAME))EXIT
 END DO
 !## delete result txt file
 CLOSE(IU,STATUS='DELETE')
 N=I

 END SUBROUTINE UTL_DIRINFO

 !###======================================================================
 LOGICAL FUNCTION UTL_DIRINFO_POINTER(DIR,WC,LISTNAME,FT,CORDER)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR,WC,FT
 CHARACTER(LEN=*),INTENT(OUT),DIMENSION(:),POINTER :: LISTNAME
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: CORDER
 CHARACTER(LEN=256),DIMENSION(:),POINTER :: C_LISTNAME
 CHARACTER(LEN=512) :: LINE,BATFILE,TXTFILE
 INTEGER :: IU,I,J,N,IOS
 LOGICAL :: LEX

 UTL_DIRINFO_POINTER=.FALSE.

 IF(LEN(C_LISTNAME).LT.LEN(LISTNAME))CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'c_listname()<listname()','error')

 BATFILE=TRIM(PREFVAL(1))//'\tmp\'//TRIM(OSD_GETENV('USERNAME'))//'_dir_imod.bat'
 TXTFILE=TRIM(PREFVAL(1))//'\tmp\'//TRIM(OSD_GETENV('USERNAME'))//'_dir_imod.txt'

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=BATFILE,ACTION='WRITE',FORM='FORMATTED',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD does not have priveleges to write CREATE: '//CHAR(13)//TRIM(BATFILE),'Error')
  IU=0; N=0; RETURN
 ENDIF

 INQUIRE(FILE=TXTFILE,EXIST=LEX)
 !## successfully deleted
 IF(LEX)THEN
  I=WINFOERROR(1)
  CALL IOSDELETEFILE(TXTFILE)
  I=WINFOERROR(1)
  IF(I.EQ.ERROSCOMMAND)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD does not have priveleges to DELETE: '//CHAR(13)//TRIM(TXTFILE),'Error')
   CLOSE(IU); IU=0; N=0; RETURN
  ENDIF
 ENDIF

 IF(FT.EQ.'F'.OR.FT.EQ.'f')THEN
  IF(PRESENT(CORDER))THEN
   LINE='dir /b /o'//TRIM(CORDER)//' "'//TRIM(DIR)//'\'//TRIM(WC)//'" > "'//TRIM(TXTFILE)//'"'
  ELSE
   LINE='dir /b /o "'                  //TRIM(DIR)//'\'//TRIM(WC)//'" > "'//TRIM(TXTFILE)//'"'
  ENDIF
 ELSEIF(FT.EQ.'D'.OR.FT.EQ.'d')THEN
  IF(PRESENT(CORDER))THEN
   LINE='dir /ad /b /o'//TRIM(CORDER)//' "'//TRIM(DIR)//'\'//TRIM(WC)//'" > "'//TRIM(TXTFILE)//'"'
  ELSE
   LINE='dir /ad /b /o "'                  //TRIM(DIR)//'\'//TRIM(WC)//'" > "'//TRIM(TXTFILE)//'"'
  ENDIF
 ENDIF

 !## remove \\
 DO
  I=INDEX(LINE,'\\')
  IF(I.EQ.0)EXIT
  LINE(I+1:256-1)=LINE(I+2:)
 ENDDO

 WRITE(IU,'(A)') TRIM(LINE)
 CLOSE(IU)

 CALL IOSCOMMAND('"'//TRIM(BATFILE)//'"',PROCSILENT+PROCBLOCKED+PROCCMDPROC)

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TXTFILE,ACTION='READ',FORM='FORMATTED')
 IF(IU.LE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot OPEN: '//CHAR(13)//TRIM(TXTFILE),'Error')
  IU=0; N=0; RETURN
 ENDIF

 ALLOCATE(C_LISTNAME(50))

 I=0
 DO
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  J=LEN_TRIM(LINE)
  IF(J.EQ.0)EXIT
  LINE(2:J+1)=LINE(1:J)
  LINE(1:1)='"'
  LINE(J+2:J+2)='"'
  I=I+1
  IF(I.GT.SIZE(C_LISTNAME))THEN
   N=SIZE(C_LISTNAME)
   ALLOCATE(LISTNAME(N));  LISTNAME(1:N)=C_LISTNAME(1:N)
   DEALLOCATE(C_LISTNAME); ALLOCATE(C_LISTNAME(N*2))
   C_LISTNAME(1:N)=LISTNAME(1:N); DEALLOCATE(LISTNAME)
  ENDIF
  READ(LINE,*,IOSTAT=IOS) C_LISTNAME(I)
  IF(IOS.NE.0)EXIT
 END DO

 CLOSE(IU)

 N=I

 ALLOCATE(LISTNAME(N))
 LISTNAME(1:N)=C_LISTNAME(1:N)
 DEALLOCATE(C_LISTNAME)

 UTL_DIRINFO_POINTER=.TRUE.

 END FUNCTION UTL_DIRINFO_POINTER

 !###======================================================================
 SUBROUTINE UTL_IDFSNAPTOGRID(MINX,MAXX,MINY,MAXY,CS,NCOL,NROW)
 !###======================================================================
 REAL(KIND=DP_KIND),INTENT(INOUT) :: MINX,MAXX,MINY,MAXY
 REAL(KIND=DP_KIND),INTENT(IN) :: CS
 INTEGER,INTENT(OUT) :: NCOL,NROW
 REAL(KIND=DP_KIND) :: D

 D=MOD(MINX,CS)
 IF(D.NE.0.0D0)MINX=(MINX+(CS-D))-CS
 D=MOD(MAXX,CS)
 IF(D.NE.0.0D0)MAXX=(MAXX-D)+CS
 D=MOD(MINY,CS)
 IF(D.NE.0.0D0)MINY=(MINY+(CS-D))-CS
 D=MOD(MAXY,CS)
 IF(D.NE.0.0D0)MAXY=(MAXY-D)+CS

 NCOL=INT((MAXX-MINX)/CS)
 NROW=INT((MAXY-MINY)/CS)

 END SUBROUTINE UTL_IDFSNAPTOGRID

 !###======================================================================
 SUBROUTINE UTL_IDFSNAPTONICEGRID(MINX,MAXX,MINY,MAXY,CS,NCOL,NROW)
 !###======================================================================
 REAL(KIND=DP_KIND),INTENT(INOUT) :: MINX,MAXX,MINY,MAXY
 REAL(KIND=DP_KIND),INTENT(IN) :: CS
 INTEGER,INTENT(OUT) :: NCOL,NROW
 REAL(KIND=DP_KIND) :: D

 D=MOD(ABS(MINX),CS)
 IF(D.NE.0.0D0)MINX=(MINX+(CS-D))-CS
 D=MOD(ABS(MAXX),CS)
 IF(D.NE.0.0D0)MAXX=(MAXX-D)+CS
 D=MOD(ABS(MINY),CS)
 IF(D.NE.0.0D0)MINY=(MINY+(CS-D))-CS
 D=MOD(ABS(MAXY),CS)
 IF(D.NE.0.0D0)MAXY=(MAXY-D)+CS

 NCOL=INT((MAXX-MINX)/CS)
 NROW=INT((MAXY-MINY)/CS)

 END SUBROUTINE UTL_IDFSNAPTONICEGRID

 !###======================================================================
 SUBROUTINE UTL_IDFSNAPTOGRID_LLC(MINX,MAXX,MINY,MAXY,CSX,CSY,NCOL,NROW,LLC)
 !###======================================================================
 REAL(KIND=DP_KIND),INTENT(INOUT) :: MINX,MAXX,MINY,MAXY
 REAL(KIND=DP_KIND),INTENT(IN) :: CSX,CSY
 INTEGER,INTENT(OUT) :: NCOL,NROW
 LOGICAL,INTENT(IN),OPTIONAL :: LLC
 LOGICAL :: LLLC

 NCOL=(MAXX-MINX)/CSX
 NROW=(MAXY-MINY)/CSY

 LLLC=.TRUE.; IF(PRESENT(LLC))LLLC=LLC

 IF(LLLC)THEN
  MAXX=MINX+NCOL*CSX
  MAXY=MINY+NROW*CSY
 ELSE
  MINX=MAXX-NCOL*CSX
  MINY=MAXY-NROW*CSY
 ENDIF

 END SUBROUTINE UTL_IDFSNAPTOGRID_LLC

 !###====================================================================
 REAL(KIND=DP_KIND) FUNCTION UTL_GETMOSTFREQ(FREQ,MFREQ,NFREQ)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: MFREQ,NFREQ
 REAL(KIND=DP_KIND),DIMENSION(MFREQ),INTENT(IN) :: FREQ
 INTEGER :: I,MI,NI

 NI=1  !number of unique
 MI=NI !max. number of unique
 UTL_GETMOSTFREQ=FREQ(NI)

 DO I=2,NFREQ
  IF(FREQ(I).NE.FREQ(I-1))THEN
   IF(NI.GT.MI)THEN
    UTL_GETMOSTFREQ=FREQ(I-1)
    MI=NI
   ENDIF
   NI=1
  ELSE
   NI=NI+1
  ENDIF
 END DO
 !test final
 IF(NI.GT.MI) UTL_GETMOSTFREQ=FREQ(NFREQ)

 END FUNCTION UTL_GETMOSTFREQ

! !###====================================================================
! REAL(KIND=DP_KIND) FUNCTION UTL_GETMOSTFREQ(FREQ,MFREQ,NFREQ,NODATA)
! !###====================================================================
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: MFREQ,NFREQ
! REAL(KIND=DP_KIND),INTENT(IN) :: NODATA
! REAL(KIND=DP_KIND),DIMENSION(MFREQ),INTENT(IN) :: FREQ
! INTEGER :: I,IS,MI,NI
!
! UTL_GETMOSTFREQ=NODATA
!
! IS=0
! DO
!  IS=IS+1
!  IF(FREQ(IS).NE.NODATA)EXIT
!  IF(IS.GE.NFREQ)RETURN !## nothing found ne nodata
! ENDDO
! UTL_GETMOSTFREQ=FREQ(IS)
! MI=1 !NI !max. number of unique
!
! NI=1
! IS=IS+1
! DO I=IS,NFREQ
!  IF(FREQ(I).EQ.NODATA)CYCLE
!  IF(FREQ(I).NE.FREQ(I-1))THEN
!   IF(NI.GT.MI)THEN
!    UTL_GETMOSTFREQ=FREQ(I-1)
!    MI=NI
!   ENDIF
!   NI=1
!  ELSE
!   NI=NI+1
!  ENDIF
! END DO
! !## test final
! IF(NI.GT.MI)UTL_GETMOSTFREQ=FREQ(NFREQ)
!
! END FUNCTION UTL_GETMOSTFREQ

 !###====================================================
 SUBROUTINE UTL_GETHIST(X,NX,NODATA,HIST,NHIST,MX,XHIST)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NX,NHIST         !## size array,number of percentiles to be comp.
 INTEGER,INTENT(OUT) :: MX              !## number of values ne nodata
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(:) :: HIST   !## percentile 0-100%
 REAL(KIND=DP_KIND),INTENT(OUT),DIMENSION(:) :: XHIST !## yielding percentile(s)
 REAL(KIND=DP_KIND),INTENT(IN) :: NODATA              !## nodata value !,PERC
 REAL(KIND=DP_KIND),DIMENSION(NX),INTENT(INOUT) :: X  !## array
 INTEGER :: I,J

 XHIST=0.0D0; MX=0; IF(NX.LE.0)RETURN

 DO I=1,NX
  IF(X(I).EQ.NODATA)CYCLE
  MX=MX+1
  DO J=1,NHIST-1
   IF(X(I).GT.HIST(J).AND.X(I).LE.HIST(J+1))THEN
    XHIST(J)=XHIST(J)+1
    EXIT
   ENDIF
  ENDDO
 ENDDO

 END SUBROUTINE UTL_GETHIST

 !###====================================================
 SUBROUTINE UTL_GETMED(X,NX,NODATA,PERC,NPERC,MX,XMED)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NX,NPERC        !## size array,number of percentiles to be comp.
 INTEGER,INTENT(OUT) :: MX             !## number of values ne nodata
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(:) :: PERC  !## percentile 0-100%
 REAL(KIND=DP_KIND),INTENT(OUT),DIMENSION(:) :: XMED !## yielding percentile(s)
 REAL(KIND=DP_KIND),INTENT(IN) :: NODATA             !## nodata value !,PERC
 REAL(KIND=DP_KIND),DIMENSION(NX),INTENT(INOUT) :: X !## array
 INTEGER :: I,J,IP
 REAL(KIND=DP_KIND) :: FRAC

 XMED=NODATA; MX=0

 IF(NX.LE.0)RETURN

 !## only one sample
 IF(NX.EQ.1)THEN
  IF(X(1).NE.NODATA)THEN
   XMED=X(1)
   MX  =1
  ENDIF
  RETURN
 ENDIF

 !## do not include nodata values for median-computation
 DO I=1,NX
  IF(X(I).NE.NODATA)THEN
   MX   =MX+1
   X(MX)=X(I)
  ENDIF
 END DO

 IF(MX.LE.0)RETURN

 CALL QKSORT_SGL(MX,X)

 DO IP=1,NPERC

  IF(PERC(IP).LE.0.0D0)THEN
   XMED(IP)=X(1)
  ELSEIF(PERC(IP).GE.100.0D0)THEN
   XMED(IP)=X(MX)
  ELSE
   FRAC=1.0D0/(PERC(IP)/100.0D0)

   IF(MOD(REAL(MX),FRAC).EQ.0.0D0)THEN
    I=MAX(1,INT(REAL(MX)/FRAC))
    XMED(IP)=X(I)
   ELSE
    I=MAX(1,INT(REAL(MX)/FRAC))
    J=MIN(I+1,MX)
    XMED(IP)=(X(I)+X(J))/2.0D0
   ENDIF
  ENDIF
 ENDDO

 END SUBROUTINE UTL_GETMED

 !###====================================================
 SUBROUTINE UTL_GETMED_INVERSE(X,NX,NODATA,PERC,NPERC,MX,XMED)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NX,NPERC        !## size array,number of percentiles to be comp.
 INTEGER,INTENT(OUT) :: MX             !## number of values ne nodata
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(:) :: PERC  !## percentile 0-100%
 REAL(KIND=DP_KIND),INTENT(OUT),DIMENSION(:) :: XMED !## yielding percentile(s)
 REAL(KIND=DP_KIND),INTENT(IN) :: NODATA             !## nodata value
 REAL(KIND=DP_KIND),DIMENSION(NX),INTENT(INOUT) :: X !## array
 INTEGER :: I,IP
 REAL(KIND=DP_KIND) :: FRAC

 XMED=NODATA

 IF(NX.LE.0)RETURN

 !## only one sample
 IF(NX.EQ.1)THEN
  DO IP=1,NPERC
   XMED(IP)=0.0D0
   IF(X(1).LE.PERC(IP))XMED(IP)=1.0D0
  ENDDO
  MX  =1
  RETURN
 ENDIF

 !## do not include nodata values for median-computation
 MX=0
 DO I=1,NX
  IF(X(I).NE.NODATA)THEN
   MX   =MX+1
   X(MX)=X(I)
  ENDIF
 END DO

 IF(MX.LE.0)RETURN

 !## sort data, excl. nodata values
! IF(MX.LE.100)THEN
!  CALL SHELLSORT(MX,X)
! ELSE
 CALL QKSORT_SGL(MX,X) !MX,X)
! ENDIF

 !## find appropriate values for percentiles
 FRAC=1.0D0
 DO IP=1,NPERC
  IF(MX.EQ.1)THEN
   XMED(IP)=0.0D0
   IF(X(1).LE.PERC(IP))XMED(IP)=1.0D0
  ELSE
   IF(PERC(IP).LE.X(1))THEN
    XMED(IP)=0.0D0
   ELSEIF(PERC(IP).GT.X(MX))THEN
    XMED(IP)=1.0D0
   ELSE
    DO I=2,MX
     IF(X(I-1).LE.PERC(IP).AND. &
        X(I)  .GE.PERC(IP))THEN
      FRAC=(PERC(IP)-X(I-1))/(X(I)-X(I-1))
      XMED(IP)=(REAL(I-1)+FRAC)/REAL(MX)
      EXIT
     ENDIF
    ENDDO
   ENDIF
  ENDIF
  WRITE(*,*) 'PERC(IP)=',PERC(IP)
  WRITE(*,*) 'XMED(IP)=',XMED(IP)
  WRITE(*,*) 'FRAC    =',FRAC
  DO I=1,MX; WRITE(*,'(I10,F15.7)') I,X(I); ENDDO
 ENDDO

 END SUBROUTINE UTL_GETMED_INVERSE

 !###======================================================================
 INTEGER FUNCTION CALCPERIODS(ISTARTDATE,IENDDATE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ISTARTDATE,IENDDATE

 CALCPERIODS=UTL_IDATETOJDATE(IENDDATE)-UTL_IDATETOJDATE(ISTARTDATE)+1

 END FUNCTION CALCPERIODS

 !###====================================================
 SUBROUTINE UTL_GETUNIQUE(X,N,NU,NODATA)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER,INTENT(OUT) :: NU
 REAL(KIND=DP_KIND),INTENT(IN),OPTIONAL :: NODATA
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(N) :: X
 INTEGER :: I

 CALL QKSORT_SGL(N,X) !N,X)

 !## determine number of unique classes
 IF(PRESENT(NODATA))THEN
  NU=0
  DO I=1,N
   IF(NU.EQ.0)THEN
    IF(X(I).NE.NODATA)THEN
     NU=NU+1
     X(NU)=X(I)
    ENDIF
   ELSE
    IF(X(I).NE.X(NU).AND.X(I).NE.NODATA)THEN
     NU   =NU+1
     X(NU)=X(I)
    ENDIF
   ENDIF
  END DO
 ELSE
  NU=1
  DO I=2,N
   IF(X(I).NE.X(NU))THEN
    NU   =NU+1
    X(NU)=X(I)
   ENDIF
  END DO
 ENDIF

 END SUBROUTINE UTL_GETUNIQUE

 !###====================================================
 SUBROUTINE UTL_GETUNIQUE_POINTER(X,N,NU,NODATA)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER,INTENT(OUT) :: NU
 REAL(KIND=DP_KIND),INTENT(IN),OPTIONAL :: NODATA
 REAL(KIND=DP_KIND),POINTER,INTENT(INOUT),DIMENSION(:) :: X
 INTEGER :: I

 CALL WSORT(X,1,N)

 !## determine number of unique classes
 IF(PRESENT(NODATA))THEN
  NU=0
  DO I=1,N
   IF(NU.EQ.0)THEN
    IF(X(I).NE.NODATA)THEN
     NU=NU+1
     X(NU)=X(I)
    ENDIF
   ELSE
    IF(X(I).NE.X(NU).AND.X(I).NE.NODATA)THEN
     NU   =NU+1
     X(NU)=X(I)
    ENDIF
   ENDIF
  END DO
 ELSE
  NU=1
  DO I=2,N
   IF(X(I).NE.X(NU))THEN
    NU   =NU+1
    X(NU)=X(I)
   ENDIF
  END DO
 ENDIF

 END SUBROUTINE UTL_GETUNIQUE_POINTER

 !###====================================================
 SUBROUTINE UTL_GETUNIQUE_INT(IX,N,NU,NODATA)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER,INTENT(OUT) :: NU
 INTEGER,INTENT(INOUT),DIMENSION(N) :: IX
 INTEGER,INTENT(IN),OPTIONAL :: NODATA
 INTEGER :: I

 CALL SHELLSORT_INT(N,IX)

 !## determine number of unique classes
 IF(PRESENT(NODATA))THEN
  NU=0
  DO I=1,N
   IF(NU.EQ.0)THEN
    IF(IX(I).NE.NODATA)THEN
     NU=NU+1
     IX(NU)=IX(I)
    ENDIF
   ELSE
    IF(IX(I).NE.IX(NU).AND.IX(I).NE.NODATA)THEN
     NU    =NU+1
     IX(NU)=IX(I)
    ENDIF
   ENDIF
  END DO
 ELSE
  !## determine number of unique classes
  NU=1
  DO I=2,N
   IF(IX(I).NE.IX(NU))THEN
    NU    =NU+1
    IX(NU)=IX(I)
   ENDIF
  END DO
 ENDIF

 END SUBROUTINE UTL_GETUNIQUE_INT

  !###====================================================
 SUBROUTINE UTL_GETUNIQUE_DINT(IX,N,NU,NODATA)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER,INTENT(OUT) :: NU
 INTEGER(KIND=8),INTENT(INOUT),DIMENSION(N) :: IX
 INTEGER(KIND=8),INTENT(IN),OPTIONAL :: NODATA
 INTEGER :: I

 CALL SHELLSORT_DINT(N,IX)

 !## determine number of unique classes
 IF(PRESENT(NODATA))THEN
  NU=0
  DO I=1,N
   IF(NU.EQ.0)THEN
    IF(IX(I).NE.NODATA)THEN
     NU=NU+1
     IX(NU)=IX(I)
    ENDIF
   ELSE
    IF(IX(I).NE.IX(NU).AND.IX(I).NE.NODATA)THEN
     NU    =NU+1
     IX(NU)=IX(I)
    ENDIF
   ENDIF
  END DO
 ELSE
  !## determine number of unique classes
  NU=1
  DO I=2,N
   IF(IX(I).NE.IX(NU))THEN
    NU    =NU+1
    IX(NU)=IX(I)
   ENDIF
  END DO
 ENDIF

 END SUBROUTINE UTL_GETUNIQUE_DINT

 !###====================================================
 SUBROUTINE UTL_GETUNIQUE_CHAR(CH,N,NU)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER,INTENT(OUT) :: NU
 CHARACTER(LEN=*),INTENT(INOUT),DIMENSION(N) :: CH
 INTEGER :: I,J

 !## determine number of unique classes
 NU=1
 DO I=2,N
  DO J=1,NU
   IF(CH(J).EQ.CH(I))EXIT !## get it already
  ENDDO
  IF(J.LE.NU)CYCLE
  !## add new to unique string
  NU=NU+1
  CH(NU)=CH(I)
 END DO

 END SUBROUTINE UTL_GETUNIQUE_CHAR

 !###====================================================
 LOGICAL FUNCTION UTL_EQUALS_REAL(A,B)
 !###====================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: A,B
 REAL(KIND=DP_KIND) :: EPS

 EPS=ABS(A)*EPSILON(A) ! SCALE EPSILON

 IF(EPS.EQ.0.0D0)THEN
  EPS=TINY (A) ! IF EPS UNDERFLOWED TO 0
 ! USE A VERY SMALL
 ! POSITIVE VALUE FOR EPSILON
 END IF

 IF(ABS(A-B).GT.EPS)THEN
  UTL_EQUALS_REAL=.FALSE. ! NOT EQUAL IF DIFFERENCE>EPS
 ELSE
  UTL_EQUALS_REAL=.TRUE.  ! EQUAL OTHERWISE
 ENDIF

 END FUNCTION UTL_EQUALS_REAL

 !###====================================================
 SUBROUTINE PEUCKER_SIMPLIFYLINE(XC,YC,PDCODE,N)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(N) :: XC,YC
 REAL(KIND=DP_KIND),INTENT(OUT),DIMENSION(N) :: PDCODE
 INTEGER :: MJ,J1,J2
 REAL(KIND=DP_KIND) :: MD

 PDCODE=-999.99D0
 !## set first and last point, distance is zero
 PDCODE(1)=0.0D0; PDCODE(N)=0.0D0

 !## process each intermediate point
 DO

  !## get the start point (first empty spot)
  DO J1=1,N-1; IF(PDCODE(J1).LT.0.0D0)EXIT; ENDDO
  !## finished
  IF(J1.EQ.N)EXIT
  !## previous fixed point
  J1=J1-1

  !## get the end point (fixed point)
  DO J2=J1+1,N; IF(PDCODE(J2).GE.0.0D0)EXIT; ENDDO

  !## get the maximal distance in between i1 and i2 and tag it
  CALL PEUCKER_CALCDISTANCE(J1,J2,N,XC,YC,MJ,MD)

  !## tag decrease line segment to examine
  IF(MJ.GT.0)PDCODE(MJ)=MD

 ENDDO

 END SUBROUTINE PEUCKER_SIMPLIFYLINE

 !###====================================================
 SUBROUTINE PEUCKER_CALCDISTANCE(J1,J2,N,XC,YC,MJ,MD)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,J1,J2
 REAL(KIND=DP_KIND),INTENT(OUT) :: MD
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(N) :: XC,YC
 INTEGER,INTENT(OUT) :: MJ
 INTEGER :: J
 REAL(KIND=DP_KIND) :: B,A,D,Y,DX,DY

 !## line equation
 B=YC(J1)
 DY=(YC(J2)-YC(J1))
 DX=(XC(J2)-XC(J1))
 A=10.0D10; IF(DX.NE.0.0D0)A=DY/DX
 MD=-1.0D0; MJ=0

 !## loop over all points
 DO J=J1+1,J2-1
  !## get point on line
  Y=(XC(J)-XC(J1))*A+B
  !## get difference between line and point
  D=ABS(Y-YC(J))
  !## keep this one is this is largers
  IF(D.GT.MD)THEN
   MD=D; MJ=J
  ENDIF
 ENDDO

 END SUBROUTINE PEUCKER_CALCDISTANCE

 !###====================================================================
 REAL(KIND=DP_KIND) FUNCTION UTL_NASH_SUTCLIFFE(X,Y,N)
 !###====================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),PARAMETER :: TINYYN=0.01D0
 INTEGER,INTENT(IN) :: N
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(N) :: X,Y !## x=head; y=obs
 REAL(KIND=DP_KIND) :: XN,YN,YA
 INTEGER :: I
 
 !## compute nash-sutcliff
 UTL_NASH_SUTCLIFFE=0.0D0
 
 !## average observation
 YA=0.0D0; DO I=1,N; YA=YA+Y(I)               ; ENDDO; YA=YA/REAL(N)
 XN=0.0D0; DO I=1,N; XN=XN+(Y(I)-X(I))**2.0D0 ; ENDDO
 YN=0.0D0; DO I=1,N; YN=YN+(Y(I)-YA)**2.0D0   ; ENDDO
 
 !## whenever the computed head is constant in time - assume a small yn
 IF(YN.EQ.0.0D0)YN=TINYYN
 UTL_NASH_SUTCLIFFE=1.0D0-XN/YN

 END FUNCTION UTL_NASH_SUTCLIFFE 
 
 !###====================================================================
 REAL(KIND=DP_KIND) FUNCTION UTL_GOODNESS_OF_FIT(X,Y,N)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(N) :: X,Y
 REAL(KIND=DP_KIND) :: XN,YN,X1,X2,X3
 INTEGER :: I

 UTL_GOODNESS_OF_FIT=0.0D0

 XN=SUM(X)/DBLE(N)
 YN=SUM(Y)/DBLE(N)

 X1=0.0D0; X2=0.0D0; X3=0.0D0
 DO I=1,N
  X1=X1+(X(I)-XN)*(Y(I)-YN)
  X2=X2+(X(I)-XN)**2.0D0
  X3=X3+(Y(I)-YN)**2.0D0
 ENDDO

 !## sample correlation coefficient
 IF(X2.NE.0.0D0.AND.X3.NE.0.0D0)UTL_GOODNESS_OF_FIT=X1/SQRT(X2*X3)

 END FUNCTION UTL_GOODNESS_OF_FIT

 !###====================================================
 SUBROUTINE UTL_FIT_REGRESSION(X,Y,NDATA,SIG,MWT,A,B,SIGA,SIGB,CHI2,Q)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NDATA,MWT
 REAL(KIND=DP_KIND),INTENT(OUT) :: A,B,CHI2,Q,SIGA,SIGB
 REAL(KIND=DP_KIND),DIMENSION(NDATA) :: X,Y,SIG
 INTEGER :: I
 REAL(KIND=DP_KIND) :: SIGDAT,SS,ST2,SX,SXOSS,SY,T,WT

 SX =0.0D0
 SY =0.0D0
 ST2=0.0D0
 B  =0.0D0

 IF(MWT.NE.0)THEN
  SS=0.0D0
  DO I=1,NDATA
   WT=1.0D0/(SIG(I)**2)
   SS=SS+WT
   SX=SX+X(I)*WT
   SY=SY+Y(I)*WT
  ENDDO
 ELSE
  DO I=1,NDATA
   SX=SX+X(I)
   SY=SY+Y(I)
  ENDDO
  SS=FLOAT(NDATA)
 ENDIF

 SXOSS=SX/SS

 IF(MWT.NE.0)THEN
  DO I=1,NDATA
   T=(X(I)-SXOSS)/SIG(I)
   ST2=ST2+T*T
   B=B+T*Y(I)/SIG(I)
  ENDDO
 ELSE
  DO I=1,NDATA
   T=X(I)-SXOSS
   ST2=ST2+T*T
   B=B+T*Y(I)
  ENDDO
 ENDIF

 B=B/ST2
 A=(SY-SX*B)/SS
 SIGA=SQRT((1.0D0+SX*SX/(SS*ST2))/SS)
 SIGB=SQRT(1.0D0/ST2)
 CHI2=0.0D0

 IF(MWT.EQ.0)THEN
  DO I=1,NDATA
   CHI2=CHI2+(Y(I)-A-B*X(I))**2
  ENDDO
  Q=1.0D0
  SIGDAT=SQRT(CHI2/REAL(NDATA-2))
  SIGA=SIGA*SIGDAT
  SIGB=SIGB*SIGDAT
 ELSE
  DO I=1,NDATA
   CHI2=CHI2+((Y(I)-A-B*X(I))/SIG(I))**2
  ENDDO
  Q=UTL_GAMMQ(REAL(0.5*(NDATA-2),8),REAL(0.5*CHI2,8))
 ENDIF

 END SUBROUTINE UTL_FIT_REGRESSION

 !###====================================================
 REAL(KIND=DP_KIND) FUNCTION UTL_GAMMQ(A,X)
 !###====================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: A,X
 REAL(KIND=DP_KIND) :: GAMMCF,GAMSER,GLN

 IF(X.LT.0.0D0.OR.A.LE.0.0D0)PAUSE 'BAD ARGUMENT IN UTL_GAMMQ'
 IF(X.LT.A+1.0D0)THEN
  CALL UTL_GSER(GAMSER,A,X,GLN)
  UTL_GAMMQ=1.0D0-GAMSER
 ELSE
  CALL UTL_GCF(GAMMCF,A,X,GLN)
  UTL_GAMMQ=GAMMCF
 ENDIF

 END FUNCTION

 !###====================================================
 SUBROUTINE UTL_GSER(GAMSER,A,X,GLN)
 !###====================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: A,X
 REAL(KIND=DP_KIND),INTENT(OUT) :: GLN,GAMSER
 INTEGER,PARAMETER :: ITMAX=100
 REAL(KIND=DP_KIND),PARAMETER :: EPS=3.0E-7
 INTEGER :: N
 REAL(KIND=DP_KIND) :: AP,DEL,SUM

 GLN=UTL_GAMMLN(A)
 IF(X.LE.0.0D0)THEN
  IF(X.LT.0.0D0)PAUSE 'X < 0 IN UTL_GSER'
  GAMSER=0.0D0
  RETURN
 ENDIF

 AP=A
 SUM=1.0D0/A
 DEL=SUM

 DO N=1,ITMAX
  AP=AP+1.0D0
  DEL=DEL*X/AP
  SUM=SUM+DEL
  IF(ABS(DEL).LT.ABS(SUM)*EPS)GOTO 1
 ENDDO

 PAUSE 'A TOO LARGE, ITMAX TOO SMALL IN UTL_GSER'
1 GAMSER=SUM*EXP(-X+A*LOG(X)-GLN)

 END SUBROUTINE UTL_GSER

 !###====================================================
 SUBROUTINE UTL_GCF(GAMMCF,A,X,GLN)
 !###====================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: ITMAX=100
 REAL(KIND=DP_KIND),PARAMETER :: EPS=3.0E-7,FPMIN=1.0D0-30
 REAL(KIND=DP_KIND),INTENT(IN) :: A,X
 REAL(KIND=DP_KIND),INTENT(OUT) :: GAMMCF,GLN
 INTEGER :: I
 REAL(KIND=DP_KIND) :: AN,B,C,D,DEL,H

 GLN=UTL_GAMMLN(A)
 B=X+1.0D0-A
 C=1.0D0/FPMIN
 D=1.0D0/B
 H=D
 DO I=1,ITMAX
  AN=-I*(I-A)
  B=B+2.0
  D=AN*D+B
  IF(ABS(D).LT.FPMIN)D=FPMIN
  C=B+AN/C
  IF(ABS(C).LT.FPMIN)C=FPMIN
  D=1.0D0/D
  DEL=D*C
  H=H*DEL
  IF(ABS(DEL-1.0D0).LT.EPS)GOTO 1
 ENDDO
 PAUSE 'A TOO LARGE, ITMAX TOOP SMALL IN UTL_GCF'
1 GAMMCF=EXP(-X+A*LOG(X)-GLN)*H

 END SUBROUTINE UTL_GCF

 !###====================================================
 REAL(KIND=DP_KIND) FUNCTION UTL_GAMMLN(XX)
 !###====================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: XX
 INTEGER :: J
 REAL(KIND=DP_KIND),SAVE :: SER,STP,TMP,X,Y,COF(6)
 DATA COF,STP/76.18009172947146D0,-86.50532032941677D0, &
              24.01409824083091D0,-1.231739572450155D0,0.1208650973866179D-2, &
              -0.5395239384953D-5,2.5066282746310005D0/

 X=XX
 Y=X
 TMP=X+5.5D0
 TMP=(X+0.5D0)*LOG(TMP)-TMP
 SER=1.000000000190015D0
 DO J=1,6
  Y=Y+1.0D0
  SER=SER+COF(J)/Y
 ENDDO
 UTL_GAMMLN=TMP+LOG(STP*SER/X)

 END FUNCTION UTL_GAMMLN

 !###======================================================================
 SUBROUTINE UTL_SYSCOREINFO(NOCINT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: NOCINT
 CHARACTER(LEN=512) :: LINE,BATFILE,TXTFILE
 INTEGER :: IU,I,IOS
 LOGICAL :: LEX

 !## initial value
 NOCINT=1

 BATFILE=TRIM(PREFVAL(1))//'\tmp\'//TRIM(OSD_GETENV('USERNAME'))//'_syscore_imod.bat'
 TXTFILE=TRIM(PREFVAL(1))//'\tmp\'//TRIM(OSD_GETENV('USERNAME'))//'_syscore_imod.txt'

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=BATFILE,ACTION='WRITE',FORM='FORMATTED',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD does not have priveleges to write CREATE: '//CHAR(13)//TRIM(BATFILE),'Error')
  IU=0; RETURN
 ENDIF

 INQUIRE(FILE=TXTFILE,EXIST=LEX)
 !## successfully deleted
 IF(LEX)THEN
  I=WINFOERROR(1)
  CALL IOSDELETEFILE(TXTFILE)
  I=WINFOERROR(1)
  IF(I.EQ.ERROSCOMMAND)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD does not have priveleges to DELETE: '//CHAR(13)//TRIM(TXTFILE),'Error')
   CLOSE(IU); IU=0; RETURN
  ENDIF
 ENDIF

 LINE='wmic cpu get NumberOfCores > "'//TRIM(TXTFILE)//'"'
! LINE='echo %NUMBER_OF_PROCESSORS% > "'//TRIM(TXTFILE)//'"'
! LINE='wmic cpu get NumberOfLogicalProcessors > "'//TRIM(TXTFILE)//'"'

 WRITE(IU,'(A)') TRIM(LINE)
 CLOSE(IU)

 CALL IOSCOMMAND('"'//TRIM(BATFILE)//'"',PROCSILENT+PROCBLOCKED+PROCCMDPROC)

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TRIM(TXTFILE),STATUS='OLD',ACTION='READ')
 IF(IU.LE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot OPEN: '//CHAR(13)//TRIM(TXTFILE),'Error')
  IU=0; RETURN
 ENDIF

 !READ(IU,*,IOSTAT=IOS)
 !READ(IU,*,IOSTAT=IOS) NOCINT
 !IF(IOS.NE.0)NOCINT=-1
 NOCINT=4

 !## delete result txt file
 CLOSE(IU,STATUS='DELETE')

 END SUBROUTINE UTL_SYSCOREINFO

END MODULE MOD_UTL
