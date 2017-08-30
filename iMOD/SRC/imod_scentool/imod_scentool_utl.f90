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
MODULE MOD_SCENTOOL_UTL

USE WINTERACTER
USE RESOURCE
USE MOD_UTL, ONLY : ITOS,RTOS,IDFPLOT1BITMAP,IDFPLOT2BITMAP,MXMESSAGE,INVERSECOLOUR,UTL_IMODFILLMENU, &
                    UTL_IMODFILLMENU_DEAL,LISTNAME,JDATETOGDATE,GDATETOJDATE,UTL_WSELECTFILE,UTL_GETUNIT, &
                    UTL_JDATETOIDATE,UTL_IDATETOJDATE
USE MODPLOT, ONLY : MPW
USE MOD_GRAPH, ONLY : GRAPH_PLOT,GRAPH,GRAPH_DEALLOCATE,GRAPH_ALLOCATE
USE MOD_SCENTOOL_PAR
USE MOD_OSD, ONLY : OSD_OPEN

REAL,PRIVATE,DIMENSION(:),ALLOCATABLE :: X,Y
INTEGER,PRIVATE,DIMENSION(:),ALLOCATABLE :: XYP

CONTAINS
 
 !###======================================================================
 INTEGER FUNCTION ST1ERROR(IOS,TEXT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOS
 CHARACTER(LEN=*),INTENT(IN) :: TEXT

 ST1ERROR=IOS
 IF(IOS.EQ.0)RETURN

 CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,TRIM(TEXT),'Error')

 END FUNCTION ST1ERROR

 !###======================================================================
 SUBROUTINE ST1_GETCOLOUR(IDD,IDF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDD,IDF
 INTEGER :: IRGB
 
 CALL WDIALOGSELECT(IDD)
 CALL WDIALOGGETINTEGER(IDF,IRGB)
 CALL WSELECTCOLOUR(IRGB)
 IF(WINFODIALOG(4).EQ.1)CALL WDIALOGPUTINTEGER(IDF,IRGB)
 CALL WDIALOGCOLOUR(IDF,IRGB,IRGB)
 
 END SUBROUTINE ST1_GETCOLOUR

 !###======================================================================
 SUBROUTINE ST_SYMBOLCOLOUR(IDD,IDC)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDD,IDC
 INTEGER :: ICLR
 
 CALL WDIALOGSELECT(IDD)
 CALL WDIALOGGETINTEGER(IDC,ICLR)
 CALL WDIALOGCOLOUR(IDC,ICLR,ICLR)
 
 END SUBROUTINE ST_SYMBOLCOLOUR
 
 !###======================================================================
 SUBROUTINE ST_SYMBOLDRAW(IDD,IDF,IDM,IDC)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDD,IDF,IDM,IDC
 INTEGER :: IMARKER
 
 CALL ST_SYMBOLCOLOUR(IDD,IDC)

 CALL WDIALOGSELECT(IDD)
 CALL WDIALOGGETMENU(IDM,IMARKER)
 
 CALL IGRPLOTMODE(MODECOPY)
 CALL IGRSELECT(DRAWFIELD,IDF)
 CALL IGRCOLOURN(WRGB(255,255,255))
 CALL IGRFILLPATTERN(SOLID)
 CALL IGRRECTANGLE(0.0,0.0,1.0,1.0)
 CALL IGRFILLPATTERN(OUTLINE)
 CALL IGRCOLOURN(WRGB(0,0,0))
 CALL IGRAREA(0.0,0.0,1.0,1.0)
 CALL IGRUNITS(0.0,0.0,1.0,1.0)

 CALL IGRLINETYPE(SOLIDLINE)
 CALL WGRTEXTFONT(WIDTH=0.5,HEIGHT=0.5)
 CALL IGRMARKER(0.5,0.5,IMARKER)
  
 END SUBROUTINE ST_SYMBOLDRAW

 !###======================================================================
 SUBROUTINE ST_DRAWPNTS(IDD,IDG,IPOS,IACTION,ISYMBOL,ICLR,IMODE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDD,IDG,ISYMBOL,ICLR,IACTION,IMODE
 INTEGER,INTENT(IN),DIMENSION(:) :: IPOS
 TYPE(WIN_MESSAGE)  :: MESSAGE
 INTEGER :: ITYPE,NROW,NCOL,IROW,ICOL,I,ICTYPE,NP,IP,JP,IMOVE
 REAL :: DX,DY,D,MIND,ISIZE

 ISIZE=0.01 !% of graphical units to be used for plotting markers
 
 !## imode  =0 wells
 !## imode  =1 observations
 
 !## iaction=1 add
 !## iaction=2 move
 !## iaction=3 delete
 
 !## determine maximum of points to be added to grid
 CALL WDIALOGSELECT(IDD)
 NROW=WINFOGRID(IDG,GRIDROWSCUR)
 NCOL=WINFOGRID(IDG,GRIDCOLUMNS)

 CALL WGRTEXTFONT(WIDTH=ISIZE,HEIGHT=ISIZE*WINFOGRREAL(GRAPHICSRATIO))

 !## find first blanco line
 IF(IACTION.EQ.1)THEN
  DO IROW=1,NROW
   I=0
   DO ICOL=1,NCOL
    IF(WINFOGRIDCELL(IDG,ICOL,IROW,GRIDCELLDEFINED).EQ.0)I=I+1
   ENDDO
   !## find first empty row
   IF(I.EQ.NCOL)EXIT
  END DO
  IF(IROW.GT.NROW)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot insert more points!','Information')
   RETURN
  ENDIF
  CALL WCURSORSHAPE(ID_CURSORPOINTPLUS)
 ELSE
  CALL WCURSORSHAPE(ID_CURSORPOINT)
  ALLOCATE(X(NROW),Y(NROW),XYP(NROW))
  !## read grid for locations
  IF(.NOT.STGETPNTS(IDD,IDG,IPOS,NROW,NP))RETURN
 ENDIF

 CALL IGRPLOTMODE(MODECOPY)
 CALL WINDOWOUTSTATUSBAR(4,'Terminate with the right mouse-button!')

 IP=0
 JP=0
 IMOVE=0

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)

   !## mouse-move
   CASE (MOUSEMOVE)
    CALL WINDOWSELECT(0)
    CALL WINDOWOUTSTATUSBAR(1,'X:'//TRIM(ITOS(INT(MESSAGE%GX)))//' m, Y:'// &
                                    TRIM(ITOS(INT(MESSAGE%GY)))//' m')
    
    !## move/delete actions
    IF(IACTION.EQ.2.OR.IACTION.EQ.3)THEN

     IF(IMOVE.EQ.0)THEN
      !## find nearest point
      MIND=(MPW%XMAX-MPW%XMIN)/100.0
      IP  = 0
      DO I=1,NP
       DX=(MESSAGE%GX-X(I))**2.0
       DY=(MESSAGE%GY-Y(I))**2.0
       D=0.0
       IF(DX+DY.GT.0.0)D=SQRT(DX+DY)
       IF(D.LT.MIND)THEN
        MIND=D
        IP  =I
       ENDIF
      END DO
      
      IF(IP.EQ.0)CALL WCURSORSHAPE(ID_CURSORPOINT)
      IF(IP.NE.0)THEN
       IF(IACTION.EQ.2)CALL WCURSORSHAPE(ID_CURSORMOVE)
       IF(IACTION.EQ.3)CALL WCURSORSHAPE(ID_CURSORPOINTMIN)
       IF(IP.NE.JP)THEN
        CALL WDIALOGSELECT(IDD)
        CALL WGRIDCOLOURROW(IDG,IP,RGBBACK=WRGB(200,0,0))
        IF(JP.NE.0)CALL WGRIDCOLOURROW(IDG,JP,RGBBACK=-1)
        JP=IP
       ENDIF
      ENDIF
     
     ELSE

      CALL IDFPLOT1BITMAP()
      CALL IGRCOLOURN(INVERSECOLOUR(ICLR))
      CALL IGRMARKER(X(IP),Y(IP),ISYMBOL)

      CALL WDIALOGSELECT(IDD)
      CALL WGRIDPUTCELLREAL(IDG,IPOS(1),XYP(IP),MESSAGE%GX)
      CALL WGRIDPUTCELLREAL(IDG,IPOS(2),XYP(IP),MESSAGE%GY)
      X(IP)=MESSAGE%GX
      Y(IP)=MESSAGE%GY

      CALL IGRMARKER(X(IP),Y(IP),ISYMBOL)
      CALL IDFPLOT2BITMAP()

     ENDIF
    ENDIF

   !## put point
   CASE (MOUSEBUTUP)
    IF(IACTION.NE.1)THEN
     CALL WDIALOGSELECT(IDD)
     IF(JP.NE.0)CALL WGRIDCOLOURROW(IDG,JP,RGBBACK=-1)
     JP=0
     IF(IMOVE.NE.0)CALL IDFPLOT(1)
    ENDIF
    IMOVE=0

   !## put point
   CASE (MOUSEBUTDOWN)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (1)

      !## add point
      IF(IACTION.EQ.1)THEN
       CALL IDFPLOT1BITMAP()
       CALL IGRCOLOURN(ICLR)
       CALL IGRMARKER(MESSAGE%GX,MESSAGE%GY,ISYMBOL)
       CALL IDFPLOT2BITMAP()

       CALL WDIALOGSELECT(IDD)
       DO ICOL=1,NCOL
        IF(ICOL.EQ.IPOS(1))THEN
         CALL WGRIDPUTCELLREAL(IDG,IPOS(1),IROW,MESSAGE%GX)
        ELSEIF(ICOL.EQ.IPOS(2))THEN
         CALL WGRIDPUTCELLREAL(IDG,IPOS(2),IROW,MESSAGE%GY)
        ELSE
         !## fill in default values
         ICTYPE=WINFOGRIDCELL(IDG,ICOL,IROW,GRIDCELLTYPE)
         SELECT CASE (ICTYPE)
          CASE (4)
           IF(IMODE.EQ.1)CALL WGRIDPUTCELLSTRING(IDG,ICOL,IROW,'Well '//TRIM(ITOS(IROW)))
           IF(IMODE.EQ.2)CALL WGRIDPUTCELLSTRING(IDG,ICOL,IROW,'Screen '//TRIM(ITOS(IROW)))
          CASE (5)
           CALL WGRIDPUTCELLINTEGER(IDG,ICOL,IROW,0)
          CASE (6)
           CALL WGRIDPUTCELLREAL(IDG,ICOL,IROW,0.0)
         END SELECT
        ENDIF
       ENDDO
       IROW=IROW+1
       IF(IROW.GT.NROW)THEN
        CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot insert more points!','Information')
        EXIT
       ENDIF
      !## move
      ELSEIF(IACTION.EQ.2)THEN
       IMOVE=0
       IF(IP.NE.0)THEN
        IMOVE=IP
        CALL IGRPLOTMODE(MODEXOR)
        CALL WCURSORSHAPE(ID_NOCURSOR)
       ENDIF
      !## delete
      ELSEIF(IACTION.EQ.3)THEN
       IMOVE=0
       !## remove ip-number out of grid
       IF(IP.NE.0)THEN
        IMOVE=IP
        CALL WDIALOGSELECT(IDD)
        CALL WGRIDDELETEROWS(IDG,IP,NDEL=1,IREDUCE=DISABLED,ILABELS=ENABLED)
        CALL WCURSORSHAPE(ID_CURSORPOINT)
        CALL IDFPLOT(1)
        !## read grid for locations
        IF(.NOT.STGETPNTS(IDD,IDG,IPOS,NROW,NP))EXIT
       ENDIF
      ENDIF

     CASE (3)
      EXIT
    END SELECT

   !## bitmap scrolled, renew top-left pixel coordinates
   CASE (BITMAPSCROLLED)
    MPW%IX=MESSAGE%VALUE1
    MPW%IY=MESSAGE%VALUE2

  END SELECT
 END DO

 CALL WDIALOGSELECT(IDD)
 IF(JP.NE.0)CALL WGRIDCOLOURROW(IDG,JP,RGBBACK=-1)

 CALL WCURSORSHAPE(CURARROW)
 CALL WINDOWOUTSTATUSBAR(4,'')
 IF(IACTION.EQ.2)THEN
  IF(ALLOCATED(X))  DEALLOCATE(X)
  IF(ALLOCATED(Y))  DEALLOCATE(Y)
  IF(ALLOCATED(XYP))DEALLOCATE(XYP)
 ENDIF
 CALL IDFPLOT(1)

 END SUBROUTINE ST_DRAWPNTS

 !###======================================================================
 LOGICAL FUNCTION STGETPNTS(IDD,IDG,IPOS,N,NP)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDD,IDG,N
 INTEGER,INTENT(IN),DIMENSION(2) :: IPOS
 INTEGER,INTENT(OUT) :: NP
 INTEGER :: I
  
 STGETPNTS=.FALSE.
 
 CALL WDIALOGSELECT(IDD)

 NP=0
 DO I=1,N
  IF(WINFOGRIDCELL(IDG,IPOS(1),I,GRIDCELLDEFINED).NE.0.AND. &
     WINFOGRIDCELL(IDG,IPOS(2),I,GRIDCELLDEFINED).NE.0)THEN
   NP=NP+1
   XYP(NP)=I
   CALL WGRIDGETCELLREAL(IDF_GRID1,IPOS(1),I,X(NP))
   CALL WGRIDGETCELLREAL(IDF_GRID1,IPOS(2),I,Y(NP))
  ENDIF
 ENDDO
 
 IF(NP.EQ.0)THEN
  DEALLOCATE(X,Y,XYP)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Nothing to select (anymore).','Information')
  RETURN
 ENDIF

 STGETPNTS=.TRUE.
 
 END FUNCTION STGETPNTS

 !###======================================================================
 SUBROUTINE ST1FILLRESULTS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 !## get available folders in scenario project
 CALL WDIALOGSELECT(ID_DSCENTOOLTAB5)
 CALL UTL_IMODFILLMENU(IDF_MENU1,SCFFNAME(:INDEX(SCFFNAME,'.',.TRUE.)-1),'*','D',NRES,0,1)
 IF(NRES.GT.0)THEN
  DO I=1,NRES; RES(I)%CNAME=LISTNAME(I); ENDDO
  CALL UTL_IMODFILLMENU_DEAL()
 ENDIF
 
 END SUBROUTINE ST1FILLRESULTS

 !###======================================================================
 SUBROUTINE ST1FIELDS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: ITAB

 CALL WDIALOGSELECT(ID_DSCENTOOL)
 CALL WDIALOGGETTAB(IDF_TAB,ITAB)
 SELECT CASE (ITAB)
  CASE (ID_DSCENTOOLTAB1)
   CALL ST1FIELDS_STATE(ITAB,NWEL)
  CASE (ID_DSCENTOOLTAB2)
   CALL ST1FIELDS_STATE(ITAB,NCUT)
  CASE (ID_DSCENTOOLTAB3)
   CALL ST1FIELDS_STATE(ITAB,NOBS)
  CASE (ID_DSCENTOOLTAB4)
   CALL ST1FIELDS_STATE(ITAB,NMON)
  CASE (ID_DSCENTOOLTAB5)
   CALL ST1FIELDS_STATE(ITAB,NRES)
 END SELECT

 !## no computation available without wells or cuttings
 ITAB=1
 IF((NWEL.EQ.0.AND.NCUT.EQ.0))ITAB=0 !.OR.LEN_TRIM(SCFFNAME).EQ.0)ITAB=0
 CALL WDIALOGSELECT(ID_DSCENTOOL)
 CALL WDIALOGTABSTATE(IDF_TAB,ID_DSCENTOOLTAB5,ITAB)

! CALL WDIALOGSELECT(ID_DSCENTOOLTAB5)
! CALL WDIALOGFIELDSTATE(IDF_MENU2,MIN(1,NRES))
! CALL WDIALOGFIELDSTATE(IDF_LABEL2,MIN(1,NRES))

 END SUBROUTINE ST1FIELDS

 !###======================================================================
 SUBROUTINE ST1FIELDS_STATE(ITAB,N)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITAB,N
 INTEGER :: I,J

 CALL WDIALOGSELECT(ITAB)

 !## check number of active elements
 I=MAX(0,MIN(N,1))
 IF(I.GT.0)THEN
  IF(ITAB.EQ.ID_DSCENTOOLTAB5)THEN
   CALL WDIALOGGETMENU(IDF_MENU1,RES%IRES)
   I=MIN(1,SUM(RES(1:NRES)%IRES))
  ELSE
   CALL WDIALOGGETMENU(IDF_MENU1,I)
   I=MAX(0,MIN(1,I))
  ENDIF
 ENDIF
 CALL WDIALOGFIELDSTATE(ID_DELETE,I)
 IF(ITAB.EQ.ID_DSCENTOOLTAB5)THEN
  CALL WDIALOGFIELDSTATE(ID_MAP,I)
  J=I
  IF(J.EQ.1.AND.NOBS.EQ.0)J=0 
  CALL WDIALOGFIELDSTATE(ID_HISTOGRAM,J)
!  J=I
!  IF(J.EQ.1.AND.NMON.EQ.0)J=0
!  CALL WDIALOGFIELDSTATE(ID_PROFILE,J)
 ELSE
  CALL WDIALOGFIELDSTATE(ID_INFO,I)
 ENDIF
 
 END SUBROUTINE ST1FIELDS_STATE

 !###======================================================================
 SUBROUTINE ST1_PROPOPENSAVE(ICODE,ID,ITYPE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ICODE,ID,ITYPE
 CHARACTER(LEN=256) :: FNAME,LINE
 CHARACTER(LEN=10) :: CDATE
 CHARACTER(LEN=30) :: LABEL
 INTEGER :: IU,IOS,I,IDATE
 REAL :: X,Y,Z1,Z2

 FNAME=SCFFNAME(:INDEX(SCFFNAME,'\',.TRUE.)-1)

 IF(ICODE.EQ.ID_OPEN)THEN        !## open
  IF(.NOT.UTL_WSELECTFILE('Comma-Separated File (*.csv)|*.csv|',&
       LOADDIALOG+PROMPTON+APPENDEXT,FNAME,'Open CSV File'))RETURN
 ELSE                            !## save/saveas
  IF(.NOT.UTL_WSELECTFILE('Comma-Separated File (*.csv)|*.csv|',&
       SAVEDIALOG+PROMPTON+APPENDEXT,FNAME,'Open CSV File'))RETURN
 ENDIF

 IU=UTL_GETUNIT()

 !## open
 IF(ICODE.EQ.ID_OPEN)THEN
  CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ,DENYWRITE',FORM='FORMATTED',IOSTAT=IOS)
 !## write
 ELSE 
  CALL OSD_OPEN(IU,FILE=FNAME,STATUS='REPLACE',ACTION='WRITE,DENYREAD',FORM='FORMATTED',IOSTAT=IOS)
 ENDIF
 IF(ST1ERROR(IOS,'iMOD cannot open file '//TRIM(FNAME)).NE.0)RETURN

 CALL WDIALOGSELECT(ID)
 CALL WDIALOGUNDEFINED(RVALUE=NODATAGRID)

 SELECT CASE (ID)
  !## strength/measures
  CASE (ID_DSCENTOOL_PROPTAB1)

   !## date/duration switch
   CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO1)
   IF(ICODE.EQ.ID_OPEN)THEN
    !## (empty) grid
    CALL WDIALOGCLEARFIELD(IDF_GRID1)
    !## read labels
    READ(IU,*,IOSTAT=IOS)
    !## read all data from csv file
    I=0
    DO
     READ(IU,*,IOSTAT=IOS) IDATE,X
     IF(IOS.NE.0)EXIT
     I=I+1
     IF(I.GT.NROWQ)THEN
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Current iMOD version cannot read more than '//TRIM(ITOS(NROWQ))// &
       ' lines'//CHAR(13)//'Current table will be filled in with max '//TRIM(ITOS(NROWQ))//' values','Error/warning')
      EXIT
     ENDIF
     CDATE=JDATETOGDATE(UTL_IDATETOJDATE(IDATE))
     CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,I,CDATE)
     IF(X.EQ.NODATAGRID)EXIT
     CALL WGRIDPUTCELLREAL(IDF_GRID1,3,I,X)
    ENDDO
    CALL ST1_PROPUPDATEGRID(ITYPE)
   ELSE
    !## (recompute) grid
    CALL ST1_PROPUPDATEGRID(ITYPE)
    !## write labels
    IF(ITYPE.EQ.1)WRITE(IU,'(A)') 'date[yyyymmdd],q[m/hr]'
    IF(ITYPE.EQ.2)WRITE(IU,'(A)') 'date[yyyymmdd],measure[m+nap]'
    !## write all data into csv file
    DO I=1,NROWQ
     CALL WGRIDGETCELLSTRING(IDF_GRID1,1,I,CDATE)
     IF(LEN_TRIM(CDATE).EQ.0)EXIT
     CALL WGRIDGETCELLREAL(IDF_GRID1,3,I,X)
     IDATE=UTL_JDATETOIDATE(GDATETOJDATE(CDATE))
     LINE=TRIM(ITOS(IDATE))//','//TRIM(RTOS(X,'F',7))
     WRITE(IU,'(A)') TRIM(LINE)
    ENDDO
   ENDIF

  !## dimensions
  CASE (ID_DSCENTOOL_PROPTAB2)

   IF(ICODE.EQ.ID_OPEN)THEN
    !## (empty) grid
    CALL WDIALOGCLEARFIELD(IDF_GRID1)
    !## read labels
    READ(IU,*,IOSTAT=IOS)
    !## read all data from csv file
    I=0
    DO
     READ(IU,*,IOSTAT=IOS) LABEL,X,Y,Z1,Z2
     IF(IOS.NE.0)EXIT
     I=I+1
     IF(I.GT.NROWL)THEN
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Current iMOD version cannot read more than '//TRIM(ITOS(NROWL))// &
       ' lines'//CHAR(13)//'Current table will be filled in with max '//TRIM(ITOS(NROWL))//' values','Error/warning')
      EXIT
     ENDIF
     CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,I,LABEL)
     CALL WGRIDPUTCELLREAL(IDF_GRID1,2,I,X)
     CALL WGRIDPUTCELLREAL(IDF_GRID1,3,I,Y)
     CALL WGRIDPUTCELLREAL(IDF_GRID1,4,I,Z1)
     CALL WGRIDPUTCELLREAL(IDF_GRID1,5,I,Z2)
    ENDDO
    CALL IDFPLOT(1)

   ELSE
    !## write labels
    WRITE(IU,'(A)') 'label,z1,z2,zx,y'
    !## write all data into csv file
    DO I=1,NROWL
     CALL WGRIDGETCELLSTRING(IDF_GRID1,1,I,LABEL)
     IF(LEN_TRIM(LABEL).EQ.0)EXIT
     CALL WGRIDGETCELLREAL(IDF_GRID1,2,I,Z1)
     IF(Z1.EQ.NODATAGRID)EXIT
     CALL WGRIDGETCELLREAL(IDF_GRID1,3,I,Z2)
     IF(Z2.EQ.NODATAGRID)EXIT
     CALL WGRIDGETCELLREAL(IDF_GRID1,4,I,X)
     IF(X.EQ.NODATAGRID)EXIT
     CALL WGRIDGETCELLREAL(IDF_GRID1,5,I,Y)
     IF(Y.EQ.NODATAGRID)EXIT
     LINE='"'//TRIM(LABEL)//'",'//TRIM(RTOS(Z1,'F',2))//','//TRIM(RTOS(Z2,'F',2))//','// &
                                  TRIM(RTOS(X,'F',2)) //','//TRIM(RTOS(Y,'F',2))
     WRITE(IU,'(A)') TRIM(LINE)
    ENDDO
   ENDIF

 END SELECT

 CLOSE(IU)

 END SUBROUTINE ST1_PROPOPENSAVE

 !###======================================================================
 SUBROUTINE ST1_PROPUPDATEGRID(ITYPE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 INTEGER :: I,J,K,ITIME
 REAL :: DELT,Q
 CHARACTER(LEN=10) :: CDATE

 CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB1)
 CALL WDIALOGUNDEFINED(RVALUE=NODATAGRID)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,ITIME)

 !## if duration .. compute dates
 IF(ITIME.EQ.2)THEN

  !## initialize date in grid
  CALL WDIALOGGETSTRING(IDF_STRING2,CDATE)
  J=GDATETOJDATE(CDATE)  !## correct cdate whenever it exceeds possible dates!!!!

  CALL WDIALOGPUTSTRING(IDF_STRING2,CDATE)
  CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,1,CDATE)

  DO I=1,NROWQ-1
   CALL WGRIDGETCELLREAL(IDF_GRID1,2,I,DELT)
   IF(DELT.LE.0.0)EXIT
   J=J+INT(DELT)
   CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,I+1,JDATETOGDATE(J))
  END DO

 ENDIF

 !## compute all julian dates
 DO I=1,NROWQ
  CALL WGRIDGETCELLSTRING(IDF_GRID1,1,I,CDATE)
  IF(LEN_TRIM(CDATE).EQ.0)EXIT
  J=GDATETOJDATE(CDATE)  !## correct cdate whenever it exceeds possible dates!!!!
  CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,I,CDATE)
  CALL WGRIDPUTCELLINTEGER(IDF_GRID1,4,I,J)
 ENDDO
 !## clear the rest
 DO J=I,NROWQ
  DO K=1,4; CALL WGRIDCLEARCELL(IDF_GRID1,K,I); END DO
 END DO

 !## sort on julian dates
 CALL WGRIDSORT(IDF_GRID1,4)

 !## computes length in between
 CALL WGRIDGETCELLINTEGER(IDF_GRID1,4,1,J)
 DO I=2,NROWQ-1
  CALL WGRIDGETCELLINTEGER(IDF_GRID1,4,I,K)
  IF(K.LE.0)EXIT
  DELT=REAL(K-J)
  CALL WGRIDPUTCELLREAL(IDF_GRID1,2,I-1,DELT)
  J=K
  IF(ITYPE.EQ.1)THEN
   !## initialise extraction rate
   CALL WGRIDGETCELLREAL(IDF_GRID1,3,I-1,Q)
   IF(Q.EQ.NODATAGRID)CALL WGRIDPUTCELLREAL(IDF_GRID1,3,I-1,0.0)
  ENDIF
 END DO

 END SUBROUTINE ST1_PROPUPDATEGRID

!###======================================================================
 SUBROUTINE ST1_PROPFIELDS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: J,K,ITIME
 INTEGER,DIMENSION(0:1) :: ICOLOR

 ICOLOR(0)=WRGB(255,0,0)
 ICOLOR(1)=-1

 CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB1)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,ITIME)
 !## input in dates
 IF(ITIME.EQ.1)THEN
  J=1
  K=0
 !## input in durations
 ELSE
  J=0
  K=1
 ENDIF

 CALL WDIALOGFIELDSTATE(IDF_STRING2,K)

 CALL WGRIDSTATE(IDF_GRID1,1,J)
 CALL WGRIDSTATE(IDF_GRID1,2,K)
 CALL WGRIDCOLOURCOLUMN(IDF_GRID1,1,-1,ICOLOR(J))
 CALL WGRIDCOLOURCOLUMN(IDF_GRID1,2,-1,ICOLOR(K))

 END SUBROUTINE ST1_PROPFIELDS

 !###======================================================================
 SUBROUTINE ST1_PROPGRAPH(ITYPE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE !## 1=histogram 2=connected lines
 INTEGER :: I,NP,IDATE

 !## update grid ...
 CALL ST1_PROPUPDATEGRID(ITYPE)

 CALL GRAPH_ALLOCATE(1,1)
 ALLOCATE(GRAPH(1,1)%RX(NROWQ))
 ALLOCATE(GRAPH(1,1)%RY(NROWQ))

 GRAPH(1,1)%RX=0.0
 GRAPH(1,1)%RY=0.0
 NP=0
 DO I=1,NROWQ
  IF(WINFOGRIDCELL(IDF_GRID1,4,I,GRIDCELLDEFINED).EQ.1)THEN
   NP=NP+1
   !## date
   CALL WGRIDGETCELLINTEGER(IDF_GRID1,4,I,IDATE)
   GRAPH(1,1)%RX(NP)=REAL(IDATE)
  ELSE
   EXIT
  ENDIF
  IF(WINFOGRIDCELL(IDF_GRID1,4,I,GRIDCELLDEFINED).EQ.1)THEN
   !## z-value (measure-q3/hr)
   CALL WGRIDGETCELLREAL(IDF_GRID1,3,I,GRAPH(1,1)%RY(NP))
  ENDIF
 END DO
 IF(NP.LE.1)THEN
  CALL GRAPH_DEALLOCATE()
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Nothing to draw!','Information')
  RETURN
 ENDIF
 !## make sure last is equal to final-least
 GRAPH(1,1)%RY(NP)=GRAPH(1,1)%RY(NP-1)
 GRAPH(1,1)%NP=NP
 IF(ITYPE.EQ.2)THEN
  GRAPH(1,1)%NP=GRAPH(1,1)%NP-1
 ENDIF
 GRAPH(1,1)%ICLR=WRGB(56,180,176)
!DO I=1,NP
!WRITE(*,*) I,GRAPH(1,1)%RX(I),GRAPH(1,1)%RY(I)
!END DO

 CALL WDIALOGGETSTRING(IDF_STRING1,GRAPH(1,1)%LEGTXT)

 GRAPH(1,1)%GTYPE=ITYPE
 IF(ITYPE.EQ.1)CALL GRAPH_PLOT('Date','Q-Rate (m3/hr)',.TRUE.,.FALSE.)
 IF(ITYPE.EQ.2)CALL GRAPH_PLOT('Date','Measure (m)'   ,.TRUE.,.FALSE.)
 
 CALL GRAPH_DEALLOCATE

 END SUBROUTINE ST1_PROPGRAPH

END MODULE MOD_SCENTOOL_UTL