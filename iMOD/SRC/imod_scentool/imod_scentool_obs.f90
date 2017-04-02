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
MODULE MOD_SCENTOOL_OBS

USE WINTERACTER
USE RESOURCE
USE MODPLOT, ONLY : MPW
USE MOD_SCENTOOL_PAR
USE MOD_SCENTOOL_UTL, ONLY : ST_DRAWPNTS,ST1FIELDS,ST_SYMBOLDRAW,ST1_PROPUPDATEGRID,ST1_PROPFIELDS,ST1_PROPGRAPH, &
              ST1_PROPOPENSAVE
USE MOD_UTL, ONLY : UTL_HIDESHOWDIALOG,JDATETOGDATE,ITOS,RTOS,GDATETOJDATE,UTL_GETUNIT,UTL_JDATETOIDATE, &
                    UTL_CREATEDIR,UTL_WSELECTFILE,UTL_IDATETOJDATE,UTL_IDATETOJDATE
USE MOD_IDF, ONLY : IDFIROWICOL,IDFGETVAL,IDFREAD
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_IPF, ONLY : IPFPLOTLABEL

CHARACTER(LEN=10),PRIVATE :: CDATE
INTEGER,PRIVATE :: IMODE

CONTAINS

 !###======================================================================
 SUBROUTINE STOBS1INIT(IOPT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOPT

 IMODE=IOPT

 IF(IOPT.EQ.ID_ADD)THEN
  IF(NOBS.EQ.MAXNOBS)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You cannot add another OBS SYSTEM in the current configuration.'//CHAR(13)// &
     'Currently the maximum OBS SYSTEMS is '//TRIM(ITOS(MAXNOBS))//'.'//CHAR(13)// &
     'Increase/add the keyword MAXNOBS= in the SCENTOOL file to increase the number of OBS SYSTEMS allowed','Information')
   IMODE=0
   CALL STOBS1CLOSE()
   IMODE=IOPT
  ENDIF
  NOBS           =NOBS+1
  IOBS           =NOBS
  OBS(IOBS)%CNAME='Observation '//TRIM(ITOS(IOBS))
  OBS(IOBS)%NLOC =0
  OBS(IOBS)%NZ   =0
  OBS(IOBS)%ILOCT=1
  OBS(IOBS)%ICLR =WRGB(255,0,0)
  OBS(IOBS)%ISYMBOL=7
  ALLOCATE(OBS(IOBS)%Z(NROWQ))
  ALLOCATE(OBS(IOBS)%LOC(NROWL))
 ELSE
  CALL WDIALOGSELECT(ID_DSCENTOOLTAB3)
  CALL WDIALOGGETMENU(IDF_MENU1,IOBS)
 ENDIF

 CALL UTL_HIDESHOWDIALOG(ID_DSCENTOOL,0)

 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(ID_SCENTOOL,1,0)

 CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB1)
 CALL WDIALOGTITLE('Observation')
 CALL WDIALOGPUTSTRING(IDF_LABEL1,'Observation name:')
 CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO2) !## start enter durations
 
 !## put grid columnlabels
 CALL WGRIDLABELCOLUMN(IDF_GRID1,1,'Date')
 CALL WGRIDLABELCOLUMN(IDF_GRID1,2,'Duration')
 CALL WGRIDLABELCOLUMN(IDF_GRID1,3,'Measure (m)')  
 
 !## turn off irrelevant options
 CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB2)
 CALL WDIALOGFIELDSTATE(IDF_GROUP2,3)
 CALL WDIALOGFIELDSTATE(IDF_RADIO3,3)
 CALL WDIALOGFIELDSTATE(IDF_RADIO4,3)
 
 CALL STOBS1PUTFIELDS()
 CALL ST1_PROPUPDATEGRID(2)
 CALL ST1_PROPFIELDS()

 !## initialize date in grid
 CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB1)
 IF(OBS(IOBS)%NZ.EQ.0)THEN
  CALL WDIALOGGETSTRING(IDF_STRING2,CDATE)
  CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,1,CDATE)
 ENDIF

 CALL WDIALOGSELECT(ID_DSCENTOOL_PROP)
 CALL WDIALOGSETTAB(IDF_TAB,ID_DSCENTOOL_PROPTAB1)
 CALL WDIALOGTITLE('Observation Wells')
 CALL WDIALOGSHOW(-1,-1,0,2)

 END SUBROUTINE STOBS1INIT

 !###======================================================================
 SUBROUTINE STOBS1MAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER :: IRGB
 
 CALL WDIALOGSELECT(ID_DSCENTOOL_PROP)

 SELECT CASE (MESSAGE%WIN)

  !## main dialog
  CASE (ID_DSCENTOOL_PROP)
   SELECT CASE (ITYPE)
    CASE (PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      !## quit OBSl editing
      CASE (IDCANCEL)
       CALL STOBS1CLOSE()
     END SELECT
    CASE (TABCHANGED)
     SELECT CASE (MESSAGE%VALUE2)
      CASE (ID_DSCENTOOL_PROPTAB3)
       CALL ST_SYMBOLDRAW(ID_DSCENTOOL_PROPTAB3,IDF_PICTURE1,IDF_MENU1,IDF_INTEGER1)
     END SELECT
   END SELECT

  !## OBSl strengths
  CASE (ID_DSCENTOOL_PROPTAB1)
   SELECT CASE (ITYPE)
    CASE (FIELDCHANGED)
     CALL ST1_PROPFIELDS()
    CASE (PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (ID_OPEN,ID_SAVEAS)
       CALL ST1_PROPOPENSAVE(MESSAGE%VALUE1,ID_DSCENTOOL_PROPTAB1,2)
      CASE (ID_GRAPH)
       CALL ST1_PROPGRAPH(2)
      CASE (ID_CALC)
       CALL ST1_PROPUPDATEGRID(2)
     END SELECT
   END SELECT

  !## OBSl locations
  CASE (ID_DSCENTOOL_PROPTAB2)
   SELECT CASE (ITYPE)
    CASE (FIELDCHANGED)
!      CALL STOBS1FIELDS()
    CASE (PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (ID_OPEN,ID_SAVEAS)
       CALL ST1_PROPOPENSAVE(MESSAGE%VALUE1,ID_DSCENTOOL_PROPTAB2,2)
      CASE (ID_MOVE,ID_DRAW,ID_DELETE)
       CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB2)
       CALL WDIALOGFIELDSTATE(ID_OPEN,2)
       CALL WDIALOGFIELDSTATE(ID_SAVEAS,2)
       IF(MESSAGE%VALUE1.EQ.ID_MOVE)THEN
        CALL WDIALOGFIELDSTATE(ID_DRAW,2)
        CALL WDIALOGFIELDSTATE(ID_DELETE,2)
        CALL ST_DRAWPNTS(ID_DSCENTOOL_PROPTAB2,IDF_GRID1,(/4,5/),2,OBS(IOBS)%ISYMBOL,OBS(IOBS)%ICLR,2)  !## move
       ELSEIF(MESSAGE%VALUE1.EQ.ID_DELETE)THEN
        CALL WDIALOGFIELDSTATE(ID_DRAW,2)
        CALL WDIALOGFIELDSTATE(ID_MOVE,2)
        CALL ST_DRAWPNTS(ID_DSCENTOOL_PROPTAB2,IDF_GRID1,(/4,5/),3,OBS(IOBS)%ISYMBOL,OBS(IOBS)%ICLR,2)  !## delete
       ELSE
        CALL WDIALOGFIELDSTATE(ID_MOVE,2)
        CALL WDIALOGFIELDSTATE(ID_DELETE,2)
        CALL ST_DRAWPNTS(ID_DSCENTOOL_PROPTAB2,IDF_GRID1,(/4,5/),1,OBS(IOBS)%ISYMBOL,OBS(IOBS)%ICLR,2)  !## add
       ENDIF
       CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB2)
       CALL WDIALOGFIELDSTATE(ID_OPEN,1)
       CALL WDIALOGFIELDSTATE(ID_SAVEAS,1)
       CALL WDIALOGFIELDSTATE(ID_MOVE,1)
       CALL WDIALOGFIELDSTATE(ID_DRAW,1)
       CALL WDIALOGFIELDSTATE(ID_DELETE,1)
     END SELECT
   END SELECT

  !## OBSl display options
  CASE (ID_DSCENTOOL_PROPTAB3)
   SELECT CASE (ITYPE)
    CASE (RESIZE,EXPOSE)
     CALL ST_SYMBOLDRAW(ID_DSCENTOOL_PROPTAB3,IDF_PICTURE1,IDF_MENU1,IDF_INTEGER1)
    CASE (FIELDCHANGED)
     SELECT CASE (MESSAGE%VALUE2)
      CASE (IDF_MENU1,IDF_REAL1)
       CALL ST_SYMBOLDRAW(ID_DSCENTOOL_PROPTAB3,IDF_PICTURE1,IDF_MENU1,IDF_INTEGER1)
      END SELECT   !IDD,IDF,IDM,IDC,IDS)
    CASE(PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (ID_COLOUR)
       CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB3)
       CALL WDIALOGGETINTEGER(IDF_INTEGER1,IRGB)
       CALL WSELECTCOLOUR(IRGB)
       IF(WINFODIALOG(4).EQ.1)THEN
        CALL WDIALOGPUTINTEGER(IDF_INTEGER1,IRGB)
        CALL ST_SYMBOLDRAW(ID_DSCENTOOL_PROPTAB3,IDF_PICTURE1,IDF_MENU1,IDF_INTEGER1)
       ENDIF
     END SELECT
  END SELECT
 END SELECT

 END SUBROUTINE STOBS1MAIN

 !###======================================================================
 SUBROUTINE STOBS1CLOSE()
 !###======================================================================
 IMPLICIT NONE

 IF(IMODE.NE.0)THEN
  CALL WMESSAGEBOX(YESNOCANCEL,QUESTIONICON,COMMONNO,'Do you want to save any adjustments ?','Question')
  !## save and quit
  IF(WINFODIALOG(4).EQ.1)THEN
   CALL ST1_PROPUPDATEGRID(2)
   IF(.NOT.STOBS1GETFIELDS())RETURN   !## get data from grid, if not correct, return - not closing!
  !## no save and quit
  ELSEIF(WINFODIALOG(4).EQ.2)THEN
   IF(IMODE.EQ.ID_ADD)THEN
    IOBS=IOBS-1
    NOBS=NOBS-1
   ENDIF
  ENDIF
  !## not canceling
  IF(WINFODIALOG(4).EQ.0)RETURN
 ENDIF

 CALL WDIALOGSELECT(ID_DSCENTOOL_PROP)
 CALL WDIALOGHIDE()

 CALL STOBS1TITLE()
 IOBS=0

 CALL UTL_HIDESHOWDIALOG(ID_DSCENTOOL,2)
 CALL WMENUSETSTATE(ID_SCENTOOL,1,1)
 CALL ST1FIELDS()
 CALL IDFPLOT(1)
 
 END SUBROUTINE STOBS1CLOSE

 !###======================================================================
 SUBROUTINE STOBS1TITLE()
 !###======================================================================
 IMPLICIT NONE
 
 CALL WDIALOGSELECT(ID_DSCENTOOLTAB3)
 CALL WDIALOGTITLE('Observation Wells ('//TRIM(ITOS(NOBS))//')')
 IF(IOBS.NE.0)CALL WDIALOGPUTMENU(IDF_MENU1,OBS%CNAME,NOBS,IOBS)
 
 END SUBROUTINE STOBS1TITLE

 !###======================================================================
 SUBROUTINE ST1SAVELOADOBS(IU,ICODE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,ICODE
 CHARACTER(LEN=256) :: LINE
 INTEGER :: I,J,IOS,IDATE

 IF(ICODE.EQ.ID_SAVEAS.OR.ICODE.EQ.ID_SAVE)THEN

  LINE=TRIM(ITOS(NOBS))
  WRITE(IU,'(A)') TRIM(LINE)//' !## nobs[i]'
  DO I=1,NOBS
   WRITE(IU,'(A)') '"'//TRIM(OBS(I)%CNAME)//'" !## name of observation system[c]'
   LINE=TRIM(ITOS(OBS(I)%NZ))
   WRITE(IU,'(A)') TRIM(LINE)//' !## nmeasures[i]'
   DO J=1,OBS(I)%NZ
    LINE=TRIM(ITOS(UTL_JDATETOIDATE(OBS(I)%Z(J)%IDATE)))//','//TRIM(RTOS(OBS(I)%Z(J)%MEASURE,'F',2))  !## m3/day -> m3/hr
    WRITE(IU,'(A)') TRIM(LINE)//' !## date[yyyymmdd],measure[r]'
   END DO
   LINE=TRIM(ITOS(OBS(I)%NLOC))//','//TRIM(ITOS(OBS(I)%ILOCT))//','//  &
        TRIM(ITOS(OBS(I)%ISYMBOL))//','//TRIM(ITOS(OBS(I)%ICLR)) !## number of locations, iloct:1=msl 2=surface
   WRITE(IU,'(A)') TRIM(LINE)//' !## nloc[i],ltype[i],isymbol[i],iclr[i]'
   DO J=1,OBS(I)%NLOC
    LINE='"'//TRIM(OBS(I)%LOC(J)%ID)//'",'//TRIM(RTOS(OBS(I)%LOC(J)%Z1,'F',2))//','// &
                                            TRIM(RTOS(OBS(I)%LOC(J)%Z2,'F',2))//','// &
                                            TRIM(RTOS(OBS(I)%LOC(J)%X ,'F',2))//','// &
                                            TRIM(RTOS(OBS(I)%LOC(J)%Y ,'F',2))
    WRITE(IU,'(A)') TRIM(LINE)//' !## id[c],z1[r],z2[r],x[r],y[r]'
   END DO
  END DO

 ELSEIF(ICODE.EQ.ID_OPEN)THEN

  READ(IU,*,IOSTAT=IOS) NOBS
  IF(IOS.NE.0)NOBS=0
  DO I=1,NOBS
   READ(IU,*) OBS(I)%CNAME
   READ(IU,*) OBS(I)%NZ
   OBS(I)%ITYPE=1 !## not used
!   OBS(I)%NZ=OBS(I)%NZ+1
   IF(.NOT.ASSOCIATED(OBS(I)%Z)) ALLOCATE(OBS(I)%Z(NROWQ))
   DO J=1,OBS(I)%NZ
    READ(IU,*) IDATE,OBS(I)%Z(J)%MEASURE
    OBS(I)%Z(J)%IDATE=UTL_IDATETOJDATE(IDATE)
   END DO
   READ(IU,*) OBS(I)%NLOC,OBS(I)%ILOCT,OBS(I)%ISYMBOL,OBS(I)%ICLR
   IF(.NOT.ASSOCIATED(OBS(I)%LOC))ALLOCATE(OBS(I)%LOC(NROWL))
   DO J=1,OBS(I)%NLOC
    READ(IU,*) OBS(I)%LOC(J)%ID,OBS(I)%LOC(J)%Z1,OBS(I)%LOC(J)%Z2,OBS(I)%LOC(J)%X,OBS(I)%LOC(J)%Y
   ENDDO
  END DO

 ENDIF

 END SUBROUTINE ST1SAVELOADOBS

 !###======================================================================
 SUBROUTINE ST1PLOTOBS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,IROW,ICLR,IMARKER
 REAL,PARAMETER :: TSIZE=0.01
 REAL :: X,Y,Z,TWIDTH,THEIGTH
 CHARACTER(LEN=50),DIMENSION(5) :: CLABEL,CVALUE
 CHARACTER(LEN=10),DIMENSION(2) :: CUNIT
 INTEGER,DIMENSION(5) :: IOPTION
 INTEGER :: IALL,JOBS,ILABEL
 DATA CUNIT/'m+MSL','m+SLevel'/

 CALL WDIALOGSELECT(ID_DSCENTOOLTAB3)
 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,IALL)
 CALL WDIALOGGETCHECKBOX(IDF_CHECK2,ILABEL)
 CALL WDIALOGGETMENU(IDF_MENU1,JOBS)

 CLABEL=''
 IF(ILABEL.EQ.1)THEN
  CLABEL(1)='ID'
  CLABEL(2)='Top Screen'
  CLABEL(3)='Bot Screen'
  CLABEL(4)='X-coord.'
  CLABEL(5)='Y-coord.'
 ENDIF

 !## overrule in case iOBS.ne.0
 IF(IOBS.NE.0)THEN
  IALL=0
  JOBS=0
 ENDIF
 
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,I)
 IOPTION=1
 SELECT CASE (I)
  CASE (1) !## no labeling
   IOPTION=0
  CASE (2) !## id
   IOPTION(2:5)=0
  CASE (3) !## id+filter screens
   IOPTION(4:5)=0
  CASE (4) !## all
   IOPTION=1
 END SELECT

 DO I=1,NOBS

  TWIDTH =TSIZE/2.0
  THEIGTH=TWIDTH*(0.03333333/0.013333)
  THEIGTH=THEIGTH*WINFOGRREAL(GRAPHICSRATIO)

  !## currently selected OBSl-system - draw from grid-field
  IF(I.EQ.IOBS)THEN
   CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB3)
   CALL WDIALOGGETINTEGER(IDF_INTEGER1,ICLR)
   CALL WDIALOGGETMENU(IDF_MENU1,IMARKER)
   CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB2)
   DO IROW=1,NROWL
    X=0.0
    Y=0.0
    CVALUE=''
    IF(WINFOGRIDCELL(IDF_GRID1,1,IROW,GRIDCELLDEFINED).EQ.1)CALL WGRIDGETCELLSTRING(IDF_GRID1,1,IROW,CVALUE(1)) !## id
    IF(WINFOGRIDCELL(IDF_GRID1,2,IROW,GRIDCELLDEFINED).EQ.1)THEN
     CALL WGRIDGETCELLREAL(IDF_GRID1,2,IROW,Z)   !## z1
     CVALUE(2)=TRIM(RTOS(Z,'F',2))//' '//TRIM(CUNIT(OBS(I)%ILOCT))
    ENDIF
    IF(WINFOGRIDCELL(IDF_GRID1,3,IROW,GRIDCELLDEFINED).EQ.1)THEN
     CALL WGRIDGETCELLREAL(IDF_GRID1,3,IROW,Z)   !## z2
     CVALUE(3)=TRIM(RTOS(Z,'F',2))//' '//TRIM(CUNIT(OBS(I)%ILOCT))
    ENDIF
    IF(WINFOGRIDCELL(IDF_GRID1,4,IROW,GRIDCELLDEFINED).EQ.1)THEN
     CALL WGRIDGETCELLREAL(IDF_GRID1,4,IROW,X) !## x
     CVALUE(4)=TRIM(RTOS(X,'F',2))//' m'
    ENDIF
    IF(WINFOGRIDCELL(IDF_GRID1,5,IROW,GRIDCELLDEFINED).EQ.1)THEN
     CALL WGRIDGETCELLREAL(IDF_GRID1,5,IROW,Y) !## y
     CVALUE(5)=TRIM(RTOS(Y,'F',2))//' m'
    ENDIF
    CALL WGRTEXTFONT(WIDTH=TSIZE,HEIGHT=TSIZE*WINFOGRREAL(GRAPHICSRATIO))
    CALL IGRCOLOURN(ICLR)       !OBS(I)%ICLR)
    CALL IGRMARKER(X,Y,IMARKER) !OBS(I)%ISYMBOL)
    CALL IPFPLOTLABEL(X,Y,CVALUE,IOPTION,5,TWIDTH,THEIGTH,CLABEL,.FALSE.,-1)
   ENDDO
  ELSE
   DO IROW=1,OBS(I)%NLOC
    IF(IALL.EQ.1.OR.(IALL.EQ.0.AND.JOBS.EQ.I))THEN
     CVALUE(1)=OBS(I)%LOC(IROW)%ID
     CVALUE(2)=TRIM(RTOS(OBS(I)%LOC(IROW)%Z1,'F',2))//' '//TRIM(CUNIT(OBS(I)%ILOCT))
     CVALUE(3)=TRIM(RTOS(OBS(I)%LOC(IROW)%Z2,'F',2))//' '//TRIM(CUNIT(OBS(I)%ILOCT))
     CVALUE(4)=TRIM(RTOS(OBS(I)%LOC(IROW)%X ,'F',2))//' m'
     CVALUE(5)=TRIM(RTOS(OBS(I)%LOC(IROW)%Y ,'F',2))//' m'
     CALL IPFPLOTLABEL(OBS(I)%LOC(IROW)%X,OBS(I)%LOC(IROW)%Y,CVALUE,IOPTION,5,TWIDTH,THEIGTH,CLABEL,.FALSE.,-1)
    ENDIF
    CALL IGRCOLOURN(OBS(I)%ICLR)
    CALL WGRTEXTFONT(WIDTH=TSIZE,HEIGHT=TSIZE*WINFOGRREAL(GRAPHICSRATIO))
    CALL IGRMARKER(OBS(I)%LOC(IROW)%X,OBS(I)%LOC(IROW)%Y,OBS(I)%ISYMBOL)
   ENDDO
  ENDIF
 END DO

 END SUBROUTINE ST1PLOTOBS

 !###======================================================================
 SUBROUTINE ST1SIMBOXOBS(XMIN,YMIN,XMAX,YMAX)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(INOUT) :: XMIN,YMIN,XMAX,YMAX
 INTEGER :: I,J

 DO I=1,NOBS
  DO J=1,OBS(I)%NLOC
   XMIN=MIN(XMIN,OBS(I)%LOC(J)%X)
   XMAX=MAX(XMAX,OBS(I)%LOC(J)%X)
   YMIN=MIN(YMIN,OBS(I)%LOC(J)%Y)
   YMAX=MAX(YMAX,OBS(I)%LOC(J)%Y)
  END DO
 END DO

 END SUBROUTINE ST1SIMBOXOBS

 !###======================================================================
 SUBROUTINE STOBS1DELETE()
 !###======================================================================
 IMPLICIT NONE

 CALL WDIALOGSELECT(ID_DSCENTOOLTAB3)
 CALL WDIALOGGETMENU(IDF_MENU1,IOBS)

 CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to delete the definition for:'//CHAR(13)// &
  TRIM(OBS(IOBS)%CNAME)//' ?','Question')
 IF(WINFODIALOG(4).NE.1)RETURN

 END SUBROUTINE STOBS1DELETE

 !###======================================================================
 SUBROUTINE STOBS1PUTFIELDS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,IY,IM,ID

 !## clear fields
 CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB1)
 DO I=1,NROWQ
  DO J=1,4; CALL WGRIDCLEARCELL(IDF_GRID1,J,I); END DO
 END DO
 CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB2)
 DO I=1,NROWL
  DO J=1,5; CALL WGRIDCLEARCELL(IDF_GRID1,J,I); END DO
 ENDDO

 CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB1)
 CALL WDIALOGPUTSTRING(IDF_STRING1,OBS(IOBS)%CNAME)

 IF(OBS(IOBS)%NZ.GT.0)THEN
  CALL WDIALOGPUTSTRING(IDF_STRING2,JDATETOGDATE(OBS(IOBS)%Z(1)%IDATE))
 ELSE
  CALL IOSDATE(IY,IM,ID)
  CALL WDIALOGPUTSTRING(IDF_STRING2,TRIM(ITOS(ID))//'/'//TRIM(ITOS(IM))//'/'//TRIM(ITOS(IY)))
 ENDIF

 DO I=1,OBS(IOBS)%NZ
  CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,I,JDATETOGDATE(OBS(IOBS)%Z(I)%IDATE))
!  IF(I.LT.OBS(IOBS)%NZ)
  CALL WGRIDPUTCELLREAL(IDF_GRID1,3,I,OBS(IOBS)%Z(I)%MEASURE) 
  CALL WGRIDPUTCELLINTEGER(IDF_GRID1,4,I,OBS(IOBS)%Z(I)%IDATE)
 END DO
 CALL WGRIDSETCELL(IDF_GRID1,1,1)!,IPOS)

 CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB2)

 DO I=1,OBS(IOBS)%NLOC
  CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,I,OBS(IOBS)%LOC(I)%ID)
  CALL WGRIDPUTCELLREAL(IDF_GRID1,2,I,OBS(IOBS)%LOC(I)%Z1)
  CALL WGRIDPUTCELLREAL(IDF_GRID1,3,I,OBS(IOBS)%LOC(I)%Z2)
  CALL WGRIDPUTCELLREAL(IDF_GRID1,4,I,OBS(IOBS)%LOC(I)%X)
  CALL WGRIDPUTCELLREAL(IDF_GRID1,5,I,OBS(IOBS)%LOC(I)%Y)
 END DO
 CALL WGRIDSETCELL(IDF_GRID1,1,1)!,IPOS)

 IF(OBS(IOBS)%ILOCT.EQ.1)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO1)
 IF(OBS(IOBS)%ILOCT.EQ.2)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO2)
 IF(OBS(IOBS)%ITYPE.EQ.1)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO3)
 IF(OBS(IOBS)%ITYPE.EQ.2)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO4)

 CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB3)
 CALL WDIALOGPUTOPTION(IDF_MENU1,OBS(IOBS)%ISYMBOL)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,OBS(IOBS)%ICLR)

 END SUBROUTINE STOBS1PUTFIELDS

 !###======================================================================
 LOGICAL FUNCTION STOBS1GETFIELDS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 STOBS1GETFIELDS=.FALSE.

 CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB1)
 CALL WDIALOGGETSTRING(IDF_STRING1,OBS(IOBS)%CNAME)
 IF(LEN_TRIM(OBS(IOBS)%CNAME).EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should specify a name for the OBS-system at least','Warning')
  RETURN
 ENDIF

 CALL WDIALOGUNDEFINED(RVALUE=NODATAGRID)

 OBS(IOBS)%NZ=0
 DO I=1,NROWQ
  CALL WGRIDGETCELLSTRING(IDF_GRID1,1,I,CDATE)
  !## empty
  IF(LEN_TRIM(CDATE).EQ.0)EXIT
  CALL WGRIDGETCELLREAL(IDF_GRID1,3,I,OBS(IOBS)%Z(I)%MEASURE)
  OBS(IOBS)%Z(I)%IDATE=GDATETOJDATE(CDATE)
  !## date conversion went wrong
  IF(OBS(IOBS)%Z(I)%IDATE.EQ.0)EXIT
  OBS(IOBS)%NZ=OBS(IOBS)%NZ+1
 END DO

 CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB2)

 OBS(IOBS)%NLOC=0

 DO I=1,NROWL
  CALL WGRIDGETCELLSTRING(IDF_GRID1,1,I,OBS(IOBS)%LOC(I)%ID)
  IF(LEN_TRIM(OBS(IOBS)%LOC(I)%ID).EQ.0)EXIT
  CALL WGRIDGETCELLREAL(IDF_GRID1,2,I,OBS(IOBS)%LOC(I)%Z1)
  IF(OBS(IOBS)%LOC(I)%Z1.EQ.NODATAGRID)EXIT
  CALL WGRIDGETCELLREAL(IDF_GRID1,3,I,OBS(IOBS)%LOC(I)%Z2)
  IF(OBS(IOBS)%LOC(I)%Z2.EQ.NODATAGRID)EXIT

  IF(OBS(IOBS)%LOC(I)%Z1.LT.OBS(IOBS)%LOC(I)%Z2)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should specify top filter to be above the bottom filter'//CHAR(13)// &
    'Check Observation number '//TRIM(ITOS(I)),'Warning')
   RETURN
  ENDIF

  CALL WGRIDGETCELLREAL(IDF_GRID1,4,I,OBS(IOBS)%LOC(I)%X)
  IF(OBS(IOBS)%LOC(I)%X.EQ.NODATAGRID)EXIT
  CALL WGRIDGETCELLREAL(IDF_GRID1,5,I,OBS(IOBS)%LOC(I)%Y)
  IF(OBS(IOBS)%LOC(I)%Y.EQ.NODATAGRID)EXIT
  OBS(IOBS)%NLOC=OBS(IOBS)%NLOC+1
 END DO

 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,OBS(IOBS)%ILOCT)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO3,OBS(IOBS)%ITYPE)

 CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB3)
 CALL WDIALOGGETMENU(IDF_MENU1,OBS(IOBS)%ISYMBOL)
 CALL WDIALOGGETINTEGER(IDF_INTEGER1,OBS(IOBS)%ICLR)

 STOBS1GETFIELDS=.TRUE.

 END FUNCTION STOBS1GETFIELDS
 
END MODULE MOD_SCENTOOL_OBS
