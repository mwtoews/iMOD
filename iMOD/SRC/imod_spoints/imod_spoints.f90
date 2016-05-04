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
MODULE MOD_SPOINTS

USE WINTERACTER
USE RESOURCE
USE MOD_COLOURS
USE MODPLOT
USE MOD_POLYGON, ONLY : POLYGON1MAIN,POLYGON1CREATESHAPE
USE MOD_POLYGON_PAR
USE MOD_POLYGON_DRAW, ONLY : POLYGON1DRAWSHAPE
USE MOD_POLYGON_UTL, ONLY : POLYGON1CLOSE,POLYGON1INIT,POLYGON1FIELDS,POLYGON1IMAGES
USE IMODVAR, ONLY : IDIAGERROR,PI
USE MOD_SCEN_PAR
USE MOD_SPOINTS_PAR
USE MOD_UTL, ONLY : ITOS,RTOS,UTL_GETUNIT,IDFPLOT1BITMAP,IDFPLOT2BITMAP,UTL_CREATEDIR,UTL_IMODFILLMENU, &
         UTL_WSELECTFILE,UTL_INSIDEPOLYGON

USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_OSD, ONLY : OSD_OPEN

INTEGER,PRIVATE :: ISHAPE

CONTAINS

 !###======================================================================
 SUBROUTINE STARTP1MAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE
 CHARACTER(LEN=256) :: IDFNAME
 INTEGER :: IQUIT

 CALL WDIALOGSELECT(MESSAGE%WIN)
 SELECT CASE(ITYPE)

  CASE (TABCHANGED)
   SELECT CASE (MESSAGE%VALUE2)
    CASE (ID_DSPTAB1)
     IF(.NOT.STARTP1FIELDS_GETTAB2(IQUIT))THEN
      !## stay on the same tab!
      CALL WDIALOGSELECT(ID_DSPOINTS)
      CALL WDIALOGSETTAB(ID_DTAB,ID_DSPTAB2)
     ENDIF
    CASE (ID_DSPTAB2)
     CALL STARTP1FIELDS_PUTTAB2()
     CALL STARTP1FIELDS()
   END SELECT

  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%WIN)
    CASE (ID_DSPOINTS)
    CASE (ID_DSPTAB1)
     CALL POLYGON1MAIN(ITYPE,MESSAGE)
     CALL STARTP1FIELDS()
    CASE (ID_DSPTAB2)
     SELECT CASE (MESSAGE%VALUE2)
      CASE (IDF_CHECK1)
       CALL STARTP1FIELDS()
     END SELECT
   END SELECT

  CASE(PUSHBUTTON)
   SELECT CASE (MESSAGE%WIN)
    CASE (ID_DSPOINTS)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (ID_SHOW)
       IF(STARTP1FIELDS_GETTAB2(IQUIT))CALL STARTP1SHOW()
      CASE (IDCANCEL)
       CALL STARTP1CLOSE(1)
      CASE (IDHELP)
       CALL IMODGETHELP('5.10','TMO.DefStartP')
     END SELECT

    CASE (ID_DSPTAB1)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (ID_DRAW)
       IACTSHAPES=(/1,3,1,1,1,3/)
       CALL POLYGON1CREATESHAPE(ID_DSPTAB1)
       CALL POLYGON1FIELDS(ID_DSPTAB1)
      CASE DEFAULT
       CALL POLYGON1MAIN(ITYPE,MESSAGE)
     END SELECT
     CALL STARTP1FIELDS()

    CASE (ID_DSPTAB2)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (ID_OPEN1,ID_OPEN2,ID_OPEN3)
       IF(UTL_WSELECTFILE('iMOD IDF Files|*.idf|',&
                   LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,IDFNAME,&
                   'Load iMOD Map (*.idf)'))THEN
        IF(MESSAGE%VALUE1.EQ.ID_OPEN1)CALL WDIALOGPUTSTRING(IDF_STRING2,TRIM(IDFNAME))
        IF(MESSAGE%VALUE1.EQ.ID_OPEN2)CALL WDIALOGPUTSTRING(IDF_STRING3,TRIM(IDFNAME))
        IF(MESSAGE%VALUE1.EQ.ID_OPEN3)CALL WDIALOGPUTSTRING(IDF_STRING4,TRIM(IDFNAME))
       ENDIF
     END SELECT
   END SELECT

 END SELECT

 END SUBROUTINE STARTP1MAIN

 !###======================================================================
 LOGICAL FUNCTION STARTP1OPENSP()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,N
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE

 STARTP1OPENSP=.FALSE.

 CALL WDIALOGLOAD(ID_DSCENNAME,ID_DSCENNAME)
 CALL UTL_IMODFILLMENU(IDF_MENU1,TRIM(PREFVAL(1))//'\STARTPOINTS','*.isd','F',N,0,0)
 CALL WDIALOGFIELDSTATE(IDF_MENU1,1)
 CALL WDIALOGTITLE('Open/Create a StartPoint Definition')
 CALL WDIALOGPUTSTRING(IDOK,'Open and continue')
 CALL WDIALOGPUTSTRING(IDCANCEL,'Close')
 CALL WDIALOGFIELDSTATE(ID_NOK,3)

 CALL WDIALOGPUTSTRING(IDF_GROUP1,'Select a StartPoint Definition, or Enter a new name')
 CALL WDIALOGSHOW(-1,-1,0,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDOK)
      CALL WDIALOGGETMENU(IDF_MENU1,I,SDFFNAME)
      IF(LEN_TRIM(SDFFNAME).NE.0)THEN
       I=INDEX(SDFFNAME,'.',.TRUE.)
       IF(I.EQ.0)THEN
        SDFFNAME=TRIM(SDFFNAME)//'.isd'
       ELSE
        SDFFNAME=TRIM(SDFFNAME(:I-1))//'.isd'
       ENDIF
       EXIT
      ENDIF
     CASE (IDHELP)
       CALL IMODGETHELP('5.10','TMO.DefStartP')
     CASE (IDCANCEL)
      EXIT
    END SELECT
  END SELECT
 ENDDO

 CALL WDIALOGUNLOAD()
 IF(MESSAGE%VALUE1.EQ.IDCANCEL)RETURN

 !## get folder name
 SDFFNAME=TRIM(PREFVAL(1))//'\STARTPOINTS\'//TRIM(SDFFNAME)
 I      =INDEXNOCASE(SDFFNAME,'\',.TRUE.)-1
 SPDIR  =SDFFNAME(:I)

 STARTP1OPENSP=.TRUE.

 END FUNCTION STARTP1OPENSP

 !###======================================================================
 LOGICAL FUNCTION STARTP1SAVELOAD(CODE,IQUIT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: CODE
 INTEGER,INTENT(OUT) :: IQUIT
 INTEGER :: IU,I,J,K,IOS
 CHARACTER(LEN=256) :: FNAME,LINE
 LOGICAL :: LEX

 STARTP1SAVELOAD=.FALSE.

 FNAME=TRIM(PREFVAL(1))//'\STARTPOINTS\*.isd'

 IF(CODE.EQ.0)THEN
  IF(SDFFNAME.EQ.'')THEN
   IF(.NOT.UTL_WSELECTFILE('iMOD Start Point Definition Files (*.isd)|*.isd|',  &
        LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,&
        'Load iMOD Start Point Definition File'))RETURN
  ELSE
   FNAME=SDFFNAME
  ENDIF

  !## test to see whether the file exists
  INQUIRE(FILE=FNAME,EXIST=LEX)
  IF(LEX)THEN
   IU=UTL_GETUNIT()
   CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ,DENYWRITE',FORM='FORMATTED')
  ELSE
   IU=0
  ENDIF
  
  SHPNO  =0
  SHPI   =0
  SHPIACT=0
  SHPNAME=''
  NSDFNAME=0
  SDFNAME=''

  IF(IU.GT.0)THEN
   DO
    READ(IU,*,IOSTAT=IOS)
    IF(IOS.NE.0)EXIT
    SHPI=SHPI+1
    IF(SHPI.GT.MAXSHAPES)THEN
     CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'Total number of polygons read is '// &
              TRIM(ITOS(SHPI))//CHAR(13)//'* Maximal allowed is '//TRIM(ITOS(MAXSHAPES))//CHAR(13)//&
              'You can increase these settings in the menu-option: Preferences'//CHAR(13)//CHAR(13)// &
              'Selected file not read!','Error')
     CLOSE(IU)
     RETURN
    ENDIF
    READ(IU,*,IOSTAT=IOS) SHPNAME(SHPI)
    IF(.NOT.STARTP1CHECK(IU,IOS,'ShapeName'//TRIM(ITOS(SHPI))))RETURN
    READ(IU,*,IOSTAT=IOS)
    IF(.NOT.STARTP1CHECK(IU,IOS,''))RETURN
    READ(IU,*,IOSTAT=IOS) SHPTYPE(SHPI)
    IF(.NOT.STARTP1CHECK(IU,IOS,'ShapeType'//TRIM(ITOS(SHPI))))RETURN
    SELECT CASE (SHPTYPE(SHPI))
     CASE (ID_GRID)

     CASE (ID_POLYGON)
      READ(IU,*,IOSTAT=IOS) SPNT(SHPI)%IDX,SPNT(SHPI)%IDY
      SPNT(SHPI)%IDX=MAX(0.01,SPNT(SHPI)%IDX)
      SPNT(SHPI)%IDY=MAX(0.01,SPNT(SHPI)%IDY)
     CASE (ID_CIRCLE)
      !## overrule id_point -> id_circle
      SHPTYPE(SHPI)=ID_CIRCLE
      READ(IU,*,IOSTAT=IOS) SPNT(SHPI)%IRADIUS,SPNT(SHPI)%ISX
      SPNT(SHPI)%IRADIUS=MAX(0.01,SPNT(SHPI)%IRADIUS)
      SPNT(SHPI)%ISX=MAX(0.01,SPNT(SHPI)%ISX)
     CASE (ID_POINT)
      !## overrule id_point -> id_circle
      SHPTYPE(SHPI)=ID_POINT
      READ(IU,*,IOSTAT=IOS) SPNT(SHPI)%IRADIUS,SPNT(SHPI)%ISX
      SPNT(SHPI)%IRADIUS=MAX(0.01,SPNT(SHPI)%IRADIUS)
      SPNT(SHPI)%ISX=MAX(0.01,SPNT(SHPI)%ISX)
     CASE (ID_LINE)
      READ(IU,*,IOSTAT=IOS) SPNT(SHPI)%ISX
      SPNT(SHPI)%ISX=MAX(0.01,SPNT(SHPI)%ISX)
     CASE (ID_RECTANGLE)
      IF(.NOT.STARTP1CHECK(IU,IOS,'iMOD can not recognize other shapes than polygon,circle and/or lines in an *,.isd-file'))RETURN
    END SELECT
    IF(.NOT.STARTP1CHECK(IU,IOS,'ShapeLat.Dimension'//TRIM(ITOS(SHPI))))RETURN
    READ(IU,*,IOSTAT=IOS) SPNT(SHPI)%TOPIDF
    IF(.NOT.STARTP1CHECK(IU,IOS,'ShapeTopIDF'//TRIM(ITOS(SHPI))))RETURN
    READ(IU,*,IOSTAT=IOS) SPNT(SHPI)%BOTIDF
    IF(.NOT.STARTP1CHECK(IU,IOS,'ShapeBotIDF'//TRIM(ITOS(SHPI))))RETURN
    READ(IU,'(A256)',IOSTAT=IOS) LINE
    READ(LINE,*,IOSTAT=IOS) SPNT(SHPI)%IREF
    IF(.NOT.STARTP1CHECK(IU,IOS,'ShapeIREF'//TRIM(ITOS(SHPI))))RETURN
    SPNT(SHPI)%IREF=MAX(0,MIN(1,SPNT(SHPI)%IREF))
    IF(SPNT(SHPI)%IREF.EQ.1)THEN
     READ(LINE,*,IOSTAT=IOS) SPNT(SHPI)%IREF,SPNT(SHPI)%REFIDF
     IF(.NOT.STARTP1CHECK(IU,IOS,'RefName'//TRIM(ITOS(SHPI))))RETURN
     SPNT(SHPI)%IREF=MAX(0,MIN(1,SPNT(SHPI)%IREF))
    ENDIF
    READ(IU,*,IOSTAT=IOS)  SPNT(SHPI)%ISZ
    IF(.NOT.STARTP1CHECK(IU,IOS,'ISZ'//TRIM(ITOS(SHPI))))RETURN
    READ(IU,*,IOSTAT=IOS)
    IF(.NOT.STARTP1CHECK(IU,IOS,''))RETURN
    READ(IU,*,IOSTAT=IOS) SHPNCRD(SHPI)
    IF(.NOT.STARTP1CHECK(IU,IOS,'ShpNcrd'//TRIM(ITOS(SHPI))))RETURN
    IF(SHPNCRD(SHPI).GT.MAXSHPCRD)THEN
     CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'The number of coordinates within one polygon is '// &
               TRIM(ITOS(SHPNCRD(SHPI)))//CHAR(13)//'* Maximal allowed is '//TRIM(ITOS(MAXSHPCRD))//CHAR(13)//&
               'You can increase these settings in the menu-option: Preferences'//CHAR(13)//CHAR(13)// &
              'Selected file not read completely!','Error')
     CLOSE(IU)
     RETURN
    ENDIF
    DO J=1,SHPNCRD(SHPI)
     READ(IU,*,IOSTAT=IOS) SHPXC(J,SHPI),SHPYC(J,SHPI)
     IF(.NOT.STARTP1CHECK(IU,IOS,'Polygon:'//TRIM(ITOS(SHPI)//',coord.:'//TRIM(ITOS(J)))))RETURN
    END DO
    READ(IU,*,IOSTAT=IOS)
    IF(.NOT.STARTP1CHECK(IU,IOS,''))RETURN
    SHPIACT(SHPI) =1
    SHPCOLOR(SHPI)=ICOLOR(SHPI)
    SHPWIDTH(SHPI)=2
   ENDDO
   CLOSE(IU)
  ENDIF
  
  SHPNO=SHPI

  CALL WDIALOGSELECT(ID_DSPTAB1)
  CALL WDIALOGPUTMENU(IDF_MENU1,SHPNAME,SHPNO,SHPIACT)
  CALL POLYGON1FIELDS(ID_DSPTAB1)
  CALL STARTP1FIELDS()
  CALL IDFPLOTFAST(0)

 ELSEIF(CODE.EQ.1)THEN

  !## update last dialog fields --- error occured
  IF(.NOT.STARTP1FIELDS_GETTAB2(IQUIT))RETURN

  IF(SDFFNAME.EQ.'')THEN
   IF(.NOT.UTL_WSELECTFILE('iMOD Start Point Definition Files (*.isd)|*.isd|',  &
        SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,  &
        'Save iMOD Start Point Definition File'))RETURN
  ELSE
   INQUIRE(FILE=SDFFNAME,EXIST=LEX)
   IF(LEX)THEN
    CALL WMESSAGEBOX(YESNOCANCEL,COMMONNO,QUESTIONICON,'Do you want to overwrite the existing file:'// &
     CHAR(13)//TRIM(SDFFNAME),'Question')
    IF(WINFODIALOG(4).EQ.0)RETURN    !## cancel
    STARTP1SAVELOAD=.TRUE.
    IF(WINFODIALOG(4).EQ.2)RETURN    !## no
   ENDIF
   FNAME=SDFFNAME
  ENDIF
  
  IU=UTL_GETUNIT()
  CALL OSD_OPEN(IU,FILE=FNAME,STATUS='REPLACE',ACTION='WRITE,DENYREAD',FORM='FORMATTED')
  
  DO SHPI=1,SHPNO
   IF(SHPNCRD(SHPI).GT.0)THEN
    WRITE(IU,'(50A1)') ('=',K=1,50)
    WRITE(IU,'(A)') TRIM(SHPNAME(SHPI))//' <-Shapename'
    WRITE(IU,'(50A1)') ('-',K=1,50)
    SELECT CASE (SHPTYPE(SHPI))
     CASE (ID_GRID)

     CASE (ID_POLYGON)
      LINE=TRIM(ITOS(SHPTYPE(SHPI)))
      WRITE(IU,'(A)') TRIM(LINE)//',POLYGON'
      LINE=TRIM(RTOS(SPNT(SHPI)%IDX,'F',2))//','//TRIM(RTOS(SPNT(SHPI)%IDY,'F',2))
      WRITE(IU,'(A)') TRIM(LINE)
     CASE (ID_POINT)
      LINE=TRIM(ITOS(SHPTYPE(SHPI)))
      WRITE(IU,'(A)') TRIM(LINE)//',POINT'
      LINE=TRIM(RTOS(SPNT(SHPI)%IRADIUS,'F',2))//','//TRIM(RTOS(SPNT(SHPI)%ISX,'F',2))
      WRITE(IU,'(A)') TRIM(LINE)
     CASE (ID_CIRCLE)
      LINE=TRIM(ITOS(SHPTYPE(SHPI)))
      WRITE(IU,'(A)') TRIM(LINE)//',CIRCLE'
      LINE=TRIM(RTOS(SPNT(SHPI)%IRADIUS,'F',2))//','//TRIM(RTOS(SPNT(SHPI)%ISX,'F',2))
      WRITE(IU,'(A)') TRIM(LINE)
     CASE (ID_LINE)
      LINE=TRIM(ITOS(SHPTYPE(SHPI)))
      WRITE(IU,'(A)') TRIM(LINE)//',LINE'
      LINE=TRIM(RTOS(SPNT(SHPI)%ISX,'F',2))
      WRITE(IU,'(A)') TRIM(LINE)
    END SELECT
    WRITE(IU,'(A)') '"'//TRIM(ADJUSTL(SPNT(SHPI)%TOPIDF))//'"'
    WRITE(IU,'(A)') '"'//TRIM(ADJUSTL(SPNT(SHPI)%BOTIDF))//'"'
    IF(SPNT(SHPI)%IREF.EQ.0)THEN
     LINE=TRIM(ITOS(SPNT(SHPI)%IREF))
     WRITE(IU,'(A)') TRIM(LINE)
    ELSE
     LINE=TRIM(ITOS(SPNT(SHPI)%IREF))//',"'//TRIM(ADJUSTL(SPNT(SHPI)%REFIDF))//'"'
     WRITE(IU,'(A)') TRIM(LINE)
    ENDIF
    LINE=TRIM(ITOS(SPNT(SHPI)%ISZ))
    WRITE(IU,'(A)') TRIM(LINE)
    WRITE(IU,'(50A1)') ('-',K=1,50)
    LINE=TRIM(ITOS(SHPNCRD(SHPI)))
    WRITE(IU,'(A)') TRIM(LINE)//' <-No. Points Polygon'
    DO J=1,SHPNCRD(SHPI)
     LINE=TRIM(RTOS(SHPXC(J,SHPI),'F',2))//','//TRIM(RTOS(SHPYC(J,SHPI),'F',2))
     WRITE(IU,'(A)') TRIM(LINE)
    END DO
    WRITE(IU,'(50A1)') ('=',K=1,50)
   ENDIF
  END DO
  CLOSE(IU)
 ENDIF

 STARTP1SAVELOAD=.TRUE.

 END FUNCTION STARTP1SAVELOAD

 !###======================================================================
 LOGICAL FUNCTION STARTP1CHECK(IU,TIOS,LABEL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,TIOS
 CHARACTER(LEN=*),INTENT(IN) :: LABEL

 STARTP1CHECK=.TRUE.
 IF(TIOS.NE.0)THEN
  CLOSE(IU)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Selected file not read because there were errors inside the file'//CHAR(13)// &
   TRIM(LABEL),'Error')
  STARTP1CHECK=.FALSE.
 ENDIF

 END FUNCTION STARTP1CHECK

 !###======================================================================
 SUBROUTINE STARTP1SHOW()
 !###======================================================================
 IMPLICIT NONE

 !## refresh
 CALL IDFPLOTFAST(0)
 CALL IDFPLOT1BITMAP()

 CALL IGRPLOTMODE(MODECOPY)
 CALL IGRFILLPATTERN(SOLID)
 CALL IGRLINETYPE(SOLIDLINE)

 CALL WDIALOGSELECT(ID_DSPTAB1)
 CALL WDIALOGGETMENU(IDF_MENU1,SHPIACT)
 
 CALL IGRFILLPATTERN(SOLID)
 CALL IGRLINETYPE(SOLIDLINE)

 DO SHPI=1,SHPNO
  IF(SHPIACT(SHPI).EQ.1)THEN
   CALL IGRCOLOURN(ICOLOR(SHPI))
   CALL STARTP2SHOW()
  ENDIF
 ENDDO

 CALL IDFPLOT2BITMAP()

 END SUBROUTINE STARTP1SHOW

 !###======================================================================
 SUBROUTINE STARTP2SHOW()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: J,K,NOR
 REAL :: XC,YC,RADIUS,GRAD2RAD,DIFFX,DIFFY,TL,DL
 REAL :: XCMIN,YCMIN,XCMAX,YCMAX,OR

 RADIUS=SQRT((MPW%XMAX-MPW%XMIN)**2.0+(MPW%YMAX-MPW%YMIN)**2.0)/1000.0

 YCMIN=MINVAL(SHPYC(1:SHPNCRD(SHPI),SHPI))
 YCMAX=MAXVAL(SHPYC(1:SHPNCRD(SHPI),SHPI))
 XCMIN=MINVAL(SHPXC(1:SHPNCRD(SHPI),SHPI))
 XCMAX=MAXVAL(SHPXC(1:SHPNCRD(SHPI),SHPI))

 SELECT CASE (SHPTYPE(SHPI))
  CASE (ID_POLYGON)

   K=((YCMAX-YCMIN)/SPNT(SHPI)%IDX)*((XCMAX-XCMIN)/SPNT(SHPI)%IDY)
   IF(K.GT.10000)THEN
    CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to display '//TRIM(ITOS(K))//' points!','Question')
    IF(WINFODIALOG(4).NE.1)RETURN
   ENDIF

   YC=YCMAX
   DO
    YC=YC-SPNT(SHPI)%IDY
    IF(YC.LT.YCMIN)EXIT
    XC=XCMIN
    DO
     XC=XC+SPNT(SHPI)%IDX
     IF(XC.GT.XCMAX)EXIT
     IF(UTL_INSIDEPOLYGON(XC,YC,SHPXC(:,SHPI),SHPYC(:,SHPI),SHPNCRD(SHPI)).EQ.1)THEN
      CALL IGRCIRCLE(XC,YC,RADIUS)
     ENDIF
    ENDDO
   ENDDO

  CASE (ID_CIRCLE)

   !## length between points on circle
   DIFFX=(2.0*PI*REAL(SPNT(SHPI)%IRADIUS))/REAL(SPNT(SHPI)%ISX)
   !## number of points on circle (minimal = 1)
   NOR  =INT(DIFFX)+1

   GRAD2RAD=360.0/(2.0*PI)
   DO J=1,SHPNCRD(SHPI)
    OR =360.0/NOR!REAL(SPNT(I)%ISX)
    DO K=1,NOR!SPNT(I)%ISX
     IF(K*OR.GT.360.0)EXIT
     DIFFX=SPNT(SHPI)%IRADIUS*COS((K*OR)/GRAD2RAD)
     DIFFY=SPNT(SHPI)%IRADIUS*SIN((K*OR)/GRAD2RAD)
     CALL IGRCIRCLE(SHPXC(J,SHPI)+DIFFX,SHPYC(J,SHPI)+DIFFY,RADIUS)
    END DO
   END DO

  CASE (ID_LINE)

   CALL IGRCIRCLE(SHPXC(1,SHPI),SHPYC(1,SHPI),RADIUS)
   DL=0.0
   DO J=2,SHPNCRD(SHPI)
    DIFFX=SHPXC(J,SHPI)-SHPXC(J-1,SHPI)
    DIFFY=SHPYC(J,SHPI)-SHPYC(J-1,SHPI)
    TL   =SQRT(DIFFX**2.0+DIFFY**2.0)
    DIFFX=DIFFX/TL
    DIFFY=DIFFY/TL
    DO
     DL=DL+REAL(SPNT(SHPI)%ISX)
     IF(DL.LE.TL)THEN
      CALL IGRCIRCLE(SHPXC(J-1,SHPI)+DIFFX*DL,SHPYC(J-1,SHPI)+DIFFY*DL,RADIUS)
     ELSE
      !## remaining part
      DL=DL-TL-REAL(SPNT(SHPI)%ISX)
      EXIT
     ENDIF
    ENDDO
   ENDDO

 END SELECT

 END SUBROUTINE STARTP2SHOW

 !###======================================================================
 LOGICAL FUNCTION STARTP1FIELDS_GETTAB2(IQUIT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IQUIT
 
 STARTP1FIELDS_GETTAB2=.FALSE.
 IQUIT=0
 
 !## something went wrong
 IF(ISHAPE.LE.0.OR.ISHAPE.GT.SHPNO)THEN
  CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'You did not specify any startpoints, do you realy want to quit?','Question')
  IF(WINFODIALOG(4).EQ.1)IQUIT=1
  RETURN
 ENDIF
 
 CALL WDIALOGSELECT(ID_DSPTAB2)
 CALL WDIALOGGETSTRING(IDF_STRING2,SPNT(ISHAPE)%TOPIDF)
 CALL WDIALOGGETSTRING(IDF_STRING3,SPNT(ISHAPE)%BOTIDF)
 CALL WDIALOGGETINTEGER(IDF_INTEGER5,SPNT(ISHAPE)%ISZ)  !Z.SAMPL.

 IF(LEN_TRIM(SPNT(ISHAPE)%TOPIDF).EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to specify TOP IDF.','Error')
  RETURN
 ENDIF
 IF(LEN_TRIM(SPNT(ISHAPE)%BOTIDF).EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to specify BOT IDF.','Error')
  RETURN
 ENDIF
 IF(SPNT(ISHAPE)%ISZ.LE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to specify Z-interval.','Error')
  RETURN
 ENDIF

 SELECT CASE (SHPTYPE(ISHAPE))
  CASE (ID_POLYGON)
   CALL WDIALOGGETREAL(IDF_REAL1,SPNT(ISHAPE)%IDX)  !DISTX
   CALL WDIALOGGETREAL(IDF_REAL3,SPNT(ISHAPE)%IDY)  !DISTY
   IF(SPNT(ISHAPE)%IDX.LE.0.0.OR.SPNT(ISHAPE)%IDY.LE.0.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to specify positive values for'//CHAR(13)// &
      'Distance X and Distance Y.','Error')
    RETURN
   ENDIF
  CASE (ID_CIRCLE)
   CALL WDIALOGGETREAL(IDF_REAL1,SPNT(ISHAPE)%IRADIUS)  !RADIUS
   CALL WDIALOGGETREAL(IDF_REAL3,SPNT(ISHAPE)%ISX)  !SAMPL.
   IF(SPNT(ISHAPE)%IRADIUS.LE.0.0.OR.SPNT(ISHAPE)%ISX.LE.0.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to specify positive values for'//CHAR(13)// &
      'Radius and Sampling.','Error')
    RETURN
   ENDIF
  CASE (ID_LINE)
   CALL WDIALOGGETREAL(IDF_REAL1,SPNT(ISHAPE)%ISX)  !SAMPL.
   IF(SPNT(ISHAPE)%ISX.LE.0.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to specify a positive value for Sampling.','Error')
    RETURN
   ENDIF
 END SELECT

 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,SPNT(ISHAPE)%IREF)
 IF(SPNT(ISHAPE)%IREF.EQ.1)THEN
  CALL WDIALOGGETSTRING(IDF_STRING4,SPNT(ISHAPE)%REFIDF)
  IF(LEN_TRIM(SPNT(ISHAPE)%REFIDF).EQ.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You need to enter an reference IDF.','Error')
   RETURN
  ENDIF
 ENDIF

 CALL WDIALOGGETCHECKBOX(IDF_CHECK2,SPNT(ISHAPE)%ISNAP)

 STARTP1FIELDS_GETTAB2=.TRUE.

 END FUNCTION STARTP1FIELDS_GETTAB2

 !###======================================================================
 SUBROUTINE STARTP1FIELDS_PUTTAB2()
 !###======================================================================
 IMPLICIT NONE

 CALL WDIALOGSELECT(ID_DSPTAB1)
 CALL WDIALOGGETMENU(IDF_MENU1,SHPIACT)
 DO SHPI=1,SHPNO; IF(SHPIACT(SHPI).EQ.1)EXIT; END DO
 !## nothing selected --- return
 IF(SHPI.GT.SHPNO)RETURN

 ISHAPE=SHPI

 CALL WDIALOGSELECT(ID_DSPTAB2)

 SELECT CASE (SHPTYPE(ISHAPE))
  CASE (ID_POLYGON)
   CALL WDIALOGPUTIMAGE(IDF_PICTURE1,ID_ICONPOLYGON,1)
   CALL WDIALOGFIELDSTATE(IDF_LABEL4,1)
   CALL WDIALOGFIELDSTATE(IDF_LABEL6,1)
   CALL WDIALOGPUTSTRING(IDF_LABEL4,'Distance X (m)')
   CALL WDIALOGPUTSTRING(IDF_LABEL6,'Distance Y (m)')
   CALL WDIALOGFIELDSTATE(IDF_REAL1,1)
   CALL WDIALOGFIELDSTATE(IDF_REAL3,1)
   CALL WDIALOGPUTREAL(IDF_REAL1,SPNT(ISHAPE)%IDX)  !DISTX
   CALL WDIALOGPUTREAL(IDF_REAL3,SPNT(ISHAPE)%IDY)  !DISTY
  CASE (ID_CIRCLE)
   CALL WDIALOGPUTIMAGE(IDF_PICTURE1,ID_ICONCIRCLE,1)
   CALL WDIALOGFIELDSTATE(IDF_LABEL4,1)
   CALL WDIALOGFIELDSTATE(IDF_LABEL6,1)
   CALL WDIALOGPUTSTRING(IDF_LABEL4,'Radius (m)')
   CALL WDIALOGPUTSTRING(IDF_LABEL6,'Sampling (m)')
   CALL WDIALOGFIELDSTATE(IDF_REAL1,1)
   CALL WDIALOGFIELDSTATE(IDF_REAL3,1)
   CALL WDIALOGPUTREAL(IDF_REAL1,SPNT(ISHAPE)%IRADIUS)  !RADIUS
   CALL WDIALOGPUTREAL(IDF_REAL3,SPNT(ISHAPE)%ISX)      !SAMPL.
  CASE (ID_LINE)
   CALL WDIALOGPUTIMAGE(IDF_PICTURE1,ID_ICONLINE,1)
   CALL WDIALOGFIELDSTATE(IDF_LABEL4,1)
   CALL WDIALOGFIELDSTATE(IDF_LABEL6,3)
   CALL WDIALOGPUTSTRING(IDF_LABEL4,'Sampling (m)')
   CALL WDIALOGFIELDSTATE(IDF_REAL1,1)
   CALL WDIALOGFIELDSTATE(IDF_REAL3,3)
   CALL WDIALOGPUTREAL(IDF_REAL1,SPNT(ISHAPE)%ISX)  !SAMPL.
 END SELECT
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK2,SPNT(ISHAPE)%ISNAP)

 CALL WDIALOGPUTSTRING(IDF_STRING2,SPNT(ISHAPE)%TOPIDF)
 CALL WDIALOGPUTSTRING(IDF_STRING3,SPNT(ISHAPE)%BOTIDF)
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,SPNT(ISHAPE)%IREF)
 CALL WDIALOGPUTSTRING(IDF_STRING4,SPNT(ISHAPE)%REFIDF)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER5,SPNT(ISHAPE)%ISZ)  !Z.SAMPL.

 END SUBROUTINE STARTP1FIELDS_PUTTAB2

 !###======================================================================
 SUBROUTINE STARTP1FIELDS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL WDIALOGSELECT(ID_DSPOINTS)
 I=0
 IF(SHPNO.GT.0)THEN
  IF(SUM(SHPIACT(1:SHPNO)).EQ.1)I=1
 ENDIF
 CALL WDIALOGTABSTATE(ID_DTAB,ID_DSPTAB2,I)
 
 CALL WDIALOGSELECT(ID_DSPTAB2)
 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I)
 CALL WDIALOGFIELDSTATE(IDF_STRING4,I)
 CALL WDIALOGFIELDSTATE(ID_OPEN3,I)

 END SUBROUTINE STARTP1FIELDS

 !###======================================================================
 SUBROUTINE STARTP1INIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: N,IQUIT

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_SPOINTS,2).EQ.1)THEN
  CALL STARTP1CLOSE(1)
  RETURN
 ENDIF

 CALL MAIN1INACTMODULE(ID_SPOINTS)

 !## other module no closed, no approvement given
 IF(IDIAGERROR.EQ.1)RETURN

 CALL WMENUSETSTATE(ID_SPOINTS,2,1)

 IF(.NOT.IOSDIREXISTS(TRIM(PREFVAL(1))//'\STARTPOINTS'))CALL UTL_CREATEDIR(TRIM(PREFVAL(1))//'\STARTPOINTS')

 CALL POLYGON1INIT()

 IF(ALLOCATED(SPNT))DEALLOCATE(SPNT)
 IF(ALLOCATED(SDFNAME))DEALLOCATE(SDFNAME)
 IF(ALLOCATED(NSDFNAME))DEALLOCATE(NSDFNAME)

 ALLOCATE(SPNT(MAXSHAPES))
 ALLOCATE(SDFNAME(MAXSHAPES,MAXSDF))
 ALLOCATE(NSDFNAME(MAXSHAPES))

 NSDFNAME=0

 SPNT%IDX=25
 SPNT%IDY=25
 SPNT%ISX=25
 SPNT%ISY=25
 SPNT%ISZ=1
 SPNT%IRADIUS=100
 SPNT%BOTIDF=''
 SPNT%TOPIDF=''
 SPNT%REFIDF=''
 SPNT%IREF=0

 CALL WDIALOGLOAD(ID_DSPOINTS,ID_DSPOINTS)
 CALL POLYGON1IMAGES(ID_DSPTAB1)
 CALL POLYGON1FIELDS(ID_DSPTAB1)

 CALL WDIALOGSELECT(ID_DSPTAB2)
 CALL WDIALOGPUTIMAGE(ID_OPEN1,ID_ICONOPENIDF)
 CALL WDIALOGPUTIMAGE(ID_OPEN2,ID_ICONOPENIDF)
 CALL WDIALOGPUTIMAGE(ID_OPEN3,ID_ICONOPENIDF)
 CALL WDIALOGSPINNERSTEP(IDF_REAL1,1,10)
 CALL WDIALOGSPINNERSTEP(IDF_REAL3,1,10)
 CALL WDIALOGRANGEREAL(IDF_REAL1,0.0,10000.0)
 CALL WDIALOGRANGEREAL(IDF_REAL3,0.0,10000.0)

 CALL STARTP1FIELDS()

 !## open scenario
 IF(STARTP1OPENSP())THEN
  IF(STARTP1SAVELOAD(0,IQUIT))THEN
   CALL WDIALOGSELECT(ID_DSPOINTS)
   CALL WDIALOGPUTSTRING(IDF_LABEL1,'StartPoint Definition: '//TRIM(SDFFNAME(INDEX(SDFFNAME,'\',.TRUE.)+1:)))
   CALL WDIALOGSHOW(-1,-1,0,2)
   RETURN
  ENDIF
 ENDIF

 CALL STARTP1CLOSE(0)

 END SUBROUTINE STARTP1INIT

 !###======================================================================
 SUBROUTINE STARTP1CLOSE(ICODE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ICODE
 INTEGER :: IQUIT

 IDIAGERROR=1

 IF(ICODE.EQ.1)THEN
  !## save startpoint
  IF(.NOT.STARTP1SAVELOAD(1,IQUIT))THEN
   IF(IQUIT.EQ.0)RETURN
  ENDIF
 ENDIF

 IF(ALLOCATED(SPNT))DEALLOCATE(SPNT)
 IF(ALLOCATED(SDFNAME))DEALLOCATE(SDFNAME)
 IF(ALLOCATED(NSDFNAME))DEALLOCATE(NSDFNAME)

 CALL POLYGON1DRAWSHAPE(1,SHPNO)
 CALL POLYGON1CLOSE()

 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(ID_SPOINTS,2,0)

 CALL WDIALOGSELECT(ID_DSPOINTS)
 CALL WDIALOGUNLOAD()

 !## refresh window
 CALL IDFPLOTFAST(0)

 IDIAGERROR=0

 END SUBROUTINE STARTP1CLOSE

END MODULE MOD_SPOINTS

