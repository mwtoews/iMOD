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
MODULE MOD_SCENTOOL

USE WINTERACTER
USE RESOURCE
USE MOD_DBL
USE MOD_IDFPLOT
USE MODPLOT, ONLY : MPW
USE MOD_SCENTOOL_PAR
USE MOD_SCENTOOL_WELLS, ONLY : STWEL1DELETE,ST1SAVELOADWELLS,STWEL1INIT ,STWEL1MAIN,STWEL1TITLE
USE MOD_SCENTOOL_OBS, ONLY :   STOBS1DELETE,ST1SAVELOADOBS  ,STOBS1INIT ,STOBS1MAIN,STOBS1TITLE
USE MOD_SCENTOOL_RESULTS, ONLY : STRES1MAIN,STRES1DELETE    ,STRES1TITLE,STRES1TSERIE_INIT     ,STRES1MAPS
USE MOD_SCENTOOL_UTL, ONLY : ST1FIELDS,ST1FIELDS_STATE,ST_SYMBOLDRAW,ST1FILLRESULTS,ST_SYMBOLCOLOUR,ST1ERROR,ST1DEALLOCATE
USE MOD_SCENTOOL_PLOT, ONLY : ST1PLOTOBS,ST1PLOTWELLS
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_MODEL_PAR, ONLY : REPLACESTRING
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_READINITFILE,UTL_CAP,ITOS,UTL_CREATEDIR,JDATETOGDATE,UTL_IMODFILLMENU,UTL_CLOSEUNITS,UTL_WSELECTFILE
USE MOD_IDF, ONLY : IDFDEALLOCATE,IDFNULLIFY
USE MOD_OSD, ONLY : OSD_OPEN
USE IMODVAR, ONLY : DP_KIND,SP_KIND,IDIAGERROR
USE MOD_MAIN_UTL

INTEGER,PARAMETER :: NTAB=5
INTEGER,PARAMETER,PRIVATE :: NDIALOGS=2
INTEGER,PRIVATE :: STIPROP  !## 1=wells, 2=observations
INTEGER,DIMENSION(NTAB) :: IDTAB
INTEGER,DIMENSION(NDIALOGS),PRIVATE :: IDDIAG
DATA IDTAB /ID_DSCENTOOLTAB1,ID_DSCENTOOLTAB2,ID_DSCENTOOLTAB3,ID_DSCENTOOLTAB4,ID_DSCENTOOLTAB5/
DATA IDDIAG/ID_DSCENTOOL_PROPTAB1,ID_DSCENTOOL_PROPTAB2/

CONTAINS

 !###======================================================================
 SUBROUTINE ST1MAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER :: I
 LOGICAL :: LEX
 
 CALL WDIALOGSELECT(MESSAGE%WIN)
 SELECT CASE (MESSAGE%WIN)

  !## main scenario
  CASE (ID_DSCENTOOL)
   SELECT CASE (ITYPE)

    CASE (TABCHANGED)
     
     SELECT CASE (MESSAGE%VALUE2)
      CASE (ID_DSCENTOOLTAB5)
       !## save first
       LEX=.TRUE.
       IF(LEN_TRIM(SCFFNAME).EQ.0)LEX=ST1SAVELOAD(MESSAGE%VALUE1)
       IF(LEX)THEN
        CALL ST1FILLRESULTS()
        CALL STRES1TITLE()
        CALL ST1FIELDS()
       ELSE
        CALL WDIALOGSETTAB(IDF_TAB,MESSAGE%VALUE1)
        CALL ST1FIELDS()
       ENDIF
      CASE DEFAULT
       CALL ST1FIELDS()
       
     END SELECT
     
    CASE (FIELDCHANGED)
     CALL ST1FIELDS()

    CASE (PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (ID_OPEN)
       I=2 !## default no saving
       !## ask to save first
       IF(LEN_TRIM(SCFFNAME).NE.0)THEN
        CALL WMESSAGEBOX(YESNOCANCEL,QUESTIONICON,COMMONYES,'Do you want to save your work first?','Question')
        I=WINFODIALOG(4)
       ENDIF
       !## canceling from save question
       IF(I.NE.0)THEN              
        !## save first?
        LEX=.TRUE.
        IF(I.EQ.1)LEX=ST1SAVELOAD(ID_SAVEAS)
        IF(LEX)LEX=ST1SAVELOAD(ID_OPEN)
       ENDIF
      CASE (ID_SAVEAS,ID_SAVE)
       IF(.NOT.ST1SAVELOAD(MESSAGE%VALUE1))THEN
       ENDIF      
      CASE (IDCANCEL)
       CALL ST1CLOSE()
     END SELECT
   END SELECT

  !## well systems
  CASE (ID_DSCENTOOLTAB1)
   SELECT CASE (ITYPE)
    CASE (FIELDCHANGED)
     SELECT CASE (MESSAGE%VALUE2)
      CASE (IDF_RADIO1,IDF_RADIO2,IDF_RADIO3,IDF_RADIO4,IDF_MENU1,IDF_CHECK1,IDF_CHECK2)
       CALL IDFPLOT(1)
      CASE DEFAULT
       CALL ST1FIELDS()
     END SELECT
    CASE (PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (ID_ADD,ID_INFO)
       CALL STWEL1INIT(MESSAGE%VALUE1)
       STIPROP=1
      CASE (ID_DELETE)
       CALL STWEL1DELETE()
       CALL IDFPLOT(1)
!      CASE (ID_COLOUR)
!       CALL ST1_GETCOLOUR(ID_DSCENTOOLTAB1,IDF_INTEGER1)        
     END SELECT
   END SELECT

  !## cutouts
  CASE (ID_DSCENTOOLTAB2)
   SELECT CASE (ITYPE)
    CASE (FIELDCHANGED)
     SELECT CASE (MESSAGE%VALUE2)
      CASE (IDF_RADIO1,IDF_RADIO2,IDF_RADIO3,IDF_RADIO4,IDF_MENU1,IDF_CHECK1,IDF_CHECK2)
       CALL IDFPLOT(1)
      CASE DEFAULT
       CALL ST1FIELDS()
     END SELECT
    CASE (PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (ID_ADD,ID_INFO)
      CASE (ID_DELETE)
!      CASE (ID_COLOUR)
!       CALL ST1_GETCOLOUR(ID_DSCENTOOLTAB2,IDF_INTEGER1)        
     END SELECT
   END SELECT

  !## observations
  CASE (ID_DSCENTOOLTAB3)
   SELECT CASE (ITYPE)
    CASE (FIELDCHANGED)
     SELECT CASE (MESSAGE%VALUE2)
      CASE (IDF_RADIO1,IDF_RADIO2,IDF_RADIO3,IDF_RADIO4,IDF_MENU1,IDF_CHECK1,IDF_CHECK2)
       CALL IDFPLOT(1)
      CASE DEFAULT
       CALL ST1FIELDS()
     END SELECT
    CASE (PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (ID_ADD,ID_INFO)
       STIPROP=2
       CALL STOBS1INIT(MESSAGE%VALUE1)
      CASE (ID_DELETE)
       CALL STOBS1DELETE()
       CALL IDFPLOT(1)
!      CASE (ID_COLOUR)
!       CALL ST1_GETCOLOUR(ID_DSCENTOOLTAB3,IDF_INTEGER1)        
     END SELECT
   END SELECT

  !## monitoring
  CASE (ID_DSCENTOOLTAB4)
   SELECT CASE (ITYPE)
    CASE (FIELDCHANGED)
     SELECT CASE (MESSAGE%VALUE2)
      CASE (IDF_RADIO1,IDF_RADIO2,IDF_RADIO3,IDF_RADIO4,IDF_MENU1,IDF_CHECK1,IDF_CHECK2)
       CALL IDFPLOT(1)
      CASE DEFAULT
       CALL ST1FIELDS()
     END SELECT
    CASE (PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (ID_ADD,ID_INFO)
      CASE (ID_DELETE)
!      CASE (ID_COLOUR)
!       CALL ST1_GETCOLOUR(ID_DSCENTOOLTAB4,IDF_INTEGER1)        
     END SELECT
   END SELECT

  !## results
  CASE (ID_DSCENTOOLTAB5)
   SELECT CASE (ITYPE)
    CASE (FIELDCHANGED)
     CALL ST1FIELDS()
    CASE (PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (ID_ADD)
       CALL STRES1MAIN()
!       CALL ST1FIELDS()
      CASE (ID_DELETE)
       CALL STRES1DELETE()
      CASE (ID_MAP)
       CALL STRES1MAPS()
      CASE (ID_HISTOGRAM)
       CALL STRES1TSERIE_INIT()
!      CASE (ID_INFO)
!       CALL WDIALOGSELECT(ID_DSCENTOOLTAB4)
!       CALL WDIALOGGETMENU(IDF_MENU1,I) !,FNAME)
!       CALL WINDOWOPENCHILD(IWIN,FLAGS=SYSMENUON+MAXBUTTON,WIDTH=1000,HEIGHT=500)
!       CALL WINDOWSELECT(IWIN)
!       FNAME=TRIM(RES(1)%CNAME)//'\RUNFILES\'//TRIM(FNAME)
!       CALL WEDITFILE(FNAME,ITYPE=MODAL,IDMENU=0,IFONT=COURIERNEW,ISIZE=10)

     END SELECT
   END SELECT

 END SELECT

 END SUBROUTINE ST1MAIN

 !###======================================================================
 SUBROUTINE ST1PROPMAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE

 IF(STIPROP.EQ.1)CALL STWEL1MAIN(ITYPE,MESSAGE)
 IF(STIPROP.EQ.2)CALL STOBS1MAIN(ITYPE,MESSAGE)
 
 END SUBROUTINE ST1PROPMAIN
 
 !###======================================================================
 LOGICAL FUNCTION ST1SAVELOAD(ICODE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ICODE  !## 1=read 2=write
 INTEGER :: IU,IOS
 LOGICAL :: LEX
 CHARACTER(LEN=256) :: FNAME
 
 ST1SAVELOAD=.FALSE.

 LEX=.TRUE.
 IF(ICODE.EQ.ID_OPEN)THEN        !## open
  FNAME=TRIM(RESDIR)//'\*.isf'
  FNAME='d:\IMOD-MODELS\IMOD\IMOD_USER\SCENTOOL\TEST.isf'
!  IF(.NOT.UTL_WSELECTFILE('iMOD Scenario File (*.isf)|*.isf|',&
!       LOADDIALOG+PROMPTON+APPENDEXT,FNAME,'Open iMOD Scenario File'))RETURN
  SCFFNAME=FNAME
 ELSE                            !## save/saveas
  LEX=LEN_TRIM(SCFFNAME).GT.0
  IF(LEX)INQUIRE(FILE=SCFFNAME,EXIST=LEX)
  IF(.NOT.LEX.OR.ICODE.EQ.ID_SAVEAS)THEN
   FNAME=TRIM(RESDIR)//'\*.isf'
   IF(.NOT.UTL_WSELECTFILE('iMOD Scenario File (*.isf)|*.isf|',&
        SAVEDIALOG+PROMPTON+APPENDEXT,FNAME,'Save iMOD Scenario File'))RETURN
   SCFFNAME=FNAME
  ENDIF
 ENDIF

 IU=UTL_GETUNIT()

 !## open
 IF(ICODE.EQ.ID_OPEN)THEN
  CALL OSD_OPEN(IU,FILE=SCFFNAME,STATUS='OLD',ACTION='READ,DENYWRITE',FORM='FORMATTED',IOSTAT=IOS)
 !## write
 ELSE 
  CALL OSD_OPEN(IU,FILE=SCFFNAME,STATUS='REPLACE',ACTION='WRITE,DENYREAD',FORM='FORMATTED',IOSTAT=IOS)
 ENDIF
 IF(ST1ERROR(IOS,'iMOD cannot open file '//TRIM(SCFFNAME)).NE.0)RETURN

 !## save well systems
 CALL ST1SAVELOADWELLS(IU,ICODE)
 !## save observations
 CALL ST1SAVELOADOBS(IU,ICODE)
 
 CLOSE(IU)

 IF(ICODE.EQ.ID_OPEN)THEN
 
  !## well systems
  IF(NWEL.GT.0)THEN
   IWEL=1; CALL STWEL1TITLE()
  ENDIF
  !## cuttings
  IF(NCUT.GT.0)THEN
   CALL WDIALOGSELECT(ID_DSCENTOOLTAB2)
!   ICUT=1; CALL STCUT1TITLE()
   CALL WDIALOGPUTMENU(IDF_MENU1,CUT%CNAME,NCUT,1)
  ENDIF
  !## observations
  IF(NOBS.GT.0)THEN
   IOBS=1; CALL STOBS1TITLE()
  ENDIF
  IF(NMON.GT.0)THEN
  ENDIF
  CALL ST1FILLRESULTS()
  CALL STRES1TITLE()
  CALL WDIALOGSELECT(ID_DSCENTOOL)
  CALL WDIALOGSETTAB(IDF_TAB,ID_DSCENTOOLTAB1)
  CALL ST1FIELDS()
  !## these should be zero otherwise points will be drawn from inputgrid
  IWEL=0
  ICUT=0
  IOBS=0
  IMON=0
  RES%IRES=0
  CALL IDFPLOT(1)
 ENDIF

 CALL WDIALOGSELECT(ID_DSCENTOOL)
 CALL WDIALOGFIELDSTATE(ID_SAVE,1)
 CALL WDIALOGPUTSTRING(IDF_STRING1,TRIM(SCFFNAME))

 ST1SAVELOAD=.TRUE.

 END FUNCTION ST1SAVELOAD

 !###======================================================================
 SUBROUTINE ST1INIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_SCENTOOL,2).EQ.1)THEN
  CALL ST1CLOSE()
  RETURN
 ENDIF

 CALL MAIN_UTL_INACTMODULE(ID_SCENTOOL)

 !## other module no closed, no approvement given
 IF(IDIAGERROR.EQ.1)RETURN

 !## clean everything
 CALL ST1DEALLOCATE()

 !## read scenario tool settings
 IF(.NOT.ST1INITFILES())THEN
  CALL ST1DEALLOCATE()
  RETURN
 ENDIF

 CALL WMENUSETSTATE(ID_SCENTOOL,2,1)

 CALL WDIALOGLOAD(ID_DSCENTOOL,ID_DSCENTOOL)
 CALL WDIALOGPUTIMAGE(ID_SAVE,ID_ICONSAVE)
 CALL WDIALOGPUTIMAGE(ID_SAVEAS,ID_ICONSAVEAS)
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPEN)
 CALL WDIALOGFIELDSTATE(ID_SAVE,0)
 
 !## fill tabs for main dialog
 DO I=1,NTAB
  CALL WDIALOGSELECT(IDTAB(I))
  CALL WDIALOGPUTIMAGE(ID_ADD,ID_ICONPLUS)
  CALL WDIALOGPUTIMAGE(ID_DELETE,ID_ICONDELETE)
!  CALL WDIALOGPUTIMAGE(ID_COLOUR,ID_ICONLEGEND) 
  SELECT CASE (I)
   CASE (1:4) !## well systems (red)
    CALL WDIALOGPUTIMAGE(ID_INFO,ID_ICONPROPERTIES) !INFO)
!    CALL WDIALOGPUTINTEGER(IDF_INTEGER1,WRGB(200,0,0))
!   CASE (2) !## cutout systems (brown)
!    CALL WDIALOGPUTINTEGER(IDF_INTEGER1,WRGB(163,82,82))
!   CASE (3) !## observation wells (blue)
!    CALL WDIALOGPUTINTEGER(IDF_INTEGER1,WRGB(0,255,255))
!   CASE (4) !## monitoring (yellow)
!    CALL WDIALOGPUTINTEGER(IDF_INTEGER1,WRGB(255,255,128))
   CASE (5) !## results (wit)
    CALL WDIALOGPUTIMAGE(ID_MAP,ID_ICONCONTOUR)
    CALL WDIALOGPUTIMAGE(ID_HISTOGRAM,ID_ICONHISTOGRAM)
!    CALL WDIALOGPUTIMAGE(ID_PROFILE,ID_ICONPROFILE)
!    CALL WDIALOGPUTIMAGE(ID_MOVIE,ID_ICONMOVIE)
!    CALL WDIALOGPUTIMAGE(ID_3DTOOL,ID_ICON3DTOOL)
!    CALL WDIALOGPUTIMAGE(ID_TIMESERIES,ID_ICONTIMESERIE)
  END SELECT
!  CALL ST_SYMBOLCOLOUR(IDTAB(I),IDF_INTEGER1)
 END DO

! CALL ST1FIELDS()

 !## fill tabs for well,,,, dialogs
 CALL WDIALOGLOAD(ID_DSCENTOOL_PROP,ID_DSCENTOOL_PROP)
 DO I=1,NDIALOGS
  CALL WDIALOGSELECT(IDDIAG(I))
  IF(I.EQ.2)THEN
   CALL WDIALOGPUTIMAGE(ID_DRAW,ID_ICONPOINTPLUS)
   CALL WDIALOGPUTIMAGE(ID_MOVE,ID_ICONMOVEARROW)
   CALL WDIALOGPUTIMAGE(ID_DELETE,ID_ICONDELETE)
  ENDIF
  IF(I.EQ.1)THEN
   CALL WDIALOGPUTIMAGE(ID_CALC,ID_ICONCALC)
   CALL WDIALOGPUTIMAGE(ID_GRAPH,ID_ICONHISTOGRAM)
  ENDIF
  CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPEN)
  CALL WDIALOGPUTIMAGE(ID_SAVEAS,ID_ICONSAVEAS)
 END DO

 CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB1)
 NROWQ=WINFOGRID(IDF_GRID1,GRIDROWSMAX)
 CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB2)
 NROWL=WINFOGRID(IDF_GRID1,GRIDROWSMAX)
 CALL WDIALOGSELECT(ID_DSCENTOOL_PROPTAB3)
 CALL WDIALOGPUTIMAGE(ID_COLOUR,ID_ICONLEGEND) 

 STIPROP=0
 
! CALL ST1FIELDS_STATE(IDTAB(1),NWEL)
 CALL ST1FIELDS()
! CALL ST1FIELDS_STATE(IDTAB(2),NCUT)
! CALL ST1FIELDS_STATE(IDTAB(3),NOBS)
! CALL ST1FIELDS_STATE(IDTAB(4),NMON)
! CALL ST1FIELDS_STATE(IDTAB(5),NRES)

 !## colour fields
 CALL ST_SYMBOLDRAW(ID_DSCENTOOL_PROPTAB3,IDF_PICTURE1,IDF_MENU1,IDF_INTEGER1)
 
 CALL STWEL1TITLE()
! CALL STCUT1TITLE() 
 CALL STOBS1TITLE() 
! CALL STMON1TITLE() 
! CALL ST1FILLRESULTS()
 CALL STRES1TITLE()
 
 CALL WDIALOGSELECT(ID_DSCENTOOL)
 CALL WDIALOGSHOW(-1,-1,0,2)

 END SUBROUTINE ST1INIT

 !###======================================================================
 LOGICAL FUNCTION ST1INITFILES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,IOS,IU
 LOGICAL :: LEX
 CHARACTER(LEN=256) :: LINE

 ST1INITFILES=.FALSE.

 IF(TRIM(PREFVAL(10)).EQ.'')THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot initiate SCENARIO-tool since the variable SCENTOOL '//CHAR(13)// &
    'is not loaded from the currently selected *.prf file!','Error')
  RETURN
 ENDIF

 INQUIRE(FILE=PREFVAL(10),EXIST=LEX)
 IF(.NOT.LEX)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot initiate SCENARIO-tool since file: '//CHAR(13)// &
    TRIM(PREFVAL(10))//CHAR(13)//'does not exists !','Error')
  RETURN
 ENDIF

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TRIM(PREFVAL(10)),ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='READ,DENYWRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open '//CHAR(13)// &
       TRIM(PREFVAL(10))//CHAR(13)//'for reading!','Error')
  RETURN
 ENDIF

 !## result directory
 RESDIR=TRIM(PREFVAL(1))//'\SCENTOOL'
 CALL UTL_CREATEDIR(TRIM(RESDIR))

 !## number of scenario configurations
 IF(.NOT.UTL_READINITFILE(UTL_CAP('NSCNCONF','U'),LINE,IU,0))RETURN
 READ(LINE,*) NSCNCONF

 IF(ALLOCATED(CONF))DEALLOCATE(CONF)
 ALLOCATE(CONF(NSCNCONF))

 DO I=1,NSCNCONF

  IF(.NOT.UTL_READINITFILE(UTL_CAP('RUNFNAME'//TRIM(ITOS(I)),'U'),LINE,IU,0))RETURN
  READ(LINE,*) CONF(I)%PRJF
  CALL IUPPERCASE(CONF(I)%PRJF)
  CONF(I)%PRJF=UTL_SUBST(CONF(I)%PRJF,TRIM(REPLACESTRING),PREFVAL(5))

  IF(.NOT.UTL_READINITFILE(UTL_CAP('RUNNAME'//TRIM(ITOS(I)),'U'),LINE,IU,0))RETURN
  READ(LINE,*) CONF(I)%PRJNAME

 END DO

 !## initiate max. number of objects
 MAXNWEL=10
 MAXNCUT=10
 MAXNOBS=10
 MAXNMON=10
 MAXNRES=10

 !## read if available from *.ini file different object sizes (optional)
 IF(UTL_READINITFILE(UTL_CAP('MAXNWEL','U'),LINE,IU,1))READ(LINE,*) MAXNWEL
 IF(UTL_READINITFILE(UTL_CAP('MAXNCUT','U'),LINE,IU,1))READ(LINE,*) MAXNCUT
 IF(UTL_READINITFILE(UTL_CAP('MAXNOBS','U'),LINE,IU,1))READ(LINE,*) MAXNOBS
 IF(UTL_READINITFILE(UTL_CAP('MAXNMON','U'),LINE,IU,1))READ(LINE,*) MAXNMON
 IF(UTL_READINITFILE(UTL_CAP('MAXNRES','U'),LINE,IU,1))READ(LINE,*) MAXNRES

 CLOSE(IU)

 !## allocate memory
 ALLOCATE(WEL(MAXNWEL))
 ALLOCATE(CUT(MAXNCUT))
 ALLOCATE(OBS(MAXNOBS))
 ALLOCATE(MON(MAXNMON))
 ALLOCATE(RES(MAXNRES))

 !## nullify objects
 DO I=1,SIZE(WEL)
  NULLIFY(WEL(I)%LOC)
  NULLIFY(WEL(I)%Q)
  WEL(I)%NLOC=0
  WEL(I)%NQ  =0
 END DO
 DO I=1,SIZE(CUT)
  NULLIFY(CUT(I)%XY)
  CUT(I)%NXY=0
 END DO
 DO I=1,SIZE(OBS)
  NULLIFY(OBS(I)%LOC)
  NULLIFY(OBS(I)%Z)
  OBS(I)%NLOC=0
  OBS(I)%NZ=0
 END DO
 DO I=1,SIZE(MON)
  NULLIFY(MON(I)%XY)
  NULLIFY(MON(I)%XZ)
  MON(I)%NXY=0
  MON(I)%NXZ=0
 END DO
 DO I=1,SIZE(RES)
!  NULLIFY(RES(I)%IDFNAME)
  RES(I)%IRES  =0
!  RES(I)%NFILES=0
 END DO

 !## initiate number of (selected) objects
 NWEL=0
 IWEL=0
 NCUT=0
 ICUT=0
 NOBS=0
 IOBS=0
 NMON=0
 IMON=0
 NRES=0
 
 SCFFNAME=''

 ST1INITFILES=.TRUE.

 END FUNCTION ST1INITFILES

END MODULE MOD_SCENTOOL
