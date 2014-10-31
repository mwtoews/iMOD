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
MODULE MOD_PREF

USE WINTERACTER
USE RESOURCE
USE MOD_POLYGON_PAR
USE MOD_COLOURS
USE MODPLOT
USE MOD_UTL, ONLY : ITOS,UTL_GETUNIT,UTL_CREATEDIR,UTL_IMODFILLMENU,UTL_WSELECTFILE
USE MOD_PREF_PAR
USE MOD_ISG_PAR, ONLY : ICLRSC,ICLRSD,ICLRSP,ICLRND,ICLRST,ICLRQH
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_MANAGER, ONLY : MANAGERUPDATE

CONTAINS

 !###======================================================================
 SUBROUTINE PREFMAIN()
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,I,J
 CHARACTER(LEN=3),DIMENSION(:),ALLOCATABLE :: STRING
 CHARACTER(LEN=50) :: PRFFILE

 CALL WDIALOGLOAD(ID_DPREF)
 CALL WDIALOGSELECT(ID_DPREFTAB1)
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPEN)
 CALL WDIALOGPUTIMAGE(ID_RIGHT,ID_ICONRIGHT)
 CALL UTL_IMODFILLMENU(IDF_MENU2,TRIM(PREFDIR),'*.PRF','F',I,0,0)
 I=MIN(I,1)
 CALL WDIALOGFIELDSTATE(ID_RIGHT,I)
 CALL WDIALOGSELECT(ID_DPREFTAB2)
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPEN)
 CALL WDIALOGPUTIMAGE(ID_SAVEAS,ID_ICONSAVEAS)
 CALL WDIALOGPUTIMAGE(ID_PROPERTIES,ID_ICONPROPERTIES)
 IF(ALLOCATED(STRING))DEALLOCATE(STRING)
 ALLOCATE(STRING(MAXCOLOUR))
 DO I=1,MAXCOLOUR
  STRING(I)=ITOS(I)
 END DO
 CALL WDIALOGPUTMENU(IDF_MENU1,STRING,MAXCOLOUR,1)
 DEALLOCATE(STRING)

 CALL WDIALOGSELECT(ID_DPREFTAB1)
 CALL WDIALOGPUTMENU(IDF_MENU1,PREF,MAXPREF,1)

 CALL WDIALOGSELECT(ID_DPREFTAB3)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER3,MAXSHPCRD)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER4,MXMPLOT)
 CALL WDIALOGFIELDSTATE(IDF_INTEGER4,2)
 CALL WDIALOGRANGEINTEGER(IDF_INTEGER3,1,500000)
 CALL WDIALOGRANGEINTEGER(IDF_INTEGER4,1,150)   
 CALL WDIALOGSPINNERSTEP(IDF_INTEGER3,10,100)

 CALL PREFUPDATE()
 CALL PREFFIELDS()
 CALL WDIALOGSELECT(ID_DPREF)
 CALL WDIALOGSHOW(-1,-1,1,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (MESSAGE%WIN)

   CASE (ID_DPREF)
    SELECT CASE (ITYPE)
     CASE (PUSHBUTTON)
      SELECT CASE (MESSAGE%VALUE1)
       CASE (IDCANCEL)
        CALL PREFSAVECOLOURS(TRIM(PREFVAL(1))//'\IMOD_INIT.CLR')
        EXIT
       CASE (IDHELP)
        CALL IMODGETHELP('2.3','Preferences')
      END SELECT
    END SELECT

   CASE (ID_DPREFTAB1)
    SELECT CASE (ITYPE)
     CASE (FIELDCHANGED)
      SELECT CASE (MESSAGE%VALUE2)
       CASE (IDF_MENU1)
        CALL PREFUPDATE()
       CASE (IDF_MENU2)

      END SELECT

     CASE (PUSHBUTTON)
      SELECT CASE (MESSAGE%VALUE1)
       CASE (ID_RIGHT)
        CALL WDIALOGSELECT(ID_DPREFTAB1)
        CALL WDIALOGGETMENU(IDF_MENU2,I,PRFFILE)
        CALL PREFREAD(TRIM(PREFDIR)//'\'//TRIM(PRFFILE))
        CALL PREFUPDATE()
       CASE (ID_OPEN)
        CALL PREFREAD('')
        CALL PREFUPDATE()
      END SELECT
    END SELECT

   CASE (ID_DPREFTAB2)
    SELECT CASE (ITYPE)
     CASE (FIELDCHANGED)
      SELECT CASE (MESSAGE%VALUE1)
       CASE (IDF_MENU1)
        CALL PREFFIELDS()
      END SELECT
     CASE (PUSHBUTTON)
      SELECT CASE (MESSAGE%VALUE1)
       CASE (ID_OPEN)
        CALL PREFOPENCOLOURS('')
        CALL PREFFIELDS()
       CASE (ID_SAVEAS)
        CALL PREFSAVECOLOURS('')
       CASE (ID_PROPERTIES)
        CALL WDIALOGGETMENU(IDF_MENU1,I)
        J=ICOLOR(I)
        CALL WSELECTCOLOUR(J)
        IF(WINFODIALOG(4).EQ.1)ICOLOR(I)=J
        CALL PREFFIELDS()
      END SELECT
    END SELECT

  END SELECT
 ENDDO

 CALL WDIALOGSELECT(ID_DPREFTAB3)

 CALL WDIALOGGETINTEGER(IDF_INTEGER3,MAXSHPCRD)

 CALL WDIALOGSELECT(ID_DPREF)
 CALL WDIALOGUNLOAD()

 END SUBROUTINE PREFMAIN

 !###======================================================================
 SUBROUTINE PREFSAVECOLOURS(FNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN)      :: FNAME
 CHARACTER(LEN=256)               :: CLRFNAME
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IRGB
 INTEGER                          :: I,IU

 IF(LEN(TRIM(FNAME)).EQ.0)THEN
  CLRFNAME=TRIM(PREFVAL(1))//'\*.CLR'
  IF(.NOT.UTL_WSELECTFILE('iMOD Preferences Colours (*.clr)|*.clr|',&
       SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,CLRFNAME,'Save iMOD Preferences Colours'))RETURN
 ELSE
  CLRFNAME=FNAME
 ENDIF
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=CLRFNAME,STATUS='UNKNOWN',ACTION='WRITE,DENYREAD')
 ALLOCATE(IRGB(3))
 WRITE(IU,*) '!##NO,RED,GREEN,BLUE'
 DO I=1,MAXCOLOUR
  CALL WRGBSPLIT(ICOLOR(I),IRGB(1),IRGB(2),IRGB(3))
  WRITE(IU,*) I,IRGB(1),IRGB(2),IRGB(3)
 END DO
 DEALLOCATE(IRGB)
 CLOSE(IU)

 END SUBROUTINE PREFSAVECOLOURS

 !###======================================================================
 SUBROUTINE PREFOPENCOLOURS(FNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN)      :: FNAME
 CHARACTER(LEN=256)               :: CLRFNAME
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IRGB
 INTEGER                          :: I,J,IU,IOS

 IF(LEN(TRIM(FNAME)).EQ.0)THEN
  CLRFNAME=TRIM(PREFVAL(1))//'\*.CLR'
  IF(.NOT.UTL_WSELECTFILE('iMOD Preferences Colours (*.clr)|*.clr|',&
       LOADDIALOG+PROMPTON+DIRCHANGE+APPENDEXT+MUSTEXIST,CLRFNAME,'Load iMOD Preferences Colours'))RETURN
 ELSE
  CLRFNAME=FNAME
 ENDIF
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=CLRFNAME,STATUS='OLD',ACTION='READ,DENYWRITE')
 ALLOCATE(IRGB(3))
 READ(IU,*)
 DO I=1,MAXCOLOUR
  READ(IU,*,IOSTAT=IOS) J,IRGB(1),IRGB(2),IRGB(3)
  IF(IOS.NE.0)EXIT
  IF(J.GT.0.AND.J.LE.MAXCOLOUR)ICOLOR(J)=WRGB(IRGB(1),IRGB(2),IRGB(3))
 END DO
 DEALLOCATE(IRGB)
 CLOSE(IU)

 END SUBROUTINE PREFOPENCOLOURS

 !###======================================================================
 SUBROUTINE PREFCOLOURSINIT(LSAVE)
 !###======================================================================
 IMPLICIT NONE
 LOGICAL,INTENT(IN) :: LSAVE
 INTEGER :: I
 REAL,ALLOCATABLE,DIMENSION(:) :: IRGB
 LOGICAL :: LEX

 LEX=.FALSE.
 IF(LSAVE)INQUIRE(FILE=TRIM(PREFVAL(1))//'\IMOD_INIT.CLR',EXIST=LEX)

 IF(.NOT.LEX)THEN
  ALLOCATE(IRGB(3))
  CALL IGRPALETTEINIT()
  DO I=1,MAXCOLOUR
   CALL RANDOM_NUMBER(IRGB)
   IRGB=IRGB*255.0
   ICOLOR(I)=WRGB(INT(IRGB(1)),INT(IRGB(2)),INT(IRGB(3)))
  END DO
  DEALLOCATE(IRGB)
  IF(LSAVE)CALL PREFSAVECOLOURS(TRIM(PREFVAL(1))//'\IMOD_INIT.CLR')
 ELSE
  CALL PREFOPENCOLOURS(TRIM(PREFVAL(1))//'\IMOD_INIT.CLR')
 ENDIF

! !## initialize colours for segment-package
 ICLRSC=WRGB(0,255,0)
 ICLRSD=WRGB(0,255,255)
 ICLRSP=WRGB(255,0,0)
 ICLRND=WRGB(0,0,255)
 ICLRST=WRGB(255,0,255)
 ICLRQH=WRGB(255,255,0)

 END SUBROUTINE PREFCOLOURSINIT

 !###======================================================================
 SUBROUTINE PREFUPDATE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL WDIALOGSELECT(ID_DPREFTAB1)
 CALL WDIALOGGETMENU(IDF_MENU1,I)
 IF(TRIM(PREFVAL(I)).EQ.'')THEN
  CALL WDIALOGCOLOUR(IDF_LABEL1,WRGB(250,0,0),-1)
  CALL WDIALOGPUTSTRING(IDF_LABEL1,'NOT a parameter asigned to this keyword, add in *.prf-file!')
 ELSE
  CALL WDIALOGCOLOUR(IDF_LABEL1,-1,-1)
  CALL WDIALOGPUTSTRING(IDF_LABEL1,TRIM(PREFVAL(I)))
 ENDIF

 END SUBROUTINE PREFUPDATE

 !###======================================================================
 SUBROUTINE PREFFIELDS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IRED,IGREEN,IBLUE,I

 CALL WDIALOGSELECT(ID_DPREFTAB2)
 CALL WDIALOGGETMENU(IDF_MENU1,I)
 CALL WDIALOGCOLOUR(IDF_LABEL9,RGBBACK=ICOLOR(I))
 CALL WRGBSPLIT(ICOLOR(I),IRED,IGREEN,IBLUE)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,IRED)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER2,IGREEN)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER3,IBLUE)

 END SUBROUTINE PREFFIELDS

 !###======================================================================
 SUBROUTINE PREFREAD(FNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IOS,I,IU,J,IOR
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 CHARACTER(LEN=256) :: DIRNAME,PRFNAME
 CHARACTER(LEN=25) :: CCTYPE
 LOGICAL :: LEX
 
 IF(LEN(TRIM(FNAME)).EQ.0)THEN
  PRFNAME=''
  IF(.NOT.UTL_WSELECTFILE('iMOD Preferences (*.prf)|*.prf|',&
       LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,PRFNAME,&
       'Load iMOD Preferences'))RETURN
 ELSE
  PRFNAME=FNAME
 ENDIF

 PREFVAL=''
 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(ID_SHOWNARROW,1,0)

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=PRFNAME,ACTION='READ,DENYWRITE',FORM='FORMATTED',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not read '//TRIM(PRFNAME),'Error')
  RETURN
 ENDIF

 IOR=0
 DO
  READ(IU,*,IOSTAT=IOS) CCTYPE,DIRNAME
  IF(LEN_TRIM(CCTYPE).EQ.0)EXIT
  IF(IOS.NE.0)EXIT
  CALL IUPPERCASE(CCTYPE)
  CALL IUPPERCASE(DIRNAME)
  !## get drive
  I  =INDEX(DIRNAME,':')
  IF(I.GT.0)THEN
   I=ICHAR(DIRNAME(:I))
   IF(INFOOPSYSTEM(I).EQ.DRIVEUNKNOWN)DIRNAME=''
  ENDIF

  DO I=1,MAXPREF
   IF(TRIM(CCTYPE).EQ.TRIM(PREF(I)))THEN
    PREFVAL(I)=DIRNAME
    EXIT
   ENDIF
  END DO

  SELECT CASE (I)
   !## user[1]
   CASE(1)
    J=LEN_TRIM(PREFVAL(I))
    IF(INDEX(PREFVAL(I),'\',.TRUE.).EQ.J)PREFVAL(I)(J:J)=' '
    CALL UTL_CREATEDIR(TRIM(PREFVAL(I)))
    CALL UTL_CREATEDIR(TRIM(PREFVAL(I))//'\LEGEND')
    CALL UTL_CREATEDIR(TRIM(PREFVAL(I))//'\TMP')
    CALL UTL_CREATEDIR(TRIM(PREFVAL(I))//'\RUNFILES')
    CALL UTL_CREATEDIR(TRIM(PREFVAL(I))//'\IMFILES')
    CALL UTL_CREATEDIR(TRIM(PREFVAL(I))//'\SHAPES')
    CALL UTL_CREATEDIR(TRIM(PREFVAL(I))//'\SETTINGS')
   !## vector[3],dbase[5]
   CASE(3,5)
    CALL UTL_CREATEDIR(TRIM(PREFVAL(I)))
   !## top25[2],help[4],modflow[8],irdbase[6],tags[7]
   CASE(2,4,6,7,8)
   CASE(10)
   !## northarrow
   CASE(11)
    INQUIRE(FILE=PREFVAL(I),EXIST=LEX)    
    IF(LEX)CALL WMENUSETSTATE(ID_SHOWNARROW,1,1)
   !## ro-tool
   CASE(14:24)
    IOR=IOR+1

  END SELECT
 END DO

 CLOSE(IU)

 IF(IOR.NE.11)CALL WMENUSETSTATE(ID_ROTOOL,1,0)
 IF(IOR.EQ.11)CALL WMENUSETSTATE(ID_ROTOOL,1,1)
 
 END SUBROUTINE PREFREAD

 !###======================================================================
 SUBROUTINE PREFINIT(IERROR)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IERROR
 LOGICAL :: LEX
 INTEGER :: IU
 
 IERROR=1

 CALL IOSDIRNAME(PREFDIR)
 INQUIRE(FILE=TRIM(PREFDIR)//'\IMOD_INIT.PRF',EXIST=LEX)
 !## create dummy imod_init.prf
 IF(.NOT.LEX)THEN
  CALL WMESSAGEBOX(YESNO,COMMONNO,QUESTIONICON,'Can not find '//TRIM(PREFDIR)//'\IMOD_INIT.PRF'//CHAR(13)// &
   'If you agree iMOD will create this file and add the keyword'//CHAR(13)//&
   'USER='//TRIM(PREFDIR)//'\IMOD_USER'//CHAR(13)//'After that iMOD can start!','Error')
  IF(WINFODIALOG(4).NE.1)THEN
   CALL WINDOWCLOSE()
   STOP
  ENDIF
  IU=UTL_GETUNIT()
  CALL OSD_OPEN(IU,FILE=TRIM(PREFDIR)//'\IMOD_INIT.PRF',STATUS='UNKNOWN')
  WRITE(IU,*) 'USER "'//TRIM(PREFDIR)//'\IMOD_USER"'
  CLOSE(IU) 
 ENDIF

 CALL PREFREAD(TRIM(PREFDIR)//'\IMOD_INIT.PRF')
 !not user keyword assigned
 IF(PREFVAL(1).EQ.'')RETURN

! OPENDIR=PREFVAL(1)
! IF(PREFVAL(5).NE.'')OPENDIR=PREFVAL(5)
! SAVEDIR=PREFVAL(1)

 MAXSHAPES=400 !400
 MAXSHPCRD=500  !0

 IERROR=0

 END SUBROUTINE PREFINIT

END MODULE MOD_PREF
