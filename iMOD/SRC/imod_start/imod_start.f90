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
MODULE MOD_START

USE WINTERACTER
USE RESOURCE

USE IMODVAR, ONLY : IMFFNAME,RVERSION
USE MOD_PREF, ONLY : PREFMAIN
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_UTL, ONLY : UTL_DIRINFO,UTL_GETUNIT
USE MOD_IR_QPF, ONLY : IR1PRJFILES
USE MOD_IR_PAR,ONLY : MAINRESDIR
USE MOD_OSD, ONLY : OSD_OPEN

CHARACTER(LEN=256) :: DIR

CONTAINS

 !###======================================================================
 SUBROUTINE START_MAIN(EXT,IDEXIT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IDEXIT
 CHARACTER(LEN=*),INTENT(IN) :: EXT
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,I
 CHARACTER(LEN=256) :: IMFFILE,FNAME

 SELECT CASE (EXT)
  CASE ('IMF','QPF')
  CASE DEFAULT
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not initiate start-dialog','Error')
   IDEXIT=0
   RETURN
 END SELECT

 CALL START_LOAD(EXT)
 CALL START_INIT(EXT)
 CALL START_FIELDS()

 CALL WDIALOGSHOW(-1,-1,0,3)
 DO

  CALL WMESSAGE(ITYPE,MESSAGE)
!WRITE(*,*) itype,message%win
!  CALL WDIALOGSELECT(MESSAGE%WIN)

  SELECT CASE (ITYPE)

   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_RADIO1,IDF_RADIO2)
      CALL START_FIELDS()
    END SELECT

   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)

     !## get file in editor
     CASE (ID_DELETE)
      CALL WDIALOGGETMENU(IDF_MENU1,I,IMFFILE)
      IF(TRIM(EXT).EQ.'IMF')THEN
       FNAME=TRIM(PREFVAL(1))//'\IMFILES\'//TRIM(IMFFILE)
      ELSEIF(TRIM(EXT).EQ.'QPF')THEN
       FNAME=TRIM(MAINRESDIR)//'\'//TRIM(IMFFILE)
      ENDIF
      CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to delete the file:'//CHAR(13)//TRIM(FNAME),'Question')
      IF(WINFODIALOG(4).EQ.1)THEN
       CALL IOSDELETEFILE(FNAME)
       CALL START_INIT(EXT)
      ENDIF

     !## get file in editor
     CASE (ID_INFO)
      CALL WDIALOGGETMENU(IDF_MENU1,I,IMFFILE)
      IF(TRIM(EXT).EQ.'IMF')THEN
       FNAME=TRIM(PREFVAL(1))//'\IMFILES\'//TRIM(IMFFILE)
      ELSEIF(TRIM(EXT).EQ.'QPF')THEN
       FNAME=TRIM(MAINRESDIR)//'\'//TRIM(IMFFILE)
      ENDIF
      CALL WINDOWOPENCHILD(I,FLAGS=HIDEWINDOW,TITLE='File ')!//TRIM(FNAME))
      CALL WEDITFILE(FNAME,MODAL,0,0,COURIERNEW,ISIZE=8)

     CASE (IDHELP)
       CALL IMODGETHELP('2.1','Starting iMOD')
     CASE (ID_PREFERENCES)

      IF(TRIM(EXT).EQ.'IMF'.OR.TRIM(EXT).EQ.'QPF')THEN
       CALL PREFMAIN()
       IF(TRIM(EXT).EQ.'IMF')THEN
        DIR=TRIM(PREFVAL(1))//'\IMFILES'
       ELSEIF(TRIM(EXT).EQ.'QPF')THEN
        DIR=TRIM(MAINRESDIR)
       ENDIF
       CALL START_INIT(EXT)
      ENDIF

     !## open different imf
     CASE (ID_OPEN)
      CALL WDIALOGUNLOAD()

      IF(TRIM(EXT).EQ.'IMF')THEN
       CALL IMODLOADSAVE(ID_OPEN,I)
      ELSEIF(TRIM(EXT).EQ.'QPF')THEN
       I=1
       IF(.NOT.IR1PRJFILES(ID_OPEN))I=0
      ENDIF
      IF(I.EQ.0)THEN
       CALL START_LOAD(EXT)
       CALL START_INIT(EXT)
       CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO2)
       CALL START_FIELDS()
       CALL WDIALOGSHOW(-1,-1,0,3)
      ELSE
       !## okay
       IDEXIT=1
       EXIT
      ENDIF

     CASE (IDCANCEL)
      !## stopped
      IDEXIT=0
      CALL WDIALOGUNLOAD()
      EXIT

     CASE (IDOK)
      CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,I)
      IF(I.EQ.2)THEN
       CALL WDIALOGGETMENU(IDF_MENU1,I,IMFFILE)
       CALL WDIALOGUNLOAD()
       IF(TRIM(EXT).EQ.'IMF')THEN
        IMFFNAME=TRIM(PREFVAL(1))//'\IMFILES\'//TRIM(IMFFILE)
        CALL IMODLOADIMF()
       ELSEIF(TRIM(EXT).EQ.'QPF')THEN
        FNAME=TRIM(MAINRESDIR)//'\'//TRIM(IMFFILE)
        IF(.NOT.IR1PRJFILES(ID_OPEN,FNAME))THEN
        ENDIF
       ENDIF
      ELSE
       CALL WDIALOGUNLOAD()
      ENDIF
      IDEXIT=1
      EXIT
    END SELECT

  END SELECT

 END DO

 END SUBROUTINE START_MAIN

 !###======================================================================
 SUBROUTINE START_LOAD(EXT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: EXT

 CALL WDIALOGLOAD(ID_DSTART,ID_DSTART)
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPEN,1)
 CALL WDIALOGPUTIMAGE(ID_INFO,ID_ICONINFO,1)
 CALL WDIALOGPUTIMAGE(ID_DELETE,ID_ICONDELETE,1)

 IF(TRIM(EXT).EQ.'IMF')THEN
  DIR=TRIM(PREFVAL(1))//'\IMFILES'
  CALL WDIALOGPUTSTRING(IDF_RADIO1,'Create a new iMOD Project')
  CALL WDIALOGPUTSTRING(IDF_RADIO2,'Open an existing iMOD Project')
  CALL WDIALOGTITLE('Start iMOD')
  CALL WDIALOGPUTSTRING(ID_PREFERENCES,'Preferences ...')
 ELSEIF(TRIM(EXT).EQ.'QPF')THEN
  DIR=TRIM(MAINRESDIR)
  CALL WDIALOGPUTSTRING(IDF_RADIO1,'Create a new Quick-Scan Project')
  CALL WDIALOGPUTSTRING(IDF_RADIO2,'Open an existing Quick-Scan Project')
  CALL WDIALOGTITLE('Start Quick-Scan Tool')
  CALL WDIALOGFIELDSTATE(ID_PREFERENCES,3)
  CALL WDIALOGFIELDSTATE(ID_OPEN,3)
  CALL WDIALOGPUTSTRING(ID_PREFERENCES,'Preferences ...')
 ENDIF

 END SUBROUTINE START_LOAD

 !###======================================================================
 SUBROUTINE START_FIELDS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,I)
 IF(I.EQ.1)THEN
  I=2
 ELSE
  I=1
 ENDIF
 CALL WDIALOGFIELDSTATE(IDF_MENU1,I)
 CALL WDIALOGFIELDSTATE(ID_OPEN,I)
 CALL WDIALOGFIELDSTATE(ID_INFO,I)
 CALL WDIALOGFIELDSTATE(ID_DELETE,I)

 END SUBROUTINE START_FIELDS

 !###======================================================================
 SUBROUTINE START_INIT(EXT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: EXT
 INTEGER :: NRES
 CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: LISTNAME

 CALL WDIALOGSELECT(ID_DSTART)

 CALL IOSDIRENTRYTYPE('F')
 CALL IOSDIRCOUNT(DIR,'*.'//TRIM(EXT),NRES)

 IF(NRES.GT.0)THEN
  ALLOCATE(LISTNAME(NRES))
  CALL UTL_DIRINFO(DIR,'*.'//TRIM(EXT),LISTNAME,NRES,'F')
  CALL WDIALOGFIELDSTATE(IDF_MENU1,1)
  CALL WDIALOGPUTMENU(IDF_MENU1,LISTNAME,NRES,1)
  DEALLOCATE(LISTNAME)
  CALL WDIALOGFIELDSTATE(IDF_RADIO2,1)
  CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO2)
 ELSE
  CALL WDIALOGFIELDSTATE(IDF_RADIO2,0)
  CALL WDIALOGCLEARFIELD(IDF_MENU1)
  CALL WDIALOGFIELDSTATE(IDF_MENU1,0)
  CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO1)
 ENDIF

 CALL WDIALOGSETFIELD(IDOK)
 CALL START_FIELDS()

 END SUBROUTINE START_INIT

END MODULE

