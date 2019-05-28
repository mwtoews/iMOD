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
MODULE MOD_START

USE WINTERACTER
USE RESOURCE

USE IMODVAR, ONLY : DP_KIND,SP_KIND,IMFFNAME 
USE MOD_PREF, ONLY : PREFMAIN
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_UTL, ONLY : UTL_DIRINFO,UTL_GETUNIT,UTL_DIRINFO_POINTER,UTL_GETHELP
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_PLUGIN, ONLY : PLUGIN_INITMENU_FILL,PLUGIN_SETTIMER
USE MOD_START_PAR
USE MOD_MAIN_UTL

CONTAINS

 !###======================================================================
 SUBROUTINE START_MAIN(IDEXIT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IDEXIT
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,I
 CHARACTER(LEN=256) :: IMFFILE,FNAME

 CALL START_INIT()
 CALL START_FILLMENU()

 CALL WDIALOGSHOW(-1,-1,0,3)
 DO

  CALL WMESSAGE(ITYPE,MESSAGE)

  SELECT CASE (ITYPE)

   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_STRING1,IDF_RADIO3,IDF_RADIO4,IDF_RADIO5)
      IF(MESSAGE%VALUE1.EQ.MESSAGE%VALUE2)CALL START_FILLMENU()
     CASE (IDF_RADIO1,IDF_RADIO2)
      CALL START_FIELDS()
    END SELECT

   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)

     !## get file in editor
     CASE (ID_DELETE)
      CALL WDIALOGGETMENU(IDF_MENU1,I,IMFFILE)
      FNAME=TRIM(PREFVAL(1))//'\IMFILES\'//TRIM(IMFFILE)
      CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to delete the file:'//CHAR(13)//TRIM(FNAME),'Question')
      IF(WINFODIALOG(4).EQ.1)THEN
       CALL IOSDELETEFILE(FNAME)
       CALL START_FILLMENU()
      ENDIF

     !## get file in editor
     CASE (ID_INFO)
      CALL WDIALOGGETMENU(IDF_MENU1,I,IMFFILE)
      FNAME=TRIM(PREFVAL(1))//'\IMFILES\'//TRIM(IMFFILE)
      CALL WINDOWOPENCHILD(I,FLAGS=HIDEWINDOW,TITLE='File ')
      CALL WEDITFILE(FNAME,MODAL,0,0,COURIERNEW,ISIZE=8)

     CASE (IDHELP)
       CALL UTL_GETHELP('2.1','GS.StartiMOD')
     CASE (ID_PREFERENCES)

      CALL PREFMAIN()
      DIR=TRIM(PREFVAL(1))//'\IMFILES'
      CALL START_FILLMENU()

     !## open different imf
     CASE (ID_OPEN)
      CALL WDIALOGUNLOAD()

      CALL MAIN_UTL_LOAD_SAVE_IMF(ID_OPEN,I)
      IF(I.EQ.0)THEN
       CALL START_INIT()
       CALL START_FILLMENU()
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
       IMFFNAME=TRIM(PREFVAL(1))//'\IMFILES\'//TRIM(IMFFILE)
       CALL MAIN_UTL_LOAD_IMF(); CALL MAIN_UTL_LOAD()
      ELSE
       CALL WDIALOGUNLOAD()
       IF(PLUGIN_INITMENU_FILL())THEN; ENDIF
      ENDIF
      IDEXIT=1
      EXIT
    END SELECT

  END SELECT

 END DO

 IF(ASSOCIATED(STLISTNAME))DEALLOCATE(STLISTNAME)

 END SUBROUTINE START_MAIN

 !###======================================================================
 SUBROUTINE START_INIT()
 !###======================================================================
 IMPLICIT NONE

 CALL WDIALOGLOAD(ID_DSTART,ID_DSTART)
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPEN,1)
 CALL WDIALOGPUTIMAGE(ID_INFO,ID_ICONINFO,1)
 CALL WDIALOGPUTIMAGE(ID_DELETE,ID_ICONDELETE,1)
 CALL WDIALOGPUTSTRING(IDF_STRING1,'*')
 CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO4)
 CALL WDIALOGFIELDOPTIONS(IDF_STRING1,EDITFIELDCHANGED,1)

 IF(PLUGIN_INITMENU_FILL())THEN; ENDIF
 CALL PLUGIN_SETTIMER()
 
 DIR=TRIM(PREFVAL(1))//'\IMFILES'
 CALL WDIALOGPUTSTRING(IDF_RADIO1,'Create a new iMOD Project')
 CALL WDIALOGPUTSTRING(IDF_RADIO2,'Open an existing iMOD Project')
 CALL WDIALOGTITLE('Start iMOD')
 CALL WDIALOGPUTSTRING(ID_PREFERENCES,'Preferences ...')
 
 END SUBROUTINE START_INIT

 !###======================================================================
 SUBROUTINE START_FIELDS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,IRADIO

 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,I); I=I-1
 J=0; IF(ASSOCIATED(STLISTNAME))J=I*1

 CALL WDIALOGFIELDSTATE(IDF_MENU1,J)
 CALL WDIALOGFIELDSTATE(ID_OPEN,J)
 CALL WDIALOGFIELDSTATE(ID_INFO,J)
 CALL WDIALOGFIELDSTATE(ID_DELETE,J)
 CALL WDIALOGFIELDSTATE(IDF_STRING1,I)

 CALL WDIALOGFIELDSTATE(IDF_LABEL1,I)
 CALL WDIALOGFIELDSTATE(IDF_RADIO3,I)
 CALL WDIALOGFIELDSTATE(IDF_RADIO4,I)
 CALL WDIALOGFIELDSTATE(IDF_RADIO5,I)
 CALL WDIALOGFIELDSTATE(IDF_LABEL2,I)

 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO3,IRADIO)
 SELECT CASE (IRADIO)
  CASE (1)
   CALL WDIALOGPUTSTRING(IDF_LABEL2,'Alphabetic Sort (A-Z)')
  CASE (2)
   CALL WDIALOGPUTSTRING(IDF_LABEL2,'By date/time (newest first)')
  CASE (3)
   CALL WDIALOGPUTSTRING(IDF_LABEL2,'By Size (largest first)')
 END SELECT
 
 K=1; IF(I.EQ.1)K=J
 CALL WDIALOGFIELDSTATE(IDOK,K)

 END SUBROUTINE START_FIELDS

 !###======================================================================
 SUBROUTINE START_FILLMENU()
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=52) :: WC
 INTEGER :: IRADIO
 LOGICAL :: LEX
 CHARACTER(LEN=2),DIMENSION(3) :: CORDER
 DATA CORDER/'N ','-D','-S'/
 
 CALL WDIALOGSELECT(ID_DSTART)
 CALL WDIALOGGETSTRING(IDF_STRING1,WC)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO3,IRADIO)
  
 IF(LEN_TRIM(WC).EQ.0)WC='*'
 IF(INDEX(WC,'.').GT.0)WC=WC(:INDEX(WC,'.',.TRUE.)-1)
 WC=TRIM(WC)//'.IMF'

 LEX=UTL_DIRINFO_POINTER(DIR,WC,STLISTNAME,'F',CORDER=CORDER(IRADIO))
 IF(SIZE(STLISTNAME).LE.0)THEN
  IF(ASSOCIATED(STLISTNAME))DEALLOCATE(STLISTNAME)
  LEX=.FALSE.
 ENDIF
 
 IF(LEX)THEN
 
  CALL WDIALOGFIELDSTATE(IDF_MENU1,1)
  CALL WDIALOGPUTMENU(IDF_MENU1,STLISTNAME,SIZE(STLISTNAME),1)
 
 ELSE
 
  CALL WDIALOGCLEARFIELD(IDF_MENU1)
 
 ENDIF

 CALL START_FIELDS()

 END SUBROUTINE START_FILLMENU

END MODULE

