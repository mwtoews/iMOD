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
MODULE MOD_PLUGIN

USE WINTERACTER
USE RESOURCE
USE MOD_PLUGIN_PAR
USE MOD_PREF_PAR, ONLY : PREFVAL        !## used for calling variables from preference file (*.prf) e.g. needed directorypath
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_READINITFILE,UTL_CAP,ITOS,UTL_CREATEDIR,JDATETOGDATE,UTL_IMODFILLMENU,CLOSEUNITS,UTL_WSELECTFILE
USE MOD_IDF, ONLY : IDFDEALLOCATE,IDFNULLIFY
USE MOD_OSD, ONLY : OSD_OPEN
USE IMODVAR, ONLY : IDIAGERROR

CONTAINS

!###======================================================================
 SUBROUTINE PLUGINMAIN(ITYPE,MESSAGE)
!###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 
 CALL WDIALOGSELECT(MESSAGE%WIN)
 SELECT CASE (MESSAGE%WIN)

  !## plugin manager 1
  CASE (ID_DMANAGE_PLUGIN1)
   SELECT CASE (ITYPE)
   
   CASE(PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)

    !## close Plugin-tool
       CASE (IDCANCEL)
        CALL PLUGINCLOSE()

    !## save and close
!       CASE (IDOK)
!        CALL PLUGINSAVE()

!       CASE(IDHELP)
!       CALL IMODGETHELP(,)

    END SELECT
   
   END SELECT
 
   !## plugin manager 2
  CASE (ID_DMANAGE_PLUGIN2)
   SELECT CASE (ITYPE)
   CALL WDIALOGTITLE("Plugin Manager 2")
   
   CASE(PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)

    !## close Plugin-tool
       CASE (IDCANCEL)
        CALL PLUGINCLOSE()

    !## save and close
!       CASE (IDOK)
!        CALL PLUGINSAVE()

!       CASE(IDHELP)
!       CALL IMODGETHELP(,)

    END SELECT
   
   END SELECT
  
 END SELECT
 
 END SUBROUTINE PLUGINMAIN
 
!###======================================================================
 SUBROUTINE PLUGINIT()
!###======================================================================
 IMPLICIT NONE
 
 CALL WMENUSETSTATE(ID_PLUGIN,1,1)
 CALL WDIALOGLOAD(ID_MANAGE_PLUGIN,ID_DMANAGE_PLUGIN1)
 CALL WDIALOGSHOW(-1,-1,0,2) 
 
 END SUBROUTINE PLUGINIT

 !###======================================================================
 SUBROUTINE PLUGINCLOSE()
 !###======================================================================
 IMPLICIT NONE

 IDIAGERROR=1

 !## save scenario?
! CALL WMESSAGEBOX(YESNOCANCEL,QUESTIONICON,COMMONYES, &
!      'Do you want to save the current settings'//CHAR(13)//'before stopping the Plugin manager?','Question')
! IF(WINFODIALOG(4).EQ.0)RETURN  !## cancel
! IF(WINFODIALOG(4).EQ.1)THEN
!  IF(.NOT.PLUGINSAVELOAD(ID_SAVEAS))RETURN  !## yes, do save and quit if succesfull
! ENDIF

! CALL PLUGINDEALLOCATE()

 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(ID_MANAGE_PLUGIN,2,0)

 CALL WDIALOGSELECT(ID_MANAGE_PLUGIN)
 CALL WDIALOGUNLOAD()

 IDIAGERROR=0

 END SUBROUTINE PLUGINCLOSE

 !###======================================================================
! SUBROUTINE PLUGINDEALLOCATE()
 !###======================================================================
! IMPLICIT NONE
! INTEGER :: I
! 
!  CALL CLOSEUNITS()
!
! END SUBROUTINE PLUGINDEALLOCATE
!
END MODULE MOD_PLUGIN
