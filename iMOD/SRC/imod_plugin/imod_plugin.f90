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
 SUBROUTINE PLUGINMAIN(IPLUGIN)
!###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPLUGIN
 INTEGER :: ITYPE
 TYPE(WIN_MESSAGE) :: MESSAGE
 
 CALL WMENUSETSTATE(ID_PLUGIN,1,1)
 CALL WDIALOGLOAD(ID_MANAGE_PLUGIN)
 CALL WDIALOGSHOW(-1,-1,0,2)
 
 CALL PLUGUPDATE(IPLUGIN)
 
 !## forces the program to close the manager window
 DO WHILE(.TRUE.)
  CALL WMESSAGE(ITYPE,MESSAGE)
  IF(ITYPE.EQ.PUSHBUTTON.AND.MESSAGE%VALUE1.EQ.IDCANCEL)EXIT 
 END DO
 CALL WDIALOGUNLOAD()
 
 END SUBROUTINE PLUGINMAIN

!###======================================================================
 SUBROUTINE PLUGUPDATE(IPLUGIN)
!###======================================================================
!### updates plugin-menu with ungraying the manager chosen by the user
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPLUGIN
 
 SELECT CASE (IPLUGIN)
  CASE(ID_DMANAGE_PLUGIN1)
   CALL WMENUSETSTATE(ID_DMANAGE_PLUGIN1,1,0)
  CASE(ID_DMANAGE_PLUGIN2)
   CALL WMENUSETSTATE(ID_DMANAGE_PLUGIN2,1,0)
   CALL WDIALOGTITLE('Plugin Manager 2')
 END SELECT
  
 END SUBROUTINE PLUGUPDATE
 
!###======================================================================
 SUBROUTINE PLUGINIT()
!###======================================================================
 IMPLICIT NONE
 
 END SUBROUTINE PLUGINIT

END MODULE MOD_PLUGIN
