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
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_READINITFILE,UTL_DIRINFO_POINTER,UTL_CAP
USE MOD_IDF, ONLY : IDFDEALLOCATE,IDFNULLIFY
USE MOD_OSD, ONLY : OSD_GETARG,OSD_OPEN,OSD_GETENV
USE IMODVAR, ONLY : IDIAGERROR


CONTAINS

!###======================================================================
 SUBROUTINE PLUGINMAIN(IPLUGIN)
!###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPLUGIN
 INTEGER :: ITYPE,I,J,IROW,ICOL,IPI
 TYPE(WIN_MESSAGE) :: MESSAGE
 
 CALL WDIALOGLOAD(ID_DMANAGE_PLUGIN)
 
 SELECT CASE (IPLUGIN)
  CASE(ID_MANAGE_PLUGIN1)
   CALL WDIALOGTITLE('PlugIn Manager 1');IPI=27
  CASE(ID_MANAGE_PLUGIN2)
   CALL WDIALOGTITLE('Plugin Manager 2');IPI=28
 END SELECT
 
 CALL PLUGINREAD(IPI)
 CALL PLUGIN_FIELDCHANGE(IPI,1,1)
 CALL WDIALOGSHOW(-1,-1,0,2)
 
 !## forces the program to close the manager window
 DO WHILE(.TRUE.)
  CALL WMESSAGE(ITYPE,MESSAGE)
  
  SELECT CASE(ITYPE)
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDCANCEL)
      EXIT
    END SELECT
   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_GRID1)
      CALL WGRIDPOS(MESSAGE%Y,ICOL,IROW)
      CALL PLUGIN_FIELDCHANGE(IPI,IROW,ICOL)
    END SELECT

  END SELECT

 END DO
 CALL WDIALOGUNLOAD()
 
 END SUBROUTINE PLUGINMAIN

!###======================================================================
 SUBROUTINE PLUGIN_UPDATEMENU()
!###======================================================================
!### updates plugin-menu with ungraying the manager chosen by the user
 IMPLICIT NONE
 INTEGER :: I,J
 
 I=1; IF(LEN_TRIM(PREFVAL(27)).EQ.0)I=0
 CALL WMENUSETSTATE(ID_MANAGE_PLUGIN1,1,I)
 J=1; IF(LEN_TRIM(PREFVAL(28)).EQ.0)J=0
 CALL WMENUSETSTATE(ID_MANAGE_PLUGIN2,1,J)
 
 IF(I+J.EQ.0)THEN
  CALL WMENUSETSTATE(ID_PLUGIN,1,0); RETURN
 ENDIF  

 CALL WMENUSETSTATE(ID_PLUGIN,1,1)

 !## VULLEN VAN DEELMENUS ERONDER
 IF(ALLOCATED(PI))DEALLOCATE(PI)
 
 
 END SUBROUTINE PLUGIN_UPDATEMENU

 !###======================================================================
 SUBROUTINE PLUGIN_FIELDCHANGE(IPI,IROW,ICOL)   !#plugin file number=J,plugin folder number=I
 !###======================================================================
 !## subroutine to read ini-file of plugin tool and echoes this to plugin manager
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IROW,ICOL,IPI
 CHARACTER(LEN=256) :: DIRNAME
 CHARACTER(LEN=512) :: FNAME
 CHARACTER(LEN=1256) :: LINE
 INTEGER :: IOS,IU
 LOGICAL :: LEX
  
 CALL WGRIDGETCELLSTRING(IDF_GRID1,ICOL,IROW,DIRNAME)
 FNAME=TRIM(PREFVAL(IPI))//TRIM(DIRNAME)//'\PLUG-IN.INI'
 
 INQUIRE(FILE=FNAME,EXIST=LEX)
 IF(.NOT.LEX)THEN
  CALL WDIALOGPUTSTRING(IDF_LABEL1,'Error, can not find '//TRIM(FNAME))
  CALL WDIALOGFIELDSTATE(IDF_LABEL1,0)
  RETURN
 ENDIF
 
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 
 !## read variables from plugin ini-file
 IF(.NOT.UTL_READINITFILE('TXT',LINE,IU,0))RETURN !zelfs als het keyword wel bestaat krijg je de genoemde error en daarna wordt de betreffende file gewoon uitgelezen, hoe kan dit??
 !READ(LINE,*) FNAME
 
 CLOSE(IU)
 
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 READ(IU,'(A)') LINE
 CLOSE(IU)

 CALL WDIALOGPUTSTRING(IDF_LABEL1,TRIM(LINE))
 CALL WDIALOGFIELDSTATE(IDF_LABEL1,0)
  
 END SUBROUTINE PLUGIN_FIELDCHANGE
 
!###======================================================================
 SUBROUTINE PLUGINREAD(I)
!###======================================================================
 !## subroutine to read folderpath(s) and name(s) of plugin-ini files
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I
 CHARACTER(LEN=512) :: LINE,TXTFILE
 CHARACTER(LEN=256),DIMENSION(:),POINTER :: LISTNAME
 
 IF(.NOT.UTL_DIRINFO_POINTER(PREFVAL(I),'*',LISTNAME,'D'))THEN; ENDIF

 IF(ASSOCIATED(LISTNAME))THEN
  IF(SIZE(LISTNAME).EQ.0)THEN
   CALL WGRIDROWS(IDF_GRID1,1)
   CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,1,'No plugin found')
   CALL WDIALOGFIELDSTATE(IDF_GRID1,0)
  ELSE
   CALL WGRIDROWS(IDF_GRID1,SIZE(LISTNAME))
   CALL WGRIDPUTSTRING(IDF_GRID1,1,LISTNAME,SIZE(LISTNAME))
  ENDIF
  CALL WGRIDSTATECELL(IDF_GRID1,1,1,2)
 ENDIF
 !## checken vlag iact als plugin al in imf staat
!  CALL WGRIDPUTCHECKBOX(IDF_GRID1,1,PI(,J)%IACT,SIZE(LISTNAME))

 DEALLOCATE(LISTNAME)

 END SUBROUTINE PLUGINREAD
 
END MODULE MOD_PLUGIN
