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
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_READINITFILE,UTL_DIRINFO_POINTER
USE MOD_IDF, ONLY : IDFDEALLOCATE,IDFNULLIFY
USE MOD_OSD, ONLY : OSD_GETARG,OSD_OPEN,OSD_GETENV
USE IMODVAR, ONLY : IDIAGERROR


CONTAINS

!###======================================================================
 SUBROUTINE PLUGINMAIN(IPLUGIN)
!###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPLUGIN
 INTEGER :: ITYPE,I
 TYPE(WIN_MESSAGE) :: MESSAGE
 
 CALL WDIALOGLOAD(ID_DMANAGE_PLUGIN)
 
 SELECT CASE (IPLUGIN)
  CASE(ID_MANAGE_PLUGIN1)
   CALL WDIALOGTITLE('PlugIn Manager 1');I=1
  CASE(ID_MANAGE_PLUGIN2)
   CALL WDIALOGTITLE('Plugin Manager 2');I=2
 END SELECT
 
 !CALL PLUGINREAD(I)
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
  END SELECT

 END DO
 CALL WDIALOGUNLOAD()
 
 END SUBROUTINE PLUGINMAIN

!###======================================================================
 SUBROUTINE PLUGUPDATEMENU()
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
 
 
 END SUBROUTINE PLUGUPDATEMENU

!###======================================================================
 SUBROUTINE PLUGFIELDCHANGE()
!###======================================================================
 !## subroutine to read ini-file of plugin tool and echoes this to plugin manager
! IMPLICIT NONE
! CHARACTER(LEN=256):: ARGSTRING,LINE
! INTEGER :: IOS, IU, I
 
! IU=UTL_GETUNIT()
! CALL OSD_OPEN(IU,FILE=ARGSTRING,STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 
! ALLOCATE(PI(:,2)); DO I=1,SIZE(PI); CALL IDFNULLIFY(PI(:,I)); ENDDO
 
!## read plugin folder name and save in PIOBJ
! IF(.NOT.UTL_READINITFILE('PLUGIN1',LINE,IU,0))RETURN   !# foldername plugin 1
! READ(LINE,*) PI(:,1)%FNAME
! IF(.NOT.UTL_READINITFILE('PLUGIN2',LINE,IU,0))RETURN   !# foldername plugin 2
! READ(LINE,*) PI(:,2)%FNAME
 
 END SUBROUTINE PLUGFIELDCHANGE
 
!###======================================================================
 SUBROUTINE PLUGINREAD(I)
!###======================================================================
 !## subroutine to read folderpath(s) and name(s) of plugin-ini files
 !IMPLICIT NONE
 !CHARACTER(LEN=512) :: LINE,TXTFILE
 !CHARACTER(LEN=256),DIMENSION(:),POINTER :: PLLISTNAME
 !CHARACTER(LEN=256) :: FPL
 !CHARACTER(LEN=52) :: WC 
 !INTEGER, INTENT(IN) :: I
 !INTEGER :: J,N,IOS,IU
 
 !WC=TRIM(FPL(INDEX(FPL,'\',.TRUE.)+1:)); FPL=FPL(:INDEX(FPL,'\',.TRUE.)-1)
 !IF(.NOT.UTL_DIRINFO_POINTER(TRIM(PREFVAL(27))//,PLLISTNAME,'F'))STOP
 !IF(.NOT.UTL_DIRINFO_POINTER(FPL,WC,PLLISTNAME,'F'))STOP


 END SUBROUTINE PLUGINREAD
  
 
END MODULE MOD_PLUGIN
