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
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_READINITFILE,UTL_DIRINFO_POINTER,UTL_CAP,ITOS
USE MOD_IDF, ONLY : IDFDEALLOCATE,IDFNULLIFY
USE MOD_OSD, ONLY : OSD_GETARG,OSD_OPEN,OSD_GETENV
USE IMODVAR, ONLY : IDIAGERROR

CONTAINS

!###======================================================================
 SUBROUTINE PLUGIN_MAIN(IPLUGIN)
!###======================================================================
 !# subroutine with main-program; includes call to all plugin-subroutines
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPLUGIN
 INTEGER :: ITYPE,I,J,IROW,ICOL,IPI,N
 TYPE(WIN_MESSAGE) :: MESSAGE
 
 CALL WDIALOGLOAD(ID_DMANAGE_PLUGIN)
 
 SELECT CASE (IPLUGIN)
  CASE(ID_MANAGE_PLUGIN1)
   CALL WDIALOGTITLE('PlugIn Manager 1');IPI=27
  CASE(ID_MANAGE_PLUGIN2)
   CALL WDIALOGTITLE('Plugin Manager 2');IPI=28
 END SELECT
 
 CALL PLUGIN_READ(IPI)
 CALL WGRIDSETCELL(IDF_GRID1,1,1)
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
     CASE (IDOK)    
      IF(IPI.EQ.27)THEN; N=SIZE(PI1); CALL WGRIDGETCHECKBOX(IDF_GRID1,2,PI1%IACT,N); ENDIF
      IF(IPI.EQ.28)THEN; N=SIZE(PI2); CALL WGRIDGETCHECKBOX(IDF_GRID1,2,PI2%IACT,N); ENDIF
      IF(PLUGIN_UPDATEMENU_FILL())EXIT
    END SELECT
   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_GRID1)
      CALL WGRIDPOS(MESSAGE%Y,ICOL,IROW) !#y="to"
      CALL PLUGIN_FIELDCHANGE(IPI,IROW,ICOL)
    END SELECT

  END SELECT

 END DO
 
 CALL WDIALOGUNLOAD()
 
 END SUBROUTINE PLUGIN_MAIN

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
    
 END SUBROUTINE PLUGIN_UPDATEMENU

 !###======================================================================
 SUBROUTINE PLUGIN_FIELDCHANGE(IPI,IROW,ICOL)
 !###======================================================================
 !## subroutine to read ini-file of plugin tool and echoes this to plugin manager
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IROW,ICOL,IPI
 CHARACTER(LEN=52)  :: EXE
 CHARACTER(LEN=256) :: DIRNAME
 CHARACTER(LEN=1256):: LINE,FNAME
 INTEGER :: IOS,IU
 LOGICAL :: LEX
  
 !#sets directory+filename
 CALL WGRIDGETCELLSTRING(IDF_GRID1,ICOL,IROW,DIRNAME)
 FNAME=TRIM(PREFVAL(IPI))//'\'//TRIM(DIRNAME)//'\PLUG-IN.INI'
 
 !# raise expection error if the specific file cannot be found or an none-existing directory is given in preference file.
 INQUIRE(FILE=FNAME,EXIST=LEX)
 IF(.NOT.LEX)THEN
  CALL WDIALOGPUTSTRING(IDF_LABEL1,'Error, cannot find '//TRIM(FNAME))
  CALL WDIALOGFIELDSTATE(IDF_LABEL1,0)
  RETURN
 ENDIF
 
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 
 !## read variables from plugin ini-file
 IF(.NOT.UTL_READINITFILE('TXT',LINE,IU,0))RETURN
 READ(LINE,*) FNAME
 CLOSE(IU)
 
 !#select TXT-file from ini-file and read this file, echo to description part of plugin-manager
 FNAME=TRIM(PREFVAL(IPI))//'\'//TRIM(DIRNAME)//'\'//TRIM(FNAME)
  
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 
 READ(IU,'(A)') LINE
 CLOSE(IU)

 CALL WDIALOGPUTSTRING(IDF_LABEL1,TRIM(LINE))
 CALL WDIALOGFIELDSTATE(IDF_LABEL1,0)
 
 CLOSE(IU)
 
 END SUBROUTINE PLUGIN_FIELDCHANGE
 
!###======================================================================
 SUBROUTINE PLUGIN_READ(I)
!###======================================================================
 !## subroutine to read folderpath(s) and name(s) of plugin-ini files
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I
 CHARACTER(LEN=512) :: LINE,TXTFILE
 CHARACTER(LEN=256),DIMENSION(:),POINTER :: LISTNAME
 INTEGER :: NROW,ICOL,IROW
 
 !#lists all file names in specific plugin-folder
 IF(.NOT.UTL_DIRINFO_POINTER(PREFVAL(I),'*',LISTNAME,'D'))THEN; ENDIF

!# check the amount of plugins in folder, if none returns "No plugin found" in window.
 IF(ASSOCIATED(LISTNAME))THEN
  IF(SIZE(LISTNAME).EQ.0)THEN
   CALL WGRIDROWS(IDF_GRID1,1)
   CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,1,'No plugin found')
   CALL WDIALOGFIELDSTATE(IDF_GRID1,0)
  ELSE
   CALL WGRIDROWS(IDF_GRID1,SIZE(LISTNAME))
   CALL WGRIDPUTSTRING(IDF_GRID1,1,LISTNAME,SIZE(LISTNAME))
  ENDIF
  !# outgraying of all the rows in the first column of the plugin manager
  !# in order to prevent the user from modifying the text in the dialog
  NROW=SIZE(LISTNAME)
  DO IROW = 1,NROW
   CALL WGRIDSTATECELL(IDF_GRID1,1,IROW,2)
  ENDDO  
 ENDIF
 
 !## check flag iact on occurance plugin in imf-file and save in imf-file after plugin-session 
 !## (happens in imod_main.f90 -> imodsaveimf/imodloadimf).
 
 IF(I.EQ.27)THEN  
  CALL PLUGIN_CHECK(PI1,LISTNAME,SIZE(LISTNAME))
 ELSEIF(I.EQ.28)THEN
  CALL PLUGIN_CHECK(PI2,LISTNAME,SIZE(LISTNAME))
 ENDIF
  
 DEALLOCATE(LISTNAME)
 
 END SUBROUTINE PLUGIN_READ

!###======================================================================
 SUBROUTINE PLUGIN_CHECK(PI,LN,NLN)
!###======================================================================
!# Subroutine to check if PI# contains the same filenames as Listname (folder)
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NLN
 TYPE(PIOBJ),POINTER,DIMENSION(:),INTENT(INOUT) :: PI
 CHARACTER(LEN=*),DIMENSION(NLN),INTENT(IN) :: LN
 TYPE(PIOBJ),POINTER,DIMENSION(:) :: PI3
 INTEGER :: J,K
 
  IF(.NOT.ASSOCIATED(PI))THEN
   ALLOCATE(PI(NLN))
   PI%PNAME=LN
   PI%IACT=0
  ELSE
  !# check content of saved PI-list with avaiable plug-in files in specific folder
  !# In case size(PI) isn't equal to size(listname) and if both lists are equal in size
  !# this routine will be executed. Size(PI) needs to be equal to size(Listname)!!
  !# If size(pi) is not equal to size(listname) iact=0
   ALLOCATE(PI3(SIZE(PI))); PI3=PI
   DEALLOCATE(PI); ALLOCATE(PI(NLN)); PI%PNAME=LN; PI%IACT=0
   DO K=1,NLN
    DO J=1,SIZE(PI)
     IF(TRIM(UTL_CAP(LN(K),'U')).EQ.TRIM(UTL_CAP(PI(J)%PNAME,'U')))THEN
      IF(K.LE.SIZE(PI3))THEN; PI(K)%IACT=PI3(J)%IACT; EXIT; ENDIF
     ENDIF
    ENDDO
   ENDDO
   DEALLOCATE(PI3)   
  ENDIF
  CALL WGRIDPUTCHECKBOX(IDF_GRID1,2,PI(:)%IACT,NLN)  

 END SUBROUTINE PLUGIN_CHECK

 !###======================================================================
 LOGICAL FUNCTION PLUGIN_UPDATEMENU_FILL()
 !###======================================================================
!# Function to connect the plugin-names to the plugin-menu
 IMPLICIT NONE
 INTEGER :: I,J,K,L
 CHARACTER(LEN=3) :: AMOUNT

!##Procedure:
!# 1. Call menu/submenu in mainmenu 
!# 2. Change name of id-menu parts based on available PI-names
!# 3. Fill menu based on available plugins in plugin-folder(s) for both PI1 and PI2 until total list has a size of 10.
!# 4. Link routine to change the names to the apply/save button in the manager.
!#    Only update the names if apply/save button is called, else nothing will be read into the menu.
!# 6. If less than 10 names are available and activated in total in manager 1 and 2, the menuid place will be hided.
!# 7. If more than 10 names are available and activated in total in manager 1 and 2, an error message will show up which counts the exceedence amount. 
!#    {if>10 then: "Menu is full. Amount of available places is exeeded with {amount-10}}     
 
 PLUGIN_UPDATEMENU_FILL = .FALSE.
 
 DO I=1,10; CALL WMENUITEMDELETE(MENUID(I)); ENDDO
 
 J=0
 
  DO I=SIZE(PI2),1,-1
  IF(PI2(I)%IACT.EQ.1)THEN
   J=J+1
   IF(J.GT.10)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Menu is full. Amount of available places is exeeded with '//TRIM(ITOS(J-10)),'Error')
    RETURN     
   ENDIF
   CALL WMENUITEMINSERT(ID_MANAGE_PLUGIN2,2,MENUID(J),'PL2 '//TRIM(PI2(I)%PNAME))
   PI2(I)%ID=MENUID(J)
  ELSE
   PI2(I)%ID=0
  ENDIF
 ENDDO
 
 DO I=SIZE(PI1),1,-1
  IF(PI1(I)%IACT.EQ.1)THEN
   J=J+1
   IF(J.GT.10)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Menu is full. Amount of available places is exeeded with '//TRIM(ITOS(J-10)),'Error')
    RETURN     
   ENDIF
   CALL WMENUITEMINSERT(ID_MANAGE_PLUGIN2,2,MENUID(J),'PL1 '//TRIM(PI1(I)%PNAME))
   PI1(I)%ID=MENUID(J)
  ELSE
   PI1(I)%ID=0
  ENDIF
 ENDDO
 
 PLUGIN_UPDATEMENU_FILL = .TRUE.
 
 END FUNCTION PLUGIN_UPDATEMENU_FILL

 !###======================================================================
 SUBROUTINE PLUGIN_EXE(IDPLUGIN)
 !###======================================================================
 !## subroutine to execute the plugin executable. The connection with menu-item is made in 'PLUGIN_UPDATEMENU_FILL'-function
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDPLUGIN
 INTEGER :: IOS,IPI,IDP
 
 CALL PLUGIN_EXE_PI_SEARCH(IPI,IDP,IDPLUGIN)    !# returns/discovers plugin number (PI1 or PI2)
 IF(IDP.EQ.0)RETURN
 
 CALL PLUGIN_EXE_READ_INI('CMD',IPI,IDP)
 
 END SUBROUTINE PLUGIN_EXE

 !###======================================================================
 SUBROUTINE PLUGIN_EXE_PI_SEARCH(IPI,IDP,IDPLUGIN)
 !###======================================================================
 !## Subroutine to discover plugin number (PI1 or PI2) for specific selected plugin executable from menu
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDPLUGIN
 INTEGER,INTENT(OUT) :: IPI,IDP
 INTEGER :: I,J

 IDP=0
  DO J=1,SIZE(PI1)
   IF(PI1(J)%ID.EQ.IDPLUGIN)THEN
    IDP=J; IPI=27; RETURN
   ENDIF
  ENDDO 
  
  DO J=1,SIZE(PI2)
   IF(PI2(J)%ID.EQ.IDPLUGIN)THEN
    IDP=J; IPI=28; RETURN
   ENDIF
  ENDDO 

 END SUBROUTINE PLUGIN_EXE_PI_SEARCH
 
 !###======================================================================
 SUBROUTINE PLUGIN_EXE_READ_INI(COMMAND,IPI,IDP)
 !###======================================================================
 !## Function to read exe-specifications from plugin ini-file variables/commands
 IMPLICIT NONE
 CHARACTER(LEN=3),INTENT(IN) :: COMMAND
 CHARACTER(LEN=1256):: DIRNAME
 INTEGER,INTENT(IN) :: IPI,IDP
 CHARACTER(LEN=52)  :: WNW
 CHARACTER(LEN=1256):: EXE,LINE,FNAME
 INTEGER :: IU,IOS,IFLAG
 LOGICAL :: LEX

 !#sets directory+filename
 IF(IPI.EQ.27)THEN
  FNAME=TRIM(PREFVAL(IPI))//'\'//TRIM(PI1(IDP)%PNAME)//'\PLUG-IN.INI'
  DIRNAME=TRIM(PI1(IDP)%PNAME)
 ELSE
  FNAME=TRIM(PREFVAL(IPI))//'\'//TRIM(PI2(IDP)%PNAME)//'\PLUG-IN.INI'
  DIRNAME=TRIM(PI2(IDP)%PNAME)
 ENDIF
 
 !# raise expection error if the specific file cannot be found or an none-existing directory is given in preference file.
 INQUIRE(FILE=FNAME,EXIST=LEX)
 IF(.NOT.LEX)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error, cannot find '//TRIM(FNAME),'Error')
  RETURN
 ENDIF
 
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 
 !#select executable file from ini-file and run this file based on the command wait/nowait
 IF(TRIM(COMMAND).EQ.'CMD')THEN
  IF(.NOT.UTL_READINITFILE(COMMAND,LINE,IU,0))RETURN
  READ(LINE,*) EXE,WNW                                      !#read exe-name and wait/nowait command in 2 different variables
  EXE=TRIM(PREFVAL(IPI))//'\'//TRIM(DIRNAME)//'\'//TRIM(EXE)
  
  IF(TRIM(WNW).EQ.'WAIT')THEN
   !#Inbouwen link naar menu window
   
   IFLAG = 2
   CALL IOSCOMMAND(TRIM(EXE),IFLAG)
  ELSEIF(TRIM(WNW).EQ.'NOWAIT')THEN
   !#Inbouwen link naar menu window
   IFLAG = 1
   CALL IOSCOMMAND(TRIM(EXE),IFLAG) 
  ENDIF
 
 ENDIF
 
 CLOSE(IU)
 
 END SUBROUTINE PLUGIN_EXE_READ_INI
 
END MODULE MOD_PLUGIN

