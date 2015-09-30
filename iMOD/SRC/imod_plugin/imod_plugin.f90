!!  Copyright (C) Stichting Deltares, 2005-2015.
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
MODULE MOD_PLUGIN

USE WINTERACTER
USE RESOURCE
USE MOD_PLUGIN_PAR
USE MOD_PREF_PAR, ONLY : PREFVAL        
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_READINITFILE,UTL_DIRINFO_POINTER,UTL_CAP,ITOS,UTL_LISTOFFILES
USE MOD_IDF, ONLY : IDFDEALLOCATE,IDFNULLIFY
USE MOD_OSD, ONLY : OSD_GETARG,OSD_OPEN,OSD_GETENV
USE IMODVAR, ONLY : IDIAGERROR
USE MODPLOT
USE IMOD

CONTAINS

!###======================================================================
 SUBROUTINE PLUGIN_MAIN(IPLUGIN)
!###======================================================================
 !# subroutine with main-program; includes call to all plugin-subroutines
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPLUGIN
 INTEGER :: ITYPE,IROW,ICOL,IPI,N
 TYPE(WIN_MESSAGE) :: MESSAGE
 
 CALL WDIALOGLOAD(ID_DMANAGE_PLUGIN,ID_DMANAGE_PLUGIN)
 
 SELECT CASE (IPLUGIN)
  CASE(ID_MANAGE_PLUGIN1)
   CALL WDIALOGTITLE('PlugIn Manager 1'); IPI=27
   CASE(ID_MANAGE_PLUGIN2)
   CALL WDIALOGTITLE('Plugin Manager 2'); IPI=28
 END SELECT
 
 CALL PLUGIN_READ(IPI)
 CALL WGRIDSETCELL(IDF_GRID1,1,1)
 CALL PLUGIN_FIELDCHANGE(IPI,1,1)
 CALL WDIALOGSHOW(-1,-1,0,2)
  
 DO
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
      CALL WGRIDPOS(MESSAGE%Y,ICOL,IROW) 
      CALL PLUGIN_FIELDCHANGE(IPI,IROW,ICOL)
    END SELECT

  END SELECT

 END DO

 CALL WDIALOGUNLOAD()
 
 END SUBROUTINE PLUGIN_MAIN

 !###======================================================================
 SUBROUTINE PLUGIN_SAVEPLUGIN(PI,IPI,LINE,IU)
 !###======================================================================
 IMPLICIT NONE
 TYPE(PIOBJ),POINTER,DIMENSION(:),INTENT(INOUT) :: PI
 CHARACTER(LEN=300),INTENT(IN) :: LINE
 INTEGER, INTENT(IN) :: IPI,IU
 INTEGER :: I,IVALUE,IOS !,POS
 CHARACTER(LEN=256) :: PLUGDIR

 IF(.NOT.ASSOCIATED(PI))THEN
  READ(LINE,'(8X,I2)') IVALUE
  ALLOCATE(PI(IVALUE))
 ELSE
  READ(LINE,'(8X,I2)') IVALUE
  DEALLOCATE(PI); ALLOCATE(PI(IVALUE))
 ENDIF

 READ(IU,*,IOSTAT=IOS) PLUGDIR !# plugin-directory (not) equal to prefval test
 IF(TRIM(PLUGDIR).NE.TRIM(PREFVAL(IPI)))THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Given plugin-directory ('//TRIM(PLUGDIR)//') in .imf file is not similar to given directory in preference file ('//TRIM(PREFVAL(IPI))//')','Error')     
 ENDIF    
 DO I=1,IVALUE
  READ(IU,*,IOSTAT=IOS) PI(I)%PNAME,PI(I)%IACT
  IF(IOS.NE.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot found plugin: '//TRIM(PI(I)%PNAME)//' in plugin folder.','Error')
   DEALLOCATE(PI); EXIT
  ENDIF
 ENDDO 

 IF(PLUGIN_UPDATEMENU_FILL())THEN;ENDIF

 END SUBROUTINE PLUGIN_SAVEPLUGIN

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
 
 IF(I+J.EQ.0)THEN; CALL WMENUSETSTATE(ID_PLUGIN,1,0); RETURN; ENDIF  

 CALL WMENUSETSTATE(ID_PLUGIN,1,1)  
    
 END SUBROUTINE PLUGIN_UPDATEMENU

 !###======================================================================
 SUBROUTINE PLUGIN_FIELDCHANGE(IPI,IROW,ICOL)
 !###======================================================================
 !## subroutine to read ini-file of plugin tool and echoes this to plugin manager
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IROW,ICOL,IPI
 CHARACTER(LEN=256) :: DIRNAME,FNAME
 CHARACTER(LEN=1256):: LINE
 INTEGER :: IU
 LOGICAL :: LEX
  
 !## sets directory+filename
 CALL WGRIDGETCELLSTRING(IDF_GRID1,ICOL,IROW,DIRNAME)
 FNAME=TRIM(PREFVAL(IPI))//'\'//TRIM(DIRNAME)//'\PLUG-IN.INI'
 
 !## raise expection error if the specific file cannot be found or a none-existing directory given in preference file.
 INQUIRE(FILE=FNAME,EXIST=LEX)
 IF(.NOT.LEX)THEN
  CALL WDIALOGPUTSTRING(IDF_LABEL1,'Error, cannot find '//TRIM(FNAME))
  CALL WDIALOGFIELDSTATE(IDF_LABEL1,0)
  RETURN
 ENDIF
 
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ')
 IF(IU.EQ.0)RETURN
 
 !## read variables from plugin ini-file
 IF(.NOT.UTL_READINITFILE('TXT',LINE,IU,0))THEN; CLOSE(IU); RETURN; ENDIF
 READ(LINE,'(A256)') FNAME
 CLOSE(IU)
 
 !## select TXT-file from ini-file and read this file, echo to description part of plugin-manager
 FNAME=TRIM(PREFVAL(IPI))//'\'//TRIM(DIRNAME)//'\'//TRIM(FNAME)
  
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ')
 IF(IU.EQ.0)RETURN
 
 READ(IU,'(A1256)') LINE
 CLOSE(IU)

 CALL WDIALOGPUTSTRING(IDF_LABEL1,TRIM(LINE))
 CALL WDIALOGFIELDSTATE(IDF_LABEL1,2)
  
 END SUBROUTINE PLUGIN_FIELDCHANGE
 
 !###======================================================================
 SUBROUTINE PLUGIN_READ(IPI)
 !###======================================================================
 !## subroutine to read folderpath(s) and name(s) of plugin-ini files
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPI
 CHARACTER(LEN=256),DIMENSION(:),POINTER :: LISTNAME
 INTEGER :: IROW
 
 !## lists all file names in specific plugin-folder
 IF(.NOT.UTL_DIRINFO_POINTER(PREFVAL(IPI),'*',LISTNAME,'D'))THEN; ENDIF

 !## check the amount of plugins in folder, if none returns "No plugin found" in window.
 IF(ASSOCIATED(LISTNAME))THEN
  IF(SIZE(LISTNAME).EQ.0)THEN
   CALL WGRIDROWS(IDF_GRID1,1)
   CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,1,'No plugin found')
   CALL WDIALOGFIELDSTATE(IDF_GRID1,0)
  ELSE
   CALL WGRIDROWS(IDF_GRID1,SIZE(LISTNAME))
   CALL WGRIDPUTSTRING(IDF_GRID1,1,LISTNAME,SIZE(LISTNAME))
  ENDIF
  !## outgraying of all the rows in the first column of the plugin manager
  !## in order to prevent the user from modifying the text in the dialog
  DO IROW=1,SIZE(LISTNAME)
   CALL WGRIDSTATECELL(IDF_GRID1,1,IROW,2)
  ENDDO  
 ENDIF
 
 !## check flag iact on occurance plugin in imf-file and save in imf-file after plugin-session 
 !## (happens in imod_main.f90 -> imodsaveimf/imodloadimf).
 IF(IPI.EQ.27)THEN  
  CALL PLUGIN_CHECK(PI1,LISTNAME,SIZE(LISTNAME))
 ELSEIF(IPI.EQ.28)THEN
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
  PI%ID=0
  PI%IFLAG=0
  PI%BACK=''
 ELSE
  !# check content of saved PI-list with avaiable plug-in files in specific folder
  !# In case size(PI) isn't equal to size(listname) and if both lists are equal in size
  !# this routine will be executed. Size(PI) needs to be equal to size(Listname)!!
  !# If size(pi) is not equal to size(listname) iact=0
  ALLOCATE(PI3(SIZE(PI))); PI3=PI
  DEALLOCATE(PI); ALLOCATE(PI(NLN)); PI%PNAME=LN; PI%IACT=0; PI%ID=0
  DO K=1,NLN
   DO J=1,SIZE(PI)
    IF(TRIM(UTL_CAP(LN(K),'U')).EQ.TRIM(UTL_CAP(PI(J)%PNAME,'U')))THEN
     IF(K.LE.SIZE(PI3))THEN
      PI(K)=PI3(J)
      EXIT
     ENDIF
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
 INTEGER :: I,J
  
 PLUGIN_UPDATEMENU_FILL = .FALSE.
 
 DO I=1,10; CALL WMENUITEMDELETE(MENUID(I)); ENDDO
 
 J=0
 
 DO I=SIZE(PI2),1,-1
  IF(PI2(I)%IACT.EQ.1)THEN
   J=J+1
   IF(J.GT.10)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Plugin Menu is full. Amount of available places exeeds by '//TRIM(ITOS(J-10)),'Error')
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
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Plugin Menu is full. Amount of available places exeeds by '//TRIM(ITOS(J-10)),'Error')
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
 INTEGER :: IPI,IDP,IFLAG,IU,I,BACTION
 CHARACTER(LEN=52) :: WNW 
 CHARACTER(LEN=256) :: EXE,DIRNAME,DIRNAME_PL
 LOGICAL :: LEX

 CALL PLUGIN_EXE_PI_SEARCH(IPI,IDP,IDPLUGIN)    !# returns/discovers plugin number (PI1 or PI2)
 IF(IDP.EQ.0)RETURN
 
 !## read in plug-in.ini, return/stop by error
 IF(.NOT.PLUGIN_EXE_READ_INI(IPI,IDP,WNW,EXE,BACTION))RETURN
   
 !# executes with a status based upon the 'wait' or 'nowait' command given in the ini-file
 IF(IPI.EQ.27)THEN
  PI1%IFLAG=1; IF(TRIM(UTL_CAP(WNW,'U')).EQ.'WAIT')PI1%IFLAG=2
 ELSEIF(IPI.EQ.28)THEN
  PI2%IFLAG=1; IF(TRIM(UTL_CAP(WNW,'U')).EQ.'WAIT')PI2%IFLAG=2
 ENDIF

 !## move fysically to the plugin folder
 CALL IOSDIRNAME(DIRNAME)
 !## clear available errors first
 I=WINFOERROR(LASTERROR)
 CALL IOSDIRCHANGE(EXE(:INDEX(EXE,'\',.TRUE.)-1))
 !## check whether dirchange was possible - if not, return
 IF(WINFOERROR(LASTERROR).NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot move to plugin directory'//CHAR(13)// &
     EXE(:INDEX(EXE,'\',.TRUE.)-1),'Error')
  RETURN
 ENDIF
 
 !# executable runs only if "apply"-button is called
 IF(BACTION.EQ.3)THEN; CALL IOSCOMMAND(TRIM(EXE),IFLAG); ENDIF
  
 !## plugin finished, nothing to do furthermore
 !## If Back-file available read at fixed moments during executing time, based upon the PLUGIN.IN file
! IF(TRIM(BACK).NE.'')
 IF(BACTION.EQ.3)THEN; CALL PLUGIN_EXE_READ_BACK(IPI,IDP); ENDIF
  
 !## move back to the iMOD folder
 CALL IOSDIRCHANGE(DIRNAME)

 END SUBROUTINE PLUGIN_EXE

 !###======================================================================
 SUBROUTINE PLUGIN_EXE_PI_SEARCH(IPI,IDP,IDPLUGIN)
 !###======================================================================
 !## Subroutine to discover plugin number (PI1 or PI2) for specific selected plugin executable from menu
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDPLUGIN
 INTEGER,INTENT(OUT) :: IPI,IDP
 INTEGER :: J
 
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
 LOGICAL FUNCTION PLUGIN_EXE_READ_INI(IPI,IDP,WNW,EXE,BACTION)
 !###======================================================================
 !## Function to read exe-specifications from plugin ini-file variables/commands
 IMPLICIT NONE
 INTEGER,PARAMETER :: STRLEN=256 
 CHARACTER(LEN=*),INTENT(OUT) :: WNW,EXE
 INTEGER,INTENT(IN) :: IPI,IDP
 INTEGER,INTENT(OUT) :: BACTION
 CHARACTER(LEN=STRLEN),POINTER,DIMENSION(:) :: FLIST
 CHARACTER(LEN=256) :: DIRNAME,LINE,MENU,BACK
 CHARACTER(LEN=256),DIMENSION(6) :: STRING
 INTEGER :: IU,I,J
 LOGICAL :: LEX
 
 PLUGIN_EXE_READ_INI=.FALSE.
 
 !## sets directory+filename
 IF(IPI.EQ.27)THEN
  DIRNAME=TRIM(PREFVAL(IPI))//'\'//TRIM(PI1(IDP)%PNAME)
 ELSEIF(IPI.EQ.28)THEN
  DIRNAME=TRIM(PREFVAL(IPI))//'\'//TRIM(PI2(IDP)%PNAME)
 ENDIF
 
 !## raise expection error if the specific file cannot be found or an none-existing directory is given in preference file.
 INQUIRE(FILE=TRIM(DIRNAME)//'\PLUG-IN.INI',EXIST=LEX)
 IF(.NOT.LEX)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error, cannot find '//TRIM(DIRNAME)//'\PLUG-IN.INI','Error')
  RETURN
 ENDIF
 
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIRNAME)//'\PLUG-IN.INI',STATUS='OLD',ACTION='READ')
 IF(IU.EQ.0)RETURN
 
 !## select executable file from ini-file and run this file based on the command wait/nowait
 IF(.NOT.UTL_READINITFILE('CMD',LINE,IU,0))THEN; CLOSE(IU); RETURN; ENDIF
 !## read exe-name and wait/nowait command in 2 different variables
 READ(LINE,*) EXE,WNW                  
 EXE=TRIM(DIRNAME)//'\'//TRIM(EXE)
 
 !## Read menu-file and link to menu window
 MENU=''; IF(UTL_READINITFILE('MENU',LINE,IU,1))READ(LINE,*) MENU
 
 !## call back-file if available
 BACK=''; IF(UTL_READINITFILE('BACK',LINE,IU,1))READ(LINE,*) BACK
 IF(IPI.EQ.27)THEN
  PI1(IDP)%BACK=BACK
 ELSE
  PI2(IDP)%BACK=BACK
 ENDIF
 
 CLOSE(IU)
 
 !## read variables from menu-file and read file-list into UTL_LISTOFFILES()
 IF(MENU.NE.'')THEN
  IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIRNAME)//'\'//TRIM(MENU),STATUS='OLD',ACTION='READ')
  IF(IU.EQ.0)RETURN
  
  STRING=''
  IF(UTL_READINITFILE('WINDOW',LINE,IU,1)) READ(LINE,'(A)') STRING(1)
  IF(UTL_READINITFILE('BUTTON1',LINE,IU,1))READ(LINE,*) STRING(2)
  IF(UTL_READINITFILE('BUTTON2',LINE,IU,1))READ(LINE,*) STRING(3)
  IF(UTL_READINITFILE('BUTTON3',LINE,IU,1))READ(LINE,*) STRING(4)
  IF(UTL_READINITFILE('LIST',LINE,IU,1))   READ(LINE,*) STRING(5)
  IF(UTL_READINITFILE('TEXT',LINE,IU,1))   READ(LINE,*) STRING(6)
  CLOSE(IU)
 
  !## include files in exe-window from selected place based upon predefined List-name
  IF(TRIM(UTL_CAP(STRING(5),'U')).EQ.'IMODMANAGER')THEN  !#feeds from iMOD Manager
   J=0; DO I=1,SIZE(MP); IF(MP(I)%IACT)J=J+1; ENDDO
   !## no files found in the imod manager
   IF(J.EQ.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Menu window cannot be started: there are no files available in the iMOD Manager. '// &
      'Inspect your plugin settings.','Warning');RETURN
   ENDIF
   ALLOCATE(FLIST(J))
   DO I=1,J; FLIST(I)=MP(I)%IDFNAME; ENDDO
  ENDIF

  !## handling of files
  CALL UTL_LISTOFFILES(FLIST,STRING,BACTION)
   
  IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIRNAME)//'\PLUG-IN.IN',STATUS='UNKNOWN',ACTION='WRITE')
  IF(IU.EQ.0)RETURN
  DO I=1,SIZE(FLIST); WRITE(IU,'(A)') TRIM(FLIST(I)); ENDDO
  CLOSE(IU)
  
 ENDIF
  
 PLUGIN_EXE_READ_INI=.TRUE.
  
 END FUNCTION PLUGIN_EXE_READ_INI

 !###======================================================================
 SUBROUTINE PLUGIN_EXE_READ_BACK(IPI,IDP)
 !###======================================================================
 !## Subroutine to read variables from PLUG-IN.out
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPI, IDP
 INTEGER :: I,J,IU,LEX,IOS,NFILE,IFLAG
 CHARACTER(LEN=256) :: DIRNAME,BACK,LINE,STOPCOM,MESSINFO,MESSERR,MESSPROG
 CHARACTER(LEN=52) :: RESULTFILE
 REAL,DIMENSION(4) :: WINDOW
 
 IF(IPI.EQ.27)THEN
  BACK=PI1(IDP)%BACK
  IFLAG=PI1(IDP)%IFLAG
 ELSE
  BACK=PI2(IDP)%BACK
  IFLAG=PI2(IDP)%IFLAG
 ENDIF
 
 INQUIRE(FILE=BACK,EXIST=LEX)
 IF(.NOT.LEX)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error, cannot find '//TRIM(BACK)//'.'//CHAR(13)// &
  'Inspect your *.INI file in plugin folder.','Error')
  RETURN
 ENDIF
 
 IU=UTL_GETUNIT()
 
 IF(IFLAG.EQ.2)THEN !# if "wait"-command given then Back-file will be read
  CALL OSD_OPEN(IU,FILE=BACK,STATUS='OLD',ACTION='READ')
  IF(IU.EQ.0)RETURN
 ENDIF 
 
 !#Handling different types of optional Plugin-messages
 MESSINFO=''; IF(UTL_READINITFILE('MESSAGE_INFO',LINE,IU,1))READ(LINE,'(A)',IOSTAT=IOS) MESSINFO
 IF(MESSINFO.NE.'')THEN
  IF(IOS.EQ.0)THEN
   CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,TRIM(MESSINFO),'Plugin information') 
  ELSE
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error, reading message.','Error')  
  ENDIF
 ENDIF
 
 MESSERR=''; IF(UTL_READINITFILE('MESSAGE_ERROR',LINE,IU,1))READ(LINE,*) MESSERR
 IF(MESSERR.NE.'')THEN
  IF(IOS.EQ.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,TRIM(MESSERR),'Plugin error') 
  ELSE
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error, reading message.','Error')  
  ENDIF 
 ENDIF
! MESSPROG=''; IF(UTL_READINITFILE('MESSAGE_PROGRESS',LINE,IU,1))READ(LINE,*) MESSPROG
  
 IF(IPI.EQ.27)THEN
  DIRNAME=TRIM(PREFVAL(IPI))//'\'//TRIM(PI1(IDP)%PNAME)//'\'
 ELSEIF(IPI.EQ.28)THEN
  DIRNAME=TRIM(PREFVAL(IPI))//'\'//TRIM(PI2(IDP)%PNAME)//'\'
 ENDIF

 !#Reads list of files from line and returns this list to iMOD manager
 !#plot update at the end of reading all files
 !#select all new files in iMOD manager after executable is finsihed 
 NFILE=0; IF(UTL_READINITFILE('NFILE',LINE,IU,1))READ(LINE,*,IOSTAT=IOS) NFILE
 IF(NFILE.GT.0)THEN
  DO I=1,NFILE
   IF(.NOT.UTL_READINITFILE('FILE'//ITOS(I),LINE,IU,0))THEN
    CLOSE(IU); RETURN
   ENDIF
   READ(LINE,*,IOSTAT=IOS) RESULTFILE
   IF(IOS.EQ.0)THEN
    INQUIRE(FILE=RESULTFILE,EXIST=LEX)
    IF(.NOT.LEX)THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error, cannot find '//TRIM(RESULTFILE)//'.'//CHAR(13)// &
     'Inspect your plugin settings.','Error')
     RETURN
    ENDIF
    IF(I.EQ.1)THEN
     CALL IDFINIT(IDFNAMEGIVEN=TRIM(DIRNAME)//TRIM(RESULTFILE),LPLOT=.FALSE.,LDEACTIVATE=.TRUE.)
    ELSE
     CALL IDFINIT(IDFNAMEGIVEN=TRIM(DIRNAME)//TRIM(RESULTFILE),LPLOT=.FALSE.,LDEACTIVATE=.FALSE.)
    ENDIF
   ELSE
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error, reading files.','Error')    
   ENDIF
  ENDDO
 ENDIF
 
 !#Reset extend of plotting window based upon given coordinates by plugin executable if "window" command is available
 IF(UTL_READINITFILE('WINDOW',LINE,IU,1))THEN
  READ(LINE,*,IOSTAT=IOS) WINDOW(1),WINDOW(2),WINDOW(3),WINDOW(4)
  IF(IOS.EQ.0)THEN
   MPW%XMIN=WINDOW(1)
   MPW%XMAX=WINDOW(2)
   MPW%YMIN=WINDOW(3)
   MPW%YMAX=WINDOW(4) 
  ELSE
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Window-option is empty/not defined.'//CHAR(13)// &
   'Results are plotted with default window extend.','Warning')    
  ENDIF
 ENDIF
 
 !#If Stop is read in file then iMOD will check whether or not the executable is still running. 
 !#Only if exe runs in "nowait"-modus. If it is stopped running Plug-in.out won't be checked anymore
 STOPCOM=''; IF(UTL_READINITFILE('STOP',LINE,IU,1))READ(LINE,*) STOPCOM

 CLOSE(IU)

 CALL IDFPLOTFAST(0) !#plot last loaded file in manager to window

 END SUBROUTINE PLUGIN_EXE_READ_BACK
 
END MODULE MOD_PLUGIN

