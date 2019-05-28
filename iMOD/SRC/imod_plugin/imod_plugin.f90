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
MODULE MOD_PLUGIN

USE WINTERACTER
USE RESOURCE
USE MOD_PLUGIN_PAR
USE MOD_PREF_PAR
USE MOD_UTL
USE MOD_IDF
USE MOD_OSD
USE IMODVAR
USE MOD_MANAGER_UTL
USE MODPLOT
USE MOD_UTL

CONTAINS

 !###======================================================================
 SUBROUTINE PLUGIN_MAIN(IPLUGIN)
 !###======================================================================
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
 CALL PLUGIN_FIELDCHANGE(IPI,1)
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
      IF(PLUGIN_INITMENU_FILL())EXIT
     CASE(IDHELP)
    END SELECT
   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_GRID1)
      CALL WGRIDPOS(MESSAGE%Y,ICOL,IROW) 
      CALL PLUGIN_FIELDCHANGE(IPI,IROW)
    END SELECT

  END SELECT

 END DO

 CALL WDIALOGUNLOAD()
 CALL PLUGIN_SETTIMER()
 
 END SUBROUTINE PLUGIN_MAIN

 !###======================================================================
 SUBROUTINE PLUGIN_LOAD(PI,IPI,LINE,IU)
 !###======================================================================
 IMPLICIT NONE
 TYPE(PIOBJ),POINTER,DIMENSION(:),INTENT(INOUT) :: PI
 CHARACTER(LEN=300),INTENT(IN) :: LINE
 INTEGER, INTENT(IN) :: IPI,IU
 INTEGER :: I,IVALUE,IOS
 CHARACTER(LEN=256) :: PLUGDIR

 !## nothing to do, probably imf with plugin using in a configuration without plugins active
 IF(TRIM(PREFVAL(IPI)).EQ.'')RETURN
 
 IF(.NOT.ASSOCIATED(PI))THEN
  READ(LINE,'(8X,I2)') IVALUE
  ALLOCATE(PI(IVALUE))
 ELSE
  READ(LINE,'(8X,I2)') IVALUE
  DEALLOCATE(PI); ALLOCATE(PI(IVALUE))
 ENDIF

 READ(IU,*,IOSTAT=IOS) PLUGDIR !## plugin-directory (not) equal to prefval test
 IF(TRIM(PLUGDIR).NE.TRIM(PREFVAL(IPI)))THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Given plugin-directory ('//TRIM(PLUGDIR)//') in .imf file is not similar to given directory in preference file ('//TRIM(PREFVAL(IPI))//')','Error')
 ENDIF    
 DO I=1,IVALUE
  READ(IU,*,IOSTAT=IOS) PI(I)%PNAME,PI(I)%IACT
  IF(IOS.NE.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot find plugin: '//TRIM(PI(I)%PNAME)//' in plugin folder.'//CHAR(13)// &
       'Reinitialize plugins in the PlugIn Manager.','Error')
   DEALLOCATE(PI); EXIT
  ENDIF
 ENDDO 

 END SUBROUTINE PLUGIN_LOAD

 !###======================================================================
 SUBROUTINE PLUGIN_INITMENU()
 !###======================================================================
 !### updates plugin-menu with ungraying the manager chosen by the user
 IMPLICIT NONE
 INTEGER :: I,J
  
!## plugin not available for winteracter 8
 I=1; IF(LEN_TRIM(PREFVAL(27)).EQ.0)I=0
 CALL WMENUSETSTATE(ID_MANAGE_PLUGIN1,1,I)
 J=1; IF(LEN_TRIM(PREFVAL(28)).EQ.0)J=0
 CALL WMENUSETSTATE(ID_MANAGE_PLUGIN2,1,J)
 
 IF(I+J.EQ.0)THEN
  CALL WMENUSETSTATE(ID_PLUGIN,1,0)
 ELSE 
  CALL WMENUSETSTATE(ID_PLUGIN,1,1)  
 ENDIF
 
 END SUBROUTINE PLUGIN_INITMENU
 
 !###======================================================================
 SUBROUTINE PLUGIN_SETTIMER()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: TI,I
 
 !## no plugin available/active, do not set the timer for plugins (ID=2)
 IF(.NOT.ASSOCIATED(PI1).AND..NOT.ASSOCIATED(PI2))THEN
  CALL WMENUSETSTATE(ID_PLUGIN_TIMECHECK,1,0); RETURN
 ELSE
  CALL WMENUSETSTATE(ID_PLUGIN_TIMECHECK,1,0)
  !## check whether plugin are active
  IF(ASSOCIATED(PI1))THEN
   DO I=1,SIZE(PI1); IF(PI1(I)%IACT.EQ.1)EXIT; ENDDO
   IF(I.LE.SIZE(PI1))CALL WMENUSETSTATE(ID_PLUGIN_TIMECHECK,1,1)
  ENDIF
  IF(ASSOCIATED(PI2))THEN
   DO I=1,SIZE(PI2); IF(PI2(I)%IACT.EQ.1)EXIT; ENDDO
   IF(I.LE.SIZE(PI2))CALL WMENUSETSTATE(ID_PLUGIN_TIMECHECK,1,1)
  ENDIF
  IF(WMENUGETSTATE(ID_PLUGIN_TIMECHECK,1).EQ.0)RETURN
 ENDIF

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_TI1,2).EQ.1)TI=60
 IF(WMENUGETSTATE(ID_TI2,2).EQ.1)TI=30
 IF(WMENUGETSTATE(ID_TI3,2).EQ.1)TI=15
 IF(WMENUGETSTATE(ID_TI4,2).EQ.1)TI=1
 
 CALL WMESSAGETIMER(TI*1000,ID=2,IREPEAT=1)  !## minutes
    
 END SUBROUTINE PLUGIN_SETTIMER
 
 !###======================================================================
 LOGICAL FUNCTION PLUGIN_INITMENU_FILL()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,MXPLUGIN,ID
 
 PLUGIN_INITMENU_FILL=.FALSE.

 MXPLUGIN=SIZE(MENUID)
 
 CALL UTL_DEBUGLEVEL(0)
 DO I=1,MXPLUGIN; CALL WMENUITEMDELETE(MENUID(I)); ENDDO
 CALL UTL_DEBUGLEVEL(1)

 J=0
 
 IF(ASSOCIATED(PI1))THEN
  
  !## initiate all
  PI1%ID=0
  PI1%IFLAG=0
  PI1%BACK=''
  PI1%IDPROC(1)=0; PI1%IDPROC(2)=0 

  DO I=1,SIZE(PI1)
   IF(PI1(I)%IACT.EQ.1)THEN
    J=J+1
    IF(J.GT.MXPLUGIN)THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Plugin Menu is full. Amount of available places exeeds by '//TRIM(ITOS(J-MXPLUGIN))//'.','Warning')
     EXIT     
    ENDIF
    ID=ID_MANAGE_PLUGIN2; IF(J.GT.1)ID=MENUID(J-1)
    CALL WMENUITEMINSERT(ID,2,MENUID(J),'PL1: '//TRIM(PI1(I)%PNAME))
    PI1(I)%ID=MENUID(J)
   ENDIF
  ENDDO

 ENDIF
 
 IF(ASSOCIATED(PI2))THEN

  !## initiate all
  PI2%ID=0
  PI2%IFLAG=0
  PI2%BACK=''
  PI2%IDPROC(1)=0; PI2%IDPROC(2)=0 

  DO I=1,SIZE(PI2)
   IF(PI2(I)%IACT.EQ.1)THEN
    J=J+1
    IF(J.GT.MXPLUGIN)THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Plugin Menu is full. Amount of available places exeeds by '//TRIM(ITOS(J-MXPLUGIN))//'.','Warning')
     EXIT  
    ENDIF
    ID=ID_MANAGE_PLUGIN2; IF(J.GT.1)ID=MENUID(J-1)
    CALL WMENUITEMINSERT(ID,2,MENUID(J),'PL2: '//TRIM(PI2(I)%PNAME))
    PI2(I)%ID=MENUID(J)
   ENDIF
  ENDDO
 
 ENDIF
 
 PLUGIN_INITMENU_FILL=.TRUE.
  
 END FUNCTION PLUGIN_INITMENU_FILL
 
 !###======================================================================
 SUBROUTINE PLUGIN_INITMENU_RUN()
 !###======================================================================
 !## Subroutine to check whether plugin executable is still running or not 
 !## control outgraying plugin menu for plugin manager
 IMPLICIT NONE
 INTEGER :: NPROC
 INTEGER :: I,J
 
 CALL WINDOWSELECT(0)
 
 !## number of running processes 
 NPROC=0
 IF(ASSOCIATED(PI1))THEN
  DO I=1,SIZE(PI1); IF(PI1(I)%IDPROC(1).NE.0)NPROC=NPROC+1; ENDDO
 ENDIF
 IF(ASSOCIATED(PI2))THEN
  DO I=1,SIZE(PI2); IF(PI2(I)%IDPROC(1).NE.0)NPROC=NPROC+1; ENDDO
 ENDIF
 
 !## not allowed to enter the plugin managers
 IF(NPROC.GT.0)THEN 
  CALL WMENUSETSTATE(ID_MANAGE_PLUGIN1,1,0)
  CALL WMENUSETSTATE(ID_MANAGE_PLUGIN2,1,0)
 ELSE
  I=1; IF(LEN_TRIM(PREFVAL(27)).EQ.0)I=0
  CALL WMENUSETSTATE(ID_MANAGE_PLUGIN1,1,I)
  J=1; IF(LEN_TRIM(PREFVAL(28)).EQ.0)J=0
  CALL WMENUSETSTATE(ID_MANAGE_PLUGIN2,1,J)
 ENDIF 
 
 !## plugin1
 IF(ASSOCIATED(PI1))THEN

  J=0
  DO I=1,SIZE(PI1)
   IF(PI1(I)%IACT.EQ.0)CYCLE
   J=J+1
   IF(PI1(I)%IDPROC(1).NE.0)THEN
    CALL WMENUSETSTRING(MENUID(J),'PL1: '//TRIM(PI1(I)%PNAME)//' is running ...')
    CALL WMENUSETSTATE(MENUID(J),1,0)
    CALL WMENUSETSTATE(MENUID(J),2,1)
   ELSE
    CALL WMENUSETSTRING(MENUID(J),'PL1: '//TRIM(PI1(I)%PNAME))
    CALL WMENUSETSTATE(MENUID(J),1,1)
    CALL WMENUSETSTATE(MENUID(J),2,0)
   ENDIF
  ENDDO
 
 ENDIF
 
 !## plugin2
 IF(ASSOCIATED(PI2))THEN

  DO I=1,SIZE(PI2)
   IF(PI2(I)%IACT.EQ.0)CYCLE
   J=J+1
   IF(PI2(I)%IDPROC(1).NE.0)THEN
    CALL WMENUSETSTRING(MENUID(J),'PL2: '//TRIM(PI2(I)%PNAME)//' is running ...')
    CALL WMENUSETSTATE(MENUID(J),1,0)
    CALL WMENUSETSTATE(MENUID(J),2,1)
   ELSE
    CALL WMENUSETSTRING(MENUID(J),'PL2: '//TRIM(PI2(I)%PNAME))
    CALL WMENUSETSTATE(MENUID(J),1,1)
    CALL WMENUSETSTATE(MENUID(J),2,0)
   ENDIF
  ENDDO
 
 ENDIF
 
 END SUBROUTINE PLUGIN_INITMENU_RUN
 
 !###======================================================================
 SUBROUTINE PLUGIN_FIELDCHANGE(IPI,IROW)
 !###======================================================================
 !## subroutine to read ini-file of plugin tool and echoes this to plugin manager
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IROW,IPI
 CHARACTER(LEN=256) :: DIRNAME,FNAME
 CHARACTER(LEN=1256):: LINE
 INTEGER :: IU
 LOGICAL :: LEX
  
 !## sets directory+filename
 CALL WGRIDGETCELLSTRING(IDF_GRID1,1,IROW,DIRNAME)
 FNAME=TRIM(PREFVAL(IPI))//'\'//TRIM(DIRNAME)//'\PLUG-IN.INI'
 
 !## raise expection error if the specific file cannot be found or a none-existing directory given in preference file.
 INQUIRE(FILE=FNAME,EXIST=LEX)
 IF(.NOT.LEX)THEN
  CALL WDIALOGPUTSTRING(IDF_LABEL1,'Error, cannot find '//TRIM(FNAME))
  CALL WDIALOGFIELDSTATE(IDF_LABEL1,0)
  RETURN
 ENDIF
 
 CALL WDIALOGCLEARFIELD(IDF_LABEL1)
 
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
 !## (happens in imod_main.f90 -> MAIN_UTL_SAVE_IMF/MAIN_UTL_LOAD_IMF).
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
 !## Subroutine to check if PI# contains the same filenames as Listname (folder)
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
  PI%IDPROC(1)=0
  PI%IDPROC(2)=0
 ELSE
  !## check content of saved PI-list with avaiable plug-in files in specific folder
  !## In case size(PI) isn't equal to size(listname) and if both lists are equal in size
  !## this routine will be executed. Size(PI) needs to be equal to size(Listname)!!
  !## If size(pi) is not equal to size(listname) iact=0
  ALLOCATE(PI3(SIZE(PI))); PI3=PI
  DEALLOCATE(PI); ALLOCATE(PI(NLN)); PI%PNAME=LN; PI%IACT=0; PI%ID=0
  DO K=1,NLN
   DO J=1,SIZE(PI3)
    IF(TRIM(UTL_CAP(LN(K),'U')).EQ.TRIM(UTL_CAP(PI3(J)%PNAME,'U')))THEN    
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
 SUBROUTINE PLUGIN_EXE(IDPLUGIN)
 !###======================================================================
 !## subroutine to execute the plugin executable. The connection with menu-item is made in 'PLUGIN_INITMENU_FILL'-function
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDPLUGIN
 INTEGER :: IPI,IDP,I
 CHARACTER(LEN=52) :: WNW 
 CHARACTER(LEN=256) :: EXE,DIRNAME

 !## returns/discovers plugin number (PI1 or PI2)
 CALL PLUGIN_EXE_PI_SEARCH(IPI,IDP,IDPLUGIN)    
 IF(IDP.EQ.0)RETURN
 
 !## read in plug-in.ini, return/stop by error/cancelation
 IF(.NOT.PLUGIN_EXE_READ_INI(IPI,IDP,WNW,EXE))RETURN
   
 !## executes with a status based upon the 'wait' or 'nowait' command given in the ini-file
 IF(IPI.EQ.27)THEN
  PI1%IFLAG=0; IF(TRIM(UTL_CAP(WNW,'U')).EQ.'WAIT')PI1%IFLAG=2
 ELSEIF(IPI.EQ.28)THEN
  PI2%IFLAG=0; IF(TRIM(UTL_CAP(WNW,'U')).EQ.'WAIT')PI2%IFLAG=2
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
 
 !## executable runs only if "apply"-button is called, process identifer (IDPROC) is saved in PI-variable
 IF(IPI.EQ.27)THEN
  PI1(IDP)%IDPROC=0
  CALL IOSCOMMAND(TRIM(EXE),IFLAGS=PI1(IDP)%IFLAG,IDPROC=PI1(IDP)%IDPROC)
  !## clear idproc in case wait status is used
  IF(PI1(IDP)%IFLAG.EQ.2)PI1(IDP)%IDPROC=0
 ELSEIF(IPI.EQ.28)THEN
  PI2(IDP)%IDPROC=0
  CALL IOSCOMMAND(TRIM(EXE),IFLAGS=PI2(IDP)%IFLAG,IDPROC=PI2(IDP)%IDPROC)
  !## clear idproc in case wait status is used
  IF(PI2(IDP)%IFLAG.EQ.2)PI2(IDP)%IDPROC=0
 ENDIF
 !## move back to the iMOD folder
 CALL IOSDIRCHANGE(DIRNAME)

 !## check outfile
 CALL PLUGIN_EXE_READ_BACK(IPI,IDP)
 
 !## update menu
 CALL PLUGIN_INITMENU_RUN()
 
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
 LOGICAL FUNCTION PLUGIN_EXE_READ_INI(IPI,IDP,WNW,EXE)
 !###======================================================================
 !## Function to read exe-specifications from plugin ini-file variables/commands
 IMPLICIT NONE
 INTEGER,PARAMETER :: STRLEN=256 
 CHARACTER(LEN=*),INTENT(OUT) :: WNW,EXE
 INTEGER,INTENT(IN) :: IPI,IDP
 CHARACTER(LEN=STRLEN),POINTER,DIMENSION(:) :: FLIST => NULL()
 CHARACTER(LEN=256) :: DIRNAME,LINE,MENU,BACK,FNAME,HELP
 CHARACTER(LEN=256),DIMENSION(6) :: STRING
 CHARACTER(LEN=1256) :: TEXT
 INTEGER :: IU,I,J,BACTION,YN
 LOGICAL :: LEX
 
 PLUGIN_EXE_READ_INI=.FALSE.
 
 YN=0
 
 !## sets directory+filename and initialize plugin-type
 IF(IPI.EQ.27)THEN
  PI1(IDP)%IFLAG=0
  PI1(IDP)%BACK=''
  PI1(IDP)%IDPROC(1)=0; PI1(IDP)%IDPROC(2)=0  
  DIRNAME=TRIM(PREFVAL(IPI))//'\'//TRIM(PI1(IDP)%PNAME)
 ELSEIF(IPI.EQ.28)THEN
  PI2(IDP)%IFLAG=0
  PI2(IDP)%BACK=''
  PI2(IDP)%IDPROC(1)=0; PI2(IDP)%IDPROC(2)=0
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
 IF(INDEX(EXE,':').EQ.0)EXE=TRIM(DIRNAME)//'\'//TRIM(EXE)
 
 !## Read menu-file and link to menu window
 MENU=''; IF(UTL_READINITFILE('MENU',LINE,IU,1))READ(LINE,'(A)') MENU
 
 !## call back-file if available AND read help file
 HELP=''; IF(UTL_READINITFILE('HELP',LINE,IU,1))READ(LINE,'(A)') HELP
 HELP=TRIM(DIRNAME)//'\'//TRIM(HELP)
 BACK=''; IF(UTL_READINITFILE('BACK',LINE,IU,1))READ(LINE,'(A)') BACK
 IF(IPI.EQ.27)THEN
  PI1(IDP)%BACK=BACK
 ELSEIF(IPI.EQ.28)THEN
  PI2(IDP)%BACK=BACK
 ENDIF
 
 CLOSE(IU)
 
 !## BACK: If output-file not available create new file, else replace by empty file
 FNAME=TRIM(DIRNAME)//'\'//TRIM(BACK); INQUIRE(FILE=FNAME,EXIST=LEX)
 IF(LEX)THEN
  I=INFOERROR(1); CALL IOSDELETEFILE(FNAME); I=INFOERROR(1)
  IF(I.NE.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'IMOD cannot remove the '//TRIM(FNAME),'Error'); RETURN
  ENDIF
 ENDIF
 
 !## MENU: no menu keyword available, return
 IF(MENU.EQ.'')THEN; PLUGIN_EXE_READ_INI=.TRUE.; RETURN; ENDIF

 !## MENU: read variables from menu-file and read file-list into utl_listoffiles()
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIRNAME)//'\'//TRIM(MENU),STATUS='OLD',ACTION='READ')
 IF(IU.EQ.0)RETURN
  
 STRING=''
 IF(UTL_READINITFILE('LIST',LINE,IU,1))   READ(LINE,*)     STRING(1)
 IF(UTL_READINITFILE('TITLE',LINE,IU,1))  READ(LINE,'(A)') STRING(2)
 IF(UTL_READINITFILE('BUTTON1',LINE,IU,1))READ(LINE,*)     STRING(3)
 IF(UTL_READINITFILE('BUTTON2',LINE,IU,1))READ(LINE,*)     STRING(4)
 IF(UTL_READINITFILE('BUTTON3',LINE,IU,1))READ(LINE,*)     STRING(5)
 IF(UTL_READINITFILE('TEXT',LINE,IU,1))   READ(LINE,*)     STRING(6)
 CLOSE(IU)
 
 !## include files in exe-window from selected place based upon predefined List-name
 IF(TRIM(UTL_CAP(STRING(1),'U')).EQ.'IMODMANAGER')THEN  !#feeds from iMOD Manager
  !## make sure it is capitals from now on - easier to evaluate in later subroutines
  STRING(1)='IMODMANAGER'
  J=0; DO I=1,SIZE(MP); IF(MP(I)%IACT)J=J+1; ENDDO
  !## no files found in the imod manager
  IF(J.EQ.0)THEN
   CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONYES,'You are about to start the plugin, but there are no files available in the iMOD Manager. '// &
     'Would you like to start the plugin anyway?','Question')
     IF(WINFODIALOG(4).NE.1)THEN
      YN=0; RETURN; ELSE; YN=1
     ENDIF
  ENDIF
  ALLOCATE(FLIST(J))
  DO I=1,J
   IF(MP(I)%ISEL)THEN
    FLIST(I)='+'//MP(I)%IDFNAME
   ELSE
    FLIST(I)='-'//MP(I)%IDFNAME
   ENDIF
  ENDDO
 ENDIF
  
 !## handling of files
 CALL UTL_READTXTFILE(TRIM(DIRNAME)//'\'//TRIM(STRING(6)),TEXT)
 !## get selected files
 IF(YN.NE.1)THEN
  CALL UTL_LISTOFFILES(FLIST,STRING,BACTION,TEXT,HELP)
 ELSE
  BACTION=1
 ENDIF
 
 !## selected the OK button
 IF(BACTION.EQ.1)THEN
    
  IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(DIRNAME)//'\PLUG-IN.IN',STATUS='UNKNOWN',ACTION='WRITE')
  IF(IU.NE.0)THEN
   DO I=1,SIZE(FLIST); WRITE(IU,'(A)') TRIM(FLIST(I)); ENDDO; CLOSE(IU)
  ENDIF
 
 ENDIF
  
 !## return function value to be true whenever files are selected
 IF(ASSOCIATED(FLIST))THEN; PLUGIN_EXE_READ_INI=.TRUE.; DEALLOCATE(FLIST); ENDIF
 
 !## process terminated by user
 IF(BACTION.EQ.0)PLUGIN_EXE_READ_INI=.FALSE.
 
 END FUNCTION PLUGIN_EXE_READ_INI

 !###======================================================================
 SUBROUTINE PLUGIN_EXE_READ_BACK(IPI,IDP)
 !###======================================================================
 !## Subroutine to read variables from PLUG-IN.out
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPI,IDP
 INTEGER :: I,IU,IOS,NFILE 
 CHARACTER(LEN=256) :: DIRNAME,BACK,FNAME,LINE,MESSINFO,MESSERR,MESSPROG,RESULTFILE
 CHARACTER(LEN=52) :: PNAME
 REAL(KIND=DP_KIND),DIMENSION(4) :: WINDOW
 LOGICAL :: LEX
 
 IF(IPI.EQ.27)THEN
  BACK=PI1(IDP)%BACK
  PNAME=PI1(IDP)%PNAME
  DIRNAME=TRIM(PREFVAL(IPI))//'\'//TRIM(PI1(IDP)%PNAME)//'\'
 ELSEIF(IPI.EQ.28)THEN
  BACK=PI2(IDP)%BACK
  PNAME=PI2(IDP)%PNAME
  DIRNAME=TRIM(PREFVAL(IPI))//'\'//TRIM(PI2(IDP)%PNAME)//'\'
 ENDIF
 
 FNAME=TRIM(DIRNAME)//TRIM(BACK); INQUIRE(FILE=FNAME,EXIST=LEX)
 !## nothing to do
 IF(.NOT.LEX)RETURN
 
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ')
 !## error in opening file
 IF(IU.EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'iMOD cannot read output file:'//CHAR(13)//TRIM(FNAME),'Warning'); RETURN
 ENDIF
  
 !## handling different types of optional Plugin-messages
 IF(UTL_READINITFILE('MESSAGE_INFO',LINE,IU,1))THEN
  READ(LINE,'(A)',IOSTAT=IOS) MESSINFO
  IF(IOS.EQ.0)THEN
   CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,TRIM(MESSINFO),'Plugin information') 
  ELSE
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error, reading message.','Error')  
  ENDIF
 ENDIF
 
 IF(UTL_READINITFILE('MESSAGE_ERROR',LINE,IU,1))THEN
  READ(LINE,'(A)',IOSTAT=IOS) MESSERR
  IF(IOS.EQ.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,TRIM(MESSERR),'Plugin error') 
  ELSE
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error, reading message.','Error')  
  ENDIF 
 ENDIF

 IF(UTL_READINITFILE('MESSAGE_PROGRESS',LINE,IU,1))THEN
  READ(LINE,'(A)',IOSTAT=IOS) MESSPROG
  IF(IOS.EQ.0)THEN; CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(4,'Plugin message - '//TRIM(PNAME)//': '//TRIM(MESSPROG)); ENDIF
 ENDIF
   
 !## reads list of files from line and returns this list to iMOD manager
 !## plot update at the end of reading all files
 !## select all new files in iMOD manager after executable is finsihed 
 NFILE=0; IF(UTL_READINITFILE('NFILE',LINE,IU,1))READ(LINE,*,IOSTAT=IOS) NFILE
 IF(NFILE.GT.0)THEN
  DO I=1,NFILE
   IF(.NOT.UTL_READINITFILE('FILE'//ITOS(I),LINE,IU,0))THEN
    CLOSE(IU,STATUS='DELETE'); RETURN
   ENDIF
   READ(LINE,*,IOSTAT=IOS) RESULTFILE
   IF(IOS.EQ.0)THEN
    INQUIRE(FILE=RESULTFILE,EXIST=LEX)
    IF(.NOT.LEX)THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error, cannot find '//TRIM(RESULTFILE)//'.'//CHAR(13)// &
     'Inspect your plugin settings.','Error')
     CLOSE(IU,STATUS='DELETE'); RETURN
    ENDIF
    IF(I.EQ.1)THEN
     CALL MANAGER_UTL_ADDFILE(IDFNAMEGIVEN=RESULTFILE,LDEACTIVATE=.TRUE.)
    ELSE
     CALL MANAGER_UTL_ADDFILE(IDFNAMEGIVEN=RESULTFILE,LDEACTIVATE=.FALSE.)
    ENDIF
   ELSE
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error, reading files.','Error')
    CLOSE(IU,STATUS='DELETE'); RETURN    
   ENDIF
  ENDDO
 ENDIF
 
 !## reset extend of plotting window based upon given coordinates by plugin executable if "window" command is available
 IF(UTL_READINITFILE('WINDOW',LINE,IU,1))THEN
  READ(LINE,*,IOSTAT=IOS) WINDOW
  IF(IOS.EQ.0)THEN
   MPW%XMIN=WINDOW(1); MPW%YMIN=WINDOW(2)
   MPW%XMAX=WINDOW(3); MPW%YMAX=WINDOW(4) 
  ELSE
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Window-option is empty/not defined.'//CHAR(13)// &
   'Results are plotted with default window extend.','Warning')    
  ENDIF
 ENDIF

 CLOSE(IU,STATUS='DELETE')
 
! !## plot last loaded file in manager to window
! CALL IDFPLOTFAST(0) 

 END SUBROUTINE PLUGIN_EXE_READ_BACK

 !###======================================================================
 SUBROUTINE PLUGIN_EXE_CHECK_RUN(IRUN,IKILL)
 !###======================================================================
 !## Subroutine to check whether plugin executable is still running or not
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IRUN
 INTEGER,INTENT(INOUT) :: IKILL
 INTEGER :: IPI,IDP,ISTATUS,IEXCOD
 
 IKILL=0
 
 !## plugin1
 IF(ASSOCIATED(PI1))THEN

  IPI=27 
  DO IDP=1,SIZE(PI1)
   IF(PI1(IDP)%IDPROC(1).NE.0)THEN 
    CALL IOSCOMMANDCHECK(PI1(IDP)%IDPROC,ISTATUS,IEXCOD=IEXCOD)
    IF(IRUN.EQ.1)THEN
     CALL PLUGIN_EXE_READ_BACK(IPI,IDP)
    ELSEIF(IRUN.EQ.0)THEN
     CALL PLUGIN_EXE_CHECK_STOP(PI1(IDP)%IDPROC,PI1(IDP)%PNAME,IKILL)
    ENDIF
    IF(ISTATUS.EQ.0)PI1(IDP)%IDPROC=0
   ENDIF
  ENDDO
 
 ENDIF
 
 !## plugin2
 IF(ASSOCIATED(PI2))THEN

  IPI=28 
  DO IDP=1,SIZE(PI2)
   IF(PI2(IDP)%IDPROC(1).NE.0)THEN 
    CALL IOSCOMMANDCHECK(PI2(IDP)%IDPROC,ISTATUS,IEXCOD=IEXCOD)
    IF(IRUN.EQ.1)THEN
     CALL PLUGIN_EXE_READ_BACK(IPI,IDP)
    ELSEIF(IRUN.EQ.0)THEN
     CALL PLUGIN_EXE_CHECK_STOP(PI2(IDP)%IDPROC,PI2(IDP)%PNAME,IKILL)
    ENDIF 
    IF(ISTATUS.EQ.0)PI2(IDP)%IDPROC=0
   ENDIF
  ENDDO
 
 ENDIF
  
 CALL PLUGIN_INITMENU_RUN()
 
 END SUBROUTINE PLUGIN_EXE_CHECK_RUN
 
 !###======================================================================
 SUBROUTINE PLUGIN_EXE_CHECK_STOP(IDPROC,PNAME,IKILL)
 !###======================================================================
 !## Subroutine to check whether plugin executable is still running or not
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: PNAME
 INTEGER,DIMENSION(2),INTENT(INOUT) :: IDPROC
 INTEGER,INTENT(OUT) :: IKILL
 INTEGER :: I
 
 IKILL=1
 
 !## If stop command is available in *.OUT file
 CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONYES,'The '//TRIM(PNAME)//' plugin is running!'//CHAR(13)//CHAR(13)// &
  'This will be terminated* if you close iMOD.'//CHAR(13)//'* only whenever the plugin is not redirecting'//CHAR(13)// &
  'Are you sure you want to exit iMOD?','QUESTION')
 IF(WINFODIALOG(EXITBUTTONCOMMON).EQ.1)THEN
  I=INFOERROR(LASTERROR)
  CALL IOSCOMMANDKILL(IDPROC,0)
  I=INFOERROR(LASTERROR)
  !## no process
  IKILL=0; IDPROC=0
 ENDIF

 END SUBROUTINE PLUGIN_EXE_CHECK_STOP

END MODULE MOD_PLUGIN

