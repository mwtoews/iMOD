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
      EXIT
    END SELECT
   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_GRID1)
      CALL WGRIDPOS(MESSAGE%Y,ICOL,IROW) !#y="to"
      IF(ICOL.EQ.1)CALL PLUGIN_FIELDCHANGE(IPI,IROW,ICOL)
    END SELECT

  END SELECT

 END DO
 
 CALL WDIALOGUNLOAD()
 
 CALL PLUGIN_UPDATEMENU_FILL()
 
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
 CHARACTER(LEN=256) :: DIRNAME
 CHARACTER(LEN=1256) :: LINE,FNAME
 INTEGER :: IOS,IU
 LOGICAL :: LEX
  
 !#sets directory+filename
 CALL WGRIDGETCELLSTRING(IDF_GRID1,ICOL,IROW,DIRNAME)
 FNAME=TRIM(PREFVAL(IPI))//'\'//TRIM(DIRNAME)//'\PLUG-IN.INI'
 
 !# raise expection error if the specific file cannot be found or an none-existing directory is given in preference file.
 INQUIRE(FILE=FNAME,EXIST=LEX)
 IF(.NOT.LEX)THEN
  CALL WDIALOGPUTSTRING(IDF_LABEL1,'Error, can not find '//TRIM(FNAME))
  CALL WDIALOGFIELDSTATE(IDF_LABEL1,0)
  RETURN
 ENDIF
 
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 
 !## read variables from plugin ini-file
 IF(.NOT.UTL_READINITFILE('TXT',LINE,IU,0))RETURN
 READ(LINE,*) FNAME
 CLOSE(IU)
 
 !#select files from ini-file and read these files, echo to description part of plugin-manager
 FNAME=TRIM(PREFVAL(IPI))//'\'//TRIM(DIRNAME)//'\'//TRIM(FNAME)
 
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 
 READ(IU,'(A)') LINE
 CLOSE(IU)

 CALL WDIALOGPUTSTRING(IDF_LABEL1,TRIM(LINE))
 CALL WDIALOGFIELDSTATE(IDF_LABEL1,0)
  
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

!##checking and saving
 !#CALL WGRIDGETCHECKBOX(IDF_GRID1,2,PI(:)%IACT,NROW) !#naar andere subroutine 

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
   ALLOCATE(PI3(SIZE(PI))); PI3=PI
   DEALLOCATE(PI); ALLOCATE(PI(NLN)); PI%PNAME=LN; PI%IACT=0
   DO K=1,NLN
    DO J=1,SIZE(PI)
     IF(TRIM(UTL_CAP(LN(K),'U')).EQ.TRIM(UTL_CAP(PI(J)%PNAME,'U')))THEN; PI(K)%IACT=PI3(J)%IACT; EXIT; ENDIF
    ENDDO
   ENDDO
   DEALLOCATE(PI3)   
  ENDIF
  CALL WGRIDPUTCHECKBOX(IDF_GRID1,2,PI(:)%IACT,NLN)  

 END SUBROUTINE PLUGIN_CHECK

 !###======================================================================
 SUBROUTINE PLUGIN_UPDATEMENU_FILL()
 !###======================================================================
!# Subroutine to connect the plugin-names to the plugin-menu
 IMPLICIT NONE
 INTEGER :: I,J,K,L
 INTEGER,DIMENSION(11) :: MENUID
 DATA MENUID/ID_PLUGIN1,ID_PLUGIN2,ID_PLUGIN3,ID_PLUGIN4,ID_PLUGIN5, &
             ID_PLUGIN6,ID_PLUGIN7,ID_PLUGIN8,ID_PLUGIN9,ID_PLUGIN10/

!#V 1. Call menu/submenu in hoofdmenu 
!#V 2. Verander naam van plugin-name menu onderdelen op basis van PI-names
!#V 3. Dit gebeurt voor zowel PI1 als PI2: vul op basis van beschikbare plugins
!#    Totdat er 10 namen staan met PL1 of PL2 ervoor afhankelijk of de plugin in mapje 1 of mapje 2 staat.
!#V 4. Link het veranderen van de namen aan het apply/save knopje in de manager.
!#V 5. update de namen alleen als het apply/save knopje wordt aangeroepen, anders wordt er niks ingelezen.
!# 6. Indien er minder plugin-namen beschikbaar zijn dan in totaal 10 (zowel manager 1 als 2) dan wordt de
!#    plek weggegooid/verstopt.  
!# 7. Het aantal namen in het menu worden opgeteld in de plugin-manager na klikken op het apply knopje.
!#    if aantal namen > 10 dan komt er een melding met hoeveel namen er te veel zijn aangezet; 
!#    count(aantal namen), if>10 then: "Menu is full. Amount of available places is exeeded with {amount-10}  
!#V 8. Link IACT=1 aan checkbox=1   

 DO I=1,11; CALL WMENUITEMDELETE(MENUID(I)); ENDDO
 
 J=0
 
 DO I=1,SIZE(PI1)
  IF(PI1(I)%IACT.EQ.1)THEN
   J=J+1
    CALL WMENUITEMINSERT(ID_MANAGE_PLUGIN2,2,MENUID(J),'PL1 '//TRIM(PI1(I)%PNAME))
   PI1(I)%ID=MENUID(J)
  ELSE
   PI1(I)%ID=0
  ENDIF
 ENDDO

 DO I=1,SIZE(PI2)
  IF(PI2(I)%IACT.EQ.1)THEN
   J=J+1
   CALL WMENUITEMINSERT(ID_MANAGE_PLUGIN2,2,MENUID(J),'PL2 '//TRIM(PI2(I)%PNAME))
   PI2(I)%ID=MENUID(J)
  ELSE
   PI2(I)%ID=0
  ENDIF
 ENDDO

 END SUBROUTINE PLUGIN_UPDATEMENU_FILL

END MODULE MOD_PLUGIN

