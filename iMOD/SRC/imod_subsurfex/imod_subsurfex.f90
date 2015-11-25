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
!!
MODULE MOD_SUBSURFEX

USE MOD_SUBSURFEX_PAR
USE MOD_MATH_MERGE_PAR, ONLY : IDFNAMES,OUTNAME
USE MOD_MATH_MERGE
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_BATCH
USE MOD_UTL
USE MOD_MANAGER
USE BMPVAR
USE MOD_TOPO
USE MODPLOT
USE IMODVAR
USE RESOURCE
USE WINTERACTER

CONTAINS

 !###======================================================================
 SUBROUTINE SUBSURFEXMAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: ITYPE
 TYPE(WIN_MESSAGE),INTENT(INOUT) :: MESSAGE
 
  SELECT CASE (ITYPE) 
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDCANCEL)
      CALL SUBSURFEXCLOSE()
     CASE (IDOK)
      CALL SUBSURFEXOK()  
     CASE (IDDELETE)
      CALL SUBSURFEXDELETE()
     CASE (IDHELP)
      CALL IMODGETHELP('ssexplorer','VMO.SubsurfExpl')
     CASE (IDZOOMIN)
      CALL SUBSURFEXZOOMIN()
     CASE (IDZOOMOUT)
      CALL SUBSURFEXZOOMOUT()
     CASE (IDOPEN)
      CALL SUBSURFEXOPEN()
     CASE (IDMOVE)
      CALL SUBSURFEXMOVEBUTTON()
     CASE (IDRESET)
      CALL SUBSURFEXRESET()
     CASE (IDDRAW)
      CALL SUBSURFEX_DRAWBUTTON() 
      CLICKS=0     
    END SELECT 
   CASE (EXPOSE, RESIZE) 
    CALL SUBSURFEXDRAWMAP() 
   CASE (MOUSEBUTDOWN)
    SELECT CASE(MESSAGE%VALUE1)
     CASE(1)
      IF(MOVE .EQ. .TRUE.)THEN
       OLDX=MESSAGE%GX; OLDY=MESSAGE%GY
       CALL WCURSORSHAPE(ID_CURSORHANDGREP)
      ELSEIF(DRAW .EQ. .TRUE.)THEN
       CLICKS=CLICKS+1
       IF(CLICKS .EQ. 1)THEN
        OLDX=MESSAGE%GX; OLDY=MESSAGE%GY       
       ELSEIF(CLICKS .EQ. 2)THEN
        NEWX=MESSAGE%GX; NEWY=MESSAGE%GY
        CALL SUBSURFEXDRAWPOLYGON(OLDX,OLDY,NEWX,NEWY,MESSAGE%VALUE1)  
       ELSE
        OLDX=NEWX; OLDY=NEWY
        NEWX=MESSAGE%GX; NEWY=MESSAGE%GY
        CALL SUBSURFEXDRAWPOLYGON(OLDX,OLDY,NEWX,NEWY,MESSAGE%VALUE1)        
       END IF  
      ELSE
       CALL SUBSURFEXSELECTCELL(MESSAGE%GX,MESSAGE%GY)      
      END IF
     CASE(3)
      IF(DRAW .EQ. .TRUE. .AND. CLICKS .GT. 2) CALL SUBSURFEXDRAWPOLYGON(OLDX,OLDY,NEWX,NEWY,MESSAGE%VALUE1) 
      MOVE=.FALSE.; DRAW=.FALSE.       
      CALL WCURSORSHAPE(CURARROW)
    END SELECT
   CASE (MOUSEBUTUP)
    IF(MOVE .EQ. .TRUE.)THEN
     MOVE=.FALSE.; DRAW=.FALSE.
     CALL SUBSURFEXMOVE(MESSAGE%GX,MESSAGE%GY)
     MOVE=.TRUE.    
     CALL WCURSORSHAPE(ID_CURSORHAND)
    END IF
   CASE (MOUSEMOVE)
     CALL SUBSURFEXPOSITION(MESSAGE%GX,MESSAGE%GY)
  END SELECT
         
 END SUBROUTINE SUBSURFEXMAIN
 
  !###======================================================================
 SUBROUTINE SUBSURFEXINIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IU
 INTEGER,ALLOCATABLE,DIMENSION(:) :: HIGHLIGHTED
 CHARACTER(LEN=256) :: GENFILE
 CHARACTER(LEN=16) :: COLOUR
 LOGICAL :: EX
  
 IF(LEN_TRIM(PREFVAL(26)).EQ. 0)THEN
  CALL WMESSAGEBOX(0,1,1,'The tool cannot be used because the keyword SUBSURFEXDBASE is not found in IMOD_INIT.PRF'&
                   ,'Error')
  RETURN
 ENDIF

 IF(LEN_TRIM(PREFVAL(25)).EQ.0)THEN
  PREFVAL(25)='C:\PROGRAM FILES\7-ZIP\7Z.EXE'
  INQUIRE(FILE=PREFVAL(25),EXIST=EX)
  IF(.NOT.EX) THEN
   CALL WMESSAGEBOX(0,1,1,'Subsurface explorer cannot be used, because the keyword to the 7ZIP executable was not '//CHAR(13)// &
       'found in IMOD_INIT.PRF and iMOD has not found the executable in the '//CHAR(13)// &
       'standard directory. Please put the executable in IMOD_INIT.PRF','Error')
   RETURN
  ENDIF
 ENDIF
 
 !## show the subsurface explorer
 IF(WMENUGETSTATE(ID_SUBSURFEX,2).EQ.1)THEN
  CALL SUBSURFEXCLOSE()
  RETURN
 ENDIF 
 CALL WMENUSETSTATE(ID_SUBSURFEX,2,1)
  
 CALL WDIALOGLOAD(ID_DSUBSURFEX)
 CALL WDIALOGSELECT(ID_DSUBSURFEX)
 CALL WDIALOGSHOW(-1,-1,0,3)
 CALL WDIALOGPUTIMAGE(IDZOOMOUT,ID_ICONZOOMOUT,1)
 CALL WDIALOGPUTIMAGE(IDZOOMIN,ID_ICONZOOMIN,1) 
 CALL WDIALOGPUTIMAGE(IDMOVE,ID_ICONMOVE,1)  
 CALL WDIALOGPUTIMAGE(IDRESET,ID_ICONZOOMTO,1)
 CALL SUBSURFEXCLEARTMP()
 
 !## import the map for the manager if it is missing
 INQUIRE(FILE=TRIM(PREFVAL(1))//'\TMP\PROVINCES.GEN',EXIST=EX)
 IF(EX .EQ. .FALSE.) THEN
  CALL SYSTEM('""'//TRIM(PREFVAL(25))//'" X -O"'//TRIM(PREFVAL(1))//'\TMP\" "'// &
                                 TRIM(PREFVAL(26))//'\PROVINCES.7Z""')
  !## set file's attribute to read-only (needed in SUBSURFEXClearTMP subroutine)                                 
  CALL SYSTEM('ATTRIB +R "'//TRIM(PREFVAL(1))//'\TMP\PROVINCES.GEN"')
 END IF                                                           
 
 !## set up and draw the map
 IU=UTL_GETUNIT()
 OPEN(UNIT=IU,FILE=TRIM(PREFVAL(26))//'\SETTINGS.TXT',ACTION='READ',FORM='FORMATTED')
 READ(IU,*) INIXMIN
 READ(IU,*) INIYMIN
 READ(IU,*) INIXMAX
 READ(IU,*) INIYMAX
 READ(IU,*) GRIDSIZE
 CLOSE(IU)
 XMIN=INIXMIN
 YMIN=INIYMIN
 XMAX=INIXMAX
 YMAX=INIYMAX 
 CALL IGRSELECT(3,IDF_PICTURE5)
 CALL IGRAREA(0.0,0.0,1.0,1.0)
 CALL IGRUNITS(XMIN,YMIN,XMAX,YMAX) 
 CALL IGRAREACLEAR()
 CALL WDIALOGCOLOUR(IDF_PICTURE5,255,255) 
 CALL SUBSURFEXDRAWMAP()

 !## check which projects are present in the projects folder and show them in the SUBSURFEX
 IF(.NOT.UTL_DIRINFO_POINTER(TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER','*',PROJECTNAMES,'D'))RETURN
 CALL WDIALOGPUTMENU(IDF_MENU3,PROJECTNAMES,SIZE(PROJECTNAMES),0,"New project")
 
 !## check which data are present in the database folder and show them in the SUBSURFEX
 IF(.NOT.UTL_DIRINFO_POINTER(TRIM(PREFVAL(26)),'*',FOLDERNAMES,'D'))RETURN
 NPARAM=SIZE(FOLDERNAMES)
 ALLOCATE(PARAM(NPARAM),HIGHLIGHTED(NPARAM))
 HIGHLIGHTED(:)=0
 CALL WDIALOGPUTMENU(IDF_MENU4,FOLDERNAMES,NPARAM,HIGHLIGHTED)
 PARAM(:)=FOLDERNAMES(:)
 
 !## miscellaneous settings
 MOVE=.FALSE.; DRAW=.FALSE.; POLYGONNR=1
 
 DEALLOCATE(HIGHLIGHTED)
 END SUBROUTINE SUBSURFEXINIT
 
  !###======================================================================
 SUBROUTINE SUBSURFEXOK()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: OK,IU,I
 INTEGER :: SPACE,IOPTION
 LOGICAL :: EX
 
 !## read the selected cells
 INQUIRE(FILE=TRIM(PREFVAL(1))//'\TMP\CELLEN.TXT',EXIST=EX)
 IF(EX .EQ. .TRUE.) THEN
  IU=UTL_GETUNIT()
  OPEN(UNIT=IU,FILE=TRIM(PREFVAL(1))//'\TMP\CELLEN.TXT',ACTION='READ',FORM='FORMATTED')
  READ(IU,*) NROFCELLS
  ALLOCATE(CELLS(NROFCELLS))
  DO I=1,NROFCELLS
   READ(IU,*) CELLS(I)
  END DO
  CLOSE(IU)
 ELSE
  CALL WMESSAGEBOX(0,3,0,'No cells were selected on the map, please select at least one cell to continue.','Warning')
  RETURN  
 END IF
 
 ALLOCATE(DATASELECTION(NPARAM)) 
 CALL WDIALOGGETMENU(IDF_MENU3,IOPTION,PROJECT)
 CALL WDIALOGGETMENU(IDF_MENU4,DATASELECTION)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO3,CLEAR) 
 
 !## check if all fields are used
 OK=1
 IF(PROJECT .EQ. 'New project' .OR. SUM(DATASELECTION) .EQ. 0 .OR. NROFCELLS .EQ. 0) OK=0 
 IF(OK .EQ. 0)THEN
  CALL WMESSAGEBOX(0,3,0,'One or more fields were left open. Please make sure all fields are used.','Warning')
  DEALLOCATE(DATASELECTION,CELLS)
  RETURN
 END IF
  
 CALL WMESSAGEBOX(3,2,2,'Are you sure you want to load this selection?','Confirm')
 IF(WINFODIALOG(ExitButtonCommon) == 1) THEN 
  !## create  new project folder if wanted and make sure the project name does not contain spaces
  IF(IOPTION == 0) CALL SYSTEM('MKDIR "'//TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'"') 
  CALL WINDOWSELECT(0) 
  CALL WMENUSETSTATE(ID_SUBSURFEX,2,0)
  CALL WDIALOGSELECT(ID_DSUBSURFEX)
  CALL WDIALOGHIDE()
  CALL WDIALOGUNLOAD()
  CALL SUBSURFEX_DATALOADER() 
 ELSE
  DEALLOCATE(DATASELECTION,CELLS) 
  RETURN
 END IF
 
 CALL WCURSORSHAPE(CURARROW)
 CALL SUBSURFEXCLEARTMP()
 DEALLOCATE(PARAM) 
 END SUBROUTINE SUBSURFEXOK 
 
 !###======================================================================
 SUBROUTINE SUBSURFEX_DATALOADER()
 !###====================================================================== 
 IMPLICIT NONE
 INTEGER ::  A,I,J,K,L,N,M,IU,JU,NLINES,NCOL,NROW,EXISTS,NROWEXISTS,IOS,COUNTER,INDX,LEGEND
 INTEGER,ALLOCATABLE,DIMENSION(:) :: X,Y 
 INTEGER,ALLOCATABLE,DIMENSION(:) :: PREVLOADED,NONPREVLOADEDCELLS
 CHARACTER(LEN=8) :: EXTENSION
 CHARACTER(LEN=52) :: NAME
 CHARACTER(LEN=256) :: LINE,COMMAND,SUBST1,SUBST2,FILE1,FILE2,FNAME
 CHARACTER(LEN=16),ALLOCATABLE,DIMENSION(:) :: CELLSCHAR 
 CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: ID,XCHAR,YCHAR,SURFACELEVELCHAR,YENDCHAR,OUTPUT,IDFNAMES_TEMP,IDFNAMESCOPY,ALLIDFNAMES
 CHARACTER(LEN=256),DIMENSION(:),POINTER :: IDFNAMES_POINTER,DEL_IDFNAMES,IMAGES
 REAL,ALLOCATABLE,DIMENSION(:) :: SURFACELEVEL,YEND
 LOGICAL :: EX,EX2
 
 CHARACTER(LEN=4) :: EXT
 LOGICAL :: LEX
 REAL :: DX,DY,OR1,OR2
 
 ALLOCATE(CELLSCHAR(NROFCELLS))
 
 DO I=1,NROFCELLS
  WRITE(CELLSCHAR(I),'(I0.0)') CELLS(I)
 END DO
 
 !## clear project first if wanted 
 IF(CLEAR .EQ. 1) THEN
  CALL SYSTEM('RMDIR "'//TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'" /S /Q')
  CALL SYSTEM('MKDIR "'//TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'"')
 END IF   
 
 !## show progress bar
 CALL WDIALOGLOAD(ID_DSUBSURFEX_PROGRESS)
 CALL WDIALOGSHOW(ITYPE=SEMIMODELESS)
 CALL WDIALOGRANGEPROGRESSBAR(IDF_PROGRESS1,1,NPARAM)

 !## load selected data 
 DO I=1,NPARAM
  !## update progress bar
  CALL WDIALOGSELECT(ID_DSUBSURFEX_PROGRESS) 
  CALL WDIALOGPUTPROGRESSBAR(IDF_PROGRESS1,I) 
  CALL WDIALOGPUTSTRING(IDF_LABEL1,'Importing '//TRIM(PARAM(I))//' data ...')
  
  IF(DATASELECTION(I).EQ.0)CYCLE
  
  !## read from settings file: extension, legend and if ipf the header
  IU=UTL_GETUNIT()
  OPEN(UNIT=IU,FILE=TRIM(PREFVAL(26))//'\'//TRIM(PARAM(I))//'\SETTINGS.TXT',ACTION='READ',FORM='FORMATTED')
  READ(IU,*) EXTENSION
  READ(IU,*) LEGEND
  IF(TRIM(EXTENSION) .EQ. 'IPF') THEN
   READ(IU,*) NCOL
   ALLOCATE(HEADER(NCOL+1))
   DO J=1,NCOL+1
    READ(IU,'(A)') LINE
    HEADER(J)=TRIM(LINE)
   END DO
  END IF
  CLOSE(IU)

  INQUIRE(FILE=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\PREVLOADED.TXT',EXIST=EX)
  IF(EX == .TRUE.)THEN     !## read which cells have already been loaded before  
   JU=UTL_GETUNIT()
   OPEN(UNIT=JU,FILE=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
        //TRIM(PARAM(I))//'\PREVLOADED.TXT',ACTION='READ',FORM='FORMATTED')
   READ(JU,*) N
   ALLOCATE(PREVLOADED(N))
   DO J=1,N; READ(JU,*) PREVLOADED(J); ENDDO
   CLOSE(JU,STATUS='DELETE')
  ELSE     !## set prevloaded to 0, so all selected cells will be loaded
   ALLOCATE(PREVLOADED(NROFCELLS))
   N=NROFCELLS
   PREVLOADED(:)=0
  END IF
   
  !## create data type folder in project folder if needed
  INQUIRE(DIRECTORY=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I)),EXIST=EX2)
  IF(EX2 == .FALSE.) CALL SYSTEM('MKDIR "'//TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'&
                                 //TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'"')   
   
  !## open new prevloaded file
  JU=UTL_GETUNIT()
  OPEN(UNIT=JU,FILE=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
       //TRIM(PARAM(I))//'\PREVLOADED.TXT',ACTION='WRITE',FORM='FORMATTED')
  IF(EX == .TRUE.)THEN
   DO J=1,N
    WRITE(JU,'(I0.0)') PREVLOADED(J)
   END DO
  END IF
    
  !## load missing cells
  M=0  !## M is the nr of actually loaded cells that were not loaded before
  DO J=1,NROFCELLS
   EXISTS=0
   DO K=1,N
    IF(CELLS(J) .EQ. PREVLOADED(K)) THEN
     EXISTS=1
     EXIT
    END IF 
   END DO
   IF(EXISTS .EQ. 1) CYCLE   
   CALL SYSTEM('""'//TRIM(PREFVAL(25))//'" X -O"'//TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\' &
               //TRIM(PARAM(I))//'\" "'//TRIM(PREFVAL(26))//'\'//TRIM(PARAM(I))//'\'//TRIM(CELLSCHAR(J))//'.7Z""')
   M=M+1
   WRITE(JU,'(I0.0)') CELLS(J)
  END DO 
  CLOSE(JU)
  
  !## update prevloaded.txt with nr of cells in the file
  JU=UTL_GETUNIT()
  OPEN(UNIT=JU,FILE=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
       //TRIM(PARAM(I))//'\PREVLOADED.TXT',ACTION='READ',FORM='FORMATTED') 
  IF(EX .EQ. .FALSE.) N=0
  DEALLOCATE(PREVLOADED); ALLOCATE(PREVLOADED(N+M))
  DO J=1,N+M
   READ(JU,*) PREVLOADED(J)
  END DO
  CLOSE(JU,STATUS='DELETE') !## delete the prevloaded.txt without the nr of cells in it
  JU=UTL_GETUNIT()
  OPEN(UNIT=JU,FILE=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
       //TRIM(PARAM(I))//'\PREVLOADED.TXT',ACTION='WRITE',FORM='FORMATTED')
  WRITE(JU,*) N+M !## open a new prevloaded.txt and also write the nr of cells
  DO J=1,N+M
   WRITE(JU,'(I0.0)') PREVLOADED(J)
  END DO
  CLOSE(JU)
   
  !## update progress bar
  CALL WDIALOGSELECT(ID_DSUBSURFEX_PROGRESS) 
  CALL WDIALOGPUTSTRING(IDF_LABEL1,'Merging '//TRIM(PARAM(I))//' files ...')    
  
  !## load ipf/idf and legends into iMOD manager and merge idf's/ipf's 
  IF(M .GT. 0)THEN !## only when new cells are selected
  SELECT CASE (TRIM(EXTENSION))
   CASE ('IPF') !## ipf's
  
  INQUIRE(FILE=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
          //TRIM(PARAM(I))//'\'//TRIM(PARAM(I))//'.IPF',EXIST=EX)   
   IF(EX .EQ. .FALSE.) THEN
    CALL SUBSURFEX_IPFMERGE(I,NCOL)  
   ELSE
    !##store existing entries in memory
    IU=UTL_GETUNIT()
    OPEN(UNIT=IU,FILE=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
         //TRIM(PARAM(I))//'\'//TRIM(PARAM(I))//'.IPF',ACTION='READ',FORM='FORMATTED')
    READ(IU,*) NLINES
    READ(IU,*) NCOL
    DO J=1,NCOL+1
     READ(IU,*)
    END DO
    ALLOCATE(OUTPUT(NLINES))
    DO J=1,NLINES
     READ(IU,'(A)') LINE
     WRITE(OUTPUT(J),'(A)') TRIM(LINE)
    END DO
    CLOSE(IU)
   
    !## write existing entries to file without header
    IU=UTL_GETUNIT()
    OPEN(IU,FILE=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
         //TRIM(PARAM(I))//'\EXISTING.IPF',FORM='FORMATTED')
    DO J=1,NLINES
     WRITE(IU,'(A)') TRIM(OUTPUT(J))
    END DO
    CLOSE(IU)    
    CALL SYSTEM('DEL "'//TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
                //TRIM(PARAM(I))//'\'//TRIM(PARAM(I))//'.IPF"')
    DEALLOCATE (OUTPUT)
    !## merge new and existing points to one ipf with a header
    CALL SUBSURFEX_IPFMERGE(I,NCOL)
   
   END IF  
   !## import the legend if it is available but missing
   IF(LEGEND .EQ. 1)THEN
   INQUIRE(FILE=TRIM(PREFVAL(1))//'\SETTINGS\DRILL.DLF',EXIST=EX)
   IF(EX .EQ. .FALSE.) CALL SYSTEM('""'//TRIM(PREFVAL(25))//'" X -O"'//TRIM(PREFVAL(1))//'\SETTINGS\" "'&
                                   //TRIM(PREFVAL(26))//'\'//TRIM(PARAM(I))//'\LEGEND.7Z""')
   END IF                                   
   !## load the merged ipf and legend into the iMOD manager
   CALL IDFINIT(TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\'&
                //TRIM(PARAM(I))//'.IPF',LPLOT=.FALSE.,LDEACTIVATE=.FALSE.)            
   
  CASE ('IDF') !## idf's
  
  IF(M .GT. 1 .OR. N .NE. 0)THEN !## only merge idf's when two or more idf's are being loaded
   
   !## get a list of idf's to merge
   IF(.NOT.UTL_DIRINFO_POINTER(TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
                               //TRIM(PARAM(I))//'\','*.IDF',IDFNAMES_POINTER,'F'))RETURN
   ALLOCATE(IDFNAMES_TEMP(SIZE(IDFNAMES_POINTER)))
   IDFNAMES_TEMP(:)='NoData'
   !## remove duplicates from list
   COUNTER=0
   DO J=1,SIZE(IDFNAMES_POINTER)
    A=INDEX(IDFNAMES_POINTER(J),'_')
    NAME=TRIM(IDFNAMES_POINTER(J)(:A-1))
 
    EXISTS=0
    DO K=1,SIZE(IDFNAMES_POINTER)
     IF(TRIM(NAME) .EQ. TRIM(IDFNAMES_TEMP(K))) EXISTS=1
     IF(EXISTS .EQ. 1) EXIT 
    END DO
    IF(EXISTS .EQ. 0) THEN
     IDFNAMES_TEMP(J)=TRIM(NAME)
     COUNTER=COUNTER+1
    END IF     
   END DO   
   ALLOCATE(ALLIDFNAMES(COUNTER))
   COUNTER=0
   DO J=1,SIZE(IDFNAMES_TEMP)
    IF(IDFNAMES_TEMP(J) .NE. 'NoData')THEN
     COUNTER=COUNTER+1
     ALLIDFNAMES(COUNTER)=IDFNAMES_TEMP(J)
    END IF
   END DO
 
   !## merge idf's
   DO J=1,SIZE(ALLIDFNAMES) 
    OUTNAME=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\'&
            //TRIM(ALLIDFNAMES(J))//'_MERGED.IDF'
    INQUIRE(FILE=OUTNAME,EXIST=EX2)
    IF(EX2 .EQ. .TRUE.) THEN
     COMMAND='"REN "'//TRIM(OUTNAME)//'" "'//TRIM(ALLIDFNAMES(J))//'_MERGEDOLD.IDF""'
     CALL SYSTEM(TRIM(COMMAND))
    END IF 
    CALL IOSDIRCOUNT(TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
                     //TRIM(PARAM(I))//'\',TRIM(ALLIDFNAMES(J))//'_*.IDF',N)
    ALLOCATE(IDFNAMES(N))
    CALL UTL_DIRINFO(TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
                     //TRIM(PARAM(I))//'\',TRIM(ALLIDFNAMES(J))//'_*.IDF',IDFNAMES,N,'F')
    DO K=1,N
     IDFNAMES(K)=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\'//TRIM(IDFNAMES(K))
    END DO
    IMASK=0; IEXT=2
    IF(MATH1MERGE(0))THEN
    ENDIF
    CALL MATH1MERGECLOSE(0)
   END DO
   !## delete files that are already merged
   IF(.NOT.UTL_DIRINFO_POINTER(TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
      //TRIM(PARAM(I))//'\','*.IDF',DEL_IDFNAMES,'F'))RETURN
   DO K=1,SIZE(DEL_IDFNAMES)
    INDX=0
    INDX=INDEX(DEL_IDFNAMES(K),'MERGED.IDF')
    IF(INDX .NE. 0) CYCLE
    CALL SYSTEM('DEL "'//TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
                //TRIM(PARAM(I))//'\'//TRIM(DEL_IDFNAMES(K))//'"')
   END DO
   DEALLOCATE(DEL_IDFNAMES)
   
  END IF 
   
   !## update progress bar
   CALL WDIALOGSELECT(ID_DSUBSURFEX_PROGRESS) 
   CALL WDIALOGPUTSTRING(IDF_LABEL1,'Creating '//TRIM(PARAM(I))//' MDF file ...')   
   
   !## create mdf 
   INQUIRE(FILE=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
           //TRIM(PARAM(I))//'\'//TRIM(PARAM(I))//'.MDF',EXIST=EX2)
   IF(EX2 .EQ. .FALSE. .OR. EX2 .EQ. .TRUE. .AND. N .EQ. 2)THEN 
    CALL SYSTEM('MKDIR "'//TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\TEMP"')
    CALL SYSTEM('""'//TRIM(PREFVAL(25))//'" X -O"'//TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
                //TRIM(PARAM(I))//'\TEMP\" "'//TRIM(PREFVAL(26))//'\'//TRIM(PARAM(I))//'\MDF.7Z""')
                
    IF(M .EQ. 1 .AND. N .EQ. 0) THEN !## M=nr of new cells, N=nr of prevloaded cells
     A=3 !## once for the path, twice for _merged substitution
    ELSE
     A=1 !## only once substitute the path
    END IF
    SUBST1='PATH'
    SUBST2=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))
    FILE1=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\TEMP\'//TRIM(PARAM(I))//'.MDF'
    FILE2=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\'//TRIM(PARAM(I))//'.MDF'
    DO J=1,A
     IU=UTL_GETUNIT()
     OPEN(UNIT=IU,FILE=FILE1,ACTION='READ',FORM='FORMATTED')
     JU=UTL_GETUNIT()
     OPEN(UNIT=JU,FILE=FILE2,ACTION='WRITE',FORM='FORMATTED')
     DO 
      READ(IU,'(A256)',IOSTAT=IOS) LINE
      IF(IOS .NE. 0) EXIT    
      LINE=UTL_CAP(LINE,'U')
      LINE=UTL_SUBST(LINE,TRIM(SUBST1),TRIM(SUBST2))
      WRITE(JU,'(A)') TRIM(LINE)
     END DO
     CLOSE(IU); CLOSE(JU)
     SUBST1='_MERGED'; SUBST2='_'//TRIM(CELLSCHAR(1)) !## corresponds to A=2
     FILE1=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\'//TRIM(PARAM(I))//'.MDF'
     FILE2=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\'//TRIM(PARAM(I))//'.TEMPMDF'
     IF(J .EQ. 2)THEN
      CALL SYSTEM('DEL "'//TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
                  //TRIM(PARAM(I))//'\'//TRIM(PARAM(I))//'.MDF"')
      FILE1=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\'&
            //TRIM(PARAM(I))//'.TEMPMDF'
      FILE2=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\'//TRIM(PARAM(I))//'.MDF'
     ELSEIF(J .EQ. 3) THEN
      CALL SYSTEM('DEL "'//TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
                  //TRIM(PARAM(I))//'\'//TRIM(PARAM(I))//'.TEMPMDF"')      
     END IF
    END DO  
    CALL SYSTEM('RMDIR "'//TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\TEMP" /Q /S')    
   END IF
   
   !## load mdf into iMOD manager
   SELECT CASE(LEGEND)
    CASE(1)
   !## import legend if it is missing
    INQUIRE(FILE=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\'&
            //TRIM(PARAM(I))//'.LEG',EXIST=EX)
    IF(EX .EQ. .FALSE.) CALL SYSTEM('""'//TRIM(PREFVAL(25))//'" X -O"'&
    //TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\" "'&
    //TRIM(PREFVAL(26))//'\'//TRIM(PARAM(I))//'\LEGEND.7Z""')
    CALL IDFINIT(TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\'&
                 //TRIM(PARAM(I))//'.MDF',TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
                 //TRIM(PARAM(I))//'\'//TRIM(PARAM(I))//'.LEG',LPLOT=.FALSE.,LDEACTIVATE=.FALSE.)                                    
   
   CASE(0)
    CALL IDFINIT(TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\'&
                 //TRIM(PARAM(I))//'.MDF',LPLOT=.FALSE.,LDEACTIVATE=.FALSE.)   
   END SELECT
   
  CASE('PNG') !## images
   
   DO J=1,NROFCELLS
    FNAME=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
          //TRIM(PARAM(I))//'\'//TRIM(CELLSCHAR(J))//'.png'
    INQUIRE(FILE=TRIM(FNAME),EXIST=EX2)
    IF(EX2 .EQ. .TRUE.) THEN
     BMP(NBMP+1)%BMPFNAME=TRIM(FNAME)
     CALL TOPO1OPENBMP(0)
    END IF 
   END DO  
   
  END SELECT 
  END IF
  DEALLOCATE(PREVLOADED)
  IF(ALLOCATED(IDFNAMES_TEMP)) DEALLOCATE(IDFNAMES_TEMP)
  IF(ALLOCATED(ALLIDFNAMES)) DEALLOCATE(ALLIDFNAMES)
  IF(ALLOCATED(HEADER)) DEALLOCATE(HEADER)
 END DO
 
 !## update progress bar
 CALL WDIALOGSELECT(ID_DSUBSURFEX_PROGRESS) 
 CALL WDIALOGPUTSTRING(IDF_LABEL1,'Plotting selected data ...') 
   
 !## hide progress bar
 CALL WINDOWSELECT(0) 
 CALL WDIALOGSELECT(ID_DSUBSURFEX_PROGRESS)
 CALL WDIALOGHIDE()
 CALL WDIALOGUNLOAD()
 
 !## plot data
 CALL TOPOINIT() 
 !## zoom to correct location 
 CALL IDFZOOM(ID_ZOOMFULLMAP,(XMAX+XMIN)/2.0,(YMAX+YMIN)/2.0,0)
 CALL IDFPLOT(1)

 !## show iMOD manager
 MP(:)%ASSCOL1=3 !## legend info of ipf's is stored in the third column
 CALL MANAGERSHOW()
 
 DEALLOCATE(CELLS,CELLSCHAR,DATASELECTION)
 
 END SUBROUTINE SUBSURFEX_DATALOADER
 
 !###======================================================================
 SUBROUTINE SUBSURFEX_IPFMERGE(I,NCOL)
 !###======================================================================
 IMPLICIT NONE 
 INTEGER :: I,J,NLINES,IU,IOS,NCOL
 LOGICAL :: EX
 
 NLINES=0
 
 !## merge all headerless ipf's  in the folder
 CALL SYSTEM('MKDIR "'//TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\TEMP\"')
 CALL SYSTEM('"COPY /B "'//TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\*.IPF" "'&
             //TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\TEMP\TMP.TXT""')  

 !## determine nlines 
 INQUIRE(FILE=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\TEMP\TMP.TXT',EXIST=EX)
 IF(EX .EQ. .TRUE.) THEN
  IU=UTL_GETUNIT()
  OPEN(UNIT=IU,FILE=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
                    //TRIM(PARAM(I))//'\TEMP\TMP.TXT',ACTION='READ',FORM='FORMATTED')
  DO
   READ(IU,*,IOSTAT=IOS)
   IF(IOS.NE.0)EXIT
   NLINES=NLINES+1
  END DO     
  CLOSE(IU)  
  
  IU=UTL_GETUNIT()
  OPEN(UNIT=IU,FILE=TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
       //TRIM(PARAM(I))//'\TEMP\HEADER.TXT',ACTION='WRITE',FORM='FORMATTED')
  WRITE(IU,*) NLINES
  WRITE(IU,*) NCOL
  DO J=1,NCOL+1
   WRITE(IU,'(A)') TRIM(HEADER(J))
  END DO
  CLOSE(IU)  
 END IF

 !## merge header and tmp.txt and clean project folder
 CALL SYSTEM('DEL "'//TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\*.IPF"')
 CALL SYSTEM('"COPY /B "'//TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'&
             //TRIM(PARAM(I))//'\TEMP\*.TXT" "'&
             //TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\'&
             //TRIM(PARAM(I))//'.IPF""')
 CALL SYSTEM('RMDIR "'//TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'//TRIM(PROJECT)//'\'//TRIM(PARAM(I))//'\TEMP\" /S /Q') 
 
 END SUBROUTINE SUBSURFEX_IPFMERGE
 
 !###======================================================================
 SUBROUTINE SUBSURFEXDELETE()
 !###======================================================================
 IMPLICIT NONE 
 INTEGER :: IOPTION
 
 CALL WDIALOGGETMENU(IDF_MENU3,IOPTION,PROJECT)
 IF(IOPTION .GE. 1)THEN
  CALL WMESSAGEBOX(3,2,2,'Are you sure you want to delete this project?','Confirm')
 
  !## delete the project folder
  IF(WINFODIALOG(ExitButtonCommon) == 1) CALL SYSTEM('RMDIR "'//TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\'&
                                                     //TRIM(PROJECT)//'" /S /Q')
  
  !## refresh the contents of menu field showing the available projects
  IF(.NOT.UTL_DIRINFO_POINTER(TRIM(PREFVAL(1))//'\SUBSURFACE_EXPLORER\','*',PROJECTNAMES,'D'))RETURN
  CALL WDIALOGPUTMENU(IDF_MENU3,PROJECTNAMES,SIZE(PROJECTNAMES),0,"New project")
 ELSE !## if no project was selected to delete, set an error message
  CALL WMESSAGEBOX(0,1,1,'Select a project to delete','Error')
 END IF
 
 END SUBROUTINE SUBSURFEXDELETE
 
 !###======================================================================
 SUBROUTINE SUBSURFEXCLOSE()
 !###======================================================================
 IMPLICIT NONE
 LOGICAL :: EX
 
 !## close the dialog, clear the tmp folder
 CALL WINDOWSELECT(0) 
 CALL WMENUSETSTATE(ID_SUBSURFEX,2,0)
 CALL WDIALOGSELECT(ID_DSUBSURFEX)
 CALL WDIALOGUNLOAD()
 CALL SUBSURFEXCLEARTMP()
 CALL WCURSORSHAPE(CURARROW)
 
 !## remove the map shown on the dialog from memory
 DEALLOCATE(VERTICESX,VERTICESY,PARAM)

 END SUBROUTINE SUBSURFEXCLOSE
 
 !###======================================================================
 SUBROUTINE SUBSURFEXCLEARTMP()
 !###======================================================================
 IMPLICIT NONE
 LOGICAL :: EX

 CALL SYSTEM('DEL /A:-R "'//TRIM(PREFVAL(1))//'\TMP\*.GEN"')
 INQUIRE(FILE=TRIM(PREFVAL(1))//'\TMP\CELLEN.TXT',EXIST=EX)
 IF(EX .EQ. .TRUE.) CALL SYSTEM('DEL "'//TRIM(PREFVAL(1))//'\TMP\CELLEN.TXT"')
 INQUIRE(FILE=TRIM(PREFVAL(1))//'\TMP\GENFILES.TXT',EXIST=EX)
 IF(EX .EQ. .TRUE.) CALL SYSTEM('DEL "'//TRIM(PREFVAL(1))//'\TMP\GENFILES.TXT"')

 END SUBROUTINE SUBSURFEXCLEARTMP
 
  !###======================================================================
 SUBROUTINE SUBSURFEXDRAWMAP()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,IU,N,LINEWIDTH,FINDCELLS,IOS
 REAL :: FROMX,FROMY,TOX,TOY,WIDTH,HEIGTH,XMID,YMID
 CHARACTER(LEN=256) :: GENFILE
 CHARACTER(LEN=32) :: XMINSTRING,XMAXSTRING,YMINSTRING,YMAXSTRING,XMIDSTRING,YMIDSTRING
 CHARACTER(LEN=16) :: COLOUR
 LOGICAL :: EX
 
  !## select and set up the frame on the dialog
  !## uncomment these lines to use the IDFCRDCOR functionality when plotting the map
! WIDTH=WINFODIALOGFIELD(IDF_PICTURE5,FieldWidth)
! HEIGTH=WINFODIALOGFIELD(IDF_PICTURE5,FieldHeight)
! CALL UTL_IDFCRDCOR(XMIN,YMIN,XMAX,YMAX,WIDTH,HEIGTH)
 CALL IGRSELECT(3,IDF_PICTURE5)
 CALL IGRAREA(0.0,0.0,1.0,1.0)
 CALL IGRUNITS(XMIN,YMIN,XMAX,YMAX)
 WRITE(XMINSTRING,'(I0.0)') INT(XMIN)
 WRITE(XMAXSTRING,'(I0.0)') INT(XMAX)
 WRITE(YMINSTRING,'(I0.0)') INT(YMIN)
 WRITE(YMAXSTRING,'(I0.0)') INT(YMAX)
 CALL WDIALOGPUTSTRING(IDF_LABEL1,XMINSTRING)
 CALL WDIALOGPUTSTRING(IDF_LABEL2,XMAXSTRING)
 CALL WDIALOGPUTSTRING(IDF_LABEL4,YMINSTRING)
 CALL WDIALOGPUTSTRING(IDF_LABEL5,YMAXSTRING)
 CALL IGRAREACLEAR()
 CALL WDIALOGCOLOUR(IDF_PICTURE5,255,255) 
 !## draw the map and grid
 CALL SUBSURFEX_DRAWPROVINCES()
 CALL SUBSURFEXDRAWGRID()
 
 !## (re)draw polygons and selected cells
 INQUIRE(FILE=TRIM(PREFVAL(1))//'\TMP\GENFILES.TXT',EXIST=EX)
 IF(EX .EQ. .TRUE.)THEN
  IU=UTL_GETUNIT()
  OPEN(UNIT=IU,FILE=TRIM(PREFVAL(1))//'\TMP\GENFILES.TXT',ACTION='READ',FORM='FORMATTED')
  DO 
   READ(IU,*,IOSTAT=IOS) GENFILE,COLOUR,LINEWIDTH,FINDCELLS
   IF(IOS .NE. 0) EXIT
   CALL SUBSURFEXGENDRAW(GENFILE,COLOUR,LINEWIDTH,FINDCELLS)
  END DO
  CLOSE(IU)
 END IF

 END SUBROUTINE SUBSURFEXDRAWMAP
 
  !###======================================================================
 SUBROUTINE SUBSURFEXDRAWGRID()
 !###======================================================================
 IMPLICIT NONE 
 INTEGER :: I,A,B,LINEWIDTH
 REAL :: FROMX,FROMY,TOX,TOY
 
 !## draw the grid by joining points
 CALL IGRCOLOUR('BLACK')
 CALL IGRLINEWIDTH(1)
 FROMX=INIXMIN
 TOX=FROMX
 FROMY=INIYMIN
 TOY=INIYMAX
 A=(INIXMAX-INIXMIN)/GRIDSIZE
 B=(INIYMAX-INIYMIN)/GRIDSIZE
 DO I=1,A+1
  CALL IGRJOIN(FROMX,FROMY,TOX,TOY)
  FROMX=FROMX+GRIDSIZE
  TOX=FROMX
 END DO
 FROMX=INIXMIN
 TOX=INIXMAX
 FROMY=INIYMIN
 TOY=FROMY
 DO I=1,B+1
  CALL IGRJOIN(FROMX,FROMY,TOX,TOY)
  FROMY=FROMY+GRIDSIZE
  TOY=FROMY
 END DO
 
 END SUBROUTINE SUBSURFEXDRAWGRID 
 
  !###======================================================================
 SUBROUTINE SUBSURFEXZOOMIN()
 !###======================================================================
 IMPLICIT NONE
 REAL :: X1,X2,Y1,Y2,DX,DY 
 
 MOVE=.FALSE.; DRAW=.FALSE.
 
 X1=WINFOGRREAL(11)
 Y1=WINFOGRREAL(12)
 X2=WINFOGRREAL(13)
 Y2=WINFOGRREAL(14) 
 DX=(X2-X1)/10    
 DY=(Y2-Y1)/10
 
 XMIN=XMIN+DX
 YMIN=YMIN+DY
 XMAX=XMAX-DX
 YMAX=YMAX-DY 

 CALL SUBSURFEXDRAWMAP()

 END SUBROUTINE SUBSURFEXZOOMIN
 
  !###======================================================================
 SUBROUTINE SUBSURFEXZOOMOUT()
 !###======================================================================
 IMPLICIT NONE
 REAL :: X1,X2,Y1,Y2,DX,DY

 MOVE=.FALSE.; DRAW=.FALSE.

 X1=WINFOGRREAL(11)
 Y1=WINFOGRREAL(12)
 X2=WINFOGRREAL(13)
 Y2=WINFOGRREAL(14) 
 DX=(X2-X1)/10
 DY=(Y2-Y1)/10

 XMIN=XMIN-DX
 YMIN=YMIN-DY 
 XMAX=XMAX+DX
 YMAX=YMAX+DY 

 CALL SUBSURFEXDRAWMAP()

 END SUBROUTINE SUBSURFEXZOOMOUT
 
 !###======================================================================
 SUBROUTINE SUBSURFEXSELECTCELL(X,Y)
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,CELL,N,IU,JU,X1,X2,Y1,Y2,EXISTS,INDX,IOS
 INTEGER,ALLOCATABLE,DIMENSION(:) :: SELECTION
 CHARACTER(LEN=256) :: GENFILE,LINE
 CHARACTER(LEN=16) :: COLOUR
 CHARACTER(LEN=16) :: CELLSTRING
 REAL :: X,Y
 LOGICAL :: EX 
  
 INDX=0; CELL=0
 X1=INIXMIN
 X2=INIXMAX-GRIDSIZE
 Y1=INIYMIN
 Y2=INIYMAX-GRIDSIZE
 DO I=Y1,Y2,INT(GRIDSIZE)
  DO J=X1,X2,INT(GRIDSIZE)
   CELL=CELL+1
   WRITE(CELLSTRING,'(I0.1)') CELL
   IF(X .GT. J .AND. X .LT. J+INT(GRIDSIZE) .AND. Y .GT. I.AND. Y .LT. I+INT(GRIDSIZE)) THEN
    INQUIRE(FILE=TRIM(PREFVAL(1))//'\TMP\'//TRIM(CELLSTRING)//'.GEN',EXIST=EX)
    IF(EX .EQ. .TRUE.) THEN !## if selected already, deselect the cell
    !## delete gen of the cell
    WRITE(CELLSTRING,'(I0.0)') CELL
    INQUIRE(FILE=TRIM(PREFVAL(1))//'\TMP\'//TRIM(CELLSTRING)//'.GEN',EXIST=EX)
    IF(EX .EQ. .FALSE.) CYCLE
    CALL SYSTEM('DEL '//TRIM(PREFVAL(1))//'\TMP\'//TRIM(CELLSTRING)//'.GEN')
    
    !## update cellen.txt
    IU=UTL_GETUNIT()
    OPEN(UNIT=IU,FILE=TRIM(PREFVAL(1))//'\TMP\CELLEN.TXT',ACTION='READ',FORM='FORMATTED')
    JU=UTL_GETUNIT()
    OPEN(UNIT=JU,FILE=TRIM(PREFVAL(1))//'\TMP\CELLENTMP.TXT',ACTION='WRITE',FORM='FORMATTED')
    READ(IU,*) N 
    WRITE(JU,*) N-1 
    DO
     READ(IU,'(A)',IOSTAT=IOS) LINE
     IF(IOS .NE. 0) EXIT    
     INDX=INDEX(TRIM(LINE),TRIM(CELLSTRING))
     IF(INDX .EQ. 0) THEN    
      WRITE(JU,'(A)') TRIM(LINE)
     END IF 
    END DO
    CLOSE(IU,STATUS='DELETE')
    CLOSE(JU)
    CALL SYSTEM('REN '//TRIM(PREFVAL(1))//'\TMP\CELLENTMP.TXT CELLEN.TXT')
    IF(N .EQ. 1) CALL SYSTEM('DEL '//TRIM(PREFVAL(1))//'\TMP\CELLEN.TXT')
    INDX=0
    
    !## update genfiles.txt
    IU=UTL_GETUNIT()
    OPEN(UNIT=IU,FILE=TRIM(PREFVAL(1))//'\TMP\GENFILES.TXT',ACTION='READ',FORM='FORMATTED')
    JU=UTL_GETUNIT()
    OPEN(UNIT=JU,FILE=TRIM(PREFVAL(1))//'\TMP\GENFILESTMP.TXT',ACTION='WRITE',FORM='FORMATTED')
    DO
     READ(IU,'(A)',IOSTAT=IOS) LINE
     IF(IOS .NE. 0) EXIT
     INDX=INDEX(TRIM(LINE),TRIM(CELLSTRING)//'.GEN')
     IF(INDX .EQ. 0) WRITE(JU,'(A)') TRIM(LINE)
    END DO
    CLOSE(IU,STATUS='DELETE')
    CLOSE(JU)
    CALL SYSTEM('REN '//TRIM(PREFVAL(1))//'\TMP\GENFILESTMP.TXT GENFILES.TXT')
    IF(N .EQ. 1) CALL SYSTEM('DEL '//TRIM(PREFVAL(1))//'\TMP\GENFILES.TXT')
    INDX=0   
    CALL SUBSURFEXDRAWMAP()
    
    ELSE !## if not selected yet, select the cell
    CALL IGRCOLOUR('GREEN')
    CALL IGRLINEWIDTH(3)
    CALL IGRRECTANGLE(J*1.0,I*1.0,J*1.0+GRIDSIZE,I*1.0+GRIDSIZE)
    !## create and add gen of the cell
    GENFILE=TRIM(PREFVAL(1))//'\TMP\'//TRIM(CELLSTRING)//'.GEN'    
    IU=UTL_GETUNIT()
    OPEN(UNIT=IU,FILE=GENFILE,ACTION='WRITE',FORM='FORMATTED')
    WRITE(IU,*) '1,SHAPE1'
    WRITE(IU,'(I0.1,1X,I0.1)') J,I
    WRITE(IU,'(I0.1,1X,I0.1)') J+INT(GRIDSIZE),I
    WRITE(IU,'(I0.1,1X,I0.1)') J+INT(GRIDSIZE),I+INT(GRIDSIZE)
    WRITE(IU,'(I0.1,1X,I0.1)') J,I+INT(GRIDSIZE)
    WRITE(IU,'(I0.1,1X,I0.1)') J,I
    WRITE(IU,*) 'END'
    WRITE(IU,*) 'END'
    CLOSE(IU)  
    COLOUR='GREEN'
    CALL SUBSURFEXADDGEN(GENFILE,COLOUR,3,0)    
    !## create or update cells.txt
    INQUIRE(FILE=TRIM(PREFVAL(1))//'\TMP\CELLEN.TXT',EXIST=EX)
    IF(EX .EQ. .TRUE.)THEN
     IU=UTL_GETUNIT()
     OPEN(UNIT=IU,FILE=TRIM(PREFVAL(1))//'\TMP\CELLEN.TXT',ACTION='READ',FORM='FORMATTED')
     READ(IU,*) N
     ALLOCATE(SELECTION(N))
     DO K=1,N
      READ(IU,*) SELECTION(K)
     END DO
     CLOSE(IU)
    ELSE
     N=1
    END IF 
    IU=UTL_GETUNIT()
    OPEN(UNIT=IU,FILE=TRIM(PREFVAL(1))//'\TMP\CELLEN.TXT',ACTION='WRITE',FORM='FORMATTED')
    
    !## make sure no duplicates are included    
    IF(EX .EQ. .TRUE.)THEN
     EXISTS=0
     DO K=1,N
      IF(CELL == SELECTION(K)) THEN
       EXISTS=1
       EXIT
      END IF 
     END DO    
    END IF 
    IF(EX .EQ. .TRUE. .AND. EXISTS .EQ. 0)THEN
     N=N+1
     WRITE(IU,*) N
     WRITE(IU,*) CELL    
     DO K=1,N-1
      WRITE(IU,*) SELECTION(K)
     END DO
    ELSEIF(EX .EQ. .TRUE. .AND. EXISTS .EQ. 1)THEN
     WRITE(IU,*) N    
     DO K=1,N
      WRITE(IU,*) SELECTION(K)
     END DO     
    ELSE
     WRITE(IU,*) N
     WRITE(IU,*) CELL
    END IF 

    CLOSE(IU)
    IF(ALLOCATED(SELECTION)) DEALLOCATE(SELECTION)
    EXIT
    END IF
   END IF
  END DO
 END DO  

 END SUBROUTINE SUBSURFEXSELECTCELL
  
 !###======================================================================
 SUBROUTINE SUBSURFEXOPEN()
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=256) :: FILENAME
 CHARACTER(LEN=16) :: COLOUR
 
 FILENAME=TRIM(PREFVAL(1))
 CALL WSELECTFILE('GEN file|*.GEN|',LOADDIALOG,FILENAME,'Load a polygon ...')
 IF(WINFODIALOG(EXITBUTTONCOMMON).EQ. COMMONOK)THEN
  COLOUR='BLUE'
  CALL SUBSURFEXADDGEN(FILENAME,COLOUR,2,0)  
  CALL SUBSURFEXGENDRAW(FILENAME,COLOUR,2,1)  
 END IF
 MOVE=.FALSE.
 CALL WCURSORSHAPE(CURARROW)

 END SUBROUTINE SUBSURFEXOPEN
 
 !###======================================================================
 SUBROUTINE SUBSURFEXGENDRAW(GENFILE,COLOUR,LINEWIDTH,FINDCELLS)
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: A,B,IU,JU,KU,IOS,I,J,K,N,NPOINTS,LINEWIDTH,FINDCELLS,COUNTER,OLDNROFCELLS,LOCALNROFCELLS,X1,X2,Y1,Y2,EXISTS,NROW,NCOL
 INTEGER,ALLOCATABLE,DIMENSION(:) :: CELL,OLDCELLS
 REAL,ALLOCATABLE,DIMENSION(:) :: POLYGONX,POLYGONY,POINTSX,POINTSY
 REAL :: FROMX,FROMY,TOX,TOY,X,Y,MAXVALUE
 CHARACTER(LEN=256) :: GENFILE,GENFILECELL !GENFILECELLS
 CHARACTER(LEN=16) :: COLOUR,COLOUR2,COUNTERSTRING
 LOGICAL :: EX
 
 CALL WCURSORSHAPE(CURHOURGLASS)
 
 !## plot the gen file's polygons
 IU=UTL_GETUNIT()
 OPEN(UNIT=IU,FILE=GENFILE,ACTION='READ',FORM='FORMATTED')
 DO
  !## define style
  CALL IGRCOLOUR(COLOUR)
  CALL IGRLINEWIDTH(LINEWIDTH)
  COUNTER=0
  READ(IU,'(A256)',IOSTAT=IOS)
  IF(IOS.NE.0)EXIT
  IF(FINDCELLS .EQ. 1)THEN
   JU=UTL_GETUNIT()
   OPEN(UNIT=JU,FILE=TRIM(PREFVAL(1))//'\TMP\POLYGON.TXT',ACTION='WRITE',FORM='FORMATTED')  
  END IF
  !## read first point
  READ(IU,*,IOSTAT=IOS) FROMX,FROMY
  DO
   READ(IU,*,IOSTAT=IOS) TOX,TOY
   IF(IOS.NE.0)EXIT
   IF((FROMX.GT.XMIN.OR.TOX.GT.XMIN).AND. &
      (FROMX.LT.XMAX.OR.TOX.LT.XMAX).AND. &
      (FROMY.GT.YMIN.OR.TOY.GT.YMIN).AND. &
      (FROMY.LT.YMAX.OR.TOY.LT.YMAX))THEN
    CALL IGRJOIN(FROMX,FROMY,TOX,TOY)
   ENDIF
   IF(FINDCELLS .EQ. 1)THEN   
    WRITE(JU,'(2F)') FROMX,FROMY
    COUNTER=COUNTER+1
   END IF 
   FROMX=TOX; FROMY=TOY
  END DO
  IF(FINDCELLS .EQ. 1) CLOSE(JU) 
  

  IF(FINDCELLS .EQ. 1) THEN !## find which cells the polygon contains
   !## read the polygons vertices
   ALLOCATE(POLYGONX(COUNTER),POLYGONY(COUNTER))
   JU=UTL_GETUNIT()
   OPEN(UNIT=JU,FILE=TRIM(PREFVAL(1))//'\TMP\POLYGON.TXT',ACTION='READ',FORM='FORMATTED')
   DO I=1,COUNTER
    READ(JU,*) POLYGONX(I),POLYGONY(I)
   END DO
   CLOSE(JU)
   
   !## define points  
   NROW=(INIYMAX-INIYMIN)/GRIDSIZE
   NCOL=(INIXMAX-INIXMIN)/GRIDSIZE
   NPOINTS=100*NROW*NCOL
   COUNTER=0  
   ALLOCATE(POINTSX(NPOINTS),POINTSY(NPOINTS))
   X=INIXMIN+0.5*(GRIDSIZE/10)
   Y=INIYMIN+0.5*(GRIDSIZE/10)
   DO I=1,NROW*10
    DO J=1,NCOL*10
     COUNTER=COUNTER+1
     POINTSX(COUNTER)=X ; POINTSY(COUNTER)=Y   
     X=X+GRIDSIZE/10.0
    END DO
    X=INIXMIN+0.5*(GRIDSIZE/10)
    Y=Y+GRIDSIZE/10.0     
   END DO
   
   !## remove points that are outside the polygon
   N=SIZE(POLYGONX)
   DO I=1,SIZE(POINTSX)
     IF(IGRINSIDEPOLYGON(POLYGONX,POLYGONY,N,POINTSX(I),POINTSY(I)))THEN
     
     ELSE
      POINTSX(I)=0.0; POINTSY(I)=0.0
     END IF
   END DO
    
   ALLOCATE(CELL(NCOL*NROW))
   DO I=1,NCOL*NROW
    CELL(I)=I
   END DO
   
   !## find the cells which correspond to the left over points
   MAXVALUE=MAXVAL(POINTSY)
   COUNTER=0
   LOCALNROFCELLS=0
   X1=INT(INIXMIN)
   X2=INT(INIXMAX)-INT(GRIDSIZE)
   Y1=INT(INIYMIN)
   Y2=INT(INIYMAX)-INT(GRIDSIZE)
   JU=UTL_GETUNIT()
   OPEN(UNIT=JU,FILE=TRIM(PREFVAL(1))//'\TMP\POLYGONCELLS.TXT',ACTION='WRITE',FORM='FORMATTED')   
   DO K=1,SIZE(POINTSX)
    DO I=Y1,Y2,INT(GRIDSIZE)
     DO J=X1,X2,INT(GRIDSIZE)
      IF(I .GT. MAXVALUE) EXIT
      COUNTER=COUNTER+1
      IF(POINTSX(K) .GT. J .AND. &
         POINTSX(K) .LT. J+INT(GRIDSIZE) .AND. & 
         POINTSY(K) .GT. I .AND. &
         POINTSY(K) .LT. I+INT(GRIDSIZE) .AND. & 
         CELL(COUNTER) .NE. 0) THEN
       CALL IGRCOLOUR('GREEN')
       CALL IGRLINEWIDTH(3)
       CALL IGRRECTANGLE(J*1.0,I*1.0,J*1.0+GRIDSIZE,I*1.0+GRIDSIZE)        
       WRITE(JU,*) CELL(COUNTER)
       CELL(COUNTER)=0       
       KU=UTL_GETUNIT()
       WRITE(COUNTERSTRING,'(I0.0)') COUNTER
       OPEN(UNIT=KU,FILE=TRIM(PREFVAL(1))//'\TMP\'//TRIM(COUNTERSTRING)//'.GEN',ACTION='WRITE',FORM='FORMATTED')
       WRITE(KU,*) '1,SHAPE1'
       WRITE(KU,'(I0.1,1X,I0.1)') J,I
       WRITE(KU,'(I0.1,1X,I0.1)') J+INT(GRIDSIZE),I
       WRITE(KU,'(I0.1,1X,I0.1)') J+INT(GRIDSIZE),I+INT(GRIDSIZE)
       WRITE(KU,'(I0.1,1X,I0.1)') J,I+INT(GRIDSIZE)
       WRITE(KU,'(I0.1,1X,I0.1)') J,I
       WRITE(KU,*) 'END'       
       WRITE(KU,*) 'END'
       CLOSE(KU)
       COLOUR2='GREEN'
       GENFILECELL=TRIM(PREFVAL(1))//'\TMP\'//TRIM(COUNTERSTRING)//'.GEN'
       CALL SUBSURFEXADDGEN(GENFILECELL,COLOUR2,3,0)       
       LOCALNROFCELLS=LOCALNROFCELLS+1       
      END IF    
     END DO
    END DO
    COUNTER=0
   END DO   
   CLOSE(JU)   
   
   !## read the selected cells
   DEALLOCATE(CELL); ALLOCATE(CELL(LOCALNROFCELLS))
   JU=UTL_GETUNIT()
   OPEN(UNIT=JU,FILE=TRIM(PREFVAL(1))//'\TMP\POLYGONCELLS.TXT',ACTION='READ',FORM='FORMATTED')
   DO I=1,LOCALNROFCELLS
    READ(JU,*) CELL(I)
   END DO
   CLOSE(JU)
   CALL SYSTEM('DEL "'//TRIM(PREFVAL(1))//'\TMP\POLYGONCELLS.TXT"')
   
   !## create/update cells.txt   
   OLDNROFCELLS=0
   INQUIRE(FILE=TRIM(PREFVAL(1))//'\TMP\CELLEN.TXT',EXIST=EX)
   IF(EX .EQ. .TRUE.)THEN
    JU=UTL_GETUNIT()
    OPEN(UNIT=JU,FILE=TRIM(PREFVAL(1))//'\TMP\CELLEN.TXT',ACTION='READ',FORM='FORMATTED')
    READ(JU,*) OLDNROFCELLS
    ALLOCATE(OLDCELLS(OLDNROFCELLS))
    DO I=1,OLDNROFCELLS
     READ(JU,*) OLDCELLS(I)
    END DO
    CLOSE(JU)
   END IF
   JU=UTL_GETUNIT()
   OPEN(UNIT=JU,FILE=TRIM(PREFVAL(1))//'\TMP\CELLEN.TXT',ACTION='WRITE',FORM='FORMATTED')
   !## make sure no duplicates are included    
    EXISTS=0
    DO I=1,LOCALNROFCELLS
     DO J=1,OLDNROFCELLS
      IF(CELL(I) .EQ. OLDCELLS(J)) THEN
       EXISTS=1
      END IF 
     END DO
     IF(EXISTS .EQ. 1) THEN   
      LOCALNROFCELLS=LOCALNROFCELLS-1       
      CELL(I)=0
     END IF
     EXISTS=0
    END DO
    WRITE(JU,*) OLDNROFCELLS+LOCALNROFCELLS
    DO I=1,SIZE(CELL)
     IF(CELL(I) .NE. 0) WRITE(JU,*) CELL(I)
    END DO
    DO I=1,SIZE(OLDCELLS)
     WRITE(JU,*) OLDCELLS(I) 
    END DO
    CLOSE(JU)
      
  IF(ALLOCATED(OLDCELLS)) DEALLOCATE(OLDCELLS)
  DEALLOCATE(POLYGONX,POLYGONY,POINTSX,POINTSY,CELL)
 END IF
  
 END DO 
 CLOSE(IU);
 
 IF(FINDCELLS .EQ. 1) CALL SYSTEM('DEL "'//TRIM(PREFVAL(1))//'\TMP\POLYGON.TXT"')
 IF(MOVE .EQ. .TRUE.)THEN
  CALL WCURSORSHAPE(ID_CURSORHAND) 
 ELSEIF(DRAW .EQ. .TRUE.)THEN
  CALL WCURSORSHAPE(ID_CURSORPOINTPLUS)
 ELSE 
  CALL WCURSORSHAPE(CURARROW)
 END IF
 END SUBROUTINE SUBSURFEXGENDRAW
 
 !###======================================================================
 SUBROUTINE SUBSURFEXPOSITION(X,Y)
 !###======================================================================
 IMPLICIT NONE
 REAL :: X,Y
 CHARACTER(LEN=56) :: XSTRING,YSTRING
  
 WRITE(XSTRING,'(I0.0)') INT(X)
 WRITE(YSTRING,'(I0.0)') INT(Y)
 CALL WDIALOGPUTSTRING(IDF_LABEL9,'X: '//TRIM(XSTRING)//'   Y: '//TRIM(YSTRING))

 END SUBROUTINE SUBSURFEXPOSITION
 
 !###======================================================================
 SUBROUTINE SUBSURFEXMOVE(NEWX,NEWY)
 !###======================================================================
 IMPLICIT NONE
 REAL :: NEWX,NEWY,XDIFF,YDIFF,X1,X2,Y1,Y2
 
 X1=WINFOGRREAL(11)
 Y1=WINFOGRREAL(12)
 X2=WINFOGRREAL(13)
 Y2=WINFOGRREAL(14) 
 
 IF(NEWX .GT. OLDX)THEN
  XDIFF=NEWX-OLDX
  XMIN=X1-XDIFF
  XMAX=X2-XDIFF    
 ELSEIF(NEWX .LT. OLDX)THEN
  XDIFF=OLDX-NEWX
  XMIN=X1+XDIFF
  XMAX=X2+XDIFF 
 END IF
 IF(NEWY .GT. OLDY)THEN
  YDIFF=NEWY-OLDY
  YMIN=Y1-YDIFF
  YMAX=Y2-YDIFF  
 ELSEIF(NEWY .LT. OLDY)THEN
  YDIFF=OLDY-NEWY
  YMIN=Y1+YDIFF
  YMAX=Y2+YDIFF  
 END IF

 CALL SUBSURFEXDRAWMAP()
  
 END SUBROUTINE SUBSURFEXMOVE
 
!###======================================================================
 SUBROUTINE SUBSURFEXMOVEBUTTON()
 !###======================================================================
 IMPLICIT NONE
 
 IF(MOVE .EQ. .FALSE.) THEN
  MOVE=.TRUE.
  DRAW=.FALSE.
  CALL WCURSORSHAPE(ID_CURSORHAND)  
 ELSE 
  MOVE=.FALSE.
  CALL WCURSORSHAPE(CURARROW)  
 END IF 
  
 END SUBROUTINE SUBSURFEXMOVEBUTTON

!###======================================================================
 SUBROUTINE SUBSURFEXRESET()
 !###======================================================================
 IMPLICIT NONE
 
 !## reset the coordinate system to the original view and redraw the map
 XMIN=INIXMIN
 YMIN=INIYMIN
 XMAX=INIXMAX
 YMAX=INIYMAX
 CALL SUBSURFEXDRAWMAP() 
  
 END SUBROUTINE SUBSURFEXRESET
 
!###======================================================================
 SUBROUTINE SUBSURFEXADDGEN(GENFILE,COLOUR,LINEWIDTH,FINDCELLS)
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,IU,N,LINEWIDTH,FINDCELLS,IOS,INDX
 INTEGER,ALLOCATABLE,DIMENSION(:) :: LINEWIDTHS,FINDCELLSARRAY
 LOGICAL :: EX
 CHARACTER(LEN=256) :: LINE,GENFILE
 CHARACTER(LEN=16) :: COLOUR
 CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: GENFILES
 CHARACTER(LEN=56),ALLOCATABLE,DIMENSION(:) :: COLOURS
 
 INQUIRE(FILE=TRIM(PREFVAL(1))//'\TMP\GENFILES.TXT',EXIST=EX)
 IF(EX .EQ. .TRUE.) THEN
  
  !##  check whether GENFILE is already present in genfiles.txt
  IU=UTL_GETUNIT()
  OPEN(UNIT=IU,FILE=TRIM(PREFVAL(1))//'\TMP\GENFILES.TXT',ACTION='READ',FORM='FORMATTED')
  DO 
   READ(IU,'(A)',IOSTAT=IOS) LINE
   IF(IOS .NE. 0) EXIT
   INDX=INDEX(TRIM(LINE),TRIM(GENFILE))
   IF(INDX .NE. 0) THEN
    CLOSE(IU)
    RETURN   
   END IF 
  END DO
  IF(INDX .EQ. 0) CLOSE(IU)
  
  !## update genfiles.txt
  N=0
  IU=UTL_GETUNIT()
  OPEN(UNIT=IU,FILE=TRIM(PREFVAL(1))//'\TMP\GENFILES.TXT',ACTION='READ',FORM='FORMATTED')  
  DO
   READ(IU,'(A)',IOSTAT=IOS) LINE
   IF(IOS .NE. 0) EXIT
   N=N+1
  END DO
  CLOSE(IU)
  ALLOCATE(GENFILES(N),COLOURS(N),LINEWIDTHS(N),FINDCELLSARRAY(N))
  IU=UTL_GETUNIT()
  OPEN(UNIT=IU,FILE=TRIM(PREFVAL(1))//'\TMP\GENFILES.TXT',ACTION='READ',FORM='FORMATTED')
  DO I=1,N
   READ(IU,*) GENFILES(I),COLOURS(I),LINEWIDTHS(I),FINDCELLSARRAY(I)
  END DO
  CLOSE(IU,STATUS='DELETE')  
  IU=UTL_GETUNIT()
  OPEN(UNIT=IU,FILE=TRIM(PREFVAL(1))//'\TMP\GENFILES.TXT',ACTION='WRITE',FORM='FORMATTED')  
  DO I=1,N
   WRITE(IU,'(A,1X,A,1X,I0.1,1X,I0.1)') TRIM(GENFILES(I)),TRIM(COLOURS(I)),LINEWIDTHS(I),FINDCELLSARRAY(I)
  END DO
  WRITE(IU,'(A,1X,A,1X,I0.1,1X,I0.1)') TRIM(GENFILE),TRIM(COLOUR),LINEWIDTH,FINDCELLS
  CLOSE(IU)
  DEALLOCATE(GENFILES,COLOURS,LINEWIDTHS,FINDCELLSARRAY)
 ELSE
  IU=UTL_GETUNIT()
  OPEN(UNIT=IU,FILE=TRIM(PREFVAL(1))//'\TMP\GENFILES.TXT',ACTION='WRITE',FORM='FORMATTED')  
  WRITE(IU,'(A,1X,A,1X,I0.1,1X,I0.1)') TRIM(GENFILE),TRIM(COLOUR),LINEWIDTH,FINDCELLS
  CLOSE(IU)  
 END IF
 
 END SUBROUTINE SUBSURFEXADDGEN
 
 !###======================================================================
 SUBROUTINE SUBSURFEX_DRAWBUTTON()
 !###======================================================================
 IMPLICIT NONE
 
 IF(DRAW .EQ. .FALSE.) THEN
  DRAW=.TRUE.
  MOVE=.FALSE.
  CALL WCURSORSHAPE(ID_CURSORPOINTPLUS)  
 ELSE 
  DRAW=.FALSE.
  CALL WCURSORSHAPE(CURARROW)  
 END IF 
 
 END SUBROUTINE SUBSURFEX_DRAWBUTTON 
 
 !###======================================================================
 SUBROUTINE SUBSURFEXDRAWPOLYGON(FROMX,FROMY,TOX,TOY,BUTTON)
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IU,JU,BUTTON,IOS
 REAL :: FROMX,FROMY,TOX,TOY
 CHARACTER(LEN=256) :: GENFILE,LINE
 CHARACTER(LEN=16) :: COLOUR
 CHARACTER(LEN=8) :: POLYGONNRSTRING
 LOGICAL :: EX
 
 WRITE(POLYGONNRSTRING,'(I0.1)') POLYGONNR
 GENFILE=TRIM(PREFVAL(1))//'\TMP\SEPOLYGON'//TRIM(POLYGONNRSTRING)//'.GEN'
 INQUIRE(FILE=GENFILE,EXIST=EX) 
 
  !## store first x,y coordinates
  IF(CLICKS .EQ. 2) THEN
   FIRSTX=FROMX; FIRSTY=FROMY
  END IF
 
  !## draw line between mouseclicks  
  COLOUR='BLUE'
  CALL IGRLINEWIDTH(2)
  CALL IGRCOLOUR(COLOUR)
  CALL IGRJOIN(FROMX,FROMY,TOX,TOY)
 
  !## update gen file
  IF(EX .EQ. .FALSE.)THEN
   IU=UTL_GETUNIT()
   OPEN(UNIT=IU,FILE=GENFILE,ACTION='WRITE',FORM='FORMATTED')
   WRITE(IU,*) '1,SHAPE1'
   WRITE(IU,'(F10.3,1X,F10.3)') FROMX,FROMY
   WRITE(IU,'(F10.3,1X,F10.3)') TOX,TOY
   CLOSE(IU)
  ELSE
   IU=UTL_GETUNIT()
   OPEN(UNIT=IU,FILE=GENFILE,ACTION='READ',FORM='FORMATTED')
   JU=UTL_GETUNIT()
   OPEN(UNIT=JU,FILE=TRIM(PREFVAL(1))//'\TMP\TMP.GEN',ACTION='WRITE',FORM='FORMATTED')
   DO
    READ(IU,'(A)',IOSTAT=IOS) LINE
    IF(IOS .NE. 0) EXIT
    WRITE(JU,'(A)') TRIM(LINE)
   END DO  
   CLOSE(IU,STATUS='DELETE') 
   IF(BUTTON .EQ. 1)THEN
    WRITE(JU,'(F10.3,1X,F10.3)') TOX,TOY  
   ELSEIF(BUTTON .EQ. 3)THEN
    WRITE(JU,'(F10.3,1X,F10.3)') FIRSTX,FIRSTY
    WRITE(JU,*) 'END'
    WRITE(JU,*) 'END'   
   END IF
   CLOSE(JU)
   CALL SYSTEM('"REN "'//TRIM(PREFVAL(1))//'\TMP\TMP.GEN" "SEPOLYGON'//TRIM(POLYGONNRSTRING)//'.GEN""')
  END IF
    
 !## plot gen file and find corresponding cells    
  IF(BUTTON .EQ. 3)THEN
   CALL IGRJOIN(FIRSTX,FIRSTY,TOX,TOY)  
   CALL SUBSURFEXADDGEN(GENFILE,COLOUR,2,0)
   CALL SUBSURFEXGENDRAW(GENFILE,COLOUR,2,1)
   POLYGONNR=POLYGONNR+1
  END IF
 
 END SUBROUTINE SUBSURFEXDRAWPOLYGON
 
 !###======================================================================
 SUBROUTINE SUBSURFEX_DRAWPROVINCES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,IU,N,IOS
 REAL :: FROMX,FROMY,TOX,TOY
 
 !## if the map is not in the memory yet, read it into the memory
 !## first determine the size of the array (nr of vertices) N
 IF(.NOT. ALLOCATED(VERTICESX)) THEN   
 N=0
 IU=UTL_GETUNIT()
 OPEN(UNIT=IU,FILE=TRIM(PREFVAL(1))//'\TMP\PROVINCES.GEN',ACTION='READ',FORM='FORMATTED')
 DO
  READ(IU,'(A256)',IOSTAT=IOS)
  IF(IOS.NE.0)EXIT
  !## read first point
  READ(IU,*,IOSTAT=IOS) FROMX,FROMY
  N=N+1
  DO
   READ(IU,*,IOSTAT=IOS) TOX,TOY
   IF(IOS.NE.0)EXIT
   IF((FROMX.GT.MPW%XMIN.OR.TOX.GT.MPW%XMIN).AND. &
      (FROMX.LT.MPW%XMAX.OR.TOX.LT.MPW%XMAX).AND. &
      (FROMY.GT.MPW%YMIN.OR.TOY.GT.MPW%YMIN).AND. &
      (FROMY.LT.MPW%YMAX.OR.TOY.LT.MPW%YMAX))THEN
   ENDIF
   N=N+1   
   FROMX=TOX; FROMY=TOY
  END DO
  N=N+1
 END DO
 CLOSE(IU)
 
 !## allocate vertices arrays and read the vertices
 ALLOCATE(VERTICESX(N),VERTICESY(N))
 N=0
 IU=UTL_GETUNIT()
 OPEN(UNIT=IU,FILE=TRIM(PREFVAL(1))//'\TMP\PROVINCES.GEN',ACTION='READ',FORM='FORMATTED')
 DO
  READ(IU,'(A256)',IOSTAT=IOS)
  IF(IOS.NE.0)EXIT
  !## read first point
  READ(IU,*,IOSTAT=IOS) FROMX,FROMY
  N=N+1
  VERTICESX(N)=FROMX
  VERTICESY(N)=FROMY
  DO
   READ(IU,*,IOSTAT=IOS) TOX,TOY
   IF(IOS.NE.0)EXIT
   IF((FROMX.GT.MPW%XMIN.OR.TOX.GT.MPW%XMIN).AND. &
      (FROMX.LT.MPW%XMAX.OR.TOX.LT.MPW%XMAX).AND. &
      (FROMY.GT.MPW%YMIN.OR.TOY.GT.MPW%YMIN).AND. &
      (FROMY.LT.MPW%YMAX.OR.TOY.LT.MPW%YMAX))THEN
   ENDIF
   N=N+1
   VERTICESX(N)=TOX
   VERTICESY(N)=TOY
   FROMX=TOX; FROMY=TOY   
  END DO
  N=N+1
  VERTICESX(N)=99999.00
  VERTICESY(N)=99999.00  
 END DO
 CLOSE(IU)
 END IF
 
 !## draw the map
 CALL IGRCOLOUR('RED')
 CALL IGRLINEWIDTH(2) 
 DO I=1,SIZE(VERTICESX)-1
  IF(VERTICESX(I) .EQ. 99999.00 .OR. VERTICESX(I+1) .EQ. 99999.00) CYCLE
  CALL IGRJOIN(VERTICESX(I),VERTICESY(I),VERTICESX(I+1),VERTICESY(I+1))
 END DO
 
 END SUBROUTINE SUBSURFEX_DRAWPROVINCES

END MODULE MOD_SUBSURFEX