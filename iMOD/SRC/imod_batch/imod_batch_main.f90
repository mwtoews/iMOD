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
MODULE MOD_BATCH_MAIN

USE WINTERACTER
USE RESOURCE
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_UTL
USE MOD_BATCH_PAR

TYPE(PROCOBJ),DIMENSION(:),POINTER,PRIVATE :: PROC
CHARACTER(LEN=3*256),PRIVATE :: LINE

CONTAINS

 !###======================================================================
 SUBROUTINE CREATEIMODBATCHMAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE
 CHARACTER(LEN=256) :: FNAME
 
 CALL WDIALOGSELECT(MESSAGE%WIN)

 SELECT CASE(ITYPE)

  CASE (MENUSELECT)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
  CASE(FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE2)
   END SELECT
  CASE(PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (IDOK)
     CALL CREATEIMODBATCHCREATE()
    CASE (ID_KILL)
!     CALL CREATEIMODBATCHKILL()
    CASE (ID_INFO)
     FNAME=''
     CALL CREATEIMODBATCHINFO(FNAME)
    CASE (ID_REFRESH)
     CALL CREATEIMODBATCHREFRESH()
    CASE (ID_EXECUTE)
     CALL CREATEIMODBATCHEXECUTE()
    CASE (IDHELP)
     CALL UTL_GETHELP('3.2.6','EMO.iMODBatch')
    CASE (IDCANCEL)
     CALL CREATEIMODBATCHCLOSE()
   END SELECT

 END SELECT

 END SUBROUTINE CREATEIMODBATCHMAIN

 !###======================================================================
 SUBROUTINE CREATEIMODBATCHINFO(FNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IWIN,IU,I
 CHARACTER(LEN=256),INTENT(INOUT) :: FNAME
  
 CALL WDIALOGSELECT(ID_DBATCH)
 IF(TRIM(FNAME).EQ.'')THEN
  CALL WDIALOGGETMENU(IDF_MENU2,I,FNAME)
  FNAME=TRIM(PREFVAL(1))//'\IMODBATCH\'//TRIM(FNAME)
 ENDIF
 
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',IOSTAT=I)
 IF(I.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot view the created file : '//CHAR(13)// &
   TRIM(FNAME)//'.'//CHAR(13)//'It is probably opened already in another application','Error')
 ELSE
  CLOSE(IU)
  CALL WINDOWOPENCHILD(IWIN,FLAGS=SYSMENUON+MAXWINDOW,WIDTH=1000,HEIGHT=500)
  CALL WINDOWSELECT(IWIN)
  CALL WEDITFILE(FNAME,ITYPE=MODAL,IDMENU=0, &
                 IFLAGS=NOTOOLBAR+WORDWRAP+NOFILENEWOPEN,&
                 IFONT=4,ISIZE=10)
 ENDIF
 
 END SUBROUTINE CREATEIMODBATCHINFO

 !###======================================================================
 SUBROUTINE CREATEIMODBATCHREFRESH()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 I=UTL_GETIDPROC(PROC,1)
 IF(I.EQ.0)THEN
  CALL WDIALOGFIELDSTATE(ID_KILL,0) 
  CALL WDIALOGCLEARFIELD(IDF_MENU3); CALL WDIALOGPUTMENU(IDF_MENU3,(/''/),1,1)
  CALL WDIALOGFIELDSTATE(IDF_MENU3,0)
  CALL WDIALOGPUTSTRING(IDF_LABEL1,'No processes active')
 ELSE
  CALL WDIALOGFIELDSTATE(ID_KILL,1)
  CALL WDIALOGPUTMENU(IDF_MENU3,PROC%CID,SIZE(PROC),I)
  CALL WDIALOGPUTSTRING(IDF_LABEL1,'Current processes')
 ENDIF
 
 END SUBROUTINE CREATEIMODBATCHREFRESH

 !###======================================================================
 SUBROUTINE CREATEIMODBATCHCREATE()
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=256) :: FNAME,INIFNAME
 INTEGER :: IU,I,J,K,II,I1,I2
 CHARACTER(LEN=12) :: CN
 LOGICAL :: LEX
 
 FNAME=TRIM(PREFVAL(1))//'\IMODBATCH\*.bat'
 IF(.NOT.IMODBATCH_SAVE(FNAME))RETURN
 !IF(.NOT.UTL_WSELECTFILE('Batch Files (*.bat)|',&
 !                  SAVEDIALOG+PROMPTON+APPENDEXT,FNAME,&
 !                  'Save Selected Function to Batchfile (*.bat)'))RETURN

 INQUIRE(FILE=FNAME,EXIST=LEX)
 IF(LEX)THEN
  J=INDEX(FNAME,'\',.TRUE.)
  CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Do you want to overwrite the existing file'//CHAR(13)//TRIM(FNAME(J+1:))//'?','Question')
  IF(WINFODIALOG(4).NE.1)RETURN
 ENDIF
 
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=I)
 IF(I.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot create the file:'//CHAR(13)//TRIM(FNAME),'Error')
  RETURN
 ENDIF

 CALL WDIALOGSELECT(ID_DBATCH)
 CALL WDIALOGGETMENU(IDF_MENU1,J,INIFNAME)
 INIFNAME=TRIM(UTL_CAP(INIFNAME,'L'))//'.ini'
 
 WRITE(IU,'(A/)') 'rem iMOD Batch generated by '//TRIM(UTL_IMODVERSION())
 WRITE(IU,'(A)') 'rem FUNCTION'
 LINE='echo FUNCTION='//TRIM(BAT(J)%CFUNC); LINE(52:)='>  '//TRIM(INIFNAME)
 WRITE(IU,'(A)') TRIM(LINE)
 K=0
 DO I=1,BAT(J)%N
  LINE='echo'
  SELECT CASE (BAT(J)%LVL(I))
   CASE ('(COMP)')
    K=0; II=6
   CASE ('(OPT)')
    II=6
    IF(K.EQ.0)THEN 
      WRITE(IU,'(A)') 'rem ------------------'; K=1
      WRITE(IU,'(A)') 'rem OPTIONAL ARGUMENTS'; K=1
    ENDIF  
   CASE ('(DEP)')
    II=8
   CASE ('(DEP2)')
    II=10
  END SELECT
  IF(INDEX(BAT(J)%KEY(I),'{').GT.0)THEN
   I1=INDEX(BAT(J)%KEY(I),',',.TRUE.)+1
   I2=INDEX(BAT(J)%KEY(I),'}',.TRUE.)-1
   READ(BAT(J)%KEY(I)(I1:I2),*) CN
   LINE=''; LINE(II:)='for /l %%a in (1,1,{ value of '//TRIM(CN)//'} ) do (' 
   WRITE(IU,'(A)') TRIM(LINE)
   I2=INDEX(BAT(J)%KEY(I),'{',.TRUE.)-1
   LINE='echo'; LINE(II:)=BAT(J)%KEY(I)(:I2)//'%%a= ...'
   LINE(52:)='>> '//TRIM(INIFNAME)
   WRITE(IU,'(A)') TRIM(LINE)
   LINE=''; LINE(II:)=')'; WRITE(IU,'(A)') TRIM(LINE)
  ELSE
   LINE(II:)=TRIM(BAT(J)%KEY(I))//'= ...'
   LINE(52:)='>> '//TRIM(INIFNAME)
   WRITE(IU,'(A)') TRIM(LINE) 
  ENDIF
 ENDDO 
 WRITE(IU,'(A)') 
 WRITE(IU,'(A)') '"'//TRIM(EXENAME)//'" '//TRIM(INIFNAME)
 WRITE(IU,'(A)') 

 CLOSE(IU)
 CALL CREATEIMODBATCH_UPDATE()
 CALL CREATEIMODBATCHINFO(FNAME)
 
 END SUBROUTINE CREATEIMODBATCHCREATE
 
 !###======================================================================
 SUBROUTINE CREATEIMODBATCHEXECUTE()
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=52) :: FNAME
 INTEGER :: IFLAGS,I
 INTEGER,DIMENSION(2) :: PID
 
 CALL WDIALOGSELECT(ID_DBATCH)
 CALL WDIALOGGETMENU(IDF_MENU2,I,FNAME)
 IFLAGS=0
 !## hidden window
 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I)
 IF(I.EQ.1)IFLAGS=IFLAGS+PROCSILENT
 !## I=0: show execution window
 !## I=1: hide execution window
 !## block execution
 CALL WDIALOGGETCHECKBOX(IDF_CHECK2,I)
 !## I=0: do not block execution till complete
 !## I=1: block execution till complete
 IF(I.EQ.1)IFLAGS=IFLAGS+PROCBLOCKED
 !## executes on commandtool such that commands alike 'dir' etc. works

 IFLAGS=IFLAGS+PROCCMDPROC

 I=UTL_GETIDPROC(PROC,0)

 CALL IOSCOMMAND(TRIM(PREFVAL(1))//'\IMODBATCH\'//TRIM(FNAME),IFLAGS,0,PID)

 PROC(I)%ID=PID(1)
 PROC(I)%CID=TRIM(FNAME)
 
 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,PROC(I)%IFLAGS(1))
 CALL WDIALOGGETCHECKBOX(IDF_CHECK2,PROC(I)%IFLAGS(2))
 CALL WDIALOGPUTMENU(IDF_MENU3,PROC%CID,SIZE(PROC),I)

 END SUBROUTINE CREATEIMODBATCHEXECUTE

 !###======================================================================
 SUBROUTINE CREATEIMODBATCHINIT()
 !###======================================================================
 IMPLICIT NONE
 
 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_IMODBATCH,2).EQ.1)THEN
  CALL CREATEIMODBATCHCLOSE(); RETURN
 ENDIF

 CALL UTL_CREATEDIR(TRIM(PREFVAL(1))//'\IMODBATCH')
 
 CALL WMENUSETSTATE(ID_IMODBATCH,2,1)
 CALL WDIALOGLOAD(ID_DBATCH,ID_DBATCH)
 CALL BATCHINIT()
 CALL CREATEIMODBATCH_UPDATE()
 CALL CREATEIMODBATCHREFRESH()
 
 CALL WDIALOGSELECT(ID_DBATCH)
 CALL WDIALOGPUTIMAGE(ID_REFRESH,ID_ICONREDRAW,1)
 CALL WDIALOGPUTIMAGE(ID_INFO,ID_ICONINFO,1)
   
 CALL WDIALOGSHOW(-0,100,0,2)

 END SUBROUTINE CREATEIMODBATCHINIT

 !###======================================================================
 LOGICAL FUNCTION IMODBATCH_SAVE(FNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,N
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE
 CHARACTER(LEN=256),INTENT(INOUT) :: FNAME

 IMODBATCH_SAVE=.FALSE.

 CALL WDIALOGLOAD(ID_DSCENNAME,ID_DSCENNAME)
 CALL UTL_IMODFILLMENU(IDF_MENU1,TRIM(PREFVAL(1))//'\IMODBATCH\','*.bat','F',N,0,0)
 CALL WDIALOGFIELDSTATE(IDF_MENU1,1)
 CALL WDIALOGTITLE('Save iMOD Batch file')
 CALL WDIALOGPUTSTRING(IDOK,'Save and Open file')

 CALL WDIALOGSHOW(-1,-1,0,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDOK)
      CALL WDIALOGGETMENU(IDF_MENU1,I,FNAME)
      IF(LEN_TRIM(FNAME).NE.0)THEN
       I=INDEX(FNAME,'.',.TRUE.)
       IF(I.EQ.0)THEN
        FNAME=TRIM(FNAME)//'.bat'
       ELSE
        FNAME=TRIM(FNAME(:I-1))//'.bat'
       ENDIF
       EXIT
      ENDIF
     CASE (IDHELP)
       !CALL UTL_GETHELP('5.10','TMO.DefStartP')
     CASE (IDCANCEL)
      EXIT
    END SELECT
  END SELECT
 ENDDO

 CALL WDIALOGUNLOAD()
 IF(MESSAGE%VALUE1.EQ.IDCANCEL)RETURN

 !## get folder name
 FNAME=TRIM(PREFVAL(1))//'\IMODBATCH\'//TRIM(FNAME)
 I      =INDEXNOCASE(FNAME,'\',.TRUE.)-1

 IMODBATCH_SAVE=.TRUE.

 END FUNCTION IMODBATCH_SAVE
 
 !###======================================================================
 SUBROUTINE CREATEIMODBATCH_UPDATE()
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=52),POINTER,DIMENSION(:) :: INIFNAME

 !## get available batchfiles (*.ini)
 IF(.NOT.UTL_DIRINFO_POINTER(TRIM(PREFVAL(1))//'\IMODBATCH','*.bat',INIFNAME,'F'))STOP
 IF(SIZE(INIFNAME).EQ.0)THEN
  CALL WDIALOGFIELDSTATE(IDF_MENU2,0); CALL WDIALOGFIELDSTATE(ID_EXECUTE,0); CALL WDIALOGCLEARFIELD(IDF_MENU2)
  CALL WDIALOGFIELDSTATE(ID_INFO,0)
 ELSE
  CALL WDIALOGFIELDSTATE(IDF_MENU2,1); CALL WDIALOGFIELDSTATE(ID_EXECUTE,1); CALL WDIALOGFIELDSTATE(ID_INFO,1)
  CALL WDIALOGPUTMENU(IDF_MENU2,INIFNAME,SIZE(INIFNAME),1)
 ENDIF
 
 END SUBROUTINE CREATEIMODBATCH_UPDATE
 
 !###======================================================================
 SUBROUTINE CREATEIMODBATCHCLOSE()
 !###======================================================================
 IMPLICIT NONE

 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_IMODBATCH,2,0)
 CALL WDIALOGSELECT(ID_DBATCH); CALL WDIALOGUNLOAD()

 END SUBROUTINE CREATEIMODBATCHCLOSE

END MODULE MOD_BATCH_MAIN
