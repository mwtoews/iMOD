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
MODULE MOD_SCEN

USE WINTERACTER
USE RESOURCE
USE MOD_COLOURS
USE MODPLOT
USE MOD_POLYGON_PAR
USE MOD_POLYGON_UTL, ONLY : POLYGON1INIT,POLYGON1CLOSE,POLYGON1IMAGES
USE MOD_POLYGON, ONLY : POLYGON1MAIN,POLYGON1FIELDS
USE MOD_POLYGON_DRAW, ONLY : POLYGON1DRAWSHAPE
USE MOD_SCEN_PAR
USE MOD_SCEN_FIELDS
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_UTL, ONLY : ITOS,RTOS,UTL_GETUNIT,UTL_CREATEDIR,UTL_IMODFILLMENU
USE MOD_OSD, ONLY : OSD_GETENV
USE IMODVAR, ONLY : BVERSION,IDIAGERROR
USE DATEVAR
USE MOD_OSD, ONLY : OSD_OPEN

CONTAINS

 !###======================================================================
 SUBROUTINE SCEN1MAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER :: I
 CHARACTER(LEN=256) :: FNAME

 CALL WDIALOGSELECT(MESSAGE%WIN)
 SELECT CASE (MESSAGE%WIN)

  !## main scenario
  CASE (ID_DSCENARIO)
   SELECT CASE (ITYPE)

    CASE (TABCHANGED)
     SELECT CASE (MESSAGE%VALUE2)
      CASE (ID_DSCEN1)
      CASE (ID_DSCEN2)
       CALL SCEN1FIELDS2()
!       CALL SCEN1FIELDS(ID_DSCENARIO,ID_DSCEN1,ID_DSCEN2)
!       CALL SCEN1ADDDEL(0,'')
     END SELECT

    CASE (FIELDCHANGED)

    CASE (PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (IDCANCEL)
       CALL SCEN1CLOSE(1)
      CASE (IDHELP)
       CALL IMODGETHELP('5.6','TMO.ModScen')         
     END SELECT
   END SELECT

  !## polygons
  CASE (ID_DSCEN1)

   IACTSHAPES=(/3,3,1,3,3,3/)
   CALL POLYGON1MAIN(ITYPE,MESSAGE)
   CALL SCEN1FIELDS1()

   SELECT CASE (ITYPE)
    CASE (FIELDCHANGED)
     SELECT CASE (MESSAGE%VALUE2)
     END SELECT
    CASE (PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
     END SELECT
   END SELECT

  !## scenario definitions
  CASE (ID_DSCEN2)

   SELECT CASE (ITYPE)
    CASE (FIELDCHANGED)
     SELECT CASE (MESSAGE%VALUE2)
      CASE (IDF_MENU1)
       CALL SCEN1FIELDS2()
     END SELECT

    CASE (PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (ID_INFO)
       CALL WDIALOGGETMENU(IDF_MENU1,I,FNAME)
       CALL SCEN1INFO(FNAME)
      CASE (ID_OPEN)
       CALL SCEN1ADDDEL(1,'')
      CASE (ID_DELETE)
       CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONOK,'Are you sure to remove the selected Definition from the List?','Question')
       IF(WINFODIALOG(4).EQ.1)CALL SCEN1ADDDEL(2,'')
      CASE (ID_NEW)
       CALL SCEN1NEW()
     END SELECT

   END SELECT

 END SELECT

 END SUBROUTINE SCEN1MAIN

 !###======================================================================
 SUBROUTINE SCEN1INFO(FNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: IWIN,IFLAGS

 CALL WINDOWOPENCHILD(IWIN,FLAGS=SYSMENUON,WIDTH=750,HEIGHT=500)
 CALL WINDOWSELECT(IWIN)
 IFLAGS=0
 CALL WEDITFILE(FNAME,ITYPE=MODAL,IFONT=COURIERNEW,ISIZE=10,IDMENU=0,IFLAGS=IFLAGS)

 END SUBROUTINE SCEN1INFO

 !###======================================================================
 SUBROUTINE SCEN1NEW()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IY,IM,ID,IU,I,J
 CHARACTER(LEN=256) :: FNAME,LINE

 FNAME=TRIM(SCNDIR)//'\*.sdf'
 CALL WSELECTFILE('iMOD Scenario Definition File (*.sdf)|*.sdf|',&
       SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,'Save iMOD Scenario Definition File')
 IF(WINFODIALOG(4).NE.1)RETURN

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='REPLACE',FORM='FORMATTED')
 LINE='iMOD Scenario Definition File '//TRIM(BVERSION)
 WRITE(IU,'(A)') TRIM(LINE)
 WRITE(IU,*)
 WRITE(IU,'(A)') 'CREATOR: '//TRIM(OSD_GETENV('USERNAME'))
 CALL IOSDATE(IY,IM,ID)
 LINE='DATE: '//TRIM(ITOS(ID))//'-'//TRIM(CDATE(IM))//'-'//TRIM(ITOS(IY))
 WRITE(IU,'(A)') TRIM(LINE)
 WRITE(IU,*)
 DO I=1,MXSCEN
  WRITE(IU,'(54A1)') ('=',J=1,54)
  WRITE(IU,'(A)') TRIM(SCEN(I)%SCENDEF)//': <-- do not change this KEYWORD!'
  WRITE(IU,'(54A1)') ('=',J=1,54)
!  !#capsim
!  IF(I.EQ.MXSCEN)THEN
!   DO J=1,SCEN(I)%N
!    WRITE(IU,'(A24,2F10.2,A)') SCEN(I)%PACKAGE(J),SCEN(I)%FCT(J),SCEN(I)%IMP(J),SCEN(I)%IDFNAME(J)
!   END DO
!  ELSE
  WRITE(IU,'(A24,2I5)') SCEN(I)%PACKAGE(1),SCEN(I)%IS1,SCEN(I)%IS2
  WRITE(IU,'(A24,2I5)') SCEN(I)%PACKAGE(2),SCEN(I)%IL1,SCEN(I)%IL2
  WRITE(IU,'(A24,6I5)') SCEN(I)%PACKAGE(3),SCEN(I)%IY1,SCEN(I)%IY2,SCEN(I)%IM1,SCEN(I)%IM2,SCEN(I)%ID1,SCEN(I)%ID2
  DO J=4,SCEN(I)%N
   WRITE(IU,'(A24,2F5.1,A)') SCEN(I)%PACKAGE(J),SCEN(I)%FCT(J),SCEN(I)%IMP(J),TRIM(SCEN(I)%IDFNAME(J))
  END DO
! ENDIF
 ENDDO
 CLOSE(IU)

 CALL SCEN1INFO(FNAME)
! CALL IUPPERCASE(FNAME)
 CALL SCEN1ADDDEL(1,FNAME)

 END SUBROUTINE SCEN1NEW

 !###======================================================================
 LOGICAL FUNCTION SCEN1OPENCLOSESCEN()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,N
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE

 SCEN1OPENCLOSESCEN=.FALSE.

 CALL WDIALOGLOAD(ID_DSCENNAME,ID_DSCENNAME)
 CALL UTL_IMODFILLMENU(IDF_MENU1,TRIM(PREFVAL(1))//'\SCENARIOS','*','D',N,0,0)
 CALL WDIALOGFIELDSTATE(IDF_MENU1,1)
 CALL WDIALOGTITLE('Open/Create a Model Scenario')
 CALL WDIALOGPUTSTRING(IDOK,'Open and continue')
 CALL WDIALOGPUTSTRING(IDCANCEL,'Close')
 CALL WDIALOGFIELDSTATE(ID_NOK,3)

 CALL WDIALOGPUTSTRING(IDF_GROUP1,'Select a Scenario, or Enter a new name')
 CALL WDIALOGSHOW(-1,-1,0,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDOK)
      CALL WDIALOGGETMENU(IDF_MENU1,I,SCNFNAME)
      IF(LEN_TRIM(SCNFNAME).NE.0)THEN
       IF(INDEX(TRIM(SCNFNAME),' ').NE.0)THEN
        CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Do not use a space delimiter in a scenario name!','Error')
       ELSE
        EXIT
       ENDIF
      ENDIF
     CASE (IDHELP)
       CALL IMODGETHELP('5.6','TMO.ModScen')
     CASE (IDCANCEL)
      EXIT
    END SELECT
  END SELECT
 ENDDO

 CALL WDIALOGUNLOAD()
 IF(MESSAGE%VALUE1.EQ.IDCANCEL)RETURN

 !## get folder name
 SCNFNAME=TRIM(PREFVAL(1))//'\SCENARIOS\'//TRIM(SCNFNAME)
 SCNDIR  =SCNFNAME
 I       =INDEXNOCASE(SCNFNAME,'\',.TRUE.)
 SCNFNAME=TRIM(SCNFNAME)//'\'//TRIM(SCNFNAME(I+1:))//'.SCN'

 SCEN1OPENCLOSESCEN=.TRUE.

 END FUNCTION SCEN1OPENCLOSESCEN

 !###======================================================================
 LOGICAL FUNCTION SCEN1SAVELOADSCEN(ICODE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ICODE
 INTEGER :: IU,I,J,K,IOS,N
 LOGICAL :: LEX
 CHARACTER(LEN=256) :: LINE
 
 SCEN1SAVELOADSCEN=.FALSE.
 
 IU=UTL_GETUNIT()

 !## open
 IF(ICODE.EQ.1)THEN

  !## not existing, new scenario
  INQUIRE(FILE=SCNFNAME,EXIST=LEX)

  IF(.NOT.LEX)THEN
   I=INDEXNOCASE(SCNFNAME,'\',.TRUE.)
   CALL UTL_CREATEDIR(SCNFNAME(:I-1))
   SHPNO=0
   NSDFNAME=0

  ELSE

   CALL OSD_OPEN(IU,FILE=SCNFNAME,STATUS='OLD',ACTION='READ,DENYWRITE',FORM='FORMATTED')

   SHPNO  =0
   SHPI   =0
   SHPIACT=0
   SHPNAME=''
   NSDFNAME=0
   SDFNAME=''
   DO
    READ(IU,*,IOSTAT=IOS)
    IF(IOS.NE.0)EXIT
    SHPI=SHPI+1
    IF(SHPI.GT.MAXSHAPES)THEN
     CLOSE(IU)
     SHPNO=0
     CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'Total number of shapes read is '// &
              TRIM(ITOS(SHPI))//CHAR(13)//'* Maximal allowed is '//TRIM(ITOS(MAXSHAPES))//CHAR(13)//&
              'You can increase these settings in the menu-option: Preferences'//CHAR(13)//CHAR(13)// &
              'Selected file not read!','Error')
     RETURN
    ENDIF
    READ(IU,*,IOSTAT=IOS) SHPNAME(SHPI)
    READ(IU,*,IOSTAT=IOS)
    READ(IU,*,IOSTAT=IOS) NSDFNAME(SHPI)
    DO K=1,NSDFNAME(SHPI)
     READ(IU,*,IOSTAT=IOS) SDFNAME(SHPI,K)
    END DO
    READ(IU,*,IOSTAT=IOS) LINE
    IF(IOS.EQ.0)THEN
     READ(LINE,*,IOSTAT=IOS) SHPNCRD(SHPI),SHPCOLOR(SHPI)
     IF(IOS.NE.0)THEN
      SHPCOLOR(SHPI)=ICOLOR(SHPI) !ICLRPOLG !WRGB(255,0,0)
      READ(LINE,*,IOSTAT=IOS) SHPNCRD(SHPI)
     ENDIF
    ENDIF
    IF(SHPNCRD(SHPI).GT.MAXSHPCRD)THEN
     CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'The number of coordinates within one shape is '// &
               TRIM(ITOS(SHPNCRD(SHPI)))//CHAR(13)//'* Maximal allowed is '//TRIM(ITOS(MAXSHPCRD))//CHAR(13)//&
               'You can increase these settings in the menu-option: Preferences'//CHAR(13)//CHAR(13)// &
               'Selected file not read completely!','Error')
     CLOSE(IU)
     RETURN
    ENDIF
    DO J=1,SHPNCRD(SHPI)
     READ(IU,*,IOSTAT=IOS) SHPXC(J,SHPI),SHPYC(J,SHPI)
    END DO
    READ(IU,*,IOSTAT=IOS)
    SHPIACT(SHPI) =1
    SHPTYPE(SHPI) =ID_POLYGON   !## default polygon
!    SHPCOLOR(SHPI)=ICOLOR(SHPI) !ICLRPOLG !WRGB(255,0,0)
    SHPWIDTH(SHPI)=2
   ENDDO
   CLOSE(IU)
   SHPNO=SHPI
  ENDIF

  CALL WDIALOGSELECT(ID_DSCEN1)
  IF(SHPNO.EQ.0)THEN
   CALL WDIALOGCLEARFIELD(IDF_MENU1)
  ELSE
   CALL WDIALOGPUTMENU(IDF_MENU1,SHPNAME,SHPNO,SHPIACT)
  ENDIF
  CALL SCEN1FIELDS1()
  CALL POLYGON1FIELDS(ID_DSCEN1)
  CALL IDFPLOTFAST(0)

 ELSEIF(ICODE.EQ.2)THEN

  INQUIRE(FILE=SCNFNAME,EXIST=LEX)
  IF(LEX)THEN
   CALL WMESSAGEBOX(YESNOCANCEL,COMMONNO,QUESTIONICON,'Do you want to overwrite the existing file:'//CHAR(13)//TRIM(SCNFNAME),'Question')
   IF(WINFODIALOG(4).EQ.0)RETURN    !## cancel
   SCEN1SAVELOADSCEN=.TRUE.
   IF(WINFODIALOG(4).EQ.2)RETURN    !## no
  ENDIF

  I=INDEXNOCASE(SCNFNAME,'\',.TRUE.)
  CALL UTL_CREATEDIR(SCNFNAME(:I-1))
  CALL OSD_OPEN(IU,FILE=SCNFNAME,STATUS='REPLACE',ACTION='WRITE,DENYREAD',FORM='FORMATTED')
  N=0
  DO SHPI=1,SHPNO
   IF(SHPNCRD(SHPI).GT.0)THEN
    WRITE(IU,'(50A1)') ('=',K=1,50)
    WRITE(IU,'(A)') TRIM(SHPNAME(SHPI))//' <-Shapename'
    WRITE(IU,'(50A1)') ('-',K=1,50)
    LINE=TRIM(ITOS(NSDFNAME(SHPI)))
    WRITE(IU,*) TRIM(LINE)//' <-No. Definitions Included'
    N=N+NSDFNAME(SHPI)
    DO K=1,NSDFNAME(SHPI)
     WRITE(IU,'(A)') TRIM(SDFNAME(SHPI,K))//' <-Scenario Definition File'
    END DO
    LINE=TRIM(ITOS(SHPNCRD(SHPI)))//','//TRIM(ITOS(SHPCOLOR(SHPI)))
    WRITE(IU,*) TRIM(LINE)//' <-No. Points Polygon,LineColour'
    DO J=1,SHPNCRD(SHPI)
     LINE=TRIM(RTOS(SHPXC(J,SHPI),'F',2))//','//TRIM(RTOS(SHPYC(J,SHPI),'F',2))
     WRITE(IU,'(A)') TRIM(LINE) !SHPXC(J,SHPI),',',SHPYC(J,SHPI)
    END DO
    WRITE(IU,'(50A1)') ('=',K=1,50)
   ENDIF
  END DO
  CLOSE(IU)
  IF(N.EQ.0)THEN
   CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONYES,'iMOD did not found any *.sdf files associated to any polygon.'//CHAR(13)// &
     'Such a scenario yields NO effect whatsoever during a model simulation!'//CHAR(13)// &
     'Would you like to leave the Scenario Tool anyhow?','Question')
   IF(WINFODIALOG(4).NE.1)THEN
    SCEN1SAVELOADSCEN=.FALSE.
    RETURN
   ENDIF
  ENDIF
 ENDIF

 CALL SCEN1FIELDS1()
 CALL WDIALOGSELECT(ID_DSCEN1)

 SCEN1SAVELOADSCEN=.TRUE.

 END FUNCTION SCEN1SAVELOADSCEN

 !###======================================================================
 SUBROUTINE SCEN1INIT()
 !###======================================================================
 IMPLICIT NONE

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_SCENARIO,2).EQ.1)THEN
  CALL SCEN1CLOSE(1)
  RETURN
 ENDIF

 CALL MAIN1INACTMODULE(ID_SCENARIO)

 !other module no closed, no approvement given
 IF(IDIAGERROR.EQ.1)RETURN

 CALL POLYGON1INIT()
 
 ALLOCATE(SDFNAME(MAXSHAPES,MAXSDF))
 ALLOCATE(NSDFNAME(MAXSHAPES))

 CALL WMENUSETSTATE(ID_SCENARIO,2,1)

 IF(.NOT.IOSDIREXISTS(TRIM(PREFVAL(1))//'\SCENARIOS'))CALL UTL_CREATEDIR(TRIM(PREFVAL(1))//'\SCENARIOS')

 CALL WDIALOGLOAD(ID_DSCENARIO,ID_DSCENARIO)

 CALL POLYGON1IMAGES(ID_DSCEN1)

 CALL WDIALOGSELECT(ID_DSCEN2)
 CALL WDIALOGPUTIMAGE(ID_NEW,ID_ICONNEW)
 CALL WDIALOGPUTIMAGE(ID_INFO,ID_ICONINFO)
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONPLUS)
 CALL WDIALOGPUTIMAGE(ID_DELETE,ID_ICONDELETE)
 
 CALL SCEN1INITVAR()

 !## open scenario
 IF(SCEN1OPENCLOSESCEN())THEN
  IF(SCEN1SAVELOADSCEN(1))THEN
   CALL WDIALOGSELECT(ID_DSCENARIO)
   CALL WDIALOGTITLE('Scenario: '//TRIM(SCNFNAME(INDEX(SCNFNAME,'\',.TRUE.)+1:)))
   CALL WDIALOGSHOW(-1,-1,0,2)
   RETURN
  ENDIF
 ENDIF

 CALL SCEN1CLOSE(0)

 END SUBROUTINE SCEN1INIT

 !###======================================================================
 SUBROUTINE SCEN1CLOSE(ICODE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ICODE

 IDIAGERROR=1

 IF(ICODE.EQ.1)THEN
  !## save scenario
  IF(.NOT.SCEN1SAVELOADSCEN(2))RETURN
 ENDIF

 CALL POLYGON1DRAWSHAPE(1,SHPNO)
 CALL POLYGON1CLOSE()

 IF(ALLOCATED(SDFNAME))DEALLOCATE(SDFNAME)
 IF(ALLOCATED(NSDFNAME))DEALLOCATE(NSDFNAME)

 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(ID_SCENARIO,2,0)

 CALL WDIALOGSELECT(ID_DSCENARIO)
 CALL WDIALOGUNLOAD()

 !## refresh window
 CALL IDFPLOTFAST(0)

 IDIAGERROR=0

 END SUBROUTINE SCEN1CLOSE

END MODULE MOD_SCEN