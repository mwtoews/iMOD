!!  Copyright (C) Stichting Deltares, 2005-2018.
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
MODULE MOD_MSPINSPECTOR

USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : OFFSETX,OFFSETY
USE MOD_MSPINSPECTOR_PAR
USE MOD_MSPINSPECTOR_UTL
USE MOD_IDFPLOT, ONLY : IDFPLOT,IDFZOOM
USE MODPLOT, ONLY : MPW
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_DBL, ONLY : DBL_IGRRECTANGLE
USE MOD_IDF, ONLY : IDFNULLIFY,IDFALLOCATEX,IDFGETEDGE,IDFIROWICOL
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_CAP,UTL_IMODFILLMENU,ITIMETOGDATE,RTOS,UTL_PLOT1BITMAP,UTL_PLOT2BITMAP,UTL_FILLDATES,LISTNAME,UTL_INVERSECOLOUR
USE MOD_MAIN_UTL, ONLY : MAIN_UTL_INACTMODULE

CONTAINS

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_MAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(INOUT) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE

 SELECT CASE (MESSAGE%WIN)

  CASE (ID_DMSPANALYSER_TAB1); CALL MSPINSPECTOR_TAB1(ITYPE,MESSAGE)

  CASE (ID_DMSPANALYSER_TAB2); CALL MSPINSPECTOR_TAB2(ITYPE,MESSAGE)

  CASE (ID_DMSPANALYSER_TAB3); CALL MSPINSPECTOR_TAB3(ITYPE,MESSAGE)

  CASE (ID_DMSPANALYSER_TAB4); CALL MSPINSPECTOR_TAB4(ITYPE,MESSAGE)

  CASE (ID_DMSPANALYSER_TAB5); CALL MSPINSPECTOR_TAB5(ITYPE,MESSAGE)

  CASE (ID_DMSPANALYSER)
   SELECT CASE (ITYPE)

    !## case tab changed
    CASE (TABCHANGED)
     SELECT CASE (MESSAGE%VALUE1)
     END SELECT

    !## case field changed
    CASE (FIELDCHANGED)
     CALL MSPINSPECTOR_MAIN_FIELDS()

    !## pushbutton
    CASE (PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      CASE(IDOK) 
       CALL MSPINSPECTOR_GETXY()

      CASE (IDHELP)
          ! CALL UTL_GETHELP('4.4.3.3','MMO.IGO.IE.Search')
      CASE (IDCANCEL)
       CALL MSPINSPECTOR_CLOSE()
     END SELECT
   END SELECT
 END SELECT

 END SUBROUTINE MSPINSPECTOR_MAIN

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_TAB1(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(INOUT) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE
 INTEGER :: I

 SELECT CASE (ITYPE)

  !## case field changed
  CASE (FIELDCHANGED)
   IF(MESSAGE%VALUE1.EQ.MESSAGE%VALUE2)THEN
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDF_RADIO1,IDF_RADIO2,IDF_STRING1)
      CALL MSPINSPECTOR_TAB1_FIELDS()
    END SELECT
   ENDIF
   
  !## pushbutton
  CASE (PUSHBUTTON)
      
   SELECT CASE (MESSAGE%VALUE1)
    
    !## select folder
    CASE (ID_SELECT)
     ROOT=TRIM(PREFVAL(1))//'\MODEL\'
     CALL WSELECTDIR(DIRCHANGE,ROOT,'Select Model Result Directory')
     IF(WINFODIALOG(4).EQ.1)THEN
      CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB1); CALL WDIALOGPUTSTRING(IDF_STRING1,TRIM(ROOT))
      CALL MSPINSPECTOR_FILL_FOLDERLIST()
     ENDIF
   
    !## read files
    CASE (ID_OPEN)
     I=1; IF(.NOT.MSPINSPECTOR_OPENFILES())THEN; I=0; CALL MSPINSPECTOR_DEALLOCATE(); ENDIF
     CALL MSPINSPECTOR_OPENFILES_FIELDS(I)
     
    !## zoom to full extent
    CASE (ID_ZOOMFULL)
     MPW%XMIN=MSPIDF%XMIN; MPW%XMAX=MSPIDF%XMAX; MPW%YMIN=MSPIDF%YMIN; MPW%YMAX=MSPIDF%YMAX
     CALL IDFZOOM(ID_DGOTOXY,0.0D0,0.0D0,0); CALL IDFPLOT(1)
   END SELECT
   
 END SELECT

 END SUBROUTINE MSPINSPECTOR_TAB1

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_TAB2(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(INOUT) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE

 SELECT CASE (ITYPE)

  !## case field changed
  CASE (FIELDCHANGED)
   CALL MSPINSPECTOR_TAB2_FIELDS()

  !## pushbutton
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
 END SELECT

 END SUBROUTINE MSPINSPECTOR_TAB2

  !###======================================================================
 SUBROUTINE MSPINSPECTOR_TAB3(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(INOUT) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE

 SELECT CASE (ITYPE)

  !## case field changed
  CASE (FIELDCHANGED)
   CALL MSPINSPECTOR_TAB3_FIELDS()

  !## pushbutton
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    !## draw graph of selected features
    CASE(ID_GRAPH)
!      ! moet nog een subroutine  MSPINSPECTOR_GRAPH komen
!      CALL WDIALOGLOAD(ID_DMSPANALYSER_GRAPH,ID_DMSPANALYSER_GRAPH); CALL WDIALOGSHOW(-1,-1,0,2)
   END SELECT
 END SELECT

 END SUBROUTINE MSPINSPECTOR_TAB3

  !###======================================================================
 SUBROUTINE MSPINSPECTOR_TAB4(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(INOUT) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE

 SELECT CASE (ITYPE)

  !## case field changed
  CASE (FIELDCHANGED)
   CALL MSPINSPECTOR_TAB4_FIELDS()

  !## pushbutton
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
 END SELECT

 END SUBROUTINE MSPINSPECTOR_TAB4

  !###======================================================================
 SUBROUTINE MSPINSPECTOR_TAB5(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(INOUT) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE

 SELECT CASE (ITYPE)

  !## case field changed
  CASE (FIELDCHANGED)
   IF(MESSAGE%VALUE1.EQ.MESSAGE%VALUE2)THEN
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDF_RADIO1,IDF_RADIO2,IDF_INTEGER2,IDF_MENU5,IDF_INTEGER1,IDF_INTEGER12,IDF_MENU6,IDF_INTEGER11)
      CALL MSPINSPECTOR_TAB5_FIELDS()   
     CASE (IDF_MENU1)
      CALL MSPINSPECTOR_TAB5_PUTPARAMLIST()   
     CASE (IDF_MENU2)
      CALL MSPINSPECTOR_TAB5_GETPARAMLIST()
      CALL MSPINSPECTOR_TAB3_PUTPARAMLIST()
    END SELECT
   ENDIF      
     
  !## pushbutton
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    !## load en save van settings file
    CASE (ID_OPEN,ID_SAVE)   
   END SELECT
 END SELECT

 END SUBROUTINE MSPINSPECTOR_TAB5

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_FILL_FOLDERLIST()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: N,I
 
 CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB1)
 
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,I)
 IF(I.EQ.1)THEN
  CALL WDIALOGGETSTRING(IDF_STRING1,ROOT)
 ELSE
  !## defaut root in iMOD is models
  ROOT=TRIM(PREFVAL(1))//'\MODELS'
 ENDIF

 CALL UTL_IMODFILLMENU(IDF_MENU1,TRIM(ROOT),'*','D',N,0,0)

 !## disable fields
 N=MAX(0,MIN(1,N)); CALL WDIALOGFIELDSTATE(ID_OPEN,N)
 
 END SUBROUTINE MSPINSPECTOR_FILL_FOLDERLIST
 
!###======================================================================
 SUBROUTINE MSPINSPECTOR_TAB3_PUTPARAMLIST()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: N,I,J,K
 
 CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB3)
 CALL WGRIDCLEAR(IDF_GRID1)
 !## determine number of rows active
 DO K=1,2
 
  N=0; J=0; DO I=1,SIZE(AREASVAT%LABEL)
   IF(AREASVAT%IACT(I).EQ.0)CYCLE
   N=N+1; J=J+1; IF(K.EQ.2)CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,J,AREASVAT%LABEL(I))
  END DO

  IF(K.EQ.1)CALL WGRIDROWS(IDF_GRID1,N)
   
 ENDDO
 
 END SUBROUTINE MSPINSPECTOR_TAB3_PUTPARAMLIST

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_TAB5_PUTPARAMLIST()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB5)
 CALL WDIALOGGETMENU(IDF_MENU1,I)
 
 SELECT CASE (I)
  CASE (1)
   CALL WDIALOGPUTMENU(IDF_MENU2,MODSVAT%LABEL,SIZE(MODSVAT%LABEL),MODSVAT%IACT)
  CASE (2)
   CALL WDIALOGPUTMENU(IDF_MENU2,IDFSVAT%LABEL,SIZE(IDFSVAT%LABEL),IDFSVAT%IACT)
  CASE (3)
!   CALL WDIALOGPUTMENU(IDF_MENU2,AREASVAT%LABEL,SIZE(AREASVAT%LABEL),AREASVAT%IACT)
  CASE (4)
   CALL WDIALOGPUTMENU(IDF_MENU2,AREASVAT%LABEL,SIZE(AREASVAT%LABEL),AREASVAT%IACT)
  END SELECT

 END SUBROUTINE MSPINSPECTOR_TAB5_PUTPARAMLIST

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_TAB5_GETPARAMLIST()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB5)
 CALL WDIALOGGETMENU(IDF_MENU1,I)
 
 SELECT CASE (I)
  CASE (1)
   CALL WDIALOGGETMENU(IDF_MENU2,MODSVAT%IACT)
  CASE (2)
   CALL WDIALOGGETMENU(IDF_MENU2,IDFSVAT%IACT)
  CASE (3)
!   CALL WDIALOGGETMENU(IDF_MENU2,AREASVAT%IACT)
  CASE (4)
   CALL WDIALOGGETMENU(IDF_MENU2,AREASVAT%IACT)
  END SELECT

 END SUBROUTINE MSPINSPECTOR_TAB5_GETPARAMLIST

 !###======================================================================
 LOGICAL FUNCTION MSPINSPECTOR_OPENFILES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IU,I,N
 LOGICAL :: LEX

 MSPINSPECTOR_OPENFILES=.FALSE.

 !## get foldername
 CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB1)
 CALL WDIALOGGETMENU(IDF_MENU1,I,CVALUE=MNAME)
 
 !## get modelname
 N=1; CALL UTL_IMODFILLMENU(0,TRIM(ROOT)//'\'//TRIM(MNAME)//'\MF2005_TMP','*.DXC','F',N,0,1)
 IF(N.EQ.1)THEN
  MFNAME=LISTNAME(1)
 ELSE
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot find file '//CHAR(13)//TRIM(ROOT)//'\'//TRIM(MNAME)//'\MF2005_TMP\*.DXC','Error'); RETURN  
 ENDIF
 IF(ALLOCATED(LISTNAME))DEALLOCATE(LISTNAME)
 
 CALL MSPINSPECTOR_DEALLOCATE();

 !## open files
 IF(.NOT.MSPINSPECTOR_OPENFILES_DXC(IU))THEN
  INQUIRE(UNIT=IU,OPENED=LEX); IF(LEX)CLOSE(IU)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open file '//CHAR(13)//TRIM(ROOT)//'\'//TRIM(MNAME)//'\MF2005_TMP\'//TRIM(MFNAME),'Error'); RETURN
 ENDIF
 IF(.NOT.MSPINSPECTOR_OPENFILES_MOD2SVAT(IU))THEN
  INQUIRE(UNIT=IU,OPENED=LEX); IF(LEX)CLOSE(IU)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open file '//CHAR(13)//TRIM(ROOT)//'\'//TRIM(MNAME)//'\MOD2SVAT.INP','Error'); RETURN
 ENDIF
 IF(.NOT.MSPINSPECTOR_OPENFILES_IDF2SVAT(IU))THEN
  INQUIRE(UNIT=IU,OPENED=LEX); IF(LEX)CLOSE(IU)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open file '//CHAR(13)//TRIM(ROOT)//'\'//TRIM(MNAME)//'\IDF_SVAT.INP','Error'); RETURN
 ENDIF
 IF(.NOT.MSPINSPECTOR_OPENFILES_PARASIM(IU))THEN
  INQUIRE(UNIT=IU,OPENED=LEX); IF(LEX)CLOSE(IU)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open file '//CHAR(13)//TRIM(ROOT)//'\'//TRIM(MNAME)//'\PARA_SIM.INP','Error'); RETURN
 ENDIF
 IF(.NOT.MSPINSPECTOR_OPENFILES_AREASVAT(IU))THEN
  INQUIRE(UNIT=IU,OPENED=LEX); IF(LEX)CLOSE(IU)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open file '//CHAR(13)//TRIM(ROOT)//'\'//TRIM(MNAME)//'\AREA_SVAT.INP','Error'); RETURN
 ENDIF

 MSPINSPECTOR_OPENFILES=.TRUE.

 END FUNCTION MSPINSPECTOR_OPENFILES

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_OPENFILES_FIELDS(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I
 
 !## enable fields and activate tabs
 CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB1)
 CALL WDIALOGFIELDSTATE(ID_ZOOMFULL,I)
 CALL WDIALOGFIELDSTATE(IDF_LABEL1,I)  

 CALL WDIALOGSELECT(ID_DMSPANALYSER)
 CALL WDIALOGFIELDSTATE(IDOK,I)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DMSPANALYSER_TAB2,I)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DMSPANALYSER_TAB3,I)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DMSPANALYSER_TAB4,I)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DMSPANALYSER_TAB5,I)
 IF(I.EQ.1)THEN
  CALL MSPINSPECTOR_TAB3_PUTPARAMLIST() 
  CALL MSPINSPECTOR_TAB5_PUTPARAMLIST() 
 ENDIF
 
 END SUBROUTINE MSPINSPECTOR_OPENFILES_FIELDS
 
 !###======================================================================
 LOGICAL FUNCTION MSPINSPECTOR_OPENFILES_DXC(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IU
 INTEGER :: I,IOS

 MSPINSPECTOR_OPENFILES_DXC=.FALSE.

 !## allocate memory - mainly number of svats
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(ROOT)//'\'//TRIM(MNAME)//'\MF2005_TMP\'//TRIM(MFNAME),STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 IF(IOS.NE.0)RETURN

 READ(IU,*,IOSTAT=IOS) DXC%MXID; IF(IOS.NE.0)RETURN
 READ(IU,*,IOSTAT=IOS) DXC%MXID; IF(IOS.NE.0)RETURN
 ALLOCATE(DXC%INFO(DXC%MXID),STAT=IOS); IF(IOS.NE.0)RETURN
 ALLOCATE(DXC%LABEL(DXC%MXID),STAT=IOS); IF(IOS.NE.0)RETURN
 DO I=1,DXC%MXID
  READ(IU,*,IOSTAT=IOS) DXC%INFO(I)%ILAY,DXC%INFO(I)%IROW,DXC%INFO(I)%ICOL,DXC%INFO(I)%ID
  IF(IOS.NE.0)RETURN
 ENDDO
 CLOSE(IU)

 ALLOCATE(DXC%LABEL(4),DXC%IACT(4))
 DXC%LABEL(1)='Layer-Number'
 DXC%LABEL(2)='Row-Number'
 DXC%LABEL(3)='Column-Number'
 DXC%LABEL(4)='Unique-ID'
 DXC%IACT=1
 
 MSPINSPECTOR_OPENFILES_DXC=.TRUE.

 END FUNCTION MSPINSPECTOR_OPENFILES_DXC

 !###======================================================================
 LOGICAL FUNCTION MSPINSPECTOR_OPENFILES_PARASIM(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IU
 INTEGER :: IOS

 MSPINSPECTOR_OPENFILES_PARASIM=.FALSE.

 !## read netwerk from paramsim.inp
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(ROOT)//'\'//TRIM(MNAME)//'\PARA_SIM.INP',STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 IF(IOS.NE.0)RETURN

 CALL IDFNULLIFY(MSPIDF)
 DO
  READ(IU,'(A)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
  IF(INDEX(UTL_CAP(LINE,'U'),'IDF_XMIN').GT.0)READ(LINE(INDEX(LINE,'=',.TRUE.)+1:),*) MSPIDF%XMIN
  IF(INDEX(UTL_CAP(LINE,'U'),'IDF_YMIN').GT.0)READ(LINE(INDEX(LINE,'=',.TRUE.)+1:),*) MSPIDF%YMIN
  IF(INDEX(UTL_CAP(LINE,'U'),'IDF_DX').GT.0)  READ(LINE(INDEX(LINE,'=',.TRUE.)+1:),*) MSPIDF%DX
  IF(INDEX(UTL_CAP(LINE,'U'),'IDF_DY').GT.0)  READ(LINE(INDEX(LINE,'=',.TRUE.)+1:),*) MSPIDF%DY
  IF(INDEX(UTL_CAP(LINE,'U'),'IDF_NCOL').GT.0)READ(LINE(INDEX(LINE,'=',.TRUE.)+1:),*) MSPIDF%NCOL
  IF(INDEX(UTL_CAP(LINE,'U'),'IDF_NROW').GT.0)READ(LINE(INDEX(LINE,'=',.TRUE.)+1:),*) MSPIDF%NROW
 ENDDO
 CLOSE(IU)

 MSPIDF%XMAX=MSPIDF%XMIN+MSPIDF%DX*MSPIDF%NCOL
 MSPIDF%YMAX=MSPIDF%YMIN+MSPIDF%DY*MSPIDF%NROW
 MSPIDF%NODATA=HUGE(1.0D0)
 IF(.NOT.IDFALLOCATEX(MSPIDF))RETURN; MSPIDF%X=0.0D0

 MSPINSPECTOR_OPENFILES_PARASIM=.TRUE.

 END FUNCTION MSPINSPECTOR_OPENFILES_PARASIM

 !###======================================================================
 LOGICAL FUNCTION MSPINSPECTOR_OPENFILES_MOD2SVAT(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IU
 INTEGER :: I,N,IOS

 MSPINSPECTOR_OPENFILES_MOD2SVAT=.FALSE.

 !## allocate memory - mainly number of svats
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(ROOT)//'\'//TRIM(MNAME)//'\MOD2SVAT.INP',STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 IF(IOS.NE.0)RETURN

 ALLOCATE(MODSVAT%INFO(1),STAT=IOS); IF(IOS.NE.0)RETURN
 DO I=1,2
  MODSVAT%MXID=0; N=1
  DO
   READ(IU,'(I10,2X,I10,I5)',IOSTAT=IOS) MODSVAT%INFO(N)%UNID,MODSVAT%INFO(N)%NUND,MODSVAT%INFO(N)%LYBE
   IF(IOS.NE.0)EXIT; MODSVAT%MXID=MODSVAT%MXID+1
   IF(I.EQ.2)THEN; N=N+1; IF(N.GT.MODSVAT%MXID)EXIT; ENDIF
  ENDDO
  IF(I.EQ.1)THEN; DEALLOCATE(MODSVAT%INFO); ALLOCATE(MODSVAT%INFO(MODSVAT%MXID),STAT=IOS); IF(IOS.NE.0)RETURN; ENDIF
  REWIND(IU)
 ENDDO
 CLOSE(IU)

 ALLOCATE(MODSVAT%LABEL(3),MODSVAT%IACT(3))
 MODSVAT%LABEL(1)='Unique-ID'
 MODSVAT%LABEL(2)='SVAT-ID'
 MODSVAT%LABEL(3)='Layer'
 MODSVAT%IACT=1
 
 MSPINSPECTOR_OPENFILES_MOD2SVAT=.TRUE.

 END FUNCTION MSPINSPECTOR_OPENFILES_MOD2SVAT

 !###======================================================================
 LOGICAL FUNCTION MSPINSPECTOR_OPENFILES_IDF2SVAT(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IU
 INTEGER :: I,N,IOS

 MSPINSPECTOR_OPENFILES_IDF2SVAT=.FALSE.

 !## allocate memory - mainly number of svats
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(ROOT)//'\'//TRIM(MNAME)//'\IDF_SVAT.INP',STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 IF(IOS.NE.0)RETURN

 ALLOCATE(IDFSVAT%INFO(1),STAT=IOS); IF(IOS.NE.0)RETURN
 DO I=1,2
  IDFSVAT%MXID=0; N=1
  DO
   READ(IU,'(3I10)',IOSTAT=IOS) IDFSVAT%INFO(N)%NUND,IDFSVAT%INFO(N)%IROW,IDFSVAT%INFO(N)%ICOL !,IDFSVAT(N)%X,IDFSVAT(N)%Y
   IF(IOS.NE.0)EXIT; IDFSVAT%MXID=IDFSVAT%MXID+1
   IF(I.EQ.2)THEN; N=N+1; IF(N.GT.IDFSVAT%MXID)EXIT; ENDIF
  ENDDO
  IF(I.EQ.1)THEN; DEALLOCATE(IDFSVAT%INFO); ALLOCATE(IDFSVAT%INFO(IDFSVAT%MXID),STAT=IOS); IF(IOS.NE.0)RETURN; ENDIF
  REWIND(IU)
 ENDDO
 CLOSE(IU)

 ALLOCATE(IDFSVAT%LABEL(3),IDFSVAT%IACT(3))
 
 IDFSVAT%LABEL(1)='SVAT-ID'
 IDFSVAT%LABEL(2)='Row-Number'
 IDFSVAT%LABEL(3)='Column-Number'
 IDFSVAT%IACT=1
 
 MSPINSPECTOR_OPENFILES_IDF2SVAT=.TRUE.

 END FUNCTION MSPINSPECTOR_OPENFILES_IDF2SVAT

 !###======================================================================
 LOGICAL FUNCTION MSPINSPECTOR_OPENFILES_AREASVAT(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IU
 INTEGER :: I,IOS,N

 MSPINSPECTOR_OPENFILES_AREASVAT=.FALSE.

 !## allocate memory - mainly number of svats
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(ROOT)//'\'//TRIM(MNAME)//'\AREA_SVAT.INP',STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 IF(IOS.NE.0)RETURN

 ALLOCATE(AREASVAT%INFO(1),STAT=IOS); IF(IOS.NE.0)RETURN
 DO I=1,2
  AREASVAT%MXID=0; N=1
  DO
   READ(IU,'(I10,F10.1,F8.3,8X,I6,16X,I6,F8.3,I10,2F8.3)',IOSTAT=IOS) AREASVAT%INFO(N)%NUND,AREASVAT%INFO(N)%ARND, &
     AREASVAT%INFO(N)%SURF,AREASVAT%INFO(N)%SOIL,AREASVAT%INFO(N)%LUSE,AREASVAT%INFO(N)%RZ,AREASVAT%INFO(N)%METE
   IF(IOS.NE.0)EXIT; AREASVAT%MXID=AREASVAT%MXID+1
   IF(I.EQ.2)THEN; N=N+1; IF(N.GT.AREASVAT%MXID)EXIT; ENDIF
  ENDDO
  IF(I.EQ.1)THEN; DEALLOCATE(AREASVAT%INFO); ALLOCATE(AREASVAT%INFO(AREASVAT%MXID),STAT=IOS); IF(IOS.NE.0)RETURN; ENDIF
  REWIND(IU)
 ENDDO
 CLOSE(IU)

 ALLOCATE(AREASVAT%LABEL(7),AREASVAT%IACT(7))
 AREASVAT%IACT=0
 AREASVAT%LABEL(1)='SVAT-ID'              
 AREASVAT%LABEL(2)='Area'                 
 AREASVAT%LABEL(3)='Surface-Elevation'    ; AREASVAT%IACT(3)=1
 AREASVAT%LABEL(4)='Soil-Type'            ; AREASVAT%IACT(4)=1
 AREASVAT%LABEL(5)='Land-Use'             ; AREASVAT%IACT(5)=1
 AREASVAT%LABEL(6)='Root-Zone'            ; AREASVAT%IACT(6)=1
 AREASVAT%LABEL(7)='Meteo-Station'        ; AREASVAT%IACT(7)=1
 
 MSPINSPECTOR_OPENFILES_AREASVAT=.TRUE.

 END FUNCTION MSPINSPECTOR_OPENFILES_AREASVAT

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_GETXY()
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,ICOL,IROW,JROW,JCOL,ICURSOR,JCURSOR
 REAL(KIND=DP_KIND) :: MOUSEX,MOUSEY,X1,Y1,X2,Y2
 LOGICAL :: LEX
 
 CALL WCURSORSHAPE(ID_CURSORIDFVALUE)
 CALL IGRLINETYPE(OUTLINE); CALL IGRLINEWIDTH(3); CALL IGRCOLOURN(UTL_INVERSECOLOUR(WRGB(255,0,0))); CALL IGRPLOTMODE(MODEXOR)
 
 IROW=0; ICOL=0; LEX=.FALSE.; JCURSOR=ID_CURSORIDFVALUE
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)

  ICURSOR=WINFOMOUSE(MOUSECURSOR)
  SELECT CASE (MESSAGE%WIN)
   CASE (1);               JCURSOR=ID_CURSORIDFVALUE
   CASE (ID_DMSPANALYSER); JCURSOR=CURARROW
  END SELECT
  IF(ICURSOR.NE.JCURSOR)CALL WCURSORSHAPE(ID_CURSORIDFVALUE)
 
  !## shift mouse coordinates
  MOUSEX=DBLE(MESSAGE%GX)+OFFSETX
  MOUSEY=DBLE(MESSAGE%GY)+OFFSETY

  SELECT CASE (ITYPE)

   !## mouse-move
   CASE (MOUSEMOVE)

    CALL WINDOWSELECT(0)
    CALL WINDOWOUTSTATUSBAR(1,'x = '//TRIM(RTOS(MOUSEX,'F',3))//' m; y = '//TRIM(RTOS(MOUSEY,'F',3))//' m')
    
    CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB2)
    CALL WGRIDPUTCELLDOUBLE(IDF_GRID3,2,2,MOUSEX,'(F15.3)')
    CALL WGRIDPUTCELLDOUBLE(IDF_GRID3,3,2,MOUSEY,'(F15.3)')

    CALL IDFIROWICOL(MSPIDF,JROW,JCOL,MOUSEX,MOUSEY)
    IF(ICOL.NE.JCOL.OR.IROW.NE.JROW)THEN
     CALL MSPINSPECTOR_GETXY_PUTVALUES(MOUSEX,MOUSEY)

     CALL UTL_PLOT1BITMAP()
     !## remove drawn rectangle
     IF(LEX)THEN
      CALL IDFGETEDGE(MSPIDF,IROW,ICOL,X1,Y1,X2,Y2)
      CALL DBL_IGRRECTANGLE(X1,Y1,X2,Y2,IOFFSET=1)
     ENDIF
     !## drawn new rectangle
     CALL IDFGETEDGE(MSPIDF,JROW,JCOL,X1,Y1,X2,Y2)
     CALL DBL_IGRRECTANGLE(X1,Y1,X2,Y2,IOFFSET=1)
     CALL UTL_PLOT2BITMAP()
     IROW=JROW; ICOL=JCOL; LEX=.TRUE.
    ENDIF
    
   !## mouse button pressed
   CASE (MOUSEBUTDOWN)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (RIGHTBUTTON,LEFTBUTTON)
      !## remove drawn rectangle
      IF(LEX)THEN
       CALL UTL_PLOT1BITMAP()
       CALL IDFGETEDGE(MSPIDF,IROW,ICOL,X1,Y1,X2,Y2)
       CALL DBL_IGRRECTANGLE(X1,Y1,X2,Y2,IOFFSET=1)
       CALL UTL_PLOT2BITMAP()
      ENDIF
      EXIT
    END SELECT
    
   !## bitmap scrolled, renew top-left pixel coordinates
   CASE (BITMAPSCROLLED)
    MPW%IX=MESSAGE%VALUE1
    MPW%IY=MESSAGE%VALUE2

  END SELECT
 ENDDO
 
 CALL WCURSORSHAPE(CURARROW)
 CALL IGRLINETYPE(OUTLINE); CALL IGRLINEWIDTH(1); CALL IGRCOLOURN((WRGB(0,0,0))); CALL IGRPLOTMODE(MODECOPY)

 END SUBROUTINE MSPINSPECTOR_GETXY
 
 !###======================================================================
 SUBROUTINE MSPINSPECTOR_GETXY_PUTVALUES(MOUSEX,MOUSEY)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: MOUSEX,MOUSEY
 
! CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB2)
! CALL WGRIDPUTCELLDOUBLE(IDF_GRID3,2,2,MOUSEX,'(F15.3)')
! CALL WGRIDPUTCELLDOUBLE(IDF_GRID3,2,3,MOUSEY,'(F15.3)')
! CALL WGRIDPUTCELLINTEGER(IDF_GRID1,4,3,MOUSEY,'(F15.3)')
 
 !CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB2)
 !i=779
 !CALL WGRIDPUTCELLINTEGER(IDF_GRID1,2,1,MODSVAT%INFO(i)%UNID)
 !CALL WGRIDPUTCELLINTEGER(IDF_GRID1,3,1,MODSVAT%INFO(i)%NUND)
 !CALL WGRIDPUTCELLINTEGER(IDF_GRID1,4,1,MODSVAT%INFO(i)%LYBE)
 !
 !
 !CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB4)
 !CALL WGRIDROWS(IDF_GRID3,5)
 !
 !DO I=1,4
 !  DO J=1,3
 !    CALL WGRIDPUTCELLINTEGER(IDF_GRID3,i,j,i*10+j)
 !  END DO 
 !END DO
 !

 END SUBROUTINE MSPINSPECTOR_GETXY_PUTVALUES

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_MAIN_FIELDS()
 !###======================================================================
 IMPLICIT NONE

 END SUBROUTINE MSPINSPECTOR_MAIN_FIELDS

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_TAB1_FIELDS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB1)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,I)
 CALL WDIALOGFIELDSTATE(IDF_STRING1,I)
 CALL WDIALOGFIELDSTATE(ID_SELECT,I)
 
 !## refill menu of folders
 CALL MSPINSPECTOR_FILL_FOLDERLIST()

 END SUBROUTINE MSPINSPECTOR_TAB1_FIELDS

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_TAB2_FIELDS()
 !###======================================================================
 IMPLICIT NONE

 END SUBROUTINE MSPINSPECTOR_TAB2_FIELDS

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_TAB3_FIELDS()
 !###======================================================================
 IMPLICIT NONE

 END SUBROUTINE MSPINSPECTOR_TAB3_FIELDS

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_TAB4_FIELDS()
 !###======================================================================
 IMPLICIT NONE

 END SUBROUTINE MSPINSPECTOR_TAB4_FIELDS

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_TAB5_FIELDS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J
 INTEGER,DIMENSION(14) :: ID
 DATA ID/IDF_LABEL1,IDF_LABEL2,IDF_MENU5,IDF_MENU6,IDF_INTEGER1,IDF_INTEGER2,IDF_INTEGER3,IDF_INTEGER4,IDF_INTEGER5, &
   IDF_INTEGER11,IDF_INTEGER12,IDF_INTEGER13,IDF_INTEGER14,IDF_INTEGER15/
 
 CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB5)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,J)
   
 DO I=1,SIZE(ID); CALL WDIALOGFIELDSTATE(ID(I),ABS(J-1)); ENDDO

 !## from date
 CALL UTL_FILLDATES(IDF_INTEGER2, IDF_MENU5,IDF_INTEGER1 ) 
 !## to date
 CALL UTL_FILLDATES(IDF_INTEGER12,IDF_MENU6,IDF_INTEGER11) 

 END SUBROUTINE MSPINSPECTOR_TAB5_FIELDS

!###======================================================================
 SUBROUTINE MSPINSPECTOR_INIT()
 !###======================================================================
 IMPLICIT NONE

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_MSPANALYSER,2).EQ.1)THEN
  CALL MSPINSPECTOR_CLOSE(); RETURN
 ENDIF

 CALL MAIN_UTL_INACTMODULE(ID_MSPANALYSER)

 !## other module not closed, no approvement given to start this functionality
 IF(IDIAGERROR.EQ.1)RETURN

 CALL WMENUSETSTATE(ID_MSPANALYSER,2,1)
 CALL WDIALOGLOAD(ID_DMSPANALYSER,ID_DMSPANALYSER)
 
 !## fill front tab
 CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB1)
 CALL WDIALOGPUTIMAGE(ID_SELECT,ID_ICONOPEN)      ; CALL WDIALOGTOOLTIP(ID_SELECT,'Open non-default MODEL location')
 CALL WDIALOGPUTIMAGE(ID_ZOOMFULL,ID_ICONZOOMFULL); CALL WDIALOGTOOLTIP(ID_ZOOMFULL,'Zoom to modelwindow')
 CALL WDIALOGFIELDOPTIONS(IDF_STRING1,EDITFIELDCHANGED,ENABLED)

 CALL MSPINSPECTOR_TAB1_FIELDS()
 
 !## initial outgrey tabs
 CALL WDIALOGSELECT(ID_DMSPANALYSER)
 CALL WDIALOGFIELDSTATE(IDOK,0)                   ; CALL WDIALOGTOOLTIP(IDOK,'Start hoovering to inspect parameter values')
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DMSPANALYSER_TAB2,0)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DMSPANALYSER_TAB3,0)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DMSPANALYSER_TAB4,0)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DMSPANALYSER_TAB5,0)

 !## fill tabs 2 
 CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB2)
 CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,1,'Rural')
 CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,2,'Irrigation')
 CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,3,'Urban')
 CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,4,'Nopp')
 
 CALL WGRIDPUTCELLSTRING(IDF_GRID3,1,1,'SVAT_ID')
 CALL WGRIDPUTCELLSTRING(IDF_GRID3,1,2,'Mouse')

 !## fill tabs 4
 CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB4)
 CALL WDIALOGPUTIMAGE(IDF_PICTURE1,ID_ICONIRRIGATION1,1)
 CALL WDIALOGPUTIMAGE(IDF_PICTURE2,ID_ICONIRRIGATION2,1)
 
 !## fill tabs 5
 CALL WDIALOGSELECT(ID_DMSPANALYSER_TAB5)
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPEN,1)     ; CALL WDIALOGTOOLTIP(ID_OPEN,'Save selected parameterset to file')
 CALL WDIALOGPUTIMAGE(ID_SAVE,ID_ICONSAVEAS,1)   ; CALL WDIALOGTOOLTIP(ID_SAVE,'Load selected parameterset from file')

CALL WDIALOGSELECT(ID_DMSPANALYSER); CALL WDIALOGSHOW(-1,-1,0,2)

 END SUBROUTINE MSPINSPECTOR_INIT

END MODULE MOD_MSPINSPECTOR
