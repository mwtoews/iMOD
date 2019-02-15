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
USE MOD_MSPINSPECTOR_PAR
USE MOD_MSPINSPECTOR_UTL
USE MOD_IDF, ONLY : IDFNULLIFY,IDFALLOCATEX
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_CAP
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
      CASE (IDHELP)

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

 SELECT CASE (ITYPE)

  !## case field changed
  CASE (FIELDCHANGED)
   CALL MSPINSPECTOR_TAB1_FIELDS()

  !## pushbutton
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (ID_OPEN)
     IF(.NOT.MSPINSPECTOR_OPENFILES())CALL MSPINSPECTOR_DEALLOCATE()
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
 LOGICAL FUNCTION MSPINSPECTOR_OPENFILES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IU
 LOGICAL :: LEX
 
 MSPINSPECTOR_OPENFILES=.FALSE.
 
 !## get foldername
 ROOT='D:\IMOD-MODELS\CGO-MSWP'; MNAME='A27'

 CALL MSPINSPECTOR_DEALLOCATE(); 

 !## open files
 IF(.NOT.MSPINSPECTOR_OPENFILES_DXC(IU))THEN
  INQUIRE(UNIT=IU,OPENED=LEX); IF(LEX)CLOSE(IU)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open file '//CHAR(13)//TRIM(ROOT)//'\MF2005_TMP\'//TRIM(MNAME)//'.DXC','Error'); RETURN
 ENDIF
 IF(.NOT.MSPINSPECTOR_OPENFILES_MOD2SVAT(IU))THEN
  INQUIRE(UNIT=IU,OPENED=LEX); IF(LEX)CLOSE(IU)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open file '//CHAR(13)//TRIM(ROOT)//'\MOD2SVAT.INP','Error'); RETURN
 ENDIF
 IF(.NOT.MSPINSPECTOR_OPENFILES_IDF2SVAT(IU))THEN
  INQUIRE(UNIT=IU,OPENED=LEX); IF(LEX)CLOSE(IU)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open file '//CHAR(13)//TRIM(ROOT)//'\IDF_SVAT.INP','Error'); RETURN
 ENDIF
 IF(.NOT.MSPINSPECTOR_OPENFILES_PARASIM(IU))THEN
  INQUIRE(UNIT=IU,OPENED=LEX); IF(LEX)CLOSE(IU)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open file '//CHAR(13)//TRIM(ROOT)//'\PARA_SIM.INP','Error'); RETURN
 ENDIF
 IF(.NOT.MSPINSPECTOR_OPENFILES_AREASVAT(IU))THEN
  INQUIRE(UNIT=IU,OPENED=LEX); IF(LEX)CLOSE(IU)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open file '//CHAR(13)//TRIM(ROOT)//'\AREA_SVAT.INP','Error'); RETURN
 ENDIF
 
 MSPINSPECTOR_OPENFILES=.TRUE.
 
 END FUNCTION MSPINSPECTOR_OPENFILES

 !###======================================================================
 LOGICAL FUNCTION MSPINSPECTOR_OPENFILES_DXC(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IU
 INTEGER :: I,IOS

 MSPINSPECTOR_OPENFILES_DXC=.FALSE.

 !## allocate memory - mainly number of svats
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(ROOT)//'\MF2005_TMP\'//TRIM(MNAME)//'.DXC',STATUS='OLD',ACTION='READ',IOSTAT=IOS)
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
 
 ALLOCATE(DXC%LABEL(4))
 DXC%LABEL(1)='Layer-Number'
 DXC%LABEL(2)='Row-Number'
 DXC%LABEL(3)='Column-Number'
 DXC%LABEL(4)='Unique-ID'

 MSPINSPECTOR_OPENFILES_DXC=.TRUE.
 
 END FUNCTION MSPINSPECTOR_OPENFILES_DXC

 !###======================================================================
 LOGICAL FUNCTION MSPINSPECTOR_OPENFILES_PARASIM(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IU
 INTEGER :: IOS

 MSPINSPECTOR_OPENFILES_PARASIM=.FALSE.

 !## netwerk inlezen ... paramsim.inp
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(ROOT)//'\PARA_SIM.INP',STATUS='OLD',ACTION='READ',IOSTAT=IOS)
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
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(ROOT)//'\MOD2SVAT.INP',STATUS='OLD',ACTION='READ',IOSTAT=IOS)
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
 
 ALLOCATE(MODSVAT%LABEL(3))
 MODSVAT%LABEL(1)='Unique-ID'
 MODSVAT%LABEL(2)='SVAT-ID'
 MODSVAT%LABEL(3)='Layer'
 
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
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(ROOT)//'\IDF_SVAT.INP',STATUS='OLD',ACTION='READ',IOSTAT=IOS)
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
 
 ALLOCATE(IDFSVAT%LABEL(3))
 IDFSVAT%LABEL(1)='SVAT-ID'
 IDFSVAT%LABEL(2)='Row-Number'
 IDFSVAT%LABEL(3)='Column-Number'

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
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(ROOT)//'\AREA_SVAT.INP',STATUS='OLD',ACTION='READ',IOSTAT=IOS)
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
 
 ALLOCATE(AREASVAT%LABEL(7))
 AREASVAT%LABEL(1)='SVAT-ID'
 AREASVAT%LABEL(2)='Area'
 AREASVAT%LABEL(3)='Surface-Elevation'
 AREASVAT%LABEL(4)='Soil-Type'
 AREASVAT%LABEL(5)='Land-Use'
 AREASVAT%LABEL(6)='Root-Zone'
 AREASVAT%LABEL(7)='Meteo-Station'

 MSPINSPECTOR_OPENFILES_AREASVAT=.TRUE.
 
 END FUNCTION MSPINSPECTOR_OPENFILES_AREASVAT
 
 !###======================================================================
 SUBROUTINE MSPINSPECTOR_PLOTLOC() !X,Y,ICODE)
 !###======================================================================
 IMPLICIT NONE
! INTEGER,INTENT(IN) :: ICODE
! REAL(KIND=DP_KIND),INTENT(IN) :: X,Y
! INTEGER :: I,IROW,ICOL,N
 !
 !!## remove all rectangles
 !IF(ICODE.EQ.0)THEN
 ! DO I=1,N; CALL UTL_PLOTLOCATIONIDF(IDF(I),IPOSIDF(1,I),IPOSIDF(2,I)); ENDDO
 !ELSE
 ! DO I=1,N 
 !  IF(IDF(I)%XMIN.LE.X.AND.IDF(I)%XMAX.GE.X.AND. &
 !     IDF(I)%YMIN.LE.Y.AND.IDF(I)%YMAX.GE.Y)THEN
 !   CALL IGRCOLOURN(UTL_INVERSECOLOUR(ICOLORIDF(I))) 
 !   CALL IDFIROWICOL(IDF(I),IROW,ICOL,X,Y)
 !   IF(IROW.NE.IPOSIDF(1,I).OR.ICOL.NE.IPOSIDF(2,I))THEN
 !    !## remove it, previous one
 !    CALL UTL_PLOTLOCATIONIDF(IDF(I),IPOSIDF(1,I),IPOSIDF(2,I))
 !    !## plot new one
 !    IF(ICODE.EQ.1)CALL UTL_PLOTLOCATIONIDF(IDF(I),IROW,ICOL)
 !    IPOSIDF(1,I)=IROW
 !    IPOSIDF(2,I)=ICOL
 !   ENDIF
 !  ENDIF
 ! ENDDO
 !ENDIF

 END SUBROUTINE MSPINSPECTOR_PLOTLOC

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_MAIN_FIELDS()
 !###======================================================================
 IMPLICIT NONE

 END SUBROUTINE MSPINSPECTOR_MAIN_FIELDS

 !###======================================================================
 SUBROUTINE MSPINSPECTOR_TAB1_FIELDS()
 !###======================================================================
 IMPLICIT NONE

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

 CALL WDIALOGLOAD(ID_DMSPANALYSER,ID_DMSPANALYSER); CALL WDIALOGSHOW(-1,-1,0,2)

 END SUBROUTINE MSPINSPECTOR_INIT
   
END MODULE MOD_MSPINSPECTOR
