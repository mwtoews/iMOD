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

 !## open files
 IF(.NOT.MSPINSPECTOR_OPENFILES_DXC(IU))THEN
  INQUIRE(UNIT=IU,OPENED=LEX); IF(LEX)CLOSE(IU)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open file '//CHAR(13)// &
   TRIM(ROOT)//'\MF2005_TMP\'//TRIM(MNAME)//'.DXC','Error'); RETURN
 ENDIF
 IF(.NOT.MSPINSPECTOR_OPENFILES_PARASIM(IU))THEN
  INQUIRE(UNIT=IU,OPENED=LEX); IF(LEX)CLOSE(IU)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open file '//CHAR(13)// &
   TRIM(ROOT)//'\PARA_SIM.INP','Error'); RETURN
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
 IF(.NOT.MSPINSPECTOR_ALLOCATE_DXC())RETURN
 DO I=1,DXC%MXID 
  READ(IU,*,IOSTAT=IOS) DXC%ILAY(I),DXC%IROW(I),DXC%ICOL(I),DXC%ID(I)
  IF(IOS.NE.0)RETURN
 ENDDO
 CLOSE(IU)
 
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
 SUBROUTINE MSPINSPECTOR_OPENFILES_MOD2SVAT(ROOT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: ROOT
 INTEGER :: IU

 !## allocate memory - mainly number of svats
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(ROOT)//'MOD2SVAT.INP',STATUS='OLD',ACTION='READ')
! DO I=1,2
!  N=N+1
!  DO
!   READ(IU,'(I10,2X,I10,I5)',IOSTAT=IOS) UNID,NUND,LYBE
!   IF(IOS.NE.0)EXIT; N=N+1
!   IF(I.EQ.2)THEN
!!    (N)%UNID=UNID
!!    (N)%NUND=NUND
!!    (N)%LYBE=LYBE
!   ENDIF
!  ENDDO
! ENDDO
 
 END SUBROUTINE MSPINSPECTOR_OPENFILES_MOD2SVAT

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
