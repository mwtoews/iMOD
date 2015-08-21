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
MODULE MOD_IR_TARGETS

USE WINTERACTER
USE RESOURCE
USE MOD_UTL, ONLY : ITOS,RTOS
USE MOD_IR_PAR
USE MOD_IR_UTL, ONLY : IR1GETTREEVIEWID,IR1SHAPE2POL,IR1POL2SHAPE
USE MOD_IR_FIELDS, ONLY : IR1FIELDS_WRITETAB1,IR1DELETETARGETIDF

TYPE(WIN_MESSAGE) :: MESSAGE
INTEGER,PRIVATE :: ITYPE,IFIELD,ITREE,ISEL1,ISEL2

INTEGER,ALLOCATABLE,DIMENSION(:),PRIVATE :: BU_INEWT,BU_INEWP
REAL,ALLOCATABLE,DIMENSION(:),PRIVATE :: BU_LOWER,BU_UPPER
INTEGER,PRIVATE :: NDEF!,BU_EFFECT

CONTAINS

 !###======================================================================
 SUBROUTINE IR1TARGETS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,IERROR,INWT,INWP,JNWT,JNWP,IPOL
 REAL :: X1,X2

 !## get level of treeview
 CALL IR1GETTREEVIEWID(ITREE,IFIELD)

 !## read current number of polygons
 CALL IR1SHAPE2POL(ITREE,IFIELD)
 CALL IR1POL2SHAPE(ITREE,IFIELD)

 !## selected polygon
 DO IPOL=1,TTREE(IFIELD)%NPOL
  IF(TTREE(IFIELD)%POL(IPOL)%IACT.EQ.1)EXIT
 ENDDO

 !## make sure memory is allocated to store def
 IF(TTREE(IFIELD)%POL(IPOL)%NDEF.EQ.0)THEN
! IF(.NOT.ASSOCIATED(TTREE(IFIELD)%POL(IPOL)%DEF))THEN
  ALLOCATE(TTREE(IFIELD)%POL(IPOL)%DEF(MAXDEF))
  TTREE(IFIELD)%POL(IPOL)%NDEF=0
!  TTREE(IFIELD)%POL(IPOL)%EFFECT=70
 ENDIF

 NDEF=TTREE(IFIELD)%POL(IPOL)%NDEF

 !## create backup
 ALLOCATE(BU_INEWT(MAXDEF),BU_INEWP(MAXDEF),BU_LOWER(MAXDEF),BU_UPPER(MAXDEF))

 !## fill in measures
 DO I=1,NDEF
  BU_INEWT(I)=TTREE(IFIELD)%POL(IPOL)%DEF(I)%INEWT
  BU_INEWP(I)=TTREE(IFIELD)%POL(IPOL)%DEF(I)%INEWP
  BU_LOWER(I)=TTREE(IFIELD)%POL(IPOL)%DEF(I)%LOWER
  BU_UPPER(I)=TTREE(IFIELD)%POL(IPOL)%DEF(I)%UPPER
 ENDDO
! BU_EFFECT=MAX(1,MIN(TTREE(IFIELD)%POL(IPOL)%EFFECT,100))

 !## initialize dialog
 CALL IR1INITTARGET()

 !## fill dialog
 CALL IR1PUTTARGET()
 CALL IR1FIELDTARGET()

 CALL WDIALOGSELECT(ID_DIRTARGETS)
 CALL WDIALOGRANGEINTEGER(IDF_INTEGER1,0,100)
 CALL WDIALOGTITLE('Assign Target definitions')
 CALL WDIALOGPUTSTRING(IDF_GROUP2,'Assign Target definitions')
 CALL WDIALOGSETFIELD(IDOK)
 CALL WDIALOGSHOW(-1,-1,0,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  CALL WDIALOGSELECT(MESSAGE%WIN)
  SELECT CASE (ITYPE)
   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)!1)
!     CASE (IDF_TRACKBAR1)
!      CALL WDIALOGGETTRACKBAR(IDF_TRACKBAR1,BU_EFFECT)
!      CALL WDIALOGPUTINTEGER(IDF_INTEGER1,BU_EFFECT)
!     CASE (IDF_INTEGER1)
!      CALL WDIALOGGETINTEGER(IDF_INTEGER1,BU_EFFECT)
!      CALL WDIALOGPUTTRACKBAR(IDF_TRACKBAR1,BU_EFFECT)
!    END SELECT
!    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_GRID1)
      CALL IR1GETPOSTARGET()
      CALL IR1FIELDTARGET()
    END SELECT
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (ID_ADD)
      CALL IR1ADDTARGET()
     CASE (ID_REMOVE)
      CALL IR1REMOVETARGET()
     CASE (IDOK)
      IERROR=0
      !## check parameters
ILOOP: DO I=1,NDEF
       !## get selected measure
       CALL WGRIDGETCELLMENU(IDF_GRID1,1,I,INWT)
       CALL WGRIDGETCELLMENU(IDF_GRID1,2,I,INWP)
       DO J=1,NDEF
        CALL WGRIDGETCELLMENU(IDF_GRID1,1,J,JNWT)
        CALL WGRIDGETCELLMENU(IDF_GRID1,2,J,JNWP)
        IF(J.NE.I.AND.JNWT.EQ.INWT.AND.JNWP.EQ.INWP)THEN
         CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'There is a duplicate target definition number: ['//TRIM(ITOS(J))//']', &
          'Error')
         IERROR=1
         EXIT ILOOP
        ENDIF
        CALL WGRIDGETCELLREAL(IDF_GRID1,3,J,X1)
        CALL WGRIDGETCELLREAL(IDF_GRID1,4,J,X2)
        IF(X1.GE.X2)THEN
         CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK, &
          'The lower limit ('//TRIM(RTOS(X1,'F',2))//') is higher or equal than the upper limit ('//TRIM(RTOS(X2,'F',2))// &
          ') for target definition number: ['//TRIM(ITOS(J))//']','Error')
         IERROR=1
         EXIT ILOOP
        ENDIF
       END DO
      ENDDO ILOOP
      IF(IERROR.EQ.0)THEN
       CALL IR1GETTARGET()
       !## reset measures
       TTREE(IFIELD)%POL(IPOL)%NDEF=NDEF
       DO I=1,NDEF
        TTREE(IFIELD)%POL(IPOL)%DEF(I)%INEWT=BU_INEWT(I)
        TTREE(IFIELD)%POL(IPOL)%DEF(I)%INEWP=BU_INEWP(I)
        TTREE(IFIELD)%POL(IPOL)%DEF(I)%LOWER=BU_LOWER(I)
        TTREE(IFIELD)%POL(IPOL)%DEF(I)%UPPER=BU_UPPER(I)
       END DO
!       TTREE(IFIELD)%POL(IPOL)%EFFECT=MAX(0,MIN(BU_EFFECT,100))
       CALL IR1FIELDS_WRITETAB1(IFIELD)
       !# delete target-idf if it exists
       CALL IR1DELETETARGETIDF()
       EXIT
      ENDIF
     CASE (IDCANCEL)
      EXIT
     CASE (IDHELP)
       CALL IMODGETHELP('5.8.2','TMO.QS.Start')     
    END SELECT
  END SELECT
 ENDDO

 DEALLOCATE(BU_INEWT,BU_INEWP,BU_LOWER,BU_UPPER)

 CALL WDIALOGSELECT(ID_DIRTARGETS)
 CALL WDIALOGHIDE()

 END SUBROUTINE IR1TARGETS

 !###======================================================================
 SUBROUTINE IR1PUTTARGET()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL WDIALOGSELECT(ID_DIRTARGETS)
 !## fill in targets
 IF(NDEF.EQ.0)THEN
  CALL WDIALOGCLEARFIELD(IDF_GRID1)
  CALL WGRIDROWS(IDF_GRID1,1)
  CALL WDIALOGFIELDSTATE(IDF_GRID1,0)
 ELSE
  IF(WINFODIALOGFIELD(IDF_GRID1,FIELDSTATE).NE.1)CALL WDIALOGFIELDSTATE(IDF_GRID1,1)
  CALL WGRIDROWS(IDF_GRID1,NDEF)
  DO I=1,NDEF
   CALL WGRIDPUTCELLOPTION(IDF_GRID1,1,I,BU_INEWT(I))
   CALL WGRIDPUTCELLOPTION(IDF_GRID1,2,I,BU_INEWP(I))
   CALL WGRIDPUTCELLREAL  (IDF_GRID1,3,I,BU_LOWER(I))
   CALL WGRIDPUTCELLREAL  (IDF_GRID1,4,I,BU_UPPER(I))
  END DO
 ENDIF

! CALL WDIALOGPUTINTEGER(IDF_INTEGER1,BU_EFFECT)
! CALL WDIALOGPUTTRACKBAR(IDF_TRACKBAR1,BU_EFFECT)

 END SUBROUTINE IR1PUTTARGET

 !###======================================================================
 SUBROUTINE IR1GETTARGET()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL WDIALOGSELECT(ID_DIRTARGETS)
 !## get filled in targets
 DO I=1,NDEF
  CALL WGRIDGETCELLMENU(IDF_GRID1,1,I,BU_INEWT(I)) !## topic
  CALL WGRIDGETCELLMENU(IDF_GRID1,2,I,BU_INEWP(I)) !## period
  CALL WGRIDGETCELLREAL(IDF_GRID1,3,I,BU_LOWER(I)) !## >=
  CALL WGRIDGETCELLREAL(IDF_GRID1,4,I,BU_UPPER(I)) !## <=
 END DO

! CALL WDIALOGGETINTEGER(IDF_INTEGER1,BU_EFFECT)

 END SUBROUTINE IR1GETTARGET

 !###======================================================================
 SUBROUTINE IR1FIELDTARGET()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL WDIALOGSELECT(ID_DIRTARGETS)

 I=1
 IF(NDEF.EQ.MAXDEF)I=0
 CALL WDIALOGFIELDSTATE(ID_ADD,I)

 I=1
 IF(NDEF.EQ.0)THEN
  I=0
 ELSE
  IF(ISEL1.EQ.0.OR.ISEL2.EQ.0)I=0
 ENDIF

 CALL WDIALOGFIELDSTATE(ID_REMOVE,I)

 END SUBROUTINE IR1FIELDTARGET

 !###======================================================================
 SUBROUTINE IR1GETPOSTARGET()
 !###======================================================================
 IMPLICIT NONE

 ISEL1=WINFOGRID(IDF_GRID1,GRIDSELROW1)
 ISEL2=WINFOGRID(IDF_GRID1,GRIDSELROW2)
 IF(ISEL1.EQ.0.OR.ISEL2.EQ.0)THEN
  CALL WGRIDPOS(MESSAGE%Y,ISEL1,ISEL2)
  ISEL1=ISEL2
 ENDIF

 END SUBROUTINE IR1GETPOSTARGET

 !###======================================================================
 SUBROUTINE IR1ADDTARGET()
 !###======================================================================
 IMPLICIT NONE

 !##initialize dialog in case ndef=0
 IF(NDEF.EQ.0)CALL IR1INITTARGET

 CALL IR1GETTARGET()

 NDEF=NDEF+1

 BU_INEWP(NDEF)=1
 BU_INEWT(NDEF)=1
 BU_LOWER(NDEF)=0.0
 BU_UPPER(NDEF)=1.0

 !## fill dialog
 CALL IR1PUTTARGET()
 CALL IR1FIELDTARGET()

 END SUBROUTINE IR1ADDTARGET

 !###======================================================================
 SUBROUTINE IR1REMOVETARGET()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: ND1,ND2,N,I,INEWT,INEWP
 REAL :: LOWER,UPPER
 CHARACTER(LEN=500) :: TXT

 CALL IR1GETTARGET()

 CALL WDIALOGSELECT(ID_DIRTARGETS)

 !## fill in measures
 TXT='Are you sure to delete:'
 DO I=ISEL1,ISEL2
  INEWT=BU_INEWT(I)
  INEWP=BU_INEWP(I)
  LOWER=BU_LOWER(I)
  UPPER=BU_UPPER(I)
  TXT=TRIM(TXT)//CHAR(13)//CHAR(10)//TRIM(PER(INEWP)%NAMEPER)//' '//TRIM(RES(INEWT)%NAMERES)
  TXT=TRIM(TXT)//' ['//TRIM(RTOS(LOWER,'F',2))//' - '//TRIM(RTOS(UPPER,'F',2))//']'
 END DO

 CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,TRIM(TXT)//' ?','Question')
 IF(WINFODIALOG(4).NE.1)RETURN

 N  =((ISEL2-ISEL1)+1)
 ND1 = NDEF
 ND2 = ND1-N

!WRITE(*,*) NDEF,ISEL1,ND2,ISEL2,ND1
 BU_INEWP(ISEL1:ND2)=BU_INEWP(ISEL2+1:ND1)
 BU_INEWT(ISEL1:ND2)=BU_INEWT(ISEL2+1:ND1)
 BU_LOWER(ISEL1:ND2)=BU_LOWER(ISEL2+1:ND1)
 BU_UPPER(ISEL1:ND2)=BU_UPPER(ISEL2+1:ND1)

 NDEF=NDEF-N

 !## fill dialog
 CALL IR1PUTTARGET()
 CALL IR1FIELDTARGET()

 END SUBROUTINE IR1REMOVETARGET

 !###======================================================================
 SUBROUTINE IR1INITTARGET()
 !###======================================================================
 IMPLICIT NONE
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IOPTIONS

 CALL WDIALOGSELECT(ID_DIRTARGETS)
 CALL WGRIDROWS(IDF_GRID1,1)

 ALLOCATE(IOPTIONS(NPER))

 IOPTIONS   =1
 IOPTIONS(1)=1
 CALL WGRIDPUTMENU(IDF_GRID1,2,PER(1:NPER)%NAMEPER,NPER,IOPTIONS,1)
 IF(ALLOCATED(IOPTIONS))DEALLOCATE(IOPTIONS)
 ALLOCATE(IOPTIONS(NRES))
 IOPTIONS   =1
 IOPTIONS(1)=1
 CALL WGRIDPUTMENU(IDF_GRID1,1,RES(1:NRES)%NAMERES,NRES,IOPTIONS,1)
 CALL WGRIDPUTCELLREAL(IDF_GRID1,3,1,0.0)
 CALL WGRIDPUTCELLREAL(IDF_GRID1,4,1,0.0)

 CALL WGRIDSPINNERSTEP(IDF_GRID1,3,0.01,0.2)
 CALL WGRIDSPINNERSTEP(IDF_GRID1,4,0.01,0.2)

 CALL WDIALOGFIELDOPTIONS(IDF_GRID1,EDITFIELDCHANGED,1)

 DEALLOCATE(IOPTIONS)

 END SUBROUTINE IR1INITTARGET

END MODULE MOD_IR_TARGETS
