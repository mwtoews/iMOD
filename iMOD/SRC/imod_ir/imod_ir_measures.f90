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
MODULE MOD_IR_MEASURES

USE WINTERACTER
USE RESOURCE
USE MOD_UTL, ONLY : ITOS,RTOS
USE MOD_IR_PAR
USE MOD_IR_UTL, ONLY : IR1GETTREEVIEWID,IR1SHAPE2POL,IR1POL2SHAPE
USE MOD_IR_FIELDS, ONLY : IR1FIELDS_WRITETAB2

TYPE(WIN_MESSAGE) :: MESSAGE
INTEGER,PRIVATE :: ITYPE,IFIELD,ITREE,ISEL1,ISEL2

INTEGER,ALLOCATABLE,DIMENSION(:),PRIVATE :: BU_IMES
REAL,ALLOCATABLE,DIMENSION(:),PRIVATE :: BU_IMP
INTEGER,PRIVATE :: NMES

CONTAINS

 !###======================================================================
 SUBROUTINE IR1MEASURES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,IMES,JMES,IPOL
 INTEGER :: IERROR,ICOL,IROW

 !## get level of treeview
 CALL IR1GETTREEVIEWID(ITREE,IFIELD)

 !## read current number of polygons
 CALL IR1SHAPE2POL(ITREE,IFIELD)
 CALL IR1POL2SHAPE(ITREE,IFIELD)

 !## selected polygon
 DO IPOL=1,MTREE(IFIELD)%NPOL
  IF(MTREE(IFIELD)%POL(IPOL)%IACT.EQ.1)EXIT
 ENDDO

 !## make sure memory is allocated to store def
! IF(.NOT.ASSOCIATED(MTREE(IFIELD)%POL(IPOL)%MES))THEN
 IF(MTREE(IFIELD)%POL(IPOL)%NMES.EQ.0)THEN
  ALLOCATE(MTREE(IFIELD)%POL(IPOL)%MES(MAXMES))
  MTREE(IFIELD)%POL(IPOL)%NMES=0
 ENDIF

 NMES=MTREE(IFIELD)%POL(IPOL)%NMES

 !## create backup
 ALLOCATE(BU_IMES(MAXMES),BU_IMP(MAXMES))

 !## fill in measures
 DO I=1,MTREE(IFIELD)%POL(IPOL)%NMES
  BU_IMES(I)=MTREE(IFIELD)%POL(IPOL)%MES(I)%IMES
  BU_IMP(I) =MTREE(IFIELD)%POL(IPOL)%MES(I)%IMP
 END DO

 !## initialize dialog
 CALL IR1INITMEASURES()

 !## fill dialog
 CALL IR1PUTMEASURES()
 CALL IR1FIELDMEASURES()

 CALL WDIALOGSELECT(ID_DIRMEASURES)
 CALL WDIALOGSETFIELD(IDOK)
 CALL WDIALOGSHOW(-1,-1,0,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  CALL WDIALOGSELECT(MESSAGE%WIN)

  SELECT CASE (ITYPE)

   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_GRID1)
      CALL IR1GETPOSMEASURES()
      CALL IR1FIELDMEASURES()
      CALL WGRIDPOS(MESSAGE%Y,ICOL,IROW)
      IF(ICOL.EQ.1)THEN
       CALL WGRIDGETCELLMENU(IDF_GRID1,ICOL,IROW,I)
       !## real
       IF(IR(I)%TYPEIR.EQ.0)THEN
        CALL WGRIDSTATECELL(IDF_GRID1,2,IROW,1)
        CALL WGRIDPUTCELLREAL(IDF_GRID1,2,IROW,BU_IMP(ICOL))
       !## binair
       ELSEIF(IR(I)%TYPEIR.EQ.1)THEN
        CALL WGRIDSTATECELL(IDF_GRID1,2,IROW,0)
        CALL WGRIDPUTCELLREAL(IDF_GRID1,2,IROW,1.0)
       ENDIF
      ENDIF
    END SELECT

   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (ID_ADD)
      CALL IR1ADDMEASURES()
     CASE (ID_REMOVE)
      CALL IR1REMOVEMEASURES()
     CASE (IDOK)
      IERROR=0
      !## check parameters
ILOOP: DO I=1,NMES
       !## get selected measure
       CALL WGRIDGETCELLMENU(IDF_GRID1,1,I,IMES)
       DO J=1,NMES
        CALL WGRIDGETCELLMENU(IDF_GRID1,1,J,JMES)
        IF(J.NE.I.AND.JMES.EQ.IMES)THEN
         CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'There is a duplicate measure definition number: ['//TRIM(ITOS(J))//']', &
          'Info')
         IERROR=1
         EXIT ILOOP
        ENDIF
       END DO
      ENDDO ILOOP
      IF(IERROR.EQ.0)THEN
       CALL IR1GETMEASURES(IERROR)
       IF(IERROR.EQ.0)THEN
        !## adjust measures
        MTREE(IFIELD)%POL(IPOL)%NMES=NMES
        DO I=1,MTREE(IFIELD)%POL(IPOL)%NMES
         MTREE(IFIELD)%POL(IPOL)%MES(I)%IMES=BU_IMES(I)
         MTREE(IFIELD)%POL(IPOL)%MES(I)%IMP =BU_IMP(I)
        END DO
        CALL IR1FIELDS_WRITETAB2(IFIELD)
        EXIT
       ENDIF
      ENDIF
     CASE (IDCANCEL)
      EXIT
     CASE (IDHELP)
       CALL IMODGETHELP('5.8.2','Start Quick Scan Tool')     
    END SELECT
  END SELECT
 ENDDO

 CALL WDIALOGSELECT(ID_DIRMEASURES)
 CALL WDIALOGHIDE()

 DEALLOCATE(BU_IMES,BU_IMP)

 END SUBROUTINE IR1MEASURES

 !###======================================================================
 SUBROUTINE IR1PUTMEASURES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,IMES

 !## expand grid
 CALL WDIALOGSELECT(ID_DIRMEASURES)
 IF(NMES.EQ.0)THEN
  CALL WDIALOGCLEARFIELD(IDF_GRID1)
  CALL WGRIDROWS(IDF_GRID1,1)
  CALL WDIALOGFIELDSTATE(IDF_GRID1,0)
 ELSE
  CALL WGRIDROWS(IDF_GRID1,NMES)
  !## fill in measures
  DO I=1,NMES
   IF(WINFODIALOGFIELD(IDF_GRID1,FIELDSTATE).NE.1)CALL WDIALOGFIELDSTATE(IDF_GRID1,1)
   CALL WGRIDPUTCELLOPTION(IDF_GRID1,1,I,BU_IMES(I))

   !## real
   IF(IR(BU_IMES(I))%TYPEIR.EQ.0)THEN
    CALL WGRIDSTATECELL(IDF_GRID1,2,I,1)
    CALL WGRIDPUTCELLREAL(IDF_GRID1,2,I,BU_IMP(I))
   !## binair
   ELSEIF(IR(BU_IMES(I))%TYPEIR.EQ.1)THEN
    CALL WGRIDSTATECELL(IDF_GRID1,2,I,0)
    CALL WGRIDPUTCELLREAL(IDF_GRID1,2,I,1.0)
   ENDIF

  END DO
 ENDIF

 END SUBROUTINE IR1PUTMEASURES

 !###======================================================================
 SUBROUTINE IR1GETMEASURES(IERROR)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: IERROR
 INTEGER :: I

 CALL WDIALOGSELECT(ID_DIRMEASURES)

 !## read-out measures
 DO I=1,NMES
  !## get selected measure
  CALL WGRIDGETCELLMENU(IDF_GRID1,1,I,BU_IMES(I))
  !## get selected measure strength
  CALL WGRIDGETCELLREAL(IDF_GRID1,2,I,BU_IMP(I))

  IF(IERROR.EQ.0)THEN
   IF(BU_IMP(I).LT.IR(BU_IMES(I))%MINIR)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Input for '//TRIM(IR(BU_IMES(I))%NAMEIR)// &
      ' exceeds minimum value ('//TRIM(RTOS(IR(BU_IMES(I))%MINIR,'F',2))//'). This has been adjusted automatically','Error')
    BU_IMP(I)=IR(BU_IMES(I))%MINIR
    CALL WGRIDPUTCELLREAL(IDF_GRID1,2,I,BU_IMP(I))
    IERROR=1
   ENDIF
   IF(BU_IMP(I).LT.IR(BU_IMES(I))%MINIR.OR.BU_IMP(I).GT.IR(BU_IMES(I))%MAXIR)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Input for '//TRIM(IR(BU_IMES(I))%NAMEIR)// &
      ' exceeds maximum value ('//TRIM(RTOS(IR(BU_IMES(I))%MAXIR,'F',2))//'). This has been adjusted automatically','Error')
    BU_IMP(I)=IR(BU_IMES(I))%MAXIR
    CALL WGRIDPUTCELLREAL(IDF_GRID1,2,I,BU_IMP(I))
    IERROR=1
   ENDIF
  ENDIF

 END DO

 END SUBROUTINE IR1GETMEASURES

 !###======================================================================
 SUBROUTINE IR1FIELDMEASURES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL WDIALOGSELECT(ID_DIRMEASURES)
 I=1
 IF(NMES.EQ.NIR)I=0
 CALL WDIALOGFIELDSTATE(ID_ADD,I)

 I =1
 IF(NMES.EQ.0)THEN
  I=0
 ELSE
  IF(ISEL1.EQ.0.OR.ISEL2.EQ.0)I=0
 ENDIF

 CALL WDIALOGFIELDSTATE(ID_REMOVE,I)

! IF(RES(IRES)%ITYPERES.EQ.0)THEN
!  CALL WDIALOGPUTSTRING(IDF_LABEL3,'meter')
! ELSEIF(RES(IRES)%ITYPERES.EQ.1)THEN
!  CALL WDIALOGPUTSTRING(IDF_LABEL3,'mm/day')
! ENDIF

 END SUBROUTINE IR1FIELDMEASURES

 !###======================================================================
 SUBROUTINE IR1GETPOSMEASURES()
 !###======================================================================
 IMPLICIT NONE

 ISEL1=WINFOGRID(IDF_GRID1,GRIDSELROW1)
 ISEL2=WINFOGRID(IDF_GRID1,GRIDSELROW2)
 IF(ISEL1.EQ.0.OR.ISEL2.EQ.0)THEN
  CALL WGRIDPOS(MESSAGE%Y,ISEL1,ISEL2)
  ISEL1=ISEL2
 ENDIF

 END SUBROUTINE IR1GETPOSMEASURES

 !###======================================================================
 SUBROUTINE IR1ADDMEASURES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 !##initialize dialog in case nmes=0
 IF(NMES.EQ.0)CALL IR1INITMEASURES

 I=1
 CALL IR1GETMEASURES(I)

 NMES=NMES+1

 BU_IMES(NMES)=1!NMES
 BU_IMP(NMES) =IR(1)%MAXIR

 CALL IR1PUTMEASURES()
 CALL IR1FIELDMEASURES()

 END SUBROUTINE IR1ADDMEASURES

 !###======================================================================
 SUBROUTINE IR1REMOVEMEASURES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: N,I,NM1,NM2,IMES
 REAL :: IMP
 CHARACTER(LEN=500) :: TXT

 I=1
 CALL IR1GETMEASURES(I)

 CALL WDIALOGSELECT(ID_DIRMEASURES)
 IF(ISEL1.EQ.0.OR.ISEL2.EQ.0)RETURN

 TXT='Are you sure to remove:'
 !## fill in measures
 DO I=ISEL1,ISEL2
  IMES =BU_IMES(I)
  IMP  =BU_IMP(I)
  TXT=TRIM(TXT)//CHAR(13)//CHAR(10)//TRIM(IR(IMES)%NAMEIR)//' + '//TRIM(RTOS(IMP,'F',2))//' meter'
 END DO

 CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,TRIM(TXT)//' ?','Question')
 IF(WINFODIALOG(4).NE.1)RETURN

 N  =((ISEL2-ISEL1)+1)
 NM1=  N
 NM2=  NM1-N

!WRITE(*,*) NDEF,ISEL1,ND2,ISEL2,ND1
 BU_IMES(ISEL1:NM2)=BU_IMES(ISEL2+1:NM1)
 BU_IMP(ISEL1:NM2) =BU_IMP(ISEL2+1:NM1)

!WRITE(*,*) 'NMES=',NMES
 NMES=NMES-N

 CALL IR1PUTMEASURES()
 CALL IR1FIELDMEASURES()

 END SUBROUTINE IR1REMOVEMEASURES

 !###======================================================================
 SUBROUTINE IR1INITMEASURES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IOPTIONS

 !## settings for measures
 CALL WDIALOGSELECT(ID_DIRMEASURES)
 CALL WGRIDROWS(IDF_GRID1,NIR)

 ALLOCATE(IOPTIONS(NIR))

 IOPTIONS   =1
 IOPTIONS(1)=1
 CALL WGRIDPUTMENU(IDF_GRID1,1,IR(1:NIR)%NAMEIR,NIR,IOPTIONS,1)
 CALL WGRIDSPINNERSTEP(IDF_GRID1,2,0.01,0.2)
! CALL WGRIDRANGEREAL(IDF_GRID1,2,0.0,1.0)
! DO I=1,NIR
!  CALL WGRIDPUTCELLREAL(IDF_GRID1,2,I,1.0,'(F7.2)')
! END DO

 DEALLOCATE(IOPTIONS)

 CALL WDIALOGFIELDOPTIONS(IDF_GRID1,EDITFIELDCHANGED,1)

 END SUBROUTINE IR1INITMEASURES

END MODULE MOD_IR_MEASURES
