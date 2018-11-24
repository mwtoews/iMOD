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
MODULE MOD_IR_INVERSE

USE WINTERACTER
USE RESOURCE
USE MOD_UTL, ONLY : ITOS,RTOS,UTL_GETHELP
USE MOD_IR_PAR
USE MOD_IR_UTL, ONLY : IR1GETTREEVIEWID

TYPE(WIN_MESSAGE) :: MESSAGE
INTEGER,PRIVATE :: ITREE,IFIELD,ISEL1,ISEL2,ITYPE
INTEGER,ALLOCATABLE,DIMENSION(:),PRIVATE :: BU_ISEL,BU_IFIXED
REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:),PRIVATE :: BU_LLIMP,BU_ULIMP,BU_IMP
INTEGER,PRIVATE :: NOPT

CONTAINS

 !###======================================================================
 SUBROUTINE IR1INVERSE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,IOPT,JOPT,IERROR
 REAL(KIND=DP_KIND) :: X1,X2

 !## get level of treeview
 CALL IR1GETTREEVIEWID(ITREE,IFIELD)
! WRITE(*,*) ' ITREE=',ITREE

 CALL WDIALOGLOAD(ID_DIRINVERSE,ID_DIRINVERSE)
 CALL WGRIDSPINNERSTEP(IDF_GRID1,3,0.01,0.2)
 CALL WGRIDSPINNERSTEP(IDF_GRID1,4,0.01,0.2)
 CALL WGRIDSPINNERSTEP(IDF_GRID1,5,0.01,0.2)

 !## create backup
 ALLOCATE(BU_ISEL(NIR),BU_LLIMP(NIR),BU_ULIMP(NIR),BU_IMP(NIR),BU_IFIXED(NIR))

 !## fill in measures
 NOPT=MTREE(IFIELD)%NOPT
 DO I=1,MTREE(IFIELD)%NOPT
  BU_ISEL(I)  =MTREE(IFIELD)%OPT(I)%ISEL
  BU_LLIMP(I) =MTREE(IFIELD)%OPT(I)%LLIMP
  BU_ULIMP(I) =MTREE(IFIELD)%OPT(I)%ULIMP
  BU_IMP(I)   =MTREE(IFIELD)%OPT(I)%IMP
  BU_IFIXED(I)=MTREE(IFIELD)%OPT(I)%IFIXED
 END DO

 CALL IR1PUTINVERSE()
 CALL IR1FIELDINVERSE()
 CALL WDIALOGSHOW(-1,-1,0,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (FIELDCHANGED)
    !## position in grid
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_GRID1)
      CALL IR1GETPOSINVERSE()
      CALL IR1GETINVERSE()
      CALL IR1FIELDINVERSE()
    END SELECT

   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (ID_ADD)
      CALL IR1ADDINVERSE()
     CASE (ID_REMOVE)
      CALL IR1REMOVEINVERSE()
     CASE (IDOK)
      !## get given constrains
      CALL IR1GETINVERSE()

      IERROR=0
      !## check parameters
ILOOP: DO I=1,NOPT
       !## get selected measure
       CALL WGRIDGETCELLMENU(IDF_GRID1,2,I,IOPT)
       DO J=1,NOPT
        CALL WGRIDGETCELLMENU(IDF_GRID1,2,J,JOPT)
        IF(J.NE.I.AND.JOPT.EQ.IOPT)THEN
         CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'There is a duplicate CONSTRAINT definition number: ['// &
           TRIM(ITOS(J))//']','Error')
         IERROR=1
         EXIT ILOOP
        ENDIF
       END DO
       IF(BU_IFIXED(I).EQ.1)THEN
        CALL WGRIDGETCELLDOUBLE(IDF_GRID1,5,I,X1)
        X2=X1
       ELSE
        CALL WGRIDGETCELLDOUBLE(IDF_GRID1,3,I,X1)
        CALL WGRIDGETCELLDOUBLE(IDF_GRID1,4,I,X2)
        IF(X1.GE.X2)THEN
         CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK, &
          'The lower limit ('//TRIM(RTOS(X1,'F',2))//') is higher or equal than the upper limit ('//TRIM(RTOS(X2,'F',2))// &
          ') for CONSTRAINT definition number: ['//TRIM(ITOS(I))//']','Error')
         IERROR=1
         EXIT ILOOP
        ENDIF
       ENDIF
       IF(X1.GT.IR(IOPT)%MAXIR.OR.X2.GT.IR(IOPT)%MAXIR)THEN
        CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Input for '//TRIM(IR(BU_ISEL(I))%NAMEIR)//' exceeds maximum value ('//&
             TRIM(RTOS(IR(IOPT)%MAXIR,'F',2))//'). This has been adjusted automatically.','Error')
        BU_ULIMP(I)=IR(BU_ISEL(I))%MAXIR
        IF(BU_IFIXED(I).EQ.0)CALL WGRIDPUTCELLDOUBLE(IDF_GRID1,4,I,BU_ULIMP(I))
        IF(BU_IFIXED(I).EQ.1)CALL WGRIDPUTCELLDOUBLE(IDF_GRID1,5,I,BU_ULIMP(I))
        IERROR=1
        EXIT ILOOP
       ENDIF
       IF(X1.LT.IR(IOPT)%MINIR.OR.X2.LT.IR(IOPT)%MINIR)THEN
        CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Input for '//TRIM(IR(BU_ISEL(I))%NAMEIR)//' exceeds minimum value ('//&
             TRIM(RTOS(IR(IOPT)%MINIR,'F',2))//'). This has been adjusted automatically.','Error')
        BU_LLIMP(I)=IR(BU_ISEL(I))%MINIR
        IF(BU_IFIXED(I).EQ.0)CALL WGRIDPUTCELLDOUBLE(IDF_GRID1,3,I,BU_LLIMP(I))
        IF(BU_IFIXED(I).EQ.1)CALL WGRIDPUTCELLDOUBLE(IDF_GRID1,5,I,BU_LLIMP(I))
        IERROR=1
        EXIT ILOOP
       ENDIF

      ENDDO ILOOP
      IF(IERROR.EQ.0)THEN
       !## fill in measures
       MTREE(IFIELD)%NOPT=NOPT
       DO I=1,MTREE(IFIELD)%NOPT
!WRITE(*,*) i,MTREE(IFIELD)%NOPT,bu_isel(i),BU_LLIMP(I),BU_uLIMP(I)
        MTREE(IFIELD)%OPT(I)%ISEL  =BU_ISEL(I)
        MTREE(IFIELD)%OPT(I)%LLIMP =BU_LLIMP(I)
        MTREE(IFIELD)%OPT(I)%ULIMP =BU_ULIMP(I)
        MTREE(IFIELD)%OPT(I)%IMP   =BU_IMP(I)
        MTREE(IFIELD)%OPT(I)%IFIXED=BU_IFIXED(I)
       END DO
       EXIT
      ENDIF
     CASE (IDCANCEL)
      EXIT
     CASE (IDHELP)
       CALL UTL_GETHELP('5.8.2','TMO.QS.Start')
    END SELECT
  END SELECT
 ENDDO

 DEALLOCATE(BU_ISEL,BU_LLIMP,BU_ULIMP,BU_IMP,BU_IFIXED)

 CALL WDIALOGSELECT(ID_DIRINVERSE)
 CALL WDIALOGUNLOAD()

 END SUBROUTINE IR1INVERSE

 !###======================================================================
 SUBROUTINE IR1ADDINVERSE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL IR1GETINVERSE()

 NOPT           =NOPT+1
 BU_ISEL(NOPT)  =1
 BU_LLIMP(NOPT) =0.0D0
 BU_ULIMP(NOPT) =1.0D0
 BU_IMP(NOPT)   =1.0D0
 BU_IFIXED(NOPT)=0

 I=1
 IF(NOPT.EQ.NIR)I=0
 CALL WDIALOGFIELDSTATE(ID_ADD,I)

 CALL IR1PUTINVERSE()
 CALL IR1FIELDINVERSE()

 END SUBROUTINE IR1ADDINVERSE

 !###======================================================================
 SUBROUTINE IR1REMOVEINVERSE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: N,I,J,NM1,NM2
 CHARACTER(LEN=500) :: TXT
 REAL(KIND=DP_KIND) :: LOWER,UPPER

 CALL IR1GETINVERSE()

 IF(ISEL1.EQ.0.OR.ISEL2.EQ.0)RETURN

 !## fill in measures
 TXT='Are you sure to delete:'
 DO I=ISEL1,ISEL2
! WRITE(*,*) I,SIZE(BU_LLIMP)
  LOWER=BU_LLIMP(I)
  UPPER=BU_ULIMP(I)
  J    =BU_ISEL(I)
  TXT  =TRIM(TXT)//CHAR(13)//CHAR(10)//TRIM(IR(J)%NAMEIR)
  TXT  =TRIM(TXT)//' ['//TRIM(RTOS(LOWER,'F',2))//' - '//TRIM(RTOS(UPPER,'F',2))//']'
 END DO
 CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,TRIM(TXT)//' ?','Question')
 IF(WINFODIALOG(4).NE.1)RETURN

 N  =((ISEL2-ISEL1)+1)
 NM1=  N
 NM2=  NM1-N

 BU_ISEL(ISEL1:NM2)  =BU_ISEL(ISEL2+1:NM1)
 BU_LLIMP(ISEL1:NM2) =BU_LLIMP(ISEL2+1:NM1)
 BU_ULIMP(ISEL1:NM2) =BU_ULIMP(ISEL2+1:NM1)
 BU_IMP(ISEL1:NM2)   =BU_IMP(ISEL2+1:NM1)
 BU_IFIXED(ISEL1:NM2)=BU_IFIXED(ISEL2+1:NM1)

 NOPT=NOPT-N

 CALL IR1PUTINVERSE()
 CALL IR1FIELDINVERSE()

 END SUBROUTINE IR1REMOVEINVERSE

 !###======================================================================
 SUBROUTINE IR1GETPOSINVERSE()
 !###======================================================================
 IMPLICIT NONE

 ISEL1=WINFOGRID(IDF_GRID1,GRIDSELROW1)
 ISEL2=WINFOGRID(IDF_GRID1,GRIDSELROW2)

! WRITE(*,*) 'isel1,isel2=',isel1,isel2

 IF(ISEL1.EQ.0.OR.ISEL2.EQ.0)THEN
  CALL WGRIDPOS(MESSAGE%Y,ISEL1,ISEL2)
  ISEL1=ISEL2
 ENDIF

! WRITE(*,*) 'isel1,isel2=',isel1,isel2

 END SUBROUTINE IR1GETPOSINVERSE

 !###======================================================================
 SUBROUTINE IR1PUTINVERSE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

!WRITE(*,*) 'nopt=',nopt

 IF(NOPT.EQ.0)THEN
  CALL WDIALOGCLEARFIELD(IDF_GRID1)
  CALL WGRIDROWS(IDF_GRID1,1)
  CALL WDIALOGFIELDSTATE(IDF_GRID1,0)
  CALL WGRIDPUTMENU(IDF_GRID1,2,IR(1:NIR)%NAMEIR,NIR,(/1/),1)
 ELSE
  !## settings for measures
  CALL WGRIDROWS(IDF_GRID1,NOPT)
  CALL WGRIDPUTMENU(IDF_GRID1,2,IR(1:NIR)%NAMEIR,NIR,BU_ISEL,NOPT)
  DO I=1,NOPT
   CALL WGRIDPUTCELLCHECKBOX(IDF_GRID1,1,I,BU_IFIXED(I))
   CALL WGRIDPUTCELLDOUBLE(IDF_GRID1,3,I,BU_LLIMP(I),'(F8.2)')
   CALL WGRIDPUTCELLDOUBLE(IDF_GRID1,4,I,BU_ULIMP(I),'(F8.2)')
   CALL WGRIDPUTCELLDOUBLE(IDF_GRID1,5,I,BU_IMP(I),'(F8.2)')
  END DO
 ENDIF

 END SUBROUTINE IR1PUTINVERSE

 !###======================================================================
 SUBROUTINE IR1FIELDINVERSE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J

 CALL WDIALOGSELECT(ID_DIRINVERSE)

 I=1
 IF(NOPT.EQ.0)I=0
 CALL WDIALOGFIELDSTATE(IDF_GRID1,I)

 DO I=1,NOPT
  CALL WGRIDGETCELLCHECKBOX(IDF_GRID1,1,I,J)
  CALL WGRIDSTATECELL(IDF_GRID1,3,I,ABS(J-1))
  CALL WGRIDSTATECELL(IDF_GRID1,4,I,ABS(J-1))
  CALL WGRIDSTATECELL(IDF_GRID1,5,I,J)
  IF(J.EQ.1)THEN
   CALL WGRIDCOLOURCELL(IDF_GRID1,3,I,-1,WRGB(100,100,100))
   CALL WGRIDCOLOURCELL(IDF_GRID1,4,I,-1,WRGB(100,100,100))
   CALL WGRIDCOLOURCELL(IDF_GRID1,5,I,-1,-1)
  ELSE
   CALL WGRIDCOLOURCELL(IDF_GRID1,3,I,-1,-1)
   CALL WGRIDCOLOURCELL(IDF_GRID1,4,I,-1,-1)
   CALL WGRIDCOLOURCELL(IDF_GRID1,5,I,-1,WRGB(100,100,100))
  ENDIF
 END DO

 I=1
 IF(NOPT.EQ.0)THEN
  I=0
 ELSE
  IF(ISEL1.EQ.0.OR.ISEL2.EQ.0)I=0
 ENDIF
 CALL WDIALOGFIELDSTATE(ID_REMOVE,I)

 END SUBROUTINE IR1FIELDINVERSE

 !###======================================================================
 SUBROUTINE IR1GETINVERSE()
 !###======================================================================
 IMPLICIT NONE

 CALL WDIALOGSELECT(ID_DIRINVERSE)
 CALL WGRIDGETMENU(IDF_GRID1,2,BU_ISEL,NOPT)
 CALL WGRIDGETCHECKBOX(IDF_GRID1,1,BU_IFIXED,NOPT)
 CALL WGRIDGETDOUBLE(IDF_GRID1,3,BU_LLIMP,NOPT)
 CALL WGRIDGETDOUBLE(IDF_GRID1,4,BU_ULIMP,NOPT)
 CALL WGRIDGETDOUBLE(IDF_GRID1,5,BU_IMP,NOPT)

 END SUBROUTINE IR1GETINVERSE

END MODULE MOD_IR_INVERSE
