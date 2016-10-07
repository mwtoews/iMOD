!!  Copyright (C) Stichting Deltares, 2005-2016.
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
MODULE MOD_CREATEGEN

USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : IDIAGERROR
USE MOD_POLYGON_PAR, ONLY : SHPNO,IACTSHAPES,SHPNAME,SHPIACT,SHPXC,SHPYC,CSHPXC,CSHPYC,SHPNCRD,CSHPNCRD, &
        MAXSHPCRD,MAXSHAPES,SHPCOLOR,SHPWIDTH,SHPTYPE
USE MOD_POLYGON_DRAW, ONLY : POLYGON1DRAWSHAPE
USE MOD_POLYGON_UTL, ONLY : POLYGON1CLOSE,POLYGON1INIT,POLYGON1IMAGES,POLYGON1FIELDS,POLYGON1SAVELOADSHAPE
USE MOD_POLYGON, ONLY : POLYGON1MAIN
USE MOD_GENPLOT, ONLY : GENDATAGRID
USE MOD_UTL, ONLY : UTL_MEASURE

CONTAINS

 !###======================================================================
 SUBROUTINE CREATEGEN1MAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE
! CHARACTER(LEN=256) :: GENFNAME

 !## check polygon actions
 IACTSHAPES=(/3,1,1,3,1,3/)
! IF(GENFNAME.EQ.'')THEN
  CALL POLYGON1MAIN(ITYPE,MESSAGE,IDAT=1)
! ELSE
!  CALL POLYGON1MAIN(ITYPE,MESSAGE,IDAT=1,GENFNAME=GENFNAME)
! ENDIF
 
 CALL WDIALOGSELECT(MESSAGE%WIN)
 CALL WDIALOGFIELDSTATE(ID_INFO,MIN(1,SHPNO))
 
 SELECT CASE(ITYPE)

  CASE(FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE2)
   END SELECT

  CASE(PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (ID_INFO)
     !## read/show current data from memory!     
     CALL GENDATAGRID('') 
    CASE (ID_CUT)
     CALL CREATEGEN_CUTLINES()
     CALL WDIALOGPUTMENU(IDF_MENU1,SHPNAME,SHPNO,SHPIACT)
     CALL IDFPLOT(1)
    CASE (IDHELP)
     CALL IMODGETHELP('3.2.2','EMO.CreateGEN')
    CASE (IDCANCEL)
     CALL CREATEGEN1CLOSE()
   END SELECT

 END SELECT

 END SUBROUTINE

 !###======================================================================
 SUBROUTINE CREATEGEN_CUTLINES()
 !###======================================================================
 IMPLICIT NONE
 REAL,DIMENSION(:),POINTER :: XCRD,YCRD => NULL()
 INTEGER :: NCRD,I,J,K,II,JJ,ISTATUS,NINT
 REAL :: X1,X2,X3,X4,Y1,Y2,Y3,Y4,XINTER,YINTER
 LOGICAL :: LINT
 
 IF(ASSOCIATED(XCRD))DEALLOCATE(XCRD); IF(ASSOCIATED(YCRD))DEALLOCATE(YCRD)
 CALL UTL_MEASURE(XCRD,YCRD,NCRD)

 !## intersect them and clip them into seperate elements
 DO II=1,2
  NINT=0
  DO I=1,SHPNO
   NINT=NINT+1; JJ=0

   !## copy first coordinates
   IF(II.EQ.2)THEN
    JJ             =JJ+1
    CSHPXC(JJ,NINT)=SHPXC(1,I)
    CSHPYC(JJ,NINT)=SHPYC(1,I)
    SHPNAME(NINT)  =SHPNAME(I)
    SHPCOLOR(NINT) =SHPCOLOR(I)
    SHPWIDTH(NINT) =SHPWIDTH(I)
    SHPTYPE(NINT)  =SHPTYPE(I)
   ENDIF
   
   LINT=.FALSE.
   DO J=1,SHPNCRD(I)-1
    !## try intersection with all of them
    X3=SHPXC(J,I); Y3=SHPYC(J,I); X4=SHPXC(J+1,I); Y4=SHPYC(J+1,I)
    
    DO K=1,NCRD-1
     X1=XCRD(K); Y1=YCRD(K); X2=XCRD(K+1); Y2=YCRD(K+1)
     CALL IGRINTERSECTLINE(X1,Y1,X2,Y2,X3,Y3,X4,Y4,XINTER,YINTER,ISTATUS)
     !## intersect
     IF(ISTATUS.EQ.5)THEN
      !## include intersection and start new segment
      IF(II.EQ.2)THEN
      
       !## finish current segment
       JJ             =JJ+1
       CSHPXC(JJ,NINT)=XINTER
       CSHPYC(JJ,NINT)=YINTER
       CSHPNCRD(NINT) =JJ
       SHPNAME(NINT)  =SHPNAME(I)
       SHPTYPE(NINT)  =ID_LINE

       !## start new segment
       NINT           =NINT+1
       JJ             =1
       CSHPXC(JJ,NINT)=XINTER
       CSHPYC(JJ,NINT)=YINTER
       SHPNAME(NINT)  =SHPNAME(I)
       SHPCOLOR(NINT) =SHPCOLOR(I)
       SHPWIDTH(NINT) =SHPWIDTH(I)
       SHPTYPE(NINT)  =ID_LINE 
       LINT           =.TRUE.
      ELSE
       !## count number of intersections (resulting segments)
       NINT=NINT+1
      ENDIF
!      EXIT
     ENDIF
    ENDDO

    !## last coordinaten of segment
    IF(II.EQ.2)THEN
     JJ             =JJ+1
     CSHPXC(JJ,NINT)=SHPXC(J+1,I)
     CSHPYC(JJ,NINT)=SHPYC(J+1,I)
     CSHPNCRD(NINT) =JJ
    ENDIF

   ENDDO

  ENDDO
  IF(II.EQ.1)ALLOCATE(CSHPXC(MAXSHPCRD,MAXSHAPES+1),CSHPYC(MAXSHPCRD,MAXSHAPES+1),CSHPNCRD(MAXSHAPES))
 ENDDO
 
 DEALLOCATE(SHPXC,SHPYC,SHPNCRD)
 SHPXC=>CSHPXC 
 SHPYC=>CSHPYC 
 SHPNCRD=>CSHPNCRD
 SHPNO=NINT
 
 DEALLOCATE(XCRD,YCRD)
 
 END SUBROUTINE CREATEGEN_CUTLINES

 !###======================================================================
 SUBROUTINE CREATEGEN1INIT()
 !###======================================================================
 IMPLICIT NONE
 
 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_CREATEGEN,2).EQ.1)THEN
  CALL CREATEGEN1CLOSE(); RETURN
 ENDIF

 CALL MAIN1INACTMODULE(ID_CREATEGEN)

 !## other module no closed, no approvement given
 IF(IDIAGERROR.EQ.1)RETURN

 CALL WMENUSETSTATE(ID_CREATEGEN,2,1)

 CALL WDIALOGLOAD(ID_DCREATEGEN,ID_DCREATEGEN)
 CALL POLYGON1INIT()
 CALL POLYGON1IMAGES(ID_DCREATEGEN)
 CALL POLYGON1FIELDS(ID_DCREATEGEN)
 CALL WDIALOGPUTIMAGE(ID_INFO,ID_ICONINFO)
 CALL WDIALOGPUTIMAGE(ID_CUT,ID_ICONCUT)
 CALL WDIALOGFIELDSTATE(ID_INFO,0)
! GENFNAME=''
 
 CALL WDIALOGSELECT(ID_DCREATEGEN); CALL WDIALOGSHOW(-0,100,0,2)

 END SUBROUTINE CREATEGEN1INIT

 !###======================================================================
 SUBROUTINE CREATEGEN1CLOSE()
 !###======================================================================
 IMPLICIT NONE

 IDIAGERROR=1

 CALL POLYGON1DRAWSHAPE(1,SHPNO); CALL POLYGON1CLOSE()
 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_CREATEGEN,2,0)
 CALL WDIALOGSELECT(ID_DCREATEGEN); CALL WDIALOGUNLOAD()

 IDIAGERROR=0

 END SUBROUTINE CREATEGEN1CLOSE

END MODULE MOD_CREATEGEN
