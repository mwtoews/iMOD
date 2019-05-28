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
MODULE MOD_CREATEGEN

USE WINTERACTER
USE RESOURCE
USE MOD_DBL
USE MOD_IDFPLOT
USE IMODVAR, ONLY : DP_KIND,SP_KIND,IDIAGERROR
USE MOD_POLYGON_PAR
USE MOD_POLYGON_DRAW, ONLY : POLYGON1DRAWSHAPE
USE MOD_POLYGON_UTL, ONLY : POLYGON1CLOSE,POLYGON1INIT,POLYGON1IMAGES,POLYGON1FIELDS,POLYGON_UTL_FILLDATAGRID  !POLYGON1SAVELOADSHAPE
USE MOD_POLYGON, ONLY : POLYGON1MAIN,POLYGON1CREATEPOLYGON
USE MODPLOT, ONLY : MPW
USE MOD_UTL, ONLY : UTL_MEASURE,ITOS,DBL_IGRINSIDEPOLYGON,UTL_PLOT1BITMAP,UTL_PLOT2BITMAP,UTL_GETHELP
USE MOD_MAIN_UTL
USE MOD_CREATE_UTL

CONTAINS

 !###======================================================================
 SUBROUTINE CREATEGEN1MAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE

 !## check polygon actions
 IACTSHAPES=(/1,1,1,1,1,3/)
 !## apply general polygon-functionlities
 CALL POLYGON1MAIN(ITYPE,MESSAGE)

 IF(ITYPE.EQ.PUSHBUTTON.AND.MESSAGE%VALUE1.EQ.ID_ZOOMSELECT)THEN
  CALL IDFZOOM(ID_DGOTOXY,0.0D0,0.0D0,0); CALL IDFPLOT(1)
 ENDIF

 CALL WDIALOGSELECT(MESSAGE%WIN)
 CALL WDIALOGFIELDSTATE(ID_INFO,MIN(1,SHP%NPOL))
 
 SELECT CASE(ITYPE)

  CASE(FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE2)
   END SELECT

  CASE(PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (ID_INFO)
     !## read/show current data from memory!     
     CALL POLYGON_UTL_FILLDATAGRID() 
     CALL WDIALOGPUTMENU(IDF_MENU1,SHP%POL%PNAME,SHP%NPOL,SHP%POL%IACT)
    CASE (ID_CUT)
     CALL CREATEGEN_CUTLINES()
     CALL WDIALOGPUTMENU(IDF_MENU1,SHP%POL%PNAME,SHP%NPOL,SHP%POL%IACT)
     CALL IDFPLOT(1)
    CASE (ID_SELECTPOLYGON)
     CALL CREATEGEN_SELECTPOLYGON()
     CALL IDFPLOTFAST(1)
    CASE (IDHELP)
     CALL UTL_GETHELP('3.2.2','EMO.CreateGEN')
    CASE (IDCANCEL)
     CALL CREATEGEN1CLOSE()
   END SELECT

 END SELECT

 END SUBROUTINE

 !###====================================================================
 SUBROUTINE CREATEGEN_SELECTPOLYGON()
 !###====================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: MAXPOL=500
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: XPOL,YPOL
 REAL(KIND=DP_KIND) :: X,Y
 INTEGER :: NPOL,I,J,N
 
 ALLOCATE(XPOL(MAXPOL),YPOL(MAXPOL))
 CALL POLYGON1CREATEPOLYGON(XPOL,YPOL,MAXPOL,NPOL)

 !## correct polygon specified
 IF(NPOL.GT.0)THEN
  !## get lines completely within polygon
  SHP%POL%IACT=0
  DO I=1,SHP%NPOL
   !## count number of points within polygon
   N=0; DO J=1,SHP%POL(I)%N
    X=SHP%POL(I)%X(J); Y=SHP%POL(I)%Y(J)
    IF(MPW%XMIN.LE.X.AND.MPW%XMAX.GE.X.AND.MPW%YMIN.LE.Y.AND.MPW%YMAX.GE.Y)THEN
     IF(DBL_IGRINSIDEPOLYGON(X,Y,XPOL,YPOL,NPOL).NE.1)EXIT
     N=N+1
    ENDIF
   ENDDO
   IF(N.EQ.SHP%POL(I)%N)SHP%POL(I)%IACT=1
  ENDDO
 ENDIF
 
 DEALLOCATE(XPOL,YPOL)
 CALL WCURSORSHAPE(CURARROW); CALL IGRPLOTMODE(MODECOPY); CALL IGRFILLPATTERN(OUTLINE)

 CALL WDIALOGSELECT(ID_DCREATEGEN)
 CALL WDIALOGPUTMENU(IDF_MENU1,SHP%POL%PNAME,SHP%NPOL,SHP%POL%IACT)

 END SUBROUTINE CREATEGEN_SELECTPOLYGON

 !###======================================================================
 SUBROUTINE CREATEGEN_CUTLINES()
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),DIMENSION(:),POINTER :: XCRD,YCRD => NULL()
 INTEGER :: NCRD,I,J,K,II,JJ,ISTATUS,NINT
 REAL(KIND=DP_KIND) :: X1,X2,X3,X4,Y1,Y2,Y3,Y4,XINTER,YINTER
 LOGICAL :: LINT
 
 IF(SHP%NPOL.GE.MAXSHAPES)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You can only use this functionality whenever you have at least'//CHAR(13)// &
    '1 shape less than the maximal number which is allowed','Error')
  RETURN
 ENDIF
 
! IF(ASSOCIATED(XCRD))DEALLOCATE(XCRD); IF(ASSOCIATED(YCRD))DEALLOCATE(YCRD)
! CALL UTL_MEASURE(XCRD,YCRD,NCRD,ID_CURSORLINE)
!
! !## intersect them and clip them into seperate elements
! DO II=1,2
!  NINT=0
!  DO I=1,SHP%NPOL
!   NINT=NINT+1; JJ=0
!   
!   !## copy original coordinates first
!   DO J=1,SHP%POL(I)%N
!    SHP%POL(MAXSHAPES)%X(J)=SHP%POL(I)%X(J)
!    SHP%POL(MAXSHAPES)%Y(J)=SHP%POL(I)%Y(J)
!   ENDDO
!   
!   !## copy first coordinates
!   IF(II.EQ.2)THEN
!    JJ                  =JJ+1
!    SHP%POL(I)%X(1)     =SHP%POL(MAXSHAPES)%X(1)
!    SHP%POL(I)%X(1)     =SHP%POL(MAXSHAPES)%Y(1)
!    SHP%POL(NINT)%PNAME =SHP%POL(I)%PNAME
!    SHP%POL(NINT)%ICOLOR=SHP%POL(I)%ICOLOR
!    SHP%POL(NINT)%IWIDTH=SHP%POL(I)%IWIDTH
!    SHP%POL(NINT)%ITYPE =SHP%POL(I)%ITYPE
!   ENDIF
!   
!!  CALL POLYGON1CREATESHAPE_ADDSEGMENT(INEW)
!  
!  LINT=.FALSE.
!   DO J=1,SHP%POL(I)%N-1
!    !## try intersection with all of them
!    X3=SHP%POL(I)%X(J); Y3=SHP%POL(I)%Y(J); X4=SHP%POL(I)%X(J+1); Y4=SHP%POL(I)%Y(J+1)
!    
!    DO K=1,NCRD-1
!     X1=XCRD(K); Y1=YCRD(K); X2=XCRD(K+1); Y2=YCRD(K+1)
!     CALL DBL_IGRINTERSECTLINE(X1,Y1,X2,Y2,X3,Y3,X4,Y4,XINTER,YINTER,ISTATUS)
!     !## intersect
!     IF(ISTATUS.EQ.5)THEN
!      !## include intersection and start new segment
!      IF(II.EQ.2)THEN
!      
!       !## finish current segment
!       JJ             =JJ+1
!       CSHPXC(JJ,NINT)=XINTER
!       CSHPYC(JJ,NINT)=YINTER
!       CSHPNCRD(NINT) =JJ
!       SHP%POL(NINT)%PNAME=SHP%POL(I)%PNAME
!       SHP%POL(NINT)%ITYPE=ID_LINE
!
!       !## start new segment
!       NINT            =NINT+1
!       JJ              =1
!       CSHPXC(JJ,NINT) =XINTER
!       CSHPYC(JJ,NINT) =YINTER
!       SHP%POL(NINT)%PNAME =SHP%POL(I)%PNAME
!       SHP%POL(NINT)%ICOLOR=SHP%POL(I)%ICOLOR
!       SHP%POL(NINT)%IWIDTH=SHP%POL(I)%IWIDTH
!       SHP%POL(NINT)%ITYPE =ID_LINE 
!       LINT            =.TRUE.
!      ELSE
!       !## count number of intersections (resulting segments)
!       NINT=NINT+1
!      ENDIF
!     ENDIF
!    ENDDO
!
!    !## last coordinate of segment
!    IF(II.EQ.2)THEN
!     JJ             =JJ+1
!     CSHPXC(JJ,NINT)=SHP%POL(I)%X(J+1)
!     CSHPYC(JJ,NINT)=SHP%POL(I)%Y(J+1)
!     CSHPNCRD(NINT) =JJ
!    ENDIF
!
!   ENDDO
!
! ENDDO
!  IF(II.EQ.1)ALLOCATE(CSHPXC(MAXSHPCRD,MAXSHAPES+1),CSHPYC(MAXSHPCRD,MAXSHAPES+1),CSHPNCRD(MAXSHAPES))
! ENDDO
! 
! DEALLOCATE(XCRD,YCRD)
 
 END SUBROUTINE CREATEGEN_CUTLINES

 !###======================================================================
 SUBROUTINE CREATEGEN1INIT()
 !###======================================================================
 IMPLICIT NONE
 
 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_CREATEGEN,2).EQ.1)THEN
  CALL CREATEGEN1CLOSE(); RETURN
 ENDIF

 CALL MAIN_UTL_INACTMODULE(ID_CREATEGEN)

 !## other module not closed, no approvement given
 IF(IDIAGERROR.EQ.1)RETURN

 CALL WMENUSETSTATE(ID_CREATEGEN,2,1)

 CALL WDIALOGLOAD(ID_DCREATEGEN,ID_DCREATEGEN)
 CALL POLYGON1INIT()

 CALL POLYGON1IMAGES(ID_DCREATEGEN)
 CALL POLYGON1FIELDS(ID_DCREATEGEN)
 CALL WDIALOGPUTIMAGE(ID_INFO,ID_ICONINFO)
 CALL WDIALOGPUTIMAGE(ID_CUT,ID_ICONCUT)
 CALL WDIALOGPUTIMAGE(ID_SELECTPOLYGON,ID_ICONPOLYGON)
 CALL WDIALOGFIELDSTATE(ID_INFO,0)
 
 CALL WDIALOGSELECT(ID_DCREATEGEN); CALL WDIALOGSHOW(-1,-1,0,2)

 END SUBROUTINE CREATEGEN1INIT

END MODULE MOD_CREATEGEN
