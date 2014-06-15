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

MODULE MOD_CREATEIPF

USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : IDIAGERROR
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MODPLOT, ONLY : MPW
USE MOD_POLYGON_PAR, ONLY : SHPNO,SHPFILE,IACTSHAPES,SHPIACT,SHPXC,SHPYC,SHPI,SHPNAME
USE MOD_POLYGON_DRAW, ONLY : POLYGON1DRAWSHAPE
USE MOD_POLYGON_UTL, ONLY : POLYGON1CLOSE,POLYGON1INIT,POLYGON1IMAGES,POLYGON1FIELDS,POLYGON1SAVELOADSHAPE
USE MOD_POLYGON, ONLY : POLYGON1MAIN
USE MOD_GENPLOT, ONLY : GENDATAGRID
USE MOD_UTL, ONLY : NV,NL,IVAR,VAR,UTL_INSIDEPOLYGON,ITOS,RTOS,IDFPLOT1BITMAP,IDFPLOT2BITMAP,UTL_WSELECTFILE
USE MOD_OSD, ONLY : OSD_GETENV,OSD_OPEN

CONTAINS

 !###======================================================================
 SUBROUTINE CREATEIPF1MAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN)           :: ITYPE
 INTEGER :: MSHPNO
 
 !## check polygon actions
 IACTSHAPES=(/1,3,3,3,3,3/)
 CALL POLYGON1MAIN(ITYPE,MESSAGE,IDAT=1,GENFNAME='.IPF')
 
 CALL WDIALOGSELECT(MESSAGE%WIN)
 CALL WDIALOGFIELDSTATE(ID_INFO,MIN(1,SHPNO))
 CALL WDIALOGFIELDSTATE(ID_SAVE,MIN(1,SHPNO))
 
 SELECT CASE(ITYPE)

  CASE (MENUSELECT)
   SELECT CASE (MESSAGE%VALUE1)
    !## current extent
    CASE (ID_FCUREXT)
     SHPIACT=0
     DO SHPI=1,SHPNO
      IF(MPW%XMIN.LE.SHPXC(1,SHPI).AND.MPW%XMAX.GE.SHPXC(1,SHPI).AND. &
         MPW%YMIN.LE.SHPYC(1,SHPI).AND.MPW%YMAX.GE.SHPYC(1,SHPI))SHPIACT(SHPI)=1
     END DO
     !## entire domain
    CASE (ID_FENTDOM)
     SHPIACT=1
    CASE (ID_DESELECTALL)
     SHPIACT=0
    !## zoombox
    CASE (ID_FZOOMBOX)
     CALL CREATEIPF_SELECTBOX()
   END SELECT
   CALL WDIALOGSELECT(ID_DCREATEIPF)
   CALL WDIALOGPUTMENU(IDF_MENU1,SHPNAME,SHPNO,SHPIACT)
   CALL POLYGON1FIELDS(ID_DCREATEIPF) 

  CASE(FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE2)
   END SELECT
  CASE(MOUSEBUTDOWN)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (3)
     CALL WMENUFLOATING(ID_MENU10,MESSAGE%X,MESSAGE%Y)
   END SELECT
  CASE(PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (ID_INFO)
     !## read/show current data from memory!     
     CALL GENDATAGRID('') 
    CASE (IDCANCEL)
     CALL CREATEIPF1CLOSE()
    CASE (IDHELP)
     CALL IMODGETHELP('3.2.3','Create an IPF-file')

   END SELECT

 END SELECT

 END SUBROUTINE

 !###======================================================================
 SUBROUTINE CREATEIPF_DRAWPOLYGON(MAXPOL,NPOL,XPOL,YPOL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NPOL,MAXPOL
 REAL,INTENT(IN),DIMENSION(MAXPOL) :: XPOL,YPOL

 IF(NPOL.EQ.2)THEN
  CALL IGRJOIN(XPOL(1),YPOL(1),XPOL(2),YPOL(2))
 ELSE
  CALL IGRPOLYGONCOMPLEX(XPOL,YPOL,NPOL)
 ENDIF
 
 END SUBROUTINE CREATEIPF_DRAWPOLYGON
 
 !###======================================================================
 SUBROUTINE CREATEIPF_SELECTBOX()
 !###======================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: MAXPOL=50
 INTEGER :: I,NPOL,ITYPE
 TYPE(WIN_MESSAGE) :: MESSAGE
 LOGICAL :: LEX
 REAL,ALLOCATABLE,DIMENSION(:) :: XPOL,YPOL,XPOL2,YPOL2
 REAL :: XC1,YC1

 CALL WCURSORSHAPE(ID_CURSORPOLYGON)

 ALLOCATE(XPOL(MAXPOL),YPOL(MAXPOL),XPOL2(MAXPOL),YPOL2(MAXPOL)); NPOL=1

 CALL IGRPLOTMODE(MODEXOR)
 CALL IGRCOLOURN(WRGB(255,255,255))
 CALL IGRFILLPATTERN(OUTLINE)
 LEX=.FALSE.

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   !## mouse-move
   CASE (MOUSEMOVE)
    CALL WINDOWSELECT(0)
    CALL WINDOWOUTSTATUSBAR(1,'Xcrd:'//TRIM(RTOS(MESSAGE%GX,'F',2))//' m, Ycrd:'// &
                                       TRIM(RTOS(MESSAGE%GY,'F',2))//' m')
    XC1=MESSAGE%GX; YC1=MESSAGE%GY

    IF(NPOL.GT.1)THEN
     CALL IDFPLOT1BITMAP()
     IF(LEX)CALL CREATEIPF_DRAWPOLYGON(SIZE(XPOL),NPOL,XPOL,YPOL)
     LEX=.TRUE.
     XPOL(NPOL)=XC1; YPOL(NPOL)=YC1
     CALL CREATEIPF_DRAWPOLYGON(SIZE(XPOL),NPOL,XPOL,YPOL)
     CALL IDFPLOT2BITMAP()
    ENDIF   

   CASE (MOUSEBUTDOWN)
    CALL IDFPLOT1BITMAP()
    IF(LEX)CALL CREATEIPF_DRAWPOLYGON(SIZE(XPOL),NPOL,XPOL,YPOL)
    SELECT CASE (MESSAGE%VALUE1)
     !## left button
     CASE (1)
      XPOL(NPOL:NPOL+1)=XC1; YPOL(NPOL:NPOL+1)=YC1; NPOL=NPOL+1
      CALL CREATEIPF_DRAWPOLYGON(SIZE(XPOL),NPOL,XPOL,YPOL)
      CALL IDFPLOT2BITMAP()
     !## right button
     CASE (3)
      NPOL=NPOL-1
      CALL IDFPLOT2BITMAP()
      EXIT
    END SELECT
    !## bitmap scrolled, renew top-left pixel coordinates
   CASE (BITMAPSCROLLED)
    MPW%IX=MESSAGE%VALUE1; MPW%IY=MESSAGE%VALUE2
   END SELECT
 END DO

 SHPIACT=0
 IF(NPOL.GT.2)THEN
  DO SHPI=1,SHPNO
   IF(MPW%XMIN.LE.SHPXC(1,SHPI).AND.MPW%XMAX.GE.SHPXC(1,SHPI).AND. &
      MPW%YMIN.LE.SHPYC(1,SHPI).AND.MPW%YMAX.GE.SHPYC(1,SHPI))THEN
    IF(UTL_INSIDEPOLYGON(SHPXC(1,SHPI),SHPYC(1,SHPI),XPOL,YPOL,XPOL2,YPOL2,NPOL+1).EQ.1)SHPIACT(SHPI)=1
   ENDIF
  END DO
 ENDIF
 DEALLOCATE(XPOL,YPOL,XPOL2,YPOL2)
 CALL WCURSORSHAPE(CURARROW)
 CALL IGRPLOTMODE(MODECOPY)
 CALL IGRFILLPATTERN(OUTLINE)

 END SUBROUTINE CREATEIPF_SELECTBOX

 !###======================================================================
 SUBROUTINE CREATEIPF1INIT()
 !###======================================================================
 IMPLICIT NONE
 LOGICAL :: LEX
 
 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_CREATEIPF,2).EQ.1)THEN
  CALL CREATEIPF1CLOSE()
  RETURN
 ENDIF

 CALL MAIN1INACTMODULE(ID_CREATEIPF)

 !## other module no closed, no approvement given
 IF(IDIAGERROR.EQ.1)RETURN

 CALL WMENUSETSTATE(ID_CREATEIPF,2,1)

 CALL WDIALOGLOAD(ID_DCREATEIPF,ID_DCREATEIPF)
 CALL POLYGON1INIT()
 CALL POLYGON1IMAGES(ID_DCREATEIPF)
 CALL POLYGON1FIELDS(ID_DCREATEIPF)
 CALL WDIALOGPUTIMAGE(ID_INFO,ID_ICONINFO)
 CALL WDIALOGPUTIMAGE(ID_SAVE,ID_ICONSAVE)
 CALL WDIALOGFIELDSTATE(ID_INFO,0)
 CALL WDIALOGFIELDSTATE(ID_SAVE,0)
 
 CALL WDIALOGSELECT(ID_DCREATEIPF); CALL WDIALOGSHOW(-0,100,0,2)

 END SUBROUTINE CREATEIPF1INIT

 !###======================================================================
 SUBROUTINE CREATEIPF1CLOSE()
 !###======================================================================
 IMPLICIT NONE

 IDIAGERROR=1

 CALL POLYGON1DRAWSHAPE(1,SHPNO); CALL POLYGON1CLOSE()
 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_CREATEIPF,2,0)
 CALL WDIALOGSELECT(ID_DCREATEIPF); CALL WDIALOGUNLOAD()

 IDIAGERROR=0

 END SUBROUTINE CREATEIPF1CLOSE

END MODULE MOD_CREATEIPF
