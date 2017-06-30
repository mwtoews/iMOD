!!  Copyright (C) Stichting Deltares, 2005-2017.
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
MODULE MOD_POLYGON_DRAW

USE WINTERACTER
USE RESOURCE
USE MOD_POLYGON_PAR
USE MOD_UTL, ONLY : INVERSECOLOUR,IDFPLOT1BITMAP,IDFPLOT2BITMAP,ITOS
USE MODPLOT, ONLY : MPW

CONTAINS

 !###======================================================================
 SUBROUTINE POLYGON1DRAWSHAPE(I1,I2)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I1,I2

 CALL IDFPLOT1BITMAP()
 CALL IGRPLOTMODE(MODEXOR)

 IF(I1.GT.0.AND.I2.GT.0)THEN
  !## draw actual shapes
  DO SHPI=I1,I2
   IF(SHPNCRD(SHPI).GT.0)THEN
    SHPCOLOR(SHPI)=INVERSECOLOUR(SHPCOLOR(SHPI))
    CALL POLYGON1PLOTSHAPE()
    SHPCOLOR(SHPI)=INVERSECOLOUR(SHPCOLOR(SHPI))
   ENDIF
  ENDDO
 ENDIF

 !## plot ysel if available
 CALL POLYGON1DRAWYSEL()

 CALL IDFPLOT2BITMAP()
 CALL IGRPLOTMODE(MODECOPY)
 CALL IGRFILLPATTERN(OUTLINE)
 CALL IGRLINETYPE(SOLIDLINE)

 END SUBROUTINE POLYGON1DRAWSHAPE

 !###======================================================================
 SUBROUTINE POLYGON1PLOTSHAPE()
 !###======================================================================
 IMPLICIT NONE
 REAL :: DX,X1,X2,Y1,Y2,X,Y
 INTEGER :: I
 
 IF(SHPNCRD(SHPI).LE.0)RETURN

 CALL WGROFONTSWISS()

 CALL IGRCOLOURN(SHPCOLOR(SHPI))
 CALL IGRFILLPATTERN(OUTLINE)
 CALL IGRLINETYPE(SOLIDLINE)
 CALL IGRLINEWIDTH(SHPWIDTH(SHPI))

 DX=SQRT((MPW%XMAX-MPW%XMIN)**2.0+(MPW%YMAX-MPW%YMIN)**2.0)/500.0

 SELECT CASE (SHPTYPE(SHPI))
  CASE (ID_RECTANGLE)
   X1=MIN(SHPXC(1,SHPI),SHPXC(2,SHPI))
   X2=MAX(SHPXC(1,SHPI),SHPXC(2,SHPI))
   Y1=MIN(SHPYC(1,SHPI),SHPYC(2,SHPI))
   Y2=MAX(SHPYC(1,SHPI),SHPYC(2,SHPI))
   IF(X1.EQ.X2)X2=X2+1; IF(Y1.EQ.Y2)Y2=Y2+1
   CALL IGRRECTANGLE(X1,Y1,X2,Y2)
   X=(X1+X2)/2.0; Y=(Y1+Y2)/2.0
!   SHPXC(3,SHPI)=SHPXC(2,SHPI)
!   SHPXC(4,SHPI)=SHPXC(1,SHPI)
!   SHPYC(3,SHPI)=SHPYC(2,SHPI)
!   SHPYC(4,SHPI)=SHPYC(1,SHPI)
  CASE (ID_POLYGON)
   !##draw lines
   IF(SHPNCRD(SHPI).EQ.2)THEN
    CALL IGRPOLYLINE(SHPXC(:,SHPI),SHPYC(:,SHPI),SHPNCRD(SHPI))
   ELSE
    CALL IGRPOLYGONSIMPLE(SHPXC(:,SHPI),SHPYC(:,SHPI),SHPNCRD(SHPI))
   ENDIF
   X=SUM(SHPXC(1:SHPNCRD(SHPI),SHPI))/REAL(SHPNCRD(SHPI))
   Y=SUM(SHPYC(1:SHPNCRD(SHPI),SHPI))/REAL(SHPNCRD(SHPI))
  CASE (ID_POINT)
   DO I=1,SHPNCRD(SHPI); CALL IGRCIRCLE(SHPXC(I,SHPI),SHPYC(I,SHPI),DX*2.0); END DO
   X=SHPXC(1,SHPI); Y=SHPYC(1,SHPI)
  CASE (ID_LINE)
   CALL IGRPOLYLINE(SHPXC(:,SHPI),SHPYC(:,SHPI),SHPNCRD(SHPI))
   X=SHPXC(1,SHPI); Y=SHPYC(1,SHPI)
 END SELECT

 CALL IGRLINEWIDTH(1)

 IF(SHPIACT(SHPI).EQ.0)RETURN

 !##draw shapename
 CALL WGRTEXTFONT(FFSOFTWARE,ISTYLE=FSBOLD,WIDTH=0.5*0.013,HEIGHT=0.5*0.033)
 CALL WGRTEXTORIENTATION(ALIGNLEFT)
 CALL WGRTEXTSTRING(X,Y,TRIM(SHPNAME(SHPI)))

 !## draw boxes
 DO I=1,SHPNCRD(SHPI)
  CALL IGRRECTANGLE(SHPXC(I,SHPI)-DX,SHPYC(I,SHPI)-DX,SHPXC(I,SHPI)+DX,SHPYC(I,SHPI)+DX)
 END DO

 END SUBROUTINE POLYGON1PLOTSHAPE

 !###======================================================================
 SUBROUTINE POLYGON1DRAWYSEL()
 !###======================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: MYSEL=100000
 INTEGER :: I

 IF(.NOT.ALLOCATED(SELIDF))RETURN
 IF(.NOT.LPLOTYSEL)RETURN

 IF(SELIDF(1)%NTHREAD.GT.MYSEL)THEN
  CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to draw '//TRIM(ITOS(SELIDF(1)%NTHREAD))//' selected cell?'//CHAR(13)// &
      'Click No to skip drawing these cells, click YES to draw them.'//CHAR(13)//'Be aware in that case, it can take a while.','Question')
  IF(WINFODIALOG(4).NE.1)THEN; LPLOTYSEL=.FALSE.; RETURN; ENDIF
 ENDIF
 
 CALL IDFPLOT1BITMAP()
 CALL IGRPLOTMODE(MODEXOR)
 CALL IGRCOLOURN(WRGB(255,255,255))
 CALL IGRFILLPATTERN(SOLID)

 DO I=1,SELIDF(1)%NTHREAD
  CALL POLYGON1PLOTYSEL(I)
 ENDDO

 CALL IDFPLOT2BITMAP()
 CALL IGRPLOTMODE(MODECOPY)

 END SUBROUTINE POLYGON1DRAWYSEL

 !###======================================================================
 SUBROUTINE POLYGON1PLOTYSEL(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I
 INTEGER :: IROW,ICOL

 IF(SELIDF(1)%IEQ.EQ.0)THEN
  CALL IGRRECTANGLE(SELIDF(1)%XMIN+(SELIDF(1)%YSEL(1,I)-1)*SELIDF(1)%DX, &
                    SELIDF(1)%YMAX-(SELIDF(1)%YSEL(2,I)-1)*SELIDF(1)%DY, &
                    SELIDF(1)%XMIN+ SELIDF(1)%YSEL(1,I)   *SELIDF(1)%DX, &
                    SELIDF(1)%YMAX- SELIDF(1)%YSEL(2,I)   *SELIDF(1)%DY)
 ELSEIF(SELIDF(1)%IEQ.EQ.1)THEN
  ICOL=INT(SELIDF(1)%YSEL(1,I))
  IROW=INT(SELIDF(1)%YSEL(2,I))
  CALL IGRRECTANGLE(SELIDF(1)%SX(ICOL-1),SELIDF(1)%SY(IROW-1), &
                    SELIDF(1)%SX(ICOL)  ,SELIDF(1)%SY(IROW))
 ENDIF

 END SUBROUTINE POLYGON1PLOTYSEL

END MODULE MOD_POLYGON_DRAW


