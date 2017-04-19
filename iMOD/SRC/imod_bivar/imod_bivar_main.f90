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
!!
MODULE MOD_BIVARIATE

USE WINTERACTER
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_UTL, ONLY : UTL_DIST

CONTAINS

 !###======================================================================
 SUBROUTINE BIVARIATE_INT(XD,YD,ZD,MD,IERROR,IDF)
 !###======================================================================
 IMPLICIT NONE 
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER,INTENT(IN) :: MD
 INTEGER,INTENT(OUT) :: IERROR
 REAL,INTENT(INOUT),DIMENSION(:),POINTER :: XD,YD,ZD
 !## bivariate interpolation
 INTEGER :: NX,NY,NZ,NIWK,NWK,I,J,ICOL,IROW,IMODE,ND,IOS
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IWK
 REAL,ALLOCATABLE,DIMENSION(:) :: WK,XI,YI
 REAL,ALLOCATABLE,DIMENSION(:,:) :: ZI
 
 IERROR=1
 
 CALL BIVARIATE_DATA(XD,YD,ZD,MD,I,IDF%NODATA); ND=I

 IF(ND.LE.3)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'The number of unique point should > 3!','Error')
  RETURN
 ENDIF

 NIWK=31*ND+IDF%NCOL*IDF%NROW; NWK=6*ND
 ALLOCATE(WK(NWK),XI(IDF%NCOL),YI(IDF%NROW),ZI(IDF%NCOL,IDF%NROW),IWK(NIWK),STAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'To many fixed points selected!','Error')
  RETURN
 ENDIF

 !## set up points for the rectangular network
 DO ICOL=1,IDF%NCOL; XI(ICOL)=IDF%XMIN+(ICOL-1)*IDF%DX+0.5*IDF%DX ; ENDDO
 DO IROW=1,IDF%NROW; YI(IROW)=IDF%YMAX-(IROW-1)*IDF%DY-0.5*IDF%DY ; ENDDO

 IMODE=1 !## first call (imode=1,2,3, see idsfft subroutine)
 CALL IDSFFT(IMODE,ND,XD,YD,ZD,IDF%NCOL,IDF%NROW,IDF%NCOL,XI,YI,ZI,IWK,WK)

 !## copy results to idf()%x
 IDF%X=ZI
  
 IF(ALLOCATED(WK))DEALLOCATE(WK); IF(ALLOCATED(XI))DEALLOCATE(XI)
 IF(ALLOCATED(YI))DEALLOCATE(YI); IF(ALLOCATED(ZI))DEALLOCATE(ZI)
 IF(ALLOCATED(IWK))DEALLOCATE(IWK)

 IERROR=0
 
 END SUBROUTINE BIVARIATE_INT  

 !###======================================================================
 SUBROUTINE BIVARIATE_DATA(XD,YD,ZD,MD,ND,NODATA)
 !###======================================================================
 IMPLICIT NONE 
 INTEGER,INTENT(IN) :: MD
 INTEGER,INTENT(OUT) :: ND
 REAL,INTENT(IN) :: NODATA
 REAL,INTENT(INOUT),DIMENSION(MD) :: XD,YD,ZD
 INTEGER :: I,J,IE,IS,N,XP,YP,ZP
 
  !## mark doubles with nodata and compute mean for those double points
 DO I=1,MD
  !## done allready
  IF(ZD(I).EQ.NODATA)CYCLE
  N=1; XP=XD(I); YP=YD(I); ZP=ZD(I)
  DO J=I+1,MD
   !## done allready
   IF(ZD(J).EQ.NODATA)CYCLE
   !## get distance between points - increase distance whenever points are intersected by fault
   IF(UTL_DIST(XD(I),YD(I),XD(J),YD(J)).LE.0.0)THEN
    N=N+1
    ZP=ZP+ZD(J)
    !## add point to existing point and turn location off
    XP=XP+XD(J); YP=YP+YD(J); ZD(J)=NODATA
   ENDIF
  ENDDO
  !## determine average spot
  XD(I)=XP/REAL(N); YD(I)=YP/REAL(N); ZD(I)=ZP/REAL(N)

 END DO
  
 !## eliminate doubles
 J=0
 DO I=1,MD
  IF(ZD(I).NE.NODATA)THEN
   J=J+1
   IF(J.NE.I)THEN
    XD(J)=XD(I); YD(J)=YD(I); ZD(J)=ZD(I)
   ENDIF
  ENDIF
 END DO
 ND=J
 
! !## sort x-coordinates for x, to skip double coordinates and mean values for those
! CALL SORTEM(1,MD,XD,2,YD,ZD,(/0.0/),(/0.0/),(/0.0/),(/0.0/),(/0.0/)) !,ZD,ZD,ZD,ZD,ZD)
!
! !## sort y-coordinates
! IS=1
! DO I=2,MD
!  IF(XD(I).NE.XD(I-1))THEN
!   IE=I-1
!   CALL SORTEM(IS,IE,YD,2,XD,ZD,(/0.0/),(/0.0/),(/0.0/),(/0.0/),(/0.0/)) !,ZD,ZD,ZD,ZD,ZD)
!   IS=I
!  ENDIF
! ENDDO
! !## sort last
! IE=I-1
! CALL SORTEM(IS,IE,YD,2,XD,ZD,(/0.0/),(/0.0/),(/0.0/),(/0.0/),(/0.0/)) !,ZD,ZD,ZD,ZD,ZD)
!
! J=1
! !## mark doubles with nodata
! DO I=2,MD
!  IF(XD(I).EQ.XD(I-1).AND.YD(I).EQ.YD(I-1))ZD(I)=NODATA
! END DO
!
! J=0
! DO I=1,MD
!  IF(ZD(I).NE.NODATA)THEN
!   J=J+1
!   IF(J.NE.I)THEN
!    XD(J)=XD(I); YD(J)=YD(I); ZD(J)=ZD(I)
!   ENDIF
!  ENDIF
! END DO
! ND=J

 END SUBROUTINE BIVARIATE_DATA
 
END MODULE MOD_BIVARIATE
