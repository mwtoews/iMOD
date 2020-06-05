!!  Copyright (C) Stichting Deltares, 2005-2020.
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
MODULE MOD_TOOLS_UTL

USE WINTERACTER
USE RESOURCE
USE MOD_PREF_PAR
USE MOD_IDF 
USE MOD_POLYGON_PAR
USE MOD_POLYGON_UTL
USE MOD_POLYGON_DRAW
USE MOD_IDF_PAR 
USE MOD_UTL 
USE IMODVAR
USE MOD_TOOLS_PAR

CONTAINS

 !###======================================================================
 SUBROUTINE TOOLS_UTL_GETPERIODS(FN,IPERIOD,IERROR)
 !###======================================================================
 !
 !## get period for mean,wbal computation
 !
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IERROR
 INTEGER,POINTER,DIMENSION(:,:) :: IPERIOD
 INTEGER,POINTER,DIMENSION(:,:) :: CPERIOD
 CHARACTER(LEN=*),INTENT(INOUT) :: FN
 INTEGER :: I,J,K,IOS,N
 INTEGER,DIMENSION(2) :: ID,IM

 ALLOCATE(IPERIOD(2,2))

 IERROR=1
 N     =0
 DO

  DO K=1,2

   J=0
   I=INDEX(FN,CHAR(45))            !-
   IF(I.LE.0)EXIT
   READ(FN(J+1:I-1),*,IOSTAT=IOS) ID(K)
   IF(IOS.NE.0)RETURN

   IF(K.EQ.1)J=INDEX(FN,CHAR(47))  !/
   IF(K.EQ.2)THEN
    J=INDEX(FN,CHAR(59))           !;
    IF(J.LE.0)J=LEN_TRIM(FN)+1
   ENDIF
   IF(J.LE.0)EXIT
   READ(FN(I+1:J-1),*,IOSTAT=IOS) IM(K)
   IF(IOS.NE.0)RETURN

   FN=FN(J+1:)

  ENDDO

  IF(IM(1).GT.12.OR.IM(1).LT.1.OR. &
     IM(2).GT.12.OR.IM(2).LT.1)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Given month lt 1 or gt 12.','Error')
   RETURN
  ENDIF
  IF(IM(1).GT.IM(2))THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'First given month greater than second given month.','Error')
   RETURN
  ENDIF
  IF(IM(1).EQ.IM(2).AND.ID(1).GT.ID(2))THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'First given date greater than second given date.','Error')
   RETURN
  ENDIF
  IF(ID(1).LT.1.OR.ID(2).LT.1)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Given day(s) lt 1.','Error')
   RETURN
  ENDIF

  IF(N+2.GT.SIZE(IPERIOD,1))THEN
   ALLOCATE(CPERIOD(N+2,2))
   CPERIOD(1:N,:)=IPERIOD(1:N,:)
   DEALLOCATE(IPERIOD)
   IPERIOD=>CPERIOD
  ENDIF

  N           =N+1
  IPERIOD(N,1)=ID(1)
  IPERIOD(N,2)=IM(1)
  N           =N+1
  IPERIOD(N,1)=ID(2)
  IPERIOD(N,2)=IM(2)

  IF(LEN_TRIM(FN).EQ.0)EXIT

 ENDDO

 IERROR=0

 END SUBROUTINE TOOLS_UTL_GETPERIODS

 !###======================================================================
 SUBROUTINE TOOLS_UTL_GETPERIODS_GXG(FN,IPERIOD,IERROR)
 !###======================================================================
 !
 !## get period for gxg-computation 
 !
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IERROR
 INTEGER,INTENT(OUT),DIMENSION(12,2) :: IPERIOD
 CHARACTER(LEN=*),INTENT(INOUT) :: FN
 INTEGER :: I,J,K,IOS
 INTEGER,DIMENSION(2) :: ID,IM

 IERROR=1

 DO

  DO K=1,2

   J=0
   I=INDEX(FN,CHAR(45))            !-
   IF(I.LE.0)EXIT
   READ(FN(J+1:I-1),*,IOSTAT=IOS) ID(K)
   IF(IOS.NE.0)RETURN

   IF(K.EQ.1)J=INDEX(FN,CHAR(47))  !/
   IF(K.EQ.2)THEN
    J=INDEX(FN,CHAR(59))           !;
    IF(J.LE.0)J=LEN_TRIM(FN)+1
   ENDIF
   IF(J.LE.0)EXIT
   READ(FN(I+1:J-1),*,IOSTAT=IOS) IM(K)
   IF(IOS.NE.0)RETURN

   FN=FN(J+1:)

  ENDDO

  IF(IM(1).GT.12.OR.IM(1).LT.1.OR. &
     IM(2).GT.12.OR.IM(2).LT.1)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Given month lt 1 or gt 12.','Error')
   RETURN
  ENDIF
  IF(IM(1).GT.IM(2))THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'First given month greater than second given month.','Error')
   RETURN
  ENDIF
  IF(IM(1).EQ.IM(2).AND.ID(1).GT.ID(2))THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'First given date greater than second given date.','Error')
   RETURN
  ENDIF
  IF(ID(1).LT.1.OR.ID(2).LT.1)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Given day(s) lt 1.','Error')
   RETURN
  ENDIF

  !## fill iperiod
  DO I=IM(1),IM(2)
   IF(I.EQ.IM(1))THEN
    IF(IM(1).EQ.IM(2))THEN
     IF(ID(1).LE.14.AND.ID(2).GE.14)IPERIOD(I,1)=1
     IF(ID(1).LE.28.AND.ID(2).GE.28)IPERIOD(I,2)=1
    ELSE
     IF(ID(1).LE.14)IPERIOD(I,1)=1
     IF(ID(1).LE.28)IPERIOD(I,2)=1
    ENDIF
   ENDIF
   IF(I.EQ.IM(2))THEN
    IF(ID(2).GE.14)IPERIOD(I,1)=1
    IF(ID(2).GE.28)IPERIOD(I,2)=1
   ENDIF
   IF(I.NE.IM(1).AND.I.NE.IM(2))THEN
    IPERIOD(I,1)=1
    IPERIOD(I,2)=1
   ENDIF
  END DO

  IF(LEN_TRIM(FN).EQ.0)EXIT

 ENDDO

 IERROR=0

 END SUBROUTINE TOOLS_UTL_GETPERIODS_GXG

 !###======================================================================
 SUBROUTINE TOOLS_UTL_FILLPOINTER(ISEL,IDF,IPIDF,NIP)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ISEL
 INTEGER,INTENT(OUT) :: NIP
 TYPE(IDFOBJ),INTENT(INOUT) :: IPIDF,IDF
 INTEGER :: IC1,IC2,IR1,IR2,ICOL,IROW,I,J,SHPI
 REAL(KIND=DP_KIND) :: X1,X2,Y1,Y2,XVAL,YVAL,IDFVALUE

 IF(ALLOCATED(IPLIST))DEALLOCATE(IPLIST)

 !## entire area
 IF(ISEL.EQ.1)THEN
  ALLOCATE(IPLIST(1))
  IPIDF%X  =1.0D0
  NIP      =1
  IPLIST(1)=INT(1,2)
 !## polygons
 ELSEIF(ISEL.EQ.2)THEN
  ALLOCATE(IPLIST(SHP%NPOL))
  IPIDF%X=0.0D0
  NIP    =0
  DO SHPI=1,SHP%NPOL
   IF(SHP%POL(SHPI)%IACT.EQ.1.AND.SHP%POL(SHPI)%N.GT.0)THEN
    NIP        =NIP+1
    IPLIST(NIP)=INT(SHPI,2)

    X1=MINVAL(SHP%POL(SHPI)%X(1:SHP%POL(SHPI)%N))
    X2=MAXVAL(SHP%POL(SHPI)%X(1:SHP%POL(SHPI)%N))
    Y1=MINVAL(SHP%POL(SHPI)%Y(1:SHP%POL(SHPI)%N))
    Y2=MAXVAL(SHP%POL(SHPI)%Y(1:SHP%POL(SHPI)%N))

    !## get ofset from xmin/ymin in the number of cells
    !## increase them in case of frf en fff computation
    CALL IDFIROWICOL(IPIDF,IR1,IC1,X1,Y2)
    CALL IDFIROWICOL(IPIDF,IR2,IC2,X2,Y1)
    
    IF(IC2.EQ.0)IC2=IPIDF%NCOL
    IF(IR2.EQ.0)IR2=IPIDF%NROW
    
    IC1=MAX(1,IC1)
    IC2=MIN(IC2,IPIDF%NCOL)
    IR1=MAX(1,IR1)
    IR2=MIN(IR2,IPIDF%NROW)

    DO IROW=IR1,IR2
     DO ICOL=IC1,IC2
      CALL IDFGETLOC(IPIDF,IROW,ICOL,XVAL,YVAL)
      IF(DBL_IGRINSIDESHAPE(XVAL,YVAL,SHP%POL(SHPI)).EQ.1)IPIDF%X(ICOL,IROW)=REAL(SHPI)
!      IF(DBL_IGRINSIDEPOLYGON(XVAL,YVAL,SHP%POL(SHPI)%X,SHP%POL(SHPI)%Y,SHP%POL(SHPI)%N).EQ.1)IPIDF%X(ICOL,IROW)=REAL(SHPI)
     ENDDO
    ENDDO
   ENDIF
  ENDDO

 !## idf
 ELSEIF(ISEL.EQ.3)THEN

  IPIDF%X=IPIDF%NODATA
  DO IROW=1,IPIDF%NROW
   DO ICOL=1,IPIDF%NCOL
    !## get x/y coordinates
    CALL IDFGETLOC(IPIDF,IROW,ICOL,XVAL,YVAL)
    !## get irow/icol coordinates
    CALL IDFIROWICOL(IDF,IR1,IC1,XVAL,YVAL)
    !## get idfvalue
    IDFVALUE=IDFGETVAL(IDF,IR1,IC1)
    IF(IDFVALUE.NE.IPIDF%NODATA)IPIDF%X(ICOL,IROW)=IDFVALUE
   ENDDO
  ENDDO
  CLOSE(IDF%IU)

  I=INT(MAXVAL(IPIDF%X))
  J=INT(MINVAL(IPIDF%X))
  DO IROW=1,IPIDF%NROW
   DO ICOL=1,IPIDF%NCOL
    IF(IPIDF%X(ICOL,IROW).NE.IPIDF%NODATA)THEN
     I=MIN(INT(IPIDF%X(ICOL,IROW)),I)
     J=MAX(INT(IPIDF%X(ICOL,IROW)),J)
    ENDIF
   ENDDO
  ENDDO

  ALLOCATE(IPLIST(J-I+1))
  NIP=0
  DO IROW=1,IPIDF%NROW
   DO ICOL=1,IPIDF%NCOL
    IF(IPIDF%X(ICOL,IROW).NE.IPIDF%NODATA)THEN
     DO I=1,NIP
      IF(INT(IPIDF%X(ICOL,IROW),2).EQ.IPLIST(I))EXIT
     ENDDO
     IF(I.GT.NIP)THEN
      NIP        =NIP+1
      IPLIST(NIP)=INT(IPIDF%X(ICOL,IROW),2)
     ENDIF
    ENDIF
   ENDDO
  ENDDO

 ENDIF

 CALL IDFFILLCOMMENT(IPIDF,'Units: dimensionless')
 IF(.NOT.IDFWRITE(IPIDF,TRIM(PREFVAL(1))//'\TMP\POINTER.IDF',1))THEN
 ENDIF

!## reshuffle pointer to fit iplist()
 DO IROW=1,IPIDF%NROW
  DO ICOL=1,IPIDF%NCOL
   DO I=1,NIP
    IF(IPLIST(I).EQ.INT(IPIDF%X(ICOL,IROW),2))THEN
     IPIDF%X(ICOL,IROW)=REAL(I)
     EXIT
    ENDIF
   ENDDO
   !## reset nodata value to zero!
   IF(IPIDF%X(ICOL,IROW).EQ.IPIDF%NODATA)IPIDF%X(ICOL,IROW)=0.0D0
  ENDDO
 ENDDO

 END SUBROUTINE TOOLS_UTL_FILLPOINTER

 !###======================================================================
 SUBROUTINE TOOLS_CLOSE()
 !###======================================================================
 IMPLICIT NONE

 IDIAGERROR=1

 CALL POLYGON1DRAWSHAPE(1,SHP%NPOL)
 CALL POLYGON1CLOSE()

 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(TOOLSID,2,0)

 CALL WDIALOGSELECT(ID_TOOLS)
 CALL WDIALOGUNLOAD()

 !## used in imod_utl: UTL_IMODFILLMENU
 IF(ALLOCATED(LISTNAME))DEALLOCATE(LISTNAME)
 IF(ALLOCATED(ILIST))DEALLOCATE(ILIST)
 IF(ALLOCATED(CLAY))DEALLOCATE(CLAY)
 IF(ALLOCATED(ILAY))DEALLOCATE(ILAY)
 IF(ALLOCATED(CYEAR))DEALLOCATE(CYEAR)
 IF(ALLOCATED(IYEAR))DEALLOCATE(IYEAR)

 IDIAGERROR=0

 END SUBROUTINE TOOLS_CLOSE
 
END MODULE MOD_TOOLS_UTL
