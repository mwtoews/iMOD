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
MODULE MOD_IR_GEN

USE WINTERACTER
USE RESOURCE
USE MOD_IR_PAR
USE MOD_POLYGON_PAR
USE MODPLOT, ONLY : MPW
USE MOD_UTL, ONLY : INVERSECOLOUR,ITOS,UTL_WAITMESSAGE,UTL_GETUNIT,UTL_MESSAGEHANDLE,UTL_CAP,IDFPLOT1BITMAP, &
    IDFPLOT2BITMAP,UTL_INSIDEPOLYGON
USE MOD_POLYGON_PAR
USE MOD_POLYGON, ONLY : POLYGON1FIELDS
USE MOD_POLYGON_DRAW, ONLY : POLYGON1DRAWSHAPE
USE MOD_IPF, ONLY : IPFPLOTPOINT
USE MOD_OSD, ONLY : OSD_OPEN

INTEGER,PARAMETER :: MAXDV=5
INTEGER,DIMENSION(MAXDV) :: IDV
DATA IDV/ID_VIEWBC1,ID_VIEWBC2,ID_VIEWBC3,ID_VIEWBC4,ID_VIEWBC5/

CONTAINS

 !###======================================================================
 SUBROUTINE IR1GENREAD(IBC,FNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBC
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: IU,IOS,I1,I2,I
 REAL :: X,Y

 NULLIFY(BC(IBC)%X)
 NULLIFY(BC(IBC)%Y)
 NULLIFY(BC(IBC)%IXY)
 NULLIFY(BC(IBC)%XMIN)
 NULLIFY(BC(IBC)%XMAX)
 NULLIFY(BC(IBC)%YMIN)
 NULLIFY(BC(IBC)%YMAX)
 IF(FNAME.EQ.'')RETURN

 IF(INDEX(UTL_CAP(FNAME,'U'),'.GEN',.TRUE.).GT.0)BC(IBC)%ITYPE=0
 IF(INDEX(UTL_CAP(FNAME,'U'),'.IPF',.TRUE.).GT.0)BC(IBC)%ITYPE=1
 BC(IBC)%BCNAME=FNAME

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,FORM='FORMATTED',ACTION='READ,DENYWRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not open/read the optional file: '//CHAR(13)// &
    TRIM(FNAME),'Warning')
  RETURN
 ENDIF

 CALL UTL_MESSAGEHANDLE(0)
 CALL WINDOWSELECT(0)
 CALL WINDOWOUTSTATUSBAR(4,'Scanning '//TRIM(FNAME)//'...')

 IF(BC(IBC)%ITYPE.EQ.0)THEN

  !## count number of polygons
  BC(IBC)%NGEN=0
  BC(IBC)%NCRD=0

  DO
   READ(IU,*,IOSTAT=IOS)
   IF(IOS.NE.0)EXIT
   BC(IBC)%NGEN=BC(IBC)%NGEN+1
   DO
    READ(IU,*,IOSTAT=IOS) X,Y
    IF(IOS.NE.0)EXIT
    BC(IBC)%NCRD=BC(IBC)%NCRD+1
   END DO
  ENDDO
  REWIND(IU)

  !## allocate memory
  ALLOCATE(BC(IBC)%X(BC(IBC)%NCRD),BC(IBC)%Y(BC(IBC)%NCRD))
  ALLOCATE(BC(IBC)%IXY(BC(IBC)%NGEN+1))
  ALLOCATE(BC(IBC)%XMIN(BC(IBC)%NGEN),BC(IBC)%YMIN(BC(IBC)%NGEN))
  ALLOCATE(BC(IBC)%XMAX(BC(IBC)%NGEN),BC(IBC)%YMAX(BC(IBC)%NGEN))

  CALL WINDOWOUTSTATUSBAR(4,'Reading '//TRIM(ITOS(BC(IBC)%NGEN))//' polygon ...')

  !## read gen-file and store into memory
  BC(IBC)%NGEN=0
  BC(IBC)%NCRD=0
  DO
   READ(IU,*,IOSTAT=IOS)
   IF(IOS.NE.0)EXIT
   BC(IBC)%NGEN =BC(IBC)%NGEN+1
   I1           =BC(IBC)%NCRD+1
   BC(IBC)%IXY(BC(IBC)%NGEN)=I1
   DO
    READ(IU,*,IOSTAT=IOS) X,Y
    IF(IOS.NE.0)EXIT
    BC(IBC)%NCRD=BC(IBC)%NCRD+1
    BC(IBC)%X(BC(IBC)%NCRD)=X
    BC(IBC)%Y(BC(IBC)%NCRD)=Y
   END DO
   I2            =BC(IBC)%NCRD
   BC(IBC)%XMIN(BC(IBC)%NGEN)=MINVAL(BC(IBC)%X(I1:I2))
   BC(IBC)%YMIN(BC(IBC)%NGEN)=MINVAL(BC(IBC)%Y(I1:I2))
   BC(IBC)%XMAX(BC(IBC)%NGEN)=MAXVAL(BC(IBC)%X(I1:I2))
   BC(IBC)%YMAX(BC(IBC)%NGEN)=MAXVAL(BC(IBC)%Y(I1:I2))
  ENDDO
  BC(IBC)%IXY(BC(IBC)%NGEN+1)=I2+1

 ELSEIF(BC(IBC)%ITYPE.EQ.1)THEN

  READ(IU,*,IOSTAT=IOS) BC(IBC)%NGEN !## number of points
  READ(IU,*,IOSTAT=IOS) BC(IBC)%NCRD !## number of columns
  DO I=1,BC(IBC)%NCRD
   READ(IU,*,IOSTAT=IOS)
  END DO
  READ(IU,*,IOSTAT=IOS)

  !## allocate memory
  ALLOCATE(BC(IBC)%X(BC(IBC)%NGEN),BC(IBC)%Y(BC(IBC)%NGEN))

  CALL WINDOWOUTSTATUSBAR(4,'Reading '//TRIM(ITOS(BC(IBC)%NCRD))//' points ...')

  DO I=1,BC(IBC)%NGEN
   READ(IU,*,IOSTAT=IOS) BC(IBC)%X(I),BC(IBC)%Y(I)
  END DO

 ENDIF

 CLOSE(IU)

 CALL WINDOWOUTSTATUSBAR(4,'')
 CALL UTL_MESSAGEHANDLE(1)

 END SUBROUTINE IR1GENREAD

 !###======================================================================
 SUBROUTINE IR1GENCOPY(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID!,IBC
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,IBC

 !## remove selected polygons
 CALL POLYGON1DRAWSHAPE(1,SHPNO)
 !## turn all polygons off
 SHPIACT=0
 CALL POLYGON1DRAWSHAPE(1,SHPNO)

 ALLOCATE(XPOL(100),YPOL(100))

 CALL IGRFILLPATTERN(SOLID)
 IF(ID.EQ.ID_DIR_PMTAB1TAB2)CALL IGRCOLOURN(INVERSECOLOUR(ICLRTARGET))   !## target
 IF(ID.EQ.ID_DIR_PMTAB2TAB2)CALL IGRCOLOURN(INVERSECOLOUR(ICLRMEASURE))  !## measure

 CALL WCURSORSHAPE(ID_CURSORPOLYGON)

 IBC=1

 ALLOCATE(ISELGEN(BC(IBC)%NGEN))
 ISELGEN=0
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)

   CASE (MOUSEMOVE)
    !## highlight polygon
    CALL IR1GENHIGHLIGHT(MESSAGE%GX,MESSAGE%GY,IBC)

   CASE (MOUSEBUTDOWN)
    SELECT CASE (MESSAGE%VALUE1)
     !## left mouse button pressed
     CASE (1)
      CALL IR1GENCOPYPOLYGON(ID,IBC)
      EXIT
     !## right mouse button pressed
     CASE (3)
      EXIT
    END SELECT

  END SELECT

 END DO

 CALL WCURSORSHAPE(CURARROW)

 !## remove previous one, if not nul
 CALL IR1GENDRAWSHAPES(IBC)

 DEALLOCATE(XPOL,YPOL,ISELGEN)

 !## polygon accepted
 IF(MESSAGE%VALUE1.EQ.1)THEN
  CALL POLYGON1FIELDS(ID)
  CALL POLYGON1DRAWSHAPE(SHPNO,SHPNO)
 ENDIF

 END SUBROUTINE IR1GENCOPY

 !###======================================================================
 SUBROUTINE IR1GENDRAWSHAPES(IBC)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBC
 INTEGER :: I,I1,I2,N

 CALL IDFPLOT1BITMAP()
 CALL IGRPLOTMODE(MODEXOR)
 DO I=1,BC(IBC)%NGEN
  IF(ISELGEN(I).EQ.INT(1,1))THEN
   I1=BC(IBC)%IXY(I)
   I2=BC(IBC)%IXY(I+1)
   N =I2-I1
   CALL IGRPOLYGONCOMPLEX(BC(IBC)%X(I1:N),BC(IBC)%Y(I1:N),N)
  ENDIF
 ENDDO
 CALL IDFPLOT2BITMAP()
 CALL IGRPLOTMODE(MODECOPY)

 END SUBROUTINE IR1GENDRAWSHAPES

 !###======================================================================
 SUBROUTINE IR1GENCOPYPOLYGON(ID,IBC)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID,IBC
 REAL,PARAMETER :: DCRIT=15.0  !## points closer than 5 meter will be removed!
 INTEGER :: I,J,I1,I2,N,ISEL
 REAL :: D,DX,DY

 IF(SUM(ISELGEN).EQ.0)RETURN

 DO ISEL=1,BC(IBC)%NGEN
  IF(ISELGEN(ISEL).EQ.INT(1,1))EXIT
 END DO

 I1=BC(IBC)%IXY(ISEL)
 I2=BC(IBC)%IXY(ISEL+1)
 N =I2-I1

 SHPNO           =SHPNO+1
 SHPTYPE(SHPNO)  =ID_POLYGON
 SHPIACT(SHPNO)  =1
 IF(ID.EQ.ID_DIR_PMTAB1TAB2)SHPCOLOR(SHPNO) =ICLRTARGET  !## target
 IF(ID.EQ.ID_DIR_PMTAB2TAB2)SHPCOLOR(SHPNO) =ICLRMEASURE !## measure
 SHPNAME(SHPNO)  ='Peilvak_'//TRIM(ITOS(ISEL))
 SHPWIDTH(SHPNO) =2

 !## analyse polygon, remove double/close points
 J=I1
 !## set first point
 N=1
 SHPXC(N,SHPNO)=BC(IBC)%X(I1)
 SHPYC(N,SHPNO)=BC(IBC)%Y(I1)
 DO I=I1+1,I2-1
  DX=BC(IBC)%X(I)-BC(IBC)%X(J)
  DY=BC(IBC)%Y(I)-BC(IBC)%Y(J)
  D =DX**2.0+DY**2.0
  IF(D.NE.0.0)D=SQRT(D)
  IF(D.GT.DCRIT)THEN
   N=N+1
   SHPXC(N,SHPNO)=BC(IBC)%X(I)
   SHPYC(N,SHPNO)=BC(IBC)%Y(I)
   J=I
  ENDIF
 END DO
 !## remove double point on begin/end
 IF(SHPXC(1,SHPNO).EQ.SHPXC(N,SHPNO).AND. &
    SHPYC(1,SHPNO).EQ.SHPYC(N,SHPNO))N=N-1

 SHPNCRD(SHPNO)=N

 CALL WDIALOGSELECT(ID)
 CALL WDIALOGPUTMENU(IDF_MENU1,SHPNAME,SHPNO,SHPIACT)

 END SUBROUTINE IR1GENCOPYPOLYGON

 !###======================================================================
 SUBROUTINE IR1GENHIGHLIGHT(XC,YC,IBC)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBC
 REAL,INTENT(IN) :: XC,YC
 INTEGER :: I,I1,I2,N

 IF(BC(IBC)%NGEN.LE.0)RETURN
 IF(ABS(XC-XCMOUSE).LE.0.01.AND.ABS(YC-YCMOUSE).LE.0.01)RETURN

 XCMOUSE=XC
 YCMOUSE=YC

 !## remove previous one, if not nul
 CALL IR1GENDRAWSHAPES(IBC)

 ISELGEN=INT(0,1)
 DO I=1,BC(IBC)%NGEN

  IF(BC(IBC)%XMIN(I).LT.XC.AND.BC(IBC)%XMAX(I).GT.XC.AND. &
     BC(IBC)%YMIN(I).LT.YC.AND.BC(IBC)%YMAX(I).GT.YC)THEN

   I1=BC(IBC)%IXY(I)
   I2=BC(IBC)%IXY(I+1)
   N =I2-I1

   IF(UTL_INSIDEPOLYGON(XC,YC,BC(IBC)%X(I1:N),BC(IBC)%Y(I1:N),N).EQ.1)ISELGEN(I)=INT(1,1)

  END IF
 END DO

 !## draw new ones
 CALL IR1GENDRAWSHAPES(IBC)

 END SUBROUTINE IR1GENHIGHLIGHT

 !###======================================================================
 SUBROUTINE IR1GENCLOSESTPOINT(J1,J2,I1,I2,IPOS,IBC)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: J1,J2,I1,I2,IBC
 INTEGER,INTENT(OUT) :: IPOS
 REAL ::  D,MIND
 INTEGER :: I,J

 MIND=10.0E10
 DO J=J1,J2
  DO I=I1,I2
   D=((BC(IBC)%X(J)-BC(IBC)%X(I))**2.0+(BC(IBC)%Y(J)-BC(IBC)%Y(I))**2.0)
   IF(D.NE.0.0)D=SQRT(D)
   IF(D.LT.MIND)THEN
    D   =MIND
    IPOS=I
   ENDIF
  END DO
 END DO

 END SUBROUTINE IR1GENCLOSESTPOINT

 !###======================================================================
 SUBROUTINE IR1GENDRAW()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,I1,I2,N,IRAT,IRAT1,IBC

 IF(NBC.LE.0)RETURN

 CALL IDFPLOT1BITMAP()

 CALL IGRLINEWIDTH(1)
 CALL IGRFILLPATTERN(OUTLINE)
 CALL IGRCOLOURN(WRGB(50,50,50))

 IRAT1=0
 DO IBC=1,NBC

  CALL WINDOWSELECT(IRWIN)
  IF(WMENUGETSTATE(IDV(IBC),2).EQ.1)THEN

   !## polygons
   IF(BC(IBC)%ITYPE.EQ.0)THEN

    DO I=1,BC(IBC)%NGEN
     IF(BC(IBC)%XMIN(I).LT.MPW%XMAX.AND.BC(IBC)%XMAX(I).GT.MPW%XMIN.AND. &
        BC(IBC)%YMIN(I).LT.MPW%YMAX.AND.BC(IBC)%YMAX(I).GT.MPW%YMIN)THEN
      I1=BC(IBC)%IXY(I)
      I2=BC(IBC)%IXY(I+1)
      N =I2-I1
      CALL IGRPOLYGONCOMPLEX(BC(IBC)%X(I1:N),BC(IBC)%Y(I1:N),N)
     END IF
     CALL WINDOWSELECT(0)
     CALL UTL_WAITMESSAGE(IRAT,IRAT1,I,BC(IBC)%NGEN,'Drawing polygons ...')
    END DO

   !## points
   ELSEIF(BC(IBC)%ITYPE.EQ.1)THEN

    DO I=1,BC(IBC)%NGEN
     CALL IPFPLOTPOINT(BC(IBC)%X(I),BC(IBC)%Y(I),IBC,1.0)!MP(IPLOT)%SYMBOL,MP(IPLOT)%THICKNESS)
    END DO

   ENDIF

  ENDIF

 END DO
 CALL WINDOWSELECT(0)
 CALL WINDOWOUTSTATUSBAR(4,'')

 CALL IDFPLOT2BITMAP()

 END SUBROUTINE IR1GENDRAW

END MODULE MOD_IR_GEN

