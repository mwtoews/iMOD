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
MODULE MOD_CREATEIDF

USE WINTERACTER
USE RESOURCE

USE IMODVAR
USE MODPLOT
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_POLYGON_PAR
USE MOD_POLYGON, ONLY : POLYGON1MAIN
USE MOD_POLYGON_DRAW, ONLY : POLYGON1DRAWSHAPE
USE MOD_POLYGON_UTL, ONLY : POLYGON1INIT,POLYGON1CLOSE,POLYGON1FIELDS,POLYGON1IMAGES
USE MOD_IDFEDIT, ONLY : IDFEDITGETLOGICAL
USE MOD_INTERSECT, ONLY : INTERSECT_EQUI,INTERSECT_DEALLOCATE
USE MOD_INTERSECT_PAR, ONLY : XA,YA,XA_DUMMY,YA_DUMMY,LN
USE MOD_UTL, ONLY : UTL_GETUNIT,ITOS,UTL_IDFSNAPTOGRID,UTL_WAITMESSAGE,UTL_MESSAGEHANDLE,UTL_CAP,NEWLINE, &
   UTL_WSELECTFILE,UTL_INSIDEPOLYGON,UTL_GENLABELSGET,VAR,IV
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFREAD,IDFWRITE,IDFALLOCATEX,IDFDEALLOCATE,IDFCOPY,IDFIROWICOL,IDFGETLOC, &
                    IDFNULLIFY,IDFFILLCOMMENT,IDFALLOCATESXY
USE MOD_IPF, ONLY : IPFALLOCATE,IPFREAD2,IPFDEALLOCATE
USE MOD_IPF_PAR
USE MOD_IFF, ONLY : UTL_GETUNITIFF,IFF,IFFPLOT_GETIFFVAL
USE MOD_KRIGING, ONLY : KRIGING_MAIN,KRIGING_VARIOGRAM,KRIGINGSETTINGS
USE MOD_BIVARIATE, ONLY : BIVARIATE_INT
USE MOD_SOLID_PCG, ONLY : SOLID_PCGINT,MXITER1,MXITER2,HCLOSE,RCLOSE,ITIGHT,MICNVG,RELAX,IDAMPING,FTIGHT
USE MOD_PCG, ONLY : PCGSETTINGS
USE MOD_GENPLOT, ONLY : GENDATAGRID
USE MOD_PROFILE_UTL, ONLY : PROFILE_PLOTGRAPH
USE IMOD

INTEGER,PRIVATE :: TOOLSID
REAL,PRIVATE :: CS,XMIN,YMIN,XMAX,YMAX,NODATA
INTEGER,PRIVATE :: NCOL,NROW
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:),PRIVATE :: X,NX
CHARACTER(LEN=256),PRIVATE :: IPFNAME,IFFNAME
INTEGER,PRIVATE :: IX,IY,IZ

CHARACTER(LEN=256),PRIVATE :: GENFNAME
INTEGER,PRIVATE :: MINP=10,KTYPE=2,PNTSEARCH=0,IEXPVARIOGRAM=0
REAL,PRIVATE :: RANGE=3000.0,SILL=30.0,NUGGET=0

CONTAINS

 !###======================================================================
 SUBROUTINE CREATEIDF1MAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE
 CHARACTER(LEN=256) :: TMPNAME

 CALL WDIALOGSELECT(MESSAGE%WIN)

 SELECT CASE (MESSAGE%WIN)

  CASE (ID_DCREATEIDF)
   SELECT CASE(ITYPE)
    !## changes in coordinates
    CASE (FIELDCHANGED)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (IDF_STRING1)
       CALL CREATEIDF1FIELDS()
     END SELECT

    CASE(PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (IDF_EXTENT1)
       CALL CREATEIDF1GETCRD(0)  !## automatically
      CASE (IDF_EXTENT2)
       CALL CREATEIDF1GETCRD(-1) !## automatically

      CASE (IDHELP)
       CALL IMODGETHELP('3.2.1','EMO.CreateIDF') 
      CASE (IDCANCEL)
       CALL CREATEIDF1CLOSE()
      CASE (ID_PROPERTIES)
       CALL CREATEIDF1SETTINGS()
      CASE (IDOK)
       CALL CREATEIDF1APPLY()

     END SELECT
   END SELECT

  !## ipf's
  CASE (ID_DCREATEIDFTAB1)
   SELECT CASE(ITYPE)
    CASE (FIELDCHANGED)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (IDF_STRING1)
       CALL CREATEIDF1FIELDS()
     END SELECT

    CASE(PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (ID_OPEN)
       TMPNAME=''
       IF(UTL_WSELECTFILE('iMOD Point File (*.ipf)|*.ipf|',&
                    LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT+MULTIFILE,TMPNAME,&
                    'Load iMOD Point File (*.ipf)'))THEN
        IPFNAME=TMPNAME
        IF(CREATEIDF1READATTRIB())THEN
         CALL WDIALOGPUTSTRING(IDF_STRING1,IPFNAME)
         CALL CREATEIDF1FIELDS()
        ENDIF
       ENDIF

     END SELECT
   END SELECT

  !## gen's
  CASE (ID_DCREATEIDFTAB2)

   !## check polygon actions
   IACTSHAPES=(/3,1,1,3,1,3/)
   IF(GENFNAME.EQ.'')THEN
    CALL POLYGON1MAIN(ITYPE,MESSAGE,IDAT=1)
   ELSE
    CALL POLYGON1MAIN(ITYPE,MESSAGE,IDAT=1,GENFNAME=GENFNAME)
   ENDIF

   !## automatically set "GEN extract" after loading gen-file
   IF(SHPNO.GT.0)THEN
    IF(ITYPE.EQ.PUSHBUTTON.AND.MESSAGE%VALUE1.EQ.ID_LOADSHAPE)CALL CREATEIDF1GETCRD(0)
   ENDIF
     
   CALL CREATEIDF1FIELDS()
   SELECT CASE (MESSAGE%VALUE2)
    !## read/show current data from memory!     
    CASE(ID_INFO)
     CALL GENDATAGRID('')
   END SELECT
   
  !## iff's
  CASE (ID_DCREATEIDFTAB3)
   SELECT CASE(ITYPE)
    CASE (FIELDCHANGED)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (IDF_STRING1,IDF_CHECK1)
       CALL CREATEIDF1FIELDS()
     END SELECT

    CASE(PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
!      CASE (IDF_EXTENT1)
!       CALL CREATEIDF1GETCRD(0) !## automatically

      CASE (ID_OPEN)
       TMPNAME=''
       IF(UTL_WSELECTFILE('iMOD Flowline File (*.iff)|*.iff|',&
                    LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT+MULTIFILE,TMPNAME,&
                    'Load iMOD Flowline File (*.iff)'))THEN
        IFFNAME=TMPNAME
        IF(CREATEIDF1READATTRIB())THEN
         CALL WDIALOGPUTSTRING(IDF_STRING1,IFFNAME)
         CALL CREATEIDF1FIELDS()
        ENDIF
       ENDIF

     END SELECT
   END SELECT

 END SELECT

 END SUBROUTINE CREATEIDF1MAIN

 !###======================================================================
 SUBROUTINE CREATEIDF1SETTINGS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IINT
 
 CALL WDIALOGSELECT(ID_DCREATEIDF)
 CALL WDIALOGGETMENU(IDF_MENU1,IINT)
 SELECT CASE (IINT)
  CASE (1) !## simple
  CASE (2) !## bivar
  CASE (3) !## pcg
   CALL  PCGSETTINGS(MXITER1,MXITER2,HCLOSE,RCLOSE,ITIGHT,MICNVG,RELAX,IDAMPING,FTIGHT)
  CASE (4)   !## variogram
  CASE (5,6) !## kriging
   CALL KRIGINGSETTINGS(MINP,KTYPE,RANGE,SILL,NUGGET,PNTSEARCH)
 END SELECT
 
 END SUBROUTINE
 
 !###======================================================================
 LOGICAL FUNCTION CREATEIDF1READIPF()
 !###======================================================================
 IMPLICIT NONE
 LOGICAL :: LEX

 CREATEIDF1READIPF=.FALSE.

! CALL WDIALOGSELECT(ID_DCREATEIDFTAB1)
! IF(WINFODIALOGFIELD(IDF_MENU1,FIELDSTATE).EQ.1)CALL WDIALOGGETMENU(IDF_MENU1,IX)
! IF(WINFODIALOGFIELD(IDF_MENU2,FIELDSTATE).EQ.1)CALL WDIALOGGETMENU(IDF_MENU2,IY)
! IF(WINFODIALOGFIELD(IDF_MENU3,FIELDSTATE).EQ.1)CALL WDIALOGGETMENU(IDF_MENU3,IZ)

 NIPF=1

 IF(ALLOCATED(IPF))THEN
  LEX=.FALSE.
  !## check whether information for current ipf is already in memory
  IF(UTL_CAP(IPF(1)%FNAME,'U').NE.UTL_CAP(IPFNAME,'U'))THEN
   LEX=.TRUE.
  ELSE
   IF(IX.NE.IPF(1)%XCOL)LEX=.TRUE.
   IF(IY.NE.IPF(1)%YCOL)LEX=.TRUE.
   IF(IZ.NE.IPF(1)%ZCOL)LEX=.TRUE.
  ENDIF
 ELSE
  LEX=.TRUE.
 ENDIF

 IF(LEX)THEN
  CALL IPFALLOCATE()
  IPF(1)%FNAME=IPFNAME
  IPF(1)%XCOL =IX !1
  IPF(1)%YCOL =IY !2
  IPF(1)%ZCOL =IZ !3
  IPF(1)%Z2COL=IZ !3
  IPF(1)%QCOL =IZ !3
  IF(.NOT.IPFREAD2(1,1,1))RETURN
 ENDIF

 CREATEIDF1READIPF=.TRUE.

 END FUNCTION CREATEIDF1READIPF

 !###======================================================================
 LOGICAL FUNCTION CREATEIDF1READATTRIB()
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=25),DIMENSION(:),ALLOCATABLE :: ATTRIB
 INTEGER :: IU,I,N

 CREATEIDF1READATTRIB=.FALSE.

 IF(TOOLSID.EQ.ID_CREATEIDF_IPF)THEN

  IF(.NOT.CREATEIDF1READIPF())RETURN
  
  CALL WDIALOGSELECT(ID_DCREATEIDFTAB1)
  CALL WDIALOGPUTMENU(IDF_MENU1,IPF(1)%ATTRIB,IPF(1)%NCOL,IPF(1)%XCOL)
  CALL WDIALOGPUTMENU(IDF_MENU2,IPF(1)%ATTRIB,IPF(1)%NCOL,IPF(1)%YCOL)
  CALL WDIALOGPUTMENU(IDF_MENU3,IPF(1)%ATTRIB,IPF(1)%NCOL,IPF(1)%ZCOL)
  
 ELSEIF(TOOLSID.EQ.ID_CREATEIDF_IFF)THEN
  !#reading IPF
  IU=UTL_GETUNITIFF(IFFNAME,'OLD'); IF(IU.LE.0)RETURN
  READ(IU,*) N; ALLOCATE(ATTRIB(N))
  DO I=1,N; READ(IU,*) ATTRIB(I); END DO
  CALL WDIALOGSELECT(ID_DCREATEIDFTAB3)
  CALL WDIALOGPUTMENU(IDF_MENU4,ATTRIB,N,MIN(N,6))
  CALL WDIALOGPUTMENU(IDF_MENU5,ATTRIB,N,MIN(N,2))
  DEALLOCATE(ATTRIB); CLOSE(IU)

 ENDIF

 CREATEIDF1READATTRIB=.TRUE.

 END FUNCTION CREATEIDF1READATTRIB

 !###======================================================================
 SUBROUTINE CREATEIDF1APPLY()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IERROR,IWHILE,IINT
 REAL :: IZVAL
 CHARACTER(LEN=256) :: IDFNAME

 !## make sure coordinates are correct!
 CALL CREATEIDF1GETCRD(1)

 IF(NCOL.LE.0.OR.NROW.LE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not create an IDF with zero/negative number of columns/rows!','Error')
  RETURN
 ENDIF

 CALL WDIALOGSELECT(ID_DCREATEIDF)
 CALL WDIALOGGETMENU(IDF_MENU1,IINT)
 IF(IINT.EQ.4)THEN
  IDFNAME=''
 ELSE
  IDFNAME=''
  IF(.NOT.UTL_WSELECTFILE('iMOD IDF File (*.idf)|*.idf|', &
   SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,IDFNAME,TITLE='Specify New IDF'))RETURN
 ENDIF
 
 CALL UTL_MESSAGEHANDLE(0)

 CALL WDIALOGSELECT(ID_DCREATEIDF)

 SELECT CASE (TOOLSID)

  !## ipf's
  CASE (ID_CREATEIDF_IPF)
   CALL CREATEIDF1APPLYIPF(IDFNAME,IERROR)

  !## gen's
  CASE (ID_CREATEIDF_GEN)
   CALL CREATEIDF1APPLYGEN(IDFNAME,IERROR,IINT)

  !## iff's
  CASE (ID_CREATEIDF_IFF)
   CALL WDIALOGSELECT(ID_DCREATEIDFTAB3)
   CALL WDIALOGGETMENU(IDF_MENU4,IX) !attribute
   CALL WDIALOGGETMENU(IDF_MENU5,IY) !condition
   CALL WDIALOGGETMENU(IDF_MENU6,IZ) !operator
   CALL WDIALOGGETCHECKBOX(IDF_CHECK1,IWHILE)
   CALL WDIALOGGETREAL(IDF_REAL1,IZVAL)
   CALL CREATEIDF1APPLYIFF(IDFNAME,IERROR,IWHILE,IZVAL)
  
  !## scratch
  CASE (ID_CREATEIDF_SCRATCH)
   CALL CREATEIDF1APPLYSCRATCH(IDFNAME,IERROR)
   
 END SELECT

 CALL CREATEIDF1DEALLOCATE()

 IF(IERROR.EQ.0)THEN
  IF(TOOLSID.EQ.ID_CREATEIDF_GEN)THEN
   IF(IINT.EQ.1)CALL IDFINIT(IDFNAMEGIVEN=IDFNAME(:INDEX(IDFNAME,'.',.TRUE.)-1)//'_LENGTH.IDF',LPLOT=.FALSE.)
   IF(IINT.EQ.5)CALL IDFINIT(IDFNAMEGIVEN=IDFNAME(:INDEX(IDFNAME,'.',.TRUE.)-1)//'_SKVARIANCE.IDF',LPLOT=.FALSE.)
   IF(IINT.EQ.6)CALL IDFINIT(IDFNAMEGIVEN=IDFNAME(:INDEX(IDFNAME,'.',.TRUE.)-1)//'_OKVARIANCE.IDF',LPLOT=.FALSE.)
  ENDIF
  CALL IDFINIT(IDFNAMEGIVEN=IDFNAME,LPLOT=.TRUE.)
 ENDIF

 END SUBROUTINE CREATEIDF1APPLY

 !###======================================================================
 SUBROUTINE CREATEIDF1APPLYSCRATCH(IDFNAME,IERROR)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 INTEGER,INTENT(INOUT) :: IERROR

 ALLOCATE(X(1),NX(1))
 CALL IDFNULLIFY(X(1)); CALL IDFNULLIFY(NX(1))
 
 X%NCOL  =NCOL
 X%NROW  =NROW
 X%XMIN  =XMIN
 X%YMIN  =YMIN
 X%XMAX  =XMAX
 X%YMAX  =YMAX
 X%NODATA=NODATA
 X%IEQ   =0
 X%DX    =CS
 X%DY    =CS
 NX      =X

 IF(.NOT.IDFALLOCATEX(X(1))) RETURN
 X(1)%X=NODATA
 
 IERROR=1; IF(IDFWRITE(X(1),IDFNAME,1))IERROR=0
 
 END SUBROUTINE CREATEIDF1APPLYSCRATCH
 
 !###======================================================================
 SUBROUTINE CREATEIDF1APPLYIFF(IDFNAME,IERROR,IWHILE,IZVAL)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 REAL,INTENT(IN) :: IZVAL
 INTEGER,INTENT(INOUT) :: IERROR
 INTEGER,INTENT(IN) :: IWHILE
 REAL :: X1,X2,Y1,Y2,XVAL,YVAL
 INTEGER :: I,J,L,IU,N,NC,IROW,ICOL,ITYPE,IOS
 LOGICAL :: LEX
 TYPE(WIN_MESSAGE) :: MESSAGE

 !## reading IFF
 IU=UTL_GETUNITIFF(IFFNAME,'OLD'); IF(IU.LE.0)RETURN

 READ(IU,*) N; DO I=1,N; READ(IU,*); ENDDO

 ALLOCATE(X(1),NX(1)); CALL IDFNULLIFY(X(1)); CALL IDFNULLIFY(NX(1))
 
 X%NCOL  =NCOL
 X%NROW  =NROW
 X%XMIN  =XMIN
 X%YMIN  =YMIN
 X%XMAX  =XMAX
 X%YMAX  =YMAX
 X%NODATA=NODATA
 X%IEQ   =0
 X%DX    =CS
 X%DY    =CS
 NX      =X
 
 IF(.NOT.IDFALLOCATEX(X(1))) RETURN
 IF(.NOT.IDFALLOCATEX(NX(1)))RETURN

 X(1)%X =0.0
 NX(1)%X=0.0

 ALLOCATE(IFF(2)); N=NC-5
 DO I=1,SIZE(IFF); NULLIFY(IFF(I)%XVAL); ALLOCATE(IFF(I)%XVAL(N)); ENDDO

 CALL WCURSORSHAPE(CURHOURGLASS); CALL WINDOWSELECT(0)

 IFF(2)%IPART=0
 DO
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)

!  READ(IU,*,IOSTAT=IOS) IFF(1)%IPART,IFF(1)%IL,IFF(1)%X,IFF(1)%Y,IFF(1)%Z,IFF(1)%T,IFF(1)%V
  READ(IU,*,IOSTAT=IOS) IFF(1)%IPART,IFF(1)%IL,IFF(1)%X,IFF(1)%Y,IFF(1)%Z,(IFF(1)%XVAL(J),J=1,N)
  IF(IOS.NE.0)EXIT

  !## same particle
  IF(IFF(1)%IPART.EQ.IFF(2)%IPART)THEN

   !## current point insize viewable extent
   IF(IFF(1)%X.LT.XMAX.AND.IFF(1)%X.GE.XMIN.AND. &
      IFF(1)%Y.LT.YMAX.AND.IFF(1)%Y.GE.YMIN)THEN

    !## get plot value
    CALL IFFPLOT_GETIFFVAL(IX,XVAL)

    !## apply selection
    LEX=.TRUE.
    IF(IWHILE.EQ.1)THEN
     !## get selection value
     CALL IFFPLOT_GETIFFVAL(IY,YVAL)
     LEX=IDFEDITGETLOGICAL(IZ,IZVAL,YVAL,0,0.0)
    ENDIF

    IF(LEX)THEN
     X1=IFF(1)%X; X2=IFF(2)%X
     Y1=IFF(1)%Y; Y2=IFF(2)%Y
     !## intersect line
     N=0; CALL INTERSECT_EQUI(XMIN,XMAX,YMIN,YMAX,CS,X1,X2,Y1,Y2,N,.FALSE.,.TRUE.)

     !## fill result array
     DO L=1,N
      ICOL=INT(XA(L))
      IROW=INT(YA(L))
      IF(ICOL.GE.1.AND.IROW.GE.1.AND.ICOL.LE.NCOL.AND.IROW.LE.NROW)THEN
       X(1)%X(ICOL,IROW) = X(1)%X(ICOL,IROW)+XVAL*LN(L)
       NX(1)%X(ICOL,IROW)=NX(1)%X(ICOL,IROW)+LN(L)
      ENDIF
     ENDDO

    ENDIF

   ENDIF

  ENDIF
  IFF(2)=IFF(1)
 ENDDO
 CLOSE(IU)

 DO IROW=1,NROW
  DO ICOL=1,NCOL
   IF(NX(1)%X(ICOL,IROW).GT.0.0)THEN
    X(1)%X(ICOL,IROW)=X(1)%X(ICOL,IROW)/NX(1)%X(ICOL,IROW)
   ELSE
    X(1)%X(ICOL,IROW)=NODATA
   ENDIF
  END DO
 END DO

 IF(IDFWRITE(X(1),IDFNAME,1))THEN
 ENDIF
 
 CALL INTERSECT_DEALLOCATE()
 IF(ALLOCATED(IFF))THEN
  DO I=1,SIZE(IFF); DEALLOCATE(IFF(I)%XVAL); ENDDO; DEALLOCATE(IFF)
 ENDIF
 
 CALL WCURSORSHAPE(CURARROW)

 IERROR=0

 END SUBROUTINE CREATEIDF1APPLYIFF

 !###======================================================================
 SUBROUTINE CREATEIDF1APPLYIPF(IDFNAME,IERROR)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 INTEGER,INTENT(INOUT) :: IERROR
 INTEGER :: I,IROW,ICOL,IRAT,IRAT1,IINT,ND,IDUPLICATE
 CHARACTER(LEN=9),DIMENSION(6) :: TINT
 DATA TINT/'Sample','Bivariate','PCG','','Simple Kriging','Ordinate Kriging'/

 !## bivariate interpolation
 REAL,POINTER,DIMENSION(:) :: XD,YD,ZD

 IERROR=1

 CALL WDIALOGSELECT(ID_DCREATEIDFTAB1)
 IF(WINFODIALOGFIELD(IDF_MENU1,FIELDSTATE).EQ.1)CALL WDIALOGGETMENU(IDF_MENU1,IX)
 IF(WINFODIALOGFIELD(IDF_MENU2,FIELDSTATE).EQ.1)CALL WDIALOGGETMENU(IDF_MENU2,IY)
 IF(WINFODIALOGFIELD(IDF_MENU3,FIELDSTATE).EQ.1)CALL WDIALOGGETMENU(IDF_MENU3,IZ)

 !## reread ipf, if neccessary
 IF(.NOT.CREATEIDF1READIPF())RETURN

 CALL WDIALOGSELECT(ID_DCREATEIDF)
 CALL WDIALOGGETMENU(IDF_MENU1,IINT)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO3,IDUPLICATE)

 ALLOCATE(X(3)); DO I=1,SIZE(X); CALL IDFNULLIFY(X(I)); ENDDO

 X(1)%NCOL  =NCOL
 X(1)%NROW  =NROW
 X(1)%XMIN  =XMIN
 X(1)%YMIN  =YMIN
 X(1)%XMAX  =XMAX
 X(1)%YMAX  =YMAX
 X(1)%NODATA=NODATA
 X(1)%IEQ   =0
 X(1)%IXV   =0
 X(1)%ITB   =0
 X(1)%DX    =CS
 X(1)%DY    =CS
 IF(.NOT.IDFALLOCATEX(X(1)))RETURN
 !## sampling
 IF(IINT.EQ.1)THEN
  CALL IDFCOPY(X(1),X(2))
 !## bivar
 ELSEIF(IINT.EQ.2)THEN
  IF(.NOT.IDFALLOCATESXY(X(1)))RETURN
 !## kriging
 ELSEIF(IINT.EQ.5.OR.IINT.EQ.6)THEN
  CALL IDFCOPY(X(1),X(3))
 ENDIF

 !## sampling
 IF(IINT.EQ.1)THEN
  X(1)%X=0.0; X(2)%X=0.0; IRAT=0
  DO I=1,IPF(1)%NROW
   IF(IPF(1)%XYZ(3,I).NE.NODATA)THEN
    CALL IDFIROWICOL(X(1),IROW,ICOL,IPF(1)%XYZ(1,I),IPF(1)%XYZ(2,I))
    IF(ICOL.GE.1.AND.ICOL.LE.X(1)%NCOL.AND. &
       IROW.GE.1.AND.IROW.LE.X(1)%NROW)THEN
     X(1)%X(ICOL,IROW)=X(1)%X(ICOL,IROW)+IPF(1)%XYZ(3,I)
     X(2)%X(ICOL,IROW)=X(2)%X(ICOL,IROW)+1.0
    ENDIF
   ENDIF
   CALL UTL_WAITMESSAGE(IRAT,IRAT1,I,IPF(1)%NROW,'Constructing IDF ...')
  ENDDO
  IF(IDUPLICATE.EQ.1)X(2)%X(ICOL,IROW)=1.0
  DO ICOL=1,NCOL; DO IROW=1,NROW
   IF(X(2)%X(ICOL,IROW).GT.0.0)THEN
    X(1)%X(ICOL,IROW)=X(1)%X(ICOL,IROW)/X(2)%X(ICOL,IROW)
   ELSE
    X(1)%X(ICOL,IROW)=NODATA
   ENDIF
  END DO; END DO
  IERROR=0
  
 !## bivariate interpolation
 ELSEIF(IINT.EQ.2)THEN

  !## number data points
  ND=IPF(1)%NROW; ALLOCATE(XD(ND),YD(ND),ZD(ND))

  !## fill coordinates --- ne nodatavalue
  ND=0
  DO I=1,IPF(1)%NROW
   IF(IPF(1)%XYZ(3,I).NE.NODATA)THEN
    ND=ND+1
    XD(ND)=IPF(1)%XYZ(1,I); YD(ND)=IPF(1)%XYZ(2,I); ZD(ND)=IPF(1)%XYZ(3,I)
   ENDIF
  ENDDO
 
  CALL BIVARIATE_INT(XD,YD,ZD,ND,IERROR,X(1))

 !## pcg
 ELSEIF(IINT.EQ.3)THEN

  ND=IPF(1)%NROW; ALLOCATE(XD(ND),YD(ND),ZD(ND))
  DO I=1,IPF(1)%NROW
   IF(IPF(1)%XYZ(3,I).NE.NODATA)THEN
    CALL IDFIROWICOL(X(1),IROW,ICOL,IPF(1)%XYZ(1,I),IPF(1)%XYZ(2,I))
    IF(IROW.NE.0.AND.ICOL.NE.0)THEN; ND=ND+1; XD(ND)=REAL(ICOL); YD(ND)=REAL(IROW); ZD(ND)=IPF(1)%XYZ(3,I); ENDIF  
   ENDIF
  ENDDO
  CALL SOLID_PCGINT(XD,YD,ZD,INT(ND,4),IERROR,X(1),1,IDUPLICATE)

 !## variogram
 ELSEIF(IINT.EQ.4)THEN

  ND=IPF(1)%NROW; ALLOCATE(XD(ND),YD(ND),ZD(ND))
  !## fill coordinates --- ne nodatavalue
  ND=0
  DO I=1,IPF(1)%NROW
   IF(IPF(1)%XYZ(3,I).NE.NODATA)THEN
    ND=ND+1; XD(ND)=IPF(1)%XYZ(1,I); YD(ND)=IPF(1)%XYZ(2,I); ZD(ND)=IPF(1)%XYZ(3,I)
   ENDIF
  ENDDO
  CALL KRIGING_VARIOGRAM(INT(ND,4),XD,YD,ZD,I,X(1),IBATCH=0)

 !## kriging (simple/ordinary)
 ELSEIF(IINT.EQ.5.OR.IINT.EQ.6)THEN

  ND=IPF(1)%NROW; ALLOCATE(XD(ND),YD(ND),ZD(ND))
  !## fill coordinates --- ne nodatavalue
  ND=0
  DO I=1,IPF(1)%NROW
   IF(IPF(1)%XYZ(3,I).NE.NODATA)THEN
    ND=ND+1; XD(ND)=IPF(1)%XYZ(1,I); YD(ND)=IPF(1)%XYZ(2,I); ZD(ND)=IPF(1)%XYZ(3,I)
   ENDIF
  ENDDO
  
  X(1)%X=X(1)%NODATA
  CALL KRIGING_MAIN(ND,XD,YD,ZD,X(1),X(3),MINP,RANGE,SILL,NUGGET,KTYPE,PNTSEARCH,IEXPVARIOGRAM,0,0.0,0,'')
  IERROR=0

 ENDIF

 IF(ASSOCIATED(XD))DEALLOCATE(XD); IF(ASSOCIATED(YD))DEALLOCATE(YD); IF(ASSOCIATED(ZD))DEALLOCATE(ZD)

 IF(IERROR.EQ.0)THEN
  CALL IDFFILLCOMMENT(X(1),'Units: Unknown'//NEWLINE// &
                           'Source: '//TRIM(IPFNAME)//NEWLINE// &
                           'Interpolation: '//TRIM(TINT(IINT)))
  IF(IDFWRITE(X(1),IDFNAME,1))THEN; ENDIF
  CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Gridded '//TRIM(ITOS(IPF(1)%NROW))//' points','Information')
 ENDIF

 END SUBROUTINE CREATEIDF1APPLYIPF

 !###======================================================================
 SUBROUTINE CREATEIDF1APPLYGEN(IDFNAME,IERROR,IINT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 INTEGER,INTENT(INOUT) :: IERROR
 INTEGER,INTENT(OUT) :: IINT
 INTEGER :: IRAT,IRAT1,IR,IR1,IR2,IC,IC1,IC2,MX,I,J,N,IROW,ICOL,JL,IOS,IDUPLICATE,IFORCELINE
 REAL :: X1,X2,Y1,Y2,XC,YC,ANGLE,D,MD,XV
 REAL,DIMENSION(:),POINTER :: XD,YD,ZD,XD_DUMMY,YD_DUMMY,ZD_DUMMY
 LOGICAL :: LEX
 CHARACTER(LEN=52) :: CID
 
 IERROR=1

 CALL WDIALOGSELECT(ID_DCREATEIDF)
 CALL WDIALOGGETMENU(IDF_MENU1,IINT)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO3,IDUPLICATE)
 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,IFORCELINE)
 
 ALLOCATE(X(2)); DO IR=1,SIZE(X); CALL IDFNULLIFY(X(IR)); ENDDO

 X(1)%NCOL  =NCOL
 X(1)%NROW  =NROW
 X(1)%XMIN  =XMIN
 X(1)%YMIN  =YMIN
 X(1)%XMAX  =XMAX
 X(1)%YMAX  =YMAX
 X(1)%NODATA=NODATA
 X(1)%IEQ   =0
 X(1)%IXV   =0
 X(1)%ITB   =0
 X(1)%DX    =CS
 X(1)%DY    =CS
 IF(.NOT.IDFALLOCATEX(X(1)))RETURN
 
 !## simple sampling
 IF(IINT.EQ.1)THEN
 
  MX=MAXVAL(SHPNCRD(1:SHPNO))
  CALL IDFCOPY(X(1),X(2)); X(2)%NODATA=0.0
  X(1)%X=0.0; X(2)%X=0.0
 
  IRAT=0
  DO SHPI=1,SHPNO
   !## selected polygon
   IF(SHPNCRD(SHPI).GT.0)THEN
    !## grid with values from dat file
    XV=REAL(SHPI)
    IF(ASSOCIATED(VAR))THEN
     IF(IV.LE.0)THEN
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should specify an appropriate column in the dat-file to be used for gridding','Error')
      RETURN
     ENDIF
     WRITE(CID,*) SHPID(SHPI)
     CALL UTL_GENLABELSGET(CID,JL)
     IF(JL.LE.0)THEN
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not get the right label for current shape '//TRIM(ITOS(IV)),'Error'); RETURN
     ENDIF
     READ(VAR(IV,JL),*,IOSTAT=IOS) XV
     IF(IOS.NE.0)THEN
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not convert '//TRIM(VAR(IV,JL))//' into a real','Error'); RETURN
     ENDIF
    ENDIF
    LEX=.FALSE.
    IF(SHPTYPE(SHPI).EQ.ID_LINE)LEX=.TRUE.
    IF(IFORCELINE.EQ.1)LEX=.TRUE.
        
    !## check whether it is a polygon or a line
    IF(LEX)THEN 
     DO I=1,SHPNCRD(SHPI)-1
      X1=SHPXC(I,SHPI); X2=SHPXC(I+1,SHPI)
      Y1=SHPYC(I,SHPI); Y2=SHPYC(I+1,SHPI)
      !## intersect line
      N=0; CALL INTERSECT_EQUI(X(1)%XMIN,X(1)%XMAX,X(1)%YMIN,X(1)%YMAX,X(1)%DX,X1,X2,Y1,Y2,N,.FALSE.,.TRUE.)
      !## fill result array
      DO J=1,N
       IC=INT(XA(J)); IR=INT(YA(J))
       IF(IC.GE.1.AND.IR.GE.1.AND.IC.LE.X(1)%NCOL.AND.IR.LE.X(1)%NROW)THEN
        X(1)%X(IC,IR)=X(1)%X(IC,IR)+XV*LN(J)
        X(2)%X(IC,IR)=X(2)%X(IC,IR)+LN(J)
       ENDIF
      ENDDO
     ENDDO
     CALL UTL_WAITMESSAGE(IRAT,IRAT1,SHPI,SHPNO,'Line '//TRIM(ITOS(SHPI))//', constructing IDF')
    ELSEIF(SHPTYPE(SHPI).EQ.ID_POLYGON)THEN
     !## proces polygon
     X1 =MINVAL(SHPXC(1:SHPNCRD(SHPI),SHPI)); X2 =MAXVAL(SHPXC(1:SHPNCRD(SHPI),SHPI))
     Y1 =MINVAL(SHPYC(1:SHPNCRD(SHPI),SHPI)); Y2 =MAXVAL(SHPYC(1:SHPNCRD(SHPI),SHPI))
     IC1=INT((X1-XMIN)/CS)-1; IC2=INT((X2-XMIN)/CS)+1
     IR1=INT((YMAX-Y2)/CS)-1; IR2=INT((YMAX-Y1)/CS)+1
     DO IR=MAX(1,IR1),MIN(IR2,NROW)
      DO IC=MAX(1,IC1),MIN(IC2,NCOL)
       CALL IDFGETLOC(X(1),IR,IC,XC,YC)
       IF(UTL_INSIDEPOLYGON(XC,YC,SHPXC(:,SHPI),SHPYC(:,SHPI),SHPNCRD(SHPI)).EQ.1)THEN
        X(1)%X(IC,IR)=X(1)%X(IC,IR)+XV 
        X(2)%X(IC,IR)=X(2)%X(IC,IR)+1.0 
       ENDIF
      END DO
      CALL UTL_WAITMESSAGE(IRAT,IRAT1,IR,MIN(IR2,NROW),'Polygon '//TRIM(ITOS(SHPI))//', constructing IDF')
     END DO
    ELSEIF(SHPTYPE(SHPI).EQ.ID_RECTANGLE)THEN
     !## proces rectangle
     X1 =MINVAL(SHPXC(1:SHPNCRD(SHPI),SHPI)); X2 =MAXVAL(SHPXC(1:SHPNCRD(SHPI),SHPI))
     Y1 =MINVAL(SHPYC(1:SHPNCRD(SHPI),SHPI)); Y2 =MAXVAL(SHPYC(1:SHPNCRD(SHPI),SHPI))
     IC1=INT((X1-XMIN)/CS)-1; IC2=INT((X2-XMIN)/CS)+1
     IR1=INT((YMAX-Y2)/CS)-1; IR2=INT((YMAX-Y1)/CS)+1
     DO IR=MAX(1,IR1),MIN(IR2,NROW)
      DO IC=MAX(1,IC1),MIN(IC2,NCOL)
       CALL IDFGETLOC(X(1),IR,IC,XC,YC)
       IF(XC.GE.X1.AND.XC.LE.X2.AND.YC.GE.Y1.AND.YC.LE.Y2)THEN
        X(1)%X(IC,IR)=X(1)%X(IC,IR)+XV 
        X(2)%X(IC,IR)=X(2)%X(IC,IR)+1.0 
       ENDIF
      END DO
      CALL UTL_WAITMESSAGE(IRAT,IRAT1,IR,MIN(IR2,NROW),'Rectangle '//TRIM(ITOS(SHPI))//', constructing IDF')
     END DO
    ENDIF
   ENDIF
  ENDDO
  IF(ASSOCIATED(XA))DEALLOCATE(XA); IF(ASSOCIATED(YA))DEALLOCATE(YA)
  DO IC=1,NCOL; DO IR=1,NROW
   IF(X(2)%X(IC,IR).GE.1.0)THEN
    X(1)%X(IC,IR)=X(1)%X(IC,IR)/X(2)%X(IC,IR)
   ELSE
    X(1)%X(IC,IR)=NODATA
   ENDIF
  END DO; END DO
 
  IF(IDFWRITE(X(1),IDFNAME,1))IERROR=0
  IF(IDFWRITE(X(2),IDFNAME(:INDEX(IDFNAME,'.',.TRUE.)-1)//'_LENGTH.IDF',1))IERROR=0

 !## apply interpolation
 ELSE
  !## include an interpolation from the contours using the PCG solver
  N=1000; ALLOCATE(XD(N),YD(N),ZD(N))
  N=0; DO SHPI=1,SHPNO
   !## selected polygon
   IF(SHPNCRD(SHPI).GT.0)THEN
    !## grid with values from dat file
    XV=REAL(SHPI)
    IF(ASSOCIATED(VAR))THEN
     IF(IV.LE.0)THEN
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should specify an appropriate column in the dat-file to be used for gridding','Error')
      DEALLOCATE(XD,YD,ZD); RETURN
     ENDIF
     WRITE(CID,*) SHPID(SHPI)
     CALL UTL_GENLABELSGET(CID,JL)
     READ(VAR(IV,JL),*,IOSTAT=IOS) XV
     IF(IOS.NE.0)THEN
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not convert '//TRIM(VAR(IV,JL))//' into a real','Error')
      DEALLOCATE(XD,YD,ZD); RETURN
     ENDIF
    ENDIF

    DO I=1,SHPNCRD(SHPI) 
     IF(I.EQ.SHPNCRD(SHPI).AND.SHPTYPE(SHPI).EQ.ID_LINE)EXIT
     IF(I.EQ.SHPNCRD(SHPI))THEN
      X1=SHPXC(I,SHPI); X2=SHPXC(1,SHPI); Y1=SHPYC(I,SHPI); Y2=SHPYC(1,SHPI)
     ELSE
      X1=SHPXC(I,SHPI); X2=SHPXC(I+1,SHPI); Y1=SHPYC(I,SHPI); Y2=SHPYC(I+1,SHPI)
     ENDIF
     ANGLE=ATAN2(Y2-Y1,X2-X1); MD=(Y2-Y1)**2.0+(X2-X1)**2.0; IF(MD.GT.0.0)MD=SQRT(MD); D=0.0
     DO
      IF(N+1.GT.SIZE(XD))THEN
       ALLOCATE(XD_DUMMY(N*2),YD_DUMMY(N*2),ZD_DUMMY(N*2))
       XD_DUMMY(1:N)=XD(1:N); YD_DUMMY(1:N)=YD(1:N); ZD_DUMMY(1:N)=ZD(1:N)
       DEALLOCATE(XD,YD,ZD); XD=>XD_DUMMY; YD=>YD_DUMMY; ZD=>ZD_DUMMY
      ENDIF
      N=N+1; XD(N)=X1+D*COS(ANGLE); YD(N)=Y1+D*SIN(ANGLE); ZD(N)=XV 
      D=D+X(1)%DX; IF(D.GE.MD)EXIT      
     ENDDO    
    ENDDO
   ENDIF
  ENDDO     
  
  SELECT CASE (IINT)
   !## bivar
   CASE(2)
    CALL BIVARIATE_INT(XD,YD,ZD,INT(N,4),IERROR,X(1))
   !## pcg
   CASE(3)
    J=0; DO I=1,N
     CALL IDFIROWICOL(X(1),IROW,ICOL,XD(I),YD(I))
     IF(IROW.NE.0.AND.ICOL.NE.0)THEN; J=J+1; XD(J)=REAL(ICOL); YD(J)=REAL(IROW); ZD(J)=ZD(I); ENDIF  
    ENDDO; N=J
    CALL SOLID_PCGINT(XD,YD,ZD,INT(N,4),IERROR,X(1),1,IDUPLICATE)
   CASE (4)
    CALL KRIGING_VARIOGRAM(INT(N,4),XD,YD,ZD,I,X(1),IBATCH=0)
   CASE (5,6)
    CALL IDFCOPY(X(1),X(2)); X(2)%NODATA=0.0
     
    X(1)%X=X(1)%NODATA
    CALL KRIGING_MAIN(N,XD,YD,ZD,X(1),X(2),MINP,RANGE,SILL,NUGGET,KTYPE,PNTSEARCH,IEXPVARIOGRAM,0,0.0,0,'')
    IERROR=0   
   CASE DEFAULT
  END SELECT
  DEALLOCATE(XD,YD,ZD)

  IF(IDFWRITE(X(1),IDFNAME,1))IERROR=0

 ENDIF
 
 END SUBROUTINE CREATEIDF1APPLYGEN

 !###======================================================================
 SUBROUTINE CREATEIDF1FIELDS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,IWHILE
 LOGICAL :: LEX

 CALL WDIALOGSELECT(ID_DCREATEIDF)

 I=0
 !## ipf's
! IF(ITAB.EQ.ID_DCREATEIDFTAB1)THEN
 SELECT CASE (TOOLSID)
  CASE (ID_CREATEIDF_IPF)
   CALL WDIALOGSELECT(ID_DCREATEIDFTAB1)
   CALL WDIALOGGETSTRING(IDF_STRING1,IPFNAME)
   IF(LEN_TRIM(IPFNAME).GT.0)THEN
    INQUIRE(FILE=IPFNAME,EXIST=LEX)
    IF(LEX)I=1
   ENDIF
   CALL WDIALOGFIELDSTATE(IDF_MENU1,I)
   CALL WDIALOGFIELDSTATE(IDF_MENU2,I)
   CALL WDIALOGFIELDSTATE(IDF_MENU3,I)
   CALL WDIALOGFIELDSTATE(IDF_LABEL2,I)
   CALL WDIALOGFIELDSTATE(IDF_LABEL3,I)
   CALL WDIALOGFIELDSTATE(IDF_LABEL4,I)
   CALL WDIALOGFIELDSTATE(IDF_GROUP2,I)
   CALL WDIALOGSELECT(ID_DCREATEIDF)
   CALL WDIALOGFIELDSTATE(IDF_EXTENT1,I)

  !## gen's
  CASE (ID_CREATEIDF_GEN)
   IF(SHPNO.GT.0)I=1
   CALL WDIALOGSELECT(ID_DCREATEIDF)
   CALL WDIALOGFIELDSTATE(IDF_EXTENT1,I)
   CALL WDIALOGSELECT(ID_DCREATEIDFTAB2)
   CALL WDIALOGFIELDSTATE(ID_INFO,I)

  !## iff's
  CASE (ID_CREATEIDF_IFF)
   CALL WDIALOGSELECT(ID_DCREATEIDFTAB3)
   CALL WDIALOGGETSTRING(IDF_STRING1,IFFNAME)
   IF(LEN_TRIM(IFFNAME).GT.0)THEN
    INQUIRE(FILE=IFFNAME,EXIST=LEX)
    IF(LEX)I=1
   ENDIF
   CALL WDIALOGFIELDSTATE(IDF_MENU4,I)
   CALL WDIALOGFIELDSTATE(IDF_LABEL4,I)
   CALL WDIALOGFIELDSTATE(IDF_CHECK1,I)
   IWHILE=I
   IF(I.EQ.1)CALL WDIALOGGETCHECKBOX(IDF_CHECK1,IWHILE)
   CALL WDIALOGFIELDSTATE(IDF_MENU5,IWHILE)
   CALL WDIALOGFIELDSTATE(IDF_MENU6,IWHILE)
   CALL WDIALOGFIELDSTATE(IDF_REAL1,IWHILE)
   CALL WDIALOGSELECT(ID_DCREATEIDF)
   CALL WDIALOGFIELDSTATE(IDF_EXTENT1,I)

  CASE (ID_CREATEIDF_SCRATCH)
   I=1
 END SELECT

 CALL WDIALOGSELECT(ID_DCREATEIDF)
 CALL CREATEIDF1GETCRD(1)
 CALL WDIALOGFIELDSTATE(IDOK,I)

 END SUBROUTINE CREATEIDF1FIELDS

 !###======================================================================
 SUBROUTINE CREATEIDF1GETCRD(IEDIT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IEDIT
 INTEGER :: I,J,K,N,IU,IOS,IINT,IERROR
 REAL :: X,Y,X1,X2,Y1,Y2
 CHARACTER(LEN=256) :: IDFNAME

 CALL WDIALOGSELECT(ID_DCREATEIDF)
 CALL WDIALOGGETREAL(IDF_REAL3,CS)
 CALL WDIALOGGETREAL(IDF_REAL4,NODATA)

 !## get them automatically
 IF(IEDIT.EQ.0)THEN

  SELECT CASE (TOOLSID)
   CASE (ID_CREATEIDF_GEN)

    J=0
    DO I=1,SHPNO
     IF(SHPNCRD(I).GT.0)THEN
      J=J+1
      IF(J.EQ.1)THEN
       XMIN=MINVAL(SHPXC(1:SHPNCRD(I),I))
       YMIN=MINVAL(SHPYC(1:SHPNCRD(I),I))
       XMAX=MAXVAL(SHPXC(1:SHPNCRD(I),I))
       YMAX=MAXVAL(SHPYC(1:SHPNCRD(I),I))
      ELSE
       XMIN=MIN(XMIN,MINVAL(SHPXC(1:SHPNCRD(I),I)))
       YMIN=MIN(YMIN,MINVAL(SHPYC(1:SHPNCRD(I),I)))
       XMAX=MAX(XMAX,MAXVAL(SHPXC(1:SHPNCRD(I),I)))
       YMAX=MAX(YMAX,MAXVAL(SHPYC(1:SHPNCRD(I),I)))
      ENDIF
     ENDIF
    ENDDO

   CASE (ID_CREATEIDF_IPF)
    
    CALL WDIALOGSELECT(ID_DCREATEIDFTAB1)
    IF(WINFODIALOGFIELD(IDF_MENU1,FIELDSTATE).EQ.1)CALL WDIALOGGETMENU(IDF_MENU1,IX)
    IF(WINFODIALOGFIELD(IDF_MENU2,FIELDSTATE).EQ.1)CALL WDIALOGGETMENU(IDF_MENU2,IY)
    IF(WINFODIALOGFIELD(IDF_MENU3,FIELDSTATE).EQ.1)CALL WDIALOGGETMENU(IDF_MENU3,IZ)

    IF(CREATEIDF1READIPF())THEN
     DO I=1,IPF(1)%NROW
      IF(I.EQ.1)THEN
       XMIN=IPF(1)%XYZ(1,I)
       YMIN=IPF(1)%XYZ(2,I)
       XMAX=IPF(1)%XYZ(1,I)
       YMAX=IPF(1)%XYZ(2,I)
      ELSE
       XMIN=MIN(XMIN,IPF(1)%XYZ(1,I))
       YMIN=MIN(YMIN,IPF(1)%XYZ(2,I))
       XMAX=MAX(XMAX,IPF(1)%XYZ(1,I))
       YMAX=MAX(YMAX,IPF(1)%XYZ(2,I))
      ENDIF
     ENDDO
    ENDIF

   CASE (ID_CREATEIDF_IFF)

    !##open idf-file - get idfkind
    IU=UTL_GETUNITIFF(IFFNAME,'OLD')
    IF(IU.GT.0)THEN
     READ(IU,*) N
     DO I=1,N; READ(IU,*); ENDDO
     XMIN= 10.0E10
     YMIN= 10.0E10
     XMAX=-10.0E10
     YMAX=-10.0E10

     CALL WCURSORSHAPE(CURHOURGLASS)

     DO
      READ(IU,*,IOSTAT=IOS) J,K,X,Y
      IF(IOS.NE.0)EXIT
      XMIN=MIN(XMIN,X)
      XMAX=MAX(XMAX,X)
      YMIN=MIN(YMIN,Y)
      YMAX=MAX(YMAX,Y)
     ENDDO

     CALL WCURSORSHAPE(CURARROW)

     CLOSE(IU)
     !## make them cs-bigger to ensure that everything is captured
     XMIN=XMIN-CS
     XMAX=XMAX+CS
     YMIN=YMIN-CS
     YMAX=YMAX+CS
    ENDIF
  END SELECT

  CALL WDIALOGSELECT(ID_DCREATEIDF)
  CALL WDIALOGPUTREAL(IDF_REAL1,XMIN,'(F10.2)')
  CALL WDIALOGPUTREAL(IDF_REAL2,YMIN,'(F10.2)')
  CALL WDIALOGPUTREAL(IDF_REAL5,YMAX,'(F10.2)')
  CALL WDIALOGPUTREAL(IDF_REAL6,XMAX,'(F10.2)')

 !## take current zoomlevel
 ELSEIF(IEDIT.EQ.-1)THEN

  XMIN=MPW%XMIN
  XMAX=MPW%XMAX
  YMIN=MPW%YMIN
  YMAX=MPW%YMAX
  CALL WDIALOGSELECT(ID_DCREATEIDF)
  CALL WDIALOGPUTREAL(IDF_REAL1,XMIN,'(F10.2)')
  CALL WDIALOGPUTREAL(IDF_REAL2,YMIN,'(F10.2)')
  CALL WDIALOGPUTREAL(IDF_REAL5,YMAX,'(F10.2)')
  CALL WDIALOGPUTREAL(IDF_REAL6,XMAX,'(F10.2)')

 ENDIF

 CALL WDIALOGSELECT(ID_DCREATEIDF)
 CALL WDIALOGGETREAL(IDF_REAL3,CS)
 CALL WDIALOGGETREAL(IDF_REAL1,XMIN)
 CALL WDIALOGGETREAL(IDF_REAL2,YMIN)
 CALL WDIALOGGETREAL(IDF_REAL5,YMAX)
 CALL WDIALOGGETREAL(IDF_REAL6,XMAX)

 X1  =MIN(XMIN,XMAX)
 X2  =MAX(XMIN,XMAX)
 XMIN=X1
 XMAX=X2
 Y1  =MIN(YMIN,YMAX)
 Y2  =MAX(YMIN,YMAX)
 YMIN=Y1
 YMAX=Y2

 CALL UTL_IDFSNAPTOGRID(XMIN,XMAX,YMIN,YMAX,CS,NCOL,NROW)

 CALL WDIALOGPUTREAL(IDF_REAL1,XMIN,'(F10.2)')
 CALL WDIALOGPUTREAL(IDF_REAL2,YMIN,'(F10.2)')
 CALL WDIALOGPUTREAL(IDF_REAL5,YMAX,'(F10.2)')
 CALL WDIALOGPUTREAL(IDF_REAL6,XMAX,'(F10.2)')

 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,NROW)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER2,NCOL)

 CALL WDIALOGSPINNERSTEP(IDF_REAL1,CS,10*CS)
 CALL WDIALOGSPINNERSTEP(IDF_REAL2,CS,10*CS)
 CALL WDIALOGSPINNERSTEP(IDF_REAL5,CS,10*CS)
 CALL WDIALOGSPINNERSTEP(IDF_REAL6,CS,10*CS)


 END SUBROUTINE CREATEIDF1GETCRD

 !###======================================================================
 SUBROUTINE CREATEIDF1DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE

 IF(ALLOCATED(X))THEN
  CALL IDFDEALLOCATE(X,SIZE(X))
  DEALLOCATE(X)
 ENDIF
 IF(ALLOCATED(NX))THEN
  CALL IDFDEALLOCATE(NX,SIZE(NX))
  DEALLOCATE(NX)
 ENDIF

 IF(ALLOCATED(IPF))CALL IPFDEALLOCATE()

 CALL UTL_MESSAGEHANDLE(1)

 END SUBROUTINE CREATEIDF1DEALLOCATE

 !###======================================================================
 SUBROUTINE CREATEIDF1INIT(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 INTEGER :: I
 
 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID,2).EQ.1)THEN
  CALL CREATEIDF1CLOSE(); RETURN
 ENDIF

 CALL MAIN1INACTMODULE(ID)
 TOOLSID=ID; IPFNAME=''; IX=1; IY=2; IZ=3
 
 !other module no closed, no approvement given
 IF(IDIAGERROR.EQ.1)RETURN

 CALL WMENUSETSTATE(TOOLSID,2,1)

 CALL WDIALOGLOAD(ID_DCREATEIDF,ID_DCREATEIDF)
 CALL WDIALOGRANGEREAL(IDF_REAL3,0.1,10000.0)
 CALL WDIALOGSPINNERSTEP(IDF_REAL3,25.0,100.0)
 CALL WDIALOGPUTREAL(IDF_REAL3,25.0,'(F10.2)')
 CALL WDIALOGPUTIMAGE(ID_PROPERTIES,ID_ICONPROPERTIES,1)
 
 SELECT CASE (TOOLSID)

  CASE (ID_CREATEIDF_IPF)
   CALL WDIALOGPUTSTRING(IDF_EXTENT1,'IPF Extent')
   !## fill in whenever an/first IPF is selected
   CALL WDIALOGSELECT(ID_DCREATEIDFTAB1)
   DO I=1,MXMPLOT
    IF(MP(I)%ISEL.AND.MP(I)%IPLOT.EQ.2)THEN
     IPFNAME=MP(I)%IDFNAME
     IX=MP(I)%XCOL; IY=MP(I)%YCOL; IZ=MP(I)%ZCOL
     CALL WDIALOGPUTSTRING(IDF_STRING1,MP(I)%IDFNAME)
     EXIT  
    ENDIF
   ENDDO

  CASE (ID_CREATEIDF_GEN)
   CALL WDIALOGPUTSTRING(IDF_EXTENT1,'GEN Extent')

  CASE (ID_CREATEIDF_IFF)
   CALL WDIALOGPUTSTRING(IDF_EXTENT1,'IFF Extent')
   !## fill in whenever an/first IFF is selected
   CALL WDIALOGSELECT(ID_DCREATEIDFTAB1)
   DO I=1,MXMPLOT
    IF(MP(I)%ISEL.AND.MP(I)%IPLOT.EQ.3)THEN
     IFFNAME=MP(I)%IDFNAME
     CALL WDIALOGPUTSTRING(IDF_STRING1,MP(I)%IDFNAME)
     IF(CREATEIDF1READATTRIB())CALL CREATEIDF1FIELDS()
     EXIT  
    ENDIF
   ENDDO

  CASE (ID_CREATEIDF_SCRATCH)
   CALL WDIALOGFIELDSTATE(IDF_EXTENT1,3)
   CALL WDIALOGFIELDSTATE(IDF_TAB1,3)

 END SELECT

 CALL WDIALOGSELECT(ID_DCREATEIDFTAB1)
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPENIDF,1)
 CALL WDIALOGFIELDOPTIONS(IDF_STRING1,EDITFIELDCHANGED,ENABLED)  !## xmin

 CALL POLYGON1IMAGES(ID_DCREATEIDFTAB2)
 CALL WDIALOGPUTIMAGE(ID_INFO,ID_ICONINFO)
 CALL WDIALOGFIELDSTATE(ID_INFO,0)

 CALL WDIALOGSELECT(ID_DCREATEIDFTAB3)
 CALL WDIALOGFIELDOPTIONS(IDF_STRING1,EDITFIELDCHANGED,ENABLED)  !## xmin
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPENIDF,1) 

 CALL POLYGON1INIT()
 CALL POLYGON1FIELDS(ID_DCREATEIDFTAB2)

 CALL CREATEIDF1FIELDS()

 !## block all other tabs
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DCREATEIDFTAB1,0)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DCREATEIDFTAB2,0)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DCREATEIDFTAB3,0)
 SELECT CASE (TOOLSID)
  CASE (ID_CREATEIDF_IPF)
   CALL WDIALOGFIELDSTATE(IDF_CHECK1,0)
   CALL WDIALOGTABSTATE(IDF_TAB1,ID_DCREATEIDFTAB1,1)
   CALL WDIALOGSETTAB(IDF_TAB1,ID_DCREATEIDFTAB1)
   CALL WDIALOGPUTMENU(IDF_MENU1,(/'(SPP) Simple Point Sampling', &
                                   '(BI) Bivariate Interpolation', &
                                   '(PCG) Preconditioned Conjugate Gradient', &
                                   '(VG) Variogram', &
                                   '(SKI) Simple Kriging Interpolation', &
                                   '(OKI) Ordinary Kriging Interpolation'/),6,1)
   IF(CREATEIDF1READATTRIB())CALL CREATEIDF1FIELDS()
  CASE (ID_CREATEIDF_GEN)
   CALL WDIALOGTABSTATE(IDF_TAB1,ID_DCREATEIDFTAB2,1)
   CALL WDIALOGSETTAB(IDF_TAB1,ID_DCREATEIDFTAB2)
!   CALL WDIALOGFIELDSTATE(IDF_LABEL10,3)
   CALL WDIALOGPUTMENU(IDF_MENU1,(/'(SPP) Simple Point Sampling', &
                                   '(BI) Bivariate Interpolation', &
                                   '(PCG) Preconditioned Conjugate Gradient', &
                                   '(VG) Variogram', &
                                   '(SKI) Simple Kriging Interpolation', &
                                   '(OKI) Ordinary Kriging Interpolation'/),6,1)
  CASE (ID_CREATEIDF_IFF)
   CALL WDIALOGFIELDSTATE(IDF_CHECK1,0)
   CALL WDIALOGTABSTATE(IDF_TAB1,ID_DCREATEIDFTAB3,1)
   CALL WDIALOGSETTAB(IDF_TAB1,ID_DCREATEIDFTAB3)
!   CALL WDIALOGFIELDSTATE(IDF_LABEL10,3)
   CALL WDIALOGPUTMENU(IDF_MENU1,(/'(SPP) Simple Point Sampling', &
                                   '(BI) Bivariate Interpolation', &
                                   '(PCG) Preconditioned Conjugate Gradient'/),3,1)
  CASE (ID_CREATEIDF_SCRATCH)
   CALL WDIALOGFIELDSTATE(IDF_GROUP3,3)
   CALL WDIALOGFIELDSTATE(IDF_LABEL11,3)
   CALL WDIALOGFIELDSTATE(IDF_RADIO3,3)
   CALL WDIALOGFIELDSTATE(IDF_RADIO4,3)
   CALL WDIALOGFIELDSTATE(IDF_LABEL10,3)
   CALL WDIALOGFIELDSTATE(IDF_MENU1,3)
   CALL WDIALOGFIELDSTATE(ID_PROPERTIES,3)
   CALL WDIALOGFIELDSTATE(IDF_CHECK1,3)

 END SELECT
 
 !## initial value
 NODATA=0.0
 GENFNAME=''
 MXITER1=1000; MXITER2=500
 
 CALL WDIALOGSELECT(ID_DCREATEIDF); CALL WDIALOGSHOW(-1,-1,0,2)

 END SUBROUTINE CREATEIDF1INIT

 !###======================================================================
 SUBROUTINE CREATEIDF1CLOSE()
 !###======================================================================
 IMPLICIT NONE

 IDIAGERROR=1

 CALL POLYGON1DRAWSHAPE(1,SHPNO)
 CALL POLYGON1CLOSE()

 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(TOOLSID,2,0)
 CALL WDIALOGSELECT(ID_DCREATEIDF); CALL WDIALOGUNLOAD()
 IDIAGERROR=0

 END SUBROUTINE CREATEIDF1CLOSE

END MODULE MOD_CREATEIDF

