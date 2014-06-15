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
MODULE MOD_NC2IDF

USE NETCDF 

USE WINTERACTER
USE RESOURCE
USE MOD_IDF, ONLY : IDFREAD,IDFREADPART,IDFWRITE,IDFDEALLOCATE,IDFALLOCATEX,IDFDEALLOCATEX,IDFALLOCATESXY,IDFNULLIFY
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_UTL, ONLY : UTL_MESSAGEHANDLE,UTL_SUBST,UTL_GETUNIT,ITOS,RTOS,UTL_WAITMESSAGE,CLOSEUNITS
USE MODPLOT, ONLY : MXMPLOT,MP,MPW
USE MOD_POLINT, ONLY : POL1LOCATE
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_GOOGLE, ONLY : GOOGLE_LATLONG
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF

INTEGER,PRIVATE :: NCID,VARID,XVARID,YVARID,LLEN,NDIMS,NVARS,NGATTS,UNLIMDIMID,NATTS,ATTNUM,XTP, &
                   IV,IX,IY
CHARACTER(LEN=100),DIMENSION(:),ALLOCATABLE,PRIVATE :: LAB_DIM,LAB_VAR
CHARACTER(LEN=100),DIMENSION(:,:),ALLOCATABLE,PRIVATE :: LAB_ATT,LAB_ATT_V
CHARACTER(LEN=100),DIMENSION(:),ALLOCATABLE,PRIVATE :: LAB_GATT,LAB_GATT_V
INTEGER,DIMENSION(:),ALLOCATABLE,PRIVATE :: NDIM_VAR,NATT_VAR,LDIM,XTYPE
INTEGER,DIMENSION(:,:),ALLOCATABLE,PRIVATE :: DIMIDS
CHARACTER(LEN=256),PRIVATE :: NETCDF_NAME
INTEGER(KIND=2) :: IV1
CHARACTER(LEN=256) :: IV2
INTEGER(KIND=4) :: IV3
INTEGER(KIND=8) :: IV4
REAL :: IV5,NODATA
DOUBLE PRECISION :: IV6
 
REAL,DIMENSION(:,:,:),ALLOCATABLE,PRIVATE :: XNETCDF
INTEGER,ALLOCATABLE,DIMENSION(:),PRIVATE :: ZVID
 
INTEGER :: INETCDF=1  !## no support of netcdf

CONTAINS

 !###======================================================================
 SUBROUTINE NC2IDF_IMPORTNC(IDFNAME,IERROR)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 INTEGER,INTENT(INOUT) :: IERROR
 LOGICAL :: LEX
 INTEGER :: I
 
 IERROR=1
 
 NETCDF_NAME=IDFNAME
 
 !## Open the file. NF90_NOWRITE tells netCDF we want read-only access to the file.
 IF(.NOT.NC_CHECK(NF90_OPEN(NETCDF_NAME,NF90_NOWRITE,NCID)))RETURN
 !## Get the varid of the data variable, based on its name.
 IF(.NOT.NC_CHECK(NF90_INQUIRE(NCID,NDIMS,NVARS,NGATTS,UNLIMDIMID)))RETURN
 !## allocate memory for labels
 ALLOCATE(LAB_DIM(NDIMS),LAB_VAR(NVARS),NDIM_VAR(NVARS),NATT_VAR(NVARS),LDIM(NDIMS),DIMIDS(NDIMS,NVARS),XTYPE(NVARS))
 DO I=1,NVARS
  IF(.NOT.NC_CHECK(NF90_INQUIRE_VARIABLE(NCID,I,NAME=LAB_VAR(I),XTYPE=XTYPE(I),NDIMS=NDIM_VAR(I),DIMIDS=DIMIDS(:,I),&
                   NATTS=NATT_VAR(I))))RETURN
 ENDDO 
 ALLOCATE(LAB_ATT(SUM(NATT_VAR),NVARS),LAB_ATT_V(SUM(NATT_VAR),NVARS), &
          LAB_GATT(NGATTS)            ,LAB_GATT_V(NGATTS))
 
  !## read netCDF file
 IF(NC2IDF_IMPORTNC_GET())THEN
  ALLOCATE(IDF(1)); DO I=1,SIZE(IDF); CALL IDFNULLIFY(IDF(I)); ENDDO 
  IF(NC2IDF_IMPORTNC_IDF())IERROR=0
  CALL IDFDEALLOCATE(IDF,SIZE(IDF))
  DEALLOCATE(IDF)
 ENDIF
 DEALLOCATE(LAB_DIM,LAB_VAR,LAB_ATT,LAB_ATT_V,LAB_GATT,LAB_GATT_V,NDIM_VAR,NATT_VAR,LDIM,DIMIDS,XTYPE)

 CALL WINDOWSELECT(0)
 CALL WINDOWOUTSTATUSBAR(4,'')

 !## Close the file, freeing all resources.
 IF(.NOT.NC_CHECK(NF90_CLOSE(NCID)))RETURN

 CALL UTL_MESSAGEHANDLE(1)
 
 END SUBROUTINE NC2IDF_IMPORTNC
 
 !###======================================================================
 LOGICAL FUNCTION NC2IDF_IMPORTNC_GET()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,ID,ITYPE
 TYPE(WIN_MESSAGE) :: MESSAGE
 
 NC2IDF_IMPORTNC_GET=.FALSE.
 
 !## get dimensions
 DO I=1,NDIMS
  IF(.NOT.NC_CHECK(NF90_INQUIRE_DIMENSION(NCID,I,LAB_DIM(I),LDIM(I))))RETURN
 ENDDO

 DO I=1,NGATTS
  !## get global attribute name
  IF(.NOT.NC_CHECK(NF90_INQ_ATTNAME(NCID,NF90_GLOBAL,I,NAME=LAB_GATT(I))))RETURN 
  IF(.NOT.NC_CHECK(NF90_INQUIRE_ATTRIBUTE(NCID,NF90_GLOBAL,NAME=LAB_GATT(I),XTYPE=XTP,LEN=LLEN,ATTNUM=ATTNUM)))RETURN
  CALL NC2IDF_GETATTRIBUTE(LAB_GATT(I),LAB_GATT_V(I),NF90_GLOBAL)
 ENDDO
 
 !## get variables
 DO I=1,NVARS
!  IF(.NOT.NC_CHECK(NF90_INQUIRE_VARIABLE(NCID,I,NAME=LAB_VAR(I),XTYPE=XTYPE(I),NDIMS=NDIM_VAR(I),DIMIDS=DIMIDS(:,I),&
!                   NATTS=NATT_VAR(I))))RETURN
  !## get global attributes
  DO J=1,NATT_VAR(I)
   IF(.NOT.NC_CHECK(NF90_INQ_ATTNAME(NCID,I,J,NAME=LAB_ATT(J,I))))RETURN
   !## toevoegende attributen
   IF(.NOT.NC_CHECK(NF90_INQUIRE_ATTRIBUTE(NCID,I,NAME=LAB_ATT(J,I),XTYPE=XTP,LEN=LLEN,ATTNUM=ATTNUM)))RETURN
   CALL NC2IDF_GETATTRIBUTE(LAB_ATT(J,I),LAB_ATT_V(J,I),I)
  ENDDO
 ENDDO

 CALL WDIALOGLOAD(ID_DNETCDF,ID_DNETCDF)
 
 !## fill treeview field
 CALL WDIALOGCLEARFIELD(ID_TREEVIEW1)
 
 K=99
 CALL WDIALOGINSERTTREEVIEWITEM(ID_TREEVIEW1,0,INSERTAFTER,1,'Globals')
 DO I=1,NGATTS
  CALL WDIALOGINSERTTREEVIEWITEM(ID_TREEVIEW1,1,INSERTCHILD,K+I,TRIM(LAB_GATT(I))//'='//TRIM(LAB_GATT_V(I)))
 ENDDO
 K=K+NGATTS
 
 DO I=1,NVARS
  K=K+1
  CALL WDIALOGINSERTTREEVIEWITEM(ID_TREEVIEW1,I  ,INSERTAFTER,I+1,TRIM(LAB_VAR(I))) 
  CALL WDIALOGINSERTTREEVIEWITEM(ID_TREEVIEW1,I+1,INSERTCHILD,K,'ndim='//TRIM(ITOS(NDIM_VAR(I)))) 
  DO J=1,NDIM_VAR(I)
   ID=DIMIDS(J,I)
   CALL WDIALOGINSERTTREEVIEWITEM(ID_TREEVIEW1,K ,INSERTCHILD,K+J,TRIM(LAB_DIM(ID))//'='//TRIM(ITOS(LDIM(ID)))) 
  ENDDO
  K=K+NDIM_VAR(I)
  DO J=1,NATT_VAR(I)
   K=K+1
   CALL WDIALOGINSERTTREEVIEWITEM(ID_TREEVIEW1,I+1,INSERTCHILD,K,TRIM(LAB_ATT(J,I))//'='//TRIM(LAB_ATT_V(J,I)))
  ENDDO
 ENDDO 
 CALL WDIALOGPUTMENU(IDF_MENU1,LAB_VAR,NVARS,1)

 !## make a quess for x ...
 DO I=1,NVARS; IF(INDEX(LAB_VAR(I),'X').GT.0.OR.INDEX(LAB_VAR(I),'x').GT.0)EXIT; ENDDO
 CALL WDIALOGPUTMENU(IDF_MENU3,LAB_VAR,NVARS,MIN(NVARS,I))
 DO I=1,NVARS; IF(INDEX(LAB_VAR(I),'Y').GT.0.OR.INDEX(LAB_VAR(I),'y').GT.0)EXIT; ENDDO
 CALL WDIALOGPUTMENU(IDF_MENU4,LAB_VAR,NVARS,MIN(NVARS,I))
 
 CALL UTL_MESSAGEHANDLE(1)
 CALL NC2IDF_IMPORTNC_FIELDS1()
 CALL NC2IDF_IMPORTNC_FIELDS2()
 CALL NC2IDF_IMPORTNC_FIELDS3()
 CALL WDIALOGSHOW(-1,-1,0,3)
 DO 
  CALL WMESSAGE(ITYPE,MESSAGE)

  SELECT CASE (ITYPE)
   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_MENU1)
      CALL NC2IDF_IMPORTNC_FIELDS1()
     CASE (IDF_RADIO1,IDF_RADIO2)
      CALL NC2IDF_IMPORTNC_FIELDS2()
     CASE (IDF_MENU2) !,IDF_MENU5,IDF_MENU6)
      CALL NC2IDF_IMPORTNC_FIELDS3()
    END SELECT
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDCANCEL,IDOK)
      EXIT
     CASE (IDHELP) 
       !CALL IMODGETHELP('xx','NedCDF 2 IDF')      
    END SELECT
  END SELECT
 ENDDO
 
 CALL WDIALOGGETMENU(IDF_MENU1,IV) !## variable
 CALL WDIALOGGETMENU(IDF_MENU3,IX) !## x
 CALL WDIALOGGETMENU(IDF_MENU4,IY) !## y
 CALL WDIALOGGETREAL(IDF_REAL1,NODATA)

 CALL WDIALOGUNLOAD()
 IF(MESSAGE%VALUE1.EQ.IDCANCEL)RETURN

 CALL UTL_MESSAGEHANDLE(0)
 
 NC2IDF_IMPORTNC_GET=.TRUE.

 END FUNCTION NC2IDF_IMPORTNC_GET

 !###======================================================================
 SUBROUTINE NC2IDF_GETATTRIBUTE(LAB_IN,LAB_OUT,I)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: LAB_IN
 CHARACTER(LEN=*),INTENT(OUT) :: LAB_OUT
 INTEGER,INTENT(IN) :: I
 
 SELECT CASE (XTP)
  CASE (1)
   IF(.NOT.NC_CHECK(NF90_GET_ATT(NCID,I,NAME=LAB_IN,VALUES=IV1)))RETURN  !## INT1
   LAB_OUT=TRIM(ITOS(INT(IV1)))
  CASE (2)
   IV2=''
   IF(.NOT.NC_CHECK(NF90_GET_ATT(NCID,I,NAME=LAB_IN,VALUES=IV2)))RETURN  !## CHR   
   LAB_OUT=TRIM(IV2(1:LEN(IV2)-1))
  CASE (3)
   IF(.NOT.NC_CHECK(NF90_GET_ATT(NCID,I,NAME=LAB_IN,VALUES=IV3)))RETURN  !## INT4
   LAB_OUT=TRIM(ITOS(INT(IV3)))
  CASE (4)
   IF(.NOT.NC_CHECK(NF90_GET_ATT(NCID,I,NAME=LAB_IN,VALUES=IV4)))RETURN  !## INT8
   LAB_OUT=TRIM(ITOS(INT(IV4)))
  CASE (5)
   IF(.NOT.NC_CHECK(NF90_GET_ATT(NCID,I,NAME=LAB_IN,VALUES=IV5)))RETURN  !## REAL
   LAB_OUT=TRIM(RTOS(IV5,'F',7))
  CASE (6)
   IF(.NOT.NC_CHECK(NF90_GET_ATT(NCID,I,NAME=LAB_IN,VALUES=IV6)))RETURN  !## DOUBLE
   LAB_OUT=TRIM(RTOS(REAL(IV6),'F',7))
 END SELECT

 END SUBROUTINE NC2IDF_GETATTRIBUTE

 !###======================================================================
 SUBROUTINE NC2IDF_IMPORTNC_FIELDS1()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J

 CALL WDIALOGGETMENU(IDF_MENU3,I)
 J=1; IF(NDIM_VAR(I).NE.1)J=0; CALL WDIALOGFIELDSTATE(IDOK,J)
 CALL WDIALOGGETMENU(IDF_MENU4,I)
 J=1; IF(NDIM_VAR(I).NE.1)J=0; CALL WDIALOGFIELDSTATE(IDOK,J)

 CALL WDIALOGGETMENU(IDF_MENU1,I)
 CALL WDIALOGPUTMENU(IDF_MENU2,LAB_ATT(:,I),NATT_VAR(I),1) !## nodata
 J=1; IF(NDIM_VAR(I).NE.2)J=0; CALL WDIALOGFIELDSTATE(IDOK,J)

 END SUBROUTINE NC2IDF_IMPORTNC_FIELDS1
 
 !###======================================================================
 SUBROUTINE NC2IDF_IMPORTNC_FIELDS2()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J
 
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,I)
 J=I
 CALL WDIALOGFIELDSTATE(IDF_MENU2,J)
 CALL WDIALOGFIELDSTATE(IDF_LABEL4,J)
 CALL WDIALOGFIELDSTATE(IDF_REAL1,J)

 END SUBROUTINE NC2IDF_IMPORTNC_FIELDS2
 
 !###======================================================================
 SUBROUTINE NC2IDF_IMPORTNC_FIELDS3()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,IOS
 REAL :: X
 
 CALL WDIALOGGETMENU(IDF_MENU1,I) !## variable
 CALL WDIALOGGETMENU(IDF_MENU2,J) !## nodata
 READ(LAB_ATT_V(J,I),*,IOSTAT=IOS) X 
 IF(IOS.NE.0)X=0.0
 CALL WDIALOGPUTREAL(IDF_REAL1,X)

 END SUBROUTINE NC2IDF_IMPORTNC_FIELDS3

 !###======================================================================
 LOGICAL FUNCTION NC2IDF_IMPORTNC_IDF()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,IROW,ICOL,JCOL,JROW
 REAL :: DX1,DX2,DY1,DY2,X
  
 NC2IDF_IMPORTNC_IDF=.FALSE.
 
 I=1
 CALL WINDOWSELECT(0)
 CALL WINDOWOUTSTATUSBAR(4,'Importing '//TRIM(NETCDF_NAME))

 IF(.NOT.NC_CHECK(NF90_INQ_VARID(NCID,LAB_VAR(IV) ,VARID)))RETURN
 IF(.NOT.NC_CHECK(NF90_INQ_VARID(NCID,LAB_VAR(IX),XVARID)))RETURN
 IF(.NOT.NC_CHECK(NF90_INQ_VARID(NCID,LAB_VAR(IY),YVARID)))RETURN

 IDF(I)%NCOL=LDIM(DIMIDS(1,IX))
 IDF(I)%NROW=LDIM(DIMIDS(1,IY))
 
 IDF(I)%IXV =0 !## allocate ()%x
 IDF(I)%ITB =0 !## do not use top/bottom information
 
 IF(.NOT.IDFALLOCATEX(IDF(I)))RETURN
 IF(.NOT.IDFALLOCATESXY(IDF(I)))RETURN

 !## read the x coordinates
 IF(.NOT.NC2IDF_IMPORTNC_READ1D(XVARID,XTYPE(IX),IDF(I)%NCOL,IDF(I)%SX(1:)))RETURN
 !## read the y coordinates
 IF(.NOT.NC2IDF_IMPORTNC_READ1D(YVARID,XTYPE(IY),IDF(I)%NROW,IDF(I)%SY(1:)))RETURN
 !## read the selected data.
 IF(.NOT.NC2IDF_IMPORTNC_READ2D( VARID,XTYPE(IV),IDF(I)%NCOL,IDF(I)%NROW,IDF(I)%X))RETURN
 
 !## get right borders of columns
 IF(.NOT.NC2IDF_IMPORTNC_DX(IDF(I)%NCOL,IDF(I)%SX,DX1,DX2))RETURN
 !## get right borders of columns
 IF(.NOT.NC2IDF_IMPORTNC_DX(IDF(I)%NROW,IDF(I)%SY,DY1,DY2))RETURN
 
 DX1=ABS(DX1); DX2=ABS(DX2)
 DY1=ABS(DY1); DY2=ABS(DY2)
 
 !## check whether coordinate systems are not reversed orde
 IF(IDF(I)%SX(1).LT.IDF(I)%SX(0))THEN
  !## flip along x-axes
  DO IROW=1,IDF(I)%NROW; DO ICOL=1,IDF(I)%NCOL/2
   JCOL=IDF(I)%NCOL-(ICOL-1)
   X=IDF(I)%X(ICOL,IROW); IDF(I)%X(ICOL,IROW)=IDF(I)%X(JCOL,IROW); IDF(I)%X(JCOL,IROW)=X
  ENDDO; ENDDO
  DO ICOL=0,IDF(I)%NCOL/2
   JCOL=IDF(I)%NCOL-ICOL
   X=IDF(I)%SX(ICOL); IDF(I)%SX(ICOL)=IDF(I)%SX(JCOL); IDF(I)%SX(JCOL)=X
  ENDDO
 ENDIF
 IF(IDF(I)%SY(1).GT.IDF(I)%SY(0))THEN
  !## flip along y-axes
  DO ICOL=1,IDF(I)%NCOL; DO IROW=1,IDF(I)%NROW/2
   JROW=IDF(I)%NROW-(IROW-1)
   X=IDF(I)%X(ICOL,IROW); IDF(I)%X(ICOL,IROW)=IDF(I)%X(ICOL,JROW); IDF(I)%X(ICOL,JROW)=X
  ENDDO; ENDDO
  DO IROW=0,IDF(I)%NROW/2
   JROW=IDF(I)%NROW-IROW
   X=IDF(I)%SY(IROW); IDF(I)%SY(IROW)=IDF(I)%SY(JROW); IDF(I)%SY(JROW)=X
  ENDDO
 ENDIF
 
 IDF(I)%XMIN  =IDF(I)%SX(0)
 IDF(I)%XMAX  =IDF(I)%SX(IDF(I)%NCOL)
 IDF(I)%YMIN  =IDF(I)%SY(IDF(I)%NROW)
 IDF(I)%YMAX  =IDF(I)%SY(0)

 IDF(I)%NODATA=NODATA

 IDF(I)%IEQ=1; IF(DX1.EQ.DX2.AND.DY1.EQ.DY2)IDF(I)%IEQ=0
 IDF(I)%DX =DX1; IDF(I)%DY=DY1

 J=INDEX(NETCDF_NAME,'.',.TRUE.)-1
 IF(.NOT.IDFWRITE(IDF(I),NETCDF_NAME(:J)//'.IDF',1))THEN
 !## error occured
 ENDIF
 
 NC2IDF_IMPORTNC_IDF=.TRUE.
 
 END FUNCTION NC2IDF_IMPORTNC_IDF

 !###======================================================================
 LOGICAL FUNCTION NC2IDF_IMPORTNC_DX(N,SX,DX1,DX2)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL,INTENT(OUT) :: DX1,DX2
 REAL,INTENT(INOUT),DIMENSION(0:N) :: SX
 REAL,ALLOCATABLE,DIMENSION(:) :: D
 INTEGER :: I,J
 
 NC2IDF_IMPORTNC_DX=.FALSE.
 
 ALLOCATE(D(N))
 !## compute "thicknesses"
 DO I=1,N-1; D(I)=SX(I+1)-SX(I); ENDDO
 !## search start, if not found than no import possible
 DO I=1,N-1; IF(D(I).EQ.D(I+1))EXIT; ENDDO
 !## no sequence of two identical cells found, is necessary
 IF(I.GT.N-1)RETURN
 J=I
 !## compute cell "thicknesses" before
 DO I=J-1,1,-1; D(I)=(SX(I+1)-0.5*D(I))-SX(I); ENDDO
 !## compute cell "thicknesses" after
 DO I=J+1,N; D(I)=2.0*(SX(I)-(SX(I-1)+0.5*D(I-1))); ENDDO
 !## minimal cellsize; maximal cellsize
 DX1=MINVAL(D); DX2=MAXVAL(D)
 !## compute true sx coordinates of cell edges
 SX(0)=SX(1)-0.5*D(1)
 DO I=1,N; SX(I)=SX(I-1)+D(I); ENDDO
 !## release memory
 DEALLOCATE(D)
 
 NC2IDF_IMPORTNC_DX=.TRUE.

 END FUNCTION NC2IDF_IMPORTNC_DX

 !###======================================================================
 LOGICAL FUNCTION NC2IDF_IMPORTNC_READ1D(XARID,XTYPE_IN,DIM1,X)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: XTYPE_IN,DIM1,XARID
 REAL,INTENT(OUT),DIMENSION(DIM1) :: X
 DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: XR8
 INTEGER(KIND=2),ALLOCATABLE,DIMENSION(:)  :: XI2
 INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:)  :: XI4
 INTEGER(KIND=8),ALLOCATABLE,DIMENSION(:)  :: XI8
 
 NC2IDF_IMPORTNC_READ1D=.FALSE.
 
 !## Read the x-data.
 SELECT CASE (XTYPE_IN)
  CASE (1) !## int(2)
   ALLOCATE(XI2(DIM1))
   IF(.NOT.NC_CHECK(NF90_GET_VAR(NCID,XARID,VALUES=XI2,START=(/1/),COUNT=(/DIM1/))))RETURN  
   X=REAL(XI2)
   DEALLOCATE(XI2)
  CASE (3) !## int(4)
   ALLOCATE(XI4(DIM1))
   IF(.NOT.NC_CHECK(NF90_GET_VAR(NCID,XARID,VALUES=XI4,START=(/1/),COUNT=(/DIM1/))))RETURN 
   X=REAL(XI4)
   DEALLOCATE(XI4)
  CASE (4) !## int(8)
   ALLOCATE(XI8(DIM1))
   IF(.NOT.NC_CHECK(NF90_GET_VAR(NCID,XARID,VALUES=XI8,START=(/1/),COUNT=(/DIM1/))))RETURN 
   X=REAL(XI8)
   DEALLOCATE(XI8)
  CASE (5) !## real(4)
   IF(.NOT.NC_CHECK(NF90_GET_VAR(NCID,XARID,VALUES=X,START=(/1/),COUNT=(/DIM1/))))RETURN 
  CASE (6) !## real(8)
   ALLOCATE(XR8(DIM1))
   IF(.NOT.NC_CHECK(NF90_GET_VAR(NCID,XARID,VALUES=XR8,START=(/1/),COUNT=(/DIM1/))))RETURN  
   X=REAL(XR8)
   DEALLOCATE(XR8)
 END SELECT

 NC2IDF_IMPORTNC_READ1D=.TRUE.
 
 END FUNCTION NC2IDF_IMPORTNC_READ1D

 !###======================================================================
 LOGICAL FUNCTION NC2IDF_IMPORTNC_READ2D(XARID,XTYPE_IN,DIM1,DIM2,X)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: XTYPE_IN,DIM1,DIM2,XARID
 REAL,INTENT(OUT),DIMENSION(DIM1,DIM2) :: X
 DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:) :: XR8
 INTEGER(KIND=2),ALLOCATABLE,DIMENSION(:,:)  :: XI2
 INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:,:)  :: XI4
 INTEGER(KIND=8),ALLOCATABLE,DIMENSION(:,:)  :: XI8

 NC2IDF_IMPORTNC_READ2D=.FALSE.
  
 !## Read the x-data.
 SELECT CASE (XTYPE_IN)
  CASE (1) !## int(2)
   ALLOCATE(XI2(DIM1,DIM2))
   IF(.NOT.NC_CHECK(NF90_GET_VAR(NCID,XARID,VALUES=XI2,START=(/1,1/),COUNT=(/DIM1,DIM2/))))RETURN  
   X=REAL(XI2)
   DEALLOCATE(XI2)
  CASE (3) !## int(4)
   ALLOCATE(XI4(DIM1,DIM2))
   IF(.NOT.NC_CHECK(NF90_GET_VAR(NCID,XARID,VALUES=XI4,START=(/1,1/),COUNT=(/DIM1,DIM2/))))RETURN 
   X=REAL(XI4)
   DEALLOCATE(XI4)
  CASE (4) !## int(8)
   ALLOCATE(XI8(DIM1,DIM2))
   IF(.NOT.NC_CHECK(NF90_GET_VAR(NCID,XARID,VALUES=XI8,START=(/1,1/),COUNT=(/DIM1,DIM2/))))RETURN 
   X=REAL(XI8)
   DEALLOCATE(XI8)
  CASE (5) !## real(4)
   IF(.NOT.NC_CHECK(NF90_GET_VAR(NCID,XARID,VALUES=X,START=(/1,1/),COUNT=(/DIM1,DIM2/))))RETURN 
  CASE (6) !## real(8)
   ALLOCATE(XR8(DIM1,DIM2))
   IF(.NOT.NC_CHECK(NF90_GET_VAR(NCID,XARID,VALUES=XR8,START=(/1,1/),COUNT=(/DIM1,DIM2/))))RETURN  
   X=REAL(XR8)
   DEALLOCATE(XR8)
 END SELECT
 
 NC2IDF_IMPORTNC_READ2D=.TRUE.
 
 END FUNCTION NC2IDF_IMPORTNC_READ2D

 !###======================================================================
 SUBROUTINE NC2IDF_EXPORTNC(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 INTEGER :: XID,XVID,YID,YVID,ZID,TID,TVID,I,J,IPLOT, &
    LONGID,LATID,MAPID,MVID,IROW,ICOL,NPLOT,ICONFIG,IOS
 REAL :: LAT,LONG
 REAL,ALLOCATABLE,DIMENSION(:,:) :: CCLASS
 INTEGER,ALLOCATABLE,DIMENSION(:,:) :: RGB
 CHARACTER(LEN=256) :: FNAME
 LOGICAL :: LEX !,INETCDF
! CHARACTER(LEN=10),DIMENSION(2) :: TXTCONFIG
! DATA TXTCONFIG/'ZTOP','FLUX'/
 
 !## 
! ICONFIG=1  !## top/bot
 ICONFIG=2  !## fluxes
  
 !## find how many idf to be exported into a single NetCDF.
 NPLOT=0
 DO IPLOT=1,MXMPLOT; IF(MP(IPLOT)%ISEL)NPLOT=NPLOT+1; ENDDO
 IF(NPLOT.EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should select one/more IDF files','Error')
  RETURN
 ENDIF
 
 !## get path to save netcdf files
 DO IPLOT=1,MXMPLOT; IF(MP(IPLOT)%ISEL)EXIT; ENDDO
 FNAME=TRIM(MP(IPLOT)%IDFNAME)
 !## replace current directory for selected directory
 FNAME=FNAME(1:INDEX(FNAME,'\',.TRUE.)-1)
 !## ask name of the NetCDF
 CALL WSELECTFILE('NetCDF File (*.nc)|*.nc|',&
      SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,'Save IDF"s into a single NetCDF File')
 IF(WINFODIALOG(4).NE.1)RETURN

 CALL UTL_MESSAGEHANDLE(0)
 
 ALLOCATE(IDF(NPLOT))
 DO I=1,SIZE(IDF); CALL IDFNULLIFY(IDF(I)); ENDDO 
 ALLOCATE(ZVID(NPLOT))
  
 I=0
 DO IPLOT=1,MXMPLOT
  IF(.NOT.MP(IPLOT)%ISEL)CYCLE
  
  I=I+1
  !## read entire IDF
  IF(ID.EQ.ID_MAPEXPORTNC1)THEN
   IF(.NOT.IDFREAD(IDF(I),MP(IPLOT)%IDFNAME,1))CYCLE
  !## window
  ELSEIF(ID.EQ.ID_MAPEXPORTNC2)THEN
   IF(.NOT.IDFREAD(IDF(I),MP(IPLOT)%IDFNAME,0))CYCLE
   IF(.NOT.IDFREADPART(IDF(I),MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX))CYCLE
   CLOSE(IDF(I)%IU)
  !## 
  ELSEIF(ID.EQ.ID_MAPEXPORTNC3)THEN
  
  ENDIF
 ENDDO
 NPLOT=I
  
 !## use first IDF top compute cell-dimensions
 IF(IDF(1)%IEQ.EQ.0)THEN
  IF(.NOT.IDFALLOCATESXY(IDF(1)))RETURN
  IDF(1)%SX(0)=IDF(1)%XMIN; DO J=1,IDF(1)%NCOL; IDF(1)%SX(J)=IDF(1)%SX(J-1)+IDF(1)%DX; ENDDO
  IDF(1)%SY(0)=IDF(1)%YMAX; DO J=1,IDF(1)%NROW; IDF(1)%SY(J)=IDF(1)%SY(J-1)-IDF(1)%DY; ENDDO
 ENDIF
 DO J=IDF(1)%NCOL,1,-1; IDF(1)%SX(J)=(IDF(1)%SX(J-1)+IDF(1)%SX(J))/2.0; ENDDO
 DO J=IDF(1)%NROW,1,-1; IDF(1)%SY(J)=(IDF(1)%SY(J-1)+IDF(1)%SY(J))/2.0; ENDDO
 
 !## create the netCDF file. The nf90_clobber parameter tells netCDF to
 !## overwrite this file, if it already exists.
 NCID=0
 IF(.NOT.NC_CHECK(NF90_CREATE(FNAME,NF90_CLOBBER,NCID)))RETURN

 !## define NetCDF header --- use IDF(1)
 !## add georeference information (global attribute)
 IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,NF90_GLOBAL,'grid_mapping_name','stereographic')))RETURN
 IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,NF90_GLOBAL,'proj','sterea')))RETURN
 IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,NF90_GLOBAL,'latitude_of_projection_center','52.15616055555555')))RETURN
 IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,NF90_GLOBAL,'longitude_of_projection_center','5.38763888888889')))RETURN
 IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,NF90_GLOBAL,'scale_factor_at_natural_center','0.9999079')))RETURN
 IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,NF90_GLOBAL,'projection_x_coordinate_center','155000.0')))RETURN
 IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,NF90_GLOBAL,'projection_y_coordinate_center','463000.0')))RETURN
 IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,NF90_GLOBAL,'ellps','bessel')))RETURN
 IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,NF90_GLOBAL,'units','m')))RETURN
 IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,NF90_GLOBAL,'no_defs','')))RETURN    
 !## www.spatialreference.org
 !+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs 

 !## define the dimensions of the x
 IF(.NOT.NC_CHECK(NF90_DEF_DIM(NCID,'xc',IDF(1)%NCOL,XID)))RETURN
 !## define the dimensions of the y
 IF(.NOT.NC_CHECK(NF90_DEF_DIM(NCID,'yc',IDF(1)%NROW,YID)))RETURN
 
 IF(ICONFIG.NE.0)THEN
  !## define the dimensions of the levels
  IF(.NOT.NC_CHECK(NF90_DEF_DIM(NCID,'lev',IDF(1)%NROW,ZID)))RETURN
 ENDIF
! IF(ICONFIG.EQ.2)THEN
!  !## define the dimensions of the times
!  IF(.NOT.NC_CHECK(NF90_DEF_DIM(NCID,'time',IDF(1)%NROW,TID)))RETURN
! ENDIF
 
 !## define the coordinate variables. They will hold the coordinate
 !## information, that is, the celmids (x/y). A varid is returned for each.
 IF(.NOT.NC_CHECK(NF90_DEF_VAR(NCID,'xc' ,NF90_REAL,XID        ,XVID)))RETURN
 IF(.NOT.NC_CHECK(NF90_DEF_VAR(NCID,'yc' ,NF90_REAL,YID        ,YVID)))RETURN
!   IF(.NOT.NC_CHECK(NF90_DEF_VAR(NCID,'iMOD:Long'      ,NF90_REAL   ,(/XID,YID/),LONGID)))RETURN
!   IF(.NOT.NC_CHECK(NF90_DEF_VAR(NCID,'iMOD:Lat'       ,NF90_REAL   ,(/XID,YID/),LATID)))RETURN
   
 !## define variables
 I=0
 DO IPLOT=1,MXMPLOT
  IF(.NOT.MP(IPLOT)%ISEL)CYCLE
  I=I+1
  !## define the netCDF variables. The dimids array is used to pass the dimids of the dimensions of the netCDF variables.
  IF(.NOT.NC_CHECK(NF90_DEF_VAR(NCID,TRIM(MP(IPLOT)%ALIAS),NF90_REAL,(/XID,YID/),ZVID(I))))RETURN
 ENDDO
   
 !## assign units attributes to coordinate var data. This attaches a
 !## global text attribute to each of the coordinate variables, containing the units.
 IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,XVID  ,'units','m')))RETURN
 IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,XVID  ,'standard_name','projection_x_coordinate')))RETURN
 IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,XVID  ,'axes','X')))RETURN
 IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,XVID  ,'long_name','x-coordinate in Cartesian system')))RETURN
 !## attribute of y coordinates
 IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,YVID  ,'units','m')))RETURN
 IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,YVID  ,'standard_name','projection_y_coordinate')))RETURN
 IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,YVID  ,'axes','Y')))RETURN
 IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,YVID  ,'long_name','y-coordinate in Cartesian system')))RETURN
! !## attribute of levels
! IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,ZVID  ,'units','level')))RETURN
! IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,ZVID  ,'standard_name','underground_water_coordinate')))RETURN
! IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,ZVID  ,'positive','up')))RETURN
! IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,ZVID  ,'long_name','z-coordinate of elevation in meter + NAP')))RETURN
! IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,ZVID  ,'formula_terms','(TOP_laag{lev}.IDF-BOT_laag{lev}.IDF)/2.0')))RETURN
! !## attribute of time
! IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,TVID  ,'units','days since 1990-1-1 0:0:0')))RETURN

!  !## attribute of long coordinates
!  IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,LONGID,'units','degrees_east')))RETURN
!  IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,LONGID,'long_name','Longitude Coordinate')))RETURN
!  IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,LONGID,'standard_name','longitude')))RETURN  
!  !## attribute of lat coordinates
!  IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,LATID ,'units','degrees_north')))RETURN
!  IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,LATID ,'long_name','Latitude Coordinate')))RETURN
!  IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,LATID ,'standard_name','latitude')))RETURN  

 !## put attributes variables
 I=0
 DO IPLOT=1,MXMPLOT
  IF(.NOT.MP(IPLOT)%ISEL)CYCLE
  I=I+1
  !## attribute of z (value) coordinates
  IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,ZVID(I),'standard_name',TRIM(MP(IPLOT)%ALIAS))))RETURN
  SELECT CASE (ICONFIG)
   CASE (0)
    IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,ZVID(I),'units','---')))RETURN
   CASE (1)
    IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,ZVID(I),'units','m NAP')))RETURN
   CASE (2)
    IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,ZVID(I),'units','m d-1')))RETURN
  END SELECT
  IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,ZVID(I),'long_name',TRIM(MP(IPLOT)%IDFNAME))))RETURN
  IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,ZVID(I),'missing_value',IDF(I)%NODATA)))RETURN
  IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,ZVID(I),'valid_max',IDF(I)%DMAX)))RETURN
  IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,ZVID(I),'valid_min',IDF(I)%DMIN)))RETURN
  IF(.NOT.NC_CHECK(NF90_PUT_ATT(NCID,ZVID(I),'grid_mapping','projection')))RETURN
 ENDDO

 !## end define mode.
 IF(.NOT.NC_CHECK(NF90_ENDDEF(NCID)))RETURN
 
 !## write the coordinate variable data. This will put the xmids and ymids of our data grid into the netCDF file.
 IF(.NOT.NC_CHECK(NF90_PUT_VAR(NCID,XVID,IDF(1)%SX(1:IDF(1)%NCOL))))RETURN
!  !## translate sx into longitudes
!  DO IROW=1,IDF%NROW
!   DO ICOL=1,IDF%NCOL
!    CALL GOOGLE_LATLONG(IDF%SX(ICOL),IDF%SY(IROW),LAT,LONG)
!    IDF%X(ICOL,IROW)=LONG
!   ENDDO
!  ENDDO
!  !## write the longitude variable data. This will put the xmids and ymids of our data grid into the netCDF file.
!  IF(.NOT.NC_CHECK(NF90_PUT_VAR(NCID,LONGID,IDF%X)))CYCLE
  
 IF(.NOT.NC_CHECK(NF90_PUT_VAR(NCID,YVID,IDF(1)%SY(1:IDF(1)%NROW))))RETURN
!  !## translate sx into latitudes
!  DO IROW=1,IDF%NROW
!   DO ICOL=1,IDF%NCOL
!    CALL GOOGLE_LATLONG(IDF%SY(ICOL),IDF%SY(IROW),LAT,LONG)
!    IDF%X(ICOL,IROW)=LAT
!   ENDDO
!  ENDDO
!  !## write the longitude variable data. This will put the xmids and ymids of our data grid into the netCDF file.
!  IF(.NOT.NC_CHECK(NF90_PUT_VAR(NCID,LONGID,IDF%SX(1:IDF%NCOL))))CYCLE

 I=0
 DO IPLOT=1,MXMPLOT
  IF(.NOT.MP(IPLOT)%ISEL)CYCLE
  I=I+1
  !## write the pretended data. The arrays of data are the same size as the netCDF variables we have defined.
  IF(.NOT.NC_CHECK(NF90_PUT_VAR(NCID,ZVID(I),IDF(I)%X)))RETURN
 ENDDO

 CALL NC2IDF_EXPORTNC_CLOSE()
 
 CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Succesfully converted selected IDF file(s) to:'//CHAR(13)// &
    TRIM(FNAME)//CHAR(13)//' in NetCDF 3 format (*.nc)','Information')

 END SUBROUTINE NC2IDF_EXPORTNC

 !###======================================================================
 SUBROUTINE NC2IDF_EXPORTNC_CLOSE()
 !###======================================================================
 IMPLICIT NONE
 
 !## Close the file.
 IF(NCID.NE.0)THEN
  IF(.NOT.NC_CHECK(NF90_CLOSE(NCID)))THEN
  ENDIF
  NCID=0
 ENDIF
 IF(ALLOCATED(IDF))THEN
  CALL IDFDEALLOCATE(IDF,SIZE(IDF))
  DEALLOCATE(IDF)
 ENDIF
 IF(ALLOCATED(ZVID))DEALLOCATE(ZVID)
 IF(ALLOCATED(XNETCDF))DEALLOCATE(XNETCDF) 
 CALL UTL_MESSAGEHANDLE(1)

 END SUBROUTINE NC2IDF_EXPORTNC_CLOSE
 
 !###======================================================================
 LOGICAL FUNCTION NC_CHECK(STATUS)
 !###======================================================================
 INTEGER,INTENT(IN) :: STATUS
   
 NC_CHECK=.TRUE.
 IF(STATUS.EQ.NF90_NOERR)RETURN
  
 CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not open/read selected NetCDF-file:'//CHAR(13)//TRIM(NETCDF_NAME)//CHAR(13)// &
  'Error syntax:'//CHAR(13)//TRIM(NF90_STRERROR(STATUS)),'Error')

 NC_CHECK=.FALSE.

 END FUNCTION NC_CHECK  

END MODULE MOD_NC2IDF
