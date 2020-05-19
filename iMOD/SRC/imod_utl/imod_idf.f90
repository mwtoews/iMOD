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
MODULE MOD_IDF

USE WINTERACTER
USE MOD_UTL, ONLY : UTL_GETUNIT,JD,UTL_CAP,UTL_IDFGETDATE,UTL_IDATETOJDATE,ITOS,RTOS,UTL_FILLARRAY,NEWLINE,UTL_CREATEDIR, &
        UTL_IDFGETCLASS,UTL_GETMED,UTL_GETMOSTFREQ,UTL_IDFSNAPTOGRID,UTL_IMODVERSION,UTL_GETRECORDLENGTH,UTL_PUTRECORDLENGTH
USE MOD_POLINT, ONLY : POL1LOCATE,POL1INTMAIN
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_OSD, ONLY : OSD_OPEN,OSD_IOSTAT_MSG,OSD_DATE_AND_TIME,OSD_GETENV
USE MODPLOT, ONLY : LEGENDOBJ
USE MOD_QKSORT

CONTAINS

 !###======================================================================
 SUBROUTINE IDFWRITEFREE(IU,IDF,IINT,CPOS,TSEP)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,IINT
 CHARACTER(LEN=1),INTENT(IN) :: CPOS
 CHARACTER(LEN=*),INTENT(IN) :: TSEP
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER :: IROW

 IF(CPOS.EQ.'T'.OR.CPOS.EQ.'t')CALL IDFWRITEFREE_HEADER(IU,IDF)
 
 DO IROW=1,IDF%NROW
  
  CALL IDFWRITEFREE_ROW(IU,IDF%X(:,IROW),IDF%NCOL,IDF%NODATA,IINT,TSEP)
  
 ENDDO
  
 IF(CPOS.EQ.'B'.OR.CPOS.EQ.'b')CALL IDFWRITEFREE_HEADER(IU,IDF)

 END SUBROUTINE IDFWRITEFREE

 !###======================================================================
 SUBROUTINE IDFWRITEFREE_ROW(IU,X,NCOL,NODATA,IINT,TSEP)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,NCOL,IINT
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(NCOL) :: X
 CHARACTER(LEN=*),INTENT(IN) :: TSEP
 REAL(KIND=DP_KIND),INTENT(IN) :: NODATA
 INTEGER :: ICOL,N
 REAL(KIND=DP_KIND) :: XC,PC
 LOGICAL :: LEX
 CHARACTER(LEN=52) :: LINE
  
 N=1; XC=X(1)
 DO ICOL=1,NCOL

  LEX=.FALSE.
  IF(ICOL.LT.NCOL)THEN
   IF(X(ICOL+1).NE.XC)LEX=.TRUE.
  ENDIF 
  IF(ICOL.EQ.NCOL)LEX=.TRUE.

  IF(LEX)THEN

   !## replace by replace-value in case of nodata-value
   PC=XC; IF(NODATA.EQ.PC)PC=0.0D0
    
   !## write values   
   IF(N.GT.1)THEN
    IF(IINT.EQ.0)LINE=TRIM(ITOS(N))//TRIM(TSEP)//TRIM(RTOS(PC,'*',0))
    IF(IINT.EQ.1)LINE=TRIM(ITOS(N))//TRIM(TSEP)//TRIM(ITOS(INT(PC)))
    WRITE(IU,'(A)') TRIM(LINE)
   ELSE
    IF(IINT.EQ.0)WRITE(IU,*) PC
    IF(IINT.EQ.1)WRITE(IU,*) INT(PC)
   ENDIF
    
   IF(ICOL.LT.NCOL)THEN
    N=1; XC=X(ICOL+1)
   ENDIF

  ELSE

   N=N+1

  ENDIF

 ENDDO

 END SUBROUTINE IDFWRITEFREE_ROW
 
 !###======================================================================
 LOGICAL FUNCTION IDFREADFREE(IU,IDF,CPOS)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 CHARACTER(LEN=1),INTENT(IN) :: CPOS
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER :: IOS,IROW,ICOL

 IDFREADFREE=.FALSE.

 IF(CPOS.EQ.'T'.OR.CPOS.EQ.'t')THEN
  IF(.NOT.IDFREADFREE_HEADER(IU,IDF))RETURN
 ENDIF
 
 IF(.NOT.IDFALLOCATEX(IDF))RETURN
 
 READ(IU,*,IOSTAT=IOS) ((IDF%X(ICOL,IROW),ICOL=1,IDF%NCOL),IROW=1,IDF%NROW)
 IF(IOS.NE.0)RETURN
   
 IF(CPOS.EQ.'B'.OR.CPOS.EQ.'b')THEN
  IF(.NOT.IDFREADFREE_HEADER(IU,IDF))RETURN
 ENDIF
 
 IDFREADFREE=.TRUE.

 END FUNCTION IDFREADFREE

 !###======================================================================
 SUBROUTINE IDFWRITEFREE_HEADER(IU,IDF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER :: ICOL,IROW
 
 WRITE(IU,*) 'DIMENSIONS'
 WRITE(IU,*) IDF%NCOL
 WRITE(IU,*) IDF%NROW
 WRITE(IU,*) IDF%XMIN
 WRITE(IU,*) IDF%YMIN
 WRITE(IU,*) IDF%XMAX
 WRITE(IU,*) IDF%YMAX
 WRITE(IU,*) IDF%NODATA
 WRITE(IU,*) IDF%IEQ
 IF(IDF%IEQ.EQ.0)THEN
  WRITE(IU,*) IDF%DX
  WRITE(IU,*) IDF%DY
 ELSE
  DO ICOL=0,IDF%NCOL; WRITE(IU,*) IDF%SX(ICOL); ENDDO
  DO IROW=0,IDF%NROW; WRITE(IU,*) IDF%SY(IROW); ENDDO 
 ENDIF

 END SUBROUTINE IDFWRITEFREE_HEADER

 !###======================================================================
 LOGICAL FUNCTION IDFREADFREE_HEADER(IU,IDF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER :: ICOL,IROW
 
 IDFREADFREE_HEADER=.FALSE.
 
 READ(IU,*) !'DIMENSIONS'
 READ(IU,*) IDF%NCOL
 READ(IU,*) IDF%NROW
 READ(IU,*) IDF%XMIN
 READ(IU,*) IDF%YMIN
 READ(IU,*) IDF%XMAX
 READ(IU,*) IDF%YMAX
 READ(IU,*) IDF%NODATA
 READ(IU,*) IDF%IEQ
 IF(IDF%IEQ.EQ.0)THEN
  READ(IU,*) IDF%DX
  READ(IU,*) IDF%DY
 ELSE
  IF(.NOT.IDFALLOCATESXY(IDF))RETURN
  DO ICOL=0,IDF%NCOL; READ(IU,*) IDF%SX(ICOL); ENDDO
  DO IROW=0,IDF%NROW; READ(IU,*) IDF%SY(IROW); ENDDO 
 ENDIF

 IDFREADFREE_HEADER=.TRUE.
 
 END FUNCTION IDFREADFREE_HEADER
 
 !###======================================================================
 LOGICAL FUNCTION IDF_EXTENT(N,IDF1,IDF2,IOPTION)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,IOPTION
 TYPE(IDFOBJ),INTENT(IN),DIMENSION(N) :: IDF1
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF2 
 INTEGER :: I,J
 
 IDF_EXTENT=.FALSE.
 
 !## maximum size
 IF(IOPTION.EQ.1)THEN
  J=0; DO I=1,N
   !## no idf given/readin
   IF(IDF1(I)%NCOL.EQ.0.OR.IDF1(I)%NROW.EQ.0)CYCLE
   J=J+1
   IF(J.EQ.1)THEN
    IDF2%XMIN=IDF1(I)%XMIN
    IDF2%XMAX=IDF1(I)%XMAX
    IDF2%YMIN=IDF1(I)%YMIN
    IDF2%YMAX=IDF1(I)%YMAX
   ELSE 
    IDF2%XMIN=MIN(IDF2%XMIN,IDF1(I)%XMIN)
    IDF2%XMAX=MAX(IDF2%XMAX,IDF1(I)%XMAX)
    IDF2%YMIN=MIN(IDF2%YMIN,IDF1(I)%YMIN)
    IDF2%YMAX=MAX(IDF2%YMAX,IDF1(I)%YMAX)
   ENDIF
  ENDDO
 !## minimum size
 ELSEIF(IOPTION.EQ.2)THEN
  J=0; DO I=1,N
   !## no idf given/readin
   IF(IDF1(I)%NCOL.EQ.0.OR.IDF1(I)%NROW.EQ.0)CYCLE
   J=J+1
   IF(J.EQ.1)THEN
    IDF2%XMIN=IDF1(I)%XMIN
    IDF2%XMAX=IDF1(I)%XMAX
    IDF2%YMIN=IDF1(I)%YMIN
    IDF2%YMAX=IDF1(I)%YMAX
   ELSE 
    IDF2%XMIN=MAX(IDF2%XMIN,IDF1(I)%XMIN)
    IDF2%XMAX=MIN(IDF2%XMAX,IDF1(I)%XMAX)
    IDF2%YMIN=MAX(IDF2%YMIN,IDF1(I)%YMIN)
    IDF2%YMAX=MIN(IDF2%YMAX,IDF1(I)%YMAX)
   ENDIF
  ENDDO
 ENDIF
 J=0; DO I=1,N
  IF(IDF1(I)%NCOL.EQ.0.OR.IDF1(I)%NROW.EQ.0)CYCLE
  J=J+1
  IF(J.EQ.1)THEN
   IDF2%DX=IDF1(I)%DX
   IDF2%DY=IDF1(I)%DY 
  ELSE
   IDF2%DX=MIN(IDF2%DX,IDF1(I)%DX)
   IDF2%DY=MIN(IDF2%DY,IDF1(I)%DY)   
  ENDIF
 ENDDO
 CALL UTL_IDFSNAPTOGRID(IDF2%XMIN,IDF2%XMAX,IDF2%YMIN,IDF2%YMAX,IDF2%DX,IDF2%NCOL,IDF2%NROW)
 
 IDF_EXTENT=.TRUE.
 
 END FUNCTION IDF_EXTENT
 
! !###======================================================================
! LOGICAL FUNCTION IDFCREATEIVF(DIRNAME,TXT,TBNAME,NLAY)
! !###======================================================================
! IMPLICIT NONE
! CHARACTER(LEN=*),INTENT(IN) :: DIRNAME,TXT
! CHARACTER(LEN=*),INTENT(IN),DIMENSION(NLAY,2) :: TBNAME
! INTEGER,INTENT(IN) :: NLAY
! TYPE(IDFOBJ),DIMENSION(:),ALLOCATABLE :: IDF
! CHARACTER(LEN=6),DIMENSION(4) :: BDGTXT
! DATA BDGTXT/'BDGFRF','BDGFFF','BDGFLF','BDGFLF'/
! INTEGER :: I,IROW,ICOL,ILAY
! LOGICAL :: LZERO
! 
! IDFCREATEIVF=.FALSE.
! 
! ALLOCATE(IDF(7)); DO I=1,SIZE(IDF); CALL IDFNULLIFY(IDF(I)); ENDDO
! 
! DO ILAY=1,NLAY
!  DO I=1,SIZE(BDGTXT)
!   LZERO=.FALSE.
!   !## bdgflf
!   IF(I.EQ.3)THEN
!    IF(ILAY.GT.1)THEN
!     IDF(I)%FNAME=TRIM(DIRNAME)//'\'//TRIM(BDGTXT(I))//'\'//TRIM(BDGTXT(I))//'_'//TRIM(TXT)//'_L'//TRIM(ITOS(ILAY-1))//'.IDF'
!    ELSE
!     IDF(I)%FNAME=TRIM(DIRNAME)//'\'//TRIM(BDGTXT(1))//'\'//TRIM(BDGTXT(1))//'_'//TRIM(TXT)//'_L'//TRIM(ITOS(ILAY))//'.IDF'
!     LZERO=.TRUE.
!    ENDIF
!    IF(ILAY.LT.NLAY)THEN
!     IDF(I)%FNAME=TRIM(DIRNAME)//'\'//TRIM(BDGTXT(I))//'\'//TRIM(BDGTXT(I))//'_'//TRIM(TXT)//'_L'//TRIM(ITOS(ILAY))//'.IDF'
!    ELSE
!     IDF(I)%FNAME=TRIM(DIRNAME)//'\'//TRIM(BDGTXT(1))//'\'//TRIM(BDGTXT(1))//'_'//TRIM(TXT)//'_L'//TRIM(ITOS(ILAY))//'.IDF'
!     LZERO=.TRUE.
!    ENDIF
!   ELSE
!    IDF(I)%FNAME=TRIM(DIRNAME)//'\'//TRIM(BDGTXT(I))//'\'//TRIM(BDGTXT(I))//'_'//TRIM(TXT)//'_L'//TRIM(ITOS(ILAY))//'.IDF'
!   ENDIF
!   IF(.NOT.IDFREAD(IDF(I),IDF(I)%FNAME,1,IQ=1))EXIT
!   IF(LZERO)IDF(I)%X=0.0D0
!  ENDDO
!  
!  !## read top/bottoms using dimensions of the flux-files
!  CALL IDFCOPY(IDF(1),IDF(5)); CALL IDFDEALLOCATEX(IDF(5))
!  CALL IDFCOPY(IDF(1),IDF(6)); CALL IDFDEALLOCATEX(IDF(6))
!  IF(.NOT.IDFREADSCALE(TBNAME(ILAY,1),IDF(5),2,1,0.0D0,0))THEN; RETURN; ENDIF !## top
!  IF(.NOT.IDFREADSCALE(TBNAME(ILAY,2),IDF(6),2,1,0.0D0,0))THEN; RETURN; ENDIF !## bot
!
!  !## create vector-idf
!  CALL IDFCOPY(IDF(1),IDF(7)); CALL IDFDEALLOCATEX(IDF(7))
!  IDF(7)%IXV=3; IF(.NOT.IDFALLOCATEX(IDF(7)))THEN; ENDIF
!  
!  !## compute velocities
!  DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
!!   DX1=0.0D0; IF(ICOL.GT.1)DX1=IDF(1)%X(ICOL-1,IROW); DX2=IDF(1)%X(ICOL,IROW)
!!   DY1=0.0D0; IF(IROW.GT.1)DY1=IDF(1)%X(ICOL,IROW-1); DY2=IDF(2)%X(ICOL,IROW)
!!   DZ1=IDF(3)%X(ICOL,IROW); DZ2=IDF(4)%X(ICOL,IROW)
!
!!   !## thickness of aquifer
!!   D =  IDF(5)%X(ICOL,IROW)-IDF(6)%X(ICOL,IROW)
!!   A  = IDFGETAREA(IDF(1),ICOL,IROW)
!!   !## mean flux/velocity
!!   DX=((DX1+DX2)/2.0)/D; DY=((DY1+DY2)/2.0)/D; DZ=((DZ1+DZ2)/2.0)/A
!
!   !## velocity (m3/day)
!   IDF(7)%XV(ICOL,IROW,1)=IDF(5)%X(ICOL,IROW)
!   IDF(7)%XV(ICOL,IROW,2)=IDF(6)%X(ICOL,IROW)
!   IDF(7)%XV(ICOL,IROW,3)=IDF(1)%X(ICOL,IROW)
!   IDF(7)%XV(ICOL,IROW,4)=IDF(2)%X(ICOL,IROW)
!   IDF(7)%XV(ICOL,IROW,5)=IDF(3)%X(ICOL,IROW)
!   IDF(7)%XV(ICOL,IROW,6)=IDF(4)%X(ICOL,IROW)
!
!  ENDDO; ENDDO  
!  IDF(7)%FNAME=TRIM(DIRNAME)//'\VECTOR\VECTOR_'//TRIM(TXT)//'_L'//TRIM(ITOS(ILAY))//'.IDF'
!  IF(IDFWRITE(IDF(7),IDF(7)%FNAME,1))IDFCREATEIVF=.TRUE.
! ENDDO
! CALL IDFDEALLOCATE(IDF,SIZE(IDF))  
! 
! END FUNCTION IDFCREATEIVF
 
 !###======================================================================
 INTEGER FUNCTION IDFGETICLR(IDF,LEG,UNITS,IROW,ICOL,UMIN,UMAX)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 TYPE(LEGENDOBJ),INTENT(INOUT) :: LEG
 REAL(KIND=DP_KIND),INTENT(INOUT) :: UMIN,UMAX
 INTEGER,INTENT(IN) :: IROW,ICOL,UNITS
 REAL(KIND=DP_KIND) :: GRD
 INTEGER :: ICLR

 GRD=IDFGETVAL(IDF,IROW,ICOL,UNITS)
 !## make sure GRD is a number
 IF(GRD.NE.IDF%NODATA.AND.GRD.EQ.GRD)THEN
  ICLR=UTL_IDFGETCLASS(LEG,GRD)
  UMIN=MIN(UMIN,GRD)
  UMAX=MAX(UMAX,GRD)
 ELSE
  ICLR=WRGB(255,255,255)
 ENDIF
 IDFGETICLR=ICLR

 END FUNCTION IDFGETICLR

 !###======================================================================
 LOGICAL FUNCTION IDFREAD(IDF,IDFNAME,IDATA,IQ)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 INTEGER,INTENT(IN) :: IDATA 
 INTEGER,INTENT(IN),OPTIONAL :: IQ !## 0=questioning 1=no questioning
 CHARACTER(LEN=2) :: TXT
 INTEGER :: IOPEN,IQUEST,IOS
 REAL(KIND=DP_KIND) :: X
 
 IDFREAD=.FALSE.

 TXT='RO'                   !## read only
 IF(IDATA.EQ.-1)TXT='RW'    !## read/write

 IOPEN=ABS(IDATA)
! SELECT CASE (IDATA)
!  !## direct access
!  CASE (0,-1)
!   IOPEN=0
!  !## stream access
!  CASE (1)
!   IOPEN=1
! END SELECT

 IQUEST=0; IF(PRESENT(IQ))IQUEST=IQ
  
 !## check whether it is a constant in filename
 READ(IDFNAME,*,IOSTAT=IOS) X
 IF(IOS.EQ.0)THEN
  IF(IDATA.EQ.1)THEN
   IF(ASSOCIATED(IDF%X))THEN
    IDF%X=X; IDFREAD=.TRUE.; RETURN
   ENDIF
  ELSE
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot read from a constant value without allocating data block.','Error'); RETURN
  ENDIF
 ENDIF
 
 !## open idf
 CALL IDFNULLIFY(IDF)
 IF(IDFOPEN(IDF%IU,IDFNAME,TXT,IDF%ITYPE,IOPEN,IQUESTION=IQUEST))THEN
  IF(IDFREADDIM(IOPEN,IDF))THEN
   IDF%FNAME=IDFNAME
   IF(IDFREADDATA(IDATA,IDF))THEN
    !## get gregorian-date if possible
    IDF%JD=UTL_IDFGETDATE(IDFNAME,IDF%DAYFRACTION,IDY=IDF%IDY,IMH=IDF%IMH,IYR=IDF%IYR,IHR=IDF%IHR,IMT=IDF%IMT,ISC=IDF%ISC)
    !## get julian-date if possible
    IF(IDF%JD.NE.0)THEN
     IDF%JD=UTL_IDATETOJDATE(IDF%JD)
    ENDIF
    IDF%ILAY=IDFGETILAY(IDFNAME)
    IF(IDATA.EQ.1)CALL IDFGETCOMMENT(IDF,IOPEN) 
    IDFREAD=.TRUE.
   ELSE
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error occured reading DATA from IDF'//CHAR(13)//TRIM(IDFNAME),'Error')
    CLOSE(IDF%IU); IDF%IU=0
   ENDIF
  ELSE
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error occured reading DIMENSIONS from IDF'//CHAR(13)//TRIM(IDFNAME),'Error')
   IF(IDF%IU.GT.0)CLOSE(IDF%IU); IDF%IU=0
  ENDIF
 ENDIF

 !## if stream access, close file
 IF(IDATA.EQ.1)THEN; IF(IDF%IU.GT.0)CLOSE(IDF%IU); IDF%IU=0; ENDIF

 END FUNCTION IDFREAD

 !###======================================================================
 LOGICAL FUNCTION IDFREADREAL(IDATA,IU,ITYPE,IREC,DVAL) !,INICE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDATA,IU,ITYPE !,INICE
 INTEGER(KIND=DP_KIND),INTENT(IN) :: IREC
 REAL(KIND=DP_KIND),INTENT(OUT) :: DVAL
 INTEGER :: IOS
 REAL(KIND=SP_KIND) :: RVAL
 CHARACTER(LEN=20) :: TXT
 
 IDFREADREAL=.FALSE.

 IF(ITYPE.EQ.4)THEN
  IF(IDATA.EQ.0)READ(IU,REC=IREC,IOSTAT=IOS) RVAL
  IF(IDATA.EQ.1)READ(IU,         IOSTAT=IOS) RVAL
!  !## convert to double
!  IF(INICE.EQ.1)THEN
  WRITE(TXT,*) RVAL; READ(TXT,*) DVAL
!  ELSE
!   DVAL=REAL(RVAL,8)
!  ENDIF
 ELSEIF(ITYPE.EQ.8)THEN
  IF(IDATA.EQ.0)READ(IU,REC=IREC,IOSTAT=IOS) DVAL
  IF(IDATA.EQ.1)READ(IU,         IOSTAT=IOS) DVAL
 ENDIF
 
 IF(IOS.EQ.0)IDFREADREAL=.TRUE.
 
 END FUNCTION IDFREADREAL
 
 !###======================================================================
 LOGICAL FUNCTION IDFWRITEREAL(IDATA,IU,ITYPE,IREC,DVAL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDATA,IU,ITYPE
 INTEGER(KIND=DP_KIND),INTENT(IN) :: IREC
 REAL(KIND=DP_KIND),INTENT(IN) :: DVAL
 INTEGER :: IOS
 REAL(KIND=SP_KIND) :: RVAL
  
 IDFWRITEREAL=.FALSE.

 IF(ITYPE.EQ.4)THEN
  RVAL=REAL(DVAL,4)
  IF(IDATA.EQ.0)WRITE(IU,REC=IREC,IOSTAT=IOS) RVAL
  IF(IDATA.EQ.1)WRITE(IU,         IOSTAT=IOS) RVAL
 ELSEIF(ITYPE.EQ.8)THEN
  IF(IDATA.EQ.0)WRITE(IU,REC=IREC,IOSTAT=IOS) DVAL
  IF(IDATA.EQ.1)WRITE(IU,         IOSTAT=IOS) DVAL
 ENDIF
 
 IF(IOS.EQ.0)IDFWRITEREAL=.TRUE.
 
 END FUNCTION IDFWRITEREAL

 !###======================================================================
 LOGICAL FUNCTION IDFREADINTEGER(IDATA,IU,ITYPE,IREC,IVAL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDATA,IU,ITYPE
 INTEGER(KIND=DP_KIND),INTENT(IN) :: IREC
 INTEGER,INTENT(OUT) :: IVAL
 INTEGER :: I,IOS
  
 IDFREADINTEGER=.FALSE.

 IF(ITYPE.EQ.4)THEN
  IF(IDATA.EQ.0)READ(IU,REC=IREC,IOSTAT=IOS) IVAL
  IF(IDATA.EQ.1)READ(IU,         IOSTAT=IOS) IVAL
 ELSEIF(ITYPE.EQ.8)THEN
  IF(IDATA.EQ.0)READ(IU,REC=IREC,IOSTAT=IOS) IVAL
  IF(IDATA.EQ.1)READ(IU,         IOSTAT=IOS) IVAL,I
 ENDIF
 
 IF(IOS.EQ.0)IDFREADINTEGER=.TRUE.
 
 END FUNCTION IDFREADINTEGER

 !###======================================================================
 LOGICAL FUNCTION IDFWRITEINTEGER(IDATA,IU,ITYPE,IREC,IVAL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDATA,IU,ITYPE
 INTEGER(KIND=DP_KIND),INTENT(IN) :: IREC
 INTEGER,INTENT(IN) :: IVAL
 INTEGER :: IOS
  
 IDFWRITEINTEGER=.FALSE.

 IF(ITYPE.EQ.4)THEN
  IF(IDATA.EQ.0)WRITE(IU,REC=IREC,IOSTAT=IOS) IVAL
  IF(IDATA.EQ.1)WRITE(IU,         IOSTAT=IOS) IVAL
 ELSEIF(ITYPE.EQ.8)THEN
  IF(IDATA.EQ.0)WRITE(IU,REC=IREC,IOSTAT=IOS) IVAL
  IF(IDATA.EQ.1)WRITE(IU,         IOSTAT=IOS) IVAL,INT(0,4)
 ENDIF
 
 IF(IOS.EQ.0)IDFWRITEINTEGER=.TRUE.
 
 END FUNCTION IDFWRITEINTEGER

 !###======================================================================
 LOGICAL FUNCTION IDFREADCROSS(IDF,IDFNAME)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 TYPE(IDFOBJ) :: TMP
 REAL(KIND=DP_KIND) :: DX,DY,A,X1,Y1,X,Y
 INTEGER :: ICOL,IROW,JROW,JCOL
  
 IDFREADCROSS=.FALSE.

 !## read dimensions in other object
 IF(.NOT.IDFREAD(TMP,IDFNAME,0))RETURN
 DX=IDF%XMAX-IDF%XMIN; DY=IDF%YMAX-IDF%YMIN
 X1=IDF%XMIN; Y1=IDF%YMIN; A=ATAN(DY/DX)
 
 DO IROW=1,IDF%NROW
  DX=-0.5*IDF%DX
  DO ICOL=1,IDF%NCOL
   DX=DX+IDF%DX
   X=X1+COS(A)*DX; Y=Y1+SIN(A)*DX
   CALL IDFIROWICOL(TMP,JROW,JCOL,X,Y)
   !## get value
   IDF%X(ICOL,IROW)=IDFGETVAL(TMP,JROW,JCOL)
  ENDDO
 ENDDO
 !## correct ymax
 IDF%YMAX=IDF%YMIN+IDF%NROW*IDF%DY
 IDF%XMAX=IDF%XMIN+IDF%NCOL*IDF%DX
  
 IF(TMP%IU.GT.0)CLOSE(TMP%IU); TMP%IU=0
 
 IDFREADCROSS=.TRUE.

 END FUNCTION IDFREADCROSS

 !###======================================================================
 LOGICAL FUNCTION IDFREADPART(IDF,XMIN,YMIN,XMAX,YMAX)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 REAL(KIND=DP_KIND),INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
 INTEGER :: IR,IR1,IR2,IC,IC1,IC2,IROW,ICOL,NROW,NCOL
 REAL(KIND=DP_KIND) :: DX,DY
 
 IDFREADPART=.FALSE.

! CALL UTL_IDFSNAPTOGRID_LLC(XMIN,XMAX,YMIN,YMAX,IDF%DX,IDF%DY,NCOL,NROW,LLC=.TRUE.)
 
 !## get position to be read from idf inside current view-extent
 DX=0.5D0*IDF%DX
 DY=0.5D0*IDF%DY
 CALL IDFIROWICOL(IDF,IR1,IC1,XMIN+DX,YMAX-DY)
 CALL IDFIROWICOL(IDF,IR2,IC2,XMAX-DX,YMIN+DY)
 !## adjust ic1,ic2
 IF(IDF%XMIN.GE.XMIN)IC1=1; IF(IDF%XMAX.LE.XMAX)IC2=IDF%NCOL
 !## adjust ir1,ir2
 IF(IDF%YMIN.GE.YMIN)IR2=IDF%NROW; IF(IDF%YMAX.LE.YMAX)IR1=1

 IF(IR1.NE.0.AND.IR2.NE.0.AND.IC1.NE.0.AND.IC2.NE.0)THEN
  NROW=IR2-IR1+1; NCOL=IC2-IC1+1
  ALLOCATE(IDF%X(NCOL,NROW))
  IROW=0; DO IR=IR1,IR2
  IROW=IROW+1; ICOL=0; DO IC=IC1,IC2
   ICOL=ICOL+1
   IDF%X(ICOL,IROW)=IDFGETVAL(IDF,IR,IC)
  END DO; END DO
  !## overrule current dimensions of idf()
  IDF%NROW=NROW
  IDF%NCOL=NCOL
  IF(IDF%IEQ.EQ.0)THEN
   IDF%XMAX=IDF%XMIN+ IC2   *IDF%DX
   IDF%XMIN=IDF%XMIN+(IC1-1)*IDF%DX
   IDF%YMIN=IDF%YMAX- IR2   *IDF%DY
   IDF%YMAX=IDF%YMAX-(IR1-1)*IDF%DY
  ELSE
   !## shift coordinates
   IDF%SX(0:IDF%NCOL)=IDF%SX(IC1-1:IC2)
   IDF%SY(0:IDF%NROW)=IDF%SY(IR1-1:IR2)
   IDF%XMIN=IDF%SX(0)
   IDF%XMAX=IDF%SX(IDF%NCOL)
   IDF%YMIN=IDF%SY(IDF%NROW)
   IDF%YMAX=IDF%SY(0)
  ENDIF

  IDFREADPART=.TRUE.

 ELSE
 
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Current file:'//CHAR(13)// &
       TRIM(IDF%FNAME)//CHAR(13)//'is outside graphical window','Error')

 ENDIF

 END FUNCTION IDFREADPART

 !###======================================================================
 LOGICAL FUNCTION IDFREADSCALE(FNAME,IDFM,SCLTYPE_UP,SCLTYPE_DOWN,PERC,IOPTIONAL)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDFM
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER,INTENT(IN) :: SCLTYPE_UP,SCLTYPE_DOWN
 REAL(KIND=DP_KIND),INTENT(IN) :: PERC
 INTEGER,INTENT(IN) :: IOPTIONAL 
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
 LOGICAL :: LEX
 INTEGER :: IOS
 REAL(KIND=DP_KIND) :: X,CS1,CS2
 
 IDFREADSCALE=.FALSE.
 
 READ(FNAME,*,IOSTAT=IOS) X
 IF(IOS.EQ.0)THEN
  IF(.NOT.ASSOCIATED(IDFM%X))THEN
   IF(.NOT.IDFALLOCATEX(IDFM))THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot ALLOCATE memory for a constant value.','Error'); RETURN
   ENDIF
  ENDIF
  IF(ASSOCIATED(IDFM%X))THEN
    IDFM%X=X; IDFM%NODATA=HUGE(1.0); IDFREADSCALE=.TRUE.; RETURN
  ELSE
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot read from a constant value without allocating data block.','Error'); RETURN
  ENDIF
 ENDIF

 ALLOCATE(IDF(1)); CALL IDFNULLIFY(IDF(1))
 !## determine ratio between cutout area and entire idf
 CS1=SQRT((IDFM%XMAX-IDFM%XMIN)**2.0D0+(IDFM%YMAX-IDFM%YMIN)**2.0D0)
 LEX=IDFREAD(IDF(1),FNAME,0,IQ=IOPTIONAL); IF(.NOT.LEX)RETURN; CLOSE(IDF(1)%IU)
 CS2=SQRT((IDF(1)%XMAX-IDF(1)%XMIN)**2.0D0+(IDF(1)%YMAX-IDF(1)%YMIN)**2.0D0)
 !## ratio
 X  =CS1/CS2
 
 !## read entire IDF - cutout is more than 25%
 IF(X.GE.0.25D0)THEN
  !## try to allocate idf(1) and read the entire idf
  LEX=IDFREAD(IDF(1),FNAME,1,IQ=IOPTIONAL)
  IF(.NOT.LEX)THEN
   LEX=IDFREAD(IDF(1),FNAME,0,IQ=IOPTIONAL)
  ENDIF
 ELSE
  LEX=IDFREAD(IDF(1),FNAME,0,IQ=IOPTIONAL)
 ENDIF
 IF(LEX)THEN
  !## copy type of idf
  IDFM%ITYPE=IDF(1)%ITYPE
  IDFREADSCALE=IDFREADSCALE_GETX(IDF(1),IDFM,SCLTYPE_UP,SCLTYPE_DOWN,PERC)
  IDFM%NODATA=IDF(1)%NODATA
  IDFM%ITB=IDF(1)%ITB
  IDFM%TOP=IDF(1)%TOP
  IDFM%BOT=IDF(1)%BOT
 ENDIF
 !## close file if neccessary
 INQUIRE(IDF(1)%IU,OPENED=LEX); IF(LEX)CLOSE(IDF(1)%IU)
 
 CALL IDFDEALLOCATE(IDF,SIZE(IDF)); DEALLOCATE(IDF)
 
 END FUNCTION IDFREADSCALE
  
 !###======================================================================
 LOGICAL FUNCTION IDFREADSCALE_GETX(IDFC,IDFM,SCLTYPE_UP,SCLTYPE_DOWN,PERC)
 ! idfm = mother idf and will return values on grid defined by idfm
 ! idfc = child  idf and uses grid defined by idfc to scale on idfm
 ! scltype_up:
 ! 1 = special (iboundary)
 ! 2 = arithmetic (shead/vcont/s)
 ! 3 = geometrisch (kd)
 ! 4 = sum
 ! 5 = sum(cond)*ratio (riv/drn/ghb conductance/rch/evt)
 ! 6 = inverse (c)
 ! 7 = most frequent occurence
 ! 8 = sum (1/c)*ratio
 ! 9 = percentile 
 !10 = blockvalue
 !11 = darcian not supported here
 !12 = homogenization not supported here
 !13 = global-local not supported here
 !14 = 3-d simulation not supported here
 !15 = zonation
 !16 = artithmetic mean (rch/evt) discarding nodata
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDFM 
 TYPE(IDFOBJ),INTENT(INOUT) :: IDFC
 INTEGER,INTENT(IN) :: SCLTYPE_UP,SCLTYPE_DOWN
 REAL(KIND=DP_KIND),INTENT(IN) :: PERC
 INTEGER :: IRC1,IRC2,ICC1,ICC2,IRM,ICM,MXN,MXM,N,M,I,IINT,IDOWN
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: FREQ
 REAL(KIND=DP_KIND) :: SVALUE,DXM,DYM,DXC,DYC
 REAL(KIND=DP_KIND) :: XD1,XD2,YD1,YD2,TINY
   
 IDFREADSCALE_GETX=.FALSE.

 IINT=4
 IDOWN=0

 !## check whether CHILD array is allocated, otherwise allocate it
 IF(.NOT.IDFALLOCATEX(IDFM))RETURN
  
 !## clean array
 IF(IDFM%IXV.EQ.0)IDFM%X =IDFC%NODATA
 IDFM%NODATA=IDFC%NODATA
 
 !## construct sx/sy arrays for child/mother (if not yet existing)
 IF(.NOT.IDFFILLSXSY(IDFC))RETURN
 IF(.NOT.IDFFILLSXSY(IDFM))RETURN

 !## most-frequent,percentiles
 MXN=1; MXM=1
 IF(SCLTYPE_UP.EQ.7.OR.SCLTYPE_UP.EQ.9.OR.SCLTYPE_UP.EQ.15)THEN 
  MXN=0; DO I=1,IDFM%NCOL; N=(IDFM%SX(I)-IDFM%SX(I-1))/IDFC%DX; MXN=MAX(MXN,N); END DO
  MXN=MXN+2
  MXM=0; DO I=1,IDFM%NROW; M=(IDFM%SY(I-1)-IDFM%SY(I))/IDFC%DY; MXM=MAX(MXM,M); END DO
  MXM=MXM+2

  IF(SCLTYPE_UP.EQ.15)THEN
   ALLOCATE(FREQ(MXN*MXM,2))
  ELSE
   ALLOCATE(FREQ(MXN*MXM,1))
  ENDIF

 ELSE
  MXN=1; MXM=1; ALLOCATE(FREQ(MXN*MXM,1))
 ENDIF
 
 !## read/scale parameters
 DO IRM=1,IDFM%NROW
   
  !## get location to scale/cut data from IDFC
  IF(SCLTYPE_UP.EQ.10)THEN
   YD1=(IDFM%SY(IRM-1)+IDFM%SY(IRM  ))/2.0D0
   CALL POL1LOCATE(IDFC%SY,IDFC%NROW+1,YD1,IRC1)
   IRC2=IRC1
  ELSE
   TINY=MIN(1.0D0,0.01D0*(IDFM%SY(IRM-1)-IDFM%SY(IRM)))
   YD2=IDFM%SY(IRM-1)-TINY
   YD1=IDFM%SY(IRM  )+TINY
   CALL POL1LOCATE(IDFC%SY,IDFC%NROW+1,YD2,IRC1)
   CALL POL1LOCATE(IDFC%SY,IDFC%NROW+1,YD1,IRC2)
  ENDIF
  IRC2=MIN(IRC2,IDFC%NROW)
  IRC1=MAX(IRC1,        1)
  
  IF(IRC2.GE.IRC1.AND.IRC2.LE.IDFC%NROW.AND.IRC1.NE.0.AND.IRC2.NE.0)THEN
   
   DO ICM=1,IDFM%NCOL

    !## get location to scale/cut data from IDFC
    IF(SCLTYPE_UP.EQ.10)THEN
     XD1=(IDFM%SX(ICM-1)+IDFM%SX(ICM))/2.0D0
     CALL POL1LOCATE(IDFC%SX,IDFC%NCOL+1,XD1,ICC1)
     ICC2=ICC1
    ELSE
     TINY=MIN(1.0D0,0.01D0*(IDFM%SX(ICM)-IDFM%SX(ICM-1)))
     XD1=IDFM%SX(ICM-1)+TINY
     XD2=IDFM%SX(ICM  )-TINY
     CALL POL1LOCATE(IDFC%SX,IDFC%NCOL+1,XD1,ICC1)
     CALL POL1LOCATE(IDFC%SX,IDFC%NCOL+1,XD2,ICC2)
    ENDIF
    ICC2=MIN(ICC2,IDFC%NCOL)
    ICC1=MAX(ICC1,        1)
    
    IF(ICC2.GE.ICC1.AND.ICC2.LE.IDFC%NCOL.AND.ICC1.NE.0.AND.ICC2.NE.0)THEN
    
     DYM=IDFM%SY(IRM-1) -IDFM%SY(IRM)
     DYC=IDFC%SY(IRC1-1)-IDFC%SY(IRC2)

     DXM=IDFM%SX(ICM) -IDFM%SX(ICM-1)
     DXC=IDFC%SX(ICC2)-IDFC%SX(ICC1-1)

     CALL IDFGETBLOCKVALUE(IDFC,SCLTYPE_UP,IRC1,IRC2,ICC1,ICC2,FREQ,PERC,SVALUE)

     !## downscaling
     IF(DXC*DYC.GT.DXM*DYM)THEN
      IDOWN=1
      IF(SCLTYPE_UP.EQ.5)THEN
       !## downscaling
       IF(SVALUE.NE.IDFC%NODATA)THEN
        SVALUE=SVALUE*((DXM*DYM)/(DXC*DYC))
       ENDIF
      ENDIF
     ENDIF

     IDFM%X(ICM,IRM)=SVALUE

    ENDIF
   ENDDO
  ENDIF
 END DO

 !## smooth only if cs.gt.dx
 IF(IDOWN.EQ.1.AND.SCLTYPE_DOWN.EQ.1)CALL IDFSMOOTH(IDFC,IDFM,IINT)
 IF(ALLOCATED(FREQ))DEALLOCATE(FREQ)

 IDFREADSCALE_GETX=.TRUE.

 END FUNCTION IDFREADSCALE_GETX

 !###====================================================================
 LOGICAL FUNCTION IDFFILLSXSY(IDF)
 !###====================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER :: I

 IDFFILLSXSY=.TRUE.

 !## already filled in
 IF(IDF%IEQ.EQ.1)RETURN
 
 IDFFILLSXSY=.FALSE.
 IF(.NOT.IDFALLOCATESXY(IDF))RETURN
 
 IDF%SX(0)=0.0D0
 DO I=1,IDF%NCOL; IDF%SX(I)=DBLE(I)*IDF%DX; END DO
 IDF%SX=IDF%SX+IDF%XMIN
 
 IDF%SY(0)=0.0D0
 DO I=1,IDF%NROW; IDF%SY(I)=-DBLE(I)*IDF%DY; END DO
 IDF%SY=IDF%SY+IDF%YMAX

 IDFFILLSXSY=.TRUE.

 END FUNCTION IDFFILLSXSY

 !###====================================================================
 SUBROUTINE IDFGETBLOCKVALUE(IDF,SCLTYPE,IR1,IR2,IC1,IC2,FREQ,SFCT,SVALUE)
 !###====================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 REAL(KIND=DP_KIND),INTENT(OUT),DIMENSION(:,:) :: FREQ
 REAL(KIND=DP_KIND),INTENT(IN) :: SFCT
 INTEGER,INTENT(IN) :: IR1,IR2,IC1,IC2,SCLTYPE
 REAL(KIND=DP_KIND),INTENT(OUT) :: SVALUE
 INTEGER :: IROW,ICOL,NAJ,I,N 
 REAL(KIND=DP_KIND) :: IDFVAL,NVALUE,NFRAC,F
 REAL(KIND=DP_KIND),DIMENSION(1) :: XTEMP

 SVALUE=0.0D0 !## scale value
 NVALUE=0.0D0
 SELECT CASE (SCLTYPE)
  CASE (7,9,15)
   FREQ=IDF%NODATA
 END SELECT

 IF(ASSOCIATED(IDF%X))THEN
  SELECT CASE (SCLTYPE)
   !## special for boundary purposes
   CASE (1)
    DO IROW=IR1,IR2
     DO ICOL=IC1,IC2
      IDFVAL=IDF%X(ICOL,IROW) 
      !## set value to inactive for nodata values
      IF(IDFVAL.EQ.IDF%NODATA)IDFVAL=0.0D0
      IF(IDFVAL.LT.0.0D0)SVALUE=IDFVAL
      IF(SVALUE.EQ.0.0D0.AND.IDFVAL.GT.0.0D0)SVALUE=IDFVAL
      NVALUE=NVALUE+1.0D0
     ENDDO
    ENDDO
   !## arithmetic mean (HEAD/SC); sum
   CASE (2,5)
    DO IROW=IR1,IR2
     DO ICOL=IC1,IC2
      IDFVAL=IDF%X(ICOL,IROW) 
      IF(IDFVAL.NE.IDF%NODATA)THEN
       IF(IDFVAL.EQ.IDFVAL)THEN
        SVALUE=SVALUE+IDFVAL 
        NVALUE=NVALUE+1.0D0
       ENDIF
      ENDIF
     ENDDO
    ENDDO
   CASE (4,16) !## sum; rch/evt
    DO IROW=IR1,IR2
     DO ICOL=IC1,IC2
      IDFVAL=IDF%X(ICOL,IROW) 
      IF(IDFVAL.EQ.IDFVAL)THEN
       IF(IDFVAL.NE.IDF%NODATA)SVALUE=SVALUE+IDFVAL 
       NVALUE=NVALUE+1.0D0
      ENDIF
     ENDDO
    ENDDO
   !## geometric mean (KD)
   CASE (3)
    DO IROW=IR1,IR2
     DO ICOL=IC1,IC2
      IDFVAL=IDF%X(ICOL,IROW) 
      !## idfvalue greater than zero for log-function
      IF(IDFVAL.EQ.IDFVAL)THEN
       IF(IDFVAL.NE.IDF%NODATA.AND.IDFVAL.GT.0.0D0)THEN
        SVALUE=SVALUE+LOG(IDFVAL) 
        NVALUE=NVALUE+1.0D0
       ENDIF
      ENDIF
     ENDDO
    ENDDO
   !## sum, sum inverse
   CASE (6,8)
    DO IROW=IR1,IR2
     DO ICOL=IC1,IC2
      IDFVAL=IDF%X(ICOL,IROW) 
      IF(IDFVAL.EQ.IDFVAL)THEN
       IF(IDFVAL.NE.IDF%NODATA)THEN
        IF(IDFVAL.NE.0.0D0)SVALUE=SVALUE+(1.0D0/IDFVAL)
        NVALUE=NVALUE+1.0D0
       ENDIF
      ENDIF
     ENDDO
    ENDDO
   !## most frequent occurence,percentile
   CASE (7,9,15)
    DO IROW=IR1,IR2
     DO ICOL=IC1,IC2
      IDFVAL=IDF%X(ICOL,IROW) 
      IF(IDFVAL.EQ.IDFVAL)THEN
       IF(IDFVAL.NE.IDF%NODATA)THEN
        NVALUE=NVALUE+1.0D0
        FREQ(INT(NVALUE),1)=IDFVAL
       ENDIF
      ENDIF
     ENDDO
    ENDDO
   CASE (10)
    DO IROW=IR1,IR2
     DO ICOL=IC1,IC2
      IDFVAL=IDF%X(ICOL,IROW) 
      IF(IDFVAL.EQ.IDFVAL)THEN
       SVALUE=IDFVAL
       NVALUE=NVALUE+1.0D0
      ENDIF
     ENDDO
    ENDDO
   CASE DEFAULT
    WRITE(*,'(//A//)') 'Scaling not known for: '//TRIM(IDF%FNAME)
  END SELECT
 
 ELSE

  DO IROW=IR1,IR2
   DO ICOL=IC1,IC2

    IDFVAL=IDFGETVAL(IDF,IROW,ICOL)
   
    SELECT CASE (SCLTYPE)
     !## special for boundary purposes
     CASE (1)
      !## set value to inactive for nodata values
      IF(IDFVAL.EQ.IDF%NODATA)IDFVAL=0.0D0
      IF(IDFVAL.LT.0.0D0)SVALUE=IDFVAL
      IF(SVALUE.EQ.0.0D0.AND.IDFVAL.GT.0.0D0)SVALUE=IDFVAL
      NVALUE=NVALUE+1.0D0
     !## arithmetic mean (HEAD/SC); sum
     CASE (2,5)
      IF(IDFVAL.EQ.IDFVAL)THEN
       IF(IDFVAL.NE.IDF%NODATA)THEN
        SVALUE=SVALUE+IDFVAL 
        NVALUE=NVALUE+1.0D0
       ENDIF
      ENDIF
     !## sum; arithmetic mean (rch/evt)
     CASE (4,16)
      IF(IDFVAL.EQ.IDFVAL)THEN
       IF(IDFVAL.NE.IDF%NODATA)SVALUE=SVALUE+IDFVAL
       NVALUE=NVALUE+1.0D0
      ENDIF
     !## geometric mean (KD)
     CASE (3)
      !## idfvalue greater than zero for log-function
      IF(IDFVAL.EQ.IDFVAL)THEN
        IF(IDFVAL.NE.IDF%NODATA.AND.IDFVAL.GT.0.0D0)THEN
        SVALUE=SVALUE+LOG(IDFVAL) 
        !## count number of values ne nodata - including zero
        NVALUE=NVALUE+1.0D0
       ENDIF
      ENDIF
     !## sum, sum inverse
     CASE (6,8)
      IF(IDFVAL.EQ.IDFVAL)THEN
       IF(IDFVAL.NE.IDF%NODATA)THEN
        IF(IDFVAL.NE.0.0D0)SVALUE=SVALUE+(1.0D0/IDFVAL)
        NVALUE=NVALUE+1.0D0
       ENDIF
      ENDIF
     !## most frequent occurence, percentile
     CASE (7,9,15)
      IF(IDFVAL.EQ.IDFVAL)THEN
       IF(IDFVAL.NE.IDF%NODATA)THEN
        NVALUE=NVALUE+1.0D0
        FREQ(INT(NVALUE),1)=IDFVAL
       ENDIF
      ENDIF
     CASE (10)
      IF(IDFVAL.EQ.IDFVAL)THEN
       SVALUE=IDFVAL
       NVALUE=NVALUE+1.0D0
      ENDIF
     CASE DEFAULT
      WRITE(*,'(//A//)') 'Scaling not known for: '//TRIM(IDF%FNAME)
    END SELECT
   ENDDO
  ENDDO
 
 ENDIF
 
 IF(NVALUE.LE.0.0D0)THEN
  SVALUE=IDF%NODATA
  RETURN
 ENDIF

 SELECT CASE (SCLTYPE)
  CASE (1,10)!## boundary, sum

  CASE (2,16)  !## arithmetic mean
   SVALUE=SVALUE/NVALUE
  CASE (3)  !## geometric
   IF(NVALUE.NE.0.0D0)THEN
    SVALUE=EXP(SVALUE/NVALUE)
   ELSE
    SVALUE=0.0D0
   ENDIF
  CASE (6)  !## c-waarde reciprook opgeteld, terug naar gem. dagen
   IF(SVALUE.NE.0.0D0)THEN
    SVALUE=1.0D0/(SVALUE/NVALUE)
   ELSE
    SVALUE=0.0D0
   ENDIF
  CASE (7)
   N=INT(NVALUE)
   CALL QKSORT_SGL(N,FREQ(:,1)) 
   SVALUE=UTL_GETMOSTFREQ(FREQ(:,1),SIZE(FREQ,1),N) 
  CASE (8)  !## PWT c-waarde reciprook opgeteld, terug naar gem. dagen * fraction
   NFRAC=NVALUE/REAL(((IR2-IR1)+1)*((IC2-IC1)+1))
   IF(SVALUE.NE.0.0D0)THEN
    SVALUE=1.0D0/((SVALUE*NFRAC)/NVALUE)
   ELSE
    SVALUE=0.0D0
   ENDIF
  CASE (9)  !## percentile
   CALL UTL_GETMED(FREQ,INT(NVALUE),IDF%NODATA,(/SFCT*100.0D0/),1,NAJ,XTEMP)
   SVALUE=XTEMP(1)
  CASE (15)  !## zonation
   N=INT(NVALUE)
   IF(MAXVAL(FREQ(1:N,1)).GT.0.0D0)THEN
    !## get fractions
    DO I=1,N; F=MOD(FREQ(I,1),1.0D0); IF(F.EQ.0.0D0)F=1.0D0; FREQ(I,2)=F; ENDDO
    !## remove fractions
    DO I=1,N; FREQ(I,1)=INT(FREQ(I,1)); ENDDO
    !## sort zones
    CALL QKSORT_SGL(N,FREQ(:,1)) 
    !## get most available zone, exclude zone eq 0
    SVALUE=UTL_GETMOSTFREQ(FREQ(:,1),SIZE(FREQ,1),N) 
    IF(SVALUE.GT.0)THEN
     !## set fraction to zero for zones not equal to most available zone
     !## only whenever fraction are existing priorly
     IF(INT(SUM(FREQ(1:N,2))).NE.N)THEN
      DO I=1,N; IF(INT(SVALUE).NE.INT(FREQ(I,1)))FREQ(I,2)=0.0D0; ENDDO
      !## get mean fraction
      F=0; DO I=1,N; F=F+FREQ(I,2); ENDDO; F=F/REAL(N)
      !## add fraction to most available zone
      IF(F.LT.1.0D0)SVALUE=SVALUE+F
     ENDIF
    ENDIF     
   ENDIF

 END SELECT

 END SUBROUTINE IDFGETBLOCKVALUE

 !###====================================================================
 SUBROUTINE IDFSMOOTH(IDFC,IDFM,IINT)
 !###====================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDFC,IDFM
 INTEGER,INTENT(IN) :: IINT
 INTEGER :: IC,IR,IC1,IC2,IR1,IR2,NPC,NPR,ICOL,IROW,I,J,II,JJ,IY
 REAL(KIND=DP_KIND) :: XMID,YMID,Y,IDFVAL
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: X1A,X2A
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: Y2A,XCOPY
 REAL(KIND=DP_KIND) :: TINY
 
 IF(ALLOCATED(X1A))  DEALLOCATE(X1A)
 IF(ALLOCATED(X2A))  DEALLOCATE(X2A)
 IF(ALLOCATED(Y2A))  DEALLOCATE(Y2A)
 IF(ALLOCATED(XCOPY))DEALLOCATE(XCOPY)

 !## find number of x- y locations of grid to be interpolated
 TINY=MIN(1.0D0,0.01D0*(IDFM%SX(IDFM%NCOL)-IDFM%SX(0)))
 CALL POL1LOCATE(IDFC%SX,IDFC%NCOL+1,REAL(IDFM%SX(0)+TINY,8),IC1) 
 CALL POL1LOCATE(IDFC%SX,IDFC%NCOL+1,REAL(IDFM%SX(IDFM%NCOL)-TINY,8),IC2)
 TINY=MIN(1.0D0,0.01D0*(IDFM%SY(0)-IDFM%SY(IDFM%NROW)))
 CALL POL1LOCATE(IDFC%SY,IDFC%NROW+1,REAL(IDFM%SY(0)-TINY,8),IR1)
 CALL POL1LOCATE(IDFC%SY,IDFC%NROW+1,REAL(IDFM%SY(IDFM%NROW)+TINY,8),IR2)

 IR1=MAX(1,IR1)
 IC1=MAX(1,IC1)
 IR2=MIN(IR2,IDFC%NROW)
 IC2=MIN(IC2,IDFC%NCOL)
 
 !## number of distinguished coordinates from child idf (coarser)
 NPC=(IC2-IC1)+1; NPR=(IR2-IR1)+1
 !## add extra for boundary (north/west/east/south)
 NPR=NPR+2; NPC=NPC+2 
 
 !## assign one extra row/column for boundary
 ALLOCATE(X1A(NPC),X2A(NPR),Y2A(NPC,NPR),XCOPY(IDFM%NCOL,IDFM%NROW))

 NPR=1
 DO IR=IR1,IR2  !## loop over row in coarser child
  NPC=1
  NPR=NPR+1
  YMID=(IDFC%SY(IR-1)+IDFC%SY(IR))/2.0
  X2A(NPR)=YMID
  DO IC=IC1,IC2 !## loop over col in coarser child idf
   NPC=NPC+1
   XMID=(IDFC%SX(IC-1)+IDFC%SX(IC))/2.0
   X1A(NPC)=XMID
   !## read value from idfm%x()
   CALL IDFIROWICOL(IDFM,IROW,ICOL,X1A(NPC),X2A(NPR))

   IF(ICOL.EQ.0.OR.IROW.EQ.0)THEN
    IF(ASSOCIATED(IDFC%X))THEN
     IDFVAL=IDFC%X(IC,IR)
    ELSE
     IDFVAL=IDFGETVAL(IDFC,IR,IC)
    ENDIF
    IF(IDFVAL.EQ.IDFC%NODATA) IDFVAL=IDFM%NODATA
   ELSE 
    IDFVAL=IDFM%X(ICOL,IROW)  
   ENDIF
   Y2A(NPC,NPR)=IDFVAL

  ENDDO
 ENDDO
  
 NPC=NPC+1; NPR=NPR+1
 
 X1A(1)  =IDFC%SX(IC1-1)
 X1A(NPC)=IDFC%SX(IC2)
 X2A(1)  =IDFC%SY(IR1-1)
 X2A(NPR)=IDFC%SY(IR2)
 
 Y2A(1,:)  =Y2A(2,:)
 Y2A(NPC,:)=Y2A(NPC-1,:)
 Y2A(:,1)  =Y2A(:,2)
 Y2A(:,NPR)=Y2A(:,NPR-1)

 !## change value of temp.grid if values in neighborhood
 !## in this way all points have a real/false value
 DO I=1,NPC
  DO J=1,NPR
   IF(Y2A(I,J).EQ.IDFM%NODATA)THEN
    IY=0
    Y =0.0D0
    DO II=MAX(1,I-1),MIN(NPC,I+1)
     DO JJ=MAX(1,J-1),MIN(NPR,J+1)
      IF(Y2A(II,JJ).NE.IDFM%NODATA)THEN
       Y =Y+Y2A(II,JJ)
       IY=IY+1
      ENDIF
     END DO
    END DO
    IF(IY.GT.0)Y2A(I,J)=Y/REAL(IY)
   ENDIF
  END DO
 END DO

 XCOPY=IDFM%X
 CALL POL1INTMAIN(IDFM%NCOL,IDFM%NROW,NPC,NPR,X1A,X2A,Y2A,IDFM%SX,IDFM%SY,IDFM%X,IINT,IDFM%NODATA)

 !## reset nodata values
 DO I=1,IDFM%NCOL
  DO J=1,IDFM%NROW
   IF(XCOPY(I,J).EQ.IDFM%NODATA)IDFM%X(I,J)=IDFM%NODATA
  ENDDO
 ENDDO
  
 IF(ALLOCATED(X1A))  DEALLOCATE(X1A)
 IF(ALLOCATED(X2A))  DEALLOCATE(X2A)
 IF(ALLOCATED(Y2A))  DEALLOCATE(Y2A)
 IF(ALLOCATED(XCOPY))DEALLOCATE(XCOPY)

 END SUBROUTINE IDFSMOOTH

 !###======================================================================
 LOGICAL FUNCTION IDFPART(IDF,XMIN,YMIN,XMAX,YMAX)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 REAL(KIND=DP_KIND),INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
 INTEGER :: IR,IR1,IR2,IC,IC1,IC2,IROW,ICOL
 
 IDFPART=.FALSE.
 
 !## get mid for determining cell position
 CALL IDFIROWICOL(IDF,IR1,IC1,XMIN+(IDF%DX/2.0),YMAX-(IDF%DX/2.0))
 CALL IDFIROWICOL(IDF,IR2,IC2,XMAX-(IDF%DX/2.0),YMIN+(IDF%DX/2.0))

 IF(IR1.EQ.0.OR.IR2.EQ.0.OR.IC1.EQ.0.OR.IC2.EQ.0)RETURN

 !## new dimensions of idf
 IDF%NROW=IR2-IR1+1
 IDF%NCOL=IC2-IC1+1
 IDF%XMIN=XMIN
 IDF%YMIN=YMIN
 IDF%XMAX=XMAX
 IDF%YMAX=YMAX
 
 IROW=0
 DO IR=IR1,IR2
  IROW=IROW+1
  ICOL=0
  DO IC=IC1,IC2
   ICOL=ICOL+1
   IDF%X(ICOL,IROW)=IDF%X(IC,IR)
  END DO
 END DO

 IDFPART=.TRUE.
 
 END FUNCTION IDFPART
 
 !###======================================================================
 LOGICAL FUNCTION IDFWRITEPART(IDF1,IDF2)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF1,IDF2  !## write idf1 in idf2
 INTEGER :: IR,IR1,IR2,IC,IC1,IC2,IROW,ICOL,NROW,NCOL
 REAL(KIND=DP_KIND) :: X,Y

 IDFWRITEPART=.FALSE.

 !## get position to write idf to
 IF(IDF1%IEQ.EQ.0)THEN
  X=IDF1%XMIN+IDF1%DX/2.0
  Y=IDF1%YMAX-IDF1%DY/2.0
 ELSE
  X=(IDF1%SX(0)+IDF1%SX(1))/2.0
  Y=(IDF1%SY(0)+IDF1%SY(1))/2.0
 ENDIF
 CALL IDFIROWICOL(IDF2,IR1,IC1,X,Y)!IDF1%XMIN,IDF1%YMAX)
 IF(IDF1%IEQ.EQ.0)THEN
  X=IDF1%XMAX-IDF1%DX/2.0
  Y=IDF1%YMIN+IDF1%DY/2.0
 ELSE
  X=(IDF1%SX(IDF1%NCOL)+IDF1%SX(IDF1%NCOL-1))/2.0
  Y=(IDF1%SY(IDF1%NROW)+IDF1%SY(IDF1%NROW-1))/2.0
 ENDIF
 CALL IDFIROWICOL(IDF2,IR2,IC2,X,Y)!IDF1%XMAX,IDF1%YMIN)

 IF(IR1.NE.0.AND.IR2.NE.0.AND.IC1.NE.0.AND.IC2.NE.0)THEN

  NROW=IR2-IR1+1
  NCOL=IC2-IC1+1

  !## wrong dimensions
  IF(NROW.NE.IDF1%NROW.OR.NCOL.NE.IDF1%NCOL)RETURN

  IROW=0
  DO IR=IR1,IR2
   IROW=IROW+1
   ICOL=0
   DO IC=IC1,IC2
    ICOL=ICOL+1
    CALL IDFPUTVAL(IDF2,IR,IC,IDF1%X(ICOL,IROW))
    IF(IDF1%X(ICOL,IROW).NE.IDF2%NODATA)THEN
     IDF2%DMIN=MIN(IDF1%X(ICOL,IROW),IDF2%DMIN)
     IDF2%DMAX=MAX(IDF1%X(ICOL,IROW),IDF2%DMAX)
    ENDIF
   END DO
  END DO

  WRITE(IDF2%IU,REC=7) IDF2%DMIN
  WRITE(IDF2%IU,REC=8) IDF2%DMAX

  IDFWRITEPART=.TRUE.

 ENDIF

 END FUNCTION IDFWRITEPART

!###======================================================================
 LOGICAL FUNCTION IDFWRITE_WRAPPER(NCOL,NROW,X,DX,DY,XMIN,YMIN,NODATA,COMMENT,FNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NCOL,NROW
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(NCOL,NROW) :: X
 REAL(KIND=DP_KIND),DIMENSION(:),INTENT(IN) :: DX,DY
 REAL(KIND=DP_KIND),INTENT(IN) :: XMIN,YMIN,NODATA
 CHARACTER(LEN=*),INTENT(IN) :: FNAME,COMMENT
 TYPE(IDFOBJ) :: IDF
 INTEGER :: I
 LOGICAL :: LEX

 IDFWRITE_WRAPPER=.FALSE.

 CALL IDFNULLIFY(IDF)
 IDF%XMIN  =XMIN
 IDF%YMIN  =YMIN
 IDF%NCOL  =NCOL
 IDF%NROW  =NROW
 IDF%IXV   =0
 IDF%ITB   =0
 IDF%NODATA=NODATA

 IF(SIZE(DX).EQ.IDF%NCOL.AND.SIZE(DY).EQ.IDF%NROW)THEN
  IF(.NOT.IDFALLOCATESXY(IDF))RETURN
  IDF%IEQ =1
  IDF%XMAX=IDF%XMIN+SUM(DX)
  IDF%SX(0)=IDF%XMIN
  DO I=1,IDF%NCOL; IDF%SX(I)=IDF%SX(I-1)+DX(I); ENDDO
  IDF%YMAX =IDF%YMIN+SUM(DY)
  IDF%SY(0)=IDF%YMAX
  DO I=1,IDF%NROW; IDF%SY(I)=IDF%SY(I-1)-DY(I); ENDDO
 ELSEIF(SIZE(DX).EQ.1.AND.SIZE(DY).EQ.1)THEN
  IDF%IEQ =0
  IDF%DX  =DX(1)
  IDF%DY  =DY(1)
  IDF%XMAX=IDF%XMIN+REAL(IDF%NCOL)*DX(1)
  IDF%YMAX=IDF%YMIN+REAL(IDF%NROW)*DY(1)
 ELSE
  WRITE(*,*) 'ERROR, check array dx(.) and dy(.) to be consistent with ncol and nrow'
  RETURN
 ENDIF

 IF(.NOT.IDFALLOCATEX(IDF))RETURN
 IDF%X=X

 CALL IDFFILLCOMMENT(IDF,COMMENT)

 IDFWRITE_WRAPPER=IDFWRITE(IDF,FNAME,0)

 CALL IDFDEALLOCATEX(IDF)
 IF(IDF%IU.GT.0)THEN
  INQUIRE(UNIT=IDF%IU,OPENED=LEX); IF(LEX)CLOSE(IDF%IU)
 ENDIF

 END FUNCTION IDFWRITE_WRAPPER
 
 !###======================================================================
 LOGICAL FUNCTION IDFWRITE(IDF,IDFNAME,IQ,WDATA)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IQ  !## question to overwrite yes=0;no=1
 INTEGER,INTENT(IN),OPTIONAL :: WDATA
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 INTEGER :: IDATA
 CHARACTER(LEN=2) :: TXT

 IDFWRITE=.FALSE.
 
 IDATA=1; IF(PRESENT(WDATA))IDATA=WDATA
 
 TXT  ='WO'  !## write only

 !## open idf
 IF(IDFOPEN(IDF%IU,IDFNAME,TXT,IDF%ITYPE,ABS(IDATA),IQUESTION=IQ))THEN
  IF(IDFWRITEDIM(ABS(IDATA),IDF))THEN
   IF(IDATA.EQ.1)THEN
    IF(IDFWRITEDATA(IDATA,IDF))THEN
     IDFWRITE=.TRUE.
     !## try to write comment, if available
     CALL IDFWRITECOMMENT(IDF,IDATA)
    ENDIF
   ELSE
    IDFWRITE=.TRUE.
   ENDIF
  ENDIF
 ENDIF

 IF(IDATA.EQ.1)THEN
  IF(IDF%IU.NE.0)CLOSE(IDF%IU); IDF%IU=0
 ENDIF
 
 END FUNCTION IDFWRITE

 !###====================================================================
 LOGICAL FUNCTION IDFWRITE_EQUI(IDF,IDFNAME)
 !###====================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 INTEGER :: SNCOL,SNROW,IROW,ICOL,IR,IC,NR,NC
 REAL(KIND=DP_KIND) :: CS,DX,DY

 IDFWRITE_EQUI=.FALSE.

 !## open idf
 IF(.NOT.IDFOPEN(IDF%IU,IDFNAME,'WO',IDF%ITYPE,1,IQUESTION=0))RETURN

 CS=IDF%XMAX-IDF%XMIN
 DO ICOL=1,IDF%NCOL
  CS=MIN(CS,IDF%SX(ICOL)-IDF%SX(ICOL-1))
 END DO

 SNCOL=(IDF%XMAX-IDF%XMIN)/CS
 SNROW=(IDF%YMAX-IDF%YMIN)/CS

 IDF%DMIN= 10.0D10
 IDF%DMAX=-10.0D10
 DO IROW=1,IDF%NROW
  DO ICOL=1,IDF%NCOL
   IF(IDF%X(ICOL,IROW).NE.IDF%NODATA)THEN
    IDF%DMIN=MIN(IDF%DMIN,IDF%X(ICOL,IROW))
    IDF%DMAX=MAX(IDF%DMAX,IDF%X(ICOL,IROW))
   ENDIF
  END DO
 END DO

 WRITE(IDF%IU) UTL_PUTRECORDLENGTH(4) !1271       !header: 1271
 WRITE(IDF%IU) SNCOL      !1
 WRITE(IDF%IU) SNROW      !2
 WRITE(IDF%IU) IDF%XMIN   !3
 WRITE(IDF%IU) IDF%XMAX   !4
 WRITE(IDF%IU) IDF%YMIN   !5
 WRITE(IDF%IU) IDF%YMAX   !6
 WRITE(IDF%IU) IDF%DMIN   !7
 WRITE(IDF%IU) IDF%DMAX   !8
 WRITE(IDF%IU) IDF%NODATA !9
 WRITE(IDF%IU) INT(0,1),INT(0,1),INT(0,1),INT(0,1)         
 WRITE(IDF%IU) CS         !11
 WRITE(IDF%IU) CS         !12

 DO IROW=1,IDF%NROW
  DY=IDF%SY(IROW-1)-IDF%SY(IROW)
  NR=INT(DY/CS)
  DO IR=1,NR
   DO ICOL=1,IDF%NCOL
    DX=IDF%SX(ICOL)-IDF%SX(ICOL-1)
    NC=INT(DX/CS)
    WRITE(IDF%IU) (IDF%X(ICOL,IROW),IC=1,NC)
   END DO
  END DO
 END DO

 CLOSE(IDF%IU)
 IDF%IU=0

 IDFWRITE_EQUI=.TRUE.

 END FUNCTION IDFWRITE_EQUI

 !###======================================================================
 LOGICAL FUNCTION IDFREADDATA(IDATA,IDF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDATA
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER :: IROW,ICOL,I,IOS
 REAL(KIND=SP_KIND),ALLOCATABLE,DIMENSION(:,:) :: X
 REAL(KIND=SP_KIND),ALLOCATABLE,DIMENSION(:) :: V
! CHARACTER(LEN=20) :: STRING
 INTEGER :: BINUNIT
 
 IDFREADDATA=.TRUE.

 IF(IDF%IU.LE.0)RETURN

 IF(IDATA.EQ.1)THEN
 
  IDFREADDATA=.FALSE.

  IF(.NOT.IDFALLOCATEX(IDF))RETURN
  IF(IDF%IXV.EQ.0)THEN

   IF(IDF%ITYPE.EQ.4)THEN
    ALLOCATE(X(IDF%NCOL,IDF%NROW))
    READ(IDF%IU,IOSTAT=IOS) ((X(ICOL,IROW),ICOL=1,IDF%NCOL),IROW=1,IDF%NROW)
    !## convert to temp-file and read it in again
    OPEN(NEWUNIT=BINUNIT,STATUS='SCRATCH',FORM='FORMATTED',ACTION='READWRITE')
    !## dump data
    WRITE(BINUNIT,*) X
    !## rewind
    REWIND(BINUNIT)
    !## read dumo in double precision
    READ(BINUNIT,*) IDF%X
    !## close and delete
    CLOSE(BINUNIT)
!    IDF%X=REAL(X,8)
    DEALLOCATE(X)
   ELSE
    READ(IDF%IU,IOSTAT=IOS) ((IDF%X(ICOL,IROW),ICOL=1,IDF%NCOL),IROW=1,IDF%NROW)
   ENDIF
   IF(IOS.NE.0)RETURN  

  ELSEIF(IDF%IXV.EQ.1)THEN

   IF(IDF%ITYPE.EQ.4)THEN
    ALLOCATE(V(IDF%NCOL*IDF%NROW))
    READ(IDF%IU,IOSTAT=IOS) (V(I),I=1,IDF%NCOL*IDF%NROW)
    !## convert to temp-file and read it in again
    OPEN(NEWUNIT=BINUNIT,STATUS='SCRATCH',FORM='FORMATTED',ACTION='READWRITE')
    !## dump data
    WRITE(BINUNIT,*) V
    !## rewind
    REWIND(BINUNIT)
    !## read dumo in double precision
    READ(BINUNIT,*) IDF%V
    !## close and delete
    CLOSE(BINUNIT)
!    IDF%V=V
    DEALLOCATE(V)
   ELSE
    READ(IDF%IU,IOSTAT=IOS) (IDF%V(I),I=1,IDF%NROW*IDF%NCOL)
   ENDIF
   IF(IOS.NE.0)RETURN
  ENDIF
! CLOSE(IDF%IU)
 ENDIF
 
 IDFREADDATA=.TRUE.

 END FUNCTION IDFREADDATA

 !###======================================================================
 SUBROUTINE IDFWRITE_SIMPLE(FNAME,NCOL,NROW,X,XMIN,YMIN,CS,NODATA)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=256),INTENT(IN) :: FNAME
 INTEGER,INTENT(IN) :: NROW,NCOL
 REAL(KIND=DP_KIND),INTENT(IN) :: XMIN,YMIN,CS,NODATA
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(NCOL,NROW) :: X
 INTEGER :: IU,IROW,ICOL
 REAL(KIND=DP_KIND) :: DMIN,DMAX
      
 IU=UTL_GETUNIT()
 OPEN(IU,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED',ACCESS='STREAM',ACTION='WRITE')

 DMIN= 10.0D10; DMAX=-10.0D10
 DO IROW=1,NROW; DO ICOL=1,NCOL
  IF(X(ICOL,IROW).NE.NODATA)THEN
   DMIN=MIN(DMIN,X(ICOL,IROW))
   DMAX=MAX(DMAX,X(ICOL,IROW))
  ENDIF
 END DO; END DO

 WRITE(IU) UTL_PUTRECORDLENGTH(4) !1271      
 WRITE(IU) NCOL      
 WRITE(IU) NROW      
 WRITE(IU) XMIN   
 WRITE(IU) XMIN+(CS*NCOL)
 WRITE(IU) YMIN
 WRITE(IU) YMIN+(CS*NROW)   
 WRITE(IU) DMIN   
 WRITE(IU) DMAX
 WRITE(IU) NODATA 
 WRITE(IU) INT(0,1),INT(0,1),INT(0,1),INT(0,1)         
 WRITE(IU) CS
 WRITE(IU) CS
 !## write data block
 WRITE(IU) X

 CLOSE(IU)
      
 END SUBROUTINE IDFWRITE_SIMPLE

 !###======================================================================
 SUBROUTINE IDFGETCOMMENT(IDF,IDATA)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: NP=1
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER,INTENT(IN) :: IDATA
 INTEGER :: IOS,N,I,IADIT
 INTEGER,DIMENSION(NP) :: IP
 INTEGER(KIND=DP_KIND) :: IREC
 
 !## read entire data-block
 IF(IDATA.EQ.1)THEN 
  IF(IDF%ITYPE.EQ.4)THEN  
   READ(IDF%IU,IOSTAT=IOS) IADIT
  ELSEIF(IDF%ITYPE.EQ.8)THEN  
   READ(IDF%IU,IOSTAT=IOS) IADIT,I
  ENDIF
 ELSE
  IREC= 11 + ABS(IDF%IEQ-1) * 2 + IDF%IEQ * (IDF%NROW+IDF%NCOL) + IDF%ITB*2 + (IDF%NROW*IDF%NCOL) + 1
  READ(IDF%IU,REC=IREC,IOSTAT=IOS) IADIT
 ENDIF

 IF(IOS.NE.0)RETURN
 
 !## read binair number (e.g. 256) and returns array (/1,0,0,1,0,0,1/)
 CALL UTL_FILLARRAY(IP,NP,IADIT)
 
 !## read comments
 IF(IP(1).EQ.1)THEN
  IF(IDATA.EQ.1)THEN
   IF(IDF%ITYPE.EQ.4)THEN
    READ(IDF%IU,IOSTAT=IOS) N
   ELSEIF(IDF%ITYPE.EQ.8)THEN
    READ(IDF%IU,IOSTAT=IOS) N,I
   ENDIF
  ELSE
   IREC=IREC+1
   READ(IDF%IU,REC=IREC,IOSTAT=IOS) N
  ENDIF

  !## error in reading
  IF(IOS.NE.0)RETURN
  ALLOCATE(IDF%COMMENT(N))
  IF(IDATA.EQ.1)THEN
   READ(IDF%IU,IOSTAT=IOS) (IDF%COMMENT(I),I=1,N)
   IF(IOS.NE.0)RETURN
  ELSE
   IF(IDF%ITYPE.EQ.4)THEN
    DO I=1,N
     IREC=IREC+1
     READ(IDF%IU,REC=IREC,IOSTAT=IOS) IDF%COMMENT(I)
     IF(IOS.NE.0)RETURN
    ENDDO
   ELSEIF(IDF%ITYPE.EQ.8)THEN
    I=1
    DO
     IREC=IREC+1
     IF(I.EQ.N)THEN
      READ(IDF%IU,REC=IREC,IOSTAT=IOS) IDF%COMMENT(I)
     ELSE
      READ(IDF%IU,REC=IREC,IOSTAT=IOS) IDF%COMMENT(I),IDF%COMMENT(I+1)
     ENDIF
     IF(IOS.NE.0)RETURN
     I=I+2; IF(I.GT.N)EXIT
    ENDDO
   ENDIF
  ENDIF
 ENDIF
 
 END SUBROUTINE IDFGETCOMMENT
 
 !###======================================================================
 SUBROUTINE IDFFILLCOMMENT(IDF,STRING)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 CHARACTER(LEN=*),INTENT(IN) :: STRING
 CHARACTER(LEN=50) :: DATESTRING
 CHARACTER(LEN=256) :: DEFAULT
 
 !## fill default values in the beginning
 CALL OSD_DATE_AND_TIME(DATEANDTIME=DATESTRING) 
 DEFAULT='User: '//TRIM(OSD_GETENV('USERNAME'))//NEWLINE// &
         TRIM(UTL_IMODVERSION())//NEWLINE// &
         'Creation Date: '//TRIM(DATESTRING)//NEWLINE
 
 CALL IDFFILLCOMMENT2(IDF,TRIM(DEFAULT)//TRIM(STRING))
 
 END SUBROUTINE IDFFILLCOMMENT
 
 !###======================================================================
 SUBROUTINE IDFFILLCOMMENT2(IDF,STRING)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 CHARACTER(LEN=*),INTENT(IN) :: STRING
 INTEGER :: I,N,L
 
 L=LEN(STRING)
 N=0
 IF(MOD(L,4).NE.0)N=1
 N=N+L/4
 
 IF(ASSOCIATED(IDF%COMMENT))DEALLOCATE(IDF%COMMENT)
 ALLOCATE(IDF%COMMENT(N))
  
 DO I=1,N
  IDF%COMMENT(I)='    '
  IDF%COMMENT(I)=STRING(((I-1)*4)+1:MIN(L,I*4))
 ENDDO
 
 END SUBROUTINE IDFFILLCOMMENT2

 !###======================================================================
 SUBROUTINE IDFWRITECOMMENT(IDF,IDATA)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER,INTENT(IN) :: IDATA
 INTEGER :: I
 INTEGER(KIND=DP_KIND) :: IREC
 
 IF(.NOT.ASSOCIATED(IDF%COMMENT))RETURN

 !## direct mode
 IF(IDATA.EQ.0)THEN
  IREC= 11 + ABS(IDF%IEQ-1) * 2 + IDF%IEQ * (IDF%NROW+IDF%NCOL) + IDF%ITB*2 + (IDF%NROW*IDF%NCOL)
  !## write additional information
  IREC=IREC+1
  !## iadit
  IF(IDF%ITYPE.EQ.4)THEN
   WRITE(IDF%IU,REC=IREC) 1 
  ELSEIF(IDF%ITYPE.EQ.8)THEN
   WRITE(IDF%IU,REC=IREC) 1,0
  ENDIF
  IREC=IREC+1
  IF(IDF%ITYPE.EQ.4)THEN
   WRITE(IDF%IU,REC=IREC) SIZE(IDF%COMMENT)
  ELSEIF(IDF%ITYPE.EQ.8)THEN
   WRITE(IDF%IU,REC=IREC) SIZE(IDF%COMMENT),0
  ENDIF
  IF(IDF%ITYPE.EQ.4)THEN
   DO I=1,SIZE(IDF%COMMENT)
    IREC=IREC+1
    WRITE(IDF%IU,REC=IREC) IDF%COMMENT(I)
   ENDDO
  ELSEIF(IDF%ITYPE.EQ.8)THEN
   I=1
   DO
    IREC=IREC+1
    IF(I.EQ.SIZE(IDF%COMMENT))THEN
     WRITE(IDF%IU,REC=IREC) IDF%COMMENT(I)
    ELSE
     WRITE(IDF%IU,REC=IREC) IDF%COMMENT(I),IDF%COMMENT(I+1)
    ENDIF
    I=I+2; IF(I.GT.SIZE(IDF%COMMENT))EXIT
   ENDDO
  ENDIF 
 !## stream-mode
 ELSE
  IF(IDF%ITYPE.EQ.4)THEN
   WRITE(IDF%IU) 1
   WRITE(IDF%IU) SIZE(IDF%COMMENT)
  ELSEIF(IDF%ITYPE.EQ.8)THEN
   WRITE(IDF%IU) 1,0
   WRITE(IDF%IU) SIZE(IDF%COMMENT),0
  ENDIF
  DO I=1,SIZE(IDF%COMMENT); WRITE(IDF%IU) IDF%COMMENT(I); ENDDO
 ENDIF

 END SUBROUTINE IDFWRITECOMMENT

 !###======================================================================
 LOGICAL FUNCTION IDFWRITEDATA(IDATA,IDF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN)  :: IDATA
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER :: IROW,ICOL,I
 REAL(KIND=SP_KIND),ALLOCATABLE,DIMENSION(:,:) :: X
 REAL(KIND=SP_KIND),ALLOCATABLE,DIMENSION(:) :: V

 IDFWRITEDATA=.TRUE.

 IF(IDF%IU.LE.0)RETURN
 IF(IDATA.EQ.0)RETURN

 IDFWRITEDATA=.FALSE.

 IF(IDF%IXV.EQ.0)THEN

  IF(IDF%ITYPE.EQ.4)THEN
   ALLOCATE(X(IDF%NCOL,IDF%NROW))
   X=REAL(IDF%X,4)
   WRITE(IDF%IU) ((X(ICOL,IROW),ICOL=1,IDF%NCOL),IROW=1,IDF%NROW)
   DEALLOCATE(X)
  ELSE  
   WRITE(IDF%IU) ((IDF%X(ICOL,IROW),ICOL=1,IDF%NCOL),IROW=1,IDF%NROW)
  ENDIF

 ELSEIF(IDF%IXV.EQ.1)THEN

  IF(IDF%ITYPE.EQ.4)THEN
   ALLOCATE(V(IDF%NCOL*IDF%NROW))
   V=REAL(IDF%V,4)
   WRITE(IDF%IU) (V(I),I=1,IDF%NCOL*IDF%NROW)
   DEALLOCATE(V)
  ELSE  
   WRITE(IDF%IU) (IDF%V(I),I=1,IDF%NCOL*IDF%NROW)
  ENDIF
  
 ENDIF

 IDFWRITEDATA=.TRUE.

 END FUNCTION IDFWRITEDATA

 !###======================================================================
 SUBROUTINE IDFGETLOC(IDF,IROW,ICOL,X,Y)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: IROW,ICOL
 REAL(KIND=DP_KIND),INTENT(OUT) :: X,Y

 IF(IDF%IEQ.EQ.0)THEN

  X=IDF%XMIN+((ICOL-1)*IDF%DX)+0.5*IDF%DX
  Y=IDF%YMAX-((IROW-1)*IDF%DY)-0.5*IDF%DY

 ELSEIF(IDF%IEQ.EQ.1)THEN

  X=(IDF%SX(ICOL-1)+IDF%SX(ICOL))/2.0
  Y=(IDF%SY(IROW-1)+IDF%SY(IROW))/2.0

 ENDIF

 END SUBROUTINE IDFGETLOC

 !###======================================================================
 SUBROUTINE IDFGETEDGE(IDF,IROW,ICOL,X1,Y1,X2,Y2)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: IROW,ICOL
 REAL(KIND=DP_KIND),INTENT(OUT) :: X1,Y1,X2,Y2

 IF(IDF%IEQ.EQ.0)THEN

  X1=IDF%XMIN+((ICOL-1)*IDF%DX); X2=X1+IDF%DX
  Y1=IDF%YMAX-((IROW  )*IDF%DY); Y2=Y1+IDF%DY

 ELSEIF(IDF%IEQ.EQ.1)THEN

  X1=IDF%SX(ICOL-1)
  X2=IDF%SX(ICOL  )
  Y1=IDF%SY(IROW  )
  Y2=IDF%SY(IROW-1)

 ENDIF

 END SUBROUTINE IDFGETEDGE
 
 !###======================================================================
 SUBROUTINE IDFIROWICOL(IDF,IROW,ICOL,X,Y)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Y
 INTEGER,INTENT(OUT) :: ICOL,IROW
 REAL(KIND=DP_KIND) :: DX,DY
 INTEGER :: I
 REAL(KIND=DP_KIND),PARAMETER :: TINY=0.01D0
 REAL(KIND=DP_KIND) :: XD, YD

 XD=X; YD=Y

 ICOL=0
 IROW=0

 IF(IDF%IEQ.EQ.0)THEN

  DX=X-IDF%XMIN
  I=0; IF(MOD(DX,IDF%DX).NE.0.0D0)I=1
  IF(XD+TINY.GT.REAL(IDF%XMIN,8).AND.XD-TINY.LT.REAL(IDF%XMAX,8))ICOL=INT((X-IDF%XMIN)/IDF%DX)+I

  DY=IDF%YMAX-Y
  I=0; IF(MOD(DY,IDF%DY).NE.0.0D0)I=1
  IF(YD+TINY.GT.REAL(IDF%YMIN,8).AND.YD-TINY.LT.REAL(IDF%YMAX,8))IROW=INT((IDF%YMAX-Y)/IDF%DY)+I
  
  ICOL=MIN(ICOL,IDF%NCOL)
  IROW=MIN(IROW,IDF%NROW)

 ELSEIF(IDF%IEQ.EQ.1)THEN

  CALL POL1LOCATE(IDF%SX,IDF%NCOL+1,XD,ICOL)
  CALL POL1LOCATE(IDF%SY,IDF%NROW+1,YD,IROW)
  IF(ICOL.LT.0.OR.ICOL.GT.IDF%NCOL) ICOL=0
  IF(IROW.LT.0.OR.IROW.GT.IDF%NROW) IROW=0

 ENDIF

 END SUBROUTINE IDFIROWICOL
 
 !###======================================================================
 REAL(KIND=DP_KIND) FUNCTION IDFGETXYVAL(IDF,X,Y)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 REAL(KIND=DP_KIND),INTENT(IN) :: X,Y
 INTEGER :: IROW,ICOL
 
 IDFGETXYVAL=IDF%NODATA
 
 CALL IDFIROWICOL(IDF,IROW,ICOL,X,Y)
 IF(IROW.GT.0.AND.IROW.LE.IDF%NROW.AND. &
    ICOL.GT.0.AND.ICOL.LE.IDF%NCOL)THEN
  IDFGETXYVAL=IDFGETVAL(IDF,IROW,ICOL)
 ENDIF
  
 END FUNCTION IDFGETXYVAL

 !#####=================================================================
 REAL(KIND=DP_KIND) FUNCTION IDFGETAGGREGATEDVAL(IDF,XC,YC,CS,CS1,VALS,AS,T)
 !#####=================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: AS,T 
 REAL(KIND=DP_KIND),INTENT(IN) :: XC,YC,CS,CS1 
 REAL(KIND=DP_KIND),INTENT(INOUT),DIMENSION(AS) :: VALS
 INTEGER :: I,INTR,J,C,NAJ
 REAL(KIND=DP_KIND) :: CSRATIO,XC2,YC2 
 REAL(KIND=DP_KIND),DIMENSION(1) :: VAL

 CSRATIO = CS/CS1

 IF (CSRATIO.LE.1) THEN
  VAL(1)=IDFGETXYVAL(IDF,XC,YC) 
 ELSE
  !## Retrieve proper xy of underlaying idf
  INTR = NINT(CSRATIO)
  XC2 = XC-CS1
  C=0
  DO I=1,INTR
   XC2 = XC2+I*1/CSRATIO*CS1
   YC2 = YC-CS1
   DO J=1,INTR
    YC2 = YC2+J*1/CSRATIO*CS1
    VAL(1) = IDFGETXYVAL(IDF,XC2,YC2) 
    IF(VAL(1).NE.IDF%NODATA)THEN
     C=C+1; VALS(C)=VAL(1)
    ENDIF
   ENDDO
  ENDDO

  SELECT CASE(T)
   CASE(1) !## nominal data (i.e. landuse or soildata)
    !## majority values
    VAL=UTL_GETMOSTFREQ(VALS,C,C)
   CASE(2) !## ordinal ==> get median value (i.e. elevation, heads ...)
    CALL UTL_GETMED(VALS,C,IDF%NODATA,(/50.0D0/),1,NAJ,VAL)
  END SELECT

 ENDIF

 IDFGETAGGREGATEDVAL = VAL(1)

 END FUNCTION IDFGETAGGREGATEDVAL

 !###======================================================================
 REAL(KIND=DP_KIND) FUNCTION IDFGETVAL(IDF,IROW,ICOL,UNITS)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: IROW,ICOL
 INTEGER,INTENT(IN),OPTIONAL :: UNITS
 INTEGER(KIND=DP_KIND) :: IREC
 REAL(KIND=DP_KIND) :: XVAL
 
 IREC=11  +ABS(IDF%IEQ-1) *2            &
          +IDF%IEQ*(IDF%NROW+IDF%NCOL)  &
          +IDF%ITB*2                    &
          +((IROW-1)*IDF%NCOL)+ICOL

 IDFGETVAL=IDF%NODATA
 IF(.NOT.IDFREADREAL(0,IDF%IU,IDF%ITYPE,IREC,XVAL))RETURN
 IDFGETVAL=XVAL
 
 IF(PRESENT(UNITS))IDFGETVAL=IDFTRANSFORM_F(IDF,IDFGETVAL,UNITS,ICOL,IROW)

 END FUNCTION IDFGETVAL
 
 !###======================================================================
 LOGICAL FUNCTION IDFGETVAL_CHECK(IDF,IROW,ICOL,IDFVAL)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 REAL(KIND=DP_KIND),INTENT(OUT) :: IDFVAL
 INTEGER,INTENT(IN) :: IROW,ICOL
 INTEGER(KIND=DP_KIND) :: IREC
 
 IREC=11  +ABS(IDF%IEQ-1) *2    +IDF%IEQ*(IDF%NROW+IDF%NCOL) +IDF%ITB*2
 IREC=IREC+  ((IROW-1)*IDF%NCOL)+ICOL

 IDFGETVAL_CHECK=IDFREADREAL(0,IDF%IU,IDF%ITYPE,IREC,IDFVAL)

 END FUNCTION IDFGETVAL_CHECK

 !###======================================================================
 REAL(KIND=DP_KIND) FUNCTION IDFTRANSFORM_F(IDF,X,UNITS,ICOL,IROW)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: ICOL,IROW,UNITS
 REAL(KIND=DP_KIND),INTENT(INOUT) :: X

 IDFTRANSFORM_F=X

 IF(UNITS.EQ.0)RETURN  !## no transformation
 IF(X.EQ.IDF%NODATA)RETURN

 SELECT CASE (UNITS)
  CASE (1) !## m --- cm
   X=X*100.0D0
  CASE (2) !## cm --- m
   X=X/100.0D0
  CASE (3) !## m --- mm
   X=X*1000.0D0
  CASE (4) !## mm --- m
   X=X/1000.0D0
  CASE (5) !## m3/day --- mm/day
   X=(X*1000.0D0)/IDFGETAREA(IDF,ICOL,IROW)
  CASE (6) !## mm/day --- m3/day
   X=(X/1000.0D0)*IDFGETAREA(IDF,ICOL,IROW)
 END SELECT

 IDFTRANSFORM_F=X

 END FUNCTION IDFTRANSFORM_F

 !###======================================================================
 REAL(KIND=DP_KIND) FUNCTION IDFTRANSFORM_B(IDF,X,UNITS,ICOL,IROW) !## reciprook
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: ICOL,IROW,UNITS
 REAL(KIND=DP_KIND),INTENT(INOUT) :: X

 IDFTRANSFORM_B=X

 IF(UNITS.EQ.0)RETURN
 IF(X.EQ.IDF%NODATA)RETURN

 SELECT CASE (UNITS)
  CASE (1) !## m --- cm
   X=X/100.0D0
  CASE (2) !## cm --- m
   X=X*100.0D0
  CASE (3) !## m --- mm
   X=X/1000.0D0
  CASE (4) !## mm --- m
   X=X*1000.0D0
  CASE (5) !## m3/day --- mm/day
   X=(X/1000.0D0)*IDFGETAREA(IDF,ICOL,IROW)
  CASE (6) !## mm/day --- m3/day
   X=(X*1000.0D0)/IDFGETAREA(IDF,ICOL,IROW)
 END SELECT

 IDFTRANSFORM_B=X

 END FUNCTION IDFTRANSFORM_B

 !###======================================================================
 REAL(KIND=DP_KIND) FUNCTION IDFGETAREA(IDF,ICOL,IROW)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: ICOL,IROW

 IF(IDF%IEQ.EQ.0)THEN
  IDFGETAREA=IDF%DX*IDF%DY
 ELSE
  IDFGETAREA=(IDF%SY(IROW-1)-IDF%SY(IROW))* &
             (IDF%SX(ICOL)  -IDF%SX(ICOL-1))
 ENDIF

 END FUNCTION IDFGETAREA

 !###======================================================================
 SUBROUTINE IDFGETDXDY(IDF,ICOL,IROW,DX,DY)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: ICOL,IROW
 REAL(KIND=DP_KIND),INTENT(OUT) :: DX,DY
 
 IF(IDF%IEQ.EQ.0)THEN
  DX=IDF%DX
  DY=IDF%DY
 ELSE
  DX=IDF%SY(IROW-1)-IDF%SY(IROW)
  DY=IDF%SX(ICOL)  -IDF%SX(ICOL-1)
 ENDIF

 END SUBROUTINE IDFGETDXDY

 !###======================================================================
 SUBROUTINE IDFPUTVAL(IDF,IROW,ICOL,IDFVALUE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 REAL(KIND=DP_KIND),INTENT(IN) :: IDFVALUE
 INTEGER,INTENT(IN) :: IROW,ICOL
 INTEGER(KIND=DP_KIND) :: IREC
 
 IREC=11  +ABS(IDF%IEQ-1) *2    +IDF%IEQ*(IDF%NROW+IDF%NCOL) +IDF%ITB*2
 IREC=IREC+  ((IROW-1)*IDF%NCOL)+ICOL

 IF(IDF%ITYPE.EQ.4)WRITE(IDF%IU,REC=IREC) REAL(IDFVALUE,4)
 IF(IDF%ITYPE.EQ.8)WRITE(IDF%IU,REC=IREC)      IDFVALUE

 END SUBROUTINE IDFPUTVAL

 !###======================================================================
 LOGICAL FUNCTION IDFREADDIM(IDATA,IDF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN)  :: IDATA
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER :: I,IOS
 INTEGER(KIND=1) :: I1,I2,I3,I4
 INTEGER(KIND=DP_KIND) :: IREC
 
 IDFREADDIM=.FALSE.

 IF(IDF%IU.LE.0)RETURN

 IF(IDATA.EQ.1)THEN
  IF(IDF%ITYPE.EQ.4)READ(IDF%IU,IOSTAT=IOS) I     !header: 1271
  IF(IDF%ITYPE.EQ.8)READ(IDF%IU,IOSTAT=IOS) I,I   !header: 1271
 ENDIF
 
 IF(.NOT.IDFREADINTEGER(IDATA,IDF%IU,IDF%ITYPE,2,IDF%NCOL))  RETURN
 IF(.NOT.IDFREADINTEGER(IDATA,IDF%IU,IDF%ITYPE,3,IDF%NROW))  RETURN

 IF(.NOT.IDFREADREAL   (IDATA,IDF%IU,IDF%ITYPE,4,IDF%XMIN))  RETURN
 IF(.NOT.IDFREADREAL   (IDATA,IDF%IU,IDF%ITYPE,5,IDF%XMAX))  RETURN
 IF(.NOT.IDFREADREAL   (IDATA,IDF%IU,IDF%ITYPE,6,IDF%YMIN))  RETURN
 IF(.NOT.IDFREADREAL   (IDATA,IDF%IU,IDF%ITYPE,7,IDF%YMAX))  RETURN
  
 IF(.NOT.IDFREADREAL   (IDATA,IDF%IU,IDF%ITYPE,8,IDF%DMIN))  RETURN
 IF(.NOT.IDFREADREAL   (IDATA,IDF%IU,IDF%ITYPE,9,IDF%DMAX))  RETURN
 IF(.NOT.IDFREADREAL   (IDATA,IDF%IU,IDF%ITYPE,10,IDF%NODATA))RETURN

 IF(IDATA.EQ.0)THEN
  IF(IDF%ITYPE.EQ.4)READ(IDF%IU,REC=11,IOSTAT=IOS) I1,I2,I3,I4     
  IF(IDF%ITYPE.EQ.8)READ(IDF%IU,REC=11,IOSTAT=IOS) I1,I2,I3,I4,I   
 ELSEIF(IDATA.EQ.1)THEN
  IF(IDF%ITYPE.EQ.4)READ(IDF%IU,       IOSTAT=IOS) I1,I2,I3,I4     
  IF(IDF%ITYPE.EQ.8)READ(IDF%IU,       IOSTAT=IOS) I1,I2,I3,I4,I   
 ENDIF

 IDF%IEQ=MIN(1,MAX(0,INT(I1)))
 IDF%ITB=MIN(1,MAX(0,INT(I2)))

 IF(IDF%IEQ.EQ.0)THEN
  
  IF(.NOT.IDFREADREAL   (IDATA,IDF%IU,IDF%ITYPE,12,IDF%DX))  RETURN
  IF(.NOT.IDFREADREAL   (IDATA,IDF%IU,IDF%ITYPE,13,IDF%DY))  RETURN
  I=14

 ELSE
  
  I=12
  
 ENDIF

 IF(IDF%ITB.EQ.1)THEN

  IF(.NOT.IDFREADREAL   (IDATA,IDF%IU,IDF%ITYPE,INT(I,8)  ,IDF%TOP))  RETURN
  IF(.NOT.IDFREADREAL   (IDATA,IDF%IU,IDF%ITYPE,INT(I+1,8),IDF%BOT))  RETURN

 ENDIF

 !## non-equidistantial grid
 IF(IDF%IEQ.EQ.1)THEN

  CALL IDFDEALLOCATEX(IDF)

  IF(.NOT.IDFALLOCATESXY(IDF))RETURN

  IREC=11+IDF%ITB*2
  DO I=1,IDF%NCOL
   IREC=IREC+1
   IF(.NOT.IDFREADREAL   (IDATA,IDF%IU,IDF%ITYPE,IREC,IDF%SX(I)))  RETURN
  END DO
  DO I=1,IDF%NROW
   IREC=IREC+1
   IF(.NOT.IDFREADREAL   (IDATA,IDF%IU,IDF%ITYPE,IREC,IDF%SY(I)))  RETURN
  END DO

  !## minimal cell-sizes
  IDF%DX=MINVAL(IDF%SX(1:IDF%NCOL))
  IDF%DY=MINVAL(IDF%SY(1:IDF%NROW))

  IDF%SX(0)=IDF%XMIN
  DO I=1,IDF%NCOL
   IDF%SX(I)=IDF%SX(I-1)+IDF%SX(I)
  END DO
  IDF%SY(0)=IDF%YMAX
  DO I=1,IDF%NROW
   IDF%SY(I)=IDF%SY(I-1)-IDF%SY(I)
  END DO                       

 ENDIF

 IDFREADDIM=.TRUE.

 END FUNCTION IDFREADDIM

 !###======================================================================
 LOGICAL FUNCTION IDFWRITEDIM(IDATA,IDF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDATA
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER :: I,J,IOS
 INTEGER(KIND=DP_KIND) :: IREC
 
 IDFWRITEDIM=.FALSE.

 IF(IDF%IU.LE.0)RETURN

 IF(IDF%IXV.EQ.0.AND.ASSOCIATED(IDF%X))THEN
  IDF%DMIN= 10.0D10; IDF%DMAX=-10.0D10
  DO I=1,SIZE(IDF%X,2); DO J=1,SIZE(IDF%X,1)
   IF(IDF%X(J,I).NE.IDF%NODATA)THEN
    IDF%DMIN=MIN(IDF%DMIN,IDF%X(J,I))
    IDF%DMAX=MAX(IDF%DMAX,IDF%X(J,I))
   ENDIF
  END DO; END DO
 ELSEIF(IDF%IXV.EQ.1.AND.ASSOCIATED(IDF%V))THEN
  IDF%DMIN= 10.0D10; IDF%DMAX=-10.0D10
  DO I=1,SIZE(IDF%X,1)*SIZE(IDF%X,2) 
   IF(IDF%V(I).NE.IDF%NODATA)THEN
    IDF%DMIN=MIN(IDF%DMIN,IDF%V(I))
    IDF%DMAX=MAX(IDF%DMAX,IDF%V(I))
   ENDIF
  END DO
 ENDIF

 !## make sure
 IDF%ITB=MAX(MIN(IDF%ITB,1),0)
 IDF%IEQ=MAX(MIN(IDF%IEQ,1),0)
 
 I=UTL_PUTRECORDLENGTH(IDF%ITYPE)
 IF(.NOT.IDFWRITEINTEGER(IDATA,IDF%IU,IDF%ITYPE,1 ,I         ))RETURN
 IF(.NOT.IDFWRITEINTEGER(IDATA,IDF%IU,IDF%ITYPE,2 ,IDF%NCOL  ))RETURN
 IF(.NOT.IDFWRITEINTEGER(IDATA,IDF%IU,IDF%ITYPE,3 ,IDF%NROW  ))RETURN
 IF(.NOT.IDFWRITEREAL   (IDATA,IDF%IU,IDF%ITYPE,4 ,IDF%XMIN  ))RETURN
 IF(.NOT.IDFWRITEREAL   (IDATA,IDF%IU,IDF%ITYPE,5 ,IDF%XMAX  ))RETURN
 IF(.NOT.IDFWRITEREAL   (IDATA,IDF%IU,IDF%ITYPE,6 ,IDF%YMIN  ))RETURN
 IF(.NOT.IDFWRITEREAL   (IDATA,IDF%IU,IDF%ITYPE,7 ,IDF%YMAX  ))RETURN
 IF(.NOT.IDFWRITEREAL   (IDATA,IDF%IU,IDF%ITYPE,8 ,IDF%DMIN  ))RETURN
 IF(.NOT.IDFWRITEREAL   (IDATA,IDF%IU,IDF%ITYPE,9 ,IDF%DMAX  ))RETURN
 IF(.NOT.IDFWRITEREAL   (IDATA,IDF%IU,IDF%ITYPE,10,IDF%NODATA))RETURN

 IF(IDATA.EQ.0)THEN
  IF(IDF%ITYPE.EQ.4)WRITE(IDF%IU,REC=11,IOSTAT=IOS) INT(IDF%IEQ,1),INT(IDF%ITB,1),INT(0,1),INT(0,1)
  IF(IDF%ITYPE.EQ.8)WRITE(IDF%IU,REC=11,IOSTAT=IOS) INT(IDF%IEQ,1),INT(IDF%ITB,1),INT(0,1),INT(0,1),I
 ELSEIF(IDATA.EQ.1)THEN
  IF(IDF%ITYPE.EQ.4)WRITE(IDF%IU,       IOSTAT=IOS) INT(IDF%IEQ,1),INT(IDF%ITB,1),INT(0,1),INT(0,1)    
  IF(IDF%ITYPE.EQ.8)WRITE(IDF%IU,       IOSTAT=IOS) INT(IDF%IEQ,1),INT(IDF%ITB,1),INT(0,1),INT(0,1),I
 ENDIF

 !## equidistantial raster
 IF(IDF%IEQ.EQ.0)THEN
  IF(.NOT.IDFWRITEREAL(IDATA,IDF%IU,IDF%ITYPE,12,IDF%DX))RETURN
  IF(.NOT.IDFWRITEREAL(IDATA,IDF%IU,IDF%ITYPE,13,IDF%DY))RETURN
  I=14
 ELSE
  I=12  
 ENDIF
  
 IF(IDF%ITB.EQ.1)THEN
  IF(.NOT.IDFWRITEREAL(IDATA,IDF%IU,IDF%ITYPE,INT(I,8)  ,IDF%TOP))RETURN
  IF(.NOT.IDFWRITEREAL(IDATA,IDF%IU,IDF%ITYPE,INT(I+1,8),IDF%BOT))RETURN
 ENDIF

 !## non-equidistantial grid
 IF(IDF%IEQ.EQ.1)THEN

  IREC=12+IDF%ITB*2
  DO I=1,IDF%NCOL
   IREC=IREC+1
   IF(.NOT.IDFWRITEREAL(IDATA,IDF%IU,IDF%ITYPE,IREC,IDF%SX(I)-IDF%SX(I-1)))RETURN
  END DO
  DO I=1,IDF%NROW
   IREC=IREC+1
   IF(.NOT.IDFWRITEREAL(IDATA,IDF%IU,IDF%ITYPE,IREC,IDF%SY(I-1)-IDF%SY(I)))RETURN
  END DO
 
 ENDIF
 
 IDFWRITEDIM=.TRUE.

 END FUNCTION IDFWRITEDIM

 !###======================================================================
 SUBROUTINE IDFCOPY(IDF1,IDF2)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF1
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF2

 IDF2%NCOL  =IDF1%NCOL
 IDF2%NROW  =IDF1%NROW
 IDF2%XMIN  =IDF1%XMIN
 IDF2%XMAX  =IDF1%XMAX
 IDF2%YMIN  =IDF1%YMIN
 IDF2%YMAX  =IDF1%YMAX
 IDF2%DX    =IDF1%DX
 IDF2%DY    =IDF1%DY
 IDF2%IXV   =IDF1%IXV
 IDF2%JD    =IDF1%JD
 IDF2%ILAY  =IDF1%ILAY
 IDF2%ITB   =IDF1%ITB
 IDF2%TOP   =IDF1%TOP
 IDF2%BOT   =IDF1%BOT
 IDF2%IEQ   =IDF1%IEQ
 IDF2%NODATA=IDF1%NODATA 
 IDF2%DMIN  =IDF1%DMIN   
 IDF2%DMAX  =IDF1%DMAX   
 IDF2%ITYPE =IDF1%ITYPE
 
 CALL IDFDEALLOCATEX(IDF2)

 !## allocate memory x/v/ysel/ithrd
 IF(ASSOCIATED(IDF1%X))THEN
  IF(IDFALLOCATEX(IDF2))IDF2%X=IDF1%X
 ENDIF
 IF(ASSOCIATED(IDF1%SX))THEN
  IF(IDFALLOCATESXY(IDF2))THEN
   IDF2%SX=IDF1%SX
   IDF2%SY=IDF1%SY
  ENDIF
 ENDIF
 IF(IDF2%IXV.EQ.2)IDF2%NTHREAD=0

 END SUBROUTINE IDFCOPY

 !###======================================================================
 LOGICAL FUNCTION IDFEQUAL(IDF1,IDF2,IERROR)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF1,IDF2
 INTEGER,INTENT(IN) :: IERROR
 INTEGER :: I
 
 IDFEQUAL=.TRUE. 
 
 IF(IDF1%NCOL.NE.IDF2%NCOL)IDFEQUAL=.FALSE.
 IF(IDF1%NROW.NE.IDF2%NROW)IDFEQUAL=.FALSE.
 IF(IDF1%XMIN.NE.IDF2%XMIN)IDFEQUAL=.FALSE.
 IF(IDF1%XMAX.NE.IDF2%XMAX)IDFEQUAL=.FALSE.
 IF(IDF1%YMIN.NE.IDF2%YMIN)IDFEQUAL=.FALSE.
 IF(IDF1%YMAX.NE.IDF2%YMAX)IDFEQUAL=.FALSE.
 IF(IDF1%DX  .NE.IDF2%DX)  IDFEQUAL=.FALSE.
 IF(IDF1%DY  .NE.IDF2%DY)  IDFEQUAL=.FALSE.
 IF(IDF1%IEQ .NE.IDF2%IEQ) IDFEQUAL=.FALSE.
 !## test cellsizes
 IF(IDFEQUAL)THEN
  IF(IDF1%IEQ.EQ.1.AND.IDF2%IEQ.EQ.1)THEN
   DO I=1,IDF1%NCOL
    IF(IDF1%SX(I).NE.IDF2%SX(I))IDFEQUAL=.FALSE.
   ENDDO
   DO I=1,IDF1%NROW
    IF(IDF1%SY(I).NE.IDF2%SY(I))IDFEQUAL=.FALSE.
   ENDDO
  ENDIF  
 ENDIF
 
 IF(IDFEQUAL)RETURN
 IF(IERROR.EQ.0)RETURN
 
 CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'IDF1 ['//TRIM(IDF1%FNAME)//']'//CHAR(13)// &
    'IDF2 ['//TRIM(IDF2%FNAME)//']'//CHAR(13)// &
    'are not equal!','Error')
 
 END FUNCTION IDFEQUAL

  !###======================================================================
 LOGICAL FUNCTION IDFCHECKRC(IDF,IROW,ICOL)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: ICOL,IROW
 !!# Functions  checks if given IROW/ICOL is within the given GRID
 IDFCHECKRC=.TRUE. 
 
 IF(IROW.LT.1)        IDFCHECKRC=.FALSE.
 IF(IROW.GT.IDF%NROW) IDFCHECKRC=.FALSE.
 IF(ICOL.LT.1)        IDFCHECKRC=.FALSE.
 IF(ICOL.GT.IDF%NCOL) IDFCHECKRC=.FALSE.
 
 END FUNCTION IDFCHECKRC

 !###======================================================================
 LOGICAL FUNCTION IDFALLOCATESXY(IDF)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER,DIMENSION(2) :: IOS

 IDFALLOCATESXY=.FALSE.

 IF(ASSOCIATED(IDF%SX))DEALLOCATE(IDF%SX)
 IF(ASSOCIATED(IDF%SY))DEALLOCATE(IDF%SY)
 NULLIFY(IDF%SX)
 NULLIFY(IDF%SY)

 ALLOCATE(IDF%SX(0:IDF%NCOL),STAT=IOS(1))
 ALLOCATE(IDF%SY(0:IDF%NROW),STAT=IOS(2))

 IF(SUM(IOS).NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot allocate enough memory to store sx/sy data','Error')
  RETURN
 ENDIF

 IDFALLOCATESXY=.TRUE.

 END FUNCTION IDFALLOCATESXY

 !###======================================================================
 LOGICAL FUNCTION IDFALLOCATEX(IDF)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER :: IOS

 IDFALLOCATEX=.FALSE.
 
 CALL IDFDEALLOCATEX(IDF)
 
 IF(IDF%IXV.EQ.0)THEN
  ALLOCATE(IDF%X(IDF%NCOL,IDF%NROW),STAT=IOS)
 ELSEIF(IDF%IXV.EQ.1)THEN
  ALLOCATE(IDF%V(IDF%NCOL*IDF%NROW),STAT=IOS)
 ELSEIF(IDF%IXV.EQ.2)THEN
  ALLOCATE(IDF%YSEL(2,IDF%NCOL*IDF%NROW),STAT=IOS)
 ELSEIF(IDF%IXV.EQ.3)THEN
  ALLOCATE(IDF%XV(IDF%NCOL,IDF%NROW,6),STAT=IOS)
 ELSE
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot recognize ixv variable within IDF-object','Error')
  RETURN
 ENDIF

 IF(IOS.NE.0)THEN
!  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot allocate enough memory to store entire IDF'//CHAR(13)// &
!       'ncol='//TRIM(ITOS(IDF%NCOL))//';nrow='//TRIM(ITOS(IDF%NROW)),'Error')
  RETURN
 ENDIF

 IDFALLOCATEX=.TRUE.

 END FUNCTION IDFALLOCATEX

 !###======================================================================
 SUBROUTINE IDFDEALLOCATE(IDF,NIDF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NIDF
 TYPE(IDFOBJ),DIMENSION(NIDF),INTENT(INOUT) :: IDF
 INTEGER :: I
 LOGICAL :: LEX

 DO I=1,SIZE(IDF)
  CALL IDFDEALLOCATEX(IDF(I))
  CALL IDFDEALLOCATESX(IDF(I))
  IF(IDF(I)%IU.GT.0)THEN
   INQUIRE(UNIT=IDF(I)%IU,OPENED=LEX)
   IF(LEX)CLOSE(IDF(I)%IU)
   IDF(I)%IU=0
  ENDIF
 END DO

 END SUBROUTINE IDFDEALLOCATE

 !###======================================================================
 SUBROUTINE IDFDEALLOCATEX(IDF)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF

 !## deallocate x
 IF(ASSOCIATED(IDF%X))DEALLOCATE(IDF%X); NULLIFY(IDF%X)
 !## deallocate v
 IF(ASSOCIATED(IDF%V))DEALLOCATE(IDF%V); NULLIFY(IDF%V)
 !## deallocate ysel
 IF(ASSOCIATED(IDF%YSEL))DEALLOCATE(IDF%YSEL); NULLIFY(IDF%YSEL)
 !## deallocate iv
 IF(ASSOCIATED(IDF%XV))DEALLOCATE(IDF%XV); NULLIFY(IDF%XV)
 !## deallocate comment
 IF(ASSOCIATED(IDF%COMMENT))DEALLOCATE(IDF%COMMENT); NULLIFY(IDF%COMMENT)

 END SUBROUTINE IDFDEALLOCATEX

 !###======================================================================
 SUBROUTINE IDFDEALLOCATESX(IDF)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF

 !## deallocate sx
 IF(ASSOCIATED(IDF%SX))DEALLOCATE(IDF%SX)
 NULLIFY(IDF%SX)
 !## deallocate sy
 IF(ASSOCIATED(IDF%SY))DEALLOCATE(IDF%SY)
 NULLIFY(IDF%SY)

 END SUBROUTINE IDFDEALLOCATESX

 !###======================================================================
 SUBROUTINE IDFNULLIFY(IDF)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 
 NULLIFY(IDF%X)
 NULLIFY(IDF%V)
 NULLIFY(IDF%SX)
 NULLIFY(IDF%SY)
 NULLIFY(IDF%YSEL)
 NULLIFY(IDF%XV)
 NULLIFY(IDF%COMMENT)
 IDF%IEQ= 0
 IDF%IXV= 0
 IDF%ITB= 0
 IDF%JD=  0
 IDF%ILAY=0
 IDF%TOP= 0.0D0
 IDF%BOT= 0.0D0
 IDF%NTHREAD=0
 IDF%DAYFRACTION=1.0D0
 IDF%ITYPE=4
 
 END SUBROUTINE IDFNULLIFY
 
 !###======================================================================
 INTEGER FUNCTION IDFGETILAY(IDFNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 INTEGER :: I,J,ILAY,IOS

 IDFGETILAY=0

 !## get layer
 I=INDEX(UTL_CAP(IDFNAME,'U'),'L',.TRUE.)+1
 J=INDEX(UTL_CAP(IDFNAME,'U'),'.IDF',.TRUE.)-1
 !## not proper file-name format
 IF(J.GE.I)THEN
  READ(IDFNAME(I:J),*,IOSTAT=IOS) ILAY
  IF(IOS.EQ.0)IDFGETILAY=ILAY
 ENDIF

 END FUNCTION IDFGETILAY

 !###======================================================================
 LOGICAL FUNCTION IDFOPEN(IU,IDFNAME,TSTAT,IDFTYPE,IDATA,IQUESTION)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IU
 INTEGER,INTENT(IN) :: IDATA
 INTEGER,INTENT(IN),OPTIONAL :: IQUESTION     !## 0=yes ; 1=no
 INTEGER,INTENT(INOUT) :: IDFTYPE    !## 0=real(4); 1=real(8)
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME,TSTAT
 INTEGER :: RECLEN,I,IOS,IQ
 LOGICAL :: LEX,LOPEN
 CHARACTER(LEN=100) :: MSG
 CHARACTER(LEN=25) :: TACTION

 IDFOPEN=.FALSE.

 !## default questioning if file need to be overwritten
 IQ=0; IF(PRESENT(IQUESTION))IQ=IQUESTION
 
 IF(TSTAT(1:1).NE.'W'.AND.TSTAT(1:1).NE.'R')THEN
  IF(IQ.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Opening Status [ '//TRIM(TSTAT)//' ] not recognized','Error'); RETURN
 ENDIF

 IU=0

 IF(LEN_TRIM(IDFNAME).EQ.0)THEN
  IF(IQ.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No IDF filename given!','Error'); RETURN
 ENDIF

 INQUIRE(FILE=IDFNAME,OPENED=LOPEN)
 IF(LOPEN)THEN

  !## check opening action to be similar
  INQUIRE(FILE=IDFNAME,ACTION=TACTION)
  LEX=.FALSE.
  SELECT CASE (TRIM(TACTION))
   CASE ('READ')
    IF(TSTAT(1:1).EQ.'R')LEX=.TRUE.
   CASE ('WRITE')
    IF(TSTAT(1:1).EQ.'W')LEX=.TRUE.
   CASE ('READWRITE')
    IF(LEN_TRIM(TSTAT).EQ.2)LEX=.TRUE.
  END SELECT

  IF(LEX)THEN
   INQUIRE(FILE=IDFNAME,NUMBER=IU)
   IF(IDATA.EQ.0)THEN
    IDFOPEN=.TRUE.
    READ(IU,REC=1) RECLEN
    !## record length
    IDFTYPE=(RECLEN-247)/256  !## in bytes
    RETURN
   ELSEIF(IDATA.EQ.1)THEN
    !## close and open again
    CLOSE(IU)
   ENDIF
  ELSE
   !## close and open again
   CLOSE(IU)
  ENDIF
 
 ENDIF

 IF(TSTAT(1:1).EQ.'R')THEN  !## read
  INQUIRE(FILE=IDFNAME,EXIST=LEX)
  IF(.NOT.LEX)THEN
   IF(IQ.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot find'//CHAR(13)//'['//TRIM(IDFNAME)//']','Error')
   RETURN
  ENDIF
 ELSEIF(TSTAT(1:1).EQ.'W')THEN !## write
  INQUIRE(FILE=IDFNAME,EXIST=LEX)
  IF(IQ.EQ.0.AND.LEX)THEN
   CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONOK,TRIM(IDFNAME)//CHAR(13)//'exists, overwrite it?','Question')
   IF(WINFODIALOG(4).NE.1)RETURN
  ENDIF
 ENDIF

 !## get free unit number
 IU=UTL_GETUNIT()

 !## default record-length
 RECLEN=1
 
 !## open file for reading
 IF(TSTAT(1:1).EQ.'R')THEN

  !## get idftype 0/1
  IDFTYPE=UTL_GETRECORDLENGTH(IDFNAME)
  RECLEN=IDFTYPE/4

  IF(IDATA.EQ.1)THEN

   CALL OSD_OPEN(IU,FILE=IDFNAME,STATUS='OLD',FORM='UNFORMATTED',ACCESS='TRANSPARENT', &
                 ACTION='READ,DENYWRITE',IOSTAT=IOS)

  ELSEIF(IDATA.EQ.0)THEN

   !## read only
   IF(LEN_TRIM(TSTAT).EQ.1)THEN

    CALL OSD_OPEN(IU,FILE=IDFNAME,STATUS='OLD',FORM='UNFORMATTED',ACCESS='DIRECT', &
                  RECL=RECLEN,ACTION='READ,DENYWRITE',IOSTAT=IOS)

   ELSE

    !## read only
    IF(TSTAT.EQ.'RO')THEN
     CALL OSD_OPEN(IU,FILE=IDFNAME,STATUS='OLD',FORM='UNFORMATTED',ACCESS='DIRECT', &
                   RECL=RECLEN,ACTION='READ,DENYWRITE',IOSTAT=IOS)
    !## read/write
    ELSEIF(TSTAT.EQ.'RW')THEN
     CALL OSD_OPEN(IU,FILE=IDFNAME,STATUS='OLD',FORM='UNFORMATTED',ACCESS='DIRECT', &
                   RECL=RECLEN,ACTION='READWRITE',IOSTAT=IOS)

    ENDIF
   ENDIF
  
  ENDIF
 
 !## open file for writing
 ELSEIF(TSTAT(1:1).EQ.'W')THEN
 
  !## set recordlength in words (bytes/4)
  RECLEN=IDFTYPE/4
  
  CALL UTL_CREATEDIR(IDFNAME(:INDEX(IDFNAME,'\',.TRUE.)-1))
  IF(IDATA.EQ.1)THEN
  
   CALL OSD_OPEN(IU,FILE=IDFNAME,STATUS='REPLACE',ACTION='WRITE',ACCESS='TRANSPARENT',IOSTAT=IOS,FORM='UNFORMATTED')

  ELSEIF(IDATA.EQ.0)THEN

   CALL OSD_OPEN(IU,FILE=IDFNAME,STATUS='REPLACE',ACTION='READWRITE',ACCESS='DIRECT', &
                 RECL=RECLEN,IOSTAT=IOS,FORM='UNFORMATTED')

  ENDIF
  IF(IOS.NE.0)IU=0

 ENDIF

 IF(IOS.NE.0)THEN
  CALL OSD_IOSTAT_MSG(IOS,MSG)
  I=INDEX(IDFNAME,'/')
  IF(I.NE.0)MSG=TRIM(MSG)//'"/" inside filename!'
  IF(IQ.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error opening '//TRIM(IDFNAME)//CHAR(13)// &
  'Status: '//TRIM(MSG),'Error')
  RETURN
 ENDIF

 IDFOPEN=.TRUE.

 END FUNCTION IDFOPEN

END MODULE MOD_IDF
