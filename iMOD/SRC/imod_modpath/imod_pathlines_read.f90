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
!!
MODULE MOD_PLINES_READ

USE WINTERACTER
USE RESOURCE
USE MOD_UTL, ONLY : UTL_GETUNIT,EQUALS_REAL
USE MOD_POLINT, ONLY : POL1LOCATE
USE MOD_OSD, ONLY : OSD_OPEN,OSD_IOSTAT_MSG,ICF

CONTAINS

 !###====================================================================
 SUBROUTINE TRACEREADBLOCK_I(IB,NROW,NCOL,FNAME,IU,SCLTYPE,DELC,DELR)
 !###====================================================================
 IMPLICIT NONE
 !-----SCLTYPE:
 !  1 = SPECIAAL (IBOUNDARY)
 !  2 = REKENKUNDIG (SHEAD/TOP/BOT)
 !  4 = SUM (Q)
 INTEGER,INTENT(IN) :: NROW,NCOL,SCLTYPE
 INTEGER,INTENT(OUT) :: IU
 CHARACTER(LEN=*),INTENT(IN)  :: FNAME
 INTEGER,DIMENSION(NCOL,NROW) :: IB
 REAL,DIMENSION(0:NCOL) :: DELR
 REAL,DIMENSION(0:NROW) :: DELC
 INTEGER,ALLOCATABLE,DIMENSION(:,:) :: PDELR
 INTEGER,ALLOCATABLE,DIMENSION(:,:) :: PDELC
 REAL :: XMIN,XMAX,YMIN,YMAX,CS,IDFVAL,NODATA
 INTEGER :: IROW,ICOL,DNROW,DNCOL,IR1,IR2,IC1,IC2,IEQ

 IF(.NOT.TRACEREADOPENFILE(IU,DNROW,DNCOL,FNAME,NODATA,XMIN,YMIN,XMAX,YMAX,CS,IEQ))RETURN

 ALLOCATE(PDELR(2,NCOL)); ALLOCATE(PDELC(2,NROW))

 CALL TRACESCALEPDELRC(XMIN,YMIN,XMAX,YMAX,DELC,DELR,PDELR,PDELC,NROW,NCOL,CS,DNROW,DNCOL,IU,IEQ)

 IF(IU.GT.0)THEN
  DO IROW=1,NROW
   IR1=PDELC(1,IROW)
   IR2=PDELC(2,IROW)
   DO ICOL=1,NCOL
    IC1=PDELR(1,ICOL)
    IC2=PDELR(2,ICOL)
    CALL TRACEREADGETBLOCKVALUE(IR1,IR2,IC1,IC2,DNCOL,DNROW,IU,NODATA,IDFVAL,SCLTYPE,IEQ)
    IB(ICOL,IROW)=INT(IDFVAL)
   ENDDO
  ENDDO
 ENDIF

 !## transform nodata into nul!
 DO IROW=1,NROW; DO ICOL=1,NCOL
  IF(IB(ICOL,IROW).EQ.NODATA)IB(ICOL,IROW)=0.0
 ENDDO; ENDDO

 CLOSE(ABS(IU))

 IF(IU.LE.0)IU=0

 DEALLOCATE(PDELR,PDELC)

 END SUBROUTINE

 !###====================================================================
 SUBROUTINE TRACEREADBLOCK_R(X,NROW,NCOL,FNAME,IU,SCLTYPE,DELC,DELR,ISMOOTH,BUFF,NODATAVALUE)
 !###====================================================================
 IMPLICIT NONE
 !--SCLTYPE: 1 = SPECIAAL (IBOUNDARY)
 !  2 = REKENKUNDIG (SHEAD/TOP/BOT)
 !  4 = SUM (Q)
 INTEGER,INTENT(IN) :: NROW,NCOL,SCLTYPE,ISMOOTH
 INTEGER,INTENT(OUT) :: IU
 REAL,INTENT(OUT) :: NODATAVALUE
 CHARACTER(LEN=*),INTENT(IN)  :: FNAME
 REAL,DIMENSION(NCOL,NROW) :: X,BUFF
 REAL,DIMENSION(0:NCOL) :: DELR
 REAL,DIMENSION(0:NROW) :: DELC
 INTEGER,ALLOCATABLE,DIMENSION(:,:) :: PDELR
 INTEGER,ALLOCATABLE,DIMENSION(:,:) :: PDELC
 REAL :: XMIN,YMIN,XMAX,YMAX,CS,IDFVAL,DX
 INTEGER :: IROW,ICOL,DNROW,DNCOL,IR1,IR2,IC1,IC2,IEQ

 IF(.NOT.TRACEREADOPENFILE(IU,DNROW,DNCOL,FNAME,NODATAVALUE,XMIN,YMIN,XMAX,YMAX,CS,IEQ))RETURN

 IF(ALLOCATED(PDELR))DEALLOCATE(PDELR)
 IF(ALLOCATED(PDELC))DEALLOCATE(PDELC)
 ALLOCATE(PDELR(2,NCOL))
 ALLOCATE(PDELC(2,NROW))

 CALL TRACESCALEPDELRC(XMIN,YMIN,XMAX,YMAX,DELC,DELR,PDELR,PDELC,NROW,NCOL,CS,DNROW,DNCOL,IU,IEQ)

 IF(IU.GT.0)THEN
  DO IROW=1,NROW
   IR1=PDELC(1,IROW)
   IR2=PDELC(2,IROW)
   IF(IR2-IR1.LT.0)STOP 'IR2-IR1.LT.0'
   DO ICOL=1,NCOL
    IC1=PDELR(1,ICOL)
    IC2=PDELR(2,ICOL)
    IF(IC2-IC1.LT.0)STOP 'IC2-IC1.LT.0'
    CALL TRACEREADGETBLOCKVALUE(IR1,IR2,IC1,IC2,DNCOL,DNROW,IU,NODATAVALUE,IDFVAL,SCLTYPE,IEQ)
    X(ICOL,IROW)=IDFVAL
   ENDDO
  ENDDO
  !## smooth only if cs.gt.dx
  IF(ISMOOTH.EQ.1)THEN
   DX=(DELR(NCOL)-DELR(0))/REAL(NCOL)
   IF(DX.LT.CS)CALL TRACEREADBLOCKSMOOTH(X,NROW,NCOL,PDELR,PDELC,CS,DX,DELR(0),DELC(0),DNROW,DNCOL,BUFF,NODATAVALUE)
  ENDIF
 ENDIF

 CLOSE(ABS(IU))

 !## error occured
 IF(IU.LT.0)IU=0

 IF(ALLOCATED(PDELR))DEALLOCATE(PDELR)
 IF(ALLOCATED(PDELC))DEALLOCATE(PDELC)

 END SUBROUTINE TRACEREADBLOCK_R

 !###====================================================================
 LOGICAL FUNCTION TRACEREADOPENFILE(IU,NROW,NCOL,FNAME,NODATA,XMIN,YMIN,XMAX,YMAX,CS,IEQ)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: NROW,NCOL,IU,IEQ
 REAL,INTENT(OUT) :: NODATA,XMIN,YMIN,XMAX,YMAX,CS
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: IOS,IRECL
 INTEGER(KIND=1) :: I1,I2,I3,I4
 REAL :: DX,DY
 CHARACTER(LEN=50) :: ERRORMESSAGE
 LOGICAL :: LEX

 TRACEREADOPENFILE=.FALSE.

 INQUIRE(FILE=FNAME,EXIST=LEX)
 IF(.NOT.LEX)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not find IDF-file:'//CHAR(13)//TRIM(FNAME),'Error')
  RETURN
 ENDIF

 IU=UTL_GETUNIT()

 IF(ICF.EQ.0)IRECL=4  !## lahey
 IF(ICF.EQ.1)IRECL=1  !## intel
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=IRECL,ACTION='READ,DENYWRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL OSD_IOSTAT_MSG(IOS,ERRORMESSAGE)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not open IDF-file:'//CHAR(13)//TRIM(FNAME)//CHAR(13)// &
                   TRIM(ERRORMESSAGE),'Error')
  RETURN
 ENDIF
 READ(IU,REC=1+ICF)  NCOL
 READ(IU,REC=2+ICF)  NROW
 READ(IU,REC=3+ICF)  XMIN
 READ(IU,REC=4+ICF)  XMAX
 READ(IU,REC=5+ICF)  YMIN
 READ(IU,REC=6+ICF)  YMAX
 READ(IU,REC=9+ICF)  NODATA
 READ(IU,REC=10+ICF,IOSTAT=IOS) I1,I2,I3,I4
 IF(IOS.NE.0)RETURN
 IEQ=MIN(1,MAX(0,INT(I1)))
! IDF%ITB=MIN(1,MAX(0,INT(I2)))
! IDF%IVF=MIN(1,MAX(0,INT(I3)))
! IDF%IPG=MIN(1,MAX(0,INT(I4)))
! READ(IU,REC=10+ICF) IEQ
 IF(IEQ.EQ.0)THEN
  READ(IU,REC=11+ICF) DY
  READ(IU,REC=12+ICF) DX
  IF(DX.NE.DY)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'dx ne dy','ERROR')
   RETURN
  ENDIF
 ELSE
  DX=0.0
  DY=0.0
 ENDIF
 CS=DX

 TRACEREADOPENFILE=.TRUE.

 END FUNCTION

 !###====================================================================
 SUBROUTINE TRACESCALEPDELRC(XMIN,YMIN,XMAX,YMAX,DELC,DELR,PDELR,PDELC,NROW,NCOL,CS,DNROW,DNCOL,IU,IEQ)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NROW,NCOL,DNROW,DNCOL,IEQ
 INTEGER,INTENT(INOUT) :: IU
 REAL,INTENT(IN) :: CS,XMIN,YMAX,XMAX,YMIN
 REAL,INTENT(IN),DIMENSION(0:NCOL) :: DELR
 REAL,INTENT(IN),DIMENSION(0:NROW) :: DELC
 REAL :: DX,DY
 INTEGER,INTENT(OUT),DIMENSION(2,NCOL) :: PDELR
 INTEGER,INTENT(OUT),DIMENSION(2,NROW) :: PDELC
 REAL,ALLOCATABLE,DIMENSION(:) :: DELRIDF,DELCIDF
 INTEGER :: I,J,IREC
 CHARACTER(LEN=256) :: IDFNAME
 LOGICAL,DIMENSION(4) :: LEX
 
 IF(XMIN.GT.DELR(0).OR.XMAX.LT.DELR(NCOL).OR.YMIN.GT.DELC(NROW).OR.YMAX.LT.DELC(0))THEN
  LEX(1)=EQUALS_REAL(XMIN,DELR(0))
  LEX(2)=EQUALS_REAL(XMAX,DELR(NCOL))
  LEX(3)=EQUALS_REAL(YMIN,DELC(NROW))
  LEX(4)=EQUALS_REAL(YMAX,DELC(0))
  IF(LEX(1).AND.LEX(2).AND.LEX(3).AND.LEX(4))THEN
  ELSE
   INQUIRE(UNIT=IU,NAME=IDFNAME)
   WRITE(*,*) 'idfxmin,mdlxmin=',XMIN,DELR(0),XMIN.GT.DELR(0)
   WRITE(*,*) 'idfxmax,mdlxmax=',XMAX,DELR(NCOL),XMAX.LT.DELR(NCOL)
   WRITE(*,*) 'idfymin,mdlymin=',YMIN,DELC(NROW),YMIN.GT.DELC(NROW)
   WRITE(*,*) 'idfymax,mdlymax=',YMAX,DELC(0),YMAX.LT.DELC(0)
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'File '//TRIM(IDFNAME)//CHAR(13)// &
    'Undersizes current model dimensions','Error')
   IU=-1*IU; RETURN
  ENDIF
 ENDIF

 IF(ALLOCATED(DELRIDF))DEALLOCATE(DELRIDF)
 IF(ALLOCATED(DELCIDF))DEALLOCATE(DELCIDF)
 ALLOCATE(DELRIDF(0:DNCOL),DELCIDF(0:DNROW))

 DELRIDF(0)=XMIN
 DELCIDF(0)=YMAX
 
 IF(IEQ.EQ.0)THEN

  DO I=1,DNCOL; DELRIDF(I)=XMIN+REAL(I)*CS; ENDDO
  DO I=1,DNROW; DELCIDF(I)=YMAX-REAL(I)*CS; ENDDO

 ELSEIF(IEQ.EQ.1)THEN

  DELRIDF(0)=XMIN
  IREC      =10+ICF
  DO I=1,DNCOL
   IREC=IREC+1
   READ(IU,REC=IREC) DELRIDF(I)
   DELRIDF(I)=DELRIDF(I-1)+DELRIDF(I)
  END DO
  DELCIDF(0)=YMAX
  DO I=1,DNROW
   IREC=IREC+1
   READ(IU,REC=IREC) DELCIDF(I)
   DELCIDF(I)=DELCIDF(I-1)-DELCIDF(I)
  END DO

 ENDIF
 
   !#start/end column direction
  DO I=1,NCOL
!   DX=(DELR(I-1)-XMIN)/CS
!   PDELR(1,I)=INT(DX)+1  !van
!   DX=(DELR(I)-XMIN)/CS
!   PDELR(2,I)=INT(DX)+1  !tot/met
!   DX=DELR(I)-XMIN
!   IF(MOD(DX,CS).EQ.0.0)PDELR(2,I)=PDELR(2,I)-1
   CALL POL1LOCATE(DELRIDF,DNCOL+1,REAL(DELR(I-1),8),PDELR(1,I))
   !## check whether position is exact equally
   J=PDELR(1,I)
   IF(J.LE.NCOL)THEN
    IF(DELRIDF(J).EQ.DELR(I-1))PDELR(1,I)=PDELR(1,I)+1
   ENDIF
   CALL POL1LOCATE(DELRIDF,DNCOL+1,REAL(DELR(I),8),PDELR(2,I))
   PDELR(1,I)=MIN(PDELR(1,I),DNCOL)
   PDELR(2,I)=MIN(PDELR(2,I),DNCOL)
  ENDDO
  DO I=1,NROW
!   DY=(YMAX-DELC(I-1))/CS
!   PDELC(1,I)=INT(DY)+1
!   DY=(YMAX-DELC(I))/CS
!   PDELC(2,I)=INT(DY)+1
!   DY=YMAX-DELC(I)
!   IF(MOD(DY,CS).EQ.0.0)PDELC(2,I)=PDELC(2,I)-1
   CALL POL1LOCATE(DELCIDF,DNROW+1,REAL(DELC(I-1),8),PDELC(1,I))
   CALL POL1LOCATE(DELCIDF,DNROW+1,REAL(DELC(I),8),PDELC(2,I))
   !## check whether position is exact equally
   J=PDELC(2,I)
   IF(J.LE.DNROW)THEN
    IF(DELCIDF(J-1).EQ.DELC(I))PDELC(2,I)=PDELC(2,I)-1
   ENDIF
   PDELC(1,I)=MIN(PDELC(1,I),DNROW)
   PDELC(2,I)=MIN(PDELC(2,I),DNROW)
  ENDDO

! !#start/end column direction
! DO I=1,NCOL
!
!  CALL POL1LOCATE(DELRIDF,DNCOL+1,REAL(DELR(I-1),8),PDELR(1,I))
!  !## check whether position is exact equally
!  J=PDELR(1,I)
!  IF(J.LE.DNCOL)THEN
!   IF(DELRIDF(J).EQ.DELR(I-1))PDELR(1,I)=PDELR(1,I)+1
!  ENDIF
!
!  CALL POL1LOCATE(DELRIDF,DNCOL+1,REAL(DELR(I),8),PDELR(2,I))
!
!  PDELR(1,I)=MIN(PDELR(1,I),DNCOL)
!  PDELR(2,I)=MIN(PDELR(2,I),DNCOL)
! ENDDO
!
! DO I=1,NROW
!
!  CALL POL1LOCATE(DELCIDF,DNROW+1,REAL(DELC(I-1),8),PDELC(1,I))
!
!  CALL POL1LOCATE(DELCIDF,DNROW+1,REAL(DELC(I),8),PDELC(2,I))
!  !## check whether position is exact equally
!  J=PDELC(2,I)
!  IF(J.LE.DNROW)THEN
!   IF(DELCIDF(J-1).EQ.DELC(I))PDELC(2,I)=PDELC(2,I)-1
!  ENDIF
!
!  PDELC(1,I)=MIN(PDELC(1,I),DNROW)
!  PDELC(2,I)=MIN(PDELC(2,I),DNROW)
! ENDDO

!  !#start/end column direction
!  DO I=1,NCOL
!   CALL POL1LOCATE(DELRIDF,DNCOL+1,DELR(I-1),PDELR(1,I))
!   PDELR(1,I)=PDELR(1,I)+1
!   CALL POL1LOCATE(DELRIDF,DNCOL+1,DELR(I),PDELR(2,I))
!  ENDDO
!  DO I=1,NROW
!   CALL POL1LOCATE(DELCIDF,DNROW+1,DELC(I-1),PDELC(1,I))
!   CALL POL1LOCATE(DELCIDF,DNROW+1,DELC(I),PDELC(2,I))
!   PDELC(2,I)=PDELC(2,I)-1
!  ENDDO

  IF(ALLOCATED(DELRIDF))DEALLOCATE(DELRIDF)
  IF(ALLOCATED(DELCIDF))DEALLOCATE(DELCIDF)

! ENDIF

 DO I=1,NCOL
  IF(PDELR(2,I).LT.PDELR(1,I))THEN
   WRITE(*,*) 'PDELR(2,I).LT.PDELR(1,I) ',I,PDELR(2,I),PDELR(1,I)
   STOP
  ENDIF
 ENDDO
 DO I=1,NROW
  IF(PDELC(2,I).LT.PDELC(1,I))THEN
   WRITE(*,*) 'PDELC(2,I).LT.PDELC(1,I) ',I,PDELC(2,I),PDELC(1,I)
   STOP
  ENDIF
 ENDDO

 END SUBROUTINE

 !###====================================================================
 SUBROUTINE TRACEREADGETBLOCKVALUE(IR1,IR2,IC1,IC2,DNCOL,DNROW,IU,NODATA, &
                              BVALUE,SCLTYPE,IEQ)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IR1,IR2,IC1,IC2,DNCOL,DNROW,IU,SCLTYPE,IEQ
 REAL,INTENT(IN) :: NODATA
 REAL,INTENT(OUT) :: BVALUE
 INTEGER :: IROW,ICOL,NVALUE,IREC,JREC
 REAL :: IDFVAL

 BVALUE=0.0
 NVALUE=0

 JREC=10+ABS(IEQ-1)*2+IEQ*(DNROW+DNCOL)+ICF !+IDFOFFSET

 SELECT CASE (SCLTYPE)

 !special for boundary purposes
  CASE(1)
   DO IROW=IR1,IR2
    DO ICOL=IC1,IC2
     IREC=JREC+((IROW-1)*DNCOL)+ICOL
     READ(IU,REC=IREC) IDFVAL
     IF(IDFVAL.LT.0)BVALUE=IDFVAL
     IF(BVALUE.EQ.0.AND.IDFVAL.GT.0)BVALUE=IDFVAL
     NVALUE=NVALUE+1
    ENDDO
   ENDDO

 !arithmetic mean (HEAD/TOP/BOT)
  CASE (2)
   DO IROW=IR1,IR2
    DO ICOL=IC1,IC2
     IREC=JREC+((IROW-1)*DNCOL)+ICOL
     READ(IU,REC=IREC) IDFVAL
     IF(IDFVAL.NE.NODATA)THEN
      BVALUE=BVALUE+IDFVAL
      NVALUE=NVALUE+1
     ENDIF
    ENDDO
   ENDDO

 !sum
  CASE (4)
   DO IROW=IR1,IR2
    DO ICOL=IC1,IC2
     IREC=JREC+((IROW-1)*DNCOL)+ICOL
     READ(IU,REC=IREC) IDFVAL
     IF(IDFVAL.NE.NODATA)THEN
      BVALUE=BVALUE+IDFVAL
      NVALUE=NVALUE+1
     ENDIF
    ENDDO
   ENDDO

  CASE DEFAULT
   WRITE(*,*) 'UNKNOWN SCLTYPE'
   STOP
 END SELECT

 IF(NVALUE.LE.0)THEN
  BVALUE=NODATA
  RETURN
 ENDIF

 SELECT CASE (SCLTYPE)
  CASE (2)  !## arithmetic mean
   BVALUE=BVALUE/REAL(NVALUE)
 END SELECT

 RETURN
 END SUBROUTINE

 !###====================================================================
 SUBROUTINE TRACEREADBLOCKSMOOTH(X,NROW,NCOL,PDELR,PDELC,CS,DX, &
         XMIN,YMAX,IDFNROW,IDFNCOL,BUFF,NODATA)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: NSMOOTH=1
 INTEGER,INTENT(IN) :: NROW,NCOL,IDFNROW,IDFNCOL
 REAL,INTENT(IN)  :: CS,DX,XMIN,YMAX,NODATA
 REAL,DIMENSION(NCOL,NROW) :: X,BUFF
 INTEGER,DIMENSION(2,NCOL) :: PDELR
 INTEGER,DIMENSION(2,NROW) :: PDELC
 INTEGER :: IC,IR,I,J,IROW,ICOL,ISTEP,ISMOOTH
 REAL :: XT,XN,D,XC,XX,YY,YC

 BUFF=0.0
 !## copy array into buff
 J =PDELC(1,1)
 IR=1
 DO IROW=1,NROW
  IF(PDELC(1,IROW).EQ.J)THEN
   I =PDELR(1,1)
   IC=1
   DO ICOL=1,NCOL
    IF(PDELR(1,ICOL).EQ.I)THEN
     BUFF(IC,IR)=X(ICOL,IROW)
     I =I+1
     IC=IC+1
    ENDIF
   ENDDO
   BUFF(IC,IR)=BUFF(IC-1,IR)
   J =J+1
   IR=IR+1
  ENDIF
 ENDDO
 BUFF(:,IR)=BUFF(:,IR-1)

 PDELR=0
 !## mid of cell
 XC=XMIN-0.5*DX
 !## shift cell artificial
 XX=XMIN-0.5*CS
 DO ICOL=1,NCOL
  XC=XC+DX
  IC=INT((XC-XX)/CS)+1
  PDELR(1,ICOL)=MAX(1,IC-1)
  PDELR(2,ICOL)=MIN(IC,IDFNCOL)
 END DO

 PDELC=0
 !## mid of cell
 YC=YMAX+0.5*DX
 !## shift cell artificial
 YY=YMAX+0.5*CS
 DO IROW=1,NROW
  YC=YC-DX
  IR=INT((YY-YC)/CS)+1
  PDELC(1,IROW)=MAX(1,IR-1)
  PDELC(2,IROW)=MIN(IR,IDFNROW)
 END DO

 DO IROW=1,NROW
  DO ICOL=1,NCOL
   XN=0.0
   XT=0.0
   DO IR=PDELC(1,IROW),PDELC(2,IROW)
    YY=YMAX-(IR*CS)+(0.5*CS)
    YC=YMAX-(IROW*DX)+(0.5*DX)
    DO IC=PDELR(1,ICOL),PDELR(2,ICOL)

     IF(BUFF(IC,IR).NE.NODATA)THEN
      XX=XMIN+(IC*CS)-(0.5*CS)
      XC=XMIN+(ICOL*DX)-(0.5*DX)
      D =SQRT((XX-XC)**2.0+(YY-YC)**2.0)
      IF(D.NE.0.0)THEN
        D=1.0/D
      ELSE
        D=1.0
      ENDIF
      XT=XT+BUFF(IC,IR)*D
      XN=XN+D
     ENDIF

    END DO
   END DO
   IF(XN.GT.0.0)THEN
    X(ICOL,IROW)=XT/XN
   ELSE
    X(ICOL,IROW)=NODATA
   ENDIF
  ENDDO
 ENDDO

 ISTEP=INT(DX/CS)

 DO ISMOOTH=1,NSMOOTH
  BUFF=NODATA

  I=0
  DO IROW=1,NROW
   I=I+1
   IF(I.GT.ISTEP)I=1
   J=0
   DO ICOL=1,NCOL
    J=J+1
    IF(J.GT.ISTEP)J=1

    IF(I.NE.ISTEP.AND.J.NE.ISTEP)THEN
     XN=0.0
     XT=0.0
     DO IR=MAX(1,IROW-1),MIN(NROW,IROW+1)
      DO IC=MAX(1,ICOL-1),MIN(NCOL,ICOL+1)
        IF(X(IC,IR).NE.NODATA)THEN
         XT=XT+X(IC,IR)
         XN=XN+1.0
        ENDIF
      END DO
     END DO
     IF(XN.GT.0.0)BUFF(ICOL,IROW)=XT/XN
    ENDIF

   ENDDO
  ENDDO
  X=BUFF
 ENDDO

 RETURN
 END SUBROUTINE

END MODULE MOD_PLINES_READ

