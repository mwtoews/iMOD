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
MODULE MOD_SOF

USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFREAD,IDFREADSCALE,IDFCOPY,IDFALLOCATEX,IDFWRITE,IDFDEALLOCATEX,IDFNULLIFY,IDFGETLOC,IDFIROWICOL,IDFGETVAL, &
        IDFDEALLOCATE
USE MOD_IDFEDIT_TRACE, ONLY : IDFEDITTRACE 
USE MOD_OSD, ONLY : OSD_TIMER,OSD_OPEN
USE MOD_UTL, ONLY : UTL_IDFSNAPTOGRID,UTL_GETUNIT,UTL_DIST,ITOS,UTL_DIRINFO_POINTER,UTL_GETMED
USE MOD_SOLID_PAR, ONLY : HCLOSE,MXITER1,MXITER2,IDAMPING,RELAX,ITIGHT,MICNVG
USE MOD_POLINT, ONLY : POL1LOCATE
USE MOD_IPF, ONLY : IPFALLOCATE,IPFREAD2,IPFDEALLOCATE
USE MOD_IPF_PAR, ONLY : IPF,NIPF
USE IMODVAR, ONLY : PI
USE MOD_IDFEDIT_TRACE, ONLY : IDFEDITTRACE

REAL,POINTER,DIMENSION(:,:),PRIVATE :: PL,PL_BU
INTEGER,PRIVATE :: NP

TYPE BPXOBJ
 INTEGER :: ICOL,IROW
 REAL :: Z
END TYPE BPXOBJ
TYPE(BPXOBJ),ALLOCATABLE,DIMENSION(:),PRIVATE :: BPX,PPX,TPX

CONTAINS

 !###======================================================================
 SUBROUTINE SOF_CATCHMENTS(RESULTIDF,OUTPUTFOLDER,IDF,TQP,PTQP,ITQP,TTQP,YTQP)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: RESULTIDF,OUTPUTFOLDER
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:) :: IDF
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:,:) :: TQP
 REAL,DIMENSION(:),INTENT(IN) :: PTQP
 INTEGER,INTENT(IN) :: ITQP,TTQP,YTQP
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: FIDF,RIDF
 INTEGER :: I,J,IROW,ICOL,JROW,JCOL,IPZ,JPZ,NIDF,NX,IMND
 REAL :: FP
 INTEGER,ALLOCATABLE,DIMENSION(:) :: NSORT
 INTEGER(KIND=1),POINTER,DIMENSION(:) :: ISPEC
 INTEGER(KIND=2),POINTER,DIMENSION(:,:) :: THREAD,YSEL
 INTEGER :: MAXTHREAD,NTHREAD,MAXN,DTERM,ISTOP,IMENU,IYR,IMD
 CHARACTER(LEN=256),DIMENSION(:),POINTER :: LISTNAME
 CHARACTER(LEN=256) :: DIR
 CHARACTER(LEN=52) :: WCD
 REAL,DIMENSION(:,:),ALLOCATABLE :: XSORT,YSORT,XMED
 
 DIR=RESULTIDF(:INDEX(RESULTIDF,'\',.TRUE.)-1)
 WCD=RESULTIDF(INDEX(RESULTIDF,'\',.TRUE.)+1:)
 !## get result files
 IF(.NOT.UTL_DIRINFO_POINTER(DIR,WCD,LISTNAME,'F'))RETURN
 !## nothing found
 IF(.NOT.ASSOCIATED(LISTNAME))RETURN
 
 NIDF=SIZE(LISTNAME); IF(NIDF.LE.0)THEN; WRITE(*,'(/A/)') 'No resultfiles found'; RETURN; ENDIF
 ALLOCATE(FIDF(NIDF),RIDF(NIDF));DO I=1,NIDF; CALL IDFNULLIFY(FIDF(I)); CALL IDFNULLIFY(RIDF(I)); ENDDO
 
 !## read in all result files in fidf() and make a copy of it to ridf()
 DO I=1,SIZE(LISTNAME)
  FIDF(I)%FNAME=TRIM(DIR)//'\'//TRIM(LISTNAME(I))
  IF(.NOT.IDFREAD(FIDF(I),FIDF(I)%FNAME,1))RETURN
  CALL IDFCOPY(FIDF(I),RIDF(I))
 ENDDO
 
 IMENU=-2 !## smaller and equal
 ISTOP= 0 !## 
 DTERM= 1 !## 9-points
 IPZ  = 1 !## pointer value
 JPZ  = 0 !## new pointer value
  
 !## read in dewatering pointer file
 IF(.NOT.IDFREAD(IDF(1),IDF(1)%FNAME,1))THEN; ENDIF
 !# read percentiles
 IF(ITQP.EQ.0)THEN
  DO I=1,SIZE(TQP,1); DO J=1,SIZE(TQP,2)
   IF(.NOT.IDFREAD(TQP(I,J),TQP(I,J)%FNAME,1))THEN; ENDIF
  ENDDO; ENDDO
 ELSE
  DO I=1,SIZE(TQP,1); DO J=1,SIZE(TQP,2); CALL IDFCOPY(IDF(1),TQP(I,J)); ENDDO; ENDDO
 ENDIF

 !## extra array of trace-functionality
 CALL IDFCOPY(IDF(1),IDF(2)) 
 J=1+11*TTQP; ALLOCATE(XSORT(NIDF,J),YSORT(NIDF,J),NSORT(J),XMED(SIZE(TQP,1),J))

 MAXTHREAD=1000; ALLOCATE(ISPEC(MAXTHREAD),THREAD(3,MAXTHREAD),YSEL(2,MAXTHREAD)); MAXN=MAXTHREAD
 
 WRITE(6,'(1X,A)') 'Tracing '
 
 IDF(2)%X=IDF(2)%NODATA
 
 !## start tracing
 DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
  !## skip nodata
  IF(IDF(1)%X(ICOL,IROW).EQ.IDF(1)%NODATA.OR.IDF(1)%X(ICOL,IROW).EQ.0)CYCLE
         
  !## set begin values
  NTHREAD=1; YSEL(1,NTHREAD)=ICOL; YSEL(2,NTHREAD)=IROW 
  !## trace all higher than neighbouring cell, imenu=-4, idf(1) will be adjusted
  CALL IDFEDITTRACE(IDF(1),IDF(2),THREAD,YSEL,ISPEC,DTERM,IMENU,MAXTHREAD,MAXN,IDF(1)%X(ICOL,IROW),NTHREAD,IPZ)

  !## add fluxes
  XSORT=0.0; YSORT=0.0; NSORT=0
  DO J=1,NIDF
   !## month of current result idf-file
   IMND=FIDF(J)%IMH
   !## skip if month cannot be derived
   IF(IMND.LE.0)CYCLE
   NSORT(IMND)=NSORT(IMND)+1
   DO I=1,NTHREAD
    JCOL=YSEL(1,I); JROW=YSEL(2,I)
    XSORT(NSORT(IMND),IMND)=XSORT(NSORT(IMND),IMND)+FIDF(J)%X(JCOL,JROW)
!    XSORT(J)=XSORT(J)+FIDF(J)%X(JCOL,JROW)
!    !## sum fluxes
!    IDF(3)%X(JCOL,JROW)=IDF(3)%X(JCOL,JROW)+1.0
   ENDDO
  ENDDO

  !## make a copy of xsort to ysort
  YSORT=XSORT

  !## get percentile
  IF(ITQP.EQ.1)THEN

   !## get percentiles (per month)
   DO IMND=1,SIZE(TQP,2)
    CALL UTL_GETMED(XSORT(:,IMND),NSORT(IMND),FIDF(1)%NODATA,PTQP,SIZE(PTQP),NX,XMED(:,IMND))
    DO I=1,SIZE(TQP,1); TQP(I,IMND)%X(ICOL,IROW)=XMED(I,IMND); ENDDO
   ENDDO

!   DO J=1,NIDF
!    !## found appropriate percentile in xsort()
!    CALL POL1LOCATE(XSORT,NIDF,REAL(YSORT(J),8),I)
!    IF(I.EQ.0)THEN
!     FP=1.0
!    ELSEIF(I.EQ.NIDF)THEN
!     FP=REAL(NIDF)
!    ELSE
!     FP=REAL(I)+(YSORT(J)-XSORT(I))/(XSORT(I+1)-XSORT(I))
!    ENDIF
!    FP=FP/REAL(NIDF)*100.0
!    RIDF(J)%X(ICOL,IROW)=FP !-PMED(1) !fp !YSORT(J) !real(i) !ysort(j) !fp !ysort(j) !FP !YSORT(J)-IDF(3)%X(ICOL,IROW)
!   ENDDO
   
  !## read percentile
  ELSE
  
   DO I=1,SIZE(TQP,1); DO J=1,SIZE(TQP,2); XMED(I,J)=TQP(I,J)%X(ICOL,IROW); ENDDO; ENDDO
   NSORT=SIZE(TQP,2)

  ENDIF
  
  !## check what category of percentile
  DO J=1,NIDF
   IMND=FIDF(J)%IMH
   !## skip if month cannot be derived
   IF(IMND.LE.0)CYCLE
   CALL POL1LOCATE(XMED(:,IMND),NSORT(IMND),REAL(YSORT(J,IMND),8),I)
!   CALL POL1LOCATE(XMED,SIZE(XMED),REAL(YSORT(J,IMND),8),I)
   RIDF(J)%X(ICOL,IROW)=REAL(I)
  ENDDO
   
  !## clean visited place
  DO I=1,NTHREAD; JCOL=YSEL(1,I); JROW=YSEL(2,I); IDF(2)%X(JCOL,JROW)=IDF(2)%NODATA; ENDDO
      
 ENDDO; WRITE(6,'(A,F7.3,A)') '+Progress ',REAL(IROW*100)/REAL(IDF(1)%NROW),' % finished        '; ENDDO

 !## save percentile discharges
 IF(ITQP.EQ.1)THEN
  DO I=1,SIZE(TQP,1); DO J=1,SIZE(TQP,2)
   IF(.NOT.IDFWRITE(TQP(I,J),TQP(I,J)%FNAME,1))RETURN
  ENDDO; ENDDO
 ENDIF

! IF(.NOT.IDFWRITE(IDF(3),IDF(1)%FNAME(:INDEX(IDF(1)%FNAME,'.',.TRUE.)-1)//'_TEST.IDF',1))THEN; ENDIF
 IYR=YTQP; IMD=0
 DO I=1,NIDF
  IMD=IMD+1
  IF(IMD.GT.12)THEN
   IYR=IYR+1; IMD=1
  ENDIF
  WRITE(WCD,'(I4.4,2I2.2)') IYR,IMD,1
  IF(.NOT.IDFWRITE(RIDF(I),TRIM(OUTPUTFOLDER)//'\TQ_PERCENT_'//TRIM(WCD)//'.IDF',1))THEN; ENDIF
 ENDDO
 
 DEALLOCATE(LISTNAME,XSORT,YSORT,XMED)
 CALL IDFDEALLOCATE(FIDF,SIZE(FIDF)); DEALLOCATE(FIDF)  

 END SUBROUTINE SOF_CATCHMENTS

 !###======================================================================
 SUBROUTINE SOF_TRACE(IDF,N,IWRITE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IWRITE,N 
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(N) :: IDF
 INTEGER :: IROW,ICOL,IR,IC,IP,IU,I,NF,M,NFTHREAD
 REAL :: A,DX,DY,F,SSX,SSY,XWBAL
 REAL,DIMENSION(0:2) :: LX,LY
 LOGICAL :: LWBAL,LSTOP
 TYPE TPOBJ
  INTEGER,POINTER,DIMENSION(:) :: IC,IR,IC_BU,IR_BU
  INTEGER :: NT
 END TYPE TPOBJ
 TYPE(TPOBJ),DIMENSION(:),ALLOCATABLE :: TP
 
 NFTHREAD=500

 ALLOCATE(TP(1))
 NULLIFY(TP(1)%IC); NULLIFY(TP(1)%IR)

 F=1.0
 
 IF(.NOT.IDFREAD(IDF(1),IDF(1)%FNAME,1))THEN; ENDIF
 
 CALL IDFCOPY(IDF(1),IDF(2)) !## total passes of particles
 CALL IDFCOPY(IDF(1),IDF(3)) !## counts individual particles

 LWBAL=.FALSE.
 IF(IDF(4)%FNAME.NE.'')THEN
  LWBAL=.TRUE.
  CALL IDFCOPY(IDF(1),IDF(4)); IDF(4)%X=0.0
  !## read entire ipf
  NIPF=1; CALL IPFALLOCATE(); IPF(1)%XCOL=1; IPF(1)%YCOL=2; IPF(1)%ZCOL=3; IPF(1)%Z2COL=1; IPF(1)%QCOL=1 
  IPF(1)%FNAME=IDF(4)%FNAME; IF(.NOT.IPFREAD2(1,1,0))RETURN
  IDF(4)%FNAME=IDF(1)%FNAME(:INDEX(IDF(1)%FNAME,'.',.TRUE.)-1)//'_ZONE.IDF'
  DO I=1,IPF(1)%NROW
   CALL IDFIROWICOL(IDF(4),IROW,ICOL,IPF(1)%XYZ(1,I),IPF(1)%XYZ(2,I))
   IDF(4)%X(ICOL,IROW)=IPF(1)%XYZ(3,I)
  ENDDO
  CALL IPFDEALLOCATE()
 ENDIF

 IF(IWRITE.EQ.1)THEN
  IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=IDF(1)%FNAME(:INDEX(IDF(1)%FNAME,'.',.TRUE.)-1)//'.GEN',STATUS='UNKNOWN')
 ENDIF

 WRITE(6,'(1X,A/)') 'Tracing ...'

 IDF(2)%X=0.0; IDF(3)%X=0.0
 ALLOCATE(TP(1)%IC(NFTHREAD),TP(1)%IR(NFTHREAD))

 !## number of particles
 IP=0

 !## stepsize
 SSX=F*IDF(1)%DX; SSY=SSX

 !## start tracing
 DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
  !## skip nodata
  IF(IDF(1)%X(ICOL,IROW).EQ.IDF(1)%NODATA)CYCLE
  
   IF(.TRUE.)THEN
!  IF(IROW.EQ.174.AND.ICOL.EQ.1441)THEN
  
   !## start in the middle
   CALL IDFGETLOC(IDF(1),IROW,ICOL,LX(0),LY(0))

   !## next particle
   IP=IP+1

   IF(IWRITE.EQ.1)THEN; WRITE(IU,*) IP; WRITE(IU,'(2(F15.3,1X))') LX(0),LY(0); ENDIF
   IC=ICOL; IR=IROW
   IDF(2)%X(IC,IR)=IDF(2)%X(IC,IR)+1.0
   IDF(3)%X(IC,IR)=REAL(IP)

   !## store starting location
   TP(1)%NT=1; TP(1)%IC(TP(1)%NT)=IC; TP(1)%IR(TP(1)%NT)=IR 

   NF=0
   LSTOP=.FALSE.
   
   DO
    NF=NF+1
    IF(NF.GT.IDF(1)%NCOL*IDF(1)%NROW)THEN
     WRITE(*,'(A,3I10)') 'Particle wont stop (IR,IC,NF): ',IR,IC,NF
     STOP
    ENDIF
        
    A=IDF(1)%X(IC,IR) !SOF_TRACE_GET_ANGLE_MEAN(IDF(1),LX(0),LY(0))
    
    DX=-COS(A)*SSX; DY=-SIN(A)*SSY
    LX(1)=LX(0)+DX; LY(1)=LY(0)+DY   
     
    !## get new grid location
    CALL IDFIROWICOL(IDF(1),IR,IC,LX(1),LY(1))
    
    !## outside model
    IF(IR.EQ.0.OR.IR.GT.IDF(1)%NROW.OR.IC.EQ.0.OR.IC.GT.IDF(1)%NCOL)THEN
     IF(LX(0).GT.IDF(1)%XMAX)IC=IDF(1)%NCOL
     IF(LX(0).LT.IDF(1)%XMIN)IC=1
     IF(LY(0).GT.IDF(1)%YMAX)IR=1
     IF(LY(0).LT.IDF(1)%YMIN)IR=IDF(1)%NROW
     LSTOP=.TRUE.
    ENDIF
    
    !## get new location in centre of grid
    CALL IDFGETLOC(IDF(1),IR,IC,LX(0),LY(0))

    IF(IWRITE.EQ.1)THEN
     WRITE(IU,'(2(F15.3,1X))') LX(0),LY(0)
     FLUSH(IU)
    ENDIF

    !## outside model
    IF(LSTOP)EXIT 

    !## trapped in nodata area
    IF(IDF(1)%X(IC,IR).EQ.IDF(1)%NODATA)EXIT
    
    !## count particles passes
    IF(IDF(3)%X(IC,IR).NE.REAL(IP))THEN
     IDF(2)%X(IC,IR)=IDF(2)%X(IC,IR)+1.0
     
     !## store thread
     TP(1)%NT=TP(1)%NT+1; M=TP(1)%NT
     IF(M.GT.SIZE(TP(1)%IC))THEN
      ALLOCATE(TP(1)%IC_BU(M*2),TP(1)%IR_BU(M*2))
      DO I=1,M-1
       TP(1)%IC_BU(I)=TP(1)%IC(I)
       TP(1)%IR_BU(I)=TP(1)%IR(I)
      ENDDO
      DEALLOCATE(TP(1)%IC,TP(1)%IR)
      TP(1)%IC=>TP(1)%IC_BU; TP(1)%IR=>TP(1)%IR_BU
     ENDIF
     TP(1)%IC(TP(1)%NT)=IC; TP(1)%IR(TP(1)%NT)=IR 
     
    ENDIF
    
    !## not to be counted again
    IDF(3)%X(IC,IR)=REAL(IP)

   ENDDO
   
   IF(IWRITE.EQ.1)WRITE(IU,*) 'END'
      
   !## waterbalance - trace backwards because of waterbalance-stations
   IF(LWBAL)THEN
    XWBAL=0.0
    DO I=TP(1)%NT,1,-1
     IC=TP(1)%IC(I); IR=TP(1)%IR(I)
     IF(IDF(4)%X(IC,IR).NE.0)XWBAL=IDF(4)%X(IC,IR)
     IDF(4)%X(IC,IR)=XWBAL
    ENDDO
   ENDIF

  ENDIF
 ENDDO; WRITE(6,'(A,F7.3,A)') '+Progress ',REAL(IROW*100)/REAL(IDF(1)%NROW),' % finished        '; ENDDO

 IF(IWRITE.EQ.1)THEN
  WRITE(IU,*) 'END'
  CLOSE(IU)
 ENDIF
 
 IF(.NOT.IDFWRITE(IDF(2),IDF(1)%FNAME(:INDEX(IDF(1)%FNAME,'.',.TRUE.)-1)//'_COUNT.IDF',1))THEN; ENDIF
 IF(LWBAL)THEN
  IF(.NOT.IDFWRITE(IDF(4),IDF(1)%FNAME(:INDEX(IDF(1)%FNAME,'.',.TRUE.)-1)//'_ZONE.IDF',1))THEN; ENDIF
 ENDIF
 
 IF(ASSOCIATED(TP(1)%IC))DEALLOCATE(TP(1)%IC)
 IF(ASSOCIATED(TP(1)%IR))DEALLOCATE(TP(1)%IR)
 DEALLOCATE(TP)
 
 END SUBROUTINE SOF_TRACE

 !###======================================================================
 REAL FUNCTION SOF_TRACE_GET_ANGLE_MEAN(IDF,XC,YC)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 REAL,INTENT(IN) :: XC,YC
 REAL,DIMENSION(2) :: A
 INTEGER :: IQ,IROW,ICOL,IC1,IC2,IR1,IR2,IC,IR,IWINDOW
 REAL :: DX,DY,X1,Y1,F,AF,TD,X2,Y2

 !## get proper cell
 CALL IDFIROWICOL(IDF,IR,IC,XC,YC)

 SOF_TRACE_GET_ANGLE_MEAN=IDF%X(IC,IR); RETURN
 
 IWINDOW=1
   
 !## get midpoint
 CALL IDFGETLOC(IDF,IR,IC,X1,Y1)

 !##get quadrants
 IF(X1.GE.XC)THEN
  IC1=IC-IWINDOW; IC2=IC
  IF(Y1.LE.YC)THEN
   IR1=IR-IWINDOW; IR2=IR  !## top right
  ELSE
   IR1=IR; IR2=IR+IWINDOW  !## bottom right
  ENDIF
 ELSE
  IC1=IC; IC2=IC+IWINDOW
  IF(Y1.LE.YC)THEN
   IR1=IR-IWINDOW; IR2=IR  !## top left
  ELSE
   IR1=IR; IR2=IR+IWINDOW  !## bottom left
  ENDIF
 ENDIF
    
 IC1=IC-IWINDOW
 IC2=IC+IWINDOW
 IR1=IR-IWINDOW
 IR2=IR+IWINDOW

 if(.TRUE.)then
  !## get mean of 4 points
  A=0.0; AF=0.0
  DO IROW=MAX(1,IR1),MIN(IDF%NROW,IR2); DO ICOL=MAX(1,IC1),MIN(IDF%NCOL,IC2)
   IF(IDF%X(ICOL,IROW).EQ.IDF%NODATA)EXIT
!   CALL IDFGETLOC(IDF,IROW,ICOL,X1,Y1)
!   F=UTL_DIST(XC,YC,X1,Y1)
!   IF(F.EQ.0.0)THEN
!F=1.0
!    SOF_TRACE_GET_ANGLE_MEAN=IDF%X(ICOL,IROW); RETURN
!   ENDIF
f=0.0
if(irow.eq.ir.or.icol.eq.ic)f=0.0
if(irow.eq.ir.and.icol.eq.ic)f=1.0
   !## each weighted as much as the others, in that way it converges
!   F=1.0/F
   DX=IDF%DX*COS(IDF%X(ICOL,IROW)); DY=IDF%DY*SIN(IDF%X(ICOL,IROW))
   A(1)=A(1)+DX*F; A(2)=A(2)+DY*F; AF=AF+F
  ENDDO; ENDDO
 
 else

   CALL IDFGETLOC(IDF,IR1,IC1,X1,Y1)   
   CALL IDFGETLOC(IDF,IR1,IC2,X2,Y2)   
   TD=X2-X1; F=(XC-X1)/TD
   
   DX=0.0; DY=0.0
   DX=   (1.0-F)*COS(IDF%X(IC1,IR1)); DY=   (1.0-F)*SIN(IDF%X(IC1,IR1))
   DX=DX+     F *COS(IDF%X(IC2,IR1)); DY=DY+     F *SIN(IDF%X(IC2,IR1))
   A(1)=ATAN2(DY,DX)

   DX=0.0; DY=0.0
   DX=   (1.0-F)*COS(IDF%X(IC1,IR2)); DY=   (1.0-F)*SIN(IDF%X(IC1,IR2))
   DX=DX+     F *COS(IDF%X(IC2,IR2)); DY=DY+     F *SIN(IDF%X(IC2,IR2))
   A(2)=ATAN2(DY,DX)
   
   TD=IDF%DY
   CALL IDFGETLOC(IDF,IR1,IC1,X1,Y1)   
   CALL IDFGETLOC(IDF,IR2,IC1,X2,Y2)   
   TD=Y1-Y2; F=(Y1-YC)/TD

   DX=0.0; DY=0.0
   DX=DX+(1.0-F)*COS(A(1)); DY=DY+(1.0-F)*SIN(A(1))
   DX=DX+     F *COS(A(2)); DY=DY+     F *SIN(A(2))

   A(1)=DX
   A(2)=DY
  
 endif
 
 SOF_TRACE_GET_ANGLE_MEAN=ATAN2(A(2),A(1)) 
 
 END FUNCTION SOF_TRACE_GET_ANGLE_MEAN
 
 !###======================================================================
 SUBROUTINE SOF_MAIN(IDF,IPNTR,IWINDOW,XMIN,YMIN,XMAX,YMAX,CELLSIZE,IGRAD)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:) :: IDF
 INTEGER,INTENT(IN) :: IWINDOW,IGRAD
 REAL,INTENT(IN) :: XMIN,YMIN,XMAX,YMAX,CELLSIZE
 INTEGER,INTENT(IN) :: IPNTR !## ipntr=0 no pointer to stop, 1 use pointer to stop
 INTEGER :: ITIC,ITOC,I,IROW,ICOL
  
 !## idf(1)=level (adjusted by outlet-pointer)
 !## idf(2)=slope
 !## idf(3)=aspect
 !## idf(4)=visited places
 !## idf(5)=pointer flow area, then copy of dem

 CALL OSD_TIMER(ITIC); WRITE(6,'(/A/)') '----'

 !## read levelidf
 IF(IWINDOW.EQ.0)THEN
  IF(.NOT.IDFREAD(IDF(1),IDF(1)%FNAME,1))THEN; RETURN; ENDIF
 ELSE
  IF(.NOT.IDFREAD(IDF(1),IDF(1)%FNAME,0))THEN; RETURN; ENDIF; CLOSE(IDF(1)%IU)
  IDF(1)%XMIN=XMIN; IDF(1)%XMAX=XMAX; IDF(1)%YMIN=YMIN; IDF(1)%YMAX=YMAX
  IDF(1)%DX=CELLSIZE; IDF(1)%DY=IDF(1)%DX
  CALL UTL_IDFSNAPTOGRID(IDF(1)%XMIN,IDF(1)%XMAX,IDF(1)%YMIN,IDF(1)%YMAX,IDF(1)%DX,IDF(1)%NCOL,IDF(1)%NROW)
  !## take blockvalue since no scaling is used, only a window is specified
  IF(.NOT.IDFREADSCALE(IDF(1)%FNAME,IDF(1),10,1,0.0,0))THEN; RETURN; ENDIF
 ENDIF

 !## allocate grid to determine area of influence by pointer
 DO I=2,SIZE(IDF)
  CALL IDFCOPY(IDF(1),IDF(I)); IF(.NOT.IDFALLOCATEX(IDF(I)))STOP
  IDF(I)%NODATA=-999.99; IDF(I)%X=IDF(I)%NODATA
 ENDDO

 !## read pointer (scale with most frequent value, option 7)
 IF(IPNTR.EQ.1)THEN
  IF(.NOT.IDFREADSCALE(IDF(5)%FNAME,IDF(5),7,1,0.0,0))THEN; RETURN; ENDIF
  !## adjust idf(1) for pointer
  DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
   IF(IDF(5)%X(ICOL,IROW).NE.IDF(5)%NODATA)IDF(1)%X(ICOL,IROW)=IDF(1)%NODATA
  ENDDO; ENDDO
 ENDIF
 
 !## indentify pitts
 CALL SOF_GET_PITT(IDF(1),IDF(5))
 IDF(5)%NODATA=0.0; IF(.NOT.IDFWRITE(IDF(5),IDF(3)%FNAME(:INDEX(IDF(3)%FNAME,'.',.TRUE.)-1)//'_pitt.idf',1))THEN; ENDIF

 !## copy dem
 IDF(5)%X=IDF(1)%X; IDF(5)%NODATA=IDF(1)%NODATA
 IF(IWINDOW.NE.0)THEN
  IF(.NOT.IDFWRITE(IDF(5),IDF(3)%FNAME(:INDEX(IDF(3)%FNAME,'.',.TRUE.)-1)//'_copydem.idf',1))THEN; ENDIF
 ENDIF
 
 !## fill in pitts - fill in gradients for flat-areas
 CALL SOF_FILL_PITT(IDF(1),IDF(2),IDF(3),IDF(4),IGRAD)
 IF(.NOT.IDFWRITE(IDF(1),IDF(3)%FNAME,1))THEN; ENDIF

 IF(IGRAD.EQ.1)THEN
  CALL SOF_COMPUTE_SLOPE_ASPECT(IDF(1),IDF(2),IDF(3))
  IF(.NOT.IDFWRITE(IDF(2),IDF(3)%FNAME(:INDEX(IDF(3)%FNAME,'.',.TRUE.)-1)//'_slope.idf',1))THEN; ENDIF
  IF(.NOT.IDFWRITE(IDF(3),IDF(3)%FNAME(:INDEX(IDF(3)%FNAME,'.',.TRUE.)-1)//'_aspect.idf',1))THEN; ENDIF
 ENDIF
 
 CALL OSD_TIMER(ITOC)
   
 I=(ITOC/100)-(ITIC/100)
 IF(I.LT.360)THEN
  WRITE(6,'(A,F10.3,A)') 'Process took',REAL(I),' seconds'
 ELSEIF(I.LT.3600.0)THEN
  WRITE(6,'(A,F10.3,A)') 'Process took',REAL(I)/360.0,' minutes'
 ELSEIF(I.LT.86400.0)THEN
  WRITE(6,'(A,F10.3,A)') 'Process took',REAL(I)/3600.0,' hours'
 ELSE
  WRITE(6,'(A,F10.3,A)') 'Process took',REAL(I)/86400.0,' days'
 ENDIF
 
 END SUBROUTINE SOF_MAIN

 !###======================================================================
 SUBROUTINE SOF_COMPUTE_SLOPE_ASPECT(DEM,SLOPE,ASPECT)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ) :: DEM,SLOPE,ASPECT
 INTEGER :: ICOL,IROW
 REAL :: DZDX,DZDY,S,A
 
 DO IROW=1,DEM%NROW
  DO ICOL=1,DEM%NCOL

!   if(icol.eq.61.and.irow.eq.28)then
!   write(*,*) 'dsds'
!   endif

   !## nodata dem map
   IF(DEM%X(ICOL,IROW).EQ.DEM%NODATA)CYCLE
   !## slope map already filled in
   IF(SLOPE%X(ICOL,IROW).NE.SLOPE%NODATA)CYCLE

!   if(icol.eq.219.and.irow.eq.184)then
!   write(*,*) 'dsds'
!   endif

!   CALL SOF_COMPUTE_GRAD(DEM,ICOL,IROW,DZDX,DZDY)

   !## from DEM it is much better to use this direction instead - treat nodata as sink
   CALL SOF_COMPUTE_GRAD_STEEPEST(DEM,ICOL,IROW,DZDX,DZDY,.TRUE.)
        
   !## radians  
   S=ATAN(SQRT(DZDX**2.0+DZDY**2.0))
   A=ATAN2(-1.0*DZDY,DZDX)

    !## degrees
!    S=S*(360.0/(2.0*3.1415)) 
!    A=A*(360.0/(2.0*3.1415))
    
   IF(S.NE.0.0)THEN
    SLOPE%X(ICOL,IROW) =S
    ASPECT%X(ICOL,IROW)=A
   ENDIF
      
  ENDDO
 ENDDO
 
! !## check whether 
!     A=SOF_TRACE_GET_ANGLE_MEAN(IDF(1),LX(0),LY(0))
!    ENDIF
!    
!    !## stepsize
!    SSX=F*IDF(1)%DX; SSY=SSX
!
!    DX=-COS(A)*SSX; DY=-SIN(A)*SSY
!    LX(1)=LX(0)+DX; LY(1)=LY(0)+DY   
     
 END SUBROUTINE SOF_COMPUTE_SLOPE_ASPECT
 
 !###======================================================================
 SUBROUTINE SOF_COMPUTE_GRAD_STEEPEST(DEM,ICOL,IROW,DZDX,DZDY,LNODATSINK)
 !###====================================================================== 
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: DEM
 INTEGER,INTENT(IN) :: ICOL,IROW
 REAL,INTENT(OUT) :: DZDX,DZDY
 LOGICAL,INTENT(IN) :: LNODATSINK
 REAL,DIMENSION(9) :: Z,GRADX,GRADY
 INTEGER :: IC1,IC2,IR1,IR2,I,J
 REAL :: AX,AY,ZM,F,TG,G
 DATA GRADX/-1.0, 0.0, 1.0, 1.0, 1.0, 0.0,-1.0,-1.0,0.0/
 DATA GRADY/ 1.0, 1.0, 1.0, 0.0,-1.0,-1.0,-1.0, 0.0,0.0/
                   
 !## steepest all around
 IC1=MAX(1,ICOL-1)
 IC2=MIN(DEM%NCOL,ICOL+1)
 IR1=MAX(1,IROW-1)
 IR2=MIN(DEM%NROW,IROW+1)

 Z(1)=DEM%X(IC1 ,IR1)
 Z(2)=DEM%X(ICOL,IR1 )
 Z(3)=DEM%X(IC2 ,IR1 )
 Z(4)=DEM%X(IC2 ,IROW)
 Z(5)=DEM%X(IC2 ,IR2)
 Z(6)=DEM%X(ICOL,IR2)
 Z(7)=DEM%X(IC1 ,IR2)
 Z(8)=DEM%X(IC1 ,IROW)
 Z(9)=DEM%X(ICOL,IROW)

 !## is it a pit on the edge ???
 IF(IC1.EQ.ICOL.OR.IC2.EQ.ICOL.OR.IR1.EQ.IROW.OR.IR2.EQ.IROW)THEN
  DO I=1,8
   IF(Z(I).LT.Z(9))EXIT
  ENDDO
  !## no escape
  IF(I.GT.8)THEN
   IF(IC1.EQ.ICOL)THEN
    Z(1)=DEM%NODATA
    Z(7)=DEM%NODATA
    Z(8)=DEM%NODATA
   ENDIF
   IF(IC2.EQ.ICOL)THEN
    Z(3)=DEM%NODATA
    Z(4)=DEM%NODATA
    Z(5)=DEM%NODATA
   ENDIF
   IF(IR1.EQ.IROW)THEN
    Z(1)=DEM%NODATA
    Z(2)=DEM%NODATA
    Z(3)=DEM%NODATA
   ENDIF
   IF(IR2.EQ.IROW)THEN
    Z(5)=DEM%NODATA
    Z(6)=DEM%NODATA
    Z(7)=DEM%NODATA
   ENDIF
  ENDIF
 ENDIF
      
 !## nodata act as as a strong sink, water want to move there urgently, in opposite direction
 ZM=Z(9)
  
 !## take steepest descent direction
 IF(LNODATSINK)THEN
  DO I=1,8
   IF(Z(I).EQ.DEM%NODATA)THEN
    IF(I.LE.4)THEN
     Z(I)=-1.0*Z(I+4)
    ELSE
     Z(I)=-1.0*Z(I-4)
    ENDIF
   ENDIF
  ENDDO

 !## take average downhill slope
 ELSE
  !## nodata acts as flat area
  DO I=1,9
   IF(Z(I).EQ.DEM%NODATA)Z(I)=ZM
  ENDDO
   
!  AX=0.0; AY=0.0 
!  DO I=1,9
!   Z(I)=Z(I)-ZM
!   IF(Z(I).LT.0.0)THEN
!    !## distance
!    F=SQRT(GRADX(I)**2.0+GRADY(I)**2.0)
!    !## inverse distance
!    F=1.0/F
!    AX=AX+GRADX(I)*F*ABS(Z(I))
!    AY=AY+GRADY(I)*F*ABS(Z(I))
!   ENDIF
!  ENDDO
 
 ENDIF
 
 J=0; TG=0.0; AX=0.0; AY=0.0 
 DO I=1,9
  Z(I)=Z(I)-ZM
  IF(Z(I).LT.0.0)THEN
   !## distance
   F=SQRT(GRADX(I)**2.0+GRADY(I)**2.0)
   G=Z(I)/F
   IF(G.LT.TG)THEN
    TG=G; J=I
   ENDIF
  ENDIF
 ENDDO
 !## perfect dome in flat area - occurs, but direction is irrelevant, choose flow right
 IF(J.EQ.0)THEN
  J=6
 ENDIF
 AX=GRADX(J)
 AY=GRADY(J)

 DZDX=-AX 
 DZDY= AY 

 !## perfect dome in flat area - occurs, but direction is irrelevant, choose 
 IF(DZDX.EQ.0.0.AND.DZDY.EQ.0.0)THEN
  DZDX=1.0
 ENDIF
  
 END SUBROUTINE SOF_COMPUTE_GRAD_STEEPEST
 
 !###======================================================================
 SUBROUTINE SOF_COMPUTE_GRAD(DEM,ICOL,IROW,DZDX,DZDY)
 !###====================================================================== 
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: DEM
 INTEGER,INTENT(IN) :: ICOL,IROW
 REAL,INTENT(OUT) :: DZDX,DZDY
 INTEGER :: IR1,IR2,IC1,IC2,I
 REAL :: Z1,Z2
 REAL,DIMENSION(0:6) :: Z
 
 !## 1nd order derivative
 IC1=MAX(1,ICOL-1)
 IC2=MIN(DEM%NCOL,ICOL+1)
 IR1=MAX(1,IROW-1)
 IR2=MIN(DEM%NROW,IROW+1)

 if(.false.)then
 
  Z(0)=DEM%X(ICOL,IROW) !## mid
  Z(1)=DEM%X(IC2 ,IROW) !## east
  Z(2)=DEM%X(ICOL ,IR1) !## north
  Z(3)=DEM%X(IC1 ,IROW) !## west
  Z(4)=DEM%X(ICOL ,IR2) !## south

  DO I=1,4; IF(Z(I).EQ.DEM%NODATA)Z(I)=DEM%X(ICOL,IROW); ENDDO
  DZDX=( Z(1)-Z(3) )/ (2.0*DEM%DX)
  DZDY=( Z(4)-Z(2) )/ (2.0*DEM%DX)

 ELSE
 
  ! tan (slope) = sqrt (b2 + c2)
  !b = (z3 + 2z6 + z9 - z1 - 2z4 - z7) / 8D 
  !c = (z1 + 2z2 + z3 - z7 - 2z8 - z9) / 8D
  !b denotes slope in the x direction 
  !c denotes slope in the y direction 
  !D is the spacing of points (30 m)
  !find the slope that fits best to the 9 elevations 
  !minimizes the total of squared differences between point elevation and the fitted slope 
  !weighting four closer neighbors higher
                 
  Z(0)=DEM%X(ICOL,IROW)
  Z(1)=DEM%X(IC2 ,IR2 ) !## right-down
  Z(2)=DEM%X(IC2 ,IROW) !## right
  Z(3)=DEM%X(IC2 ,IR1 ) !## right-up
  Z(4)=DEM%X(IC1 ,IR2 ) !## left-down
  Z(5)=DEM%X(IC1 ,IROW) !## left
  Z(6)=DEM%X(IC1 ,IR1 ) !## left-up
    
  DO I=1,6; IF(Z(I).EQ.DEM%NODATA)Z(I)=DEM%X(ICOL,IROW); ENDDO

  Z1=(Z(1)+2.8*Z(2)+Z(3)) !## right
  Z2=(Z(4)+2.8*Z(5)+Z(6)) !## left
  DZDX=( Z1 - Z2 )/ (9.6*DEM%DX)
   
  Z(1)=DEM%X(IC2 ,IR2) !## right-down
  Z(2)=DEM%X(ICOL,IR2) !## mid-down
  Z(3)=DEM%X(IC1 ,IR2) !## left-down
  Z(4)=DEM%X(IC2 ,IR1) !## right-up
  Z(5)=DEM%X(ICOL,IR1) !## mid-up
  Z(6)=DEM%X(IC1 ,IR1) !## left-up

  DO I=1,6; IF(Z(I).EQ.DEM%NODATA)Z(I)=DEM%X(ICOL,IROW); ENDDO
    
  Z1=(Z(1)+2.8*Z(2)+Z(3))
  Z2=(Z(4)+2.8*Z(5)+Z(6))
  DZDY=( Z1 - Z2 )/ (9.6*DEM%DX)

 ENDIF
   
 END SUBROUTINE SOF_COMPUTE_GRAD

 !###======================================================================
 SUBROUTINE SOF_COMPUTE_GRAD_3D(VEC,ICOL,IROW,DZDX,DZDY,DZDZ)
 !###====================================================================== 
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: VEC
 INTEGER,INTENT(IN) :: ICOL,IROW
 REAL,INTENT(OUT) :: DZDX,DZDY,DZDZ
 INTEGER :: IR1,IR2,IC1,IC2
 REAL :: FE,FN,FW,FS,FU,FD,TM,TE,TW,TN,TS,DX,DY
 
 !## 1nd order derivative
 IC1=MAX(1,ICOL-1)
 IC2=MIN(VEC%NCOL,ICOL+1)
 IR1=MAX(1,IROW-1)
 IR2=MIN(VEC%NROW,IROW+1)
 
 !## cellsizes
 IF(VEC%IEQ.EQ.0)THEN
  DX=VEC%DX
  DY=VEC%DY
 ELSE
  DX=VEC%SX(ICOL)-VEC%SX(ICOL-1)
  DY=VEC%SY(IROW-1)-VEC%SY(IROW)
 ENDIF
 
 !## flux east,north,west,south,up,down (m3/day)
 FE=     VEC%XV(ICOL,IROW,3)
 FN=-1.0*VEC%XV(ICOL,IR1 ,4)
 FW=-1.0*VEC%XV(IC1 ,IROW,3)
 FS=     VEC%XV(ICOL,IROW,4)
 FU=-1.0*VEC%XV(ICOL,IROW,5)
 FD=     VEC%XV(ICOL,IROW,6)

 !## thicknesses (m)
 TM=     VEC%XV(ICOL,IROW,1)-VEC%XV(ICOL,IROW,2)
 TE=0.5*((VEC%XV(IC2,IROW,1)-VEC%XV(IC2,IROW,2))+TM)
 TW=0.5*((VEC%XV(IC1,IROW,1)-VEC%XV(IC1,IROW,2))+TM)
 TN=0.5*((VEC%XV(ICOL,IR1,1)-VEC%XV(ICOL,IR1,2))+TM)
 TS=0.5*((VEC%XV(ICOL,IR2,1)-VEC%XV(ICOL,IR2,2))+TM)
 
 !## flux in m/day
 FE=FE/(TE*DY)
 FN=FN/(TN*DX)
 FW=FW/(TW*DY)
 FS=FS/(TS*DX)
 FU=FU/(DX*DY)
 FD=FD/(DX*DY)

 DZDX=(FE-FW)/2.0
 DZDY=(FS-FN)/2.0
 DZDZ=(FU-FD)/2.0
! DO I=1,4; IF(Z(I).EQ.VEC%NODATA)Z(I)=VEC%X(ICOL,IROW); ENDDO
! DZDX=( Z(1)-Z(3) )/ (2.0*DEM%DX)
! DZDY=( Z(4)-Z(2) )/ (2.0*DEM%DX)

 END SUBROUTINE SOF_COMPUTE_GRAD_3D
   
 !###======================================================================
 SUBROUTINE SOF_GET_PITT(DEM,IDFP)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: DEM
 TYPE(IDFOBJ),INTENT(INOUT) :: IDFP
 INTEGER :: ICOL,IROW,IR,IC
 REAL :: Z,F
 INTEGER :: IP,I,J
 
 ALLOCATE(PL(10,3)); IDFP%X=0.0; NP=0
 WRITE(6,'(1X,A/)') 'Searching for pitts'

 DO IROW=1,DEM%NROW
  DO ICOL=1,DEM%NCOL
   IF(DEM%X(ICOL,IROW).EQ.DEM%NODATA)CYCLE

   Z=DEM%X(ICOL,IROW); IP=0
   DO IR=MAX(1,IROW-1),MIN(DEM%NROW,IROW+1)
    DO IC=MAX(1,ICOL-1),MIN(DEM%NCOL,ICOL+1)
     !## nodata counts for pit-location
     IF(DEM%X(IC,IR).EQ.DEM%NODATA)THEN
      IP=IP+1
     ELSE
      IF(DEM%X(IC,IR).GE.Z)IP=IP+1
     ENDIF
    ENDDO
   ENDDO
   !## pit 
   IF(IP.EQ.9)THEN
    IF(NP+1.GT.SIZE(PL,1))THEN
     ALLOCATE(PL_BU(NP+100,3))
     DO I=1,NP; DO J=1,3; PL_BU(I,J)=PL(I,J); ENDDO; ENDDO
     DEALLOCATE(PL); PL=>PL_BU
    ENDIF
    NP=NP+1; PL(NP,1)=REAL(ICOL); PL(NP,2)=REAL(IROW); PL(NP,3)=DEM%X(ICOL,IROW)
    IDFP%X(ICOL,IROW)=1.0  
   ENDIF

  ENDDO
  F=REAL(IROW)/REAL(DEM%NROW)*100.0
  WRITE(6,'(A,F7.3,A)') '+Progress ',F,' % finished         '
 ENDDO
 
 !## sort list
 CALL SORTEM(1,NP,PL(:,3),2,PL(:,1),PL(:,2),PL,PL,PL,PL,PL)
 
 END SUBROUTINE SOF_GET_PITT

 !###======================================================================
 SUBROUTINE SOF_FILL_PITT(DEM,SLOPE,ASPECT,IDFP,IGRAD)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: DEM,IDFP,SLOPE,ASPECT
 INTEGER,INTENT(IN) :: IGRAD
 INTEGER :: I,J,ICOL,IROW,NBPX,NPPX,NTPX,ITYPE
 REAL :: F,ZMAX
 
 WRITE(6,'(1X,A/)') 'Filling in pitts'
  
 ALLOCATE(BPX(DEM%NROW*DEM%NCOL),PPX(DEM%NROW*DEM%NCOL),TPX(DEM%NROW*DEM%NCOL))
 IDFP%NODATA=0.0; IDFP%X=IDFP%NODATA
  
 !## trace depression for exit points
 DO I=1,NP 
  ICOL=INT(PL(I,1)); IROW=INT(PL(I,2))
    
  !## add this point to the pitt list, remove from the DEM
  !## pitt list
  NPPX=1; PPX(NPPX)%ICOL=ICOL; PPX(NPPX)%IROW=IROW; PPX(NPPX)%Z=DEM%X(ICOL,IROW); DEM%X(ICOL,IROW)=10.0E10
  !## current spill level
  ZMAX=PPX(NPPX)%Z
  !## boundary list
  NBPX=0; IDFP%X(ICOL,IROW)=1.0
  !## total boundary list (cleaning)
  NTPX=1; TPX(NPPX)%ICOL=ICOL; TPX(NPPX)%IROW=IROW

  ITYPE=0
  DO     
   !## fill bnd-pixel list and pit-list with irow,icol
   CALL SOF_FILL_BND(DEM,IDFP,IROW,ICOL,NBPX,NTPX)
   !## get lowest point in boundary list, natural exit function value becomes true
   IF(SOF_GET_EXITPOINT(DEM,IDFP,IROW,ICOL,ZMAX,NBPX,NPPX,ITYPE))THEN
    IF(ITYPE.EQ.0)THEN
     ITYPE=1
    ELSE
     EXIT
    ENDIF
   ENDIF
  ENDDO
  
  !## change all pittpoints for this spill-value (zmax)
  DO J=1,NPPX; DEM%X(PPX(J)%ICOL,PPX(J)%IROW)=ZMAX; ENDDO
   
  F=REAL(I)/REAL(NP)*100.0; WRITE(6,'(A,F10.3,2(A,I5),A)') '+Processing pitt:',F,' % (nppx= ',NPPX,' ; nbpx= ',NBPX,')'

  IF(IGRAD.EQ.1)CALL SOF_FILL_FLATAREAS(DEM,SLOPE,ASPECT,NPPX)
  
  !## clean idfp%x()
  DO J=1,NTPX; IDFP%X(TPX(J)%ICOL,TPX(J)%IROW)=IDFP%NODATA; ENDDO 
   
 ENDDO
 
 DEALLOCATE(BPX,PPX,TPX)
 
 END SUBROUTINE SOF_FILL_PITT 
 
 !###======================================================================
 SUBROUTINE SOF_FILL_FLATAREAS(DEM,SLOPE,ASPECT,NPPX)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: SLOPE,ASPECT,DEM
 INTEGER,INTENT(IN) :: NPPX
 TYPE(IDFOBJ) :: PCG 
 INTEGER :: IC1,IC2,IR1,IR2,I,IERROR,ICOL,IROW,IFCT,IC,IR
 REAL,POINTER,DIMENSION(:) :: XA,YA,ZA 
 REAL :: A
 REAL,PARAMETER :: HINI=0.0
   
 CALL IDFNULLIFY(PCG)

 IC1=MINVAL(PPX(1:NPPX)%ICOL)
 IC2=MAXVAL(PPX(1:NPPX)%ICOL)
 IR1=MINVAL(PPX(1:NPPX)%IROW)
 IR2=MAXVAL(PPX(1:NPPX)%IROW)

 PCG%NCOL=IC2-IC1+1; PCG%NROW=IR2-IR1+1; PCG%DX=DEM%DX; PCG%DY=DEM%DY
 PCG%XMIN=DEM%XMIN+(IC1-1)*DEM%DX; PCG%XMAX=PCG%XMIN+PCG%NCOL*DEM%DX
 PCG%YMAX=DEM%YMAX-(IR1-1)*DEM%DY; PCG%YMIN=PCG%YMAX-PCG%NROW*DEM%DY

 PCG%NCOL=PCG%NCOL; PCG%NROW=PCG%NROW; PCG%DX=PCG%DX; PCG%DY=PCG%DX

 IF(.NOT.IDFALLOCATEX(PCG))RETURN; PCG%NODATA=SLOPE%NODATA; PCG%X=PCG%NODATA 
 
 !## activate mid locations, exclusive spill level
 DO I=1,NPPX-1
  IC=PPX(I)%ICOL-IC1+1
  IR=PPX(I)%IROW-IR1+1
  PCG%X(IC,IR)=HINI
 ENDDO
 IC=PPX(NPPX)%ICOL-IC1+1
 IR=PPX(NPPX)%IROW-IR1+1
 PCG%X(IC,IR)=-1.0
 
! if(.not.idfwrite(pcg,'d:\pcg_ini'//trim(itos(1))//'X'//trim(itos(1))//'.idf',1))then; endif

 CALL SOF_SOLVE(PCG)

 DO I=1,NPPX-1
  ICOL=PPX(I)%ICOL-IC1+1
  IROW=PPX(I)%IROW-IR1+1
  A   =PCG%X(ICOL,IROW)
  IF(A.NE.PCG%NODATA)THEN
   ICOL=PPX(I)%ICOL
   IROW=PPX(I)%IROW
   SLOPE%X(ICOL,IROW) =0.0
   ASPECT%X(ICOL,IROW)=A
  ENDIF
 ENDDO
 !## clear pit location
 I=NPPX
 ICOL=PPX(I)%ICOL
 IROW=PPX(I)%IROW
 SLOPE%X(ICOL,IROW) =SLOPE%NODATA
 ASPECT%X(ICOL,IROW)=ASPECT%NODATA
 
! if(.not.idfwrite(pcg,'d:\pcg'//trim(itos(1))//'X'//trim(itos(1))//'.idf',1))then; endif

 CALL IDFDEALLOCATEX(PCG)
 
 END SUBROUTINE SOF_FILL_FLATAREAS
 
 !###======================================================================
 SUBROUTINE SOF_SOLVE(PCG)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ) :: PCG
 INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:,:) :: ID
 INTEGER(KIND=2),POINTER,DIMENSION(:,:,:) :: CR
 REAL,ALLOCATABLE,DIMENSION(:,:) :: A
 INTEGER :: IROW,ICOL,NCR,D,DR,DC,IL,I,IR,IC,ICR,JCR
 
 ALLOCATE(A(PCG%NCOL,PCG%NROW),ID(PCG%NCOL,PCG%NROW),CR(PCG%NCOL*PCG%NROW,2,2))
 A=PCG%NODATA; ID=INT(3,1)
 
 !## find starting location
ROWLOOP: DO IROW=1,PCG%NROW
  DO ICOL=1,PCG%NCOL
   IF(PCG%X(ICOL,IROW).EQ.-1)EXIT ROWLOOP
  ENDDO
 ENDDO ROWLOOP
 
 NCR=1; ICR=1; JCR=2
 CR(NCR,1,ICR)=ICOL; CR(NCR,2,ICR)=IROW
 DO
  
  IL=0
  DO I=1,NCR
   
   ICOL=CR(I,1,ICR)
   IROW=CR(I,2,ICR)
   !## no to be visited again
   PCG%X(ICOL,IROW)=2  

   DO IR=MAX(1,IROW-1),MIN(PCG%NROW,IROW+1)
    DO IC=MAX(1,ICOL-1),MIN(PCG%NCOL,ICOL+1)
     IF(PCG%X(IC,IR).EQ.0.0)THEN
      !## relative distance
      DR=IR-IROW
      DC=IC-ICOL
      D=ABS(DR)+ABS(DC)
      !## compare with distance to see if it is smaller
      IF(D.LT.ID(IC,IR))THEN
       ID(IC,IR)=D
       A(IC,IR)=ATAN2(-1.0*REAL(DR),REAL(DC))
       !## add to list
       IL=IL+1
       CR(IL,1,JCR)=IC
       CR(IL,2,JCR)=IR
      ENDIF
     ENDIF
    ENDDO
   ENDDO

  ENDDO

  !## no new location found
  IF(IL.EQ.0)EXIT
  
  !# interchange list
  IF(ICR.EQ.1)THEN
   ICR=2; JCR=1
  ELSE
   ICR=1; JCR=2
  ENDIF
  
  NCR=IL

 ENDDO

 !## fill pcg%x with angles
 PCG%X=A

 DEALLOCATE(A,ID,CR)
  
 END SUBROUTINE SOF_SOLVE

 !###======================================================================
 LOGICAL FUNCTION SOF_GET_EXITPOINT(DEM,IDFP,IROW,ICOL,ZMAX,NBPX,NPPX,ITYPE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: DEM,IDFP
 INTEGER,INTENT(IN) :: ITYPE
 INTEGER,INTENT(INOUT) :: NBPX,NPPX
 REAL,INTENT(INOUT) :: ZMAX
 REAL :: ZMIN
 INTEGER,INTENT(OUT) :: IROW,ICOL
 INTEGER :: I,IBND,I1
   
 !## find lowest spill-level
 IF(ITYPE.EQ.0)THEN
  ZMIN=BPX(1)%Z; IBND=1; I1=2
 ELSEIF(ITYPE.EQ.1)THEN
  I1=1; IBND=0
 ENDIF
 DO I=I1,NBPX
  !## search for lowest
  IF(ITYPE.EQ.0)THEN
   IF(BPX(I)%Z.LT.ZMIN)THEN
    IBND=I; ZMIN=BPX(I)%Z
   ENDIF
  !## search for equal
  ELSEIF(ITYPE.EQ.1)THEN
   IF(BPX(I)%Z.EQ.ZMAX)THEN
    IBND=I
   ENDIF  
  ENDIF
 ENDDO

 IF(IBND.NE.0)THEN
  !## location of the lowest point on the boundary
  ICOL=BPX(IBND)%ICOL; IROW=BPX(IBND)%IROW 
 ENDIF
 
 IF(ITYPE.EQ.0)THEN
  !## natural exit (zmin), level on boundary less than spill level (zmax)
  IF(ZMIN.LT.ZMAX)THEN; SOF_GET_EXITPOINT=.TRUE.; RETURN; ENDIF
 ELSEIF(ITYPE.EQ.1)THEN
  IF(IBND.EQ.0)THEN; SOF_GET_EXITPOINT=.TRUE.; RETURN; ENDIF
 ENDIF
 
 !## set new spill level
 IF(ITYPE.EQ.0)ZMAX=ZMIN
 
 !## include a very high offset
 BPX(IBND)%Z=10.0E10
 !## remove from boundary list not to be used again
 IDFP%X(BPX(IBND)%ICOL,BPX(IBND)%IROW)=IDFP%NODATA  
 DO I=IBND,NBPX-1; BPX(I)=BPX(I+1); ENDDO; NBPX=NBPX-1
 
 !## add this point to the pitt list
 IF(ITYPE.EQ.0)THEN
  NPPX            =NPPX+1
  PPX(NPPX)%ICOL  =ICOL
  PPX(NPPX)%IROW  =IROW
  PPX(NPPX)%Z     =DEM%X(ICOL,IROW)
 ELSEIF(ITYPE.EQ.1)THEN
  NPPX            =NPPX+1
  PPX(NPPX)%ICOL  =PPX(NPPX-1)%ICOL
  PPX(NPPX)%IROW  =PPX(NPPX-1)%IROW
  PPX(NPPX)%Z     =PPX(NPPX-1)%Z
  PPX(NPPX-1)%ICOL=ICOL
  PPX(NPPX-1)%IROW=IROW
  PPX(NPPX-1)%Z   =ZMAX
 ENDIF
 DEM%X(ICOL,IROW)=10.0E10 
 
 SOF_GET_EXITPOINT=.FALSE.
 
 END FUNCTION SOF_GET_EXITPOINT
 
 !###======================================================================
 SUBROUTINE SOF_FILL_BND(DEM,IDFP,IROW,ICOL,NBPX,NTPX)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: DEM,IDFP
 INTEGER,INTENT(IN) :: IROW,ICOL
 INTEGER,INTENT(INOUT) :: NBPX,NTPX
 INTEGER :: IR,IC
 REAL :: Z
 
 !## fill in boundary matrix
 DO IR=MAX(IROW-1,1),MIN(IROW+1,DEM%NROW)
  DO IC=MAX(ICOL-1,1),MIN(ICOL+1,DEM%NCOL)
   !## centre point
   IF(IC.EQ.ICOL.AND.IR.EQ.IROW)CYCLE
   !## already pit location
   IF(DEM%X(IC,IR).EQ.10.0E10)CYCLE
   !## already member of boundary
   IF(IDFP%X(IC,IR).NE.IDFP%NODATA)CYCLE

   Z=DEM%X(IC,IR)
   !## nodata location - include as spill-level   
   IF(DEM%X(IC,IR).EQ.DEM%NODATA)Z=-10.0E10 

   NBPX=NBPX+1
   BPX(NBPX)%ICOL=IC
   BPX(NBPX)%IROW=IR
   BPX(NBPX)%Z   =Z 
   NTPX=NTPX+1
   TPX(NTPX)%ICOL=IC
   TPX(NTPX)%IROW=IR
   IDFP%X(IC,IR) =1.0
  ENDDO
 ENDDO
 
 END SUBROUTINE SOF_FILL_BND

END MODULE MOD_SOF    