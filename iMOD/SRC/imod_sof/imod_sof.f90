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
MODULE MOD_SOF

USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFREAD,IDFREADSCALE,IDFCOPY,IDFALLOCATEX,IDFWRITE,IDFDEALLOCATEX,IDFNULLIFY,IDFGETLOC,IDFIROWICOL
USE MOD_IDFEDIT_TRACE, ONLY : IDFEDITTRACE 
USE MOD_OSD, ONLY : OSD_TIMER,OSD_OPEN
USE MOD_UTL, ONLY : UTL_IDFSNAPTOGRID,UTL_GETUNIT,UTL_DIST
USE MOD_SOLID_PCG, ONLY : SOLID_PCGINT
USE MOD_SOLID_PAR, ONLY : HCLOSE,MXITER1,MXITER2,IDAMPING,RELAX,ITIGHT,MICNVG
USE MOD_IPF, ONLY : IPFALLOCATE,IPFREAD2,IPFDEALLOCATE
USE MOD_IPF_PAR, ONLY : IPF,NIPF
USE IMODVAR, ONLY : PI

REAL,POINTER,DIMENSION(:,:),PRIVATE :: PL,PL_BU
INTEGER,PRIVATE :: NP

TYPE BPXOBJ
 INTEGER :: ICOL,IROW
 REAL :: Z
END TYPE BPXOBJ
TYPE(BPXOBJ),ALLOCATABLE,DIMENSION(:),PRIVATE :: BPX,PPX,TPX

CONTAINS

 !###======================================================================
 SUBROUTINE SOF_TRACE(IDF,N,IWRITE,FRACTION,NFTHREAD,MXTHREAD)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: FRACTION
 INTEGER,INTENT(IN) :: IWRITE,N,NFTHREAD,MXTHREAD
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(N) :: IDF
 INTEGER :: IROW,ICOL,IR,IC,IP,IU,I,J,NF,IT,NT,IPT,M
 REAL :: A,DX,DY,F,SSX,SSY,XWBAL
 REAL,DIMENSION(0:2) :: LX,LY
 REAL,DIMENSION(4) :: RK
 LOGICAL :: LRUNGAKUTTA,LWBAL
 TYPE TPOBJ
  INTEGER,POINTER,DIMENSION(:) :: IC,IR,IC_BU,IR_BU
  INTEGER :: NT
 END TYPE TPOBJ
 TYPE(TPOBJ),DIMENSION(:),ALLOCATABLE :: TP
 logical :: ltrial
 
 ALLOCATE(TP(0:MXTHREAD))
 DO I=0,MXTHREAD; NULLIFY(TP(I)%IC); NULLIFY(TP(I)%IR); ENDDO
  
 LRUNGAKUTTA=.TRUE.
 
 F=FRACTION
 
 IF(.NOT.IDFREAD(IDF(1),IDF(1)%FNAME,1))THEN; ENDIF
 
 CALL IDFCOPY(IDF(1),IDF(2)) !## total passes of particles
 CALL IDFCOPY(IDF(1),IDF(3)) !## counts individual particles
 CALL IDFCOPY(IDF(1),IDF(4)) !## number of particle in thread
 CALL IDFCOPY(IDF(1),IDF(5)) !## position in thread

 LWBAL=.FALSE.
 IF(IDF(6)%FNAME.NE.'')THEN
  LWBAL=.TRUE.
  CALL IDFCOPY(IDF(1),IDF(6)); IDF(6)%X=0.0
  !## read entire ipf
  NIPF=1; CALL IPFALLOCATE(); IPF(1)%XCOL=1; IPF(1)%YCOL=2; IPF(1)%ZCOL=3; IPF(1)%Z2COL=1; IPF(1)%QCOL=1 
  IPF(1)%FNAME=IDF(6)%FNAME; IF(.NOT.IPFREAD2(1,1,0))RETURN
  IDF(6)%FNAME=IDF(1)%FNAME(:INDEX(IDF(1)%FNAME,'.',.TRUE.)-1)//'_ZONE.IDF'
  DO I=1,IPF(1)%NROW
   CALL IDFIROWICOL(IDF(6),IROW,ICOL,IPF(1)%XYZ(1,I),IPF(1)%XYZ(2,I))
   IDF(6)%X(ICOL,IROW)=IPF(1)%XYZ(3,I)
  ENDDO
  CALL IPFDEALLOCATE()
 ENDIF

 IF(IWRITE.EQ.1)THEN
  IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=IDF(1)%FNAME(:INDEX(IDF(1)%FNAME,'.',.TRUE.)-1)//'.GEN',STATUS='UNKNOWN')
 ENDIF

 WRITE(6,'(1X,A/)') 'Tracing ...'

 IDF(2)%X=0.0; IDF(3)%X=0.0; IDF(4)%X=0.0; IDF(5)%X=0.0
 
 ALLOCATE(TP(0)%IC(NFTHREAD),TP(0)%IR(NFTHREAD))

 !## number of threads
 NT=0
 !## number of particles
 IP=0

 !## start tracing
 DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
  !## skip nodata
  IF(IDF(1)%X(ICOL,IROW).EQ.IDF(1)%NODATA)CYCLE
  
!  IF(IROW.GE.80.AND.IROW.LE.130.AND. &
!     ICOL.GE.130.AND.ICOL.LE.180)THEN
!  IF(IROW.GE.65.AND.IROW.LE.100.AND. &
!     ICOL.GE.95.AND.ICOL.LE.113)THEN
!  IF(IROW.EQ.74.AND.ICOL.EQ.102)THEN
!  IF(IROW.EQ.44.AND.ICOL.EQ.63)THEN
!  IF(IROW.EQ.389.AND.ICOL.EQ.234)THEN
!  IF(IROW.EQ.318.AND.ICOL.EQ.356)THEN
  IF(IROW.EQ.150.AND.ICOL.EQ.255)THEN
!  IF(.TRUE.)THEN
  
   !## start in the middle
   CALL IDFGETLOC(IDF(1),IROW,ICOL,LX(0),LY(0))

 ltrial=.false.
! LX(0)=583828.0
! LY(0)=5794983.0

   IP=IP+1
   IF(IWRITE.EQ.1)THEN; WRITE(IU,*) IP; WRITE(IU,'(2(F15.3,1X))') LX(0),LY(0); ENDIF
   IC=ICOL; IR=IROW
   IDF(2)%X(IC,IR)=IDF(2)%X(IC,IR)+1.0
   IDF(3)%X(IC,IR)=REAL(IP)

   !## store starting location
   TP(0)%NT=1; TP(0)%IC(TP(0)%NT)=IC; TP(0)%IR(TP(0)%NT)=IR 

   NF=0
   
   DO
    NF=NF+1
    IF(NF.GT.IDF(1)%NCOL*IDF(1)%NROW)THEN
     WRITE(*,'(A,3I10)') 'Particle wont stop (IR,IC,NF): ',IR,IC,NF
     STOP
    ENDIF
    
    !## stepsize
    SSX=F*IDF(1)%DX; SSY=SSX

    !## get gradient on entry point
    IF(LRUNGAKUTTA)THEN

     !## first order
     RK(1)=SOF_TRACE_GET_ANGLE_MEAN(IDF(1),LX(0),LY(0))
     
     !## second order
     DX=-COS(RK(1))*SSX*0.5; DY=-SIN(RK(1))*SSY*0.5
     LX(1)=LX(0)+DX; LY(1)=LY(0)+DY
     RK(2)=SOF_TRACE_GET_ANGLE_MEAN(IDF(1),LX(1),LY(1))

if(ltrial)then
     WRITE(IU,*) 1
     WRITE(IU,'(2(F15.3,1X))') LX(0),LY(0)
     WRITE(IU,'(2(F15.3,1X))') LX(1),LY(1)
     WRITE(IU,'(A)') 'END'
endif
     
     !## third order
     DX=-COS(RK(2))*SSX*0.5; DY=-SIN(RK(2))*SSY*0.5
     LX(1)=LX(0)+DX; LY(1)=LY(0)+DY
     RK(3)=SOF_TRACE_GET_ANGLE_MEAN(IDF(1),LX(1),LY(1))

if(ltrial)then
     WRITE(IU,*) 1
     WRITE(IU,'(2(F15.3,1X))') LX(0),LY(0)
     WRITE(IU,'(2(F15.3,1X))') LX(1),LY(1)
     WRITE(IU,'(A)') 'END'
endif

     !## fourth order
     DX=-COS(RK(3))*SSX; DY=-SIN(RK(3))*SSY
     LX(1)=LX(0)+DX; LY(1)=LY(0)+DY
     RK(4)=SOF_TRACE_GET_ANGLE_MEAN(IDF(1),LX(1),LY(1))

if(ltrial)then
     WRITE(IU,*) 1
     WRITE(IU,'(2(F15.3,1X))') LX(0),LY(0)
     WRITE(IU,'(2(F15.3,1X))') LX(1),LY(1)
     WRITE(IU,'(A)') 'END'
endif

     DX=0.0; DY=0.0
     DX=DX+COS(RK(1))*SSX;     DY=DY+SIN(RK(1))*SSY
     DX=DX+COS(RK(2))*SSX*2.0; DY=DY+SIN(RK(2))*SSY*2.0
     DX=DX+COS(RK(3))*SSX*2.0; DY=DY+SIN(RK(3))*SSY*2.0
     DX=DX+COS(RK(4))*SSX;     DY=DY+SIN(RK(4))*SSY
     A=ATAN2(DY,DX)
     
    ELSE
     A=SOF_TRACE_GET_ANGLE_MEAN(IDF(1),LX(0),LY(0))
    ENDIF
    
    DX=-COS(A)*SSX; DY=-SIN(A)*SSY
    LX(1)=LX(0)+DX; LY(1)=LY(0)+DY   
   
if(ltrial)then
     WRITE(IU,*) 1
     WRITE(IU,'(2(F15.3,1X))') LX(0),LY(0)
     WRITE(IU,'(2(F15.3,1X))') LX(1),LY(1)
     WRITE(IU,'(A)') 'END'
     WRITE(IU,'(A)') 'END'
STOP
endif

    !## new coordinates
    LX(0)=LX(1); LY(0)=LY(1)
    
    IF(IWRITE.EQ.1)WRITE(IU,'(2(F15.3,1X))') LX(0),LY(0)
    IF(IWRITE.EQ.1)FLUSH(IU)

    !## get new grid location
    CALL IDFIROWICOL(IDF(1),IR,IC,LX(0),LY(0))

    IT=0

    !## outside model
    IF(IR.EQ.0.OR.IR.GT.IDF(1)%NROW.OR.IC.EQ.0.OR.IC.GT.IDF(1)%NCOL)EXIT
    !## trapped in nodata area
    IF(IDF(1)%X(IC,IR).EQ.IDF(1)%NODATA)EXIT
    
    !## count particles passes
    IF(IDF(3)%X(IC,IR).NE.REAL(IP))THEN
     IDF(2)%X(IC,IR)=IDF(2)%X(IC,IR)+1.0
     
     !## store thread
     TP(0)%NT=TP(0)%NT+1; M=TP(0)%NT
     IF(M.GT.SIZE(TP(0)%IC))THEN
      ALLOCATE(TP(0)%IC_BU(M*2),TP(0)%IR_BU(M*2))
      DO I=1,M-1
       TP(0)%IC_BU(I)=TP(0)%IC(I)
       TP(0)%IR_BU(I)=TP(0)%IR(I)
      ENDDO
      DEALLOCATE(TP(0)%IC,TP(0)%IR)
      TP(0)%IC=>TP(0)%IC_BU; TP(0)%IR=>TP(0)%IR_BU
     ENDIF
     TP(0)%IC(TP(0)%NT)=IC; TP(0)%IR(TP(0)%NT)=IR 
     
    ENDIF
    
    !## not to be counted again
    IDF(3)%X(IC,IR)=REAL(IP)

    !## trace particle in thread and whenever trace is captured, stop processing and add selected tread
    IF(IDF(4)%X(IC,IR).NE.0)THEN
     !## probably traced before, fill in appropriate thread
     IT =INT(IDF(4)%X(IC,IR)); IPT=INT(IDF(5)%X(IC,IR))
     !# fill in rest of thread
     DO I=IPT+1,TP(IT)%NT
      IC=TP(IT)%IC(I); IR=TP(IT)%IR(I)
      IDF(2)%X(IC,IR)=IDF(2)%X(IC,IR)+1.0
     ENDDO
     EXIT
    ENDIF

   ENDDO
   
   IF(IWRITE.EQ.1)WRITE(IU,*) 'END'
      
   !## particle long enough to form a complete new thread to be used again
   IF(TP(0)%NT.GT.NFTHREAD)THEN
    !## copy thread
    NT=NT+1
    NF=TP(0)%NT
    !## add intersecting thread
    IF(IT.NE.0)NF=NF+(TP(IT)%NT-IPT)
    ALLOCATE(TP(NT)%IC(NF),TP(NT)%IR(NF))
    DO I=1,TP(0)%NT
     IC=TP(0)%IC(I);  IR=TP(0)%IR(I)
     TP(NT)%IC(I)=IC; TP(NT)%IR(I)=IR
     IDF(4)%X(IC,IR)=REAL(NT)
     IDF(5)%X(IC,IR)=REAL(I)
    ENDDO
    I=I-1
    IF(IT.NE.0)THEN
     !## add remaining thread that has been intersected
     DO J=TP(IT)%NT,IPT+1,-1
      I=I+1
      IC=TP(IT)%IC(J); IR=TP(IT)%IR(J)
      TP(NT)%IC(I)=IC; TP(NT)%IR(I)=IR
     ENDDO
    ENDIF
    TP(NT)%NT=NF
   ENDIF

   !## waterbalance - trace backwards becasue of waterbalance-stations
   IF(LWBAL)THEN
    XWBAL=0.0
    DO I=TP(IT)%NT,1,-1
     IC=TP(IT)%IC(I); IR=TP(IT)%IR(I)
     IF(IDF(6)%X(IC,IR).NE.0)XWBAL=IDF(6)%X(IC,IR)
     IDF(6)%X(IC,IR)=XWBAL
    ENDDO
   ENDIF

  ENDIF
 ENDDO; WRITE(6,'(A,F7.3,A)') '+Progress ',REAL(IROW*100)/REAL(IDF(1)%NROW),' % finished        '; ENDDO

 IF(IWRITE.EQ.1)THEN
  WRITE(IU,*) 'END'
  CLOSE(IU)
 ENDIF
 
 IF(.NOT.IDFWRITE(IDF(2),IDF(1)%FNAME(:INDEX(IDF(1)%FNAME,'.',.TRUE.)-1)//'_COUNT.IDF',1))THEN; ENDIF
 IF(.NOT.IDFWRITE(IDF(4),IDF(1)%FNAME(:INDEX(IDF(1)%FNAME,'.',.TRUE.)-1)//'_THREAD.IDF',1))THEN; ENDIF
 IF(.NOT.IDFWRITE(IDF(5),IDF(1)%FNAME(:INDEX(IDF(1)%FNAME,'.',.TRUE.)-1)//'_THREADPOS.IDF',1))THEN; ENDIF
 IF(LWBAL)THEN
  IF(.NOT.IDFWRITE(IDF(6),IDF(1)%FNAME(:INDEX(IDF(1)%FNAME,'.',.TRUE.)-1)//'_ZONE.IDF',1))THEN; ENDIF
 ENDIF
 
 DO I=0,MXTHREAD
  IF(ASSOCIATED(TP(I)%IC))DEALLOCATE(TP(I)%IC)
  IF(ASSOCIATED(TP(I)%IR))DEALLOCATE(TP(I)%IR)
 ENDDO
 DEALLOCATE(TP)
 
 END SUBROUTINE SOF_TRACE

 !###======================================================================
 REAL FUNCTION SOF_TRACE_GET_ANGLE_MEAN(IDF,XC,YC)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 REAL,INTENT(IN) :: XC,YC
 REAL,DIMENSION(2) :: A
 INTEGER :: IQ,IROW,ICOL,IC1,IC2,IR1,IR2,IC,IR
 REAL :: DX,DY,X1,Y1,F,AF

 !## get proper cell
 CALL IDFIROWICOL(IDF,IR,IC,XC,YC)
 !## get midpoint
 CALL IDFGETLOC(IDF,IR,IC,X1,Y1)
 !##get quadrants
 IF(X1.GE.XC)THEN
  IC1=IC-1; IC2=IC
  IF(Y1.LE.YC)THEN
   IR1=IR-1; IR2=IR  !## top right
  ELSE
   IR1=IR; IR2=IR+1  !## bottom right
  ENDIF
 ELSE
  IC1=IC; IC2=IC+1
  IF(Y1.LE.YC)THEN
   IR1=IR-1; IR2=IR  !## top left
  ELSE
   IR1=IR; IR2=IR+1  !## bottom left
  ENDIF
 ENDIF
    
 !## handig om gemiddelde vector tenemen van 8 punten , beter denk gemiddelde van 4 dichtbijgelegen punten
 A=0.0; AF=0.0
 DO IROW=MAX(1,IR1),MIN(IDF%NROW,IR2); DO ICOL=MAX(1,IC1),MIN(IDF%NCOL,IC2)
  IF(IDF%X(ICOL,IROW).EQ.IDF%NODATA)EXIT
  CALL IDFGETLOC(IDF,IROW,ICOL,X1,Y1)
  F=UTL_DIST(XC,YC,X1,Y1)
  IF(F.EQ.0.0)THEN
   SOF_TRACE_GET_ANGLE_MEAN=IDF%X(ICOL,IROW); RETURN
  ENDIF
  !## each weighted as much as the others, in that way it converges
  F=1.0
  DX=IDF%DX*COS(IDF%X(ICOL,IROW)); DY=IDF%DY*SIN(IDF%X(ICOL,IROW))
  A(1)=A(1)+DX*F; A(2)=A(2)+DY*F; AF=AF+F
 ENDDO; ENDDO

 SOF_TRACE_GET_ANGLE_MEAN=ATAN2(A(2),A(1)) 
 
 END FUNCTION SOF_TRACE_GET_ANGLE_MEAN
 
 !###======================================================================
 SUBROUTINE SOF_MAIN(IDF,IFLOW,IPNTR,IWINDOW,XMIN,YMIN,XMAX,YMAX,CELLSIZE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:) :: IDF
 INTEGER,INTENT(IN) :: IWINDOW
 REAL,INTENT(IN) :: XMIN,YMIN,XMAX,YMAX,CELLSIZE
 INTEGER,INTENT(IN) :: IFLOW !## iflow=nul compute depressions, iflow=one, compute flowpaths
 INTEGER,INTENT(IN) :: IPNTR !## ipntr=0 no pointer to stop, 1 use pointer to stop
 INTEGER :: ITIC,ITOC,I,IROW,ICOL
  
 !## idf(1)=level (adjusted)
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
  CALL IDFCOPY(IDF(1),IDF(5)); IF(.NOT.IDFREADSCALE(IDF(5)%FNAME,IDF(5),7,1,0.0,0))THEN; RETURN; ENDIF
  !## adjust idf(1) for pointer
  DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
   IF(IDF(5)%X(ICOL,IROW).EQ.IDF(5)%NODATA)IDF(1)%X(ICOL,IROW)=IDF(1)%NODATA
  ENDDO; ENDDO
  CALL IDFDEALLOCATEX(IDF(5))
 ENDIF
 
 !## indentify pitts
 CALL SOF_GET_PITT(IDF(1),IDF(5))
 IDF(5)%NODATA=0.0; IF(.NOT.IDFWRITE(IDF(5),IDF(3)%FNAME(:INDEX(IDF(3)%FNAME,'.',.TRUE.)-1)//'_pitt.idf',1))THEN; ENDIF

 !## copy dem
 IDF(5)%X=IDF(1)%X
 IF(IWINDOW.NE.0)THEN
  IF(.NOT.IDFWRITE(IDF(5),IDF(3)%FNAME(:INDEX(IDF(3)%FNAME,'.',.TRUE.)-1)//'_copydem.idf',1))THEN; ENDIF
 ENDIF
 
 !## fill in pitts - fill in gradients for flat-areas using PCG interpolation from higher/lower grounds
 CALL SOF_FILL_PITT(IDF(1),IDF(2),IDF(3),IDF(4))
 IF(.NOT.IDFWRITE(IDF(1),IDF(3)%FNAME,1))THEN; ENDIF
! IF(.NOT.IDFWRITE(IDF(1),IDF(3)%FNAME(:INDEX(IDF(3)%FNAME,'.',.TRUE.)-1)//'_zmax.idf',1))THEN; ENDIF

 CALL SOF_COMPUTE_SLOPE_ASPECT(IDF(1),IDF(2),IDF(3))
 IF(.NOT.IDFWRITE(IDF(2),IDF(3)%FNAME(:INDEX(IDF(3)%FNAME,'.',.TRUE.)-1)//'_slope.idf',1))THEN; ENDIF
 IF(.NOT.IDFWRITE(IDF(3),IDF(3)%FNAME(:INDEX(IDF(3)%FNAME,'.',.TRUE.)-1)//'_aspect.idf',1))THEN; ENDIF
 
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
 REAL,DIMENSION(6) :: Z
 REAL :: DZDX,DZDY,S,A
 
 DO IROW=1,DEM%NROW
  DO ICOL=1,DEM%NCOL
   !## nodata dem map
   IF(DEM%X(ICOL,IROW).EQ.DEM%NODATA)CYCLE
   !## slope map allready filled in
   IF(SLOPE%X(ICOL,IROW).NE.SLOPE%NODATA)CYCLE

   if(icol.eq.255.and.irow.eq.150)then
   write(*,*) 'dsds'
   endif

!   CALL SOF_COMPUTE_GRAD(DEM,ICOL,IROW,DZDX,DZDY)

   !## from DEM it is much better to use this direction instead
   CALL SOF_COMPUTE_GRAD_STEEPEST(DEM,ICOL,IROW,DZDX,DZDY)
        
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
 
 END SUBROUTINE SOF_COMPUTE_SLOPE_ASPECT
 
 !###======================================================================
 SUBROUTINE SOF_COMPUTE_GRAD_STEEPEST(DEM,ICOL,IROW,DZDX,DZDY)
 !###====================================================================== 
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: DEM
 INTEGER,INTENT(IN) :: ICOL,IROW
 REAL,INTENT(OUT) :: DZDX,DZDY
 REAL,DIMENSION(9) :: Z,GRADX,GRADY
 INTEGER :: IC1,IC2,IR1,IR2,I,J
 REAL :: AX,AY,NA,ZM
 DATA GRADX/-1.0, 0.0, 1.0,-1.0,0.0,1.0,-1.0, 0.0, 1.0/
 DATA GRADY/ 1.0, 1.0, 1.0, 0.0,0.0,0.0,-1.0,-1.0,-1.0/
   
 !## steepest all around
 IC1=MAX(1,ICOL-1)
 IC2=MIN(DEM%NCOL,ICOL+1)
 IR1=MAX(1,IROW-1)
 IR2=MIN(DEM%NROW,IROW+1)
                
 Z(1)=DEM%X(IC1 ,IR1 )
 Z(2)=DEM%X(ICOL,IR1 )
 Z(3)=DEM%X(IC2 ,IR1 )
 Z(4)=DEM%X(IC1 ,IROW)
 Z(5)=DEM%X(ICOL,IROW)
 Z(6)=DEM%X(IC2 ,IROW)
 Z(7)=DEM%X(IC1 ,IR2 )
 Z(8)=DEM%X(ICOL,IR2 )
 Z(9)=DEM%X(IC2 ,IR2 )
    
! DO I=1,9; IF(Z(I).EQ.DEM%NODATA)Z(I)=DEM%X(ICOL,IROW); ENDDO
 !## nodata act as as a strong sink, water want to move there urgently, in opposite direction
 ZM=Z(5)
 J=9; DO I=1,9
  IF(Z(I).EQ.DEM%NODATA)Z(I)=-1.0*Z(J)
  J=J-1
 ENDDO
 AX=0.0; AY=0.0; NA=0.0
 DO I=1,9
  Z(I)=Z(I)-ZM
  IF(Z(I).LT.0.0)THEN
   AX=AX+GRADX(I)*ABS(Z(I))
   AY=AY+GRADY(I)*ABS(Z(I))
   NA=NA+ABS(Z(I))    
  ENDIF
 ENDDO
 DZDX=-AX 
 DZDY= AY 
 
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
 DZDX=( Z1 - Z2 )/ (8.0*DEM%DX)
   
 Z(1)=DEM%X(IC2 ,IR2) !## right-down
 Z(2)=DEM%X(ICOL,IR2) !## mid-down
 Z(3)=DEM%X(IC1 ,IR2) !## left-down
 Z(4)=DEM%X(IC2 ,IR1) !## right-up
 Z(5)=DEM%X(ICOL,IR1) !## mid-up
 Z(6)=DEM%X(IC1 ,IR1) !## left-up

 DO I=1,6; IF(Z(I).EQ.DEM%NODATA)Z(I)=DEM%X(ICOL,IROW); ENDDO
    
 Z1=(Z(1)+2.8*Z(2)+Z(3))
 Z2=(Z(4)+2.8*Z(5)+Z(6))
 DZDY=( Z1 - Z2 )/ (8.0*DEM%DX)

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
 TYPE(IDFOBJ),INTENT(OUT) :: IDFP
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
 SUBROUTINE SOF_FILL_PITT(DEM,SLOPE,ASPECT,IDFP)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: DEM,IDFP,SLOPE,ASPECT
 INTEGER :: I,J,ICOL,IROW,NBPX,NPPX,NTPX
 REAL :: F,ZMAX
 
 WRITE(6,'(1X,A/)') 'Filling in pitts'
  
 ALLOCATE(BPX(DEM%NROW*DEM%NCOL),PPX(DEM%NROW*DEM%NCOL),TPX(DEM%NROW*DEM%NCOL))
 IDFP%NODATA=0.0; IDFP%X=IDFP%NODATA
  
 !## trace depression for exit points
 DO I=1,NP
  ICOL=INT(PL(I,1)); IROW=INT(PL(I,2))
  
!  IF(ICOL.EQ.110.AND.IROW.EQ.89)THEN
  IF(ICOL.EQ.228.AND.IROW.EQ.136)THEN
  WRITE(*,*) 'DSDS'
  ENDIF
  
  !## add this point to the pitt list, remove from the DEM
  !## pitt list
  NPPX=1; PPX(NPPX)%ICOL=ICOL; PPX(NPPX)%IROW=IROW; PPX(NPPX)%Z=DEM%X(ICOL,IROW); DEM%X(ICOL,IROW)=10.0E10
  !## current spill level
  ZMAX=PPX(NPPX)%Z
  !## boundary list
  NBPX=0; IDFP%X(ICOL,IROW)=1.0
  !## total boundary list (cleaning)
  NTPX=1; TPX(NPPX)%ICOL=ICOL; TPX(NPPX)%IROW=IROW

  DO     
   !## fill bnd-pixel list and pit-list with irow,icol
   CALL SOF_FILL_BND(DEM,IDFP,IROW,ICOL,NBPX,NTPX)
   !## get lowest point in boundary list, natural exit function value becomes true
   IF(SOF_GET_EXITPOINT(DEM,IDFP,IROW,ICOL,ZMAX,NBPX,NPPX))EXIT
  ENDDO

!  if(abs(zmax-903.8867).le.0.01)then
  if(abs(zmax-908.6198).le.0.01)then
write(*,*) 'area'
  endif
  
  !## change all pittpoints for this spill-value (zmax)
  DO J=1,NPPX; DEM%X(PPX(J)%ICOL,PPX(J)%IROW)=ZMAX; ENDDO
    
  F=REAL(I)/REAL(NP)*100.0; WRITE(6,'(A,F10.3,2(A,I5),A)') '+Processing pitt:',F,' % (nppx= ',NPPX,' ; nbpx= ',NBPX,')'

  CALL SOF_FILL_FLATAREAS(DEM,SLOPE,ASPECT,NBPX,NPPX)

  !## clean idfp%x()
  DO J=1,NTPX; IDFP%X(TPX(J)%ICOL,TPX(J)%IROW)=IDFP%NODATA; ENDDO 
   
 ENDDO
 
 DEALLOCATE(BPX,PPX,TPX)
 
 END SUBROUTINE SOF_FILL_PITT 
 
 !###======================================================================
 SUBROUTINE SOF_FILL_FLATAREAS(DEM,SLOPE,ASPECT,NBPX,NPPX)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: SLOPE,ASPECT,DEM
 INTEGER,INTENT(IN) :: NBPX,NPPX
 TYPE(IDFOBJ) :: PCG
 INTEGER :: IC1,IC2,IR1,IR2,I,IERROR,ICOL,IROW,IFCT,IC,IR
 REAL,POINTER,DIMENSION(:) :: XA,YA,ZA,CA
 REAL :: DZDX,DZDY,S,A
  
 CALL IDFNULLIFY(PCG)
 
 IC1=MIN(MINVAL(BPX(1:NBPX)%ICOL),MINVAL(PPX(1:NPPX)%ICOL))
 IC2=MAX(MAXVAL(BPX(1:NBPX)%ICOL),MAXVAL(PPX(1:NPPX)%ICOL))
 IR1=MIN(MINVAL(BPX(1:NBPX)%IROW),MINVAL(PPX(1:NPPX)%IROW))
 IR2=MAX(MAXVAL(BPX(1:NBPX)%IROW),MAXVAL(PPX(1:NPPX)%IROW))
 PCG%NCOL=IC2-IC1+1; PCG%NROW=IR2-IR1+1; PCG%DX=DEM%DX; PCG%DY=DEM%DY
 PCG%XMIN=DEM%XMIN+(IC1-1)*DEM%DX; PCG%XMAX=PCG%XMIN+PCG%NCOL*DEM%DX
 PCG%YMAX=DEM%YMAX-(IR1-1)*DEM%DY; PCG%YMIN=PCG%YMAX-PCG%NROW*DEM%DY

 IFCT=3; PCG%NCOL=PCG%NCOL*IFCT; PCG%NROW=PCG%NROW*IFCT; PCG%DX=PCG%DX/REAL(IFCT); PCG%DY=PCG%DX

 IF(.NOT.IDFALLOCATEX(PCG))RETURN; PCG%NODATA=SLOPE%NODATA; PCG%X=PCG%NODATA 
 
 ALLOCATE(XA(NBPX+1),YA(NBPX+1),ZA(NBPX+1),CA(NBPX+1))
 !## transform to correct location
 DO I=1,NBPX
  XA(I)=((BPX(I)%ICOL-IC1+1)*IFCT)-1
  YA(I)=((BPX(I)%IROW-IR1+1)*IFCT)-1
  ZA(I)=  1.0
  CA(I)=  1.0/REAL(NBPX)
 ENDDO
 !## add last pit-location (outflow) as boundary condition as well
 I         = NPPX
 XA(NBPX+1)=((PPX(I)%ICOL-IC1+1)*IFCT)-1
 YA(NBPX+1)=((PPX(I)%IROW-IR1+1)*IFCT)-1
 ZA(NBPX+1)= -1.0
 CA(NBPX+1)=  1.0
    
 !## activate boundary location
 DO I=1,NBPX+1
  DO IROW=YA(I)-1,YA(I)+1
   DO ICOL=XA(I)-1,XA(I)+1
    PCG%X(ICOL,IROW)=0.0
   ENDDO
  ENDDO
 ENDDO
 
 !## activate mid locations
 DO I=1,NPPX
  IC=((PPX(I)%ICOL-IC1+1)*IFCT)-1
  IR=((PPX(I)%IROW-IR1+1)*IFCT)-1
  DO IROW=IR-1,IR+1 
   DO ICOL=IC-1,IC+1 
    PCG%X(ICOL,IROW)=0.0
   ENDDO
  ENDDO
 ENDDO
 
 HCLOSE=0.0001; RELAX=1.0; IDAMPING=0; MXITER1=100; MXITER2=1000; ITIGHT=2; MICNVG=0
 CALL SOLID_PCGINT(XA,YA,ZA,NBPX+1,IERROR,PCG,0,HNOFLOW=PCG%NODATA,CD=CA)

! if(.not.idfwrite(pcg,'d:\pcg.idf',1))then; endif

 !## compute aspect for active nodes, exclusive the spill-level
 DO I=1,NPPX-1

  ICOL=((PPX(I)%ICOL-IC1+1)*IFCT)-1
  IROW=((PPX(I)%IROW-IR1+1)*IFCT)-1

  CALL SOF_COMPUTE_GRAD_STEEPEST(PCG,ICOL,IROW,DZDX,DZDY)

  !## radians  
  S=ATAN(SQRT(DZDX**2.0+DZDY**2.0))
  A=ATAN2(-1.0*DZDY,DZDX)

  ICOL=PPX(I)%ICOL
  IROW=PPX(I)%IROW

  SLOPE%X(ICOL,IROW) =S
  ASPECT%X(ICOL,IROW)=A

 ENDDO !; ENDDO
 
    !## degrees
!    S=S*(360.0/(2.0*3.1415)) 
!    A=A*(360.0/(2.0*3.1415))
     
 DEALLOCATE(XA,YA,ZA,CA) 

 END SUBROUTINE SOF_FILL_FLATAREAS
 
 !###======================================================================
 LOGICAL FUNCTION SOF_GET_EXITPOINT(DEM,IDFP,IROW,ICOL,ZMAX,NBPX,NPPX)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: DEM,IDFP
 INTEGER,INTENT(INOUT) :: NBPX,NPPX
 REAL,INTENT(INOUT) :: ZMAX
 REAL :: ZMIN
 INTEGER,INTENT(OUT) :: IROW,ICOL
 INTEGER :: I,IBND
   
 ZMIN=BPX(1)%Z; IBND=1
 DO I=2,NBPX
  IF(BPX(I)%Z.LT.ZMIN)THEN
   IBND=I; ZMIN=BPX(I)%Z
  ENDIF
 ENDDO
 !## location of the lowest point on the boundary
 ICOL=BPX(IBND)%ICOL; IROW=BPX(IBND)%IROW 

 !## natural exit (zmin), level on boundary less than spill level (zmax)
 IF(ZMIN.LT.ZMAX)THEN; SOF_GET_EXITPOINT=.TRUE.; RETURN; ENDIF
 
 !## set new spill level
 ZMAX=ZMIN
 
 !## include a very high offset
 BPX(IBND)%Z=10.0E10
 !## remove from boundary list not to be used again
 IDFP%X(BPX(IBND)%ICOL,BPX(IBND)%IROW)=IDFP%NODATA  
 DO I=IBND,NBPX-1; BPX(I)=BPX(I+1); ENDDO; NBPX=NBPX-1
 
 !## add this point to the pitt list
 NPPX            =NPPX+1
 PPX(NPPX)%ICOL  =ICOL
 PPX(NPPX)%IROW  =IROW
 PPX(NPPX)%Z     =DEM%X(ICOL,IROW)
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
   !## allready pit location
   IF(DEM%X(IC,IR).EQ.10.0E10)CYCLE
   !## allready member of boundary
   IF(IDFP%X(IC,IR).NE.IDFP%NODATA)CYCLE

   Z=DEM%X(IC,IR)
   !## nodata location - include as spill-level   
   IF(DEM%X(IC,IR).EQ.DEM%NODATA)Z=-10.0E10 !CYCLE

   NBPX=NBPX+1
   BPX(NBPX)%ICOL=IC
   BPX(NBPX)%IROW=IR
   BPX(NBPX)%Z   =DEM%X(IC,IR)
   NTPX=NTPX+1
   TPX(NTPX)%ICOL=IC
   TPX(NTPX)%IROW=IR
   IDFP%X(IC,IR) =1.0
  ENDDO
 ENDDO
 
 END SUBROUTINE SOF_FILL_BND

END MODULE MOD_SOF    