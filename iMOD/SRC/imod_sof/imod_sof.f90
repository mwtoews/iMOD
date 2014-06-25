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
USE MOD_SOLID_PAR, ONLY : HCLOSE,MXITER1,MXITER2,IDAMPING,RELAX,ITIGHT
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
 SUBROUTINE SOF_TRACE(IDF,IWRITE)
 !###======================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: F=0.25
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:) :: IDF
 INTEGER,INTENT(IN) :: IWRITE
 INTEGER :: IROW,ICOL,IR,IC,IP,IU,I,JR,JC,NF
 REAL :: A,DX,DY
 REAL,DIMENSION(0:2) :: LX,LY
 
 IF(IWRITE.EQ.1)THEN
  IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE='D:\TEST_TRACE3.GEN',STATUS='UNKNOWN')
 ENDIF
 IF(.NOT.IDFREAD(IDF(1),IDF(1)%FNAME,1))THEN; ENDIF
 IP=0; DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
  !## skip nodata
  IF(IDF(1)%X(ICOL,IROW).EQ.IDF(1)%NODATA)CYCLE
  
  IF(IROW.GE.80.AND.IROW.LE.130.AND. &
     ICOL.GE.130.AND.ICOL.LE.180)THEN

   !## start in the middle
   CALL IDFGETLOC(IDF(1),IROW,ICOL,LX(0),LY(0))
   IP=IP+1; WRITE(IU,*) IP; WRITE(IU,'(2(F15.3,1X))') LX(0),LY(0)
   IC=ICOL; IR=IROW

   NF=0
   DO
    NF=NF+1
    IF(NF.GT.1000)EXIT

    !## verbeteringen: gradienten op basis van afstand bepalen (gewogen naar afstand)

    !## get gradient on entry point
    A=SOF_TRACE_GET_ANGLE_MEAN(IDF(1),IC,IR,LX(0),LY(0))        

    DX=-COS(A)*F*IDF(1)%DX
    DY=-SIN(A)*F*IDF(1)%DY
    LX(1)=LX(0)+DX
    LY(1)=LY(0)+DY   
   
    !## new coordinates
    LX(0)=LX(1); LY(0)=LY(1)
    
    IF(IWRITE.EQ.1)WRITE(IU,'(2(F15.3,1X))') LX(0),LY(0)
    IF(IWRITE.EQ.1)FLUSH(IU)

    !## get new grid location
    CALL IDFIROWICOL(IDF(1),IR,IC,LX(0),LY(0))
    IF(IDF(1)%X(IC,IR).EQ.IDF(1)%NODATA)EXIT

   ENDDO
   IF(IWRITE.EQ.1)WRITE(IU,*) 'END'
      
  ENDIF
 ENDDO; ENDDO
 IF(IWRITE.EQ.1)WRITE(IU,*) 'END'
 
 END SUBROUTINE SOF_TRACE

 !###======================================================================
 REAL FUNCTION SOF_TRACE_GET_ANGLE_MEAN(IDF,IC,IR,XC,YC)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: IC,IR
 REAL,INTENT(IN) :: XC,YC
 REAL,DIMENSION(2) :: A
 INTEGER :: IQ,IROW,ICOL
 REAL :: DX,DY,X1,Y1,F,AF
  
 A=0.0; AF=0.0
 DO IROW=MAX(1,IR-1),MIN(IDF%NROW,IR+1); DO ICOL=MAX(1,IC-1),MIN(IDF%NCOL,IC+1)
  CALL IDFGETLOC(IDF,IROW,ICOL,X1,Y1)
  F=UTL_DIST(XC,YC,X1,Y1)
  IF(F.EQ.0.0)THEN
   SOF_TRACE_GET_ANGLE_MEAN=IDF%X(ICOL,IROW); RETURN
  ENDIF
  F=1.0/F
  DX=IDF%DX*COS(IDF%X(ICOL,IROW)); DY=IDF%DY*SIN(IDF%X(ICOL,IROW))
  A(1)=A(1)+DX*F; A(2)=A(2)+DY*F; AF=AF+F
 ENDDO; ENDDO

 SOF_TRACE_GET_ANGLE_MEAN=ATAN2(A(2),A(1)) !/4.0
 
 END FUNCTION SOF_TRACE_GET_ANGLE_MEAN
  
 !###======================================================================
 SUBROUTINE SOF_TRACE_WRITE(IU,IDF,X,Y,ICOL,IROW)
 !###======================================================================
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 REAL,INTENT(IN) :: X,Y
 INTEGER,INTENT(IN) :: ICOL,IROW,IU
 
 CALL IDFGETLOC(IDF,IROW,ICOL,XC,YC)
 !## transform to lower-left-corner
 XC=XC-IDF%DX/2.0
 YC=YC-IDF%DY/2.0
 !## get location 
 XC=XC+X*IDF%DX
 YC=YC+Y*IDF%DY

 WRITE(IU,'(2(F15.3,1X))') XC,YC
 flush(iu)
  
 END SUBROUTINE SOF_TRACE_WRITE
 
 !###======================================================================
 SUBROUTINE SOF_MAIN(IDF,IFLOW,IPNTR,IWINDOW,XMIN,YMIN,XMAX,YMAX)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:) :: IDF
 INTEGER,INTENT(IN) :: IWINDOW
 REAL,INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
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
  IDF(1)%XMIN=XMIN; IDF(1)%XMAX=XMAX; IDF(1)%YMIN=YMIN; IDF(1)%YMAX=YMAX;
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
 IF(.NOT.IDFWRITE(IDF(5),IDF(3)%FNAME(:INDEX(IDF(3)%FNAME,'.',.TRUE.)-1)//'_copydem.idf',1))THEN; ENDIF
 
 !## fill in pitts - fill in gradients for flat-areas using PCG interpolation from higher/lower grounds
 CALL SOF_FILL_PITT(IDF(1),IDF(2),IDF(3),IDF(4))
 IF(.NOT.IDFWRITE(IDF(1),IDF(3)%FNAME(:INDEX(IDF(3)%FNAME,'.',.TRUE.)-1)//'_zmax.idf',1))THEN; ENDIF

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
   
!    CALL SOF_COMPUTE_GRAD(DEM,ICOL,IROW,DZDX,DZDY)
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
      
!  XYPOL(1,1)=X1+COS(TNG)*XSIGHT
!  XYPOL(1,2)=Y1+SIN(TNG)*XSIGHT

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
 INTEGER :: IC1,IC2,IR1,IR2,I
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
    
 DO I=1,9; IF(Z(I).EQ.DEM%NODATA)Z(I)=DEM%X(ICOL,IROW); ENDDO
 ZM=Z(5)
 AX=0.0; AY=0.0; NA=0.0
 DO I=1,9
  Z(I)=Z(I)-ZM
  IF(Z(I).LT.0.0)THEN
   AX=AX+GRADX(I)*ABS(Z(I))
   AY=AY+GRADY(I)*ABS(Z(I))
   NA=NA+ABS(Z(I)) !1.0   
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
 REAL,DIMENSION(0:4) :: Z
 
 !## 1nd order derivative
 IC1=MAX(1,ICOL-1)
 IC2=MIN(DEM%NCOL,ICOL+1)
 IR1=MAX(1,IROW-1)
 IR2=MIN(DEM%NROW,IROW+1)

 Z(0)=DEM%X(ICOL,IROW) !## mid
 Z(1)=DEM%X(IC2 ,IROW) !## east
 Z(2)=DEM%X(ICOL ,IR1) !## north
 Z(3)=DEM%X(IC1 ,IROW) !## west
 Z(4)=DEM%X(ICOL ,IR2) !## south

 DO I=1,4; IF(Z(I).EQ.DEM%NODATA)Z(I)=DEM%X(ICOL,IROW); ENDDO
 DZDX=( Z(1)-Z(3) )/ (2.0*DEM%DX)
 DZDY=( Z(4)-Z(2) )/ (2.0*DEM%DX)

 RETURN
 
! LSND=.FALSE.
!  
! !## 2nd order derivative
! IC1=MAX(1,ICOL-1)
! IC2=MIN(DEM%NCOL,ICOL+1)
! IR1=MAX(1,IROW-1)
! IR2=MIN(DEM%NROW,IROW+1)
!                
! Z(0)=DEM%X(ICOL,IROW)
! Z(1)=DEM%X(IC2 ,IR2 )
! Z(2)=DEM%X(IC2 ,IROW)
! Z(3)=DEM%X(IC2 ,IR1 )
! Z(4)=DEM%X(IC1 ,IR2 )
! Z(5)=DEM%X(IC1 ,IROW)
! Z(6)=DEM%X(IC1 ,IR1 )
!    
! DO I=1,6; IF(Z(I).EQ.DEM%NODATA)Z(I)=DEM%X(ICOL,IROW); ENDDO
!
! IF(LSND)THEN
!  Z1=(Z(1)+2.0*Z(2)+Z(3))
!  Z2=(Z(4)+2.0*Z(5)+Z(6))
!  IF(Z1/4.0.GT.Z(0).AND.Z2/4.0.GT.Z(0))THEN
!   DZDX=0.0
!  ELSE  
!   DZDX=( Z1 - Z2 )/ (8.0*DEM%DX)
!  ENDIF
! ELSE
!  DZDX=( Z(2)-Z(5) )/ (2.0*DEM%DX)
!  IF(Z(2).GT.Z(0).AND.Z(5).GT.Z(0))DZDX=0.0
! ENDIF
!   
! Z(1)=DEM%X(IC2 ,IR2)
! Z(2)=DEM%X(ICOL,IR2)
! Z(3)=DEM%X(IC1 ,IR2)
! Z(4)=DEM%X(IC2 ,IR1)
! Z(5)=DEM%X(ICOL,IR1)
! Z(6)=DEM%X(IC1 ,IR1)
!
! DO I=1,6; IF(Z(I).EQ.DEM%NODATA)Z(I)=DEM%X(ICOL,IROW); ENDDO
!    
! IF(LSND)THEN
!  Z1=(Z(1)+2.0*Z(2)+Z(3))
!  Z2=(Z(4)+2.0*Z(5)+Z(6))
!  IF(Z1/4.0.GT.Z(0).AND.Z2/4.0.GT.Z(0))THEN
!   DZDY=0.0
!  ELSE
!   DZDY=( Z1 - Z2 )/ (8.0*DEM%DX)
! ! DZDY=( (Z(1)+2.0*Z(2)+Z(3)) - (Z(4)+2.0*Z(5)+Z(6)) )/(8.0*DEM%DY)
!  ENDIF
! ELSE
!  DZDY=( Z(2)-Z(5) )/ (2.0*DEM%DY)
!  IF(Z(2).GT.Z(0).AND.Z(5).GT.Z(0))DZDY=0.0
! ENDIF
   
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
     IF(DEM%X(IC,IR).NE.DEM%NODATA.AND.DEM%X(IC,IR).GE.Z)IP=IP+1
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

!  IF(ICOL.EQ.175.AND.IROW.EQ.160)THEN
!  WRITE(*,*)
!  ENDIF
!  ICOL=148; IROW=130
!  ICOL=150; IROW=128
  
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

  !## change all pittpoints for this spill-value (zmax)
  DO J=1,NPPX; DEM%X(PPX(J)%ICOL,PPX(J)%IROW)=ZMAX; ENDDO
  
!  IF(PPX(J)%ICOL.EQ.148.AND.PPX(J)%IROW.EQ.130)THEN
!  WRITE(*,*)
!  ENDIF
  
  CALL SOF_FILL_FLATAREAS(DEM,SLOPE,ASPECT,NBPX,NPPX)

!## dummy export
!  IF(.NOT.IDFWRITE(IDFP,'d:\buffer.idf',1))THEN; ENDIF
!  IDFP%X=DEM%NODATA
!  DO J=1,NPPX; IDFP%X(PPX(J)%ICOL,PPX(J)%IROW)=ZMAX; ENDDO
!  IDFP%NODATA=DEM%NODATA; IF(.NOT.IDFWRITE(IDFP,'d:\zmax.idf',1))THEN; ENDIF
!  IDFP%NODATA=0.0; IDFP%X=IDFP%NODATA
!## dummy export

  !## clean idfp%x()
  DO J=1,NTPX; IDFP%X(TPX(J)%ICOL,TPX(J)%IROW)=IDFP%NODATA; ENDDO 

  F=REAL(I)/REAL(NP)*100.0; WRITE(6,'(A,F10.3,2(A,I5),A)') '+Progress finished ',F,' % (nppx= ',NPPX,' ; nbpx= ',NBPX,')'
   
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
 INTEGER :: IC1,IC2,IR1,IR2,I,IERROR,ICOL,IROW
 REAL,POINTER,DIMENSION(:) :: XA,YA,ZA
 REAL :: DZDX,DZDY,S,A
  
 CALL IDFNULLIFY(PCG)
 
 IC1=MINVAL(BPX(1:NBPX)%ICOL); IC2=MAXVAL(BPX(1:NBPX)%ICOL)
 IR1=MINVAL(BPX(1:NBPX)%IROW); IR2=MAXVAL(BPX(1:NBPX)%IROW)
 PCG%NCOL=IC2-IC1+1; PCG%NROW=IR2-IR1+1; PCG%DX=DEM%DX; PCG%DY=DEM%DY
 PCG%XMIN=DEM%XMIN+IC1*DEM%DX; PCG%XMAX=PCG%XMIN+PCG%NCOL*DEM%DX
 PCG%YMAX=DEM%YMAX-IR1*DEM%DY; PCG%YMIN=PCG%YMAX-PCG%NROW*DEM%DY
 IF(.NOT.IDFALLOCATEX(PCG))RETURN; PCG%NODATA=SLOPE%NODATA; PCG%X=PCG%NODATA 
 
 ALLOCATE(XA(NBPX+1),YA(NBPX+1),ZA(NBPX+1))
 DO I=1,NBPX
  XA(I)=BPX(I)%ICOL-IC1+1
  YA(I)=BPX(I)%IROW-IR1+1
  ZA(I)=1.0 
 ENDDO
 DO I=1,NPPX
  ICOL            =PPX(I)%ICOL-IC1+1
  IROW            =PPX(I)%IROW-IR1+1
  PCG%X(ICOL,IROW)=0.0 
 ENDDO
 !## add last pit-location (outflow) as boundary condition as well
 I         = NPPX
 XA(NBPX+1)= PPX(I)%ICOL-IC1+1
 YA(NBPX+1)= PPX(I)%IROW-IR1+1
 ZA(NBPX+1)=-1.0
   
 HCLOSE=0.0001; RELAX=1.0; IDAMPING=0; MXITER1=100; MXITER2=1000; ITIGHT=2
 CALL SOLID_PCGINT(XA,YA,ZA,NBPX+1,IERROR,PCG,0,HNOFLOW=PCG%NODATA)
 IF(IERROR.EQ.1)STOP 'ERROR IN PCG'

 !## remove boundary nodes, not to be used for aspect computation
 DO I=1,NBPX+1; PCG%X(INT(XA(I)),INT(YA(I)))=PCG%NODATA; ENDDO
 
 DO IROW=1,PCG%NROW; DO ICOL=1,PCG%NCOL
  IF(PCG%X(ICOL,IROW).EQ.PCG%NODATA)CYCLE

  CALL SOF_COMPUTE_GRAD(PCG,ICOL,IROW,DZDX,DZDY)

  !## radians  
  S=ATAN(SQRT(DZDX**2.0+DZDY**2.0))
  A=ATAN2(-1.0*DZDY,DZDX)

  SLOPE%X(ICOL+IC1-1,IROW+IR1-1) =S
  ASPECT%X(ICOL+IC1-1,IROW+IR1-1)=A

 ENDDO; ENDDO
 
    !## degrees
!    S=S*(360.0/(2.0*3.1415)) 
!    A=A*(360.0/(2.0*3.1415))
     
! if(.not.idfwrite(pcg,'d:\pcg.idf',1))then; endif
 
 DEALLOCATE(XA,YA,ZA) 

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
 
 !## fill in boundary matrix
 DO IR=MAX(IROW-1,1),MIN(IROW+1,DEM%NROW)
  DO IC=MAX(ICOL-1,1),MIN(ICOL+1,DEM%NCOL)
   !## centre point
   IF(IC.EQ.ICOL.AND.IR.EQ.IROW)CYCLE
   !## nodata location
   IF(DEM%X(IC,IR).EQ.DEM%NODATA)CYCLE
   !## allready pit location
   IF(DEM%X(IC,IR).EQ.10.0E10)CYCLE
   !## allready member of boundary
   IF(IDFP%X(IC,IR).NE.IDFP%NODATA)CYCLE
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