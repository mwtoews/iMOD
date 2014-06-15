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
!  IF(IROW.eq.108.AND.Icol.eq.136)THEN

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
 
!    write(*,*) a*(180.0/pi)
    
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

! !###======================================================================
! SUBROUTINE SOF_TRACE_TRIM(X0,Y0,X,Y)
! !###======================================================================
! IMPLICIT NONE
! REAL,INTENT(IN) :: X0,Y0
! REAL,INTENT(INOUT) :: X,Y
! REAL :: F
! 
! !## overshoot to the right/top
! IF(X.GT.1.0)THEN
!  F=(1.0-X0)/(X-X0)  !1.0/X
!  X=X0+(X-X0)*F !1.0 --- to be exact
!  Y=Y0+(Y-Y0)*F 
! !## overshoot to the left/bottom
! ELSEIF(X.LT.0.0)THEN
!  F=(X0)/(X0-X)  
!  X=X0+(X-X0)*F !0.0 --- to be exact
!  Y=Y0+(Y-Y0)*F 
! ENDIF
! 
! END SUBROUTINE SOF_TRACE_TRIM

! !###======================================================================
! SUBROUTINE SOF_TRACE_GETLXLY(LX,LY,A,IP) 
! !###======================================================================
! IMPLICIT NONE
! REAL,INTENT(IN) :: A
! INTEGER,INTENT(OUT) :: IP
! REAL,INTENT(INOUT),DIMENSION(0:2) :: LX,LY
! REAL,DIMENSION(2) :: L
!    
! !## flow to the left, or to the right
! LX(1)=0.0; IF(ABS(A).GE.PI/2.0)LX(1)=1.0
! LY(1)=LY(0)+(LX(1)-LX(0))*TAN(A)
! !## flow to the bottom or to the top
! LY(2)=0.0; IF(A.LE.0.0)LY(2)=1.0
! LX(2)=LX(0)+(LY(2)-LY(0))/TAN(A)
!
! !## location of both exit points
! L(1)=SQRT((LX(1)-LX(0))**2.0+(LY(1)-LY(0))**2.0)
! L(2)=SQRT((LX(2)-LX(0))**2.0+(LY(2)-LY(0))**2.0)
! !## get shortest vectorlength
! IP=1; IF(L(2).LT.L(1))IP=2
! 
! !## 8 gradienten
!
! !## exit point may no be equal to startpoint, choose otherwise
! IF(LX(IP).EQ.LX(0).AND.LY(IP).EQ.LY(0))THEN
!  IF(IP.EQ.1)THEN
!   IP=2
!  ELSE
!   IP=1
!  ENDIF
! ENDIF
! 
! END SUBROUTINE SOF_TRACE_GETLXLY
 
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
 
! IF(X.EQ.0.5.AND.Y.EQ.0.5)THEN
!  SOF_TRACE_GET_ANGLE_MEAN=IDF%X(IC,IR)
!  RETURN
! ENDIF
  
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

! A=A/AF

 SOF_TRACE_GET_ANGLE_MEAN=ATAN2(A(2),A(1)) !/4.0
 
! SOF_TRACE_GET_ANGLE=IDF%X(IC,IR)

 END FUNCTION SOF_TRACE_GET_ANGLE_MEAN
 
! !###======================================================================
! REAL FUNCTION SOF_TRACE_GET_ANGLE(IDF,IC,IR,X,Y)
! !###======================================================================
! IMPLICIT NONE
! TYPE(IDFOBJ),INTENT(IN) :: IDF
! INTEGER,INTENT(IN) :: IC,IR
! REAL,INTENT(IN) :: X,Y
! REAL,DIMENSION(2) :: A
! INTEGER :: IQ,IROW,ICOL
! REAL :: DX,DY
! 
! IF(X.EQ.0.5.AND.Y.EQ.0.5)THEN
!  SOF_TRACE_GET_ANGLE=IDF%X(IC,IR)
!  RETURN
! ENDIF
!  
! !## in what quadrant is current location
! !##   1  |  2
! !##   -------
! !##   4  |  3
! IF(X.GT.0.5)THEN
!  IF(Y.GT.0.5)THEN
!   IQ=2
!  ELSE
!   IQ=3
!  ENDIF
! ELSE
!  IF(Y.GT.0.5)THEN
!   IQ=1
!  ELSE
!   IQ=4
!  ENDIF
! ENDIF
!
! A=0.0
! SELECT CASE (IQ)
!  CASE (1)
!   DO IROW=MAX(1,IR-1),IR; DO ICOL=MAX(1,IC-1),IC
!    DX=COS(IDF%X(ICOL,IROW)); DY=SIN(IDF%X(ICOL,IROW))
!    A(1)=A(1)+DX; A(2)=A(2)+DY
!   ENDDO; ENDDO
!  CASE (2)
!   DO IROW=MAX(1,IR-1),IR; DO ICOL=IC,MIN(IDF%NCOL,IC+1)
!    DX=COS(IDF%X(ICOL,IROW)); DY=SIN(IDF%X(ICOL,IROW))
!    A(1)=A(1)+DX; A(2)=A(2)+DY
!   ENDDO; ENDDO
!  CASE (3)
!   DO IROW=IR,MIN(IDF%NROW,IR+1); DO ICOL=IC,MIN(IDF%NCOL,IC+1)
!    DX=COS(IDF%X(ICOL,IROW)); DY=SIN(IDF%X(ICOL,IROW))
!    A(1)=A(1)+DX; A(2)=A(2)+DY
!   ENDDO; ENDDO
!  CASE (4)
!   DO IROW=IR,MIN(IDF%NROW,IR+1); DO ICOL=MAX(1,IC-1),IC
!    DX=COS(IDF%X(ICOL,IROW)); DY=SIN(IDF%X(ICOL,IROW))
!    A(1)=A(1)+DX; A(2)=A(2)+DY
!   ENDDO; ENDDO
! END SELECT
! 
! SOF_TRACE_GET_ANGLE=ATAN2(A(2),A(1)) !/4.0
! 
!! SOF_TRACE_GET_ANGLE=IDF%X(IC,IR)
!
! END FUNCTION SOF_TRACE_GET_ANGLE
  
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
 SUBROUTINE SOF_MAIN2(IDF,IFLOW,IPNTR,IWINDOW,XMIN,YMIN,XMAX,YMAX) !,DTERM)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:) :: IDF
 INTEGER,INTENT(IN) :: IWINDOW
 REAL,INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
 INTEGER,INTENT(IN) :: IFLOW !## iflow=nul compute depressions, iflow=one, compute flowpaths
! INTEGER,INTENT(IN) :: DTERM !## diagonal yes (3) no (2)
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
!  IF(.NOT.IDFREADSCALE(IDF(1)%FNAME,IDF(1),2,1,0.0,0))THEN; RETURN; ENDIF
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
 
 END SUBROUTINE SOF_MAIN2

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
 DZDX=-AX !/NA
 DZDY= AY !/NA
 
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
! REAL :: Z1,Z2
! LOGICAL :: LSND
 
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
  ZA(I)=1.0 !BPX(I)%Z
!  pcg%x(int(xa(i)),int(ya(i)))=za(i)
 ENDDO
 DO I=1,NPPX
  ICOL            =PPX(I)%ICOL-IC1+1
  IROW            =PPX(I)%IROW-IR1+1
  PCG%X(ICOL,IROW)=0.0 !PPX(I)%Z
 ENDDO
 !## add last pit-location (outflow) as boundary condition as well
 I         = NPPX
 XA(NBPX+1)= PPX(I)%ICOL-IC1+1
 YA(NBPX+1)= PPX(I)%IROW-IR1+1
 ZA(NBPX+1)=-1.0
   
 HCLOSE=0.0001; RELAX=1.0; IDAMPING=0; MXITER1=100; MXITER2=1000; ITIGHT=2
 CALL SOLID_PCGINT(XA,YA,ZA,NBPX+1,IERROR,PCG,0,HNOFLOW=PCG%NODATA)
 IF(IERROR.EQ.1)STOP 'ERROR IN PCG'

 !## remove boundary nodes, no to be used for aspect computation
 DO I=1,NBPX+1; PCG%X(INT(XA(I)),INT(YA(I)))=PCG%NODATA; ENDDO
 
 ! DO IROW=1,NROW; DO ICOL=1,NCOL
 !  IF(IDF%X(ICOL,IROW).EQ.HNOFLOW)IB(ICOL,IROW,1)=0
 ! ENDDO; ENDDO

 DO IROW=1,PCG%NROW; DO ICOL=1,PCG%NCOL
  IF(PCG%X(ICOL,IROW).EQ.PCG%NODATA)CYCLE

!  DO IR=MAX(IROW-1,1),MIN(IROW+1,PCG%NROW)
!   DO IC=MAX(ICOL-1,1),MIN(ICOL+1,PCG%NCOL)
!    IF(PCG%X(IC,IR).EQ.HNOFLOW)
!   ENDDO
!  ENDDO

  CALL SOF_COMPUTE_GRAD(PCG,ICOL,IROW,DZDX,DZDY)

!  CALL SOF_COMPUTE_GRAD_STEEPEST(PCG,ICOL,IROW,DZDX,DZDY)
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
 
 !###======================================================================
 SUBROUTINE SOF_FILL_PITT2(DEM,IDFP,DTERM)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: DEM,IDFP
 INTEGER,INTENT(IN) :: DTERM !## diagonal yes (3) no (2)
 INTEGER :: I,J,ICOL,IROW,IC,IR,MAXTHREAD,MAXN,IMENU,IPZ,JPZ,NTHREAD
 INTEGER(KIND=1),POINTER,DIMENSION(:) :: ISPEC 
 INTEGER(KIND=2),POINTER,DIMENSION(:,:) :: THREAD,YSEL 
 REAL :: ZMAX,F
 REAL,ALLOCATABLE,DIMENSION(:) :: DZ  

 WRITE(6,'(1X,A/)') 'Filling in pitts'

 !##fill in pitts by tracing up to the exit points
 MAXTHREAD=1000; MAXN=MAXTHREAD; ALLOCATE(ISPEC(MAXTHREAD),THREAD(3,MAXTHREAD),YSEL(2,MAXTHREAD))
  
! DTERM=2 !## 0=no diagonal (use 1 to apply diagonals,2=steepest)
! DTERM=3 !## 0=no diagonal (use 1 to apply diagonals,2=steepest)
 IMENU=7 !## 1=equal,2=less than,3=less or equal,4=greater than,5=greater than or equal,6=ne,7=never mind
         !## negative means according to last location during search

 !## search for depressions
 IPZ=0; NTHREAD=0
 
 IF(DTERM.EQ.2)ALLOCATE(DZ(4))
 IF(DTERM.EQ.3)ALLOCATE(DZ(8))

 !## trace depression for exit points
 DO I=1,NP
  ICOL=INT(PL(I,1)); IROW=INT(PL(I,2))

  !## set begin values
  NTHREAD=1; YSEL(1,NTHREAD)=INT(ICOL,2); YSEL(2,NTHREAD)=INT(IROW,2); IPZ=1
  !## trace all higher than neighbouring cell, imenu=-4, idf(1) will be adjusted
  CALL IDFEDITTRACE(DEM,IDFP,THREAD,YSEL,ISPEC,DTERM,IMENU,MAXTHREAD,MAXN,10.0E21,NTHREAD,IPZ,JPZ,DZ=DZ) !## entire path
     
  !# spill points
  IC=INT(YSEL(1,1)); IR=INT(YSEL(2,1)); ZMAX=DEM%X(IC,IR)
  DO J=1,NTHREAD,1
   IC=INT(YSEL(1,J)); IR=INT(YSEL(2,J)); DEM%X(IC,IR)=ZMAX
   IDFP%X(IC,IR)=IDFP%NODATA
  ENDDO
 
  F=REAL(I)/REAL(NP)*100.0
  WRITE(6,'(A,F10.3,A,F10.3,A)') '+Progress z:',PL(I,3),'finished ',F,' %         '
   
 ENDDO
 
 DEALLOCATE(DZ)
 
 END SUBROUTINE SOF_FILL_PITT2 

 !###======================================================================
 SUBROUTINE SOF_MAIN(IDF,IFLOW,IPNTR,IWINDOW,XMIN,YMIN,XMAX,YMAX,MINTHREAD,DTERM)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT),DIMENSION(:) :: IDF
 INTEGER,INTENT(IN) :: IWINDOW,MINTHREAD
 REAL,INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
 INTEGER,INTENT(IN) :: IFLOW !## iflow=nul compute depressions, iflow=one, compute flowpaths
 INTEGER,INTENT(IN) :: DTERM !## diagonal yes (3) no (2)
 INTEGER,INTENT(IN) :: IPNTR !## ipntr=0 no pointer to stop, 1 use pointer to stop
 INTEGER :: MAXTHREAD,IMENU,NTHREAD,I,J,IROW,ICOL,IR,IC,IPZ,JPZ,MAXN,ITRACE,ITIC,ITOC, &
     IRC,DROW,DCOL,ISAVEDTRACE,JTRACE,N
! INTEGER(KIND=2),POINTER,DIMENSION(:,:) :: RCPOS
 INTEGER(KIND=1),POINTER,DIMENSION(:) :: ISPEC 
 INTEGER(KIND=2),POINTER,DIMENSION(:,:) :: THREAD,YSEL 
 REAL :: ZMIN,ZMAX,ZCRIT,T,F,Z,Z1,Z2
 INTEGER :: IC1,IC2,IR1,IR2,DC,DR
 TYPE YSELOBJ
  INTEGER(KIND=2),POINTER,DIMENSION(:,:) :: YSEL
  REAL,POINTER,DIMENSION(:) :: Z
  INTEGER :: NTHREAD
 END TYPE YSELOBJ
 REAL,ALLOCATABLE,DIMENSION(:) :: DZ
 TYPE(YSELOBJ),POINTER,DIMENSION(:) :: YSEL_PREV,YSEL_PREV_BU
  
 !## read levelidf
 IF(IWINDOW.EQ.0)THEN
  IF(.NOT.IDFREAD(IDF(1),IDF(1)%FNAME,1))THEN; RETURN; ENDIF
 ELSE
  IF(.NOT.IDFREAD(IDF(1),IDF(1)%FNAME,0))THEN; RETURN; ENDIF; CLOSE(IDF(1)%IU)
  IDF(1)%XMIN=XMIN; IDF(1)%XMAX=XMAX; IDF(1)%YMIN=YMIN; IDF(1)%YMAX=YMAX;
  CALL UTL_IDFSNAPTOGRID(IDF(1)%XMIN,IDF(1)%XMAX,IDF(1)%YMIN,IDF(1)%YMAX,IDF(1)%DX,IDF(1)%NCOL,IDF(1)%NROW)
  IF(.NOT.IDFREADSCALE(IDF(1)%FNAME,IDF(1),2,1,0.0,0))THEN; RETURN; ENDIF
 ENDIF
 !## set nodata idf(1)=-9999.00
 IF(IDF(1)%NODATA.NE.-9999.0)THEN
  DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
   IF(IDF(1)%X(ICOL,IROW).EQ.IDF(1)%NODATA)IDF(1)%X(ICOL,IROW)=-9999.00
  ENDDO; ENDDO
  IDF(1)%NODATA=-9999.00
 ENDIF
    
 !## allocate grid to determine area of influence by pointer
 DO I=2,SIZE(IDF); CALL IDFCOPY(IDF(1),IDF(I)); IF(.NOT.IDFALLOCATEX(IDF(I)))STOP; IDF(I)%NODATA=0.0; ENDDO
 !## initialize arrays
 DO I=2,SIZE(IDF); IDF(I)%X=IDF(I)%NODATA; ENDDO
  
 !## initialize number of tracing along path
 IF(IFLOW.EQ.1)THEN
  IDF(4)%X=0.0; IDF(6)%X=IDF(1)%X
 ENDIF

 !## read pointer (scale with most frequent value, option 7)
 IF(IPNTR.EQ.1)THEN
  CALL IDFCOPY(IDF(1),IDF(5)); IF(.NOT.IDFREADSCALE(IDF(5)%FNAME,IDF(5),7,1,0.0,0))THEN; RETURN; ENDIF
  !## adjust idf(1) for pointer
  DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
   IF(IDF(5)%X(ICOL,IROW).NE.IDF(5)%NODATA)IDF(1)%X(ICOL,IROW)=IDF(1)%NODATA
  ENDDO; ENDDO
  CALL IDFDEALLOCATEX(IDF(5))
 ENDIF
 
 MAXTHREAD=1000; MAXN=MAXTHREAD; ALLOCATE(ISPEC(MAXTHREAD),THREAD(3,MAXTHREAD),YSEL(2,MAXTHREAD))
 !## allocate pointer (takes too much time to increase)
 IF(IFLOW.EQ.1)THEN; ALLOCATE(YSEL_PREV(250)); YSEL_PREV%NTHREAD=0; ENDIF
 
 !## idf(1)=level
 !## idf(2)=ldd (pointer with flowdirection 1-8)
 !## idf(3)=visited places/
 !## idf(4)=zmax/counter
 !## idf(5)=pointer flow area
 !## if iflow=1
 !##  idf(6)=original level used to reset after tracing
 !##  idf(7)=last tracked flowpath (numbered)
 !##  idf(8)=number of saved trace
 
! DTERM=2 !## 0=no diagonal (use 1 to apply diagonals,2=steepest)
! DTERM=3 !## 0=no diagonal (use 1 to apply diagonals,2=steepest)
 IMENU=7 !## 1=equal,2=less than,3=less or equal,4=greater than,5=greater than or equal,6=ne,7=never mind
         !## negative means according to last location during search

 CALL OSD_TIMER(ITIC)
 WRITE(6,'(/A/)') '----'
 
 !## search for depressions
 IPZ=0; NTHREAD=0; DROW=30; DCOL=30; ITRACE=0; JTRACE=0; ISAVEDTRACE=0
 
 IF(DTERM.EQ.2)ALLOCATE(DZ(4))
 IF(DTERM.EQ.3)ALLOCATE(DZ(8))

 !## adjust surface in backwards direction
 IF(IFLOW.EQ.0)THEN

  DO IROW=1,IDF(1)%NROW
   DO ICOL=1,IDF(1)%NCOL
    
    IF(IDF(2)%X(ICOL,IROW).EQ.IDF(2)%NODATA.AND. &  !## not yet visited
       IDF(1)%X(ICOL,IROW).NE.IDF(1)%NODATA)THEN    !## level.ne.nodata
          
     !## set begin values
     NTHREAD=1; YSEL(1,NTHREAD)=INT(ICOL,2); YSEL(2,NTHREAD)=INT(IROW,2); IPZ=IPZ+1
     !## trace all higher than neighbouring cell, imenu=-4, idf(1) will be adjusted
     CALL IDFEDITTRACE(IDF(1),IDF(3),THREAD,YSEL,ISPEC,DTERM,IMENU,MAXTHREAD,MAXN,10.0E21,NTHREAD,IPZ,JPZ,DZ=DZ) !## entire path

     IF(NTHREAD.GT.0)THEN
      I=NTHREAD; IC=INT(YSEL(1,I)); IR=INT(YSEL(2,I)); ZMAX=IDF(1)%X(IC,IR)
      DO I=NTHREAD,1,-1
       IC=INT(YSEL(1,I)); IR=INT(YSEL(2,I)); ZMAX=MAX(ZMAX,IDF(1)%X(IC,IR)); IDF(4)%X(IC,IR)=ZMAX
       !## put number of processes along path
       IDF(2)%X(IC,IR)=I
      ENDDO
      IF(IPZ.NE.JPZ)THEN; IC=INT(YSEL(1,NTHREAD)); IR=INT(YSEL(2,NTHREAD)); IDF(3)%X(IC,IR)=JPZ; ENDIF !## reset zone to captured zone
     ENDIF
      
      F=(REAL(IRC)/REAL(IDF(1)%NROW*IDF(1)%NCOL))*100.0
      WRITE(6,'(A,F7.3,A,2I6,A,2I10)') '+Progress ',F,' % finished (',IROW,ICOL,')',NTHREAD,IRC
   
    ENDIF
   ENDDO
  ENDDO

 !## adjust surface in backwards direction
 ELSEIF(IFLOW.EQ.2)THEN

!  IDF(2)%X=0.0; IDF(3)%X=0.0
!  DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL-1
!    IDF(2)%X(ICOL,IROW)=IDF(1)%X(ICOL+1,IROW)-IDF(1)%X(ICOL,IROW)
!  ENDDO; ENDDO
!  DO IROW=1,IDF(1)%NROW-1; DO ICOL=1,IDF(1)%NCOL
!   IDF(3)%X(ICOL,IROW)=IDF(1)%X(ICOL,IROW+1)-IDF(1)%X(ICOL,IROW)
!  ENDDO; ENDDO
  
!!open(99,file='d:\log.txt',status='unknown',action='write')
!  DO IROW=1,IDF(1)%NROW
!   DO ICOL=1,IDF(1)%NCOL
!    
!    IF(IDF(2)%X(ICOL,IROW).EQ.IDF(2)%NODATA.AND. &  !## not yet visited
!       IDF(1)%X(ICOL,IROW).NE.IDF(1)%NODATA)THEN    !## level.ne.nodata
!          
!     !## set begin values
!     NTHREAD=1; YSEL(1,NTHREAD)=INT(ICOL,2); YSEL(2,NTHREAD)=INT(IROW,2); IPZ=IPZ+1
!     !## trace all higher than neighbouring cell, imenu=-4, idf(1) will be adjusted
!     CALL IDFEDITTRACE(IDF(1),IDF(3),THREAD,YSEL,ISPEC,DTERM,IMENU,MAXTHREAD,MAXN,10.0E21,NTHREAD,IPZ,JPZ,DZ=DZ) !## entire path
!     
!     IF(NTHREAD.GT.0)THEN
!      DO I=NTHREAD,2,-1
!       IC1=INT(YSEL(1,I))  ; IR1=INT(YSEL(2,I));   Z1=IDF(1)%X(IC1,IR1)
!       IC2=INT(YSEL(1,I-1)); IR2=INT(YSEL(2,I-1)); Z2=IDF(1)%X(IC2,IR2)
!       DC=IC1-IC2; DR=IR1-IR2
!       IF(ABS(DC).LE.1.AND.ABS(DR).LE.1)THEN
!        IDF(2)%X(IC,IR)=UTL_GETDIR(DC,DR)
!       ENDIF
!      ENDDO
!      IF(IPZ.NE.JPZ)THEN; IC=INT(YSEL(1,NTHREAD)); IR=INT(YSEL(2,NTHREAD)); IDF(3)%X(IC,IR)=JPZ; ENDIF !## reset zone to captured zone
!     ENDIF
! 
!      F=REAL(IROW)/REAL(IDF(1)%NROW)*100.0
!      WRITE(6,'(A,F7.3,A,2I6,A,I10)') '+Progress ',F,' % finished (',IROW,ICOL,')',NTHREAD
!   
!    ENDIF
!   ENDDO
!  ENDDO
!!close(99)

 ELSEIF(IFLOW.EQ.1)THEN
 
  DO IROW=1,IDF(1)%NROW
   DO ICOL=1,IDF(1)%NCOL
    
    IF(IDF(2)%X(ICOL,IROW).EQ.IDF(2)%NODATA.AND. &  !## not yet visited
       IDF(1)%X(ICOL,IROW).NE.IDF(1)%NODATA)THEN    !## level.ne.nodata
           
    !## if allready traced, skip tracing it, copy saved thread instead
    IF(IDF(7)%X(ICOL,IROW).GT.0.0)THEN

     ITRACE=INT(IDF(7)%X(ICOL,IROW))
     JTRACE=INT(IDF(8)%X(ICOL,IROW))
     NTHREAD=0

    ELSE
 
     !## set begin values
     NTHREAD=1; YSEL(1,NTHREAD)=INT(ICOL,2); YSEL(2,NTHREAD)=INT(IROW,2); IPZ=1; ITRACE=0; JTRACE=0 
     !## trace all higher than neighbouring cell, imenu=-4, idf(1) will be adjusted
     CALL IDFEDITTRACE(IDF(1),IDF(3),THREAD,YSEL,ISPEC,DTERM,IMENU,MAXTHREAD,MAXN,10.0E21,NTHREAD,IPZ,JPZ, &
                       PTRACE=IDF(7),ITRACE=ITRACE,STRACE=IDF(8),JTRACE=JTRACE,NTRACE=YSEL_PREV%NTHREAD ,&
                       MINTHREAD=MINTHREAD,DZ=DZ) !## entire path
    
    ENDIF
    
    !## overrule variable itrace if trace is too small to be stored
    IF(ITRACE.EQ.0.AND.NTHREAD.LT.MINTHREAD)ITRACE=-1

! if(icol.eq.64.and.irow.eq.17)then
! if(icol.eq.61.and.irow.eq.21)then
!     DO I=1,NTHREAD
!      IC=INT(YSEL(1,I)); IR=INT(YSEL(2,I))
! if(ic.eq.122.and.ir.eq.211)then
!    write(*,*) icol,irow,i,ic,ir,idf(4)%x(ic,ir),idf(1)%x(ic,ir)
!     idf(2)%x(icol,irow)=1.0
!     if(.not.idfwrite(idf(2),'d:\softrace.idf',1))then; endif
!endif
!    enddo
!    pause
!endif
 
    !## clean history path if itrace.eq.0; reset/update it!
    IF(ITRACE.EQ.0)THEN
     ISAVEDTRACE=ISAVEDTRACE+1
     DO I=1,NTHREAD
      IC=INT(YSEL(1,I)); IR=INT(YSEL(2,I))
      IDF(7)%X(IC,IR)=REAL(I)                !## segment number along thread
      IDF(8)%X(IC,IR)=REAL(ISAVEDTRACE)      !## thread number
     ENDDO
!     if(.not.idfwrite(idf(7),'d:\softrace.idf',1))then; endif
!     if(.not.idfwrite(idf(8),'d:\softrace_id.idf',1))then; endif
    ENDIF
      
    !## process renewed trace (part of total trace)
    DO I=1,NTHREAD
     IC=INT(YSEL(1,I)); IR=INT(YSEL(2,I))            !## put number of processes along path
     IDF(4)%X(IC,IR)=IDF(4)%X(IC,IR)+1.0             !## increase number of processes along path
    ENDDO   

    !## continue trace from history trace
    IF(ITRACE.GT.0)THEN
     DO J=ITRACE,SIZE(YSEL_PREV(JTRACE)%YSEL,2) 
      IC=INT(YSEL_PREV(JTRACE)%YSEL(1,J))
      IR=INT(YSEL_PREV(JTRACE)%YSEL(2,J)) 
      IDF(4)%X(IC,IR)=IDF(4)%X(IC,IR)+1.0            !## increase number of processes along path
     ENDDO         
     !## include a backtrace to increase pointer for flat areas (lakes/ponds)
     IC=INT(YSEL_PREV(JTRACE)%YSEL(1,ITRACE))
     IR=INT(YSEL_PREV(JTRACE)%YSEL(2,ITRACE))
     Z=YSEL_PREV(JTRACE)%Z(ITRACE)
     DO J=ITRACE-1,1,-1
      IC=INT(YSEL_PREV(JTRACE)%YSEL(1,J))
      IR=INT(YSEL_PREV(JTRACE)%YSEL(2,J))
      IF(YSEL_PREV(JTRACE)%Z(J).NE.Z)EXIT
      IDF(4)%X(IC,IR)=IDF(4)%X(IC,IR)+1.0            !## increase number of processes along path
     ENDDO
    ENDIF

    !## copy last trace, if it is big enough
    IF(ITRACE.EQ.0)THEN

     IF(ISAVEDTRACE.GT.SIZE(YSEL_PREV))THEN
      ALLOCATE(YSEL_PREV_BU(ISAVEDTRACE+50))
      DO I=1,SIZE(YSEL_PREV)
       N=SIZE(YSEL_PREV(I)%YSEL,2)
       ALLOCATE(YSEL_PREV_BU(I)%YSEL(2,N),YSEL_PREV_BU(I)%Z(N))
       DO J=1,N 
        YSEL_PREV_BU(I)%YSEL(1,J)=YSEL_PREV(I)%YSEL(1,J)
        YSEL_PREV_BU(I)%YSEL(2,J)=YSEL_PREV(I)%YSEL(2,J)
        YSEL_PREV_BU(I)%Z(J)     =YSEL_PREV(I)%Z(J)
       ENDDO
       YSEL_PREV_BU(I)%NTHREAD=YSEL_PREV(I)%NTHREAD
       DEALLOCATE(YSEL_PREV(I)%YSEL)
      ENDDO
      DEALLOCATE(YSEL_PREV)
      YSEL_PREV=>YSEL_PREV_BU
     ENDIF
      
     ALLOCATE(YSEL_PREV(ISAVEDTRACE)%YSEL(2,NTHREAD),YSEL_PREV(ISAVEDTRACE)%Z(NTHREAD))
     YSEL_PREV(ISAVEDTRACE)%NTHREAD=NTHREAD
     DO I=1,NTHREAD
      IC=INT(YSEL(1,I)); IR=INT(YSEL(2,I))
      YSEL_PREV(ISAVEDTRACE)%YSEL(1,I)=YSEL(1,I)
      YSEL_PREV(ISAVEDTRACE)%YSEL(2,I)=YSEL(2,I)
      YSEL_PREV(ISAVEDTRACE)%Z(I)     =IDF(1)%X(IC,IR)
     ENDDO
 
    ENDIF

    !## reset values for next thread
    DO I=1,NTHREAD
     IC=INT(YSEL(1,I)); IR=INT(YSEL(2,I))            !## put number of processes along path
     IDF(1)%X(IC,IR)=IDF(6)%X(IC,IR)                 !## reset dem
     IDF(3)%X(IC,IR)=IDF(3)%NODATA                   !## reset visited positions
    ENDDO   

    F=(REAL(IRC)/REAL(IDF(1)%NROW*IDF(1)%NCOL))*100.0
    IF(JTRACE.EQ.0)THEN
     WRITE(6,'(A,F7.3,A,2I4,A,5I6,I10)') '+Progress ',F,' % finished (',IROW,ICOL,')',NTHREAD,ITRACE,JTRACE, &
                                             0,ISAVEDTRACE,IRC
    ELSE
     WRITE(6,'(A,F7.3,A,2I4,A,5I6,I10)') '+Progress ',F,' % finished (',IROW,ICOL,')',NTHREAD,ITRACE,JTRACE, &
                   YSEL_PREV(JTRACE)%NTHREAD,ISAVEDTRACE,IRC
    ENDIF
    
   ENDIF   

   ENDDO
  ENDDO
 ENDIF
 
 DEALLOCATE(DZ)
 
 CALL OSD_TIMER(ITOC)

 !## idf(3)=number of zone after trace
 IF(IFLOW.EQ.0)THEN
  IF(.NOT.IDFWRITE(IDF(1),IDF(3)%FNAME,1))THEN; ENDIF
  IF(.NOT.IDFWRITE(IDF(2),IDF(3)%FNAME(:INDEX(IDF(3)%FNAME,'\',.TRUE.))//'nz.idf',1))THEN; ENDIF   !## number thread()
  IF(.NOT.IDFWRITE(IDF(3),IDF(3)%FNAME(:INDEX(IDF(3)%FNAME,'\',.TRUE.))//'ip.idf',1))THEN; ENDIF   !## number sequention
  IF(.NOT.IDFWRITE(IDF(4),IDF(3)%FNAME(:INDEX(IDF(3)%FNAME,'\',.TRUE.))//'zmax.idf',1))THEN; ENDIF !## sof elevation
 ELSEIF(IFLOW.EQ.2)THEN
  IDF(2)%NODATA=0.0; IF(.NOT.IDFWRITE(IDF(2),IDF(3)%FNAME(:INDEX(IDF(3)%FNAME,'\',.TRUE.))//'frf.idf',1))THEN; ENDIF
  IDF(3)%NODATA=0.0; IF(.NOT.IDFWRITE(IDF(3),IDF(3)%FNAME(:INDEX(IDF(3)%FNAME,'\',.TRUE.))//'fff.idf',1))THEN; ENDIF
 ELSEIF(IFLOW.EQ.1)THEN
  IDF(4)%NODATA=-999.99
  IF(.NOT.IDFWRITE(IDF(4),IDF(3)%FNAME,1))THEN; ENDIF
 ENDIF
  
 DEALLOCATE(THREAD,ISPEC,YSEL); IF(ASSOCIATED(YSEL_PREV))DEALLOCATE(YSEL_PREV)
 
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
 INTEGER FUNCTION UTL_GETDIR(DC,DR)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DC,DR
 INTEGER,DIMENSION(-1:1,-1:1) :: IDIR
 DATA IDIR/1,2,3,&
           8,0,4,&
           7,6,5 / 
 
 UTL_GETDIR=IDIR(DC,DR)
 
 END FUNCTION UTL_GETDIR

 !###======================================================================
 LOGICAL FUNCTION SOF_TOP(IDF,ICOL,IROW)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF 
 INTEGER,INTENT(IN) :: ICOL,IROW
 INTEGER :: I,IR,IC
 REAL,DIMENSION(4) :: DZ 
 
 SOF_TOP=.FALSE.

! IF(ICOL.EQ.300.AND.IROW.EQ.156)THEN; SOF_TOP=.TRUE.; RETURN; ENDIF  !## afvoerputje
  
 SOF_TOP=.TRUE.
 
 END FUNCTION SOF_TOP

END MODULE MOD_SOF    