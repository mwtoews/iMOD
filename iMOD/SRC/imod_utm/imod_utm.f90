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
MODULE MOD_UTM

USE MOD_UTM_PAR
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_IDF, ONLY : IDFREAD,IDFGETLOC,IDFGETVAL
USE MOD_UTL, ONLY : UTL_GETUNIT
USE IMODVAR, ONLY : PI

CONTAINS

 !###======================================================================
 SUBROUTINE UTM_MAIN()
 !###======================================================================
 IMPLICIT NONE
 DOUBLE PRECISION :: LAT,LONG,LONG0,XUTM,YUTM,lat1,long1
 REAL :: DEG,MIN,SEC
 INTEGER :: IZONE,IW,IE,IM
 CHARACTER(LEN=1) :: LBAND
 
 DEG =40.0;MIN=30.0; SEC=0.0
 LAT =DEG+MIN/60.0+SEC/3600.0
 DEG =-73.0;MIN=30.0; SEC=00.0
 LONG=DEG+MIN/60.0+SEC/3600.0
 LAT1=LAT
 LONG1=LONG
 
 CALL UTM_LATLONG2UTM(1,LAT,LONG,IZONE,LBAND,XUTM,YUTM)
 CALL UTM_UTM2LATLONG(1,IZONE,LBAND,XUTM,YUTM,LAT,LONG)
   
 END SUBROUTINE UTM_MAIN
 
 !###======================================================================
 SUBROUTINE UTM_INIT(LBAND)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=1),INTENT(IN) :: LBAND
 INTEGER :: I
 DOUBLE PRECISION :: N 

 UTM(1)%DATUM='NAD83/WGS84'
 UTM(1)%A    = 6378.1370
 UTM(1)%B    = 6356.7523142

 UTM(2)%DATUM='GRS80'
 UTM(2)%A    = 6378.1370
 UTM(2)%B    = 6356.7523141
 
 DO I=1,NUTM
  
  UTM(I)%FLAT=(UTM(I)%A-UTM(I)%B)/UTM(I)%A
  UTM(I)%K0  = 0.9996D0
  UTM(I)%E0  = 500.0D0   !## kilometers
  
  !## get starting latitude for the latitue band
  SELECT CASE (LBAND)
   CASE ('C'); UTM(I)%LBAND=-80.0D0
   CASE ('D'); UTM(I)%LBAND=-72.0D0
   CASE ('E'); UTM(I)%LBAND=-64.0D0
   CASE ('F'); UTM(I)%LBAND=-56.0D0
   CASE ('G'); UTM(I)%LBAND=-48.0D0
   CASE ('H'); UTM(I)%LBAND=-40.0D0
   CASE ('J'); UTM(I)%LBAND=-32.0D0
   CASE ('K'); UTM(I)%LBAND=-24.0D0
   CASE ('L'); UTM(I)%LBAND=-16.0D0
   CASE ('M'); UTM(I)%LBAND= -8.0D0
   CASE ('N'); UTM(I)%LBAND=  0.0D0
   CASE ('P'); UTM(I)%LBAND=  8.0D0
   CASE ('Q'); UTM(I)%LBAND= 16.0D0
   CASE ('R'); UTM(I)%LBAND= 24.0D0
   CASE ('S'); UTM(I)%LBAND= 32.0D0
   CASE ('T'); UTM(I)%LBAND= 40.0D0
   CASE ('U'); UTM(I)%LBAND= 48.0D0
   CASE ('V'); UTM(I)%LBAND= 56.0D0
   CASE ('W'); UTM(I)%LBAND= 64.0D0
   CASE ('X'); UTM(I)%LBAND= 72.0D0
  END SELECT
  
  IF(UTM(I)%N0.LE.0.0D0)THEN
   UTM(I)%N0=    0.0D0 !## kilometers
  ELSE
   UTM(I)%N0=10000.0D0 !## kilometers
  ENDIF
  
  N              = UTM(I)%FLAT/(2.0D0-UTM(I)%FLAT)
  UTM(I)%N       = N
  UTM(I)%AA      =(UTM(I)%A/(1.0D0+N)) * (1.0D0 + (N**2/4.0D0) + (N**4/64.0D0))
 
  UTM(I)%ALPHA(1)=(( 1.0D0/  2.0D0)*N)      - ((2.0D0/ 3.0D0)*N**2.0) + (( 5.0D0/16.0D0)*N**3.0)
  UTM(I)%ALPHA(2)=((13.0D0/ 48.0D0)*N**2.0) - ((3.0D0/ 5.0D0)*N**3.0)
  UTM(I)%ALPHA(3)=((61.0D0/240.0D0)*N**3.0)
  UTM(I)%BETA(1) =(( 1.0D0/  2.0D0)*N)      - ((2.0D0/ 3.0D0)*N**2.0) + ((37.0D0/96.0D0)*N**3.0)
  UTM(I)%BETA(2) =(( 1.0D0/ 48.0D0)*N**2.0) + ((1.0D0/15.0D0)*N**3.0)
  UTM(I)%BETA(3) =((17.0D0/480.0D0)*N**3.0)
  UTM(I)%DELTA(1)=   2.0D0         *N       - ((2.0D0/ 3.0D0)*N**2.0) - (  2.0D0        *N**3.0)
  UTM(I)%DELTA(2)=(( 7.0D0/  3.0D0)*N**2.0) - ((8.0D0/ 5.0D0)*N**3.0)
  UTM(I)%DELTA(3)=((56.0D0/ 15.0D0)*N**3.0)

 ENDDO
 
 END SUBROUTINE UTM_INIT

 !###======================================================================
 SUBROUTINE UTM_GETIZONE(LONG_DEG,IZONE,LONG0)
 !###======================================================================
 IMPLICIT NONE
 DOUBLE PRECISION,INTENT(IN) :: LONG_DEG  
 INTEGER,INTENT(OUT) :: IZONE
 DOUBLE PRECISION,INTENT(OUT) :: LONG0
 
 !## get zone number
 IZONE=(FLOOR(LONG_DEG+180.0D0)/6.0D0)+1
 CALL UTM_GETLONG0(IZONE,LONG0)

 END SUBROUTINE UTM_GETIZONE

 !###======================================================================
 SUBROUTINE UTM_GETLONG0(IZONE,LONG0)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IZONE
 DOUBLE PRECISION,INTENT(OUT) :: LONG0
 
 !## get central meridean
 LONG0= REAL(IZONE)*6.0D0-183.0D0

 END SUBROUTINE UTM_GETLONG0

 !###======================================================================
 SUBROUTINE UTM_GETLBAND(LAT,LBAND)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=1),INTENT(OUT) :: LBAND
 DOUBLE PRECISION,INTENT(IN) :: LAT
 INTEGER :: IBAND
 CHARACTER(LEN=1),DIMENSION(20) :: CBAND
 DATA CBAND/'C','D','E','F','G','H','J','K','L','M', &
            'N','P','Q','R','S','T','U','V','W','X'/

 !## get the latitude band
 IBAND=FLOOR(LAT+80.0D0)/8.0D0
 LBAND=CBAND(IBAND)
  
 END SUBROUTINE UTM_GETLBAND

 !###======================================================================
 SUBROUTINE UTM_LATLONG2UTM(IPROJ,LAT_DEG,LONG_DEG,IZONE,LBAND,XUTM,YUTM)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPROJ
 INTEGER,INTENT(OUT) :: IZONE
 CHARACTER(LEN=1),INTENT(OUT) :: LBAND
 DOUBLE PRECISION,INTENT(IN) :: LAT_DEG,LONG_DEG !## in degrees
 DOUBLE PRECISION,INTENT(OUT) :: XUTM,YUTM
 DOUBLE PRECISION :: N,T,EPS,NU,D,TAU,LONG0,LAT,LONG,LONG0_DEG
 INTEGER :: I
 
 !## get zone number and central meridean
 CALL UTM_GETIZONE(LONG_DEG,IZONE,LONG0_DEG)
 !## get latitude band
 CALL UTM_GETLBAND(LAT_DEG,LBAND)
 !## initialise parameters   
 CALL UTM_INIT(LBAND)

 !## convert to radians
 LAT  =LAT_DEG  *(PI/180.0D0)
 LONG =LONG_DEG *(PI/180.0D0)
 LONG0=LONG0_DEG*(PI/180.0D0)

 !## compute xutm/yutm coordinates

 N=UTM(IPROJ)%N
 
 T  =SINH( ATANH(SIN(LAT)) - (2.0D0*SQRT(N))/(1.0D0+N) * ATANH( (2.0D0*SQRT(N))/(1.0D0+N) * SIN(LAT) ) )
 EPS=ATAN( T / COS(LONG-LONG0) )
 NU =ATANH(SIN(LONG-LONG0)/SQRT(1.0D0+T**2.0))
  
 D=0.0D0; DO I=1,3
  D=D + UTM(IPROJ)%ALPHA(I)*COS(2.0D0*REAL(I)*EPS)*SINH(2.0D0*REAL(I)*NU)
 ENDDO
 XUTM=UTM(IPROJ)%E0+UTM(IPROJ)%K0*UTM(IPROJ)%AA*(NU+D)
   
 D=0.0D0; DO I=1,3
  D=D + UTM(IPROJ)%ALPHA(I)*SIN(2.0D0*REAL(I)*EPS)*COSH(2.0D0*REAL(I)*NU)
 ENDDO
 YUTM=UTM(IPROJ)%N0+UTM(IPROJ)%K0*UTM(IPROJ)%AA*(EPS+D)

 END SUBROUTINE UTM_LATLONG2UTM

 !###======================================================================
 SUBROUTINE UTM_UTM2LATLONG(IPROJ,IZONE,LBAND,XUTM,YUTM,LAT_DEG,LONG_DEG)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPROJ,IZONE
 CHARACTER(LEN=1),INTENT(IN) :: LBAND
 DOUBLE PRECISION,INTENT(OUT) :: LAT_DEG,LONG_DEG !## in degrees
 DOUBLE PRECISION,INTENT(IN) :: XUTM,YUTM
 DOUBLE PRECISION :: N,T,EPS,NU,NUQ,D,TAU,EPSQ,CHI,LAT,LONG,LONG0
 INTEGER :: I
 
 !## initialise parameters   
 CALL UTM_INIT(LBAND)

 NU =(XUTM-UTM(IPROJ)%E0)/(UTM(IPROJ)%K0*UTM(IPROJ)%A)
 EPS=(YUTM-UTM(IPROJ)%N0)/(UTM(IPROJ)%K0*UTM(IPROJ)%A)

 D=0.0D0; DO I=1,3
  D=D + UTM(IPROJ)%BETA(I)*SIN(2.0D0*REAL(I)*EPS)*COSH(2.0D0*REAL(I)*NU)
 ENDDO
 EPSQ=EPS-D 

 D=0.0D0; DO I=1,3
  D=D + UTM(IPROJ)%BETA(I)*COS(2.0D0*REAL(I)*EPS)*SINH(2.0D0*REAL(I)*NU)
 ENDDO
 NUQ=NU-D 
 
 CHI=ASIN(SIN(EPSQ)/COSH(NUQ))
 
 D=0.0D0; DO I=1,3
  D=D+UTM(IPROJ)%DELTA(I)*SIN(2.0D0*REAL(I)*CHI)
 ENDDO
 LAT=CHI+D
 
 CALL UTM_GETLONG0(IZONE,LONG0)
 !## convert to radians
 LONG0=LONG0*(PI/180.0D0)
 LONG =LONG0+ATAN(SINH(NUQ)/COS(EPSQ))

 !## convert to degrees
 LAT_DEG =LAT *(180.0D0/PI)
 LONG_DEG=LONG*(180.0D0/PI)
   
 END SUBROUTINE UTM_UTM2LATLONG
 
 !###====================================================================
 SUBROUTINE UTM_IDF2LATLONG(IDFNAME)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 REAL :: XUTM,YUTM
! REAL :: LAT,LONG
 INTEGER :: IROW,ICOL,IU
 REAL PHIBES,LAMBES,PHIWGS,LAMWGS
 TYPE(IDFOBJ) :: IDF
  
 IF(.NOT.IDFREAD(IDF,IDFNAME,0))RETURN
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=IDFNAME(:INDEX(IDFNAME,'.',.TRUE.))//'IPF',STATUS='UNKNOWN',ACTION='WRITE')
 WRITE(IU,'(I10)') IDF%NCOL*IDF%NROW
 WRITE(IU,'(I10)') 3
 WRITE(IU,'(A10)') 'LONGITUDE'
 WRITE(IU,'(A10)') 'ATTITUDE'
 WRITE(IU,'(A10)') 'IDFVALUE'
 WRITE(IU,'(A10)') '0,TXT'
 
 DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
  CALL IDFGETLOC(IDF,IROW,ICOL,XUTM,YUTM)
  
! XUTM=75167.0
! YUTM=436716.0
 
  CALL GOOGLE_RD2BESSEL(XUTM,YUTM,PHIBES,LAMBES)
  CALL GOOGLE_BESSEL2WGS84(PHIBES,LAMBES,PHIWGS,LAMWGS)

!  LAT =LAMWGS
!  LONG=PHIWGS

  WRITE(IU,'(3(e15.7,1X))') PHIWGS,LAMWGS,IDFGETVAL(IDF,IROW,ICOL)
  
 ENDDO; ENDDO
 CLOSE(IU)
 
 END SUBROUTINE UTM_IDF2LATLONG

 !###====================================================================
 SUBROUTINE GOOGLE_RD2BESSEL(X,Y,PHIBES,LAMBES)
 !###====================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: X0=1.55E5
 REAL,PARAMETER :: Y0=4.63E5
 REAL,PARAMETER :: K=0.9999079
 REAL,PARAMETER :: BIGR=6382644.571
 REAL,PARAMETER :: M=0.003773953832
 REAL,PARAMETER :: N=1.00047585668
 REAL,PARAMETER :: E=0.08169683122
 REAL,INTENT(IN) :: X,Y
 REAL,INTENT(OUT) :: PHIBES,LAMBES
 REAL :: D__1,D__2
 REAL :: CPSI,SPSI,PHIPRIME,B
 INTEGER :: I
 REAL :: Q,R,W,CA,CB,DL,SA,SB,LAMBDA0
 REAL :: DQ,SDL,PSI,B0

 !## convert XY to Bessel
 !## input is x,y in RD output is phi,PHIBES on the Bessel ellipsoid
! PI=ATAN(1.0)*4
 LAMBDA0=PI*0.029931327161111111
 B0=PI*0.28956165138333334

 D__1=X-X0
 D__2=Y-Y0
 R=SQRT(D__1*D__1+D__2*D__2)
 IF(R.NE.0.0)THEN
  SA=(X-X0)/R
  CA=(Y-Y0)/R
 ELSE
  SA=0.0
  CA=0.0
 ENDIF

 PSI=ATAN2(R, K*2.*BIGR)*2.
 CPSI=COS(PSI)
 SPSI=SIN(PSI)
 SB=CA*COS(B0)*SPSI+SIN(B0)*CPSI
 D__1=SB
 CB=SQRT(1.-D__1*D__1)
 B=ACOS(CB)
 SDL=SA*SPSI/CB
 DL=ASIN(SDL)
 PHIBES=DL/N+LAMBDA0
 W=LOG(TAN(B/2.+PI/4.))
 Q=(W-M)/N
 PHIPRIME=ATAN(EXP(Q))*2.-PI/2.

 DO I=1,4
  DQ=E/2.*LOG((E*SIN(PHIPRIME)+1.)/(1.-E*SIN(PHIPRIME)))
  LAMBES=ATAN(EXP(Q+DQ))*2.-PI/2.
  PHIPRIME=LAMBES
 ENDDO

 PHIBES=PHIBES/PI*180.
 LAMBES=LAMBES/PI*180.

 END SUBROUTINE GOOGLE_RD2BESSEL

 !###====================================================================
 SUBROUTINE GOOGLE_BESSEL2WGS84(PHIBES,LAMBES,PHIWGS,LAMWGS)
 !###====================================================================
 REAL,INTENT(IN) :: PHIBES,LAMBES
 REAL,INTENT(OUT) :: PHIWGS,LAMWGS
 REAL :: DLAM,DPHI,LAMCOR,PHICOR
 REAL,PARAMETER :: A=52.0
 REAL,PARAMETER :: B=5.0
 REAL,PARAMETER :: C=-96.862
 REAL,PARAMETER :: D=11.714
 REAL,PARAMETER :: E=0.125
 REAL,PARAMETER :: F=1.0E-5
 REAL,PARAMETER :: G=0.329
 REAL,PARAMETER :: H=37.902
 REAL,PARAMETER :: I=14.667

 !## convert bessel2 wgs84
 DPHI=PHIBES-A
 DLAM=LAMBES-B
 PHICOR=(C-DPHI*D-DLAM*E)*F
 LAMCOR=(DPHI*G-H-DLAM*I)*F
 PHIWGS=PHIBES+PHICOR
 LAMWGS=LAMBES+LAMCOR

 END SUBROUTINE GOOGLE_BESSEL2WGS84

END MODULE MOD_UTM
