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

MODULE MOD_GOOGLE

USE MODPLOT, ONLY : MPW
USE MOD_UTL, ONLY : RTOS,DECDEGREES_TO_DMS
USE IMODVAR, ONLY : PI
USE MOD_UTM, ONLY : UTM_INIT,UTM_UTM2LATLONG

CONTAINS

 !###====================================================================
 SUBROUTINE GOOGLE_MAIN()
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=256) :: CGM
 CHARACTER(LEN=50) :: LL
! REAL :: X,Y,LAT,LONG,LATMIN,LONGMIN,LATMAX,LONGMAX
 DOUBLE PRECISION :: XUTM,YUTM,LAT,LONG,LONG0
 REAL :: D,M,S
 
 !## xmin/ymin
 XUTM=MPW%XMIN/1000.0; YUTM=MPW%YMIN/1000.0

 CALL UTM_INIT('N') !## northern hemisphere

 XUTM=152.332; YUTM=346.616
 CALL UTM_UTM2LATLONG(1,30,'N',XUTM,YUTM,LAT,LONG)

! !## radians to degrees
! LAT =LAT *(180.0D0/PI); LONG=LONG*(180.0D0/PI)

 CALL DECDEGREES_TO_DMS(LAT,D,M,S)
 write(*,*) d,m,s
 
 CALL DECDEGREES_TO_DMS(LONG,D,M,S)
 write(*,*) d,m,s
return

! CALL GOOGLE_LATLONG(X,Y,LATMIN,LONGMIN)
! !## xmax/ymax
! X=MPW%XMAX
! Y=MPW%YMAX
! CALL GOOGLE_LATLONG(X,Y,LATMAX,LONGMAX)
! !## xmid/ymid
! X=(MPW%XMAX+MPW%XMIN)/2.0
! Y=(MPW%YMAX+MPW%YMIN)/2.0
! CALL GOOGLE_LATLONG(X,Y,LAT,LONG)

! LL=TRIM(RTOS(LAT,'F',5))//','//TRIM(RTOS(LONG,'F',5))
!
! CGM='explorer '//'"http://maps.google.com/?ie=UTF8&ll='//TRIM(LL)//'&spn='
!
! LL=TRIM(RTOS(LATMAX-LATMIN,'F',5))//','//TRIM(RTOS(LONGMAX-LONGMIN,'F',5))
!
! CGM=TRIM(CGM)//TRIM(LL)//'"'!//'&z=11"'
!
!!WRITE(*,*) TRIM(cgm)
!
!!http://maps.google.com/?ie=UTF8&ll=51.360259,5.454798&spn=0.011255,0.0421&z=15
!
!! CGM='explorer '//'"http://maps.google.com/?ie=UTF8&ll='//TRIM(LL)//'&spn=0.179101,0.673599&z=11"'
!
!!http://maps.google.com/?ie=UTF8&ll=51.60821,6.047287&spn=0.179101,0.673599&z=11
!
!!    write(CGM,'(A256)') trim(CGM)
!!http://maps.google.com/maps?f=q&hl=en&geocode=&ie=UTF8&q=51.93256,5.712010&sll=51.93256,5.712010&sspn=0.038101,0.076218&ie=UTF8&t=h&z=16
!!call system('c:\users\peter\work\gw\imod\src\google.bat')
!!    OPEN(1,FILE='d:\test.txt')
!!    WRITE(1,*) "start '"//TRIM(cgm)//"'"
!
!!explorer "http://maps.google.com/maps?f=q&hl=en&geocode=&ie=UTF8&q=51.93256,5.712010&sll=51.93256,5.712010&sspn=0.038101,0.076218&ie=UTF8&t=h&z=16"
!call system(cgm)
!!    call system ('start "'//TRIM(cgm)//'"')

 END SUBROUTINE GOOGLE_MAIN

 !###====================================================================
 SUBROUTINE GOOGLE_LATLONG(X,Y,LAT,LONG)
 !###====================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: X,Y
 REAL,INTENT(OUT) :: LAT,LONG
 REAL PHIBES,LAMBES,PHIWGS,LAMWGS

 CALL GOOGLE_RD2BESSEL(X,Y,PHIBES,LAMBES)
 CALL GOOGLE_BESSEL2WGS84(PHIBES,LAMBES,PHIWGS,LAMWGS)

 LAT =LAMWGS
 LONG=PHIWGS

 END SUBROUTINE GOOGLE_LATLONG

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

END MODULE

