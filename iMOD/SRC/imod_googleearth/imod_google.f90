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
MODULE MOD_GOOGLE

USE MODPLOT, ONLY : MPW
USE MOD_UTL, ONLY : RTOS,DECDEGREES_TO_DMS
USE IMODVAR, ONLY : DP_KIND,SP_KIND,PI
USE MOD_UTM, ONLY : UTM_INIT,UTM_UTM2LATLONG

CONTAINS
 
 !###====================================================================
 SUBROUTINE GOOGLE_MAIN()
 !###====================================================================
 IMPLICIT NONE
! CHARACTER(LEN=256) :: CGM
! CHARACTER(LEN=50) :: LL
! REAL(KIND=DP_KIND) :: X,Y,LAT,LONG,LATMIN,LONGMIN,LATMAX,LONGMAX
 REAL(KIND=DP_KIND) :: XUTM,YUTM,LAT,LONG !,LONG0
 REAL(KIND=DP_KIND) :: D,M,S
 
 !## xmin/ymin
 XUTM=MPW%XMIN/1000.0D0; YUTM=MPW%YMIN/1000.0D0
 
 CALL UTM_INIT('N') !## northern hemisphere

! XUTM=152.332; YUTM=346.616
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
!!http://maps.google.com/?ie=UTF8&ll=51.360259,5.454798&spn=0.011255D0,0.0421D0&z=15
!
!! CGM='explorer '//'"http://maps.google.com/?ie=UTF8&ll='//TRIM(LL)//'&spn=0.179101,0.673599&z=11"'
!
!!http://maps.google.com/?ie=UTF8&ll=51.60821,6.047287&spn=0.179101,0.673599&z=11
!
!!    write(CGM,'(A256)') trim(CGM)
!!http://maps.google.com/maps?f=q&hl=en&geocode=&ie=UTF8&q=51.93256,5.712010&sll=51.93256,5.712010&sspn=0.0D038101,0.0D076218&ie=UTF8&t=h&z=16
!!call system('c:\users\peter\work\gw\imod\src\google.bat')
!!    OPEN(1,FILE='d:\test.txt')
!!    WRITE(1,*) "start '"//TRIM(cgm)//"'"
!
!!explorer "http://maps.google.com/maps?f=q&hl=en&geocode=&ie=UTF8&q=51.93256,5.712010&sll=51.93256,5.712010&sspn=0.0D038101,0.0D076218&ie=UTF8&t=h&z=16"
!call system(cgm)
!!    call system ('start "'//TRIM(cgm)//'"')

 END SUBROUTINE GOOGLE_MAIN
 
END MODULE

