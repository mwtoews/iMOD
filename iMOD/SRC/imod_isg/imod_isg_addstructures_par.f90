!!  Copyright (C) Stichting Deltares, 2005-2017.
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
MODULE MOD_ISG_STRUCTURES_PAR

USE IMODVAR
USE MOD_ISG_PAR

CHARACTER(LEN=256) :: STRUCIPFFNAME,LOGFNAME,LINE
INTEGER :: IX,IY,ID,IO,IS,IWISG,IBATCH,SY,EY
REAL(KIND=DP_KIND) :: MAXDIST
CHARACTER(LEN=5) :: CSPS,CEPS,CSPW,CEPW
CHARACTER(LEN=10) :: CMD

REAL(KIND=DP_KIND),PARAMETER :: R2G=360.0D0/(2.0*3.1415)  !1 rad = 57.15 degrees
REAL(KIND=DP_KIND),PARAMETER :: HNODATA=-999.99
INTEGER :: IUIPF,NCOLIPF,NROWIPF,IOS,IASS,NY,NIP,JDS,IDMD,IMMD,IYMD
INTEGER,ALLOCATABLE,DIMENSION(:,:) :: IP
REAL(KIND=DP_KIND) :: XC,YC,WP,ZP,OR,ANGL,DIST,DORTHO
CHARACTER(LEN=MAXLENISG) :: CI
CHARACTER(LEN=3) :: TXT
CHARACTER(LEN=MAXLENISG),ALLOCATABLE,DIMENSION(:) :: STRING

END MODULE MOD_ISG_STRUCTURES_PAR
